%%% ocs_simple_auth_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  @doc Test suite for authentication 
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_simple_auth_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("kernel/include/inet.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(NAS_APPLICATION_ID, 1).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for authentication in OCS"}]},
	{timetrap, {minutes, 1}},
	{require, radius},
	{default_config, radius, [{username, "ocs"},
			{password, "ocs123"}, {secret, "xyzzy5461"}]},
	{require, diameter},
	{default_config, diameter, [{address, {127,0,0,1}}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:load(ocs),
	RadiusAddress = ct:get_config({radius, address}, {127,0,0,1}),
	RadiusAuthPort = ct:get_config({radius, auth_port}, rand:uniform(64511) + 1024),
	RadiusAcctPort = ct:get_config({radius, acct_port}, rand:uniform(64511) + 1024),
	RadiusAppVar = [{auth, [{RadiusAddress, RadiusAuthPort, []}]},
			{acct, [{RadiusAddress, RadiusAcctPort, []}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterAddress = ct:get_config({diameter, address}, {127,0,0,1}),
	DiameterAuthPort = ct:get_config({diameter, auth_port}, rand:uniform(64511) + 1024),
	DiameterAcctPort = ct:get_config({diameter, acct_port}, rand:uniform(64511) + 1024),
	DiameterAppVar = [{auth, [{DiameterAddress, DiameterAuthPort, []}]},
		{acct, [{DiameterAddress, DiameterAcctPort, []}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = ocs_test_lib:start(),
	Realm = ct:get_config({diameter, realm}, "mnc001.mcc001.3gppnetwork.org"),
	Host = ct:get_config({diameter, host}, atom_to_list(?MODULE) ++ "." ++ Realm),
	Config1 = [{host, Host}, {realm, Realm},
			{radius_auth_address, RadiusAddress},
			{radius_auth_port, RadiusAuthPort},
			{radius_acct_address, RadiusAddress},
			{radius_acct_port, RadiusAcctPort},
			{diameter_acct_address, DiameterAddress} | Config],
	ok = diameter:start_service(?MODULE, client_service_opts(Config1)),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE, DiameterAddress, DiameterAuthPort, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Info}
				when element(1, Info) == up ->
			Config1;
		_ ->
			{skip, diameter_client_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = diameter:remove_transport(?MODULE, true),
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_diameter; TestCase == bad_password_diameter;
		TestCase == unknown_username_diameter; TestCase == out_of_credit_diameter;
		TestCase == session_termination_diameter ->
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, _, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	Config;
init_per_testcase(_TestCase, Config) ->
	NasId = atom_to_list(node()),
	CalledStationId = "E4-8D-8C-D6-E0-AC:TestSSID",
	Config1 = [{nas_id, NasId}, {called_id, CalledStationId} | Config],
	SharedSecret = ct:get_config({radius, secret}),
	{ok, _} = ocs:add_client({127, 0, 0, 1}, 3799, radius, SharedSecret, true),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, _, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, Address}, binary]),
	[{socket, Socket} | Config1].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_diameter; TestCase == bad_password_diameter;
		TestCase == unknown_username_diameter; TestCase == out_of_credit_diameter;
		TestCase == session_termination_diameter ->
	Client = ?config(diameter_acct_address, Config),
	ok = ocs:delete_client(Client);
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	ok = 	gen_udp:close(Socket).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[simple_authentication_radius, simple_auth_radius_chap, out_of_credit_radius,
	bad_password_radius, unknown_username_radius, simple_authentication_diameter,
	bad_password_diameter, unknown_username_diameter, out_of_credit_diameter,
	session_termination_diameter, authenticate_voice_call].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

simple_authentication_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessAccept to the peer"}]}].

simple_authentication_radius(Config) ->
	Id = 1,
	NasId = ?config(nas_id, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerID,
			password = PeerPassword} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	CalledStationId = ?config(called_id, Config),
	MAC = "DD:EE:DD:EE:BB:AA",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(PeerID), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e0", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = Id,
			authenticator = Authenticator, attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id} = radius:codec(AccessAcceptPacket).

simple_authentication_diameter() ->
	[{userdata, [{doc, "Successful simple authentication using DIAMETER NAS application"}]}].

simple_auth_radius_chap() ->
	[{userdata, [{doc, "RADIUS Access-Request with CHAP"}]}].

simple_auth_radius_chap(Config) ->
	Id = 1,
	NasId = ?config(nas_id, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerID,
			password = PeerPassword} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	CalledStationId = ?config(called_id, Config),
	MAC = "DE:FE1:DE:EE:BE:AE",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	ChapId = 42,
	ChapPassword = crypto:hash(md5, [ChapId, PeerPassword, Authenticator]),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(PeerID), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e0", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?ChapPassword, {ChapId, ChapPassword}, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = Id,
			authenticator = Authenticator, attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id} = radius:codec(AccessAcceptPacket).

simple_authentication_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username,
			password = Password} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?MODULE, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY'} = Answer.

out_of_credit_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessReject response to the peer when balance
			less than 0"}]}].

out_of_credit_radius(Config) ->
	Id = 2,
	NasId = ?config(nas_id, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerID,
			password = PeerPassword} =  add_service(ProdRef),
	CalledStationId = ?config(called_id, Config),
	MAC = "DD:EE:DD:EE:CC:BB",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 12, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(PeerID), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e1", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessRejectPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = Id, attributes = AccessRejectData} =
			radius:codec(AccessRejectPacket),
	AccessReject = radius_attributes:codec(AccessRejectData),
	{ok, "Out of Credit"} = radius_attributes:find(?ReplyMessage, AccessReject).

out_of_credit_diameter() ->
	[{userdata, [{doc, "Diameter authentication failure when subscriber has a balance less than 0"}]}].

out_of_credit_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = UserName,
			password = Password} =  add_service(ProdRef),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [UserName], 'User-Password' = [Password],
			'Service-Type' = [11]},
	{ok, Answer} = diameter:call(?MODULE, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY'} = Answer.

bad_password_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessReject response to the peer when password 
			not matched"}]}].

bad_password_radius(Config) ->
	Id = 2,
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerID} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	NasId = ?config(nas_id, Config),
	CalledStationId = ?config(called_id, Config),
	MAC = "DD:EE:DD:EE:DD:CC",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	BoguesPassowrd = radius_attributes:hide(SharedSecret, Authenticator, "bogus"),	
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(PeerID), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e2", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, BoguesPassowrd, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessRejectPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = Id, attributes = AccessRejectData} =
			radius:codec(AccessRejectPacket),
	AccessReject = radius_attributes:codec(AccessRejectData),
	{ok, "Bad Password"} = radius_attributes:find(?ReplyMessage, AccessReject).

bad_password_diameter() ->
	[{userdata, [{doc, "Diameter simple authentication failure wheh a wrong password is used"}]}].

bad_password_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	InvalidPassword = ocs:generate_password(), 
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [InvalidPassword]},
	{ok, Answer} = diameter:call(?MODULE, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY'} = Answer.

unknown_username_radius() ->
	[{userdata, [{doc, "Send RADIUS RAccessReject response to the peer for unknown username"}]}].

unknown_username_radius(Config) ->
	Id = 3,
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{password = PeerPassword} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	NasId = ?config(nas_id, Config),
	CalledStationId = ?config(called_id, Config),
	MAC = "DD:EE:DD:EE:DD:CC",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	BogusUserName = ocs:generate_password(),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, BogusUserName, A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e3", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessRejectPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = Id, attributes = AccessRejectData} =
			radius:codec(AccessRejectPacket),
	AccessReject = radius_attributes:codec(AccessRejectData),
	{ok, "Unknown Username"} = radius_attributes:find(?ReplyMessage, AccessReject).

unknown_username_diameter() ->
	[{userdata, [{doc, "Diameter simple authentication failure wheh a unknown username is used"}]}].

unknown_username_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{password = Password} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	UnknownUsername = ocs:generate_identity(),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [UnknownUsername], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?MODULE, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY'} = Answer.

session_termination_diameter() ->
	[{userdata, [{doc, "Successful simple authentication using DIAMETER NAS application"}]}].

session_termination_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username,
		password = Password} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?MODULE, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY'} = Answer,
	NAS_STR = #diameter_nas_app_STR{'Session-Id' = SId, 'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Termination-Cause' = ?'DIAMETER_NAS_APP_TERMINATION-CAUSE_LOGOUT', 'User-Name' = [Username]},
	{ok, Answer1} = diameter:call(?MODULE, nas_app_test, NAS_STR, []),
	true = is_record(Answer1, diameter_nas_app_STA),
	#diameter_nas_app_STA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1.

authenticate_voice_call() ->
	[{userdata, [{doc, "Successful
		authenticate and authorize voice call"}]}].

authenticate_voice_call(Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, []),
	Id = 1,
	NasId = ?config(nas_id, Config),
	SharedSecret = ct:get_config({radius, secret}),
	{ok, _} = ocs:add_client({127, 0, 0, 1}, 3799, radius, SharedSecret, false),
	CallingStationId = "99771234567",
	CalledStationId = "99771234568",
	PeerPassword = ocs:generate_password(),
	B1 = bucket(cents, 3000),
	_BId = add_bucket(ProdRef, B1),
	RadiusReserveSessionTime = 60,
	Chars = [{"radiusReserveSessionTime", RadiusReserveSessionTime}],
	{ok, _} = ocs:add_service(CallingStationId, PeerPassword, ProdRef, Chars),
	Authenticator = radius:authenticator(),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 12, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, CallingStationId, A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e0", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?NasIdentifier, NasId, A7),
	AccessReqest = #radius{code = ?AccessRequest, id = Id,
			authenticator = Authenticator, attributes = A8},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id,
			attributes = Attributes} = radius:codec(AccessAcceptPacket),
	RadiusAttributes = radius_attributes:codec(Attributes),
	RadiusReserveSessionTime = radius_attributes:fetch(?SessionTimeout, RadiusAttributes),
	{ok, _} = ocs:add_client({127, 0, 0, 1}, 3799, radius, SharedSecret, true).

%%--------------------------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------------------------

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
client_service_opts(Config) ->
	[{'Origin-Host', ?config(host, Config)},
			{'Origin-Realm', ?config(realm, Config)},
			{'Product-Name', "SigScale Test Client (auth)"},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?NAS_APPLICATION_ID]},
			{string_decode, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_client_cb}]},
			{application, [{alias, nas_app_test},
					{dictionary, diameter_gen_nas_application_rfc7155},
					{module, diameter_test_client_cb}]}].

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans}, {transport_config,
			[{raddr, RemAddr}, {rport, RemPort},
			{reuseaddr, true}, {ip, LocalAddr}]}].

%% @hidden
price(Type, Units, Size, Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, units = Units,
			size = Size, amount = Amount}.

%% @hidden
bucket(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
		start_date = erlang:system_time(millisecond),
		end_date = erlang:system_time(millisecond) + 2592000000}.

%% @hidden
add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, Spec) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId.

%% @hidden
add_product(OfferId) ->
	add_product(OfferId, []).
add_product(OfferId, Chars) ->
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, Chars),
	ProdRef.

%% @hidden
add_service(ProdRef) ->
	{ok, Service} =
			ocs:add_service(ocs:generate_identity(), ocs:generate_password(),
			ProdRef, []),
	Service.

%% @hidden
add_bucket(ProdRef, Bucket) ->
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.
