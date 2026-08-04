%%% ocs_simple_auth_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2026 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-behaviour(ct_suite).

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
	DefaultAddress = {127,0,0,1},
	DefaultRealm = "mnc001.mcc001.3gppnetwork.org",
	DefaultHost = atom_to_list(?MODULE) ++ "." ++ DefaultRealm,
	[{userdata, [{doc, "Test suite for authentication in OCS"}]},
	{timetrap, {seconds, 5}},
	{require, radius},
	{default_config, radius,
			[{address, DefaultAddress},
			{client_address, DefaultAddress},
			{secret, "abc456"}]},
	{require, diameter},
	{default_config, diameter,
			{realm, DefaultRealm},
			{host, DefaultHost},
			[{address, DefaultAddress},
			{client_address, DefaultAddress}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the entire suite.
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:load(ocs),
	RadiusAddress = ct:get_config({radius, address}, {127,0,0,1}),
	RadiusAuthPort = ct:get_config({radius, auth_port}, rand:uniform(64511) + 1024),
	RadiusClientAddress = ct:get_config({radius, client_address}, {127,0,0,1}),
	RadiusSecret = ct:get_config({radius, secret}, "abc456"),
	RadiusAppVar = [{auth, [{RadiusAddress, RadiusAuthPort, []}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	Realm = ct:get_config({diameter, realm}, "mnc001.mcc001.3gppnetwork.org"),
	Host = ct:get_config({diameter, host}, atom_to_list(?MODULE) ++ "." ++ Realm),
	DiameterAddress = ct:get_config({diameter, address}, {127,0,0,1}),
	DiameterAuthPort = ct:get_config({diameter, auth_port}, rand:uniform(64511) + 1024),
	DiameterClientAddress = ct:get_config({diameter, client_address}, {127,0,0,1}),
	DiameterAppVar = [{auth, [{DiameterAddress, DiameterAuthPort, []}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = application:set_env(ocs, min_reserve_octets, 1000000),
	ok = application:set_env(ocs, min_reserve_seconds, 60),
	ok = application:set_env(ocs, min_reserve_messages, 1),
	ok = ocs_test_lib:start(),
	Config1 = [{host, Host}, {realm, Realm},
			{nas_id, atom_to_list(node())},
			{called_id, "E4-8D-8C-D6-E0-AC:TestSSID"},
			{radius_secret, RadiusSecret},
			{radius_address, RadiusAddress},
			{radius_auth_port, RadiusAuthPort},
			{radius_address, RadiusAddress},
			{radius_client_address, RadiusClientAddress},
			{diameter_address, DiameterAddress},
			{diameter_auth_port, DiameterAuthPort},
			{diameter_client_address, DiameterClientAddress}| Config],
	ServiceName = ?MODULE,
	ok = diameter:start_service(ServiceName,
			client_service_opts(Config1)),
	{ok, _} = ocs:add_client(DiameterClientAddress,
			undefined, diameter, undefined, true),
	true = diameter:subscribe(ServiceName),
	{ok, _} = connect(ServiceName,
			DiameterAddress, DiameterAuthPort, diameter_tcp),
	receive
		#diameter_event{service = ServiceName, info = Info}
				when element(1, Info) == up ->
			Config1;
		_Other ->
			{skip, diameter_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = diameter:remove_transport(?MODULE, true),
	ok = ocs_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_radius;
		TestCase == simple_auth_radius_chap;
		TestCase == out_of_credit_radius;
		TestCase == bad_password_radius;
		TestCase == unknown_username_radius;
		TestCase == authenticate_voice;
		TestCase == auth_data_fail ->
	SharedSecret = proplists:get_value(radius_secret, Config),
	ClientAddress = proplists:get_value(radius_client_address, Config),
	{ok, _} = ocs:add_client(ClientAddress,
			3799, radius, SharedSecret, true),
	{ok, Socket} = gen_udp:open(0,
			[{active, false}, inet, {ip, ClientAddress}, binary]),
	lists:keystore(socket, 1, Config, {socket, Socket});
init_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_diameter;
		TestCase == bad_password_diameter;
		TestCase == unknown_username_diameter;
		TestCase == out_of_credit_diameter;
		TestCase == session_termination_diameter;
		TestCase == client_authorized ->
	ClientAddress = proplists:get_value(diameter_client_address, Config),
	{ok, _} = ocs:add_client(ClientAddress,
			undefined, diameter, undefined, true),
	Config;
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when
		TestCase == client_authorized;
		TestCase == client_not_authorized->
	ClientAddress = proplists:get_value(diameter_client_address, Config),
	ServiceName = lists:concat([?MODULE, $:, TestCase]),
	ok = diameter:stop_service(ServiceName),
	ok = diameter:remove_transport(ServiceName, true),
	ok = ocs:delete_client(ClientAddress),
	Config;
end_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_radius;
		TestCase == simple_auth_radius_chap;
		TestCase == out_of_credit_radius;
		TestCase == bad_password_radius;
		TestCase == unknown_username_radius;
		TestCase == authenticate_voice;
		TestCase == auth_data_fail ->
	ClientAddress = proplists:get_value(radius_client_address, Config),
	ok = ocs:delete_client(ClientAddress),
	Socket = proplists:get_value(socket, Config),
	ok = gen_udp:close(Socket);
end_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_diameter;
		TestCase == bad_password_diameter;
		TestCase == unknown_username_diameter;
		TestCase == out_of_credit_diameter;
		TestCase == session_termination_diameter ->
	ClientAddress = proplists:get_value(diameter_client_address, Config),
	ok = ocs:delete_client(ClientAddress),
	Config;
end_per_testcase(_TestCase, Config) ->
	Config.

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
	session_termination_diameter, authenticate_voice, auth_data_fail,
	client_authorized, client_not_authorized].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

simple_authentication_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessAccept to the peer"}]}].

simple_authentication_radius(Config) ->
	RadId = 1,
	NasId = proplists:get_value(nas_id, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = UserName,
			password = PeerPassword} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	CalledStationId = proplists:get_value(called_id, Config),
	MAC = "DD:EE:DD:EE:BB:AA",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(UserName), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e0", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = RadId,
			authenticator = Authenticator, attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = RadId} = radius:codec(AccessAcceptPacket).

simple_auth_radius_chap() ->
	[{userdata, [{doc, "RADIUS Access-Request with CHAP"}]}].

simple_auth_radius_chap(Config) ->
	RadId = 2,
	NasId = proplists:get_value(nas_id, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = UserName,
			password = PeerPassword} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	CalledStationId = proplists:get_value(called_id, Config),
	MAC = "DE:FE1:DE:EE:BE:AE",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	ChapId = 42,
	ChapPassword = crypto:hash(md5, [ChapId, PeerPassword, Authenticator]),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(UserName), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e0", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?ChapPassword, {ChapId, ChapPassword}, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = RadId,
			authenticator = Authenticator, attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = RadId} = radius:codec(AccessAcceptPacket).

simple_authentication_diameter() ->
	[{userdata, [{doc, "Successful simple authentication using DIAMETER NAS application"}]}].

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
	RadId = 3,
	NasId = proplists:get_value(nas_id, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = UserName,
			password = PeerPassword} =  add_service(ProdRef),
	CalledStationId = proplists:get_value(called_id, Config),
	MAC = "DD:EE:DD:EE:CC:BB",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 12, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(UserName), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e1", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = RadId, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessRejectPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = RadId, attributes = AccessRejectData} =
			radius:codec(AccessRejectPacket),
	AccessReject = radius_attributes:codec(AccessRejectData),
	{ok, "Out of Credit"} = radius_attributes:find(?ReplyMessage, AccessReject).

out_of_credit_diameter() ->
	[{userdata, [{doc, "Diameter authentication failure when subscriber has zero balance"}]}].

out_of_credit_diameter(Config) ->
	Host = proplists:get_value(host, Config),
	Realm = proplists:get_value(realm, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username, password = Password} = add_service(ProdRef),
	SId = diameter:session_id(Host),
	AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Origin-Host' = Host, 'Origin-Realm' = Realm,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'User-Name' = [Username], 'User-Password' = [Password]},
	{ok, AAA} = diameter:call(?MODULE, nas_app_test, AAR, []),
	#diameter_nas_app_AAA{'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'} = AAA.

bad_password_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessReject response to the peer when password not matched"}]}].

bad_password_radius(Config) ->
	RadId = 4,
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = UserName} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	NasId = proplists:get_value(nas_id, Config),
	CalledStationId = proplists:get_value(called_id, Config),
	MAC = "DD:EE:DD:EE:DD:CC",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	BoguesPassowrd = radius_attributes:hide(SharedSecret, Authenticator, "bogus"),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(UserName), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e2", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, BoguesPassowrd, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = RadId, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessRejectPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = RadId, attributes = AccessRejectData} =
			radius:codec(AccessRejectPacket),
	AccessReject = radius_attributes:codec(AccessRejectData),
	{ok, "Bad Password"} = radius_attributes:find(?ReplyMessage, AccessReject).

bad_password_diameter() ->
	[{userdata, [{doc, "Diameter simple authentication failure wheh a wrong password is used"}]}].

bad_password_diameter(Config) ->
	Host = proplists:get_value(host, Config),
	Realm = proplists:get_value(realm, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username} = add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	InvalidPassword = ocs:generate_password(),
	SId1 = diameter:session_id(Host),
	AAR1 = #diameter_nas_app_AAR{'Session-Id' = SId1,
			'Origin-Host' = Host, 'Origin-Realm' = Realm,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [InvalidPassword]},
	{ok, AAA1} = diameter:call(?MODULE, nas_app_test, AAR1, []),
	#diameter_nas_app_AAA{'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED'} = AAA1,
	SId2 = diameter:session_id(Host),
	AAR2 = #diameter_nas_app_AAR{'Session-Id' = SId2,
			'Origin-Host' = Host, 'Origin-Realm' = Realm,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'User-Name' = [Username], 'User-Password' = [InvalidPassword]},
	{ok, AAA2} = diameter:call(?MODULE, nas_app_test, AAR2, []),
	#diameter_nas_app_AAA{'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED'} = AAA2.

unknown_username_radius() ->
	[{userdata, [{doc, "Send RADIUS RAccessReject response to the peer for unknown username"}]}].

unknown_username_radius(Config) ->
	RadId = 5,
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{password = PeerPassword} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	NasId = proplists:get_value(nas_id, Config),
	CalledStationId = proplists:get_value(called_id, Config),
	MAC = "DD:EE:DD:EE:DD:CC",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),
	BogusUserName = ocs:generate_password(),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
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
	AccessReqest = #radius{code = ?AccessRequest, id = RadId, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessRejectPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = RadId, attributes = AccessRejectData} =
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

authenticate_voice() ->
	[{userdata, [{doc, "Successful authenticate and authorize voice call"}]}].

authenticate_voice(Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	RadiusReserveSessionTime = 60,
	CharValue = #char_value{units = "seconds", value = RadiusReserveSessionTime},
	Chars = [#char_value_use{name = "radiusReserveSessionTime",
			values = [CharValue]}],
	OfferId = add_offer([P1], "9", Chars),
	ProdRef = add_product(OfferId, []),
	#service{name = UserName, password = PeerPassword} =  add_service(ProdRef),
	RadId = 6,
	NasId = proplists:get_value(nas_id, Config),
	CallingStationId = "99771234567",
	CalledStationId = "99771234568",
	B1 = bucket(cents, 3000),
	_BId = add_bucket(ProdRef, B1),
	RadiusReserveSessionTime = 60,
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 12, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(UserName), A3),
	A5 = radius_attributes:add(?AcctSessionId, "826005e0", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = RadId,
			authenticator = Authenticator, attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = RadId,
			attributes = Attributes} = radius:codec(AccessAcceptPacket),
	RadiusAttributes = radius_attributes:codec(Attributes),
	RadiusReserveSessionTime = radius_attributes:fetch(?SessionTimeout, RadiusAttributes).

auth_data_fail() ->
	[{userdata, [{doc, "Successful authenticate and authorize data"}]}].

auth_data_fail(Config) ->
	PackagePrice = 1,
	PackageSize = 1000000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId, []),
	#service{name = UserName, password = PeerPassword} =  add_service(ProdRef),
	RadId = 7,
	NasId = proplists:get_value(nas_id, Config),
	CallingStationId = "99771234567",
	CalledStationId = "99771234568",
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config({radius, secret}),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = proplists:get_value(socket, Config),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, binary_to_list(UserName), A3),
	A5 = radius_attributes:add(?AcctSessionId, "92641849", A4),
	A6 = radius_attributes:add(?CallingStationId, CallingStationId, A5),
	A7 = radius_attributes:add(?CalledStationId, CalledStationId, A6),
	A8 = radius_attributes:add(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:add(?NasIdentifier, NasId, A8),
	AccessReqest = #radius{code = ?AccessRequest, id = RadId,
			authenticator = Authenticator, attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessReject, id = RadId} = radius:codec(AccessAcceptPacket).

client_authorized() ->
	[{userdata, [{doc, "Authorize a Diameter Peer"}]}].

client_authorized(Config) ->
	ServiceName = lists:concat([?MODULE, $:, ?FUNCTION_NAME]),
	AuthAddress = proplists:get_value(diameter_address, Config),
	AuthPort = proplists:get_value(diameter_auth_port, Config),
	ClientAddress = proplists:get_value(diameter_client_address, Config),
	Realm = atom_to_list(?MODULE),
	Host = lists:concat([?FUNCTION_NAME, $., Realm]),
	ok = diameter:start_service(ServiceName,
			client_service_opts(Host, Realm)),
	true = diameter:subscribe(ServiceName),
	{ok, _} = ocs:add_client(ClientAddress,
			undefined, diameter, undefined, true),
	{ok, _Ref} = connect(ServiceName,
			AuthAddress, AuthPort, diameter_tcp),
	receive
		#diameter_event{service = ServiceName, info = Info}
				when element(1, Info) == up ->
			client_authorized(ServiceName, Config);
		 _Other ->
			{fail, diameter_service_not_started}
	end.
client_authorized(ServiceName, _Config) ->
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
	{ok, Answer} = diameter:call(ServiceName, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY'} = Answer.

client_not_authorized() ->
	[{userdata, [{doc, "Deny Service To An Unknown Diameter Peer"}]}].

client_not_authorized(Config) ->
	ServiceName = lists:concat([?MODULE, $:, ?FUNCTION_NAME]),
	AuthAddress = proplists:get_value(diameter_address, Config),
	AuthPort = proplists:get_value(diameter_auth_port, Config),
	ClientAddress = proplists:get_value(diameter_client_address, Config),
	Realm = atom_to_list(?MODULE),
	Host = lists:concat([?FUNCTION_NAME, $., Realm]),
	ok = ocs:delete_client(ClientAddress),
	ok = diameter:start_service(ServiceName,
			client_service_opts(Host, Realm)),
	true = diameter:subscribe(ServiceName),
	{ok, _Ref} = connect(ServiceName,
			AuthAddress, AuthPort, ClientAddress, diameter_tcp),
	receive
		#diameter_event{service = ServiceName, info = Info}
				when element(1, Info) == closed ->
			ok;
		 _Other ->
			{fail, diameter_service_not_started}
	end.

%%--------------------------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------------------------

client_service_opts(Config) ->
	Host = proplists:get_value(host, Config),
	Realm = proplists:get_value(realm, Config),
	client_service_opts(Host, Realm).

client_service_opts(Host, Realm) ->
	[{'Origin-Host', Host}, {'Origin-Realm', Realm},
			{'Product-Name', "SigScale Test Client (acct)"},
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

connect(SvcName, Address, Port, Transport)
		when is_atom(Transport) ->
	TransportOpts = transport_opts(Address, Port, Transport),
	connect(SvcName, [{connect_timer, 30000} | TransportOpts]).
connect(SvcName, RemAddress, Port, LocalIp, Transport)
		when is_atom(Transport) ->
	TransportOpts = transport_opts(RemAddress, Port, LocalIp, Transport),
	connect(SvcName, [{connect_timer, 30000} | TransportOpts]).

connect(SvcName, Opts) ->
	diameter:add_transport(SvcName, {connect, Opts}).

transport_opts(Address, Port, Module) when is_atom(Module) ->
	transport_opts1(Module, Address, Address, Port).
transport_opts(Address, Port, LocalIp, Module) when is_atom(Module) ->
	transport_opts1(Module, LocalIp, Address, Port).
transport_opts1(Module, LocalAddr, RemAddr, Port) ->
	Config = [{raddr, RemAddr}, {rport, Port},
			{ip, LocalAddr}, {reuseaddr, true}],
	[{transport_module, Module}, {transport_config, Config}].

%% @hidden
price(Type, Units, Size, Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, units = Units,
			size = Size, amount = Amount}.

%% @hidden
bucket(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
			attributes = #{bucket_type => normal},
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000}.

%% @hidden
add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, "8" = Spec) ->
	Values = [#char_value{value = 1000000}],
	ValueUse = [#char_value_use{name = "radiusReserveSessionOctets", values = Values}],
	Offer = #offer{name = ocs:generate_identity(),
			char_value_use = ValueUse,
			price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId;
add_offer(Prices, "9" = Spec) ->
	Values = [#char_value{value = 60}],
	ValueUse = [#char_value_use{name = "radiusReserveSessionTime", values = Values}],
	Offer = #offer{name = ocs:generate_identity(),
			char_value_use = ValueUse,
			price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId;
add_offer(Prices, Spec) ->
	add_offer(Prices, Spec, []).
%% @hidden
add_offer(Prices, Spec, Chars) ->
	Offer = #offer{name = ocs:generate_identity(),
			price = Prices, specification = Spec,
			char_value_use = Chars},
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

