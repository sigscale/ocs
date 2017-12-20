%%% ocs_simple_auth_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

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

-define(SVC, diameter_client_service).
-define(BASE_APPLICATION_ID, 0).
-define(NAS_APPLICATION_ID, 1).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for authentication in OCS"}]},
	{timetrap, {minutes, 1}},
	{require, radius_username}, {default_config, radius_username, "ocs"},
	{require, radius_password}, {default_config, radius_password, "ocs123"},
	{require, radius_shared_secret},{default_config, radius_shared_secret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	{ok, ProdID} = ocs_test_lib:add_product(),
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	true = diameter:subscribe(?SVC),
	ok = diameter:start_service(?SVC, client_service_opts()),
	{ok, _Ref} = connect(?SVC, Address, Port, diameter_tcp),
	receive
		#diameter_event{service = ?SVC, info = start} ->
			[{product_id, ProdID}, {diameter_client, Address}] ++ Config;
		_ ->
			{skip, diameter_client_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop_service(?SVC),
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(TestCase, Config) when
		TestCase == simple_authentication_diameter; TestCase == bad_password_diameter;
		TestCase == unknown_username_diameter; TestCase == out_of_credit_diameter;
		TestCase == session_termination_diameter ->
	UserName = "Wentworth",
	Password = "53cr37",
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, _, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	ProdID = ?config(product_id, Config),
	Buckets = [#bucket{units = cents, remain_amount = 3000}],
	{ok, _} = ocs:add_subscriber(UserName, Password, ProdID, [], Buckets, []),
	[{username, UserName}, {password, Password}] ++ Config;
init_per_testcase(_TestCase, Config) ->
	NasId = atom_to_list(node()),
	CalledStationId = "E4-8D-8C-D6-E0-AC:TestSSID",
	Config1 = [{nas_id, NasId}, {called_id, CalledStationId} | Config],
	SharedSecret = ct:get_config(radius_shared_secret),
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
	UserName= ?config(username, Config),
	Client = ?config(diameter_client, Config),
	ok = ocs:delete_client(Client),
	ok = ocs:delete_subscriber(UserName);
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
	[simple_authentication_radius, out_of_credit_radius, bad_password_radius,
	unknown_username_radius, simple_authentication_diameter, bad_password_diameter, 
	unknown_username_diameter, out_of_credit_diameter, session_termination_diameter].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

simple_authentication_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessAccept to the peer"}]}].

simple_authentication_radius(Config) ->
	Id = 1,
	NasId = ?config(nas_id, Config),
	ProdID = ?config(product_id, Config),
	CalledStationId = ?config(called_id, Config),
	MAC = "DD:EE:DD:EE:BB:AA",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	PeerID = string:to_lower(lists:append(MACtokens)),
	PeerPassword = ocs:generate_password(),
	Buckets = [#bucket{units = cents, remain_amount = 3000}],
	{ok, _} = ocs:add_subscriber(PeerID, PeerPassword, ProdID, [], Buckets, []),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config(radius_shared_secret),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, PeerID, A3),
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

simple_authentication_diameter(Config) ->
	Username = ?config(username, Config),
	Password = ?config(password, Config),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?SVC, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	{ok, HostName} = inet:gethostname(),
	{ok, #hostent{h_name = Realm}} = inet:gethostbyname(HostName),
	OriginHost = list_to_binary(HostName),
	OriginRealm = list_to_binary(Realm),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer.

out_of_credit_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessReject response to the peer when balance
			less than 0"}]}].

out_of_credit_radius(Config) ->
	Id = 2,
	NasId = ?config(nas_id, Config),
	ProdID = ?config(product_id, Config),
	CalledStationId = ?config(called_id, Config),
	MAC = "DD:EE:DD:EE:CC:BB",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	PeerID = string:to_lower(lists:append(MACtokens)),
	PeerPassword = ocs:generate_password(),
	Buckets = [#bucket{units = cents, remain_amount = 2290}], % subscription total price
	{ok, _} = ocs:add_subscriber(PeerID, PeerPassword, ProdID, [], Buckets, []),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config(radius_shared_secret),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, PeerID, A3),
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

out_of_credit_diameter(Config) ->
	UserName = "Axl Rose",
	Password = "Guns&Roses",
	ProdID = ?config(product_id, Config),
	Buckets = [#bucket{units = cents, remain_amount = 2290}], % subscription total price
	{ok, _} = ocs:add_subscriber(UserName, Password, ProdID, [], Buckets, []),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [UserName], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?SVC, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	{ok, HostName} = inet:gethostname(),
	{ok, #hostent{h_name = Realm}} = inet:gethostbyname(HostName),
	OriginHost = list_to_binary(HostName),
	OriginRealm = list_to_binary(Realm),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer.

bad_password_radius() ->
	[{userdata, [{doc, "Send RADIUS AccessReject response to the peer when password 
			not matched"}]}].

bad_password_radius(Config) ->
	Id = 2,
	NasId = ?config(nas_id, Config),
	CalledStationId = ?config(called_id, Config),
	ProdID = ?config(product_id, Config),
	MAC = "DD:EE:DD:EE:DD:CC",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	PeerID = string:to_lower(lists:append(MACtokens)),
	PeerPassword = ocs:generate_password(),
	Buckets = [#bucket{units = cents, remain_amount = 3000}],
	{ok, _} = ocs:add_subscriber(PeerID, PeerPassword, ProdID, [], Buckets, []),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config(radius_shared_secret),
	BoguesPassowrd = radius_attributes:hide(SharedSecret, Authenticator, "bogus"),	
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{AuthAddress, AuthPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?ServiceType, 2, A0),
	A2 = radius_attributes:add(?NasPortId, "wlan1", A1),
	A3 = radius_attributes:add(?NasPortType, 19, A2),
	A4 = radius_attributes:add(?UserName, PeerID, A3),
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

bad_password_diameter(Config) ->
	Username = ?config(username, Config),
	InvalidPassword = "starGazer987",
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [InvalidPassword]},
	{ok, Answer} = diameter:call(?SVC, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	{ok, HostName} = inet:gethostname(),
	{ok, #hostent{h_name = Realm}} = inet:gethostbyname(HostName),
	OriginHost = list_to_binary(HostName),
	OriginRealm = list_to_binary(Realm),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
	'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer.


unknown_username_radius() ->
	[{userdata, [{doc, "Send RADIUS RAccessReject response to the peer for unknown username"}]}].

unknown_username_radius(Config) ->
	Id = 3,
	NasId = ?config(nas_id, Config),
	CalledStationId = ?config(called_id, Config),
	ProdID = ?config(product_id, Config),
	MAC = "DD:EE:DD:EE:DD:CC",
	MACtokens = string:tokens(MAC, ":"),
	CallingStationId = string:join(MACtokens, "-"),
	PeerID = string:to_lower(lists:append(MACtokens)),
	PeerPassword = ocs:generate_password(),
	Buckets = [#bucket{units = cents, remain_amount = 3000}],
	{ok, _} = ocs:add_subscriber(PeerID, PeerPassword, ProdID, [], Buckets, []),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config(radius_shared_secret),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	BogusUserName = "tormentor",
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

unknown_username_diameter(Config) ->
	UnknownUsername = "PeterGriffin",
	Password = ?config(password, Config),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [UnknownUsername], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?SVC, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	{ok, HostName} = inet:gethostname(),
	{ok, #hostent{h_name = Realm}} = inet:gethostbyname(HostName),
	OriginHost = list_to_binary(HostName),
	OriginRealm = list_to_binary(Realm),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
	'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer.

session_termination_diameter() ->
	[{userdata, [{doc, "Successful simple authentication using DIAMETER NAS application"}]}].

session_termination_diameter(Config) ->
	Username = ?config(username, Config),
	Password = ?config(password, Config),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = [Username], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?SVC, nas_app_test, NAS_AAR, []),
	true = is_record(Answer, diameter_nas_app_AAA),
	{ok, HostName} = inet:gethostname(),
	{ok, #hostent{h_name = Realm}} = inet:gethostbyname(HostName),
	OriginHost = list_to_binary(HostName),
	OriginRealm = list_to_binary(Realm),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer,
	NAS_STR = #diameter_nas_app_STR{'Session-Id' = SId, 'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Termination-Cause' = ?'DIAMETER_NAS_APP_TERMINATION-CAUSE_LOGOUT', 'User-Name' = [Username]},
	{ok, Answer1} = diameter:call(?SVC, nas_app_test, NAS_STR, []),
	true = is_record(Answer1, diameter_nas_app_STA),
	{ok, HostName} = inet:gethostname(),
	{ok, #hostent{h_name = Realm}} = inet:gethostbyname(HostName),
	OriginHost = list_to_binary(HostName),
	OriginRealm = list_to_binary(Realm),
	#diameter_nas_app_STA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer1.

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
client_service_opts() ->
	OriginHost = ocs:generate_password() ++ "@siscale.org",
	OriginRealm = ocs:generate_password() ++ "@siscale.org",
	[{'Origin-Host', OriginHost},
		{'Origin-Realm', OriginRealm},
		{'Vendor-Id', 10415},
		{'Product-Name', "SigScale Test Client (auth)"},
		{'Auth-Application-Id', [?BASE_APPLICATION_ID,
														 ?NAS_APPLICATION_ID]},
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
	[{transport_module, Trans},
		{transport_config, [{raddr, RemAddr},
		{rport, RemPort},
		{reuseaddr, true}
		| [{ip, LocalAddr}]]}].

