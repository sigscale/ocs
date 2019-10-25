%%% ocs_eap_aka_SUITE.erl
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
%%%  @doc Test suite for authentication using Extensible Authentication
%%% 	Protocol (EAP) using only a password (EAP-PWD)
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_eap_aka_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_eap_application_rfc4072.hrl").
-include_lib("kernel/include/inet.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(EAP_APPLICATION_ID, 5).

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
	[{userdata, [{doc, "Test suite for authentication with EAP-AKA in OCS"}]},
	{timetrap, {seconds, 8}},
	{require, radius_username}, {default_config, radius_username, "ocs"},
	{require, radius_password}, {default_config, radius_password, "ocs123"},
	{require, radius_shared_secret},{default_config, radius_shared_secret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	RadiusPort = rand:uniform(64511) + 1024,
	Options = [{eap_method_prefer, akap}, {eap_method_order, [akap]}],
	RadiusAppVar = [{auth, [{{127,0,0,1}, RadiusPort, Options}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterPort = rand:uniform(64511) + 1024,
	DiameterAppVar = [{auth, [{{127,0,0,1}, DiameterPort, Options}]}],
	ok = application:set_env(ocs, diameter, RadiusAppVar),
	ok = ocs_test_lib:start(),
	{ok, ProdID} = ocs_test_lib:add_offer(),
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	ok = diameter:start_service(?MODULE, client_service_opts()),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE, Address, Port, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Info}
				when element(1, Info) == up ->
			[{product_id, ProdID}, {diameter_client, Address} | Config];
		_ ->
			{skip, diameter_client_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{RadIP, RadPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, RadIP}, binary]),
	SharedSecret = ct:get_config(radius_shared_secret),
	Protocol = radius,
	{ok, _} = ocs:add_client(RadIP, RadPort, Protocol, SharedSecret, true),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}, {socket, Socket}, {radius_client, RadIP} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	RadClient = ?config(radius_client, Config),
	ok = ocs:delete_client(RadClient),
	ok = gen_udp:close(Socket).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[eap_identity_over_radius].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

eap_identity_over_radius() ->
	[{userdata, [{doc, "Send an EAP-Identity/Response using RADIUS to peer"}]}].        

eap_identity_over_radius(Config) ->
	MAC = "AA-BB-CC-DD-EE-FF",
	PeerId = <<"12345678">>,
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth = radius:authenticator(),
	RadId = 1, EapId = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth, EapId, RadId),
	NextEapId = EapId + 1,
	{NextEapId, _Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth, RadId).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

radius_access_request(Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId, EapMsg)
		when is_binary(UserName) ->
	radius_access_request(Socket, Address, Port, NasId,
			binary_to_list(UserName), Secret, MAC, Auth, RadId,
			EapMsg);       
radius_access_request(Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId, EapMsg) ->
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?UserName, UserName, A0), 
	A2 = radius_attributes:add(?NasPort, 0, A1), 
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2), 
	A4 = radius_attributes:add(?CallingStationId, MAC, A3), 
	A5 = radius_attributes:add(?CalledStationId,
			"FE-EF-DE-ED-CE-ED:TestSSID", A4),
	A6 = radius_attributes:add(?EAPMessage, EapMsg, A5), 
	A7 = radius_attributes:add(?MessageAuthenticator, <<0:128>>, A6), 
	Request1 = #radius{code = ?AccessRequest, id = RadId,
      authenticator = Auth, attributes = A7},
   ReqPacket1 = radius:codec(Request1),
   MsgAuth1 = crypto:hmac(md5, Secret, ReqPacket1),
   A8 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A7), 
   Request2 = Request1#radius{attributes = A8}, 
   ReqPacket2 = radius:codec(Request2),
   gen_udp:send(Socket, Address, Port, ReqPacket2).

send_radius_identity(Socket, Address, Port, NasId,
		UserName, Secret, PeerId, MAC, Auth, EapId, RadId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity,
	identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_radius_id(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = radius_access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none, token = Token,
			identity = ServerID} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	{EapId, Token, ServerID}.

%% @hidden
client_service_opts() ->
	OriginHost = ocs:generate_password() ++ "@siscale.org",
	OriginRealm = ocs:generate_password() ++ "@siscale.org",
	[{'Origin-Host', OriginHost},
		{'Origin-Realm', OriginRealm},
		{'Vendor-Id', 10415},
		{'Product-Name', "SigScale Test Client (auth)"},
		{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?EAP_APPLICATION_ID]},
		{string_decode, false},
		{application, [{alias, base_app_test},
				{dictionary, diameter_gen_base_rfc6733},
				{module, diameter_test_client_cb}]},
		{application, [{alias, eap_app_test},
				{dictionary, diameter_gen_eap_application_rfc4072},
				{module, diameter_test_client_cb}]}].

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans}, {transport_config,
			[{raddr, RemAddr}, {rport, RemPort},
			{reuseaddr, true}, {ip, LocalAddr}]}].

radius_access_challenge(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessChallenge, Socket, Address, Port, Secret, RadId, ReqAuth).

radius_access_accept(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessAccept, Socket, Address, Port, Secret, RadId, ReqAuth).

radius_access_reject(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessReject, Socket, Address, Port, Secret, RadId, ReqAuth).

receive_radius(Code, Socket, Address, Port, Secret, RadId, ReqAuth) ->
	{ok, {Address, Port, RespPacket1}} = gen_udp:recv(Socket, 0),
	Resp1 = radius:codec(RespPacket1),
	#radius{code = Code, id = RadId, authenticator = RespAuth,
		attributes = BinRespAttr1} = Resp1,
	Resp2 = Resp1#radius{authenticator = ReqAuth},
	RespPacket2 = radius:codec(Resp2),
	RespAuth = binary_to_list(crypto:hash(md5, [RespPacket2, Secret])),
	RespAttr1 = radius_attributes:codec(BinRespAttr1),
	{ok, MsgAuth} = radius_attributes:find(?MessageAuthenticator, RespAttr1),
	RespAttr2 = radius_attributes:store(?MessageAuthenticator, <<0:128>>, RespAttr1),
	Resp3 = Resp2#radius{attributes = RespAttr2},
	RespPacket3 = radius:codec(Resp3),
	MsgAuth = crypto:hmac(md5, Secret, RespPacket3),
	{ok, EapMsg} = radius_attributes:find(?EAPMessage, RespAttr1),
	EapMsg.

