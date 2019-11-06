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
%%% 	Protocol (EAP) using only a password (EAP-AKA)
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
	{require, mcc}, {default_config, mcc, "413"},
	{require, mnc}, {default_config, mnc, "726"},
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
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = ocs_test_lib:start(),
	{ok, ProdID} = ocs_test_lib:add_offer(),
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	ok = diameter:start_service(?MODULE, client_service_opts(Config)),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE, Address, Port, diameter_tcp),
	Realm = "wlan.mnc" ++ ct:get_config(mnc) ++ ".mcc"
			++ ct:get_config(mcc) ++ ".3gppnetwork.org",
	Config1 = [{realm, Realm}, {product_id, ProdID},
		{diameter_client, Address} | Config],
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
	[eap_identity_radius, eap_identity_diameter].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

eap_identity_radius() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using RADIUS"}]}].

eap_identity_radius(Config) ->
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	ReqAuth = radius:authenticator(),
	RadId = 1, EapId = 1,
	Secret = ct:get_config(radius_shared_secret),
	Realm = ?config(realm, Config),
	MSIN = msin(),
	PeerId = "6" + ct:get_config(mcc) ++ ct:get_config(mcc)
			++ MSIN ++ "@wlan." ++ Realm,
	ok = send_radius_identity(Socket, Address, Port, NasId,
			PeerId, Secret, ReqAuth, EapId, RadId),
	NextEapId = EapId + 1,
	{NextEapId, _Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth, RadId).

eap_identity_diameter() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using DIAMETER"}]}].

eap_identity_diameter(Config) ->
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId = 1,
	Realm = ?config(realm, Config),
	MSIN = msin(),
	PeerId = "6" + ct:get_config(mcc) ++ ct:get_config(mcc)
			++ MSIN ++ "@wlan." ++ Realm,
	DEA = send_diameter_identity(SId, EapId, PeerId),
	SIdbin = list_to_binary(SId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH'} = DEA.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @hidden
client_service_opts(Config) ->
	[{'Origin-Realm', ?config(realm, Config)},
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

%% @hidden
radius_access_request(Socket, Address, Port, NasId,
		UserName, Secret, Auth, RadId, EapMsg)
		when is_binary(UserName) ->
	radius_access_request(Socket, Address, Port, NasId,
			binary_to_list(UserName), Secret, Auth, RadId, EapMsg);
radius_access_request(Socket, Address, Port, NasId,
		UserName, Secret, Auth, RadId, EapMsg) ->
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?UserName, UserName, A0),
	A2 = radius_attributes:add(?NasPortType, 19, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2),
	A4 = radius_attributes:add(?CallingStationId, mac(), A3),
	A5 = radius_attributes:add(?CalledStationId, mac(), A4),
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

%% @hidden
send_radius_identity(Socket, Address, Port, NasId,
		PeerId, Secret, Auth, EapId, RadId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	radius_access_request(Socket, Address, Port, NasId,
			PeerId, Secret, Auth, RadId, EapMsg).

%% @hidden
send_diameter_identity(SId, EapId, PeerId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'EAP-Payload' = EapMsg},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

%% @hidden
mac() ->
	mac([]).
%% @hidden
mac(Acc) when length(Acc) =:= 12 ->
	list_to_binary(Acc);
mac(Acc) ->
	mac([integer_to_list(rand:uniform(255), 16) | Acc]).

-spec msin(Length) -> string()
	when
		Length :: pos_integer().
%% @doc Generate a random mobile subscription identification number (MSIN).
%% @private
msin() ->
	msin([]).
%% @hidden
msin(Acc) when length(Acc) =:= 10 ->
	Acc;
msin(Acc) ->
	msin([rand:uniform(10) + 47 | Acc]).

