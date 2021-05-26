%%% ocs_eap_aka_SUITE.erl
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
%%%  @doc Test suite for authentication using Extensible Authentication
%%% 	Protocol (EAP) using only a password (EAP-AKA)
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_eap_aka_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

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
-include("../include/diameter_gen_eap_application_rfc4072.hrl").
-include("../include/diameter_gen_3gpp_swm_application.hrl").
-include_lib("kernel/include/inet.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(EAP_APPLICATION_ID, 5).
-define(SWm_APPLICATION_ID, 16777264).

-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

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
	{require, mcc}, {default_config, mcc, "001"},
	{require, mnc}, {default_config, mnc, "001"},
	{require, radius},
	{default_config, radius, [{username, "ocs"},
			{password, "ocs123"}, {secret, "xyzzy5461"}]},
	{require, diameter},
	{default_config, diameter, [{address, {127,0,0,1}}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:load(ocs),
	RadiusAddress = ct:get_config({radius, address}, {127,0,0,1}),
	RadiusAuthPort = ct:get_config({radius, auth_port}, rand:uniform(64511) + 1024),
	Options = [{eap_method_prefer, aka}, {eap_method_order, [aka, pwd, ttls]}],
	RadiusAppVar = [{auth, [{RadiusAddress, RadiusAuthPort, Options}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterAddress = ct:get_config({diameter, address}, {127,0,0,1}),
	DiameterAuthPort = ct:get_config({diameter, auth_port}, rand:uniform(64511) + 1024),
	DiameterAppVar = [{auth, [{DiameterAddress, DiameterAuthPort, Options}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = ocs_test_lib:start(),
	Realm = ct:get_config({diameter, realm}, "mnc001.mcc001.3gppnetwork.org"),
	Host = ct:get_config({diameter, host}, atom_to_list(?MODULE) ++ "." ++ Realm),
	Config1 = [{host, Host}, {realm, Realm},
		{diameter_client, DiameterAddress} | Config],
	ok = diameter:start_service(?MODULE, client_service_opts(Config1)),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE, DiameterAddress, DiameterAuthPort, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Info}
				when element(1, Info) == up ->
			Config1;
		_Other ->
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
%% Initialization before each test case.
%%
init_per_testcase(prf, Config) ->
	Config;
init_per_testcase(identity_radius, Config) ->
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{RadIP, _, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, RadIP}, binary]),
	SharedSecret = ct:get_config({radius, secret}),
	Protocol = radius,
	{ok, _} = ocs:add_client(RadIP, undefined, Protocol, SharedSecret, true, false),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}, {socket, Socket}, {radius_client, RadIP} | Config];
init_per_testcase(TestCase, Config)
		when TestCase == identity_diameter_eap;
		TestCase == identity_diameter_swm ->
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, _, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true, false),
	[{diameter_client, Address} | Config];
init_per_testcase(TestCase, Config)
		when TestCase == identity_diameter_swm_trusted ->
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, _, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true, true),
	[{diameter_client, Address} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(prf, _Config) ->
	ok;
end_per_testcase(identity_radius, Config) ->
	Socket = ?config(socket, Config),
	RadClient = ?config(radius_client, Config),
	ok = ocs:delete_client(RadClient),
	ok = gen_udp:close(Socket);
end_per_testcase(TestCase, Config)
		when TestCase == identity_diameter_eap;
		TestCase == identity_diameter_swm;
		TestCase == identity_diameter_swm_trusted ->
	DiameterClient = ?config(diameter_client, Config),
	ok = ocs:delete_client(DiameterClient).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[prf, identity_radius, identity_diameter_eap, identity_diameter_swm,
			identity_diameter_swm_trusted].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

identity_radius() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using RADIUS"}]}].

identity_radius(Config) ->
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	ReqAuth = radius:authenticator(),
	RadId = 1, EapId = 1,
	Secret = ct:get_config({radius, secret}),
	Realm = ?config(realm, Config),
	MSIN = msin(),
	PeerId = "0" ++ ct:get_config(mcc) ++ ct:get_config(mcc)
			++ MSIN ++ "@wlan." ++ Realm,
	PeerId1 = list_to_binary(PeerId),
	ok = send_radius_identity(Socket, Address, Port, NasId,
			PeerId1, Secret, ReqAuth, EapId, RadId),
	NextEapId = EapId + 1,
	EapMsg = radius_access_challenge(Socket,
			Address, Port, Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?AKA, identifier = NextEapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_aka_identity{fullauth_id_req = true,
			identity = ServerId} = ocs_eap_codec:eap_aka(EapData).

identity_diameter_eap() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using DIAMETER EAP application"}]}].

identity_diameter_eap(Config) ->
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId = 1,
	Realm = ?config(realm, Config),
	MSIN = msin(),
	PeerId = "0" ++ ct:get_config(mcc) ++ ct:get_config(mcc)
			++ MSIN ++ "@wlan." ++ Realm,
	PeerId1 = list_to_binary(PeerId),
	DEA = send_diameter_identity(?EAP_APPLICATION_ID, SId, EapId, PeerId1),
	SIdbin = list_to_binary(SId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [EapPayload]} = DEA,
	NextEapId = EapId + 1,
	#eap_packet{code = request, type = ?AKA, identifier = NextEapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapPayload),
	#eap_aka_identity{fullauth_id_req = true} = ocs_eap_codec:eap_aka(EapData).

identity_diameter_swm() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using DIAMETER EAP application"}]}].

identity_diameter_swm(Config) ->
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId = 1,
	Realm = ?config(realm, Config),
	MSIN = msin(),
	PeerId = "0" ++ ct:get_config(mcc) ++ ct:get_config(mcc)
			++ MSIN ++ "@wlan." ++ Realm,
	PeerId1 = list_to_binary(PeerId),
	DEA = send_diameter_identity(?SWm_APPLICATION_ID, SId, EapId, PeerId1),
	SIdbin = list_to_binary(SId),
	#'3gpp_swm_DEA'{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [EapPayload]} = DEA,
	NextEapId = EapId + 1,
	#eap_packet{code = request, type = ?AKA, identifier = NextEapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapPayload),
	#eap_aka_identity{fullauth_id_req = true} = ocs_eap_codec:eap_aka(EapData).

identity_diameter_swm_trusted() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using DIAMETER 3GPP SWm application from a trusted client"}]}].

identity_diameter_swm_trusted(Config) ->
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId = 1,
	Realm = ?config(realm, Config),
	MSIN = msin(),
	Name = ct:get_config(mcc) ++ ct:get_config(mcc) ++ MSIN,
	PeerId = "0" ++ Name ++ "@wlan." ++ Realm,
	PeerId1 = list_to_binary(PeerId),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	_Service = add_service(Name, ProdRef),
	DEA = send_diameter_identity(?SWm_APPLICATION_ID, SId, EapId, PeerId1),
	SIdbin = list_to_binary(SId),
	#'3gpp_swm_DEA'{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [EapPayload]} = DEA,
	NextEapId = EapId + 1,
	#eap_packet{code = request, type = ?AKA, identifier = NextEapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapPayload),
	#eap_aka_challenge{mac = <<_:128>>, rand = <<_:128>>,
			autn = <<_:128>>} = ocs_eap_codec:eap_aka(EapData).

prf() ->
   [{userdata, [{doc, "Psuedo-Random Number Function (PRF) (RFC4187 Appendix A)"}]}].

prf(_Config) ->
	Identity = <<"0001001000000001@wlan.mnc001.mcc001.3gppnetwork.org">>,
	IK = <<151,68,135,26,211,43,249,187,209,221,92,229,78,62,46,90>>,
	CK = <<83,73,251,224,152,100,159,148,143,93,46,151,58,129,192,15>>,
	Kencr = <<148,91,8,16,161,208,23,165,64,169,134,69,67,227,0,133>>,
	Kaut = <<87,96,126,37,22,113,111,207,109,211,155,62,214,86,159,97>>,
	MSK = <<217,187,67,94,211,52,132,255,234,40,196,63,79,207,34,84,171,
			250,178,244,145,168,248,131,227,111,91,87,47,47,47,141,150,226,
			109,195,79,38,25,151,172,239,221,28,30,61,96,155,141,114,17,144,
			81,226,225,131,225,71,143,0,217,133,111,196>>,
	EMSK = <<160,209,144,223,251,129,233,55,81,60,175,138,195,210,165,45,
			7,201,181,3,118,57,115,64,33,209,210,205,179,197,91,41,227,157,
			150,91,143,235,198,126,109,163,130,110,165,180,216,175,57,135,
			249,221,157,140,125,189,158,4,81,175,147,246,89,192>>,
	MK = crypto:hash(sha, [Identity, IK, CK]),
			<<Kencr:16/binary, Kaut:16/binary, MSK:64/binary,
			EMSK:64/binary>> = ocs_eap_aka:prf(MK).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @hidden
client_service_opts(Config) ->
	[{'Origin-Host', ?config(host, Config)},
			{'Origin-Realm', ?config(realm, Config)},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Client (auth)"},
			{'Auth-Application-Id', [?BASE_APPLICATION_ID,
					?EAP_APPLICATION_ID, ?SWm_APPLICATION_ID]},
			{string_decode, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_client_cb}]},
			{application, [{alias, eap_app_test},
					{dictionary, diameter_gen_eap_application_rfc4072},
					{module, diameter_test_client_cb}]},
			{application, [{alias, swm_app_test},
					{dictionary, diameter_gen_3gpp_swm_application},
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
send_diameter_identity(?EAP_APPLICATION_ID, SId, EapId, PeerId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'EAP-Payload' = EapMsg},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer;

send_diameter_identity(?SWm_APPLICATION_ID, SId, EapId, PeerId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #'3gpp_swm_DER'{'Session-Id' = SId,
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'EAP-Payload' = EapMsg, 'RAT-Type' = [0]},
	{ok, Answer} = diameter:call(?MODULE, swm_app_test, DER, []),
	Answer.

%% @hidden
mac() ->
	mac([]).
%% @hidden
mac(Acc) when length(Acc) =:= 12 ->
	Acc;
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

%% @hidden
price(Type, Units, Size, Amount) ->
	Name = ocs:generate_identity(),
	#price{name = Name,
			type = Type, units = Units,
			size = Size, amount = Amount}.

%% @hidden
add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, Spec) ->
	Name = ocs:generate_identity(),
	Offer = #offer{name = Name,
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
add_service(Name, ProdRef) ->
	K = crypto:strong_rand_bytes(16),
	OPc = crypto:strong_rand_bytes(16),
	Credentials = #aka_cred{k = K, opc = OPc},
	{ok, Service} = ocs:add_service(Name, Credentials,
			ProdRef, []),
	Service.

-spec sqn(DIF) -> SQN
	when
		DIF :: integer(),
		SQN :: integer().
%% @doc Sequence Number (SQN).
%%
%%      3GPP RTS 33.102 Annex C.1.1.3.
%% @private
sqn(DIF) when is_integer(DIF) ->
	(erlang:system_time(10) - DIF) bsl 5.

-spec amf() -> AMF
	when
		AMF :: binary().
%% @doc Authentication Management Field (AMF).
%%
%%      See 3GPP TS 33.102 Annex F.
%% @private
amf() ->
	<<1:1, 0:15>>.

-spec kdf(CK, IK, ANID, SQN, AK) -> MSK
        when
                CK :: binary(),
                IK :: binary(),
                ANID :: string(),
                SQN :: integer(),
                AK :: integer(),
                MSK :: binary().
%% @doc Key Derivation Function (KDF).
%%
%%      See 3GPP TS 33.402 Annex A,
%%          3GPP TS 32.220 Annex B.
%% @private
kdf(CK, IK, "WLAN", SQN, AK)
		when byte_size(CK) =:= 16, byte_size(IK) =:= 16,
		is_integer(SQN), is_integer(AK) ->
	SQNi = SQN bxor AK,
	crypto:hmac(sha256, <<CK/binary, IK/binary>>,
			<<16#20, "WLAN", 4:16, SQNi:48, 6:16>>).

