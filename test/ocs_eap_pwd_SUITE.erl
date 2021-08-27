%%% ocs_eap_pwd_SUITE.erl
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
%%% 	Protocol (EAP) using only a password (EAP-PWD)
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_eap_pwd_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_eap_application_rfc4072.hrl").
-include_lib("kernel/include/inet.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(EAP_APPLICATION_ID, 5).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

-dialyzer({[nowarn_function, no_match],
		[radius_access_request/10, receive_radius/7]}).
-ifdef(OTP_RELEASE).
	-define(HMAC(Key, Data),
		case ?OTP_RELEASE of
			OtpRelease when OtpRelease >= 23 ->
				crypto:mac(hmac, md5, Key, Data);
			OtpRelease when OtpRelease < 23 ->
				crypto:hmac(md5, Key, Data)
		end).
-else.
	-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
-endif.

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for authentication with EAP-PWD in OCS"}]},
	{timetrap, {seconds, 8}},
	{require, radius},
	{default_config, radius, [{username, "ocs"},
			{password, "ocs123"}, {secret, "xyzzy5461"}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:load(ocs),
	RadiusAddress = ct:get_config({radius, address}, {127,0,0,1}),
	RadiusAuthPort = ct:get_config({radius, auth_port}, rand:uniform(64511) + 1024),
	Options = [{eap_method_prefer, pwd}, {eap_method_order, [pwd, ttls, akap, aka]}],
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
%% Initialization before each test case.
%%
init_per_testcase(TestCase, Config) when TestCase == eap_identity_diameter;
		TestCase == pwd_id_diameter; TestCase == pwd_commit_diameter;
		TestCase == pwd_confirm_diameter ->
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, _, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	[{diameter_client, Address} | Config];
init_per_testcase(_TestCase, Config) ->
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{RadIP, RadPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, RadIP}, binary]),
	SharedSecret = ct:get_config({radius, secret}),
	Protocol = radius,
	{ok, _} = ocs:add_client(RadIP, RadPort, Protocol, SharedSecret, true),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}, {socket, Socket}, {radius_client, RadIP} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when TestCase == eap_identity_diameter;
		TestCase == pwd_id_diameter; TestCase == pwd_commit_diameter;
		TestCase == pwd_confirm_diameter	->
	DClient = ?config(diameter_client, Config),
	ok = ocs:delete_client(DClient);
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	RadClient = ?config(radius_client, Config),
	ok = ocs:delete_client(RadClient),
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
	[eap_identity_radius, pwd_id_radius, pwd_commit_radius, pwd_confirm_radius,
	message_authentication_radius, role_reversal_radius, validate_pwd_id_cipher_radius,
	validate_pwd_id_prep_radius, validate_pwd_id_token_radius, negotiate_method_radius,
	eap_identity_diameter, pwd_id_diameter, pwd_commit_diameter, pwd_confirm_diameter].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

eap_identity_radius() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using RADIUS to peer"}]}].

eap_identity_radius(Config) ->
	MAC = "AA-BB-CC-DD-EE-FF",
	PeerId = <<"12345678">>,
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth = radius:authenticator(),
	RadId = 1, EapId = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth, EapId, RadId),
	NextEapId = EapId + 1,
	{NextEapId, _Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth, RadId).

eap_identity_diameter() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response using DIAMETER to peer"}]}].

eap_identity_diameter(_Config) ->
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId = 1,
	PeerId = <<"12345678">>,
	DEA = send_diameter_identity(SId, EapId, PeerId),
	SIdbin = list_to_binary(SId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH'} = DEA.

pwd_id_radius() ->
   [{userdata, [{doc, "Send an EAP-pwd-ID/Response to peer"}]}].

pwd_id_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "BB-CC-DD-EE-FF-AA",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 2, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	send_radius_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, Token, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	{EapId3, _ElementS, _ScalarS} = receive_radius_commit(Socket, Address,
			Port, Secret, ReqAuth2, RadId2).

pwd_id_diameter() ->
   [{userdata, [{doc, "Send an EAP-pwd-ID/Response to peer using DIAMETER"}]}].

pwd_id_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId0 = 0,
	DEA = send_diameter_identity(SId, EapId0, PeerId),
	SIdbin = list_to_binary(SId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload]} = DEA,
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
		data = EapData} = ocs_eap_codec:eap_packet(Payload),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none, token = Token,
			identity = _ServerId} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	DEA1 = send_diameter_id(SId, Token, PeerId, EapId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload1]} = DEA1,
	#eap_packet{code = request, type = ?PWD, identifier = EapId1,
		data = EapData1} = ocs_eap_codec:eap_packet(Payload1),
	EapId1 = EapId + 1,
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = EapPwdData1} = ocs_eap_codec:eap_pwd(EapData1),
	#eap_pwd_commit{element = Element, scalar = Scalar} = ocs_eap_codec:eap_pwd_commit(EapPwdData1),
	true = is_binary(Element),
	true = is_binary(Scalar),
	ok = ocs:delete_service(PeerId).

pwd_commit_radius() ->
	[{userdata, [{doc, "Send an EAP-pwd-Commit/Response using RADIUS to peer"}]}].

pwd_commit_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId, password = PeerAuth} =  add_service(ProdRef),
	MAC = "CC-DD-EE-FF-AA-BB",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 5, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	send_radius_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, Token, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	{EapId3, _ElementS, _ScalarS} = receive_radius_commit(Socket, Address,
			Port, Secret, ReqAuth2, RadId2),
	Prand = crypto:rand_uniform(1, ?R),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerID, PeerAuth),
	{ScalarP, ElementP} = ocs_eap_pwd:compute_scalar(<<Prand:256>>, PWE),
	RadId3 = RadId2 + 1,
	ReqAuth3 = radius:authenticator(),
	ok = send_radius_commit(Socket, Address, Port, Secret, ReqAuth3,
			UserName, NasId, MAC, ScalarP, ElementP, EapId3, RadId3),
	EapId4 = EapId3 + 1,
	{EapId4, _ConfirmS} = receive_radius_confirm(Socket,
			Address, Port, Secret, ReqAuth3, RadId3).

pwd_commit_diameter() ->
   [{userdata, [{doc, "Send an EAP-pwd-Commit/Response to peer using DIAMETER"}]}].

pwd_commit_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId, password = PeerAuth} =  add_service(ProdRef),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId0 = 0,
	DEA = send_diameter_identity(SId, EapId0, PeerId),
	SIdbin = list_to_binary(SId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload]} = DEA,
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
		data = EapData} = ocs_eap_codec:eap_packet(Payload),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none, token = Token,
			identity = ServerId} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	DEA1 = send_diameter_id(SId, Token, PeerId, EapId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload1]} = DEA1,
	#eap_packet{code = request, type = ?PWD, identifier = EapId1,
		data = EapData1} = ocs_eap_codec:eap_packet(Payload1),
	EapId1 = EapId + 1,
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = EapPwdData1} = ocs_eap_codec:eap_pwd(EapData1),
	#eap_pwd_commit{element = ElementS, scalar = ScalarS} = ocs_eap_codec:eap_pwd_commit(EapPwdData1),
	true = is_binary(ElementS),
	true = is_binary(ScalarS),
	Prand = crypto:rand_uniform(1, ?R),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerId, PeerAuth),
	{ScalarP, ElementP} = ocs_eap_pwd:compute_scalar(<<Prand:256>>, PWE),
	DEA2 = send_diameter_commit(SId, ScalarP, ElementP, EapId1),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload2]} = DEA2,
	#eap_packet{code = request, type = ?PWD, identifier = EapId2,
		data = EapData2} = ocs_eap_codec:eap_packet(Payload2),
	EapId2 = EapId1 + 1,
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
			data = _EapPwdData2} = ocs_eap_codec:eap_pwd(EapData2),
	ok = ocs:delete_service(PeerId).
	
pwd_confirm_radius() ->
	[{userdata, [{doc, "Send an EAP-pwd-Confirm/Response using RADIUS to peer"}]}].

pwd_confirm_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId, password = PeerAuth} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	MAC = "DD-EE-FF-AA-BB-CC",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 8, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	send_radius_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, Token, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	{EapId3, ElementS, ScalarS} = receive_radius_commit(Socket, Address,
			Port, Secret, ReqAuth2, RadId2),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerID, PeerAuth),
	Prand = crypto:rand_uniform(1, ?R),
	{ScalarP, ElementP} = ocs_eap_pwd:compute_scalar(<<Prand:256>>, PWE),
	RadId3 = RadId2 + 1,
	ReqAuth3 = radius:authenticator(),
	ok = send_radius_commit(Socket, Address, Port, Secret, ReqAuth3,
			UserName, NasId, MAC, ScalarP, ElementP, EapId3, RadId3),
	EapId4 = EapId3 + 1,
	{EapId4, _ConfirmS} = receive_radius_confirm(Socket,
			Address, Port, Secret, ReqAuth3, RadId3),
	Ciphersuite = <<19:16, 1, 1>>,
	Kp = ocs_eap_pwd:compute_ks(<<Prand:256>>, PWE, ScalarS, ElementS),
	Input = [Kp, ElementP, ScalarP, ElementS, ScalarS, Ciphersuite],
	ConfirmP = ocs_eap_pwd:h(Input),
	RadId4 = RadId3 + 1,
	ReqAuth4 = radius:authenticator(),
	send_radius_confirm(Socket, Address, Port, Secret, ReqAuth4, UserName,
			NasId, MAC, ConfirmP, EapId4, RadId4),
	EapId4 = receive_radius_success(Socket, Address, Port, Secret, ReqAuth4, RadId4).

pwd_confirm_diameter() ->
   [{userdata, [{doc, "Send an EAP-pwd-Confirm/Response to peer using DIAMETER"}]}].

pwd_confirm_diameter(_Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId, password = PeerAuth} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId0 = 0,
	DEA = send_diameter_identity(SId, EapId0, PeerId),
	SIdbin = list_to_binary(SId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload]} = DEA,
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
		data = EapData} = ocs_eap_codec:eap_packet(Payload),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none, token = Token,
			identity = ServerId} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	DEA1 = send_diameter_id(SId, Token, PeerId, EapId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload1]} = DEA1,
	#eap_packet{code = request, type = ?PWD, identifier = EapId1,
		data = EapData1} = ocs_eap_codec:eap_packet(Payload1),
	EapId1 = EapId + 1,
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = EapPwdData1} = ocs_eap_codec:eap_pwd(EapData1),
	#eap_pwd_commit{element = ElementS, scalar = ScalarS} = ocs_eap_codec:eap_pwd_commit(EapPwdData1),
	true = is_binary(ElementS),
	true = is_binary(ScalarS),
	Prand = crypto:rand_uniform(1, ?R),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerId, PeerAuth),
	{ScalarP, ElementP} = ocs_eap_pwd:compute_scalar(<<Prand:256>>, PWE),
	DEA2 = send_diameter_commit(SId, ScalarP, ElementP, EapId1),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [Payload2]} = DEA2,
	#eap_packet{code = request, type = ?PWD, identifier = EapId2,
		data = EapData2} = ocs_eap_codec:eap_packet(Payload2),
	EapId2 = EapId1 + 1,
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
			data = _EapPwdData2} = ocs_eap_codec:eap_pwd(EapData2),
	Ciphersuite = <<19:16, 1, 1>>,
	Kp = ocs_eap_pwd:compute_ks(<<Prand:256>>, PWE, ScalarS, ElementS),
	Input = [Kp, ElementP, ScalarP, ElementS, ScalarS, Ciphersuite],
	ConfirmP = ocs_eap_pwd:h(Input),
	DEA3 = send_diameter_confirm(SId, ConfirmP, EapId2),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'EAP-Payload' = [Payload3]} = DEA3,
	#eap_packet{code = success,
			identifier = EapId2} = ocs_eap_codec:eap_packet(Payload3),
	ok = ocs:delete_service(PeerId).

message_authentication_radius() ->
	[{userdata, [{doc, "Send corrupt Message-Authenticator using RADIUS"}]}].

message_authentication_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "EE-FF-AA-BB-CC-DD",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ocs:generate_password(), 
	ReqAuth = radius:authenticator(),
	RadId = 13, EapId = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth, EapId, RadId),
	{error, timeout} = gen_udp:recv(Socket, 0, 2000).

role_reversal_radius() ->
	[{userdata, [{doc, "Send EAP-Request (unsupported role reversal) using RADIUS"}]}].

role_reversal_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "FF-AA-BB-CC-DD-FF",
	Socket = ?config(socket, Config),
	Address = {127, 0, 0, 1},
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth = radius:authenticator(),
	RadId = 14, EapId = 1,
	Token = crypto:strong_rand_bytes(4),
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = request, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	ok = radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth, RadId, EapMsg),
	{EapId, <<0>>} = receive_radius_nak(Socket, Address, Port, Secret, ReqAuth, RadId).

validate_pwd_id_cipher_radius() ->
	[{userdata, [{doc, "Send invalid EAP-pwd-ID (bad cipher) using RADIUS"}]}].

validate_pwd_id_cipher_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "AB-CD-EF-FE-DC-BA",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 16, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	InvalidGroup = 20, % 384-bit random ECP group
	EapPwdId = #eap_pwd_id{group_desc = InvalidGroup, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD,
			identifier = EapId2, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	ok = radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth2, RadId2, EapMsg),
	EapId2 = receive_radius_failure(Socket, Address, Port, Secret, ReqAuth2, RadId2).

validate_pwd_id_prep_radius() ->
	[{userdata, [{doc, "Send invalid EAP-pwd-ID (bad prep) using RADIUS"}]}].

validate_pwd_id_prep_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "CD-EF-FE-DC-BA-AB",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 16, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	UnimplementedPrep = saslprep,
	EapPwdId = #eap_pwd_id{group_desc = 20, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = UnimplementedPrep, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD,
			identifier = EapId2, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	ok = radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth2, RadId2, EapMsg),
	EapId2 = receive_radius_failure(Socket, Address, Port, Secret, ReqAuth2, RadId2).

validate_pwd_id_token_radius() ->
	[{userdata, [{doc, "Send invalid EAP-pwd-ID (bad token) using RADIUS"}]}].

validate_pwd_id_token_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "EF-FE-DC-BA-AB-CD",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 16, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, _Token, _ServerID} = receive_radius_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	InvalidToken = crypto:strong_rand_bytes(4),
	EapPwdId = #eap_pwd_id{group_desc = 20, random_fun = 16#1, prf = 16#1,
			token = InvalidToken, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD,
			identifier = EapId2, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	ok = radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth2, RadId2, EapMsg),
	EapId2 = receive_radius_failure(Socket, Address, Port, Secret, ReqAuth2, RadId2).

negotiate_method_radius() ->
	[{userdata, [{doc, "Send EAP-Nak with alternate methods using RADIUS"}]}].

negotiate_method_radius(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerId} =  add_service(ProdRef),
	MAC = "FE-DC-BA-AB-CD-EF",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 16, EapId1 = 1,
	ok = send_radius_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	EapMsg1 = radius_access_challenge(Socket, Address, Port,
			Secret, RadId1, ReqAuth1),
	#eap_packet{code = request, type = Method,
			identifier = EapId2} = ocs_eap_codec:eap_packet(EapMsg1),
	true = (((Method > 4) and (Method < 254)) or (Method == 255)),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	Methods = [?PEAP, ?TLS, ?TTLS, ?SIM, ?AKA, ?AKAprime, ?PWD],
	AlternateMethods = lists:delete(Method, Methods),
	EapPacket  = #eap_packet{code = response,
			type = ?LegacyNak, identifier = EapId2,
			data = list_to_binary(AlternateMethods)},
	EapMsg2 = ocs_eap_codec:eap_packet(EapPacket),
	ok = radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth2, RadId2, EapMsg2),
	EapMsg3 = radius_access_challenge(Socket, Address, Port,
			Secret, RadId2, ReqAuth2),
	EapId3 = EapId2 + 1,
	#eap_packet{code = request, type = Type,
			identifier = EapId3} = ocs_eap_codec:eap_packet(EapMsg3),
	true = lists:member(Type, AlternateMethods).

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
	MsgAuth1 = ?HMAC(Secret, ReqPacket1),
	A8 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A7),
	Request2 = Request1#radius{attributes = A8},
	ReqPacket2 = radius:codec(Request2),
	gen_udp:send(Socket, Address, Port, ReqPacket2).

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
	MsgAuth = ?HMAC(Secret, RespPacket3),
	{ok, EapMsg} = radius_attributes:find(?EAPMessage, RespAttr1),
	EapMsg.

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

send_radius_id(Socket, Address, Port, Secret, Auth, UserName,
		NasId, PeerId, MAC, Token, EapId, RadId) ->
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD, identifier = EapId,
			data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_radius_commit(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = radius_access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_commit{element = ElementS,
			scalar = ScalarS} = ocs_eap_codec:eap_pwd_commit(EapPwdData),
	{EapId, ElementS, ScalarS}.

send_radius_commit(Socket, Address, Port, Secret, Auth, UserName,
		NasId, MAC, ScalarP, ElementP, EapId, RadId) ->
	EapPwdCommit = #eap_pwd_commit{scalar = ScalarP, element = ElementP},
	EapPwd = #eap_pwd{length = false, more = false, pwd_exch = commit,
			data = ocs_eap_codec:eap_pwd_commit(EapPwdCommit)},
	EapPacket = #eap_packet{code = response, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_radius_confirm(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = radius_access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
			data = ConfirmS} = ocs_eap_codec:eap_pwd(EapData),
	{EapId, ConfirmS}.

send_radius_confirm(Socket, Address, Port, Secret, Auth, UserName,
		NasId, MAC, ConfirmP, EapId, RadId) ->
	EapPwd = #eap_pwd{length = false, more = false,
			pwd_exch = confirm, data = ConfirmP},
	EapPacket = #eap_packet{code = response, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	radius_access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_radius_success(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = radius_access_accept(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = success,
			identifier = EapId} = ocs_eap_codec:eap_packet(EapMsg),
	EapId.

receive_radius_failure(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = radius_access_reject(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = failure,
			identifier = EapId} = ocs_eap_codec:eap_packet(EapMsg),
	EapId.

receive_radius_nak(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = radius_access_reject(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = response, type = ?LegacyNak, identifier = EapId,
			data = AuthTypes} = ocs_eap_codec:eap_packet(EapMsg),
	{EapId, AuthTypes}.

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
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Client (auth)"},
			{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?EAP_APPLICATION_ID]},
			{string_decode, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_client_cb}]},
			{application, [{alias, eap_app_test},
					{dictionary, diameter_gen_eap_application_rfc4072},
					{module, diameter_test_client_cb}]}].

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans}, {transport_config,
			[{raddr, RemAddr}, {rport, RemPort},
			{reuseaddr, true}, {ip, LocalAddr}]}].

send_diameter_identity(SId, EapId, PeerId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity, identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
		'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapMsg},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

send_diameter_id(SId, Token, PeerId, EapId) ->
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD, identifier = EapId,
			data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapPayload = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
		'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapPayload},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

send_diameter_commit(SId, ScalarP, ElementP, EapId) ->
	EapPwdCommit = #eap_pwd_commit{scalar = ScalarP, element = ElementP},
	EapPwd = #eap_pwd{length = false, more = false, pwd_exch = commit,
			data = ocs_eap_codec:eap_pwd_commit(EapPwdCommit)},
	EapPacket = #eap_packet{code = response, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapPayload = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
		'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapPayload},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

send_diameter_confirm(SId, ConfirmP, EapId) ->
	EapPwdConfirm = #eap_pwd{length = false, more = false, pwd_exch = confirm, data = ConfirmP},
	EapPacket = #eap_packet{code = response, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwdConfirm)},
	EapPayload = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
		'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapPayload},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

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
