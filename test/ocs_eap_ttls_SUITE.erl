%%% ocs_eap_ttls_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2024 SigScale Global Inc.
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
%%% 	Protocol (EAP) Tunneled Transport Layer Security (EAP-TTLS)
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_eap_ttls_SUITE).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-behaviour(ct_suite).

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%Macro definitions for TLS record Content Type
-define(ChangeCipherSpec,	20).
-define(Alert,					21).
-define(Handshake,			22).
-define(Application,			23).
-define(Heartbeat,			24).

%%Macro definitions for TLS handshake protocal message type
-define(HelloRequest,			0).
-define(ClientHello,				1).
-define(ServerHello,				2).
-define(NewSessionTicket,		4).
-define(Certificate,				11).
-define(ServerKeyExchange,		12).
-define(CertificateRequest,	13).
-define(ServerHelloDone,		14).
-define(CertificateVerify,		15).
-define(ClientKeyExchange,		16).
-define(Finished,					20).

%% Macro definitions for diameter service
-define(BASE_APPLICATION_ID, 0).
-define(EAP_APPLICATION_ID, 5).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_eap_application_rfc4072.hrl").
-include_lib("kernel/include/inet.hrl").

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(HMAC(Key, Data), crypto:mac(hmac, md5, Key, Data)).
	-else.
		-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
	-endif.
	-if(?OTP_RELEASE >= 24).
		-define(BROKEN_TLS, true).
	-else.
		-define(BROKEN_TLS, false).
	-endif.
-else.
	-define(BROKEN_TLS, false).
	-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
-endif.

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for authentication with EAP-TTLS in OCS."}]},
	{timetrap, {seconds, 8}},
	{require, radius},
	{default_config, radius,
			[{bind, {127,0,0,1}},
			{username, "ocs"},
			{password, "ocs123"},
			{secret, "xyzzy5461"}]},
	{require, diameter},
	{default_config, diameter,
			[{bind, {127,0,0,1}},
			{realm, "mnc001.mcc001.3gppnetwork.org"}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) when ?BROKEN_TLS == true ->
	% The `ssl' application stopped being transport agnostic in OTP 24.1
	% https://erlangforums.com/t/any-reason-why-the-ssl-application-is-no-longer-transport-agnostic/3467
	{skip, broken_tls};
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:load(ocs),
	RadiusBindAddress = ct:get_config({radius, bind}),
	RadiusAuthPort = ct:get_config({radius, auth_port}, rand:uniform(64511) + 1024),
	Options = [{eap_method_prefer, pwd}, {eap_method_order, [pwd, akap, ttls]}],
	RadiusAppVar = [{auth, [{RadiusBindAddress, RadiusAuthPort, Options}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterBindAddress = ct:get_config({diameter, bind}),
	DiameterAuthPort = ct:get_config({diameter, auth_port}, rand:uniform(64511) + 1024),
	DiameterAppVar = [{auth, [{DiameterBindAddress, DiameterAuthPort, Options}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = ocs_test_lib:start(),
	NasId = atom_to_list(node()),
	OriginRealm = ct:get_config({diameter, realm}),
	OriginHost = ct:get_config({diameter, host},
			atom_to_list(?MODULE) ++ "." ++ OriginRealm),
	Config1 = [{origin_host, OriginHost}, {origin_realm, OriginRealm},
			{nas_id, NasId} | Config],
	DiameterClientAddress = {127, 0, 0, 1},
	{ok, _} = ocs:add_client(DiameterClientAddress,
			undefined, diameter, undefined, true),
	ok = diameter:start_service(?MODULE, client_service_opts(Config1)),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE,
			DiameterBindAddress, DiameterAuthPort, diameter_tcp),
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
init_per_testcase(TestCase, Config) when TestCase == eap_ttls_authentication_diameter ->
	ClientAddress = {127, 0, 0, 1},
	{ok, _} = ocs:add_client(ClientAddress, undefined, diameter, undefined, true),
	lists:keystore(diameter_client, 1, Config, {diameter_client, ClientAddress});
init_per_testcase(_TestCase, Config) ->
	ClientAddress = {127, 0, 0, 1},
	Secret = ct:get_config({radius, secret}),
	{ok, _} = ocs:add_client(ClientAddress, 3799, radius, Secret, true),
	{ok, Socket} = gen_udp:open(0, [{active, false},
			inet, {ip, ClientAddress}, binary]),
	lists:keystore(socket, 1, Config, {socket, Socket}).

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when TestCase == eap_ttls_authentication_diameter ->
	Dclient = ?config(diameter_client, Config),
	ok = ocs:delete_client(Dclient);
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
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
	[eap_ttls_authentication_radius, eap_ttls_authentication_diameter].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
eap_ttls_authentication_radius() ->
	[{userdata, [{doc, "EAP-TTLS Authentication using RADIUS"}]}].

eap_ttls_authentication_radius(Config) ->
	DataDir = ?config(data_dir, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username,
			password = PeerAuth} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	AnonymousName = ocs:generate_password(),
	MAC = "DD-EE-FF-AA-BB-CC",
	Socket = ?config(socket, Config),
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config({radius, username}),
	Secret = ct:get_config({radius, secret}),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 8, EapId1 = 1,
	ok = send_identity_radius(Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, ReqAuth1, EapId1, RadId1),
	{RadId2, EapId2} = receive_identity(Socket, Address, Port, Secret,
			ReqAuth1, RadId1),
	ReqAuth2 = radius:authenticator(),
	ok = send_legacy_nak_radius(Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, ReqAuth2, EapId2, RadId2),
	{RadId3, EapId3} = receive_ttls_start_radius(Socket, Address,Port, Secret, ReqAuth2,
					RadId2),
	Options  = [{cacertfile, DataDir ++ "CAcert.pem"}],
	proc_lib:spawn_link(peer_tls_transport, ssl_connect, [Address, self(), Options]),
	{SslPid1, ClientHelloMsg} = ssl_handshake(),
	ReqAuth3 = radius:authenticator(),
	{RadId4, CHAuth} = client_hello_radius(ClientHelloMsg, Socket, Address, Port,
			NasId, AnonymousName, Secret, MAC, ReqAuth3, EapId3, RadId3),
	{RadId5, EapId4, ServerHello} = server_hello_radius(Socket, Address, Port,
			NasId, AnonymousName, Secret, MAC, CHAuth, RadId4),
	peer_tls_transport:deliver(SslPid1, self(), ServerHello),
	{_SslPid2, ClientCipher} = ssl_handshake(),
	ReqAuth5 = radius:authenticator(),
	{RadId6, CCAuth} = client_cipher_radius(ClientCipher, Socket, Address, Port,
			NasId, AnonymousName, Secret, MAC, ReqAuth5, EapId4, RadId5),
	{RadId7, EapId5, ServerCipher} = server_cipher_radius(Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, CCAuth, RadId6),
	peer_tls_transport:deliver(SslPid1, self(), ServerCipher),
	SslSocket = ssl_handshake(),
	Seed = prf_seed(ClientHelloMsg, ServerHello),
	{MSK, _} = prf(SslSocket, master_secret ,
			<<"ttls keying material">>, Seed, 128),
	ReqAuth6 = radius:authenticator(),
	{RadId7, CPAuth} = client_passthrough_radius(SslSocket,
			binary_to_list(Username), PeerAuth, Socket, Address,
			Port, NasId, Secret, MAC, ReqAuth6, EapId5, RadId7),
	ok = server_passthrough_radius(Socket, Address, Port, NasId, UserName, Secret,
			MAC, MSK, CPAuth, RadId7),
	ok = ssl:close(SslSocket).

eap_ttls_authentication_diameter() ->
	[{userdata, [{doc, "EAP-TTLS Authentication using DIAMETER"}]}].

eap_ttls_authentication_diameter(Config) ->
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	EapId = 1,
	SIdbin = list_to_binary(SId),
	DataDir = ?config(data_dir, Config),
	AnonymousName = ocs:generate_password(),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = Username,
			password = PeerAuth} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, _Port, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	DEA1 = send_identity_diameter(SId, AnonymousName, EapId),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [EapMsg]} = DEA1,
	#eap_packet{code = request, type = ?PWD, identifier = EapId1,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	DEA2 = send_legacy_nak_diameter(SId, EapId1),
	#diameter_eap_app_DEA{'Session-Id' = SIdbin, 'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'EAP-Payload' = [EapMsg2]} = DEA2,
	EapId2 = EapId1 + 1,
	#eap_packet{code = request, type = ?TTLS, identifier = EapId2,
			data = EapData2} = ocs_eap_codec:eap_packet(EapMsg2),
	#eap_ttls{start = true} = ocs_eap_codec:eap_ttls(EapData2),
	Options  = [{cacertfile, DataDir ++ "CAcert.pem"}],
	proc_lib:spawn_link(peer_tls_transport, ssl_connect, [Address, self(), Options]),
	{SslPid1, ClientHelloMsg} = ssl_handshake(),
	{EapId3, ServerHello} = client_hello_diameter(SId, ClientHelloMsg, EapId2),
	peer_tls_transport:deliver(SslPid1, self(), ServerHello),
	{_SslPid2, ClientCipher} = ssl_handshake(),
	{EapId4, ServerCipher} = client_cipher_diameter(ClientCipher, AnonymousName, SId, EapId3),
	peer_tls_transport:deliver(SslPid1, self(), ServerCipher),
	SslSocket = ssl_handshake(),
	Seed = prf_seed(ClientHelloMsg, ServerHello),
	{_MSK, _} = prf(SslSocket, master_secret ,
			<<"ttls keying material">>, Seed, 128),
	ok = client_passthrough_diameter(SslSocket, binary_to_list(Username), PeerAuth, SId, EapId4),
	ok = ssl:close(SslSocket).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------
send_identity_radius(Socket, Address, Port, NasId, AnonymousName, Secret, MAC,
		Auth, EapId, RadId) ->
	BinAnonymousName = list_to_binary(AnonymousName),
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = BinAnonymousName},
	access_request(EapPacket, Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, Auth, RadId).

receive_identity(Socket, Address, Port, Secret, Auth, RadId) ->
erlang:display({?MODULE, ?LINE, application:get_env(ocs, radius)}),
	Radius = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	NewRadId = RadId + 1,
	{NewRadId, EapId}.

send_legacy_nak_radius(Socket, Address, Port, NasId, AnonymousName, Secret, MAC,
		Auth, EapId, RadId) ->
	EapPacket  = #eap_packet{code = response, type = ?LegacyNak, identifier = EapId,
			data = <<?TTLS>>},
	access_request(EapPacket, Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, Auth, RadId).

receive_ttls_start_radius(Socket, Address, Port, Secret, Auth, RadId) ->
	Radius = access_challenge(Socket, Address, Port, Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, type = ?TTLS, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_ttls{start = true} = ocs_eap_codec:eap_ttls(EapData),
	NewRadId = RadId + 1,
	{NewRadId, EapId}.

client_hello_radius(<<?Handshake, _:32, ?ClientHello, _/binary>> = Data,
		Socket, Address, Port, NasId, UserName, Secret, MAC,
		Auth, EapId, RadId) ->
	client_hello_radius1(Data, Socket, Address, Port, NasId, UserName,
			Secret, MAC, Auth, EapId, RadId).
%% @hidden
client_hello_radius1(<<Chunk:1386/binary, Rest/binary>> = Data, Socket, Address,
		Port, NasId, UserName, Secret, MAC, Auth, EapId, RadId) ->
	Size = size(Data),
	EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId),
	NewEapId = receive_ack(Socket, Address, Port, Secret, Auth, RadId),
	NewRadId = RadId + 1,
	NewAuth = radius:athenticator(),
	client_hello_radius1(Rest, Socket, Address, Port, NasId, UserName,
			Secret, MAC, NewAuth, NewEapId, NewRadId);
client_hello_radius1(Chunk, Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, EapId, RadId) ->
	EapTtls = #eap_ttls{data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId),
	{RadId, Auth}.

server_hello_radius(Socket, Address, Port, NasId, UserName, Secret,
		MAC, Auth, RadId) ->
	Radius = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, identifier = EapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsData = ocs_eap_codec:eap_ttls(EapData),
	server_hello_radius1(TtlsData, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapId, <<>>).
%% @hidden
server_hello_radius1(#eap_ttls{more = true, data = SH}, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, RadId, EapId, Buf) ->
	send_ack(Socket, Address, Port, NasId, UserName, Secret,
			MAC, Auth, RadId, EapId),
	Radius = access_challenge(Socket, Address, Port, Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, identifier = NewEapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsPacket = ocs_eap_codec:eap_ttls(EapData),
	NewRadId = RadId + 1,
	NewAuth = radius:authenticator(),
	server_hello_radius1(TtlsPacket, Socket, Address, Port, NasId,
		UserName, Secret, MAC, NewAuth, NewRadId, NewEapId, <<SH/binary, Buf/binary>>);
server_hello_radius1(#eap_ttls{data = SH}, _, _, _, _, _, _, _, _,
		RadId, EapId, Buf) ->
	%send_ack(Socket, Address, Port, NasId, UserName, Secret,
	%		MAC, Auth, RadId, EapId),
	NewRadId = RadId + 1,
	{NewRadId, EapId, <<Buf/binary, SH/binary>>}.

client_cipher_radius(Data, Socket, Address, Port, NasId, UserName, Secret,
		MAC, Auth, EapId, RadId) ->
	client_cipher_radius1(Data, Socket, Address, Port, NasId, UserName, Secret,
			MAC, Auth, EapId, RadId, <<>>).
%% @hidden
client_cipher_radius1(<<?Handshake, _:32, ?ClientKeyExchange, _/binary>>  = Data, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, EapId, RadId, Buff) ->
	client_cipher_radius2(Data, Socket, Address, Port,
			NasId, UserName, Secret, MAC, Auth, EapId, RadId, Buff);
client_cipher_radius1(<<?ChangeCipherSpec, _:32, 1, _/binary>>  = Data, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, EapId, RadId, Buff) ->
	client_cipher_radius2(Data, Socket, Address, Port,
			NasId, UserName, Secret, MAC, Auth, EapId, RadId, Buff);
client_cipher_radius1(<<?Handshake, _/binary>>  = Data, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, EapId, RadId, Buff) ->
	ClientCipher = <<Buff/binary, Data/binary>>,
	client_cipher_radius3(ClientCipher, Socket, Address, Port,
			NasId, UserName, Secret, MAC, Auth, EapId, RadId).
%% @hidden
client_cipher_radius2(<<_:24, L1:16, _/binary>>  = Data, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, EapId, RadId, Buff) ->
	case Data of
		<<_:40, _:L1/binary>> ->
			{_SslPid2, Rest} = ssl_handshake(),
			Chunk = <<Buff/binary, Data/binary>>,
			client_cipher_radius1(Rest, Socket, Address, Port,
				NasId, UserName, Secret, MAC, Auth, EapId, RadId, Chunk);
		<<_:40, _:L1/binary, Rest/binary>> = Payload->
			BlockSize = L1 + 5,
			<<CCBlock:BlockSize/binary, Rest/binary>> = Payload,
			Chunk = <<Buff/binary, CCBlock/binary>>,
			client_cipher_radius1(Rest, Socket, Address, Port,
				NasId, UserName, Secret, MAC, Auth, EapId, RadId, Chunk)
	end.
%% @hidden
client_cipher_radius3(<<Chunk:1386/binary, Rest/binary>> = Data, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, EapId, RadId) ->
	Size = size(Data),
	EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId),
	NewEapId = receive_ack(Socket, Address, Port, Secret, Auth, RadId),
	NewRadId = RadId + 1,
	NewAuth = radius:athenticator(),
	client_cipher_radius3(Rest, Socket, Address, Port, NasId, UserName,
			Secret, MAC, NewAuth, NewEapId, NewRadId);
client_cipher_radius3(Chunk, Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, EapId, RadId) ->
	EapTtls = #eap_ttls{data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId),
	{RadId, Auth}.

server_cipher_radius(Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, RadId) ->
	Radius = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, identifier = EapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsData = ocs_eap_codec:eap_ttls(EapData),
	server_cipher_radius1(TtlsData, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapId, <<>>).
%% @hidden
server_cipher_radius1(#eap_ttls{more = true, data = SC}, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, RadId, EapId, Buf) ->
	send_ack(Socket, Address, Port, NasId, UserName, Secret,
			MAC, Auth, RadId, EapId),
	Radius = access_challenge(Socket, Address, Port, Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, identifier = NewEapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsPacket = ocs_eap_codec:eap_ttls(EapData),
	NewRadId = RadId + 1,
	NewAuth = radius:authenticator(),
	server_hello_radius1(TtlsPacket, Socket, Address, Port, NasId,
		UserName, Secret, MAC, NewAuth, NewRadId, NewEapId, <<Buf/binary, SC/binary>>);
server_cipher_radius1(#eap_ttls{data = SC}, _, _, _, _, _, _, _, _,
		RadId, EapId, Buf) ->
	%send_ack(Socket, Address, Port, NasId, UserName, Secret,
	%		MAC, Auth, RadId, EapId),
	NewRadId = RadId + 1,
	{NewRadId, EapId, <<Buf/binary, SC/binary>>}.

client_passthrough_radius(SslSocket, UserName, Password, Socket, Address, Port,
		NasId, Secret, MAC, Auth, EapId, RadId) ->
	UN = #diameter_avp{code = ?UserName, is_mandatory = true,
			data = UserName},
	PW = #diameter_avp{code = ?UserPassword, is_mandatory = true,
			data = Password},
	AVPs = list_to_binary(lists:map(fun diameter_codec:pack_avp/1,
			[UN, PW])),
	ok = ssl:send(SslSocket, AVPs),
	{_SslPid, UserCredential} = ssl_handshake(),
	TtlsPacket = #eap_ttls{data = UserCredential},
	TtlsData = ocs_eap_codec:eap_ttls(TtlsPacket),
	EapPacket = #eap_packet{code = response, identifier = EapId, type = ?TTLS,
		data = TtlsData},
	access_request(EapPacket, Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId),
	{RadId, Auth}.

server_passthrough_radius(Socket, Address, Port, _NasId, _UserName, Secret,
			_MAC, MSK, Auth, RadId) ->
	Radius = access_accept(Socket, Address, Port,
			Secret, RadId, Auth),
	Attributes = radius_attributes:codec(Radius#radius.attributes),
	{ok, {Salt, MsMppeKey}} = radius_attributes:find(?Microsoft, ?MsMppeSendKey, Attributes),
	{ok, {Salt, MsMppeKey}} = radius_attributes:find(?Microsoft, ?MsMppeRecvKey, Attributes),
	MsMppeKey = encrypt_key(list_to_binary(Secret), binary_to_list(Auth), Salt, MSK),
	EapMsg = get_eap(Radius),
	#eap_packet{code = success, identifier = _EapId} = ocs_eap_codec:eap_packet(EapMsg),
	ok.

%% EapPacket :: #eap_packet{}.
access_request(EapPacket, Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, RadId) ->
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request1(EapMsg, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, []).
%% @hidden
access_request1(<<Chunk:253/binary, Rest/binary>>, Socket, Address,
		Port, NasId, UserName, Secret, MAC, Auth, RadId, RadiusAttributes) ->
	AttributeList =
			radius_attributes:add(?EAPMessage, Chunk, RadiusAttributes),
	access_request1(Rest, Socket, Address, Port, NasId, UserName,
			Secret, MAC, Auth, RadId, AttributeList);
access_request1(<<>>, Socket, Address, Port, NasId, UserName, Secret, MAC,
		Auth, RadId, RadiusAttributes) ->
	access_request2(Socket, Address, Port, NasId, UserName, Secret, MAC,
		Auth, RadId, RadiusAttributes);
access_request1(Chunk, Socket, Address, Port, NasId, UserName, Secret, MAC,
		Auth, RadId, RadiusAttributes) ->
	AttributeList =
			radius_attributes:add(?EAPMessage, Chunk, RadiusAttributes),
	access_request2(Socket, Address, Port, NasId, UserName,
			Secret, MAC, Auth, RadId, AttributeList).
%% @hidden
access_request2(Socket, Address, Port, NasId, UserName, Secret, MAC,
		Auth, RadId, RadiusAttributes) ->
	A0 = radius_attributes:add(?FramedMtu, 65536, RadiusAttributes),
	A1 = radius_attributes:add(?UserName, UserName, A0),
	A2 = radius_attributes:add(?NasPort, 0, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2),
	A4 = radius_attributes:add(?CallingStationId, MAC, A3),
	A5 = radius_attributes:add(?CalledStationId,
			"FE-EF-DE-ED-CE-ED:TestSSID", A4),
	A6 = radius_attributes:add(?MessageAuthenticator, <<0:128>>, A5),
	Request1 = #radius{code = ?AccessRequest, id = RadId,
		authenticator = Auth, attributes = A6},
	ReqPacket1 = radius:codec(Request1),
	MsgAuth1 = ?HMAC(Secret, ReqPacket1),
	A7 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A6),
	Request2 = Request1#radius{attributes = A7},
	ReqPacket2 = radius:codec(Request2),
	gen_udp:send(Socket, Address, Port, ReqPacket2).

access_challenge(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessChallenge, Socket, Address, Port, Secret, RadId, ReqAuth).

access_accept(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessAccept, Socket, Address, Port, Secret, RadId, ReqAuth).

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
	RespAttr2 = radius_attributes:store(?MessageAuthenticator,
			<<0:128>>, RespAttr1),
	Resp3 = Resp2#radius{attributes = RespAttr2},
	RespPacket3 = radius:codec(Resp3),
	MsgAuth = ?HMAC(Secret, RespPacket3),
	Resp1.

get_eap(#radius{attributes = BinAttr}) ->
	Attr = radius_attributes:codec(BinAttr),
	EapMsg = radius_attributes:get_all(?EAPMessage, Attr),
	iolist_to_binary(EapMsg).

receive_ack(Socket, Address, Port, Secret, Auth, RadId) ->
	Radius = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	EapMsg = get_eap(Radius),
	#eap_packet{code = request, type = ?TTLS, identifier = EapId,
			data = TtlsData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_ttls{data = <<>>} = ocs_eap_codec:eap_ttls(TtlsData),
	EapId.

send_ack(Socket, Address, Port, NasId, UsreName, Secret,
		MAC, Auth, RadId, EapId) ->
	TtlsPacket = #eap_ttls{},
	EapData = ocs_eap_codec:eap_ttls(TtlsPacket),
	EapPacket = #eap_packet{code = response, identifier = EapId,
			type = ?TTLS, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UsreName, Secret, MAC, Auth, RadId).

ssl_handshake() ->
	receive
		{eap_tls, SslPid, Data} ->
			{SslPid, Data};
		{ssl_socket, SslSocket} ->
			SslSocket
	end.

-dialyzer({nowarn_function, prf/5}).
prf(SslSocket, Secret, Lable, Seed, WantedLength) ->
	{ok, <<MSK:64/binary, EMSK:64/binary>>} =
			ssl:prf(SslSocket, Secret , Lable, Seed, WantedLength),
	{MSK, EMSK}.

prf_seed(ClientHello, ServerHello) when is_list(ClientHello) ->
	CH = iolist_to_binary(ClientHello),
	prf_seed(CH, ServerHello);
prf_seed(ClientHello, ServerHello) when is_list(ServerHello) ->
	SH = iolist_to_binary(ServerHello),
	prf_seed(ClientHello, SH);
prf_seed(ClientHello, ServerHello) when
		is_binary(ClientHello), is_binary(ServerHello) ->
	CR = cs_random(ClientHello, ?ClientHello),
	SR = cs_random(ServerHello, ?ServerHello),
	[<<CR/binary, SR/binary>>].

cs_random(TlsRecordLayer, MsgType) ->
	<<?Handshake, _:32, MsgType, _:24, _:16,
		Rand:32/binary, _/binary>> = TlsRecordLayer,
	Rand.

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
client_service_opts(Config) ->
	[{'Origin-Host', ?config(origin_host, Config)},
			{'Origin-Realm', ?config(origin_realm, Config)},
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

send_identity_diameter(SId, AnonymousName, EapId) ->
	BinAnonymousName = list_to_binary(AnonymousName),
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = BinAnonymousName},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapMsg},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

send_legacy_nak_diameter(SId, EapId) ->
	EapPacket  = #eap_packet{code = response, type = ?LegacyNak, identifier = EapId,
			data = <<?TTLS>>},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapMsg},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	Answer.

client_hello_diameter(SId, <<?Handshake, _:32, ?ClientHello, _/binary>> = Data, EapId) ->
	EapTtls = #eap_ttls{data = Data},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	EapPayload = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'EAP-Payload' = EapPayload, 'Framed-MTU' = [65536],
			'NAS-Port-Type' = [19]},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	server_hello_diameter(Answer, SId).

server_hello_diameter(Answer, SId) ->
	[EapMsg] = Answer#diameter_eap_app_DEA.'EAP-Payload',
	#eap_packet{code = request, identifier = EapId, type = ?TTLS,
		data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsData = ocs_eap_codec:eap_ttls(EapData),
	server_hello_diameter1(TtlsData, EapId, SId, <<>>).

server_hello_diameter1(#eap_ttls{more = true, data = SH}, EapId, SId, Buf) ->
	EapMsg = send_ack_diameter(EapId, SId),
	#eap_packet{code = request, identifier = NewEapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsPacket = ocs_eap_codec:eap_ttls(EapData),
	server_hello_diameter1(TtlsPacket, NewEapId, SId, <<SH/binary, Buf/binary>>);
server_hello_diameter1(#eap_ttls{data = SH}, EapId, _, Buf) ->
	{EapId, <<SH/binary, Buf/binary>>}.

send_ack_diameter(EapId, SId) ->
	TtlsPacket = #eap_ttls{},
	EapData = ocs_eap_codec:eap_ttls(TtlsPacket),
	EapPacket = #eap_packet{code = response, identifier = EapId,
			type = ?TTLS, data = EapData},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'EAP-Payload' = EapMsg},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	[EapMsg] = Answer#diameter_eap_app_DEA.'EAP-Payload',
	EapMsg.

client_cipher_diameter(Data, UserName, SId, EapId) ->
	client_cipher_diameter1(Data, UserName, SId, EapId, <<>>).
%% @hidden
client_cipher_diameter1(<<?Handshake, _:32, ?ClientKeyExchange, _/binary>>  = Data,
		UserName, SId, EapId, Buff) ->
	client_cipher_diameter2(Data, UserName, SId, EapId, Buff);
client_cipher_diameter1(<<?ChangeCipherSpec, _:32, 1, _/binary>>  = Data,
		UserName, SId, EapId, Buff) ->
	client_cipher_diameter2(Data, UserName, SId, EapId, Buff);
client_cipher_diameter1(<<?Handshake, _/binary>>  = Data,
		UserName, SId, EapId, Buff) ->
	ClientCipher = <<Buff/binary, Data/binary>>,
	client_cipher_diameter3(ClientCipher, UserName, SId, EapId).
%% @hidden
client_cipher_diameter2(<<_:24, L1:16, _/binary>>  = Data, UserName, SId, EapId, Buff) ->
	case Data of
		<<_:40, _:L1/binary>> ->
			{_SslPid2, Rest} = ssl_handshake(),
			Chunk = <<Buff/binary, Data/binary>>,
			client_cipher_diameter1(Rest, UserName, SId, EapId, Chunk);
		<<_:40, _:L1/binary, Rest/binary>> = Payload ->
			BlockSize = L1 + 5,
			<<CCBlock:BlockSize/binary, Rest/binary>> = Payload,
			Chunk = <<Buff/binary, CCBlock/binary>>,
			client_cipher_diameter1(Rest, UserName, SId, EapId, Chunk)
	end.
%% @hidden
client_cipher_diameter3(<<Chunk:1386/binary, Rest/binary>> = Data, UserName, SId, EapId) ->
	Size = size(Data),
	EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	{NewEapId, _TtlsData} = access_request_diameter(EapPacket, UserName, SId, EapId),
	client_cipher_diameter3(Rest, UserName, SId, NewEapId);
client_cipher_diameter3(Chunk, UserName, SId, EapId) ->
	EapTtls = #eap_ttls{data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	{NewEapId, TtlsData} = access_request_diameter(EapPacket, UserName, SId, EapId),
	{NewEapId, TtlsData }.

access_request_diameter(EapPacket, UserName, SId, _EapId) ->
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'EAP-Payload' = EapMsg, 'Framed-MTU' = [65536],
			'NAS-Port-Type' = [19], 'User-Name' = [UserName]},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	server_cipher_diameter(Answer, SId).

server_cipher_diameter(Answer, SId) ->
	[EapMsg] = Answer#diameter_eap_app_DEA.'EAP-Payload',
	#eap_packet{code = request, identifier = EapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsData = ocs_eap_codec:eap_ttls(EapData),
	server_cipher_diameter1(TtlsData, SId, EapId, <<>>).
%% @hidden
server_cipher_diameter1(#eap_ttls{more = true, data = SC}, SId, EapId, Buf) ->
	EapMsg = send_ack_diameter(EapId, SId),
	#eap_packet{code = request, identifier = NewEapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsPacket = ocs_eap_codec:eap_ttls(EapData),
	server_hello_diameter1(TtlsPacket, NewEapId, SId, <<Buf/binary, SC/binary>>);
server_cipher_diameter1(#eap_ttls{data = SC}, _SId, EapId, Buf) ->
	{EapId, <<Buf/binary, SC/binary>>}.

client_passthrough_diameter(SslSocket, UserName, Password, SId, EapId) ->
	UN = #diameter_avp{code = ?UserName, is_mandatory = true,
			data = UserName},
	PW = #diameter_avp{code = ?UserPassword, is_mandatory = true,
			data = Password},
	AVPs = list_to_binary(lists:map(fun diameter_codec:pack_avp/1,
			[UN, PW])),
	ok = ssl:send(SslSocket, AVPs),
	{_SslPid, UserCredential} = ssl_handshake(),
	TtlsPacket = #eap_ttls{data = UserCredential},
	TtlsData = ocs_eap_codec:eap_ttls(TtlsPacket),
	EapPacket = #eap_packet{code = response, identifier = EapId, type = ?TTLS,
		data = TtlsData},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	DER = #diameter_eap_app_DER{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'EAP-Payload' = EapMsg, 'Framed-MTU' = [65536],
			'NAS-Port-Type' = [19], 'User-Name' = [UserName]},
	{ok, Answer} = diameter:call(?MODULE, eap_app_test, DER, []),
	server_passthrough_diameter(Answer, SId).

server_passthrough_diameter(Answer, SId) ->
	SIdbin = list_to_binary(SId),
	%[EapMsg] = Answer#diameter_eap_app_DEA.'EAP-Payload',
	#diameter_eap_app_DEA{'Session-Id' = SIdbin,
		'Auth-Application-Id' = ?EAP_APPLICATION_ID,
		'Auth-Request-Type' =  ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', 
		'EAP-Payload' = [EapMsg]} = Answer,
	#eap_packet{code = success, identifier = _EapId} = ocs_eap_codec:eap_packet(EapMsg),
	ok.

encrypt_key(Secret, RequestAuthenticator, Salt, Key)
	when (Salt bsr 15) == 1 ->
	KeyLength = size(Key),
	Plaintext = case (KeyLength + 1) rem 16 of
		0 ->
			<<KeyLength, Key/binary>>;
		N ->
			PadLength = (16 - N) * 8,
			<<KeyLength, Key/binary, 0:PadLength>>
	end,
	F = fun(P, [H | _] = Acc) ->
				B = crypto:hash(md5, [Secret, H]),
				C = crypto:exor(P, B),
				[C | Acc]
	end,
	AccIn = [[RequestAuthenticator, <<Salt:16>>]],
	AccOut = lists:foldl(F, AccIn, [P || <<P:16/binary>> <= Plaintext]),
	iolist_to_binary(tl(lists:reverse(AccOut))).

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
