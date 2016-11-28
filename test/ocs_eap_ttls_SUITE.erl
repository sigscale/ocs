%%% ocs_eap_ttls_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
%%%  Test suite for the EAP-TTLS Authentication.
%%%
-module(ocs_eap_ttls_SUITE).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

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

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "This suite tests the EAP-TTLS Authentication."}]},
	{timetrap, {seconds, 8}},
	{require, radius_shared_secret},{default_config, radius_shared_secret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	DataDir = ?config(data_dir, Config),
	ok = ocs_test_lib:initialize_db(),
	ok = application:set_env(ocs, tls_key,
			DataDir ++ "ct-key.pem", [{persistent, true}]), 
	ok = application:set_env(ocs, tls_cert,
			DataDir ++ "ct-cert.pem", [{persistent, true}]), 
	ok = application:set_env(ocs, tls_cacert,
			DataDir ++ "CAcert.pem", [{persistent, true}]), 
	ok = ocs_test_lib:start(),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_secret),
	ok = ocs:add_client(AuthAddress, SharedSecret),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}] ++ Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	{ok, IP} = application:get_env(ocs, radius_auth_addr),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, IP}, binary]),
	[{socket, Socket} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
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
	[eap_ttls_authentication].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
eap_ttls_authentication() ->
	[{userdata, [{doc, "EAP-TTLS Authentication"}]}].

eap_ttls_authentication(Config) ->
	DataDir = ?config(data_dir, Config),
	AnonymousName = "DonaldTrump",
	Subscriber = <<"45678901">>,
	MAC = "dd:ee:ff:aa:bb:cc",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(Subscriber, PeerAuth, [], 10000),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 8, EapId1 = 1,
	ok = send_identity(Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, ReqAuth1, EapId1, RadId1),
	{RadId2, EapId2} = receive_id(Socket, Address, Port, Secret,
			ReqAuth1, RadId1),
	ReqAuth2 = radius:authenticator(),
	ok = send_legacy_nak(Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, ReqAuth2, EapId2, RadId2),
	{RadId3, EapId3} = receive_ttls_start(Socket, Address,Port, Secret, ReqAuth2,
					RadId2),
	Options  = [{cacertfile, DataDir ++ "CAcert.pem"}],
	proc_lib:spawn_link(peer_ttls_transport, ssl_connect, [Address, self(), Options]),
	{SslPid1, ClientHelloMsg} = ssl_handshake(),
	ReqAuth3 = radius:authenticator(),
	{RadId4, CHAuth} = client_hello(ClientHelloMsg, Socket, Address, Port,
			NasId, AnonymousName, Secret, MAC, ReqAuth3, EapId3, RadId3),
	{RadId5, EapId4, ServerHello} = server_hello(Socket, Address, Port,
			NasId, AnonymousName, Secret, MAC, CHAuth, RadId4),
	peer_ttls_transport:deliver(SslPid1, self(), ServerHello),
	{SslPid2, CKeyExchange} = ssl_handshake(),
	{SslPid2, CChangeCipher} = ssl_handshake(),
	{SslPid2, Finish} = ssl_handshake(),
	ClientCipher = iolist_to_binary([CKeyExchange, CChangeCipher, Finish]),
	ReqAuth5 = radius:authenticator(), 
	{RadId6, CCAuth} = client_cipher(ClientCipher, Socket, Address, Port,
			NasId, AnonymousName, Secret, MAC, ReqAuth5, EapId4, RadId5),
	{RadId7, EapId5, ServerCipher} = server_cipher(Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, CCAuth, RadId6),
	peer_ttls_transport:deliver(SslPid1, self(), 
			iolist_to_binary(ServerCipher)),
	[<<?Handshake, _:32>>, [[?ClientHello, _,
			<<_:16, CR:32/binary, _/binary>>]| _ ]] = ClientHelloMsg,
	<<?Handshake, _:32, ?ServerHello, _:24,
			_:16, SR:32/binary,  _/binary>> = list_to_binary(ServerHello),  
	SslSocket = ssl_handshake(),
	Seed = <<CR/binary, SR/binary>>,
	{ok, <<MSK:64/binary, EMSK:64/binary>>} = ssl:prf(SslSocket,
			master_secret , <<"ttls keying material">>, Seed, 128),
	ReqAuth6 = radius:authenticator(),
	{RadId7, CPAuth} = client_passthrough(SslSocket, Subscriber, PeerAuth,
			Socket, Address, Port, NasId, Secret, MAC, ReqAuth6, EapId5, RadId7),
	ok = server_passthrough(Socket, Address, Port, NasId, UserName, Secret,
			MAC, CPAuth, RadId7),
	ok = ssl:close(SslSocket).
	
send_identity(Socket, Address, Port, NasId, AnonymousName, Secret, MAC,
		Auth, EapId, RadId) ->
	BinAnonymousName = list_to_binary(AnonymousName),
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = BinAnonymousName},
	access_request(EapPacket, Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, Auth, RadId).

receive_id(Socket, Address, Port, Secret, Auth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	NewRadId = RadId + 1,
	{NewRadId, EapId}.

send_legacy_nak(Socket, Address, Port, NasId, AnonymousName, Secret, MAC,
		Auth, EapId, RadId) ->
	EapPacket  = #eap_packet{code = response, type = ?LegacyNak, identifier = EapId,
			data = <<?TTLS>>},
	access_request(EapPacket, Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, Auth, RadId).

receive_ttls_start(Socket, Address, Port, Secret, Auth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port, Secret, RadId, Auth),
	#eap_packet{code = request, type = ?TTLS, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_ttls{start = true} = ocs_eap_codec:eap_ttls(EapData),
	NewRadId = RadId + 1,
	{NewRadId, EapId}.
 
client_hello([<<?Handshake, _/binary>>, [[?ClientHello | _] | _]] = Data,
		Socket, Address, Port, NasId, UserName, Secret, MAC,
		Auth, EapId, RadId) ->
	ClientHelloMsg = iolist_to_binary(Data),
	client_hello1(ClientHelloMsg, Socket, Address, Port, NasId, UserName,
			Secret, MAC, Auth, EapId, RadId).
%% @hidden
client_hello1(<<Chunk:1386/binary, Rest/binary>> = Data, Socket, Address,
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
	client_hello1(Rest, Socket, Address, Port, NasId, UserName,
			Secret, MAC, NewAuth, NewEapId, NewRadId);
client_hello1(Chunk, Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, EapId, RadId) -> 
	EapTtls = #eap_ttls{data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId),
	{RadId, Auth}.

server_hello(Socket, Address, Port, NasId, UserName, Secret,
		MAC, Auth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	#eap_packet{code = request, identifier = EapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsData = ocs_eap_codec:eap_ttls(EapData),
	server_hello1(TtlsData, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapId, []).
%% @hidden
server_hello1(#eap_ttls{more = true, data = SH}, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, RadId, EapId, Buf) ->
	send_ack(Socket, Address, Port, NasId, UserName, Secret,
			MAC, Auth, RadId, EapId),
	EapMsg = access_challenge(Socket, Address, Port, Secret, RadId, Auth),
	#eap_packet{code = request, identifier = NewEapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsPacket = ocs_eap_codec:eap_ttls(EapData),
	NewRadId = RadId + 1,
	NewAuth = radius:authenticator(),
	server_hello1(TtlsPacket, Socket, Address, Port, NasId,
		UserName, Secret, MAC, NewAuth, NewRadId, NewEapId, [SH | Buf]);
server_hello1(#eap_ttls{data = SH}, Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId, EapId, Buf) ->
	%send_ack(Socket, Address, Port, NasId, UserName, Secret,
	%		MAC, Auth, RadId, EapId),
	NewRadId = RadId + 1,
	{NewRadId, EapId, [SH | Buf]}.

client_cipher(<<Chunk:1386/binary, Rest/binary>> = Data, Socket, Address, Port,
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
	client_cipher(Rest, Socket, Address, Port, NasId, UserName,
			Secret, MAC, NewAuth, NewEapId, NewRadId);
client_cipher(Chunk, Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, EapId, RadId) -> 
	EapTtls = #eap_ttls{data = Chunk},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = EapData},
	access_request(EapPacket, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId),
	{RadId, Auth}.
	
server_cipher(Socket, Address, Port, NasId, UserName,
		Secret, MAC, Auth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
	#eap_packet{code = request, identifier = EapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsData = ocs_eap_codec:eap_ttls(EapData),
	server_cipher1(TtlsData, Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapId, []).
%% @hidden
server_cipher1(#eap_ttls{more = true, data = SH}, Socket, Address, Port,
		NasId, UserName, Secret, MAC, Auth, RadId, EapId, Buf) ->
	send_ack(Socket, Address, Port, NasId, UserName, Secret,
			MAC, Auth, RadId, EapId),
	EapMsg = access_challenge(Socket, Address, Port, Secret, RadId, Auth),
	#eap_packet{code = request, identifier = NewEapId, type = ?TTLS,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	TtlsPacket = ocs_eap_codec:eap_ttls(EapData),
	NewRadId = RadId + 1,
	NewAuth = radius:authenticator(),
	server_hello1(TtlsPacket, Socket, Address, Port, NasId,
		UserName, Secret, MAC, NewAuth, NewRadId, NewEapId, [SH | Buf]);
server_cipher1(#eap_ttls{data = SH}, Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId, EapId, Buf) ->
	%send_ack(Socket, Address, Port, NasId, UserName, Secret,
	%		MAC, Auth, RadId, EapId),
	NewRadId = RadId + 1,
	{NewRadId, EapId, [SH | Buf]}.
	
%% @todo enoded AVPs
client_passthrough(SslSocket, UserName, Password, Socket, Address, Port,
		NasId, Secret, MAC, Auth, EapId, RadId) ->
	UN = #diameter_avp{code = ?UserName, is_mandatory = true,
			data = UserName},
	PW = #diameter_avp{code = ?UserPassword, is_mandatory = true,
			data = Password},
	AVPs = list_to_binary(lists:map(fun diameter_codec:pack_avp/1,
			[UN, PW])),
	ok = ssl:send(SslSocket, AVPs),
	{SslPid, UserCredential} = ssl_handshake(),
	TtlsPacket = #eap_ttls{data = iolist_to_binary(UserCredential)},
	TtlsData = ocs_eap_codec:eap_ttls(TtlsPacket),
	EapPacket = #eap_packet{code = response, identifier = EapId, type = ?TTLS,
		data = TtlsData},
	access_request(EapPacket, Socket, Address, Port, NasId,
		binary_to_list(UserName), Secret, MAC, Auth, RadId),
	{RadId, Auth}.

server_passthrough(Socket, Address, Port, NasId, UserName, Secret,
			MAC, Auth, RadId) ->
	EapMsg = access_accept(Socket, Address, Port,
			Secret, RadId, Auth),
	#eap_packet{code = success, identifier = EapId} = ocs_eap_codec:eap_packet(EapMsg),
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
	A6 = radius_attributes:add(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16,0)), A4),
	Request1 = #radius{code = ?AccessRequest, id = RadId,
		authenticator = Auth, attributes = A6},
	ReqPacket1 = radius:codec(Request1),
	MsgAuth1 = crypto:hmac(md5, Secret, ReqPacket1),
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
			list_to_binary(lists:duplicate(16, 0)), RespAttr1),
	Resp3 = Resp2#radius{attributes = RespAttr2},
	RespPacket3 = radius:codec(Resp3),
	MsgAuth = crypto:hmac(md5, Secret, RespPacket3),
	EapMsg = radius_attributes:get_all(?EAPMessage, RespAttr1),
	iolist_to_binary(EapMsg).

receive_ack(Socket, Address, Port, Secret, Auth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, Auth),
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
		{eap_ttls, SslPid, Data} ->
			{SslPid, Data};
		{ssl_socket, SslSocket} ->
			SslSocket
	end.
