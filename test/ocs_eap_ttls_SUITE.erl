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

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
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
	ok = ocs_test_lib:initialize_db(),
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
erlang:display({config, Config}),
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
	EapId2 = EapId1 + 1,
	{EapId2, Token, ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	ok = send_legacy_nak(Socket, Address, Port, NasId, AnonymousName,
			Secret, MAC, ReqAuth2, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	EapId3 = receive_ttls_start(Socket, Address,Port, Secret, ReqAuth2,
					RadId2),
	ok = ssl:start(),
	Options  = [{cacertfile, "/home/prahveen/ocs/ocs.build/priv/CAcert.crt"}],
	{ok, SslSocket} = peer_ttls_transport:ssl_connect(Address, 0, Options).

send_identity(Socket, Address, Port, NasId,
		AnonymousName, Secret, MAC, Auth, EapId, RadId) ->
	BinAnonymousName = list_to_binary(AnonymousName),
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = BinAnonymousName},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request(Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, Auth, RadId, EapMsg).

receive_id(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none, token = Token,
			identity = ServerID} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	{EapId, Token, ServerID}.

send_legacy_nak(Socket, Address, Port, NasId,
		AnonymousName, Secret, MAC, Auth, EapId, RadId) ->
	EapPacket  = #eap_packet{code = response,
			type = ?LegacyNak, identifier = EapId,
			data = <<?TTLS>>},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request(Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, Auth, RadId, EapMsg).

receive_ttls_start(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?TTLS, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_ttls{start = true} = ocs_eap_codec:eap_ttls(EapData),
	EapId.

access_request(Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId, EapMsg) ->
	A = radius_attributes:new(),
	A0 = radius_attributes:add(?FramedMtu, 65536, A),
	A1 = radius_attributes:add(?UserName, UserName, A0),
	A2 = radius_attributes:add(?NasPort, 0, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2),
	A4 = radius_attributes:add(?CallingStationId, MAC, A3),
	A5 = radius_attributes:add(?EAPMessage, EapMsg, A4),
	A6 = radius_attributes:add(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16,0)), A5),
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
	{ok, EapMsg} = radius_attributes:find(?EAPMessage, RespAttr1),
	EapMsg.

