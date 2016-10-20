%%% ocs_eap_SUITE.erl
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
%%%  Test suite for the ocs API.
%%%
-module(ocs_eap_SUITE).
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
	[{userdata, [{doc, "This suite tests the application's API."}]},
	{timetrap, {seconds, 8}},
	{require, radius_username}, {default_config, radius_username, "ocs"},
	{require, radius_password}, {default_config, radius_password, "ocs123"},
	{require, radius_shared_secret},{default_config, radius_shared_secret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_lib:initialize_db(),
	ok = ocs_lib:start(),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_secret),
	ok = ocs:add_client(AuthAddress, SharedSecret),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}] ++ Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_lib:stop(),
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
	[eap_identity, pwd_id, pwd_commit, pwd_confirm,
			message_authentication, validate_eap_code, validate_pwd_id_token].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
eap_identity() ->
   [{userdata, [{doc, "Send an EAP-Identity/Response to peer"}]}].

eap_identity(Config) ->
	MAC = "aa:bb:cc:dd:ee:ff",
	PeerId = <<"12345678">>,
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth = radius:authenticator(),
	RadId = 1, EapId = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth, EapId, RadId),
	NextEapId = EapId + 1,
	{NextEapId, _Token, _ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth, RadId).

pwd_id() ->
   [{userdata, [{doc, "Send an EAP-pwd-ID/Response to peer"}]}].

pwd_id(Config) ->
	PeerId = <<"23456789">>,
	MAC = "bb:cc:dd:ee:ff:aa",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 2, EapId1 = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, _ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	send_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, Token, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	{EapId3, _ElementS, _ScalarS} = receive_commit(Socket, Address,
			Port, Secret, ReqAuth2, RadId2).

pwd_commit() ->
	[{userdata, [{doc, "Send an EAP-pwd-Commit/Response to peer"}]}].

pwd_commit(Config) ->
	PeerId = <<"34567890">>,
	MAC = "cc:dd:ee:ff:aa:bb",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 5, EapId1 = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	send_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, Token, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	{EapId3, _ElementS, _ScalarS} = receive_commit(Socket, Address,
			Port, Secret, ReqAuth2, RadId2),
	Prand = crypto:rand_uniform(1, ?R),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerID, PeerAuth),
	{ScalarP, ElementP} = ocs_eap_pwd:compute_scalar(<<Prand:256>>, PWE),
	RadId3 = RadId2 + 1,
	ReqAuth3 = radius:authenticator(),
	ok = send_commit(Socket, Address, Port, Secret, ReqAuth3,
			UserName, NasId, MAC, ScalarP, ElementP, EapId3, RadId3),
	EapId4 = EapId3 + 1,
	{EapId4, _ConfirmS} = receive_confirm(Socket,
			Address, Port, Secret, ReqAuth3, RadId3).
	
pwd_confirm() ->
	[{userdata, [{doc, "Send an EAP-pwd-Confirm/Response to peer"}]}].

pwd_confirm(Config) ->
	PeerId = <<"45678901">>,
	MAC = "dd:ee:ff:aa:bb:cc",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 8, EapId1 = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	send_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, Token, EapId2, RadId2),
	EapId3 = EapId2 + 1,
	{EapId3, ElementS, ScalarS} = receive_commit(Socket, Address,
			Port, Secret, ReqAuth2, RadId2),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerID, PeerAuth),
	Prand = crypto:rand_uniform(1, ?R),
	{ScalarP, ElementP} = ocs_eap_pwd:compute_scalar(<<Prand:256>>, PWE),
	RadId3 = RadId2 + 1,
	ReqAuth3 = radius:authenticator(),
	ok = send_commit(Socket, Address, Port, Secret, ReqAuth3,
			UserName, NasId, MAC, ScalarP, ElementP, EapId3, RadId3),
	EapId4 = EapId3 + 1,
	{EapId4, _ConfirmS} = receive_confirm(Socket,
			Address, Port, Secret, ReqAuth3, RadId3),
	Ciphersuite = <<19:16, 1, 1>>,
	Kp = ocs_eap_pwd:compute_ks(<<Prand:256>>, PWE, ScalarS, ElementS),
	Input = [Kp, ElementP, ScalarP, ElementS, ScalarS, Ciphersuite],
	ConfirmP = ocs_eap_pwd:h(Input),
	RadId4 = RadId3 + 1,
	ReqAuth4 = radius:authenticator(),
	send_confirm(Socket, Address, Port, Secret, ReqAuth4, UserName,
			NasId, MAC, ConfirmP, EapId4, RadId4),
	EapId4 = receive_success(Socket, Address, Port, Secret, ReqAuth4, RadId4).

message_authentication() ->
	[{userdata, [{doc, "Send corrupt Message-Authenticator"}]}].

message_authentication(Config) ->
	PeerId = <<"56789012">>,
	MAC = "ee:ff:aa:bb:cc:dd",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = "bogus",
	ReqAuth = radius:authenticator(),
	RadId = 13, EapId = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth, EapId, RadId),
	{error, timeout} = gen_udp:recv(Socket, 0, 2000).

validate_eap_code() ->
	[{userdata, [{doc, "Send invalid EAP (bad code)"}]}].

validate_eap_code(Config) ->
	PeerId = <<"67890123">>,
	MAC = "ff:aa:bb:cc:dd:ee",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 14, EapId1 = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, Token, _ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	InvalidEapPacket  = #eap_packet{code = request, type = ?PWD,
			identifier = EapId2, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(InvalidEapPacket),
	ok = access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth2, RadId2, EapMsg),
	{error, timeout} = gen_udp:recv(Socket, 0, 2000).

validate_pwd_id_token() ->
	[{userdata, [{doc, "Send invalid EAP-pwd-ID (bad token)"}]}].

validate_pwd_id_token(Config) ->
	PeerId = <<"78901234">>,
	MAC = "ab:cd:ef:fe:dc:ba",
	PeerAuth = list_to_binary(ocs:generate_password()),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config),
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId1 = 16, EapId1 = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId1, RadId1),
	EapId2 = EapId1 + 1,
	{EapId2, _Token, _ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId1),
	RadId2 = RadId1 + 1,
	ReqAuth2 = radius:authenticator(),
	InvalidToken = crypto:rand_bytes(4),
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = InvalidToken, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = request, type = ?PWD,
			identifier = EapId2, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	ok = access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth2, RadId2, EapMsg),
	EapId2 = receive_failure(Socket, Address, Port, Secret, ReqAuth2, RadId2).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

access_request(Socket, Address, Port, NasId,
		UserName, Secret, MAC, Auth, RadId, EapMsg) ->
	A0 = radius_attributes:new(),
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

access_accept(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessAccept, Socket, Address, Port, Secret, RadId, ReqAuth).

access_reject(Socket, Address, Port, Secret, RadId, ReqAuth) ->
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
	RespAttr2 = radius_attributes:store(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16, 0)), RespAttr1),
	Resp3 = Resp2#radius{attributes = RespAttr2},
	RespPacket3 = radius:codec(Resp3),
	MsgAuth = crypto:hmac(md5, Secret, RespPacket3),
	{ok, EapMsg} = radius_attributes:find(?EAPMessage, RespAttr1),
	EapMsg.

send_identity(Socket, Address, Port, NasId,
		UserName, Secret, PeerId, MAC, Auth, EapId, RadId) ->
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = PeerId},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

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

send_id(Socket, Address, Port, Secret, Auth, UserName,
		NasId, PeerId, MAC, Token, EapId, RadId) ->
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD, identifier = EapId,
			data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_commit(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_commit{element = ElementS,
			scalar = ScalarS} = ocs_eap_codec:eap_pwd_commit(EapPwdData),
	{EapId, ElementS, ScalarS}.

send_commit(Socket, Address, Port, Secret, Auth, UserName,
		NasId, MAC, ScalarP, ElementP, EapId, RadId) ->
	EapPwdCommit = #eap_pwd_commit{scalar = ScalarP, element = ElementP},
	EapPwd = #eap_pwd{length = false, more = false, pwd_exch = commit,
			data = ocs_eap_codec:eap_pwd_commit(EapPwdCommit)},
	EapPacket = #eap_packet{code = response, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_confirm(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
			data = ConfirmS} = ocs_eap_codec:eap_pwd(EapData),
	{EapId, ConfirmS}.

send_confirm(Socket, Address, Port, Secret, Auth, UserName,
		NasId, MAC, ConfirmP, EapId, RadId) ->
	EapPwd = #eap_pwd{length = false, more = false,
			pwd_exch = confirm, data = ConfirmP},
	EapPacket = #eap_packet{code = response, type = ?PWD,
			identifier = EapId, data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, Auth, RadId, EapMsg).

receive_success(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = access_accept(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = success,
			identifier = EapId} = ocs_eap_codec:eap_packet(EapMsg),
	EapId.

receive_failure(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	EapMsg = access_reject(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = failure,
			identifier = EapId} = ocs_eap_codec:eap_packet(EapMsg),
	EapId.

