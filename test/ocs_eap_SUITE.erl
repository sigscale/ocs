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
	unknown_authenticator, invalid_id_response_eap_packet, invalid_id_response_eap_pwd].

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
	{NextEapId, _ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth, RadId).

pwd_id() ->
   [{userdata, [{doc, "Send an EAP-pwd-ID/Response to peer"}]}].

pwd_id(Config) ->
	PeerId = <<"23456789">>,
	MAC = "bb:cc:dd:ee:ff:aa",
	PeerAuth = ocs:generate_password(),
	ok = ocs:add_subscriber(PeerId, PeerAuth, []),
	Socket = ?config(socket, Config), 
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	NasId = ?config(nas_id, Config),
	UserName = ct:get_config(radius_username),
	Secret = ct:get_config(radius_shared_secret),
	ReqAuth1 = radius:authenticator(),
	RadId = 2, EapId = 1,
	ok = send_identity(Socket, Address, Port, NasId, UserName,
			Secret, PeerId, MAC, ReqAuth1, EapId, RadId),
	NextEapId = EapId + 1,
	{NextEapId, _ServerID} = receive_id(Socket, Address,
			Port, Secret, ReqAuth1, RadId),
	ReqAuth2 = radius:authenticator(),
	send_id(Socket, Address, Port, Secret,
			ReqAuth2, UserName, NasId, PeerId, MAC, NextEapId, RadId + 1),
	{ok, {_Address, _Port, RespPacket1}} = gen_udp:recv(Socket, 0),
	Response1 = radius:codec(RespPacket1),
	#radius{code = ?AccessChallenge, attributes = Attributes} = Response1,
	A1 = radius_attributes:codec(Attributes),
	MsgAuth2 = radius_attributes:fetch(?MessageAuthenticator, A1),
	A2 = radius_attributes:store(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16,0)), A1),
	Response2 = Response1#radius{authenticator = ReqAuth2, attributes = A2},
	RespPacket2 = radius:codec(Response2),
	MsgAuth2 = crypto:hmac(md5, Secret, RespPacket2),
	EapMessage2 = radius_attributes:fetch(?EAPMessage, A1),
	#eap_packet{code = request, type = ?PWD, identifier = _,
			data = EapData} = ocs_eap_codec:eap_packet(EapMessage2),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
		data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_commit{} = ocs_eap_codec:eap_pwd_commit(EapPwdData).

pwd_commit() ->
	[{userdata, [{doc, "Send an EAP-pwd-Commit/Response to peer"}]}].

pwd_commit(Config) ->
	Id = 1,
	PeerId = ?config(peer_id, Config),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	Socket = ?config(socket, Config), 
	UserName = ct:get_config(radius_username),
	SharedSecret = ct:get_config(radius_shared_secret),
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:store(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:store(?NasPort, 1, IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:store(?NasIdentifier, "tomba1", IDReqAttributeList2),
	IDReqAttributeList4 = radius_attributes:store(?CallingStationId,"de:ad:be:ef:ca:fe",
		IDReqAttributeList3),
	IDReqAttributeList5 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), IDReqAttributeList4),
	IDRequest1 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList5},
	IDRequestPacket1 = radius:codec(IDRequest1),
	IDMsgAuth = crypto:hmac(md5, SharedSecret, IDRequestPacket1),
	IDReqAttributeList6 = radius_attributes:store(?MessageAuthenticator, IDMsgAuth, IDReqAttributeList5),
	IDRequest2 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList6},
	IDRequestPacket2 = radius:codec(IDRequest2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDRequestPacket2),
	{ok, {AuthAddress, AuthPort, IdReqPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = Id, authenticator = _IDReqAuthenticator,
		attributes = BinIDReqAttributes} = radius:codec(IdReqPacket),
	IDReqAttributes = radius_attributes:codec(BinIDReqAttributes),
	{ok, IDEAPPacket} = radius_attributes:find(?EAPMessage, IDReqAttributes),
	#eap_packet{code = request, type = ?PWD, identifier = IDEAPId, data = IDData} =
		ocs_eap_codec:eap_packet(IDEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDReqData} = ocs_eap_codec:eap_pwd(IDData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1, token = Token,
		pwd_prep = none, identity = ServerID} = ocs_eap_codec:eap_pwd_id(IDReqData),
	IDRespBody = #eap_pwd_id{token = Token, pwd_prep = none, identity = PeerId},
	IDRespBodyData = ocs_eap_codec:eap_pwd_id(IDRespBody),
	IDRespHeader = #eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDRespBodyData},
	IDEAPData = ocs_eap_codec:eap_pwd(IDRespHeader),
	IDRespPacket = #eap_packet{code = response, type = ?PWD, identifier = IDEAPId, data = IDEAPData},
	IDEAPPacketData = ocs_eap_codec:eap_packet(IDRespPacket),
	IDRespAttributeList1 = radius_attributes:store(?EAPMessage, IDEAPPacketData, IDReqAttributeList5),
	Authenticator2 = radius:authenticator(),
	IDResponse1 = #radius{code = ?AccessRequest, id = 2,  authenticator = Authenticator2,
		attributes = IDRespAttributeList1},
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResponse2 = #radius{code = ?AccessRequest, id = 2, authenticator = Authenticator2,
		attributes = IDRspAttributeList3},
	IDResPacket2 = radius:codec(IDResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDResPacket2),
	{ok, {_Address, _Port, CommitPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = 2, authenticator = _RAuthenticator,
		attributes = CommitReqAttributes} = radius:codec(CommitPacket),
	CommitReqAtt = radius_attributes:codec(CommitReqAttributes),
	{ok, CommitEAPPacket} = radius_attributes:find(?EAPMessage, CommitReqAtt),
	#eap_packet{code = request, type = ?PWD, identifier = EAPId2, data = CommitData} =
		ocs_eap_codec:eap_packet(CommitEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitReqData} = ocs_eap_codec:eap_pwd(CommitData),
	#eap_pwd_commit{element = _Element_S, scalar = _Scalar_S} = ocs_eap_codec:eap_pwd_commit(CommitReqData),
	P_Rand = crypto:rand_uniform(1, ?R),
	{ok, Password, _Attr, _Balance} = ocs:find_subscriber(PeerId),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerID, Password),
	{Scalar_P, Element_P} = ocs_eap_pwd:compute_scalar(<<P_Rand:256>>, PWE),
	CommitRespBody = #eap_pwd_commit{scalar = Scalar_P, element = Element_P},
	CommitRespBodyData = ocs_eap_codec:eap_pwd_commit(CommitRespBody),
	CommitRespHeader = #eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitRespBodyData},
	CommitEAPData = ocs_eap_codec:eap_pwd(CommitRespHeader),
	CommitRespPacket = #eap_packet{code = response, type = ?PWD, identifier = EAPId2, data = CommitEAPData},
	CommitRespPacketData = ocs_eap_codec:eap_packet(CommitRespPacket),
	CommitAttributeList1 = radius_attributes:store(?EAPMessage,
	   CommitRespPacketData, IDReqAttributeList5),
	Authenticator3 = radius:authenticator(),
	CommitResponse1 = #radius{code = ?AccessRequest, id = 3, authenticator = Authenticator3,
		attributes = CommitAttributeList1},
	CommitReqPacket1 = radius:codec(CommitResponse1),
	CommitRespMsgAuth = crypto:hmac(md5, SharedSecret, CommitReqPacket1),
	CommitAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		CommitRespMsgAuth, CommitAttributeList1),
	CommitResponse2= #radius{code = ?AccessRequest, id = 3, authenticator = Authenticator3,
		attributes = CommitAttributeList3},
	CommitReqPacket2 = radius:codec(CommitResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, CommitReqPacket2),
	{ok, {_Address, _Port, _ConfirmPacket}} = gen_udp:recv(Socket, 0),
	ok =  gen_udp:close(Socket).
	
pwd_confirm() ->
	[{userdata, [{doc, "Send an EAP-pwd-Conform/Response to peer"}]}].

pwd_confirm(Config) ->
	Id = 1,
	PeerId = ?config(peer_id, Config),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	Socket = ?config(socket, Config),
	UserName = ct:get_config(radius_username),
	SharedSecret = ct:get_config(radius_shared_secret),
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:store(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:store(?NasPort, 2, IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:store(?NasIdentifier, "tomba2", IDReqAttributeList2),
	IDReqAttributeList4 = radius_attributes:store(?CallingStationId,"de:ad:be:ef:ca:fe",
		IDReqAttributeList3),
	IDReqAttributeList5 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), IDReqAttributeList4),
	IDRequest1 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList5},
	IDRequestPacket1 = radius:codec(IDRequest1),
	IDMsgAuth = crypto:hmac(md5, SharedSecret, IDRequestPacket1),
	IDReqAttributeList6 = radius_attributes:store(?MessageAuthenticator, IDMsgAuth, IDReqAttributeList5),
	IDRequest2 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList6},
	IDRequestPacket2 = radius:codec(IDRequest2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDRequestPacket2),
	{ok, {AuthAddress, AuthPort, IdReqPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = Id, authenticator = _IDReqAuthenticator,
		attributes = BinIDReqAttributes} = radius:codec(IdReqPacket),
	IDReqAttributes = radius_attributes:codec(BinIDReqAttributes),
	{ok, IDEAPPacket} = radius_attributes:find(?EAPMessage, IDReqAttributes),
	#eap_packet{code = request, type = ?PWD, identifier = IDEAPId, data = IDData} =
		ocs_eap_codec:eap_packet(IDEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDReqData} = ocs_eap_codec:eap_pwd(IDData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1, token = Token,
		pwd_prep = none, identity = ServerID} = ocs_eap_codec:eap_pwd_id(IDReqData),
	IDRespBody = #eap_pwd_id{token = Token, pwd_prep = none, identity = PeerId},
	IDRespBodyData = ocs_eap_codec:eap_pwd_id(IDRespBody),
	IDRespHeader = #eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDRespBodyData},
	IDEAPData = ocs_eap_codec:eap_pwd(IDRespHeader),
	IDRespPacket = #eap_packet{code = response, type = ?PWD, identifier = IDEAPId, data = IDEAPData},
	IDEAPPacketData = ocs_eap_codec:eap_packet(IDRespPacket),
	IDRespAttributeList1 = radius_attributes:store(?EAPMessage, IDEAPPacketData, IDReqAttributeList5),
	Authenticator2 = radius:authenticator(),
	IDResponse1 = #radius{code = ?AccessRequest, id = 2,  authenticator = Authenticator2,
		attributes = IDRespAttributeList1},
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResponse2 = #radius{code = ?AccessRequest, id = 2, authenticator = Authenticator2,
		attributes = IDRspAttributeList3},
	IDResPacket2 = radius:codec(IDResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDResPacket2),
	{ok, {_Address, _Port, CommitPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = 2, authenticator = _RAuthenticator,
		attributes = CommitReqAttributes} = radius:codec(CommitPacket),
	CommitReqAtt = radius_attributes:codec(CommitReqAttributes),
	{ok, CommitEAPPacket} = radius_attributes:find(?EAPMessage, CommitReqAtt),
	#eap_packet{code = request, type = ?PWD, identifier = EAPId2, data = CommitData} =
		ocs_eap_codec:eap_packet(CommitEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitReqData} = ocs_eap_codec:eap_pwd(CommitData),
	#eap_pwd_commit{element = Element_S, scalar = Scalar_S} = ocs_eap_codec:eap_pwd_commit(CommitReqData),
	P_Rand = crypto:rand_uniform(1, ?R),
	{ok, Password, _Attr, _Balancen} = ocs:find_subscriber(PeerId),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerId, ServerID, Password),
	{Scalar_P, Element_P} = ocs_eap_pwd:compute_scalar(<<P_Rand:256>>, PWE),
	CommitRespBody = #eap_pwd_commit{scalar = Scalar_P, element = Element_P},
	CommitRespBodyData = ocs_eap_codec:eap_pwd_commit(CommitRespBody),
	CommitRespHeader = #eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitRespBodyData},
	CommitEAPData = ocs_eap_codec:eap_pwd(CommitRespHeader),
	CommitRespPacket = #eap_packet{code = response, type = ?PWD, identifier = EAPId2, data = CommitEAPData},
	CommitRespPacketData = ocs_eap_codec:eap_packet(CommitRespPacket),
	CommitAttributeList1 = radius_attributes:store(?EAPMessage,
	   CommitRespPacketData, IDReqAttributeList5),
	Authenticator3 = radius:authenticator(),
	CommitResponse1 = #radius{code = ?AccessRequest, id = 3, authenticator = Authenticator3,
		attributes = CommitAttributeList1},
	CommitReqPacket1 = radius:codec(CommitResponse1),
	CommitRespMsgAuth = crypto:hmac(md5, SharedSecret, CommitReqPacket1),
	CommitAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		CommitRespMsgAuth, CommitAttributeList1),
	CommitResponse2= #radius{code = ?AccessRequest, id = 3, authenticator = Authenticator3,
		attributes = CommitAttributeList3},
	CommitReqPacket2 = radius:codec(CommitResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, CommitReqPacket2),
	{ok, {_Address, _Port, ConfirmPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = 3, authenticator = _NewAuth,
		attributes = ConfirmReqAttributes} = radius:codec(ConfirmPacket),
	ConfirmReqAtt = radius_attributes:codec(ConfirmReqAttributes),
	{ok, ConfirmEAPPacket} = radius_attributes:find(?EAPMessage, ConfirmReqAtt),
	#eap_packet{code = request, type = ?PWD, identifier = EAPId3, data = ConfirmData} =
		ocs_eap_codec:eap_packet(ConfirmEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
		data = _Confirm_S} = ocs_eap_codec:eap_pwd(ConfirmData),
	Ciphersuite = <<19:16, 1, 1>>,
	Kp = ocs_eap_pwd:compute_ks(<<P_Rand:256>>, PWE, Scalar_S, Element_S),
	Input = [Kp, Element_P, Scalar_P, Element_S, Scalar_S, Ciphersuite],
	Confirm_P = ocs_eap_pwd:h(Input),
	ConfirmRespHeader = #eap_pwd{length = false, more = false, pwd_exch = confirm,
		data = Confirm_P},
	ConfirmEAPData = ocs_eap_codec:eap_pwd(ConfirmRespHeader),
	ConfirmRespPacket = #eap_packet{code = response, type = ?PWD, identifier = EAPId3,
		data = ConfirmEAPData},
	ConfirmRespPacketData = ocs_eap_codec:eap_packet(ConfirmRespPacket),
	ConfirmAttributeList1 = radius_attributes:store(?EAPMessage,
	   ConfirmRespPacketData, IDReqAttributeList5),
	Authenticator4 = radius:authenticator(),
	ConfirmResponse1 = #radius{code = ?AccessRequest, id = 4, authenticator = Authenticator4,
		attributes = ConfirmAttributeList1},
	ConfirmReqPacket1 = radius:codec(ConfirmResponse1),
	ConfirmRespMsgAuth = crypto:hmac(md5, SharedSecret, ConfirmReqPacket1),
	ConfirmAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		ConfirmRespMsgAuth, ConfirmAttributeList1),
	ConfirmResponse2= #radius{code = ?AccessRequest, id = 4, authenticator = Authenticator4,
		attributes = ConfirmAttributeList3},
	ConfirmReqPacket2 = radius:codec(ConfirmResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, ConfirmReqPacket2),
	{ok, {_Address, _Port, SuccessPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = 4, authenticator = _NewAuthe,
		attributes = SucReqAttributes} = radius:codec(SuccessPacket),
	SucReqAtt = radius_attributes:codec(SucReqAttributes),
	{ok, SucEAPPacket} = radius_attributes:find(?EAPMessage, SucReqAtt),
	#eap_packet{code = success, identifier = _EAPId} =
		ocs_eap_codec:eap_packet(SucEAPPacket).

unknown_authenticator() ->
	[{userdata, [{doc, "Unauthorised access request"}]}].

unknown_authenticator(Config) ->
	Id = 4,
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	Socket = ?config(socket, Config),
	UserName = ct:get_config(radius_username),
	SharedSecret = "bogus",
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:store(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:store(?NasPort, 3, IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:store(?NasIdentifier, "tomba3", IDReqAttributeList2),
	IDReqAttributeList4 = radius_attributes:store(?CallingStationId,"de:ad:be:ef:ca:fe", IDReqAttributeList3),
	IDReqAttributeList5 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), IDReqAttributeList4),
	IDRequest1 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList5},
	IDRequestPacket1 = radius:codec(IDRequest1),
	IDMsgAuth = crypto:hmac(md5, SharedSecret, IDRequestPacket1),
	IDReqAttributeList6 = radius_attributes:store(?MessageAuthenticator, IDMsgAuth, IDReqAttributeList5),
	IDRequest2 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList6},
	IDRequestPacket2 = radius:codec(IDRequest2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDRequestPacket2),
	{error, timeout} = gen_udp:recv(Socket, 0, 2000).

invalid_id_response_eap_packet() ->
	[{userdata, [{doc, "Send invalid eap packet"}]}].

invalid_id_response_eap_packet(Config) ->
	Id = 5,
	PeerId = ?config(peer_id, Config),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	Socket = ?config(socket, Config),
	UserName = ct:get_config(radius_username),
	SharedSecret = ct:get_config(radius_shared_secret),
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:store(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:store(?NasPort, 5, IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:store(?NasIdentifier, "tomba5", IDReqAttributeList2),
	IDReqAttributeList4 = radius_attributes:store(?CallingStationId,"de:ad:be:ef:ca:fe", IDReqAttributeList3),
	IDReqAttributeList5 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), IDReqAttributeList4),
	IDRequest1 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList5},
	IDRequestPacket1 = radius:codec(IDRequest1),
	IDMsgAuth = crypto:hmac(md5, SharedSecret, IDRequestPacket1),
	IDReqAttributeList6 = radius_attributes:store(?MessageAuthenticator, IDMsgAuth, IDReqAttributeList5),
	IDRequest2 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList6},
	IDRequestPacket2 = radius:codec(IDRequest2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDRequestPacket2),
	{ok, {AuthAddress, AuthPort, IdReqPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = Id, authenticator = _IDReqAuthenticator,
		attributes = BinIDReqAttributes} = radius:codec(IdReqPacket),
	IDReqAttributes = radius_attributes:codec(BinIDReqAttributes),
	{ok, IDEAPPacket} = radius_attributes:find(?EAPMessage, IDReqAttributes),
	#eap_packet{code = request, type = ?PWD, identifier = IDEAPId, data = IDData} =
		ocs_eap_codec:eap_packet(IDEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDReqData} = ocs_eap_codec:eap_pwd(IDData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1, token = Token,
		pwd_prep = none, identity = _ServerID} = ocs_eap_codec:eap_pwd_id(IDReqData),
	IDRespBody = #eap_pwd_id{token = Token, pwd_prep = none, identity = PeerId},
	IDRespBodyData = ocs_eap_codec:eap_pwd_id(IDRespBody),
	IDRespHeader = #eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDRespBodyData},
	IDEAPData = ocs_eap_codec:eap_pwd(IDRespHeader),

	InvalidEAPPacket = #eap_packet{code = request, type = ?PWD, identifier = IDEAPId, data = IDEAPData},
	IDEAPPacketData = ocs_eap_codec:eap_packet(InvalidEAPPacket),

	IDRespAttributeList1 = radius_attributes:store(?EAPMessage, IDEAPPacketData, IDReqAttributeList5),
	Authenticator2 = radius:authenticator(),
	IDResponse1 = #radius{code = ?AccessRequest, id = 2,  authenticator = Authenticator2,
		attributes = IDRespAttributeList1},
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResponse2 = #radius{code = ?AccessRequest, id = 2, authenticator = Authenticator2,
		attributes = IDRspAttributeList3},
	IDResPacket2 = radius:codec(IDResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDResPacket2),
	{error, timeout} = gen_udp:recv(Socket, 0, 2000).

invalid_id_response_eap_pwd() ->
	[{userdata, [{doc, "Send invalid eap packet data"}]}].

invalid_id_response_eap_pwd(Config) ->
	Id = 6,
	PeerId = ?config(peer_id, Config),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	Socket = ?config(socket, Config),
	UserName = ct:get_config(radius_username),
	SharedSecret = ct:get_config(radius_shared_secret),
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:store(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:store(?NasPort, 6, IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:store(?NasIdentifier, "tomba6", IDReqAttributeList2),
	IDReqAttributeList4 = radius_attributes:store(?CallingStationId,"de:ad:be:ef:ca:fe", IDReqAttributeList3),
	IDReqAttributeList5 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), IDReqAttributeList4),
	IDRequest1 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList5},
	IDRequestPacket1 = radius:codec(IDRequest1),
	IDMsgAuth = crypto:hmac(md5, SharedSecret, IDRequestPacket1),
	IDReqAttributeList6 = radius_attributes:store(?MessageAuthenticator, IDMsgAuth, IDReqAttributeList5),
	IDRequest2 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = IDReqAttributeList6},
	IDRequestPacket2 = radius:codec(IDRequest2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDRequestPacket2),
	{ok, {AuthAddress, AuthPort, IdReqPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessChallenge, id = Id, authenticator = _IDReqAuthenticator,
		attributes = BinIDReqAttributes} = radius:codec(IdReqPacket),
	IDReqAttributes = radius_attributes:codec(BinIDReqAttributes),
	{ok, IDEAPPacket} = radius_attributes:find(?EAPMessage, IDReqAttributes),
	#eap_packet{code = request, type = ?PWD, identifier = IDEAPId, data = IDData} =
		ocs_eap_codec:eap_packet(IDEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDReqData} = ocs_eap_codec:eap_pwd(IDData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1, token = Token,
		pwd_prep = none, identity = _ServerID} = ocs_eap_codec:eap_pwd_id(IDReqData),
	IDRespBody = #eap_pwd_id{token = Token, pwd_prep = none, identity = PeerId},
	IDRespBodyData = ocs_eap_codec:eap_pwd_id(IDRespBody),
	InvalidPWD = #eap_pwd{length = false, more = false, pwd_exch = commit,
		data = IDRespBodyData},
	IDEAPData = ocs_eap_codec:eap_pwd(InvalidPWD),
	IDResEAPPacket = #eap_packet{code = response, type = ?PWD, identifier = IDEAPId, data = IDEAPData},
	IDEAPPacketData = ocs_eap_codec:eap_packet(IDResEAPPacket),
	IDRespAttributeList1 = radius_attributes:store(?EAPMessage, IDEAPPacketData, IDReqAttributeList5),
	Authenticator2 = radius:authenticator(),
	IDResponse1 = #radius{code = ?AccessRequest, id = 2,  authenticator = Authenticator2,
		attributes = IDRespAttributeList1},
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResPacket1 = radius:codec(IDResponse1),
	IDRespMsgAuth = crypto:hmac(md5, SharedSecret, IDResPacket1),
	IDRspAttributeList3 = radius_attributes:store(?MessageAuthenticator,
		IDRespMsgAuth, IDRespAttributeList1),
	IDResponse2 = #radius{code = ?AccessRequest, id = 2, authenticator = Authenticator2,
		attributes = IDRspAttributeList3},
	IDResPacket2 = radius:codec(IDResponse2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, IDResPacket2),
	{error, timeout} = gen_udp:recv(Socket, 0, 2000).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

send_identity(Socket, Address, Port, NasId,
		UserName, Secret, PeerId, MAC, Auth, EapId, RadId) ->
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?UserName, UserName, A0),
	A2 = radius_attributes:add(?NasPort, 0, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2),
	A4 = radius_attributes:add(?CallingStationId, MAC, A3),
	EapPacket  = #eap_packet{code = response, type = ?Identity,
			identifier = EapId, data = PeerId},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	A5 = radius_attributes:add(?EAPMessage, EapMessage, A4),
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

receive_id(Socket, Address, Port, Secret, ReqAuth, RadId) ->
	{ok, {Address, Port, RespPacket1}} = gen_udp:recv(Socket, 0),
	Resp1 = radius:codec(RespPacket1),
	#radius{code = ?AccessChallenge, id = RadId, authenticator = RespAuth,
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
	#eap_packet{code = request, type = ?PWD, identifier = EapId,
			data = EapData} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = EapPwdData} = ocs_eap_codec:eap_pwd(EapData),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1,
			prf = 16#1, pwd_prep = none,
			identity = ServerID} = ocs_eap_codec:eap_pwd_id(EapPwdData),
	{EapId, ServerID}.

send_id(Socket, Address, Port, Secret, ReqAuth, UserName,
		NasId, PeerId, MAC, EapId, RadId) ->
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?UserName, UserName, A0),
	A2 = radius_attributes:add(?NasPort, 0, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2),
	A4 = radius_attributes:add(?CallingStationId, MAC, A3),
	Token = crypto:rand_bytes(4),
	EapPwdId = #eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1,
			token = Token, pwd_prep = none, identity = PeerId},
	EapPwd = #eap_pwd{pwd_exch = id, data = ocs_eap_codec:eap_pwd_id(EapPwdId)},
	EapPacket  = #eap_packet{code = response, type = ?PWD, identifier = EapId,
			data = ocs_eap_codec:eap_pwd(EapPwd)},
	EapMsg = ocs_eap_codec:eap_packet(EapPacket),
	A5 = radius_attributes:add(?EAPMessage, EapMsg, A4),
	A6 = radius_attributes:add(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16,0)), A5),
	Request1 = #radius{code = ?AccessRequest, id = RadId,
			authenticator = ReqAuth, attributes = A6},
	RequestPacket1 = radius:codec(Request1),
	MsgAuth1 = crypto:hmac(md5, Secret, RequestPacket1),
	A7 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A6),
	Request2 = Request1#radius{attributes = A7},
	RequestPacket2 = radius:codec(Request2),
	gen_udp:send(Socket, Address, Port, RequestPacket2).

