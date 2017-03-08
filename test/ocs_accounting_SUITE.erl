%%% ocs_accounting_SUITE.erl
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
%%%  @doc Test suite for accounting of the {@link //ocs. ocs} application.
%%%
-module(ocs_accounting_SUITE).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

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
	[{userdata, [{doc, "Test suite for accounting in OCS"}]},
	{require, radius_shared_secret}, {default_config, radius_shared_secret, "abc345"},
	{timetrap, {seconds, 8}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	{ok, DiscPort} = application:get_env(ocs, radius_disconnect_port),
	Protocol = ct:get_config(protocol),
	SharedSecret = ct:get_config(radius_shared_secret),
	Config1 = [{radius_shared_secret, SharedSecret} | Config],
	ok = ocs:add_client({127, 0, 0, 1}, DiscPort, Protocol, SharedSecret),
	NasId = atom_to_list(node()),
	[{nas_id, NasId} | Config1].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_test_lib:stop(),
	ok = ocs:delete_subscriber("25252525"),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	Config.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[radius_accouting, disconnect_session].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_accouting() ->
	[{userdata, [{doc, "Initiate and terminate a RADIUS accouting session"}]}].

radius_accouting(Config) ->
	Id1 = 1,
	NasId = ?config(nas_id, Config),
	AcctSessionId = "0A0055C1",
	AcctAddress = {127, 0, 0, 1},
	AuthAddress = {127, 0, 0, 1},
	{ok, [{radius, AcctPort, _}]} = application:get_env(ocs, radius_acct_config),
	{ok, [{radius, AuthPort, _}]} = application:get_env(ocs, radius_auth_config),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]), 
	PeerID = ocs:generate_password(),
	SharedSecret = ct:get_config(radius_shared_secret),
	PeerPassword = ocs:generate_password(),
   ok = ocs:add_subscriber(PeerID, PeerPassword, [], 1000),
   SharedSecret = ct:get_config(radius_shared_secret),
	ArqAuthenticator = radius:authenticator(),
   UserPassword = radius_attributes:hide(SharedSecret, ArqAuthenticator, PeerPassword),
	A0 = radius_attributes:new(),
	A1 = radius_attributes:add(?UserName, PeerID, A0),
	A2 = radius_attributes:add(?UserPassword, UserPassword, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasId, A2),
	A4 = radius_attributes:add(?NasPortType, 19, A3),
	A5 = radius_attributes:add(?CallingStationId,"DE-AD-BE-EF-FE-ED", A4),
	A6 = radius_attributes:add(?CalledStationId,"BA-DF-AD-CA-DD-AD:TestSSID", A5),
	AccessRequest = #radius{code = ?AccessRequest, id = Id1,
			authenticator = ArqAuthenticator, attributes = A6},
	ArqPacket = radius:codec(AccessRequest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, ArqPacket),
	{ok, {AuthAddress, AuthPort, ResponsePacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id1} = radius:codec(ResponsePacket),
	Id2 = Id1 + 1,
	B0 = radius_attributes:new(),
	B1 = radius_attributes:add(?UserName, PeerID, B0),
	B2 = radius_attributes:add(?NasIdentifier, NasId, B1),
	B3 = radius_attributes:add(?CallingStationId,"DE-AD-BE-EF-FE-ED", B2),
	B4 = radius_attributes:add(?CalledStationId,"BA-DF-AD-CA-DD-AD:TestSSID", B3),
	B5 = radius_attributes:add(?AcctSessionId, AcctSessionId, B4),
	B6 = radius_attributes:add(?AcctStatusType, ?AccountingStart, B5),
	AccAttributes1 = radius_attributes:codec(B6),
	Acc1Length = size(AccAttributes1) + 20,
	AccAuthenticator1 = crypto:md5([<<?AccountingRequest, Id2,
			Acc1Length:16, 0:128>>, AccAttributes1, SharedSecret]), 
	AccountingRequest1 = #radius{code = ?AccountingRequest, id = Id2,
			authenticator = AccAuthenticator1, attributes = AccAttributes1},
	AccPacket1 = radius:codec(AccountingRequest1),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, AccPacket1),
	{ok, {AcctAddress, AcctPort, AccResponse1}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccountingResponse, id = Id2, attributes = AccResponse1Attrs,
			authenticator = ResponseAuthenticator1} = radius:codec(AccResponse1),
	AccResponse1Length = size(AccResponse1Attrs) + 20,
	ResponseAuthenticator1 = binary_to_list(crypto:hash(md5,
			[<<?AccountingResponse, Id2, AccResponse1Length:16>>,
			AccAuthenticator1, AccResponse1Attrs, SharedSecret])),
	Id3 = Id2 + 1,
	C1 = radius_attributes:store(?AcctStatusType, ?AccountingStop, B6),
	C2 = radius_attributes:add(?AcctInputOctets, 24657, C1),
	C3 = radius_attributes:add(?AcctOutputOctets, 87465, C2),
	AccAttributes2 = radius_attributes:codec(C3),
	Acc2Length = size(AccAttributes2) + 20,
	AccAuthenticator2 = crypto:md5([<<?AccountingRequest, Id3,
			Acc2Length:16, 0:128>>, AccAttributes2, SharedSecret]), 
	AccountingRequest2 = #radius{code = ?AccountingRequest, id = Id3,
			authenticator = AccAuthenticator2, attributes = AccAttributes2},
	AccPacket2 = radius:codec(AccountingRequest2),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, AccPacket2),
	{ok, {AcctAddress, AcctPort, AccResponse2}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccountingResponse, id = Id3, attributes = AccResponse2Attrs,
			authenticator = ResponseAuthenticator2} = radius:codec(AccResponse2),
	AccResponse2Length = size(AccResponse2Attrs) + 20,
	ResponseAuthenticator2 = binary_to_list(crypto:hash(md5,
			[<<?AccountingResponse, Id3, AccResponse2Length:16>>,
			AccAuthenticator2, AccResponse2Attrs, SharedSecret])).

disconnect_session() ->
	[{userdata, [{doc, "Disconnect a RADIUS accouting session based on usage"}]}].

disconnect_session(Config) ->
	Id = 1,
	%PeerID = list_to_binary(?config(peer_id, Config)),
	PeerID = ocs:generate_password(),
	Password = ?config(password, Config),
	NasId = ?config(nas_id, Config),
	AcctAddress = {127, 0, 0, 1},
	AuthAddress = {127, 0, 0, 1},
	{ok, [{radius, AcctPort, _}]} = application:get_env(ocs, radius_acct_config),
	{ok, [{radius, AuthPort, _}]} = application:get_env(ocs, radius_auth_config),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]), 
	UserName = "simoon",
	SharedSecret = ct:get_config(radius_shared_secret),
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:add(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:add(?NasIdentifier, NasId, IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:add(?NasPortType, 19, IDReqAttributeList2),
	IDReqAttributeList4 = radius_attributes:add(?CallingStationId,"de:ad:be:ef:ca:fe",
		IDReqAttributeList3),
	IDReqAttributeList5 = radius_attributes:add(?MessageAuthenticator, <<0:128>>, IDReqAttributeList4),
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
	IDRespBody = #eap_pwd_id{token = Token, pwd_prep = none, identity = PeerID},
	IDRespBodyData = ocs_eap_codec:eap_pwd_id(IDRespBody),
	IDRespHeader = #eap_pwd{length = false, more = false, pwd_exch = id,
		data = IDRespBodyData},
	IDEAPData = ocs_eap_codec:eap_pwd(IDRespHeader),
	IDRespPacket = #eap_packet{code = response, type = ?PWD, identifier = IDEAPId, data = IDEAPData},
	IDEAPPacketData = ocs_eap_codec:eap_packet(IDRespPacket),
	IDRespAttributeList1 = radius_attributes:add(?EAPMessage, IDEAPPacketData, IDReqAttributeList5),
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
	#radius{code = ?AccessChallenge, id = 2, authenticator = _Authenticator,
		attributes = CommitReqAttributes} = radius:codec(CommitPacket),
	CommitReqAtt = radius_attributes:codec(CommitReqAttributes),
	{ok, CommitEAPPacket} = radius_attributes:find(?EAPMessage, CommitReqAtt),
	#eap_packet{code = request, type = ?PWD, identifier = EAPId2, data = CommitData} =
		ocs_eap_codec:eap_packet(CommitEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitReqData} = ocs_eap_codec:eap_pwd(CommitData),
	#eap_pwd_commit{element = Element_S, scalar = Scalar_S} = ocs_eap_codec:eap_pwd_commit(CommitReqData),
	P_Rand = crypto:rand_uniform(1, ?R),
	PWE = ocs_eap_pwd:compute_pwe(Token, PeerID, ServerID, Password),
	{Scalar_P, Element_P} = ocs_eap_pwd:compute_scalar(<<P_Rand:256>>, PWE),
	CommitRespBody = #eap_pwd_commit{scalar = Scalar_P, element = Element_P},
	CommitRespBodyData = ocs_eap_codec:eap_pwd_commit(CommitRespBody),
	CommitRespHeader = #eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitRespBodyData},
	CommitEAPData = ocs_eap_codec:eap_pwd(CommitRespHeader),
	CommitRespPacket = #eap_packet{code = response, type = ?PWD, identifier = EAPId2, data = CommitEAPData},
	CommitRespPacketData = ocs_eap_codec:eap_packet(CommitRespPacket),
	CommitAttributeList1 = radius_attributes:add(?EAPMessage,
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
		data = Confirm_S} = ocs_eap_codec:eap_pwd(ConfirmData),
	Ciphersuite = <<19:16, 1, 1>>,
	Kp = ocs_eap_pwd:compute_ks(<<P_Rand:256>>, PWE, Scalar_S, Element_S),
	Input = [Kp, Element_P, Scalar_P, Element_S, Scalar_S, Ciphersuite],
	Confirm_P = ocs_eap_pwd:h(Input),
	ConfirmRespHeader = #eap_pwd{length = false,
			more = false, pwd_exch = confirm, data = Confirm_P},
	ConfirmEAPData = ocs_eap_codec:eap_pwd(ConfirmRespHeader),
	ConfirmRespPacket = #eap_packet{code = response, type = ?PWD, identifier = EAPId3,
		data = ConfirmEAPData},
	ConfirmRespPacketData = ocs_eap_codec:eap_packet(ConfirmRespPacket),
	ConfirmAttributeList1 = radius_attributes:add(?EAPMessage,
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
		ocs_eap_codec:eap_packet(SucEAPPacket),
	RADAcct_ReqId = 1,
	RADAcct_ReqAuth = radius:authenticator(),
	RADAcctAttributes0 = radius_attributes:new(),
	RADAcctAttributes1 = radius_attributes:add(?UserName, "25252525", RADAcctAttributes0),
	RADAcctAttributes2 = radius_attributes:add(?AcctSessionId, "xray12", RADAcctAttributes1),
	RADAcctAttributes3 = radius_attributes:add(?AcctStatusType, ?AccountingStart, RADAcctAttributes2),
	RADAcctAttributes4 = radius_attributes:add(?NasIdentifier, NasId, RADAcctAttributes3),
	RADAcctAttributes5 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, RADAcctAttributes4),
	RADAcctAttributesList = radius_attributes:codec(RADAcctAttributes5),
	RADAcct_Rec1 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctAttributesList},
	RADAcct_packet1 = radius:codec(RADAcct_Rec1),
	RADAcct_MsgAuth = crypto:hmac(md5, SharedSecret, RADAcct_packet1),
	RADAcctAttributesList1 = radius_attributes:store(?MessageAuthenticator, RADAcct_MsgAuth, RADAcctAttributes5),
	RADAcct_Rec2 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctAttributesList1},
	RADAcct_packet2 = radius:codec(RADAcct_Rec2),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, RADAcct_packet2),
	{ok, {_, _, RADAcct_response}} = gen_udp:recv(Socket, 0),
	RADAcct_ReqId1 = 2,
	RADAcct_ReqAuth1 = radius:authenticator(),
	RADAcctAttributes10 = radius_attributes:new(),
	RADAcctAttributes11 = radius_attributes:add(?UserName, "25252525", RADAcctAttributes10),
	RADAcctAttributes12 = radius_attributes:add(?AcctSessionId, "xray12", RADAcctAttributes11),
	RADAcctAttributes13 = radius_attributes:add(?AcctStatusType, ?AccountingInterimUpdate, RADAcctAttributes12),
	RADAcctAttributes14 = radius_attributes:add(?AcctInputOctets, 200, RADAcctAttributes13),
	RADAcctAttributes15 = radius_attributes:add(?AcctOutputOctets, 200, RADAcctAttributes14),
	RADAcctAttributes16 = radius_attributes:add(?NasIpAddress, AcctAddress, RADAcctAttributes15),
	RADAcctAttributes17 = radius_attributes:add(?NasIdentifier, NasId, RADAcctAttributes16),
	RADAcctAttributes18 = radius_attributes:store(?MessageAuthenticator,
			<<0:128>>, RADAcctAttributes17),
	RADAcctAttributesList11 = radius_attributes:codec(RADAcctAttributes18),
	RADAcct_Rec11 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId1, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctAttributesList11},
	RADAcct_packet11 = radius:codec(RADAcct_Rec11),
	RADAcct_MsgAuth1 = crypto:hmac(md5, SharedSecret, RADAcct_packet11),
	RADAcctAttributesList12 = radius_attributes:store(?MessageAuthenticator, RADAcct_MsgAuth1, RADAcctAttributes18),
	RADAcct_Rec21 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId1, authenticator = RADAcct_ReqAuth1,
		attributes = RADAcctAttributesList12},
	RADAcct_packet21 = radius:codec(RADAcct_Rec21),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, RADAcct_packet21),
	{ok, {_, _, RADAcct_response1}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccountingResponse, id = RADAcct_ReqId1} = radius:codec(RADAcct_response1),
	RADAcct_ReqId21 = 3,
	RADAcct_ReqAuth2 = radius:authenticator(),
	RADAcctAttributes20 = radius_attributes:new(),
	RADAcctAttributes21 = radius_attributes:add(?UserName, "25252525", RADAcctAttributes20),
	RADAcctAttributes22 = radius_attributes:add(?AcctSessionId, "xray12", RADAcctAttributes21),
	RADAcctAttributes23 = radius_attributes:add(?AcctStatusType, ?AccountingInterimUpdate, RADAcctAttributes22),
	RADAcctAttributes24 = radius_attributes:add(?AcctInputOctets, 50, RADAcctAttributes23),
	RADAcctAttributes25 = radius_attributes:add(?AcctOutputOctets, 50, RADAcctAttributes24),
	RADAcctAttributes26 = radius_attributes:add(?NasIpAddress, AcctAddress, RADAcctAttributes25),
	RADAcctAttributes27 = radius_attributes:add(?NasIdentifier, NasId, RADAcctAttributes26),
	RADAcctAttributes28 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, RADAcctAttributes27),
	RADAcctAttributesList21 = radius_attributes:codec(RADAcctAttributes28),
	RADAcct_Rec22 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId21, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctAttributesList21},
	RADAcct_packet22 = radius:codec(RADAcct_Rec22),
	RADAcct_MsgAuth2 = crypto:hmac(md5, SharedSecret, RADAcct_packet21),
	RADAcctAttributesList22 = radius_attributes:store(?MessageAuthenticator, RADAcct_MsgAuth2, RADAcctAttributes28),
	RADAcct_Rec23 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId21, authenticator = RADAcct_ReqAuth2,
		attributes = RADAcctAttributesList22},
	RADAcct_packet23 = radius:codec(RADAcct_Rec23),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, RADAcct_packet23),
	{ok, {_, _, RADAcct_response2}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccountingResponse, id = RADAcct_ReqId} = radius:codec(RADAcct_response),
	RADAcct_NewReqId = 2,
	RADAcctNewAttributes1 = radius_attributes:add(?AcctStatusType, ?AccountingStop, RADAcctAttributes2),
	RADAcctNewAttributes2 = radius_attributes:add(?NasIdentifier, NasId, RADAcctNewAttributes1),
	RADAcctNewAttributes3 = radius_attributes:add(?AcctInputOctets, 1000, RADAcctNewAttributes2),
	RADAcctNewAttributes4 = radius_attributes:add(?AcctOutputOctets, 1000, RADAcctNewAttributes3),
	RADAcctNewAttributesList = radius_attributes:codec(RADAcctNewAttributes4),
	RADAcct_stop_rec = #radius{code = ?AccountingRequest, id = RADAcct_NewReqId, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctNewAttributesList},
	RADAcct_stop_packet = radius:codec(RADAcct_stop_rec),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, RADAcct_stop_packet),
	{ok, Socket1} = gen_udp:open(3799, [{active, false}, inet, binary]), 
	{ok, {OCSAddr, OCSPort, DiscReq}} = gen_udp:recv(Socket1, 0),
	#radius{code = ?DisconnectRequest, id = DiscReqID} = radius:codec(DiscReq),
	DiscAckAuth = radius:authenticator(),
	DiscAckAttr0 = radius_attributes:new(),
	DiscAckAttr1 = radius_attributes:add(?AcctTerminateCause, 6, DiscAckAttr0),
	DiscAckAttrBin = radius_attributes:codec(DiscAckAttr1),
	DiscAckRec = #radius{code = ?DisconnectAck, id = DiscReqID, authenticator = DiscAckAuth, attributes = DiscAckAttrBin},
	DiscAck = radius:codec(DiscAckRec),
	ok = gen_udp:send(Socket1, OCSAddr, OCSPort, DiscAck),
	ok =  gen_udp:close(Socket1),
	ok =  gen_udp:close(Socket). 

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

