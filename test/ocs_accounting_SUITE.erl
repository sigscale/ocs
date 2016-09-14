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
%%%  Test suite for the ocs API.
%%%
-module(ocs_accounting_SUITE).
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
	[{userdata, [{doc, ""}]},
	{require, radius_shared_secret}, {default_config, radius_shared_secret, "abc345"},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_lib:initialize_db(),
	ok = ocs_lib:start(),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_secret),
	ok = ocs:add_client(AuthAddress, SharedSecret),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
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
	[radius_accouting].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_accouting() ->
	[{userdata, [{doc, "Initiate and terminate a RADIUS accouting session"}]}].

radius_accouting(_Config) ->
	Id = 1,
	PeerID = "simon@sigscale",
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, AuthAddress}, binary]), 
	UserName = "simoon",
	SharedSecret = ct:get_config(radius_shared_secret),
	Authenticator = radius:authenticator(),
	IDReqAttributeList0 = radius_attributes:new(),
	IDReqAttributeList1 = radius_attributes:store(?UserName, UserName, IDReqAttributeList0),
	IDReqAttributeList2 = radius_attributes:store(?NasIdentifier, "tomba1", IDReqAttributeList1),
	IDReqAttributeList3 = radius_attributes:store(?NasPortId,"wlan1", IDReqAttributeList2),
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
		pwd_prep = none, identity = ServerID} = ocs_eap_codec:eap_pwd_id(IDReqData),
	IDRespBody = #eap_pwd_id{token = Token, pwd_prep = none, identity = PeerID},
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
	#radius{code = ?AccessChallenge, id = Id, authenticator = _Authenticator,
		attributes = CommitReqAttributes} = radius:codec(CommitPacket),
	CommitReqAtt = radius_attributes:codec(CommitReqAttributes),
	{ok, CommitEAPPacket} = radius_attributes:find(?EAPMessage, CommitReqAtt),
	#eap_packet{code = request, type = ?PWD, identifier = EAPId2, data = CommitData} =
		ocs_eap_codec:eap_packet(CommitEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = commit,
		data = CommitReqData} = ocs_eap_codec:eap_pwd(CommitData),
	#eap_pwd_commit{element = Element_S, scalar = Scalar_S} = ocs_eap_codec:eap_pwd_commit(CommitReqData),
	P_Rand = crypto:rand_uniform(1, ?R),
	BinPeerID = list_to_binary(PeerID),
	BinServerID = list_to_binary(ServerID),
	BinSharedSecret = list_to_binary(SharedSecret),
	PWE = ocs_eap_pwd:compute_pwe(Token, BinPeerID, BinServerID, BinSharedSecret),
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
	#radius{code = ?AccessChallenge, id = Id, authenticator = _NewAuth,
		attributes = ConfirmReqAttributes} = radius:codec(ConfirmPacket),
	ConfirmReqAtt = radius_attributes:codec(ConfirmReqAttributes),
	{ok, ConfirmEAPPacket} = radius_attributes:find(?EAPMessage, ConfirmReqAtt),
	#eap_packet{code = request, type = ?PWD, identifier = EAPId3, data = ConfirmData} =
		ocs_eap_codec:eap_packet(ConfirmEAPPacket),
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
		data = Confirm_S} = ocs_eap_codec:eap_pwd(ConfirmData),
	Kp = ocs_eap_pwd:compute_ks(<<P_Rand:256>>, PWE, Scalar_S, Element_S),
	Input = [<<Kp/binary, Element_P/binary, Scalar_P/binary, Element_S/binary, 
		Scalar_S/binary, 19:16, 1, 1>>],
	Confirm_P = ocs_eap_pwd:h(Input),
	ConfirmRespHeader = #eap_pwd{length = false,
			more = false, pwd_exch = confirm, data = Confirm_P},
	ConfirmEAPData = ocs_eap_codec:eap_pwd(ConfirmRespHeader),
	ConfirmRespPacket = #eap_packet{code = response, type = ?PWD, identifier = EAPId3, data = ConfirmEAPData},
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
	#radius{code = ?AccessAccept, id = Id, authenticator = _NewAuthe,
		attributes = SucReqAttributes} = radius:codec(SuccessPacket),
	SucReqAtt = radius_attributes:codec(SucReqAttributes),
	{ok, SucEAPPacket} = radius_attributes:find(?EAPMessage, SucReqAtt),
	#eap_packet{code = success, type = ?PWD, identifier = _EAPId, data = <<>>} =
		ocs_eap_codec:eap_packet(SucEAPPacket),
	AcctAddress = {127,0,0,1},
	AcctPort = 9913,
	RADAcct_ReqId = 1,
	RADAcct_ReqAuth = radius:authenticator(),
	RADAcctAttributes0 = radius_attributes:new(),
	RADAcctAttributes1 = radius_attributes:store(?AcctSessionId, "xray12", RADAcctAttributes0),
	RADAcctAttributes2 = radius_attributes:store(?AcctStatusType, ?AccountingStart, RADAcctAttributes1),
	RADAcctAttributes3 = radius_attributes:store(?NasIdentifier, "xray@point1", RADAcctAttributes2),
	RADAcctAttributes4 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), RADAcctAttributes3),
	RADAcctAttributesList = radius_attributes:codec(RADAcctAttributes4),
	RADAcct_Rec1 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctAttributesList},
	RADAcct_packet1 = radius:codec(RADAcct_Rec1),
	RADAcct_MsgAuth = crypto:hmac(md5, SharedSecret, RADAcct_packet1),
	RADAcctAttributesList1 = radius_attributes:store(?MessageAuthenticator, RADAcct_MsgAuth, RADAcctAttributes4),
	RADAcct_Rec2 = #radius{code = ?AccountingRequest, id = RADAcct_ReqId, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctAttributesList1},
	RADAcct_packet2 = radius:codec(RADAcct_Rec2),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, RADAcct_packet2),
	{ok, {_, _, RADAcct_response}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccountingResponse, id = RADAcct_ReqId} = radius:codec(RADAcct_response),
	RADAcct_NewReqId = 2,
	RADAcctNewAttributes1 = radius_attributes:store(?AcctStatusType, ?AccountingStop, RADAcctAttributes1),
	RADAcctNewAttributes2 = radius_attributes:store(?NasIdentifier, "xray@point1", RADAcctNewAttributes1),
	RADAcctNewAttributes3 = radius_attributes:store(?AcctInputOctets, 24657, RADAcctNewAttributes2),
	RADAcctNewAttributes4 = radius_attributes:store(?AcctOutputOctets, 87465, RADAcctNewAttributes3),
	RADAcctNewAttributesList = radius_attributes:codec(RADAcctNewAttributes4),
	RADAcct_stop_rec = #radius{code = ?AccountingRequest, id = RADAcct_NewReqId, authenticator = RADAcct_ReqAuth,
		attributes = RADAcctNewAttributesList},
	RADAcct_stop_packet = radius:codec(RADAcct_stop_rec),
	ok = gen_udp:send(Socket, AcctAddress, AcctPort, RADAcct_stop_packet),
	{ok, {_, _, RADAcct_stop_response}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccountingResponse, id = RADAcct_NewReqId } = radius:codec(RADAcct_stop_response),
	ok =  gen_udp:close(Socket). 

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

