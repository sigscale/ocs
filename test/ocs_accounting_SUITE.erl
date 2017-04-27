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
	Protocol = ct:get_config(protocol),
	SharedSecret = ct:get_config(radius_shared_secret),
	Config1 = [{radius_shared_secret, SharedSecret} | Config],
	ok = ocs:add_client({127, 0, 0, 1}, 3799, Protocol, SharedSecret),
	NasID = atom_to_list(node()),
	[{nas_id, NasID} | Config1].

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
	[radius_accouting, radius_disconnect_session].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_accouting() ->
	[{userdata, [{doc, "Initiate and terminate a RADIUS accouting session"}]}].

radius_accouting(Config) ->
	RadID1 = 1,
	NasID = ?config(nas_id, Config),
	AcctSessionID = "0A0055C1",
	{ok, [{auth, AuthInstance}, {acct, AcctInstance}]} = application:get_env(ocs, radius),
	[{AuthAddress, AuthPort, _}] = AuthInstance,
	[{AcctAddress, AcctPort, _}] = AcctInstance,
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]), 
	PeerID = "845652366",
	Secret = ct:get_config(radius_shared_secret),
	Password = ocs:generate_password(),
   ok = ocs:add_subscriber(PeerID, Password, [], 1000),
	ReqAuth = radius:authenticator(),
   HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID, ReqAuth, RadID1),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID2),
	RadID3 = RadID2 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID3).

radius_disconnect_session() ->
	[{userdata, [{doc, "Disconnect a RADIUS accouting session based on usage"}]}].

radius_disconnect_session(Config) ->
	RadID1 = 10,
	NasID = ?config(nas_id, Config),
	AcctSessionID = "0B0055D1",
	{ok, [{auth, AuthInstance}, {acct, AcctInstance}]} = application:get_env(ocs, radius),
	[{AuthAddress, AuthPort, _}] = AuthInstance,
	[{AcctAddress, AcctPort, _}] = AcctInstance,
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]), 
	PeerID = "458565788",
	Secret = ct:get_config(radius_shared_secret),
	Password = ocs:generate_password(),
   ok = ocs:add_subscriber(PeerID, Password, [], 1000),
	ReqAuth = radius:authenticator(),
   HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID, ReqAuth, RadID1),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID2),
	RadID3 = RadID2 + 1,
	accounting_interim(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID3, 700, 300),
	RadID4 = RadID3 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID4),
	disconnect_request().

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------
authenticate_subscriber(Socket, Address,
		Port, PeerID, Password, Secret, NasID, ReqAuth, RadID) ->
	RadAttribute = radius_attributes:add(?UserPassword, Password, []),
	access_request(Socket, Address,
		Port, PeerID, Secret, NasID, ReqAuth, RadID, RadAttribute),
	access_accept(Socket, Address, Port, RadID).

accounting_start(Socket, Address, Port,
		PeerID, Secret, NasID, AcctSessionID, RadID) ->
	ReqAuth = accounting_request(?AccountingStart, Socket,
			Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID, []),
	accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth).

accounting_interim(Socket, Address, Port, PeerID,
		Secret, NasID, AcctSessionID, RadID, InputOctets, OutputOctets) ->
	A0 = radius_attributes:add(?AcctInputOctets, InputOctets, []),
	A1 = radius_attributes:add(?AcctOutputOctets, OutputOctets, A0),
	ReqAuth = accounting_request(?AccountingInterimUpdate, Socket,
			Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID, A1),
	accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth).

accounting_stop(Socket, Address, Port, PeerID,
		Secret, NasID, AcctSessionID, RadID) ->
	A0 = radius_attributes:store(?AcctInputOctets, 100, []),
	A1 = radius_attributes:store(?AcctOutputOctets, 50, A0),
	ReqAuth = accounting_request(?AccountingStop, Socket,
			Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID, A1),
	accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth).

disconnect_request() ->
	{ok, Socket} = gen_udp:open(3799, [{active, false}, inet, binary]), 
	{ok, {OCSAddr, OCSPort, DiscReq}} = gen_udp:recv(Socket, 0),
	#radius{code = ?DisconnectRequest, id = DiscReqID} = radius:codec(DiscReq),
	DiscAckAuth = radius:authenticator(),
	DiscAckAttr0 = radius_attributes:new(),
	DiscAckAttr1 = radius_attributes:add(?AcctTerminateCause, 6, DiscAckAttr0),
	DiscAckAttrBin = radius_attributes:codec(DiscAckAttr1),
	DiscAckRec = #radius{code = ?DisconnectAck, id = DiscReqID,
			authenticator = DiscAckAuth, attributes = DiscAckAttrBin},
	DiscAck = radius:codec(DiscAckRec),
	ok = gen_udp:send(Socket, OCSAddr, OCSPort, DiscAck),
	ok =  gen_udp:close(Socket). 

access_accept(Socket, Address, Port, RadID) ->
	receive_radius(?AccessAccept, Socket, Address, Port, RadID).

accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth) -> 
	#radius{id = RadID, authenticator = RespAuth,
		attributes = Attributes} 
		= receive_radius(?AccountingResponse, Socket, Address, Port, RadID),
	AttributesLength = size(Attributes) + 20,
	RespAuth = binary_to_list(crypto:hash(md5,
			[<<?AccountingResponse, RadID, AttributesLength:16>>,
			ReqAuth, Attributes, Secret])).

receive_radius(Code, Socket, Address, Port, RadID) ->
	{ok, {Address, Port, RespPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = Code, id = RadID} = radius:codec(RespPacket).

access_request(Socket, Address, Port, UserName, Secret,
		NasID, Auth, RadID, RadAttributes) ->
	A1 = radius_attributes:add(?UserName, UserName, RadAttributes),
	A2 = radius_attributes:add(?NasPort, 19, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasID, A2),
	A4 = radius_attributes:add(?CallingStationId,"DE-AD-BE-EF-FE-ED", A3),
	A5 = radius_attributes:add(?CalledStationId,"BA-DF-AD-CA-DD-AD:TestSSID", A4),
	A6 = radius_attributes:add(?MessageAuthenticator, <<0:128>>, A5),
	Request1 = #radius{code = ?AccessRequest, id = RadID,
		authenticator = Auth, attributes = A6},
	ReqPacket1 = radius:codec(Request1),
	MsgAuth1 = crypto:hmac(md5, Secret, ReqPacket1),
	A7 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A6),
	Request2 = Request1#radius{attributes = A7},
	ReqPacket2 = radius:codec(Request2),
	gen_udp:send(Socket, Address, Port, ReqPacket2).

accounting_request(StatusType, Socket, Address, Port,
		UserName, Secret, NasID, AcctSessionID, RadID, RadAttributes) ->
	A1 = radius_attributes:add(?UserName, UserName, RadAttributes),
	A2 = radius_attributes:add(?NasPort, 19, A1),
	A3 = radius_attributes:add(?NasIdentifier, NasID, A2),
	A4 = radius_attributes:add(?CallingStationId,"DE-AD-BE-EF-FE-ED", A3),
	A5 = radius_attributes:add(?CalledStationId,"BA-DF-AD-CA-DD-AD:TestSSID", A4),
	A6 = radius_attributes:add(?AcctSessionId, AcctSessionID, A5),
	A7 = radius_attributes:add(?AcctStatusType, StatusType, A6),
	AccAttributes = radius_attributes:codec(lists:sort(A7)),
	Acc1Length = size(AccAttributes) + 20,
	AccAuthenticator = crypto:hash(md5, [<<?AccountingRequest, RadID,
			Acc1Length:16, 0:128>>, AccAttributes, Secret]), 
	AccountingRequest = #radius{code = ?AccountingRequest, id = RadID,
			authenticator = AccAuthenticator, attributes = AccAttributes},
	AccPacket = radius:codec(AccountingRequest),
	ok = gen_udp:send(Socket, Address, Port, AccPacket),
	AccAuthenticator.

