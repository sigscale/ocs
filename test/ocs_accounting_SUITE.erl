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
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("../include/diameter_gen_cc_application_rfc4006.hrl").

-define(SVC_AUTH, diameter_client_auth_service).
-define(SVC_ACCT, diameter_client_acct_service).
-define(BASE_APPLICATION_ID, 0).
-define(NAS_APPLICATION_ID, 1).
-define(CC_APPLICATION_ID, 4).

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
	Config2 = [{nas_id, NasID} | Config1],
	{ok, [{auth, DiaAuthInstance}, {acct, DiaAcctInstances}]} =
			application:get_env(ocs, diameter),
	[{AuthAddress, AuthPort, _}] = DiaAuthInstance,
	[{AcctAddress, AcctPort, _}] = DiaAcctInstances,
	true = diameter:subscribe(?SVC_AUTH),
	ok = diameter:start_service(?SVC_AUTH, client_auth_service_opts()),
	{ok, _Ref1} = connect(?SVC_AUTH, AuthAddress, AuthPort, diameter_tcp),
	receive
		#diameter_event{service = ?SVC_AUTH, info = start} ->
			true = diameter:subscribe(?SVC_ACCT),
			ok = diameter:start_service(?SVC_ACCT, client_acct_service_opts()),
			{ok, _Ref2} = connect(?SVC_ACCT, AcctAddress, AcctPort, diameter_tcp),
			receive
				#diameter_event{service = ?SVC_ACCT, info = start} ->
					[{diameter_auth_client, AuthAddress}] ++ Config2;
				_ ->
					{skip, diameter_client_acct_service_not_started}
			end;
		_ ->
			{skip, diameter_client_auth_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop_service(?SVC_AUTH),
	ok = diameter:stop_service(?SVC_ACCT),
	ok = ocs_test_lib:stop(),
	ok = ocs:delete_subscriber("25252525"),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(TestCase, Config) when
		TestCase == diameter_accounting; TestCase == diameter_disconnect_session ->
	UserName = "SlimShady",
	Password = "TeRcEs",
	{ok, [{auth, AuthInstance}, {acct, _}]} = application:get_env(ocs, diameter),
	[{Address, Port, _}] = AuthInstance,
	Secret = "s3cr3t",
	ok = ocs:add_client(Address, Port, diameter, Secret),
	ok = ocs:add_subscriber(UserName, Password, [], 1000000),
	[{username, UserName}, {password, Password}] ++ Config;
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when
		TestCase == diameter_accounting; TestCase == diameter_disconnect_session ->
	UserName= ?config(username, Config),
	Client = ?config(diameter_auth_client, Config),
	ok = ocs:delete_client(Client),
	ok = ocs:delete_subscriber(UserName);
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
	[radius_accouting, radius_disconnect_session, diameter_accounting,
	diameter_disconnect_session].

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

diameter_accounting() ->
	[{userdata, [{doc, "Initiate and terminate a Diameter accouting session"}]}].

diameter_accounting(Config) ->
	Username = ?config(username, Config),
	Password = ?config(password, Config),
	SId = diameter:session_id(atom_to_list(?FUNCTION_NAME)),
	Answer = diameter_authentication(SId, Username, Password),
	true = is_record(Answer, diameter_nas_app_AAA),
	OriginHost = list_to_binary("ocs.sigscale.com"),
	OriginRealm = list_to_binary("sigscale.com"),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer,
	RequestNum = 0,
	Answer0 = diameter_accounting_start(SId, Username, RequestNum),
	true = is_record(Answer0, diameter_cc_app_CCA),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum} = Answer0,
	NewRequestNum = RequestNum + 1,
	Answer1 = diameter_accounting_stop(SId, Username, NewRequestNum),
	true = is_record(Answer1, diameter_cc_app_CCA),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer1.

diameter_disconnect_session() ->
	[{userdata, [{doc, "Disconnect a Diameter accouting session based on usage"}]}].

diameter_disconnect_session(Config) ->
	register(?FUNCTION_NAME, self()),
	Username = ?config(username, Config),
	Password = ?config(password, Config),
	SId = diameter:session_id(atom_to_list(?FUNCTION_NAME)),
	Answer = diameter_authentication(SId, Username, Password),
	true = is_record(Answer, diameter_nas_app_AAA),
	OriginHost = list_to_binary("ocs.sigscale.com"),
	OriginRealm = list_to_binary("sigscale.com"),
	#diameter_nas_app_AAA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm} = Answer,
	RequestNum0 = 0,
	Answer0 = diameter_accounting_start(SId, Username, RequestNum0),
	true = is_record(Answer0, diameter_cc_app_CCA),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum0, 'Granted-Service-Unit' = GrantedUnits0} = Answer0,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = Balance0} = GrantedUnits0,
	Usage0 = Balance0 - 1000000,
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_accounting_interim(SId, Username, RequestNum1, Usage0),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum1, 'Granted-Service-Unit' = GrantedUnits1} = Answer1,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = Balance1} = GrantedUnits1,
	Usage2 = Balance1 - 1000000,
	RequestNum2 = RequestNum1 + 1,
	Answer2 = diameter_accounting_interim(SId, Username, RequestNum2, Usage2),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum2, 'Granted-Service-Unit' = GrantedUnits2} = Answer2,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = Balance2} = GrantedUnits2,
	Usage3 = Balance2 - 10000000,
	RequestNum3 = RequestNum2 + 1,
	% Final Interim
	Answer3 = diameter_accounting_interim(SId, Username, RequestNum3, Usage3),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum3, 'Granted-Service-Unit' = GrantedUnits3} = Answer3,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = _Balance3} = GrantedUnits3,
	Result = receive
		ASR ->
			ASR
	end,
	true = is_record(Result, diameter_base_ASR),
	#diameter_base_ASR{'Session-Id' = SId, 'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
	'Destination-Realm' = _DRealm, 'Destination-Host' = _DHost,
	'Auth-Application-Id' = ?CC_APPLICATION_ID, 'User-Name' = Username} = Result,
	ASA = #diameter_base_ASA{'Session-Id' = SId,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
	_Answer4 = diameter:call(?SVC_AUTH, cc_app_test, ASA, []).

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

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
client_auth_service_opts() ->
	[{'Origin-Host', "client.testdomain.com"},
		{'Origin-Realm', "testdomain.com"},
		{'Vendor-Id', 0},
		{'Product-Name', "Test Client"},
		{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?NAS_APPLICATION_ID]},
		{string_decode, false},
		{application, [{alias, base_app_test},
				{dictionary, diameter_gen_base_rfc6733},
				{module, diameter_test_client_cb}]},
		{application, [{alias, nas_app_test},
				{dictionary, diameter_gen_nas_application_rfc7155},
				{module, diameter_test_client_cb}]}].

%% @hidden
client_acct_service_opts() ->
	[{'Origin-Host', "client.testdomain.com"},
		{'Origin-Realm', "testdomain.com"},
		{'Vendor-Id', 0},
		{'Product-Name', "Test Acct Client"},
		{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?CC_APPLICATION_ID]},
		{string_decode, false},
		{application, [{alias, base_app_test},
				{dictionary, diameter_gen_base_rfc6733},
				{module, diameter_test_client_cb}]},
		{application, [{alias, cc_app_test},
				{dictionary, diameter_gen_cc_application_rfc4006},
				{module, diameter_test_client_cb}]}].

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans},
		{transport_config, [{raddr, RemAddr},
		{rport, RemPort},
		{reuseaddr, true}
		| [{ip, LocalAddr}]]}].

%% @hidden
diameter_authentication(SId, Username, Password) ->
	NAS_AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID ,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
			'User-Name' = Username, 'User-Password' = Password},
	{ok, Answer} = diameter:call(?SVC_AUTH, nas_app_test, NAS_AAR, []),
	Answer.

%% @hidden
diameter_accounting_start(SId, Username, RequestNum) ->
	CC_CCR = #diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = "nas45@testdomain.com" ,
			'User-Name' = Username,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum},
	{ok, Answer} = diameter:call(?SVC_ACCT, cc_app_test, CC_CCR, []),
	Answer.
	
%% @hidden
diameter_accounting_stop(SId, Username, RequestNum) ->
	CC_CCR = #diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = "nas45@testdomain.com" ,
			'User-Name' = Username,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum},
	{ok, Answer} = diameter:call(?SVC_ACCT, cc_app_test, CC_CCR, []),
	Answer.

%% @hidden
diameter_accounting_interim(SId, Username, RequestNum, Usage) ->
	UsedUnits = #'diameter_cc_app_Used-Service-Unit'{'CC-Total-Octets' = Usage},
	CC_CCR = #diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = "nas45@testdomain.com" ,
			'User-Name' = Username,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Used-Service-Unit' = UsedUnits},
	{ok, Answer} = diameter:call(?SVC_ACCT, cc_app_test, CC_CCR, []),
	Answer.
	
