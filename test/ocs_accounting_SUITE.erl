%%% ocs_accounting_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").
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

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

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
	{ok, ProdID} = ocs_test_lib:add_product(),
	NasID = atom_to_list(node()),
	Config1 = [{nas_id, NasID} | Config],
	{ok, EnvList} = application:get_env(ocs, diameter),
	{acct, [{Address, Port, Options } | _]} = lists:keyfind(acct, 1, EnvList),
	true = diameter:subscribe(?SVC_ACCT),
	ok = diameter:start_service(?SVC_ACCT, client_acct_service_opts(Options)),
	{ok, _Ref2} = connect(?SVC_ACCT, Address, Port, diameter_tcp),
	receive
		#diameter_event{service = ?SVC_ACCT, info = start} ->
			[{product_id, ProdID}, {diameter_auth_client, Address}] ++ Config1;
		_ ->
			{skip, diameter_client_acct_service_not_started}
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
	ProdID = ?config(product_id, Config),
	{ok, EnvList} = application:get_env(ocs, diameter),
	{acct, [{Address, Port, Options } | _]} = lists:keyfind(acct, 1, EnvList),
	Secret = ocs:generate_password(),
	ok = ocs:add_client(Address, Port, diameter, Secret),
	InitialAmount = 1000000000,
	Now = erlang:system_time(?MILLISECOND),
	TD = Now + 86400000,
	Buckets = [#bucket{bucket_type = octets,
			remain_amount = InitialAmount, termination_date = TD}],
	{ok, _} = ocs:add_subscriber(UserName, Password, ProdID, [], Buckets, []),
	[{username, UserName}, {password, Password}, {init_bal, InitialAmount}] ++ Config;
init_per_testcase(_TestCase, Config) ->
	SharedSecret = ct:get_config(radius_shared_secret),
	ok = ocs:add_client({127, 0, 0, 1}, 3799, radius, SharedSecret),
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
	[radius_accounting, radius_disconnect_session, radius_multisessions_not_allowed,
	radius_multisession, diameter_accounting, diameter_disconnect_session].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_accounting() ->
	[{userdata, [{doc, "Initiate and terminate a RADIUS accouting session"}]}].

radius_accounting(Config) ->
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
	Now = erlang:system_time(?MILLISECOND),
	TD = Now + 86400000,
	Buckets = [#bucket{bucket_type = octets,
			remain_amount = 1000, termination_date = TD}],
	ProdID = ?config(product_id, Config),
   {ok, _} = ocs:add_subscriber(PeerID, Password, ProdID, [], Buckets, []),
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
	ProdID = ?config(product_id, Config),
	AcctSessionID = "0B0055D1",
	{ok, [{auth, AuthInstance}, {acct, AcctInstance}]} = application:get_env(ocs, radius),
	[{AuthAddress, AuthPort, _}] = AuthInstance,
	[{AcctAddress, AcctPort, _}] = AcctInstance,
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]), 
	PeerID = "458565788",
	Secret = ct:get_config(radius_shared_secret),
	Password = ocs:generate_password(),
	Now = erlang:system_time(?MILLISECOND),
	TD = Now + 86400000,
	Buckets = [#bucket{bucket_type = octets,
			remain_amount = 1000, termination_date = TD}],
   {ok, _} = ocs:add_subscriber(PeerID, Password, ProdID, [], Buckets, []),
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

radius_multisessions_not_allowed() ->
	[{userdata, [{doc, "Start multiple RADIUS sessions for a subscriber when
			multiple RADIUS sessions are not allowed. Previous sessions should be disconnected
			allowing the last successfull session to exist."}]}].

radius_multisessions_not_allowed(Config) ->
	RadID1 = 8,
	NasID = ?config(nas_id, Config),
	ProdID = ?config(product_id, Config),
	AcctSessionID = "BAC10355",
	{ok, [{auth, AuthInstance}, {acct, AcctInstance}]} = application:get_env(ocs, radius),
	[{AuthAddress, AuthPort, _}] = AuthInstance,
	[{AcctAddress, AcctPort, _}] = AcctInstance,
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]),
	PeerID = ocs:generate_identity(),
	Secret = ct:get_config(radius_shared_secret),
	Password = ocs:generate_password(),
	Now = erlang:system_time(?MILLISECOND),
	TD = Now + 86400000,
	Buckets = [#bucket{bucket_type = octets,
			remain_amount = 1000, termination_date = TD}],
	{ok, _} = ocs:add_subscriber(PeerID, Password, ProdID, [], Buckets, [], true, false),
	ReqAuth = radius:authenticator(),
	HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID, ReqAuth, RadID1),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID2),
	{ok, #subscriber{multisession = false, session_attributes = SessionList1}}
			= ocs:find_subscriber(PeerID),
	[SessionAttr1] = SessionList1,
	F = fun({_, SessionAttributes}, Nas) ->
		{_, PeerID} = radius_attributes:find(?UserName, SessionAttributes),
		case radius_attributes:find(?NasIdentifier, SessionAttributes) of
			{_, Nas} ->
				ok;
			false ->
				{_, "127.0.0.1"} = radius_attributes:find(?NasIpAddress, SessionAttr1),
				ok
		end
	end,
	ok = F(SessionAttr1, NasID),
	Rad2ID1 = 5,
	NasID2 = "vlkf@ubip.net",
	authenticate_subscriber1(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID2, ReqAuth, Rad2ID1),
	ct:sleep(500),
	{ok, #subscriber{multisession = false, session_attributes = SessionList2}}
			= ocs:find_subscriber(PeerID),
	[SessionAttr2] = SessionList2,
	ok = F(SessionAttr2, NasID2),
	Rad2ID2 = Rad2ID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort, PeerID, Secret, NasID2,
			AcctSessionID, Rad2ID2),
	Rad2ID3 = Rad2ID2 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID2, AcctSessionID, Rad2ID3),
	{ok, #subscriber{multisession = false, session_attributes = []}}
			= ocs:find_subscriber(PeerID).

radius_multisession() ->
	[{userdata, [{doc, "Start multiple RADIUS sessions for a subscriber when
			multiple RADIUS sessions are allowed."}]}].

radius_multisession(Config) ->
	RadID1 = 11,
	NasID1 = "axe1@ap-1.org",
	AcctSessionID1 = "BAC10355",
	ProdID = ?config(product_id, Config),
	{ok, [{auth, AuthInstance}, {acct, AcctInstance}]} = application:get_env(ocs, radius),
	[{AuthAddress, AuthPort, _}] = AuthInstance,
	[{AcctAddress, AcctPort, _}] = AcctInstance,
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]),
	PeerID = ocs:generate_identity(),
	Secret = ct:get_config(radius_shared_secret),
	Password = ocs:generate_password(),
	Now = erlang:system_time(?MILLISECOND),
	TD = Now + 86400000,
	Buckets = [#bucket{bucket_type = octets,
			remain_amount = 1000000, termination_date = TD}],
	{ok, _} = ocs:add_subscriber(PeerID, Password, ProdID, [], Buckets, [], true, true),
	ReqAuth = radius:authenticator(),
	HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	%% Authenticate session 1
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID1, ReqAuth, RadID1),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID1, AcctSessionID1, RadID2),
	{ok, #subscriber{multisession = true, session_attributes = SessionList1}}
			= ocs:find_subscriber(PeerID),
	F1 = fun(F1, Session, [H1 | T1]) ->
				case lists:member(H1, Session) of
					true ->
						F1(F1, Session, T1);
					false ->
						not_found
				end;
			(_, _, []) ->
				ok
	end,
	F2 = fun(F2, [{_, H}| T], A) ->
				case F1(F1, H, A) of
					ok ->
						ok;
					not_found ->
						F2(F2, T, A)
				end;
			(_, [], _) ->
				not_found
	end,
	1 = length(SessionList1),
	ok = F2(F2, SessionList1, [{?UserName, PeerID}, {?NasIdentifier, NasID1}]),
	%% Authenticate session 2
	Rad2ID1 = 5,
	NasID2 = "axe2@ap-2.org",
	AcctSessionID2 = "BAC10356",
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID2, ReqAuth, Rad2ID1),
	ct:sleep(500),
	{ok, #subscriber{multisession = true, session_attributes = SessionList2}}
			= ocs:find_subscriber(PeerID),
	2 = length(SessionList2),
	ok = F2(F2, SessionList2, [{?UserName, PeerID}, {?NasIdentifier, NasID2}]),
	Rad2ID2 = Rad2ID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort, PeerID, Secret, NasID2,
			AcctSessionID2, Rad2ID2),
	%% Authenticate session 3
	Rad3ID1 = 21,
	NasID3 = "axe3@ap-3.org",
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID3, ReqAuth, Rad3ID1),
	ct:sleep(500),
	{ok, #subscriber{multisession = true, session_attributes = SessionList3}}
			= ocs:find_subscriber(PeerID),
	3 = length(SessionList3),
	ok = F2(F2, SessionList3, [{?UserName, PeerID}, {?NasIdentifier, NasID3}]),
	Rad3ID2 = Rad3ID1 + 1,
	AcctSessionID3 = "BAC10357",
	accounting_start(Socket, AcctAddress, AcctPort, PeerID, Secret, NasID3,
			AcctSessionID3, Rad3ID2),
	%% Disconnect session 2
	Rad2ID3 = Rad2ID2 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID2, AcctSessionID2, Rad2ID3),
	{ok, #subscriber{multisession = true, session_attributes = SessionList4}}
			= ocs:find_subscriber(PeerID),
	2 = length(SessionList4),
	ok = F2(F2, SessionList4, [{?UserName, PeerID}, {?NasIdentifier, NasID1}]),
	ok = F2(F2, SessionList4, [{?UserName, PeerID}, {?NasIdentifier, NasID3}]).

diameter_accounting() ->
	[{userdata, [{doc, "Initiate and terminate a DIAMETER accouting session"}]}].

diameter_accounting(Config) ->
	Username = ?config(username, Config),
	Password = ?config(password, Config),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
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
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MultiServices_CC,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = [TotalOctets]} = GrantedUnits,
	NewRequestNum = RequestNum + 1,
	Answer1 = diameter_accounting_stop(SId, Username, NewRequestNum, TotalOctets),
	true = is_record(Answer1, diameter_cc_app_CCA),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer1.

diameter_disconnect_session() ->
	[{userdata, [{doc, "Disconnect a DIAMETER accouting session based on usage"}]}].

diameter_disconnect_session(Config) ->
	Username = ?config(username, Config),
	Password = ?config(password, Config),
	OrigBalance = ?config(init_bal, Config),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
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
			'CC-Request-Number' = RequestNum0,
			'Multiple-Services-Credit-Control' = [MultiServices_CC0]} = Answer0,
	#'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits0]} = MultiServices_CC0,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = [InitBalance]} = GrantedUnits0,
	Usage0 = trunc(InitBalance/10),
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_accounting_interim(SId, Username, RequestNum1, Usage0),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum1,
			'Multiple-Services-Credit-Control' = [MultiServices_CC1]} = Answer1,
	#'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits1]} = MultiServices_CC1,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = [Balance1]} = GrantedUnits1,
	Usage2 = trunc(Balance1/10),
	RequestNum2 = RequestNum1 + 1,
	Answer2 = diameter_accounting_interim(SId, Username, RequestNum2, Usage2),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum2,
			'Multiple-Services-Credit-Control' = [MultiServices_CC2]} = Answer2,
	#'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits2]} = MultiServices_CC2,
	#'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = [Balance2]} = GrantedUnits2,
	Usage3 = OrigBalance,
	RequestNum3 = RequestNum2 + 1,
	Answer3 = diameter_accounting_interim(SId, Username, RequestNum3, Usage3),
	#diameter_cc_app_CCA{'Result-Code' = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum3} = Answer3.

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
		{restrict_connections, false},
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
		{restrict_connections, false},
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
			'User-Name' = [Username], 'User-Password' = [Password]},
	{ok, Answer} = diameter:call(?SVC_AUTH, nas_app_test, NAS_AAR, []),
	Answer.

%% @hidden
diameter_accounting_start(SId, Username, RequestNum) ->
	Subscription_Id = #'diameter_cc_app_Subscription-Id'{
			'Subscription-Id-Type' = ?'DIAMETER_CC_APP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	RequestedUnits = #'diameter_cc_app_Requested-Service-Unit' {
			'CC-Total-Octets' = [10000000]},
	MultiServices_CC = #'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits]}, 
	CC_CCR = #diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = "nas45@testdomain.com" ,
			'User-Name' = [Username],
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Subscription-Id' = [Subscription_Id],
			'Multiple-Services-Credit-Control' = [MultiServices_CC]},
	{ok, Answer} = diameter:call(?SVC_ACCT, cc_app_test, CC_CCR, []),
	Answer.
	
%% @hidden
diameter_accounting_stop(SId, Username, RequestNum, Usage) ->
	Subscription_Id = #'diameter_cc_app_Subscription-Id'{
			'Subscription-Id-Type' = ?'DIAMETER_CC_APP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	RequestedUnits = #'diameter_cc_app_Requested-Service-Unit' {
			'CC-Total-Octets' = [100000000]},
	UsedUnits = #'diameter_cc_app_Used-Service-Unit'{'CC-Total-Octets' = [Usage]},
	MultiServices_CC = #'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits],
			'Requested-Service-Unit' = [RequestedUnits]}, 
	CC_CCR = #diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = "nas45@testdomain.com" ,
			'User-Name' = [Username],
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [Subscription_Id]},
	{ok, Answer} = diameter:call(?SVC_ACCT, cc_app_test, CC_CCR, []),
	Answer.

%% @hidden
diameter_accounting_interim(SId, Username, RequestNum, Usage) ->
	Subscription_Id = #'diameter_cc_app_Subscription-Id'{
			'Subscription-Id-Type' = ?'DIAMETER_CC_APP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	UsedUnits = #'diameter_cc_app_Used-Service-Unit'{'CC-Total-Octets' = [Usage]},
	RequestedUnits = #'diameter_cc_app_Requested-Service-Unit' {
			'CC-Total-Octets' = [100000000]},
	MultiServices_CC = #'diameter_cc_app_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits],
			'Requested-Service-Unit' = [RequestedUnits]}, 
	CC_CCR = #diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = "nas45@testdomain.com" ,
			'User-Name' = [Username],
			'CC-Request-Type' = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [Subscription_Id]},
	{ok, Answer} = diameter:call(?SVC_ACCT, cc_app_test, CC_CCR, []),
	Answer.
	
authenticate_subscriber1(Socket, Address,
		Port, PeerID, Password, Secret, NasID, ReqAuth, RadID) ->
	RadAttribute = radius_attributes:add(?UserPassword, Password, []),
	access_request(Socket, Address,
		Port, PeerID, Secret, NasID, ReqAuth, RadID, RadAttribute),
	disconnect_request(),
	access_accept(Socket, Address, Port, RadID).

