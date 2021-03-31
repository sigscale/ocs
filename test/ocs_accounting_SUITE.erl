%%% ocs_accounting_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

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
-include_lib("../include/diameter_gen_3gpp_ro_application.hrl").
-include_lib("../include/diameter_gen_3gpp.hrl").
-include_lib("../include/diameter_gen_ietf.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

-define(EPOCH_OFFSET, 2208988800).
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
	{require, radius},
	{default_config, radius, [{secret, "abc345"}]},
	{require, diameter},
	{default_config, diameter, [{address, {127,0,0,1}}]},
	{timetrap, {seconds, 8}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:load(ocs),
	RadiusAddress = ct:get_config({radius, address}, {127,0,0,1}),
	RadiusAuthPort = ct:get_config({radius, auth_port}, rand:uniform(64511) + 1024),
	RadiusAcctPort = ct:get_config({radius, acct_port}, rand:uniform(64511) + 1024),
	RadiusAppVar = [{auth, [{RadiusAddress, RadiusAuthPort, []}]},
			{acct, [{RadiusAddress, RadiusAcctPort, []}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterAddress = ct:get_config({diameter, address}, {127,0,0,1}),
	DiameterAuthPort = ct:get_config({diameter, auth_port}, rand:uniform(64511) + 1024),
	DiameterAcctPort = ct:get_config({diameter, acct_port}, rand:uniform(64511) + 1024),
	DiameterAppVar = [{auth, [{DiameterAddress, DiameterAuthPort, []}]},
		{acct, [{DiameterAddress, DiameterAcctPort, []}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = application:set_env(ocs, min_reserve_octets, 1000000),
	ok = application:set_env(ocs, min_reserve_seconds, 60),
	ok = application:set_env(ocs, min_reserve_messages, 1),
	ok = ocs_test_lib:start(),
	Realm = ct:get_config({diameter, realm}, "mnc001.mcc001.3gppnetwork.org"),
	Host = ct:get_config({diameter, host}, atom_to_list(?MODULE) ++ "." ++ Realm),
	Config1 = [{host, Host}, {realm, Realm},
			{radius_auth_address, RadiusAddress},
			{radius_auth_port, RadiusAuthPort},
			{radius_acct_address, RadiusAddress},
			{radius_acct_port, RadiusAcctPort},
			{diameter_acct_address, DiameterAddress} | Config],
	ok = diameter:start_service(?MODULE, client_acct_service_opts(Config1)),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref2} = connect(?MODULE, DiameterAddress, DiameterAcctPort, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Info}
				when element(1, Info) == up ->
			Config1;
		_Other ->
			{skip, diameter_client_acct_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = diameter:remove_transport(?MODULE, true),
	ok = ocs_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(TestCase, Config) when
		TestCase == radius_disconnect_session;
		TestCase == radius_multisession_disallowed ->
	NasID = erlang:ref_to_list(make_ref()),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]),
	{ok, Port} = inet:port(Socket),
	Config1 = [{nas_id, NasID}, {radius_disc_port, Port},
			{radius_disc_socket, Socket} | Config],
	init_per_testcase1(TestCase, Config1);
init_per_testcase(TestCase, Config) ->
	NasID = erlang:ref_to_list(make_ref()),
	Config1 = [{nas_id, NasID}, {radius_disc_port, undefined} | Config],
	init_per_testcase1(TestCase, Config1).

init_per_testcase1(TestCase, Config) when
		TestCase == radius_accounting;
		TestCase == radius_disconnect_session;
		TestCase == radius_multisession_disallowed;
		TestCase == radius_multisession ->
	SharedSecret = ct:get_config({radius, secret}),
	Address = ?config(radius_acct_address, Config),
	DiscPort = ?config(radius_disc_port, Config),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]),
	{ok, _} = ocs:add_client(Address, DiscPort, radius, SharedSecret, true),
	[{radius_nas_socket, Socket}, {radius_nas_client, Address} | Config];
init_per_testcase1(TestCase, Config) when
		TestCase == diameter_scur;
		TestCase == diameter_scur_cud;
		TestCase == diameter_scur_no_credit;
		TestCase == diameter_scur_depletion;
		TestCase == diameter_ecur;
		TestCase == diameter_ecur_no_credit;
		TestCase == diameter_iec_cud;
		TestCase == diameter_iec_dud;
		TestCase == diameter_voice_out;
		TestCase == diameter_voice_in;
		TestCase == diameter_voice_out_tariff;
		TestCase == diameter_voice_in_tariff ->
	Address = ?config(diameter_acct_address, Config),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when
		TestCase == radius_disconnect_session;
		TestCase == radius_multisession_disallowed ->
	Address = ?config(radius_nas_client, Config),
	ok = ocs:delete_client(Address),
	Socket = ?config(radius_disc_socket, Config),
	gen_udp:close(Socket);
end_per_testcase(TestCase, Config) when
		TestCase == radius_accounting;
		TestCase == radius_multisession ->
	Address = ?config(radius_nas_client, Config),
	ok = ocs:delete_client(Address);
end_per_testcase(TestCase, Config) when
		TestCase == diameter_scur;
		TestCase == diameter_scur_cud;
		TestCase == diameter_scur_no_credit;
		TestCase == diameter_scur_depletion;
		TestCase == diameter_ecur;
		TestCase == diameter_iec_cud;
		TestCase == diameter_iec_dud;
		TestCase == diameter_ecur_no_credit;
		TestCase == diameter_voice_out;
		TestCase == diameter_voice_in;
		TestCase == diameter_voice_out_tariff;
		TestCase == diameter_voice_in_tariff ->
	Address = ?config(diameter_acct_address, Config),
	ok = ocs:delete_client(Address).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[radius_accounting, radius_disconnect_session,
	radius_multisession_disallowed, radius_multisession,
	diameter_scur, diameter_scur_cud,
	diameter_scur_no_credit, diameter_scur_depletion,
	diameter_ecur, diameter_ecur_no_credit,
	diameter_iec_cud, diameter_iec_dud,
	diameter_voice_out, diameter_voice_in,
	diameter_voice_out_tariff, diameter_voice_in_tariff].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_accounting() ->
	[{userdata, [{doc, "Initiate and terminate a RADIUS accounting session"}]}].

radius_accounting(Config) ->
	RadID1 = 1,
	NasID = ?config(nas_id, Config),
	AcctSessionID = ocs:generate_identity(),
	AuthAddress = ?config(radius_auth_address, Config),
	AuthPort = ?config(radius_auth_port, Config),
	AcctAddress = ?config(radius_acct_address, Config),
	AcctPort = ?config(radius_acct_port, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerID, password = Password} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Secret = ct:get_config({radius, secret}),
	ReqAuth = radius:authenticator(),
	HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	Socket = ?config(radius_nas_socket, Config),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID, ReqAuth, RadID1, AcctSessionID),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID2),
	RadID3 = RadID2 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID3).

radius_disconnect_session() ->
	[{userdata, [{doc, "Disconnect a RADIUS accounting session based on usage"}]}].

radius_disconnect_session(Config) ->
	RadID1 = 10,
	NasID = ?config(nas_id, Config),
	AcctSessionID = ocs:generate_identity(),
	AuthAddress = ?config(radius_auth_address, Config),
	AuthPort = ?config(radius_auth_port, Config),
	AcctAddress = ?config(radius_acct_address, Config),
	AcctPort = ?config(radius_acct_port, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = PeerID, password = Password} =  add_service(ProdRef),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Secret = ct:get_config({radius, secret}),
	ReqAuth = radius:authenticator(),
   HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	Socket = ?config(radius_nas_socket, Config),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID, ReqAuth, RadID1, AcctSessionID),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID2),
	RadID3 = RadID2 + 1,
	accounting_interim(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID3, 750000123, 750000456),
	RadID4 = RadID3 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID, RadID4, 1350000987, 1350000654),
	DiscSocket = ?config(radius_disc_socket, Config),
	disconnect_request(DiscSocket).

radius_multisession_disallowed() ->
	[{userdata, [{doc, "Start multiple RADIUS sessions for a subscriber when
			multiple RADIUS sessions are not allowed. Previous sessions should be disconnected
			allowing the last successfull session to exist."}]}].

radius_multisession_disallowed(Config) ->
	RadID1 = 8,
	NasID = ?config(nas_id, Config),
	AcctSessionID1 = ocs:generate_identity(),
	AuthAddress = ?config(radius_auth_address, Config),
	AuthPort = ?config(radius_auth_port, Config),
	AcctAddress = ?config(radius_acct_address, Config),
	AcctPort = ?config(radius_acct_port, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	#service{name = User, password = Password} =  add_service(ProdRef),
	PeerID = binary_to_list(User),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Secret = ct:get_config({radius, secret}),
	ReqAuth = radius:authenticator(),
	HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	Socket = ?config(radius_nas_socket, Config),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID, ReqAuth, RadID1, AcctSessionID1),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID, AcctSessionID1, RadID2),
	{ok, #service{multisession = false, session_attributes = SessionList1}}
			= ocs:find_service(PeerID),
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
	NasID2 = "vlkf@example.net",
	AcctSessionID2 = ocs:generate_identity(),
	DiscSocket = ?config(radius_disc_socket, Config),
	authenticate_subscriber1(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID2, ReqAuth, Rad2ID1,
			DiscSocket, AcctSessionID2),
	ct:sleep(500),
	{ok, #service{multisession = false, session_attributes = SessionList2}}
			= ocs:find_service(PeerID),
	[SessionAttr2] = SessionList2,
	ok = F(SessionAttr2, NasID2),
	Rad2ID2 = Rad2ID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort, PeerID, Secret, NasID2,
			AcctSessionID2, Rad2ID2),
	Rad2ID3 = Rad2ID2 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID2, AcctSessionID2, Rad2ID3),
	{ok, #service{multisession = false, session_attributes = []}}
			= ocs:find_service(PeerID).

radius_multisession() ->
	[{userdata, [{doc, "Start multiple RADIUS sessions for a subscriber when
			multiple RADIUS sessions are allowed."}]}].

radius_multisession(Config) ->
	RadID1 = 11,
	NasID1 = "axe1@ap-1.org",
	AcctSessionID1 = ocs:generate_identity(),
	AuthAddress = ?config(radius_auth_address, Config),
	AuthPort = ?config(radius_auth_port, Config),
	AcctAddress = ?config(radius_acct_address, Config),
	AcctPort = ?config(radius_acct_port, Config),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	PeerID = ocs:generate_identity(),
	Password = ocs:generate_identity(),
	State = active,
	{ok, #service{}} = ocs:add_service(PeerID, Password, State,
			ProdRef, [], [], true, true),
	B1 = bucket(octets, rand:uniform(100000)),
	_BId = add_bucket(ProdRef, B1),
	Secret = ct:get_config({radius, secret}),
	ReqAuth = radius:authenticator(),
	HiddenPassword = radius_attributes:hide(Secret, ReqAuth, Password),
	Socket = ?config(radius_nas_socket, Config),
	%% Authenticate session 1
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID1, ReqAuth, RadID1, AcctSessionID1),
	RadID2 = RadID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID1, AcctSessionID1, RadID2),
	{ok, #service{multisession = true, session_attributes = SessionList1}}
			= ocs:find_service(PeerID),
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
	AcctSessionID2 = ocs:generate_identity(),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID2, ReqAuth, Rad2ID1, AcctSessionID2),
	ct:sleep(500),
	{ok, #service{multisession = true, session_attributes = SessionList2}}
			= ocs:find_service(PeerID),
	2 = length(SessionList2),
	ok = F2(F2, SessionList2, [{?UserName, PeerID}, {?NasIdentifier, NasID2}]),
	Rad2ID2 = Rad2ID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort, PeerID, Secret, NasID2,
			AcctSessionID2, Rad2ID2),
	%% Authenticate session 3
	Rad3ID1 = 21,
	NasID3 = "axe3@ap-3.org",
	AcctSessionID3 = ocs:generate_identity(),
	authenticate_subscriber(Socket, AuthAddress, AuthPort, PeerID,
			HiddenPassword, Secret, NasID3, ReqAuth, Rad3ID1, AcctSessionID3),
	ct:sleep(500),
	{ok, #service{multisession = true, session_attributes = SessionList3}}
			= ocs:find_service(PeerID),
	3 = length(SessionList3),
	ok = F2(F2, SessionList3, [{?UserName, PeerID}, {?NasIdentifier, NasID3}]),
	Rad3ID2 = Rad3ID1 + 1,
	accounting_start(Socket, AcctAddress, AcctPort, PeerID, Secret, NasID3,
			AcctSessionID3, Rad3ID2),
	%% Disconnect session 2
	Rad2ID3 = Rad2ID2 + 1,
	accounting_stop(Socket, AcctAddress, AcctPort,
			PeerID, Secret, NasID2, AcctSessionID2, Rad2ID3),
	{ok, #service{multisession = true, session_attributes = SessionList4}}
			= ocs:find_service(PeerID),
	2 = length(SessionList4),
	ok = F2(F2, SessionList4, [{?UserName, PeerID}, {?NasIdentifier, NasID1}]),
	ok = F2(F2, SessionList4, [{?UserName, PeerID}, {?NasIdentifier, NasID3}]).

diameter_scur() ->
	[{userdata, [{doc, "DIAMETER Session Charging with Unit Reservation (SCUR)"}]}].

diameter_scur(_Config) ->
	P1 = price(usage, octets, rand:uniform(10000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	Username = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(Username, Password, ProdRef, []),
	Balance = rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	Answer0 = diameter_scur_start(SId, Username, RequestNum, rand:uniform(Balance)),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MultiServices_CC,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalOctets]} = GrantedUnits,
	NewRequestNum = RequestNum + 1,
	Answer1 = diameter_scur_stop(SId, Username, NewRequestNum, TotalOctets),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer1.

diamneter_scur_cud() ->
	[{userdata, [{doc, "DIAMETER SCUR voice with Centralized Unit Determination"}]}].

diameter_scur_cud(_Config) ->
	UnitSize = 60,
	P1 = price(usage, seconds, UnitSize, rand:uniform(10000000)),
	OfferId = add_offer([P1], 5),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	{ok, #service{}} = ocs:add_service(MSISDN, undefined, ProdRef, []),
	Balance = UnitSize * rand:uniform(100),
	B1 = bucket(seconds, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	RSU = #'3gpp_ro_Requested-Service-Unit' {},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CallingPartyAddress = "tel:+" ++ ocs:generate_identity(),
	CalledPartyAddress = "tel:+" ++ ocs:generate_identity(),
	ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' =
			[#'3gpp_ro_IMS-Information'{
					'Node-Functionality' = ?'3GPP_RO_NODE-FUNCTIONALITY_AS',
					'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_ORIGINATING_ROLE'],
					'Calling-Party-Address' = [CallingPartyAddress],
					'Called-Party-Address' = [CalledPartyAddress]}]},
	CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32260@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer0} = diameter:call(?MODULE, cc_app_test, CCR, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [UnitSize]} = GSU,
	NewRequestNum = RequestNum + 1,
	Answer1 = diameter_scur_stop(SId, MSISDN, NewRequestNum, UnitSize),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer1.

diameter_scur_no_credit() ->
	[{userdata, [{doc, "DIAMETER SCUR with insufficient credit"}]}].

diameter_scur_no_credit(_Config) ->
	UnitSize = 1000000 + rand:uniform(10000),
	Amount = rand:uniform(100),
	P1 = price(usage, octets, UnitSize, Amount),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceID = list_to_binary(ocs:generate_identity()),
	{ok, #service{}} = ocs:add_service(ServiceID, undefined, ProdRef, []),
	Balance = rand:uniform(100000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	#'3gpp_ro_CCA'{'Result-Code' = ?'IETF_RESULT-CODE_CREDIT_LIMIT_REACHED',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0}
			= diameter_scur_start(SId, ServiceID, 0,
			Balance + rand:uniform(Balance)).

diameter_scur_depletion() ->
	[{userdata, [{doc, "DIAMETER SCUR mid-session out of credit"}]}].

diameter_scur_depletion(_Config) ->
	UnitSize = 1000000 + rand:uniform(10000),
	P1 = price(usage, octets, UnitSize, rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceID = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(ServiceID, undefined, ProdRef, []),
	Balance = 1000000 + rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Multiple-Services-Credit-Control' = [MSCC1]}
			= diameter_scur_start(SId, ServiceID, 0, Balance div 3),
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU1]} = MSCC1,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Grant1]} = GSU1,
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = 1,
			'Multiple-Services-Credit-Control' = [MSCC2]}
			= diameter_scur_interim(SId, ServiceID, 1, Grant1, Balance div 3),
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU2]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Grant2]} = GSU2,
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = 2,
			'Multiple-Services-Credit-Control' = [MSCC3]}
			= diameter_scur_interim(SId, ServiceID, 2, Grant2, Balance div 3),
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU3]} = MSCC3,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Grant3]} = GSU3,
	#'3gpp_ro_CCA'{'Result-Code' = ?'IETF_RESULT-CODE_CREDIT_LIMIT_REACHED',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = 3}
			= diameter_scur_interim(SId, ServiceID, 3, Grant3, Balance div 3).

diameter_ecur() ->
	[{userdata, [{doc, "DIAMETER Event Charging with Unit Reservation (ECUR)"}]}].

diameter_ecur(_Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	CalledParty = ocs:generate_identity(),
	CallingParty = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(CallingParty, undefined, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = CallingParty},
	RSU = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Service-Specific-Units' = [1]},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [CalledParty]}]}]}]},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [CallingParty],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [1]} = GSU,
	USU = #'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [1]},
	MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU]},
	CCR2 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32251@3gpp.org" ,
			'User-Name' = [CalledParty],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = 1,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MSCC3],
			'Subscription-Id' = [SubscriptionId],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer2} = diameter:call(?MODULE, cc_app_test, CCR2, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = 1} = Answer2.

diameter_ecur_no_credit() ->
	[{userdata, [{doc, "DIAMETER ECUR with insufficient credit"}]}].

diameter_ecur_no_credit(_Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	CalledParty = ocs:generate_identity(),
	CallingParty = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(CallingParty, undefined, ProdRef, []),
	B1 = bucket(messages, 2),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = CallingParty},
	RSU = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Service-Specific-Units' = [5]},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [CalledParty]}]}]}]},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "nas45.32274@3gpp.org",
			'User-Name' = [CallingParty],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'IETF_RESULT-CODE_CREDIT_LIMIT_REACHED',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0} = Answer1.

diameter_iec_dud() ->
	[{userdata, [{doc, "DIAMETER Immediate Event Charging (IEC) with decentralized unit determination"}]}].

diameter_iec_dud(_Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	CalledParty = ocs:generate_identity(),
	CallingParty = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(CallingParty, undefined, ProdRef, []),
	B1 = bucket(messages, 5),
	BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = CallingParty},
	RSU = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Service-Specific-Units' = [1]},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [CalledParty]}]}]}]},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [CallingParty],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
			'CC-Request-Number' = 0,
			'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
			'CC-Request-Number' = 0,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [1]} = GSU,
	{ok, #bucket{remain_amount = 4}} = ocs:find_bucket(BId).

diameter_iec_cud() ->
	[{userdata, [{doc, "DIAMETER Immediate Event Charging (IEC) with centralized unit determination"}]}].

diameter_iec_cud(_Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	CalledParty = ocs:generate_identity(),
	CallingParty = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(CallingParty, undefined, ProdRef, []),
	B1 = bucket(messages, 5),
	BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = CallingParty},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [CalledParty]}]}]}]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [CallingParty],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
			'CC-Request-Number' = 0,
			'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
			'CC-Request-Number' = 0,
			'Multiple-Services-Credit-Control' = [MSCC]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [1]} = GSU,
	{ok, #bucket{remain_amount = 4}} = ocs:find_bucket(BId).

diameter_voice_out() ->
	[{userdata, [{doc, "DIAMETER SCUR for outgoing voice call"}]}].

diameter_voice_out(_Config) ->
	UnitSize = 60,
	P1 = price(usage, seconds, UnitSize, rand:uniform(1000000)),
	Incoming = #char_value{value = "answer"},
	CallDirection1 = #char_value_use{name = "callDirection",
			specification = "5", values = [Incoming]},
	P2 = P1#price{char_value_use = [CallDirection1]},
	P3 = price(usage, seconds, UnitSize, rand:uniform(1000000)),
	Outgoing = #char_value{value = "originate"},
	CallDirection2 = #char_value_use{name = "callDirection",
			specification = "5", values = [Outgoing]},
	P4 = P3#price{char_value_use = [CallDirection2]},
	OfferId = add_offer([P2, P4], 9),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	{ok, #service{}} = ocs:add_service(MSISDN, undefined, ProdRef, []),
	Balance1 = rand:uniform(1000000000),
	B1 = bucket(seconds, Balance1),
	B1ref = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	RSU = #'3gpp_ro_Requested-Service-Unit' {},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CallingPartyAddress = "tel:+" ++ ocs:generate_identity(),
	CalledPartyAddress = "tel:+" ++ ocs:generate_identity(),
	ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' =
			[#'3gpp_ro_IMS-Information'{
					'Node-Functionality' = ?'3GPP_RO_NODE-FUNCTIONALITY_AS',
					'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_ORIGINATING_ROLE'],
					'Calling-Party-Address' = [CallingPartyAddress],
					'Called-Party-Address' = [CalledPartyAddress]}]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32260@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedUnits]} = GSU,
	NewRequestNum = RequestNum + 1,
	UsedUnits = rand:uniform(GrantedUnits),
	USU = #'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedUnits]},
	MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU]},
	CCR2 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum,
			'Multiple-Services-Credit-Control' = [MSCC3],
			'Event-Timestamp' = [calendar:universal_time()]},
	{ok, Answer2} = diameter:call(?MODULE, cc_app_test, CCR2, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer2,
	Balance2 = Balance1 - UsedUnits,
	{ok, #bucket{remain_amount = Balance2}} = ocs:find_bucket(B1ref).

diameter_voice_in() ->
	[{userdata, [{doc, "DIAMETER SCUR for answered voice call"}]}].

diameter_voice_in(_Config) ->
	UnitSize = 60,
	P1 = price(usage, seconds, UnitSize, rand:uniform(1000000)),
	Incoming = #char_value{value = "answer"},
	CallDirection1 = #char_value_use{name = "callDirection",
			specification = "5", values = [Incoming]},
	P2 = P1#price{char_value_use = [CallDirection1]},
	P3 = price(usage, seconds, UnitSize, rand:uniform(1000000)),
	Outgoing = #char_value{value = "originate"},
	CallDirection2 = #char_value_use{name = "callDirection",
			specification = "5", values = [Outgoing]},
	P4 = P3#price{char_value_use = [CallDirection2]},
	OfferId = add_offer([P2, P4], 9),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	{ok, #service{}} = ocs:add_service(MSISDN, undefined, ProdRef, []),
	Balance1 = rand:uniform(1000000000),
	B1 = bucket(seconds, Balance1),
	B1ref = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	RSU = #'3gpp_ro_Requested-Service-Unit' {},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CallingPartyAddress = "tel:+" ++ ocs:generate_identity(),
	CalledPartyAddress = "tel:+" ++ ocs:generate_identity(),
	ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' =
			[#'3gpp_ro_IMS-Information'{
					'Node-Functionality' = ?'3GPP_RO_NODE-FUNCTIONALITY_AS',
					'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_TERMINATING_ROLE'],
					'Calling-Party-Address' = [CallingPartyAddress],
					'Called-Party-Address' = [CalledPartyAddress]}]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32260@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedUnits]} = GSU,
	NewRequestNum = RequestNum + 1,
	UsedUnits = rand:uniform(GrantedUnits),
	USU = #'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedUnits]},
	MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU]},
	CCR2 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum,
			'Multiple-Services-Credit-Control' = [MSCC3],
			'Event-Timestamp' = [calendar:universal_time()]},
	{ok, Answer2} = diameter:call(?MODULE, cc_app_test, CCR2, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer2,
	Balance2 = Balance1 - UsedUnits,
	{ok, #bucket{remain_amount = Balance2}} = ocs:find_bucket(B1ref).

diameter_voice_out_tariff() ->
	[{userdata, [{doc, "DIAMETER SCUR for originated voice call using tariff table"}]}].

diameter_voice_out_tariff(_Config) ->
	UnitSize = 60,
	TariffRateIn = rand:uniform(100000),
	TariffRateOut = rand:uniform(100000),
	CallingAddress = ocs:generate_identity(),
	CalledAddress = ocs:generate_identity(),
	TableName = ocs:generate_password(),
	PLA = #pla{name = TableName, specification = "4"},
	{ok, _} = ocs:add_pla(PLA),
	{ok, _} = ocs_gtt:insert(TableName, CallingAddress, {"in", TariffRateIn}),
	{ok, _} = ocs_gtt:insert(TableName, CalledAddress, {"out", TariffRateOut}),
	TableNameValue = #char_value{value = TableName},
	TariffTable = #char_value_use{name = "destPrefixTariffTable",
			specification = "3", values = [TableNameValue]},
	Incoming = #char_value{value = "answer"},
	CallDirection1 = #char_value_use{name = "callDirection",
			specification = "5", values = [Incoming]},
	P1 = #price{name = ocs:generate_identity(),
			type = tariff, units = seconds, size = UnitSize,
			char_value_use = [CallDirection1, TariffTable]},
	Outgoing = #char_value{value = "originate"},
	CallDirection2 = #char_value_use{name = "callDirection",
			specification = "5", values = [Outgoing]},
	P2 = #price{name = ocs:generate_identity(),
			type = tariff, units = seconds, size = UnitSize,
			char_value_use = [CallDirection2, TariffTable]},
	OfferId = add_offer([P1, P2], 9),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	{ok, #service{}} = ocs:add_service(MSISDN, undefined, ProdRef, []),
	Balance1 = rand:uniform(1000000000),
	B1 = bucket(cents, Balance1),
	B1ref = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	RSU = #'3gpp_ro_Requested-Service-Unit' {},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CallingPartyAddress = list_to_binary("tel:" ++ CallingAddress),
	CalledPartyAddress = list_to_binary("tel:" ++ CalledAddress),
	ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' =
			[#'3gpp_ro_IMS-Information'{
					'Node-Functionality' = ?'3GPP_RO_NODE-FUNCTIONALITY_AS',
					'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_ORIGINATING_ROLE'],
					'Calling-Party-Address' = [CallingPartyAddress],
					'Called-Party-Address' = [CalledPartyAddress]}]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32260@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedUnits]} = GSU,
	NewRequestNum = RequestNum + 1,
	UsedUnits = rand:uniform(GrantedUnits),
	USU = #'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedUnits]},
	MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU]},
	CCR2 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum,
			'Multiple-Services-Credit-Control' = [MSCC3],
			'Event-Timestamp' = [calendar:universal_time()]},
	{ok, Answer2} = diameter:call(?MODULE, cc_app_test, CCR2, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer2,
	Charge = case UsedUnits rem UnitSize of
		0 ->
			(UsedUnits div UnitSize) * TariffRateOut;
		_ ->
			((UsedUnits div UnitSize) + 1) * TariffRateOut
	end,
	Balance2 = Balance1 - Charge,
	{ok, #bucket{remain_amount = Balance2}} = ocs:find_bucket(B1ref).

diameter_voice_in_tariff() ->
	[{userdata, [{doc, "DIAMETER SCUR for answered voice call using tariff table"}]}].

diameter_voice_in_tariff(_Config) ->
	UnitSize = 60,
	TariffRateIn = rand:uniform(100000),
	TariffRateOut = rand:uniform(100000),
	CallingAddress = ocs:generate_identity(),
	CalledAddress = ocs:generate_identity(),
	TableName = ocs:generate_password(),
	PLA = #pla{name = TableName, specification = "4"},
	{ok, _} = ocs:add_pla(PLA),
	{ok, _} = ocs_gtt:insert(TableName, CallingAddress, {"in", TariffRateIn}),
	{ok, _} = ocs_gtt:insert(TableName, CalledAddress, {"out", TariffRateOut}),
	TableNameValue = #char_value{value = TableName},
	TariffTable = #char_value_use{name = "destPrefixTariffTable",
			specification = "3", values = [TableNameValue]},
	Incoming = #char_value{value = "answer"},
	CallDirection1 = #char_value_use{name = "callDirection",
			specification = "5", values = [Incoming]},
	P1 = #price{name = ocs:generate_identity(),
			type = tariff, units = seconds, size = UnitSize,
			char_value_use = [CallDirection1, TariffTable]},
	Outgoing = #char_value{value = "originate"},
	CallDirection2 = #char_value_use{name = "callDirection",
			specification = "5", values = [Outgoing]},
	P2 = #price{name = ocs:generate_identity(),
			type = tariff, units = seconds, size = UnitSize,
			char_value_use = [CallDirection2, TariffTable]},
	OfferId = add_offer([P1, P2], 9),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	{ok, #service{}} = ocs:add_service(MSISDN, undefined, ProdRef, []),
	Balance1 = rand:uniform(1000000000),
	B1 = bucket(cents, Balance1),
	B1ref = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	SubscriptionId = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	RSU = #'3gpp_ro_Requested-Service-Unit' {},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RSU]},
	CallingPartyAddress = list_to_binary("tel:" ++ CallingAddress),
	CalledPartyAddress = list_to_binary("tel:" ++ CalledAddress),
	ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' =
			[#'3gpp_ro_IMS-Information'{
					'Node-Functionality' = ?'3GPP_RO_NODE-FUNCTIONALITY_AS',
					'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_TERMINATING_ROLE'],
					'Calling-Party-Address' = [CallingPartyAddress],
					'Called-Party-Address' = [CalledPartyAddress]}]},
	CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32260@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = 0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]} = MSCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedUnits]} = GSU,
	NewRequestNum = RequestNum + 1,
	UsedUnits = rand:uniform(GrantedUnits),
	USU = #'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedUnits]},
	MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU]},
	CCR2 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum,
			'Multiple-Services-Credit-Control' = [MSCC3],
			'Event-Timestamp' = [calendar:universal_time()]},
	{ok, Answer2} = diameter:call(?MODULE, cc_app_test, CCR2, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = NewRequestNum} = Answer2,
	Charge = case UsedUnits rem UnitSize of
		0 ->
			(UsedUnits div UnitSize) * TariffRateIn;
		_ ->
			((UsedUnits div UnitSize) + 1) * TariffRateIn
	end,
	Balance2 = Balance1 - Charge,
	{ok, #bucket{remain_amount = Balance2}} = ocs:find_bucket(B1ref).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------
authenticate_subscriber(Socket, Address,
		Port, PeerID, Password, Secret, NasID, ReqAuth, RadID, AcctSessionID) ->
	RadAttribute = radius_attributes:add(?UserPassword, Password, []),
	access_request(Socket, Address, Port, PeerID, Secret,
			NasID, ReqAuth, RadID, AcctSessionID, RadAttribute),
	access_accept(Socket, Address, Port, RadID).

accounting_start(Socket, Address, Port,
		PeerID, Secret, NasID, AcctSessionID, RadID) ->
	ReqAuth = accounting_request(?AccountingStart, Socket,
			Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID, []),
	accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth).

accounting_interim(Socket, Address, Port, PeerID,
		Secret, NasID, AcctSessionID, RadID, InputOctets, OutputOctets) ->
	A0 = radius_attributes:add(?AcctInputOctets, InputOctets rem (1 bsl 32), []),
	A1 = radius_attributes:add(?AcctOutputOctets, OutputOctets rem (1 bsl 32), A0),
	A2 = radius_attributes:add(?AcctInputGigawords, InputOctets div (1 bsl 32), A1),
	A3 = radius_attributes:add(?AcctOutputGigawords, OutputOctets div (1 bsl 32), A2),
	ReqAuth = accounting_request(?AccountingInterimUpdate, Socket,
			Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID, A3),
	accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth).

accounting_stop(Socket, Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID) ->
	accounting_stop(Socket, Address, Port, PeerID,
			Secret, NasID, AcctSessionID, RadID, 100, 50).
accounting_stop(Socket, Address, Port, PeerID,
		Secret, NasID, AcctSessionID, RadID, InputOctets, OutputOctets) ->
	A0 = radius_attributes:add(?AcctInputOctets, InputOctets rem (1 bsl 32), []),
	A1 = radius_attributes:add(?AcctOutputOctets, OutputOctets rem (1 bsl 32), A0),
	A2 = radius_attributes:add(?AcctInputGigawords, InputOctets div (1 bsl 32), A1),
	A3 = radius_attributes:add(?AcctOutputGigawords, OutputOctets div (1 bsl 32), A2),
	ReqAuth = accounting_request(?AccountingStop, Socket,
			Address, Port, PeerID, Secret, NasID, AcctSessionID, RadID, A3),
	accounting_response(Socket, Address, Port, Secret, RadID, ReqAuth).

disconnect_request(Socket) ->
	{ok, {OCSAddr, OCSPort, DiscReq}} = gen_udp:recv(Socket, 0),
	#radius{code = ?DisconnectRequest, id = DiscReqID} = radius:codec(DiscReq),
	DiscAckAuth = radius:authenticator(),
	DiscAckAttr0 = radius_attributes:new(),
	DiscAckAttr1 = radius_attributes:add(?AcctTerminateCause, 6, DiscAckAttr0),
	DiscAckAttrBin = radius_attributes:codec(DiscAckAttr1),
	DiscAckRec = #radius{code = ?DisconnectAck, id = DiscReqID,
			authenticator = DiscAckAuth, attributes = DiscAckAttrBin},
	DiscAck = radius:codec(DiscAckRec),
	ok = gen_udp:send(Socket, OCSAddr, OCSPort, DiscAck).

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
		NasID, Auth, RadID, AcctSessionID, RadAttributes)
		when is_binary(UserName) ->
	access_request(Socket, Address, Port, binary_to_list(UserName),
			Secret, NasID, Auth, RadID, AcctSessionID, RadAttributes);
access_request(Socket, Address, Port, UserName, Secret,
		NasID, Auth, RadID, AcctSessionID, RadAttributes) ->
	A1 = session_attributes(UserName, NasID, AcctSessionID, RadAttributes),
	A2 = radius_attributes:add(?MessageAuthenticator, <<0:128>>, A1),
	Request1 = #radius{code = ?AccessRequest, id = RadID,
		authenticator = Auth, attributes = A2},
	ReqPacket1 = radius:codec(Request1),
	MsgAuth1 = crypto:hmac(md5, Secret, ReqPacket1),
	A3 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A2),
	Request2 = Request1#radius{attributes = A3},
	ReqPacket2 = radius:codec(Request2),
	gen_udp:send(Socket, Address, Port, ReqPacket2).

accounting_request(StatusType, Socket, Address, Port,
		UserName, Secret, NasID, AcctSessionID, RadID, RadAttributes)
		when is_binary(UserName) ->
	accounting_request(StatusType, Socket, Address, Port,
			binary_to_list(UserName), Secret, NasID, AcctSessionID,
			RadID, RadAttributes);
accounting_request(StatusType, Socket, Address, Port,
		UserName, Secret, NasID, AcctSessionID, RadID, RadAttributes) ->
	A1 = session_attributes(UserName, NasID, AcctSessionID, RadAttributes),
	A2 = radius_attributes:add(?AcctStatusType, StatusType, A1),
	AccAttributes = radius_attributes:codec(A2),
	Acc1Length = size(AccAttributes) + 20,
	AccAuthenticator = crypto:hash(md5, [<<?AccountingRequest, RadID,
			Acc1Length:16, 0:128>>, AccAttributes, Secret]),
	AccountingRequest = #radius{code = ?AccountingRequest, id = RadID,
			authenticator = AccAuthenticator, attributes = AccAttributes},
	AccPacket = radius:codec(AccountingRequest),
	ok = gen_udp:send(Socket, Address, Port, AccPacket),
	AccAuthenticator.

session_attributes(UserName, NasID, AcctSessionID, RadAttributes) ->
	A1 = radius_attributes:add(?UserName, UserName, RadAttributes),
	A2 = radius_attributes:add(?NasPort, 19, A1),
	A3 = radius_attributes:add(?NasIpAddress, {127, 0, 0,1}, A2),
	A4 = radius_attributes:add(?NasIdentifier, NasID, A3),
	A5 = radius_attributes:add(?CallingStationId,"DE-AD-BE-EF-FE-ED", A4),
	A6 = radius_attributes:add(?CalledStationId,"BA-DF-AD-CA-DD-AD:TestSSID", A5),
	A7 = radius_attributes:add(?AcctSessionId, AcctSessionID, A6),
	radius_attributes:add(?ServiceType, 2, A7).

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
client_acct_service_opts(Config) ->
	[{'Origin-Host', ?config(host, Config)},
			{'Origin-Realm', ?config(realm, Config)},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Client (Acct)"},
			{'Auth-Application-Id', [?RO_APPLICATION_ID]},
			{string_decode, false},
			{restrict_connections, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_client_cb}]},
			{application, [{alias, cc_app_test},
					{dictionary, diameter_gen_3gpp_ro_application},
					{module, diameter_test_client_cb}]}].

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans}, {transport_config,
			[{raddr, RemAddr}, {rport, RemPort},
			{reuseaddr, true}, {ip, LocalAddr}]}].

%% @hidden
diameter_scur_start(SId, Username, RequestNum, Requested) ->
	Subscription_Id = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Total-Octets' = [Requested]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits]},
	ServiceInformation = #'3gpp_ro_Service-Information'{'PS-Information' =
			[#'3gpp_ro_PS-Information'{
					'3GPP-PDP-Type' = [3],
					'Serving-Node-Type' = [2],
					'SGSN-Address' = [{10,1,2,3}],
					'GGSN-Address' = [{10,4,5,6}],
					'3GPP-IMSI-MCC-MNC' = [<<"001001">>],
					'3GPP-GGSN-MCC-MNC' = [<<"001001">>],
					'3GPP-SGSN-MCC-MNC' = [<<"001001">>]}]},
	CC_CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32251@3gpp.org",
			'User-Name' = [Username],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [Subscription_Id],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.
	
%% @hidden
diameter_scur_stop(SId, Username, RequestNum, Used) ->
	Subscription_Id = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [Used]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits]},
	ServiceInformation = #'3gpp_ro_Service-Information'{'PS-Information' =
			[#'3gpp_ro_PS-Information'{
					'3GPP-PDP-Type' = [3],
					'Serving-Node-Type' = [2],
					'SGSN-Address' = [{10,1,2,3}],
					'GGSN-Address' = [{10,4,5,6}],
					'3GPP-IMSI-MCC-MNC' = [<<"001001">>],
					'3GPP-GGSN-MCC-MNC' = [<<"001001">>],
					'3GPP-SGSN-MCC-MNC' = [<<"001001">>]}]},
	CC_CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32251@3gpp.org" ,
			'User-Name' = [Username],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [Subscription_Id],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

%% @hidden
diameter_scur_interim(SId, Username, RequestNum, Used, Requested) ->
	Subscription_Id = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [Used]},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Total-Octets' = [Requested]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits],
			'Requested-Service-Unit' = [RequestedUnits]},
	ServiceInformation = #'3gpp_ro_Service-Information'{'PS-Information' =
			[#'3gpp_ro_PS-Information'{
					'3GPP-PDP-Type' = [3],
					'Serving-Node-Type' = [2],
					'SGSN-Address' = [{10,1,2,3}],
					'GGSN-Address' = [{10,4,5,6}],
					'3GPP-IMSI-MCC-MNC' = [<<"001001">>],
					'3GPP-GGSN-MCC-MNC' = [<<"001001">>],
					'3GPP-SGSN-MCC-MNC' = [<<"001001">>]}]},
	CC_CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32251@3gpp.org" ,
			'User-Name' = [Username],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [Subscription_Id],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.
	
authenticate_subscriber1(Socket, Address,
		Port, PeerID, Password, Secret, NasID, ReqAuth, RadID,
		DiscSocket, AcctSessionID) ->
	RadAttribute = radius_attributes:add(?UserPassword, Password, []),
	access_request(Socket, Address, Port, PeerID, Secret,
			NasID, ReqAuth, RadID, AcctSessionID, RadAttribute),
	disconnect_request(DiscSocket),
	access_accept(Socket, Address, Port, RadID).

%% @hidden
price(Type, Units, Size, Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, units = Units,
			size = Size, amount = Amount}.

%% @hidden
bucket(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
		start_date = erlang:system_time(?MILLISECOND),
		end_date = erlang:system_time(?MILLISECOND) + 2592000000}.

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
