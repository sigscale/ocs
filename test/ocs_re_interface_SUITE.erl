%% ocs_re_interface_SUITE.erl
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
%%%  @doc Test suite for public API of the {@link //ocs. ocs} application.
%%%
-module(ocs_re_interface_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("../include/diameter_gen_cc_application_rfc4006.hrl").
-include_lib("../include/diameter_gen_3gpp_ro_application.hrl").
-include_lib("../include/diameter_gen_3gpp.hrl").
-include_lib("../include/diameter_gen_ietf.hrl").

-define(MILLISECOND, millisecond).
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).
-define(NRF_RO_APPLICATION_CALLBACK, ocs_diameter_3gpp_ro_nrf_app_cb).
-define(MINIMUM_RESERVATION, 5000).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
   [{userdata, [{doc, "Test suite for Re Interface in OCS"}]},
	{require, diameter},
	{default_config, diameter, [{address, {127,0,0,1}}]},
   {timetrap, {minutes, 10}},
	{require, rest},
	{default_config, rest, [{user, "nrf"},
			{password, "4yjhe6ydsrh4"}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
   ok = ocs_test_lib:load(ocs),
	{ok, [Auth, {acct, [{DAddress, DPort, DOptions}]}]} = application:get_env(ocs, diameter),
	NewOptions = DOptions ++ [{callback, ocs_diameter_3gpp_ro_nrf_application_cb}],
	NewEnvVar = [Auth, {acct, [{DAddress, DPort, NewOptions}]}],
	ok = application:set_env(ocs, diameter, NewEnvVar),
	{ok, Services} = application:get_env(inets, services),
	Fport = fun FPort([{httpd, L} | T]) ->
				case lists:keyfind(server_name, 1, L) of
					{_, "rest"} ->
						H1 = lists:keyfind(bind_address, 1, L),
						P1 = lists:keyfind(port, 1, L),
						{H1, P1};
					_ ->
						FPort(T)
				end;
			FPort([_ | T]) ->
				FPort(T)
	end,
	RestUser = ct:get_config({rest, user}),
	RestPass = ct:get_config({rest, password}),
	{Host, Port} = case Fport(Services) of
		{{_, H2}, {_, P2}} when H2 == "localhost"; H2 == {127,0,0,1} ->
			{ok, _} = ocs:add_user(RestUser, RestPass, "en"),
			{"localhost", P2};
		{{_, H2}, {_, P2}} ->
			{ok, _} = ocs:add_user(RestUser, RestPass, "en"),
			case H2 of
				H2 when is_tuple(H2) ->
					{inet:ntoa(H2), P2};
				H2 when is_list(H2) ->
					{H2, P2}
			end;
		{false, {_, P2}} ->
			{ok, _} = ocs:add_user(RestUser, RestPass, "en"),
			{"localhost", P2}
	end,
	Config1 = [{port, Port} | Config],
	HostUrl = "https://" ++ Host ++ ":" ++ integer_to_list(Port),
	init_per_suite1([{host_url, HostUrl} | Config1]).
%% @hidden
init_per_suite1(Config) ->
	DiameterAddress = ct:get_config({diameter, address}, {127,0,0,1}),
	DiameterAuthPort = ct:get_config({diameter, auth_port}, rand:uniform(64511) + 1024),
	DiameterAcctPort = ct:get_config({diameter, acct_port}, rand:uniform(64511) + 1024),
	DiameterAppVar = [{auth, [{DiameterAddress, DiameterAuthPort, []}]},
		{acct, [{DiameterAddress, DiameterAcctPort, [{rf_class, undefined},
				{callback, ocs_diameter_3gpp_ro_nrf_app_cb}, {sub_id_type, [msisdn, imsi]}]}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = application:set_env(ocs, min_reserve_octets, 1000000),
	ok = application:set_env(ocs, min_reserve_seconds, 60),
	ok = application:set_env(ocs, min_reserve_messages, 1),
	Realm = ct:get_config({diameter, realm}, "mnc001.mcc001.3gppnetwork.org"),
	Host = ct:get_config({diameter, host}, atom_to_list(?MODULE) ++ "." ++ Realm),
   Config1 = [{diameter_host, Host}, {realm, Realm},
         {diameter_acct_address, DiameterAddress} | Config],
	ok = ocs_test_lib:start(),
   ok = diameter:start_service(?MODULE, client_acct_service_opts(Config1)),
   true = diameter:subscribe(?MODULE),
   {ok, _Ref2} = connect(?MODULE, DiameterAddress, DiameterAcctPort, diameter_tcp),
   receive
      #diameter_event{service = ?MODULE, info = Info}
            when element(1, Info) == up ->
			init_per_suite2(Config1);
      _Other ->
         {skip, diameter_client_acct_service_not_started}
   end.
init_per_suite2(Config) ->
	case inets:start(httpd,
			[{port, 0},
			{server_name, atom_to_list(?MODULE)},
			{server_root, "./"},
			{document_root, ?config(data_dir, Config)},
			{modules, [mod_ct_nrf]}]) of
		{ok, HttpdPid} ->
			[{port, Port}] = httpd:info(HttpdPid, [port]),
			NrfUri = "http://localhost:" ++ integer_to_list(Port),
			{ok, [Auth, {acct, [{Address, DPort, Options}]}]} = application:get_env(ocs, diameter),
			NewOptions = Options ++ [{nrf_uri, NrfUri}],
			NewEnvVar = [Auth, {acct, [{Address, DPort, NewOptions}]}],
			ok = application:set_env(ocs, diameter, NewEnvVar),
			[{server_port, Port},
					{server_pid, HttpdPid}, {nrf_uri, NrfUri} | Config];
		{error, InetsReason} ->
			ct:fail(InetsReason)
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
init_per_testcase(TestCase, Config)
		when TestCase == send_initial_scur_class_b; TestCase == receive_initial_scur_class_b;
		TestCase == send_interim_scur_class_b; TestCase == receive_interim_scur_class_b;
		TestCase == send_final_scur_class_b; TestCase == receive_final_scur_class_b;
		TestCase == receive_interim_no_usu_scur_class_b;
		TestCase == send_iec_class_b;
		TestCase == receive_iec_class_b;
		TestCase == send_initial_ecur_class_b;
		TestCase == receive_initial_ecur_class_b;
		TestCase == send_final_ecur_class_b;
		TestCase == receive_final_ecur_class_b;
		TestCase == scur_vas_class_b;
		TestCase == receive_initial_cud_scur_class_b ->
	Address = ?config(diameter_acct_address, Config),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	{ok, [Auth, {acct, [{DAddress, Port, Options}]}]} = application:get_env(ocs, diameter),
	NewOptions = lists:keyreplace(rf_class, 1, Options,  {rf_class, b}),
	NewEnvVar = [Auth, {acct, [{DAddress, Port, NewOptions}]}],
	ok = application:set_env(ocs, diameter, NewEnvVar),
	Config;
init_per_testcase(TestCase, Config)
		when TestCase == send_initial_scur_class_a;
		TestCase == receive_initial_scur_class_a;
		TestCase == send_interim_scur_class_a;
		TestCase == receive_interim_scur_class_a;
		TestCase == final_scur_class_a;
		TestCase == send_initial_ecur_class_a;
		TestCase == receive_initial_ecur_class_a;
		TestCase == send_final_ecur_class_a;
		TestCase == receive_final_ecur_class_a ->
	Address = ?config(diameter_acct_address, Config),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	{ok, [Auth, {acct, [{DAddress, Port, Options}]}]} = application:get_env(ocs, diameter),
	NewOptions = lists:keyreplace(rf_class, 1, Options,  {rf_class, undefined}),
	NewEnvVar = [Auth, {acct, [{DAddress, Port, NewOptions}]}],
	ok = application:set_env(ocs, diameter, NewEnvVar),
	Config;
init_per_testcase(scur_imsi_class_b, Config) ->
	Address = ?config(diameter_acct_address, Config),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	{ok, [Auth, {acct, [{DAddress, Port, Options}]}]} = application:get_env(ocs, diameter),
	NewOptions = lists:keyreplace(rf_class, 1, Options, {rf_class, b}),
	NewOptions1 = lists:keyreplace(sub_id_type, 1, NewOptions , {sub_id_type, [imsi]}),
	NewEnvVar = [Auth, {acct, [{DAddress, Port, NewOptions1}]}],
	ok = application:set_env(ocs, diameter, NewEnvVar),
	Config;
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[send_initial_scur_class_b, receive_initial_scur_class_b, receive_initial_cud_scur_class_b,
		send_interim_scur_class_b, receive_interim_scur_class_b, send_final_scur_class_b,
		receive_final_scur_class_b, receive_interim_no_usu_scur_class_b,
		post_initial_scur_class_b, post_update_scur_class_b,
		post_final_scur_class_b, send_iec_class_b, receive_iec_class_b, send_initial_ecur_class_b,
		receive_initial_ecur_class_b, send_final_ecur_class_b, receive_final_ecur_class_b,
		post_iec_class_b, post_initial_ecur_class_b, post_final_ecur_class_b, send_initial_scur_class_a,
		receive_initial_scur_class_a, send_interim_scur_class_a, receive_interim_scur_class_a, final_scur_class_a,
		send_initial_ecur_class_a, receive_initial_ecur_class_a, send_final_ecur_class_a,
		receive_final_ecur_class_a, scur_vas_class_b, scur_imsi_class_b].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

send_initial_scur_class_b() ->
	[{userdata, [{doc, "On received SCUR CCR-I send startRating"}]}].

send_initial_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	InputOctets = rand:uniform(100),
	OutputOctets = rand:uniform(200),
	RequestedServiceUnits = {InputOctets, OutputOctets},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0.

receive_initial_scur_class_b() ->
	[{userdata, [{doc, "On SCUR startRating response send CCA-I"}]}].

receive_initial_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	InputOctets = rand:uniform(100),
	OutputOctets = rand:uniform(200),
	RequestedServiceUnits = {InputOctets, OutputOctets},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MultiServices_CC,
	TotalOctets = InputOctets + OutputOctets,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalOctets]} = GrantedUnits.

receive_initial_cud_scur_class_b() ->
	[{userdata, [{doc, "On SCUR with Centralized Unit Determination startRating response send CCA-I"}]}].

receive_initial_cud_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit'{},
	MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
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
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer0} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MSCC3]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MSCC3,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [?MINIMUM_RESERVATION]} = GrantedUnits.

send_interim_scur_class_b() ->
	[{userdata, [{doc, "On received SCUR CCR-U send updateRating"}]}].

send_interim_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(100),
	OutputOctets1 = rand:uniform(200),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 = rand:uniform(100),
	OutputOctets2 = rand:uniform(200),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1.

receive_interim_scur_class_b() ->
	[{userdata, [{doc, "On SCUR updateRating response send CCA-U"}]}].

receive_interim_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(1000),
	OutputOctets1 = rand:uniform(2000),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 = rand:uniform(100),
	OutputOctets2 = rand:uniform(200),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum1,
			'Multiple-Services-Credit-Control' = [MCC1, MCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MCC1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits1]} = MCC2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [?MINIMUM_RESERVATION]} = GrantedUnits,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [?MINIMUM_RESERVATION]} = GrantedUnits1.

send_final_scur_class_b() ->
	[{userdata, [{doc, "On received SCUR CCR-T send endRating"}]}].

send_final_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(100),
	OutputOctets1 = rand:uniform(200),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 =  rand:uniform(100),
	OutputOctets2 = rand:uniform(200),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1,
	RequestNum2 = RequestNum1 + 1,
	Grant2 = rand:uniform(300),
	Answer2 = diameter_scur_stop(SId, Subscriber, RequestNum2, Grant2),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer2.

receive_final_scur_class_b() ->
	[{userdata, [{doc, "On SCUR endRatingresponse send CCA-T"}]}].

receive_final_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(100),
	OutputOctets1 = rand:uniform(200),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 = rand:uniform(100),
	OutputOctets2 = rand:uniform(200),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1,
	RequestNum2 = RequestNum1 + 1,
	UsedServiceUnits1 = rand:uniform(300),
	Answer2 = diameter_scur_stop(SId, Subscriber, RequestNum2, UsedServiceUnits1),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum2} = Answer2.

receive_interim_no_usu_scur_class_b() ->
	[{userdata, [{doc, "On SCUR updateRating response with no USU send CCA-U"}]}].

receive_interim_no_usu_scur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(100),
	OutputOctets1 = rand:uniform(200),
	RequestedServiceUnits1 = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits1),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 = rand:uniform(100),
	OutputOctets2 = rand:uniform(200),
	RequestedServiceUnits2 = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim1(SId, Subscriber, RequestNum1, 0, RequestedServiceUnits2),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum1,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MultiServices_CC,
	TotalGranted = InputOctets2 + OutputOctets2,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalGranted]} = GrantedUnits.

post_initial_scur_class_b() ->
	[{userdata, [{doc, "Post Inital Nrf Request to be rated"}]}].

post_initial_scur_class_b(Config) ->
	P1 = price(usage, octets, rand:uniform(10000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	Password = ocs:generate_identity(),
	MSISDN = ocs:generate_identity(),
	IMSI = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(list_to_binary(MSISDN),
			Password, ProdRef, []),
	Balance = rand:uniform(100000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	InputOctets = rand:uniform(10000),
	OutputOctets = rand:uniform(20000),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	HostUrl = ?config(host_url, Config),
	Body = nrf_post_initial_scur_class_b(MSISDN, IMSI, InputOctets, OutputOctets),
	RequestBody = lists:flatten(mochijson:encode(Body)),
	Request1 = {HostUrl ++ "/nrf-rating/v1/ratingdata", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, _Location1} = lists:keyfind("location", 1, Headers),
	{struct, AttributeList} = mochijson:decode(ResponseBody),
	{_, {_, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}} = lists:keyfind("subscriptionId", 1, AttributeList),
	TotalOctets = InputOctets + OutputOctets,
	{"serviceRating", {_, [{_,ServiceRating1}]}} = lists:keyfind("serviceRating", 1, AttributeList),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating1),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating1),
	{_, 1} = lists:keyfind("serviceId", 1, ServiceRating1),
	{_, "32251@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating1),
	{_, {_, [{_, TotalOctets}]}} = lists:keyfind("grantedUnit", 1, ServiceRating1).

post_update_scur_class_b() ->
	[{userdata, [{doc, "Post Interim Nrf Request to be rated"}]}].

post_update_scur_class_b(Config) ->
	P1 = price(usage, octets, rand:uniform(10000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	Password = ocs:generate_identity(),
	MSISDN = ocs:generate_identity(),
	IMSI = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(list_to_binary(MSISDN),
			Password, ProdRef, []),
	Balance = rand:uniform(100000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	InputOctets = rand:uniform(10000),
	OutputOctets = rand:uniform(20000),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	HostUrl = ?config(host_url, Config),
	Body = nrf_post_initial_scur_class_b(MSISDN, IMSI, InputOctets, OutputOctets),
	RequestBody = lists:flatten(mochijson:encode(Body)),
	Request = {HostUrl ++ "/nrf-rating/v1/ratingdata", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, _ResponseBody} = Result,
	{_, Location1} = lists:keyfind("location", 1, Headers1),
	InputOctets1 = rand:uniform(30000),
	OutputOctets1 = rand:uniform(40000),
	Body1 = nrf_post_update_scur_class_b(MSISDN, IMSI, InputOctets1, OutputOctets1),
	RequestBody1 = lists:flatten(mochijson:encode(Body1)),
	Request1 = {HostUrl ++ "/nrf-rating/v1" ++ Location1 ++ "/update",
			[Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 200, _Ok}, _Headers1, ResponseBody1} = Result1,
	{struct, AttributeList} = mochijson:decode(ResponseBody1),
	{_, {_, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}} = lists:keyfind("subscriptionId", 1, AttributeList),
	TotalOctets1 = InputOctets1 + OutputOctets1,
	{"serviceRating", {_, [{_,ServiceRating1}, {_,ServiceRating2}]}}
			= lists:keyfind("serviceRating", 1, AttributeList),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating1),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating1),
	{_, 1} = lists:keyfind("serviceId", 1, ServiceRating1),
	{_, "32251@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating1),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating2),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating2),
	{_, 1} = lists:keyfind("serviceId", 1, ServiceRating2),
	{_, "32251@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating2),
	{_, {_, [{_, TotalOctets1}]}} = lists:keyfind("consumedUnit", 1, ServiceRating2).

post_final_scur_class_b() ->
	[{userdata, [{doc, "Post Final Nrf Request to be rated"}]}].

post_final_scur_class_b(Config) ->
	P1 = price(usage, octets, rand:uniform(10000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	Password = ocs:generate_identity(),
	MSISDN = ocs:generate_identity(),
	IMSI = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(list_to_binary(MSISDN),
			Password, ProdRef, []),
	Balance = rand:uniform(100000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	InputOctets = rand:uniform(10000),
	OutputOctets = rand:uniform(20000),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	HostUrl = ?config(host_url, Config),
	Body = nrf_post_initial_scur_class_b(MSISDN, IMSI, InputOctets, OutputOctets),
	RequestBody = lists:flatten(mochijson:encode(Body)),
	Request = {HostUrl ++ "/nrf-rating/v1/ratingdata", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, _ResponseBody} = Result,
	{_, Location1} = lists:keyfind("location", 1, Headers1),
	InputOctets1 = rand:uniform(30000),
	OutputOctets1 = rand:uniform(40000),
	Body1 = nrf_post_update_scur_class_b(MSISDN, IMSI, InputOctets1, OutputOctets1),
	RequestBody1 = lists:flatten(mochijson:encode(Body1)),
	Request1 = {HostUrl ++ "/nrf-rating/v1" ++ Location1 ++ "/update",
			[Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 200, _Ok}, _Headers1, _ResponseBody1} = Result1,
	InputOctets2 = rand:uniform(50000),
	OutputOctets2 = rand:uniform(60000),
	Body2 = nrf_post_final_scur_class_b(MSISDN, IMSI, InputOctets2, OutputOctets2),
	RequestBody2 = lists:flatten(mochijson:encode(Body2)),
	Request2 = {HostUrl ++ "/nrf-rating/v1" ++ Location1 ++ "/release",
			[Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{"HTTP/1.1", 200, _Ok}, _Headers2, ResponseBody2} = Result2,
	{struct, AttributeList} = mochijson:decode(ResponseBody2),
	{"serviceRating", {_, [{_,ServiceRating1}]}}
			= lists:keyfind("serviceRating", 1, AttributeList),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating1),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating1),
	{_, 1} = lists:keyfind("serviceId", 1, ServiceRating1),
	{_, "32251@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating1),
	TotalOctets2 = InputOctets2 + OutputOctets2,
	{_, {_, [{_, TotalOctets2}]}} = lists:keyfind("consumedUnit", 1, ServiceRating1).

send_iec_class_b() ->
	[{userdata, [{doc, "On received IEC CCR-E send startRating"}]}].

send_iec_class_b(_Config) ->
   Ref = erlang:ref_to_list(make_ref()),
   SId = diameter:session_id(Ref),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	RequestNum = 0,
	Answer0 = diameter_iec(Subscriber, SId, RequestNum),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0.

receive_iec_class_b() ->
	[{userdata, [{doc, "On IEC startRating response send CCA-E"}]}].

receive_iec_class_b(_Config) ->
   Ref = erlang:ref_to_list(make_ref()),
   SId = diameter:session_id(Ref),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	RequestNum = 0,
	Answer0 = diameter_iec(Subscriber, SId, RequestNum),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
			'CC-Request-Number' = RequestNum} = Answer0.

send_initial_ecur_class_b() ->
	[{userdata, [{doc, "On received ECUR CCR-I send startRating"}]}].

send_initial_ecur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0.

receive_initial_ecur_class_b() ->
	[{userdata, [{doc, "On ECUR startRating response send CCA-I"}]}].

receive_initial_ecur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [UsedUnits]} = MultiServices_CC,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [1]} = UsedUnits.

send_final_ecur_class_b() ->
	[{userdata, [{doc, "On received ECUR CCR-U send endRating"}]}].

send_final_ecur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_ecur_final(Subscriber, SId, RequestNum1),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1.

receive_final_ecur_class_b() ->
	[{userdata, [{doc, "On ECUR endRating response send CCA-U"}]}].

receive_final_ecur_class_b(_Config) ->
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_ecur_final(Subscriber, SId, RequestNum1),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum1} = Answer1.

post_iec_class_b() ->
	[{userdata, [{doc, "Post IEC Event Nrf Request to be rated"}]}].

post_iec_class_b(Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	MSISDN = ocs:generate_identity(),
	IMSI = ocs:generate_identity(),
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(list_to_binary(MSISDN), Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	HostUrl = ?config(host_url, Config),
	Messages = 1,
	Body = nrf_post_iec_class_b(MSISDN, IMSI, Messages),
	RequestBody = lists:flatten(mochijson:encode(Body)),
	Request1 = {HostUrl ++ "/nrf-rating/v1/ratingdata", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, _Headers, ResponseBody} = Result,
	{struct, AttributeList} = mochijson:decode(ResponseBody),
	{"serviceRating", {_, [{_,ServiceRating}]}}
			= lists:keyfind("serviceRating", 1, AttributeList),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating),
	{_, 4} = lists:keyfind("serviceId", 1, ServiceRating),
	{_, "32274@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating),
	{_, {_, [{_, Messages}]}} = lists:keyfind("consumedUnit", 1, ServiceRating).

post_initial_ecur_class_b() ->
	[{userdata, [{doc, "Post ECUR Inital Nrf Request to be rated"}]}].

post_initial_ecur_class_b(Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	MSISDN = ocs:generate_identity(),
	IMSI = ocs:generate_identity(),
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(list_to_binary(MSISDN), Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	HostUrl = ?config(host_url, Config),
	Messages = 1,
	Body = nrf_post_initial_ecur_class_b(MSISDN, IMSI, Messages),
	RequestBody = lists:flatten(mochijson:encode(Body)),
	Request1 = {HostUrl ++ "/nrf-rating/v1/ratingdata", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, _Location} = lists:keyfind("location", 1, Headers),
	{struct, AttributeList} = mochijson:decode(ResponseBody),
	{"serviceRating", {_, [{_,ServiceRating}]}}
			= lists:keyfind("serviceRating", 1, AttributeList),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating),
	{_, 4} = lists:keyfind("serviceId", 1, ServiceRating),
	{_, "32274@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating),
	{_, {_, [{_, Messages}]}} = lists:keyfind("grantedUnit", 1, ServiceRating).

post_final_ecur_class_b() ->
	[{userdata, [{doc, "Post ECUR Final Nrf Request to be rated"}]}].

post_final_ecur_class_b(Config) ->
	P1 = price(usage, messages, 1, rand:uniform(1000000)),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	MSISDN = ocs:generate_identity(),
	IMSI = ocs:generate_identity(),
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(list_to_binary(MSISDN), Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	HostUrl = ?config(host_url, Config),
	Messages = 1,
	Body = nrf_post_initial_ecur_class_b(MSISDN, IMSI, Messages),
	RequestBody = lists:flatten(mochijson:encode(Body)),
	Request = {HostUrl ++ "/nrf-rating/v1/ratingdata", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _ResponseBody} = Result,
	{_, Location} = lists:keyfind("location", 1, Headers),
	Messages1 = 2,
	Body1 = nrf_post_final_ecur_class_b(MSISDN, IMSI, Messages1),
	RequestBody1 = lists:flatten(mochijson:encode(Body1)),
	Request1 = {HostUrl ++ "/nrf-rating/v1" ++ Location ++ "/release",
			[Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 200, _Ok}, _Headers1, ResponseBody1} = Result1,
	{struct, AttributeList} = mochijson:decode(ResponseBody1),
	{"serviceRating", {_, [{_, ServiceRating}]}}
			= lists:keyfind("serviceRating", 1, AttributeList),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating),
	{_, 32} = lists:keyfind("ratingGroup", 1, ServiceRating),
	{_, 4} = lists:keyfind("serviceId", 1, ServiceRating),
	{_, "32274@3gpp.org"} = lists:keyfind("serviceContextId", 1, ServiceRating),
	{_, {_, [{_, Messages1}]}} = lists:keyfind("consumedUnit", 1, ServiceRating).

send_initial_scur_class_a() ->
	[{userdata, [{doc, "On received SCUR CCR-I send startRating"}]}].

send_initial_scur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 4),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	Balance = rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	InputOctets = rand:uniform(100),
	OutputOctets = rand:uniform(200),
	RequestedServiceUnits = {InputOctets, OutputOctets},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0.

receive_initial_scur_class_a() ->
	[{userdata, [{doc, "On SCUR startRating response send CCA-I"}]}].

receive_initial_scur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 4),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	Balance = rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	InputOctets = rand:uniform(100),
	OutputOctets = rand:uniform(200),
	RequestedServiceUnits = {InputOctets, OutputOctets},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MultiServices_CC,
	TotalOctets = InputOctets + OutputOctets,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalOctets]} = GrantedUnits.

send_interim_scur_class_a() ->
	[{userdata, [{doc, "On received SCUR CCR-U send updateRating"}]}].

send_interim_scur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 4),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	Balance = rand:uniform(100000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(100),
	OutputOctets1 = rand:uniform(200),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 = rand:uniform(Balance div 2),
	OutputOctets2 = rand:uniform(200),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1.

receive_interim_scur_class_a() ->
	[{userdata, [{doc, "On SCUR updateRating response send CCA-U"}]}].

receive_interim_scur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 4),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	Balance = rand:uniform(100000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(Balance div 3),
	OutputOctets1 = rand:uniform(Balance div 4),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 = rand:uniform(Balance div 2),
	OutputOctets2 = rand:uniform(Balance div 3),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum1,
			'Multiple-Services-Credit-Control' = [MSCC1, MSCC2]} = Answer1,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MSCC1,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [_TotalOctets]} = GrantedUnits.

final_scur_class_a() ->
	[{userdata, [{doc, "On received SCUR CCR-T send endRating"}]}].

final_scur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 4),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	Balance = rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	InputOctets1 = rand:uniform(100),
	OutputOctets1 = rand:uniform(200),
	RequestedServiceUnits = {InputOctets1, OutputOctets1},
	Answer0 = diameter_scur_start(SId, Subscriber, RequestNum0, RequestedServiceUnits),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	InputOctets2 =  rand:uniform(Balance div 2),
	OutputOctets2 = rand:uniform(200),
	UsedServiceUnits = {InputOctets2, OutputOctets2},
	Answer1 = diameter_scur_interim(SId, Subscriber, RequestNum1, UsedServiceUnits, 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1,
	RequestNum2 = RequestNum1 + 1,
	Grant2 = rand:uniform(Balance div 2),
	Answer2 = diameter_scur_stop(SId, Subscriber, RequestNum2, Grant2),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum2} = Answer2.

send_initial_ecur_class_a() ->
	[{userdata, [{doc, "On received ECUR CCR-I send startRating"}]}].

send_initial_ecur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 11),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0.

receive_initial_ecur_class_a() ->
	[{userdata, [{doc, "On ECUR startRating response send CCA-I"}]}].

receive_initial_ecur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 11),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [UsedUnits]} = MultiServices_CC,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [1]} = UsedUnits.

send_final_ecur_class_a() ->
	[{userdata, [{doc, "On received ECUR CCR-T send endRating"}]}].

send_final_ecur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 11),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_ecur_final(Subscriber, SId, RequestNum1),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1.

receive_final_ecur_class_a() ->
	[{userdata, [{doc, "On ECUR endRating response send CCA-T"}]}].

receive_final_ecur_class_a(Config) ->
	OfferId = add_offer([price_pla(Config)], 11),
	ProdRef = add_product(OfferId),
	MSISDN = list_to_binary(ocs:generate_identity()),
	IMSI = list_to_binary(ocs:generate_identity()),
	Subscriber = {MSISDN, IMSI},
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(MSISDN, Password, ProdRef, []),
	B1 = bucket(messages, 5),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	Answer0 = diameter_ecur_start(Subscriber, SId, RequestNum0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_ecur_final(Subscriber, SId, RequestNum1),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum1} = Answer1.

scur_vas_class_b() ->
	[{userdata, [{doc, "Diameter SCUR SMS Nrf Class B operation"}]}].

scur_vas_class_b(_Config) ->
	Subscriber = list_to_binary(ocs:generate_identity()),
	MSISDN = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Subscriber},
	IMSI = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = list_to_binary(ocs:generate_identity())},
	ServiceInformation = #'3gpp_ro_Service-Information'{'PS-Information' =
			[#'3gpp_ro_PS-Information'{
					'3GPP-PDP-Type' = [3],
					'Serving-Node-Type' = [2],
					'SGSN-Address' = [{10,1,2,3}],
					'GGSN-Address' = [{10,4,5,6}],
					'3GPP-IMSI-MCC-MNC' = [<<"001001">>],
					'3GPP-GGSN-MCC-MNC' = [<<"001001">>],
					'3GPP-SGSN-MCC-MNC' = [<<"001001">>]}]},
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	RequestedUnits0 = #'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units' = [1]},
	MultiServices_CC0 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits0], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	CC_CCR0 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [Subscriber],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum0,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN, IMSI],
			'Multiple-Services-Credit-Control' = [MultiServices_CC0],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer0} = diameter:call(?MODULE, cc_app_test, CC_CCR0, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	UsedUnits1 = #'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [1]},
	RequestedUnits1 = #'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units' = []},
	MultiServices_CC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits1],
			'Requested-Service-Unit' = [RequestedUnits1], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	CC_CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
		'Auth-Application-Id' = ?RO_APPLICATION_ID,
		'Service-Context-Id' = "32274@3gpp.org",
		'User-Name' = [Subscriber],
		'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
		'CC-Request-Number' = RequestNum1,
		'Event-Timestamp' = [calendar:universal_time()],
		'Multiple-Services-Credit-Control' = [MultiServices_CC1],
		'Subscription-Id' = [MSISDN, IMSI],
		'Service-Information' = [ServiceInformation]},
	{ok, Answer1} = diameter:call(?MODULE, cc_app_test, CC_CCR1, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1,
	RequestNum2 = RequestNum1 + 1,
	UsedUnits2 = #'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [1]},
	RequestedUnits2 = #'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units' = []},
	MultiServices_CC2 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits2],
			'Requested-Service-Unit' = [RequestedUnits2], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	CC_CCR2 = #'3gpp_ro_CCR'{'Session-Id' = SId,
		'Auth-Application-Id' = ?RO_APPLICATION_ID,
		'Service-Context-Id' = "32274@3gpp.org",
		'User-Name' = [Subscriber],
		'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
		'CC-Request-Number' = RequestNum2,
		'Event-Timestamp' = [calendar:universal_time()],
		'Multiple-Services-Credit-Control' = [MultiServices_CC2],
		'Subscription-Id' = [MSISDN, IMSI],
		'Service-Information' = [ServiceInformation]},
	{ok, Answer2} = diameter:call(?MODULE, cc_app_test, CC_CCR2, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer2,
	RequestNum3 = RequestNum2 + 1,
	UsedUnits3 = #'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [1]},
	RequestedUnits3 = #'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units' = []},
	MultiServices_CC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits3],
			'Requested-Service-Unit' = [RequestedUnits3], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	CC_CCR3 = #'3gpp_ro_CCR'{'Session-Id' = SId,
		'Auth-Application-Id' = ?RO_APPLICATION_ID,
		'Service-Context-Id' = "32274@3gpp.org",
		'User-Name' = [Subscriber],
		'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
		'CC-Request-Number' = RequestNum3,
		'Event-Timestamp' = [calendar:universal_time()],
		'Multiple-Services-Credit-Control' = [MultiServices_CC3],
		'Subscription-Id' = [MSISDN, IMSI],
		'Service-Information' = [ServiceInformation]},
	{ok, Answer3} = diameter:call(?MODULE, cc_app_test, CC_CCR3, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer3,
	RequestNum4 = RequestNum3 + 1,
	UsedUnits4 = #'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [1]},
	RequestedUnits4 = #'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units' = []},
	MultiServices_CC4 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits4],
			'Requested-Service-Unit' = [RequestedUnits4], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	CC_CCR4 = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org" ,
			'User-Name' = [Subscriber],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum4,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MultiServices_CC4],
			'Subscription-Id' = [MSISDN, IMSI],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer4} = diameter:call(?MODULE, cc_app_test, CC_CCR4, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum4} = Answer4.

scur_imsi_class_b() ->
	[{userdata, [{doc, "On SCUR with IMSI startRating response send CCA-I"}]}].

scur_imsi_class_b(_Config) ->
	IMSI = list_to_binary(ocs:generate_identity()),
	MSISDN = list_to_binary(ocs:generate_identity()),
	NAI = <<"user@realm">>,
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum = 0,
	InputOctets = rand:uniform(100),
	OutputOctets = rand:uniform(200),
	SubscriptionId1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	SubscriptionId2 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	SubscriptionId3 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = NAI},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Input-Octets' = [InputOctets], 'CC-Output-Octets' = [OutputOctets],
			'CC-Total-Octets' = [InputOctets + OutputOctets]},
	MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
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
			'User-Name' = [IMSI],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [SubscriptionId1, SubscriptionId2, SubscriptionId3],
			'Multiple-Services-Credit-Control' = [MSCC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer0} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]} = Answer0,
	#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]} = MultiServices_CC,
	TotalOctets = InputOctets + OutputOctets,
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalOctets]} = GrantedUnits.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

price_pla(Config) ->
	HostUrl = ?config(nrf_uri, Config),
	PlaRef = #pla_ref{id = ocs:generate_password(),
			href = HostUrl ++ "/nrf-rating/v1/ratingdata/tariffrequest",
			name = tariff, class_type = a,
			schema = nrf_rating, ref_type = pla},
	#price{name = ocs:generate_identity(),
			type = PlaRef, units = undefined}.

nrf_post_final_ecur_class_b(MSISDN, IMSI, Messages) ->
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	{struct, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", "OCF"}]}},
			{"invocationTimeStamp", InvocationTimeStamp },
			{"invocationSequenceNumber", 2},
			{"subscriptionId",
					{array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}},
			{"oneTimeEvent", true},
			{"oneTimeEventType", "PEC"},
			{"serviceRating",
					{array, [{struct, [{"serviceContextId", "32274@3gpp.org"},
							{"serviceId", 4},
							{"ratingGroup", 32},
							{"consumedUnit", {struct, [{"serviceSpecificUnit", Messages}]}},
							{"destinationId",
									{array, [{struct, [{"destinationIdType", "DN"},
									{"destinationIdData", "14165556789"}]}]}},
							{"requestSubType", "DEBIT"}]}]}}]}.

nrf_post_initial_ecur_class_b(MSISDN, IMSI, Messages) ->
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	{struct, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", "OCF"}]}},
			{"invocationTimeStamp", InvocationTimeStamp },
			{"invocationSequenceNumber", 1},
			{"subscriptionId",
					{array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}},
			{"oneTimeEvent", true},
			{"oneTimeEventType", "PEC"},
			{"serviceRating",
					{array, [{struct, [{"serviceContextId", "32274@3gpp.org"},
							{"serviceId", 4},
							{"ratingGroup", 32},
							{"requestedUnit", {struct, [{"serviceSpecificUnit", Messages}]}},
							{"destinationId",
									{array, [{struct, [{"destinationIdType", "DN"},
									{"destinationIdData", "14165556789"}]}]}},
							{"requestSubType", "RESERVE"}]}]}}]}.

nrf_post_iec_class_b(MSISDN, IMSI, Messages) ->
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	{struct, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", "OCF"}]}},
			{"invocationTimeStamp", InvocationTimeStamp },
			{"invocationSequenceNumber", 1},
			{"subscriptionId",
					{array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}},
			{"oneTimeEvent", true},
			{"oneTimeEventType", "IEC"},
			{"serviceRating",
					{array, [{struct, [{"serviceContextId", "32274@3gpp.org"},
							{"serviceId", 4},
							{"ratingGroup", 32},
							{"consumedUnit", {struct, [{"serviceSpecificUnit", Messages}]}},
							{"destinationId",
									{array, [{struct, [{"destinationIdType", "DN"},
									{"destinationIdData", "14165556789"}]}]}},
							{"requestSubType", "DEBIT"}]}]}}]}.

diameter_ecur_start({MSISDN, IMSI}, SId, RequestNum) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit'
			{'CC-Service-Specific-Units' = [1]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [ocs:generate_identity()]}]}]}]},
	CC_CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

diameter_ecur_final({MSISDN, IMSI}, SId, RequestNum) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'
			{'CC-Service-Specific-Units' = [2]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [ocs:generate_identity()]}]}]}]},
	CC_CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

diameter_iec({MSISDN, IMSI}, SId, RequestNum) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit'
			{'CC-Service-Specific-Units' = [1]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	ServiceInformation = #'3gpp_ro_Service-Information'{
			'SMS-Information' = [#'3gpp_ro_SMS-Information'{
			'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
			'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
			'Address-Data' = [ocs:generate_identity()]}]}]}]},
	CC_CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32274@3gpp.org",
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

nrf_post_initial_scur_class_b(MSISDN, IMSI, InputOctets, OutputOctets) ->
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	{struct, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", "OCF"}]}},
			{"invocationTimeStamp", InvocationTimeStamp},
			{"invocationSequenceNumber", 1},
			{"subscriptionId", {array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}},
			{"serviceRating",
					{array, [{struct, [{"serviceContextId", "32251@3gpp.org"},
							{"serviceInformation",
							{struct, [{"sgsnMccMnc",
							{struct, [{"mcc", "001"}, {"mnc", "001"}]}}]}},
							{"serviceId", 1},
							{"ratingGroup", 32},
							{"requestedUnit", {struct, [{"totalVolume", InputOctets + OutputOctets},
										{"uplinkVolume", InputOctets},
										{"downlinkVolume", OutputOctets}]}},
							{"requestSubType", "RESERVE"}]}]}}]}.

nrf_post_update_scur_class_b(MSISDN, IMSI, InputOctets, OutputOctets) ->
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	{struct, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", "OCF"}]}},
			{"invocationTimeStamp", InvocationTimeStamp},
			{"invocationSequenceNumber", 2},
			{"subscriptionId", {array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}},
			{"serviceRating",
					{array, [{struct, [{"serviceContextId", "32251@3gpp.org"},
							{"serviceId", 1},
							{"ratingGroup", 32},
							{"requestSubType", "DEBIT"},
							{"consumedUnit", {struct, [{"totalVolume", InputOctets + OutputOctets},
									{"uplinkVolume", InputOctets},
									{"downlinkVolume", OutputOctets}]}}]},
					{struct, [{"serviceContextId", "32251@3gpp.org"},
							{"serviceId", 1},
							{"ratingGroup", 32},
							{"requestSubType", "RESERVE"}]}]}}]}.

nrf_post_final_scur_class_b(MSISDN, IMSI, InputOctets, OutputOctets) ->
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	{struct, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", "OCF"}]}},
			{"invocationTimeStamp", InvocationTimeStamp},
			{"invocationSequenceNumber", 3},
			{"subscriptionId", {array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}},
			{"serviceRating",
					{array, [{struct, [{"serviceContextId", "32251@3gpp.org"},
							{"serviceId", 1},
							{"ratingGroup", 32},
							{"requestSubType", "DEBIT"},
							{"consumedUnit", {struct, [{"totalVolume", InputOctets + OutputOctets},
									{"uplinkVolume", InputOctets},
									{"downlinkVolume", OutputOctets}]}}]}]}}]}.

diameter_scur_start(SId, {MSISDN, IMSI}, RequestNum, {InputOctets, OutputOctets}) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Input-Octets' = [InputOctets], 'CC-Output-Octets' = [OutputOctets],
			'CC-Total-Octets' = [InputOctets + OutputOctets]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
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
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

diameter_scur_start1(SId, {MSISDN, IMSI}, RequestNum, _Requested) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Input-Octets' = [], 'CC-Output-Octets' = [],
			'CC-Total-Octets' = []},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits],
			'Rating-Group' = [2]},
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
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

diameter_scur_interim(SId, {MSISDN, IMSI}, RequestNum,
		{UsedInputOctets, UsedOutputOctets}, _Requested) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{
			'CC-Input-Octets' = [UsedInputOctets], 'CC-Output-Octets' = [UsedOutputOctets],
			'CC-Total-Octets' = [UsedInputOctets + UsedOutputOctets]},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Total-Octets' = []},
	MCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits],
			'Requested-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
	MCC2 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits], 'Rating-Group' = [3]},
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
		'User-Name' = [MSISDN],
		'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
		'CC-Request-Number' = RequestNum,
		'Event-Timestamp' = [calendar:universal_time()],
		'Multiple-Services-Credit-Control' = [MCC1, MCC2],
		'Subscription-Id' = [MSISDN1, IMSI1],
		'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

diameter_scur_interim1(SId, {MSISDN, IMSI}, RequestNum, _Used, {InputOctets, OutputOctets}) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{
			'CC-Total-Octets' = []},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Input-Octets' = [InputOctets], 'CC-Output-Octets' = [OutputOctets],
			'CC-Total-Octets' = [InputOctets + OutputOctets]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits],
			'Requested-Service-Unit' = [RequestedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
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
		'User-Name' = [MSISDN],
		'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
		'CC-Request-Number' = RequestNum,
		'Event-Timestamp' = [calendar:universal_time()],
		'Multiple-Services-Credit-Control' = [MultiServices_CC],
		'Subscription-Id' = [MSISDN1, IMSI1],
		'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

diameter_scur_stop(SId, {MSISDN, IMSI}, RequestNum, Used) ->
	MSISDN1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = MSISDN},
	IMSI1 = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = IMSI},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [Used]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits], 'Service-Identifier' = [1],
			'Rating-Group' = [2]},
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
			'User-Name' = [MSISDN],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [MSISDN1, IMSI1],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

%% @hidden
price(Type, Units, Size, Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, units = Units,
			size = Size, amount = Amount}.

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
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], Chars),
	ProdRef.

bucket(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000}.

%% @hidden
add_bucket(ProdRef, Bucket) ->
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.

%% @hidden
auth_header() ->
	{"authorization", basic_auth()}.

%% @hidden
basic_auth() ->
	RestUser = ct:get_config({rest, user}),
	RestPass = ct:get_config({rest, password}),
	EncodeKey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	"Basic " ++ EncodeKey.

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
client_acct_service_opts(Config) ->
	[{'Origin-Host', ?config(diameter_host, Config)},
			{'Origin-Realm', ?config(realm, Config)},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Client (Nrf)"},
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

