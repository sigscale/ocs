%%% ocs_snmp_SUITE.erl
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
-module(ocs_snmp_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	Port = rand:uniform(32767) + 32768,
	[{userdata, [{doc, "Test suite for SNMP agent in SigScale OCS"}]},
	{require, snmp_mgr_agent, snmp},
	{default_config, snmp,
			[{start_agent, true},
			{agent_udp, Port},
			{agent_engine_id, sigscale_snmp_lib:engine_id()},
			{users,
					[{ocs_mibs_test, [snmpm_user_default, []]}]},
			{managed_agents,
					[{ocs_mibs_test, [ocs_mibs_test, {127,0,0,1}, Port, []]}]}]},
	{require, snmp_app},
	{default_config, snmp_app,
			[{manager,
					[{config, [{verbosity, silence}]},
					{server, [{verbosity, silence}]},
					{net_if, [{verbosity, silence}]}]},
			{agent,
					[{config, [{verbosity, silence}]},
					{agent_verbosity, silence},
					{net_if, [{verbosity, silence}]}]}]},
	{timetrap, {minutes, 1}}].

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
	ok = ocs_test_lib:start(),
	Realm = ct:get_config({diameter, realm}, "mnc001.mcc001.3gppnetwork.org"),
	Host = ct:get_config({diameter, host}, atom_to_list(?MODULE) ++ "." ++ Realm),
	ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
	ok = application:start(sigscale_mibs),
	ok = sigscale_mib:load(),
	DataDir = filename:absname(?config(data_dir, Config)),
	TestDir = filename:dirname(DataDir),
	BuildDir = filename:dirname(TestDir),
	MibDir =  BuildDir ++ "/priv/mibs/",
	Mibs = [MibDir ++ "SIGSCALE-OCS-MIB",
			MibDir ++ "SIGSCALE-DIAMETER-BASE-PROTOCOL-MIB",
			MibDir ++ "SIGSCALE-DIAMETER-CC-APPLICATION-MIB",
			MibDir ++ "RADIUS-AUTH-SERVER-MIB",
			MibDir ++ "RADIUS-ACC-SERVER-MIB"],
	ok = ct_snmp:load_mibs(Mibs),
	Config1 = [{host, Host}, {realm, Realm} | Config],
	ok = diameter:start_service(?MODULE, client_acct_service_opts(Config1)),
	true = diameter:subscribe(?MODULE),
	{ok, _} = connect(?MODULE, DiameterAddress, DiameterAcctPort, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Up}
				when element(1, Up) == up ->
			Config1
	after
		10000 ->
			{skip, diameter_client_acct_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = diameter:remove_transport(?MODULE, true),
	ok = ocs_mib:unload(),
	ok = sigscale_mib:unload(),
	ok = application:stop(sigscale_mibs),
	ok = ct_snmp:stop(Config),
	ok = ocs_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
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
	[get_client, get_next_client,
			get_radius_auth_ident, get_radius_acct_ident,
			get_diameter_host, get_diameter_realm, get_diameter_product,
			get_diameter_packets_in, get_diameter_packets_out,
			get_diameter_uptime,get_diameter_peer_id,
			get_diameter_firmware_rev, get_diameter_asa_dropped,
			get_diameter_ccr_in, get_diameter_ccr_out,
			get_diameter_ccr_dropped, get_diameter_cca_in,
			get_diameter_cca_out, get_diameter_cca_dropped,
			get_diameter_rar_in, get_diameter_rar_dropped,
			get_diameter_raa_out, get_diameter_raa_dropped,
			get_diameter_str_out, get_diameter_str_dropped,
			get_diameter_sta_in, get_diameter_sta_dropped,
			get_diameter_aar_out, get_diameter_aar_dropped,
			get_diameter_aaa_in, get_diameter_aaa_dropped,
			get_diameter_asr_in, get_diameter_asr_dropped,
			get_diameter_asa_out].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

get_client() ->
	[{userdata, [{doc, "Get client table entry"}]}].

get_client(_Config) ->
	{value, OID} = snmpa:name_to_oid(ocsClientProtocol),
	OID1 = OID ++ [1, 4, 172, 16, 1, 3],
	{noError, _, _Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent).

get_next_client() ->
	[{userdata, [{doc, "Get next on client table"}]}].

get_next_client(_Config) ->
	{value, OID} = snmpa:name_to_oid(ocsClientTable),
	{noError, _, _Varbinds} = ct_snmp:get_next_values(ocs_mibs_test,
			[OID], snmp_mgr_agent).

get_radius_auth_ident() ->
	[{userdata, [{doc, "Get RADIUS authentication server identity"}]}].

get_radius_auth_ident(_Config) ->
	{value, OID} = snmpa:name_to_oid(radiusAuthServIdent),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'OCTET STRING', _, _}] = Varbinds.

get_radius_acct_ident() ->
	[{userdata, [{doc, "Get RADIUS accounting server identity"}]}].

get_radius_acct_ident(_Config) ->
	{value, OID} = snmpa:name_to_oid(radiusAccServIdent),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'OCTET STRING', _, _}] = Varbinds.

get_diameter_host() ->
	[{userdata, [{doc, "Get diameter Origin-Host"}]}].

get_diameter_host(_Config) ->
	{value, OID} = snmpa:name_to_oid(dbpLocalOriginHost),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'OCTET STRING', _, _}] = Varbinds.

get_diameter_realm() ->
	[{userdata, [{doc, "Get diameter Origin-Realm"}]}].

get_diameter_realm(_Config) ->
	{value, OID} = snmpa:name_to_oid(dbpLocalRealm),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'OCTET STRING', _, _}] = Varbinds.

get_diameter_product() ->
	[{userdata, [{doc, "Get diameter Product-Name"}]}].

get_diameter_product(_Config) ->
	{value, OID} = snmpa:name_to_oid(dbpLocalId),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'OCTET STRING', _, _}] = Varbinds.

get_diameter_packets_in() ->
	[{userdata, [{doc, "Get diameter Total Packets In"}]}].

get_diameter_packets_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dbpLocalStatsTotalPacketsIn),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_packets_out() ->
	[{userdata, [{doc, "Get diameter Total Packets Out"}]}].

get_diameter_packets_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dbpLocalStatsTotalPacketsOut),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_uptime() ->
	[{userdata, [{doc, "Get diameter uptime"}]}].

get_diameter_uptime(_Config) ->
	{value, OID} = snmpa:name_to_oid(dbpLocalStatsTotalUpTime),
	OID1 = OID ++ [0],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'TimeTicks', _, _}] = Varbinds.

get_diameter_peer_id() ->
	[{userdata, [{doc, "Get diameter Peer Id"}]}].

get_diameter_peer_id(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPeerId),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'OCTET STRING', _, _}] = Varbinds.

get_diameter_firmware_rev() ->
	[{userdata, [{doc, "Get diameter Firmware Revison"}]}].

get_diameter_firmware_rev(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPeerFirmwareRevision),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Unsigned32', _, _}] = Varbinds.

get_diameter_ccr_in() ->
	[{userdata, [{doc, "Get diameter CCRIn"}]}].

get_diameter_ccr_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsCCRIn),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_ccr_out() ->
	[{userdata, [{doc, "Get diameter CCROut"}]}].

get_diameter_ccr_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsCCROut),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_ccr_dropped() ->
	[{userdata, [{doc, "Get diameter CCRDropped"}]}].

get_diameter_ccr_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsCCRDropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_cca_in() ->
	[{userdata, [{doc, "Get diameter CCAIn"}]}].

get_diameter_cca_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsCCAIn),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_cca_out() ->
	[{userdata, [{doc, "Get diameter CCAOut"}]}].

get_diameter_cca_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsCCAOut),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_cca_dropped() ->
	[{userdata, [{doc, "Get diameter CCADropped"}]}].

get_diameter_cca_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsCCADropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_rar_in() ->
	[{userdata, [{doc, "Get diameter RARIn"}]}].

get_diameter_rar_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsRARIn),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_rar_dropped() ->
	[{userdata, [{doc, "Get diameter RARDropped"}]}].

get_diameter_rar_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsRARDropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_raa_out() ->
	[{userdata, [{doc, "Get diameter RAAOut"}]}].

get_diameter_raa_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsRAAOut),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_raa_dropped() ->
	[{userdata, [{doc, "Get diameter RARDropped"}]}].

get_diameter_raa_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsRAADropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_str_out() ->
	[{userdata, [{doc, "Get diameter STROut"}]}].

get_diameter_str_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsSTROut),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_str_dropped() ->
	[{userdata, [{doc, "Get diameter STRDropped"}]}].

get_diameter_str_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsSTRDropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_sta_in() ->
	[{userdata, [{doc, "Get diameter STAIn"}]}].

get_diameter_sta_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsSTAIn),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_sta_dropped() ->
	[{userdata, [{doc, "Get diameter STADropped"}]}].

get_diameter_sta_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsSTADropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_aar_out() ->
	[{userdata, [{doc, "Get diameter AAROut"}]}].

get_diameter_aar_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsAAROut),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_aar_dropped() ->
	[{userdata, [{doc, "Get diameter AARDropped"}]}].

get_diameter_aar_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsAARDropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_aaa_in() ->
	[{userdata, [{doc, "Get diameter AARIn"}]}].

get_diameter_aaa_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsAAAIn),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_aaa_dropped() ->
	[{userdata, [{doc, "Get diameter AAADropped"}]}].

get_diameter_aaa_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsAAADropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_asr_in() ->
	[{userdata, [{doc, "Get diameter ASRIn"}]}].

get_diameter_asr_in(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsASRIn),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_asr_dropped() ->
	[{userdata, [{doc, "Get diameter ASRDropped"}]}].

get_diameter_asr_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsASRDropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_asa_out() ->
	[{userdata, [{doc, "Get diameter ASAOut"}]}].

get_diameter_asa_out(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsASAOut),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.

get_diameter_asa_dropped() ->
	[{userdata, [{doc, "Get diameter ASRDropped"}]}].

get_diameter_asa_dropped(_Config) ->
	{value, OID} = snmpa:name_to_oid(dccaPerPeerStatsASADropped),
	OID1 = OID ++ [1],
	{noError, _, Varbinds} = ct_snmp:get_values(ocs_mibs_test,
			[OID1], snmp_mgr_agent),
	[{varbind, OID1, 'Counter32', _, _}] = Varbinds.
%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

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
			{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?RO_APPLICATION_ID]},
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
	[{transport_module, Trans},
		{transport_config, [{raddr, RemAddr}, {rport, RemPort},
		{reuseaddr, true}, {ip, LocalAddr}]}].

