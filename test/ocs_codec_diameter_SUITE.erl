%%% ocs_codec_diameter_SUITE.erl
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
%%%  @doc Test suite for specialized DIAMETER encoding/decoding (CODEC)
%%% 	functions of the {@link //ocs. ocs} application.
%%%
-module(ocs_codec_diameter_SUITE).
-copyright('Copyright (c) 2016 - 2020 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_s6b_application.hrl").

-define(STa_APPLICATION_ID, 16777250).
-define(SWm_APPLICATION_ID, 16777264).
-define(S6b_APPLICATION_ID, 16777272).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for specialized DIAMETER CODEC functions in OCS"}]},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = diameter:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop(),
	Config.

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
	[encode_sta_dea, encode_swm_dea, encode_s6b_aaa].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

encode_sta_dea() ->
	[{userdata, [{doc, "Encode an STa DEA"}]}].

encode_sta_dea(_Config) ->
	OriginRealm = "example.net",
	OriginHost = "ct." ++ OriginRealm,
	DEA = #'3gpp_sta_DEA'{'Session-Id' = diameter:session_id(OriginHost),
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Auth-Request-Type' = 3,
			'Result-Code' = 2001,
			'APN-Configuration' = apn_configuration(),
			'Trace-Info' = [#'3gpp_swx_Trace-Info'{
					'Trace-Data' = [#'3gpp_swx_Trace-Data'{
							'Trace-Reference' = "001001001",
							'Trace-Depth' = 1,
							'Trace-NE-Type-List' = "PDN GW",
							'Trace-Event-List' = <<0,1,2,3,4,5,6,7>>,
							'Trace-Collection-Entity' = {203,0,113,188}}]}],
			'Emergency-Info' = [#'3gpp_swx_Emergency-Info'{
					'MIP6-Agent-Info' = [#'3gpp_swx_MIP6-Agent-Info'{
							'MIP-Home-Agent-Address' = [{203,0,113,77}]}]}],
			'Subscription-Id' = [#'3gpp_swx_Subscription-Id'{
					'Subscription-Id-Type' = 0,
					'Subscription-Id-Data' = <<"14165551234">>}]},
	#diameter_packet{bin = B} = diameter_codec:encode(diameter_gen_3gpp_sta_application, DEA),
	#diameter_packet{msg = M} = diameter_codec:decode(diameter_gen_3gpp_sta_application, B),
	#'3gpp_sta_DEA'{'APN-Configuration' = [#'3gpp_sta_APN-Configuration'{} | _]} = M.

encode_swm_dea() ->
	[{userdata, [{doc, "Encode an SWm DEA"}]}].

encode_swm_dea(_Config) ->
	OriginRealm = "example.net",
	OriginHost = "ct." ++ OriginRealm,
	DEA = #'3gpp_swm_DEA'{'Session-Id' = diameter:session_id(OriginHost),
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Auth-Request-Type' = 3,
			'Result-Code' = 2001,
			'APN-Configuration' = apn_configuration(),
			'Trace-Info' = [#'3gpp_swx_Trace-Info'{
					'Trace-Data' = [#'3gpp_swx_Trace-Data'{
							'Trace-Reference' = "001001001",
							'Trace-Depth' = 1,
							'Trace-NE-Type-List' = "PDN GW",
							'Trace-Event-List' = <<0,1,2,3,4,5,6,7>>,
							'Trace-Collection-Entity' = {203,0,113,188}}]}],
			'Emergency-Info' = [#'3gpp_swx_Emergency-Info'{
					'MIP6-Agent-Info' = [#'3gpp_swx_MIP6-Agent-Info'{
							'MIP-Home-Agent-Address' = [{203,0,113,77}]}]}],
			'Subscription-Id' = [#'3gpp_swx_Subscription-Id'{
					'Subscription-Id-Type' = 0,
					'Subscription-Id-Data' = <<"14165551234">>}]},
	#diameter_packet{bin = B} = diameter_codec:encode(diameter_gen_3gpp_swm_application, DEA),
	#diameter_packet{msg = M} = diameter_codec:decode(diameter_gen_3gpp_swm_application, B),
	#'3gpp_swm_DEA'{'APN-Configuration' = [#'3gpp_swm_APN-Configuration'{} | _]} = M.

encode_s6b_aaa() ->
	[{userdata, [{doc, "Encode an S6b AAA"}]}].

encode_s6b_aaa(_Config) ->
	OriginRealm = "example.net",
	OriginHost = "ct." ++ OriginRealm,
	AAA = #'3gpp_s6b_AAA'{'Session-Id' = diameter:session_id(OriginHost),
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Auth-Request-Type' = 2,
			'Result-Code' = 2001,
			'APN-Configuration' = [hd(apn_configuration())],
			'Trace-Info' = [#'3gpp_swx_Trace-Info'{
					'Trace-Data' = [#'3gpp_swx_Trace-Data'{
							'Trace-Reference' = "001001001",
							'Trace-Depth' = 1,
							'Trace-NE-Type-List' = "PDN GW",
							'Trace-Event-List' = <<0,1,2,3,4,5,6,7>>,
							'Trace-Collection-Entity' = {203,0,113,188}}]}]},
	#diameter_packet{bin = B} = diameter_codec:encode(diameter_gen_3gpp_s6b_application, AAA),
	#diameter_packet{msg = M} = diameter_codec:decode(diameter_gen_3gpp_s6b_application, B),
	#'3gpp_s6b_AAA'{'APN-Configuration' = [#'3gpp_s6b_APN-Configuration'{} | _]} = M.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

apn_configuration() ->
	[#'3gpp_swx_APN-Configuration'{
			'Context-Identifier' = 1,
			'PDN-Type' = 0,
			'Service-Selection' = "internet",
			'EPS-Subscribed-QoS-Profile' = [#'3gpp_swx_EPS-Subscribed-QoS-Profile'{
					'QoS-Class-Identifier' = 9,
					'Allocation-Retention-Priority' = #'3gpp_swx_Allocation-Retention-Priority'{
							'Priority-Level' = 15,
							'Pre-emption-Capability' = [1],
							'Pre-emption-Vulnerability' = [0]}}],
			'MIP6-Agent-Info' = [#'3gpp_swx_MIP6-Agent-Info'{
					'MIP-Home-Agent-Address' = [{203,0,113,77}]}],
			'3GPP-Charging-Characteristics' = [<<"0800">>],
			'AMBR' = [#'3gpp_swx_AMBR'{
					'Max-Requested-Bandwidth-UL' = 50000000,
					'Max-Requested-Bandwidth-DL' = 50000000}]},
	#'3gpp_swx_APN-Configuration'{
			'Context-Identifier' = 1,
			'PDN-Type' = 0,
			'Service-Selection' = "ims",
			'EPS-Subscribed-QoS-Profile' = [#'3gpp_swx_EPS-Subscribed-QoS-Profile'{
					'QoS-Class-Identifier' = 9,
					'Allocation-Retention-Priority' = #'3gpp_swx_Allocation-Retention-Priority'{
							'Priority-Level' = 3,
							'Pre-emption-Capability' = [1],
							'Pre-emption-Vulnerability' = [0]}}],
			'MIP6-Agent-Info' = [#'3gpp_swx_MIP6-Agent-Info'{
					'MIP-Home-Agent-Address' = [{203,0,113,77}]}],
			'3GPP-Charging-Characteristics' = [<<"0800">>],
			'AMBR' = [#'3gpp_swx_AMBR'{
					'Max-Requested-Bandwidth-UL' = 50000000,
					'Max-Requested-Bandwidth-DL' = 50000000}]}].

