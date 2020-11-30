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
	OriginHost = "ct" ++ OriginRealm,
	AMBR = [#'3gpp_swx_AMBR'{
			'Max-Requested-Bandwidth-UL' = 1000000000,
			'Max-Requested-Bandwidth-DL' = 100000000}],
	APNConfiguration = [#'3gpp_swx_APN-Configuration'{
			'Context-Identifier' = 1,
			'PDN-Type' = 0,
			'Service-Selection' = "public",
			'AMBR' = AMBR}],
	DEA = #'3gpp_sta_DEA'{'Session-Id' = diameter:session_id(OriginHost),
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Auth-Request-Type' = 3,
			'Result-Code' = 2001,
			'APN-Configuration' = APNConfiguration},
	#diameter_packet{bin = B} = diameter_codec:encode(diameter_gen_3gpp_sta_application, DEA),
	#diameter_packet{msg = M} = diameter_codec:decode(diameter_gen_3gpp_sta_application, B),
	#'3gpp_sta_DEA'{'APN-Configuration' = [#'3gpp_sta_APN-Configuration'{}]} = M.

encode_swm_dea() ->
	[{userdata, [{doc, "Encode an SWm DEA"}]}].

encode_swm_dea(_Config) ->
	OriginRealm = "example.net",
	OriginHost = "ct" ++ OriginRealm,
	AMBR = [#'3gpp_swx_AMBR'{
			'Max-Requested-Bandwidth-UL' = 1000000000,
			'Max-Requested-Bandwidth-DL' = 100000000}],
	APNConfiguration = [#'3gpp_swx_APN-Configuration'{
			'Context-Identifier' = 1,
			'PDN-Type' = 0,
			'Service-Selection' = "public",
			'AMBR' = AMBR}],
	DEA = #'3gpp_swm_DEA'{'Session-Id' = diameter:session_id(OriginHost),
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Auth-Request-Type' = 3,
			'Result-Code' = 2001,
			'APN-Configuration' = APNConfiguration},
	#diameter_packet{bin = B} = diameter_codec:encode(diameter_gen_3gpp_swm_application, DEA),
	#diameter_packet{msg = M} = diameter_codec:decode(diameter_gen_3gpp_swm_application, B),
	#'3gpp_swm_DEA'{'APN-Configuration' = [#'3gpp_swm_APN-Configuration'{}]} = M.

encode_s6b_aaa() ->
	[{userdata, [{doc, "Encode an S6b AAA"}]}].

encode_s6b_aaa(_Config) ->
	OriginRealm = "example.net",
	OriginHost = "ct" ++ OriginRealm,
	AMBR = [#'3gpp_swx_AMBR'{
			'Max-Requested-Bandwidth-UL' = 1000000000,
			'Max-Requested-Bandwidth-DL' = 100000000}],
	APNConfiguration = [#'3gpp_swx_APN-Configuration'{
			'Context-Identifier' = 1,
			'PDN-Type' = 0,
			'Service-Selection' = "public",
			'AMBR' = AMBR}],
	AAA = #'3gpp_s6b_AAA'{'Session-Id' = diameter:session_id(OriginHost),
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Auth-Request-Type' = 2,
			'Result-Code' = 2001,
			'APN-Configuration' = APNConfiguration},
	#diameter_packet{bin = B} = diameter_codec:encode(diameter_gen_3gpp_s6b_application, AAA),
	#diameter_packet{msg = M} = diameter_codec:decode(diameter_gen_3gpp_s6b_application, B),
	#'3gpp_s6b_AAA'{'APN-Configuration' = [#'3gpp_s6b_APN-Configuration'{}]} = M.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

