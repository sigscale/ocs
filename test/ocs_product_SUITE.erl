%%% ocs_product_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
%%%  @doc Test suite for product in {@link //ocs. ocs} application.
%%%
-module(ocs_product_SUITE).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-behaviour(ct_suite).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for product in OCS"}]},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_test_lib:stop(),
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
	[monthly_data_allowance, monthly_voice_allowance, monthly_text_allowance].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

monthly_data_allowance() ->
	[{userdata, [{doc, "Standalone monthly data allowance"}]}].

monthly_data_allowance(_Config) ->
	Alteration = #alteration{name = name(),
			type = recurring, period = monthly,
			units = octets, size = mb(), amount = 0},
	Price = #price{name = name(), alteration = Alteration,
			type = usage, units = octets,
			size = octets(), amount = cents()},
	Offer = #offer{name = name(), specification = "8", price = [Price]},
	{ok, #offer{}} = ocs:add_offer(Offer).

monthly_voice_allowance() ->
	[{userdata, [{doc, "Standalone monthly voice allowance"}]}].

monthly_voice_allowance(_Config) ->
	Alteration = #alteration{name = name(),
			type = recurring, period = monthly,
			units = seconds, size = minutes(), amount = cents()},
	Price = #price{name = name(), alteration = Alteration,
			type = usage, units = seconds, size = seconds(), amount = cents()},
	Offer = #offer{name = name(), specification = "9", price = [Price]},
	{ok, #offer{}} = ocs:add_offer(Offer).

monthly_text_allowance() ->
	[{userdata, [{doc, "Standalone monthly text messaging allowance"}]}].

monthly_text_allowance(_Config) ->
	Alteration = #alteration{name = name(),
			type = recurring, period = monthly,
			units = messages, size = messages(), amount = 0},
	Price = #price{name = name(), alteration = Alteration,
			type = usage, units = octets, size = 1, amount = cents()},
	Offer = #offer{name = name(), specification = "11", price = [Price]},
	{ok, #offer{}} = ocs:add_offer(Offer).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

name() ->
	ocs:generate_identity().

octets() ->
	rand:uniform(1000000000).

mb() ->
	rand:uniform(1000) * 1024.

minutes() ->
	rand:uniform(600) * 60.

seconds() ->
	rand:uniform(3600).

messages() ->
	rand:uniform(100) * 10.

cents() ->
	rand:uniform(10000000).

dollars() ->
	rand:uniform(100) * 1000000.

