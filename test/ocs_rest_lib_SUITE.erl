%%% ocs_rest_lib_SUITE.erl
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
%%%  @doc Test suite for REST utility library
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_lib_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for REST utilities in OCS"}]},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
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
	[filter].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

filter() ->
	[{userdata, [{doc, "Apply filters to a JSON object"}]}].

filter(_Config) ->
	G = {"g", erlang:unique_integer()},
	F = {struct, [{"f", erlang:unique_integer()}]},
	E = {struct, [{"g", erlang:unique_integer()},
			{"h", erlang:unique_integer()}]}, 
	Y = {struct, [{"p", {struct, [{"r", 3}, {"s", 9}]}}, {"q", 8}]},
	D = {struct, [{"z", 1}, {"f", F}, {"e", E}]},
	W = {struct, [{"name", "w"}, {"value", erlang:unique_integer()}]},
	V = {struct, [{"name", "v"}, {"value", erlang:unique_integer()}]},
	U = {struct, [{"name", "u"}, {"value", erlang:unique_integer()}]},
	C = {struct, [{"d", D}, {"x", {array, [U, V, 6, W, Y]}}]},
	B = {struct, [{"s", 5}, {"t", 6}, {"c", C}]},
	A = {struct, [{"o", 3}, {"p", 4}, {"b", B},
			{struct, [{"q", 5}, {"r", 6}]}]},
	ObjectIn = {struct, [{"a", A}, G, {"m", 9}]},
	Filters = "a.b.c.d.e,g,a.b.c.x.name=v,a.b.c.x.value,a.b.c.d.f",
	ObjectOut = {struct, [{"a", {struct, [{"b", {struct, [{"c", {struct,
			[{"d", {struct, [{"e", E}, {"f", F}]}},
			{"x", {array, [V]}}]}}]}}]}}, G]},
	ObjectOut = ocs_rest:filter(Filters, ObjectIn).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

