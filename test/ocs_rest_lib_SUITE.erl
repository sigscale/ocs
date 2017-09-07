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
	[filter_members, filter_array, filter_deep_array, filter_complex,
			filter_match_list].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

filter_members() ->
	[{userdata, [{doc, "Filter JSON object members"}]}].

filter_members(_Config) ->
	H = {"h", erlang:unique_integer()},
	F = {"f", erlang:unique_integer()},
	D = {"d", erlang:unique_integer()},
	B = {struct, [D, {"e", 5}, F, {"g", 6}, H]},
	ObjectIn = {struct, [{"a", 3}, {"b", B}, {"c", 7}]},
	Filters = "b.f,b.h,b.d",
	ObjectOut = {struct, [{"b", {struct, [D, F, H]}}]},
	ObjectOut = ocs_rest:filter(Filters, ObjectIn).

filter_array() ->
	[{userdata, [{doc, "Filter JSON array"}]}].

filter_array(_Config) ->
	AX = {"x", erlang:unique_integer()},
	AY = {"y", erlang:unique_integer()},
	BX = {"x", erlang:unique_integer()},
	BY = {"y", erlang:unique_integer()},
	CX = {"x", erlang:unique_integer()},
	CY = {"y", erlang:unique_integer()},
	A = {struct, [{"w", 1}, AX, AY, {"z", 2}]},
	B = {struct, [{"w", 3}, BX, BY, {"z", 4}]},
	C = {struct, [{"w", 5}, CX, CY, {"z", 6}]},
	ObjectIn = {array, [A, B, C]},
	Filters = "x,y",
	ObjectOut = {array, [{struct, [AX, AY]},
			{struct, [BX, BY]}, {struct, [CX, CY]}]},
	ObjectOut = ocs_rest:filter(Filters, ObjectIn).

filter_deep_array() ->
	[{userdata, [{doc, "Filter JSON deep array"}]}].

filter_deep_array(_Config) ->
	FX = {"x", erlang:unique_integer()},
	FZ = {"z", erlang:unique_integer()},
	GX = {"x", erlang:unique_integer()},
	GZ = {"z", erlang:unique_integer()},
	HX = {"x", erlang:unique_integer()},
	HZ = {"z", erlang:unique_integer()},
	IX = {"x", erlang:unique_integer()},
	IZ = {"z", erlang:unique_integer()},
	JX = {"x", erlang:unique_integer()},
	JZ = {"z", erlang:unique_integer()},
	KX = {"x", erlang:unique_integer()},
	KZ = {"z", erlang:unique_integer()},
	LX = {"x", erlang:unique_integer()},
	LZ = {"z", erlang:unique_integer()},
	MX = {"x", erlang:unique_integer()},
	MZ = {"z", erlang:unique_integer()},
	NX = {"x", erlang:unique_integer()},
	NZ = {"z", erlang:unique_integer()},
	F = {struct, [{"w", 17}, FX, {"y", 18}, FZ]},
	G = {struct, [{"w", 15}, GX, {"y", 16}, GZ]},
	H = {struct, [{"w", 13}, HX, {"y", 14}, HZ]},
	I = {struct, [{"w", 11}, IX, {"y", 12}, IZ]},
	J = {struct, [{"w", 9}, JX, {"y", 10}, JZ]},
	K = {struct, [{"w", 7}, KX, {"y", 8}, KZ]},
	L = {struct, [{"w", 5}, LX, {"y", 6}, LZ]},
	M = {struct, [{"w", 3}, MX, {"y", 4}, MZ]},
	N = {struct, [{"w", 1}, NX, {"y", 2}, NZ]},
	C = {array, [F, G, H]},
	D = {array, [I, J, K]},
	E = {array, [L, M, N]},
	ObjectIn = {struct, [{"a", 1},
			{"b", {array, [C, D, E]}}, {"f", 21}]},
	Filters = "b.x,b.z",
	Fo = {struct, [FX, FZ]},
	Go = {struct, [GX, GZ]},
	Ho = {struct, [HX, HZ]},
	Io = {struct, [IX, IZ]},
	Jo = {struct, [JX, JZ]},
	Ko = {struct, [KX, KZ]},
	Lo = {struct, [LX, LZ]},
	Mo = {struct, [MX, MZ]},
	No = {struct, [NX, NZ]},
	Co = {array, [Fo, Go, Ho]},
	Do = {array, [Io, Jo, Ko]},
	Eo = {array, [Lo, Mo, No]},
	ObjectOut = {struct, [{"b", {array, [Co, Do, Eo]}}]},
	ObjectOut = ocs_rest:filter(Filters, ObjectIn).

filter_complex() ->
	[{userdata, [{doc, "Apply filters to a JSON object"}]}].

filter_complex(_Config) ->
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

filter_match_list() ->
	[{userdata, [{doc, "Filter match value list syntax"}]}].

filter_match_list(_Config) ->
	B1 = {"b", "bob"},
	D1 = {"d", erlang:unique_integer()},
	S1 = {struct, [{"a", 1}, B1, {"c", 2}, D1, {"e", 3}]},
	B2 = {"b", "carol"},
	D2 = {"d", erlang:unique_integer()},
	S2 = {struct, [{"a", 4}, B2, {"c", 5}, D2, {"e", 6}]},
	B3 = {"b", "ted"},
	D3 = {"d", erlang:unique_integer()},
	S3 = {struct, [{"a", 7}, B3, {"c", 8}, D3, {"e", 9}]},
	B4 = {"b", "alice"},
	D4 = {"d", erlang:unique_integer()},
	S4 = {struct, [{"a", 10}, B4, {"c", 11}, D4, {"e", 12}]},
	A = {array, [S1, S2, S3, S4]},
	ObjectIn = {struct, [{"f", 13}, {"g", A}, {"h", 14}]},
	Filters = "g.b=(carol,alice),g.d",
	O2 = {struct, [B2, D2]},
	O4 = {struct, [B4, D4]},
	ObjectOut = {struct, [{"g", {array, [O2, O4]}}]},
	ObjectOut = ocs_rest:filter(Filters, ObjectIn).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

