%%% ocs_log_SUITE.erl
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
%%%  @doc Test suite for logging in the {@link //ocs. ocs} application.
%%%
-module(ocs_log_SUITE).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for logging in OCS"}]},
	{timetrap, {seconds, 20}}].

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
	[log_auth_event, log_acct_event].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

log_auth_event() ->
   [{userdata, [{doc, "Log an access request event"}]}].

log_auth_event(_Config) ->
	Start = erlang:system_time(millisecond),
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1812,
	Server = {ServerAddress, ServerPort},
	ClientAddress = {192, 168, 150, 151},
	ClientPort = 49651,
	Client = {ClientAddress, ClientPort},
	Type = accept,
	RandomList = [crypto:rand_uniform(N, 256) || N <- lists:seq(1, 16)],
	RandomBin = list_to_binary(RandomList),
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "DE:AD:BE:EF:CA:FE"}, {?AcctSessionId, "8240019b"},
			{?CallingStationId, "FE-ED-BE-EF-F0-0D"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?UserPassword, RandomList},
			{?NasIdentifier, "AP 1"}, {?NasIpAddress, ClientAddress}],
	ResAttrs = [{?SessionTimeout, 3600}, {?MessageAuthenticator, RandomBin}],
	ok = ocs_log:radius_auth_log(Server, Client, Type, ReqAttrs, ResAttrs),
	End = erlang:system_time(millisecond),
	Fany = fun({TS, N, S, C, T, A1, A2}) when TS >= Start, TS =< End,
					N == Node, S == Server, C == Client, T == Type,
					A1 == ReqAttrs, A2 == ResAttrs ->
				true;
			(_) ->
				false	
	end,
	Find = fun(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(radius_auth, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(radius_auth, start)).

log_acct_event() ->
   [{userdata, [{doc, "Log an accounting event"}]}].

log_acct_event(_Config) ->
	Start = erlang:system_time(millisecond),
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	ClientAddress = {192, 168, 150, 151},
	ClientPort = 59132,
	Client = {ClientAddress, ClientPort},
	Type = start,
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "DE:AD:BE:EF:CA:FE"}, {?AcctSessionId, "8240019b"},
			{?CallingStationId, "FE-ED-BE-EF-F0-0D"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"}, {?AcctAuthentic, 1},
			{?AcctStatusType, 1}, {?NasIdentifier, "AP 1"}, {?AcctDelayTime, 0},
			{?NasIpAddress, ClientAddress}],
	ok = ocs_log:radius_acct_log(Server, Client, Type, ReqAttrs),
	End = erlang:system_time(millisecond),
	Fany = fun({TS, N, S, C, T, A}) when TS >= Start, TS =< End,
					N == Node, S == Server, C == Client, T == Type,
					A == ReqAttrs ->
				true;
			(_) ->
				false	
	end,
	Find = fun(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(radius_acct, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(radius_acct, start)).

