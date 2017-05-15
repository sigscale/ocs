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
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("../include/diameter_gen_cc_application_rfc4006.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for logging in OCS"}]},
	{timetrap, {seconds, 120}}].

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
	[radius_log_auth_event, radius_log_acct_event, get_range, ipdr_log,
	diameter_log_auth_event, diameter_log_acct_event].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_log_auth_event() ->
   [{userdata, [{doc, "Log a RADIUS access request event"}]}].

radius_log_auth_event(_Config) ->
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
			{?NasIdentifier, "ap-1.sigscale.net"},
			{?NasIpAddress, ClientAddress}],
	ResAttrs = [{?SessionTimeout, 3600}, {?MessageAuthenticator, RandomBin}],
	ok = ocs_log:auth_log(radius, Server, Client, Type, ReqAttrs, ResAttrs),
	End = erlang:system_time(millisecond),
	Fany = fun({TS, radius, N, S, C, T, A1, A2}) when TS >= Start, TS =< End,
					N == Node, S == Server, C == Client, T == Type,
					A1 == ReqAttrs, A2 == ResAttrs ->
				true;
			(_) ->
				false	
	end,
	Find = fun(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(ocs_auth, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(ocs_auth, start)).

diameter_log_auth_event() ->
   [{userdata, [{doc, "Log a Diameter AAR event"}]}].

diameter_log_auth_event(_Config) ->
	Start = erlang:system_time(millisecond),
	Protocol = diameter,
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 3668,
	Server = {ServerAddress, ServerPort},
	Subscriber = "JohnnyDepp",
	OHost = "client.testdomain.com",
	ORealm = "testdomain.com",
	AuthType = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	End = erlang:system_time(millisecond),
	ok = ocs_log:auth_log(diameter, Server, Subscriber, OHost, ORealm,
			AuthType, ResultCode),
	End = erlang:system_time(millisecond),
	Fany = fun({TS, P, N, S, Sub, OH, OR, AType, RCode}) when P == Protocol,
					TS >= Start, TS =< End, N == Node, S == Server, Sub == Subscriber,
					OH == OHost, OR == ORealm, AType == AuthType, RCode == ResultCode ->
				true;
			(_) ->
				false
	end,
	Find = fun(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(ocs_auth, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(ocs_auth, start)).

radius_log_acct_event() ->
   [{userdata, [{doc, "Log a RADIUS accounting event"}]}].

radius_log_acct_event(_Config) ->
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
			{?AcctStatusType, 1}, {?NasIdentifier, "ap-1.sigscale.net"},
			{?AcctDelayTime, 0}, {?NasIpAddress, ClientAddress}],
	ok = ocs_log:acct_log(radius, Server, Client, Type, ReqAttrs),
	End = erlang:system_time(millisecond),
	Fany = fun({TS, radius, N, S, C, T, A}) when TS >= Start, TS =< End,
					N == Node, S == Server, C == Client, T == Type,
					A == ReqAttrs ->
				true;
			(_) ->
				false	
	end,
	Find = fun(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(ocs_acct, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(ocs_acct, start)).

diameter_log_acct_event() ->
   [{userdata, [{doc, "Log a Diameter CCR event"}]}].

diameter_log_acct_event(_Config) ->
	Start = erlang:system_time(millisecond),
	Protocol = diameter,
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	OHost = "client.testdomain.com",
	ORealm = "testdomain.com",
	RequestType = ?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST',
	Subscriber  = "PaulMccartney",
	Balance = 7648,
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	ok = ocs_log:acct_log(diameter, Server, OHost, ORealm, RequestType,
			Subscriber, Balance, ResultCode),
	End = erlang:system_time(millisecond),
	Fany = fun({TS, P, N, S, OH, OR, RType, Sub, Bal, RCode})
					when P == Protocol, TS >= Start, TS =< End, N == Node,
					S == Server, OH == OHost, OR == ORealm, RType == RequestType,
					Sub == Subscriber, Bal == Balance, RCode == ResultCode ->
				true;
			(_) ->
				false	
	end,
	Find = fun(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(ocs_acct, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(ocs_acct, start)).

get_range() ->
   [{userdata, [{doc, "Get date/time range from log"}]}].

get_range(_Config) ->
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	ClientAddress = {192, 168, 151, 153},
	ClientPort = 59132,
	Client = {ClientAddress, ClientPort},
	Type = start,
	Start = erlang:system_time(millisecond),
	Attrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "BE:EF:CA:FE:FE:DE"},
			{?CallingStationId, "FE-ED-BE-EF-F1-1D"},
			{?CalledStationId, "CA-FE-AC-EF-CA-FE:AP 1"}, {?AcctAuthentic, 1},
			{?AcctStatusType, 1}, {?NasIdentifier, "ap-1.sigscale.net"},
			{?AcctDelayTime, 0}, {?NasIpAddress, ClientAddress}],
	Event = {radius, Start, Node, Server, Client, Type,
			[{?AcctSessionId, "1234567890"} | Attrs]},
	LogInfo = disk_log:info(ocs_acct),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	EventSize = erlang:external_size(Event),
	NumItems = (FileSize div EventSize) * 5,
	Fill = fun(_F, 0) ->
				ok;
			(F, N) ->
				ocs_log:acct_log(radius, Server, Client, Type,
						[{?AcctSessionId, integer_to_list(N)} | Attrs]),
				F(F, N - 1)
	end,
	Fill(Fill, NumItems),
	End = erlang:system_time(millisecond),
	Range = (End - Start),
	StartRange = Start + (Range div 3),
	EndRange = End - (Range div 3),
	Result = ocs_log:get_range(ocs_acct, StartRange, EndRange),
	true = length(Result) > ((NumItems div 3) - (NumItems div 10)),
	[{?AcctSessionId, ID} | _] = element(6, lists:nth(1, Result)),
	StartNum = list_to_integer(ID),
	Fverify = fun({radius, TS, _, _, _, _,  _}, _N)
					when TS < StartRange, TS > EndRange ->
				ct:fail(verify);
			({_, _, _, _, _, _, [{?AcctSessionId, S} | _]}, N) ->
				case list_to_integer(S) of
					N ->
						N - 1;
					_ ->
						ct:fail(verify)
				end;
			(_, N) ->
				N
	end,
	lists:foldl(Fverify, StartNum, Result).

ipdr_log() ->
   [{userdata, [{doc, "Log IPDR reords for date/time range"}]}].

ipdr_log(_Config) ->
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	ClientAddress = {192, 168, 151, 153},
	ClientPort = 59132,
	Client = {ClientAddress, ClientPort},
	Start = erlang:system_time(millisecond),
	Attrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "BE:EF:CA:FE:FE:DE"},
			{?CallingStationId, "FE-ED-BE-EF-F1-1D"},
			{?CalledStationId, "CA-FE-AC-EF-CA-FE:AP 1"}, {?AcctAuthentic, 1},
			{?AcctStatusType, 1}, {?NasIdentifier, "ap-1.sigscale.net"},
			{?AcctDelayTime, 4}, {?NasIpAddress, ClientAddress},
			{?AcctSessionTime, 245}, {?AcctTerminateCause, 1},
			{?AcctInputOctets, 16584}, {?AcctOutputOctets, 1387},
			{?AcctInputGigawords, 1}, {?AcctOutputGigawords, 0}],
	Event = {Start, Node, Server, Client, start,
			[{?AcctSessionId, "1234567890"} | Attrs]},
	LogInfo = disk_log:info(ocs_acct),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	EventSize = erlang:external_size(Event),
	Weight = [7,8] ++ lists:duplicate(32, 1) ++ lists:duplicate(32, 2)
			++ lists:duplicate(33, 3),
	NumItems = (FileSize div EventSize) * 5,
	Fill = fun(_F, 0) ->
				ok;
			(F, N) ->
				Random = crypto:rand_uniform(1, 100),
				{Type, AcctType} = case lists:nth(Random, Weight) of
					1 -> {start, 1};
					2 -> {stop, 2};
					3 -> {interim, 3};
					7 -> {on, 7};
					8 -> {off, 8}
				end,
				Attrs1 = [{?AcctSessionId, integer_to_list(N)} | Attrs],
				Attrs2 = [{?AcctStatusType, AcctType} | Attrs1],
				ocs_log:acct_log(radius, Server, Client, Type, Attrs2),
				F(F, N - 1)
	end,
	Fill(Fill, NumItems),
	End = erlang:system_time(millisecond),
	Range = (End - Start),
	StartRange = Start + (Range div 3),
	EndRange = End - (Range div 3),
	Filename = "ipdr-" ++ ocs_log:iso8601(erlang:system_time(millisecond)),
	ok = ocs_log:ipdr_log(Filename, StartRange, EndRange),
	GetRangeResult = ocs_log:get_range(ocs_acct, StartRange, EndRange),
	Fstop = fun(E, Acc) when element(5, E) == stop ->
				Acc + 1;
			(_, Acc) ->
				Acc
	end,
	NumStops = lists:foldl(Fstop, 0, GetRangeResult),
	{ok, IpdrLog} = disk_log:open([{name, Filename}, {file, Filename}]),
	Fchunk = fun(F, {Cont, Chunk}, Acc) ->
				F(F, disk_log:chunk(IpdrLog, Cont), Acc + length(Chunk));
			(_, eof, Acc) ->
				disk_log:close(IpdrLog),
				Acc
	end,
	NumStops = Fchunk(Fchunk, disk_log:chunk(IpdrLog, start), 0) - 2.

