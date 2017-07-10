%%% ocs_log_SUITE.erl
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
%%%  @doc Test suite for logging in the {@link //ocs. ocs} application.
%%%
-module(ocs_log_SUITE).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("../include/diameter_gen_cc_application_rfc4006.hrl").

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
	[radius_log_auth_event, diameter_log_auth_event,
			radius_log_acct_event, diameter_log_acct_event,
			ipdr_log, get_range, get_last, auth_query, acct_query].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

radius_log_auth_event() ->
   [{userdata, [{doc, "Log a RADIUS access request event"}]}].

radius_log_auth_event(_Config) ->
	Start = erlang:system_time(?MILLISECOND),
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
	End = erlang:system_time(?MILLISECOND),
	Fany = fun({TS, _, radius, N, S, C, T, A1, A2}) when TS >= Start, TS =< End,
					N == Node, S == Server, C == Client, T == Type,
					A1 == ReqAttrs, A2 == ResAttrs ->
				true;
			(_) ->
				false	
	end,
	Find = fun(_F, {error, Reason}) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}) ->
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
   [{userdata, [{doc, "Log a DIAMETER AAR event"}]}].

diameter_log_auth_event(_Config) ->
	Start = erlang:system_time(?MILLISECOND),
	Protocol = diameter,
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 3668,
	ClientAddress = {86,12,5,8},
	ClientPort = 5053,
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	SessionID = diameter:session_id("ackjd713eedhc"),
	OH = "client.testdomain.com",
	OR = "testdomain.com",
	DR = "sigscale.com",
	AuthType = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY',
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	DiameterRequest = #diameter_nas_app_AAR{'Session-Id' = SessionID,
			'Auth-Application-Id' = 1, 'Origin-Host' = OH,
			'Origin-Realm' = OR, 'Destination-Realm' = DR,
			'Auth-Request-Type' = AuthType},
	DiameterAnswer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
			'Auth-Application-Id' = 1, 'Auth-Request-Type' = AuthType,
			'Result-Code' = ResultCode, 'Origin-Host' = OH,
			'Origin-Realm' = OR},
	End = erlang:system_time(?MILLISECOND),
	ok = ocs_log:auth_log(diameter, Server, Client, DiameterRequest, DiameterAnswer),
	Fany = fun({TS, _, P, N, S, C, R, A}) when P == Protocol,
					TS >= Start, TS =< End, N == Node, S == Server, C == Client,
					R == DiameterRequest, A == DiameterAnswer ->
				true;
			(_) ->
				false
	end,
	Find = fun(_F, {error, Reason}) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}) ->
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
	Start = erlang:system_time(?MILLISECOND),
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	ClientAddress = {192, 168, 150, 151},
	Type = start,
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "DE:AD:BE:EF:CA:FE"}, {?AcctSessionId, "8240019b"},
			{?CallingStationId, "FE-ED-BE-EF-F0-0D"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"}, {?AcctAuthentic, 1},
			{?AcctStatusType, 1}, {?NasIdentifier, "ap-1.sigscale.net"},
			{?AcctDelayTime, 0}, {?NasIpAddress, ClientAddress}],
	ok = ocs_log:acct_log(radius, Server, Type, ReqAttrs),
	End = erlang:system_time(?MILLISECOND),
	Fany = fun({TS, _, radius, N, S, T, A}) when TS >= Start, TS =< End,
					N == Node, S == Server, T == Type, A == ReqAttrs ->
				true;
			(_) ->
				false	
	end,
	Find = fun(_F, {error, Reason}) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}) ->
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
   [{userdata, [{doc, "Log a DIAMETER CCR event"}]}].

diameter_log_acct_event(_Config) ->
	Start = erlang:system_time(?MILLISECOND),
	Protocol = diameter,
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	RequestType = start,
	ok = ocs_log:acct_log(diameter, Server, RequestType, #diameter_cc_app_CCR{}),
	End = erlang:system_time(?MILLISECOND),
	Fany = fun({TS, _, P, N, S, RType, Attr})
					when P == Protocol, TS >= Start, TS =< End, N == Node,
					S == Server, RType == RequestType, is_record(Attr, diameter_cc_app_CCR) ->
				true;
			(_) ->
				false	
	end,
	Find = fun(_F, {error, Reason}) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}) ->
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
	Start = erlang:system_time(?MILLISECOND),
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
				ok = ocs_log:acct_log(radius, Server, Type, Attrs2),
				F(F, N - 1)
	end,
	Fill(Fill, NumItems),
	End = erlang:system_time(?MILLISECOND),
	ok = disk_log:sync(ocs_acct),
	Range = (End - Start),
	StartRange = Start + (Range div 3),
	EndRange = End - (Range div 3),
	{ok, IpdrLogDir} = application:get_env(ocs, ipdr_log_dir),
	Filename = IpdrLogDir ++ "/ipdr-" ++ ocs_log:iso8601(erlang:system_time(?MILLISECOND)),
	ok = ocs_log:ipdr_log(Filename, StartRange, EndRange),
	GetRangeResult = ocs_log:get_range(ocs_acct, StartRange, EndRange),
	Fstop = fun(E, Acc) when element(5, E) == stop ->
				Acc + 1;
			(_, Acc) ->
				Acc
	end,
	NumStops = lists:foldl(Fstop, 0, GetRangeResult),
	{ok, IpdrLog} = disk_log:open([{name, make_ref()}, {file, Filename}]),
	Fchunk = fun(_F, {error, Reason}, _Acc) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}, Acc) ->
				F(F, disk_log:chunk(IpdrLog, Cont), Acc + length(Chunk));
			(_, eof, Acc) ->
				disk_log:close(IpdrLog),
				Acc
	end,
	NumStops = Fchunk(Fchunk, disk_log:chunk(IpdrLog, start), 0) - 2.

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
	Start = erlang:system_time(?MILLISECOND),
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
				ocs_log:acct_log(radius, Server, Type,
						[{?AcctSessionId, integer_to_list(N)} | Attrs]),
				F(F, N - 1)
	end,
	Fill(Fill, NumItems),
	End = erlang:system_time(?MILLISECOND),
	Range = (End - Start),
	StartRange = Start + (Range div 3),
	EndRange = End - (Range div 3),
	Result = ocs_log:get_range(ocs_acct, StartRange, EndRange),
	true = length(Result) > ((NumItems div 3) - (NumItems div 10)),
	[{?AcctSessionId, ID} | _] = element(7, lists:nth(1, Result)),
	StartNum = list_to_integer(ID),
	Fverify = fun({TS, _, radius, _, _, _,  _}, _N)
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

get_last() ->
   [{userdata, [{doc, "Get last events from log"}]}].

get_last(_Config) ->
	FileSize = 1048576,
	NumFiles = 10,
	Term = {0, lists:duplicate(250, 0)},
	AverageSize = erlang:external_size({0, Term}),
	NumChunkItems = 65536 div AverageSize,
	{ok, Log} = disk_log:open([{name, make_ref()}, {file, "last"},
			{type, wrap}, {size, {FileSize, NumFiles}}]),
	Fill = fun(F, FileNum, ItemNum, 0) ->
				Info = disk_log:info(Log),
				case lists:keyfind(current_file, 1, Info) of
					{current_file, FileNum} ->
						ItemNum;
					{current_file, _} ->
						F(F, FileNum, ItemNum, NumChunkItems)
				end;
			(F, FileNum, ItemNum, N) ->
				NewItemNum = ItemNum + 1,
				R = rand:uniform(500),
				Item = {NewItemNum, lists:duplicate(R, 0)},
				disk_log:log(Log, Item),
				F(F, FileNum, NewItemNum, N - 1)
	end,
	% check with half full wrap log
	NumTotal1 = Fill(Fill, NumFiles div 2, 0, NumChunkItems),
	MaxSize = (NumChunkItems * 3) + 25,
	{MaxSize, Items1} = ocs_log:last(Log, MaxSize),
	Fcheck = fun({N, _}, N) ->
				N - 1
	end,
	StartItem1 = NumTotal1 - MaxSize,
	StartItem1 = lists:foldl(Fcheck, NumTotal1, Items1),
	% check while logging into last wrap file
	NumTotal2 = Fill(Fill, NumFiles, NumTotal1, NumChunkItems),
	{MaxSize, Items2} = ocs_log:last(Log, MaxSize),
	StartItem2 = NumTotal2 - MaxSize,
	StartItem2 = lists:foldl(Fcheck, NumTotal2, Items2),
	% check while logging into first file, after turnover
	NumTotal3 = Fill(Fill, 1, NumTotal2, NumChunkItems),
	{MaxSize, Items3} = ocs_log:last(Log, MaxSize),
	StartItem3 = NumTotal3 - MaxSize,
	StartItem3 = lists:foldl(Fcheck, NumTotal3, Items3),
	% check while logging into second file, after turnover
	NumTotal4 = Fill(Fill, 2, NumTotal3, NumChunkItems),
	{MaxSize, Items4} = ocs_log:last(Log, MaxSize),
	StartItem4 = NumTotal4 - MaxSize,
	StartItem4 = lists:foldl(Fcheck, NumTotal4, Items4).

auth_query() ->
   [{userdata, [{doc, "Get matching access log events"}]}].

auth_query(_Config) ->
	Server = {{0,0,0,0}, 1812},
	ClientAddress = {10,0,0,1},
	Client = {ClientAddress, 37645},
	Username = ocs:generate_identity(),
	NasIdentifier = "ap13.sigscale.net",
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"},
			{?NasPortType, 19}, {?UserName, Username},
			{?CallingStationId, "BE:EF:FE:ED:CA:FE"},
			{?CalledStationId, "CA:FE:CA:FE:CA:FE:AP13"},
			{?NasIdentifier, NasIdentifier},
			{?NasIpAddress, ClientAddress}],
	RespAttrs = [{?SessionTimeout, 3600}],
	ok = fill_auth(1000),
	LogInfo = disk_log:info(ocs_auth),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{_, CurItems} = lists:keyfind(no_current_items, 1, LogInfo),
	{_, CurBytes} = lists:keyfind(no_current_bytes, 1, LogInfo),
	EventSize = CurBytes div CurItems,
	NumItems = (FileSize div EventSize) * 5,
	Start = erlang:system_time(?MILLISECOND),
	ok = fill_auth(NumItems),
	ok = ocs_log:auth_log(radius, Server, Client,
			accept, ReqAttrs, RespAttrs),
	ok = fill_auth(rand:uniform(2000)),
	ok = ocs_log:auth_log(radius, Server, Client,
			accept, ReqAttrs, RespAttrs),
	ok = fill_auth(rand:uniform(2000)),
	ok = ocs_log:auth_log(radius, Server, Client,
			accept, ReqAttrs, RespAttrs),
	ok = fill_auth(rand:uniform(2000)),
	End = erlang:system_time(?MILLISECOND),
	MatchReq = [{?UserName, Username}, {?NasIdentifier, NasIdentifier}],
	Fget = fun(_F, {eof, Events}, Acc) ->
				lists:flatten(lists:reverse([Events | Acc]));
			(F, {Cont, Events}, Acc) ->
				F(F, ocs_log:auth_query(Cont, Start, End, [accept],
						MatchReq, '_'), [Events | Acc])
	end,
	Events = Fget(Fget, ocs_log:auth_query(start, Start, End,
						[accept], MatchReq, '_'), []),
	3 = length(Events).

acct_query() ->
   [{userdata, [{doc, "Get matching accounting log events"}]}].

acct_query(_Config) ->
	Server = {{0,0,0,0}, 1812},
	Username = ocs:generate_identity(),
	ClientAddress = {10,0,0,1},
	NasIdentifier = "ap13.sigscale.net",
	Attrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"},
			{?NasPortType, 19}, {?UserName, Username},
			{?CallingStationId, "BE:EF:FE:ED:CA:FE"},
			{?CalledStationId, "CA:FE:CA:FE:CA:FE:AP13"},
			{?NasIdentifier, NasIdentifier}, {?NasIpAddress, ClientAddress},
			{?AcctStatusType, rand:uniform(3)}, 
			{?AcctSessionTime, rand:uniform(3600) + 100},
			{?AcctInputOctets, rand:uniform(100000000)},
			{?AcctOutputOctets, rand:uniform(100000)}],
	ok = fill_acct(1000),
	LogInfo = disk_log:info(ocs_acct),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{_, CurItems} = lists:keyfind(no_current_items, 1, LogInfo),
	{_, CurBytes} = lists:keyfind(no_current_bytes, 1, LogInfo),
	EventSize = CurBytes div CurItems,
	NumItems = (FileSize div EventSize) * 5,
	Start = erlang:system_time(?MILLISECOND),
	ok = fill_acct(NumItems),
	ok = ocs_log:acct_log(radius, Server, stop, Attrs),
	ok = fill_acct(rand:uniform(2000)),
	ok = ocs_log:acct_log(radius, Server, stop, Attrs),
	ok = fill_acct(rand:uniform(2000)),
	ok = ocs_log:acct_log(radius, Server, stop, Attrs),
	ok = fill_acct(rand:uniform(2000)),
	End = erlang:system_time(?MILLISECOND),
	MatchReq = [{?UserName, Username}, {?NasIdentifier, NasIdentifier}],
	Fget = fun(_F, {eof, Events}, Acc) ->
				lists:flatten(lists:reverse([Events | Acc]));
			(F, {Cont, Events}, Acc) ->
				F(F, ocs_log:acct_query(Cont, Start, End, [stop],
						MatchReq), [Events | Acc])
	end,
	Events = Fget(Fget, ocs_log:acct_query(start, Start, End,
						[stop], MatchReq), []),
	3 = length(Events).

%% internal functions

fill_auth(0) ->
	ok;
fill_auth(N) ->
	Server = {{0, 0, 0, 0}, 1812},
	I3 = rand:uniform(256) - 1,
	I4 = rand:uniform(254),
	ClientAddress = {192, 168, I3, I4},
	Client = {ClientAddress, rand:uniform(64512) + 1024},
	NASn = integer_to_list((I3 bsl 8) + I4),
	NasIdentifier = "ap-" ++ NASn ++ ".sigscale.net",
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, ocs:generate_identity()}, {?CallingStationId, mac()},
			{?CalledStationId, mac() ++ ":AP1"}, {?NasIdentifier, NasIdentifier},
			{?NasIpAddress, ClientAddress}],
	{Type, RespAttrs} = resp_attr(),
	ok = ocs_log:auth_log(radius, Server, Client, Type, ReqAttrs, RespAttrs),
	fill_auth(N - 1).

fill_acct(0) ->
	ok;
fill_acct(N) ->
	Server = {{0, 0, 0, 0}, 1812},
	I3 = rand:uniform(256) - 1,
	I4 = rand:uniform(254),
	ClientAddress = {192, 168, I3, I4},
	Client = {ClientAddress, rand:uniform(64512) + 1024},
	NASn = integer_to_list((I3 bsl 8) + I4),
	NasIdentifier = "ap-" ++ NASn ++ ".sigscale.net",
	Type = case rand:uniform(3) of
		1 -> start;
		2 -> stop;
		3 -> interim
	end,
	Attrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, ocs:generate_identity()}, {?CallingStationId, mac()},
			{?CalledStationId, mac() ++ ":AP1"}, {?NasIdentifier, NasIdentifier},
			{?NasIpAddress, ClientAddress}, {?AcctStatusType, rand:uniform(3)}, 
			{?AcctSessionTime, rand:uniform(3600) + 100},
			{?AcctInputOctets, rand:uniform(100000000)},
			{?AcctOutputOctets, rand:uniform(100000)}],
	ok = ocs_log:acct_log(radius, Server, Type, Attrs),
	fill_acct(N - 1).

mac() ->
	mac(6, []).
mac(0, Acc) ->
	io_lib:fwrite("~.16B:~.16B:~.16B:~.16B:~.16B:~.16B", Acc);
mac(N, Acc) ->
	mac(N - 1, [rand:uniform(256) - 1 | Acc]).

resp_attr() ->
	resp_attr(rand:uniform(100)).
resp_attr(N) when N < 6 ->
	{reject, [{?ReplyMessage, "Subscriber Disabled"}]};
resp_attr(N) when N < 10 ->
	{reject, [{?ReplyMessage,"Out of Credit"}]};
resp_attr(N) when N < 16 ->
	{reject, [{?ReplyMessage,"Unknown Username"}]};
resp_attr(N) when N < 50 ->
	{accept, [{?SessionTimeout, 3600}, {?AcctInterimInterval, 300},
			{?VendorSpecific, {?Ascend, {?AscendDataRate, 10000000}}}]};
resp_attr(_) ->
	{accept, [{?SessionTimeout, 3600}]}.

