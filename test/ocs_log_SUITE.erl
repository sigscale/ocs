%%% ocs_log_SUITE.erl
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
%%%  @doc Test suite for logging in the {@link //ocs. ocs} application.
%%%
-module(ocs_log_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs.hrl").
-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("../include/diameter_gen_3gpp_ro_application.hrl").
-include_lib("../include/diameter_gen_3gpp.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

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
	ok = ocs_test_lib:load(ocs),
	Address = {127,0,0,1},
	RadiusAuthPort = rand:uniform(64511) + 1024,
	RadiusAcctPort = rand:uniform(64511) + 1024,
	RadiusAppVar = [{auth, [{Address, RadiusAuthPort, []}]},
			{acct, [{Address, RadiusAcctPort, []}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterAuthPort = rand:uniform(64511) + 1024,
	DiameterAcctPort = rand:uniform(64511) + 1024,
	DiameterAppVar = [{auth, [{Address, DiameterAuthPort, []}]},
		{acct, [{Address, DiameterAcctPort, []}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = ocs_test_lib:start(),
	Host = atom_to_list(?MODULE),
	Realm = "ct.sigscale.org",
	Config1 = [{host, Host}, {realm, Realm},
			{radius_auth_address, Address},
			{radius_auth_port, RadiusAuthPort},
			{radius_acct_address, Address},
			{radius_acct_port, RadiusAcctPort},
			{diameter_acct_address, Address} | Config],
	ok = diameter:start_service(?MODULE, client_acct_service_opts(Config1)),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE, Address, DiameterAcctPort, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Info}
				when element(1, Info) == up ->
			Config1;
		_Other ->
			{skip, diameter_client_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = diameter:remove_transport(?MODULE, true),
	ok = ocs_test_lib:stop(),
	ok.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(TestCase, Config) when
		TestCase == diameter_scur ->
	Address = ?config(diameter_acct_address, Config),
	{ok, _} = ocs:add_client(Address, undefined, diameter, undefined, true),
	Config;
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) when
		TestCase == diameter_scur ->
	Address = ?config(diameter_acct_address, Config),
	ok = ocs:delete_client(Address);
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
			ipdr_log, get_range, get_last, auth_query, acct_query_radius,
			acct_query_diameter, abmf_log_event, abmf_query, binary_tree_before,
			binary_tree_after, binary_tree_backward, binary_tree_forward,
			binary_tree_last, binary_tree_first, binary_tree_half,
			diameter_scur].

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
	ok = ocs_log:acct_log(radius, Server, Type, ReqAttrs, undefined, undefined),
	End = erlang:system_time(?MILLISECOND),
	Fany = fun(E) when element(1, E) >= Start, element(1, E) =< End,
					element(3, E) == radius, element(4, E) == Node,
					element(5, E) == Server, element(6, E) == Type,
					element(7, E) == ReqAttrs ->
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
   [{userdata, [{doc, "Log a DIAMETER CCR/CCA event"}]}].

diameter_log_acct_event(_Config) ->
	Start = erlang:system_time(?MILLISECOND),
	Protocol = diameter,
	Node = node(),
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	RequestType = start,
	ok = ocs_log:acct_log(Protocol, Server, RequestType,
			#'3gpp_ro_CCR'{}, #'3gpp_ro_CCA'{}, undefined),
	End = erlang:system_time(?MILLISECOND),
	Fany = fun(E) when element(1, E) >= Start, element(1, E) =< End,
					element(3, E) == Protocol, element(4, E) == Node,
					element(5, E) == Server, element(6, E) == RequestType,
					is_record(element(7, E), '3gpp_ro_CCR'),
					is_record(element(8, E), '3gpp_ro_CCA') ->
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
				ok = ocs_log:acct_log(radius, Server,
						Type, Attrs2, undefined, undefined),
				F(F, N - 1)
	end,
	ok = Fill(Fill, NumItems),
	End = erlang:system_time(?MILLISECOND),
	ok = disk_log:sync(ocs_acct),
	Range = (End - Start),
	StartRange = Start + (Range div 3),
	EndRange = End - (Range div 3),
	Filename = "ipdr-" ++ ocs_log:iso8601(erlang:system_time(?MILLISECOND)),
	ok = ocs_log:ipdr_log(wlan, Filename, StartRange, EndRange),
	GetRangeResult = ocs_log:get_range(ocs_acct, StartRange, EndRange),
	Fstop = fun(E, Acc) when element(6, E) == stop ->
				Acc + 1;
			(_, Acc) ->
				Acc
	end,
	lists:foldl(Fstop, 0, GetRangeResult),
	{ok, IpdrLog} = disk_log:open([{name, make_ref()}, {file, Filename}]),
	Fchunk = fun(_F, {error, Reason}, _Acc) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}, Acc) ->
				F(F, disk_log:chunk(IpdrLog, Cont), Acc + length(Chunk));
			(_, eof, Acc) ->
				disk_log:close(IpdrLog),
				Acc
	end,
	Fchunk(Fchunk, disk_log:chunk(IpdrLog, start), 0) - 2.

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
						[{?AcctSessionId, integer_to_list(N)} | Attrs],
						undefined, undefined),
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
	MatchReq = [{?UserName, {exact, Username}},
			{?NasIdentifier, {exact, NasIdentifier}}],
	Fget = fun(_F, {eof, Events}, Acc) ->
				lists:flatten(lists:reverse([Events | Acc]));
			(F, {Cont, Events}, Acc) ->
				F(F, ocs_log:auth_query(Cont, Start, End, [accept],
						MatchReq, '_'), [Events | Acc])
	end,
	Events = Fget(Fget, ocs_log:auth_query(start, Start, End,
						[accept], MatchReq, '_'), []),
	3 = length(Events).

acct_query_radius() ->
   [{userdata, [{doc, "Get matching accounting log events"}]}].

acct_query_radius(_Config) ->
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
	ok = fill_acct(1000, radius),
	LogInfo = disk_log:info(ocs_acct),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{_, CurItems} = lists:keyfind(no_current_items, 1, LogInfo),
	{_, CurBytes} = lists:keyfind(no_current_bytes, 1, LogInfo),
	EventSize = CurBytes div CurItems,
	NumItems = (FileSize div EventSize) * 5,
	Start = erlang:system_time(?MILLISECOND),
	ok = fill_acct(NumItems, radius),
	ok = ocs_log:acct_log(radius, Server, stop, Attrs, undefined, undefined),
	ok = fill_acct(rand:uniform(2000), radius),
	ok = ocs_log:acct_log(radius, Server, stop, Attrs, undefined, undefined),
	ok = fill_acct(rand:uniform(2000), radius),
	ok = ocs_log:acct_log(radius, Server, stop, Attrs, undefined, undefined),
	ok = fill_acct(rand:uniform(2000), radius),
	End = erlang:system_time(?MILLISECOND),
	MatchReq = [{?UserName, {exact, Username}},
			{?NasIdentifier, {exact, NasIdentifier}}],
	Fget = fun(_F, {eof, Events}, Acc) ->
				lists:flatten(lists:reverse([Events | Acc]));
			(F, {Cont, Events}, Acc) ->
				F(F, ocs_log:acct_query(Cont, Start, End, [stop],
						MatchReq), [Events | Acc])
	end,
	Events = Fget(Fget, ocs_log:acct_query(start, Start, End,
						[stop], MatchReq), []),
	3 = length(Events).

acct_query_diameter() ->
   [{userdata, [{doc, "Get matching accounting log events for diameter"}]}].

acct_query_diameter(_Config) ->
	Server = {{0,0,0,0}, 1812},
	ok = fill_acct(1000, diameter),
	LogInfo = disk_log:info(ocs_acct),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{_, CurItems} = lists:keyfind(no_current_items, 1, LogInfo),
	{_, CurBytes} = lists:keyfind(no_current_bytes, 1, LogInfo),
	EventSize = CurBytes div CurItems,
	NumItems = (FileSize div EventSize) * 5,
	Start = erlang:system_time(?MILLISECOND),
	Sid = <<"10.170.6.80;1532594780;734917;4889089">>,
	OriginHost = <<"10.0.0.1">>,
	OriginRealm = <<"ap14.sigscale.net">>,
	EventTimeStamp = [ocs_log:date(erlang:system_time(milli_seconds))],
	Record = #'3gpp_ro_CCR'{'Session-Id' = Sid, 'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm, 'Event-Timestamp' = EventTimeStamp},
	ok = fill_acct(NumItems, diameter),
	ok = ocs_log:acct_log(diameter, Server, start, Record, undefined, undefined),
	ok = fill_acct(rand:uniform(2000), diameter),
	ok = ocs_log:acct_log(diameter, Server, interim, Record, undefined, undefined),
	ok = fill_acct(rand:uniform(2000), diameter),
	ok = ocs_log:acct_log(diameter, Server, stop, Record, undefined, undefined),
	ok = fill_acct(rand:uniform(2000), diameter),
	End = erlang:system_time(?MILLISECOND),
	MatchSpec = [{#'3gpp_ro_CCR'{'Session-Id' = '$1', _ = '_'}, [{'==', '$1', Sid}]}],
	Fget = fun(_F, {eof, Events}, Acc) ->
				lists:flatten(lists:reverse([Events | Acc]));
			(F, {Cont, Events}, Acc) ->
				F(F, ocs_log:acct_query(Cont, Start, End, diameter, '_',
						MatchSpec), [Events | Acc])
	end,
	Events = Fget(Fget, ocs_log:acct_query(start, Start, End, diameter, '_', MatchSpec), []),
	3 = length(Events).

binary_tree_half() ->
   [{userdata, [{doc, "When half of the log is used"}]}].

binary_tree_half(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	disk_log:truncate(ocs_acct),
	LogInfo = disk_log:info(ocs_acct),
	{size, {_FileSize, NumFiles}} = lists:keyfind(size, 1, LogInfo),
	File = NumFiles div 4,
	ok = fill_acct(File),
	LogInfo1 = disk_log:info(ocs_acct),
	{current_file, CurrentFile} = lists:keyfind(current_file, 1, LogInfo1),
	{ok, Cont} = disk_log:chunk_step(ocs_acct, start, CurrentFile - 1),
	Cont = ocs_log:btree_search(ocs_acct, erlang:system_time(milli_seconds)).

radius_log_acct_event() ->
   [{userdata, [{doc, "Log a RADIUS accounting event"}]}].

binary_tree_before() ->
   [{userdata, [{doc, "When `Start' is smaller and out of the log range"}]}].

binary_tree_before(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	ok = fill_acct(),
	start = ocs_log:btree_search(ocs_acct, 1).

binary_tree_after() ->
   [{userdata, [{doc, "When `Start' is bigger and out of log range"}]}].

binary_tree_after(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	ok = fill_acct(),
	LogInfo = disk_log:info(ocs_acct),
	{size, {_FileSize, NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{ok, Cont} = disk_log:chunk_step(ocs_acct, start, NumFiles - 1),
	Start = erlang:system_time(milli_seconds),
	Cont = ocs_log:btree_search(ocs_acct, Start).

binary_tree_backward() ->
   [{userdata, [{doc, "When `Start' is at first half of the log"}]}].

binary_tree_backward(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	ok = fill_acct(),
	LogInfo = disk_log:info(ocs_acct),
	{size, {_FileSize, NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{ok, Cont} = disk_log:chunk_step(ocs_acct, start, NumFiles div 4),
	{_, Events} = disk_log:chunk(ocs_acct, Cont),
	Event = lists:last(Events),
	Cont = ocs_log:btree_search(ocs_acct, element(1, Event)).

binary_tree_forward() ->
   [{userdata, [{doc, "When start is at second half of the log"}]}].

binary_tree_forward(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	ok = fill_acct(),
	LogInfo = disk_log:info(ocs_acct),
	{size, {_FileSize, NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{ok, Cont} = disk_log:chunk_step(ocs_acct, start, ((NumFiles div 4) * 3) + 1),
	{_, Events} = disk_log:chunk(ocs_acct, Cont),
	Event = lists:nth(length(Events) div 3, Events),
	Cont = ocs_log:btree_search(ocs_acct, element(1, Event)).

binary_tree_last() ->
   [{userdata, [{doc, "When start is at last file of the log"}]}].

binary_tree_last(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	ok = fill_acct(),
	LogInfo = disk_log:info(ocs_acct),
	{size, {_FileSize, NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{ok, Cont} = disk_log:chunk_step(ocs_acct, start, NumFiles - 1),
	{_, Events} = disk_log:chunk(ocs_acct, Cont),
	Event = lists:last(Events),
	Cont = ocs_log:btree_search(ocs_acct, element(1, Event)).

binary_tree_first() ->
   [{userdata, [{doc, "When start is in first chunck of the log"}]}].

binary_tree_first(_Config) ->
	ocs_log:acct_open(),
	disk_log:change_notify(ocs_acct, self(), true),
	ok = fill_acct(),
	LogInfo = disk_log:info(ocs_acct),
	{size, {_FileSize, NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{ok, Cont} = disk_log:chunk_step(ocs_acct, start, NumFiles),
	{_, Events} = disk_log:chunk(ocs_acct, Cont),
	Event = lists:last(Events),
	Cont = ocs_log:btree_search(ocs_acct, element(1, Event)).

abmf_log_event(_Config) ->
	ok = ocs_log:abmf_open(),
	Start = erlang:system_time(?MILLISECOND),
	Subscriber = list_to_binary(ocs:generate_identity()),
	Type = transfer,
	BucketId = integer_to_list(Start) ++ "-"
				++ integer_to_list(erlang:unique_integer([positive])),
	CurrentAmount = rand:uniform(100000000),
	Transfer = rand:uniform(50000),
	BucketAmount = Transfer,
	BeforeAmount = CurrentAmount,
	AfterAmount = CurrentAmount - Transfer,
	ProdId = ocs:generate_password(),
	ok = ocs_log:abmf_log(Type, Subscriber, BucketId, cents,
			ProdId, BucketAmount, BeforeAmount, AfterAmount,
			undefined, undefined, undefined, undefined, undefined,
			undefined, undefined),
	End = erlang:system_time(?MILLISECOND),
	Fany = fun({TS, _, _, T, Sub, BI, _Un, PI, BA, BeA, AA, _, _, _, _, _, _, _})
					when TS >= Start, TS =< End, Sub == Subscriber,
					T == Type, BI == BucketId, BA == BucketAmount,
					BeA == BeforeAmount, AA == AfterAmount, PI == ProdId ->
				true;
			(_) ->
				false
	end,
	Find = fun(_F, {error, Reason}) ->
				ct:fail(Reason);
			(F, {Cont, Chunk}) ->
				case lists:any(Fany, Chunk) of
					false ->
						F(F, disk_log:chunk(ocs_abmf, Cont));
					true ->
						true
				end;
			(_F, eof) ->
				false
	end,
	true = Find(Find, disk_log:chunk(ocs_abmf, start)).

abmf_query() ->
   [{userdata, [{doc, "Get matching accounting log events"}]}].

abmf_query(_Config) ->
	ok = ocs_log:abmf_open(),
	Subscriber = list_to_binary(ocs:generate_identity()),
	BucketId = integer_to_list(erlang:system_time(?MILLISECOND)) ++ "-"
				++ integer_to_list(erlang:unique_integer([positive])),
	ProdId = ocs:generate_password(),
	ok = fill_abmf(1000),
	LogInfo = disk_log:info(ocs_abmf),
	{_, {FileSize, _NumFiles}} = lists:keyfind(size, 1, LogInfo),
	{_, CurItems} = lists:keyfind(no_current_items, 1, LogInfo),
	{_, CurBytes} = lists:keyfind(no_current_bytes, 1, LogInfo),
	EventSize = CurBytes div CurItems,
	NumItems = (FileSize div EventSize) * 5,
	Start = erlang:system_time(?MILLISECOND),
	ok = fill_abmf(NumItems),
	C1= rand:uniform(100000000),
	Topup = rand:uniform(50000),
	C2 = C1 + Topup,
	ok = ocs_log:abmf_log(topup, Subscriber, BucketId, cents,
			ProdId, Topup, C1, C2, undefined, undefined, undefined,
			undefined, undefined, undefined, undefined),
	ok = fill_abmf(rand:uniform(2000)),
	Transfer = rand:uniform(50000),
	C3 = C2 - Transfer,
	ok = ocs_log:abmf_log(transfer, Subscriber, BucketId, cents,
			ProdId, Transfer, C2, C3, undefined, undefined, undefined,
			undefined, undefined, undefined, undefined),
	ok = fill_abmf(rand:uniform(2000)),
	Adjustment = rand:uniform(50000),
	ok = ocs_log:abmf_log(adjustment, Subscriber, BucketId, cents,
			ProdId, Transfer, C3, C3 - Adjustment, undefined, undefined,
			undefined, undefined, undefined, undefined, undefined),
	ok = fill_abmf(rand:uniform(2000)),
	End = erlang:system_time(?MILLISECOND),
	F1 = fun(St, En, Ty, Sub, BI, Un, PI) ->
		F2 = fun(_F2, {eof, Event}, Acc) ->
					lists:flatten(lists:reverse([Event | Acc]));
				(F2, {Cont, Events}, Acc) ->
					F2(F2, ocs_log:abmf_query(Cont, St, En, Ty, Sub,
							BI, Un, PI), [Events | Acc])
		end,
		F2(F2, ocs_log:abmf_query(start, St, En, Ty, Sub, BI, Un, PI), [])
	end,
	E1 =  F1(Start, End, topup, Subscriber, BucketId, cents, ProdId),
	E2 =  F1(Start, End, transfer, Subscriber, BucketId, cents, ProdId),
	E3 =  F1(Start, End, adjustment, Subscriber, BucketId, cents, ProdId),
	Events = E1 ++ E2 ++ E3,
	3 = length(Events).

diameter_scur() ->
	[{userdata, [{doc, "DIAMETER SCUR rated log event)"}]}].

diameter_scur(_Config) ->
	Start = erlang:system_time(?MILLISECOND),
	P1 = price(usage, octets, rand:uniform(10000000), rand:uniform(1000000)),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	Username = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_identity(),
	{ok, #service{}} = ocs:add_service(Username, Password, ProdRef, []),
	Balance = rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	RequestNum0 = 0,
	Answer0 = diameter_scur_start(SId,
			Username, RequestNum0, rand:uniform(Balance div 2)),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer0,
	RequestNum1 = RequestNum0 + 1,
	Answer1 = diameter_scur_interim(SId,
			Username, RequestNum1, rand:uniform(Balance div 2), 0),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer1,
	RequestNum2 = RequestNum1 + 1,
	Answer2 = diameter_scur_stop(SId, Username, RequestNum2, rand:uniform(Balance div 2)),
	#'3gpp_ro_CCA'{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Session-Id' = SessionId} = Answer2,
	End= erlang:system_time(?MILLISECOND),
	MatchSpec = [{#'3gpp_ro_CCR'{'Session-Id' = SessionId, _ = '_'}, []}],
	Fget = fun F({eof, Events}, Acc) ->
				lists:flatten(lists:reverse([Events | Acc]));
			F({Cont, Events}, Acc) ->
				F(ocs_log:acct_query(Cont, Start, End, diameter, '_',
						MatchSpec), [Events | Acc])
	end,
	[E1, E2, E3] = Fget(ocs_log:acct_query(start, Start, End, diameter, '_', MatchSpec), []),
	{_, _, diameter, _, _, start, #'3gpp_ro_CCR'{}, #'3gpp_ro_CCA'{}, undefined} = E1,
	{_, _, diameter, _, _, interim, #'3gpp_ro_CCR'{}, #'3gpp_ro_CCA'{}, undefined} = E2,
	{_, _, diameter, _, _, stop, #'3gpp_ro_CCR'{}, #'3gpp_ro_CCA'{}, [#rated{}]} = E3.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

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
			{?UserName, ocs:generate_identity()}, {?CallingStationId, ocs_test_lib:mac()},
			{?CalledStationId, ocs_test_lib:mac() ++ ":AP1"}, {?NasIdentifier, NasIdentifier},
			{?NasIpAddress, ClientAddress}],
	{Type, RespAttrs} = resp_attr(),
	ok = ocs_log:auth_log(radius, Server, Client, Type, ReqAttrs, RespAttrs),
	fill_auth(N - 1).

fill_acct() ->
	ok = fill_acct(10, diameter),
	receive
		{disk_log, _Node, ocs_acct, {wrap, 0}} ->
			fill_acct();
		{disk_log, _Node, ocs_acct, {wrap, N}} when N > 0 ->
			LogInfo = disk_log:info(ocs_acct),
			{items, Items} = lists:keyfind(items, 1, LogInfo),
			fill_acct(Items div 4, diameter);
		_Other ->
			fill_acct()
	after
		0 ->
			fill_acct()
	end.
fill_acct(N) ->
	ok = fill_acct(10, diameter),
	receive
		{disk_log, _Node, ocs_acct, {wrap, 0}} ->
			LogInfo = disk_log:info(ocs_acct),
			{current_file, File} = lists:keyfind(current_file, 1, LogInfo),
			case N > File of
				true ->
					fill_acct(N);
				false ->
					ok
			end;
		_Other ->
			fill_acct(N)
	after
		0 ->
			fill_acct(N)
	end.
fill_acct(0, _Protocal) ->
	ok;
fill_acct(N, Protocal) ->
	AcctOutputOctets = rand:uniform(100000),
	AcctInputOctets = rand:uniform(100000000),
	AcctSessionTime = rand:uniform(3600) + 100,
	UserName = ocs:generate_identity(),
	MSISDN = io_lib:fwrite("1416555~4.10.0b", [rand:uniform(1000) - 1]).
	IMSI = io_lib:fwrite("001001~9.10.0b", [rand:uniform(1000000000) - 1]).
	Server = {{0, 0, 0, 0}, 1812},
	I3 = rand:uniform(256) - 1,
	I4 = rand:uniform(254),
	ClientAddress = <<"10.0.0.1">>,
	NASn = integer_to_list((I3 bsl 8) + I4),
	NasIdentifier = "ap-" ++ NASn ++ ".sigscale.net",
	Type = case rand:uniform(3) of
		1 -> start;
		2 -> stop;
		3 -> interim
	end,
	case Protocal of
		radius ->
			Attrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
					{?UserName, ocs:generate_identity()}, {?CallingStationId, ocs_test_lib:mac()},
					{?CalledStationId, ocs_test_lib:mac() ++ ":AP1"}, {?NasIdentifier, NasIdentifier},
					{?NasIpAddress, ClientAddress}, {?AcctStatusType, rand:uniform(3)},
					{?AcctSessionTime, AcctSessionTime},
					{?AcctInputOctets, AcctInputOctets},
					{?AcctOutputOctets, AcctOutputOctets}],
			ok = ocs_log:acct_log(radius, Server, Type, Attrs, undefined, undefined),
			fill_acct(N - 1, radius);
		diameter ->
			ServiceContextId = <<"10.32251.3gpp.org">>,
			Sub1 = #'3gpp_ro_Subscription-Id'{'Subscription-Id-Type' = ?END_USER_E164,
					'Subscription-Id-Data' = list_to_binary(MSISDN)},
			Sub2 = #'3gpp_ro_Subscription-Id'{'Subscription-Id-Type' = ?END_USER_IMSI,
					'Subscription-Id-Data' = list_to_binary(IMSI)},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
					'CC-Time' = AcctSessionTime,
					'CC-Input-Octets' = AcctInputOctets,
					'CC-Output-Octets' = AcctOutputOctets}]},
			IMSInfo = #'3gpp_ro_IMS-Information'{'Calling-Party-Address' = ocs_test_lib:mac(),
					'Called-Party-Address' = ocs_test_lib:mac()},
			ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' = [IMSInfo]},
			Record = #'3gpp_ro_CCR'{'Origin-Host' = ClientAddress,
					'Service-Context-Id' = ServiceContextId,
					'Subscription-Id' = [Sub1, Sub2],
					'Multiple-Services-Credit-Control' = [MSCC],
					'Service-Information' = [ServiceInformation]},
			ok = ocs_log:acct_log(diameter, Server, Type, Record, undefined, undefined),
			fill_acct(N - 1, diameter)
	end.

fill_abmf(0) ->
	ok;
fill_abmf(N) ->
	Subscriber = list_to_binary(ocs:generate_identity()),
	BucketId = integer_to_list(erlang:system_time(?MILLISECOND)) ++ "-"
				++ integer_to_list(erlang:unique_integer([positive])),
	Type = case rand:uniform(3) of
		1 -> topup;
		2 -> transfer;
		3 -> adjustment
	end,
	Units = case rand:uniform(3) of
		1 -> cents;
		2 -> octets;
		3 -> seconds
	end,
	ProdId = ocs:generate_password(),
	CurrentAmount = rand:uniform(100000000),
	case Type of
		topup ->
			Topup = rand:uniform(50000),
			BucketAmount = Topup,
			BeforeAmount = CurrentAmount,
			AfterAmount = CurrentAmount + Topup,
			ok = ocs_log:abmf_log(Type, Subscriber, BucketId, Units, ProdId,
					BucketAmount, BeforeAmount, AfterAmount, undefined, undefined,
					undefined, undefined, undefined, undefined, undefined);
		transfer ->
			Transfer = rand:uniform(50000),
			BucketAmount = Transfer,
			BeforeAmount = CurrentAmount,
			AfterAmount = CurrentAmount - Transfer,
			ok = ocs_log:abmf_log(Type, Subscriber, BucketId, Units, ProdId,
					BucketAmount, BeforeAmount, AfterAmount, undefined, undefined,
					undefined, undefined, undefined, undefined, undefined);
		adjustment ->
			Adjustment = rand:uniform(50000),
			BucketAmount = Adjustment,
			BeforeAmount = CurrentAmount,
			AfterAmount = CurrentAmount - Adjustment,
			ok = ocs_log:abmf_log(Type, Subscriber, BucketId, Units, ProdId,
					BucketAmount, BeforeAmount, AfterAmount, undefined, undefined,
					undefined, undefined, undefined, undefined, undefined)
	end,
	fill_abmf(N - 1).

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

%% @hidden
price(Type, Units, Size, Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, units = Units,
			size = Size, amount = Amount}.

%% @hidden
bucket(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
		start_date = erlang:system_time(?MILLISECOND),
		end_date = erlang:system_time(?MILLISECOND) + 2592000000}.

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
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, Chars),
	ProdRef.

%% @hidden
add_bucket(ProdRef, Bucket) ->
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.

%% @hidden
diameter_scur_start(SId, Username, RequestNum, Requested) ->
	Subscription_Id = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Total-Octets' = [Requested]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = [RequestedUnits]},
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
			'User-Name' = [Username],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = [Subscription_Id],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.
	
%% @hidden
diameter_scur_interim(SId, Username, RequestNum, Used, Requested) ->
	Subscription_Id = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [Used]},
	RequestedUnits = #'3gpp_ro_Requested-Service-Unit' {
			'CC-Total-Octets' = [Requested]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits],
			'Requested-Service-Unit' = [RequestedUnits]},
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
			'User-Name' = [Username],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [Subscription_Id],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.
	
%% @hidden
diameter_scur_stop(SId, Username, RequestNum, Used) ->
	Subscription_Id = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = Username},
	UsedUnits = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [Used]},
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [UsedUnits]},
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
			'User-Name' = [Username],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Multiple-Services-Credit-Control' = [MultiServices_CC],
			'Subscription-Id' = [Subscription_Id],
			'Service-Information' = [ServiceInformation]},
	{ok, Answer} = diameter:call(?MODULE, cc_app_test, CC_CCR, []),
	Answer.

%% @hidden
client_acct_service_opts(Config) ->
	[{'Origin-Host', ?config(host, Config)},
			{'Origin-Realm', ?config(realm, Config)},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Client (Acct)"},
			{'Auth-Application-Id', [?RO_APPLICATION_ID]},
			{string_decode, false},
			{restrict_connections, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_client_cb}]},
			{application, [{alias, cc_app_test},
					{dictionary, diameter_gen_3gpp_ro_application},
					{module, diameter_test_client_cb}]}].

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans}, {transport_config,
			[{raddr, RemAddr}, {rport, RemPort},
			{reuseaddr, true}, {ip, LocalAddr}]}].

