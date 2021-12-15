%%% user_default.erl
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
%%%

-module(user_default).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% export the user_default public API
-export([help/0, ts/0, td/0, su/0]).
-export([di/0, di/1, di/2, dc/0]).
-export([ll/1, ll/2, ql/2, ql/3, ql/4]).

-include("ocs.hrl").
-include("ocs_log.hrl").
-include("ocs_eap_codec.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_gen_etsi.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_3gpp_gx_application.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include("diameter_gen_3gpp_s6a_application.hrl").
-include("diameter_gen_3gpp_s6b_application.hrl").
-include_lib("diameter/include/diameter.hrl").

-define(MAX_HEAP_SIZE, 1000000).

%%----------------------------------------------------------------------
%%  The user_default public API
%%----------------------------------------------------------------------

-spec help() -> true.
%% @doc Get help on shell local commands.
help() ->
	shell_default:help(),
	io:fwrite("** ocs commands ** \n"),
	io:fwrite("ts()            -- table sizes\n"),
	io:fwrite("td()            -- table distribution\n"),
	io:fwrite("su()            -- scheduler utilization\n"),
	io:fwrite("di()            -- diameter services info\n"),
	io:fwrite("di(Types)       -- diameter services info of types\n"),
	io:fwrite("di(acct, Types) -- diameter accounting services info\n"),
	io:fwrite("di(auth, Types) -- diameter authentication and authorization services info\n"),
	io:fwrite("dc()            -- diameter capabilities values\n"),
	io:fwrite("ll(acct)        -- last accounting log events\n"),
	io:fwrite("ll(acct, N)\n"),
	io:fwrite("ll(auth)        -- last authentication and authorization log events\n"),
	io:fwrite("ll(auth, N)\n"),
	io:fwrite("ql(acct, Match) -- query accounting log\n"),
	io:fwrite("ql(acct, Match, Start)\n"),
	io:fwrite("ql(acct, Match, Start, End)\n"),
	io:fwrite("ql(auth, Match) -- query authentication and authorization log\n"),
	io:fwrite("ql(auth, Match, Start)\n"),
	io:fwrite("ql(auth, Match, Start, End)\n"),
	true.

-spec ts() -> ok.
%% @doc Display the total number of records in ocs tables.
ts() ->
	case mnesia:system_info(is_running)  of
		yes ->
			ts00(tables(), 0, []);
		no ->
			io:fwrite("mnesia is not running\n");
		starting ->
			io:fwrite("mnesia is starting\n");
		stopping ->
			io:fwrite("mnesia is stopping\n")
	end.
%% @hidden
ts00([H | T], Max, Acc) ->
	NewMax = case length(atom_to_list(H)) of
		N when N > Max ->
			N;
		_ ->
			Max
	end,
	ts00(T, NewMax, [H | Acc]);
ts00([], Max, Acc) ->
	ts01(lists:reverse(Acc), Max, 0, []).
%% @hidden
ts01([H | T], NameLen, Max, Acc) ->
	Size = mnesia:table_info(H, size),
	NewMax = case length(integer_to_list(Size)) of
		N when N > Max ->
			N;
		_ ->
			Max
	end,
	ts01(T, NameLen, NewMax, [{H, Size} | Acc]);
ts01([], NameLen, Max, Acc) ->
	io:fwrite("Table sizes:\n"),
	ts02(lists:reverse(Acc), NameLen, Max).
%% @hidden
ts02([{Name, Size} | T], NameLen, SizeLen) ->
	io:fwrite("~*s: ~*b\n", [NameLen + 4, Name, SizeLen, Size]),
	ts02(T, NameLen, SizeLen);
ts02([], _, _) ->
	ok.

-spec td() -> ok.
%% @doc Display the current ocs table distribution.
td() ->
	Nodes = mnesia:system_info(db_nodes),
	Running = mnesia:system_info(running_db_nodes),
	io:fwrite("Table distribution:\n"),
	io:fwrite("    Nodes:\n"),
	io:fwrite("        running:  ~s\n", [snodes(Running)]),
	io:fwrite("        stopped:  ~s\n", [snodes(Nodes -- Running)]),
	io:fwrite("    Tables:\n"),
	td0(tables()).
%% @hidden
td0([H | T]) ->
	io:fwrite("        ~s:\n", [H]),
	case mnesia:table_info(H, disc_copies) of
		[] ->
			ok;
		Nodes1 ->
			io:fwrite("            disc_copies:  ~s\n", [snodes(Nodes1)])
	end,
	case mnesia:table_info(H, ram_copies) of
		[] ->
			ok;
		Nodes2 ->
			io:fwrite("             ram_copies:  ~s\n", [snodes(Nodes2)])
	end,
	td0(T);
td0([]) ->
	ok.

-spec su() -> ok.
%% @doc Display scheduler utilization.
su() ->
	case ocs:statistics(scheduler_utilization) of
		{ok, {Etag, Interval, Report}} ->
			io:fwrite("Scheduler utilization:\n"),
			su0(Report, Etag, Interval);
		{error, Reason} ->
			exit(Reason)
	end.
%% @hidden
su0([{Scheduler, Utilization} | T], Etag, Interval) ->
	io:fwrite("~*b: ~3b%\n", [5, Scheduler, Utilization]),
	su0(T, Etag, Interval);
su0([], Etag, Interval) ->
	{TS, _} = string:take(Etag, lists:seq($0, $9)),
	Next = (list_to_integer(TS) + Interval),
	Remaining = Next - erlang:system_time(millisecond),
	Seconds = case {Remaining div 1000, Remaining rem 1000} of
		{0, _} ->
			1;
		{N, R} when R < 500 ->
			N;
		{N, _} ->
			N + 1
	end,
	io:fwrite("Next report available in ~b seconds.\n", [Seconds]).

-spec di() -> Result
	when
		Result :: [ServiceResult],
		ServiceResult :: {Service, [term()]},
		Service :: term().
%% @doc Get information on running diameter services.
di() ->
	diameter_service_info(diameter:services(), []).

-spec di(Info) -> Result
	when
		Info :: [Item],
		Item :: peer | applications | capabilities | transport
				| connections | statistics | Capability,
		Capability :: 'Origin-Host' | 'Origin-Realm' | 'Vendor-Id'
				| 'Product-Name' | 'Origin-State-Id' | 'Host-IP-Address'
				| 'Supported-Vendor' | 'Auth-Application-Id'
				| 'Inband-Security-Id' | 'Acct-Application-Id'
				| 'Vendor-Specific-Application-Id' | 'Firmware-Revision',
		Result :: [ServiceResult],
		ServiceResult :: {Service, [term()]},
		Service :: term().
%% @doc Get information on running diameter services.
di(Info) ->
	diameter_service_info(diameter:services(), Info).

-spec di(ServiceType, Info) -> Result
	when
		ServiceType :: auth | acct,
		Info :: [Item],
		Item :: peer | applications | capabilities | transport
				| connections | statistics | Capability,
		Capability :: 'Origin-Host' | 'Origin-Realm' | 'Vendor-Id'
				| 'Product-Name' | 'Origin-State-Id' | 'Host-IP-Address'
				| 'Supported-Vendor' | 'Auth-Application-Id'
				| 'Inband-Security-Id' | 'Acct-Application-Id'
				| 'Vendor-Specific-Application-Id' | 'Firmware-Revision',
		Result :: term() | {error, Reason},
		Reason :: unknown_service.
%% @doc Get information on running diameter services.
di(auth, Info) ->
	F = fun({ocs_diameter_auth_service, _, _}) ->
				true;
			(_) ->
				false
	end,
	AuthServices = lists:filter(F, diameter:services()),
	diameter_service_info(AuthServices, Info);
di(acct, Info) ->
	F = fun({ocs_diameter_acct_service, _, _}) ->
				true;
			(_) ->
				false
	end,
	AcctServices = lists:filter(F, diameter:services()),
	diameter_service_info(AcctServices, Info).

-spec dc() -> Result
	when
		Result :: term().
%% @doc Get diameter capability values.
dc() ->
	Info = ['Origin-Host', 'Origin-Realm', 'Vendor-Id',
			'Product-Name', 'Origin-State-Id', 'Host-IP-Address',
			'Supported-Vendor', 'Auth-Application-Id',
			'Inband-Security-Id', 'Acct-Application-Id',
			'Vendor-Specific-Application-Id',
			'Firmware-Revision'],
	diameter_service_info(diameter:services(), Info).

-spec ll(Log) -> Events
	when
		Log :: acct | auth,
		Events :: [ocs_log:acct_event()] | [ocs_log:auth_event()].
%% @doc Get the last five events written to log.
ll(acct = _Log) ->
	case ocs_log:last(ocs_acct, 5) of
		{Count, Events} when is_integer(Count) ->
			Events;
		{error, Reason} ->
			exit(Reason)
	end;
ll(auth = _Log) ->
	case ocs_log:last(ocs_auth, 5) of
		{Count, Events} when is_integer(Count) ->
			Events;
		{error, Reason} ->
			exit(Reason)
	end.

-spec ll(Log, N) -> Events
	when
		Log :: acct | auth,
		N :: pos_integer(),
		Events :: [ocs_log:acct_event()] | [ocs_log:auth_event()].
%% @doc Get the last `N' events written to log.
ll(acct = _Log, N) when is_integer(N), N > 0 ->
	set_max_heap(),
	case ocs_log:last(ocs_acct, N) of
		{Count, Events} when is_integer(Count) ->
			Events;
		{error, Reason} ->
			exit(Reason)
	end;
ll(auth = _Log, N) when is_integer(N), N > 0 ->
	set_max_heap(),
	case ocs_log:last(ocs_auth, N) of
		{Count, Events} when is_integer(Count) ->
			Events;
		{error, Reason} ->
			exit(Reason)
	end.

-spec ql(Log, Match) -> Events
	when
		Log :: acct | auth,
		Match :: DiameterMatchSpec | RatedMatchSpec,
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{},
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
		Events :: [ocs_log:acct_event()].
%% @doc Query diameter logs.
%%
%% 	Start will be minus one hour from now.
%%
ql(acct = _Log, {MatchHead, MatchConditions} = Match)
		when is_list(MatchConditions),
		(is_record(MatchHead, '3gpp_ro_CCR')
		or is_record(MatchHead, '3gpp_ro_CCA')
		or is_record(MatchHead, rated)) ->
	End = erlang:universaltime(),
	EndS = calendar:datetime_to_gregorian_seconds(End),
	Start = calendar:gregorian_seconds_to_datetime(EndS - 3600),
	query_acct_log(Match, Start, End).

-spec ql(Log, Match, Start) -> Events
	when
		Log :: acct | auth,
		Match :: DiameterMatchSpec | RatedMatchSpec,
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{},
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
		Start :: calendar:datetime(),
		Events :: [ocs_log:acct_event()].
%% @doc Query diameter logs.
%%
%% 	End time will be now.
%%
ql(acct = _Log, {MatchHead, MatchConditions} = Match,
		{{_, _, _}, {_, _, _}} = Start)
		when is_list(MatchConditions),
		(is_record(MatchHead, '3gpp_ro_CCR')
		or is_record(MatchHead, '3gpp_ro_CCA')
		or is_record(MatchHead, rated)) ->
	End = erlang:universaltime(),
	query_acct_log(Match, Start, End).

-spec ql(Log, Match, Start, End) -> Events
	when
		Log :: acct | auth,
		Match :: DiameterMatchSpec | RatedMatchSpec,
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{},
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
		Start :: calendar:datetime(),
		End :: calendar:datetime(),
		Events :: [ocs_log:acct_event()].
%% @doc Query diameter logs.
ql(acct = _Log, {MatchHead, MatchConditions} = Match,
		{{_, _, _}, {_, _, _}} = Start,
		{{_, _, _}, {_, _, _}} = End)
		when is_list(MatchConditions),
		(is_record(MatchHead, '3gpp_ro_CCR')
		or is_record(MatchHead, '3gpp_ro_CCA')
		or is_record(MatchHead, rated)) ->
	End = erlang:universaltime(),
	query_acct_log(Match, Start, End).

%%----------------------------------------------------------------------
%%  The user_default private API
%%----------------------------------------------------------------------

-spec diameter_service_info(Services, Info) -> Result
	when
		Services :: [term()],
		Info :: [Item],
		Item :: peer | applications | capabilities | transport
				| connections | statistics | Capability,
		Capability :: 'Origin-Host' | 'Origin-Realm' | 'Vendor-Id'
				| 'Product-Name' | 'Origin-State-Id' | 'Host-IP-Address'
				| 'Supported-Vendor' | 'Auth-Application-Id'
				| 'Inband-Security-Id' | 'Acct-Application-Id'
				| 'Vendor-Specific-Application-Id' | 'Firmware-Revision',
		Result :: [ServiceResult],
		ServiceResult :: {Service, [term()]},
		Service :: term().
%% @hidden
diameter_service_info(Services, []) ->
	Info = [peer, applications, capabilities,
			transport, connections, statistics],
	diameter_service_info(Services, Info, []);
diameter_service_info(Services, Info) ->
	diameter_service_info(Services, Info, []).
%% @hidden
diameter_service_info([Service | T], Info, Acc) ->
	diameter_service_info(T, Info,
			[{Service, diameter:service_info(Service, Info)} | Acc]);
diameter_service_info([], _Info, Acc) ->
	lists:reverse(Acc).

-spec query_acct_log(Match, Start, End) -> Events
	when
		Match :: DiameterMatchSpec | RatedMatchSpec,
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{},
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Events :: [ocs_log:acct_event()].
%% @hidden
query_acct_log(Match, Start, End) ->
	set_max_heap(),
	query_acct_log(start, Start, End, Match, []).
%% @hidden
query_acct_log(eof, _, _, _, Acc) ->
	lists:flatten(lists:reverse(Acc));
query_acct_log(Context1, Start, End, Match, Acc) ->
	case ocs_log:acct_query(Context1, Start, End, diameter, '_', [Match]) of
		{error, Reason} ->
			exit(Reason);
		{Context2, []} ->
			query_acct_log(Context2, Start, End, Match, Acc);
		{Context2, Events} ->
			query_acct_log(Context2, Start, End, Match, [Events | Acc])
	end.

%% @hidden
set_max_heap() ->
	MaxHeapSize = #{error_logger => true,
			kill => true, size => ?MAX_HEAP_SIZE},
	case erlang:process_info(self(), max_heap_size) of
		{max_heap_size, #{size := 0}} ->
			erlang:process_flag(max_heap_size, MaxHeapSize);
		{max_heap_size, _} ->
			ok
	end.

%% @hidden
tables() ->
	Tables = [offer, product, service, bucket, resource, client, session],
	Other = mnesia:system_info(tables)
			-- [schema, httpd_user, httpd_group | Tables],
	tables(Other, lists:reverse(Tables)).
%% @hidden
tables([H | T], Acc) ->
	case mnesia:table_info(H, record_name) of
		gtt ->
			tables(T, [H | Acc]);
		_ ->
			tables(T, Acc)
	end;
tables([], Acc) ->
	lists:reverse(Acc).

%% @hidden
snodes(Nodes) ->
	snodes(Nodes, []).
%% @hidden
snodes([H | T], []) ->
	snodes(T, [atom_to_list(H)]);
snodes([H | T], Acc) ->
	snodes(T, [atom_to_list(H), ", " | Acc]);
snodes([], Acc) ->
	lists:reverse(Acc).

