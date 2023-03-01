%%% user_default.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2022 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the"License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an"AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This module implements short form commands for the Erlang shell.
%%%
%%% 	The functions in this module are available when called without
%%% 	a module name in an interactive shell. These are meant to be used
%%% 	by operations support staff to quickly get status information
%%% 	about a running system.
%%%
%%% @see //stdlib/shell_default.
%%%
-module(user_default).
-copyright('Copyright (c) 2016 - 2022 SigScale Global Inc.').

%% export the user_default public API
-export([help/0, ts/0, td/0, su/0, up/0]).
-export([di/0, di/1, di/2, dc/0]).
-export([ll/1, ll/2, ql/2, ql/3, ql/4]).
-export([service_name/1]).

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

-spec up() -> ok.
%% @doc Display system uptime.
up() ->
   CurrentTime = erlang:system_time(second),
   StartTime = erlang:convert_time_unit(erlang:system_info(start_time)
         +  erlang:time_offset(), native, second),
   {D, {H, M, S}} = calendar:seconds_to_daystime(CurrentTime - StartTime),
   io:fwrite("Uptime: ~b days, ~b hours, ~b minutes, ~b seconds\n", [D, M, H, S]).

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
		DiameterMatchHead :: ocs_log:acct_request() | ocs_log:acct_response(),
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
		or is_record(MatchHead, '3gpp_gx_CCR')
		or is_record(MatchHead, '3gpp_gx_CCA')
		or is_record(MatchHead, '3gpp_gx_RAR')
		or is_record(MatchHead, '3gpp_gx_RAA')
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
		DiameterMatchHead :: ocs_log:acct_request() | ocs_log:acct_response(),
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
		or is_record(MatchHead, '3gpp_ro_RAR')
		or is_record(MatchHead, '3gpp_ro_RAA')
		or is_record(MatchHead, '3gpp_gx_CCR')
		or is_record(MatchHead, '3gpp_gx_CCA')
		or is_record(MatchHead, '3gpp_gx_RAR')
		or is_record(MatchHead, '3gpp_gx_RAA')
		or is_record(MatchHead, rated)) ->
	End = erlang:universaltime(),
	query_acct_log(Match, Start, End).

-spec ql(Log, Match, Start, End) -> Events
	when
		Log :: acct | auth,
		Match :: DiameterMatchSpec | RatedMatchSpec,
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: ocs_log:acct_request() | ocs_log:acct_response(),
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
		or is_record(MatchHead, '3gpp_ro_RAR')
		or is_record(MatchHead, '3gpp_ro_RAA')
		or is_record(MatchHead, '3gpp_gx_CCR')
		or is_record(MatchHead, '3gpp_gx_CCA')
		or is_record(MatchHead, '3gpp_gx_RAR')
		or is_record(MatchHead, '3gpp_gx_RAA')
		or is_record(MatchHead, rated)) ->
	End = erlang:universaltime(),
	query_acct_log(Match, Start, End).

%%----------------------------------------------------------------------
%%  the user_default private api
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
diameter_service_info(Services, statistics) ->
	ServiceStats = [{Service, diameter:service_info(Service, statistics)} || Service <- Services],
	service_name(ServiceStats);
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
		DiameterMatchHead :: ocs_log:acct_request() | ocs_log:acct_response(),
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

%% @doc Parse service name statistics.
%% @hidden
service_name([{ServiceName, PeerStat} | T]) ->
	io:fwrite("~w:~n", [ServiceName]),
	peer_stat(PeerStat),
	service_name(T);
service_name([_H | T]) ->
	service_name(T);
service_name([]) ->
	ok.

%% @doc Parse peer statistics.
%% @hidden
peer_stat([{_PeerFsm, PeerStats} | T]) ->
	peer_stat1(PeerStats, #{}),
	peer_stat(T);
peer_stat([]) ->
	ok.
%% @hidden
peer_stat1([{{{Application, CommandCode, RequestFlag}, _Direction, {'Result-Code', ResultCode}}, Count} | T], Acc) ->
	NewAcc = case maps:find(Application, Acc) of
		{ok, CommandMap} ->
			case maps:find({CommandCode, 'Result-Code', ResultCode}, CommandMap) of
				{ok, Value} ->
					Acc#{Application => CommandMap#{{CommandCode, RequestFlag, 'Result-Code', ResultCode} => Value + Count}};
				error->
					Acc#{Application => CommandMap#{{CommandCode, RequestFlag, 'Result-Code', ResultCode} => Count}}
			end;
		error->
			Acc#{Application => #{{CommandCode, RequestFlag, 'Result-Code', ResultCode} => Count}}
	end,
	peer_stat1(T, NewAcc);
peer_stat1([{{{Application, CommandCode, RequestFlag}, _Direction, error}, Count} | T], Acc) ->
	NewAcc = case maps:find(Application, Acc) of
		{ok, CommandMap} ->
			case maps:find({CommandCode, RequestFlag, error}, CommandMap) of
				{ok, Value} ->
					Acc#{Application => CommandMap#{{CommandCode, RequestFlag, error} => Value + Count}};
				error->
					Acc#{Application => CommandMap#{{CommandCode, RequestFlag, error} => Count}}
			end;
		error->
			Acc#{Application => #{{CommandCode, RequestFlag, error} => Count}}
	end,
	peer_stat1(T, NewAcc);
peer_stat1([{{{Application, CommandCode, RequestFlag}, _Direction}, Count} | T], Acc) ->
	NewAcc = case maps:find(Application, Acc) of
		{ok, CommandMap} ->
			case maps:find({CommandCode, RequestFlag}, CommandMap) of
				{ok, Value} ->
					Acc#{Application => CommandMap#{{CommandCode, RequestFlag} => Value + Count}};
				error->
					Acc#{Application => CommandMap#{{CommandCode, RequestFlag} => Count}}
			end;
		error->
			Acc#{Application => #{{CommandCode, RequestFlag} => Count}}

	end,
	peer_stat1(T, NewAcc);
peer_stat1([], Acc) ->
	F2 = fun(Command, Count, _Acc) ->
				dia_count(Command, Count)
	end,
	F1 = fun(Application, CommandMap, _Acc) ->
				dia_application(Application),
				maps:fold(F2, [], CommandMap)
	end,
	maps:fold(F1, [], Acc).

-spec dia_application(Application) -> ok
	when
		Application :: non_neg_integer().
%% @doc Print the Application name header.
dia_application(0) ->
	io:fwrite("    Base: ~n");
dia_application(1) ->
	io:fwrite("    Nas: ~n");
dia_application(5) ->
	io:fwrite("    EAP: ~n");
dia_application(16777250) ->
	io:fwrite("    STa: ~n");
dia_application(16777264) ->
	io:fwrite("    SWm: ~n");
dia_application(16777265) ->
	io:fwrite("    SWx: ~n");
dia_application(16777251) ->
	io:fwrite("    S6a: ~n");
dia_application(16777272) ->
	io:fwrite("    S6b: ~n");
dia_application(4) ->
	io:fwrite("    Ro: ~n");
dia_application(16777238) ->
	io:fwrite("    Gx: ~n").

-spec dia_count(Command, Count) -> ok
	when
		Command :: tuple(),
		Count :: non_neg_integer().
%% @doc Print the command name and count.
dia_count({257, 1}, Count) ->
	io:fwrite("        CER: ~b~n", [Count]);
dia_count({257, 0}, Count) ->
	io:fwrite("        CEA: ~b~n", [Count]);
dia_count({280, 1}, Count) ->
	io:fwrite("        DWR: ~b~n", [Count]);
dia_count({280, 0}, Count) ->
	io:fwrite("        DWA: ~b~n", [Count]);
dia_count({271, 1}, Count) ->
	io:fwrite("        ACR: ~b~n", [Count]);
dia_count({271, 0}, Count) ->
	io:fwrite("        ACA: ~b~n", [Count]);
dia_count({282, 1}, Count) ->
	io:fwrite("        DPR: ~b~n", [Count]);
dia_count({282, 0}, Count) ->
	io:fwrite("        DPA: ~b~n", [Count]);
dia_count({258, 1}, Count) ->
	io:fwrite("        RAR: ~b~n", [Count]);
dia_count({258, 0}, Count) ->
	io:fwrite("        RAA: ~b~n", [Count]);
dia_count({274, 1}, Count) ->
	io:fwrite("        ASR: ~b~n", [Count]);
dia_count({274, 0}, Count) ->
	io:fwrite("        ASA: ~b~n", [Count]);
dia_count({275, 1}, Count) ->
	io:fwrite("        STR: ~b~n", [Count]);
dia_count({275, 0}, Count) ->
	io:fwrite("        STA: ~b~n", [Count]);
dia_count({272, 1}, Count) ->
	io:fwrite("        CCR: ~b~n", [Count]);
dia_count({272, 0}, Count) ->
	io:fwrite("        CCA: ~b~n", [Count]);
dia_count({265, 1}, Count) ->
        io:fwrite("        AAR: ~b~n", [Count]);
dia_count({265, 0}, Count) ->
        io:fwrite("        AAA: ~b~n", [Count]);
dia_count({268, 1}, Count) ->
        io:fwrite("        DER: ~b~n", [Count]);
dia_count({268, 0}, Count) ->
        io:fwrite("        DEA: ~b~n", [Count]);
dia_count({301, 1}, Count) ->
        io:fwrite("        SAR: ~b~n", [Count]);
dia_count({301, 0}, Count) ->
        io:fwrite("        SAA: ~b~n", [Count]);
dia_count({303, 1}, Count) ->
        io:fwrite("        MAR: ~b~n", [Count]);
dia_count({303, 0}, Count) ->
        io:fwrite("        MAA: ~b~n", [Count]);
dia_count({304, 1}, Count) ->
        io:fwrite("        RTR: ~b~n", [Count]);
dia_count({304, 0}, Count) ->
        io:fwrite("        RTA: ~b~n", [Count]);
dia_count({316, 1}, Count) ->
        io:fwrite("        ULR: ~b~n", [Count]);
dia_count({316, 0}, Count) ->
        io:fwrite("        ULA: ~b~n", [Count]);
dia_count({318, 1}, Count) ->
        io:fwrite("        AIR: ~b~n", [Count]);
dia_count({318, 0}, Count) ->
        io:fwrite("        AIA: ~b~n", [Count]);
dia_count({321, 1}, Count) ->
        io:fwrite("        PUR: ~b~n", [Count]);
dia_count({321, 0}, Count) ->
        io:fwrite("        PUA: ~b~n", [Count]);
dia_count({257, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        CEA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({280, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        DWA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({271, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        ACA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({282, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        DPA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({258, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        RAA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({274, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        ASA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({275, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        STA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({272, 0, 'Result-Code', ResultCode}, Count) ->
	io:fwrite("        CCA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({268, 0, 'Result-Code', ResultCode}, Count) ->
   io:fwrite("        DEA ~w ~b: ~b~n", ['Result-Code', ResultCode, Count]);
dia_count({257, 1, error}, Count) ->
	io:fwrite("        CER error: ~b~n", [Count]);
dia_count({257, 0, error}, Count) ->
	io:fwrite("        CEA error: ~b~n", [Count]);
dia_count({280, 1, error}, Count) ->
	io:fwrite("        DWR error: ~b~n", [Count]);
dia_count({280, 0, error}, Count) ->
	io:fwrite("        DWA error: ~b~n", [Count]);
dia_count({271, 1, error}, Count) ->
	io:fwrite("        ACR error: ~b~n", [Count]);
dia_count({271, 0, error}, Count) ->
	io:fwrite("        ACA error: ~b~n", [Count]);
dia_count({282, 1, error}, Count) ->
	io:fwrite("        DPR error: ~b~n", [Count]);
dia_count({282, 0, error}, Count) ->
	io:fwrite("        DPA error: ~b~n", [Count]);
dia_count({258, 1, error}, Count) ->
	io:fwrite("        RAR error: ~b~n", [Count]);
dia_count({258, 0, error}, Count) ->
	io:fwrite("        RAA error: ~b~n", [Count]);
dia_count({274, 1, error}, Count) ->
	io:fwrite("        ASR error: ~b~n", [Count]);
dia_count({274, 0, error}, Count) ->
	io:fwrite("        ASA error: ~b~n", [Count]);
dia_count({275, 1, error}, Count) ->
	io:fwrite("        STR error: ~b~n", [Count]);
dia_count({275, 0, error}, Count) ->
	io:fwrite("        STA error: ~b~n", [Count]);
dia_count({272, 1, error}, Count) ->
	io:fwrite("        CCR error: ~b~n", [Count]);
dia_count({272, 0, error}, Count) ->
	io:fwrite("        CCA error: ~b~n", [Count]).

