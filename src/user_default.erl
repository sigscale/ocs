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
-export([help/0, di/0, di/1, di/2, dc/0]).
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
	io:format("** ocs commands ** \n"),
	io:format("di()            -- diameter services info\n"),
	io:format("di(Types)       -- diameter services info of types\n"),
	io:format("di(acct, Types) -- diameter accounting services info\n"),
	io:format("di(auth, Types) -- diameter authentication and authorization services info\n"),
	io:format("dc()            -- diameter capabilities values\n"),
	io:format("ll(acct)        -- last accounting log events\n"),
	io:format("ll(acct, N)\n"),
	io:format("ll(auth)        -- last authentication and authorization log events\n"),
	io:format("ll(auth, N)\n"),
	io:format("ql(acct, Match) -- query accounting log\n"),
	io:format("ql(acct, Match, Start)\n"),
	io:format("ql(acct, Match, Start, End)\n"),
	io:format("ql(auth, Match) -- query authentication and authorization log\n"),
	io:format("ql(auth, Match, Start)\n"),
	io:format("ql(auth, Match, Start, End)\n"),
	true.

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

