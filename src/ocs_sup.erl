%%% ocs_sup.erl
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
%%% @docfile "{@docsrc supervision.edoc}"
%%%
-module(ocs_sup).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(PG,
				StartMod = pg,
				StartFunc = {StartMod, start_link, [pg_scope_ocs]},
				[{StartMod, StartFunc, permanent, 4000, worker, [StartMod]}]).
	-else.
		-define(PG, []).
	-endif.
-else.
	-define(PG, []).
-endif.

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()},
			[supervisor:child_spec()]}} | ignore.
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([LogRotateTime, LogRotateInterval] = _Args) ->
	ChildSpecs = pg() ++ [supervisor(ocs_radius_acct_top_sup, []),
			supervisor(ocs_radius_auth_sup, []),
			supervisor(ocs_diameter_auth_sup, []),
			supervisor(ocs_diameter_acct_top_sup, []),
			log_server(ocs_log_rotate_server,
					[voip, LogRotateTime, LogRotateInterval]),
			log_server(ocs_log_rotate_server,
					[wlan, LogRotateTime, LogRotateInterval]),
			supervisor(ocs_statistics_sup,
					ocs_statistics_sup, []),
			supervisor(ocs_rest_pagination_sup,
					ocs_rest_pagination_sup, []),
			supervisor(ocs_rest_hub_sup,
					ocs_rest_hub_sup, []),
			server(ocs_server, [self()]),
			event(ocs_event),
			supervisor(ocs_event_log_sup,
					ocs_event_log_sup, []),
			event(ocs_event_log)],
	{ok, {{one_for_one, 10, 60}, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
pg() ->
	?PG.

-spec supervisor(StartMod, Args) -> Result
	when
		StartMod :: atom(),
		Args :: [term()],
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/supervisor. supervisor} behaviour.
%% @private
%%
supervisor(StartMod, Args) ->
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	{StartMod, StartFunc, permanent, infinity, supervisor, [StartMod]}.

-spec supervisor(StartMod, RegName, Args) -> Result
	when
		StartMod :: atom(),
		RegName :: atom(),
		Args :: [term()],
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/supervisor. supervisor} behaviour
%% 	with registered name.
%% @private
%%
supervisor(StartMod, RegName, Args) ->
	StartArgs = [{local, RegName}, StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	{StartMod, StartFunc, permanent, infinity, supervisor, [StartMod]}.

-spec server(StartMod, Args) -> Result
	when
		StartMod :: atom(),
		Args :: [term()],
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/gen_server. gen_server} behaviour.
%% @private
%%
server(StartMod, Args) ->
	StartArgs = [{local, ocs}, StartMod, Args, []],
	StartFunc = {gen_server, start_link, StartArgs},
	{StartMod, StartFunc, permanent, 4000, worker, [StartMod]}.

%% @hidden
log_server(StartMod, [Type | _] = Args) ->
	StartArgs = [StartMod, Args, []],
	StartFunc = {gen_server, start_link, StartArgs},
	{{StartMod, Type},  StartFunc, permanent, 4000, worker, [StartMod]}.

-spec event(StartMod) -> Result
	when
		StartMod :: atom(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%%		{@link //stdlib/gen_event. gen_event} behaviour.
%% @private
%%
event(StartMod) ->
	StartArgs = [{local, StartMod}],
	StartFunc = {gen_event, start_link, StartArgs},
	{StartMod, StartFunc, permanent, 4000, worker, [StartMod]}.

