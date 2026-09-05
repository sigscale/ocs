%%% ocs_event_log_sup.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022 - 2026 SigScale Global Inc.
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
-module(ocs_event_log_sup).
-copyright('Copyright (c) 2022 - 2026 SigScale Global Inc.').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, {SupFlags, [ChildSpec]}} | ignore,
		SupFlags :: supervisor:sup_flags(),
		ChildSpec :: supervisor:child_spec().
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([] = _Args) ->
	ChildSpecs = [fsm(ocs_event_log_fsm)],
	SupFlags = #{strategy => simple_one_for_one,
			intensity => 10, period => 60},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec fsm(StartMod) -> Result
	when
		StartMod :: atom(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/gen_statem. gen_statem} behaviour.
%% @private
%%
fsm(StartMod) ->
	StartFunc = {StartMod, start_link, []},
	#{id => StartMod, start => StartFunc,
			shutdown => 4000, modules => [StartMod]}.

