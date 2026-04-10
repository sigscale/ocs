%%% ocs_log_rotate_sup.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2026 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
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
-module(ocs_log_rotate_sup).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(supervisor).

%% export the call back needed for supervisor behaviour
-export([init/1]).

-type registered_name() :: {local, Name :: atom()}
		| {global, Name :: atom()}.

%%----------------------------------------------------------------------
%%  The supervisor callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [LogName | Options],
		LogName :: registered_name(),
		Options :: [tuple()],
		Result :: {ok, {SupFlags, [ChildSpec]}} | ignore,
		SupFlags :: supervisor:sup_flags(),
		ChildSpec :: supervisor:child_spec().
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init(_Args) ->
	ChildSpecs = [server(ocs_log_rotate_server)],
	SupFlags = #{strategy => simple_one_for_one},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec server(StartMod) -> Result
	when
		StartMod :: atom(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link gen_server. gen_server} behaviour.
%% @private
server(StartMod) ->
	StartArgs = [StartMod],
	StartFunc = {gen_server, start_link, StartArgs},
	#{id => StartMod, start => StartFunc, modules => [StartMod]}.

