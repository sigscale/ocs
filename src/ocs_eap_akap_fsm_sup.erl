%%% ocs_eap_akap_fsm_sup.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
-module(ocs_eap_akap_fsm_sup).
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-behaviour(supervisor).

%% export the call back needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor call back
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
init(Args) ->
	SupFlags = #{auto_shutdown => any_significant},
	ChildSpecs = [fsm(ocs_eap_akap_fsm, [self() | Args]),
			fsm(ocs_eap_aka_auc_fsm, Args)],
	{ok, {SupFlags, ChildSpecs}}.

%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/gen_fsm. gen_fsm} behaviour.
%% @private
%%
fsm(StartMod, Args) ->
	StartArgs = [StartMod, Args, []],
	StartFunc = {gen_fsm, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			restart => temporary, significant => true,
			modules => [StartMod]}.

