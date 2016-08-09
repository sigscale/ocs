%%% ocs_eap_server_sup.erl
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
%%% @docfile "{@docsrc supervision.edoc}"
%%%
-module(ocs_eap_server_sup).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(supervisor).

%% export the call back needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor call back
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	Result :: {ok,{{RestartStrategy :: one_for_all | one_for_one
		| rest_for_one | simple_one_for_one,
		MaxR :: non_neg_integer(), MaxT :: pos_integer()},
		[ChildSpec :: supervisor:child_spec()]}} | ignore.
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([Module, Port, Address]) ->
	ChildSpecs = [server(Module, Port, Address)],
	{ok, {{one_for_all, 10, 3600}, ChildSpecs}}.

%% @hidden
server(Module, Port, Address) ->
	StartMod = ocs_eap_server,
	StartArgs = [StartMod, [self(), Module, Port, Address], []],
	StartFunc = {gen_server, start_link, StartArgs},
	{StartMod, StartFunc, transient, 4000, worker, [StartMod]}.

