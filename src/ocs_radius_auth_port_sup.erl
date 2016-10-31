%%% ocs_radius_auth_port_sup.erl
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
-module(ocs_radius_auth_port_sup).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor callback
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
init([Address, Port, Options]) ->
	ChildSpecs = [supervisor(ocs_simple_auth_fsm_sup, []),
		supervisor(ocs_eap_ttls_fsm_sup_sup, []),
		supervisor(ocs_eap_pwd_fsm_sup, []),
		server(ocs_radius_auth_port_server, Address, Port, Options),
		supervisor(ocs_radius_auth_server_sup, [Address, Port])],
	{ok, {{one_for_one, 10, 3600}, ChildSpecs}}.

%% @hidden
supervisor(ocs_radius_auth_server_sup = StartMod, StartArgs) ->
	StartFunc = {supervisor_bridge, start_link, [StartMod, StartArgs]},
	{StartMod, StartFunc, permanent, infinity, supervisor, [StartMod]};
supervisor(StartMod, StartArgs) ->
	StartFunc = {supervisor, start_link, [StartMod, StartArgs]},
	{StartMod, StartFunc, permanent, infinity, supervisor, [StartMod]}.

%% @hidden
server(StartMod, Address, Port, Options) ->
	GlobalName = {auth_port, Address, Port},
	Args = [self(), Address, Port, Options],
	StartArgs = [{global, GlobalName}, StartMod, Args, []],
	StartFunc = {gen_server, start_link, StartArgs},
	{StartMod, StartFunc, permanent, 4000, worker, [StartMod]}.

