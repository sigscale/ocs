%%% ocs_radius_acct_port_sup.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
-module(ocs_radius_acct_port_sup).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

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
init([Address, Port, Options]) ->
	ChildSpecs = [supervisor(ocs_radius_disconnect_fsm_sup, []),
			server(ocs_radius_acct_port_server, Address, Port, Options),
			supervisor_bridge(ocs_radius_acct_server_sup, [Address, Port])],
	{ok, {{one_for_one, 10, 60}, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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

%% @hidden
supervisor_bridge(StartMod, Args) ->
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor_bridge, start_link, StartArgs},
	{StartMod, StartFunc, permanent, infinity, supervisor, [StartMod]}.

%% @hidden
server(StartMod, Address, Port, Options) ->
	GlobalName = {ocs_radius_acct, Address, Port},
	Args = [self(), Address, Port, Options],
	StartArgs = [{global, GlobalName}, StartMod, Args, []],
	StartFunc = {gen_server, start_link, StartArgs},
	{StartMod, StartFunc, permanent, 4000, worker, [StartMod]}.

