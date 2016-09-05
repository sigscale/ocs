%%% ocs_radius_acct_server_sup.erl
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
-module(ocs_radius_acct_server_sup).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(supervisor_bridge).

%% export the call back needed for supervisor behaviour
-export([init/1, terminate/2]).

%%----------------------------------------------------------------------
%%  The supervisor_bridge callbacks
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	Result :: {ok, Pid :: pid(), State :: pid()}
		| ignore | {error, Error :: term()}.
%% @doc Initialize the {@module} supervisor_bridge.
%% @see //stdlib/supervisor_bridge:init/1
%% @private
%%
init([Address, Port] = _Args) ->
	StartMod = ocs_radius_accounting,
	{ok, Pid} = radius:start_link(StartMod, Port, Address),
	{ok, Pid, Pid}.

-spec terminate(Reason :: shutdown | term(), State :: pid()) -> any().
%% @doc This function is called when it is about to terminate.
terminate(_Reason, State) ->
	radius:stop(State).

