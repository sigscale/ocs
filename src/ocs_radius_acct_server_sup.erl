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

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args :: [term()]) ->
	{ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()},
			[supervisor:child_spec()]}} | ignore.
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([Address, Port]) ->
	ChildSpecs = [radius_server(ocs_radius_accounting, Address, Port)],
	{ok, {{one_for_one, 10, 60}, ChildSpecs}}.

-spec radius_server(CallbackModule :: atom(),
		Address :: inet:ip_address(), Port :: pos_integer()) ->
	supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/gen_server. gen_server} behaviour.
%% @private
%%
radius_server(CallbackModule, Address, Port) ->
	StartMod = radius,
	StartFunc = {StartMod, start_link, [CallbackModule, Port, Address]},
	{CallbackModule, StartFunc, permanent, 4000,
			worker, [StartMod, CallbackModule]}.

