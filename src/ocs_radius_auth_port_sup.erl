%%% ocs_radius_auth_port_sup.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2026 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

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
init([Address, Port, Options] = _Args) ->
	ChildSpecs = [supervisor(ocs_simple_auth_fsm_sup, []),
		supervisor(ocs_eap_ttls_fsm_sup_sup, []),
		supervisor(ocs_eap_aka_fsm_sup_sup, []),
		supervisor(ocs_eap_akap_fsm_sup_sup, []),
		supervisor(ocs_eap_pwd_fsm_sup, []),
		server(ocs_radius_auth_port_server, Address, Port, Options),
		supervisor(ocs_radius_auth_server_sup, [Address, Port])],
	SupFlags = #{intensity => 10, period => 3600},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec server(StartMod, Address, Port, Options) -> Result
	when
		StartMod :: atom(),
		Address :: inet:ip_address(),
		Port :: inet:port_number(),
		Options :: list(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link gen_server. gen_server} behaviour.
%% @private
%%
server(StartMod, Address, Port, Options) ->
	GlobalName = {ocs_radius_auth, node(), Address, Port},
	Args = [self(), Address, Port, Options],
	StartArgs = [{global, GlobalName}, StartMod, Args, []],
	StartFunc = {gen_server, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			shutdown => 4000, modules => [StartMod]}.

-spec supervisor(StartMod, Args) -> Result
	when
		StartMod :: atom(),
		Args :: list(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/supervidor. supervisor} behaviour.
%% @private
%%
supervisor(ocs_radius_auth_server_sup = StartMod, Args) ->
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor_bridge, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			type => supervisor, modules => [StartMod]};
supervisor(StartMod, Args) ->
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			type => supervisor, modules => [StartMod]}.

