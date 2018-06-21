%%% ocs_mib.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 SigScale Global Inc.
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
%%% @doc This library module implements the SNMP MIB for the
%%%     {@link //ocs. ocs} application.
%%%
-module(ocs_mib).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% export the ocs_mib public API
-export([load/0, load/1, unload/0, unload/1]).

%% export the ocs_mib snmp agent callbacks
-export([client_table/3]).

%%----------------------------------------------------------------------
%%  The ocs_mib public API
%%----------------------------------------------------------------------

-spec load() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Loads the SigScale OCS MIB.
load() ->
	MibDir = code:priv_dir(ocs) ++ "/mibs",
	Mibs = [MibDir ++ "/SIGSCALE-OCS-MIB"],
	snmpa:load_mibs(Mibs).

-spec load(Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Loads the SigScale OCS MIB.
load(Agent) ->
	MibDir = code:priv_dir(ocs) ++ "/mibs",
	Mibs = [MibDir ++ "/SIGSCALE-OCS-MIB"],
	snmpa:load_mibs(Agent, Mibs).

-spec unload() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Unloads the SigScale OCS MIB.
unload() ->
	Mibs = ["SIGSCALE-OCS-MIB"],
	snmpa:unload_mibs(Mibs).

-spec unload(Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Unloads the SigScale OCS MIB.
unload(Agent) ->
	Mibs = ["SIGSCALE-OCS-MIB"],
	snmpa:unload_mibs(Agent, Mibs).

%%----------------------------------------------------------------------
%% The ocs_mib snmp agent callbacks
%%----------------------------------------------------------------------

client_table(get, RowIndex, Cols) ->
erlang:display({?MODULE, ?LINE, get, RowIndex, Cols}),
	{ok, RowIndex, Cols};
client_table(get_next, RowIndex ,Cols) ->
erlang:display({?MODULE, ?LINE, get_next, RowIndex, Cols}),
	{ok, RowIndex, Cols}.
