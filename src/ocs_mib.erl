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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the SNMP MIB for the
%%%     {@link //ocs. ocs} application.
%%%
-module(ocs_mib).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% export the ocs_mib public API
-export([load/0, load/1, unload/0, unload/1]).

%% export the ocs_mib snmp agent callbacks
-export([client_table/3]).

-include("ocs.hrl").

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

-spec client_table(Operation, RowIndex, Columns) -> Result
	when
		Operation :: get_next,
		RowIndex :: ObjectIdentifier,
		ObjectIdentifier :: [integer()],
		Columns :: [Column],
		Column :: integer(),
		Result :: [Element] | {genErr, Column},
		Element :: {NextOid, NextValue},
		NextOid :: ObjectIdentifier,
		NextValue :: atom() | integer() | string() | [integer()].
%% @doc Handle SNMP requests for the client table.
%% @private
client_table(get_next, [] = _RowIndex, [0] = _Columns) ->
	client_table(get_next, [], [1]);
client_table(get_next, [], Columns) ->
	F = fun() ->
			 mnesia:first(client)
	end,
	client_table_get_next(F, Columns);
client_table(get_next, [1, 4] ++ Key, Columns) ->
	F = fun() ->
			 mnesia:next(client, list_to_tuple(Key))
	end,
	client_table_get_next(F, Columns).
%% @hidden
client_table_get_next(F1, Columns) ->
	case mnesia:ets(F1) of
		IP when is_tuple(IP) ->
			case ocs:find_client(IP) of
				{ok, #client{port = Port, identifier = Id, protocol = Proto}} ->
					Key = tuple_to_list(IP),
					F2 = fun(1, Acc) ->
								[{[1, 1, 4 | Key], ipv4} | Acc];
							(2, Acc) ->
								[{[2, 1, 4 | Key], Key} | Acc];
							(3, Acc) ->
								[{[3, 1, 4 | Key], Port} | Acc];
							(4, Acc) ->
								[{[4, 1, 4 | Key], binary_to_list(Id)} | Acc];
							(5, Acc) ->
								[{[5, 1, 4 | Key], Proto} | Acc]
					end,
					lists:reverse(lists:foldl(F2, [], Columns));
				{error, _Reason} ->
					{genErr, 0}
			end;
		'$end_of_table' ->
			[endOfTable]
	end.
