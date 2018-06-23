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

client_table(get_next, [] = RowIndex, [0] = Cols) ->
	F = fun() ->
			 mnesia:first(client)
	end,
	case mnesia:ets(F) of
		IP when is_tuple(IP) ->
			case ocs:find_client(IP) of
				{ok, #client{}} ->
					[{[1, 1, 4 | tuple_to_list(IP)], ipv4}];
				{error, _Reason} ->
					{genErr, 0}
			end;
		'$end_of_table' ->
			[endOfTable]
	end;
client_table(get_next, [1, 4] ++ Key1, Cols) ->
	F1 = fun() ->
			 mnesia:next(client, list_to_tuple(Key1))
	end,
	case mnesia:ets(F1) of
		IP when is_tuple(IP) ->
			Key2 = tuple_to_list(IP),
			case ocs:find_client(IP) of
				{ok, #client{port = Port, identifier = Id, protocol = Proto}} ->
					F2 = fun(1, Acc) ->
								[{[1, 1, 4 | Key2], ipv4} | Acc];
							(2, Acc) ->
								[{[2, 1, 4 | Key2], Key2} | Acc];
							(3, Acc) ->
								[{[3, 1, 4 | Key2], Port} | Acc];
							(4, Acc) ->
								[{[4, 1, 4 | Key2], binary_to_list(Id)} | Acc];
							(5, Acc) ->
								[{[5, 1, 4 | Key2], Proto} | Acc]
					end,
					lists:reverse(lists:foldl(F2, [], Cols));
				{error, _Reason} ->
					{genErr, 0}
			end;
		'$end_of_table' ->
			[endOfTable]
	end.

