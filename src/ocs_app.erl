%%% ocs_app.erl
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
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%% 	module starts and stops the {@link //ocs. ocs} application.
%%%
-module(ocs_app).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the ocs private API
-export([install/1]).

-include("ocs.hrl").

-record(state, {}).

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES, 10000).

%%----------------------------------------------------------------------
%%  The ocs_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType :: start_type(), StartArgs :: term()) ->
	{'ok', pid()} | {'ok', pid(), State :: #state{}}
			| {'error', Reason :: term()}.
%% @doc Starts the application processes.
%% @see //kernel/application:start/1
%% @see //kernel/application:start/2
%%
start(normal = _StartType, _Args) ->
	case mnesia:wait_for_tables([radius_client, subscriber, guest], 60000) of
		ok ->
			start1();
		{timeout, BadTabList} ->
			case force(BadTabList) of
				ok ->
					start1();
				{error, Reason} ->
					error_logger:error_report(["ocs application failed to start",
							{reason, Reason}, {module, ?MODULE}]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start1() ->
	{ok, RestAddr} = application:get_env(rest_addr),
	{ok, RestPort} = application:get_env(rest_port),
	{ok, AcctAddr} = application:get_env(radius_acct_addr),
	{ok, AcctPort} = application:get_env(radius_acct_port),
	{ok, AuthAddr} = application:get_env(radius_auth_addr),
	{ok, AuthPort} = application:get_env(radius_auth_port),
	try
		TopSup = case supervisor:start_link(ocs_sup,
				[RestAddr, RestPort]) of
			{ok, OcsSup} ->
				OcsSup;
			{error, Reason1} ->
				throw(Reason1)
		end,
		case ocs:start(acct, AcctAddr, AcctPort) of
			{ok, _AcctSup} ->
				ok;
			{error, Reason2} ->
				throw(Reason2)
		end,
		case ocs:start(auth, AuthAddr, AuthPort) of
			{ok, _EapSup} ->
				ok;
			{error, Reason3} ->
				throw(Reason3)
		end,
		TopSup
	of
		Sup ->
			{ok, Sup}
	catch
		Reason ->
			error_logger:error_report(["ocs application failed to start",
					{reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs private API
%%----------------------------------------------------------------------

-spec install(Nodes :: [node()]) -> {ok, Tables :: [atom()]}.
%% @doc Initialize a new installation.
%% 	`Nodes' is a list of the nodes where the 
%% 	{@link //ocs. ocs} application will run.
%% 	An mnesia schema should be created and mnesia started on
%% 	all nodes before running this function. e.g.&#058;
%% 	```
%% 		1> mnesia:create_schema([node()]).
%% 		ok
%% 		2> mnesia:start().
%% 		ok
%% 		3> {@module}:install([node()]).
%% 		{ok,[radius_client, subscriber, guest]}
%% 		ok
%% 	'''
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	try
		case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
			ok ->
				ok;
			SchemaResult ->
				throw(SchemaResult)
		end,
		case mnesia:create_table(radius_client, [{disc_copies, Nodes},
				{attributes, record_info(fields, radius_client)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new radius_client table.~n");
			{aborted, {already_exists, radius_client}} ->
				error_logger:warning_msg("Found existing radius_client table.~n");
			T1Result ->
				throw(T1Result)
		end,
		case mnesia:create_table(subscriber, [{disc_copies, Nodes},
				{attributes, record_info(fields, subscriber)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new subscriber table.~n");
			{aborted, {already_exists, subscriber}} ->
				error_logger:warning_msg("Found existing subscriber table.~n");
			T2Result ->
				throw(T2Result)
		end,
		case mnesia:create_table(guest, [{disc_copies, Nodes},
				{attributes, record_info(fields, guest)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new guest table.~n");
			{aborted, {already_exists, guest}} ->
				error_logger:warning_msg("Found existing guest table.~n");
			T3Result ->
				throw(T3Result)
		end,
		Tables = [radius_client, subscriber, guest],
		case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
			ok ->
				Tables;
			TablesResult ->
				throw(TablesResult)
		end
	of
		Result -> {ok, Result}
	catch
		throw:Error ->
			mnesia:error_description(Error)
	end.

-spec start_phase(Phase :: atom(), StartType :: start_type(),
		PhaseArgs :: term()) -> ok | {error, Reason :: term()}.
%% @doc Called for each start phase in the application and included
%% 	applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State :: #state{}) -> #state{}.
%% @doc Called when the application is about to be shut down,
%% 	before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop(State) ->
	State.

-spec stop(State :: #state{}) -> any().
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed :: [{Par :: atom(), Val :: atom()}],
		New :: [{Par :: atom(), Val :: atom()}],
		Removed :: [Par :: atom()]) -> ok.
%% @doc Called after a code  replacement, if there are any 
%% 	changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec force(Tables :: [TableName :: atom()]) ->
	ok | {error, Reason :: term()}.
%% @doc Try to force load bad tables.
force([H | T]) ->
	case mnesia:force_load_table(H) of
		yes ->
			force(T);
		ErrorDescription ->
			{error, ErrorDescription}
	end;
force([]) ->
	ok.

