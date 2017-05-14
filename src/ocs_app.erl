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

-include_lib("inets/include/mod_auth.hrl").
-include("ocs.hrl").

-record(state, {}).

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES, 10000).

%%----------------------------------------------------------------------
%%  The ocs_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType, StartArgs) -> Result
	when
		StartType :: start_type(),
		StartArgs :: term(),
		Result :: {'ok', pid()} | {'ok', pid(), State} | {'error', Reason},
		State :: #state{},
		Reason :: term().
%% @doc Starts the application processes.
%% @see //kernel/application:start/1
%% @see //kernel/application:start/2
%%
start(normal = _StartType, _Args) ->
	case mnesia:wait_for_tables([client, subscriber], 60000) of
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
	{ok, RadiusConfig} = application:get_env(radius),
	{ok, DiameterConfig} = application:get_env(diameter),
	{ok, LogRotate} = application:get_env(acct_log_rotate),
	[{auth, RadAuthInstances}, {acct, RadAcctInstances}] = RadiusConfig,
	[{auth, DiamAuthInstances}, {acct, DiamAcctInstances}] = DiameterConfig,
	F1 = fun({AcctAddr, AcctPort, _Options}= _Instance) ->
		case ocs:start(radius, acct, AcctAddr, AcctPort) of
			{ok, _AcctSup} ->
				ok;
			{error, Reason2} ->
				throw(Reason2)
		end
	end,
	F2 = fun({AuthAddr, AuthPort, _Options}= _Instance) ->
		case ocs:start(radius, auth, AuthAddr, AuthPort) of
			{ok, _Authup} ->
				ok;
			{error, Reason3} ->
				throw(Reason3)
		end
	end,
	F3 = fun({AuthAddr, AuthPort, _Options}= _Instance) ->
		case ocs:start(diameter, auth, AuthAddr, AuthPort) of
			{ok, _AuthSup} ->
				ok;
			{error, Reason3} ->
				throw(Reason3)
		end
	end,
	F4 = fun({AuthAddr, AuthPort, _Options}= _Instance) ->
		case ocs:start(diameter, acct, AuthAddr, AuthPort) of
			{ok, _AcctSup} ->
				ok;
			{error, Reason3} ->
				throw(Reason3)
		end
	end,
	try
		TopSup = case supervisor:start_link(ocs_sup, [LogRotate]) of
			{ok, OcsSup} ->
				OcsSup;
			{error, Reason1} ->
				throw(Reason1)
		end,
		lists:foreach(F1, RadAcctInstances),
		lists:foreach(F2, RadAuthInstances),
		lists:foreach(F3, DiamAuthInstances),
		lists:foreach(F4, DiamAcctInstances),
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

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables},
		Tables :: [atom()].
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
%% 		{ok,[client, subscriber, httpd_user, httpd_group]}
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
		case mnesia:create_table(client, [{disc_copies, Nodes},
				{attributes, record_info(fields, client)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new client table.~n");
			{aborted, {already_exists, client}} ->
				error_logger:warning_msg("Found existing client table.~n");
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
		case mnesia:create_table(httpd_user, [{type, bag},{disc_copies, Nodes},
				{attributes, record_info(fields, httpd_user)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new httpd_user table.~n");
			{aborted, {already_exists, httpd_user}} ->
				error_logger:warning_msg("Found existing httpd_user table.~n");
			T3Result ->
				throw(T3Result)
		end,
		case mnesia:create_table(httpd_group, [{type, bag},{disc_copies, Nodes},
				{attributes, record_info(fields, httpd_group)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new httpd_group table.~n");
			{aborted, {already_exists, httpd_group}} ->
				error_logger:warning_msg("Found existing httpd_group table.~n");
			T4Result ->
				throw(T4Result)
		end,
		Tables = [client, subscriber, httpd_user, httpd_group],
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

-spec start_phase(Phase, StartType, PhaseArgs) -> Result
	when
		Phase :: atom(),
		StartType :: start_type(),
		PhaseArgs :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Called for each start phase in the application and included
%% 	applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State) -> #state{}
	when
		State :: #state{}.
%% @doc Called when the application is about to be shut down,
%% 	before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop(State) ->
	State.

-spec stop(State) -> any()
	when
		State :: #state{}.
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed, New, Removed) -> ok
	when
		Changed:: [{Par, Val}],
		New :: [{Par, Val}],
		Removed :: [Par],
		Par :: atom(),
		Val :: atom().
%% @doc Called after a code  replacement, if there are any 
%% 	changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec force(Tables) -> Result
	when
		Tables :: [TableName],
		Result :: ok | {error, Reason},
		TableName :: atom(),
		Reason :: term().
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

