%%% ocs_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the ocs private API for installation
-export([install/0, install/1]).

-include_lib("inets/include/mod_auth.hrl").
-include("ocs.hrl").

-record(state, {}).

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES, 60000).

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
	Tables = [client, service, offer, product, pla,
			bucket, httpd_user, httpd_group, nrf_ref],
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
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
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			{ok, Profile} = application:get_env(hub_profile),
			start2(Profile, ServicesInfo);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start2(Profile, [{httpc, _Pid, Info} | T]) ->
	case proplists:lookup(profile, Info) of
		{profile, Profile} ->
			start3(Profile);
		_ ->
			start2(Profile, T)
	end;
start2(Profile, [_ | T]) ->
	start2(Profile, T);
start2(Profile, []) ->
	case inets:start(httpc, [{profile, Profile}]) of
		{ok, _Pid} ->
			start3(Profile);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start3(Profile) ->
	{ok, Options} = application:get_env(hub_options),
	case httpc:set_options(Options, Profile) of
		ok ->
			start4();
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start4() ->
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			{ok, Profile} = application:get_env(nrf_profile),
			start5(Profile, ServicesInfo);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start5(Profile, [{httpc, _Pid, Info} | T]) ->
	case proplists:lookup(profile, Info) of
		{profile, Profile} ->
			start6(Profile);
		_ ->
			start5(Profile, T)
	end;
start5(Profile, [_ | T]) ->
	start5(Profile, T);
start5(Profile, []) ->
	case inets:start(httpc, [{profile, Profile}]) of
		{ok, _Pid} ->
			start6(Profile);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start6(Profile) ->
	{ok, Options} = application:get_env(hub_options),
	case httpc:set_options(Options, Profile) of
		ok ->
			start7();
		{error, Reason} ->
			{error, Reason}
	end.
%% @doc Migrate client table, if necessary, to add Trusted field.
%% @hidden
start7() ->
	case mnesia:table_info(client, arity) of
		9 ->
			start8();
		8 ->
			F = fun({client, Address, Identifier, Port, Protocol,
						Secret, PasswordRequired, LastModified}) ->
					#client{address = Address, identifier = Identifier,
							port = Port, protocol = Protocol, secret = Secret,
							password_required = PasswordRequired,
							last_modified = LastModified}
			end,
			NewAttributes = record_info(fields, client),
			case mnesia:transform_table(client, F, NewAttributes) of
				{atomic, ok} ->
					error_logger:info_report(["Migrated client table"]),
					start8();
				{aborted, Reason} ->
					error_logger:error_report(["Failed to migrate client table",
							mnesia:error_description(Reason), {error, Reason}]),
					{error, Reason}
			end
	end.
%% @doc Migrate session table, if necessary, to add Identity field.
%% @hidden
start8() ->
	case mnesia:table_info(session, arity) of
		12 ->
			start9();
		11 ->
			F = fun({session, Id, IMSI, App, NasHost, NasRealm, NasAddress,
					HssHost, HssRealm, UserProfile, LM}) ->
				#session{id = Id, imsi = IMSI, application = App,
						nas_host = NasHost, nas_realm = NasRealm,
						nas_address = NasAddress, hss_host = HssHost,
						hss_realm = HssRealm, user_profile = UserProfile,
						last_modified = LM}
			end,
			NewAttributes = record_info(fields, session),
			case mnesia:transform_table(session, F, NewAttributes) of
				{atomic, ok} ->
					error_logger:info_report(["Migrated session table"]),
					start9();
				{aborted, Reason} ->
					error_logger:error_report(["Failed to migrate session table",
							mnesia:error_description(Reason), {error, Reason}]),
					{error, Reason}
			end
	end.
%% @hidden
start9() ->
	Options = [set, public, named_table, {write_concurrency, true}],
	ets:new(nrf_session, Options),
   ets:new(counters, Options),
   case catch ets:insert(counters, {nrf_seq, 0}) of
		true ->
			start10();
		{'EXIT', Reason} ->
			{error, Reason}
	end.
%% @hidden
start10() ->
	{ok, RadiusConfig} = application:get_env(radius),
	{ok, DiameterConfig} = application:get_env(diameter),
	{ok, RotateInterval} = application:get_env(acct_log_rotate),
	{ok, RotateTime} = application:get_env(acct_log_rotate_time),
	RadAuthInstances = case lists:keyfind(auth, 1, RadiusConfig) of
		{auth, I1} ->
			I1;
		false ->
			[]
	end,
	RadAcctInstances = case lists:keyfind(acct, 1, RadiusConfig) of
		{acct, I2} ->
			I2;
		false ->
			[]
	end,
	DiamAuthInstances = case lists:keyfind(auth, 1, DiameterConfig) of
		{auth, I3} ->
			I3;
		false ->
			[]
	end,
	DiamAcctInstances = case lists:keyfind(acct, 1, DiameterConfig) of
		{acct, I4} ->
			I4;
		false ->
			[]
	end,
	F1 = fun({AcctAddr, AcctPort, Options} = _Instance) ->
		case ocs:start(radius, acct, AcctAddr, AcctPort, Options) of
			{ok, _AcctSup} ->
				ok;
			{error, Reason} ->
				throw(Reason)
		end
	end,
	F2 = fun({AuthAddr, AuthPort, Options} = _Instance) ->
		case ocs:start(radius, auth, AuthAddr, AuthPort, Options) of
			{ok, _AuthSup} ->
				ok;
			{error, Reason} ->
				throw(Reason)
		end
	end,
	F3 = fun({AuthAddr, AuthPort, Options} = _Instance) ->
		case ocs:start(diameter, auth, AuthAddr, AuthPort, Options) of
			{ok, _AuthSup} ->
				ok;
			{error, Reason} ->
				throw(Reason)
		end
	end,
	F4 = fun({AcctAddr, AcctPort, Options} = _Instance) ->
		case ocs:start(diameter, acct, AcctAddr, AcctPort, Options) of
			{ok, _AcctSup} ->
				ok;
			{error, Reason} ->
				throw(Reason)
		end
	end,
	try
		TopSup = case supervisor:start_link(ocs_sup,
				[RotateTime, RotateInterval]) of
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
			catch ocs_mib:load(),
			case ocs_scheduler:start() of
				ok ->
					{ok, Sup};
				{error, Reason2} ->
					throw(Reason2)
			end
	catch
		Reason ->
			error_logger:error_report(["ocs application failed to start",
					{reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs private API
%%----------------------------------------------------------------------

-spec install() -> Result
	when
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @equiv install([node() | nodes()])
install() ->
	Nodes = [node() | nodes()],
	install(Nodes).

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @doc Initialize OCS tables.
%% 	`Nodes' is a list of the nodes where
%% 	{@link //ocs. ocs} tables will be replicated.
%%
%% 	If {@link //mnesia. mnesia} is not running an attempt
%% 	will be made to create a schema on all available nodes.
%% 	If a schema already exists on any node
%% 	{@link //mnesia. mnesia} will be started on all nodes
%% 	using the existing schema.
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	case mnesia:system_info(is_running) of
		no ->
			case mnesia:create_schema(Nodes) of
				ok ->
					error_logger:info_report("Created mnesia schema",
							[{nodes, Nodes}]),
					install1(Nodes);
				{error, Reason} ->
					error_logger:error_report(["Failed to create schema",
							mnesia:error_description(Reason),
							{nodes, Nodes}, {error, Reason}]),
					{error, Reason}
			end;
		_ ->
			install2(Nodes)
	end.
%% @hidden
install1([Node] = Nodes) when Node == node() ->
	case mnesia:start() of
		ok ->
			error_logger:info_msg("Started mnesia~n"),
			install2(Nodes);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end;
install1(Nodes) ->
	case rpc:multicall(Nodes, mnesia, start, [], 60000) of
		{Results, []} ->
			F = fun(ok) ->
						false;
					(_) ->
						true
			end,
			case lists:filter(F, Results) of
				[] ->
					error_logger:info_report(["Started mnesia on all nodes",
							{nodes, Nodes}]),
					install2(Nodes);
				NotOKs ->
					error_logger:error_report(["Failed to start mnesia"
							" on all nodes", {nodes, Nodes}, {errors, NotOKs}]),
					{error, NotOKs}
			end;
		{Results, BadNodes} ->
			error_logger:error_report(["Failed to start mnesia"
					" on all nodes", {nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			{error, {Results, BadNodes}}
	end.
%% @hidden
install2(Nodes) ->
	case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
		ok ->
			install3(Nodes, []);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason};
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout}
	end.
%% @hidden
install3(Nodes, Acc) ->
	case mnesia:create_table(client, [{disc_copies, Nodes},
			{attributes, record_info(fields, client)},
			{index, [#client.identifier]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new client table.~n"),
			install4(Nodes, [client | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, client}} ->
			error_logger:info_msg("Found existing client table.~n"),
			install4(Nodes, [client | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Acc) ->
	case mnesia:create_table(service, [{disc_copies, Nodes},
			{attributes, record_info(fields, service)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new service table.~n"),
			install5(Nodes, [service| Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, service}} ->
			error_logger:info_msg("Found existing service table.~n"),
			install5(Nodes, [service| Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install5(Nodes, Acc) ->
	case mnesia:create_table(product, [{disc_copies, Nodes},
			{attributes, record_info(fields, product)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new product table.~n"),
			install6(Nodes, [product | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, product}} ->
			error_logger:info_msg("Found existing product table.~n"),
			install6(Nodes, [product | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install6(Nodes, Acc) ->
	case mnesia:create_table(resource, [{disc_copies, Nodes},
			{attributes, record_info(fields, resource)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource table.~n"),
			install7(Nodes, [resource | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, resource}} ->
			error_logger:info_msg("Found existing resource table.~n"),
			install7(Nodes, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install7(Nodes, Acc) ->
	case mnesia:create_table(offer, [{disc_copies, Nodes},
			{attributes, record_info(fields, offer)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new offer table.~n"),
			case add_example_offers() of
				ok ->
					error_logger:info_msg("Added example offers to product catalog.~n"),
					install8(Nodes, [offer | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, offer}} ->
			error_logger:info_msg("Found existing offer table.~n"),
			install8(Nodes, [offer | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
install8(Nodes, Acc) ->
	case mnesia:create_table(bucket, [{disc_copies, Nodes},
			{attributes, record_info(fields, bucket)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new bucket table.~n"),
			install9(Nodes, [bucket | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, bucket}} ->
			error_logger:info_msg("Found existing bucket table.~n"),
			install9(Nodes, [bucket | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install9(Nodes, Acc) ->
	case mnesia:create_table(session, [{ram_copies, Nodes},
			{attributes, record_info(fields, session)},
			{index, [#session.imsi]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new session table.~n"),
			install10(Nodes, [session | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, session}} ->
			error_logger:info_msg("Found existing session table.~n"),
			install10(Nodes, [session | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install10(Nodes, Acc) ->
	case mnesia:create_table(nrf_ref, [{ram_copies, Nodes},
			{attributes, record_info(fields, nrf_session)},
			{index, [#nrf_session.rating_ref]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new nrf ref table.~n"),
			install11(Nodes, [nrf_ref | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, nrf_ref}} ->
			error_logger:info_msg("Found existing nrf_ref table.~n"),
			install11(Nodes, [nrf_ref | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install11(Nodes, Acc) ->
	case application:load(inets) of
		ok ->
			error_logger:info_msg("Loaded inets.~n"),
			install12(Nodes, Acc);
		{error, {already_loaded, inets}} ->
			install12(Nodes, Acc)
	end.
%% @hidden
install12(Nodes, Acc) ->
	case application:get_env(inets, services) of
		{ok, InetsServices} ->
			install13(Nodes, Acc, InetsServices);
		undefined ->
			error_logger:info_msg("Inets services not defined. "
					"User table not created~n"),
			install17(Nodes, Acc)
	end.
%% @hidden
install13(Nodes, Acc, InetsServices) ->
	case lists:keyfind(httpd, 1, InetsServices) of
		{httpd, HttpdInfo} ->
			install14(Nodes, Acc, lists:keyfind(directory, 1, HttpdInfo));
		false ->
			error_logger:info_msg("Httpd service not defined. "
					"User table not created~n"),
			install17(Nodes, Acc)
	end.
%% @hidden
install14(Nodes, Acc, {directory, {_, DirectoryInfo}}) ->
	case lists:keyfind(auth_type, 1, DirectoryInfo) of
		{auth_type, mnesia} ->
			install15(Nodes, Acc);
		_ ->
			error_logger:info_msg("Auth type not mnesia. "
					"User table not created~n"),
			install17(Nodes, Acc)
	end;
install14(Nodes, Acc, false) ->
	error_logger:info_msg("Auth directory not defined. "
			"User table not created~n"),
	install16(Nodes, Acc).
%% @hidden
install15(Nodes, Acc) ->
	case mnesia:create_table(httpd_user, [{type, bag},{disc_copies, Nodes},
			{attributes, record_info(fields, httpd_user)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_user table.~n"),
			install16(Nodes, [httpd_user | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_user}} ->
			error_logger:info_msg("Found existing httpd_user table.~n"),
			install16(Nodes, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install16(Nodes, Acc) ->
	case mnesia:create_table(httpd_group, [{type, bag},{disc_copies, Nodes},
			{attributes, record_info(fields, httpd_group)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_group table.~n"),
			install17(Nodes, [httpd_group | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_group}} ->
			error_logger:info_msg("Found existing httpd_group table.~n"),
			install17(Nodes, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install17(_Nodes, Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			install18(Tables, lists:member(httpd_user, Tables));
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout};
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install18(Tables, true) ->
	case inets:start() of
		ok ->
			error_logger:info_msg("Started inets.~n"),
			install19(Tables);
		{error, {already_started, inets}} ->
			install19(Tables);
		{error, Reason} ->
			error_logger:error_msg("Failed to start inets~n"),
			{error, Reason}
	end;
install18(Tables, false) ->
	{ok, Tables}.
%% @hidden
install19(Tables) ->
	case ocs:list_users() of
		{ok, []} ->
			case ocs:add_user("admin", "admin", "en") of
				{ok, _LastModified} ->
					error_logger:info_report(["Created a default user",
							{username, "admin"}, {password, "admin"},
							{locale, "en"}]),
					{ok, Tables};
				{error, Reason} ->
					error_logger:error_report(["Failed to creat default user",
							{username, "admin"}, {password, "admin"},
							{locale, "en"}]),
					{error, Reason}
			end;
		{ok, Users} ->
			error_logger:info_report(["Found existing http users",
					{users, Users}]),
			{ok, Tables};
		{error, Reason} ->
			error_logger:error_report(["Failed to list http users",
				{error, Reason}]),
			{error, Reason}
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
%% @private
force([H | T]) ->
	case mnesia:force_load_table(H) of
		yes ->
			force(T);
		ErrorDescription ->
			{error, ErrorDescription}
	end;
force([]) ->
	ok.

-spec add_example_offers() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Seed product catalog with example offers.
%% @private
add_example_offers() ->
	add_example_offers1(add_example_data_offers()).
%% @hidden
add_example_offers1(ok) ->
	add_example_offers2(add_example_voice_offers());
add_example_offers1({error, Reason}) ->
	{error, Reason}.
%% @hidden
add_example_offers2(ok) ->
	add_example_bundles();
add_example_offers2({error, Reason}) ->
	{error, Reason}.

%% @hidden
add_example_data_offers() ->
	Alteration = #alteration{name = "Allowance",
			description = "Usage included in monthly subscription.",
			type = recurring, period = monthly,
			units = octets, amount = 0},
	PriceSubscription = #price{name = "Subscription",
			description = "Monthly subscription charge",
			type = recurring, period = monthly,
			amount = 1000000000, alteration = Alteration},
	PriceOverage = #price{name = "Overage",
			description = "Usage over and above monthly allowance",
			type = usage, units = octets, size = 1000000, amount = 1000000},
	add_example_data_offer1(Alteration, PriceSubscription, PriceOverage).
%% @hidden
add_example_data_offer1(Alteration, PriceSubscription, PriceOverage) ->
	Alteration1 = Alteration#alteration{size = 1000000000},
	PriceSubscription1 = PriceSubscription#price{amount = 1000000000,
			alteration = Alteration1},
	Offer = #offer{name = "Data (1G)", description = "1GB/month",
			status = in_study, specification = "8",
			price = [PriceSubscription1, PriceOverage]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			add_example_data_offer2(Alteration, PriceSubscription, PriceOverage);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_example_data_offer2(Alteration, PriceSubscription, PriceOverage) ->
	Alteration2 = Alteration#alteration{size = 4000000000},
	PriceSubscription2 = PriceSubscription#price{amount = 3500000000,
			alteration = Alteration2},
	Offer = #offer{name = "Data (4G)", description = "4GB/month",
			status = in_study, specification = "8",
			price = [PriceSubscription2, PriceOverage]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			add_example_data_offer3(Alteration, PriceSubscription, PriceOverage);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_example_data_offer3(Alteration, PriceSubscription, PriceOverage) ->
	Alteration3 = Alteration#alteration{size = 10000000000},
	PriceSubscription3 = PriceSubscription#price{amount = 7500000000,
			alteration = Alteration3},
	Offer = #offer{name = "Data (10G)", description = "10GB/month",
			status = in_study, specification = "8",
			price = [PriceSubscription3, PriceOverage]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

%% @hidden
add_example_voice_offers() ->
	TariffResource = #resource{name = "example", state = "created",
			description = "Example voice tariff",
			specification = #specification_ref{id = "1",
			href = "/resourceCatalogManagement/v2/resourceSpecification/1",
			name = "TariffRowTable"}},
	PriceUsage = #price{name = "Usage", description = "Tariffed voice calling",
			type = tariff, units = seconds, size = 60,
			char_value_use = [#char_value_use{name = "destPrefixTariffTable",
			specification = "3", values = [#char_value{value = "example"}]}]},
	Offer = #offer{name = "Voice Calling", description = "Tariffed voice calling",
			status = in_study, specification = "9",
			price = [PriceUsage]},
	case ocs:add_resource(TariffResource) of
		{ok, #resource{}} ->
			case ocs:add_offer(Offer) of
				{ok, #offer{}} ->
					case code:priv_dir(ocs) of
						PrivDir when is_list(PrivDir) ->
							TariffPath = PrivDir ++ "/examples/example.csv",
							try ocs_gtt:import(TariffPath) of
								ok ->
									error_logger:info_msg("Imported example tariff table: "
											++ TariffPath ++ "~n"),
									ok
							catch
								_:Reason ->
									{error, Reason}
							end;
						{error, _Reason} ->
							ok
					end;
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

%% @hidden
add_example_bundles() ->
	PriceInstall = #price{name = "Installation",
			description = "One time installation charge",
			type = one_time, amount = 1000000000},
	add_example_bundles1(PriceInstall).
%% @hidden
add_example_bundles1(PriceInstall) ->
	Offer = #offer{name = "Voice & Data (1G)",
			description = "Tariffed voice and 1GB/month data",
			bundle = [#bundled_po{name = "Data (1G)"},
			#bundled_po{name = "Voice Calling"}],
			price = [PriceInstall]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			add_example_bundles2(PriceInstall);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_example_bundles2(PriceInstall) ->
	Offer = #offer{name = "Voice & Data (4G)",
			description = "Tariffed voice and 4GB/month data",
			bundle = [#bundled_po{name = "Data (4G)"},
			#bundled_po{name = "Voice Calling"}],
			price = [PriceInstall]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			add_example_bundles3(PriceInstall);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_example_bundles3(PriceInstall) ->
	Offer = #offer{name = "Voice & Data (10G)",
			description = "Tariffed voice and 10GB/month data",
			bundle = [#bundled_po{name = "Data (10G)"},
			#bundled_po{name = "Voice Calling"}],
			price = [PriceInstall]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

