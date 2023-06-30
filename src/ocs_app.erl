%%% ocs_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the ocs private API for installation
-export([install/0, install/1, join/1]).

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
	Tables = [client, service, offer, product, resource, bucket],
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			start1();
		{timeout, BadTables} ->
			case force(BadTables) of
				ok ->
					error_logger:warning_report(["Force loaded mnesia tables",
							{tables, BadTables}, {module, ?MODULE}]),
					start1();
				{error, Reason} ->
					error_logger:error_report(["Failed to force load mnesia tables",
							{tables, BadTables}, {reason, Reason}, {module, ?MODULE}]),
					{error, Reason}
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to load mnesia tables",
					{tables, Tables}, {reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end.
%% @hidden
start1() ->
	case is_mod_auth_mnesia() of
		true ->
			Tables = [httpd_user, httpd_group],
			start2(Tables, mnesia:wait_for_tables(Tables, ?WAITFORTABLES));
		false ->
			start3()
	end.
%% @hidden
start2(_Tables, ok) ->
	start3();
start2(_Tables, {timeout, BadTables}) ->
	case force(BadTables) of
		ok ->
			error_logger:warning_report(["Force loaded mnesia tables",
					{tables, BadTables}, {module, ?MODULE}]),
			start3();
		{error, Reason} ->
			error_logger:error_report(["Failed to force load mnesia tables",
					{tables, BadTables}, {reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end;
start2(Tables, {error, Reason}) ->
	error_logger:error_report(["Failed to load mnesia tables",
			{tables, Tables}, {reason, Reason}, {module, ?MODULE}]),
	{error, Reason}.
%% @hidden
start3() ->
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			{ok, Profile} = application:get_env(hub_profile),
			start4(Profile, ServicesInfo);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start4(Profile, [{httpc, _Pid, Info} | T]) ->
	case proplists:lookup(profile, Info) of
		{profile, Profile} ->
			start5(Profile);
		_ ->
			start4(Profile, T)
	end;
start4(Profile, [_ | T]) ->
	start4(Profile, T);
start4(Profile, []) ->
	case inets:start(httpc, [{profile, Profile}]) of
		{ok, _Pid} ->
			start5(Profile);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start5(Profile) ->
	{ok, Options} = application:get_env(hub_options),
	case httpc:set_options(Options, Profile) of
		ok ->
			start6();
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start6() ->
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			{ok, Profile} = application:get_env(nrf_profile),
			start7(Profile, ServicesInfo);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start7(Profile, [{httpc, _Pid, Info} | T]) ->
	case proplists:lookup(profile, Info) of
		{profile, Profile} ->
			start8(Profile);
		_ ->
			start7(Profile, T)
	end;
start7(Profile, [_ | T]) ->
	start7(Profile, T);
start7(Profile, []) ->
	case inets:start(httpc, [{profile, Profile}]) of
		{ok, _Pid} ->
			start8(Profile);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start8(Profile) ->
	{ok, Options} = application:get_env(nrf_options),
	case httpc:set_options(Options, Profile) of
		ok ->
			start9();
		{error, Reason} ->
			{error, Reason}
	end.
%% @doc Migrate client table, if necessary, to add Trusted field.
%% @hidden
start9() ->
	case mnesia:table_info(client, arity) of
		9 ->
			start10();
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
					start10();
				{aborted, Reason} ->
					error_logger:error_report(["Failed to migrate client table",
							mnesia:error_description(Reason), {error, Reason}]),
					{error, Reason}
			end
	end.
%% @hidden
start10() ->
	Tables = [session, nrf_ref],
	start11(Tables, mnesia:wait_for_tables(Tables, ?WAITFORTABLES)).
%% @hidden
start11(_Tables, ok) ->
	start12();
start11(_Tables, {timeout, BadTables}) ->
	F = fun F([Table | T]) ->
				case create_table(Table, mnesia:system_info(db_nodes)) of
					ok ->
						F(T);
					{error, Reason} ->
						{error, Reason}
				end;
			F([]) ->
				ok
	end,
	case F(BadTables) of
		ok ->
			start12();
		{error, Reason} ->
			{error, Reason}
	end;
start11(Tables, {error, Reason}) ->
	error_logger:error_report(["Failed to load mnesia tables",
			{tables, Tables}, {reason, Reason}, {module, ?MODULE}]),
	{error, Reason}.
%% @hidden
start12() ->
	case mnesia:table_info(session, arity) of
		12 ->
			start13();
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
					start12();
				{aborted, Reason} ->
					error_logger:error_report(["Failed to migrate session table",
							mnesia:error_description(Reason), {error, Reason}]),
					{error, Reason}
			end
	end.
%% @hidden
start13() ->
	Options = [set, public, named_table, {write_concurrency, true}],
	ets:new(nrf_session, Options),
   ets:new(counters, Options),
   case catch ets:insert(counters, {nrf_seq, 0}) of
		true ->
			start14();
		{'EXIT', Reason} ->
			{error, Reason}
	end.
%% @hidden
start14() ->
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
%					{ok, Sup};
					start15(Sup);
				{error, Reason2} ->
					throw(Reason2)
			end
	catch
		Reason ->
			error_logger:error_report(["ocs application failed to start",
					{reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end.
%% @hidden
start15(Sup) ->
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			case application:get_env(elastic_shipper) of
				{ok, undefined} ->
					{ok, Sup};
				{ok, {Url, Profile, Options}} ->
					F = fun({Key, _Value}) when Key == proxy; Key == https_proxy;
							Key == max_sessions; Key == max_keep_alive_length;
							Key == keep_alive_timeout; Key == max_pipeline_length;
							Key == pipeline_timeout; Key == cookies; Key == ipfamily;
							Key == ip; Key == port; Key == socket_opts; Key == verbose;
							Key == unix_socket ->
								true;
							({_Key, _Value}) ->
								false
					end,
					SetOptions = lists:filter(F, Options),
					case supervisor:start_child(ocs_event_log_sup,
							[Url, Profile, Options -- SetOptions]) of
						{ok, _EventLogSup, _Id} ->
							start16(Profile, ServicesInfo, Sup, SetOptions);
						{error, Reason} ->
							{error, Reason}
					end
			end;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start16(Profile, [{httpc, _Pid, Info} | T], Sup, SetOptions) ->
	case proplists:lookup(profile, Info) of
		{profile, Profile} ->
			start17(Profile, Sup, SetOptions);
		_ ->
			start16(Profile, T, Sup, SetOptions)
	end;
start16(Profile, [_ | T], Sup, SetOptions) ->
	start16(Profile, T, Sup, SetOptions);
start16(Profile, [], Sup, _SetOptions) ->
	case inets:start(httpc, [{profile, Profile}]) of
		{ok, _Pid} ->
			{ok, Sup};
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start17(Profile, Sup, SetOptions) ->
	case httpc:set_options(SetOptions, Profile) of
		ok ->
			{ok, Sup};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs private API
%%----------------------------------------------------------------------

-spec install() -> Result
	when
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @equiv install([node() | nodes()])
install() ->
	Nodes = [node() | nodes()],
	install(Nodes).

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
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
				{error, {_, {already_exists, _}}} ->
						error_logger:info_report("mnesia schema already exists",
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
		{timeout, BadTables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, BadTables}]),
			{error, timeout}
	end.
%% @hidden
install3(Nodes, Acc) ->
	case create_table(client, Nodes) of
		ok ->
			install4(Nodes, [client | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Acc) ->
	case create_table(service, Nodes) of
		ok ->
			install5(Nodes, [service | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install5(Nodes, Acc) ->
	case create_table(product, Nodes) of
		ok ->
			install6(Nodes, [product | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install6(Nodes, Acc) ->
	case create_table(resource, Nodes) of
		ok ->
			install7(Nodes, [resource | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install7(Nodes, Acc) ->
	case create_table(offer, Nodes) of
		ok ->
			case add_example_offers() of
				ok ->
					error_logger:info_msg("Added example offers to product catalog.~n"),
					install8(Nodes, [offer | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{error, already_exists} ->
			install8(Nodes, [offer | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install8(Nodes, Acc) ->
	case create_table(bucket, Nodes) of
		ok ->
			install9(Nodes, [bucket | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install9(Nodes, Acc) ->
	case create_table(session, Nodes) of
		ok ->
			install10(Nodes, [session | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install10(Nodes, Acc) ->
	case create_table(nrf_ref, Nodes) of
		ok ->
			install11(Nodes, [nrf_ref | Acc]);
		{error, Reason} ->
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
	case is_mod_auth_mnesia() of
		true ->
			install13(Nodes, Acc);
		false ->
			error_logger:info_msg("Httpd service not defined. "
					"User table not created~n"),
			install15(Nodes, Acc)
	end.
%% @hidden
install13(Nodes, Acc) ->
	case create_table(httpd_user, Nodes) of
		ok ->
			install14(Nodes, [httpd_user | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install14(Nodes, Acc) ->
	case create_table(httpd_group, Nodes) of
		ok ->
			install15(Nodes, [httpd_group | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install15(_Nodes, Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			install16(Tables, lists:member(httpd_user, Tables));
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
install16(Tables, true) ->
	case inets:start() of
		ok ->
			error_logger:info_msg("Started inets.~n"),
			install17(Tables);
		{error, {already_started, inets}} ->
			install17(Tables);
		{error, Reason} ->
			error_logger:error_msg("Failed to start inets~n"),
			{error, Reason}
	end;
install16(Tables, false) ->
	{ok, Tables}.
%% @hidden
install17(Tables) ->
	case ocs:list_users() of
		{ok, []} ->
			UserData = [{locale, "en"}],
			case ocs:add_user("admin", "admin", UserData) of
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

-spec join(Node) -> Result
	when
		Node :: atom(),
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @doc Join an existing cluster.
%%
%% 	Tables will be copied from the given `Node'.
%%
join(Node) when is_atom(Node)  ->
	case mnesia:system_info(is_running) of
		no ->
			join1(Node);
		Running ->
			error_logger:error_report(["mnesia running", {is_running, Running}]),
			{error, mnesia_running}
	end.
%% @hidden
join1(Node) ->
	case net_kernel:connect_node(Node) of
		true ->
			join2(Node);
		Connect ->
			error_logger:error_report(["Failed to connect node",
					{result, Connect}]),
			{error, Connect}
	end.
%% @hidden
join2(Node) ->
	case rpc:call(Node, mnesia, add_table_copy, [schema, node(), ram_copies]) of
		{atomic, ok} ->
			join3(Node);
		{aborted, {already_exists, schema, _}} ->
			error_logger:info_msg("Found existing schema table on ~s.~n", [Node]),
			join3(Node);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join3(Node) ->
	case application:start(mnesia) of
		ok ->
			join4(Node);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join4(Node) ->
	case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, _Nodes} ->
			join5(Node);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join5(Node) ->
	case mnesia:change_table_copy_type(schema, node(), disc_copies) of
		{atomic, ok} ->
			error_logger:info_msg("Copied schema table from ~s.~n", [Node]),
			join6(Node, mnesia:system_info(db_nodes), [schema]);
		{aborted, {already_exists, schema, _, disc_copies}} ->
			error_logger:info_msg("Found existing schema table on ~s.~n", [Node]),
			join6(Node, mnesia:system_info(db_nodes), [schema]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join6(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [client, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied client table from ~s.~n", [Node]),
			join7(Node, Nodes, [client | Acc]);
		{aborted, {already_exists, client, _}} ->
			error_logger:info_msg("Found existing client table on ~s.~n", [Node]),
			join7(Node, Nodes, [client | Acc]);
		{aborted, {no_exists, {client, _C}}} ->
			case create_table(client, Nodes) of
				ok ->
					join7(Node, Nodes, [client | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join7(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [service, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied service table from ~s.~n", [Node]),
			join8(Node, Nodes, [service | Acc]);
		{aborted, {already_exists, service, _}} ->
			error_logger:info_msg("Found existing service table on ~s.~n", [Node]),
			join8(Node, Nodes, [service | Acc]);
		{aborted, {no_exists, {service, _}}} ->
			case create_table(service, Nodes) of
				ok ->
					join8(Node, Nodes, [service | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join8(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [offer, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied offer table from ~s.~n", [Node]),
			join9(Node, Nodes, [offer | Acc]);
		{aborted, {already_exists, offer, _}} ->
			error_logger:info_msg("Found existing offer table on ~s.~n", [Node]),
			join9(Node, Nodes, [offer | Acc]);
		{aborted, {no_exists, {offer, _}}} ->
			case create_table(offer, Nodes) of
				ok ->
					join9(Node, Nodes, [offer | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join9(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [product, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied product table from ~s.~n", [Node]),
			join10(Node, Nodes, [product | Acc]);
		{aborted, {already_exists, product, _}} ->
			error_logger:info_msg("Found existing product table on ~s.~n", [Node]),
			join10(Node, Nodes, [product | Acc]);
		{aborted, {no_exists, {product, _}}} ->
			case create_table(product, Nodes) of
				ok ->
					join10(Node, Nodes, [product | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join10(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [resource, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied resource table from ~s.~n", [Node]),
			join11(Node, Nodes, [resource | Acc]);
		{aborted, {already_exists, resource, _}} ->
			error_logger:info_msg("Found existing resource table on ~s.~n", [Node]),
			join11(Node, Nodes, [resource | Acc]);
		{aborted, {no_exists, {resource, _}}} ->
			case create_table(resource, Nodes) of
				ok ->
					join11(Node, Nodes, [resource | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join11(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [bucket, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied bucket table from ~s.~n", [Node]),
			join12(Node, Nodes, [bucket | Acc]);
		{aborted, {already_exists, bucket, _}} ->
			error_logger:info_msg("Found existing bucket table on ~s.~n", [Node]),
			join12(Node, Nodes, [bucket | Acc]);
		{aborted, {no_exists, {bucket, _}}} ->
			case create_table(bucket, Nodes) of
				ok ->
					join12(Node, Nodes, [bucket | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join12(Node, Nodes, Acc) ->
   case application:load(inets) of
      ok ->
         error_logger:info_msg("Loaded inets.~n"),
         join13(Node, Nodes, Acc);
      {error, {already_loaded, inets}} ->
         join13(Node, Nodes, Acc);
      {error, Reason} ->
         {error, Reason}
   end.
%% @hidden
join13(Node, Exist, Acc) ->
   case is_mod_auth_mnesia() of
      true ->
         join14(Node, Exist, Acc);
      false ->
         error_logger:info_msg("Httpd service not defined. "
               "User table not created~n"),
         join16(Node, Exist, Acc)
   end.
%% @hidden
join14(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [httpd_user, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied httpd_user table from ~s.~n", [Node]),
			join15(Node, Nodes, [httpd_user | Acc]);
		{aborted, {already_exists, httpd_user, _}} ->
			error_logger:info_msg("Found existing httpd_user table on ~s.~n", [Node]),
			join15(Node, Nodes, [httpd_user | Acc]);
		{aborted, {no_exists, {httpd_user, _}}} ->
			case create_table(httpd_user, Nodes) of
				ok ->
					join15(Node, Nodes, [httpd_user | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join15(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [httpd_group, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied httpd_group table from ~s.~n", [Node]),
			join16(Node, Nodes, [httpd_group | Acc]);
		{aborted, {already_exists, httpd_group, _}} ->
			error_logger:info_msg("Found existing httpd_group table on ~s.~n", [Node]),
			join16(Node, Nodes, [httpd_group | Acc]);
		{aborted, {no_exists, {httpd_group, _}}} ->
			case create_table(httpd_group, Nodes) of
				ok ->
					join16(Node, Nodes, [httpd_group | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join16(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [session, node(), ram_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied session table from ~s.~n", [Node]),
			join17(Node, Nodes, [session | Acc]);
		{aborted, {already_exists, session, _}} ->
			error_logger:info_msg("Found existing session table on ~s.~n", [Node]),
			join17(Node, Nodes, [session | Acc]);
		{aborted, {no_exists, {session, _}}} ->
			case create_table(session, Nodes) of
				ok ->
					join17(Node, Nodes, [session | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join17(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [nrf_ref, node(), ram_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied nrf_ref table from ~s.~n", [Node]),
			join18(Node, Nodes, [nrf_ref | Acc]);
		{aborted, {already_exists, nrf_ref, _}} ->
			error_logger:info_msg("Found existing nrf_ref table on ~s.~n", [Node]),
			join18(Node, Nodes, [nrf_ref | Acc]);
		{aborted, {no_exists, {nrf_ref, _}}} ->
			case create_table(nrf_ref, Nodes) of
				ok ->
					join18(Node, Nodes, [nrf_ref | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join18(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, system_info, [tables]) of
		[T | _] = Tables when is_atom(T) ->
			join18(Node, Nodes, Tables, Acc);
		{badrpc, Reason} ->
			error_logger:error_report(["Bad rpc call to get list of tables",
					{badrpc, Reason}]),
			{error, Reason}
	end.
%% @hidden
join18(Node, Nodes, [Table | Tables], Acc) ->
	case rpc:call(Node, mnesia, table_info, [Table, record_name]) of
		gtt ->
			case rpc:call(Node, mnesia, add_table_copy,
					[Table, node(), disc_copies]) of
				{atomic, ok} ->
					error_logger:info_msg("Copied ~s table from ~s.~n",
							[Table, Node]),
					join18(Node, Nodes, Tables, [Table | Acc]);
				{aborted, {already_exists, Table, _}} ->
					error_logger:info_msg("Found existing ~s table on ~s.~n",
							[Table, Node]),
					join18(Node, Nodes, Tables, [Table | Acc]);
				{aborted, Reason} ->
					error_logger:error_report([mnesia:error_description(Reason),
						{error, Reason}]),
					{error, Reason}
			end;
		_ ->
			join18(Node, Nodes, Tables, Acc)
	end;
join18(Node, Nodes, [], Acc) ->
	join19(Node, Nodes, Acc).
%% @hidden
join19(_Node, _Nodes, Tables) ->
	case mnesia:wait_for_tables(lists:reverse(Tables), ?WAITFORTABLES) of
		ok ->
			{ok, Tables};
		{timeout, BadTables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, BadTables}]),
			{error, timeout};
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
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
			specification = "8", price = [PriceSubscription1, PriceOverage]},
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
			specification = "8", price = [PriceSubscription2, PriceOverage]},
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
			specification = "8", price = [PriceSubscription3, PriceOverage]},
	case ocs:add_offer(Offer) of
		{ok, #offer{}} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_example_voice_offers() ->
	PriceUsage = #price{name = "Usage",
			description = "Tariffed voice calling",
			type = tariff, units = seconds, size = 60,
			char_value_use = [#char_value_use{name = "destPrefixTariffTable",
					specification = "3",
					values = [#char_value{value = "example"}]}]},
	Offer = #offer{name = "Voice Calling", description = "Tariffed voice calling",
			specification = "9", price = [PriceUsage]},
	add_example_voice_offers1(ocs:add_offer(Offer)).
%% @hidden
add_example_voice_offers1({ok, #offer{}}) ->
	TariffResource = #resource{name = "tariff-rates",
			description = "Example tariff rates table",
			specification = #specification_ref{id = "1",
					href = "/resourceCatalogManagement/v2/resourceSpecification/1",
					name = "TariffTable"}},
	case code:priv_dir(ocs) of
		PrivDir when is_list(PrivDir) ->
			TariffPath = PrivDir ++ "/examples/tariff-rates.csv",
			try ocs_gtt:import(TariffPath) of
				ok ->
					case ocs:add_resource(TariffResource) of
						{ok, #resource{}} ->
							error_logger:info_msg("Imported example tariff rates table: "
									++ TariffPath ++ "~n"),
							ok;
						{error, Reason} ->
							{error, Reason}
					end
			catch
				_:Reason ->
					{error, Reason}
			end;
		{error, _Reason} ->
			ok
	end;
add_example_voice_offers1({error, Reason}) ->
	{error, Reason}.
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

-spec create_table(Table, Nodes) -> Result
	when
		Table :: atom(),
		Nodes :: [node()],
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Create mnesia table.
%% @private
create_table(client, Nodes) when is_list(Nodes) ->
	Options = [{disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, client)},
			{index, [#client.identifier]}],
	create_table1(client, mnesia:create_table(client,Options));
create_table(service, Nodes) when is_list(Nodes) ->
	Options = [{disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, service)}],
	create_table1(service, mnesia:create_table(service, Options));
create_table(product, Nodes) when is_list(Nodes) ->
	Options = [{disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, product)}],
	create_table1(product, mnesia:create_table(product, Options));
create_table(resource, Nodes) when is_list(Nodes) ->
	Options = [{disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, resource)}],
	create_table1(resource, mnesia:create_table(resource, Options));
create_table(offer, Nodes) when is_list(Nodes) ->
	Options = [{disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, offer)}],
	create_table1(offer, mnesia:create_table(offer, Options));
create_table(bucket, Nodes) when is_list(Nodes) ->
	Options = [{disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, bucket)}],
	create_table1(bucket, mnesia:create_table(bucket, Options));
create_table(httpd_user, Nodes) when is_list(Nodes) ->
	Options = [{type, bag}, {disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, httpd_user)}],
	create_table1(httpd_user, mnesia:create_table(httpd_user, Options));
create_table(httpd_group, Nodes) when is_list(Nodes) ->
	Options = [{type, bag}, {disc_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, httpd_group)}],
	create_table1(httpd_group, mnesia:create_table(httpd_group, Options));
create_table(session, Nodes) ->
	Options = [{ram_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, session)},
			{index, [#session.imsi]}],
	create_table1(session, mnesia:create_table(session, Options));
create_table(nrf_ref, Nodes) ->
	Options = [{ram_copies, Nodes},
			{user_properties, [{ocs, true}]},
			{attributes, record_info(fields, nrf_ref)}],
	create_table1(nrf_ref, mnesia:create_table(nrf_ref, Options)).
%% @hidden
create_table1(Table, {atomic, ok}) ->
	error_logger:info_msg("Created new ~w table.~n", [Table]),
	ok;
create_table1(offer, {aborted, {already_exists, offer}}) ->
	error_logger:info_msg("Found existing ~w table.~n", [offer]),
	{error, already_exists};
create_table1(Table, {aborted, {already_exists, Table}}) ->
	error_logger:info_msg("Found existing ~w table.~n", [Table]),
	ok;
create_table1(_Table, {aborted, {not_active, _, Node} = Reason}) ->
	error_logger:error_report(["Mnesia not started on node", {node, Node}]),
	{error, Reason};
create_table1(_Table, {aborted, Reason}) ->
	error_logger:error_report([mnesia:error_description(Reason), {error, Reason}]),
	{error, Reason}.

-spec is_mod_auth_mnesia() -> boolean().
%% @doc Check if inets mod_auth uses mmnesia tables.
%% @hidden
is_mod_auth_mnesia() ->
	case application:get_env(inets, services) of
		{ok, InetsServices} ->
			is_mod_auth_mnesia1(InetsServices);
		undefined ->
			false
	end.
%% @hidden
is_mod_auth_mnesia1(InetsServices) ->
	case lists:keyfind(httpd, 1, InetsServices) of
		{httpd, HttpdInfo} ->
			F = fun({directory, _}) ->
						true;
					(_) ->
						false
			end,
			is_mod_auth_mnesia2(lists:filter(F, HttpdInfo));
		false ->
			ok
	end.
%% @hidden
is_mod_auth_mnesia2([{directory, {_Dir, []}} | T]) ->
	is_mod_auth_mnesia2(T);
is_mod_auth_mnesia2([{directory, {_, DirectoryInfo}} | T]) ->
	case lists:keyfind(auth_type, 1, DirectoryInfo) of
		{auth_type, mnesia} ->
			true;
		_ ->
			is_mod_auth_mnesia2(T)
	end;
is_mod_auth_mnesia2([]) ->
	false.

