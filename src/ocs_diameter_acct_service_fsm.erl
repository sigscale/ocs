%%% ocs_diameter_acct_service_fsm.erl
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module implements functions to subscribe to a
%%% 	{@link //diameter. diameter} service and to react to events sent
%%% 	by {@link //diameter. diameter} service.
%%%
%%% @reference <a href="https://www.rfc-editor.org/info/rfc6733/">
%%% 	RFC6733 - DIAMETER base protocol</a>
%%% @reference <a href="https://www.rfc-editor.org/info/rfc7155/">
%%% 	RFC7155 - DIAMETER Network Access Server Application</a>
%%% @reference <a href="https://datatracker.ietf.org/doc/html/rfc4006">
%%% 	RFC4006 - DIAMETER Credit-Control Application</a>
%%%
-module(ocs_diameter_acct_service_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([wait_for_start/3, started/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("kernel/include/inet.hrl").
-include("ocs.hrl").

-record(statedata,
		{transport_ref :: undefined | reference(),
		address :: inet:ip_address(),
		port :: inet:port_number(),
		options :: list()}).
-type statedata() :: #statedata{}.
-type state() :: wait_for_start | started.

-define(DIAMETER_ACCT_SERVICE(A, P), {ocs_diameter_acct_service, A, P}).
-define(BASE_APPLICATION, ocs_diameter_base_application).
-define(BASE_APPLICATION_DICT, diameter_gen_base_rfc6733).
-define(BASE_APPLICATION_CALLBACK, ocs_diameter_base_application_cb).
-define(RO_APPLICATION_ID, 4).
-define(RO_APPLICATION, ocs_diameter_3gpp_ro_application).
-define(RO_APPLICATION_DICT, diameter_gen_3gpp_ro_application).
-define(RO_APPLICATION_CALLBACK, ocs_diameter_3gpp_ro_application_cb).
-define(NRF_RO_APPLICATION_CALLBACK, ocs_diameter_3gpp_ro_nrf_app_cb).
-define(Gx_APPLICATION_ID, 16777238).
-define(Gx_APPLICATION, ocs_diameter_3gpp_gx_application).
-define(Gx_APPLICATION_DICT, diameter_gen_3gpp_gx_application).
-define(Gx_APPLICATION_CALLBACK, ocs_diameter_3gpp_gx_application_cb).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_service_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_service_fsm gen_statem call backs
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions].

-spec init(Args) -> Result
	when
		Args :: [term()],
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:init_result(State, Data).
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_statem:init/1
%% @private
%%
init([Address, Port, Options] = _Args) ->
	process_flag(trap_exit, true),
	{TransportOpts1, ServiceOpts1, ExtraOpts} = split_options(Options),
	TransportOpts2 = transport_options(Address, Port, TransportOpts1),
	ServiceOpts2 = service_options(ServiceOpts1, ExtraOpts),
	ServiceName = ?DIAMETER_ACCT_SERVICE(Address, Port),
	diameter:subscribe(ServiceName),
	case diameter:start_service(ServiceName, ServiceOpts2) of
		ok ->
			case diameter:add_transport(ServiceName, TransportOpts2) of
				{ok, Ref} ->
					Data = #statedata{transport_ref = Ref, address = Address,
							port = Port, options = Options},
					process_flag(trap_exit, true),
					{ok, wait_for_start, Data};
				{error, Reason} ->
					{stop, Reason}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.

-spec wait_for_start(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_for_start</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
wait_for_start(info = _EventType,
		#diameter_event{info = start} = _EventContent,
		Data) ->
	{next_state, started, Data}.

-spec started(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>started</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
started(info = _EventType,
		#diameter_event{info = Info, service = Service} = _EventContent,
		_Data) when element(1, Info) == up; element(1, Info) == down ->
	{_PeerRef, #diameter_caps{origin_host = {_, Peer}}} = element(3, Info),
	error_logger:info_report(["DIAMETER peer connection state changed",
			{service, Service}, {event, element(1, Info)},
			{peer, binary_to_list(Peer)}]),
	keep_state_and_data;
started(info = _EventType,
		#diameter_event{info = {closed, _, {Command,
				{capabilities_cb, _, ResultCode},
				#diameter_caps{origin_host = {_, Peer}}, _}, _},
				service = Service} = _EventContent,
		_Data) when Command == 'CER'; Command == 'CEA' ->
	error_logger:info_report(["DIAMETER peer address not found in client table",
			{service, Service}, {result, ResultCode},
			{peer, binary_to_list(Peer)}]),
	keep_state_and_data;
started(info = _EventType,
		#diameter_event{info = {closed, _, {Command, ResultCode,
				#diameter_caps{origin_host = {_, Peer}}, _}, _},
				service = Service} = _EventContent,
		_Data) when Command == 'CER'; Command == 'CEA' ->
	error_logger:info_report(["DIAMETER peer capabilities negotiation failed",
			{service, Service}, {result, ResultCode},
			{peer, binary_to_list(Peer)}]),
	keep_state_and_data;
started(info = _EventType,
		#diameter_event{info = {watchdog, _, _, _, _}} = _EventContent,
		_Data) ->
	keep_state_and_data;
started(info = _EventType,
		#diameter_event{info = {reconnect, _Ref, _Opts}} = _EventContent,
		_Data) ->
	keep_state_and_data;
started(info = _EventType,
		#diameter_event{info = Info, service = Service} = _EventContent,
		_Data) ->
	error_logger:info_report(["DIAMETER event",
			{service, Service}, {event, Info}]),
	keep_state_and_data;
started(info = _EventType,
		{'EXIT', _Pid, noconnection} = _EventContent,
		_Data) ->
	keep_state_and_data.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State,
		#statedata{transport_ref = TransRef,
				address = Address, port = Port} = _Data) ->
	ServiceName = ?DIAMETER_ACCT_SERVICE(Address, Port),
	try diameter:stop_service(ServiceName)
	catch
		_:_ ->
			ok
	end,
	try diameter:remove_transport(ServiceName, TransRef)
	catch
		_:_ ->
			ok
	end.

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec service_options(ServiceOpts, ExtraOpts) -> Options
	when
		ServiceOpts :: [diameter:service_opt()],
		ExtraOpts :: [tuple()],
		Options :: [diameter:service_opt()].
%% @doc Returns options for a DIAMETER service.
%% @hidden
service_options(ServiceOpts, ExtraOpts) ->
	Config = maps:from_list(ExtraOpts),
	{ok, Vsn} = application:get_key(vsn),
	Version = list_to_integer([C || C <- Vsn, C /= $.]),
	{ok, Hostname} = inet:gethostname(),
	{Callback, ServiceOpts1} = case lists:keytake(callback, 1, ServiceOpts) of
		{value, {callback, Module}, O1} ->
			{Module, O1};
		false ->
			{ocs_diameter_3gpp_ro_application_cb, ServiceOpts}
	end,
	ServiceOpts2 = case lists:keymember('Origin-Host', 1, ServiceOpts1) of
		true ->
			ServiceOpts1;
		false when length(Hostname) > 0 ->
			[{'Origin-Host', Hostname} | ServiceOpts1];
		false ->
			[{'Origin-Host', "ocs"} | ServiceOpts1]
	end,
	ServiceOpts3 = case lists:keymember('Origin-Realm', 1, ServiceOpts2) of
		true ->
			ServiceOpts2;
		false ->
			OriginRealm = case inet_db:res_option(domain) of
				S when length(S) > 0 ->
					S;
				_ ->
					"example.net"
			end,
			[{'Origin-Realm', OriginRealm} | ServiceOpts2]
	end,
	ServiceOpts4 = case lists:keymember('Inband-Security-Id', 1, ServiceOpts3) of
		true ->
			ServiceOpts3;
		false ->
			[{'Inband-Security-Id', [0]} | ServiceOpts3]
	end,
	AuthAppIds = case lists:keyfind('Auth-Application-Id', 1, ServiceOpts4) of
		{_, AIDs} when is_list(AIDs) ->
			AIDs;
		false ->
			[?RO_APPLICATION_ID, ?Gx_APPLICATION_ID]
	end,
	ServiceOpts5 = lists:keystore('Auth-Application-Id', 1,
			ServiceOpts4, {'Auth-Application-Id', AuthAppIds}),
	ServiceOpts6 = ServiceOpts5 ++ [{'Vendor-Id', ?IANA_PEN_SigScale},
		{'Product-Name', "SigScale OCS"},
		{'Firmware-Revision', Version},
		{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
		{restrict_connections, false},
		{string_decode, false},
		{application, [{alias, ?BASE_APPLICATION},
				{dictionary, ?BASE_APPLICATION_DICT},
				{module, ?BASE_APPLICATION_CALLBACK},
				{request_errors, callback}]}],
	ServiceOpts7 = case lists:member(?RO_APPLICATION_ID, AuthAppIds) of
		true ->
			ServiceOpts6 ++ [{application,
					[{alias, ?RO_APPLICATION},
					{dictionary, ?RO_APPLICATION_DICT},
					{module, [Callback, Config]},
					{request_errors, callback}]}];
		false ->
			ServiceOpts6
	end,
	case lists:member(?Gx_APPLICATION_ID, AuthAppIds) of
		true ->
			ServiceOpts7 ++ [{'Vendor-Specific-Application-Id',
					[#'diameter_base_Vendor-Specific-Application-Id'{
							'Vendor-Id' = ?IANA_PEN_3GPP,
							'Auth-Application-Id' = [?Gx_APPLICATION_ID]}]},
					{application,
							[{alias, ?Gx_APPLICATION},
							{dictionary, ?Gx_APPLICATION_DICT},
							{module, ?Gx_APPLICATION_CALLBACK},
							{request_errors, callback}]}];
		false ->
			ServiceOpts7
	end.

-spec transport_options(Address, Port, Options) -> Options
	when
		Address :: inet:ip_address(),
		Port :: inet:port_number(),
		Options :: {listen, [diameter:transport_opt()]}
				| {connect, [diameter:transport_opt()]}.
%% @doc Returns options for a DIAMETER transport layer.
%% @hidden
transport_options(Address, Port, {Role, TOptions}) ->
	TOptions1 = case lists:keymember(transport_module, 1, TOptions) of
		true ->
			TOptions;
		false ->
			[{transport_module, diameter_tcp} | TOptions]
	end,
	{Config5, TOptions3} = case lists:keytake(transport_config, 1, TOptions1) of
		{value, {_, Config1}, TOptions2} ->
			Config2 = lists:keystore(reuseaddr, 1, Config1, {reuseaddr, true}),
			Config3 = lists:keystore(port, 1, Config2, {port, Port}),
			Config4 = lists:usort([{ip, Address} | Config3]),
			{Config4, TOptions2};
		false ->
			Config1 = [{reuseaddr, true}, {ip, Address}, {port, Port}],
			{Config1, TOptions1}
	end,
	TOptions4 = [{capabilities_cb,
			fun ocs_diameter:authenticate_client/2} | TOptions3],
	transport_options1(Role, Config5, TOptions4).
%% @hidden
transport_options1(listen, Config, TOptions) ->
	{listen, [{transport_config, Config} | TOptions]};
transport_options1(connect, Config, TOptions) ->
	true = lists:keymember(raddr, 1, Config),
	true = lists:keymember(rport, 1, Config),
	{connect, [{transport_config, Config} | TOptions]}.

-spec split_options(Options) -> Result
	when
		Options :: [tuple()],
		Result :: {TransportOpts, ServiceOpts, ExtraOpts},
		TransportOpts :: {Role, [diameter:transport_opt()]},
		Role :: listen | connect,
		ServiceOpts :: [diameter:service_opt()],
		ExtraOpts :: list().
%% @doc Split `Options' list into transport, service and application options.
%% @private
split_options(Options) ->
	split_options(Options, [], [], []).
%% @hidden
split_options([{'Origin-Host', DiameterIdentity} = H | T], Acc1, Acc2, Acc3)
		when is_list(DiameterIdentity) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{'Origin-Realm', DiameterIdentity} = H | T], Acc1, Acc2, Acc3)
		when is_list(DiameterIdentity) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{'Host-IP-Address', Addresses} = H | T], Acc1, Acc2, Acc3)
		when is_list(Addresses), is_tuple(hd(Addresses)) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{application, _} = H | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{callback, _} = H | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{'Vendor-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Product-Name', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Origin-State-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Supported-Vendor-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Auth-Application-Id', _} = H | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{'Acct-Application-Id', _} = H | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, [H | Acc2], Acc3);
split_options([{'Inband-Security-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Vendor-Specific-Application-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Firmware-Revision', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{capx_timeout, Timeout} = H | T], [], Acc2, Acc3)
		when is_integer(Timeout) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{capx_timeout, Timeout} = H | T], {listen, Acc}, Acc2, Acc3)
		when is_integer(Timeout) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{incoming_maxlen, MaxLength} = H | T], [], Acc2, Acc3)
		when is_integer(MaxLength) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{incoming_maxlen, MaxLength} = H | T], {listen, Acc}, Acc2, Acc3)
		when is_integer(MaxLength) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{pool_size, PoolSize} = H | T], [], Acc2, Acc3)
		when is_integer(PoolSize) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{pool_size, PoolSize} = H | T], {listen, Acc}, Acc2, Acc3)
		when is_integer(PoolSize) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{watchdog_timer, TwInit} = H | T], [], Acc2, Acc3)
		when is_integer(TwInit) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{watchdog_timer, TwInit} = H | T], {listen, Acc}, Acc2, Acc3)
		when is_integer(TwInit) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{transport_module, diameter_tcp} = H | T], [], Acc2, Acc3) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{transport_module, diameter_tcp} = H | T], {listen, Acc}, Acc2, Acc3) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{transport_module, diameter_sctp} = H | T], [], Acc2, Acc3) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{transport_module, diameter_sctp} = H | T], {listen, Acc}, Acc2, Acc3) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{transport_config, _} = H | T], [], Acc2, Acc3) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H]}, Acc2, Acc3);
split_options([{transport_config, _} = H | T],  {listen, Acc}, Acc2, Acc3) ->
	% deprecated in ocs-3.4.12
	split_options(T, {listen, [H | Acc]}, Acc2, Acc3);
split_options([{listen, Opts} | T],  Acc1, Acc2, Acc3)
		when is_list(Acc1) ->
	split_options(T, {listen, Opts ++ Acc1}, Acc2, Acc3);
split_options([{connect, Opts} | T],  Acc1, Acc2, Acc3)
		when is_list(Acc1) ->
	split_options(T, {connect, Opts ++ Acc1}, Acc2, Acc3);
split_options([H | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, [H | Acc3]);
split_options([], Acc1, Acc2, Acc3) when is_list(Acc1) ->
	{{listen, Acc1}, Acc2, Acc3};
split_options([], {listen, Opts} = _Acc1, Acc2, Acc3) ->
	{{listen, Opts}, Acc2, Acc3};
split_options([], {connect, Opts} = _Acc1, Acc2, Acc3) ->
	{{connect, Opts}, Acc2, Acc3}.

