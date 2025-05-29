%%% ocs_diameter_acct_service_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback
%%% 	module implements functions to subscribe to a {@link //diameter. diameter}
%%% 	service and to react to events sent by {@link //diameter. diameter} service.
%%%
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%% 	RFC4006 - DIAMETER Credit-Control Application</a>
%%%
-module(ocs_diameter_acct_service_fsm).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_diameter_acct_service_fsm API
-export([]).

%% export the ocs_diameter_acct_service_fsm state callbacks
-export([wait_for_start/2, started/2, wait_for_stop/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("kernel/include/inet.hrl").
-include("ocs.hrl").

-record(statedata,
		{transport_ref :: undefined | reference(),
		address :: inet:ip_address(),
		port :: inet:port_number(),
		options :: list()}).

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
-define(WAIT_STOP, 11000).

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_service_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_service_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason} | ignore,
		StateName :: atom(),
		StateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
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
					StateData = #statedata{transport_ref = Ref, address = Address,
							port = Port, options = Options},
					init1(StateData);
				{error, Reason} ->
					{stop, Reason}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.

%% @hidden
init1(StateData) ->
	case ocs_log:acct_open() of
		ok ->
			case ocs_log:abmf_open() of
				ok ->
					process_flag(trap_exit, true),
					{ok, wait_for_start, StateData, 0};
				{error, Reason} ->
					{stop, Reason}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.

-spec wait_for_start(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>wait_for_start</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
wait_for_start(timeout, StateData) ->
	{next_state, wait_for_start, StateData}.

-spec started(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>started</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
started(timeout, StateData) ->
	{next_state, started, StateData}.

-spec wait_for_stop(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>wait_for_stop</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
wait_for_stop(timeout, StateData) ->
	{stop, shutdown, StateData}.

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(),
		StateName :: atom(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason , NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid :: pid(), Tag :: term()},
		StateName :: atom(),
		StateData :: #statedata{},
		Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(),
		StateName :: atom(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info(#diameter_event{info = start}, wait_for_start, StateData) ->
	{next_state, started, StateData};
%handle_info(#diameter_event{info = stop}, _StateName, StateData) ->
%	{next_state, wait_for_stop, StateData, ?WAIT_STOP};
handle_info(#diameter_event{info = Info, service = Service},
		StateName, StateData) when element(1, Info) == up;
		element(1, Info) == down ->
	{_PeerRef, #diameter_caps{origin_host = {_, Peer}}} = element(3, Info),
	error_logger:info_report(["DIAMETER peer connection state changed",
			{service, Service}, {event, element(1, Info)},
			{peer, binary_to_list(Peer)}]),
	{next_state, StateName, StateData};
handle_info(#diameter_event{info = {closed, _,
		{Command, {capabilities_cb, _, ResultCode},
		#diameter_caps{host_ip_address = {_, HostIpAddresses},
		origin_host = {_, Peer}}, _}, _},
		service = Service}, StateName, StateData)
		when Command == 'CER'; Command == 'CEA' ->
	error_logger:info_report(["DIAMETER peer address not found in client table",
			{service, Service}, {result, ResultCode},
			{peer, binary_to_list(Peer)},
			{addresses, HostIpAddresses}]),
	{next_state, StateName, StateData};
handle_info(#diameter_event{info = {closed, _,
		{Command, ResultCode, #diameter_caps{origin_host = {_, Peer}}, _}, _},
		service = Service}, StateName, StateData)
		when Command == 'CER'; Command == 'CEA' ->
	error_logger:info_report(["DIAMETER peer capabilities negotiation failed",
			{service, Service}, {result, ResultCode},
			{peer, binary_to_list(Peer)}]),
	{next_state, StateName, StateData};
handle_info(#diameter_event{info = {watchdog, _, _, _, _}},
		StateName, StateData) ->
	{next_state, StateName, StateData};
handle_info(#diameter_event{info = {reconnect, _Ref, _Opts}},
		StateName, StateData) ->
	{next_state, StateName, StateData};
handle_info(#diameter_event{info = Info, service = Service},
		StateName, StateData) ->
	error_logger:info_report(["DIAMETER event",
			{service, Service}, {event, Info}]),
	{next_state, StateName, StateData};
handle_info({'EXIT', _Pid, noconnection}, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec terminate(Reason, StateName, StateData) -> any()
	when
		Reason :: normal | shutdown | term(),
		StateName :: atom(),
		StateData :: #statedata{}.
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason, _StateName,  #statedata{transport_ref = TransRef,
		address = Address, port = Port} = _StateData) ->
	ServiceName = ?DIAMETER_ACCT_SERVICE(Address, Port),
	catch diameter:stop_service(ServiceName),
	catch diameter:remove_transport(ServiceName, TransRef),
	catch ocs_log:acct_close().

-spec code_change(OldVsn, StateName, StateData, Extra) -> Result
	when
		OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(),
		StateData :: #statedata{},
		Extra :: term(),
		Result :: {ok, NextStateName :: atom(), NewStateData :: #statedata{}}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

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
	ServiceOpts4 ++ [{'Vendor-Id', ?IANA_PEN_SigScale},
		{'Product-Name', "SigScale OCS"},
		{'Firmware-Revision', Version},
		{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
		{'Auth-Application-Id', [?RO_APPLICATION_ID, ?Gx_APPLICATION_ID]},
		{'Vendor-Specific-Application-Id',
				[#'diameter_base_Vendor-Specific-Application-Id'{
						'Vendor-Id' = ?IANA_PEN_3GPP,
						'Auth-Application-Id' = [?Gx_APPLICATION_ID]}]},
		{restrict_connections, false},
		{string_decode, false},
		{application, [{alias, ?BASE_APPLICATION},
				{dictionary, ?BASE_APPLICATION_DICT},
				{module, ?BASE_APPLICATION_CALLBACK},
				{request_errors, callback}]},
		{application, [{alias, ?RO_APPLICATION},
				{dictionary, ?RO_APPLICATION_DICT},
				{module, [Callback, Config]},
				{request_errors, callback}]},
		{application, [{alias, ?Gx_APPLICATION},
				{dictionary, ?Gx_APPLICATION_DICT},
				{module, ?Gx_APPLICATION_CALLBACK},
				{request_errors, callback}]}].

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
split_options([{'Auth-Application-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
split_options([{'Acct-Application-Id', _} | T], Acc1, Acc2, Acc3) ->
	split_options(T, Acc1, Acc2, Acc3);
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

