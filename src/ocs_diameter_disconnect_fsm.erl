%%% ocs_diameter_disconnect_fsm.erl
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback module
%%% 	implements sending DIAMETER Abort-Session-Request to DIAMETER
%%% 	credit-control clients (Network Access Servers) using  {@link //ocs. ocs}
%%% 	application.
%%%
%%% @reference <a href="https://tools.ietf.org/html/rfc6733#section-8.5">
%%% 	RFC6733 - Diameter Base Protocol, Sec 8.5, Aborting a Session</a>
%%%
-module(ocs_diameter_disconnect_fsm).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_diameter_disconnect_fsm API
-export([]).

%% export the ocs_diameter_disconnect_fsm state callbacks
-export([send_request/2, receive_response/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-record(statedata,
		{diameter_service :: term(),
		app_alias :: term(),
		session_id :: string(),
		origin_host :: string() | binary(),
		destination_host :: string() | binary(),
		origin_realm :: string() | binary(),
		destination_realm :: string() | binary(),
		retry_time = 500 :: integer(),
		retry_count = 0 :: integer(),
		auth_app_id :: integer()}).

-define(TIMEOUT, 30000).

%%----------------------------------------------------------------------
%%  The ocs_diameter_disconnect_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_diameter_disconnect_fsm gen_fsm call backs
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
init([Svc, AppAlias, SessionId, OHost, DHost, ORealm, DRealm, AuthAppId]) ->
	process_flag(trap_exit, true),
	StateData = #statedata{diameter_service = Svc, app_alias = AppAlias,
			session_id = SessionId, origin_host = OHost,
			destination_host = DHost, origin_realm = ORealm,
			destination_realm = DRealm, auth_app_id = AuthAppId},
	{ok, send_request, StateData, 0}.

-spec send_request(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>send_request</b> state. This state is responsible
%%		for sending a DIAMETER Abort-Session-Request to an access point.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
send_request(timeout, #statedata{diameter_service = Svc, app_alias = AppAlias,
		session_id = SId, origin_host = OH, destination_host = DH, origin_realm = OR,
		destination_realm = DR, auth_app_id = AuthAppId, retry_time = Retry,
		retry_count = Count} = StateData) ->
	ASR = #diameter_base_ASR{'Session-Id' = SId, 'Origin-Host' = OH,
			'Origin-Realm' = OR, 'Destination-Realm' = DR, 'Destination-Host' = DH,
			'Auth-Application-Id' = AuthAppId},
	case diameter:call(Svc, AppAlias, ASR, []) of
		ok ->
			{stop, {shutdown, SId}, StateData};
		{error, _Reason} ->
			NewRetry = Retry * 2,
			NewCount = Count + 1,
			NewStateData = StateData#statedata{retry_count = NewCount,
					retry_time = NewRetry},
			{next_state, send_request, NewStateData, NewRetry};
		{ok, _ASA} ->
			{stop, {shutdown, SId}, StateData}
	end.

-spec receive_response(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>receive_response</b> state. This state is responsible
%%		for recieving a DIAMETER Abort-Session-Answer from an access point.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
receive_response(_Event, StateData) ->
	{next_state, receive_response, StateData}.

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
handle_info(_Info, StateName, #statedata{} = StateData) ->
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
terminate(_Reason, _StateName, #statedata{} = _StateData) ->
	ok.

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

