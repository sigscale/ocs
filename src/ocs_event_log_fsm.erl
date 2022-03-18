%%% ocs_event_log_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022 SigScale Global Inc.
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
%%%
-module(ocs_event_log_fsm).
-copyright('Copyright (c) 2022 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the public API
-export([start_link/0]).

%% export the ocs_event_log_fsm states
-export([install/2, backoff/2]).

%% export the callbacks needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4,
			terminate/3, handle_info/3, code_change/4]).

-record(statedata,
		{id :: string(),
		profile :: atom(),
		callback :: string(),
		backoff :: pos_integer()}).
-type statedata() :: #statedata{}.

%%----------------------------------------------------------------------
%%  The ocs_event_log_fsm API
%%----------------------------------------------------------------------

-spec start_link() -> Result
	when
		Result :: {ok, PageServer} | {error, Reason},
		PageServer :: pid(),
		Reason :: term().
%% @doc Start a hub fsm
start_link() ->
	{Id, _} = unique(),
	case gen_fsm:start_link({global, Id}, ?MODULE, [Id], []) of
		{ok, Child} ->
			{ok, Child, Id};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs_event_log_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData} | {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate} | {stop, Reason} | ignore,
		StateName ::atom(),
		StateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} fsm.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([Id] = _Args) ->
	process_flag(trap_exit, true),
	init(Id, application:get_env(elastic_shipper)).
%% @hidden
init(_Id, undefined) ->
	{stop, {elastic_shipper, undefined}};
init(Id, {ok, {Url, Profile, Options}}) ->
	init(Id, Url, Profile, lists:keyfind(backoff, 1, Options)).
%% @hidden
init(_Id, _Url, _Profile, false) ->
	{stop, {backoff, undefined}};
init(Id, Url, Profile, {_, Time}) ->
	State = case Url of
		U1 when is_list(U1) ->
			#statedata{id = Id, profile = Profile,
					callback = U1, backoff = Time};
		U2 when is_binary(U2) ->
			#statedata{id = Id, profile = Profile,
					callback = binary_to_list(U2), backoff = Time}
	end,
	{ok, install, State, 0}.

-spec install(Event, State) -> Result
	when
		Event :: timeout | pos_integer(),
		State :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Reason :: normal | term().
%% @doc Handle event received in `register' state.
%% @private
install(timeout, #statedata{profile = Profile, callback = Callback} = State) ->
	case gen_event:add_sup_handler(ocs_event_log, ocs_event_log,
			[self(), Profile, Callback]) of
		ok ->
			{next_state, backoff, State};
		{'EXIT', Reason} ->
			{stop, Reason, State}
	end.

-spec backoff(Event, State) -> Result
	when
		Event :: {Type, Resource, Category},
		Type :: create_bucket | delete_bucket | charge | depleted | accumulated
				| create_product | delete_product | create_service | delete_service
				| create_offer | delete_offer | create_resource | delete_resource
				| insert_gtt | delete_gtt | log_acct,
		Resource :: ocs_log:acct_event() | ocs_log:auth_event(),
		Category :: balance | product | service | resource | usage,
		State :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Reason :: normal | term().
%% @doc Handle event received in `backoff' state.
%% @private
backoff(_Result, #statedata{backoff = Time} = StateData) ->
	timer:sleep(Time),
	{next_state, install, StateData}.

-spec handle_event(Event, StateName, State) -> Result
	when
		Event :: term(),
		StateName :: atom(),
		State :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a request sent using
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2}.
%% @private
%%
handle_event(Reason, _StateName, State) ->
	{stop, Reason, State}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid, Tag},
		Pid :: pid(),
		Tag :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData }
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData} | {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(get, _From, StateName, #statedata{} = StateData) ->
	{next_state, StateName, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info({gen_event_EXIT, _Handler, Reason}, _StateName, StateData) ->
	{stop, Reason, StateData}.

-spec terminate(Reason, StateName, StateData) -> any()
	when
		Reason :: normal | shutdown | term(),
		StateName :: atom(),
		StateData :: statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(Reason, _StateName, _StateData)
		when Reason == shutdown; Reason == normal ->
	ok;
terminate({shutdown, Reason}, StateName, StateData) ->
	terminate(Reason, StateName, StateData);
terminate(Reason, StateName,
		#statedata{id = Id, callback = Callback}) ->
	error_logger:warning_report(["Notification subscription cancelled",
			{reason, Reason}, {pid, self()}, {state, StateName},
			{id, Id}, {callback, Callback}]).

-spec code_change(OldVsn, StateName, StateData, Extra) -> Result
	when
		OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(),
		StateData :: statedata(),
		Extra :: term(),
		Result :: {ok, NextStateName, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec unique() -> Result
	when
		Result :: {ID, TS},
		TS :: pos_integer(),
		ID :: string().
%% @doc Generate a unique identifier.
unique() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	ID = integer_to_list(TS) ++ integer_to_list(N),
	{ID, TS}.

