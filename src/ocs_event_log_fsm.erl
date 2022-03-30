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
-export([start_link/3]).

%% export the ocs_event_log_fsm states
-export([install/2, installed/2, backoff/2]).

%% export the callbacks needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4,
			terminate/3, handle_info/3, code_change/4]).

-record(statedata,
		{id :: string(),
		profile :: atom(),
		callback :: string(),
		backoff :: pos_integer(),
		reason :: term(),
		options :: [{atom(), term()}]}).
-type statedata() :: #statedata{}.

%%----------------------------------------------------------------------
%%  The ocs_event_log_fsm API
%%----------------------------------------------------------------------

-spec start_link(Url, Profile, Options) -> Result
	when
		Url :: http_uri:uri(),
		Profile :: httpc:profile(),
		Options :: [Option],
		Option :: {api_type, ApiType} | {backoff, Time},
		ApiType :: index_api | http_plugin,
		Time :: pos_integer(),
		Result :: {ok, EventLogServer} | {error, Reason},
		EventLogServer :: pid(),
		Reason :: term().
%% @doc Start a hub fsm
start_link(Url, Profile, Options) ->
	{Id, _} = unique(),
	case gen_fsm:start_link({global, Id},
			?MODULE, [Id, Url, Profile, Options], []) of
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
init([Id, Url, Profile, Options1] = _Args) ->
	process_flag(trap_exit, true),
	{Time, Options3} = case lists:keytake(backoff, 1, Options1) of
		{_, {backoff, Ti}, Options2} ->
			{Ti, Options2};
		false ->
			{60, Options1}
	end,
	StateData = #statedata{id = Id, profile = Profile,
			callback = Url, backoff = Time * 1000, options = Options3},
	{ok, install, StateData, 0}.

-spec install(Event, StateData) -> Result
	when
		Event :: timeout | pos_integer(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Reason :: normal | term().
%% @doc Handle event received in `register' state.
%% @private
install(timeout, #statedata{profile = Profile,
		callback = Callback} = StateData) ->
	case gen_event:add_sup_handler(ocs_event_log, ocs_event_log,
			[self(), Profile, Callback]) of
		ok ->
			{next_state, installed, StateData};
		{'EXIT', Reason} ->
			{stop, Reason, StateData}
	end.

-spec installed(Event, StateData) -> Result
	when
		Event :: timeout | pos_integer(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Reason :: normal | term().
%% @doc Handle event received in `register' state.
%% @private
installed(_Event, StateData) ->
	{next_state, installed, StateData}.

-spec backoff(Event, StateData) -> Result
	when
		Event :: term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Reason :: normal | term().
%% @doc Handle event received in `backoff' state.
%% @private
backoff(timeout, #statedata{reason = Reason} = StateData) ->
	{stop, Reason, StateData}.

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(),
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
%% @doc Handle a request sent using
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2}.
%% @private
%%
handle_event(Reason, _StateName, StateData) ->
	{stop, Reason, StateData}.

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
handle_sync_event(_Event, _From, StateName, #statedata{} = StateData) ->
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
handle_info({gen_event_EXIT, _Handler, {swapped, _, _}},
		installed, StateData) ->
	{next_state, installed, StateData};
handle_info({gen_event_EXIT, _Handler, Reason}, installed,
		#statedata{backoff = Time} = StateData) ->
	NewStateData = StateData#statedata{reason = Reason},
	{next_state, backoff, NewStateData, Time};
handle_info({'EXIT', _Handler, Reason}, _StateName, StateData) ->
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
terminate(_Reason, _StateName, _StateData) ->
	ok.

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

