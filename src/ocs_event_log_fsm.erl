%%% ocs_event_log_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022 - 2026 SigScale Global Inc.
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
%%% 	module implements supervision of
%%% 	{@link //stdlib/gen_event. gen_event} handlers
%%% 	in the {@link //ocs. ocs} application.
%%%
-module(ocs_event_log_fsm).
-copyright('Copyright (c) 2022 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the public API
-export([start_link/3]).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([install/3, installed/3, backoff/3]).

-record(statedata,
		{id :: string(),
		profile :: atom(),
		callback :: string(),
		backoff :: pos_integer(),
		reason :: term(),
		options :: [{atom(), term()}]}).
-type statedata() :: #statedata{}.
-type state() :: install | installed | backoff.

%%----------------------------------------------------------------------
%%  The ocs_event_log_fsm API
%%----------------------------------------------------------------------

-spec start_link(Url, Profile, Options) -> Result
	when
		Url :: string(),
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
	Id = unique(),
	case gen_statem:start_link({global, Id},
			?MODULE, [Id, Url, Profile, Options], []) of
		{ok, Child} ->
			{ok, Child, Id};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs_event_log_fsm gen_statem call backs
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
init([Id, Url, Profile, Options1] = _Args) ->
	process_flag(trap_exit, true),
	{Time, Options3} = case lists:keytake(backoff, 1, Options1) of
		{_, {backoff, Ti}, Options2} ->
			{Ti, Options2};
		false ->
			{60, Options1}
	end,
	Data = #statedata{id = Id, profile = Profile,
			callback = Url, backoff = Time * 1000, options = Options3},
	Action = {next_event, internal, start},
	{ok, install, Data, Action}.

-spec install(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>install</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
install(internal = _EventType, start = _EventContent,
		#statedata{profile = Profile, callback = Callback,
				options = Options} = Data) ->
	case gen_event:add_sup_handler(ocs_event_log, ocs_event_log,
			[self(), Profile, Callback, Options]) of
		ok ->
			{next_state, installed, Data};
		{'EXIT', Reason} ->
			{stop, Reason}
	end.

-spec installed(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>installed</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
installed(info = _EventType,
		{gen_event_EXIT, _Handler, {swapped, _, _}} = _EventContent,
		_Data) ->
	keep_state_and_data;
installed(info = _EventType,
		{gen_event_EXIT, _Handler, Reason} = _EventContent,
		#statedata{backoff = Time} = Data) ->
	NewData = Data#statedata{reason = Reason},
	TimeoutAction = {timeout, Time, timeout},
	{next_state, backoff, NewData, TimeoutAction};
installed(info = _EventType,
		{'EXIT', _Handler, Reason} = _EventContent,
		_Data) ->
	{stop, Reason}.

-spec backoff(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>backoff</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
backoff(timeout = _EventType, timeout = _EventContent,
		#statedata{reason = Reason} = _Data) ->
	{stop, Reason};
backoff(info = _EventType,
		{'EXIT', _Handler, Reason} = _EventContent,
		_Data) ->
	{stop, Reason}.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, _Data) ->
	ok.

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

-spec unique() -> ID
	when
		ID :: string().
%% @doc Generate a unique identifier.
unique() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ integer_to_list(N).

