%%% ocs_statistics_server.erl
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
%%%
-module(ocs_statistics_server).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_statistics_server API
-export([start_link/0]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
		{last :: integer(),
		uniq :: pos_integer(),
		interval :: pos_integer(),
		timeout :: pos_integer(),
		wall0 :: [{SchedulerId :: pos_integer(),
				ActiveTime :: non_neg_integer(),
				TotalTime :: non_neg_integer()}],
		wall1 :: [{SchedulerId :: pos_integer(),
				ActiveTime :: non_neg_integer(),
				TotalTime :: non_neg_integer()}]}).
-type state() :: #state{}.

-define(TIMEOUT, 600000).

%%----------------------------------------------------------------------
%%  The ocs_statistics_server API
%%----------------------------------------------------------------------

-spec start_link() -> Result
	when
		Result :: {ok, StatsServer} | {error, Reason},
		StatsServer :: pid(),
		Reason :: term().
%% @doc Start a handler for system statistics gathering.
start_link() ->
	case gen_server:start_link({local, ocs_statistics},
			?MODULE, [[]], []) of
		{ok, Child} ->
			{ok, Child};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs_statistics_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State}
			| {ok, State, Timeout}
			| {stop, Reason} | ignore,
		State :: state(),
		Timeout :: timeout(),
		Reason :: term().
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init(_Args) ->
	{ok, I} = application:get_env(statistics_interval),
	Interval = I * 1000,
	Now = erlang:monotonic_time(millisecond),
	N = erlang:unique_integer([positive]),
	erlang:system_flag(scheduler_wall_time, true),
	Wall = erlang:statistics(scheduler_wall_time),
	ServerTimeout = Now + erlang:time_offset(millisecond) + ?TIMEOUT,
	process_flag(trap_exit, true),
	State = #state{last = Now, uniq = N,
			interval = Interval,
			wall0 = Wall, wall1 = Wall,
			timeout = ServerTimeout},
	{ok, State, Interval}.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: {pid(), Tag},
		Tag :: any(),
		State :: state(),
		Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, timeout() | hibernate}
			| {noreply, NewState}
			| {noreply, NewState, timeout() | hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
		Reply :: term(),
		NewState :: state(),
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call(scheduler_utilization, _From,
		#state{wall0 = W0, wall1 = W1, last = Last,
		uniq = N, interval = Interval} = State) ->
	F = fun({{I, _, T0}, {I, _, T1}})
					when (T1 - T0) == 0 ->
				{I, 0};
			({{I, A0, T0}, {I, A1, T1}}) ->
				{I, (A1 - A0) * 100 div (T1 - T0)}
	end,
	Report = lists:map(F, lists:zip(lists:sort(W0), lists:sort(W1))),
	Ts = Last + erlang:time_offset(millisecond),
	Euniq = integer_to_list(Ts) ++ "-" ++ integer_to_list(N),
	Reply = {Euniq, Interval, Report},
	Now = erlang:monotonic_time(millisecond),
	ServerTimeout = Now + erlang:time_offset(millisecond) + ?TIMEOUT,
	NewState = State#state{timeout = ServerTimeout},
	Timeout = case Interval - (Now - Last) of
		To when To >= 0 ->
			To;
		_To ->
			0
	end,
	{reply, Reply, NewState, Timeout}.

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, timeout() | hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(stop, State) ->
	{stop, normal, State}.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State::state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, timeout() | hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Reason :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout,
		#state{last = Last, interval = Interval, timeout = End} = State)
		when End < (Last + Interval) ->
	{stop, shutdown, State};
handle_info(timeout,
		#state{wall1 = W1, interval = Interval} = State) ->
	Now = erlang:monotonic_time(millisecond),
	N = erlang:unique_integer([positive]),
	NewState = State#state{last = Now, uniq = N, wall0 = W1,
			wall1 = erlang:statistics(scheduler_wall_time)},
	{noreply, NewState, Interval}.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	erlang:system_flag(scheduler_wall_time, false).

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState} | {error, Reason},
		NewState :: state(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

