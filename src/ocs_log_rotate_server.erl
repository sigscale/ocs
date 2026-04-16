%%% ocs_log_rotate_server.erl
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
%%%
-module(ocs_log_rotate_server).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_log_rotate_server API
-export([]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

-record(state,
			{interval :: pos_integer(),
			schedule :: calendar:time(),
			last :: pos_integer() | undefined,
			dir :: string() | undefined,
			type :: chf | voip | wlan}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The ocs_log_rotate_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_log_rotate_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State}
				| {ok, State, Timeout}
				| {ok, State, hibernate}
				| {ok, State, {continue, Continue}}
				| {stop, Reason}
				| ignore,
		State :: state(),
		Timeout :: timeout(),
		Continue :: term(),
		Reason :: term().
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%% @todo Allow intervals shorter than one day.
init([Type, ScheduledTime, Interval] = _Args) when
		((Type == chf) or (Type == wlan) or (Type == voip)),
		tuple_size(ScheduledTime) =:= 3,
		is_integer(Interval), Interval > 0 ->
	process_flag(trap_exit, true),
	State = #state{interval = Interval,
			schedule = ScheduledTime, type = Type},
	{ok, State, {continue, init}};
init([Type, ScheduledTime, Interval] = _Args) ->
	error_logger:warning_report(["Ignored archive log specification",
			{type, Type}, {time, ScheduledTime}, {interval, Interval}]),
	ignore.

-spec handle_continue(Info, State) -> Result
	when
		Info :: term(),
		State :: state(),
		Result :: {noreply, NewState}
				| {noreply, NewState, Timeout}
				| {noreply, NewState, hibernate}
				| {noreply, NewState, {continue, Continue}}
				| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: timeout(),
		Continue :: term(),
		Reason :: term().
%% @doc Handle a callback conntinuation.
%% @see //stdlib/gen_server:handle_continue/2
%% @private
%%
handle_continue(init = _Info, #state{type = chf} = State) ->
	{ok, Directory} = application:get_env(cdr_log_dir),
	case file:make_dir(Directory) of
		ok ->
			handle_continue1(Directory, State);
		{error, eexist} ->
			handle_continue1(Directory, State);
		{error, Reason} ->
			{stop, Reason}
	end;
%% deprecated
handle_continue(init = _Info, #state{type = Type} = State)
		when Type == wlan; Type == voip ->
	{ok, Directory} = application:get_env(ipdr_log_dir),
	case file:make_dir(Directory) of
		ok ->
			handle_continue1(Directory, State);
		{error, eexist} ->
			handle_continue1(Directory, State);
		{error, Reason} ->
			{stop, Reason}
	end.
%% @hidden
handle_continue1(Directory, #state{type = Type,
		interval = Interval, schedule = ScheduledTime} = State) ->
	Directory1 = Directory ++ "/" ++ atom_to_list(Type),
	NewState = State#state{dir = Directory1},
	Timeout = next(ScheduledTime, Interval),
	case file:make_dir(Directory1) of
		ok ->
			{noreply, NewState, Timeout};
		{error, eexist} ->
			{noreply, NewState, Timeout};
		{error, Reason} ->
			{stop, Reason}
	end.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: gen_server:from(),
		State :: state(),
		Result :: {reply, Reply, NewState}
				| {reply, Reply, NewState, Timeout}
				| {reply, Reply, NewState, hibernate}
				| {reply, Reply, NewState, {continue, Continue}}
				| {noreply, NewState}
				| {noreply, NewState, Timeout}
				| {noreply, NewState, {continue, Continue}}
				| {stop, Reason, Reply, NewState}
				| {stop, Reason, NewState},
		Reply :: term(),
		NewState :: state(),
		Timeout :: timeout(),
		Continue :: term(),
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call(_Request, _From, State) ->
	{stop, not_implemented, State}.

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState}
				| {noreply, NewState, Timeout}
				| {noreply, NewState, hibernate}
				| {noreply, NewState, {continue, Continue}}
				| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: timeout(),
		Continue :: term(),
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
				| {noreply, NewState, Timeout}
				| {noreply, NewState, hibernate}
				| {noreply, NewState, {continue, Continue}}
				| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: timeout(),
		Continue :: term(),
		Reason :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, #state{last = undefined,
		interval = Interval, schedule = ScheduledTime} = State) ->
	Now = erlang:system_time(millisecond),
	Last = Now - next(ScheduledTime, Interval),
	NewState = State#state{last = Last},
	handle_info(timeout, NewState);
handle_info(timeout, #state{last = Last, interval = Interval,
		schedule = ScheduledTime, type = Type} = State)
		when Type == wlan; Type == voip ->
	Now = erlang:system_time(millisecond),
	FileName = ocs_log:iso8601(Now),
	case ocs_log:ipdr_log(Type, FileName, Last + 1, Now) of
		ok ->
			NewState = State#state{last = Now},
			{noreply, NewState, next(ScheduledTime, Interval)};
		{error, Reason} ->
			error_logger:error_report("Failed to create log",
					[{module, ?MODULE}, {file, FileName}, {reason, Reason}]),
			{noreply, State, next(ScheduledTime, Interval)}
	end;
handle_info(timeout, #state{last = Last, interval = Interval,
		schedule = ScheduledTime, type = Type} = State)
		when Type == chf ->
	Now = erlang:system_time(millisecond),
	FileName = ocs_log:iso8601(Now),
	case ocs_log:cdr_log(Type, FileName, Last + 1, Now) of
		ok ->
			NewState = State#state{last = Now},
			{noreply, NewState, next(ScheduledTime, Interval)};
		{error, Reason} ->
			error_logger:error_report("Failed to create log",
					[{module, ?MODULE}, {file, FileName}, {reason, Reason}]),
			{noreply, State, next(ScheduledTime, Interval)}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

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

-spec next(ScheduledTime, Interval) -> Timeout
	when
		ScheduledTime :: calendar:time(),
		Interval :: pos_integer(),
		Timeout :: timeout().
%% @doc Calculate time until next scheduled rotation.
%% @hidden
next(ScheduledTime, Interval) ->
	{Date, Time} = erlang:universaltime(),
	Today = calendar:date_to_gregorian_days(Date),
	Days = Interval div 1440,
	ScheduledDay = calendar:gregorian_days_to_date(Today + Days),
	Next = {ScheduledDay, ScheduledTime},
	Now = calendar:datetime_to_gregorian_seconds({Date, Time}),
	Seconds = case calendar:datetime_to_gregorian_seconds(Next) of
		Scheduled when Scheduled =:= Now ->
			Interval;
		Scheduled when Scheduled < Now ->
			Interval - ((Now - Scheduled) rem Interval);
		Scheduled when Scheduled > Now ->
			(Scheduled - Now) rem Interval
	end,
	case Seconds of
		0 ->
			Interval * 1000;
		N ->
			N * 1000
	end.

