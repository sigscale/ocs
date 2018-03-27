
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_log_rotate_server API
-export([]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
			{interval :: pos_integer(),
			schedule :: calendar:time(),
			dir :: string()}).
-type state() :: #state{}.

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

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
			| {stop, Reason} | ignore,
		State :: state(),
		Timeout :: timeout(),
		Reason :: term().
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%% @todo Allow intervals shorter than one day.
init([Type, ScheduledTime, Interval] = _Args) when
		is_tuple(ScheduledTime), size(ScheduledTime) =:= 3,
		is_integer(Interval), Interval > 0 ->
	process_flag(trap_exit, true),
	NewInterval = case round_up(Interval) of
		Interval ->
			Interval;
		I ->
			error_logger:warning_report(["Using sane log rotation interval",
					{rotate, Interval}, {interval, I}]),
			I
	end,
	{ok, Directory} = application:get_env(ipdr_log_dir),
	case file:make_dir(Directory) of
		ok ->
			init1(Directory, NewInterval, ScheduledTime, Type);
		{error, eexist} ->
			init1(Directory, NewInterval, ScheduledTime, Type);
		{error, Reason} ->
			{stop, Reason}
	end.
%% @hidden
init1(Directory, NewInterval, ScheduledTime, Type) ->
	Directory1 = Directory ++ "/" ++ atom_to_list(Type),
	State = #state{interval = NewInterval,
			schedule = ScheduledTime, dir = Directory1},
	case file:make_dir(Directory1) of
		ok ->
			{ok, State, wait(ScheduledTime, NewInterval)};
		{error, eexist} ->
			{ok, State, wait(ScheduledTime, NewInterval)};
		{error, Reason} ->
			{stop, Reason}
	end.

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
handle_call(_Request, _From, State) ->
	{stop, not_implemented, State}.

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
handle_info(timeout, #state{interval = Interval,
		schedule = ScheduledTime, dir = Directory} = State) ->
	Time = erlang:system_time(?MILLISECOND),
	FileName = Directory ++ "/" ++ ocs_log:iso8601(Time),
	{Start, End} = previous(Interval),
	case ocs_log:ipdr_log(voip, FileName, Start, End) of
		ok ->
			{noreply, State, wait(ScheduledTime, Interval)};
		{error, Reason} ->
			error_logger:error_report("Failed to create log",
					[{module, ?MODULE}, {file, FileName}, {reason, Reason}]),
			{noreply, State, wait(ScheduledTime, Interval)}
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

-spec wait(ScheduledTime, Interval) -> Timeout
	when
		ScheduledTime :: calendar:time(),
		Interval :: pos_integer(),
		Timeout :: timeout().
%% @doc Calculate time until next scheduled rotation.
%% @hidden
wait(ScheduledTime, Interval) ->
	{Date, Time} = erlang:universaltime(),
	Today = calendar:date_to_gregorian_days(Date),
	Period = Interval div 1440,
	ScheduleDay = calendar:gregorian_days_to_date(Today + Period),
	Next = {ScheduleDay, ScheduledTime},
	Now = calendar:datetime_to_gregorian_seconds({Date, Time}),
	(calendar:datetime_to_gregorian_seconds(Next) - Now) * 1000.

-spec round_up(Interval) -> Interval
	when
		Interval :: pos_integer(),
		Interval :: pos_integer().
%% @doc Interval must be a divisor of one day.
%% @hidden
round_up(Interval) when Interval =< 1440 ->
	1440;
round_up(Interval) ->
	Days = Interval div 1440,
	Days * 1440.

-spec previous(Interval) -> {Start, End}
	when
		Interval :: pos_integer(),
		Start :: calendar:datetime(),
		End :: calendar:datetime().
%% @doc Find start of previous interval.
%% @hidden
previous(Interval) when is_integer(Interval) ->
	IntervalDays = Interval div 1440,
	{Date, _Time} = erlang:universaltime(),
	Today = calendar:date_to_gregorian_days(Date),
	StartDay = Today - IntervalDays,
	Start = {calendar:gregorian_days_to_date(StartDay), {0, 0, 0}},
	Yesterday = Today - 1,
	End = {calendar:gregorian_days_to_date(Yesterday), {23, 59, 59}},
	{Start, End}.

