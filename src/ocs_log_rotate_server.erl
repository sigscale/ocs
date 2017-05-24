
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
%%
init([Rotate] = _Args) when is_integer(Rotate), Rotate > 0 ->
	process_flag(trap_exit, true),
	Interval = case round_up(Rotate) of
		Rotate ->
			Rotate;
		I ->
			error_logger:warning_report(["Using sane log rotation interval",
					{rotate, Rotate}, {interval, I}]),
			I
	end,
	{ok, Directory} = application:get_env(ipdr_log_dir),
	State = #state{interval = Interval, dir = Directory},
	case file:make_dir(Directory) of
		ok ->
			{ok, State, wait(Interval)};
		{error, eexist} ->
			{ok, State, wait(Interval)};
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
handle_info(timeout, #state{interval = Interval, dir = Directory} = State) ->
	Time = erlang:system_time(?MILLISECOND),
	FileName = Directory ++ "/" ++ ocs_log:iso8601(Time),
	{Start, End} = previous(Interval),
	case ocs_log:ipdr_log(FileName, Start, End) of
		ok ->
			{noreply, State, wait(Interval)};
		{error, Reason} ->
			error_logger:error_report("Failed to create log",
					[{module, ?MODULE}, {file, FileName}, {reason, Reason}]),
			{noreply, State, wait(Interval)}
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

-spec wait(Interval) -> Timeout
	when
		Interval :: pos_integer(),
		Timeout :: timeout().
%% @doc Calculate time until start of next interval.
%% @hidden
wait(Interval) ->
	{Date, Time} = erlang:universaltime(),
	Midnight = calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}),
	Now  = calendar:datetime_to_gregorian_seconds({Date, Time}),
	I = Interval * 60,
	Guard = 30,
	(I - ((Now - Midnight) rem I) + Guard) * 1000.

-spec round_up(Rotate) -> Interval
	when
		Rotate :: pos_integer(),
		Interval :: pos_integer().
%% @doc Rotate must be a divisor of one day.
%% @hidden
round_up(Rotate) when Rotate > 1440 ->
	1440;
round_up(Rotate) when (1440 rem Rotate) =:= 0 ->
	Rotate;
round_up(Rotate) ->
	round_up(Rotate + 1).

-spec previous(Interval) -> {Start, End}
	when
		Interval :: pos_integer(),
		Start :: calendar:datetime(),
		End :: calendar:datetime().
%% @doc Find start of previous interval.
%% @hidden
previous(Interval) when is_integer(Interval) ->
	{Date, Time} = erlang:universaltime(),
	Midnight = calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}),
	Now = calendar:datetime_to_gregorian_seconds({Date, Time}),
	previous(Interval, Date, Midnight, Now).
%% @hidden
previous(Interval, _, Midnight, Now)
		when Now - Midnight < (Interval * 60) ->
	S = Midnight - (Interval * 60),
	{SD, {SH, SM, _}} = calendar:gregorian_seconds_to_datetime(S),
	{{SD, {SH, SM, 0}}, {SD, {23, 59, 59}}};
previous(Interval, Date, Midnight, Now) ->
	I = Interval * 60,
	P = Now - I,
	Start = P - ((P - Midnight) rem I),
	End = Start + I - 1,
	{_, {SH, SM, _}} = calendar:gregorian_seconds_to_datetime(Start),
	{_, {EH, EM, _}} = calendar:gregorian_seconds_to_datetime(End),
	{{Date, {SH, SM, 0}}, {Date, {EH, EM, 59}}}.

