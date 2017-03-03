%%% ocs_log_rotate_server.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_log_rotate_server API
-export([]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state, {}).
-type state() :: #state{}.

-define(USAGE_LOG, usage_log).
-define(WAIT_TIME, 60000).

%%----------------------------------------------------------------------
%%  The ocs_log_rotate_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_log_rotate_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args :: [term()]) ->
	{ok, State :: state()}
			| {ok, State :: state(), Timeout :: timeout()}
			| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init(_Args) ->
	process_flag(trap_exit, true),
	{ok, #state{}, 0}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()},
		State :: state()) ->
	{reply, Reply :: term(), NewState :: state()}
			| {reply, Reply :: term(), NewState :: state(), timeout() | hibernate}
			| {noreply, NewState :: state()}
			| {noreply, NewState :: state(), timeout() | hibernate}
			| {stop, Reason :: term(), Reply :: term(), NewState :: state()}
			| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call(_Request, _From, State) ->
	{stop, not_implemented, State}.

-spec handle_cast(Request :: term(), State :: state()) ->
	{noreply, NewState :: state()}
			| {noreply, NewState :: state(), timeout() | hibernate}
			| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(stop, State) ->
	{stop, normal, State}.

-spec handle_info(Info :: timeout | term(), State::state()) ->
	{noreply, NewState :: state()}
			| {noreply, NewState :: state(), timeout() | hibernate}
			| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, State) ->
	{ok, {H, M, _}} = application:get_env(ocs, log_rotate_time),
	case erlang:time() of
		{H, M, _} ->
			log(State);
		_ ->
			{noreply, State, ?WAIT_TIME}
	end.



-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::state()) ->
	any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVsn :: term() | {down, term()}, State :: state(),
		Extra :: term()) ->
	{ok, NewState :: state()} | {error, Reason :: term()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
log(State) ->
	{ok, Directory} = application:get_env(ocs, acct_log_dir),
	{ok, LogPeriod} = application:get_env(ocs, log_rotate_period),
	FileName = Directory ++ "/" ++ atom_to_list(?USAGE_LOG) ++ "_" ++
		ocs_log:iso8601(erlang:system_time(millisecond)),
	Now = erlang:system_time(millisecond),
	Start = Now - LogPeriod,
	End = Now,
	case ocs_log:ipdr_log(FileName, Start, End) of
		ok ->
			{noreply, State, ?WAIT_TIME} ;
		{error, Reason} ->
			error_logger:error_report("Failed to create usage logs", [{module, ?MODULE},
				{failed_at, ocs_log:iso8601(erlang:system_time(millisecond))},
				{reason, Reason}]),
			{noreply, State, ?WAIT_TIME}
	end.


