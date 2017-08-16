%%% ocs_rest_pagination_server.erl
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
-module(ocs_rest_pagination_server).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_rest_pagination_server API
-export([start_link/1]).
-export_type([continuation/0]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-opaque continuation() :: start | disk_log:continuation().
-record(state,
		{etag :: string(),
		timeout :: pos_integer(),
		module :: atom(),
		function :: atom(),
		args :: list(),
		cont = start :: continuation(),
		buffer = [] :: [tuple()],
		pointer = 0 :: integer()}).
-type state() :: #state{}.

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The ocs_rest_pagination_server API
%%----------------------------------------------------------------------

-spec start_link(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, PageServer} | {error, Reason},
		PageServer :: pid(),
		Reason :: term().
%% @doc Start a handler for a sequence of REST range requests.
start_link(Args) ->
	Etag = integer_to_list(erlang:system_time(?MILLISECOND)) ++ "-"
			++ integer_to_list(erlang:unique_integer([positive])),
	case gen_server:start_link({global, Etag}, ?MODULE, [Etag | Args], []) of
		{ok, Child} ->
			{ok, Child, Etag};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs_rest_pagination_server gen_server call backs
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
init([Etag, M, F, A] = _Args) when is_atom(M), is_atom(F), is_list(A) ->
	{ok, Timeout} = application:get_env(rest_page_timeout),
	process_flag(trap_exit, true),
	State = #state{etag = Etag, module = M, function = F,
			args = A, timeout = Timeout},
	{ok, State, Timeout}.

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
handle_call({StartRange, EndRange}, _From,
		#state{module = Module, function = Function, args = Args,
		cont = Cont, timeout = Timeout} = State) ->
	case apply(Module, Function, [Cont | Args]) of
		{eof, Items} ->
			{reply, {ok, Items}, State, Timeout};
		{error, Reason} ->
			{reply, {error, Reason}, State, Timeout};
		{Cont2, Items} ->
			{reply, {ok, Items}, State, Timeout}
	end.

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
handle_info(timeout, State) ->
	{stop, shutdown, State}.

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

