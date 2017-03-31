%%% ocs_server.erl
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module implements a service access point (SAP) for the public API of the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs_server).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_server API
-export([]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state, {sup :: pid()}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The ocs_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_server gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: state()}
			| {ok, State :: state(), Timeout :: timeout()}
			| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([Sup] = _Args) ->
	process_flag(trap_exit, true),
	{ok, #state{sup = Sup}}.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(), 
		From :: {pid(), Tag :: any()},
		State :: state(),
		Result :: {reply, Reply :: term(), NewState :: state()}
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
handle_call({start, radius, auth, Address, Port, LogRotateTime, Options}, _From,
		#state{sup = Sup} = State) ->
	Children = supervisor:which_children(Sup),
	{_, AuthSup, _, _} = lists:keyfind(ocs_radius_auth_sup, 1, Children),
	Result = supervisor:start_child(AuthSup, [[Address, Port, LogRotateTime,
		Options]]),
	{reply, Result, State};
handle_call({start, radius, acct, Address, Port, LogRotateTime, Options}, _From,
		#state{sup = Sup} = State) ->
	Children = supervisor:which_children(Sup),
	{_, AcctSup, _, _} = lists:keyfind(ocs_radius_acct_top_sup, 1, Children),
	Result = supervisor:start_child(AcctSup, [[Address, Port, LogRotateTime,
		Options]]),
	{reply, Result, State};
handle_call({start, diameter, auth, Address, Port, LogRotateTime, Options}, _From,
		#state{sup = Sup} = State) ->
	Children = supervisor:which_children(Sup),
	{_, AuthSup, _, _} = lists:keyfind(ocs_diameter_auth_sup, 1, Children),
	Result = supervisor:start_child(AuthSup, [[Address, Port, LogRotateTime,
		Options]]),
	{reply, Result, State}.

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(), 
		State :: state(),
		Result :: {noreply, NewState :: state()}
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

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(), 
		State:: state(),
		Result :: {noreply, NewState :: state()}
			| {noreply, NewState :: state(), timeout() | hibernate}
			| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(_Info, State) ->
	{stop, not_implemented, State}.

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
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

