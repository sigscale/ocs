%%% ocs_event.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 - 2025 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_event. gen_event} behaviour callback
%%% 	module implements an event handler of the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs_event).
-copyright('Copyright (c) 2020 - 2025 SigScale Global Inc.').

-behaviour(gen_event).
-include("ocs.hrl").

%% export the ocs_event API
-export([notify/3]).

%% export the callbacks needed for gen_event behaviour
-export([init/1, handle_call/2, handle_event/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
		{fsm :: pid(),
		id :: string(),
		category :: atom()}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The ocs_event API
%%----------------------------------------------------------------------

-spec notify(EventType, EventPayLoad, Category) -> ok
	when
		EventType :: create_bucket | delete_bucket | charge | depleted
				| accumulated | create_product | delete_product
				| create_service | delete_service | create_offer | delete_offer
				| create_resource | delete_resource | log_acct,
		EventPayLoad :: #bucket{} | #product{} | #service{} | #offer{}
				| {Table, #gtt{}} | #resource{} | [#adjustment{}] | [#acc_balance{}]
				| ocs_log:acct_event(),
		Table :: atom(),
		Category :: balance | product | service | resource | usage.
%% @doc Send a notification event.
%%
%% The `EventPayload' should contain the entire new resource (create),
%% the updated attributes only (attributeValueChange) or only
%% `id' and `href' (remove).
notify(EventType, EventPayLoad, Category) ->
	catch gen_event:notify(?MODULE, {EventType, EventPayLoad, Category}),
	ok.

%%----------------------------------------------------------------------
%%  The ocs_event gen_event callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, State}
			| {ok, State, hibernate}
			| {error, State :: term()},
		State :: state().
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_event:init/1
%% @private
%%
init([Fsm, Id, Category] = _Args) ->
	{ok, #state{fsm = Fsm, id = Id, category = Category}}.

-spec handle_event(Event, State) -> Result
	when
		Event :: term(),
		State :: state(),
		Result :: {ok, NewState}
				| {ok, NewState, hibernate}
				| {swap_handler, Args1, NewState, Handler2, Args2}
				| remove_handler,
		NewState :: state(),
		Args1 :: term(),
		Args2 :: term(),
		Handler2 :: Module2 | {Module2, Id},
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_event:handle_event/2.
%% 	gen_event:notify/2, gen_event:sync_notify/2}.
%% @private
%%
handle_event({_Type, _Resource, Category} = Event,
		#state{fsm = Fsm, category = Category} = State) ->
	gen_fsm:send_event(Fsm, Event),
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

-spec handle_call(Request, Fsm) -> Result
	when
		Request :: term(),
		Fsm :: pid(),
		Result :: {ok, Reply :: term(), NewFsm :: pid()}
			| {ok, Reply :: term(), NewFsm :: pid(), hibernate}
			| {swap_handler, Reply :: term(), Args1 :: term(), NewFsm :: pid(),
				Handler2 :: Module2 | {Module2, Id}, Args2 :: term()}
			| {remove_handler, Reply :: term()},
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_event:call/3.
%% 	gen_event:call/3,4}.
%% @see //stdlib/gen_event:handle_call/3
%% @private
%%
handle_call(_Request, _Fsm) ->
	{remove_handler, not_implementedd}.

-spec handle_info(Info, Fsm) -> Result
	when
		Info :: term(),
		Fsm :: pid(),
		Result :: {ok, NewState :: term()}
			| {ok, NewState :: term(), hibernate}
			| {swap_handler, Args1 :: term(), NewState :: term(),
			Handler2, Args2 :: term()} | remove_handler,
		Handler2 :: Module2 | {Module2, Id},
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_event:handle_info/2
%% @private
%%
handle_info(_Info, _Fsm) ->
	remove_handler.

-spec terminate(Arg, Fsm) -> term()
	when
		Arg :: Args :: term() | {stop, Reason :: term()} | {error, term()}
				| stop | remove_handler | {error,{'EXIT', Reason :: term()}},
      Fsm :: pid().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_event:terminate/3
%% @private
%%
terminate(_Reason, _Fsm) ->
	ok.

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: term() | {down, term()},
		State :: term(),
		Extra :: term(),
		Result :: {ok, NewState :: term()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_event:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

