%%% ocs_event_log.erl
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
%%% @doc This {@link //stdlib/gen_event. gen_event} behaviour callback
%%% 	module implements an event handler of the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs_event_log).
-copyright('Copyright (c) 2022 SigScale Global Inc.').

-behaviour(gen_event).

%% export the private API
-export([pending_result/3, established_result/1]).
%% export the ocs_event_log API
-export([notify/2]).

%% export the callbacks needed for gen_event behaviour
-export([init/1, handle_call/2, handle_event/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
		{profile :: atom(),
		callback :: string(),
		fsm :: pid(),
		type :: atom(),
		established = false :: boolean(),
		pending = false :: boolean(),
		options :: [{atom(), term()}]}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The ocs_event_log API
%%----------------------------------------------------------------------

-spec notify(EventType, EventPayLoad) -> ok
	when
		EventType :: ocs_auth | ocs_acct,
		EventPayLoad :: ocs_log:auth_event() | ocs_log:acct_event().
%% @doc Send a notification event.
%%
%% The `EventPayload' should contain the entire new resource (create),
%% the updated attributes only (attributeValueChange) or only
%% `id' and `href' (remove).
notify(EventType, EventPayLoad) ->
	catch gen_event:notify(?MODULE, {EventType, EventPayLoad}),
	ok.

%%----------------------------------------------------------------------
%%  The ocs_event_log gen_event callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, State}
			| {ok, State, hibernate}
			| {error, State :: term()}.
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_event:init/1
%% @private
%%
init([Fsm, Profile, Callback, Options] = _Args) ->
	{ok, #state{fsm = Fsm, profile = Profile,
			callback = Callback, options = Options}}.

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
%% @doc Handle a request sent using {@link //stdlib/genevent:handle_event/2.
%% 	gen_event:notify/2, gen_event:sync_notify/2}.
%% @private
%%
handle_event(_Event, #state{established = false, pending = true} = State) ->
	{ok, State};
handle_event({Type, Resource} = _Event, #state{profile = Profile,
		callback = Callback, options = Options} = State) ->
	Headers = [{"accept", "application/json"}],
	Body = case Type of
		ocs_acct ->
			lists:flatten(mochijson:encode(ocs_log:acct_to_ecs(Resource)));
		ocs_auth ->
			lists:flatten(mochijson:encode(ocs_log:auth_to_ecs(Resource)))
	end,
	Request = {Callback, Headers, "application/json", Body},
	F = fun({Key, _Value}) when Key == timeout; Key == connect_timeout;
			Key == ssl; Key == essl; Key == autoredirect; Key == proxy_auth;
			Key == version; Key == relaxed ->
				true;
			({_Key, _Value}) ->
				false
	end,
	HttpOptions = lists:filter(F, Options),
	case lists:keyfind(timeout, 1, HttpOptions) of
		{timeout, _Val} ->
			handle_event1(Request, Profile, HttpOptions, State);
		false ->
			NewHttpOptions = HttpOptions ++ [{timeout, 4000}],
			handle_event1(Request, Profile, NewHttpOptions, State)
	end.
%% @hidden
handle_event1(Request, Profile, HttpOptions,
		#state{established = false, pending = false} = State) ->
	NewState = State#state{established = false, pending = true},
	MFA = {?MODULE, pending_result, [self(), ?MODULE]},
	Options = [{sync, false}, {receiver, MFA}],
	case httpc:request(post, Request, HttpOptions, Options, Profile) of
		{ok, RequestId} when is_reference(RequestId) ->
			{ok, NewState};
		{error, _Reason} ->
			remove_handler
	end;
handle_event1(Request, Profile, HttpOptions,
		#state{established = true, pending = false} = State) ->
	MFA = {?MODULE, established_result, []},
	Options = [{sync, false}, {receiver, MFA}],
	case httpc:request(post, Request, HttpOptions, Options, Profile) of
		{ok, RequestId} when is_reference(RequestId) ->
			{ok, State};
		{error, _Reason} ->
			remove_handler
	end.

-spec handle_call(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {ok, Reply :: term(), NewState}
			| {ok, Reply :: term(), NewState, hibernate}
			| {swap_handler, Reply :: term(), Args1 :: term(), NewState,
				Handler2 :: Module2 | {Module2, Id}, Args2 :: term()}
			| {remove_handler, Reply :: term()},
		NewState :: state(),
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_event:call/3.
%% 	gen_event:call/3,4}.
%% @see //stdlib/gen_event:handle_call/3
%% @private
%%
handle_call({RequestId,
		{{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, _Body}},
		#state{established = false, pending = true} = State)
		when StatusCode >= 200, StatusCode  < 300 ->
	error_logger:info_report(["Event shipping established",
			{module, ?MODULE}, {state, State}, {pid, self()},
			{status, StatusCode}, {reason, ReasonPhrase},
			{request, RequestId}]),
	{ok, ok, State#state{established = true, pending = false}};
handle_call({RequestId,
		{{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, _Body}}, State) ->
	error_logger:warning_report(["Pending event shipping failed",
			{module, ?MODULE}, {state, State}, {pid, self()},
			{status, StatusCode}, {reason, ReasonPhrase},
			{request, RequestId}]),
	{remove_handler, {error, ReasonPhrase}};
handle_call({RequestId, {error, Reason}}, State) ->
	error_logger:warning_report(["Pending event shipping failed",
			{module, ?MODULE}, {state, State}, {pid, self()},
			{error, Reason}, {request, RequestId}]),
	{remove_handler, {error, Reason}}.

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

-spec terminate(Arg, State) -> term()
	when
		Arg :: Args :: term() | {stop, Reson :: term()} | {error, term()}
				| stop | remove_handler | {error,{'EXIT', Reason :: term()}},
      State :: state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_event:terminate/2
%% @private
%%
terminate(_Arg, _State) ->
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

-spec pending_result(ReplyInfo, EventManager, Handler) -> ok
	when
		ReplyInfo :: tuple(),
		EventManager :: pid(),
		Handler :: gen_event_log.
%% @doc Handle async result of httpc:request/5 while session pending.
%% @private
pending_result(ReplyInfo, EventManager, Handler) ->
	case gen_event:call(EventManager, Handler, ReplyInfo) of
		ok ->
			ok;
		{error, _Reason} ->
			gen_event:delete_handler(EventManager, Handler, [])
	end.

-spec established_result(ReplyInfo) -> ok
	when
		ReplyInfo :: tuple().
%% @doc Handle async result of httpc:request/5 while session established.
%% @private
established_result({_RequestId,
		{{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}})
		when StatusCode >= 200, StatusCode  < 300 ->
	ok;
established_result({RequestId,
		{{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, _Body}}) ->
	error_logger:warning_report(["Established event shipping failed",
			{module, ?MODULE}, {pid, self()}, {status, StatusCode},
			{reason, ReasonPhrase}, {request, RequestId}]),
	gen_event:delete_handler(ocs_event_log, ocs_event_log, []);
established_result({RequestId, {error, Reason}}) ->
	error_logger:warning_report(["Established event shipping failed",
			{module, ?MODULE}, {pid, self()},
			{error, Reason}, {request, RequestId}]),
	gen_event:delete_handler(ocs_event_log, ocs_event_log, []).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

