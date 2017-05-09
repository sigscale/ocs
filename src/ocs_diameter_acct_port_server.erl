%%% ocs_diameter_acct_port_server.erl
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
%%% 	module receives {@link //diameter. diameter} messages on a port assigned
%%% 	for accounting in the {@link //ocs. ocs} application.
%%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%%% 	RFC4006 - Diameter Credit-Control Application</a>
%%%
-module(ocs_diameter_acct_port_server).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_server).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_cc_application_rfc4006.hrl").
-include("ocs.hrl").

-record(state,
		{address :: inet:ip_address(),
		port :: non_neg_integer(),
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				({NAS :: string() | inet:ip_address(), Port :: string(),
				Peer :: string()}), Value :: (Fsm :: pid()))}).

-define(CC_APPLICATION_ID, 4).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_port_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_port_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, State}
			| {ok, State, Timeout}
			| {stop, Reason} | ignore,
		State :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} server.
%% 	Args :: [Sup :: pid(), Module :: atom(), Port :: non_neg_integer(),
%% 	Address :: inet:ip_address()].
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([Address, Port, _Options]) ->
	State = #state{address = Address, port = Port},
	{ok, State}.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(), 
		From :: {Pid, Tag},
		Pid :: pid(), 
		Tag :: any(),
		State :: state(),
		Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
		Reply :: term(),
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
handle_call(shutdown, _From, State) ->
	{stop, normal, ok, State};
handle_call({diameter_request, Caps, Request}, From, State) ->
	request(Request, Caps, From, State).

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(), 
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(_Request, State) ->
	{noreply, State}.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(), 
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info({'EXIT', _Pid, {shutdown, SessionId}},
		#state{handlers = Handlers} = State) ->
	NewHandlers = gb_trees:delete(SessionId, Handlers),
	NewState = State#state{handlers = NewHandlers},
	{noreply, NewState};
handle_info({'EXIT', Fsm, _Reason},
		#state{handlers = Handlers} = State) ->
	Fdel = fun(_F, {Key, Pid, _Iter}) when Pid == Fsm ->
				Key;
			(F, {_Key, _Val, Iter}) ->
				F(F, gb_trees:next(Iter));
			(_F, none) ->
				none
	end,
	Iter = gb_trees:iterator(Handlers),
	case Fdel(Fdel, gb_trees:next(Iter)) of
		none ->
			{noreply, State};
		Key ->
			NewHandlers = gb_trees:delete(Key, Handlers),
			NewState = State#state{handlers = NewHandlers},
			{noreply, NewState}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | term(), 
		State :: state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason,  _State) ->
	ok.

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: (Vsn | {down, Vsn}),
		Vsn :: term(),
		State :: state(), 
		Extra :: term(),
		Result :: {ok, NewState},
		NewState :: state().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec request(Request, Caps, From, State) -> Result
	when
		Request :: term(),
		Caps :: capabilities(),
		From :: {Pid, Tag}, 
		Pid :: pid(),
		Tag :: term(),
		State :: state(),
		Result :: {reply, Reply, NewState},
		Reply:: term(),
		NewState :: state().
%% @doc Handle a received Diameter Accounting packet.
%% @private
request(Request, Caps,  _From, State) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm, _}} = Caps,
	#diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = _SvcContextId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum} = Request,
	try
		Subscriber = case Request#diameter_cc_app_CCR.'Subscription-Id' of
			SubscriptionId when SubscriptionId == undefine; SubscriptionId == [] ->
				case Request#diameter_cc_app_CCR.'User-Name' of
					UserName when UserName == undefined; UserName == [] ->
						throw(no_subscriber_identification_information);
					UserName ->
						UserName
				end;
			SubscriptionId ->
				SubscriptionId
		end,
		request1(RequestType, SId, RequestNum, Subscriber, OHost, ORealm, State)
	catch
		_:_ ->
			send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
					RequestNum, State)
	end.

%% @hidden
request1(?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		SId, RequestNum,Subscriber, OHost, ORealm, State) ->
	case ocs:find_subscriber(Subscriber) of
		{ok, Password, _, _Balance, true}  ->
			case ocs:authorize(Subscriber, Password) of
				{ok, _, _} ->
					send_answer(SId, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
							OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
							RequestNum, State);
				{error, _Reason1} ->
					send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
							RequestNum, State)
			end;
		{error, _Reason} ->
			send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
					RequestNum, State)
	end;
request1(?'DIAMETER_CC_APP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		SId, RequestNum, Subscriber, OHost, ORealm, State) ->
	case ocs:find_subscriber(Subscriber) of
		{ok, _, _, _Balance, true}  ->
			F = fun() ->
				case mnesia:read(subscriber, Subscriber, write) of
					[#subscriber{disconnect = false} = Entry] ->
						NewEntry = Entry#subscriber{disconnect = true},
						mnesia:write(subscriber, NewEntry, write);
					[#subscriber{disconnect = true}] ->
						ok
				end
			end,
			case mnesia:transaction(F) of
				{atomic, ok} ->
					send_answer(SId, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
							OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
							RequestNum, State);
				{aborted, Reason} ->
					error_logger:error_report(["Failed to disconnect subscriber",
							{subscriber, Subscriber}, {origin_host, OHost},
							{origin_realm, ORealm},{session, SId}, {state, State},
							{reason, Reason}]),
					send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
						OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
							RequestNum, State)
			end;
		{error, _Reason} ->
			send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
					RequestNum, State)
	end.

-spec send_answer(SessionId, ResultCode, OriginHost, OriginRealm,
		AuthAppId, RequestType, RequestNum, State) -> Result
			when
				SessionId :: string(),
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				AuthAppId :: integer(),
				RequestType :: integer(),
				RequestNum :: integer(),
				Result :: {reply, Reply, State},
				State :: state(),
				Reply :: #diameter_cc_app_CCA{}.
%% @doc Send CCA to Diameter client indicating a successful operation.
%% @hidden
send_answer(SId, ResultCode, OHost, ORealm, AuthAppId, RequestType,
		RequestNum, State) ->
	Reply = #diameter_cc_app_CCA{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = AuthAppId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	{reply, Reply, State}.

-spec send_error(SessionId, ResultCode, OriginHost, OriginRealm,
		AuthAppId, RequestType, RequestNum, State) -> Result
			when
				SessionId :: string(),
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				AuthAppId :: integer(),
				RequestType :: integer(),
				RequestNum :: integer(),
				State :: state(),
				Result :: {reply, Reply, State},
				Reply :: #diameter_cc_app_CCA{}.
%% @doc Send CCA to Diameter client indicating a operation faliure.
%% @hidden
send_error(SId, ResultCode, OHost, ORealm, AuthAppId, RequestType,
		RequestNum, State) ->
	Reply = #diameter_cc_app_CCA{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = AuthAppId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	{reply, Reply, State}.

