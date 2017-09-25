%%% ocs_diameter_acct_port_server.erl
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module receives {@link //diameter. diameter} messages on a port assigned
%%% 	for accounting in the {@link //ocs. ocs} application.
%%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%%% 	RFC4006 - DIAMETER Credit-Control Application</a>
%%%
-module(ocs_diameter_acct_port_server).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_cc_application_rfc4006.hrl").
-include("ocs.hrl").

-record(state,
		{acct_sup :: pid(),
		 disc_sup :: undefined | pid(),
		address :: inet:ip_address(),
		port :: non_neg_integer(),
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				(SessionId :: string()), Value :: (Fsm :: pid()))}).

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
init([AcctSup, Address, Port, _Options]) ->
	State = #state{address = Address, port = Port, acct_sup = AcctSup},
	case ocs_log:acct_open() of
		ok ->
			process_flag(trap_exit, true),	
			{ok, State, 0};
		{error, Reason} ->
			{stop, Reason}
	end.

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
handle_info(timeout, #state{acct_sup = AcctSup} = State) ->
	Children = supervisor:which_children(AcctSup),
	{_, DiscSup, _, _} = lists:keyfind(ocs_diameter_disconnect_fsm_sup,
			1, Children),
	{noreply, State#state{disc_sup = DiscSup}};
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
	ocs_log:acct_close().

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
%% @doc Handle a received DIAMETER Accounting packet.
%% @private
request(Request, Caps,  _From, State) ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	#diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = _SvcContextId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum} = Request,
	try
		Subscriber = case Request#diameter_cc_app_CCR.'Subscription-Id' of
			SubscriptionId when SubscriptionId == undefine; SubscriptionId == [] ->
				case Request#diameter_cc_app_CCR.'User-Name' of
					[UserName] when UserName == undefined; UserName == [] ->
						throw(no_subscriber_identification_information);
					[UserName] ->
						UserName
				end;
			SubscriptionId ->
				SubscriptionId
		end,
		case ocs:find_subscriber(Subscriber) of
			{ok, #subscriber{password = Password, buckets = Buckets,
					enabled = true}} ->
				Balance = get_balance(Buckets),
				case ocs:authorize(Subscriber, Password) of
					{ok, _, _} ->
						request1(RequestType, Request, SId, RequestNum, Subscriber,
								Balance, OHost, DHost, ORealm, DRealm, State);
					{error, _} ->
						{Reply, NewState} = generate_diameter_error(Request, SId, Subscriber,
								Balance, ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
								OHost, ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
						{reply, Reply, NewState}
				end;
			{error, _} ->
				{Reply, NewState} = generate_diameter_error(Request, SId, undefined,
					undefined, ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
					OHost, ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
				{reply, Reply, NewState}
		end
	catch
		_:_ ->
			{Reply1, NewState1} = generate_diameter_error(Request, SId, undefined,
					undefined, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
			{reply, Reply1, NewState1}
	end.

%% @hidden
request1(?'DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		Request, SId, RequestNum, Subscriber, Balance, OHost, _DHost, ORealm,
		_DRealm, State) ->
	{Reply, NewState} = generate_diameter_answer(Request, SId, Subscriber,
			Balance, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OHost, ORealm,
			?CC_APPLICATION_ID, RequestType, RequestNum, State),
	{reply, Reply, NewState};
request1(?'DIAMETER_CC_APP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		Request, SId, RequestNum, Subscriber, Balance, OHost, DHost, ORealm,
		DRealm, State) ->
	try
		[UsedUnits] = Request#'diameter_cc_app_CCR'.'Used-Service-Unit',
		#'diameter_cc_app_Used-Service-Unit'{'CC-Total-Octets' = Total,
				'CC-Input-Octets' = In, 'CC-Output-Octets' = Out} = UsedUnits,
		Usage = case Total of
			[T] when is_integer(T) ->
				T;
			_ ->
				case {In, Out} of
					{[I], [O]} when is_integer(I), is_integer(O) ->
						I + O;
					_ ->
						throw(no_diameter_accounting_usage_information)
				end
		end,
		case decrement_balance(Subscriber, Usage) of
			{ok, OverUsed, false} when OverUsed =< 0 ->
				{Reply, NewState} = generate_diameter_answer(Request, SId, Subscriber,
						0, ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED', OHost,
						ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
				NewState1 = start_disconnect(ocs_diameter_acct_service,
						ocs_diameter_base_application, SId, OHost, DHost, ORealm, DRealm,
						?CC_APPLICATION_ID, NewState),
				{reply, Reply, NewState1};
			{ok, _SufficientBalance, _Flags} ->
				{Reply, NewState} = generate_diameter_answer(Request, SId, Subscriber,
						Balance, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OHost, ORealm,
						?CC_APPLICATION_ID, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{error, not_found} ->
				error_logger:warning_report(["diameter accounting subscriber not found",
						{module, ?MODULE}, {subscriber, Subscriber},
						{origin_host, OHost}]),
				{Reply, NewState} = generate_diameter_error(Request, SId, Subscriber,
						Balance, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
						ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
				{reply, Reply, NewState}
		end
	catch
		_:_ ->
			{Reply1, NewState0} = generate_diameter_error(Request, SId, Subscriber,
					Balance, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
			{reply, Reply1, NewState0}
	end;
request1(?'DIAMETER_CC_APP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		Request, SId, RequestNum, Subscriber, Balance, OHost, _DHost, ORealm,
		_DRealm, State) ->
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
			{Reply, NewState} = generate_diameter_answer(Request, SId, Subscriber,
					Balance, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OHost, ORealm,
					?CC_APPLICATION_ID, RequestType, RequestNum, State),
			{reply, Reply, NewState};
		{aborted, Reason} ->
			error_logger:error_report(["Failed to disconnect subscriber",
					{subscriber, Subscriber}, {origin_host, OHost},
					{origin_realm, ORealm},{session, SId}, {state, State},
					{reason, Reason}]),
			{Reply, NewState} = generate_diameter_error(Request, SId, Subscriber,
					Balance, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, ?CC_APPLICATION_ID, RequestType, RequestNum, State),
			{reply, Reply, NewState}
	end.

-spec generate_diameter_answer(Request, SessionId, Subscriber, Balance,
		ResultCode, OriginHost, OriginRealm, AuthAppId, RequestType, RequestNum,
		State) -> Result
			when
				Request :: #diameter_cc_app_CCR{},
				SessionId :: string(),
				Subscriber :: string() | binary(),
				Balance :: non_neg_integer(),
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				AuthAppId :: integer(),
				RequestType :: integer(),
				RequestNum :: integer(),
				Result :: {Reply, State},
				State :: state(),
				Reply :: #diameter_cc_app_CCA{}.
%% @doc Send CCA to DIAMETER client indicating a successful operation.
%% @hidden
generate_diameter_answer(Request, SId, _Subscriber, Balance, ResultCode, OHost,
		ORealm, AuthAppId, RequestType, RequestNum, #state{address = Address,
		port = Port} = State) ->
	GrantedUnits = #'diameter_cc_app_Granted-Service-Unit'{'CC-Total-Octets' = [Balance]},
	Reply = #diameter_cc_app_CCA{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = AuthAppId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum, 'Granted-Service-Unit' = [GrantedUnits]},
	Server = {Address, Port},
	ok = ocs_log:acct_log(diameter, Server, accounting_event_type(RequestType),
			Request),
	{Reply, State}.

-spec generate_diameter_error(Request, SessionId, Subscriber, Balance, ResultCode,
		OriginHost, OriginRealm, AuthAppId, RequestType, RequestNum, State) -> Result
			when
				Request ::#diameter_cc_app_CCR{},
				SessionId :: string(),
				Subscriber :: undefined | string() | binary(),
				Balance :: undefined | integer(),
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				AuthAppId :: integer(),
				RequestType :: integer(),
				RequestNum :: integer(),
				State :: state(),
				Result :: {Reply, State},
				Reply :: #diameter_cc_app_CCA{}.
%% @doc Send CCA to DIAMETER client indicating a operation faliure.
%% @hidden
generate_diameter_error(Request, SId, _Subscriber, _Balance, ResultCode, OHost,
		ORealm, AuthAppId, RequestType, RequestNum, #state{address = Address,
		port = Port} = State) ->
	Reply = #diameter_cc_app_CCA{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = AuthAppId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	Server = {Address, Port},
	ok = ocs_log:acct_log(diameter, Server, accounting_event_type(RequestType), Request),
	{Reply, State}.

-spec decrement_balance(Subscriber, Usage) -> Result
	when
		Subscriber :: string() | binary(),
		Usage :: non_neg_integer(),
		Result :: {ok, NewBalance, DiscFlag} | {error, Reason },
		NewBalance :: integer(),
		DiscFlag :: boolean(),
		Reason :: not_found | term().
%% @doc Decrements subscriber's current balance
decrement_balance(Subscriber, Usage) when is_list(Subscriber) ->
	decrement_balance(list_to_binary(Subscriber), Usage);
decrement_balance(Subscriber, Usage) when is_binary(Subscriber),
		Usage >= 0 ->
	F = fun() ->
		case mnesia:read(subscriber, Subscriber, write) of
			[#subscriber{buckets = Buckets, disconnect = Flag} = Entry] ->
				UpdatedBuckets = update_buckets(Buckets, Usage),
				NewEntry = Entry#subscriber{buckets = UpdatedBuckets},
				mnesia:write(subscriber, NewEntry, write),
				NewBalance = get_balance(UpdatedBuckets),
				{NewBalance, Flag};
			[] ->
				throw(not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, {NewBalance, Flag}} ->
			{ok, NewBalance, Flag};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			error_logger:error_report(["Failed to decrement balance",
					{error, Reason}, {subscriber, Subscriber}]),
			{error, Reason}
end.

-spec accounting_event_type(RequestType) -> EventType
	when
	RequestType :: 1..4,
	EventType :: start | interim | stop | event.
%% @doc Converts CC-Request-Type integer value to a readable atom.
accounting_event_type(RequestType) ->
	case RequestType of 
		1 ->
			start;
		2 ->
			interim;
		3 ->
			stop;
		4 ->
			event
	end.

-spec start_disconnect(Svc, AppAlias, SessionId, OHost, DHost, ORealm,
		DRealm, AuthAppId, State) -> NewState
	when
		Svc :: term(),
		AppAlias :: term(),
		SessionId :: string(),
		OHost :: string() | binary(),
		DHost :: string() | binary(),
		ORealm :: string() | binary(),
		DRealm :: string() | binary(),
		AuthAppId :: integer(),
		State :: #state{},
		NewState :: #state{}.
%% @doc Start a disconnect_fsm to send DIAMETER Abort-Session-Request and
%% store disconnect_fsm pid() in state.
%% @hidden
start_disconnect(Svc, Alias, SessionId, OHost, DHost, ORealm, DRealm,
		AuthAppId, #state{handlers = Handlers , disc_sup = DiscSup} = State) ->
	case gb_trees:lookup(SessionId,  Handlers) of
		{value, _DiscPid} ->
			State;
		none ->
			DiscArgs = [Svc, Alias, SessionId, OHost, DHost, ORealm,
					DRealm, AuthAppId],
			StartArgs = [DiscArgs, []],
			case supervisor:start_child(DiscSup, StartArgs) of
				{ok, DiscFsm} ->
					link(DiscFsm),
					NewHandlers = gb_trees:insert(SessionId, DiscFsm, Handlers),
					State#state{handlers = NewHandlers};
				{error, Reason} ->
					error_logger:error_report(["Failed to initiate session disconnect function",
							{module, ?MODULE}, {session_id, SessionId}, {destion_host, DHost},
							{error, Reason}]),
					State
			end
	end.

-spec get_balance(Buckets) ->
		Balance when
	Buckets :: [#bucket{}],
	Balance :: integer().
%% get the availabel balance form buckets
get_balance([]) ->
	0;
get_balance(Buckets) ->
	get_balance1(Buckets, 0).
%% @hidden
get_balance1([], Balance) ->
	Balance;
get_balance1([#bucket{remain_amount = #remain_amount{amount = RemAmnt}}
		| Tail], Balance) ->
	get_balance1(Tail, RemAmnt + Balance).

-spec update_buckets(Buckets, Usage) ->
		UpdatedBuckets when
	Buckets :: [#bucket{}],
	Usage :: integer(),
	UpdatedBuckets :: [#bucket{}].
%% @doc Decrement bucket balances and return new available buckets
update_buckets([], _Usage) ->
	[];
update_buckets([#bucket{remain_amount = #remain_amount{amount = RemAmount} = RM} = Bucket |
		Tail], Usage) ->
	RemUsage = RemAmount - Usage,
	case RemUsage of
		RU when RU < 0 ->
			update_buckets(Tail, RU);
		_ ->
			UpdatedBucket = Bucket#bucket{remain_amount = RM#remain_amount{amount = RemUsage}},
			[UpdatedBucket | Tail]
	end.
