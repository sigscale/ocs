%%% ocs_rest_hub_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 - 2026 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module implements {@link //stdlib/gen_event. gen_event} handlers
%%% 	for REST notification callbacks
%%% 	in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_hub_fsm).
-copyright('Copyright (c) 2020 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

-include("ocs.hrl").

%% export the public API
-export([start_link/3, start_link/4]).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([register/3, registered/3]).

%% export the private API
-export([handle_async/2]).

-record(statedata,
		{id :: string(),
		profile :: atom(),
		module :: atom(),
		function :: atom(),
		query :: string(),
		callback :: string(),
		href :: string(),
		authorization :: string() | undefined,
		args :: list() | undefined,
		sync = true :: boolean()}).
-type statedata() :: #statedata{}.
-type state() :: register | registered.

%%----------------------------------------------------------------------
%%  The ocs_rest_hub_fsm API
%%----------------------------------------------------------------------

-spec start_link(Query, Callback, Uri) -> Result
	when
		Query :: string(),
		Callback :: string(),
		Uri :: string(),
		Result :: {ok, HubFsm} | {error, Reason},
		HubFsm :: pid(),
		Reason :: term().
%% @doc Start a hub fsm
start_link(Query, Callback, Uri) ->
	{Id, _} = unique(),
	case gen_statem:start_link({global, Id}, ?MODULE,
			[Id, Query, Callback, Uri], []) of
		{ok, Child} ->
			{ok, Child, Id};
		{error, Reason} ->
			{error, Reason}
	end.

-spec start_link(Query, Callback, Uri, Authorization) -> Result
	when
		Query :: string(),
		Callback :: string(),
		Uri :: string(),
		Authorization :: string(),
		Result :: {ok, HubFsm} | {error, Reason},
		HubFsm :: pid(),
		Reason :: term().
%% @doc Start a hub fsm
start_link(Query, Callback, Uri, Authorization) ->
	{Id, _} = unique(),
	case gen_statem:start_link({global, Id}, ?MODULE,
			[Id, Query, Callback, Uri, Authorization], []) of
		{ok, Child} ->
			{ok, Child, Id};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The ocs_rest_hub_fsm gen_statem call backs
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions].

-spec init(Args) -> Result
	when
		Args :: [term()],
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:init_result(State, Data).
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_statem:init/1
%% @private
%%
init([Id, Query, Callback, Uri] = _Args) ->
	process_flag(trap_exit, true),
	{ok, Profile} = application:get_env(hub_profile),
	Data = #statedata{id = Id, profile = Profile,
			query = Query, callback = Callback, href = Uri ++ Id},
	Action = {next_event, internal, start},
	{ok, register, Data, Action};
init([Id, Query, Callback, Uri, Authorization] = _Args) ->
	process_flag(trap_exit, true),
	{ok, Profile} = application:get_env(hub_profile),
	Data = #statedata{id = Id, profile = Profile,
			query = Query, callback = Callback, href = Uri ++ Id,
			authorization = Authorization},
	Action = {next_event, internal, start},
	{ok, register, Data, Action}.

-spec register(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>register</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/balanceManagement" ++ _} = Data) ->
	register1(balance, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/usageManagement" ++ _} = Data) ->
	register1(usage, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/partyManagement" ++ _} = Data) ->
	register1(user, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/partyRoleManagement" ++ _} = Data) ->
	register1(role, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/productCatalog" ++ _} = Data) ->
	register1(product, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/productInventory" ++ _} = Data) ->
	register1(product, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/resourceInventory" ++ _} = Data) ->
	register1(resource, Data);
register(internal = _EventType, start = _EventContent,
		#statedata{href = "/serviceInventory" ++ _} = Data) ->
	register1(service, Data).
%% @hidden
register1(Category, #statedata{id = Id} = Data) ->
	case gen_event:add_sup_handler(ocs_event,
			{ocs_event, Id}, [self(), Id, Category]) of
		ok ->
			{next_state, registered, Data};
		{'EXIT', Reason} ->
			{stop, Reason}
	end.

-spec registered(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: {Type, Resource, Category},
		Type :: create_bucket | delete_bucket | charge | depleted | accumulated
				| create_product | delete_product | create_service | delete_service
				| create_offer | delete_offer | create_resource | delete_resource
				| log_acct,
		Resource :: #bucket{} | #product{} | #service{} | #offer{} | #resource{}
				| [#adjustment{}] | [#acc_balance{}] | ocs_log:acct_event(),
		Category :: balance | product | service | resource | usage,
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>registered</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
registered(cast = _EventType, EventContent,
		#statedata{query = []} = Data) ->
	send_request(EventContent, Data);
registered(cast = _EventType,
		{_Type, [#acc_balance{} | _] = Resource, _Category} = EventContent,
		#statedata{query = Query} = Data)
		when is_list(Query), length(Query) > 0 ->
	case string:tokens(Query, "&=") of
		["totalBalance.units", Units, "totalBalance.amount.lt", Threshold] ->
			UnitsA = list_to_existing_atom(Units),
			F = fun(#acc_balance{total_balance = [#quantity{units = U}]})
							when U == UnitsA ->
						true;
					(_) ->
						false
			end,
			case lists:filter(F, Resource) of
				[] ->
					keep_state_and_data;
				[#acc_balance{total_balance = [#quantity{amount = TotalBalance}]}]
						when TotalBalance < Threshold ->
					send_request(EventContent, Data);
				_ ->
					keep_state_and_data
			end;
		_ ->
			keep_state_and_data
	end;
registered(cast = _EventType,
		{_Type, Resource, _Category} = EventContent,
		#statedata{query = Query} = Data)
		when is_list(Query), is_list(Resource) ->
	send_request(EventContent, Data);
registered(cast = _EventType,
		{Type, Resource, _Category} = EventContent,
		#statedata{query = Query} = Data)
		when is_list(Query) ->
	ResourceId = get_resource_id(Resource),
	EventName = event_type(Type),
	case string:tokens(Query, "&=") of
		["eventType", EventName, "id", ResourceId] ->
			send_request(EventContent, Data);
		["id", ResourceId, "eventType", EventName] ->
			send_request(EventContent, Data);
		["id", ResourceId] ->
			send_request(EventContent, Data);
		["eventType", EventName] ->
			send_request(EventContent, Data);
		_ ->
			keep_state_and_data
	end;
registered(cast = _EventType,
		{async, RequestId, StatusCode} = _EventContent,
		_Data) when is_integer(StatusCode) ->
	error_logger:warning_report(["Notification delivery failed",
			{module, ?MODULE}, {fsm, self()},
			{request, RequestId}, {status, StatusCode}]),
	{stop, StatusCode};
registered(cast = _EventType,
		{async, RequestId, {error, Reason}} = _EventContent,
		_Data) ->
	error_logger:warning_report(["Notification delivery failed",
			{module, ?MODULE}, {fsm, self()},
			{request, RequestId}, {error, Reason}]),
	{stop, Reason};
registered({call, From} = _EventType, get = _EventContent,
		#statedata{id = Id, query = Query, callback = Callback,
				href = Href} = _Data) ->
	Hub = #hub{id = Id, query = Query, callback = Callback, href = Href},
	ReplyAction = {reply, From, Hub},
	{keep_state_and_data, ReplyAction};
registered({call, From} = _EventType, delete = _EventContent,
		_Data) ->
	ReplyAction = {reply, From, ok},
	{stop_and_reply, shutdown, ReplyAction};
registered(info = _EventType,
		{gen_event_EXIT, _Handler, Reason} = _EventContent,
		_Data) ->
	{stop, Reason}.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(Reason, _State, _Data)
		when Reason == shutdown; Reason == normal ->
	ok;
terminate({shutdown, Reason}, State, Data) ->
	terminate(Reason, State, Data);
terminate(Reason, State,
		#statedata{href = Href, query = Query, callback = Callback}) ->
	error_logger:warning_report(["Notification subscription cancelled",
			{reason, Reason}, {pid, self()}, {state, State},
			{href, Href}, {query, Query}, {callback, Callback}]).

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%----------------------------------------------------------------------
%%  The ocs_rest_hub_fsm private API
%%----------------------------------------------------------------------

-spec handle_async(ReplyInfo, Fsm) -> ok
	when
		ReplyInfo :: tuple(),
		Fsm :: pid().
%% @doc Handle result of httpc:request/3.
%% @private
handle_async({_RequestId,
		{{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}},
		_Fsm) when StatusCode >= 200, StatusCode  < 300 ->
	ok;
handle_async({RequestId,
		{{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}},
		Fsm) ->
	gen_statem:cast(Fsm, {async, RequestId, StatusCode});
handle_async({RequestId, {error, Reason}}, Fsm) ->
	gen_statem:cast(Fsm, {async, RequestId, {error, Reason}}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec unique() -> Result
	when
		Result :: {ID, TS},
		TS :: pos_integer(),
		ID :: string().
%% @doc Generate a unique identifier.
unique() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	ID = integer_to_list(TS) ++ integer_to_list(N),
	{ID, TS}.

%% @hidden
event(Resource, Category) ->
	case Category of
		balance ->
			case Resource of
				#bucket{} ->
					ocs_rest_res_balance:bucket(Resource);
				[#adjustment{} | _] ->
					AdjStructs = [ocs_rest_res_balance:adjustment(Adjustment)
							|| Adjustment <- Resource],
					{array, AdjStructs};
				[#acc_balance{} | _] ->
					AccBalStructs =
							[ocs_rest_res_balance:acc_balance(AccBalance)
							|| AccBalance <- Resource],
					{array, AccBalStructs}
			end;
		product ->
			case Resource of
				#product{} ->
					ocs_rest_res_product:product(Resource);
				#offer{} ->
					ocs_rest_res_product:offer(Resource)
			end;
		service ->
			ocs_rest_res_service:service(Resource);
		resource ->
			ocs_rest_res_resource:resource(Resource);
		usage ->
			ocs_rest_res_usage:usage_aaa_acct(Resource, [])
	end.

%% @hidden
get_resource_id(Resource) ->
	case Resource of
		#service{name = Name} ->
			binary_to_list(Name);
		#product{id = Id} ->
			Id;
		#offer{name = Name} ->
			Name;
		#bucket{id = Id} ->
			Id;
		#resource{id = Id} ->
			Id;
		_ ->
			[]
	end.

%% @hidden
event_type(Type) ->
	case Type of
		create_bucket ->
			"BucketBalanceCreationNotification";
		depleted ->
			"BucketBalanceDeletionEvent";
		delete_bucket ->
			"BucketBalanceDeletionEvent";
		charge ->
			"BalanceAdjustmentCreationNotification";
		accumulated ->
			"AccumulatedBalanceCreationNotification";
		create_product ->
			"ProductCreationNotification";
		delete_product ->
			"ProductRemoveNotification";
		create_service ->
			"ServiceCreationNotification";
		delete_service ->
			"ServiceDeleteNotification";
		create_offer ->
			"ProductOfferingCreationNotification";
		delete_offer ->
			"ProductOfferingRemoveNotification";
		create_resource ->
			"ResourceCreationNotification";
		delete_resource ->
			"ResourceRemoveNotification";
		log_acct ->
			"UsageCreationEvent"
	end.

%% @hidden
send_request({Type, Resource, Category} = _EventContent,
		#statedata{sync = Sync,
				profile = Profile, callback = Callback,
				authorization = Authorization} = Data) ->
	Options = case Sync of
		true ->
			[{sync, true}];
		false ->
			MFA = {?MODULE, handle_async, [self()]},
			[{sync, false}, {receiver, MFA}]
	end,
	Headers = case Authorization of
		undefined ->
			[{"accept", "application/json"}, {"content_type", "application/json"}];
		Authorization ->
			[{"accept", "application/json"},
					{"authorization", Authorization}]
	end,
	{EventId, TS} = unique(),
	EventTime = ocs_rest:iso8601(TS),
	EventStruct = {struct, [{"eventId", EventId}, {"eventTime", EventTime},
			{"eventType", event_type(Type)},
			{"event", event(Resource, Category)}]},
	Body = lists:flatten(mochijson:encode(EventStruct)),
	Request = {Callback, Headers, "application/json", Body},
	case httpc:request(post, Request, [], Options, Profile) of
		{ok, RequestId} when is_reference(RequestId), Sync == false  ->
			keep_state_and_data;
		{ok, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}}
				when StatusCode >= 200, StatusCode  < 300 ->
			{keep_state, Data#statedata{sync = false}};
		{ok, {{_HttpVersion, StatusCode, Reason}, _Headers, _Body}} ->
			error_logger:warning_report(["Notification delivery failed",
					{module, ?MODULE}, {fsm, self()},
					{status, StatusCode}, {reason, Reason}]),
			{stop, {shutdown, StatusCode}};
		{error, {failed_connect, _} = Reason} ->
			error_logger:warning_report(["Notification delivery failed",
					{module, ?MODULE}, {fsm, self()}, {error, Reason}]),
			{stop, {shutdown, Reason}};
		{error, Reason} ->
			{stop, Reason}
	end.

