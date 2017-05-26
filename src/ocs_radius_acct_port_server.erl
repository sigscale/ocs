%%% ocs_radius_acct_port_server.erl
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
%%% 	module receives {@link //radius. radius} messages on a port assigned
%%% 	for accounting in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/rfc/rfc3579.txt">
%%% 	RFC3579 - RADIUS Support For EAP</a>
%%%
-module(ocs_radius_acct_port_server).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").

-record(state,
		{acct_sup :: pid(),
		disc_sup :: undefined | pid(),
		address :: inet:ip_address(),
		port :: non_neg_integer(),
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				({NAS :: string() | inet:ip_address(), Port :: string(),
				Peer :: string()}), Value :: (Fsm :: pid())),
		disc_id = 1 :: integer()}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The ocs_radius_acct_port_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_radius_acct_port_server gen_server call backs
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
handle_call({request, Address, AccPort, Secret, ListenPort,
			#radius{code = ?AccountingRequest} = Radius}, From, State) ->
	request(Address, AccPort, Secret, ListenPort, Radius, From, State).

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
	{_, DiscSup, _, _} = lists:keyfind(ocs_radius_disconnect_fsm_sup, 1, Children),
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

-spec request(Address, Port, Secret, ListenPort, Radius, From, State) -> Result
	when
		Address :: inet:ip_address(), 
		Port :: pos_integer(),
		Secret :: string(), 
		ListenPort :: pos_integer(),
		Radius :: #radius{},
		From :: {Pid, Tag}, 
		Pid :: pid(),
		Tag :: term(),
		State :: state(),
		Result :: {reply, {ok, wait}, NewState}
			| {reply, {error, ignore}, NewState},
		NewState :: state().
%% @doc Handle a received RADIUS Accounting Request packet.
%% @private
request(Address, AccPort, Secret,
				ListenPort, Radius, {_RadiusFsm, _Tag} = _From, State) ->
	try
		#radius{code = ?AccountingRequest, id = Id, attributes = Attributes,
				authenticator = Authenticator} = Radius,
		AttrBin = radius_attributes:codec(Attributes),
		Length = size(AttrBin) + 20,
		CalcAuth = crypto:hash(md5, [<<?AccountingRequest, Id, Length:16>>,
				<<0:128>>, AttrBin, Secret]),
		CalcAuth = list_to_binary(Authenticator),
		{ok, AcctStatusType}  = radius_attributes:find(?AcctStatusType, Attributes),
		NasIpAddressV = radius_attributes:find(?NasIpAddress, Attributes),
		NasIdentifierV = radius_attributes:find(?NasIdentifier, Attributes),
		NasId = case {NasIpAddressV, NasIdentifierV} of
			{{error, not_found}, {error, not_found}} ->
				throw(reject);
			{Value, {error, not_found}} ->
				Value;
			{_, {ok, Value}} ->
				Value
		end,
		{error, not_found} = radius_attributes:find(?UserPassword, Attributes),
		{error, not_found} = radius_attributes:find(?ChapPassword, Attributes),
		{error, not_found} = radius_attributes:find(?ReplyMessage, Attributes),
		{error, not_found} = radius_attributes:find(?State, Attributes),
		{ok, AcctSessionId} = radius_attributes:find(?AcctSessionId, Attributes),
		request1(AcctStatusType, AcctSessionId, Id, Authenticator,
						 Secret, NasId, Address, AccPort, ListenPort, Attributes, State)
	catch
		_:_ ->
			{reply, {error, ignore}, State}
	end.
%% @hidden
request1(?AccountingStart, _AcctSessionId, Id,
		Authenticator, Secret, _NasId, _Address, _AccPort, _ListenPort, Attributes,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	ok = ocs_log:acct_log(radius, {ServerAddress, ServerPort}, start, Attributes),
	{reply, {ok, response(Id, Authenticator, Secret)}, State};
request1(?AccountingStop, AcctSessionId, Id,
		Authenticator, Secret, NasId, Address, _AccPort, ListenPort, Attributes,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	InOctets = radius_attributes:find(?AcctInputOctets, Attributes),
	OutOctets = radius_attributes:find(?AcctOutputOctets, Attributes),
	Usage = case {InOctets, OutOctets} of
		{{error, not_found}, {error, not_found}} ->
			0;
		{{ok,In}, {ok,Out}} ->
			In + Out
	end,
	{ok, UserName} = radius_attributes:find(?UserName, Attributes),
	ok = ocs_log:acct_log(radius, {ServerAddress, ServerPort}, stop, Attributes),
	Subscriber = ocs:normalize(UserName),
	case decrement_balance(Subscriber, Usage) of
		{ok, OverUsed, false} when OverUsed =< 0 ->
			start_disconnect(AcctSessionId, Id, Authenticator, Secret, ListenPort,
					NasId, Address, Attributes, Subscriber, State);
		{ok, _SufficientBalance, _Flag} ->
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		{error, not_found} ->
			error_logger:warning_report(["Accounting subscriber not found",
					{module, ?MODULE}, {subscriber, Subscriber},
					{username, UserName}, {nas, NasId}, {address, Address},
					{session, AcctSessionId}]),
			{reply, {ok, response(Id, Authenticator, Secret)}, State}
	end;
request1(?AccountingInterimUpdate, AcctSessionId, Id,
		Authenticator, Secret, NasId, Address, _AccPort, ListenPort, Attributes,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	InOctets = radius_attributes:find(?AcctInputOctets, Attributes),
	OutOctets = radius_attributes:find(?AcctOutputOctets, Attributes),
	Usage = case {InOctets, OutOctets} of
		{{error, not_found}, {error, not_found}} ->
			0;
		{{ok,In}, {ok,Out}} ->
			In + Out
	end,
	{ok, UserName} = radius_attributes:find(?UserName, Attributes),
	ok = ocs_log:acct_log(radius, {ServerAddress, ServerPort}, interim, Attributes),
	Subscriber = ocs:normalize(UserName),
	case ocs:find_subscriber(Subscriber) of
		{ok, _, _, Balance, Enabled} when Enabled == false; Balance =< Usage ->
			start_disconnect(AcctSessionId, Id, Authenticator, Secret, ListenPort,
					NasId, Address, Attributes, Subscriber, State);
		{ok, _, _, _, _} ->
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		{error, not_found} ->
			error_logger:warning_report(["Accounting subscriber not found",
					{module, ?MODULE}, {subscriber, Subscriber},
					{username, UserName}, {nas, NasId}, {address, Address},
					{session, AcctSessionId}]),
			{reply, {ok, response(Id, Authenticator, Secret)}, State}
	end;
request1(?AccountingON, _AcctSessionId, Id,
		Authenticator, Secret, _NasId, _Address, _AccPort, _ListenPort, Attributes,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	ok = ocs_log:acct_log(radius, {ServerAddress, ServerPort}, on, Attributes),
	{reply, {ok, response(Id, Authenticator, Secret)}, State};
request1(?AccountingOFF, _AcctSessionId, Id,
		Authenticator, Secret, _NasId, _Address, _AccPort, _ListenPort, Attributes,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	ok = ocs_log:acct_log(radius, {ServerAddress, ServerPort}, off, Attributes),
	{reply, {ok, response(Id, Authenticator, Secret)}, State};
request1(_AcctStatusType, _AcctSessionId, _Id, _Authenticator,
		_Secret, _NasId, _Address, _Port, _ListenPort, _Attributes, State) ->
	{reply, {error, ignore}, State}.

-spec response(Id, RequestAuthenticator, Secret) -> AccessAccept
	when
		Id :: byte(), 
		RequestAuthenticator :: [byte()],
		Secret :: string() | binary(),
		AccessAccept :: binary().
%% @hidden
response(Id, RequestAuthenticator, Secret) ->
	Length = 20,
	ResponseAuthenticator = crypto:hash(md5, [<<?AccountingResponse, Id,
			Length:16>>, RequestAuthenticator, Secret]),
	Response = #radius{code = ?AccountingResponse, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	radius:codec(Response).

-spec decrement_balance(Subscriber :: string() | binary(),
		Usage :: non_neg_integer()) ->
	{ok, NewBalance :: integer(), DiscFlag :: boolean()}|
			{error, Reason :: not_found | term()}.
%% @doc Decrements subscriber's current balance
decrement_balance(Subscriber, Usage) when is_list(Subscriber) ->
	decrement_balance(list_to_binary(Subscriber), Usage);
decrement_balance(Subscriber, Usage) when is_binary(Subscriber),
		Usage >= 0 ->
	F = fun() ->
				case mnesia:read(subscriber, Subscriber, write) of
					[#subscriber{balance = Balance, disconnect = Flag} = Entry] ->
						NewBalance = Balance - Usage,
						NewEntry = Entry#subscriber{balance = NewBalance},
						mnesia:write(subscriber, NewEntry, write),
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

-spec start_disconnect(AcctSessionId, Id, Authenticator, Secret,
	ListenPort,NasId, Address, Attributes, Subscriber, State) -> Result
	when
		AcctSessionId :: integer(), 
		Id :: byte(),
		Authenticator :: [byte()], 
		Secret :: string(),
		ListenPort :: non_neg_integer(),
		NasId :: inet:ip_address() | string(), 
		Address :: inet:ip_address(),
		Attributes :: [binary()], 
		Subscriber :: [byte()], 
		State :: #state{},
		Result :: {reply, {ok, Response}, NewState},
		Response :: binary(),
		NewState :: #state{}.
%% @doc Start a disconnect_fsm worker.
start_disconnect(AcctSessionId, Id, Authenticator, Secret, ListenPort,
		NasId, Address, Attributes, Subscriber, #state{handlers = Handlers,
		disc_sup = DiscSup, disc_id = DiscId} = State) ->
	case gb_trees:lookup({NasId, Subscriber, AcctSessionId}, Handlers) of
		{value, _DiscPid} ->
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		none ->
			DiscArgs = [Address, NasId, Subscriber,
					AcctSessionId, Secret, ListenPort, Attributes, Id],
			StartArgs = [DiscArgs, []],
			case supervisor:start_child(DiscSup, StartArgs) of
				{ok, DiscFsm} ->
					link(DiscFsm),
					NewHandlers = gb_trees:insert({NasId, Subscriber, AcctSessionId},
							DiscFsm, Handlers),
					NewDiscId = DiscId + 1,
					NewState = State#state{handlers = NewHandlers,
							disc_id = NewDiscId},
					{reply, {ok, response(Id, Authenticator, Secret)}, NewState};
				{error, Reason} ->
					error_logger:error_report(["Failed to initiate session disconnect function",
							{module, ?MODULE}, {subscriber, Subscriber}, {nas, NasId},
							{address, Address}, {session, AcctSessionId}, {error, Reason}]),
					{reply, {ok, response(Id, Authenticator, Secret)}, State}
			end
	end.
