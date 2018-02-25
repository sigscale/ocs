%%% ocs_radius_acct_port_server.erl
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

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

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
%% 	IpAddress :: inet:ip_address()].
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([AcctSup, IpAddress, Port, _Options]) ->
	State = #state{address = IpAddress, port = Port, acct_sup = AcctSup},
	case ocs_log:acct_open() of
		ok ->
			case ocs_log:abmf_open() of
				ok ->
					process_flag(trap_exit, true),
					{ok, State, 0};
				{error, Reason} ->
					{stop, Reason}
			end;
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
handle_call({request, IpAddress, AccPort, Secret, ListenPort,
			#radius{code = ?AccountingRequest} = Radius}, From, State) ->
	request(IpAddress, AccPort, Secret, ListenPort, Radius, From, State).

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

-spec request(IpAddress, Port, Secret, ListenPort, Radius, From, State) -> Result
	when
		IpAddress :: inet:ip_address(), 
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
request(IpAddress, AccPort, Secret, ListenPort, Radius, From, State) ->
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
		request1(AcctStatusType, AcctSessionId, Id, Authenticator, Secret,
				NasId, IpAddress, AccPort, ListenPort, Attributes, From, State)
	catch
		_:_Reason ->
			{reply, {error, ignore}, State}
	end.
%% @hidden
request1(?AccountingStart, AcctSessionId, Id,
		Authenticator, Secret, NasId, IpAddress, _AccPort, _ListenPort, Attributes,
		From, #state{address = ServerAddress, port = ServerPort} = State) ->
	SessionAttributes = ocs_rating:session_attributes(Attributes),
	Subscriber = case radius_attributes:find(?UserName, Attributes) of
		{ok, Sub} ->
			Sub;
		{error, not_found} ->
			radius_attributes:fetch(?CallingStationId, Attributes)
	end,
	{ServiceType, Direction, CallAddress} = get_service_type(Attributes),
	Timestamp = case radius_attributes:find(?AcctDelayTime, Attributes) of
		{ok, AcctDelayTime} ->
			Now = calendar:local_time(),
			Seconds = calendar:datetime_to_gregorian_seconds(Now),
			calendar:gregorian_seconds_to_datetime(Seconds - AcctDelayTime);
		{error, not_found} ->
			calendar:local_time()
	end,
	case ocs_rating:rate(radius, ServiceType, Subscriber, Timestamp,
			CallAddress, Direction, initial, [], [], SessionAttributes) of
		{ok, #subscriber{}, _} ->
			ok = ocs_log:acct_log(radius,
					{ServerAddress, ServerPort}, start, Attributes, undefined, undefined),
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		{out_of_credit, SessionList}  ->
			gen_server:reply(From, {ok, response(Id, Authenticator, Secret)}),
			start_disconnect(State, Subscriber, SessionList),
			{noreply, State};
		{disabled, SessionList} ->
			gen_server:reply(From, {ok, response(Id, Authenticator, Secret)}),
			start_disconnect(State, Subscriber, SessionList),
			{noreply, State};
		{error, Reason} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, Reason}, {ip_address, IpAddress},
					{nas, NasId}, {type, initial}, {subscriber, Subscriber},
					{call_address, CallAddress}, {session, AcctSessionId}]),
			{reply, {ok, response(Id, Authenticator, Secret)}, State}
	end;
request1(?AccountingStop, AcctSessionId, Id,
		Authenticator, Secret, NasId, IpAddress, _AccPort, _ListenPort, Attributes,
		From, #state{address = ServerAddress, port = ServerPort} = State) ->
	UsageOctets = get_usage(Attributes),
	UsageSecs = case radius_attributes:find(?AcctSessionTime, Attributes) of
		{ok, Secs} ->
			Secs;
		{error, not_found} ->
			0
	end,
	Subscriber = case radius_attributes:find(?UserName, Attributes) of
		{ok, Sub} ->
			Sub;
		{error, not_found} ->
			radius_attributes:fetch(?CallingStationId, Attributes)
	end,
	ok = ocs_log:acct_log(radius,
			{ServerAddress, ServerPort}, stop, Attributes, undefined, undefined),
	SessionAttributes = ocs_rating:session_attributes(Attributes),
	DebitAmount = [{octets, UsageOctets}, {seconds, UsageSecs}],
	{ServiceType, Direction, CallAddress} = get_service_type(Attributes),
	Timestamp = case radius_attributes:find(?AcctDelayTime, Attributes) of
		{ok, AcctDelayTime} ->
			Now = calendar:local_time(),
			Seconds = calendar:datetime_to_gregorian_seconds(Now),
			calendar:gregorian_seconds_to_datetime(Seconds - AcctDelayTime);
		{error, not_found} ->
			calendar:local_time()
	end,
	case ocs_rating:rate(radius, ServiceType, Subscriber, Timestamp,
			CallAddress, Direction, final, DebitAmount, [], SessionAttributes) of
		{ok, #subscriber{}, _, Rated} ->
			ok = ocs_log:acct_log(radius,
					{ServerAddress, ServerPort}, stop, Attributes, undefined, Rated),
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		{out_of_credit, SessionList, Rated}  ->
			gen_server:reply(From, {ok, response(Id, Authenticator, Secret)}),
			ok = ocs_log:acct_log(radius,
					{ServerAddress, ServerPort}, stop, Attributes, undefined, Rated),
			start_disconnect(State, Subscriber, SessionList),
			{noreply, State};
		{disabled, SessionList} ->
			gen_server:reply(From, {ok, response(Id, Authenticator, Secret)}),
			start_disconnect(State, Subscriber, SessionList),
			{noreply, State};
		{error, Reason} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, Reason}, {ip_address, IpAddress},
					{nas, NasId}, {type, final}, {subscriber, Subscriber},
					{call_address, CallAddress}, {used, DebitAmount}, {session, AcctSessionId}]),
			{reply, {ok, response(Id, Authenticator, Secret)}, State}
	end;
request1(?AccountingInterimUpdate, AcctSessionId, Id,
		Authenticator, Secret, NasId, IpAddress, _AccPort, _ListenPort, Attributes,
		From, #state{address = ServerAddress, port = ServerPort} = State) ->
	UsageOctets = get_usage(Attributes),
	UsageSecs = case radius_attributes:find(?AcctSessionTime, Attributes) of
		{ok, Secs} ->
			Secs;
		{error, not_found} ->
			0
	end,
	Subscriber = case radius_attributes:find(?UserName, Attributes) of
		{ok, Sub} ->
			Sub;
		{error, not_found} ->
			radius_attributes:fetch(?CallingStationId, Attributes)
	end,
	ok = ocs_log:acct_log(radius,
			{ServerAddress, ServerPort}, interim, Attributes, undefined, undefined),
	SessionAttributes = ocs_rating:session_attributes(Attributes),
	ReserveAmount = [{octets, UsageOctets}, {seconds, UsageSecs}],
	{ServiceType, Direction, CallAddress} = get_service_type(Attributes),
	Timestamp = case radius_attributes:find(?AcctDelayTime, Attributes) of
		{ok, AcctDelayTime} ->
			Now = calendar:local_time(),
			Seconds = calendar:datetime_to_gregorian_seconds(Now),
			calendar:gregorian_seconds_to_datetime(Seconds - AcctDelayTime);
		{error, not_found} ->
			calendar:local_time()
	end,
	case ocs_rating:rate(radius, ServiceType, Subscriber, Timestamp,
			CallAddress, Direction, interim, [], ReserveAmount, SessionAttributes) of
		{ok, #subscriber{}, _} ->
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		{out_of_credit, SessionList} ->
			gen_server:reply(From, {ok, response(Id, Authenticator, Secret)}),
			start_disconnect(State, Subscriber, SessionList),
			{noreply, State};
		{disabled, SessionList} ->
			gen_server:reply(From, {ok, response(Id, Authenticator, Secret)}),
			start_disconnect(State, Subscriber, SessionList),
			{noreply, State};
		{error, not_found} ->
			{reply, {ok, response(Id, Authenticator, Secret)}, State};
		{error, Reason} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, Reason}, {ip_address, IpAddress},
					{nas, NasId}, {type, interim}, {subscriber, Subscriber},
					{call_address, CallAddress}, {reserved, ReserveAmount},
					{session, AcctSessionId}]),
			{reply, {ok, response(Id, Authenticator, Secret)}, State}
	end;
request1(?AccountingON, _AcctSessionId, Id,
		Authenticator, Secret, _NasId, _IpAddress, _AccPort, _ListenPort, Attributes,
		_From, #state{address = ServerAddress, port = ServerPort} = State) ->
	ok = ocs_log:acct_log(radius,
			{ServerAddress, ServerPort}, on, Attributes, undefined, undefined),
	{reply, {ok, response(Id, Authenticator, Secret)}, State};
request1(?AccountingOFF, _AcctSessionId, Id,
		Authenticator, Secret, _NasId, _IpAddress, _AccPort, _ListenPort, Attributes,
		_From, #state{address = ServerAddress, port = ServerPort} = State) ->
	ok = ocs_log:acct_log(radius,
			{ServerAddress, ServerPort}, off, Attributes, undefined, undefined),
	{reply, {ok, response(Id, Authenticator, Secret)}, State};
request1(_AcctStatusType, _AcctSessionId, _Id, _Authenticator,
		_Secret, _NasId, _IpAddress, _Port, _ListenPort, _Attributes, _From, State) ->
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

%% @hidden
%% @doc Start a disconnect_fsm worker.
%%
start_disconnect(#state{disc_sup = DiscSup} = State, Subscriber, [H | Tail]) ->
	start_disconnect1(DiscSup, Subscriber, H),
	start_disconnect(State, Subscriber, Tail);
start_disconnect(_State, _Subscriber, []) ->
	ok.
%% @hidden
start_disconnect1(DiscSup, Subscriber, SessionAttributes) ->
	DiscArgs = [Subscriber, SessionAttributes],
	StartArgs = [DiscArgs, []],
	supervisor:start_child(DiscSup, StartArgs).

%% @hidden
%% @doc Get used octets from RADIUS attributes.
%%
get_usage(Attributes) ->
	GigaInWords = radius_attributes:find(?AcctInputGigawords, Attributes),
	GigaOutWords = radius_attributes:find(?AcctOutputGigawords, Attributes),
	InOctets = radius_attributes:find(?AcctInputOctets, Attributes),
	OutOctets = radius_attributes:find(?AcctOutputOctets, Attributes),
	GigaIn = case GigaInWords of
		{error, not_found} ->
			0;
		{ok, Gin} ->
			Gin bsl 32
	end,
	GigaOut = case GigaOutWords of
		{error, not_found} ->
			0;
		{ok, Gout} ->
			Gout bsl 32
	end,
	In = case InOctets of
		{error, not_found} ->
			0;
		{ok, InOct} ->
			InOct
	end,
	Out = case OutOctets of
		{error, not_found} ->
			0;
		{ok, OutOct} ->
			OutOct
	end,
	In + GigaIn + Out + GigaOut.

get_service_type(Attr) ->
	case radius_attributes:find(?ServiceType, Attr) of
		{ok, 12} ->
			case radius_attributes:find(?Cisco, ?H323CallOrigin, Attr) of
				{ok, answer} ->
					case radius_attributes:find(?CallingStationId, Attr) of
						{ok, Address} ->
							{12, answer, Address};
						{error, not_found} ->
							{12, answer, undefined}
					end;
				_Other ->
					case radius_attributes:find(?CalledStationId, Attr) of
						{ok, Address} ->
							{12, originate, Address};
						{error, not_found} ->
							{12, originate, undefined}
					end
			end;
		{ok, ServiceType} ->
			{ServiceType, undefined, undefined};
		{error, not_found} ->
			{undefined, undefined, undefined}
	end.

