%%% ocs_acct_server.erl
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
%%% @reference <a href="http://tools.ietf.org/rfc/rfc3579.txt">
%%% 	RFC3579 - RADIUS Support For EAP</a>
%%%
-module(ocs_acct_server).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_acct_server API
-export([]).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").

-record(state,
		{acct_sup :: pid(),
		disc_sup :: pid(),
		socket :: inet:socket(),
		dir :: string(),
		address :: inet:ip_address(),
		port :: non_neg_integer(),
		module :: atom(),
		log :: term(), 
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				({NAS :: string() | inet:ip_address(), Port :: string(),
				Peer :: string()}), Value :: (Fsm :: pid()))}).

-define(LOGNAME, radius_acct).

%%----------------------------------------------------------------------
%%  The ocs_acct_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_acct_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args :: list()) -> Result :: {ok, State :: #state{}}
		| {ok, State :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} server.
%% 	Args :: [Sup :: pid(), Module :: atom(), Port :: non_neg_integer(),
%% 	Address :: inet:ip_address()].
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([AcctSup, _Address, _Port]) ->
	{ok, Directory} = application:get_env(ocs, accounting_dir),
	Log = ?LOGNAME,
	FileName = Directory ++ "/" ++ atom_to_list(Log),
	State = #state{dir = Directory, acct_sup = AcctSup},
	try case file:list_dir(Directory) of
		{ok, _} ->
			ok;
		{error, enoent} ->
			case file:make_dir(Directory) of
				ok ->
					ok;
				{error, Reason} ->
					throw(Reason)
			end;
		{error, Reason} ->
			throw(Reason)
	end of
		ok ->
			case disk_log:open([{name, Log}, {file, FileName},
					{type, wrap}, {size, {1048575, 20}}]) of
				{ok, Log} ->
					{ok, State#state{log = Log}, 0};
				{repaired, Log, {recovered, Rec}, {badbytes, Bad}} ->
					error_logger:warning_report(["Disk log repaired",
							{log, Log}, {path, FileName}, {recovered, Rec},
							{badbytes, Bad}]),
					process_flag(trap_exit, true),
					{ok, State#state{log = Log}, 0};
				{error, Reason1} ->
					{error, Reason1}
			end
	catch
		Reason2 ->
			{error, Reason2}
	end.

-spec handle_call(Request :: term(), From :: {Pid :: pid(), Tag :: any()},
		State :: #state{}) ->
	Result :: {reply, Reply :: term(), NewState :: #state{}}
		| {reply, Reply :: term(), NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {reply, Reply :: term(), NewState :: #state{}, hibernate}
		| {noreply, NewState :: #state{}}
		| {noreply, NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: #state{}, hibernate}
		| {stop, Reason :: term(), Reply :: term(), NewState :: #state{}}
		| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
handle_call(shutdown, _From, State) ->
	{stop, normal, ok, State};
handle_call(port, _From, #state{port = Port} = State) ->
	{reply, Port, State};
handle_call({request, Address, Port, Secret,
			#radius{code = ?AccountingRequest} = Radius}, From, State) ->
	accounting_request(Address, Port, Secret, Radius, From, State).

-spec handle_cast(Request :: term(), State :: #state{}) ->
	Result :: {noreply, NewState :: #state{}}
		| {noreply, NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: #state{}, hibernate}
		| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(_Request, State) ->
	{noreply, State}.

-spec handle_info(Info :: timeout | term(), State :: #state{}) ->
	Result :: {noreply, NewState :: #state{}}
		| {noreply, NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: #state{}, hibernate}
		| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, #state{acct_sup = AcctSup} = State) ->
	Children = supervisor:which_children(AcctSup),
	{_, DiscSup, _, _} = lists:keyfind(ocs_radius_disconnect_fsm_sup, 1, Children),
	{noreply, State#state{disc_sup = DiscSup}};
handle_info({'EXIT', _Pid, {shutdown, SessionID}},
		#state{handlers = Handlers} = State) ->
	NewHandlers = gb_trees:delete(SessionID, Handlers),
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

-spec terminate(Reason :: normal | shutdown | term(),
		State :: #state{}) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		State :: #state{}, Extra :: term()) ->
	Result :: {ok, NewState :: #state{}}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec accounting_request(Address :: inet:ip_address(), Port :: pos_integer(),
		Secret :: string(), Radius :: #radius{},
		From :: {Pid :: pid(), Tag :: term()}, State :: #state{}) ->
	{reply, {ok, wait}, NewState :: #state{}}
			| {reply, {error, ignore}, NewState :: #state{}}.
%% @doc Handle a received RADIUS Accounting Request packet.
%% @private
accounting_request(Address, _Port, Secret, Radius,
		{_RadiusFsm, _Tag} = _From, #state{handlers = _Handlers,
		log = Log, disc_sup = DiscSup} = State) ->
	try 
		#radius{code = ?AccountingRequest, id = Id, attributes = Attributes,
				authenticator = Authenticator} = Radius,
		NasIpAddressV = radius_attributes:find(?NasIpAddress, Attributes),
		NasIdentifierV = radius_attributes:find(?NasIdentifier, Attributes),
		InOctets = radius_attributes:find(?AcctInputOctets, Attributes),
		OutOctets = radius_attributes:find(?AcctOutputOctets, Attributes),
		{ok, Subscriber} = radius_attributes:find(?UserName, Attributes),
		NasID = case {NasIpAddressV, NasIdentifierV} of
			{{error, not_found}, {error, not_found}} ->
				throw(reject);
			{_, {error, not_found}} ->
				undefined;
			{_, {ok, Value}} ->
				Value
		end,
		case {InOctets, OutOctets} of
			{{error, not_found}, {error, not_found}} ->
				Usage = 0;
			{{ok,In}, {ok,Out}} ->
				Usage = In + Out
		end,
		{error, not_found} = radius_attributes:find(?UserPassword, Attributes),
		{error, not_found} = radius_attributes:find(?ChapPassword, Attributes),
		{error, not_found} = radius_attributes:find(?ReplyMessage, Attributes),
		{error, not_found} = radius_attributes:find(?State, Attributes),
		{ok, AcctSessionId} = radius_attributes:find(?AcctSessionId, Attributes),
		case disk_log:log(Log, Attributes) of
			ok ->
				case ocs:decrement_balance(Subscriber, Usage) of
					{ok, OverUsed} when OverUsed =< 0 ->
						case supervisor:start_child(DiscSup, [[Address, NasID,
								Subscriber, AcctSessionId, Secret, Id, Attributes], []]) of
							{ok, _Child} ->
								ok;
							{error, Reason} ->
								error_logger:error_report(["Failed to initiate session disconnect function",
									{error, Reason}])
						end;
					{ok, _SufficientBalance} ->
						ok
				end,
				{reply, {ok, response(Id, Authenticator, Secret, Attributes)}, State};
			{error, _Reason} ->
				{reply, {error, ignore}, State}
		end
	catch
		_:_ ->
			{reply, {error, ignore}, State}
	end.

-spec response(Id :: byte(), RequestAuthenticator :: [byte()],
		Secret :: string() | binary(),
		Attributes :: radius_attributes:attributes()) ->
	AccessAccept :: binary().
%% @hidden
response(Id, RequestAuthenticator, Secret, AttributeList)
		when is_list(AttributeList) ->
	AttributeList1 = radius_attributes:store(?MessageAuthenticator,
		<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, AttributeList),
	Attributes1 = radius_attributes:codec(AttributeList1),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = crypto:hmac(md5, Secret, [<<?AccountingResponse, Id,
			Length:16>>, RequestAuthenticator, Attributes1]),
	AttributeList2 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttributeList1),
	Attributes2 = radius_attributes:codec(AttributeList2),
	ResponseAuthenticator = crypto:hash(md5, [<<?AccountingResponse, Id,
			Length:16>>, RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = ?AccountingResponse, id = Id,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	radius:codec(Response).
