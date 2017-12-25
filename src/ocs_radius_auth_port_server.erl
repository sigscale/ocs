%%% ocs_radius_auth_port_server.erl
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
%%% 	for authentication in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/rfc/rfc3579.txt">
%%% 	RFC3579 - RADIUS Support For EAP</a>
%%%
-module(ocs_radius_auth_port_server).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_radius_auth_port_server API
-export([]).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").

-record(state,
		{auth_port_sup :: pid(),
		simple_auth_sup :: undefined | pid(),
		pwd_sup :: undefined | pid(),
		ttls_sup :: undefined | pid(),
		address :: inet:ip_address(),
		port :: non_neg_integer(),
		method_prefer :: ocs:eap_method(),
		method_order :: [ocs:eap_method()],
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				({NAS :: string() | inet:ip_address(), Port :: string(),
				Peer :: string()}), Value :: (Fsm :: pid()))}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The ocs_radius_auth_port_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_radius_auth_port_server gen_server call backs
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
init([AuthPortSup, Address, Port, Options]) ->
	MethodPrefer = proplists:get_value(eap_method_prefer, Options, pwd),
	MethodOrder = proplists:get_value(eap_method_order, Options, [pwd, ttls]),
	State = #state{auth_port_sup = AuthPortSup,
			address = Address, port = Port,
			method_prefer = MethodPrefer, method_order = MethodOrder},
	case ocs_log:auth_open() of
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
handle_call({request, Address, Port, Secret, PasswordReq,
			#radius{code = ?AccessRequest} = Radius, IsEap}, From, State) ->
	request(IsEap, Address, Port, Secret, PasswordReq, Radius, From, State).

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(), 
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, Timeout | infinity}
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
handle_info(timeout, #state{auth_port_sup = AuthPortSup} = State) ->
	Children = supervisor:which_children(AuthPortSup),
	{_, PwdSup, _, _} = lists:keyfind(ocs_eap_pwd_fsm_sup, 1, Children),
	{_, TtlsSup, _, _} = lists:keyfind(ocs_eap_ttls_fsm_sup_sup, 1, Children),
	{_, SimpleAuthSup, _, _} = lists:keyfind(ocs_simple_auth_fsm_sup, 1, Children),
	{noreply, State#state{pwd_sup = PwdSup, ttls_sup = TtlsSup, simple_auth_sup = SimpleAuthSup}};
handle_info({'EXIT', Pid, {shutdown, SessionID}},
		#state{handlers = Handlers} = State) ->
	 case gb_trees:lookup(SessionID, Handlers) of
		{value, {Pid, _Identity}} ->
			NewHandlers = gb_trees:delete(SessionID, Handlers),
			{noreply, State#state{handlers = NewHandlers}};
		{value, {_, _Identity}} ->
			{noreply, State};
		none ->
			{noreply, State}
	end;
handle_info({'EXIT', Fsm, _Reason},
		#state{handlers = Handlers} = State) ->
	Fdel = fun(_F, {Key, {Pid, _Identity}, _Iter}) when Pid == Fsm ->
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
terminate(_Reason, _State) ->
	ocs_log:auth_close().

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

-spec request(IsEap, Address, Port, Secret, PasswordReq, Radius, From, State) -> Result
	when
		IsEap :: {eap, binary()} | none,
		Address :: inet:ip_address(), 
		Port :: pos_integer(),
		Secret :: string(), 
		PasswordReq :: boolean(),
		Radius :: #radius{},
		From :: {Pid, Tag}, 
		Pid :: pid(), 
		Tag :: term(),
		State :: state(),
		Result :: {reply, {ok, wait}, NewState}
			| {reply, {error, ignore}, NewState},
		NewState :: state().
%% @doc Handle a received RADIUS Access-Request packet.
%% @private
request(none, Address, Port, Secret, PasswordReq, Radius, From, State) ->
	request1(none, Address, Port, Secret, PasswordReq, Radius, From, State);
request({eap, <<_:32, ?Identity, Identity/binary>>},
		Address, Port, Secret, PasswordReq, Radius, From, State) ->
	request1({identity, Identity}, Address, Port, Secret, PasswordReq, Radius, From, State);
request({eap, <<_, EapID, _:16, ?LegacyNak, Data/binary>>},
		Address, Port, Secret, PasswordReq, Radius, From, State) ->
	request1({legacy_nak, EapID, Data}, Address, Port, Secret, PasswordReq, Radius, From, State);
request(Eap, Address, Port, Secret, PasswordReq, Radius, From, State) ->
	request1(Eap, Address, Port, Secret, PasswordReq, Radius, From, State).
%% @hidden
request1(EapType, Address, Port, Secret, PasswordReq,
		#radius{id = RadiusId, authenticator = RequestAuthenticator,
		attributes = Attributes} = AccessRequest, {RadiusFsm, _Tag} = _From,
		#state{handlers = Handlers, method_prefer = MethodPrefer,
		method_order = MethodOrder, address = ServerAddress,
		port = ServerPort} = State) ->
	try
		NAS = case {radius_attributes:find(?NasIdentifier, Attributes),
				radius_attributes:find(?NasIpAddress, Attributes)} of
			{{ok, NasId}, _} ->
				NasId;
			{{error, not_found}, {ok, NasAddr}} ->
				NasAddr
		end,
		NasPort = case radius_attributes:find(?NasPort, Attributes) of
			{ok, NP} ->
				NP;
			{error, not_found} ->
				case radius_attributes:find(?NasPortType, Attributes) of
					{ok, NasPortType} ->
						NasPortType;
					{error, not_found} ->
						undefined
				end
		end,
		Peer = radius_attributes:fetch(?CallingStationId, Attributes),
		SessionID = {NAS, NasPort, Peer},
		case gb_trees:lookup(SessionID, Handlers) of
			none ->
				case EapType of
					{_, Identity} when MethodPrefer == pwd ->
						Sup = State#state.pwd_sup,
						NewState = start_fsm(AccessRequest, RadiusFsm, Address,
								Port, Secret, PasswordReq, SessionID, Identity, Sup, State),
						{reply, {ok, wait}, NewState};
					{_, Identity} when MethodPrefer == ttls ->
						Sup = State#state.ttls_sup,
						NewState = start_fsm(AccessRequest, RadiusFsm, Address,
								Port, Secret, PasswordReq, SessionID, Identity, Sup, State),
						{reply, {ok, wait}, NewState};
					none ->
						{ok, _} = radius_attributes:find(?UserName, Attributes),
						case PasswordReq of
							true ->
								{ok, _} = radius_attributes:find(?UserPassword, Attributes);
							false ->
								ok
						end,
						Sup = State#state.simple_auth_sup,
						NewState = start_fsm(AccessRequest, RadiusFsm, Address,
								Port, Secret, PasswordReq, SessionID, <<>>, Sup, State),
						{reply, {ok, wait}, NewState}
				end;
			{value, {Fsm, Identity1}} ->
				case EapType of
					{legacy_nak, EapId, AlternateMethods} -> 
						case get_alternate(MethodOrder, AlternateMethods, State) of
							{ok , Sup} ->
								gen_fsm:send_event(Fsm, {AccessRequest, RadiusFsm}),
								NewEapPacket = #eap_packet{code = response, 
										type = ?Identity, identifier = EapId, data = Identity1},
								NewEapMessage = ocs_eap_codec:eap_packet(NewEapPacket),
								RequestAttributes = radius_attributes:store(?EAPMessage,
										NewEapMessage, Attributes),
								NewAccessRequest = AccessRequest#radius{attributes = RequestAttributes},
								NewState = start_fsm(NewAccessRequest, RadiusFsm,
										Address, Port, Secret, PasswordReq, SessionID, Identity1,
										Sup, State),
								{reply, {ok, wait}, NewState};
							{error, none} ->
								Length = 20,
								NewEapPacket = #eap_packet{code = failure, identifier = EapId},
								NewEapMessage = ocs_eap_codec:eap_packet(NewEapPacket),
								NewAttributes = radius_attributes:add(?EAPMessage, NewEapMessage, []),
								RejectAttributes = radius_attributes:codec(NewAttributes),
								BinRequestAuth = list_to_binary(RequestAuthenticator),
								ResponseAuthenticator = crypto:hash(md5, [<<?AccessReject,
									RadiusId, Length:16>>, BinRequestAuth, RejectAttributes, Secret]),
								AccessRejectPacket = #radius{code = ?AccessReject, id = RadiusId,
										authenticator = ResponseAuthenticator, 
										attributes = RejectAttributes},
								AccessReject = radius:codec(AccessRejectPacket),
								ok = ocs_log:auth_log(radius, {ServerAddress, ServerPort},
										{Address, Port}, reject, Attributes, NewAttributes),
								{reply, {ok, AccessReject}, State}	
						end;
					{eap, _} ->
						gen_fsm:send_event(Fsm, {AccessRequest, RadiusFsm}),
						{reply, {ok, wait}, State};
					none ->
						gen_fsm:send_event(Fsm, {AccessRequest, RadiusFsm}),
						{reply, {ok, wait}, State}
				end
		end
	catch
		_:_ ->
			{reply, {error, ignore}, State}
	end.

-spec start_fsm(AccessRequest, RadiusFsm, Address, Port, Secret,
	PasswordReq, SessionID, Identity, Sup, State) -> NewState
	when
		AccessRequest :: #radius{}, 
		RadiusFsm :: pid(),
		Address :: inet:ip_address(), 
		Port :: integer(),
		Secret :: binary(), 
		PasswordReq :: boolean(),
		SessionID :: tuple(), 
		Identity :: binary(),
		Sup :: pid(), 
		State :: state(),
		NewState :: state().
%% @doc Start a new session handler.
%% @hidden
start_fsm(AccessRequest, RadiusFsm, ClientAddress, ClientPort, Secret,
		PasswordReq, SessionID, Identity, Sup, #state{address = ServerAddress,
		port = ServerPort, ttls_sup = Sup} = State) ->
	StartArgs = [radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
			RadiusFsm, Secret, PasswordReq, SessionID, AccessRequest],
	ChildSpec = [StartArgs],
	start_fsm1(ServerAddress, ServerPort, RadiusFsm, SessionID,
			Identity, Sup, ChildSpec, State);
start_fsm(AccessRequest, RadiusFsm, ClientAddress, ClientPort, Secret,
		PasswordReq, SessionID, Identity, Sup,  #state{address = ServerAddress,
		port = ServerPort} = State) ->
	StartArgs = [radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
			RadiusFsm, Secret, PasswordReq, SessionID, AccessRequest],
	ChildSpec = [StartArgs, []],
	start_fsm1(ServerAddress, ServerPort, RadiusFsm, SessionID,
			Identity, Sup, ChildSpec, State).
%% @hidden
start_fsm1(ServerAddress, ServerPort,
		RadiusFsm, SessionID, Identity, Sup, ChildSpec,
		#state{ttls_sup = TtlsSup, handlers = Handlers} = State) ->
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, FsmSup} when Sup == TtlsSup ->
			Children = supervisor:which_children(FsmSup),
			{_, Fsm, _, _} = lists:keyfind(ocs_eap_ttls_fsm, 1, Children),
			link(Fsm),
			NewHandlers = gb_trees:enter(SessionID, {Fsm, Identity}, Handlers),
			State#state{handlers = NewHandlers};
		{ok, Fsm} ->
			link(Fsm),
			NewHandlers = gb_trees:enter(SessionID, {Fsm, Identity}, Handlers),
			State#state{handlers = NewHandlers};
		{error, Reason} ->
			error_logger:error_report(["Error starting session handler",
					{error, Reason}, {supervisor, Sup},
					{address, ServerAddress}, {port, ServerPort},
					{radius_fsm, RadiusFsm}, {session, SessionID}]),
			State
	end.

-spec get_alternate(PreferenceOrder, AlternateMethods, State) -> Result
	when
		PreferenceOrder :: [ocs:eap_method()],
		AlternateMethods :: binary() | [byte()], 
		State :: state(),
		Result :: {ok, SupervisorModule} | {error, none},
		SupervisorModule :: pid().

get_alternate(PreferenceOrder, AlternateMethods, State) 
		when is_binary(AlternateMethods) ->
	get_alternate(PreferenceOrder,
			binary_to_list(AlternateMethods), State);
get_alternate([pwd | T], AlternateMethods, 
		#state{pwd_sup = Sup} = State) -> 
	case lists:member(?PWD, AlternateMethods) of
		true ->
			{ok, Sup};
		false ->
			get_alternate(T, AlternateMethods, State)
	end;
get_alternate([ttls | T], AlternateMethods, 
		#state{ttls_sup = Sup} = State) -> 
	case lists:member(?TTLS, AlternateMethods) of
		true ->
			{ok, Sup};
		false ->
			get_alternate(T, AlternateMethods, State)
	end;
get_alternate([], _AlternateMethods, _State) -> 
	{error, none}.

