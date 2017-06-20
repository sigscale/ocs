%%% ocs_diameter_auth_port_server.erl
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
%%% 	for authentication in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://tools.ietf.org/pdf/rfc6733.pdf">
%%% 	RFC6733 - DIAMETER base protocol</a>
%%%
%%% @reference <a href="https://tools.ietf.org/pdf/rfc7155.pdf">
%%% 	RFC7155 - DIAMETER Network Access Server Application</a>
%%%
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4072.pdf">
%%% 	RFC4072 - DIAMETER Extensible Authentication Protocol (EAP) Application</a>
%%%
-module(ocs_diameter_auth_port_server).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the ocs_diameter_auth_port_server API
-export([]).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_nas_application_rfc7155.hrl").
-include("../include/diameter_gen_eap_application_rfc4072.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").

-record(state,
		{auth_port_sup :: pid(),
		address :: inet:ip_address(),
		port :: inet:port(),
		method_prefer :: ocs:eap_method(),
		method_order :: [ocs:eap_method()],
		simple_auth_sup :: undefined | pid(),
		ttls_sup :: undefined | pid(),
		pwd_sup :: undefined | pid(),
		cb_fsms = gb_trees:empty() :: gb_trees:tree(
				Key :: (AuthFsm :: pid()), Value :: (CbProc :: pid())),
		handlers = gb_trees:empty() :: gb_trees:tree(
				Key :: (SessionId :: string()), Value :: (Fsm :: pid()))}).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.

%%----------------------------------------------------------------------
%%  The ocs_diameter_auth_port_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_diameter_auth_port_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result 
	when
		Args :: list(),
		Result :: {ok, State :: state()}
		| {ok, State :: state(), Timeout :: non_neg_integer() | infinity}
		| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} server.
%% 	Args :: [Sup :: pid(), Module :: atom(), Port :: non_neg_integer(),
%% 	Address :: inet:ip_address()].
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([AuthPortSup, Address, Port, Options]) ->
	MethodPrefer = proplists:get_value(eap_method_prefer, Options, pwd),
	MethodOrder = proplists:get_value(eap_method_order, Options, [pwd, ttls]),
	State = #state{auth_port_sup = AuthPortSup, address = Address,
			port = Port, method_prefer = MethodPrefer, method_order = MethodOrder},
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
		From :: {Pid :: pid(), Tag :: any()},
		State :: state(),
		Result :: {reply, Reply :: term(), NewState :: state()}
		| {reply, Reply :: term(), NewState :: state(), Timeout :: non_neg_integer() | infinity}
		| {reply, Reply :: term(), NewState :: state(), hibernate}
		| {noreply, NewState :: state()}
		| {noreply, NewState :: state(), Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: state(), hibernate}
		| {stop, Reason :: term(), Reply :: term(), NewState :: state()}
		| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
handle_call({diameter_request, Caps, Request, Eap}, CbProc, State) ->
	request(Caps, Eap, Request, CbProc, State).

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(), 
		State :: state(),
		Result :: {noreply, NewState :: state()}
		| {noreply, NewState :: state(), Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: state(), hibernate}
		| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast({AuthFsm, Request}, #state{cb_fsms = CbHandler} = State) ->
	case gb_trees:lookup(AuthFsm, CbHandler) of
		{value, CbProc} ->
			gen_server:reply(CbProc, Request),
			NewCbHandler = gb_trees:delete(AuthFsm, CbHandler),
			{noreply, State#state{cb_fsms = NewCbHandler}};
		none ->
			{noreply, State}
	end.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(), 
		State :: state(),
		Result :: {noreply, NewState :: state()}
		| {noreply, NewState :: state(), Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: state(), hibernate}
		| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, #state{auth_port_sup = AuthPortSup} = State) ->
	Children = supervisor:which_children(AuthPortSup),
	{_, SimpleAuthSup, _, _} = lists:keyfind(ocs_simple_auth_fsm_sup, 1, Children),
	{_, PwdSup, _, _} = lists:keyfind(ocs_eap_pwd_fsm_sup, 1, Children),
	{_, TtlsSup, _, _} = lists:keyfind(ocs_eap_ttls_fsm_sup_sup, 1, Children),
	NewState = State#state{simple_auth_sup = SimpleAuthSup, pwd_sup = PwdSup,
			ttls_sup = TtlsSup},
	{noreply, NewState};
handle_info({'EXIT', Fsm, {shutdown, SessionId}},
		#state{handlers = Handlers} = State) ->
	case gb_trees:lookup(SessionId, Handlers) of
		{value, _Fsm1} ->
			NewHandlers = gb_trees:delete(SessionId, Handlers),
			{noreply, State#state{handlers = NewHandlers}};
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
	ocs_log:auth_close(),
	stop.

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		State :: state(), 
		Extra :: term(),
		Result :: {ok, NewState :: state()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec request(Caps, Eap, Request, CbProc, State) -> Reply
	when
		Caps :: capabilities(),
		Eap :: none | {eap, #eap_packet{}},
		Request :: #diameter_nas_app_AAR{} | #diameter_nas_app_STR{}
				| #diameter_eap_app_DER{},
		CbProc :: {pid(), term()},
		State :: state(),
		Reply :: {reply, Answer, State} | {noreply, State},
		Answer :: #diameter_nas_app_AAA{} | #diameter_nas_app_STA{}
				| #diameter_eap_app_DEA{}.
%% @doc Based on the DIAMETER request generate appropriate DIAMETER
%% answer.
%% @hidden
request(Caps, none, Request, CbProc, State) when is_record(Request, diameter_nas_app_AAR) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm,_}} = Caps,
	request1(none, OHost, ORealm, Request, CbProc, State);
request(Caps, {eap, <<_:32, ?Identity, Identity/binary>>}, Request, CbProc, State)
		when is_record(Request, diameter_eap_app_DER) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm,_}} = Caps,
	request1({identity, Identity}, OHost, ORealm, Request, CbProc, State);
request(Caps, {eap, <<_, EapId, _:16, ?LegacyNak, Data/binary>>}, Request, CbProc, State)
		when is_record(Request, diameter_eap_app_DER) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm,_}} = Caps,
	request1({legacy_nak, EapId, Data}, OHost, ORealm, Request, CbProc, State);
request(Caps, Eap, Request, CbProc, State) when is_record(Request, diameter_eap_app_DER) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm,_}} = Caps,
	request1(Eap, OHost, ORealm, Request, CbProc, State);
request(Caps, none, Request, _CbProc, State) when is_record(Request, diameter_nas_app_STR) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm,_}} = Caps,
	SessionId = Request#diameter_nas_app_STR.'Session-Id',
	try
		[Username] = Request#diameter_nas_app_STR.'User-Name',
		F = fun() ->
			case mnesia:read(subscriber, Username, write) of
				[#subscriber{disconnect = false} = Entry] ->
					NewEntry = Entry#subscriber{disconnect = true},
					mnesia:write(subscriber, NewEntry, write);
				[#subscriber{disconnect = true}] ->
					ok
			end
		end,
		mnesia:transaction(F)
	of
		{atomic, ok} ->
			Answer = #diameter_nas_app_STA{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			{reply, Answer, State}
	catch
		_:_ ->
			Answer = #diameter_nas_app_STA{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			{reply, Answer, State}
	end.
%% @hidden
request1(EapType, OHost, ORealm, Request, CbProc, #state{handlers = Handlers,
		method_prefer = MethodPrefer, method_order = MethodOrder,
		cb_fsms = FsmHandler} = State) ->
	{SessionId, AuthType} = get_attibutes(Request),
	try
		case gb_trees:lookup(SessionId, Handlers) of
			none ->
				case EapType of
					{_, _Identity} when MethodPrefer == pwd ->
						PwdSup = State#state.pwd_sup,
						{_Fsm, NewState} = start_fsm(PwdSup, 5, SessionId, AuthType, OHost,
								ORealm, [], CbProc, Request, State),
						{noreply, NewState};
					none ->
						#diameter_nas_app_AAR{'User-Name' = [UserName],
								'User-Password' = [Password]} = Request,
						SimpleAuthSup = State#state.simple_auth_sup,
						case {UserName, Password} of
							{UserName, Password} when (UserName /= undefined andalso
									Password /= undefined) ->
								{_Fsm, NewState} = start_fsm(SimpleAuthSup, 1, SessionId, AuthType,
										OHost, ORealm, [UserName, Password], CbProc, Request, State),
								{noreply, NewState};
							_ ->
								Answer = #diameter_nas_app_AAA{
										'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
										'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
										'Session-Id' = SessionId},
								{reply, Answer, State}
							end
					end;
			{value, ExistingFsm} ->
				case EapType of
					{legacy_nak, EapId, AlternateMethods} ->
						case get_alternate(MethodOrder, AlternateMethods, State) of
							{ok , Sup} ->
								gen_fsm:send_event(ExistingFsm, Request),
								NewEapPacket = #eap_packet{code = response,
										type = ?Identity, identifier = EapId, data = <<>>},
								NewEapMessage = ocs_eap_codec:eap_packet(NewEapPacket),
								NewRequest = Request#diameter_eap_app_DER{'EAP-Payload' = NewEapMessage},
								{NewFsm, NewState} = start_fsm(Sup, 5, SessionId, AuthType,
										OHost, ORealm, [], CbProc, NewRequest, State),
								{noreply, NewState};
							{error, none} ->
								NewEapPacket = #eap_packet{code = failure, identifier = EapId},
								NewEapMessage = ocs_eap_codec:eap_packet(NewEapPacket),
								Answer1 = #diameter_eap_app_DEA{'Session-Id' = SessionId,
										'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
										'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
										'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
										'EAP-Payload' = [NewEapMessage]},
								{reply, Answer1, State}
						end;
					none ->
						Answer = #diameter_nas_app_AAA{'Session-Id' = SessionId,
								'Auth-Application-Id' = 1, 'Auth-Request-Type' = AuthType,
								'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
								'Origin-Host' = OHost, 'Origin-Realm' = ORealm },
						{reply, Answer, State};
					{eap, _Eap} ->
						NewFsmHandler = gb_trees:enter(ExistingFsm, CbProc, FsmHandler),
						gen_fsm:send_event(ExistingFsm, Request),
						{noreply, State#state{cb_fsms = NewFsmHandler}}
				end
		end
	catch
		_:_ ->
			Error = #diameter_nas_app_AAA{
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'Session-Id' = SessionId},
			{reply, Error, State}
	end.

%% @hidden
-spec start_fsm(AuthSup, AppId, SessionId, AuthRequestType, OHost,
		ORealm, Options, CbProc, Request, State) -> Result
	when
		AuthSup :: pid(),
		AppId :: non_neg_integer(),
		SessionId :: string(),
		AuthRequestType :: 1..3,
		OHost :: string(),
		ORealm :: string(),
		Options :: list(),
		CbProc :: {pid(), term()},
		Request :: #diameter_nas_app_AAR{} | #diameter_eap_app_DER{},
		State :: state(),
		Result :: {AuthFsm, State},
		AuthFsm :: undefined | pid().
start_fsm(AuthSup, AppId, SessId, Type, OHost, ORealm,
		Options, CbProc, Request, #state{handlers = Handlers, address = Address,
		port = Port, ttls_sup = AuthSup} = State) ->
	StartArgs = [diameter, Address, Port, SessId, AppId, Type, OHost,
			ORealm, Request, Options],
	ChildSpec = [StartArgs],
	start_fsm1(AuthSup, ChildSpec, SessId, CbProc, State);
start_fsm(AuthSup, AppId, SessId, Type, OHost, ORealm,
		Options, CbProc, Request, #state{handlers = Handlers, address = Address,
		port = Port} = State) ->
	StartArgs = [diameter, Address, Port, SessId, AppId, Type, OHost,
			ORealm, Request, Options],
	ChildSpec = [StartArgs, []],
	start_fsm1(AuthSup, ChildSpec, SessId, CbProc, State).
%% @hidden
start_fsm1(AuthSup, ChildSpec, SessId, CbProc, #state{handlers = Handlers,
		cb_fsms = FsmHandler, ttls_sup = TtlsSup} = State) ->
	case supervisor:start_child(AuthSup, ChildSpec) of
		{ok, AuthFsmSup} when AuthSup == TtlsSup ->
			Children = supervisor:which_children(AuthFsmSup),
			{_, Fsm, _, _} = lists:keyfind(ocs_eap_ttls_fsm, 1, Children),
			link(Fsm),
			NewHandlers = gb_trees:enter(SessId, Fsm, Handlers),
			NewFsmHandler = gb_trees:enter(Fsm, CbProc, FsmHandler),
			NewState = State#state{handlers = NewHandlers, cb_fsms = NewFsmHandler},
			{Fsm, NewState};
		{ok, AuthFsm} ->
			link(AuthFsm),
			NewHandlers = gb_trees:enter(SessId, AuthFsm, Handlers),
			NewFsmHandler = gb_trees:enter(AuthFsm, CbProc, FsmHandler),
			NewState = State#state{handlers = NewHandlers, cb_fsms = NewFsmHandler},
			{AuthFsm, NewState};
		{error, Reason} ->
			error_logger:error_report(["Error starting session handler",
					{error, Reason}, {supervisor, AuthSup},{session_id, SessId}]),
			{undefined, State}
	end.

-spec get_attibutes(Request) -> {SessionId, AuthRequestType}
	when
		Request :: #diameter_nas_app_AAR{} | #diameter_eap_app_DER{},
		SessionId :: string(),
		AuthRequestType :: integer().
%% @doc Return values for Session-Id and Auth-Request-Type attributes in
%% DIAMETER Request.
%% @hidden
get_attibutes(Request) when is_record(Request, diameter_nas_app_AAR) ->
	#diameter_nas_app_AAR{'Session-Id' = SessionId,
			'Auth-Request-Type' = Type} = Request,
	{SessionId, Type};
get_attibutes(Request) when is_record(Request, diameter_eap_app_DER) ->
	#diameter_eap_app_DER{'Session-Id' = SessionId,
		'Auth-Request-Type' = Type} = Request,
	{SessionId, Type}.

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

