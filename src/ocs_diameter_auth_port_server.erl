%%% ocs_diameter_auth_port_server.erl
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
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_s6b_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").

-record(state,
		{auth_port_sup :: pid(),
		address :: inet:ip_address(),
		port :: inet:port_number(),
		method_prefer :: ocs:eap_method(),
		method_order :: [ocs:eap_method()],
		simple_auth_sup :: undefined | pid(),
		ttls_sup :: undefined | pid(),
		aka_sup :: undefined | pid(),
		akap_sup :: undefined | pid(),
		pwd_sup :: undefined | pid(),
		dereg_sup :: undefined | pid(),
		term_sup :: undefined | pid(),
		pgw_sup :: undefined | pid(),
		cb_fsms = gb_trees:empty() :: gb_trees:tree(
				Key :: (AuthFsm :: pid()), Value :: (CbProc :: pid())),
		handlers = gb_trees:empty() :: gb_trees:tree(
				Key :: (SessionId :: string()), Value :: (Fsm :: pid()))}).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.

-define(NAS_APPLICATION_ID, 1).
-define(EAP_APPLICATION_ID, 5).
-define(STa_APPLICATION_ID, 16777250).
-define(SWm_APPLICATION_ID, 16777264).

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
	MethodPrefer = proplists:get_value(eap_method_prefer, Options, akap),
	MethodOrder = proplists:get_value(eap_method_order, Options,
			[akap, aka, pwd, ttls]),
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
handle_call({diameter_request, Caps, ClientAddr, ClientPort,
		PasswordReq, Trusted, Request, Eap}, CbProc, State) ->
	request(Caps, ClientAddr, ClientPort, Eap,
			PasswordReq, Trusted, Request, CbProc, State).

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
	{_, AkaSup, _, _} = lists:keyfind(ocs_eap_aka_fsm_sup_sup, 1, Children),
	{_, AkapSup, _, _} = lists:keyfind(ocs_eap_akap_fsm_sup_sup, 1, Children),
	{_, DeregSup, _, _} = lists:keyfind(ocs_deregister_fsm_sup, 1, Children),
	{_, TermSup, _, _} = lists:keyfind(ocs_deregister_fsm_sup, 1, Children),
	{_, PgwSup, _, _} = lists:keyfind(ocs_pgw_fsm_sup, 1, Children),
	NewState = State#state{simple_auth_sup = SimpleAuthSup,
			pwd_sup = PwdSup, ttls_sup = TtlsSup,
			aka_sup = AkaSup, akap_sup = AkapSup,
			dereg_sup = DeregSup, term_sup = TermSup, pgw_sup = PgwSup},
	{noreply, NewState};
handle_info({'EXIT', Fsm, {shutdown, SessionId}},
		#state{handlers = Handlers} = State) ->
	case gb_trees:lookup(SessionId, Handlers) of
		{value, Fsm} ->
			NewHandlers = gb_trees:delete(SessionId, Handlers),
			{noreply, State#state{handlers = NewHandlers}};
		{value, _} ->
			{noreply, State};
		none ->
			{noreply, State}
	end;
handle_info({'EXIT', _Pid, noconnection}, State) ->
	{noreply, State};
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

-spec request(Caps, ClientAddress, ClientPort, Eap,
		PasswordReq, Trusted, Request, CbProc, State) -> Reply
	when
		Caps :: capabilities(),
		ClientAddress :: inet:ip_address(),
		ClientPort :: inet:port_number(),
		Eap :: none | {eap, #eap_packet{}},
		PasswordReq :: boolean(),
		Trusted :: boolean(),
		Request :: #diameter_nas_app_AAR{} | #diameter_nas_app_STR{}
				| #diameter_eap_app_DER{} | #'3gpp_sta_DER'{}
				| #'3gpp_swm_DER'{} | #'3gpp_s6b_AAR'{},
		CbProc :: {pid(), term()},
		State :: state(),
		Reply :: {reply, Answer, State} | {noreply, State},
		Answer ::#diameter_nas_app_AAA{} | #diameter_nas_app_STA{}
				| #diameter_eap_app_DEA{} | #'3gpp_sta_DEA'{}
				| #'3gpp_swm_DEA'{} | #'3gpp_s6b_AAA'{}.
%% @doc Generate appropriate DIAMETER answer.
%% @hidden
request(Caps, Address, Port, none, PasswordReq, Trusted, Request, CbProc, State)
		when is_record(Request, diameter_nas_app_AAR) ->
	#diameter_caps{origin_host = {OHost,DHost}, origin_realm = {ORealm,DRealm}} = Caps,
	request1(none, Address, Port, PasswordReq, Trusted, OHost,
			ORealm, DHost, DRealm, Request, CbProc, State);
request(Caps, Address, Port, {eap, <<_:32, ?Identity, Identity/binary>>},
		PasswordReq, Trusted, Request, CbProc, State)
		when is_record(Request, diameter_eap_app_DER);
		is_record(Request, '3gpp_swm_DER'); is_record(Request, '3gpp_sta_DER') ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	request1({identity, Identity}, Address, Port, PasswordReq, Trusted,
			OHost, ORealm, DHost, DRealm, Request, CbProc, State);
request(Caps, Address, Port, {eap, <<_, EapId, _:16, ?LegacyNak, Data/binary>>},
		PasswordReq, Trusted, Request, CbProc, State)
		when is_record(Request, diameter_eap_app_DER);
		is_record(Request, '3gpp_swm_DER'); is_record(Request, '3gpp_sta_DER') ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	request1({legacy_nak, EapId, Data}, Address, Port, PasswordReq, Trusted,
			OHost, ORealm, DHost, DRealm, Request, CbProc, State);
request(Caps, Address, Port, Eap, PasswordReq, Trusted, Request, CbProc, State)
		when is_record(Request, diameter_eap_app_DER);
		is_record(Request, '3gpp_swm_DER'); is_record(Request, '3gpp_sta_DER') ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	request1(Eap, Address, Port, PasswordReq, Trusted, OHost,
			ORealm, DHost, DRealm, Request, CbProc, State);
request(Caps, _Address, _Port, none, _PasswordReq, _Trusted, Request, _CbProc, State)
		when is_record(Request, diameter_nas_app_STR) ->
	#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm,_}} = Caps,
	SessionId = Request#diameter_nas_app_STR.'Session-Id',
	try
		[Username] = Request#diameter_nas_app_STR.'User-Name',
		F = fun() ->
			case mnesia:read(service, Username, write) of
				[#service{disconnect = false} = Entry] ->
					NewEntry = Entry#service{disconnect = true},
					mnesia:write(service, NewEntry, write);
				[#service{disconnect = true}] ->
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
	end;
request(Caps, ClientAddress, ClientPort, none, _PasswordReq, _Trusted,
		#'3gpp_swx_RTR'{'Session-Id' = SessionId} = Request, _CbProc,
		#state{address = ServerAddress, port = ServerPort,
		dereg_sup = Sup} = State) ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	ChildSpec = [ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessionId, OHost, ORealm, DHost, DRealm, Request],
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, _Fsm} ->
			{noreply, State};
		{error, Reason} ->
			error_logger:error_report(["Error starting SWx UE deregister handler",
					{error, Reason}, {supervisor, Sup}, {session_id, SessionId}]),
			Answer = #'3gpp_swx_RTA'{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			{reply, Answer, State}
	end;
request(Caps, ClientAddress, ClientPort, none, _PasswordReq, _Trusted,
		#'3gpp_sta_STR'{'Session-Id' = SessionId} = Request, _CbProc,
		#state{address = ServerAddress, port = ServerPort,
		dereg_sup = Sup} = State) ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	ChildSpec = [ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessionId, OHost, ORealm, DHost, DRealm, Request],
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, _Fsm} ->
			{noreply, State};
		{error, Reason} ->
			error_logger:error_report(["Error starting STa session termination handler",
					{error, Reason}, {supervisor, Sup}, {session_id, SessionId}]),
			Answer = #'3gpp_sta_STA'{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			{reply, Answer, State}
	end;
request(Caps, ClientAddress, ClientPort, none, _PasswordReq, _Trusted,
		#'3gpp_swm_STR'{'Session-Id' = SessionId} = Request, _CbProc,
		#state{address = ServerAddress, port = ServerPort,
		dereg_sup = Sup} = State) ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	ChildSpec = [ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessionId, OHost, ORealm, DHost, DRealm, Request],
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, _Fsm} ->
			{noreply, State};
		{error, Reason} ->
			error_logger:error_report(["Error starting SWm session termination handler",
					{error, Reason}, {supervisor, Sup}, {session_id, SessionId}]),
			Answer = #'3gpp_swm_STA'{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			{reply, Answer, State}
	end;
request(Caps, ClientAddress, ClientPort, none, _PasswordReq, _Trusted,
		#'3gpp_s6b_AAR'{'Session-Id' = SessionId} = Request, _CbProc,
		#state{address = ServerAddress, port = ServerPort,
		pgw_sup = Sup} = State) ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	ChildSpec = [ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessionId, OHost, ORealm, DHost, DRealm, Request],
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, _Fsm} ->
			{noreply, State};
		{error, Reason} ->
			error_logger:error_report(["Error starting PGW session handler",
					{error, Reason}, {supervisor, Sup}, {session_id, SessionId}]),
			Answer = #'3gpp_s6b_AAA'{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			{reply, Answer, State}
	end.

%% @hidden
request1(EapType, Address, Port, PasswordReq, Trusted,
		OHost, ORealm, DHost, DRealm, Request, CbProc,
		#state{handlers = Handlers} = State) ->
	{SessionId, AuthRequestType} = get_attributes(Request),
	request2(EapType, SessionId, AuthRequestType,
			gb_trees:lookup(SessionId, Handlers),
			Address, Port, PasswordReq, Trusted, OHost, ORealm,
			DHost, DRealm, Request, CbProc, State).
%% @hidden
request2({_, _Identity}, SessionId, AuthRequestType, none, Address, Port,
		PasswordReq, Trusted, OHost, ORealm, DHost, DRealm, Request, CbProc,
		#state{aka_sup = Sup, method_prefer = aka} = State) ->
	start_fsm(Sup, Address, Port, PasswordReq, Trusted, SessionId,
			AuthRequestType, OHost, ORealm, DHost, DRealm, [], CbProc, Request, State);
request2({_, _Identity}, SessionId, AuthRequestType, none, Address, Port,
		PasswordReq, Trusted, OHost, ORealm, DHost, DRealm, Request, CbProc,
		#state{akap_sup = Sup, method_prefer = akap} = State) ->
	start_fsm(Sup, Address, Port, PasswordReq, Trusted, SessionId,
			AuthRequestType, OHost, ORealm, DHost, DRealm, [], CbProc, Request, State);
request2({_, _Identity}, SessionId, AuthRequestType, none, Address, Port,
		PasswordReq, Trusted, OHost, ORealm, DHost, DRealm, Request, CbProc,
		#state{pwd_sup = Sup, method_prefer = pwd} = State) ->
	start_fsm(Sup, Address, Port, PasswordReq, Trusted, SessionId,
			AuthRequestType, OHost, ORealm, DHost, DRealm, [], CbProc, Request, State);
request2({_, _Identity}, SessionId, AuthRequestType, none, Address, Port,
		PasswordReq, Trusted, OHost, ORealm, DHost, DRealm, Request, CbProc,
		#state{ttls_sup = Sup, method_prefer = ttls} = State) ->
	start_fsm(Sup, Address, Port, PasswordReq, Trusted, SessionId,
			AuthRequestType, OHost, ORealm, DHost, DRealm, [], CbProc, Request, State);
request2(none, SessionId, AuthRequestType, none, Address, Port,
		PasswordReq, Trusted, OHost, ORealm, DHost, DRealm,
		#diameter_nas_app_AAR{'User-Name' = [UserName],
		'User-Password' = [Password]} = Request,
		CbProc, #state{simple_auth_sup = Sup} = State) ->
	start_fsm(Sup, Address, Port, PasswordReq, Trusted, SessionId,
			AuthRequestType, OHost, ORealm, DHost, DRealm, [UserName, Password],
			CbProc, Request, State);
request2({legacy_nak, EapId, AlternateMethods},
		SessionId, AuthRequestType, {value, ExistingFsm}, Address, Port,
		PasswordReq, Trusted, OHost, ORealm, DHost, DRealm,
		#diameter_eap_app_DER{} = Request, CbProc,
		#state{method_order = MethodOrder} = State) ->
	try get_alternate(MethodOrder, AlternateMethods, State) of
		{ok, Sup} ->
			gen_fsm:send_event(ExistingFsm, Request),
			NewEapPacket = #eap_packet{code = response,
					type = ?Identity, identifier = EapId, data = <<>>},
			NewEapMessage = ocs_eap_codec:eap_packet(NewEapPacket),
			NewRequest = Request#diameter_eap_app_DER{'EAP-Payload' = NewEapMessage},
			start_fsm(Sup, Address, Port, PasswordReq, Trusted,
					SessionId, AuthRequestType, OHost, ORealm, DHost, DRealm,
					[], CbProc, NewRequest, State);
		{error, none} ->
			throw(unable_to_comply)
	catch
		_:_Reason ->
			NewEapPacket = #eap_packet{code = failure, identifier = EapId},
			NewEapMessage = ocs_eap_codec:eap_packet(NewEapPacket),
			Answer = #diameter_eap_app_DEA{'Session-Id' = SessionId,
					'Auth-Application-Id' = ?EAP_APPLICATION_ID,
					'Auth-Request-Type' = AuthRequestType,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'EAP-Payload' = [NewEapMessage]},
			{reply, Answer, State}
	end;
request2(none, SessionId, AuthRequestType, {value, _ExistingFsm},
		_Address, _Port, _PasswordReq, _Trusted, OHost, ORealm,
		_DHost, _DRealm, _Request, _CbProc, State) ->
	Answer = #diameter_nas_app_AAA{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Auth-Request-Type' = AuthRequestType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm },
	{reply, Answer, State};
request2({eap, _Eap}, _SessionId, _AuthRequestType, {value, ExistingFsm},
		_Address, _Port, _PasswordReq, _Trusted, _OHost, _ORealm, _DHost, _DRealm,
		Request, CbProc, #state{cb_fsms = FsmHandler} = State) ->
	NewFsmHandler = gb_trees:enter(ExistingFsm, CbProc, FsmHandler),
	gen_fsm:send_event(ExistingFsm, Request),
	{noreply, State#state{cb_fsms = NewFsmHandler}}.

%% @hidden
-spec start_fsm(AuthSup, ClientAddress, ClientPort, PasswordReq, Trusted, SessionId,
		AuthRequestType, OHost, ORealm, DHost, DRealm, Options, CbProc, Request, State) -> Result
	when
		AuthSup :: pid(),
		ClientAddress :: inet:ip_address(),
		ClientPort :: inet:port_number(),
		PasswordReq :: boolean(),
		Trusted :: boolean(),
		SessionId :: string(),
		AuthRequestType :: 1..3,
		OHost :: string(),
		ORealm :: string(),
		DHost :: string(),
		DRealm :: string(),
		Options :: list(),
		CbProc :: {pid(), term()},
		Request :: #diameter_nas_app_AAR{} | #diameter_eap_app_DER{}
				| #'3gpp_sta_DER'{} | #'3gpp_swm_DER'{},
		State :: state(),
		Result :: {noreply, State} | {reply, Error, State},
		Error :: #diameter_nas_app_AAA{} | #diameter_eap_app_DEA{}
				| #'3gpp_sta_DEA'{} | #'3gpp_swm_DEA'{}.
start_fsm(AuthSup, ClientAddress, ClientPort, PasswordReq, Trusted, SessionId,
		AuthRequestType, OHost, ORealm, DHost, DRealm, Options, CbProc,
		#diameter_nas_app_AAR{'Auth-Application-Id' = AppId} = Request,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	StartArgs = [diameter, ServerAddress, ServerPort, ClientAddress,
			ClientPort, PasswordReq, Trusted, SessionId, AppId, AuthRequestType,
			OHost, ORealm, DHost, DRealm, Request, Options],
	try
		start_fsm1(AuthSup, StartArgs, SessionId, CbProc, State)
	of
		{_Fsm, NewState} ->
			{noreply, NewState}
	catch
		_:_Reason ->
			Error = #diameter_nas_app_AAA{
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'Session-Id' = SessionId},
			{reply, Error, State}
	end;
start_fsm(AuthSup, ClientAddress, ClientPort, PasswordReq, Trusted, SessionId,
		AuthRequestType, OHost, ORealm, DHost, DRealm, Options, CbProc,
		#diameter_eap_app_DER{'Auth-Application-Id' = AppId} = Request,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	StartArgs = [diameter, ServerAddress, ServerPort, ClientAddress,
			ClientPort, PasswordReq, Trusted, SessionId, AppId, AuthRequestType,
			OHost, ORealm, DHost, DRealm, Request, Options],
	try
		start_fsm1(AuthSup, StartArgs, SessionId, CbProc, State)
	of
		{_Fsm, NewState} ->
			{noreply, NewState}
	catch
		_:_Reason ->
			Error = #diameter_eap_app_DEA{
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'Session-Id' = SessionId},
			{reply, Error, State}
	end;
start_fsm(AuthSup, ClientAddress, ClientPort, PasswordReq, Trusted, SessionId,
		AuthRequestType, OHost, ORealm, DHost, DRealm, Options, CbProc,
		#'3gpp_sta_DER'{'Auth-Application-Id' = AppId} = Request,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	StartArgs = [diameter, ServerAddress, ServerPort, ClientAddress,
			ClientPort, PasswordReq, Trusted, SessionId, AppId, AuthRequestType,
			OHost, ORealm, DHost, DRealm, Request, Options],
	try
		start_fsm1(AuthSup, StartArgs, SessionId, CbProc, State)
	of
		{_Fsm, NewState} ->
			{noreply, NewState}
	catch
		_:_Reason ->
			Error = #'3gpp_sta_DEA'{
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'Session-Id' = SessionId},
			{reply, Error, State}
	end;
start_fsm(AuthSup, ClientAddress, ClientPort, PasswordReq, Trusted, SessionId,
		AuthRequestType, OHost, ORealm, DHost, DRealm, Options, CbProc,
		#'3gpp_swm_DER'{'Auth-Application-Id' = AppId} = Request,
		#state{address = ServerAddress, port = ServerPort} = State) ->
	StartArgs = [diameter, ServerAddress, ServerPort, ClientAddress,
			ClientPort, PasswordReq, Trusted, SessionId, AppId, AuthRequestType,
			OHost, ORealm, DHost, DRealm, Request, Options],
	try
		start_fsm1(AuthSup, StartArgs, SessionId, CbProc, State)
	of
		{_Fsm, NewState} ->
			{noreply, NewState}
	catch
		_:_Reason ->
			Error = #'3gpp_swm_DEA'{
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'Session-Id' = SessionId},
			{reply, Error, State}
	end.
%% @hidden
start_fsm1(TtlsSup, StartArgs, SessId, CbProc, #state{handlers = Handlers,
		cb_fsms = FsmHandler, ttls_sup = TtlsSup} = State) ->
	ChildSpec = [StartArgs],
	case supervisor:start_child(TtlsSup, ChildSpec) of
		{ok, TtlsFsmSup} ->
			Children = supervisor:which_children(TtlsFsmSup),
			{_, Fsm, _, _} = lists:keyfind(ocs_eap_ttls_fsm, 1, Children),
			link(Fsm),
			NewHandlers = gb_trees:enter(SessId, Fsm, Handlers),
			NewFsmHandler = gb_trees:enter(Fsm, CbProc, FsmHandler),
			NewState = State#state{handlers = NewHandlers, cb_fsms = NewFsmHandler},
			{Fsm, NewState};
		{error, Reason} ->
			error_logger:error_report(["Error starting session handler",
					{error, Reason}, {supervisor, TtlsSup}, {session_id, SessId}]),
			{undefined, State}
	end;
start_fsm1(AkaSup, StartArgs, SessId, CbProc, #state{handlers = Handlers,
		cb_fsms = FsmHandler, aka_sup = AkaSup} = State) ->
	ChildSpec = [StartArgs],
	case supervisor:start_child(AkaSup, ChildSpec) of
		{ok, AkaFsmSup} ->
			Children = supervisor:which_children(AkaFsmSup),
			{_, Fsm, _, _} = lists:keyfind(ocs_eap_aka_fsm, 1, Children),
			link(Fsm),
			NewHandlers = gb_trees:enter(SessId, Fsm, Handlers),
			NewFsmHandler = gb_trees:enter(Fsm, CbProc, FsmHandler),
			NewState = State#state{handlers = NewHandlers, cb_fsms = NewFsmHandler},
			{Fsm, NewState};
		{error, Reason} ->
			error_logger:error_report(["Error starting session handler",
					{error, Reason}, {supervisor, AkaSup}, {session_id, SessId}]),
			{undefined, State}
	end;
start_fsm1(AkapSup, StartArgs, SessId, CbProc, #state{handlers = Handlers,
		cb_fsms = FsmHandler, akap_sup = AkapSup} = State) ->
	ChildSpec = [StartArgs],
	case supervisor:start_child(AkapSup, ChildSpec) of
		{ok, AkaFsmSup} ->
			Children = supervisor:which_children(AkaFsmSup),
			{_, Fsm, _, _} = lists:keyfind(ocs_eap_akap_fsm, 1, Children),
			link(Fsm),
			NewHandlers = gb_trees:enter(SessId, Fsm, Handlers),
			NewFsmHandler = gb_trees:enter(Fsm, CbProc, FsmHandler),
			NewState = State#state{handlers = NewHandlers, cb_fsms = NewFsmHandler},
			{Fsm, NewState};
		{error, Reason} ->
			error_logger:error_report(["Error starting session handler",
					{error, Reason}, {supervisor, AkapSup}, {session_id, SessId}]),
			{undefined, State}
	end;
start_fsm1(AuthSup, StartArgs, SessId, CbProc,
		#state{handlers = Handlers, cb_fsms = FsmHandler} = State) ->
	ChildSpec = [StartArgs, []],
	case supervisor:start_child(AuthSup, ChildSpec) of
		{ok, AuthFsm} ->
			link(AuthFsm),
			NewHandlers = gb_trees:enter(SessId, AuthFsm, Handlers),
			NewFsmHandler = gb_trees:enter(AuthFsm, CbProc, FsmHandler),
			NewState = State#state{handlers = NewHandlers, cb_fsms = NewFsmHandler},
			{AuthFsm, NewState};
		{error, Reason} ->
			error_logger:error_report(["Error starting session handler",
					{error, Reason}, {supervisor, AuthSup}, {session_id, SessId}]),
			{undefined, State}
	end.

-spec get_attributes(Request) -> {SessionId, AuthRequestType}
	when
		Request :: #diameter_nas_app_AAR{} | #diameter_eap_app_DER{}
				| #'3gpp_sta_DER'{} | #'3gpp_swm_DER'{},
		SessionId :: string(),
		AuthRequestType :: integer().
%% @doc Return values for Session-Id and Auth-Request-Type attributes in
%% DIAMETER Request.
%% @hidden
get_attributes(#diameter_nas_app_AAR{'Session-Id' = SessionId,
		'Auth-Request-Type' = AuthRequestType}) ->
	{SessionId, AuthRequestType};
get_attributes(#diameter_eap_app_DER{'Session-Id' = SessionId,
		'Auth-Request-Type' = AuthRequestType}) ->
	{SessionId, AuthRequestType};
get_attributes(#'3gpp_sta_DER'{'Session-Id' = SessionId,
		'Auth-Request-Type' = AuthRequestType}) ->
	{SessionId, AuthRequestType};
get_attributes(#'3gpp_swm_DER'{'Session-Id' = SessionId,
		'Auth-Request-Type' = AuthRequestType}) ->
	{SessionId, AuthRequestType}.

-spec get_alternate(PreferenceOrder, AlternateMethods, State) -> Result
	when
		PreferenceOrder :: [ocs:eap_method()],
		AlternateMethods :: binary() | [byte()],
		State :: state(),
		Result :: {ok, SupervisorModule} | {error, none},
		SupervisorModule :: pid().
%% @doc Get an alternative EAP method.
%% @hidden
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
get_alternate([aka | T], AlternateMethods,
		#state{aka_sup = Sup} = State) ->
	case lists:member(?AKA, AlternateMethods) of
		true ->
			{ok, Sup};
		false ->
			get_alternate(T, AlternateMethods, State)
	end;
get_alternate([akap | T], AlternateMethods,
		#state{akap_sup = Sup} = State) ->
	case lists:member(?AKAprime, AlternateMethods) of
		true ->
			{ok, Sup};
		false ->
			get_alternate(T, AlternateMethods, State)
	end;
get_alternate([], _AlternateMethods, _State) ->
	{error, none}.

