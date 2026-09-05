%%% ocs_deregister_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2026 SigScale Global Inc.
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
%%% 	module implements procedures for deregistration of user equipment (UE)
%%% 	in non-3GPP access networks.
%%%
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=29&amp;GSMSpecPart2=273">
%%% 	3GPP TS 29.273 - 3GPP EPS AAA interfaces</a>
%%%
-module(ocs_deregister_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([idle/3, abort/3]).

-include("ocs.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_3gpp.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-record(statedata,
		{identity :: binary() | undefined,
		imsi :: binary() | undefined,
		origin_host :: binary(),
		origin_realm :: binary(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		service :: tuple() | undefined,
		hss_realm :: string() | undefined,
		hss_host :: string() | undefined,
		request :: #'3gpp_swx_RTR'{} | undefined,
		session_id :: string(),
		sessions = [] :: [#session{}] | undefined,
		from :: gen_statem:from() | undefined}).
-type statedata() :: #statedata{}.
-type state() :: idle | abort.

-define(IANA_PEN_3GPP, 10415).
-define(STa_APPLICATION_ID, 16777250).
-define(SWm_APPLICATION_ID, 16777264).
-define(SWx_APPLICATION_ID, 16777265).
-define(STa_APPLICATION, ocs_diameter_3gpp_sta_application).
-define(SWm_APPLICATION, ocs_diameter_3gpp_swm_application).
-define(SWx_APPLICATION, ocs_diameter_3gpp_swx_application).

-define(TIMEOUT, 10000).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(PG_CLOSEST(Name),
				case pg:get_local_members(pg_scope_ocs, Name) of
					[] ->
						case pg:get_members(pg_scope_ocs, Name) of
							[] ->
								{error, {no_such_group, Name}};
							[Pid | _] ->
								Pid
						end;
					[Pid | _] ->
						Pid
				end).
	-else.
		-define(PG_CLOSEST(Name), pg2:get_closest_pid(Name)).
	-endif.
-else.
	-define(PG_CLOSEST(Name), pg2:get_closest_pid(Name)).
-endif.

%%----------------------------------------------------------------------
%%  The ocs_deregister_fsm gen_statem call backs
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
init([ServiceName, ServerAddress, ServerPort, ClientAddress,
		ClientPort, SessionId, OriginHost, OriginRealm,
		_DestinationHost, _DestinationRealm] = _Args) ->
	process_flag(trap_exit, true),
	{ok, idle, #statedata{service = ServiceName, session_id = SessionId,
			server_address = ServerAddress, server_port = ServerPort,
			client_address = ClientAddress, client_port = ClientPort,
			origin_host = OriginHost, origin_realm = OriginRealm}}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @private
idle({call, From} = _EventType,
		#'3gpp_swx_RTR'{'User-Name' = Identity,
				'Origin-Realm' = HssRealm, 'Origin-Host' = HssHost,
				'Deregistration-Reason' = #'3gpp_swx_Deregistration-Reason'{
						'Reason-Code' = ReasonCode,
						'Reason-Info' = ReasonInfo}} = EventContent,
		#statedata{session_id = SessionId} = Data) ->
	[IMSI | _] = binary:split(Identity, <<$@>>, []),
	NewData = Data#statedata{request = EventContent,
			from = From, imsi = IMSI, identity = Identity,
			hss_realm = HssRealm, hss_host = HssHost},
	F = fun() ->
			Sessions = mnesia:index_read(session, IMSI, #session.imsi),
			lists:foreach(fun(S) -> mnesia:delete_object(S) end, Sessions),
			Sessions
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_SESSION_ID',
			ReplyAction = {reply, From, response(ResultCode, NewData)},
			{stop_and_reply, shutdown, ReplyAction, NewData};
		{atomic, Sessions}
				when ReasonCode =:= ?'3GPP_SWX_REASON-CODE_PERMANENT_TERMINATION' ->
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			gen_statem:reply(From, response(ResultCode, NewData)),
			send_abort(Sessions, NewData);
		{atomic, _Sessions}
				when ReasonCode =:= ?'3GPP_SWX_REASON-CODE_NEW_SERVER_ASSIGNED' ->
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			ReplyAction = {reply, From, response(ResultCode, NewData)},
			{stop_and_reply, shutdown, ReplyAction, NewData};
		{aborted, Reason} ->
			error_logger:error_report(["Failed user lookup",
					{hss_host, HssHost}, {hss_realm, HssRealm},
					{imsi, IMSI}, {identity, Identity},
					{reason_info, ReasonInfo}, {session, SessionId},
					{error, Reason}]),
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
			ReplyAction = {reply, From, response(ResultCode, NewData)},
			{stop_and_reply, shutdown, ReplyAction, NewData}
	end.

-spec abort(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>abort</em> state.
%% @private
abort(info = _EventType,
		{ok, #'3gpp_sta_ASA'{'Session-Id' = SessionId,
				'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']}} = _EventContent,
		#statedata{sessions = Sessions} = Data) ->
	case lists:keydelete(SessionId, #session.id, Sessions) of
		[] ->
			{stop, shutdown};
		NewSessions ->
			NewData = Data#statedata{sessions = NewSessions},
			TimeoutAction = {timeout, ?TIMEOUT, timeout},
			{next_state, abort, NewData, TimeoutAction}
	end;
abort(info = _EventType,
		{ok, #'3gpp_swm_ASA'{'Session-Id' = SessionId,
				'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']}},
		#statedata{sessions = Sessions} = Data) ->
	case lists:keydelete(SessionId, #session.id, Sessions) of
		[] ->
			{stop, shutdown};
		NewSessions ->
			NewData = Data#statedata{sessions = NewSessions},
			TimeoutAction = {timeout, ?TIMEOUT, timeout},
			{next_state, abort, NewData, TimeoutAction}
	end;
abort(info = _EventType,
		{ok, #'3gpp_sta_ASA'{'Session-Id' = SessionId,
				'Origin-Host' = NasHost, 'Origin-Realm' = NasRealm,
				'Result-Code' = [?'DIAMETER_ERROR_USER_UNKNOWN']}},
		#statedata{sessions = Sessions} = Data) ->
	{value, #session{imsi = IMSI, identity = Identity,
			hss_host = HssHost, hss_realm = HssRealm,
			application = Application},
			NewSessions} = lists:keytake(SessionId,
			#session.id, Sessions),
	error_logger:warning_report(["Unknown user in abort",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{application, Application},
			{imsi, IMSI}, {identity, Identity},
			{session_id, SessionId},
			{result_code, ?'DIAMETER_ERROR_USER_UNKNOWN'}]),
	case NewSessions of
		[] ->
			{stop, shutdown};
		NewSessions ->
			NewData = Data#statedata{sessions = NewSessions},
			TimeoutAction = {timeout, ?TIMEOUT, timeout},
			{next_state, abort, NewData, TimeoutAction}
	end;
abort(info = _EventType,
		{ok, #'3gpp_swm_ASA'{'Session-Id' = SessionId,
				'Origin-Host' = NasHost, 'Origin-Realm' = NasRealm,
				'Result-Code' = [?'DIAMETER_ERROR_USER_UNKNOWN']}},
		#statedata{sessions = Sessions} = Data) ->
	{value, #session{imsi = IMSI, identity = Identity,
			hss_host = HssHost, hss_realm = HssRealm,
			application = Application},
			NewSessions} = lists:keytake(SessionId,
			#session.id, Sessions),
	error_logger:warning_report(["Unknown user in abort",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{application, Application},
			{imsi, IMSI}, {identity, Identity},
			{session_id, SessionId},
			{result_code, ?'DIAMETER_ERROR_USER_UNKNOWN'}]),
	case NewSessions of
		[] ->
			{stop, shutdown};
		NewSessions ->
			NewData = Data#statedata{sessions = NewSessions},
			TimeoutAction = {timeout, ?TIMEOUT, timeout},
			{next_state, abort, NewData, TimeoutAction}
	end;
abort(info = _EventType,
		{ok, #'diameter_base_answer-message'{'Session-Id' = SessionId,
				'Origin-Host' = NasHost, 'Origin-Realm' = NasRealm,
				'Result-Code' = ResultCode}},
		#statedata{sessions = Sessions} = Data) ->
	{value, #session{imsi = IMSI, identity = Identity,
			hss_host = HssHost, hss_realm = HssRealm,
			application = Application},
			NewSessions} = lists:keytake(SessionId,
			#session.id, Sessions),
	error_logger:warning_report(["Unexpected abort result",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{application, Application},
			{imsi, IMSI}, {identity, Identity},
			{session_id, SessionId},
			{result_code, ResultCode}]),
	case NewSessions of
		[] ->
			{stop, shutdown};
		NewSessions ->
			NewData = Data#statedata{sessions = NewSessions},
			TimeoutAction = {timeout, ?TIMEOUT, timeout},
			{next_state, abort, NewData, TimeoutAction}
	end;
abort(timeout = _EventType, timeout = _EventContent,
		#statedata{sessions = []} = _Data) ->
	{stop, shutdown}.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, _Data) ->
	ok.

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
%%  internal functions
%%----------------------------------------------------------------------

-spec response(ResultCode, Data) -> Result
	when
		ResultCode :: pos_integer(),
		Data :: #statedata{},
		Result :: #'3gpp_swx_RTA'{}.
%% @doc Create DIAMETER response.
%% @hidden
response(ResultCode,
		#statedata{request = #'3gpp_swx_RTR'{} = Request,
				session_id = SessionId,
				server_address = ServerAddress,
				server_port = ServerPort,
				client_address = ClientAddress,
				client_port = ClientPort,
				origin_host = OriginHost,
				origin_realm = OriginRealm} = _Data)
		when is_integer(ResultCode), is_binary(OriginHost),
		is_binary(OriginRealm) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_swx_RTA'{'Session-Id' = SessionId,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'Vendor-Specific-Application-Id' = #'3gpp_swx_Vendor-Specific-Application-Id'{
					'Vendor-Id' = ?IANA_PEN_3GPP,
					'Auth-Application-Id' = [?SWx_APPLICATION_ID]},
			'Auth-Session-State' = ?'DIAMETER_BASE_AUTH-SESSION-STATE_NO_STATE_MAINTAINED'},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	Answer.

-spec send_abort(Sessions, Data) -> Result
	when
		Sessions :: [#session{}],
		Data :: #statedata{},
		Result :: {next_state, abort, Data, Action},
		Action :: tuple().
%% @doc Send DIAMETER Abort-Session-Reqest (ASR).
%% @hidden
send_abort([#session{id = AccessSessionId,
				application = ?STa_APPLICATION_ID,
				identity = Identity,
				nas_host = NasHost,
				nas_realm = NasRealm} = H | T],
		#statedata{origin_host = OriginHost,
				origin_realm = OriginRealm,
				service = Service,
				sessions = Sessions} = Data) ->
	Request = #'3gpp_sta_ASR'{'Session-Id' = AccessSessionId,
			'User-Name' = [Identity],
			'Origin-Realm' = OriginRealm,
			'Origin-Host' = OriginHost,
			'Destination-Realm' = NasRealm,
			'Destination-Host' = NasHost,
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Auth-Session-State' = ?'DIAMETER_BASE_AUTH-SESSION-STATE_NO_STATE_MAINTAINED'},
	diameter:call(Service, ?STa_APPLICATION,
			Request, [detach, {extra, [self()]}]),
	NewData = Data#statedata{sessions = [H | Sessions]},
	send_abort(T, NewData);
send_abort([#session{id = AccessSessionId,
				application = ?SWm_APPLICATION_ID,
				identity = Identity,
				nas_host = NasHost,
				nas_realm = NasRealm} = H | T],
		#statedata{origin_host = OriginHost,
				origin_realm = OriginRealm,
				service = Service,
				sessions = Sessions} = Data) ->
	Request = #'3gpp_swm_ASR'{'Session-Id' = AccessSessionId,
			'User-Name' = [Identity],
			'Origin-Realm' = OriginRealm,
			'Origin-Host' = OriginHost,
			'Destination-Realm' = NasRealm,
			'Destination-Host' = NasHost,
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Session-State' = ?'DIAMETER_BASE_AUTH-SESSION-STATE_NO_STATE_MAINTAINED'},
	diameter:call(Service, ?STa_APPLICATION,
			Request, [detach, {extra, [self()]}]),
	NewData = Data#statedata{sessions = [H | Sessions]},
	send_abort(T, NewData);
send_abort([#session{id = AccessSessionId,
				application = undefined,
				nas_address = NasAddress,
				imsi = IMSI,
				identity = Identity} | T],
		Data) ->
	case ?PG_CLOSEST(ocs_radius_acct_port_sup) of
		{error, Reason} ->
			error_logger:error_report(["Failed to initiate session disconnect",
					{module, ?MODULE}, {address, NasAddress},
					{imsi, IMSI}, {identity, Identity},
					{session, AccessSessionId}, {error, Reason}]);
		DiscSup ->
			DiscArgs = [Identity, AccessSessionId],
			StartArgs = [DiscArgs, []],
			supervisor:start_child(DiscSup, StartArgs),
			send_abort(T, Data)
	end;
send_abort([], Data) ->
	Action = {timeout, ?TIMEOUT, timeout},
	{next_state, abort, Data, Action}.

