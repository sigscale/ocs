%%% ocs_terminate_fsm.erl
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
%%% 	module implements procedures for session termination initiated
%%% 	by non-3GPP access network.
%%%
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=29&amp;GSMSpecPart2=273">
%%% 	3GPP TS 29.273 - 3GPP EPS AAA interfaces</a>
%%%
-module(ocs_terminate_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([idle/3, deregister/3]).

-include("ocs.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_3gpp.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-define(IANA_PEN_3GPP, 10415).
-define(STa_APPLICATION_ID, 16777250).
-define(SWm_APPLICATION_ID, 16777264).
-define(SWx_APPLICATION_ID, 16777265).
-define(STa_APPLICATION, ocs_diameter_3gpp_sta_application).
-define(SWm_APPLICATION, ocs_diameter_3gpp_swm_application).
-define(SWx_APPLICATION, ocs_diameter_3gpp_swx_application).
-define(TIMEOUT, 10000).

-record(statedata,
		{identity :: binary() | undefined,
		imsi :: binary() | undefined,
		origin_host :: binary(),
		origin_realm :: binary(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: inet:ip_address(),
		client_port :: pos_integer(),
		service :: tuple() | undefined,
		hss_realm :: string() | undefined,
		hss_host :: string() | undefined,
		nas_host :: string() | undefined,
		nas_realm :: string() | undefined,
		nas_address :: inet:ip_address() | undefined,
		request :: #'3gpp_sta_STR'{} | #'3gpp_swm_STR'{} | undefined,
		session_id :: string(),
		from :: {pid(), reference()} | undefined}).
-type statedata() :: #statedata{}.
-type state() :: idle | abort.

%% 3GPP TS 23.003 19.3.2 Root NAI
-define(PERM_AKA,  $0).
%% 3GPP TS 23.003 19.3.4 Fast Re-auth
-define(FAST_AKA,  $4).
%% 3GPP TS 23.003 19.3.5 Pseudonym
-define(TEMP_AKA,  $2).

%%----------------------------------------------------------------------
%%  The ocs_terminate_fsm gen_statem call backs
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
	{ok, idle, #statedata{service = ServiceName,
			server_address = ServerAddress, server_port = ServerPort,
			client_address = ClientAddress, client_port = ClientPort,
			session_id = SessionId,
			origin_host = OriginHost, origin_realm = OriginRealm}}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @private
idle({call, From} = _EventType, EventContent,
		#statedata{session_id = SessionId} = Data)
		when is_record(EventContent, '3gpp_sta_STR');
		is_record(EventContent, '3gpp_swm_STR') ->
	try
		{Identity, NasRealm, NasHost} = case EventContent of
			#'3gpp_sta_STR'{'User-Name' = [UserName],
					'Origin-Realm' = OR, 'Origin-Host' = OH} ->
				{UserName, OR, OH};
			#'3gpp_swm_STR'{'User-Name' = [UserName],
					'Origin-Realm' = OR, 'Origin-Host' = OH} ->
				{UserName, OR, OH}
		end,
		IMSI = case Identity of
			<<?PERM_AKA, PermanentID/binary>> ->
				[H | _] = binary:split(PermanentID, <<$@>>, []),
				H;
			<<?TEMP_AKA:6, _/bits>> ->
				{ok, Keys} = application:get_env(aka_kpseu),
				[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
				CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
				ocs_eap_aka:compressed_imsi(CompressedIMSI)
%			<<?FAST_AKA:6, _/bits>> ->
		end,
		{Identity, NasRealm, NasHost, IMSI}
	of
		{Identity1, NasRealm1, NasHost1, IMSI1} ->
			NewData = Data#statedata{request = EventContent,
					from = From, nas_realm = NasRealm1, nas_host = NasHost1,
					imsi = IMSI1, identity = Identity1},
			F = fun() ->
					case mnesia:read(session, SessionId, write) of
						[#session{imsi = IMSI1, hss_realm = undefined}] ->
							mnesia:delete(session, SessionId, write);
						[#session{imsi = IMSI1, hss_realm = HR, hss_host = HH}] ->
							mnesia:delete(session, SessionId, write),
							{HR, HH};
						[] ->
							not_found
					end
			end,
			case mnesia:transaction(F) of
				{atomic, ok} ->
					ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
					Response = response(ResultCode, NewData),
					ReplyAction = {reply, From, Response},
					{stop_and_reply, shutdown, ReplyAction, NewData};
				{atomic, {HssRealm1, HssHost1}} ->
					NextData = NewData#statedata{hss_realm = HssRealm1,
							hss_host = HssHost1},
					send_deregister(NextData),
					TimeoutAction = {timeout, ?TIMEOUT, send},
					{next_state, deregister, NextData, TimeoutAction};
				{atomic, not_found} ->
					ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_SESSION_ID',
					Response = response(ResultCode, NewData),
					ReplyAction = {reply, From, Response},
					{stop_and_reply, shutdown, ReplyAction, NewData};
				{aborted, Reason} ->
					error_logger:error_report(["Failed user lookup",
							{nas_host, NasHost1}, {nas_realm, NasRealm1},
							{imsi, IMSI1}, {identity, Identity1},
							{session, SessionId}, {error, Reason}]),
					ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					Response = response(ResultCode, NewData),
					ReplyAction = {reply, From, Response},
					{stop_and_reply, Reason, ReplyAction, NewData}
			end
	catch
		_:_ ->
			NewData = Data#statedata{request = EventContent, from = From},
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_SESSION_ID',
			Response = response(ResultCode, NewData),
			ReplyAction = {reply, From, Response},
			{stop_and_reply, shutdown, ReplyAction, NewData}
	end.

-spec deregister(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>deregister</em> state.
%% @private
deregister(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code'
				= [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']}} = _EventContent,
		Data) ->
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	deregister1(response(ResultCode, Data), Data);
deregister(cast,
		{ok, #'3gpp_swx_SAA'{'Experimental-Result'
				= [#'3gpp_Experimental-Result'{'Experimental-Result-Code'
				= ?'DIAMETER_ERROR_IDENTITY_NOT_REGISTERED'}]}},
		#statedata{session_id = SessionId, imsi = IMSI,
				identity = Identity, nas_realm = NasRealm,
				nas_host = NasHost, hss_realm = HssRealm,
				hss_host = HssHost} = Data) ->
	error_logger:warning_report(["Identity not registered",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{imsi, IMSI}, {identity, Identity}, {session, SessionId},
			{result, ?'DIAMETER_ERROR_IDENTITY_NOT_REGISTERED'}]),
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	deregister1(response(ResultCode, Data), Data);
deregister(cast,
		{ok, #'3gpp_swx_SAA'{'Result-Code' = [ResultCode1]}},
		#statedata{session_id = SessionId, imsi = IMSI,
				identity = Identity, nas_realm = NasRealm,
				nas_host = NasHost, hss_realm = HssRealm,
				hss_host = HssHost} = Data) ->
	error_logger:error_report(["Unexpected deregistration result",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	deregister1(response(ResultCode2, Data), Data);
deregister(cast,
		{ok, #'3gpp_swx_SAA'{'Experimental-Result'
				= [#'3gpp_Experimental-Result'{
						'Experimental-Result-Code' = ResultCode1}]}},
		#statedata{session_id = SessionId, imsi = IMSI,
				identity = Identity, nas_realm = NasRealm,
				nas_host = NasHost, hss_realm = HssRealm,
				hss_host = HssHost} = Data) ->
	error_logger:error_report(["Unexpected deregistration result",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	deregister1(response(ResultCode2, Data), Data);
deregister(cast,
		{ok, #'diameter_base_answer-message'{
				'Result-Code' = ResultCode1}},
		#statedata{session_id = SessionId, imsi = IMSI,
				identity = Identity, nas_realm = NasRealm,
				nas_host = NasHost, hss_realm = HssRealm,
				hss_host = HssHost} = Data) ->
	error_logger:error_report(["Unexpected deregistration result",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	deregister1(response(ResultCode2, Data), Data);
deregister(cast = _EventType, {error, Reason},
		#statedata{session_id = SessionId, imsi = IMSI,
				identity = Identity, nas_realm = NasRealm,
				nas_host = NasHost, hss_realm = HssRealm,
				hss_host = HssHost} = Data) ->
	error_logger:error_report(["Deregistration failed",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {reason, Reason}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	deregister1(response(ResultCode2, Data), Data);
deregister(timeout, send,
		#statedata{session_id = SessionId, imsi = IMSI,
				identity = Identity, nas_realm = NasRealm,
				nas_host = NasHost, hss_realm = HssRealm,
				hss_host = HssHost} = Data) ->
	error_logger:error_report(["Deregistration timeout",
			{nas_host, NasHost}, {nas_realm, NasRealm},
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}]),
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	deregister1(response(ResultCode, Data), Data).

%% @hidden
deregister1(Response,
		#statedata{session_id = SessionId, from = From} = _Data) ->
	ReplyAction = {reply, From, Response},
	F = fun() ->
			mnesia:delete(session, SessionId, write)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{stop_and_reply, shutdown, ReplyAction};
		{aborted, Reason} ->
			{stop_and_reply, Reason, ReplyAction}
	end.

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

-spec response(ResultCode, Data) -> Answer
	when
		ResultCode :: pos_integer(),
		Data :: #statedata{},
		Answer ::  #'3gpp_sta_STA'{} | #'3gpp_swm_STA'{}.
%% @doc Send DIAMETER Session-Termination-Answer (STA) response.
%% @hidden
response(ResultCode,
		#statedata{request = #'3gpp_sta_STR'{} = Request,
		session_id = SessionId,
		server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm} = _Data)
		when is_integer(ResultCode), is_binary(OriginHost),
		is_binary(OriginRealm)->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_sta_STA'{'Session-Id' = SessionId,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	Answer;
response(ResultCode,
		#statedata{request = #'3gpp_swm_STR'{} = Request,
		session_id = SessionId,
		server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm} = _Data)
		when is_integer(ResultCode), is_binary(OriginHost),
		is_binary(OriginRealm) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_swm_STA'{'Session-Id' = SessionId,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	Answer.

-spec send_deregister(Data) -> Result
	when
		Data :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR) deregistration. 
%% @hidden
send_deregister(#statedata{imsi = IMSI,
		origin_host = OriginHost, origin_realm = OriginRealm,
		hss_host = HssHost, hss_realm = HssRealm,
		service = Service} = _Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'User-Name' = [IMSI],
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = [HssHost],
			'Vendor-Specific-Application-Id' = #'3gpp_swx_Vendor-Specific-Application-Id'{
					'Vendor-Id' = ?IANA_PEN_3GPP,
					'Auth-Application-Id' = [?SWx_APPLICATION_ID]},
			'Auth-Session-State' = ?'DIAMETER_BASE_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
			'Server-Assignment-Type' = ?'3GPP_SWX_SERVER-ASSIGNMENT-TYPE_USER_DEREGISTRATION'},
	diameter:call(Service, ?SWx_APPLICATION,
			Request, [detach, {extra, [self()]}]).

