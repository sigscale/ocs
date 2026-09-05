%%% ocs_pgw_fsm.erl
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
%%% 	module implements procedures for user authorization initiated by
%%% 	PDN Gateway (PGW).
%%%
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=29&amp;GSMSpecPart2=273">
%%% 	3GPP TS 29.273 - 3GPP EPS AAA interfaces</a>
%%%
-module(ocs_pgw_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([idle/3, register/3, profile/3]).

-include("ocs.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_3gpp.hrl").
-include("diameter_gen_3gpp_s6b_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-define(IANA_PEN_3GPP, 10415).
-define(S6b_APPLICATION_ID, 16777272).
-define(SWx_APPLICATION_ID, 16777265).
-define(SWx_APPLICATION, ocs_diameter_3gpp_swx_application).
-define(TIMEOUT, 10000).
-define(GTPv2_SUPPORTED, 16#0000400000000000).

-record(statedata,
		{identity :: binary() | undefined,
		imsi :: binary() | undefined,
		user_profile :: #'3gpp_swx_Non-3GPP-User-Data'{} | undefined,
		orig_host :: diameter:'OctetString'(),
		orig_realm :: diameter:'OctetString'(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: inet:ip_address(),
		client_port :: pos_integer(),
		service :: tuple(),
		hss_realm :: diameter:'OctetString'() | undefined,
		hss_host = [] :: [diameter:'OctetString'()],
		pgw_realm :: diameter:'OctetString'() | undefined,
		pgw_host = [] :: diameter:'OctetString'(),
		pgw_id = [] ::  [#'3gpp_swx_MIP6-Agent-Info'{}],
		pgw_plmn = [] :: [diameter:'OctetString'()],
		request :: #'3gpp_s6b_AAR'{} | undefined,
		session_id :: string(),
		apn_context :: pos_integer() | undefined,
		apn_name :: string() | undefined,
		from :: gen_statem:from() | undefined}).
-type statedata() :: #statedata{}.
-type state() :: idle.

%%----------------------------------------------------------------------
%%  The ocs_pgw_fsm gen_statem call backs
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
	{ok, HssRealm} = application:get_env(hss_realm),
	HssHost = case application:get_env(hss_host) of
		{ok, undefined} ->
			[];
		{ok, HH} ->
			HH
	end,
	{ok, idle, #statedata{service = ServiceName,
			server_address = ServerAddress, server_port = ServerPort,
			client_address = ClientAddress, client_port = ClientPort,
			session_id = SessionId,
			orig_host = OriginHost, orig_realm = OriginRealm,
			hss_realm = HssRealm, hss_host = HssHost}}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
idle({call, From} = _EventType,
		#'3gpp_s6b_AAR'{'MIP6-Feature-Vector' = MIP6FeatureVector} = EventContent,
		Data) ->
	F = fun(FV) when (FV band ?GTPv2_SUPPORTED) =:= ?GTPv2_SUPPORTED ->
				true;
			(_FV) ->
				false
	end,
	GTPv2Enabled = lists:any(F, MIP6FeatureVector),
	NewData = Data#statedata{from = From},
	idle1(GTPv2Enabled, EventContent, NewData);
idle({call, From} = _EventType,
		#'3gpp_s6b_STR'{'User-Name' = [Identity],
				'Session-Id' = SessionId,
				'Termination-Cause' = _Cause} = EventContent,
		#statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort, orig_host = OriginHost,
		orig_realm = OriginRealm} = Data) ->
	[IMSI | _] = binary:split(Identity, <<$@>>, []),
	NewData = Data#statedata{from = From, session_id = SessionId,
			identity = Identity, imsi = IMSI},
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_s6b_STA'{'Session-Id' = SessionId,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm},
	ok = ocs_log:auth_log(diameter, Server, Client, EventContent, Answer),
	ReplyAction = {reply, From, Answer},
	{stop_and_reply, shutdown, ReplyAction, NewData}.
%% @hidden
idle1(true = _GTPv2Enabled,
		#'3gpp_s6b_AAR'{'User-Name' = [Identity], 'Session-Id' = SessionId,
		'Origin-Host' = PgwHost, 'Origin-Realm' = PgwRealm,
		'Auth-Request-Type' = ?'3GPP_SWX_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY',
		'MIP6-Agent-Info' = AgentInfo, 'Visited-Network-Identifier' = VPLMN,
		'Service-Selection' = [APN]} = Request,
		#statedata{from = From} = Data) ->
	[IMSI | _] = binary:split(Identity, <<$@>>, []),
	PGW = agent_info(AgentInfo),
	NewData = Data#statedata{request = Request,
			identity = Identity, imsi = IMSI,
			pgw_host = PgwHost, pgw_realm = PgwRealm,
			pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN},
	F = fun() ->
			mnesia:index_read(session, IMSI, #session.imsi)
	end,
	TimeoutAction = {timeout, ?TIMEOUT, timeout},
	case mnesia:transaction(F) of
		{atomic, [#session{user_profile = UserProfile,
				hss_realm = HssRealm, hss_host = HssHost} | _]} ->
			NextData = NewData#statedata{user_profile = UserProfile,
					hss_realm = HssRealm, hss_host = [HssHost]},
			case lists:keyfind(APN,
					#'3gpp_swx_APN-Configuration'.'Service-Selection',
					UserProfile#'3gpp_swx_Non-3GPP-User-Data'.'APN-Configuration') of
				#'3gpp_swx_APN-Configuration'{'Context-Identifier' = Context} ->
					NextData1 = NextData#statedata{apn_context = Context},
					send_register(NextData1),
					{next_state, register, NextData1, TimeoutAction};
				_Other ->
					ResultCode = ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
					ReplyAction = {reply, From, response(ResultCode, NextData)},
					{stop_and_reply, shutdown, ReplyAction, NextData}
			end;
		{atomic, []} ->
			send_profile(NewData),
			{next_state, profile, NewData, TimeoutAction};
		{aborted, Reason} ->
			error_logger:error_report(["Failed user lookup",
					{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
					{imsi, IMSI}, {identity, Identity}, {apn, APN},
					{session, SessionId}, {error, Reason}]),
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
			ReplyAction = {reply, From, response(ResultCode, NewData)},
			{stop_and_reply, Reason, ReplyAction, NewData}
	end;
idle1(false = _GTPv2Enabled,
		#'3gpp_s6b_AAR'{'User-Name' = [Identity], 'Session-Id' = SessionId,
		'Origin-Host' = PgwHost, 'Origin-Realm' = PgwRealm,
		'Auth-Request-Type' = ?'3GPP_SWX_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY',
		'MIP6-Agent-Info' = AgentInfo, 'Visited-Network-Identifier' = VPLMN,
		'Service-Selection' = [APN]} = Request,
		#statedata{from = From} = Data) ->
	[IMSI | _] = binary:split(Identity, <<$@>>, []),
	PGW = agent_info(AgentInfo),
	NewData = Data#statedata{from = From, request = Request,
			identity = Identity, imsi = IMSI,
			pgw_host = PgwHost, pgw_realm = PgwRealm,
			pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN},
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	error_logger:warning_report(["PGW doesn't support GTPv2",
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode}]),
	ReplyAction = {reply, From, response(ResultCode, NewData)},
	{stop_and_reply, shutdown, ReplyAction, NewData}.

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
register(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code'
				= [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']} = _Answer},
		#statedata{from = From} = Data) ->
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	ReplyAction = {reply, From, response(ResultCode, Data)},
	{stop_and_reply, shutdown, ReplyAction};
register(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code' = [ResultCode1],
				'Origin-Host' = HssHost,
				'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Unexpected registration result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode2, Data)},
	{stop_and_reply, shutdown, ReplyAction};
register(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Experimental-Result'
				= [#'3gpp_Experimental-Result'{
						'Experimental-Result-Code' = ResultCode1}],
				'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm}},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Unexpected registration result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode2, Data)},
	{stop_and_reply, shutdown, ReplyAction};
register(cast = _EventType,
		{ok, #'diameter_base_answer-message'{'Result-Code' = ResultCode1,
		'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Unexpected registration result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode2, Data)},
	{stop_and_reply, shutdown, ReplyAction};
register(timeout = _EventType, timeout = _EventContent,
		#statedata{from = From, session_id = SessionId,
		hss_host = HssHost, hss_realm = HssRealm,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Timout on registration result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}]),
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode, Data)},
	{stop_and_reply, shutdown, ReplyAction}.

-spec profile(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>profile</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
profile(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
				'3GPP-AAA-Server-Name' = []} = _Answer} = _EventContent,
		#statedata{from = From} = Data) ->
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	ReplyAction = {reply, From, response(ResultCode, Data)},
	{stop_and_reply, shutdown, ReplyAction};
profile(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
				'3GPP-AAA-Server-Name' = [AaaServerName]} = _Answer},
		#statedata{from = From} = Data) ->
	ReplyAction = {reply, From, response(AaaServerName, Data)},
	{stop_and_reply, shutdown, ReplyAction};
profile(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Experimental-Result' = [#'3gpp_Experimental-Result'{
				'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'}],
				'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:warning_report(["Unkown user",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId},
			{result, ?'DIAMETER_ERROR_USER_UNKNOWN'}]),
	ResultCode = ?'DIAMETER_ERROR_USER_UNKNOWN',
	ReplyAction = {reply, From, response(ResultCode, Data)},
	{stop_and_reply, shutdown, ReplyAction};
profile(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code' = [ResultCode1],
				'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Unexpected get user profile result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode2, Data)},
	{stop_and_reply, shutdown, ReplyAction};
profile(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Experimental-Result'
				= [#'3gpp_Experimental-Result'{
						'Experimental-Result-Code' = ResultCode1}],
				'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Unexpected get user profile result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode2, Data)},
	{stop_and_reply, shutdown, ReplyAction};
profile(cast = _EventType,
		{ok, #'diameter_base_answer-message'{'Result-Code' = ResultCode1,
				'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = From, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Unexpected get user profile result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode2, Data)},
	{stop_and_reply, shutdown, ReplyAction};
profile(timeout = _EventType, timeout = _EventContent,
		#statedata{from = From, session_id = SessionId,
		hss_host = HssHost, hss_realm = HssRealm,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = Data) ->
	error_logger:error_report(["Timeout on get user profile result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}]),
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	ReplyAction = {reply, From, response(ResultCode, Data)},
	{stop_and_reply, shutdown, ReplyAction}.

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

-spec send_register(Data) -> Result
	when
		Data :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR)
%% 	registration request. 
%% @hidden
send_register(#statedata{imsi = IMSI,
		orig_host = OriginHost, orig_realm = OriginRealm,
		hss_host = HssHost, hss_realm = HssRealm, service = Service,
		apn_context = Context, apn_name = APN,
		pgw_id = PGW, pgw_plmn = VPLMN} = _Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'User-Name' = [IMSI],
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = HssHost,
			'Vendor-Specific-Application-Id' = #'3gpp_swx_Vendor-Specific-Application-Id'{
         		'Vendor-Id' = ?IANA_PEN_3GPP,
         		'Auth-Application-Id' = [?SWx_APPLICATION_ID]},
			'Auth-Session-State' = ?'DIAMETER_BASE_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
			'Server-Assignment-Type' = ?'3GPP_SWX_SERVER-ASSIGNMENT-TYPE_PGW_UPDATE',
			'MIP6-Agent-Info' = PGW, 'Visited-Network-Identifier' = VPLMN,
			'Context-Identifier' = [Context], 'Service-Selection' = [APN]},
	diameter:call(Service, ?SWx_APPLICATION,
			Request, [detach, {extra, [self()]}]).

-spec send_profile(Data) -> Result
	when
		Data :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR)
%% 	subscriber user profile request. 
%% @hidden
send_profile(#statedata{imsi = IMSI,
		orig_host = OriginHost, orig_realm = OriginRealm,
		hss_realm = HssRealm, hss_host = HssHost,
		service = Service} = _Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'User-Name' = [IMSI],
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = HssHost,
			'Vendor-Specific-Application-Id' = #'3gpp_swx_Vendor-Specific-Application-Id'{
         		'Vendor-Id' = ?IANA_PEN_3GPP,
         		'Auth-Application-Id' = [?SWx_APPLICATION_ID]},
			'Auth-Session-State' = ?'DIAMETER_BASE_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
			'Server-Assignment-Type' = ?'3GPP_SWX_SERVER-ASSIGNMENT-TYPE_AAA_USER_DATA_REQUEST'},
	diameter:call(Service, ?SWx_APPLICATION,
			Request, [detach, {extra, [self()]}]).

-spec response(Arg, Data) -> Result
	when
		Arg :: ResultCode | RedirectHost,
		ResultCode :: pos_integer(),
		RedirectHost :: binary(),
		Data :: #statedata{},
		Result :: #'3gpp_s6b_AAA'{}.
%% @doc Create DIAMETER response.
%% @hidden
response(ResultCode = _Arg,
		#statedata{request = #'3gpp_s6b_AAR'{
				'User-Name' = UserName,
				'Auth-Request-Type' = AuthRequestType} = Request,
		session_id = SessionId,
		server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		orig_host = OriginHost, orig_realm = OriginRealm} = _Data)
		when is_integer(ResultCode) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_s6b_AAA'{'Session-Id' = SessionId,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'User-Name' = UserName,
			'Auth-Application-Id' = ?S6b_APPLICATION_ID,
			'Auth-Request-Type' = AuthRequestType,
			'MIP6-Feature-Vector' = [?GTPv2_SUPPORTED]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	Answer;
response(RedirectHost,
		#statedata{request = #'3gpp_s6b_AAR'{
				'User-Name' = UserName,
				'Auth-Request-Type' = AuthRequestType} = Request,
		session_id = SessionId,
		server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		orig_host = OriginHost, orig_realm = OriginRealm} = _Data)
		when is_binary(RedirectHost) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_s6b_AAA'{'Session-Id' = SessionId,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_REDIRECT_INDICATION',
			'Redirect-Host' = RedirectHost,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'User-Name' = UserName,
			'Auth-Application-Id' = ?S6b_APPLICATION_ID,
			'Auth-Request-Type' = AuthRequestType},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	Answer.

-spec pgw_id(PGW) -> Result
	when
		PGW :: [#'3gpp_swx_MIP6-Agent-Info'{}],
		Result :: diameter:'OctetString'() | undefined.
%% @doc Get the PGWID hostname.
pgw_id([#'3gpp_swx_MIP6-Agent-Info'{'MIP-Home-Agent-Host'
		= [#'3gpp_swx_MIP-Home-Agent-Host'{'Destination-Host'
		= HomeAgentHostHost}]}]) when is_binary(HomeAgentHostHost) ->
	HomeAgentHostHost;
pgw_id(_) ->
	undefined.

-spec agent_info(AgentInfo) -> Result
	when
		AgentInfo :: [#'3gpp_s6b_MIP6-Agent-Info'{}],
		Result :: [#'3gpp_swx_MIP6-Agent-Info'{}].
agent_info([#'3gpp_s6b_MIP6-Agent-Info'{
		'MIP-Home-Agent-Address' = HomeAgentAddress,
		'MIP-Home-Agent-Host' = [#'3gpp_s6b_MIP-Home-Agent-Host'{
				'Destination-Realm' = HomeAgentHostRealm,
				'Destination-Host' = HomeAgentHostHost}],
		'MIP6-Home-Link-Prefix' = HomeLinkPrefix}]) ->
	[#'3gpp_swx_MIP6-Agent-Info'{'MIP-Home-Agent-Address' = HomeAgentAddress,
			'MIP-Home-Agent-Host' = [#'3gpp_swx_MIP-Home-Agent-Host'{
					'Destination-Realm' = HomeAgentHostRealm,
					'Destination-Host' = HomeAgentHostHost}],
			'MIP6-Home-Link-Prefix' = HomeLinkPrefix}];
agent_info([]) ->
	[].

