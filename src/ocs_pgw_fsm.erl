%%% ocs_pgw_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback module
%%% 	implements procedures for user authorization initiated by PDN Gateway.
%%%
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=29&amp;GSMSpecPart2=273">
%%% 	3GPP TS 29.273 - 3GPP EPS AAA interfaces</a>
%%%
-module(ocs_pgw_fsm).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_pgw_fsm API
-export([]).

%% export the ocs_pgw_fsm state callbacks
-export([idle/3, register/2, profile/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

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
		apn_context :: pos_integer(),
		apn_name :: string(),
		from :: {pid(), reference()} | undefined}).
-type statedata() :: #statedata{}.

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The ocs_pgw_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_pgw_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData}
		| {ok, StateName, StateData, Timeout}
		| {ok, StateName, StateData, hibernate}
		| {stop, Reason} | ignore,
		StateName :: atom(),
		StateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
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

-spec idle(Event, From, StateData) -> Result
	when
		Event :: timeout | term(),
		From :: {pid(), reference()},
		StateData :: statedata(),
		Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:sync_send_event/2.
%%		gen_fsm:sync_send_event/2} in the <b>idle</b> state.
%% @@see //stdlib/gen_fsm:StateName/3
%% @private
%%
idle(#'3gpp_s6b_AAR'{'User-Name' = [Identity], 'Session-Id' = SessionId,
		'Origin-Host' = PgwHost, 'Origin-Realm' = PgwRealm,
		'Auth-Request-Type' = ?'3GPP_SWX_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY',
		'MIP6-Agent-Info' = AgentInfo, 'Visited-Network-Identifier' = VPLMN,
		'MIP6-Feature-Vector' = [MIP6FeatureVector],
		'Service-Selection' = [APN]} = Request, From, StateData)
		when (MIP6FeatureVector band ?GTPv2_SUPPORTED) =:= ?GTPv2_SUPPORTED ->
	[IMSI | _] = binary:split(Identity, <<$@>>, []),
	PGW = agent_info(AgentInfo),
	NewStateData = StateData#statedata{from = From, request = Request,
			identity = Identity, imsi = IMSI,
			pgw_host = PgwHost, pgw_realm = PgwRealm,
			pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN},
	F = fun() ->
			mnesia:index_read(session, IMSI, #session.imsi)
	end,
	case mnesia:transaction(F) of
		{atomic, [#session{user_profile = UserProfile,
				hss_realm = HssRealm, hss_host = HssHost} | _]} ->
			NextStateData = NewStateData#statedata{user_profile = UserProfile,
					hss_realm = HssRealm, hss_host = [HssHost]},
			case lists:keyfind(APN,
					#'3gpp_swx_APN-Configuration'.'Service-Selection',
					UserProfile#'3gpp_swx_Non-3GPP-User-Data'.'APN-Configuration') of
				#'3gpp_swx_APN-Configuration'{'Context-Identifier' = Context} ->
					NextStateData1 = NextStateData#statedata{apn_context = Context},
					send_register(NextStateData1),
					{next_state, register, NextStateData1, ?TIMEOUT};
				_Other ->
					ResultCode = ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
					Reply = response(ResultCode, NextStateData),
					{stop, shutdown,  Reply, NextStateData}
			end;
		{atomic, []} ->
			send_profile(NewStateData),
			{next_state, profile, NewStateData, ?TIMEOUT};
		{aborted, Reason} ->
			error_logger:error_report(["Failed user lookup",
					{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
					{imsi, IMSI}, {identity, Identity}, {apn, APN},
					{session, SessionId}, {error, Reason}]),
			ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
			Reply = response(ResultCode, NewStateData),
			{stop, Reason, Reply, NewStateData}
	end;
idle(#'3gpp_s6b_AAR'{'User-Name' = [Identity], 'Session-Id' = SessionId,
		'Origin-Host' = PgwHost, 'Origin-Realm' = PgwRealm,
		'Auth-Request-Type' = ?'3GPP_SWX_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY',
		'MIP6-Agent-Info' = AgentInfo, 'Visited-Network-Identifier' = VPLMN,
		'Service-Selection' = [APN]} = Request, From, StateData) ->
	[IMSI | _] = binary:split(Identity, <<$@>>, []),
	PGW = agent_info(AgentInfo),
	NewStateData = StateData#statedata{from = From, request = Request,
			identity = Identity, imsi = IMSI,
			pgw_host = PgwHost, pgw_realm = PgwRealm,
			pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN},
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	error_logger:warning_report(["PGW doesn't support GTPv2",
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode}]),
	gen_fsm:reply(From, response(ResultCode, NewStateData)),
	{stop, shutdown, NewStateData}.

-spec register(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
				| {next_state, NextStateName, NewStateData, Timeout}
				| {next_state, NextStateName, NewStateData, hibernate}
				| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>register</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
register({ok, #'3gpp_swx_SAA'{'Result-Code'
		= [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']} = _Answer},
		#statedata{from = Caller} = StateData) ->
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	gen_fsm:reply(Caller, response(ResultCode, StateData)),
	{stop, shutdown, StateData};
register({ok, #'3gpp_swx_SAA'{'Result-Code' = [ResultCode1],
		'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = Caller, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = StateData) ->
	error_logger:error_report(["Unexpected registration result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	gen_fsm:reply(Caller, response(ResultCode2, StateData)),
	{stop, shutdown, StateData};
register({ok, #'3gpp_swx_SAA'{'Experimental-Result'
		= [#'3gpp_Experimental-Result'{'Experimental-Result-Code' = ResultCode1}],
		'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = Caller, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = StateData) ->
	error_logger:error_report(["Unexpected registration result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode1 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	gen_fsm:reply(Caller, response(ResultCode1, StateData)),
	{stop, shutdown, StateData}.

-spec profile(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
				| {next_state, NextStateName, NewStateData, Timeout}
				| {next_state, NextStateName, NewStateData, hibernate}
				| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>profile</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
profile({ok, #'3gpp_swx_SAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
		'3GPP-AAA-Server-Name' = []} = _Answer},
		#statedata{from = Caller} = StateData) ->
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
	gen_fsm:reply(Caller, response(ResultCode, StateData)),
	{stop, shutdown, StateData};
profile({ok, #'3gpp_swx_SAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
		'3GPP-AAA-Server-Name' = [AaaServerName]} = _Answer},
		#statedata{from = Caller} = StateData) ->
	gen_fsm:reply(Caller, response(AaaServerName, StateData)),
	{stop, shutdown, StateData};
profile({ok, #'3gpp_swx_SAA'{'Experimental-Result' = [#'3gpp_Experimental-Result'{
		'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'}],
		'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = Caller, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = StateData) ->
	error_logger:warning_report(["Unkown user",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId},
			{result, ?'DIAMETER_ERROR_USER_UNKNOWN'}]),
	ResultCode = ?'DIAMETER_ERROR_USER_UNKNOWN',
	gen_fsm:reply(Caller, response(ResultCode, StateData)),
	{stop, shutdown, StateData};
profile({ok, #'3gpp_swx_SAA'{'Result-Code' = [ResultCode1],
		'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = Caller, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = StateData) ->
	error_logger:error_report(["Unexpected get user profile result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	gen_fsm:reply(Caller, response(ResultCode2, StateData)),
	{stop, shutdown, StateData};
profile({ok, #'3gpp_swx_SAA'{'Experimental-Result'
		= [#'3gpp_Experimental-Result'{'Experimental-Result-Code' = ResultCode1}],
		'Origin-Host' = HssHost, 'Origin-Realm' = HssRealm} = _Answer},
		#statedata{from = Caller, session_id = SessionId,
		pgw_host = PgwHost, pgw_realm = PgwRealm,
		pgw_id = PGW, pgw_plmn = VPLMN, apn_name = APN,
		imsi = IMSI, identity = Identity} = StateData) ->
	error_logger:error_report(["Unexpected get user profile result",
			{hss_host, HssHost}, {hss_realm, HssRealm},
			{pgw_host, PgwHost}, {pgw_realm, PgwRealm},
			{pgw_id, pgw_id(PGW)}, {pgw_plmn, VPLMN}, {apn, APN},
			{imsi, IMSI}, {identity, Identity},
			{session, SessionId}, {result, ResultCode1}]),
	ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	gen_fsm:reply(Caller, response(ResultCode2, StateData)),
	{stop, shutdown, StateData}.

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(),
		StateName :: atom(),
      StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
				| {next_state, NextStateName, NewStateData, Timeout}
				| {next_state, NextStateName, NewStateData, hibernate}
				| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(Event, _StateName, StateData) ->
	{stop, Event, StateData}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid, Tag},
		Pid :: pid(),
		Tag :: term(),
      StateName :: atom(),
		StateData :: statedata(),
		Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName ::atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(Event, _From, _StateName, StateData) ->
	{stop, Event, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
				| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info(Info, request, StateData) ->
	{stop, Info, StateData}.

-spec terminate(Reason, StateName, StateData) -> any()
	when
		Reason :: normal | shutdown | term(),
		StateName :: atom(),
      StateData :: statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason, _StateName, _StateData) ->
	ok.

-spec code_change(OldVsn, StateName, StateData, Extra ) -> Result
	when
		OldVsn :: (Vsn | {down, Vsn}),
		Vsn :: term(),
      StateName :: atom(),
		StateData :: statedata(),
		Extra :: term(),
		Result :: {ok, NextStateName, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec send_register(StateData) -> Result
	when
		StateData :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR)
%% 	registration request. 
%% @hidden
send_register(#statedata{imsi = IMSI,
		orig_host = OriginHost, orig_realm = OriginRealm,
		hss_host = HssHost, hss_realm = HssRealm, service = Service,
		apn_context = Context, apn_name = APN,
		pgw_id = PGW, pgw_plmn = VPLMN} = _StateData) ->
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

-spec send_profile(StateData) -> Result
	when
		StateData :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR)
%% 	subscriber user profile request. 
%% @hidden
send_profile(#statedata{imsi = IMSI,
		orig_host = OriginHost, orig_realm = OriginRealm,
		hss_realm = HssRealm, hss_host = HssHost,
		service = Service} = _StateData) ->
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

-spec response(Arg, StateData) -> Result
	when
		Arg :: ResultCode | RedirectHost,
		ResultCode :: pos_integer(),
		RedirectHost :: binary(),
		StateData :: #statedata{},
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
		orig_host = OriginHost, orig_realm = OriginRealm} = _StateData)
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
		orig_host = OriginHost, orig_realm = OriginRealm} = _StateData)
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

