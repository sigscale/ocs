%%% ocs_pgw_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2020 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2020 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_pgw_fsm API
-export([]).

%% export the ocs_pgw_fsm state callbacks
-export([idle/2, register/2, profile/2]).

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
-define(S6b_APPLICATION_ID,  16777272).
-define(SWx_APPLICATION_ID, 16777265).
-define(S6b_APPLICATION, ocs_diameter_3gpp_s6b_application).
-define(SWx_APPLICATION, ocs_diameter_3gpp_swx_application).
-define(TIMEOUT, 5000).

-record(statedata,
		{identity :: binary() | undefined,
		user_profile :: #'3gpp_swx_Non-3GPP-User-Data'{} | undefined,
		orig_host :: diameter:'OctetString'(),
		orig_realm :: diameter:'OctetString'(),
		dest_host :: diameter:'OctetString'(),
		dest_realm :: diameter:'OctetString'(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: inet:ip_address(),
		client_port :: pos_integer(),
		service :: tuple(),
		hss_realm :: string() | undefined,
		hss_host = [] :: string(),
		pgw_id = [] :: [diameter:'OctetString'()],
		pgw_plmn = [] :: [diameter:'OctetString'()],
		request :: #'3gpp_s6b_AAR'{},
		session_id :: string()}).
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
init([ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessionId, OriginHost, OriginRealm, DestinationHost,
		DestinationRealm, Request] = _Args) ->
	Service = {ocs_diameter_auth, ServerAddress, ServerPort},
	process_flag(trap_exit, true),
	{ok, HssRealm} = application:get_env(hss_realm),
	HssHost = case application:get_env(hss_host) of
		{ok, undefined} ->
			[];
		{ok, HH} ->
			HH
	end,
	{ok, HssHost} = application:get_env(hss_host),
	{ok, idle, #statedata{service = Service,
			server_address = ServerAddress, server_port = ServerPort,
			client_address = ClientAddress, client_port = ClientPort,
			session_id = SessionId, request = Request,
			orig_host = OriginHost, orig_realm = OriginRealm,
			dest_host = DestinationHost, dest_realm = DestinationRealm,
			hss_realm = HssRealm, hss_host = HssHost}, 0}.

-spec idle(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>idle</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
idle(timeout, #statedata{session_id = SessionId,
		request = #'3gpp_s6b_AAR'{'User-Name' = [Identity],
		'Auth-Request-Type' = ?'3GPP_SWX_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY',
		'MIP6-Agent-Info' = PGW, 'Visited-Network-Identifier' = VPLMN,
		'Service-Selection' = [APN]}} = StateData) ->
	NewStateData = StateData#statedata{identity = Identity,
					pgw_id = PGW, pgw_plmn = VPLMN},
	F = fun() ->
			mnesia:index_read(session, Identity, #session.imsi)
	end,
	case mnesia:transaction(F) of
		{atomic, [#session{user_profile = UserProfile,
				hss_realm = HssRealm, hss_host = HssHost}]} ->
			NextStateData = NewStateData#statedata{user_profile = UserProfile,
					hss_realm = HssRealm, hss_host = HssHost},
			case lists:keyfind(APN,
					#'3gpp_swx_APN-Configuration'.'Service-Selection',
					UserProfile#'3gpp_swx_Non-3GPP-User-Data'.'APN-Configuration') of
				#'3gpp_swx_APN-Configuration'{} = APN ->
					send_diameter_register(NextStateData),
					{next_state, profile, NextStateData, ?TIMEOUT};
				_ ->
					send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
							NextStateData),
					{stop, {shutdown, SessionId}, NextStateData}
			end;
		{atomic, []} ->
			send_diameter_profile(NewStateData),
			{next_state, profile, NewStateData, ?TIMEOUT};
		{aborted, Reason} ->
			error_logger:error_report(["Unkown user",
					{error, Reason}, {imsi, Identity}, {session, SessionId}]),
			send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', NewStateData),
			{stop, Reason, NewStateData}
	end.

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
register(#'3gpp_swx_SAA'{'Result-Code'
		= [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']} = _Answer,
		#statedata{session_id = SessionId} = StateData) ->
	send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_SUCCESS', StateData),
	{stop, {shutdown, SessionId}, StateData};
register(#'3gpp_swx_SAA'{'Result-Code' = [ResultCode]} = _Answer,
		#statedata{session_id = SessionId, identity = Identity,
		hss_realm = HssRealm, hss_host = HssHost} = StateData) ->
	error_logger:error_report(["Unexpected registration result",
			{imsi, Identity}, {hss_realm, HssRealm},
			{hss_host, HssHost}, {session, SessionId},
			{result, ResultCode}]),
	send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', StateData),
	{stop, {shutdown, SessionId}, StateData};
register(#'3gpp_swx_SAA'{'Experimental-Result' = [ResultCode]} = _Answer,
		#statedata{session_id = SessionId, identity = Identity,
		hss_realm = HssRealm, hss_host = HssHost} = StateData) ->
	error_logger:error_report(["Unexpected registration result",
			{imsi, Identity}, {hss_realm, HssRealm},
			{hss_host, HssHost}, {session, SessionId},
			{result, ResultCode}]),
	send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', StateData),
	{stop, {shutdown, SessionId}, StateData}.

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
profile(#'3gpp_swx_SAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
		'3GPP-AAA-Server-Name' = []} = _Answer,
		#statedata{session_id = SessionId} = StateData) ->
	send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_SUCCESS', StateData),
	{stop, {shutdown, SessionId}, StateData};
profile(#'3gpp_swx_SAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
		'3GPP-AAA-Server-Name' = [AaaServerName]} = _Answer,
		#statedata{session_id = SessionId} = StateData) ->
	send_diameter_redirect(AaaServerName, StateData),
	{stop, {shutdown, SessionId}, StateData};
profile(#'3gpp_swx_SAA'{'Experimental-Result'
		= [?'DIAMETER_ERROR_USER_UNKNOWN']} = _Answer,
		#statedata{session_id = SessionId, identity = Identity,
		hss_realm = HssRealm, hss_host = HssHost} = StateData) ->
	error_logger:error_report(["Unkown user",
			{imsi, Identity}, {hss_realm, HssRealm},
			{hss_host, HssHost}, {session, SessionId}]),
	send_diameter_response(?'DIAMETER_ERROR_USER_UNKNOWN', StateData),
	{stop, {shutdown, SessionId}, StateData};
profile(#'3gpp_swx_SAA'{'Result-Code' = [ResultCode]} = _Answer,
		#statedata{session_id = SessionId, identity = Identity,
		hss_realm = HssRealm, hss_host = HssHost} = StateData) ->
	error_logger:error_report(["Unexpected get user profile result",
			{imsi, Identity}, {hss_realm, HssRealm},
			{hss_host, HssHost}, {session, SessionId},
			{result, ResultCode}]),
	send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', StateData),
	{stop, {shutdown, SessionId}, StateData};
profile(#'3gpp_swx_SAA'{'Experimental-Result' = [ResultCode]} = _Answer,
		#statedata{session_id = SessionId, identity = Identity} = StateData) ->
	error_logger:error_report(["Unexpected get user profile result",
			{imsi, Identity}, {session, SessionId}, {result, ResultCode}]),
	send_diameter_response(?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', StateData),
	{stop, {shutdown, SessionId}, StateData}.

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

-spec send_diameter_register(StateData) -> Result
	when
		StateData :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR) registration request. 
%% @hidden
send_diameter_register(#statedata{identity = Identity,
		orig_host = OriginHost, orig_realm = OriginRealm,
		hss_host = HssHost, hss_realm = HssRealm, service = Service,
		pgw_id = PGW, pgw_plmn = VPLMN} = _StateData) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'User-Name' = [Identity],
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = HssHost,
			'Server-Assignment-Type' = ?'3GPP_SWX_SERVER-ASSIGNMENT-TYPE_PGW_UPDATE',
			'MIP6-Agent-Info' = PGW, 'Visited-Network-Identifier' = VPLMN},
	diameter:call(Service, ?SWx_APPLICATION,
			Request, [detach, {extra, [self()]}]).

-spec send_diameter_profile(StateData) -> Result
	when
		StateData :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR) subscriber
%% 	user profile request. 
%% @hidden
send_diameter_profile(#statedata{identity = Identity,
		orig_host = OriginHost, orig_realm = OriginRealm,
		hss_realm = HssRealm, hss_host = HssHost,
		service = Service} = _StateData) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'User-Name' = [Identity],
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = HssHost,
			'Server-Assignment-Type' = ?'3GPP_SWX_SERVER-ASSIGNMENT-TYPE_AAA_USER_DATA_REQUEST'},
	diameter:call(Service, ?SWx_APPLICATION,
			Request, [detach, {extra, [self()]}]).

-spec send_diameter_response(ResultCode, StateData) -> ok
	when
		ResultCode :: pos_integer(),
		StateData :: #statedata{}.
%% @doc Send DIAMETER response.
%% @hidden
send_diameter_response(ResultCode,
		#statedata{request = #'3gpp_s6b_AAR'{} = Request,
		session_id = SessionId, service = Service,
		server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		orig_host = OriginHost, orig_realm = OriginRealm} = _StateData)
		when is_integer(ResultCode) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_s6b_AAA'{'Session-Id' = SessionId,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(Service, {self(), Answer}).

-spec send_diameter_redirect(RedirectHost, StateData) -> ok
	when
		RedirectHost:: diameter:'OctetString'(),
		StateData :: #statedata{}.
%% @doc Send DIAMETER response.
%% @hidden
send_diameter_redirect(RedirectHost,
		#statedata{request = #'3gpp_s6b_AAR'{} = Request,
		session_id = SessionId, service = Service,
		server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		orig_host = OriginHost, orig_realm = OriginRealm} = _StateData) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_s6b_AAA'{'Session-Id' = SessionId,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_REDIRECT_INDICATION',
			'Redirect-Host' = RedirectHost,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(Service, {self(), Answer}).

