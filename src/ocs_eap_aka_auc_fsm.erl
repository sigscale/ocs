%%% ocs_eap_aka_auc_fsm.erl
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
%%% 	module implements the functions associated with an Authentication
%%% 	Center (AuC) in the user's home domain within EAP 3rd Generation
%%% 	Authentication and Key Agreement (EAP-AKA')
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% 	The users of this module are the EAP-AKA/AKA' handlers which request
%%% 	authentication vectors by sending the event:<br />
%%% 	`{vector, {AkaFsm, Identity, AUTS, RAT, ANID}}' (AKA')<br />
%%% 	`{vector, {AkaFsm, Identity, AUTS, RAT}}' (AKA)<br />
%%% 	and expect one of these replies:<br />
%%% 	`{ok, {RAND, AUTN, CKprime, IKprime, XRES}}' (AKA')<br />
%%% 	`{ok, {RAND, AUTN, CK, IK, XRES}}' (AKA)<br />
%%% 	`{error, Reason}'
%%%
%%% 	After successful authentication an EAP-AKA/AKA' handler should
%%% 	send a registration request event:<br  />
%%% 	`{register, {AkaFsm, Identity}'<br />
%%% 	`{register, {AkaFsm, Identity, APN}'<br />
%%% 	and one of these replies is expected:<br />
%%% 	`{ok, UserProfile, HssRealm, HssHost}'<br />
%%% 	`{error, Reason}'
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc4187">
%%% 	RFC4187 - Extensible Authentication Protocol Method for 3rd Generation
%%% 		Authentication and Key Agreement (EAP-AKA)</a>
%%% @reference <a href="http://tools.ietf.org/html/rfc5448">
%%% 	RFC5448 - Improved Extensible Authentication Protocol Method for
%%% 		3rd Generation Authentication and Key Agreement (EAP-AKA')</a>
%%% @reference <a href="http://webapp.etsi.org/key/key.asp?GSMSpecPart1=33&amp;GSMSpecPart2=402">
%%% 	3GPP TS 33.402 - Security Aspects of non-3GPP Accesses</a>
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=29&amp;GSMSpecPart2=273">
%%% 	3GPP TS 29.273 - 3GPP EPS AAA interfaces</a>
%%%
-module(ocs_eap_aka_auc_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([idle/3, vector/3, register/3]).

-include("diameter_gen_3gpp.hrl").
-include("diameter_3gpp.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("ocs.hrl").

-record(statedata,
		{aka_fsm :: pid() | undefined,
		identity :: binary() | undefined,
		apn :: binary() | undefined,
		rand :: binary() | undefined,
		auts :: binary() | undefined,
		rat_type :: non_neg_integer() | undefined,
		anid :: string() | undefined,
		service :: tuple() | false,
		origin_host :: binary(),
		origin_realm :: binary(),
		hss_realm :: string() | undefined,
		hss_host :: string() | undefined,
		nas_host :: string() | undefined,
		nas_realm :: string() | undefined,
		nas_address :: inet:ip_address() | undefined,
		session_id :: string(),
		aaa_failure = false :: boolean(),
		attributes = [] :: radius_attributes:attributes()}).
-type statedata() :: #statedata{}.
-type state() :: idle | vector | register.

-define(IANA_PEN_3GPP, 10415).
-define(SWx_APPLICATION_ID, 16777265).
-define(SWx_APPLICATION_DICT, diameter_gen_3gpp_swx_application).
-define(SWx_APPLICATION, ocs_diameter_3gpp_swx_application).

-define(TIMEOUT, 10000).

-dialyzer({[nowarn_function, no_match], kdf/5}).
-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(HMAC(Key, Data), crypto:mac(hmac, sha256, Key, Data)).
	-else.
		-define(HMAC(Key, Data), crypto:hmac(sha256, Key, Data)).
	-endif.
-else.
	-define(HMAC(Key, Data), crypto:hmac(sha256, Key, Data)).
-endif.

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_auc_fsm gen_statem call backs
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
%%
init([radius, _ServerAddress, _ServerPort, ClientAddress, _ClientPort,
		_RadiusFsm, _Secret, _PasswordReq, _Trusted, _SessionId,
		#radius{attributes = Attributes} = _Request] = _Args) ->
	SessionAttributes = ocs_rating:session_attributes(Attributes),
	Service = lists:keyfind(ocs_diameter_auth_service, 1, diameter:services()),
	OriginRealm = diameter:service_info(Service, 'Origin-Realm'),
	OriginHost = diameter:service_info(Service, 'Origin-Host'),
	{ok, HssRealm} = application:get_env(hss_realm),
	{ok, HssHost} = application:get_env(hss_host),
	{ok, AaaFailure} = application:get_env(aaa_failure),
	process_flag(trap_exit, true),
	{ok, idle, #statedata{service = Service, session_id = SessionAttributes,
			origin_host = OriginHost, origin_realm = OriginRealm,
			nas_address = ClientAddress,
			hss_realm = HssRealm, hss_host = HssHost,
			aaa_failure = AaaFailure}};
init([diameter, ServerAddress, ServerPort, _ClientAddress, _ClientPort,
		_PasswordReq, _Trusted, SessionId, _ApplicationId, _AuthReqType,
		OriginHost, OriginRealm, DestinationHost, DestinationRealm,
		#'3gpp_swm_DER'{'AAA-Failure-Indication' = [1]} = _Request,
		_Options] = _Args) ->
	Service = {ocs_diameter_auth_service, ServerAddress, ServerPort},
	{ok, HssRealm} = application:get_env(hss_realm),
	{ok, HssHost} = application:get_env(hss_host),
	process_flag(trap_exit, true),
	{ok, idle, #statedata{service = Service, session_id = SessionId,
			origin_host = OriginHost, origin_realm = OriginRealm,
			nas_host = DestinationHost, nas_realm = DestinationRealm,
			hss_realm = HssRealm, hss_host = HssHost,
			aaa_failure = true}};
init([diameter, ServerAddress, ServerPort, _ClientAddress, _ClientPort,
		_PasswordReq, _Trusted, SessionId, _ApplicationId, _AuthReqType,
		OriginHost, OriginRealm, DestinationHost, DestinationRealm,
		#'3gpp_sta_DER'{'AAA-Failure-Indication' = [1]} = _Request,
		_Options] = _Args) ->
	Service = {ocs_diameter_auth_service, ServerAddress, ServerPort},
	{ok, HssRealm} = application:get_env(hss_realm),
	{ok, HssHost} = application:get_env(hss_host),
	process_flag(trap_exit, true),
	{ok, idle, #statedata{service = Service, session_id = SessionId,
			origin_host = OriginHost, origin_realm = OriginRealm,
			nas_host = DestinationHost, nas_realm = DestinationRealm,
			hss_realm = HssRealm, hss_host = HssHost,
			aaa_failure = true}};
init([diameter, ServerAddress, ServerPort, _ClientAddress, _ClientPort,
		_PasswordReq, _Trusted, SessionId, _ApplicationId, _AuthReqType,
		OriginHost, OriginRealm, DestinationHost, DestinationRealm,
		_Request, _Options] = _Args) ->
	Service = {ocs_diameter_auth_service, ServerAddress, ServerPort},
	{ok, HssRealm} = application:get_env(hss_realm),
	{ok, HssHost} = application:get_env(hss_host),
	{ok, AaaFailure} = application:get_env(aaa_failure),
	process_flag(trap_exit, true),
	{ok, idle, #statedata{service = Service, session_id = SessionId,
			origin_host = OriginHost, origin_realm = OriginRealm,
			nas_host = DestinationHost, nas_realm = DestinationRealm,
			hss_realm = HssRealm, hss_host = HssHost,
			aaa_failure = AaaFailure}}.

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
idle(cast = _EventType,
		{vector, {AkaFsm, Identity, undefined, RAT, ANID}} = _EventContent,
		Data) when is_pid(AkaFsm), is_binary(Identity),
		is_integer(RAT), is_list(ANID) ->
	NewData = Data#statedata{aka_fsm = AkaFsm,
			identity = Identity, rat_type = RAT, anid = ANID},
	idle1(ocs:find_service(Identity), NewData);
idle(cast = _EventType,
		{vector, {AkaFsm, Identity, undefined, RAT}},
		Data)
		when is_pid(AkaFsm), is_binary(Identity), is_integer(RAT) ->
	NewData = Data#statedata{aka_fsm = AkaFsm,
			identity = Identity, rat_type = RAT},
	idle1(ocs:find_service(Identity), NewData);
idle(cast = _EventType,
		{vector, {AkaFsm, Identity, AUTS, RAT, ANID}},
		Data) when is_pid(AkaFsm), is_binary(Identity),
		is_binary(AUTS), is_integer(RAT), is_list(ANID) ->
	NewData = Data#statedata{aka_fsm = AkaFsm,
			identity = Identity, auts = AUTS,
			rat_type = RAT, anid = ANID},
	idle1(ocs:find_service(Identity), NewData);
idle(cast = _EventType,
		{vector, {AkaFsm, Identity, AUTS, RAT}},
		Data) when is_pid(AkaFsm), is_binary(Identity),
		is_binary(AUTS), is_integer(RAT) ->
	NewData = Data#statedata{aka_fsm = AkaFsm,
			identity = Identity, auts = AUTS, rat_type = RAT},
	idle1(ocs:find_service(Identity), NewData);
idle(cast = _EventType,
		{register, {AkaFsm, Identity}},
		#statedata{hss_realm = HssRealm, hss_host = HssHost,
				aka_fsm = AkaFsm, identity = Identity,
				attributes = Attributes} = _Data)
		when is_pid(AkaFsm), is_binary(Identity),
		HssRealm == undefined ->
	SessionTimeout = case radius_attributes:find(?SessionTimeout,
			Attributes) of
		{ok, V1} ->
			[V1];
		{error, not_found} ->
			[]
	end,
	UserProfile = #'3gpp_swx_Non-3GPP-User-Data'{
			'Session-Timeout' = SessionTimeout},
	gen_statem:cast(AkaFsm, {ok, UserProfile, HssRealm, HssHost}),
	keep_state_and_data;
idle(cast = _EventType,
		{register, {AkaFsm, Identity, APN}},
		#statedata{hss_realm = HssRealm, hss_host = HssHost,
				aka_fsm = AkaFsm, identity = Identity,
				attributes = Attributes} = _Data)
		when is_pid(AkaFsm), is_binary(Identity), is_binary(APN),
		HssRealm == undefined ->
	SessionTimeout = case radius_attributes:find(?SessionTimeout,
			Attributes) of
		{ok, V1} ->
			[V1];
		{error, not_found} ->
			[]
	end,
	UserProfile = #'3gpp_swx_Non-3GPP-User-Data'{
			'Session-Timeout' = SessionTimeout},
	gen_statem:cast(AkaFsm, {ok, UserProfile, HssRealm, HssHost}),
	keep_state_and_data;
idle(cast = _EventType,
		{register, {AkaFsm, Identity}},
		#statedata{aka_fsm = AkaFsm, identity = Identity} = Data)
		when is_pid(AkaFsm), is_binary(Identity) ->
	case send_diameter_sar(?'3GPP_SERVER-ASSIGNMENT-TYPE_REGISTRATION',
			[], Data) of
		ok ->
			Action = {timeout, ?TIMEOUT, timeout},
			{next_state, register, Data, Action};
		{error, Reason} ->
			{stop, Reason}
	end;
idle(cast = _EventType,
		{register, {AkaFsm, Identity, APN}},
		#statedata{aka_fsm = AkaFsm, identity = Identity} = Data)
		when is_pid(AkaFsm), is_binary(Identity), is_binary(APN) ->
	case send_diameter_sar(?'3GPP_SERVER-ASSIGNMENT-TYPE_REGISTRATION',
			[APN], Data) of
		ok ->
			Action = {timeout, ?TIMEOUT, timeout},
			{next_state, register, Data, Action};
		{error, Reason} ->
			{stop, Reason}
	end.
%% @hidden
idle1({ok, #service{enabled = false}},
		#statedata{aka_fsm = AkaFsm} = _Data) ->
	gen_statem:cast(AkaFsm, {error, disabled}),
	keep_state_and_data;
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF},
		attributes = Attributes}}, #statedata{anid = undefined,
		auts = undefined, aka_fsm = AkaFsm} = Data) ->
	RAND = ocs_milenage:f0(),
	NewData = Data#statedata{rand = RAND, attributes = Attributes},
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQN = sqn(DIF),
	AMF = amf(false),
	MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
	AUTN = autn(SQN, AK, AMF, MAC),
	gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CK, IK, XRES}}),
	{keep_state, NewData};
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF},
		attributes = Attributes}}, #statedata{anid = ANID,
		auts = undefined, aka_fsm = AkaFsm} = Data) ->
	RAND = ocs_milenage:f0(),
	NewData = Data#statedata{rand = RAND, attributes = Attributes},
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQN = sqn(DIF),
	AMF = amf(true),
	MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
	AUTN = autn(SQN, AK, AMF, MAC),
	% if AMF separation bit = 1 use CK'/IK'
	<<CKprime:16/binary, IKprime:16/binary>> = kdf(CK, IK, ANID, SQN, AK),
	gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CKprime, IKprime, XRES}}),
	{keep_state, NewData};
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF},
		attributes = Attributes}}, #statedata{anid = undefined,
		rand = RAND, identity = Identity,
		auts = <<SQN:48, MAC_S:8/binary>> = AUTS,
		aka_fsm = AkaFsm} = Data) when is_binary(RAND) ->
	NewData = Data#statedata{rand = undefined, attributes = Attributes},
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQNhe = sqn(DIF),
	SQNms = sqn_ms(SQN, OPc, K, RAND),
	AMF = amf(false),
	case SQNhe - SQNms of
		A when A =< 268435456 ->
			MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNhe:48>>, AMF),
			AUTN = autn(SQNhe, AK, AMF, MAC_A),
			gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CK, IK, XRES}}),
			{keep_state, NewData};
		_ ->
			case ocs_milenage:'f1*'(OPc, K, RAND, <<SQNms:48>>, amf(false)) of
				MAC_S ->
					MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNms:48>>, AMF),
					AUTN = autn(SQNms, AK, AMF, MAC_A),
					gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CK, IK, XRES}}),
					save_dif(Identity, dif(SQNms)),
					{keep_state, NewData};
				_ ->
					error_logger:error_report(["AUTS verification failed",
							{identity, Identity}, {auts, AUTS}]),
					gen_statem:cast(AkaFsm, {error, invalid}),
					{keep_state, NewData}
			end
	end;
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF1},
		attributes = Attributes}}, #statedata{anid = ANID,
		rand = RAND, identity = Identity,
		auts = <<SQN:48, MAC_S:8/binary>> = AUTS,
		aka_fsm = AkaFsm} = Data) when is_binary(RAND) ->
	NewData = Data#statedata{rand = undefined, attributes = Attributes},
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQNhe = sqn(DIF1),
	SQNms = sqn_ms(SQN, OPc, K, RAND),
	AMF = amf(true),
	case SQNhe - SQNms of
		A when A =< 268435456 ->
			MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNhe:48>>, AMF),
			AUTN = autn(SQNhe, AK, AMF, MAC_A),
			<<CKprime:16/binary,
					IKprime:16/binary>> = kdf(CK, IK, ANID, SQNhe, AK),
			gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CKprime, IKprime, XRES}}),
			{keep_state, NewData};
		_ ->
			case ocs_milenage:'f1*'(OPc, K, RAND, <<SQNms:48>>, amf(false)) of
				MAC_S ->
					MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNms:48>>, AMF),
					AUTN = autn(SQNms, AK, AMF, MAC_A),
					<<CKprime:16/binary,
							IKprime:16/binary>> = kdf(CK, IK, ANID, SQNms, AK),
					gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CKprime, IKprime, XRES}}),
					save_dif(Identity, dif(SQNms)),
					{keep_state, NewData};
				_ ->
					error_logger:error_report(["AUTS verification failed",
							{identity, Identity}, {auts, AUTS}]),
					gen_statem:cast(AkaFsm, {error, invalid}),
					{keep_state, NewData}
			end
	end;
idle1({error, not_found},
		#statedata{hss_realm = undefined, aka_fsm = AkaFsm} = _Data) ->
	gen_statem:cast(AkaFsm, {error, user_unknown}),
	keep_state_and_data;
idle1({ok, #service{password = Password}},
		#statedata{hss_realm = undefined, aka_fsm = AkaFsm} = _Data)
		when not is_record(Password, aka_cred) ->
	gen_statem:cast(AkaFsm, {error, user_unknown}),
	keep_state_and_data;
idle1({error, not_found},
		#statedata{service = false, aka_fsm = AkaFsm} = _Data) ->
	gen_statem:cast(AkaFsm, {error, user_unknown}),
	keep_state_and_data;
idle1({ok, #service{password = Password}},
		#statedata{service = false, aka_fsm = AkaFsm} = _Data)
		when not is_record(Password, aka_cred) ->
	gen_statem:cast(AkaFsm, {error, user_unknown}),
	keep_state_and_data;
idle1({error, not_found},
		#statedata{hss_realm = _HssRealm} = Data) ->
	case send_diameter_mar(Data) of
		ok ->
			Action = {timeout, ?TIMEOUT, timeout},
			{next_state, vector, Data, Action};
		{error, Reason} ->
			{stop, Reason}
	end;
idle1({ok, #service{password = Password}},
		#statedata{hss_realm = _HssRealm} = Data)
		when not is_record(Password, aka_cred) ->
	case send_diameter_mar(Data) of
		ok ->
			Action = {timeout, ?TIMEOUT, timeout},
			{next_state, vector, Data, Action};
		{error, Reason} ->
			{stop, Reason}
	end;
idle1({error, Reason},
		#statedata{aka_fsm = AkaFsm} = _Data) ->
	error_logger:error_report(["Service lookup failure",
			{module, ?MODULE}, {error, Reason}]),
	gen_statem:cast(AkaFsm, {error, Reason}),
	keep_state_and_data.

-spec vector(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>vector</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
vector(cast = _EventType,
		{ok, #'3gpp_swx_MAA'{'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
				'Origin-Realm' = HssRealm,
				'Origin-Host' = HssHost,
				'SIP-Number-Auth-Items' = [1],
				'SIP-Auth-Data-Item' = [#'3gpp_swx_SIP-Auth-Data-Item'{
				% 'SIP-Item-Number' = [1],
				'SIP-Authenticate' = [<<RAND:16/binary, AUTN:16/binary>>],
				'SIP-Authorization' = [XRES],
				'Confidentiality-Key' = [CK],
				'Integrity-Key' = [IK]}]}} = _EventContent,
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {ok, {RAND, AUTN, CK, IK, XRES}}),
	NewData  = Data#statedata{hss_realm = HssRealm,
			hss_host = HssHost, rand = RAND},
	{next_state, idle, NewData};
vector(cast = _EventType,
		{ok, #'3gpp_swx_MAA'{'Result-Code' = [ResultCode]} = _MAA},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, ResultCode}),
	{next_state, idle, Data};
vector(cast = _EventType,
		{ok, #'3gpp_swx_MAA'{'Experimental-Result' = [?'DIAMETER_ERROR_USER_UNKNOWN']}},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, user_unknown}),
	{next_state, idle, Data};
vector(cast = _EventType,
		{ok, #'3gpp_swx_MAA'{'Experimental-Result' = [ResultCode]}},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, ResultCode}),
	{next_state, idle, Data};
vector(cast = _EventType,
		{ok, #'diameter_base_answer-message'{'Result-Code' = ResultCode}},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, ResultCode}),
	{next_state, idle, Data};
vector(cast = _EventType, {error, Reason},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, Reason}),
	{next_state, idle, Data};
vector(timeout = _EventType,  timeout = _EventContent,
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, timeout}),
	{next_state, idle, Data}.

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
				= [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
				'Origin-Realm' = HssRealm,
				'Origin-Host' = HssHost,
				'Non-3GPP-User-Data' = [#'3gpp_swx_Non-3GPP-User-Data'{
						} = UserProfile]}} = _EventContent,
		#statedata{aka_fsm = AkaFsm} = Data) ->
	NewData  = Data#statedata{hss_realm = HssRealm, hss_host = HssHost},
	gen_statem:cast(AkaFsm, {ok, UserProfile, HssRealm, HssHost}),
	{next_state, idle, NewData};
register(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Result-Code' = [ResultCode]}},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, ResultCode}),
	{next_state, idle, Data};
register(cast = _EventType,
		{ok, #'3gpp_swx_SAA'{'Experimental-Result' = [ResultCode]}},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, ResultCode}),
	{next_state, idle, Data};
register(cast = _EventType,
		{ok, #'diameter_base_answer-message'{'Result-Code' = ResultCode}},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, ResultCode}),
	{next_state, idle, Data};
register(cast = _EventType, {error, Reason},
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, Reason}),
	{next_state, idle, Data};
register(timeout = _EventType,  timeout = _EventContent,
		#statedata{aka_fsm = AkaFsm} = Data) ->
	gen_statem:cast(AkaFsm, {error, timeout}),
	{next_state, idle, Data}.

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

-spec sqn(DIF) -> SQN
	when
		DIF :: integer(),
		SQN :: integer().
%% @doc Sequence Number (SQN).
%%
%% 	3GPP RTS 33.102 Annex C.1.1.3.
%% @private
sqn(DIF) when is_integer(DIF) ->
	(erlang:system_time(10) + DIF) bsl 5.

-spec autn(SQN, AK, AMF, MAC) -> AUTN
	when
		SQN :: integer(),
		AK :: integer(),
		AMF :: binary(),
		MAC :: binary(),
		AUTN :: binary().
%% @doc Network Authentication Token (AUTN).
%%
%% @private
autn(SQN, AK, AMF, MAC)
		when is_integer(SQN), is_integer(AK),
		byte_size(AMF) =:= 2, byte_size(MAC) =:= 8 ->
	SQNa = SQN bxor AK,
	<<SQNa:48, AMF/binary, MAC/binary>>.

-spec sqn_ms(SQN, OPc, K, RAND) -> SQN
	when
		SQN:: integer(),
		OPc :: binary(),
		K :: binary(),
		RAND :: binary(),
		SQN :: integer().
%% @doc Retrieve concealed `SQNms' from AUTS.
%%
%% @private
sqn_ms(SQN, OPc, K, RAND)
		when is_integer(SQN), byte_size(OPc) =:= 16,
		byte_size(K) =:= 16, byte_size(RAND) =:= 16 ->
	<<AK:48>> = ocs_milenage:'f5*'(OPc, K, RAND),
	SQN bxor AK.

-spec dif(SQN) -> DIF
	when
		SQN :: integer(),
		DIF :: integer().
%% @doc The DIF value represents the current difference
%% 	between generated SEQ values for that user and the GLC.
%%
%% 	3GPP RTS 33.102 Annex C.1.1.3.
%% @private
dif(SQN) when is_integer(SQN) ->
	SEQ = SQN bsr 5,
	SEQ - erlang:system_time(10).

-spec amf(Seperation) -> AMF
	when
		Seperation:: boolean(),
		AMF :: binary().
%% @doc Authentication Management Field (AMF).
%%
%% 	See 3GPP TS 33.102 Annex H.
%% @private
amf(false) ->
	<<0:1, 0:15>>;
amf(true) ->
	<<1:1, 0:15>>.

-spec kdf(CK, IK, ANID, SQN, AK) -> MSK
	when
		CK :: binary(),
		IK :: binary(),
		ANID :: string(),
		SQN :: integer(),
		AK :: integer(),
		MSK :: binary().
%% @doc Key Derivation Function (KDF).
%%
%% 	See 3GPP TS 33.402 Annex A,
%% 	    3GPP TS 32.220 Annex B.
%% @private
kdf(CK, IK, "WLAN", SQN, AK)
		when byte_size(CK) =:= 16,
		byte_size(IK) =:= 16,
		is_integer(SQN), is_integer(AK) ->
	SQNi = SQN bxor AK,
	?HMAC(<<CK/binary, IK/binary>>, <<16#20, "WLAN", 4:16, SQNi:48, 6:16>>).

-spec save_dif(Identity, DIF) -> ok
	when
		Identity :: binary(),
		DIF :: integer().
%% @doc Save the new DIF for subscriber.
%% @hidden
save_dif(Identity, DIF)
		when is_binary(Identity), is_integer(DIF)->
	Now = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {Now, N},
	F = fun() ->
			[#service{password = P} = S1] = mnesia:read(service,
					Identity, write),
			S2 = S1#service{last_modified = LM,
					password = P#aka_cred{dif  = DIF}},
			mnesia:write(service, S2, write)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec send_diameter_mar(Data) -> Result
	when
		Data :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Multimedia-Auth-Request (MAR) to HSS.
%% @hidden
send_diameter_mar(#statedata{hss_host = undefined,
		hss_realm = HssRealm, origin_host = OriginHost,
		origin_realm = OriginRealm} = Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_MAR'{'Session-Id' = SessionId,
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm},
	send_diameter_mar1(Request, Data);
send_diameter_mar(#statedata{hss_host = HssHost,
		hss_realm = HssRealm, origin_host = OriginHost,
		origin_realm = OriginRealm} = Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_MAR'{'Session-Id' = SessionId,
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = [HssHost]},
	send_diameter_mar1(Request, Data).
%% @hidden
send_diameter_mar1(Request1, #statedata{anid = undefined,
		identity = Identity} = Data) ->
	AuthData = #'3gpp_swx_SIP-Auth-Data-Item'{
			'SIP-Authentication-Scheme' = [<<"EAP-AKA">>]},
	Request2 = Request1#'3gpp_swx_MAR'{'User-Name' =  Identity,
			'SIP-Number-Auth-Items' = 1,
			'SIP-Auth-Data-Item' = AuthData},
	send_diameter_mar2(Request2, Data);
send_diameter_mar1(Request1, #statedata{anid = ANID,
		identity = Identity} = Data) ->
	AuthData = #'3gpp_swx_SIP-Auth-Data-Item'{
			'SIP-Authentication-Scheme' = [<<"EAP-AKA'">>]},
	Request2 = Request1#'3gpp_swx_MAR'{'User-Name' =  Identity,
			'ANID' = [ANID], 'SIP-Number-Auth-Items' = 1,
			'SIP-Auth-Data-Item' = AuthData},
	send_diameter_mar2(Request2, Data).
%% @hidden
send_diameter_mar2(Request,
		#statedata{auts = undefined} = Data) ->
	send_diameter_mar3(Request, Data);
send_diameter_mar2(#'3gpp_swx_MAR'{
		'SIP-Auth-Data-Item' = AuthData1} = Request1,
		#statedata{auts = AUTS, rand = RAND} = Data)
		when byte_size(AUTS) =:= 14, byte_size(RAND) =:= 16 ->
	AuthData2 = AuthData1#'3gpp_swx_SIP-Auth-Data-Item'{
			'SIP-Authorization' = [<<RAND/binary, AUTS/binary>>]},
	Request2 = Request1#'3gpp_swx_MAR'{'SIP-Auth-Data-Item' = AuthData2},
	send_diameter_mar3(Request2, Data).
%% @hidden
send_diameter_mar3(Request,
		#statedata{aaa_failure = true} = Data) ->
	Request1 = Request#'3gpp_swx_MAR'{'AAA-Failure-Indication' = [1]},
	send_diameter_mar4(Request1, Data);
send_diameter_mar3(Request, Data) ->
	send_diameter_mar4(Request, Data).
%% @hidden
send_diameter_mar4(Request1,
		#statedata{rat_type = RAT, service = Service} = _Data) ->
	Request2 = Request1#'3gpp_swx_MAR'{'Auth-Session-State' = 1,
			'Vendor-Specific-Application-Id'
			= #'3gpp_swx_Vendor-Specific-Application-Id'{
			'Vendor-Id' = ?IANA_PEN_3GPP,
			'Auth-Application-Id' = [?SWx_APPLICATION_ID]},
			'RAT-Type' = [RAT]},
	diameter:call(Service, ?SWx_APPLICATION,
			Request2, [detach, {extra, [self()]}]).

-spec send_diameter_sar(ServerAssignmentType, APN, Data) -> Result
	when
		ServerAssignmentType :: ?'3GPP_SERVER-ASSIGNMENT-TYPE_REGISTRATION', 
		APN :: [binary()],
		Data :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER Server-Assignment-Request (SAR) to HSS.
%% @hidden
send_diameter_sar(ServerAssignmentType, APN, #statedata{hss_host = undefined,
		hss_realm = HssRealm, origin_host = OriginHost,
		origin_realm = OriginRealm} = Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Service-Selection' = APN},
	send_diameter_sar1(Request, ServerAssignmentType, Data);
send_diameter_sar(ServerAssignmentType, APN, #statedata{hss_host = HssHost,
		hss_realm = HssRealm, origin_host = OriginHost,
		origin_realm = OriginRealm} = Data) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_SAR'{'Session-Id' = SessionId,
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = [HssHost],
			'Service-Selection' = APN},
	send_diameter_sar1(Request, ServerAssignmentType, Data).
%% @hidden
send_diameter_sar1(Request1, SAT,
		#statedata{identity = Identity, service = Service} = _Data) ->
	Request2 = Request1#'3gpp_swx_SAR'{'Auth-Session-State' = 1,
			'User-Name' = Identity,
			'Server-Assignment-Type' = SAT,
			'Vendor-Specific-Application-Id'
			= #'3gpp_swx_Vendor-Specific-Application-Id'{
			'Vendor-Id' = ?IANA_PEN_3GPP,
			'Auth-Application-Id' = [?SWx_APPLICATION_ID]}},
	diameter:call(Service, ?SWx_APPLICATION,
			Request2, [detach, {extra, [self()]}]).

