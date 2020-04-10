%%% ocs_eap_aka_auc_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2018 SigScale Global Inc.
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
%%% 	implements the functions associated with an Authentication Center (AuC)
%%% 	in the user's home domain within EAP 3rd Generation Authentication and
%%% 	Key Agreement (EAP-AKA') in the {@link //ocs. ocs} application.
%%%
%%% 	The users of this module are the EAP-AKA/AKA' handlers which send
%%% 	`{AkaFsm, Identity, AUTS, RAT, ANID}' (AKA') or
%%% 	`{AkaFsm, Identity, AUTS, RAT}' (AKA) and expect back
%%% 	`{RAND, AUTN, CKprime, IKprime, XRES}' (AKA'),
%%% 	`{RAND, AUTN, CK, IK, XRES}' (AKA) or `{error, Reason}'.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc4187">
%%% 	RFC4187 - Extensible Authentication Protocol Method for 3rd Generation
%%% 		Authentication and Key Agreement (EAP-AKA)</a>
%%% @reference <a href="http://tools.ietf.org/html/rfc5448">
%%% 	RFC5448 - Improved Extensible Authentication Protocol Method for
%%% 		3rd Generation Authentication and Key Agreement (EAP-AKA')</a>
%%% @reference <a href="http://webapp.etsi.org/key/key.asp?GSMSpecPart1=33&amp;GSMSpecPart2=402">
%%% 	Security Aspects of non-3GPP Accesses (3GPP TS 33.402)</a>
%%%
-module(ocs_eap_aka_auc_fsm).
-copyright('Copyright (c) 2016 - 2018 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_aka_auc_fsm API
-export([]).

%% export the ocs_eap_aka_auc_fsm state callbacks
-export([idle/2, auth/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include("ocs.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include("diameter_3gpp.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-record(statedata,
		{aka_fsm :: pid() | undefined,
		identity :: binary() | undefined,
		rand :: binary() | undefined,
		auts :: binary() | undefined,
		rat_type :: non_neg_integer() | undefined,
		anid :: string() | undefined,
		service :: term(),
		origin_host :: binary(),
		origin_realm :: binary(),
		hss_realm :: string() | undefined,
		hss_host :: string() | undefined}).
-type statedata() :: #statedata{}.

-define(IANA_PEN_3GPP, 10415).
-define(SWx_APPLICATION_ID, 16777265).
-define(SWx_APPLICATION_DICT, diameter_gen_3gpp_swx_application).
-define(SWx_APPLICATION, ocs_diameter_3gpp_swx_application).

-define(TIMEOUT, 5000).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_auc_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_auc_fsm gen_fsm call backs
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
init(_Args) ->
	process_flag(trap_exit, true),
	{ok, HssRealm} = application:get_env(hss_realm),
	{ok, HssHost} = application:get_env(hss_host),
	Service = lists:keyfind(ocs_diameter_auth_service, 1, diameter:services()),
	OriginRealm = diameter:service_info(Service, 'Origin-Realm'),
	OriginHost = diameter:service_info(Service, 'Origin-Host'),
	{ok, idle, #statedata{service = Service,
			origin_host = OriginHost, origin_realm = OriginRealm,
			hss_realm = HssRealm, hss_host = HssHost}}.

-spec idle(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData} | {next_state, NextStateName, NewStateData,
		Timeout}
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
idle({AkaFsm, Identity, undefined, RAT, ANID}, StateData)
		when is_pid(AkaFsm), is_binary(Identity),
		is_integer(RAT), is_list(ANID) ->
	NewStateData = StateData#statedata{aka_fsm = AkaFsm,
			identity = Identity, rat_type = RAT, anid = ANID},
	idle1(ocs:find_service(Identity), NewStateData);
idle({AkaFsm, Identity, undefined, RAT}, StateData)
		when is_pid(AkaFsm), is_binary(Identity),
		is_integer(RAT) ->
	NewStateData = StateData#statedata{aka_fsm = AkaFsm,
			identity = Identity, rat_type = RAT},
	idle1(ocs:find_service(Identity), NewStateData);
idle({AkaFsm, Identity, AUTS, RAT, ANID}, StateData)
		when is_pid(AkaFsm), is_binary(Identity),
		is_binary(AUTS), is_integer(RAT), is_list(ANID) ->
	NewStateData = StateData#statedata{aka_fsm = AkaFsm,
			identity = Identity, auts = AUTS,
			rat_type = RAT, anid = ANID},
	idle1(ocs:find_service(Identity), NewStateData);
idle({AkaFsm, Identity, AUTS, RAT}, StateData)
		when is_pid(AkaFsm), is_binary(Identity),
		is_binary(AUTS), is_integer(RAT) ->
	NewStateData = StateData#statedata{aka_fsm = AkaFsm,
			identity = Identity, auts = AUTS, rat_type = RAT},
	idle1(ocs:find_service(Identity), NewStateData).
%% @hidden
idle1({ok, #service{enabled = false}},
		#statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, disabled}),
	{next_state, idle, StateData};
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF}}},
		#statedata{anid = undefined, auts = undefined,
		aka_fsm = AkaFsm} = StateData) ->
	RAND = ocs_milenage:f0(),
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQN = sqn(DIF),
	AMF = amf(false),
	MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
	AUTN = autn(SQN, AK, AMF, MAC),
	gen_fsm:send_event(AkaFsm, {RAND, AUTN, CK, IK, XRES}),
	{next_state, idle, StateData#statedata{rand = RAND}};
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF}}},
		#statedata{anid = ANID, auts = undefined,
		aka_fsm = AkaFsm} = StateData) ->
	RAND = ocs_milenage:f0(),
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQN = sqn(DIF),
	AMF = amf(true),
	MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
	AUTN = autn(SQN, AK, AMF, MAC),
	% if AMF separation bit = 1 use CK'/IK'
	<<CKprime:16/binary, IKprime:16/binary>> = kdf(CK, IK, ANID, SQN, AK),
	gen_fsm:send_event(AkaFsm, {RAND, AUTN, CKprime, IKprime, XRES}),
	{next_state, idle, StateData#statedata{rand = RAND}};
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF}}},
		#statedata{anid = undefined, rand = RAND, identity = Identity,
		auts = <<SQN:48, MAC_S:8/binary>> = AUTS,
		aka_fsm = AkaFsm} = StateData) when is_binary(RAND) ->
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQNhe = sqn(DIF),
	SQNms = sqn_ms(SQN, OPc, K, RAND),
	AMF = amf(false),
	case SQNhe - SQNms of
		A when A =< 268435456 ->
			MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNhe:48>>, AMF),
			AUTN = autn(SQNhe, AK, AMF, MAC_A),
			gen_fsm:send_event(AkaFsm, {RAND, AUTN, CK, IK, XRES}),
			{next_state, idle, StateData#statedata{rand = undefined}};
		_ ->
			case ocs_milenage:'f1*'(OPc, K, RAND, <<SQNms:48>>, amf(false)) of
				MAC_S ->
					MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNms:48>>, AMF),
					AUTN = autn(SQNms, AK, AMF, MAC_A),
					gen_fsm:send_event(AkaFsm, {RAND, AUTN, CK, IK, XRES}),
					save_dif(Identity, dif(SQNms)),
					{next_state, idle, StateData#statedata{rand = undefined}};
				_ ->
					error_logger:error_report(["AUTS verification failed",
							{identity, Identity}, {auts, AUTS}]),
					gen_fsm:send_event(AkaFsm, {error, invalid}),
					{next_state, idle, StateData#statedata{rand = undefined}}
			end
	end;
idle1({ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF1}}},
		#statedata{anid = ANID, rand = RAND, identity = Identity,
		auts = <<SQN:48, MAC_S:8/binary>> = AUTS,
		aka_fsm = AkaFsm} = StateData) when is_binary(RAND) ->
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
			gen_fsm:send_event(AkaFsm, {RAND, AUTN, CKprime, IKprime, XRES}),
			{next_state, idle, StateData#statedata{rand = undefined}};
		_ ->
			case ocs_milenage:'f1*'(OPc, K, RAND, <<SQNms:48>>, amf(false)) of
				MAC_S ->
					MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNms:48>>, AMF),
					AUTN = autn(SQNms, AK, AMF, MAC_A),
					<<CKprime:16/binary,
							IKprime:16/binary>> = kdf(CK, IK, ANID, SQNms, AK),
					gen_fsm:send_event(AkaFsm, {RAND, AUTN, CKprime, IKprime, XRES}),
					save_dif(Identity, dif(SQNms)),
					{next_state, idle, StateData#statedata{rand = undefined}};
				_ ->
					error_logger:error_report(["AUTS verification failed",
							{identity, Identity}, {auts, AUTS}]),
					gen_fsm:send_event(AkaFsm, {error, invalid}),
					{next_state, idle, StateData#statedata{rand = undefined}}
			end
	end;
idle1({error, not_found}, #statedata{hss_realm = undefined,
		aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, user_unknown}),
	{next_state, idle, StateData};
idle1({error, not_found},
		#statedata{hss_realm = _HssRealm} = StateData) ->
	case send_diameter_request(StateData) of
		ok ->
			{next_state, auth, StateData, ?TIMEOUT};
		{error, Reason} ->
			{stop, Reason, StateData}
	end;
idle1({error, Reason}, #statedata{aka_fsm = AkaFsm} = StateData) ->
	error_logger:error_report(["Service lookup failure",
			{module, ?MODULE}, {error, Reason}]),
	gen_fsm:send_event(AkaFsm, {error, Reason}),
	{next_state, idle, StateData}.

-spec auth(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData} | {next_state, NextStateName, NewStateData,
		Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>auth</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
auth({ok, #'3gpp_swx_MAA'{'Result-Code' = [2001],
		'Origin-Realm' = HssRealm,
		'Origin-Host' = HssHost,
		'SIP-Number-Auth-Items' = [1],
		'SIP-Auth-Data-Item' = [#'3gpp_swx_SIP-Auth-Data-Item'{
		'SIP-Item-Number' = [1],
		'SIP-Authenticate' = [<<RAND:16/binary, AUTN:16/binary>>],
		'SIP-Authorization' = [XRES],
		'Confidentiality-Key' = [CK],
		'Integrity-Key' = [IK]}]}},
		#statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {RAND, AUTN, CK, IK, XRES}),
	NewStateData  = StateData#statedata{hss_realm = HssRealm,
			hss_host = HssHost, rand = RAND},
	{next_state, idle, NewStateData};
auth({ok, #'3gpp_swx_MAA'{'Result-Code' = [ResultCode]} = _MAA},
		#statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, ResultCode}),
	{next_state, idle, StateData};
auth({ok, #'3gpp_swx_MAA'{'Experimental-Result' = [?'DIAMETER_ERROR_USER_UNKNOWN']}},
		#statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, user_unknown}),
	{next_state, idle, StateData};
auth({ok, #'3gpp_swx_MAA'{'Experimental-Result' = [ResultCode]}},
		#statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, ResultCode}),
	{next_state, idle, StateData};
auth({error, Reason}, #statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, Reason}),
	{next_state, idle, StateData};
auth(timeout, #statedata{aka_fsm = AkaFsm} = StateData) ->
	gen_fsm:send_event(AkaFsm, {error, timeout}),
	{next_state, idle, StateData}.

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(),
		StateName :: atom(),
      StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData} | {next_state, NextStateName, NewStateData,
		Timeout}
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
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

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
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

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
	crypto:hmac(sha256, <<CK/binary, IK/binary>>,
			<<16#20, "WLAN", 4:16, SQNi:48, 6:16>>).

-spec save_dif(Identity, DIF) -> ok
	when
		Identity :: binary(),
		DIF :: integer().
%% @doc Save the new DIF for subscriber.
%% @hidden
save_dif(Identity, DIF)
		when is_binary(Identity), is_integer(DIF)->
	Now = erlang:system_time(?MILLISECOND),
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

-spec send_diameter_request(StateData) -> Result
	when
		StateData :: #statedata{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Send DIAMETER request to HSS.
%% @hidden
send_diameter_request(#statedata{hss_host = undefined,
		hss_realm = HssRealm, origin_host = OriginHost,
		origin_realm = OriginRealm} = StateData) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_MAR'{'Session-Id' = SessionId,
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm},
	send_diameter_request1(Request, StateData);
send_diameter_request(#statedata{hss_host = HssHost,
		hss_realm = HssRealm, origin_host = OriginHost,
		origin_realm = OriginRealm} = StateData) ->
	SessionId = diameter:session_id([OriginHost]),
	Request = #'3gpp_swx_MAR'{'Session-Id' = SessionId,
			'Origin-Realm' = OriginRealm, 'Origin-Host' = OriginHost,
			'Destination-Realm' = HssRealm, 'Destination-Host' = [HssHost]},
	send_diameter_request1(Request, StateData).
%% @hidden
send_diameter_request1(Request1, #statedata{anid = undefined,
		identity = Identity} = StateData) ->
	AuthData = #'3gpp_swx_SIP-Auth-Data-Item'{
			'SIP-Authentication-Scheme' = [<<"EAP-AKA">>]},
	Request2 = Request1#'3gpp_swx_MAR'{'User-Name' =  Identity,
			'SIP-Number-Auth-Items' = 1,
			'SIP-Auth-Data-Item' = AuthData},
	send_diameter_request2(Request2, StateData);
send_diameter_request1(Request1, #statedata{anid = ANID,
		identity = Identity} = StateData) ->
	AuthData = #'3gpp_swx_SIP-Auth-Data-Item'{
			'SIP-Authentication-Scheme' = [<<"EAP-AKA'">>]},
	Request2 = Request1#'3gpp_swx_MAR'{'User-Name' =  Identity,
			'ANID' = [ANID], 'SIP-Number-Auth-Items' = 1,
			'SIP-Auth-Data-Item' = AuthData},
	send_diameter_request2(Request2, StateData).
%% @hidden
send_diameter_request2(Request,
		#statedata{auts = undefined} = StateData) ->
	send_diameter_request3(Request, StateData);
send_diameter_request2(#'3gpp_swx_MAR'{
		'SIP-Auth-Data-Item' = AuthData1} = Request1,
		#statedata{auts = AUTS, rand = RAND} = StateData)
		when byte_size(AUTS) =:= 14, byte_size(RAND) =:= 16 ->
	AuthData2 = AuthData1#'3gpp_swx_SIP-Auth-Data-Item'{
			'SIP-Authorization' = [<<RAND/binary, AUTS/binary>>]},
	Request2 = Request1#'3gpp_swx_MAR'{'SIP-Auth-Data-Item' = AuthData2},
	send_diameter_request3(Request2, StateData).
%% @hidden
send_diameter_request3(Request1,
		#statedata{rat_type = RAT, service = Service} = _StateData) ->
	Request2 = Request1#'3gpp_swx_MAR'{'Auth-Session-State' = 1,
			'Vendor-Specific-Application-Id'
			= #'3gpp_swx_Vendor-Specific-Application-Id'{
			'Vendor-Id' = ?IANA_PEN_3GPP,
			'Auth-Application-Id' = [?SWx_APPLICATION_ID]},
			'RAT-Type' = [RAT]},
	diameter:call(Service, ?SWx_APPLICATION,
			Request2, [detach, {extra, [self()]}]).

