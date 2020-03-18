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
-export([idle/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").

-record(statedata,
		{aka_fsm :: undefined | pid(),
		hss_realm :: string(),
		hss_host :: string()}).
-type statedata() :: #statedata{}.

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
	{ok, idle, #statedata{hss_realm = HssRealm, hss_host = HssHost}}.

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
idle({AkaFsm, Identity, ANID}, StateData)
		when is_pid(AkaFsm), is_binary(Identity), is_list(ANID) ->
	idle1(AkaFsm, ANID, StateData, ocs:find_service(Identity));
idle({AkaFsm, Identity}, StateData)
		when is_pid(AkaFsm), is_binary(Identity) ->
	idle1(AkaFsm, undefined, StateData, ocs:find_service(Identity)).
%% @hidden
idle1(AkaFsm, _ANID, StateData,
		{ok, #service{enabled = false}}) ->
	gen_fsm:send_event(AkaFsm, {error, disabled}),
	{next_state, idle, StateData};
idle1(AkaFsm, undefined, StateData,
		{ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF}}}) ->
	RAND = ocs_milenage:f0(),
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQN = sqn(DIF),
	AMF = amf(false),
	MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
	AUTN = autn(SQN, AK, AMF, MAC),
	gen_fsm:send_event(AkaFsm, {RAND, AUTN, CK, IK, XRES}),
	{next_state, idle, StateData};
idle1(AkaFsm, ANID, StateData,
		{ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF}}}) ->
	RAND = ocs_milenage:f0(),
	{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
	SQN = sqn(DIF),
	AMF = amf(true),
	MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
	AUTN = autn(SQN, AK, AMF, MAC),
	% if AMF separation bit = 1 use CK'/IK'
	<<CKprime:16/binary, IKprime:16/binary>> = kdf(CK, IK, ANID, SQN, AK),
	gen_fsm:send_event(AkaFsm, {RAND, AUTN, CKprime, IKprime, XRES}),
	{next_state, idle, StateData};
idle1(AkaFsm, _ANID, #statedata{hss_realm = undefined] = StateData,
		{error, not_found}) ->
	gen_fsm:send_event(AkaFsm, {error, user_unknown}),
	{next_state, idle, StateData};
idle1(AkaFsm, _ANID, StateData, {error, Reason}) ->
	error_logger:error_report(["Service lookup failure",
			{module, ?MODULE}, {error, Reason}]),
	gen_fsm:send_event(AkaFsm, {error, Reason}),
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
	(erlang:system_time(10) - DIF) bsl 5.

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

