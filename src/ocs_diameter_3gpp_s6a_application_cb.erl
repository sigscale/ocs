%%% ocs_diameter_3gpp_s6a_application_cb.erl 
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module receives {@link //diameter. diameter} messages on a port assigned
%%% 	for the 3GPP DIAMETER S6a/S6d in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS TS 29.272 MME and SGSN Diameter Interfaces
%%%
-module(ocs_diameter_3gpp_s6a_application_cb).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/5, prepare_request/4,
		prepare_retransmit/4, handle_answer/5, handle_error/5,
		handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_3gpp_s6a_application.hrl").
-include("ocs.hrl").

-record(state, {}).

-define(S6a_APPLICATION_ID, 16777251).
-define(IANA_PEN_3GPP, 10415).
-define(DIAMETER_AUTHENTICATION_DATA_UNAVAILABLE,  4181).
-define(DIAMETER_ERROR_CAMEL_SUBSCRIPTION_PRESENT, 4182).
-define(DIAMETER_ERROR_USER_UNKNOWN,               5001).
-define(DIAMETER_ERROR_ROAMING_NOT_ALLOWED,        5004).
-define(DIAMETER_ERROR_UNKNOWN_EPS_SUBSCRIPTION,   5420).
-define(DIAMETER_ERROR_RAT_NOT_ALLOWED,            5421).
-define(DIAMETER_ERROR_EQUIPMENT_UNKNOWN,          5422).
-define(DIAMETER_ERROR_UNKOWN_SERVING_NODE,        5423).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  #diameter_packet{}.
-type message() ::  tuple() | list().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

-dialyzer({[nowarn_function, no_match], kdf/5}).
-ifdef(OTP_RELEASE).
	-define(HMAC(Key, Data),
		case ?OTP_RELEASE of
			OtpRelease when OtpRelease >= 23 ->
				crypto:mac(hmac, sha256, Key, Data);
			OtpRelease when OtpRelease < 23 ->
				crypto:hmac(sha256, Key, Data)
		end).
-else.
	-define(HMAC(Key, Data), crypto:hmac(sha256, Key, Data)).
-endif.

%%----------------------------------------------------------------------
%%  The DIAMETER application callbacks
%%----------------------------------------------------------------------

-spec peer_up(ServiceName, Peer, State) -> NewState
	when
		ServiceName :: diameter:service_name(),
		Peer ::  peer(),
		State :: state(),
		NewState :: state().
%% @doc Invoked when the peer connection is available
peer_up(_ServiceName, _Peer, State) ->
    State.

-spec peer_down(ServiceName, Peer, State) -> NewState
	when
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		State :: state(),
		NewState :: state().
%% @doc Invoked when the peer connection is not available
peer_down(_ServiceName, _Peer, State) ->
    State.

-spec pick_peer(LocalCandidates, RemoteCandidates,
		ServiceName, State, Fsm) -> Result
	when
		LocalCandidates :: [peer()],
		RemoteCandidates :: [peer()],
		ServiceName :: diameter:service_name(),
		State :: state(),
		Fsm :: pid(),
		NewState :: state(),
		Selection :: {ok, Peer} | {Peer, NewState},
		Peer :: peer() | false,
		Result :: Selection | false.
%% @doc Invoked as a consequence of a call to diameter:call/4 to select
%% a destination peer for an outgoing request. 
pick_peer([Peer | _] = _LocalCandidates, _RemoteCandidates,
		_ServiceName, _State, _Fsm) ->
	{ok, Peer}.

-spec prepare_request(Packet, ServiceName, Peer, Fsm) -> Action
	when
		Packet :: packet(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Fsm :: pid(),
		Action :: Send | Discard | {eval_packet, Action, PostF},
		Send :: {send, packet() | message()},
		Discard :: {discard, Reason} | discard,
		Reason :: term(),
		PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and transport 
prepare_request(#diameter_packet{} = Packet, _ServiceName, _Peer, _Fsm) ->
	{send, Packet}.

-spec prepare_retransmit(Packet, ServiceName, Peer, Fsm) -> Action
	when
		Packet :: packet(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Fsm :: pid(),
		Action :: Send | Discard | {eval_packet, Action, PostF},
		Send :: {send, packet() | message()},
		Discard :: {discard, Reason} | discard,
		Reason :: term(),
		PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and retransmission.
%% In case of peer connection is lost alternate peer is selected.
prepare_retransmit(Packet, ServiceName, Peer, Fsm) ->
	prepare_request(Packet, ServiceName, Peer, Fsm).

-spec handle_answer(Packet, Request, ServiceName, Peer, Fsm) -> Result
	when
		Packet :: packet(),
		Request :: message(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Fsm :: pid(),
		Result :: term().
%% @doc Invoked when an answer message is received from a peer.
handle_answer(#diameter_packet{msg = Answer, errors = []} = _Packet,
		_Request, _ServiceName, _Peer, Fsm) ->
    gen_fsm:send_event(Fsm, {ok, Answer});
handle_answer(#diameter_packet{msg = Answer, errors = Errors} = _Packet,
		Request, ServiceName, {_, Caps} = _Peer, Fsm) ->
	errors(ServiceName, Caps, Request, Errors),
	gen_fsm:send_event(Fsm, {ok, Answer}).

-spec handle_error(Reason, Request, ServiceName, Peer, Fsm) -> Result
	when
		Reason :: timeout | failover | term(),
		Request :: message(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Fsm :: pid(),
		Result :: term().
%% @doc Invoked when an error occurs before an answer message is received
%% in response to an outgoing request.
handle_error(Reason, _Request, _ServiceName, _Peer, Fsm) ->
    gen_fsm:send_event(Fsm, {error, Reason}).

-spec handle_request(Packet, ServiceName, Peer) -> Action
	when
		Packet :: packet(),
		ServiceName :: term(),
		Peer :: peer(),
		Action :: Reply | {relay, [Opt]} | discard
			| {eval | eval_packet, Action, PostF},
		Reply :: {reply, packet() | message()}
			| {answer_message, 3000..3999|5000..5999}
			| {protocol_error, 3000..3999},
		Opt :: diameter:call_opt(),
		PostF :: diameter:evaluable().
%% @doc Invoked when a request message is received from the peer.
handle_request(#diameter_packet{msg = Request, errors = []} = _Packet,
		ServiceName, {_, Caps} = _Peer) ->
	request(ServiceName, Caps, Request);
handle_request(#diameter_packet{msg = Request, errors = Errors} = _Packet,
		ServiceName, {_, Caps} = _Peer) ->
	case errors(ServiceName, Caps, Request, Errors) of
		ok ->
			request(ServiceName, Caps, Request);
		{error, Error} ->
			{answer_message, Error}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec request(ServiceName, Capabilities, Request) -> Action
	when
		ServiceName :: term(),
		Capabilities :: capabilities(),
		Request :: message(),
		Action :: Reply | {relay, [Opt]} | discard
			| {eval|eval_packet, Action, PostF},
		Reply :: {reply, message()}
			| {answer_message, 3000..3999|5000..5999}
			| {protocol_error, 3000..3999},
		Opt :: diameter:call_opt(),
		PostF :: diameter:evaluable().
%% @doc Handle received request.
%% @private
request(ServiceName, Capabilities, Request) ->
	#diameter_caps{host_ip_address = {_, HostIpAddresses}} = Capabilities,
	request(ServiceName, Capabilities, Request, HostIpAddresses).
%% @hidden
request(ServiceName, Capabilities, Request, [H | T]) ->
	case ocs:find_client(H) of
		{ok, #client{protocol = diameter} = Client} ->
			process_request(ServiceName, Capabilities, Request, Client);
		{error, not_found} ->
			request(ServiceName, Capabilities, Request, T)
	end;
request(ServiceName, Capabilities, Request, []) ->
	{error, Error} = errors(ServiceName, Capabilities, Request,
			[?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_PEER']),
	{answer_message, Error}.

-spec process_request(ServiceName, Capabilities, Request, Client) -> Result
	when
		ServiceName :: term(),
		Capabilities :: capabilities(),
		Request :: term(),
		Client :: #client{},
		Result :: {reply, message()} | discard.
%% @doc Process a received DIAMETER packet.
%% @private
%% @todo Handle S6a/S6d requests.
process_request({_, ServerAddress, ServerPort} = ServiceName,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = Capabilities,
		#'3gpp_s6a_AIR'{'Session-Id' = SId} = Request,
		#client{address = CAddress, port = CPort} = Client) ->
	try
		authentication_information(ServiceName, Capabilities, Request, Client)
	catch
		_:_Reason ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer}
	end;
process_request({_, ServerAddress, ServerPort} = ServiceName,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = Capabilities,
		#'3gpp_s6a_ULR'{'Session-Id' = SId} = Request,
		#client{address = CAddress, port = CPort} = Client) ->
	try
		update_location(ServiceName, Capabilities, Request, Client)
	catch
		_:_Reason ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_ULA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS'],
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer}
	end;
process_request({_, ServerAddress, ServerPort} = ServiceName,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = Capabilities,
		#'3gpp_s6a_PUR'{'Session-Id' = SId} = Request,
		#client{address = CAddress, port = CPort} = Client) ->
	try
		purge_ue(ServiceName, Capabilities, Request, Client)
	catch
		_:_Reason ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_PUA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS'],
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer}
	end.

%% @hidden
authentication_information({_, ServerAddress, ServerPort} = ServiceName,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _Capabilities,
		#'3gpp_s6a_AIR'{'Session-Id' = SId, 'User-Name' = IMSI,
		'Requested-EUTRAN-Authentication-Info' = ReqEutranAuth} = Request,
		#client{address = CAddress, port = CPort}) ->
	case ocs:find_service(IMSI) of
		{ok, #service{enabled = false} = _Service} ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{ok, #service{password = #aka_cred{k = K, opc = OPc, dif = DIF},
				attributes = _Attributes} = _Service} ->
			% @todo Handle three digit MNC!
			<<Mcc1, Mcc2, Mcc3, Mnc1, Mnc2, Mnc3, _/binary>> = IMSI,
			SN = <<Mcc2:4, Mcc1:4, 15:4, Mcc3:4, Mnc2:4, Mnc1:4>>,
			AMF = <<1:1, 0:15>>,
			case ReqEutranAuth of
				[#'3gpp_s6a_Requested-EUTRAN-Authentication-Info'{
						'Re-Synchronization-Info' = []}] ->
					RAND = ocs_milenage:f0(),
					{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
					SQN = sqn(DIF),
					MAC = ocs_milenage:f1(OPc, K, RAND, <<SQN:48>>, AMF),
					AUTN = autn(SQN, AK, AMF, MAC),
					KASME = kdf(CK, IK, SN, SQN, AK),
					EutranVector = #'3gpp_s6a_E-UTRAN-Vector'{'Item-Number' = [1],
							'RAND' = RAND, 'XRES' = XRES, 'AUTN' = AUTN, 'KASME' = KASME},
					AuthInfo = #'3gpp_s6a_Authentication-Info'{'E-UTRAN-Vector' = [EutranVector]},
					Server = {ServerAddress, ServerPort},
					Client1 = {CAddress, CPort},
					Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
							'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
							'Auth-Session-State' = ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
							'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
							'Authentication-Info' = [AuthInfo]},
					ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
					{reply, Answer};
				[#'3gpp_s6a_Requested-EUTRAN-Authentication-Info'{
						'Re-Synchronization-Info' = [<<RAND:16/binary, SQN:48, MAC_S:8/binary>>]}] ->
					{XRES, CK, IK, <<AK:48>>} = ocs_milenage:f2345(OPc, K, RAND),
					SQNhe = sqn(DIF),
					SQNms = sqn_ms(SQN, OPc, K, RAND),
					case SQNhe - SQNms of
						A when A =< 268435456 ->
							MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNhe:48>>, AMF),
							AUTN = autn(SQNhe, AK, AMF, MAC_A),
							KASME = kdf(CK, IK, SN, SQNhe, AK),
							EutranVector = #'3gpp_s6a_E-UTRAN-Vector'{'Item-Number' = [1],
									'RAND' = RAND, 'XRES' = XRES, 'AUTN' = AUTN, 'KASME' = KASME},
							AuthInfo = #'3gpp_s6a_Authentication-Info'{'E-UTRAN-Vector' = [EutranVector]},
							Server = {ServerAddress, ServerPort},
							Client1 = {CAddress, CPort},
							Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
									'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
									'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
									'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
									'Authentication-Info' = [AuthInfo]},
							ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
							{reply, Answer};
						_A ->
							case ocs_milenage:'f1*'(OPc, K, RAND, <<SQNms:48>>, <<0:16>>) of
								MAC_S ->
									MAC_A = ocs_milenage:f1(OPc, K, RAND, <<SQNms:48>>, AMF),
									AUTN = autn(SQNms, AK, AMF, MAC_A),
									KASME = kdf(CK, IK, SN, SQNms, AK),
									save_dif(IMSI, dif(SQNms)),
									EutranVector = #'3gpp_s6a_E-UTRAN-Vector'{'Item-Number' = [1],
											'RAND' = RAND, 'XRES' = XRES, 'AUTN' = AUTN, 'KASME' = KASME},
									AuthInfo = #'3gpp_s6a_Authentication-Info'{'E-UTRAN-Vector' = [EutranVector]},
									Server = {ServerAddress, ServerPort},
									Client1 = {CAddress, CPort},
									Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
											'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
											'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
											'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
											'Authentication-Info' = [AuthInfo]},
									ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
									{reply, Answer};
								_ ->
									error_logger:error_report(["AUTS verification failed",
											{service, ServiceName}, {identity, IMSI},
											{auts, <<SQN:48, MAC_S/binary>>}]),
									Server = {ServerAddress, ServerPort},
									Client1 = {CAddress, CPort},
									Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
											'Experimental-Result' = [#'3gpp_s6a_Experimental-Result'{
											'Vendor-Id' = ?IANA_PEN_3GPP,
											'Experimental-Result-Code' = ?'DIAMETER_AUTHENTICATION_DATA_UNAVAILABLE'}],
											'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
											'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
									ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
									{reply, Answer}
							end
					end;
				[] ->
					Server = {ServerAddress, ServerPort},
					Client1 = {CAddress, CPort},
					Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
							'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
							'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
							'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
					ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
					{reply, Answer}
			end;
		{error, not_found} ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
					'Experimental-Result' = [#'3gpp_s6a_Experimental-Result'{
					'Vendor-Id' = ?IANA_PEN_3GPP,
					'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'}],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{error, Reason} ->
			error_logger:error_report(["Service lookup failure",
					{service, ServiceName}, {module, ?MODULE}, {error, Reason}]),
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer}
	end.

%% @hidden
update_location({_, ServerAddress, ServerPort} = ServiceName,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _Capabilities,
		#'3gpp_s6a_ULR'{'Session-Id' = SId,
		'User-Name' = IMSI, 'ULR-Flags' = RequestFlags} = Request,
		#client{address = CAddress, port = CPort}) ->
	SkipSub = case RequestFlags of
		[<<_:29, 1:1, _:2>>] ->
			true;
		_ ->
			false
	end,
	case ocs:find_service(IMSI) of
		{ok, #service{}} when SkipSub ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_ULA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{ok, #service{}} ->
			ApnConfig1 = #'3gpp_s6a_APN-Configuration'{'Service-Selection' = "cmnet",
					'Context-Identifier' = 1,
					'PDN-Type' =  ?'3GPP_S6A_3GPP-PDP-TYPE_IPV4',
					'EPS-Subscribed-QoS-Profile' = [#'3gpp_s6a_EPS-Subscribed-QoS-Profile'{
					'QoS-Class-Identifier' = ?'3GPP_S6A_QOS-CLASS-IDENTIFIER_QCI_9',
					'Allocation-Retention-Priority' = #'3gpp_s6a_Allocation-Retention-Priority'{
					'Priority-Level' = 15}}],
					'AMBR' = [#'3gpp_s6a_AMBR'{'Max-Requested-Bandwidth-UL' = 1000000000,
							 'Max-Requested-Bandwidth-DL' = 1000000000}]},
			ApnConfig2 = #'3gpp_s6a_APN-Configuration'{'Service-Selection' = "*",
					'Context-Identifier' = 2,
					'PDN-Type' =  ?'3GPP_S6A_3GPP-PDP-TYPE_IPV4',
					'EPS-Subscribed-QoS-Profile' = [#'3gpp_s6a_EPS-Subscribed-QoS-Profile'{
					'QoS-Class-Identifier' = ?'3GPP_S6A_QOS-CLASS-IDENTIFIER_QCI_9',
					'Allocation-Retention-Priority' = #'3gpp_s6a_Allocation-Retention-Priority'{
					'Priority-Level' = 15}}],
					'AMBR' = [#'3gpp_s6a_AMBR'{'Max-Requested-Bandwidth-UL' = 1000000000,
							 'Max-Requested-Bandwidth-DL' = 1000000000}]},
			ApnProfile = #'3gpp_s6a_APN-Configuration-Profile'{'Context-Identifier' = 1,
					'All-APN-Configurations-Included-Indicator' = 0,
					'APN-Configuration' = [ApnConfig1, ApnConfig2]},
			AMBR = #'3gpp_s6a_AMBR'{'Max-Requested-Bandwidth-UL' = 1000000000,
							 'Max-Requested-Bandwidth-DL' = 1000000000},
			SubscriptionData = #'3gpp_s6a_Subscription-Data'{
					'Subscriber-Status' = [?'3GPP_S6A_SUBSCRIBER-STATUS_SERVICE_GRANTED'],
					'APN-Configuration-Profile' = [ApnProfile], 'AMBR' = [AMBR]},
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_ULA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
					'Subscription-Data' = [SubscriptionData]},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{error, not_found} ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_AIA'{'Session-Id' = SId,
					'Experimental-Result' = [#'3gpp_s6a_Experimental-Result'{
					'Vendor-Id' = ?IANA_PEN_3GPP,
					'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'}],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{error, Reason} ->
			error_logger:error_report(["Service lookup failure",
					{service, ServiceName}, {module, ?MODULE}, {error, Reason}]),
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_ULA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer}
	end.

%% @hidden
purge_ue({_, ServerAddress, ServerPort} = ServiceName,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _Capabilities,
		#'3gpp_s6a_PUR'{'Session-Id' = SId, 'User-Name' = IMSI} = Request,
		#client{address = CAddress, port = CPort}) ->
	case ocs:find_service(IMSI) of
		{ok, #service{} = _Service} ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_PUA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
					'Auth-Session-State' = ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{error, not_found} ->
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_PUA'{'Session-Id' = SId,
					'Experimental-Result' = [#'3gpp_s6a_Experimental-Result'{
					'Vendor-Id' = ?IANA_PEN_3GPP,
					'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'}],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer};
		{error, Reason} ->
			error_logger:error_report(["Service lookup failure",
					{service, ServiceName}, {module, ?MODULE}, {error, Reason}]),
			Server = {ServerAddress, ServerPort},
			Client1 = {CAddress, CPort},
			Answer = #'3gpp_s6a_PUA'{'Session-Id' = SId,
					'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'],
					'Auth-Session-State' =  ?'3GPP_S6A_AUTH-SESSION-STATE_NO_STATE_MAINTAINED',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm},
			ok = ocs_log:auth_log(diameter, Server, Client1, Request, Answer),
			{reply, Answer}
	end.

-spec errors(ServiceName, Capabilities, Request, Errors) -> Result
	when
		ServiceName :: term(),
		Capabilities :: capabilities(),
		Request :: message(),
		Errors :: [Error],
		Error :: {Code, #diameter_avp{}} | Code,
		Code :: 0..4294967295,
		Result :: ok | {error, Error}.
%% @doc Handle errors in requests.
%% @private
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_AVP_UNSUPPORTED', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP unsupported",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_AVP_UNSUPPORTED'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_VALUE', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP invalid",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_VALUE'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_MISSING_AVP', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP missing",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_MISSING_AVP'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_CONTRADICTING_AVPS', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVPs contradicting",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_CONTRADICTING_AVPS'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_AVP_NOT_ALLOWED', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP not allowed",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_AVP_NOT_ALLOWED'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_AVP_OCCURS_TOO_MANY_TIMES', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP too many times",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_AVP_OCCURS_TOO_MANY_TIMES'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_LENGTH', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP invalid length",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{error, ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_LENGTH'};
errors(_ServiceName, _Capabilities, _Request, [{ResultCode, _} | _]) ->
	{error, ResultCode};
errors(_ServiceName, _Capabilities, _Request, [ResultCode | _]) ->
	{error, ResultCode};
errors(_ServiceName, _Capabilities, _Request, []) ->
	ok.

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
%% @doc Retrieve concealed `SQNms' from `AUTS'.
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

-spec kdf(CK, IK, SN, SQN, AK) -> MSK
	when
		CK :: binary(),
		IK :: binary(),
		SN :: binary(),
		SQN :: integer(),
		AK :: integer(),
		MSK :: binary().
%% @doc Key Derivation Function (KDF).
%%
%% 	See 3GPP TS 33.402 Annex A,
%% 	    3GPP TS 32.220 Annex B.
%% @private
kdf(CK, IK, SN, SQN, AK)
		when byte_size(CK) =:= 16,
		byte_size(IK) =:= 16,
		((byte_size(SN) =:= 3) or (byte_size(SN) =:= 2)),
		is_integer(SQN), is_integer(AK) ->
	FC = 16#10,
	P0 = SN,
	L0 = byte_size(SN),
	P1 = SQN bxor AK,
	L1 = 6,
	?HMAC(<<CK/binary, IK/binary>>, <<FC, P0/bytes, L0:16, P1:48, L1:16>>).

-spec save_dif(IMSI, DIF) -> ok
	when
		IMSI :: binary(),
		DIF :: integer().
%% @doc Save the new DIF for subscriber.
%% @hidden
save_dif(IMSI, DIF)
		when is_binary(IMSI), is_integer(DIF)->
	Now = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {Now, N},
	F = fun() ->
			[#service{password = P} = S1] = mnesia:read(service, IMSI, write),
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

