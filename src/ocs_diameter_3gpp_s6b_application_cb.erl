%%% ocs_diameter_3gpp_s6b_application_cb.erl 
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module receives {@link //diameter. diameter} messages on a port assigned
%%% 	for 3GPP DIAMETER S6b in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS TS 29.273 EPS AAA Interfaces
%%% @reference 3GPP TS TS 33.402 Security Aspects of non-3GPP Accesses
%%%
-module(ocs_diameter_3gpp_s6b_application_cb).
-copyright('Copyright (c) 2016 - 2020 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
		prepare_retransmit/3, handle_answer/4, handle_error/4,
		handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_gen_3gpp_s6b_application.hrl").
-include("ocs.hrl").

-record(state, {}).

-define(EPOCH_OFFSET, 2208988800).
-define(S6b_APPLICATION_ID, 16777272).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  #diameter_packet{}.
-type message() ::  tuple() | list().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

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

-spec pick_peer(LocalCandidates, RemoteCandidates, ServiceName, State) -> Result
	when
		LocalCandidates :: [peer()],
		RemoteCandidates :: [peer()],
		ServiceName :: diameter:service_name(),
		State :: state(),
		NewState :: state(),
		Selection :: {ok, Peer} | {Peer, NewState},
		Peer :: peer() | false,
		Result :: Selection | false.
%% @doc Invoked as a consequence of a call to diameter:call/4 to select
%% a destination peer for an outgoing request. 
pick_peer([Peer | _] = _LocalCandidates, _RemoteCandidates, _ServiceName, _State) ->
	{ok, Peer}.

-spec prepare_request(Packet, ServiceName, Peer) -> Action
	when
		Packet :: packet(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Action :: Send | Discard | {eval_packet, Action, PostF},
		Send :: {send, packet() | message()},
		Discard :: {discard, Reason} | discard,
		Reason :: term(),
		PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and transport 
prepare_request(#diameter_packet{msg = ['RAR' = T | Avps]} = _Packet,
		_ServiceName, {_, Caps} = _Peer) ->
	#diameter_caps{origin_host = {OH, DH}, origin_realm = {OR, DR}} = Caps,
	{send, [T, {'Origin-Host', OH}, {'Origin-Realm', OR},
			{'Destination-Host', DH}, {'Destination-Realm', DR} | Avps]};
prepare_request(#diameter_packet{msg = Record} = _Packet,
		_ServiceName, {_, Caps} = _Peer) ->
	#diameter_caps{origin_host = {OH, DH}, origin_realm = {OR, DR}} = Caps,
	ASR = Record#diameter_base_ASR{'Origin-Host' = OH, 'Origin-Realm' = OR,
	'Destination-Host' = DH, 'Destination-Realm' = DR},
	{send, ASR}.

-spec prepare_retransmit(Packet, ServiceName, Peer) -> Action
	when
		Packet :: packet(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Action :: Send | Discard | {eval_packet, Action, PostF},
		Send :: {send, packet() | message()},
		Discard :: {discard, Reason} | discard,
		Reason :: term(),
		PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and retransmission.
%% In case of peer connection is lost alternate peer is selected.
prepare_retransmit(Packet, ServiceName, Peer) ->
	prepare_request(Packet, ServiceName, Peer).

-spec handle_answer(Packet, Request, ServiceName, Peer) -> Result
	when
		Packet :: packet(),
		Request :: message(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Result :: term().
%% @doc Invoked when an answer message is received from a peer.
handle_answer(_Packet, _Request, _ServiceName, _Peer) ->
    not_implemented.

-spec handle_error(Reason, Request, ServiceName, Peer) -> Result
	when
		Reason :: timeout | failover | term(),
		Request :: message(),
		ServiceName :: diameter:service_name(),
		Peer :: peer(),
		Result :: term().
%% @doc Invoked when an error occurs before an answer message is received
%% in response to an outgoing request.
handle_error(_Reason, _Request, _ServiceName, _Peer) ->
	not_implemented.

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
	errors(ServiceName, Caps, Request, Errors).

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
		Reply :: {reply, packet() | message()}
			| {answer_message, 3000..3999|5000..5999}
			| {protocol_error, 3000..3999},
		Opt :: diameter:call_opt(),
		PostF :: diameter:evaluable().
%% @doc Handle received request.
%% 	Authorize client then forward capabilities and request
%% 	to the authorization port server matching the service the
%% 	request was received on.
%% @private
request(ServiceName, #diameter_caps{origin_host = {OHost, DHost},
		origin_realm = {ORealm, DRealm},
		host_ip_address = {_, ClientAddresses}} = Capabilities,
		Request) ->
	[Info] = diameter:service_info(ServiceName, transport),
	case lists:keyfind(options, 1, Info) of
		{options, Options} ->
			{ServerAddress, ServerPort} = case lists:keyfind(transport_config,
					1, Options) of
				{transport_config, TcpOpts} ->
					{ip, IP} = lists:keyfind(ip, 1, TcpOpts),
					{port, Port} = lists:keyfind(port, 1, TcpOpts),
					{IP, Port};
				{transport_config, SctpOpts, _} ->
					{ip, IP} = lists:keyfind(ip, 1, SctpOpts),
					{port, Port} = lists:keyfind(port, 1, SctpOpts),
					{IP, Port}
			end,
			request(ServiceName, Capabilities, ServerAddress, ServerPort,
					OHost, ORealm, DHost, DRealm, Request, ClientAddresses);
		false ->
			discard
	end.
%% @hidden
request(ServiceName, Capabilities, ServerAddress, ServerPort,
		OHost, ORealm, DHost, DRealm, Request, [H | T]) ->
	case ocs:find_client(H) of
		{ok, #client{protocol = diameter}} ->
			process_request(ServiceName, ServerAddress, ServerPort,
					H, OHost, ORealm, DHost, DRealm, Request);
		{error, not_found} ->
			request(ServiceName, Capabilities, ServerAddress, ServerPort,
					OHost, ORealm, DHost, DRealm, Request, T)
	end;
request(ServiceName, Capabilities, _, _, _, _, _, _, Request, []) ->
	errors(ServiceName, Capabilities, Request, [?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_PEER']).

-spec process_request(ServiceName, ServerAddress, ServerPort,
		ClientAddress, OHost, ORealm, DHost, DRealm, Request) -> Result
	when
		ServiceName :: term(),
		ServerAddress :: inet:ip_address(),
		ServerPort :: inet:ip_port(),
		ClientAddress :: inet:ip_address(),
		OHost :: diameter:'DiameterIdentity'(),
		ORealm :: diameter:'DiameterIdentity'(),
		DHost :: diameter:'DiameterIdentity'(),
		DRealm :: diameter:'DiameterIdentity'(),
		Request :: #'3gpp_s6b_AAR'{} | #'3gpp_s6b_STR'{},
		Result :: {reply, packet()} | discard.
%% @doc Process a received DIAMETER Authorization packet.
%% @private
process_request(ServiceName, ServerAddress, ServerPort, ClientAddress,
		OHost, ORealm, DHost, DRealm,
		#'3gpp_s6b_AAR'{'Session-Id' = SessionId,
				'Auth-Application-Id' = ?S6b_APPLICATION_ID} = Request) ->
	try
		process_request1(ServiceName,
				ServerAddress, ServerPort, ClientAddress,
				OHost, ORealm, DHost, DRealm, SessionId, Request)
	catch
		_:_Reason ->
			{reply, #'3gpp_s6b_AAA'{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm}}
	end;
process_request(ServiceName, ServerAddress, ServerPort, ClientAddress,
		OHost, ORealm, DHost, DRealm,
		#'3gpp_s6b_STR'{'Session-Id' = SessionId,
				'Auth-Application-Id' = ?S6b_APPLICATION_ID} = Request) ->
	try
		process_request1(ServiceName,
				ServerAddress, ServerPort, ClientAddress,
				OHost, ORealm, DHost, DRealm, SessionId, Request)
	catch
		_:_Reason ->
			{reply, #'3gpp_s6b_STA'{'Session-Id' = SessionId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					'Origin-Host' = OHost, 'Origin-Realm' = ORealm}}
	end.
%% @hidden
process_request1(ServiceName, ServerAddress, ServerPort, ClientAddress,
		OHost, ORealm, DHost, DRealm, SessionId, Request) ->
	Sup = global:whereis_name({ocs_diameter_auth, ServerAddress, ServerPort}),
	ChildSpec = [[ServiceName, ServerAddress, ServerPort, ClientAddress,
			undefined, SessionId, OHost, ORealm, DHost, DRealm], []],
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, Fsm} ->
			{reply, gen_fsm:sync_send_event(Fsm, Request)};
		{error, Reason} ->
			error_logger:error_report(["Error starting PGW session handler",
					{error, Reason}, {supervisor, Sup}, {session_id, SessionId}]),
			throw(Reason)
	end.

-spec errors(ServiceName, Capabilities, Request, Errors) -> Action
	when
		ServiceName :: term(),
		Capabilities :: capabilities(),
		Request :: message(),
		Errors :: [Error],
		Error :: {Code, #diameter_avp{}} | Code,
		Code :: 0..4294967295,
		Action :: Reply | {relay, [Opt]} | discard
			| {eval | eval_packet, Action, PostF},
		Reply :: {reply, packet() | message()}
			| {answer_message, 3000..3999|5000..5999}
			| {protocol_error, 3000..3999},
		Opt :: diameter:call_opt(),
		PostF :: diameter:evaluable().
%% @doc Handle errors in requests.
%% @private
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_AVP_UNSUPPORTED', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP unsupported",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_AVP_UNSUPPORTED'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_VALUE', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP invalid",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_VALUE'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_MISSING_AVP', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP missing",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_MISSING_AVP'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_CONTRADICTING_AVPS', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVPs contradicting",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_CONTRADICTING_AVPS'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_AVP_NOT_ALLOWED', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP not allowed",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_AVP_NOT_ALLOWED'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_AVP_OCCURS_TOO_MANY_TIMES', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP too many times",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_AVP_OCCURS_TOO_MANY_TIMES'};
errors(ServiceName, Capabilities, _Request,
		[{?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_LENGTH', _} | _] = Errors) ->
	error_logger:error_report(["DIAMETER AVP invalid length",
			{service_name, ServiceName}, {capabilities, Capabilities},
			{errors, Errors}]),
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_LENGTH'};
errors(_ServiceName, _Capabilities, _Request, [{ResultCode, _} | _]) ->
	{answer_message, ResultCode};
errors(_ServiceName, _Capabilities, _Request, [ResultCode | _]) ->
	{answer_message, ResultCode};
errors(ServiceName, Capabilities, Request, []) ->
	request(ServiceName, Capabilities, Request).

