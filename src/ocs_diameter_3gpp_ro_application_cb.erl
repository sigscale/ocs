%%% ocs_diameter_3gpp_ro_application_cb.erl 
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
%%% 	for the 3GPP DIAMETER Ro in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS 29.299 Diameter charging applications
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%% 	RFC4006 - DIAMETER Credit-Control Application </a>
%%%
-module(ocs_diameter_3gpp_ro_application_cb).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
			prepare_retransmit/3, handle_answer/4, handle_error/4,
			handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_cc_application_rfc4006.hrl").
-include("ocs.hrl").

-record(state, {}).

-define(EPOCH_OFFSET, 2208988800).
-define(RO_APPLICATION_ID, 4).

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
prepare_request(#diameter_packet{} = Packet, _ServiceName, _Peer) ->
	{send, Packet}.

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

-spec request(Svc, Capabilities, Request) -> Action
	when
		Svc :: atom(),
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
%% 	to the accounting port server matching the service the
%% 	request was received on.
%% @private
request(ServiceName, Capabilities, Request) ->
	#diameter_caps{host_ip_address = {_, HostIpAddresses}} = Capabilities,
	request(ServiceName, Capabilities, Request, HostIpAddresses).
%% @hidden
request({_, IpAddress, Port} = ServiceName, Capabilities, Request, [H | T]) ->
	case ocs:find_client(H) of
		{ok, #client{protocol = diameter}} ->
			{reply, process_request(IpAddress, Port, Capabilities, Request)};
		{error, not_found} ->
			request(ServiceName, Capabilities, Request, T)
	end;
request(_, _, _, []) ->
	{answer_message, ?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_PEER'}.

-spec errors(ServiceName, Capabilities, Request, Errors) -> Action
	when
		ServiceName :: atom(),
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

-spec process_request(IpAddress, Port, Caps, Request) -> Result
	when
		IpAddress :: inet:ip_address(),
		Port :: inet:port_number(),
		Request :: #'3gpp_ro_CCR'{},
		Caps :: capabilities(),
		Result :: packet() | message().
%% @doc Process a received DIAMETER Accounting packet.
%% @private
process_request(IpAddress, Port,
		#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}},
		#'3gpp_ro_CCR'{'Session-Id' = SId, 'User-Name' = NAISpecUName,
				'Auth-Application-Id' = ?RO_APPLICATION_ID,
				'Service-Context-Id' = _SvcContextId,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum,
				'Subscription-Id' = SubscriptionIds} = Request) ->
	try
		Subscriber = case SubscriptionIds of
			[#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
				Sub;
			[] ->
				case NAISpecUName of
					[] ->
						throw(no_subscriber_identification_information);
					NAI ->
						[_, Username | _] = string:tokens(NAI, ":@"),%% proto:username@realm
						Username
				end
		end,
		process_request1(RequestType, Request, SId, RequestNum,
				Subscriber, OHost, DHost, ORealm, DRealm, IpAddress, Port)
	catch
		_:Reason ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason}]),
			diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end.
%% @hidden
process_request1(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SId, RequestNum, Subscriber,
		OHost, _DHost, ORealm, _DRealm, IpAddress, Port) ->
	try
		{ServiceIdentifier, RatingGroup, ReserveAmount} = case MSCC of
			[#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Time'
					= [CCTime]}]} | _] when is_integer(CCTime) ->
				{SI, RG, [{seconds, CCTime}]};
			[#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Total-Octets'
					= [CCTotalOctets]}]} | _] when is_integer(CCTotalOctets) ->
				{SI, RG, [{octets, CCTotalOctets}]};
			[#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Output-Octets'
					= [CCOutputOctets], 'CC-Input-Octets' = [CCInputOctets]}]} | _]
					when is_integer(CCInputOctets), is_integer(CCOutputOctets) ->
				{SI, RG, [{octets, CCInputOctets + CCOutputOctets}]};
			[#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units'
					= [CCSpecUnits]}]} | _] when is_integer(CCSpecUnits) ->
				{SI, RG, [{messages, CCSpecUnits}]};
			[#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG} | _] ->
				{SI, RG, []};
			[] ->
				{[], [], []}
		end,
		{Direction, Address} = direction_address(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		ChargingKey = case RatingGroup of
			[CK] ->
				CK;
			[] ->
				undefined 
		end,
		ServiceNetwork = service_network(ServiceInformation),
		Server = {IpAddress, Port},
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		case ocs_rating:rate(diameter, ServiceType, ChargingKey, ServiceNetwork,
				Subscriber, Timestamp, Address, Direction, initial, [],
				ReserveAmount, [{'Session-Id', SId}]) of
			{ok, _, {seconds, Amount} = _GrantedAmount} ->
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [Amount]},
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup, GSU,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{ok, _, {octets, Amount} = _GrantedAmount} ->
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Amount]},
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup, GSU,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{ok, _, {messages, Amount} = _GrantedAmount} ->
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [Amount]},
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup, GSU,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{out_of_credit, _SessionList} ->
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup,
						undefined, ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{disabled, _SessionList} ->
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup,
						undefined, ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED', OHost,
						ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, subscriber_not_found} ->
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, initial}, {subscriber, Subscriber},
						{address, Address}, {direction, Direction},
						{reservation, ReserveAmount}]),
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}]),
			diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [MSCC | _],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SId, RequestNum, Subscriber,
		OHost, _DHost, ORealm, _DRealm, IpAddress, Port) ->
	try
		{ServiceIdentifier, RatingGroup, ReserveAmount} = case MSCC of
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Time'
					= [CCTime]}]} when is_integer(CCTime) ->
				{SI, RG, [{seconds, CCTime}]};
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Total-Octets'
					= [CCTotalOctets]}]} when is_integer(CCTotalOctets) ->
				{SI, RG, [{octets, CCTotalOctets}]};
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Output-Octets'
					= [CCOutputOctets], 'CC-Input-Octets' = [CCInputOctets]}]}
					when is_integer(CCInputOctets), is_integer(CCOutputOctets) ->
				{SI, RG, [{octets, CCInputOctets + CCOutputOctets}]};
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG, 'Requested-Service-Unit'
					= [#'3gpp_ro_Requested-Service-Unit'{'CC-Service-Specific-Units'
					= [CCSpecUnits]}]} when is_integer(CCSpecUnits) ->
				{SI, RG, [{messages, CCSpecUnits}]};
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = SI,
					'Rating-Group' = RG} ->
				{SI, RG, []}
		end,
		DebitAmount = case MSCC of
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Time'
					= [UsedCCTime]}]} when is_integer(UsedCCTime) ->
				[{seconds, UsedCCTime}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets'
					= [UsedCCTotalOctets]}]} when is_integer(UsedCCTotalOctets) ->
				[{octets, UsedCCTotalOctets}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Output-Octets'
					= [UsedCCOutputOctets], 'CC-Input-Octets'
					= [UsedCCInputOctets]}]} when is_integer(UsedCCInputOctets),
					is_integer(UsedCCOutputOctets) ->
				[{octets, UsedCCInputOctets + UsedCCOutputOctets}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units'
					= [UsedCCSpecUnits]}]} when is_integer(UsedCCSpecUnits) ->
				[{messages, UsedCCSpecUnits}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{}]} ->
				throw(unsupported_used_units);
			#'3gpp_ro_Multiple-Services-Credit-Control'{} ->
				[]
		end,
		{Direction, Address} = direction_address(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		ChargingKey = case RatingGroup of
			[CK] ->
				CK;
			[] ->
				undefined 
		end,
		ServiceNetwork = service_network(ServiceInformation),
		Server = {IpAddress, Port},
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		case ocs_rating:rate(diameter, ServiceType, ChargingKey, ServiceNetwork,
				Subscriber, Timestamp, Address, Direction, interim,
				DebitAmount, ReserveAmount, [{'Session-Id', SId}]) of
			{ok, _, {seconds, Amount} = _GrantedAmount} ->
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [Amount]},
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup, GSU,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{ok, _, {octets, Amount} = _GrantedAmount} ->
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Amount]},
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup, GSU,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{ok, _, {messages, Amount} = _GrantedAmount} ->
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [Amount]},
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup, GSU,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{out_of_credit, _SessionList} ->
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup,
						undefined, ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED', OHost,
						ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{disabled, _SessionList} ->
				Reply = diameter_answer(SId, ServiceIdentifier, RatingGroup,
						undefined, ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED', OHost,
						ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, subscriber_not_found} ->
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, interim}, {subscriber, Subscriber},
						{address, Address}, {direction, Direction},
						{reservation, ReserveAmount}, {used, DebitAmount}]),
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}]),
			diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [MSCC | _],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SId, RequestNum, Subscriber,
		OHost, _DHost, ORealm, _DRealm, IpAddress, Port) ->
	try
		DebitAmount = case MSCC of
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Time'
					= [UsedCCTime]}]} when is_integer(UsedCCTime) ->
				[{seconds, UsedCCTime}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets'
					= [UsedCCTotalOctets]}]} when is_integer(UsedCCTotalOctets) ->
				[{octets, UsedCCTotalOctets}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Output-Octets'
					= [UsedCCOutputOctets], 'CC-Input-Octets'
					= [UsedCCInputOctets]}]} when is_integer(UsedCCInputOctets),
					is_integer(UsedCCOutputOctets) ->
				[{octets, UsedCCInputOctets + UsedCCOutputOctets}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units'
					= [UsedCCSpecUnits]}]} when is_integer(UsedCCSpecUnits) ->
				[{messages, UsedCCSpecUnits}];
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit'
					= [#'3gpp_ro_Used-Service-Unit'{}]} ->
				throw(unsupported_used_units);
			#'3gpp_ro_Multiple-Services-Credit-Control'{} ->
				[]
		end,
		{Direction, Address} = direction_address(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		ServiceNetwork = service_network(ServiceInformation),
		Server = {IpAddress, Port},
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		case ocs_rating:rate(diameter, ServiceType, undefined, ServiceNetwork,
				Subscriber, Timestamp, Address, Direction, final, DebitAmount,
				[], [{'Session-Id', SId}]) of
			{ok, _, Rated} when is_list(Rated) ->
				Reply = diameter_answer(SId, [], [], undefined,
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{out_of_credit, _SessionList, Rated} ->
				Reply = diameter_answer(SId, [], [], undefined,
						?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{disabled, _SessionList} ->
				Reply = diameter_answer(SId, [], [], undefined,
						?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, subscriber_not_found} ->
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, final}, {subscriber, Subscriber},
						{address, Address}, {direction, Direction},
						{used, DebitAmount}]),
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}]),
			diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = []} = Request,
		SId, RequestNum, _Subscriber, OHost, _DHost,
		ORealm, _DRealm, IpAddress, Port) ->
	Reply = diameter_answer(SId, [], [], undefined,
			?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			OHost, ORealm, RequestType, RequestNum),
	ok = ocs_log:acct_log(diameter, {IpAddress, Port},
			accounting_event_type(RequestType), Request, Reply, undefined),
	Reply.

-spec diameter_answer(SessionId, ServiceIdentifier, RatingGroup, GrantedUnits,
		ResultCode, OriginHost, OriginRealm, RequestType, RequestNum) -> Result
			when
				SessionId :: string(),
				ServiceIdentifier :: [non_neg_integer()],
				RatingGroup :: [non_neg_integer()],
				GrantedUnits :: undefined | #'3gpp_ro_Granted-Service-Unit'{},
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				RequestType :: integer(),
				RequestNum :: integer(),
				Result :: #'3gpp_ro_CCA'{}.
%% @doc Build CCA response.
%% @hidden
diameter_answer(SId, _ServiceIdentifier, _RatingGroup, undefined,
		?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED', OHost, ORealm,
		RequestType, RequestNum) ->
	#'3gpp_ro_CCA'{'Session-Id' = SId,
			'Result-Code' = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum};
diameter_answer(SId, _ServiceIdentifier, _RatingGroup, undefined,
		ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_ro_CCA'{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum};
diameter_answer(SId, ServiceIdentifier, RatingGroup, GrantedUnits,
		ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Service-Identifier' = ServiceIdentifier,
			'Result-Code' = [ResultCode],
			'Rating-Group' = RatingGroup,
			'Granted-Service-Unit' = [GrantedUnits]},
	#'3gpp_ro_CCA'{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]}.

-spec diameter_error(SessionId, ResultCode, OriginHost,
		OriginRealm, RequestType, RequestNum) -> Reply
	when
		SessionId :: string(),
		ResultCode :: integer(),
		OriginHost :: string(),
		OriginRealm :: string(),
		RequestType :: integer(),
		RequestNum :: integer(),
		Reply :: #'3gpp_ro_CCA'{}.
%% @doc Send CCA to DIAMETER client indicating an operation failure.
%% @hidden
diameter_error(SId, ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_ro_CCA'{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum}.

-spec accounting_event_type(RequestType) -> EventType
	when
	RequestType :: 1..4,
	EventType :: start | interim | stop | event.
%% @doc Converts CC-Request-Type integer value to a readable atom.
accounting_event_type(1) -> start;
accounting_event_type(2) -> interim;
accounting_event_type(3) -> stop.

%% @hidden
direction_address([#'3gpp_ro_Service-Information'{
		'SMS-Information' = [#'3gpp_ro_SMS-Information'{
		'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
		'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
		'Address-Data' = [RecipientAddress]}]}]}]}]) ->
	% @todo handle multiple SMS recipients
	{originate, RecipientAddress};
direction_address([#'3gpp_ro_Service-Information'{
		'IMS-Information' = [#'3gpp_ro_IMS-Information'{
		'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_ORIGINATING_ROLE'],
		'Called-Party-Address' = [CalledParty]}]}]) ->
	{originate, destination(CalledParty)};
direction_address([#'3gpp_ro_Service-Information'{
		'IMS-Information' = [#'3gpp_ro_IMS-Information'{
		'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_TERMINATING_ROLE'],
		'Calling-Party-Address' = [CallingParty]}]}]) ->
	{answer, destination(CallingParty)};
direction_address(_) ->
	{undefined, undefined}.

%% @hidden
destination(<<"tel:", Dest/binary>>) ->
	binary_to_list(Dest);
destination(Dest) ->
	binary_to_list(Dest).

%% @hidden
service_type(Id) ->
	% allow ".3gpp.org" or the proper "@3gpp.org"
	case binary:part(Id, size(Id), -8) of
		<<"3gpp.org">> ->
			ServiceContext = binary:part(Id, byte_size(Id) - 14, 5),
			case catch binary_to_integer(ServiceContext) of
				{'EXIT', _} ->
					undefined;
				SeviceType ->
					SeviceType
			end;
		_ ->
			undefined
	end.

%% @hidden
service_network([#'3gpp_ro_Service-Information'{
		'PS-Information' = [#'3gpp_ro_PS-Information'{
		'3GPP-SGSN-MCC-MNC' = [MccMnc]}]}]) ->
	MccMnc;
service_network(_) ->
	undefined.

