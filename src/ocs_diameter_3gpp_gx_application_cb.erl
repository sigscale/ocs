%%% ocs_diameter_3gpp_gx_application_cb.erl 
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
%%% 	for the 3GPP DIAMETER Gx in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS 29.212 Policy and Charging Control (PCC)&#59; Reference points
%%%
-module(ocs_diameter_3gpp_gx_application_cb).
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
			prepare_retransmit/3, handle_answer/4, handle_error/4,
			handle_request/3, process_request/4]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_gen_3gpp_gx_application.hrl").
-include("diameter_gen_cc_application_rfc4006.hrl").
-include("ocs.hrl").

-record(state, {}).

-define(EPOCH_OFFSET, 2208988800).
-define(IANA_PEN_3GPP, 10415).
-define(Gx_APPLICATION_ID, 16777238).
-define(DIAMETER_ERROR_INITIAL_PARAMETERS, 5140).

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
		{_, Address, Port}, {_, Caps} = _Peer) ->
	{reply, process_request(Address, Port, Caps, Request)};
handle_request(#diameter_packet{msg = Request, errors = Errors} = _Packet,
		ServiceName, {_, Caps} = _Peer) ->
	errors(ServiceName, Caps, Request, Errors).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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
	{answer_message, ResultCode}.

-spec process_request(Address, Port, Caps, Request) -> Result
	when
		Address :: inet:ip_address(),
		Port :: inet:port_number(),
		Request :: #'3gpp_gx_CCR'{},
		Caps :: capabilities(),
		Result :: packet() | message().
%% @doc Process a received DIAMETER Accounting packet.
%% @private
process_request(Address, Port,
		#diameter_caps{origin_host = {OHost, _DHost}, origin_realm = {ORealm, _DRealm}},
		#'3gpp_gx_CCR'{'Session-Id' = SId,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum,
				'Subscription-Id' = []} = Request) ->
			Server = {Address, Port},
			Reply = diameter_error(SId, ?'DIAMETER_ERROR_INITIAL_PARAMETERS',
					OHost, ORealm, RequestType, RequestNum),
			ok = ocs_log:acct_log(diameter, Server,
					accounting_event_type(RequestType), Request, Reply, undefined),
			Reply;
process_request(Address, Port,
		#diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = DiameterCaps,
		#'3gpp_gx_CCR'{'Session-Id' = SId,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum,
				'Subscription-Id' = SubscriptionIds} = Request) ->
	{ok, Configurations} = application:get_env(ocs, diameter),
	{_, AcctServices} = lists:keyfind(acct, 1, Configurations),
	F = fun F([{{0, 0, 0, 0}, P, Options} | _]) when P =:= Port ->
			Options;
		F([{Ip, P, Options} | _]) when Ip == Address, P =:= Port ->
			Options;
		F([{_, _, _} | T]) ->
			F(T)
	end,
	ServiceOptions = F(AcctServices),
	SubIdTypes = case lists:keyfind(sub_id_type, 1, ServiceOptions) of
		{sub_id_type, Ts} when is_list(Ts) ->
			Ts;
		false ->
			undefined
	end,
	Server = {Address, Port},
	case subscriber_id(SubscriptionIds, SubIdTypes) of
			Subscriber when is_binary(Subscriber) ->
				case ocs:find_service(Subscriber) of
					{ok, #service{product = ProductRef}} ->
						process_request(Address, Port, DiameterCaps, Request,
								ocs:find_product(ProductRef));
					{error, _Reason} ->
						Reply = diameter_error(SId,
								?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply
				end;
			_ ->
				Reply = diameter_error(SId, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
	end.
%% @hidden
process_request(Address, Port, DiameterCaps, Request,
		{ok, #product{product = OfferId}}) ->
	process_request(Address, Port, DiameterCaps, Request,
			ocs:find_offer(OfferId));
process_request(Address, Port, DiameterCaps, Request,
		{ok, #offer{char_value_use = CharValue}}) ->
	process_request1(Address, Port, DiameterCaps, Request,
			lists:keyfind("policyTable", #char_value_use.name, CharValue));
process_request(Address, Port, #diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}},
		#'3gpp_gx_CCR'{'Session-Id' = SId,
            'CC-Request-Type' = RequestType,
            'CC-Request-Number' = RequestNum} = Request, {error, _Reason}) ->
	Server = {Address, Port},
	Reply = diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
			OHost, ORealm, RequestType, RequestNum),
	ok = ocs_log:acct_log(diameter, Server,
			accounting_event_type(RequestType), Request, Reply, undefined),
	Reply.
%% @hidden
process_request1(Address, Port, DiameterCaps, Request,
		#char_value_use{name = "policyTable",
		values = [#char_value{value = PolicyTable}]}) ->
	process_request2(Address, Port, DiameterCaps, Request,
			ocs:query_resource(start, '_', '_', '_', {exact, PolicyTable}));
process_request1(Address, Port, #diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _DiameterCaps,
		#'3gpp_gx_CCR'{'Session-Id' = SId,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum} = Request, false) ->
	Server = {Address, Port},
	Reply = #'3gpp_gx_CCA'{'Session-Id' = SId,
			'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?Gx_APPLICATION_ID,
			'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	ok = ocs_log:acct_log(diameter, Server,
			accounting_event_type(RequestType), Request, Reply, undefined),
	Reply.

%% @hidden
process_request2(Address, Port, #diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _DiameterCaps,
		#'3gpp_gx_CCR'{'Session-Id' = SId,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum} = Request,
		{_, [#resource{} | _] = PolicyResList}) ->
	Server = {Address, Port},
	try
		Reply = #'3gpp_gx_CCA'{'Session-Id' = SId,
				'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
				'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum,
				'Online' = [?'3GPP_GX_ONLINE_ENABLE_ONLINE'],
				'Charging-Rule-Install' = charging_rule(PolicyResList)},
		ok = ocs_log:acct_log(diameter, Server,
				accounting_event_type(RequestType), Request, Reply, undefined),
		Reply
	catch
		_:_Reason ->
			Reply1 = diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum),
			ok = ocs_log:acct_log(diameter, Server,
					accounting_event_type(RequestType), Request, Reply1, undefined),
			Reply1
	end;
process_request2(Address, Port, #diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _DiameterCaps,
		#'3gpp_gx_CCR'{'Session-Id' = SId,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum} = Request, {eof, []}) ->
	Server = {Address, Port},
	Reply = #'3gpp_gx_CCA'{'Session-Id' = SId,
			'Result-Code' = [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?Gx_APPLICATION_ID,
			'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	ok = ocs_log:acct_log(diameter, Server,
			accounting_event_type(RequestType), Request, Reply, undefined),
	Reply;
process_request2(Address, Port, #diameter_caps{origin_host = {OHost, _DHost},
		origin_realm = {ORealm, _DRealm}} = _DiameterCaps,
		#'3gpp_gx_CCR'{'Session-Id' = SId,
				'Auth-Application-Id' = ?Gx_APPLICATION_ID,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum} = Request, {error, _Reason}) ->
	Server = {Address, Port},
	Reply = diameter_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
			OHost, ORealm, RequestType, RequestNum),
	ok = ocs_log:acct_log(diameter, Server,
			accounting_event_type(RequestType), Request, Reply, undefined),
	Reply.

-spec charging_rule(PolicyResList) -> Result
	when
		PolicyResList :: [#resource{}],
		Result :: [#'3gpp_gx_Charging-Rule-Install'{}].
%% @doc Pharse a list of PCRF polcies
charging_rule(PolicyResList) ->
	charging_rule(PolicyResList, []).
%% @hidden
charging_rule([#resource{name = Name, characteristic = Chars} | T], Acc) ->
	ChargingRuleInstall = case lists:keyfind("predefined",
			#resource_char.name, Chars) of
		false ->
			ChargingRuleDefinitions = parse_policy_char(Chars,
					#'3gpp_gx_Charging-Rule-Definition'{'Charging-Rule-Name' = Name}),
			#'3gpp_gx_Charging-Rule-Install'{
					'Charging-Rule-Definition' = ChargingRuleDefinitions};
		#resource_char{name = "predefined", value = true} ->
			#'3gpp_gx_Charging-Rule-Install'{
					'Charging-Rule-Name' = [Name]}
	end,
	charging_rule(T, [ChargingRuleInstall | Acc]);
charging_rule([], Acc) ->
	Acc.

%% @hidden
parse_policy_char([#resource_char{name = "chargingKey", value = Value} | T],
		Acc) when is_integer(Value) ->
	parse_policy_char(T, Acc#'3gpp_gx_Charging-Rule-Definition'{
				'Rating-Group' = [Value]});
parse_policy_char([#resource_char{name = "precedence", value = Value} | T],
		Acc) when is_integer(Value) ->
	parse_policy_char(T, Acc#'3gpp_gx_Charging-Rule-Definition'{
				'Precedence' = [Value]});
parse_policy_char([#resource_char{name = "serviceId", value = Value} | T], Acc)
		 when is_list(Value) ->
	parse_policy_char(T, Acc#'3gpp_gx_Charging-Rule-Definition'{
				'Service-Identifier' = [Value]});
parse_policy_char([#resource_char{name = "qosInformation", value =
		#{"maxRequestedBandwidthDL" := MaxDL, "maxRequestedBandwidthUL" := MaxUL,
		"qosClassIdentifier" := QosId}} | T], Acc) when is_integer(MaxDL),
		is_integer(MaxUL), is_integer(QosId) ->
	QosInformation = #'3gpp_gx_QoS-Information'{
			'QoS-Class-Identifier' = [QosId],
			'Max-Requested-Bandwidth-DL' = [MaxDL],
			'Max-Requested-Bandwidth-UL' = [MaxUL]},
	parse_policy_char(T, Acc#'3gpp_gx_Charging-Rule-Definition'{
				'QoS-Information' = [QosInformation]});
parse_policy_char([#resource_char{name = "flowInformation",
		value = FlowInfo} | T], Acc) when is_list(FlowInfo) ->
	F = fun(#{"flowDirection" := FlowDirection,
			"flowDescription" := FlowDes}) when is_integer(FlowDirection),
			is_list(FlowDes) ->
		#'3gpp_gx_Flow-Information'{'Flow-Direction'
				= [FlowDirection], 'Flow-Description' = [FlowDes]}
	end,
	parse_policy_char(T, Acc#'3gpp_gx_Charging-Rule-Definition'{
				'Flow-Information' = lists:map(F, FlowInfo)});
parse_policy_char([_H | T], Acc) ->
	parse_policy_char(T, Acc);
parse_policy_char([], Acc) ->
	Acc.

-spec diameter_error(SessionId, ResultCode, OriginHost,
		OriginRealm, RequestType, RequestNum) -> Reply
	when
		SessionId :: string(),
		ResultCode :: integer(),
		OriginHost :: string(),
		OriginRealm :: string(),
		RequestType :: integer(),
		RequestNum :: integer(),
		Reply :: #'3gpp_gx_CCA'{}.
%% @doc Send CCA to DIAMETER client indicating an operation failure.
%% @hidden
diameter_error(SId, ?'DIAMETER_ERROR_INITIAL_PARAMETERS' = ResultCode,
		OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_gx_CCA'{'Session-Id' = SId,
			'Experimental-Result' = [#'3gpp_gx_Experimental-Result'{
					'Vendor-Id' = ?IANA_PEN_3GPP,
					'Experimental-Result-Code' = ResultCode}],
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?Gx_APPLICATION_ID,
			'CC-Request-Type' = RequestType, 'CC-Request-Number' = RequestNum};
diameter_error(SId, ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_gx_CCA'{'Session-Id' = SId, 'Result-Code' = [ResultCode],
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?Gx_APPLICATION_ID,
			'CC-Request-Type' = RequestType, 'CC-Request-Number' = RequestNum}.

-spec subscriber_id(SubscriberIdAVPs, SubIdTypes) -> Subscriber
	when
		SubscriberIdAVPs :: [#'3gpp_gx_Subscription-Id'{}],
		SubIdTypes :: [SubIdType] | undefined,
		SubIdType :: imsi | msisdn | nai | sip | private,
		Subscriber :: binary() | [].
%% @doc Get Subscribers From Diameter SubscriberId AVP.
subscriber_id(SubscriberIdAVPs, undefined = _SubIdTypes) ->
	subscriber_id(SubscriberIdAVPs, [msisdn]);
subscriber_id(SubscriberIdAVPs, [H | T])
		when is_atom(H) ->
	IdType = id_type(H),
	case lists:keyfind(IdType, #'3gpp_gx_Subscription-Id'.'Subscription-Id-Type',
			SubscriberIdAVPs) of
		#'3gpp_gx_Subscription-Id'{'Subscription-Id-Data' = SubId} ->
			SubId;
		_  ->
			subscriber_id(SubscriberIdAVPs, T)
	end;
subscriber_id(_, []) ->
	[].

%% @hidden
id_type(imsi) ->
	?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI';
id_type(msisdn) ->
	?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164';
id_type(nai) ->
	?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_NAI';
id_type(sip) ->
	?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_SIP_URI';
id_type(private) ->
	?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_PRIVATE';
id_type(_) ->
	[].

-spec accounting_event_type(RequestType) -> EventType
	when
	RequestType :: 1..4,
	EventType :: start | interim | stop | event.
%% @doc Converts CC-Request-Type integer value to a readable atom.
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST') -> start;
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST') -> interim;
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST') -> stop;
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST') -> event.

