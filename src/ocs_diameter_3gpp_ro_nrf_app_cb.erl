%%% ocs_diameter_3gpp_ro_nrf_app_cb.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2024 SigScale Global Inc.
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
%%% 	module receives {@link //diameter. diameter} messages for the
%%% 	3GPP Ro application and interworks with a rating function (RF)
%%% 	over the 3GPP Re interface using the Nrf Open API.
%%%
%%% @reference 3GPP TS 29.299 Diameter charging applications
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%% 	RFC4006 - DIAMETER Credit-Control Application </a>
%%% @reference <a href="https://app.swaggerhub.com/apis/SigScale/nrf-rating">
%%% 	Nrf_Rating</a>
%%%
-module(ocs_diameter_3gpp_ro_nrf_app_cb).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

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
-include("ocs_log.hrl").

-record(state, {}).

-define(EPOCH_OFFSET, 2208988800).
-define(RO_APPLICATION_ID, 4).
-define(BASE_URI, "/ratingdata").
-define(NRF_TABLE, nrf_session).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  #diameter_packet{}.
-type message() ::  tuple() | list().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

-ifdef(OTP_RELEASE). % >= 21
	-define(CATCH_STACK, _:Reason1:ST).
	-define(SET_STACK, StackTrace = ST).
-else.
	-define(CATCH_STACK, _:Reason1).
	-define(SET_STACK, StackTrace = erlang:get_stacktrace()).
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
			Class = get_option({IpAddress, Port}, rf_class),
			{reply, process_request(IpAddress, Port, Capabilities, Request, Class)};
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

-spec process_request(IpAddress, Port, Caps, Request, Class) -> Result
	when
		IpAddress :: inet:ip_address(),
		Port :: inet:port_number(),
		Request :: #'3gpp_ro_CCR'{},
		Caps :: capabilities(),
		Class :: a | b | undefined,
		Result :: packet() | message().
%% @doc Process a received DIAMETER Accounting packet.
%% @private
process_request(IpAddress, Port,
		#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}},
		#'3gpp_ro_CCR'{'Session-Id' = SessionId, 'User-Name' = UserName,
				'Auth-Application-Id' = ?RO_APPLICATION_ID,
				'Service-Context-Id' = _SvcContextId,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum,
				'Subscription-Id' = SubscriptionIds} = Request, Class) ->
	try
		{ok, Configurations} = application:get_env(ocs, diameter),
		{_, AcctServices} = lists:keyfind(acct, 1, Configurations),
		F = fun F([{{0, 0, 0, 0}, P, Options} | _]) when P =:= Port ->
				Options;
			F([{Ip, P, Options} | _]) when Ip == IpAddress, P =:= Port ->
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
		SubscriberIDs = subscriber_id(SubscriptionIds, UserName, SubIdTypes),
		process_request1(RequestType, Request, SessionId, RequestNum,
				SubscriberIDs, OHost, DHost, ORealm, DRealm, IpAddress, Port, Class)
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end.
%% @hidden
process_request1(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId, RequestNum,
		{SubscriberIDs, SubscriptionId}, OHost, _, ORealm,
		_, IpAddress, Port, b = _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		Server = {IpAddress, Port},
		{Direction, Address} = direction_address(ServiceInformation),
		NrfResponse = case service_type(SvcContextId) of
			32251 ->
				post_request_scur(Server, SubscriptionId, SvcContextId,
						SessionId, MSCC1, [], Location, {initial, b});
			Id when Id == 32260; Id == 32274 ->
				post_request_ecur(Server, SubscriptionId, SvcContextId,
						SessionId, MSCC1, [], Location, Destination, {initial, b})
		end,
		case NrfResponse of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC1),
				NewMSCC = build_mscc(ServiceRating, Container),
				Reply = diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, get_mscc(MSCC1)}]),
				Reply = diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId, RequestNum,
		{SubscriberIDs, SubscriptionId}, OHost, _, ORealm, _, IpAddress, Port, _Class) ->
	try
		{Direction, Address} = direction_address(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		ServiceNetwork = service_network(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		Server = {IpAddress, Port},
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		Amounts = get_mscc(MSCC1),
		RfResponse = case rate(ServiceType, ServiceNetwork, SubscriberIDs, Timestamp,
				Address, Direction, initial, SessionId, Amounts) of
			{{MSCC2, ResultCode, undefined}, [], []} ->
				{ok, MSCC2, ResultCode, undefined};
			{{MSCC2, _, undefined}, PLA, Amounts1} when is_list(MSCC2), length(PLA) > 0 ->
				case ServiceType of
					32251 ->
						{ok, JSON} = post_request_scur(Server, SubscriptionId, SvcContextId,
								SessionId, PLA, Amounts1, [], {initial, a}),
						{ok, JSON, PLA, Amounts1, MSCC2};
					Id when Id == 32260; Id == 32274 ->
						{ok, JSON} = post_request_ecur(Server, SubscriptionId, SvcContextId,
								SessionId, PLA, Amounts1, [], Destination, {initial, a}),
						{ok, JSON, PLA, Amounts1, MSCC2}
				end;
			{error, Reason} ->
				{error, Reason}
		end,
		case RfResponse of
			{ok, JSON1, PLA1, Amounts2, MSCC3} ->
				{struct, RatedStruct} = mochijson:decode(JSON1),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, _} = map_service_rating(ServiceElements, SessionId),
				case charge(SubscriberIDs, SessionId, initial, match_tariff(ServiceRating, Amounts2)) of
					{ok, NewMSCC1, ResultCode1} ->
						Container = build_container(MSCC1),
						NewMSCC3 = build_mscc(NewMSCC1 ++ MSCC3, Container),
						ok = insert_ref(SessionId, ServiceRating, PLA1),
						Reply = diameter_answer(SessionId, NewMSCC3, ResultCode1,
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply;
					{error, Reason2} ->
						error_logger:error_report(["Rating Error",
								{module, ?MODULE}, {error, Reason2},
								{origin_host, OHost}, {origin_realm, ORealm},
								{type, accounting_event_type(RequestType)},
								{subscriber, print_sub(SubscriberIDs)},
								{address, Address}, {direction, Direction},
								{amounts, Amounts}]),
						Reply = diameter_error(SessionId,
								?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
							accounting_event_type(RequestType), Request, Reply, undefined),
						Reply
				end;
			{ok, MSCC3, ResultCode1, undefined} ->
				Container = build_container(MSCC1),
				NewMSCC3 = build_mscc(MSCC3, Container),
				Reply = diameter_answer(SessionId, NewMSCC3, ResultCode1,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason2} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason2},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, Amounts}]),
				Reply = diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, {SubscriberIDs, SubscriptionId}, OHost, _DHost,
		ORealm, _DRealm, IpAddress, Port, b = _Class) when length(MSCC1) > 0 ->
	try
		Server = {IpAddress, Port},
		Location = get_service_location(ServiceInformation),
		{Direction, Address} = direction_address(ServiceInformation),
		Amounts = get_mscc(MSCC1),
		case post_request_scur(Server, SubscriptionId, SvcContextId,
				SessionId, MSCC1, [], Location, interim) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC1),
				NewMSCC = build_mscc(ServiceRating, Container),
				Reply = diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, Amounts}]),
				Reply = diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, {SubscriberIDs, SubscriptionId}, OHost, _DHost,
		ORealm, _DRealm, IpAddress, Port, _Class) when length(MSCC1) > 0 ->
	try
		Server = {IpAddress, Port},
		{Direction, Address} = direction_address(ServiceInformation),
		ServiceNetwork = service_network(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		Destination = get_destination(ServiceInformation),
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		Amounts = get_mscc(MSCC1),
		Price = case rate(ServiceType, ServiceNetwork, SubscriberIDs, Timestamp,
				Address, Direction, interim, SessionId, Amounts) of
			{{MSCC2, ResultCode, undefined}, [], []} ->
				{ok, MSCC2, ResultCode};
			{{MSCC2, _ResultCode, undefined}, PLA, Amounts1} ->
				case match_tariff(get_ref(SessionId), Amounts1) of
					Prices when is_list(Prices) ->
						{ok, PLA, Prices, MSCC2};
					{error, missing_tariff} ->
						{ok, JSON} = case ServiceType of
							32251 ->
								post_request_scur(Server, SubscriptionId, SvcContextId,
										SessionId, PLA, Amounts1, [], {initial, a});
							Id when Id == 32260; Id == 32274 ->
								post_request_ecur(Server, SubscriptionId, SvcContextId,
										SessionId, PLA, Amounts1, [], Destination, {initial, a})
						end,
						{struct, RatedStruct} = mochijson:decode(JSON),
						{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
						{ServiceRating, _} = map_service_rating(ServiceElements, SessionId),
						ok = insert_ref(SessionId, ServiceRating, PLA),
						{ok, PLA, match_tariff(ServiceRating, Amounts1), MSCC2}
				end;
			{error, Reason} ->
				{error, Reason}
		end,
		case Price of
			{ok, _PLA1, Prices1, MSCC3} ->
				case charge(SubscriberIDs, SessionId, interim, Prices1) of
					{ok, NewMSCC1, ResultCode1} ->
						Container = build_container(MSCC1),
						NewMSCC3 = build_mscc(NewMSCC1 ++ MSCC3, Container),
						Reply = diameter_answer(SessionId, NewMSCC3, ResultCode1,
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply;
					{error, Reason2} ->
						error_logger:error_report(["Rating Error",
								{module, ?MODULE}, {error, Reason2},
								{origin_host, OHost}, {origin_realm, ORealm},
								{type, accounting_event_type(RequestType)},
								{subscriber, print_sub(SubscriberIDs)},
								{address, Address}, {direction, Direction},
								{amounts, Amounts}]),
						Reply = diameter_error(SessionId,
								?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply
				end;
			{ok, MSCC3, ResultCode1} ->
				Container = build_container(MSCC1),
				NewMSCC3 = build_mscc(MSCC3, Container),
				Reply = diameter_answer(SessionId, NewMSCC3, ResultCode1,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason2} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason2},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, Amounts}]),
				Reply = diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, {SubscriberIDs, SubscriptionId}, OHost, _DHost,
		ORealm, _DRealm, IpAddress, Port, b = _Class) ->
	try
		Server = {IpAddress, Port},
		{Direction, Address} = direction_address(ServiceInformation),
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		NrfResponse = case service_type(SvcContextId) of
			32251 ->
				post_request_scur(Server, SubscriptionId, SvcContextId,
						SessionId, MSCC1, [], Location, final);
			Id when Id == 32260; Id == 32274 ->
				post_request_ecur(Server, SubscriptionId, SvcContextId,
						SessionId, MSCC1, [], Location, Destination, final)
		end,
		Amounts = get_mscc(MSCC1),
		case NrfResponse of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC1),
				NewMSCC = build_mscc(ServiceRating, Container),
				ok = remove_ref(SessionId),
				Reply = diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason2} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason2},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, Amounts}]),
				ok = remove_ref(SessionId),
				Reply = diameter_error(SessionId, Reason2,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, {SubscriberIDs, SubscriptionId}, OHost, _DHost,
		ORealm, _DRealm, IpAddress, Port, _Class) ->
	try
		Server = {IpAddress, Port},
		{Direction, Address} = direction_address(ServiceInformation),
		ServiceNetwork = service_network(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		Amounts = get_mscc(MSCC1),
		RfResponse = case rate(ServiceType, ServiceNetwork, SubscriberIDs, Timestamp,
				Address, Direction, final, SessionId, Amounts) of
			{{MSCC2, ResultCode, Rated}, [], []} ->
				{ok, MSCC2, ResultCode, Rated};
			{{MSCC2, _, _}, PLA, Amounts1} ->
				case match_tariff(get_ref(SessionId), Amounts1) of
					Prices when is_list(Prices) ->
						{ok, PLA, Prices, MSCC2};
					{error, missing_tariff} ->
						{ok, JSON} = case ServiceType of
							32251 ->
								post_request_scur(Server, SubscriptionId, SvcContextId,
										SessionId, PLA, Amounts1, [], {initial, a});
							Id when Id == 32260; Id == 32274 ->
								post_request_ecur(Server, SubscriptionId, SvcContextId,
										SessionId, PLA, Amounts1, [], Destination, {initial, a})
						end,
						{struct, RatedStruct} = mochijson:decode(JSON),
						{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
						{ServiceRating, _} = map_service_rating(ServiceElements, SessionId),
						ok = insert_ref(SessionId, ServiceRating, PLA),
						{ok, PLA, match_tariff(ServiceRating, Amounts1), MSCC2}
				end;
			{error, Reason} ->
				{error, Reason}
		end,
		case RfResponse of
			{ok, MSCC3, ResultCode1, [#rated{}| _] = Rated1} ->
				Container = build_container(MSCC1),
				NewMSCC = build_mscc(MSCC3, Container),
				Reply = diameter_answer(SessionId, NewMSCC, ResultCode1,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated1),
				Reply;
			{ok, _PLA1, Prices1, MSCC3} ->
				case charge(SubscriberIDs, SessionId, final, Prices1) of
					{ok, NewMSCC1, ResultCode1} ->
						Container = build_container(MSCC1),
						NewMSCC3 = build_mscc(NewMSCC1 ++ MSCC3, Container),
						ok = remove_ref(SessionId),
						Reply = diameter_answer(SessionId, NewMSCC3, ResultCode1,
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply;
					{error, Reason2} ->
						error_logger:error_report(["Rating Error",
								{module, ?MODULE}, {error, Reason2},
								{origin_host, OHost}, {origin_realm, ORealm},
								{type, accounting_event_type(RequestType)},
								{subscriber, print_sub(SubscriberIDs)},
								{address, Address}, {direction, Direction},
								{amounts, Amounts}]),
						Reply = diameter_error(SessionId,
								?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply
				end;
			{error, Reason2} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason2},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, Amounts}]),
				Reply = diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC,
		'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, {SubscriberIDs, SubscriptionId}, OHost, _DHost,
		ORealm, _DRealm, IpAddress, Port, b = _Class) ->
	try
		Server = {IpAddress, Port},
		{Direction, Address} = direction_address(ServiceInformation),
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case post_request_iec(Server, SubscriptionId, SvcContextId,
				SessionId, MSCC, Location, Destination) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				case lists:keyfind("serviceRating", 1, RatedStruct) of
					{_, {_, ServiceElements}} ->
						{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
						Container = build_container(MSCC),
						NewMSCC = build_mscc(ServiceRating, Container),
						ok = remove_ref(SessionId),
						Reply = diameter_answer(SessionId, NewMSCC, ResultCode,
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply;
					_ ->
						{_, ResultCode} = map_service_rating([], SessionId),
						Reply = diameter_answer(SessionId, [], ResultCode,
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply
				end;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, print_sub(SubscriberIDs)},
						{address, Address}, {direction, Direction},
						{amounts, []}]),
				ok = remove_ref(SessionId),
				Reply = diameter_error(SessionId, Reason,
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply
		end
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, StackTrace}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end.

-spec subscriber_id(SubscriptionIDs, UserName, SubIdTypes) -> Result
	when
		SubscriptionIDs :: [#'3gpp_ro_Subscription-Id'{}],
		UserName :: [binary()],
		SubIdTypes :: [SubIdType] | undefined,
		SubIdType :: imsi | msisdn | nai | sip | private,
		Result :: {SubscriberIDs, SubscriptionIds},
		SubscriberIDs :: [binary()],
		SubscriptionIds :: [string()].
%% @doc Get filtered subscriber IDs in priority order.
subscriber_id(SubscriptionIDs, _UserName, undefined = _SubIdTypes)
		when length(SubscriptionIDs) > 0 ->
	subscriber_id(SubscriptionIDs, [msisdn, imsi], [], []);
subscriber_id(SubscriptionIDs, _UserName, SubIdTypes)
		when length(SubscriptionIDs) > 0 ->
	subscriber_id(SubscriptionIDs, SubIdTypes, [], []);
subscriber_id([], [] = _UserName, _SubIdTypes) ->
	throw(no_subscriber_identification_information);
subscriber_id([], [NAI] = _UserName, _SubIdTypes) ->
	case binary:split(NAI, [<<":">>, <<"@">>], [global]) of
		[<<"sip">>, User, _Domain] ->
			{[User], "sip-" ++ binary_to_list(NAI)};
		[_Proto, User, _Domain] ->
			{[User], "nai-" ++ binary_to_list(NAI)};
		[User, _Domain] ->
			{[User], "nai-" ++ binary_to_list(NAI)};
		[User] ->
			{[User], "nai-" ++ binary_to_list(NAI)};
		_ ->
			throw(no_subscriber_identification_information)
	end.
%% @hidden
subscriber_id(SubscriptionIDs, [imsi | T], Acc1, Acc2) ->
	case lists:keyfind(?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			#'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
			SubscriptionIDs) of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = IMSI} ->
			subscriber_id(SubscriptionIDs, T, [IMSI | Acc1],
					["imsi-" ++ binary_to_list(IMSI) | Acc2]);
		_  ->
			subscriber_id(SubscriptionIDs, T, Acc1, Acc2)
	end;
subscriber_id(SubscriptionIDs, [msisdn | T], Acc1, Acc2) ->
	case lists:keyfind(?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			#'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
			SubscriptionIDs) of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = MSISDN} ->
			subscriber_id(SubscriptionIDs, T, [MSISDN | Acc1],
					["msisdn-" ++ binary_to_list(MSISDN) | Acc2]);
		_  ->
			subscriber_id(SubscriptionIDs, T, Acc1, Acc2)
	end;
subscriber_id(SubscriptionIDs, [nai | T], Acc1, Acc2) ->
	case lists:keyfind(?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_NAI',
			#'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
			SubscriptionIDs) of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = NAI} ->
			subscriber_id(SubscriptionIDs, T, [NAI | Acc1],
					["nai-" ++ binary_to_list(NAI) | Acc2]);
		_  ->
			subscriber_id(SubscriptionIDs, T, Acc1, Acc2)
	end;
subscriber_id(SubscriptionIDs, [sip | T], Acc1, Acc2) ->
	case lists:keyfind(?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_SIP_URI',
			#'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
			SubscriptionIDs) of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = SIP} ->
			subscriber_id(SubscriptionIDs, T, [SIP | Acc1],
					["sip-" ++ binary_to_list(SIP) | Acc2]);
		_  ->
			subscriber_id(SubscriptionIDs, T, Acc1, Acc2)
	end;
subscriber_id(SubscriptionIDs, [private | T], Acc1, Acc2) ->
	case lists:keyfind(?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_PRIVATE',
			#'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
			SubscriptionIDs) of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Private} ->
			subscriber_id(SubscriptionIDs, T, [Private | Acc1],
					["private-" ++ binary_to_list(Private) | Acc2]);
		_  ->
			subscriber_id(SubscriptionIDs, T, Acc1, Acc2)
	end;
subscriber_id(_SubscriptionIDs, [], Acc1, Acc2) ->
	{lists:reverse(Acc1), lists:reverse(Acc2)}.

-spec post_request_ecur(ServiceName, SubscriptionId,
		ServiceContextId, SessionId, ServiceRatingData,
		Amounts, Location, Destination, Flag) -> Result
	when
		ServiceName :: {IpAddress, Port},
		IpAddress :: inet:ip_address(),
		Port :: inet:port_number(),
		SubscriptionId :: [string()],
		ServiceContextId :: binary(),
		SessionId :: binary(),
		ServiceRatingData :: [MSCC] | [PLA],
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		PLA :: map(),
		Amounts :: [{ServiceIdentifier, RatingGroup,
				UsedAmounts, ReserveAmounts}] | [],
		ServiceIdentifier :: [pos_integer()],
		RatingGroup :: [pos_integer()],
		UsedAmounts :: [{Units, pos_integer()}],
		ReserveAmounts :: [{Units, pos_integer()}],
		Units :: octets | seconds | messages,
		Location :: [tuple()] | undefined,
		Flag :: {initial, Class} | interim | final,
		Class :: a | b,
		Destination :: string(),
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST ECUR rating data to a Nrf Rating Server.
post_request_ecur(_ServiceName, SubscriptionId, SvcContextId,
		SessionId, [#{price := #price{type = #pla_ref{href = Path}}}
      | _], Amounts, Location, Destination, {initial, a}) ->
	ServiceRating = initial_service_rating(Amounts,
			binary_to_list(SvcContextId), Location, a),
	post_request_ecur1(SubscriptionId, SessionId, ServiceRating, Path);
post_request_ecur(ServiceName, SubscriptionId, SvcContextId,
		SessionId, MSCC, _, Location, Destination, {initial, b}) ->
	Path = get_option(ServiceName, nrf_uri) ++ ?BASE_URI,
	ServiceRating = initial_service_rating(MSCC,
			binary_to_list(SvcContextId), Location, b),
	post_request_ecur1(SubscriptionId, SessionId, ServiceRating, Path);
post_request_ecur(ServiceName, SubscriptionId, SvcContextId,
		SessionId, MSCC, _, Location, Destination, final) ->
	Path = get_option(ServiceName, nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "release",
	ServiceRating = final_service_rating(MSCC,
			binary_to_list(SvcContextId), Location),
	post_request_ecur1(SubscriptionId, SessionId, ServiceRating, Path).
%% @hidden
post_request_ecur1(SubscriptionId, SessionId, ServiceRating, Path) ->
	{ok, Profile} = application:get_env(ocs, nrf_profile),
	TS = erlang:system_time(millisecond),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	Sequence = ets:update_counter(counters, nrf_seq, 1),
	Body = {struct, [{"nfConsumerIdentification",
							{struct, [{"nodeFunctionality", "OCF"}]}},
					{"oneTimeEvent", true},
					{"oneTimeEventType", "PEC"},
					{"invocationTimeStamp", InvocationTimeStamp},
					{"invocationSequenceNumber", Sequence},
					{"subscriptionId", {array, SubscriptionId}},
					{"serviceRating",
							{array, lists:flatten(ServiceRating)}}]},
	ContentType = "application/json",
	RequestBody = mochijson:encode(Body),
	Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
	Request = {Path, Headers, ContentType, lists:flatten(RequestBody)},
	Options = [{relaxed, true}],
	case httpc:request(post, Request, Options, [], Profile) of
		{_RequestId, {{_HttpVersion, 201, _ReasonPhrase}, Headers1, Body1}} ->
			{_, Location1} = lists:keyfind("location", 1, Headers1),
			insert_ref(SessionId, Location1, undefined),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, 200, _ReasonPhrase}, _Headers, Body1}} ->
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
	end.

-spec post_request_iec(ServiceName, SubscriptionId, ServiceContextId,
		SessionId, MSCC, Location, Destination) -> Result
	when
		ServiceName :: {IpAddress, Port},
		IpAddress :: inet:ip_address(),
		Port :: inet:port_number(),
		SubscriptionId :: [string()],
		ServiceContextId :: binary(),
		SessionId :: binary(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}] | [],
		Location :: [tuple()],
		Destination :: string(),
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST IEC rating data to a Nrf Rating Server.
post_request_iec(ServiceName, SubscriptionId, ServiceContextId, SessionId,
		[], _Location, Destination) ->
	SCID = {"serviceContextId", binary_to_list(ServiceContextId)},
	Des = {"destinationId", {array, [{struct, [{"destinationIdType", "DN"},
			{"destinationIdData", Destination}]}]}},
	ServiceRating = [{struct, [SCID, Des, {"requestSubType", "DEBIT"}]}],
	post_request_iec1(ServiceName, SubscriptionId, SessionId, ServiceRating);
post_request_iec(ServiceName, SubscriptionId, ServiceContextId, SessionId, MSCC, Location, Destination) ->
	ServiceRating = iec_service_rating(MSCC, binary_to_list(ServiceContextId), Location, Destination),
	post_request_iec1(ServiceName, SubscriptionId, SessionId, ServiceRating).
%% @hidden
post_request_iec1(ServiceName, SubscriptionId, SessionId, ServiceRating) ->
	{ok, Profile} = application:get_env(ocs, nrf_profile),
	TS = erlang:system_time(millisecond),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	Sequence = ets:update_counter(counters, nrf_seq, 1),
	Body = {struct, [{"nfConsumerIdentification",
							{struct, [{"nodeFunctionality", "OCF"}]}},
					{"oneTimeEvent", true},
					{"oneTimeEventType", "IEC"},
					{"invocationTimeStamp", InvocationTimeStamp},
					{"invocationSequenceNumber", Sequence},
					{"subscriptionId", {array, SubscriptionId}},
					{"serviceRating",
							{array, lists:flatten(ServiceRating)}}]},
	Path = get_option(ServiceName, nrf_uri) ++ ?BASE_URI,
	ContentType = "application/json",
	RequestBody = mochijson:encode(Body),
	Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
	Request = {Path, Headers, ContentType, lists:flatten(RequestBody)},
	Options = [{relaxed, true}],
	case httpc:request(post, Request, Options, [], Profile) of
		{_RequestId, {{_HttpVersion, 201, _ReasonPhrase}, Headers1, Body1}} ->
			{_, Location1} = lists:keyfind("location", 1, Headers1),
			insert_ref(SessionId, Location1, undefined),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
	end.

-spec post_request_scur(ServiceName, SubscriptionId, ServiceContextId,
		SessionId, ServiceRatingData, Amounts, Location, Flag) -> Result
	when
		ServiceName :: {IpAddress, Port},
		IpAddress :: inet:ip_address(),
		Port :: inet:port_number(),
		SubscriptionId :: [string()],
		ServiceContextId :: binary(),
		SessionId :: binary(),
		ServiceRatingData :: [MSCC] | [PLA] | undefined,
		MSCC :: #'3gpp_ro_Multiple-Services-Credit-Control'{},
		PLA :: map(),
		Amounts :: [{ServiceIdentifier, RatingGroup,
				UsedAmounts, ReserveAmounts}] | [],
		ServiceIdentifier :: [pos_integer()],
		RatingGroup :: [pos_integer()],
		UsedAmounts :: [{Units, pos_integer()}],
		ReserveAmounts :: [{Units, pos_integer()}],
		Units :: octets | seconds | messages,
		Location :: [tuple()] | [],
		Flag :: {initial, Class} | interim | final,
		Class :: a | b,
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST SCUR rating data to a Nrf Rating Server.
post_request_scur(_ServiceName, SubscriptionId, SvcContextId, SessionId,
		[#{price := #price{type = #pla_ref{href = Path}}} | _] = _ServiceRatingData,
		Amounts, Location, {initial, a}) ->
	ServiceRating = initial_service_rating(Amounts,
			binary_to_list(SvcContextId), Location, a),
	post_request_scur1(SubscriptionId, SessionId, ServiceRating, Path);
post_request_scur(ServiceName, SubscriptionId, SvcContextId,
		SessionId, MSCC, _, Location, {initial, b}) ->
	Path = get_option(ServiceName, nrf_uri) ++ ?BASE_URI,
	ServiceRating = initial_service_rating(MSCC,
			binary_to_list(SvcContextId), Location, b),
	post_request_scur1(SubscriptionId, SessionId, ServiceRating, Path);
post_request_scur(ServiceName, SubscriptionId, SvcContextId,
		SessionId, MSCC, _, Location, interim) ->
	Path = get_option(ServiceName, nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "update",
	ServiceRating = update_service_rating(MSCC,
			binary_to_list(SvcContextId), Location),
	post_request_scur1(SubscriptionId, SessionId, ServiceRating, Path);
post_request_scur(ServiceName, SubscriptionId, SvcContextId,
		SessionId, MSCC, _, Location, final) ->
	Path = get_option(ServiceName, nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "release",
	ServiceRating = final_service_rating(MSCC,
			binary_to_list(SvcContextId), Location),
	post_request_scur1(SubscriptionId, SessionId, ServiceRating, Path).
%% @hidden
post_request_scur1(SubscriptionId, SessionId, ServiceRating, Path) ->
	{ok, Profile} = application:get_env(ocs, nrf_profile),
	TS = erlang:system_time(millisecond),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	Sequence = ets:update_counter(counters, nrf_seq, 1),
	Body = {struct, [{"nfConsumerIdentification",
							{struct, [{"nodeFunctionality", "OCF"}]}},
					{"invocationTimeStamp", InvocationTimeStamp},
					{"invocationSequenceNumber", Sequence},
					{"subscriptionId", {array, SubscriptionId}},
					{"serviceRating",
							{array, lists:flatten(ServiceRating)}}]},
	ContentType = "application/json",
	RequestBody = mochijson:encode(Body),
	Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
	Request = {Path, Headers, ContentType, lists:flatten(RequestBody)},
	Options = [{relaxed, true}],
	case httpc:request(post, Request, Options, [], Profile) of
		{_RequestId, {{_HttpVersion, 201, _ReasonPhrase}, Headers1, Body1}} ->
			{_, Location1} = lists:keyfind("location", 1, Headers1),
			insert_ref(SessionId, Location1, undefined),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, 200, _ReasonPhrase}, _Headers, Body1}} ->
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
	end.

-spec get_destination(ServiceInformation) -> RequestedPartyAddress
	when
		ServiceInformation :: [#'3gpp_ro_Service-Information'{}],
		RequestedPartyAddress :: string().
%% @doc Get Requested Party Address.
get_destination([#'3gpp_ro_Service-Information'{'IMS-Information'=
		[#'3gpp_ro_IMS-Information'{'Requested-Party-Address' = [RequestedPartyAddress]}]}])
		when is_list(RequestedPartyAddress) ->
	RequestedPartyAddress;
get_destination([#'3gpp_ro_Service-Information'{
		'SMS-Information' = [#'3gpp_ro_SMS-Information'{
		'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
		'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
		'Address-Data' = [RequestedPartyAddress]}]}]}]}])
		when is_binary(RequestedPartyAddress) ->
	binary_to_list(RequestedPartyAddress);
get_destination(_) ->
	[].

-spec get_service_location(ServiceInformation) -> ServiceInformation
	when
		ServiceInformation :: [#'3gpp_ro_Service-Information'{}] |
				tuple().
%% @doc Get MCC and MNC location information.
get_service_location([#'3gpp_ro_Service-Information'{'PS-Information' =
		 [#'3gpp_ro_PS-Information'{'3GPP-SGSN-MCC-MNC' = [MCCMNC]}]}]) ->
	get_service_location1(MCCMNC);
get_service_location(_) ->
	[].
%% @hidden
get_service_location1(<<MCC1, MCC2, MCC3, MNC1, MNC2, MNC3>>) ->
	MCC = [MCC1, MCC2, MCC3],
	MNC = [MNC1, MNC2, MNC3],
	{"serviceInformation", {struct, [{"sgsnMccMnc", {struct,
			[{"mcc", MCC}, {"mnc", MNC}]}}]}};
get_service_location1(<<MCC1, MCC2, MCC3, MNC1, MNC2>>) ->
	MCC = [MCC1, MCC2, MCC3],
	MNC = [MNC1, MNC2],
	{"serviceInformation", {struct, [{"sgsnMccMnc", {struct,
			[{"mcc", MCC}, {"mnc", MNC}]}}]}}.

-spec insert_ref(SessionId, SessionState, PLAs) -> Result
	when
		SessionId :: binary(),
		SessionState :: Location | ServiceRatings,
		PLAs :: [PLA] | undefined,
		PLA :: map(),
		Location :: string(),
		ServiceRatings :: [Tariff],
		Tariff :: #{destinationId => string(),
				lastModified := integer(),
				ratingFunction := string(),
				ratingGroup => 0..4294967295,
				serviceId => 0..4294967295,
				serviceContextId := string(),
				expiryTime => 0..4294967295,
				currentTariff := TariffElement,
				nextTariff => TariffElement},
		TariffElement :: #{currencyCode => integer(),
				rateElements := RateElement},
		RateElement :: #{unitType := UnitType,
				unitSize := UnitSize, unitCost := UnitCost},
		UnitSize :: #{valueDigits := pos_integer(),
				exponent => integer()},
		UnitCost :: #{valueDigits := integer(),
				exponent => integer()},
		UnitType :: octets | seconds | messages,
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Insert a rating Data ref.
insert_ref(SessionId, SessionState, undefined)
		when is_list(SessionState), is_binary(SessionId) ->
	insert_ref1(SessionId, SessionState);
insert_ref(SessionId, SessionState,
		[#{price := #price{type = #pla_ref{href = Path}}} | _])
		when is_list(SessionState), is_binary(SessionId) ->
	F = fun F([#{} = H | T1], Acc1) ->
			NewSessionState = H#{ratingFunction => Path,
					lastModified => erlang:system_time(millisecond)},
			F(T1, [NewSessionState | Acc1]);
		F([], Acc1) ->
			Acc1
	end,
	insert_ref1(SessionId, F(SessionState, [])).
%% @hidden
insert_ref1(SessionId, SessionState) ->
	case catch ets:insert(?NRF_TABLE, {SessionId, SessionState}) of
		true ->
			ok;
		{'EXIT', Reason} ->
			{error, Reason}
	end.

-spec get_ref(SessionId) -> SessionState
	when
		SessionId :: binary(),
		SessionState :: Location | ServiceRating,
		Location :: string(),
		ServiceRating :: [map()].
%% @doc Get a rating data ref
%% @hidden
get_ref(SessionId)
		when is_binary(SessionId) ->
	case ets:lookup(?NRF_TABLE, SessionId) of
		[{SessionId, SessionState}] ->
			SessionState;
		_ ->
			[]
	end.

-spec remove_ref(SessionId) -> Result
	when
		SessionId :: binary(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Remove a rating data ref
%% @hidden
remove_ref(SessionId)
		when is_binary(SessionId) ->
	case catch ets:delete(?NRF_TABLE, SessionId) of
		true ->
			ok;
		{'EXIT', Reason} ->
			{error, Reason}
	end.

-spec build_mscc(ServiceRatings, Container) -> Result
	when
		ServiceRatings :: [ServiceRating],
		ServiceRating :: #{finalResultCode => pos_integer(),
				grantedUnit => #'3gpp_ro_Granted-Service-Unit'{},
				consumedUnit => #'3gpp_ro_Used-Service-Unit'{},
				ratingGroup => pos_integer(), resultCode => pos_integer(),
				serviceId => pos_integer(), serviceContextId => string()},
		Container :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		Result :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}].
%% @doc Build a list of CCA MSCCs
build_mscc([H | T], Container) ->
	F = fun F(#{serviceId := SI, ratingGroup := RG, resultCode := RC} = ServiceRating,
			[#'3gpp_ro_Multiple-Services-Credit-Control'
					{'Service-Identifier' = [SI], 'Rating-Group' = [RG]} = MSCC1 | T1], Acc) ->
				MSCC2 = case catch maps:get(grantedUnit, ServiceRating) of
					#'3gpp_ro_Granted-Service-Unit'{} = GrantedUnits ->
						MSCC1#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = [GrantedUnits]};
					_ ->
						MSCC1
				end,
				MSCC3 = case catch maps:get(finalUnitIndication, ServiceRating) of
					[#'3gpp_ro_Final-Unit-Indication'{}] = FUI ->
						MSCC2#'3gpp_ro_Multiple-Services-Credit-Control'{'Final-Unit-Indication' = FUI};
					_ ->
						MSCC2
				end,
				case MSCC3 of
					#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = []} ->
						lists:reverse(T1) ++ Acc;
					_ ->
						MSCC4 = MSCC3#'3gpp_ro_Multiple-Services-Credit-Control'{'Result-Code' = [RC]},
						lists:reverse(T1) ++ [MSCC4] ++ Acc
				end;
		F(#{serviceId := SI, resultCode := RC} = ServiceRating,
			[#'3gpp_ro_Multiple-Services-Credit-Control'
					{'Service-Identifier' = [SI], 'Rating-Group' = []} = MSCC1 | T1], Acc) ->
				MSCC2 = case catch maps:get(grantedUnit, ServiceRating) of
					#'3gpp_ro_Granted-Service-Unit'{} = GrantedUnits ->
						MSCC1#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = [GrantedUnits]};
					_ ->
						MSCC1
				end,
				MSCC3 = case catch maps:get(finalUnitIndication, ServiceRating) of
					[#'3gpp_ro_Final-Unit-Indication'{}] = FUI ->
						MSCC2#'3gpp_ro_Multiple-Services-Credit-Control'{'Final-Unit-Indication' = FUI};
					_ ->
						MSCC2
				end,
				MSCC4 = MSCC3#'3gpp_ro_Multiple-Services-Credit-Control'{'Result-Code' = [RC]},
				lists:reverse(T1) ++ [MSCC4] ++ Acc;
			F(#{ratingGroup := RG, resultCode := RC} = ServiceRating,
					[#'3gpp_ro_Multiple-Services-Credit-Control'
							{'Service-Identifier' = [], 'Rating-Group' = [RG]} = MSCC1 | T1], Acc) ->
				MSCC2 = case catch maps:get(grantedUnit, ServiceRating) of
					#'3gpp_ro_Granted-Service-Unit'{} = GrantedUnits ->
						MSCC1#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = [GrantedUnits]};
					_ ->
						MSCC1
				end,
				MSCC3 = case catch maps:get(finalUnitIndication, ServiceRating) of
					[#'3gpp_ro_Final-Unit-Indication'{}] = FUI ->
						MSCC2#'3gpp_ro_Multiple-Services-Credit-Control'{'Final-Unit-Indication' = FUI};
					_ ->
						MSCC2
				end,
				MSCC4 = MSCC3#'3gpp_ro_Multiple-Services-Credit-Control'{'Result-Code' = [RC]},
				lists:reverse(T1) ++ [MSCC4] ++ Acc;
			F(ServiceRating, [H1 | T1], Acc) ->
				F(ServiceRating, T1, [H1 | Acc])
	end,
	NewContainer = F(H, Container, []),
	build_mscc(T, NewContainer);
build_mscc([], Container) ->
	lists:reverse(Container).

-spec map_service_rating(ServiceRating, SessionId) -> Result
	when
		ServiceRating :: [{struct, [tuple()]}],
		SessionId :: binary() | undefined,
		Result :: {[map()], ResultCode},
		ResultCode :: integer().
%% @doc Convert a service rating struct list to list of maps.
map_service_rating(ServiceRating, SessionId)
		when SessionId =/= undefined ->
	map_service_rating(ServiceRating, undefined, []);
map_service_rating([], _) ->
	{[], ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'}.
%% @hidden
map_service_rating([{struct, Elements} | T], RC2, Acc) ->
	F = fun F([{"grantedUnit", {_, Units}} | T1], Acc1) ->
			Acc2 = Acc1#{grantedUnit => granted_units(Units)},
			F(T1, Acc2);
		F([{"consumedUnit", {_, Units}} | T1], Acc1) ->
			Acc2 = Acc1#{consumedUnit => used_units(Units)},
			F(T1, Acc2);
		F([{"currentTariff", {_, TariffElements}} | T1], Acc1) ->
			Acc2 = Acc1#{currentTariff => tariff_element(TariffElements)},
			F(T1, Acc2);
		F([{"nextTariff", {_, TariffElements}} | T1], Acc1) ->
			Acc2 = Acc1#{nextTariff => tariff_element(TariffElements)},
			F(T1, Acc2);
		F([{"resultCode", RC1} | T1], Acc1) ->
			{NewRC1, NewRC2} = result_code(RC1, RC2),
			Acc2 = Acc1#{resultCode => NewRC1,
					finalResultCode => NewRC2},
			F(T1, Acc2);
		F([{"destinationId", {array, [DestinationId]}} | T1], Acc1) ->
			F(T1, Acc1#{destinationId => DestinationId});
		F([{"ratingGroup", Value} | T1], Acc1) ->
			F(T1, Acc1#{ratingGroup => Value});
		F([{"serviceId", Value} | T1], Acc1) ->
			F(T1, Acc1#{serviceId => Value});
		F([{Name, Value} | T1], Acc1) ->
			F(T1, Acc1#{Name => Value});
		F([], Acc1) ->
			Acc1
	end,
	ServiceRatingMap = F(Elements, #{}),
	case catch maps:get(finalResultCode, ServiceRatingMap) of
		{'EXIT', _Reason} ->
			map_service_rating(T, undefined, [ServiceRatingMap | Acc]);
		RC3 ->
			map_service_rating(T, RC3, [ServiceRatingMap | Acc])
	end;
map_service_rating([], NewRC2, Acc) ->
	{lists:reverse(Acc), NewRC2}.

%% @hidden
tariff_element(Elements) ->
	tariff_element(Elements, #{}).
tariff_element([{"currencyCode", CurrencyCode} | T], Acc) ->
	tariff_element(T, Acc#{currencyCode => CurrencyCode});
tariff_element([{"rateElement", {_,[{_, RateElements}]}} | T], Acc) ->
	RateElements1 = rate_elements(RateElements, #{}),
	tariff_element(T, Acc#{rateElements => RateElements1});
tariff_element([_H | T], Acc) ->
	tariff_element(T, Acc);
tariff_element([], Acc) ->
	Acc.

%% @hidden
rate_elements([{"unitType", UnitType} | T], Acc) ->
	rate_elements(T, Acc#{unitType => unit_type(UnitType)});
rate_elements([{"unitSize", {_, [{"valueDigits", ValueDigits}]}} | T], Acc) ->
	rate_elements(T, Acc#{unitSize => ValueDigits});
rate_elements([{"unitSize", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent >= 0 ->
	rate_elements(T, Acc#{unitSize => ValueDigits * (Exponent * 10) * 10000});
rate_elements([{"unitSize", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent < 0 ->
	rate_elements(T, Acc#{unitSize => ValueDigits div (Exponent * 10) * 10000});
rate_elements([{"unitCost", {_, [{"valueDigits", ValueDigits}]}} | T], Acc) ->
	rate_elements(T, Acc#{unitCost => ValueDigits});
rate_elements([{"unitCost", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent >= 0 ->
	rate_elements(T, Acc#{unitCost => ValueDigits * (Exponent * 10) * 10000});
rate_elements([{"unitCost", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent < 0 ->
	rate_elements(T, Acc#{unitCost => ValueDigits div (Exponent * 10) * 10000});
rate_elements([_H | T], Acc) ->
	rate_elements(T, Acc);
rate_elements([], Acc) ->
	Acc.

%% @hidden
unit_type("TOTAL_VOLUME") ->
	octets;
unit_type("SERVICE_SPECIFIC_UNITS") ->
	messages;
unit_type("TIME") ->
	seconds.

-spec build_container(MSCC) -> MSCC
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}].
%% @doc Build a container for CCR MSCC.
build_container(MSCC) ->
	build_container(MSCC, []).
%% @hidden
build_container([#'3gpp_ro_Multiple-Services-Credit-Control'
		{'Service-Identifier' = SI, 'Rating-Group' = RG} = _MSCC | T], Acc) ->
	NewMSCC = #'3gpp_ro_Multiple-Services-Credit-Control'
			{'Service-Identifier' = SI, 'Rating-Group' = RG},
	build_container(T, [NewMSCC | Acc]);
build_container([], Acc) ->
	lists:reverse(Acc).

%% @hidden
result_code(RC1, RC2) ->
	ResultCode1 = case RC1 of
		"SUCCESS" ->
			?'DIAMETER_BASE_RESULT-CODE_SUCCESS';
		"END_USER_SERVICE_DENIED" ->
			?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED';
		"QUOTA_LIMIT_REACHED" ->
			?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED';
		"USER_UNKNOWN" ->
			?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN';
		"RATING_FAILED" ->
			?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED'
	end,
	ResultCode2 = case RC2 of
		undefined ->
			ResultCode1;
		RC2 ->
			RC2
	end,
	{ResultCode1, ResultCode2}.

%% @hidden
granted_units(Units) ->
	granted_units(Units, #'3gpp_ro_Granted-Service-Unit'{}).
%% @hidden
granted_units([{"time", CCTime} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [CCTime]},
	granted_units(T, Acc1);
granted_units([{"downlinkVolume", DownLinkVolume} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Granted-Service-Unit'{'CC-Output-Octets' = [DownLinkVolume]},
	granted_units(T, Acc1);
granted_units([{"uplinkVolume", UpLinkVolume} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Granted-Service-Unit'{'CC-Input-Octets' = [UpLinkVolume]},
	granted_units(T, Acc1);
granted_units([{"totalVolume", TotalVolume} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalVolume]},
	granted_units(T, Acc1);
granted_units([{"serviceSpecificUnit", SpecUnits} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [SpecUnits]},
	granted_units(T, Acc1);
granted_units([], Acc) ->
	Acc.

%% @hidden
used_units(Units) ->
	used_units(Units, #'3gpp_ro_Used-Service-Unit'{}).
%% @hidden
used_units([{"time", CCTime} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Used-Service-Unit'{'CC-Time' = [CCTime]},
	used_units(T, Acc1);
used_units([{"downlinkVolume", DownLinkVolume} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Used-Service-Unit'{'CC-Output-Octets' = [DownLinkVolume]},
	used_units(T, Acc1);
used_units([{"uplinkVolume", UpLinkVolume} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Used-Service-Unit'{'CC-Input-Octets' = [UpLinkVolume]},
	used_units(T, Acc1);
used_units([{"totalVolume", TotalVolume} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [TotalVolume]},
	used_units(T, Acc1);
used_units([{"serviceSpecificUnit", SpecUnits} | T], Acc) ->
	Acc1 = Acc#'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [SpecUnits]},
	used_units(T, Acc1);
used_units([], Acc) ->
	Acc.

-spec diameter_answer(SessionId, MSCC, ResultCode,
		OriginHost, OriginRealm, RequestType, RequestNum) -> Result
	when
		SessionId :: binary(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ResultCode :: pos_integer(),
		OriginHost :: string(),
		OriginRealm :: string(),
		RequestType :: integer(),
		RequestNum :: integer(),
		Result :: #'3gpp_ro_CCA'{}.
%% @doc Build CCA response.
%% @hidden
diameter_answer(SessionId, MSCC, ResultCode,
		OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_ro_CCA'{'Session-Id' = SessionId,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = MSCC,
			'Result-Code' = ResultCode}.

-spec diameter_error(SessionId, ResultCode, OriginHost,
		OriginRealm, RequestType, RequestNum) -> Reply
	when
		SessionId :: binary(),
		ResultCode :: pos_integer(),
		OriginHost :: string(),
		OriginRealm :: string(),
		RequestType :: integer(),
		RequestNum :: integer(),
		Reply :: #'3gpp_ro_CCA'{}.
%% @doc Send CCA to DIAMETER client indicating an operation failure.
%% @hidden
diameter_error(SessionId, 400, OHost, ORealm, RequestType, RequestNum) ->
	ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
	diameter_error1(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum);
diameter_error(SessionId, 403, OHost, ORealm, RequestType, RequestNum) ->
	ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
	diameter_error1(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum);
diameter_error(SessionId, 404, OHost, ORealm, RequestType, RequestNum) ->
	ResultCode = 'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
	diameter_error1(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum);
diameter_error(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	diameter_error1(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum).
%% @hidden
diameter_error1(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_ro_CCA'{'Session-Id' = SessionId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum}.

-spec iec_service_rating(MSCC, ServiceContextId, ServiceInformation, Destination) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: list(),
		ServiceInformation :: [tuple()],
		Destination :: string(),
		ServiceRating :: [{struct, [tuple()]}],
		Destination :: string().
%% Create list of service elements to be rated.
iec_service_rating(MSCC, ServiceContextId, ServiceInformation, Destination) ->
	SCID = {"serviceContextId", ServiceContextId},
	Des = {"destinationId", {array, [{struct, [{"destinationIdType", "DN"},
			{"destinationIdData", Destination}]}]}},
	iec_service_rating1(MSCC, SCID, ServiceInformation, Des, []).
%% @hidden
iec_service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = USU, 'Service-Identifier' = SI,
		'Rating-Group' = RG} = _MSCC | T], SCID, SInfo, Des, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			Acc1;
		[N2] ->
			[{"ratingGroup", N2} | Acc1]
	end,
	Acc3 = case SInfo of
		[] ->
			Acc2;
		_ ->
			[SInfo | Acc2]
	end,
	ServiceRating = case used_unit(USU) of
		[] ->
			{struct, [SCID, Des, {"requestSubType", "DEBIT"} | Acc3]};
		ReservedUnits ->
			{struct, [SCID, Des, {"consumedUnit", {struct, ReservedUnits}},
					{"requestSubType", "DEBIT"} | Acc3]}
	end,
	iec_service_rating1(T, SCID, SInfo, Des, [ServiceRating | Acc]);
iec_service_rating1([], _SCID, _SI, _Dec, Acc) ->
	Acc.

-spec used_unit(UsedServiceUnits) -> Result
	when
		UsedServiceUnits :: [#'3gpp_ro_Used-Service-Unit'{}] |
				[],
		Result :: [{Units, Value}] | [],
		Units :: [Unit],
		Unit :: time | downlinkVolume | uplinkVolume
				| totalVolume | serviceSpecificUnit,
		Value :: pos_integer().
used_unit(UsedServiceUnits) ->
	used_unit(UsedServiceUnits, []).
%% @hidden
used_unit([#'3gpp_ro_Used-Service-Unit'
		{'CC-Time' = [CCTime]}] = RSU, Acc)
		when CCTime > 0 ->
	used_unit1(RSU, [{"time", CCTime} | Acc]);
used_unit(RSU, Acc) ->
	used_unit1(RSU, Acc).
used_unit1([#'3gpp_ro_Used-Service-Unit'
		{'CC-Output-Octets' = [CCOutputOctets]}] = RSU, Acc)
				when CCOutputOctets > 0 ->
	used_unit2(RSU, [{"downlinkVolume", CCOutputOctets} | Acc]);
used_unit1(RSU, Acc) ->
	used_unit2(RSU, Acc).
used_unit2([#'3gpp_ro_Used-Service-Unit'
		{'CC-Input-Octets' = [CCInputOctets]}] = RSU, Acc)
				when CCInputOctets > 0 ->
	used_unit3(RSU, [{"uplinkVolume", CCInputOctets} | Acc]);
used_unit2(RSU, Acc) ->
	used_unit3(RSU, Acc).
used_unit3([#'3gpp_ro_Used-Service-Unit'
		{'CC-Total-Octets' = [CCTotalOctets]}] = RSU, Acc)
				when CCTotalOctets > 0 ->
	used_unit4(RSU, [{"totalVolume", CCTotalOctets} | Acc]);
used_unit3(RSU, Acc) ->
	used_unit4(RSU, Acc).
used_unit4([#'3gpp_ro_Used-Service-Unit'
		{'CC-Service-Specific-Units' = [CCSpecUnits]}], Acc)
				when CCSpecUnits > 0 ->
	[{"serviceSpecificUnit", CCSpecUnits} | Acc];
used_unit4(_RSU, Acc) ->
	Acc.

-spec reserved_unit(RequestedServiceUnits) -> Result
	when
		RequestedServiceUnits :: [#'3gpp_ro_Requested-Service-Unit'{}] |
				[],
		Result :: [{Units, Value}] | [],
		Units :: [Unit],
		Unit :: time | downlinkVolume | uplinkVolume
				| totalVolume | serviceSpecificUnit,
		Value :: pos_integer().
reserved_unit(RequestedServiceUnits) ->
	reserved_unit(RequestedServiceUnits, []).
%% @hidden
reserved_unit([#'3gpp_ro_Requested-Service-Unit'
		{'CC-Time' = [CCTime]}] = RSU, Acc)
		when CCTime > 0 ->
	reserved_unit1(RSU, [{"time", CCTime} | Acc]);
reserved_unit(RSU, Acc) ->
	reserved_unit1(RSU, Acc).
reserved_unit1([#'3gpp_ro_Requested-Service-Unit'
		{'CC-Output-Octets' = [CCOutputOctets]}] = RSU, Acc)
				when CCOutputOctets > 0 ->
	reserved_unit2(RSU, [{"downlinkVolume", CCOutputOctets} | Acc]);
reserved_unit1(RSU, Acc) ->
	reserved_unit2(RSU, Acc).
reserved_unit2([#'3gpp_ro_Requested-Service-Unit'
		{'CC-Input-Octets' = [CCInputOctets]}] = RSU, Acc)
				when CCInputOctets > 0 ->
	reserved_unit3(RSU, [{"uplinkVolume", CCInputOctets} | Acc]);
reserved_unit2(RSU, Acc) ->
	reserved_unit3(RSU, Acc).
reserved_unit3([#'3gpp_ro_Requested-Service-Unit'
		{'CC-Total-Octets' = [CCTotalOctets]}] = RSU, Acc)
				when CCTotalOctets > 0 ->
	reserved_unit4(RSU, [{"totalVolume", CCTotalOctets} | Acc]);
reserved_unit3(RSU, Acc) ->
	reserved_unit4(RSU, Acc).
reserved_unit4([#'3gpp_ro_Requested-Service-Unit'
		{'CC-Service-Specific-Units' = [CCSpecUnits]}], Acc)
				when CCSpecUnits > 0 ->
	[{"serviceSpecificUnit", CCSpecUnits} | Acc];
reserved_unit4(_RSU, Acc) ->
	Acc.

-spec initial_service_rating(MSCC, ServiceContextId,
		ServiceInformation, Class) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}] | [PLA],
		PLA :: map(),
		ServiceContextId :: string(),
		ServiceInformation :: [tuple()],
		ServiceRating :: [{struct, [tuple()]}],
		Class :: a | b.
%% @hidden
%% Create list of service elements to be rated.
initial_service_rating(MSCC, ServiceContextId, ServiceInformation, Class) ->
	SCID = {"serviceContextId", ServiceContextId},
	initial_service_rating(MSCC, SCID, ServiceInformation, Class, []).
%% @hidden
initial_service_rating([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = RSU, 'Service-Identifier' = SI,
		'Rating-Group' = RG} = _MSCC | T], SCID, SInfo, b, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			Acc1;
		[N2] ->
			[{"ratingGroup", N2} | Acc1]
	end,
	Acc3 = case SInfo of
		[] ->
			Acc2;
		_ ->
			[SInfo | Acc2]
	end,
	case reserved_unit(RSU) of
		[] ->
			ServiceRating = {struct, [SCID, {"requestSubType", "RESERVE"} | Acc3]},
				initial_service_rating(T, SCID, SInfo, b, [ServiceRating | Acc]);
		ReservedUnits ->
			ServiceRating = {struct, [SCID, {"requestedUnit", {struct, ReservedUnits}},
					{"requestSubType", "RESERVE"} | Acc3]},
			initial_service_rating(T, SCID, SInfo, b, [ServiceRating | Acc])
	end;
initial_service_rating([{SI, RG, _, _} | T], SCID, SInfo, a, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] when is_integer(N1) ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			Acc1;
		[N2] when is_integer(N2) ->
			[{"ratingGroup", N2} | Acc1]
	end,
	initial_service_rating(T, SCID, SInfo, a, [{struct, [SCID | Acc2]} | Acc]);
initial_service_rating([], _, _, _, Acc) ->
	Acc.

-spec update_service_rating(MSCC, ServiceContextId,
		ServiceInformation) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: string(),
		ServiceInformation :: [tuple()],
		ServiceRating :: [{struct, [tuple()]}].
%% Create list of service elements to be rated.
update_service_rating(MSCC, ServiceContextId, ServiceInformation) ->
	SCID = {"serviceContextId", ServiceContextId},
	update_service_rating1(MSCC, SCID, ServiceInformation, []).
%% @hidden
update_service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = RSU, 'Used-Service-Unit' = USU,
		'Service-Identifier' = SI, 'Rating-Group' = RG} = _MSCC | T], SCID,
		SInfo, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			Acc1;
		[N2] ->
			[{"ratingGroup", N2} | Acc1]
	end,
	Acc3 = case SInfo of
		[] ->
			Acc2;
		_ ->
			[SInfo | Acc2]
	end,
	case {used_unit(USU), reserved_unit(RSU)} of
		{[], []} ->
			ServiceRating = {struct, [SCID, {"requestSubType", "RESERVE"} | Acc3]},
			update_service_rating1(T, SCID, SInfo, [ServiceRating | Acc]);
		{ConsumedUnits, []} when length(ConsumedUnits) > 0 ->
			ServiceRating1 = {struct, [SCID, {"consumedUnit", {struct, ConsumedUnits}},
					{"requestSubType", "DEBIT"} | Acc3]},
			ServiceRating2 = {struct, [SCID, {"requestSubType", "RESERVE"} | Acc3]},
			update_service_rating1(T, SCID, SInfo, [ServiceRating1, ServiceRating2 | Acc]);
		{[], ReservedUnits} when length(ReservedUnits) > 0 ->
			ServiceRating = {struct, [SCID, {"requestedUnit", {struct, ReservedUnits}},
					{"requestSubType", "RESERVE"} | Acc3]},
			update_service_rating1(T, SCID, SInfo, [ServiceRating | Acc])
	end;
update_service_rating1([], _SCID, _SI, Acc) ->
	Acc.

-spec final_service_rating(MSCC, ServiceContextId, ServiceInformation) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: string(),
		ServiceInformation :: [tuple()],
		ServiceRating :: [{struct, [tuple()]}].
%% Create list of service elements to be rated.
final_service_rating(MSCC, ServiceContextId, ServiceInformation) ->
	SCID = {"serviceContextId", ServiceContextId},
	final_service_rating1(MSCC, SCID, ServiceInformation, []).
%% @hidden
final_service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Service-Identifier' = SI, 'Rating-Group' = RG,
		'Used-Service-Unit' = USU} = _MSCC | T], SCID, SInfo, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			Acc1;
		[N2] ->
			[{"ratingGroup", N2} | Acc1]
	end,
	Acc3 = case SInfo of
		[] ->
			Acc2;
		_ ->
			[SInfo | Acc2]
	end,
	case used_unit(USU) of
		[] ->
			ServiceRating = {struct, [SCID, {"requestSubType", "DEBIT"} | Acc3]},
			final_service_rating1(T, SCID, SInfo, [ServiceRating | Acc]);
		ConsumedUnits ->
			ServiceRating = {struct, [SCID, {"consumedUnit", {struct, ConsumedUnits}},
					{"requestSubType", "DEBIT"} | Acc3]},
			final_service_rating1(T, SCID, SInfo, [ServiceRating | Acc])
	end;
final_service_rating1([], _SCID, _SI, Acc) ->
	Acc.

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

-spec get_option(ServiceName, Option) -> Result
	when
		ServiceName :: {IpAddress, Port},
		IpAddress :: inet:ip_address(),
		Port :: inet:port_number(),
		Option :: atom(),
		Result :: term() | undefined.
%% @doc Get the Nrf endpoint uri.
get_option({IpAddress, Port} = _ServiceName, Option)
		when is_atom(Option) ->
	{ok, Configurations} = application:get_env(ocs, diameter),
	{_, AcctServices} = lists:keyfind(acct, 1, Configurations),
	F = fun F([{{0, 0, 0, 0}, P, Options} | _]) when P =:= Port ->
			Options;
		F([{Ip, P, Options} | _]) when Ip == IpAddress, P =:= Port ->
			Options;
		F([{_, _, _} | T]) ->
			F(T)
	end,
	ServiceOptions = F(AcctServices),
	case lists:keyfind(Option, 1, ServiceOptions) of
		{Option, Value} ->
			Value;
		false ->
			undefined
	end.

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
service_network([#'3gpp_ro_Service-Information'{
		'PS-Information' = [#'3gpp_ro_PS-Information'{
		'3GPP-SGSN-MCC-MNC' = [MccMnc]}]}]) ->
	MccMnc;
service_network(_) ->
	undefined.

-spec get_mscc(MSCC) -> Result
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		Result :: [{ServiceIdentifier, RatingGroup,
		UsedAmounts, ReserveAmounts}],
		ServiceIdentifier :: [pos_integer()],
		RatingGroup :: [pos_integer()],
		UsedAmounts :: [{Units, pos_integer()}] | undefined,
		ReserveAmounts :: [{Units, pos_integer()}] | undefined,
		Units :: octets | seconds | messages.
%% @doc Parse out the USU, RSU, Charging Key and Service ID from MSCC
%% @hidden
get_mscc(MSCC) ->
   get_mscc(MSCC, []).
%% @hidden
get_mscc([H | T], Acc) ->
	Amounts = {get_si(H), get_rg(H), get_usu(H), get_rsu(H)},
	get_mscc(T, [Amounts | Acc]);
get_mscc([], Acc) ->
	lists:reverse(Acc).

%% @hidden
get_si(#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = [SI]})
		when is_integer(SI) ->
	[SI];
get_si(_) ->
	[].

%% @hidden
get_rg(#'3gpp_ro_Multiple-Services-Credit-Control'{'Rating-Group' = [RG]})
		when is_integer(RG) ->
	[RG];
get_rg(_) ->
	[].

%% @hidden
get_rsu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Time' = [CCTime]}]})
		when is_integer(CCTime), CCTime > 0 ->
	[{seconds, CCTime}];
get_rsu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Total-Octets' = [CCTotalOctets]}]})
		when is_integer(CCTotalOctets), CCTotalOctets > 0 ->
	[{octets, CCTotalOctets}];
get_rsu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Output-Octets' = [CCOutputOctets],
		'CC-Input-Octets' = [CCInputOctets]}]})
		when is_integer(CCInputOctets), is_integer(CCOutputOctets),
		CCInputOctets > 0, CCOutputOctets > 0 ->
	[{octets, CCInputOctets + CCOutputOctets}];
get_rsu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Service-Specific-Units' = [CCSpecUnits]}]})
		when is_integer(CCSpecUnits), CCSpecUnits > 0 ->
	[{messages, CCSpecUnits}];
get_rsu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{}]}) ->
	[];
get_rsu(#'3gpp_ro_Multiple-Services-Credit-Control'{}) ->
	undefined.

%% @hidden
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Time' = [CCTime]} | _]})
		when is_integer(CCTime), CCTime > 0 ->
	[{seconds, CCTime}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Total-Octets' = [CCTotalOctets]} | _]})
		when is_integer(CCTotalOctets), CCTotalOctets > 0 ->
	[{octets, CCTotalOctets}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Output-Octets' = [CCOutputOctets],
		'CC-Input-Octets' = [CCInputOctets]} | _]})
		when is_integer(CCInputOctets), is_integer(CCOutputOctets),
		CCInputOctets > 0, CCOutputOctets > 0 ->
	[{octets, CCInputOctets + CCOutputOctets}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Service-Specific-Units' = [CCSpecUnits]} | _]})
		when is_integer(CCSpecUnits), CCSpecUnits > 0 ->
	[{messages, CCSpecUnits}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' = []}) ->
	[];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{}) ->
	undefined.

-spec rate(ServiceType, ServiceNetwork, SubscriberIDs, Timestamp,
		Address, Direction, Flag, SessionId, Amounts) -> Result
	when
		ServiceType :: pos_integer(),
		ServiceNetwork :: binary(),
		SubscriberIDs :: [SubscriberID],
		SubscriberID :: binary(),
		Timestamp :: calendar:datetime(),
		Address :: binary() | undefined,
		Direction :: answer | originate | undefined,
		Flag :: initial | interim | final | event,
		SessionId :: binary(),
		Amounts :: [{ServiceIdentifier, RatingGroup,
				UsedAmounts, ReserveAmounts}],
		ServiceIdentifier :: [pos_integer()],
		RatingGroup :: [pos_integer()],
		UsedAmounts :: [{Units, pos_integer()}],
		ReserveAmounts :: [{Units, pos_integer()}],
		Units :: octets | seconds | messages,
		Result :: {{MSCCs, ResultCode, Rated}, PLAs, Amounts}
				| {error, Reason},
		Reason :: term(),
		MSCCs :: [MSCC],
		MSCC :: map(),
		PLAs :: [PLA],
		PLA :: map(),
		ResultCode :: pos_integer(),
		Rated :: [#rated{}] | undefined.
%% @doc Rate all the MSCCs.
%% @hidden
rate(ServiceType, ServiceNetwork, SubscriberIDs, Timestamp,
		Address, Direction, Flag, SessionId, Amounts) ->
	rate(ServiceType, ServiceNetwork, SubscriberIDs, Timestamp,
			Address, Direction, Flag, SessionId,
			Amounts, [], [], [], undefined, undefined).
%% @hidden
rate(ServiceType, ServiceNetwork, SubscriberIDs,
		Timestamp, Address, Direction, Flag, SessionId,
		[{SI, RG, Debits, Reserves} | T],
				Acc1, Acc2, Acc3, ResultCode1, Rated1) ->
	ServiceId = case SI of
		[] ->
			undefined;
		[N1] ->
			N1
	end,
	ChargingKey = case RG of
		[] ->
			undefined;
		[N2] ->
			N2
	end,
	case ocs_rating:rate(diameter, ServiceType, ServiceId, ChargingKey,
			ServiceNetwork, SubscriberIDs, Timestamp, Address, Direction, Flag,
			Debits, Reserves, [{'Session-Id', SessionId}]) of
		{ok, {pla_ref, #price{} = Price}} ->
			PLA = #{price => Price, serviceId => ServiceId,
					ratingGroup => ChargingKey},
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc1, [PLA | Acc2], [{SI, RG, Debits, Reserves} | Acc3],
					ResultCode1, Rated1);
		{ok, _, {_, Amount} = GrantedAmount} when Amount > 0 ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			MSCC = #{grantedUnit => granted_unit(GrantedAmount),
					serviceId => ServiceId, ratingGroup => ChargingKey,
					resultCode => ResultCode2},
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc1], Acc2, Acc3, ResultCode2, Rated1);
		{ok, _, {_, 0} = _GrantedAmount} ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc1, Acc2, Acc3, ResultCode2, Rated1);
		{ok, _, {_, Amount} = GrantedAmount, Rated2} when Amount > 0,
				is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			MSCC = #{grantedUnit => granted_unit(GrantedAmount),
					serviceId => ServiceId, ratingGroup => ChargingKey,
					resultCode => ResultCode2},
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc1], Acc2, Acc3, ResultCode2, Rated2);
		{ok, _, Rated2} when is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc1, Acc2, Acc3, ResultCode2, Rated2);
		{ok, _, Rated2} when is_list(Rated2), is_list(Rated1) ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc1, Acc2, Acc3, ResultCode2, Rated1 ++ Rated2);
		{out_of_credit, RedirectServerAddress, _SessionList} ->
			ResultCode2 = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			ResultCode3 = case ResultCode1 of
				undefined->
					ResultCode2;
				ResultCode1 ->
					ResultCode1
			end,
			MSCC = case RedirectServerAddress of
				undefined ->
					#{serviceId => ServiceId, ratingGroup => ChargingKey,
							resultCode => ResultCode2};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#{serviceId => ServiceId, ratingGroup => ChargingKey,
							finalUnitIndication => fui(RedirectServerAddress),
							resultCode => ResultCode2}
			end,
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc1], Acc2, Acc3, ResultCode3, Rated1);
		{out_of_credit, RedirectServerAddress, _SessionList, Rated2}
				when is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			ResultCode3 = case ResultCode1 of
				undefined ->
					ResultCode2;
				ResultCode1 ->
					ResultCode1
			end,
			MSCC = case RedirectServerAddress of
				undefined ->
					#{serviceId => ServiceId, ratingGroup => ChargingKey,
							resultCode => ResultCode2};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#{serviceId => ServiceId, ratingGroup => ChargingKey,
							finalUnitIndication => fui(RedirectServerAddress),
							resultCode => ResultCode2}
			end,
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc1], Acc2, Acc3, ResultCode3, Rated2);
		{out_of_credit, RedirectServerAddress, _SessionList, Rated2}
				when is_list(Rated2), is_list(Rated1) ->
			ResultCode2 = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			ResultCode3 = case ResultCode1 of
				undefined ->
					ResultCode2;
				ResultCode1 ->
					ResultCode1
			end,
			MSCC = case RedirectServerAddress of
				undefined ->
					#{serviceId => ServiceId, ratingGroup => ChargingKey,
							resultCode => ResultCode2};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#{serviceId => ServiceId, ratingGroup => ChargingKey,
							finalUnitIndication => fui(RedirectServerAddress),
							resultCode => ResultCode2}
			end,
			rate(ServiceType, ServiceNetwork, SubscriberIDs,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc1], Acc2, Acc3, ResultCode3, Rated1 ++ Rated2);
		{disabled, _SessionList} ->
			{{Acc1, ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED', undefined},
					Acc2, Acc3};
		{error, service_not_found} ->
			{{Acc1, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN', undefined},
					Acc2, Acc3};
		{error, Reason} ->
			{error, Reason}
	end;
rate(ServiceType, ServiceNetwork, SubscriberIDs,
		Timestamp, Address, Direction, final, SessionId,
		[], Acc1, Acc2, Acc3, ResultCode, Rated1) ->
	case ocs_rating:rate(diameter, ServiceType, undefined, undefined,
			ServiceNetwork, SubscriberIDs, Timestamp, Address, Direction, final,
			[], [], [{'Session-Id', SessionId}]) of
		{ok, _, Rated2} when is_list(Rated2), Rated1 == undefined ->
			{{lists:reverse(Acc1), ResultCode, Rated2}, lists:reverse(Acc2),
					lists:reverse(Acc3)};
		{ok, _, Rated2} when is_list(Rated2), is_list(Rated1) ->
			{{lists:reverse(Acc1), ResultCode,  Rated1 ++ Rated2}, lists:reverse(Acc2),
					lists:reverse(Acc3)};
		{ok, {pla_ref, #price{} = _Price}} ->
			{{lists:reverse(Acc1), ResultCode, Rated1}, lists:reverse(Acc2),
					lists:reverse(Acc3)};
		{error, Reason} ->
			{error, Reason}
	end;
rate(_, _, _, _, _, _, _, _, [], [], [], [], undefined, undefined) ->
	{{[], ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED', undefined}, []};
rate(_, _, _, _, _, _, _, _, [], Acc1, Acc2, Acc3, ResultCode, undefined) ->
	{{lists:reverse(Acc1), ResultCode, undefined}, lists:reverse(Acc2), lists:reverse(Acc3)};
rate(_, _, _, _, _, _, _, _, [], Acc1, Acc2, Acc3, ResultCode, Rated) ->
	{{lists:reverse(Acc1), ResultCode, Rated}, lists:reverse(Acc2), lists:reverse(Acc3)}.

-spec match_tariff(Tariffs, MSCCs) -> Result
	when
		Tariffs:: [map()],
		MSCCs :: [{ServiceIdentifier, RatingGroup,
				UsedAmounts, ReserveAmounts}],
		ServiceIdentifier :: [pos_integer()],
		RatingGroup :: [pos_integer()],
		UsedAmounts :: [{Units, pos_integer()}],
		ReserveAmounts :: [{Units, pos_integer()}],
		Units :: octets | seconds | messages,
		Result :: Prices | {error, missing_tariff},
		Prices :: [Price],
		Price :: map().
%% @doc Match a Tariff object with an MSCC
match_tariff(Tariffs, MSCCs) ->
	match_tariff(Tariffs, MSCCs, []).
%% @hidden
match_tariff(Tariffs, [H | T], Acc) ->
	F = fun F([#{currentTariff := #{currencyCode := Currency,
					rateElements := #{unitType := Units,
					unitSize := UnitSize, unitCost := UnitPrice}},
					ratingGroup := RG1, serviceId := SI1} | _],
					{[SI], [RG], Debits, Reserves})
						when RG1 =:= RG, SI1 =:= SI ->
					#{rg => RG1, si => SI1, currency => Currency,
							units => Units, unitSize => UnitSize,
							unitPrice => UnitPrice, debits => Debits,
							reserves => Reserves};
			F([#{currentTariff := #{currencyCode := Currency,
					rateElements := #{unitType := Units,
					unitSize := UnitSize, unitCost := UnitPrice}},
					serviceId := SI1} | _],
					{[SI], [], Debits, Reserves})
						when SI1 =:= SI ->
					#{rg => undefined, si => SI1, currency => Currency,
							units => Units, unitSize => UnitSize,
							unitPrice => UnitPrice, debits => Debits,
							reserves => Reserves};
			F([#{currentTariff := #{currencyCode := Currency,
					rateElements := #{unitType := Units,
					unitSize := UnitSize, unitCost := UnitPrice}},
					ratingGroup := RG1} | _],
					{[], [RG], Debits, Reserves})
						when RG1 =:= RG ->
					#{rg => RG1, si => undefined, currency => Currency,
							units => Units, unitSize => UnitSize,
							unitPrice => UnitPrice, debits => Debits,
							reserves => Reserves};
			F([_ | T1], MSCC) ->
				F(T1, MSCC);
			F([], _MSCC) ->
				{error, missing_tariff}
	end,
	case F(Tariffs, H) of
				#{} = Charge ->
					match_tariff(Tariffs, T, [Charge | Acc]);
				{error, missing_tariff} ->
					{error, missing_tariff}
	end;
match_tariff(_Tariffs, [], Acc) ->
	lists:reverse(Acc).

-spec charge(SubscriberIDs, SessionId, Flag, Prices) -> Result
	when
		SubscriberIDs :: [SubscriberID],
		SubscriberID :: binary(),
		SessionId :: binary(),
		Flag :: initial | interim | final | event,
		Prices :: [Price],
		Price :: map(),
		Result :: {ok, MSCCs, ResultCode}
				| {error, Reason},
		ResultCode :: pos_integer(),
		Reason :: term(),
		MSCCs :: [MSCC],
		MSCC :: map().
%% @doc Rate all the MSCCs.
%% @hidden
charge(SubscriberIDs, SessionId, Flag, Prices) ->
	charge1(SubscriberIDs, SessionId, Flag, Prices, [], undefined).
%% @hidden
charge1(SubscriberIDs, SessionId, Flag, [#{rg := RG, si := SI,
		units := Units, unitSize := UnitSize, unitPrice := UnitPrice,
		debits := Debits, reserves := Reserves} | T], Acc, ResultCode1) ->
	case ocs_rating:charge(diameter, Flag, SubscriberIDs, SI, RG,
			[], [], [], Debits, Reserves, [{'Session-Id', SessionId}]) of
		{ok, _, {_, Amount} = GrantedAmount} when Amount > 0 ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			MSCC = #{grantedUnit => granted_unit(GrantedAmount),
					serviceId => SI, ratingGroup => RG,
					resultCode => ResultCode2},
			charge1(SubscriberIDs, SessionId, Flag, T, [MSCC | Acc], ResultCode2);
		{ok, _, {_, 0} = _GrantedAmount} ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			charge1(SubscriberIDs, SessionId, Flag, T, Acc, ResultCode2);
		{ok, _, {_, Amount} = GrantedAmount, _} when Amount > 0 ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			MSCC = #{grantedUnit => granted_unit(GrantedAmount),
					serviceId => SI, ratingGroup => RG,
					resultCode => ResultCode2},
			charge1(SubscriberIDs, SessionId, Flag, T, [MSCC | Acc], ResultCode2);
		{ok, #service{}, _} ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			charge1(SubscriberIDs, SessionId, Flag, T, Acc, ResultCode2);
		{out_of_credit, RedirectServerAddress, _SessionList} ->
			ResultCode2 = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
			ResultCode3 = case ResultCode1 of
				undefined ->
					ResultCode2;
				ResultCode1 ->
					ResultCode1
			end,
			MSCC = case RedirectServerAddress of
				undefined ->
					#{serviceId => SI, ratingGroup => RG,
							resultCode => ResultCode2};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#{serviceId => SI, ratingGroup => RG,
							finalUnitIndication => fui(RedirectServerAddress),
							resultCode => ResultCode2}
			end,
			charge1(SubscriberIDs, SessionId, Flag, T, [MSCC | Acc], ResultCode3);
		{disabled, _SessionList} ->
			{ok, Acc, ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED'};
		{error, service_not_found} ->
			{ok, Acc, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN'};
		{error, Reason} ->
			{error, Reason}
	end;
charge1(_, _, _, [], [], undefined) ->
	{ok, [], ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED'};
charge1(_, _, _, [], Acc, ResultCode) ->
	{ok, lists:reverse(Acc), ResultCode}.

%% @hidden
destination(<<"tel:", Dest/binary>>) ->
	binary_to_list(Dest);
destination(Dest) ->
	binary_to_list(Dest).

%% @hidden
granted_unit({seconds, CCTime}) ->
	#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [CCTime]};
granted_unit({octets, TotalVolume}) ->
	#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [TotalVolume]};
granted_unit({messages, SpecUnits}) ->
	#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [SpecUnits]}.

-spec fui(RedirectServerAddress) -> Result
	when
		RedirectServerAddress :: string(),
		Result :: [#'3gpp_ro_Final-Unit-Indication'{}].
%% @doc Parse redirect server address.
%% @private
fui(RedirectServerAddress)
		when is_list(RedirectServerAddress) ->
	AddressType = case inet:parse_address(RedirectServerAddress) of
		{ok, Address} when size(Address) =:= 4 ->
			?'3GPP_RO_REDIRECT-ADDRESS-TYPE_IPV4_ADDRESS';
		{ok, Address} when size(Address) =:= 8 ->
			?'3GPP_RO_REDIRECT-ADDRESS-TYPE_IPV6_ADDRESS';
		{error, _} ->
			case RedirectServerAddress of
				[$s, $i, $p, $: | _]  ->
					?'3GPP_RO_REDIRECT-ADDRESS-TYPE_SIP_URI';
				[$h, $t, $t, $p, $: | _]  ->
					?'3GPP_RO_REDIRECT-ADDRESS-TYPE_URL';
				_ ->
					[]
			end
	end,
	RedirectServer = #'3gpp_ro_Redirect-Server'{'Redirect-Address-Type' = AddressType,
			'Redirect-Server-Address' = RedirectServerAddress},
	Action = ?'3GPP_RO_FINAL-UNIT-ACTION_REDIRECT',
	[#'3gpp_ro_Final-Unit-Indication'{'Final-Unit-Action' = Action,
			'Redirect-Server' = [RedirectServer]}].

-spec accounting_event_type(RequestType) -> EventType
	when
	RequestType :: 1..4,
	EventType :: start | interim | stop | event.
%% @doc Converts CC-Request-Type integer value to a readable atom.
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST') -> start;
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST') -> interim;
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST') -> stop;
accounting_event_type(?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST') -> event.

%% @hidden
print_sub(SubscriberIDs) ->
	print_sub(SubscriberIDs, []).
%% @hidden
print_sub([H | T], []) ->
	print_sub(T, binary_to_list(H));
print_sub([H | T], Acc) ->
	print_sub(T, Acc ++ "," ++ binary_to_list(H));
print_sub([], Acc) ->
	Acc.

