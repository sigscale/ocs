%%% ocs_diameter_3gpp_ro_nrf_app_cb.erl
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
%%% 	for the 3GPP DIAMETER Ro in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS 29.299 Diameter charging applications
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%% 	RFC4006 - DIAMETER Credit-Control Application </a>
%%%
-module(ocs_diameter_3gpp_ro_nrf_app_cb).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
			prepare_retransmit/3, handle_answer/4, handle_error/4,
			handle_request/3]).
-export([content_types_accepted/0, content_types_provided/0]).

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

-spec content_types_accepted() -> ContentTypes
   when
      ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/merge-patch+json",
			"application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

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
			Class = get_option(class),
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
				'Subscription-Id' = SubscriptionId} = Request, Class) ->
	try
		SubscriberIds = case subscriber_id(SubscriptionId) of
			SubIds when length(SubIds) > 0,
				SubIds;
			[] ->
				case UserName of
					[] ->
						throw(no_subscriber_identification_information);
					[NAI] ->
						case string:tokens(binary_to_list(NAI), ":@") of
							[_Proto, User, _Domain] ->
								User;
							[User, _Domain] ->
								User;
							[User] ->
								User;
							_ ->
								throw(no_subscriber_identification_information)
						end
				end
		end,
		process_request1(RequestType, Request, SessionId, RequestNum,
				SubscriberIds, OHost, DHost, ORealm, DRealm, IpAddress, Port, Class)
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
		SubscriberIds, OHost, _DHost, ORealm, _DRealm, _IpAddress, _Port, b = _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case service_type(SvcContextId) of
			32251 ->
				post_request_scur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, initial, b);
			32260 ->
				post_request_ecur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, Destination, initial, b)
		end
	of
		{ok, JSON} ->
			{struct, RatedStruct} = mochijson:decode(JSON),
			{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
			{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
			Container = build_container(MSCC1),
			NewMSCC = build_mscc(ServiceRating, Container),
			diameter_answer(SessionId, NewMSCC, ResultCode,
					OHost, ORealm, RequestType, RequestNum);
		{error, Reason} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, Reason}]),
			diameter_error(SessionId,
					?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
					OHost, ORealm, RequestType, RequestNum)
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId, RequestNum,
		SubscriberIds, OHost, _DHost, ORealm, _DRealm, _IpAddress, _Port, _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case service_type(SvcContextId) of
			32251 ->
				post_request_scur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, initial, b);
			32260 ->
				post_request_ecur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, Destination, initial, b)
		end
	of
		{ok, JSON} ->
			{struct, RatedStruct} = mochijson:decode(JSON),
			{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
			{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
			Container = build_container(MSCC1),
			NewMSCC = build_mscc(ServiceRating, Container),
			diameter_answer(SessionId, NewMSCC, ResultCode,
					OHost, ORealm, RequestType, RequestNum);
		{error, Reason} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, Reason}]),
			diameter_error(SessionId,
					?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
					OHost, ORealm, RequestType, RequestNum)
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, b = _Class) when length(MSCC1) > 0 ->
	try
		Location = get_service_location(ServiceInformation),
		case post_request_scur(SubscriberIds, SvcContextId,
				SessionId, MSCC1, Location, interim, b) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC1),
				NewMSCC = build_mscc(ServiceRating, Container),
				diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum);
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason}]),
				diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum)
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
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, _Class) when length(MSCC1) > 0 ->
	try
		Location = get_service_location(ServiceInformation),
		case post_request_scur(SubscriberIds, SvcContextId,
				SessionId, MSCC1, Location, interim, a) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC1),
				NewMSCC = build_mscc(ServiceRating, Container),
				diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum);
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason}]),
				diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum)
		end
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, b = _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case service_type(SvcContextId) of
			32251 ->
				post_request_scur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, final, b);
			32260 ->
				post_request_ecur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, Destination, final, b)
		end
	of
		{ok, JSON} ->
			{struct, RatedStruct} = mochijson:decode(JSON),
			{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
			{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
			Container = build_container(MSCC1),
			NewMSCC = build_mscc(ServiceRating, Container),
			ok = remove_ref(SessionId),
			diameter_answer(SessionId, NewMSCC, ResultCode,
					OHost, ORealm, RequestType, RequestNum);
		{error, ReasonCode} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, ReasonCode}]),
			ok = remove_ref(SessionId),
			diameter_error(SessionId, ReasonCode,
					OHost, ORealm, RequestType, RequestNum)
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case service_type(SvcContextId) of
			32251 ->
				post_request_scur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, final, a);
			32260 ->
				post_request_ecur(SubscriberIds, SvcContextId,
						SessionId, MSCC1, Location, Destination, final, a)
		end
	of
		{ok, JSON} ->
			{struct, RatedStruct} = mochijson:decode(JSON),
			{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
			{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
			Container = build_container(MSCC1),
			NewMSCC = build_mscc(ServiceRating, Container),
			ok = remove_ref(SessionId),
			diameter_answer(SessionId, NewMSCC, ResultCode,
					OHost, ORealm, RequestType, RequestNum);
		{error, ReasonCode} ->
			error_logger:error_report(["Rating Error",
					{module, ?MODULE}, {error, ReasonCode}]),
			ok = remove_ref(SessionId),
			diameter_error(SessionId, ReasonCode,
					OHost, ORealm, RequestType, RequestNum)
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
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [],
		'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, b = _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case post_request_iec(SubscriberIds, SvcContextId,
				SessionId, [], Location, Destination) of
			{ok, JSON} ->
				{struct, _RatedStruct} = mochijson:decode(JSON),
				diameter_answer(SessionId, [], ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum);
			{error, ReasonCode} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, ReasonCode}]),
				ok = remove_ref(SessionId),
				diameter_error(SessionId, ReasonCode,
						OHost, ORealm, RequestType, RequestNum)
		end
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [],
		'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case post_request_iec(SubscriberIds, SvcContextId,
				SessionId, [], Location, Destination) of
			{ok, JSON} ->
				{struct, _RatedStruct} = mochijson:decode(JSON),
				diameter_answer(SessionId, [], ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum);
			{error, ReasonCode} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, ReasonCode}]),
				ok = remove_ref(SessionId),
				diameter_error(SessionId, ReasonCode,
						OHost, ORealm, RequestType, RequestNum)
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
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, b = _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case post_request_iec(SubscriberIds, SvcContextId,
				SessionId, MSCC, Location, Destination) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC),
				NewMSCC = build_mscc(ServiceRating, Container),
				ok = remove_ref(SessionId),
				diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum);
			{error, ReasonCode} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, ReasonCode}]),
				ok = remove_ref(SessionId),
				diameter_error(SessionId, ReasonCode,
						OHost, ORealm, RequestType, RequestNum)
		end
	catch
		_:Reason1 ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason1}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC,
		'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		_IpAddress, _Port, _Class) ->
	try
		Location = get_service_location(ServiceInformation),
		Destination = get_destination(ServiceInformation),
		case post_request_iec(SubscriberIds, SvcContextId,
				SessionId, MSCC, Location, Destination) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{ServiceRating, ResultCode} = map_service_rating(ServiceElements, SessionId),
				Container = build_container(MSCC),
				NewMSCC = build_mscc(ServiceRating, Container),
				ok = remove_ref(SessionId),
				diameter_answer(SessionId, NewMSCC, ResultCode,
						OHost, ORealm, RequestType, RequestNum);
			{error, ReasonCode} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, ReasonCode}]),
				ok = remove_ref(SessionId),
				diameter_error(SessionId, ReasonCode,
						OHost, ORealm, RequestType, RequestNum)
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

-spec subscriber_id(SubscriberIdAVP) -> SubscriberIds
	when
		SubscriberIdAVP :: [#'3gpp_ro_Subscription-Id'{}],
		SubscriberId :: [SubscriberIds] | [],
		SubscriberIds :: MSISDN | IMSI,
		MSISDN :: {msisdn, Id},
		IMSI:: {imsi, id},
		Id :: binary()
%% @doc Get SubscriberIds From Diameter SubscriberId AVP.
subscriber_id(SubscriberIdAVP) ->
	subscriber_id(SubscriberIdAVP, []).
%% @hidden
subscriber_id([#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = undefined,
		'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164'} | T], Acc) ->
	subscriber_id(T, Acc);
subscriber_id([#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = undefined,
		'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI'} | T], Acc) ->
	subscriber_id(T, Acc);
subscriber_id([#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = SubId,
		'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164'} | T], Acc) ->
	subscriber_id(T, [{msisdn, SubId} | Acc]);
subscriber_id([#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = SubId,
		'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI'} | T], Acc) ->
	subscriber_id(T, [{imsi, SubId} | Acc]);
subscriber_id([_ | T], Acc) ->
	subscriber_id(T, Acc);
subscriber_id([], Acc) ->
	lists:reverse(Acc).

-spec post_request_ecur(SubscriberIds, ServiceContextId,
		SessionId, MSCC, Location, Destination, Flag, Class) -> Result
	when
		SubscriberIds :: {MSISDN, IMSI},
		MSISDN :: binary(),
		IMSI :: binary(),
		ServiceContextId :: binary(),
		SessionId :: binary(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		Location :: [tuple()],
		Flag :: initial | interim | final,
		Class :: a | b,
		Destination :: string(),
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST ECUR rating data to a Nrf Rating Server.
post_request_ecur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, Destination, initial, a) ->
	Path = get_option(nrf_uri) ++ ?BASE_URI,
	ServiceRating = initial_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, Destination, a),
	post_request_ecur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_ecur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, Destination, initial, b) ->
	Path = get_option(nrf_uri) ++ ?BASE_URI,
	ServiceRating = initial_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, Destination, b),
	post_request_ecur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_ecur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, Destination, final, a) ->
	Path = get_option(nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "release",
	ServiceRating = final_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, Destination, a),
	post_request_ecur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_ecur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, Destination, final, b) ->
	Path = get_option(nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "release",
	ServiceRating = final_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, Destination, b),
	post_request_ecur1(SubscriberIds, SessionId, ServiceRating, Path).
%% @hidden
post_request_ecur1(SubscriberIds, SessionId, ServiceRating, Path) ->
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
					{"subscriptionId", {array, charged_parties(SubscriberIds)}},
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
			insert_ref(SessionId, Location1),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, 200, _ReasonPhrase}, _Headers, Body1}} ->
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
	end.

-spec post_request_iec(SubscriberIds, ServiceContextId,
		SessionId, MSCC, Location, Destination) -> Result
	when
		SubscriberIds :: {MSISDN, IMSI},
		MSISDN :: binary(),
		IMSI :: binary(),
		ServiceContextId :: binary(),
		SessionId :: binary(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}] | [],
		Location :: [tuple()],
		Destination :: string(),
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST IEC rating data to a Nrf Rating Server.
post_request_iec(SubscriberIds, ServiceContextId, SessionId,
		[], _Location, Destination) ->
	SCID = {"serviceContextId", binary_to_list(ServiceContextId)},
	Des = {"destinationId", {array, [{struct, [{"destinationIdType", "DN"},
			{"destinationIdData", Destination}]}]}},
	ServiceRating = [{struct, [SCID, Des, {"requestSubType", "DEBIT"}]}],
	post_request_iec1(SubscriberIds, SessionId, ServiceRating);
post_request_iec(SubscriberIds, ServiceContextId, SessionId, MSCC, Location, Destination) ->
	ServiceRating = iec_service_rating(MSCC, binary_to_list(ServiceContextId), Location, Destination),
	post_request_iec1(SubscriberIds, SessionId, ServiceRating).
%% @hidden
post_request_iec1(SubscriberIds, SessionId, ServiceRating) ->
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
					{"subscriptionId", {array, charged_parties(SubscriberIds)}},
					{"serviceRating",
							{array, lists:flatten(ServiceRating)}}]},
	Path = get_option(nrf_uri) ++ ?BASE_URI,
	ContentType = "application/json",
	RequestBody = mochijson:encode(Body),
	Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
	Request = {Path, Headers, ContentType, lists:flatten(RequestBody)},
	Options = [{relaxed, true}],
	case httpc:request(post, Request, Options, [], Profile) of
		{_RequestId, {{_HttpVersion, 201, _ReasonPhrase}, Headers1, Body1}} ->
			{_, Location1} = lists:keyfind("location", 1, Headers1),
			insert_ref(SessionId, Location1),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
	end.

-spec post_request_scur(SubscriberIds, ServiceContextId,
		SessionId, MSCC, Location, Flag, Class) -> Result
	when
		SubscriberIds :: {MSISDN, IMSI},
		MSISDN :: binary(),
		IMSI :: binary(),
		ServiceContextId :: binary(),
		SessionId :: binary(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		Location :: [tuple()],
		Flag :: initial | interim | final,
		Class :: a | b,
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST SCUR rating data to a Nrf Rating Server.
post_request_scur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, initial, a) ->
	Path = get_option(nrf_uri) ++ ?BASE_URI,
	ServiceRating = initial_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, undefined, a),
	post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_scur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, initial, b) ->
	Path = get_option(nrf_uri) ++ ?BASE_URI,
	ServiceRating = initial_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, undefined, b),
	post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_scur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, interim, a) ->
	Path = get_option(nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "update",
	ServiceRating = update_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, a),
	post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_scur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, interim, b) ->
	Path = get_option(nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "update",
	ServiceRating = update_service_rating(MSCC, binary_to_list(SvcContextId),
			Location, b),
	post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_scur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, final, a) ->
	Path = get_option(nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "release",
	ServiceRating = final_service_rating(MSCC, binary_to_list(SvcContextId),
			undefined, Location, a),
	post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path);
post_request_scur(SubscriberIds, SvcContextId,
		SessionId, MSCC, Location, final, b) ->
	Path = get_option(nrf_uri) ++ get_ref(SessionId) ++ "/" ++ "release",
	ServiceRating = final_service_rating(MSCC, binary_to_list(SvcContextId),
			undefined, Location, b),
	post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path).
%% @hidden
post_request_scur1(SubscriberIds, SessionId, ServiceRating, Path) ->
	{ok, Profile} = application:get_env(ocs, nrf_profile),
	TS = erlang:system_time(millisecond),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	Sequence = ets:update_counter(counters, nrf_seq, 1),
	Body = {struct, [{"nfConsumerIdentification",
							{struct, [{"nodeFunctionality", "OCF"}]}},
					{"invocationTimeStamp", InvocationTimeStamp},
					{"invocationSequenceNumber", Sequence},
					{"subscriptionId", {array, charged_parties(SubscriberIds)}},
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
			insert_ref(SessionId, Location1),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, 200, _ReasonPhrase}, _Headers, Body1}} ->
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
	end.

-spec charged_parties(Subscribers) -> SubscriptionId
	when
		Subscribers :: [Subscriber]
		Subscriber :: {Type, Id},
		Type :: msisdn | imsi,
		Id :: binary(),
		ChargedParties :: [ChargedParty],
		ChargedParty :: MSISDN | IMSI,
		MSISDN :: string(),
		IMSI :: string().
%% @doc Format subsriber ids for nrf request.
charged_parties(Subscribers) ->
	charged_parties(Subscribers, []).
%% @hidden
charged_parties([{msisdn, MSISDN} | T], Acc) ->
	charged_parties(T, ["msisdn-" ++ binary_to_list(MSISDN) | Acc]);
charged_parties([{imsi, IMSI} | T], Acc) ->
	charged_parties(T, ["imsi-" ++ binary_to_list(IMSI) | Acc]);
charged_parties([_ | T], Acc) ->
	charged_parties(T, Acc);
charged_parties([], Acc) ->
	lists:reverse(Acc).

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

-spec insert_ref(SessionId, SessionState) -> Result
	when
		SessionId :: binary(),
		SessionState :: Location | ServiceRating,
		Location :: string(),
		ServiceRating :: [map()],
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Insert a rating Data ref.
%% @hidden
insert_ref(SessionId, SessionState)
		when is_list(SessionState), is_binary(SessionId) ->
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

-spec build_mscc(ServiceRating, Container) -> Result
	when
		ServiceRating :: [map()],
		Container :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		Result :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}].
%% @doc Build a list of CCA MSCCs
build_mscc(ServiceRating, Container) ->
	build_mscc(ServiceRating, [], Container).
%% @hidden
build_mscc([H | T], Acc, Container) ->
	F = fun F(#{"serviceId" := SI, "ratingGroup" := RG, "resultCode" := RC} = ServiceRating,
			[#'3gpp_ro_Multiple-Services-Credit-Control'
					{'Service-Identifier' = [SI], 'Rating-Group' = [RG]} = MSCC1 | _]) ->
				MSCC2 = case catch maps:get("grantedUnit", ServiceRating) of
					#'3gpp_ro_Granted-Service-Unit'{} = GrantedUnits ->
						MSCC1#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = [GrantedUnits]};
					_ ->
						MSCC1
				end,
				MSCC3 = case catch maps:get("consumedUnit", ServiceRating) of
					#'3gpp_ro_Used-Service-Unit'{} = UsedUnits ->
						MSCC2#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' = [UsedUnits]};
					_ ->
						MSCC2
				end,
				MSCC4 = MSCC3#'3gpp_ro_Multiple-Services-Credit-Control'{'Result-Code' = [RC]},
				MSCC4;
		F(#{"serviceId" := SI, "resultCode" := RC } = ServiceRating,
			[#'3gpp_ro_Multiple-Services-Credit-Control'
					{'Service-Identifier' = [SI], 'Rating-Group' = []} = MSCC1 | _]) ->
			MSCC2 = case catch maps:get("grantedUnit", ServiceRating) of
				#'3gpp_ro_Granted-Service-Unit'{} = GrantedUnits ->
					MSCC1#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = [GrantedUnits]};
				_ ->
					MSCC1
			end,
			MSCC3 = case catch maps:get("consumedUnit", ServiceRating) of
				#'3gpp_ro_Used-Service-Unit'{} = UsedUnits ->
					MSCC2#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' = [UsedUnits]};
				_ ->
					MSCC2
			end,
			MSCC4 = MSCC3#'3gpp_ro_Multiple-Services-Credit-Control'{'Result-Code' = [RC]},
			MSCC4;
		F(#{"ratingGroup" := RG, "resultCode" := RC} = ServiceRating,
				[#'3gpp_ro_Multiple-Services-Credit-Control'
						{'Service-Identifier' = [], 'Rating-Group' = [RG]} = MSCC1 | _]) ->
			MSCC2 = case catch maps:get("grantedUnit", ServiceRating) of
				#'3gpp_ro_Granted-Service-Unit'{} = GrantedUnits ->
					MSCC1#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = [GrantedUnits]};
				_ ->
					MSCC1
			end,
			MSCC3 = case catch maps:get("consumedUnit", ServiceRating) of
				#'3gpp_ro_Used-Service-Unit'{} = UsedUnits ->
					MSCC2#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' = [UsedUnits]};
				_ ->
					MSCC2
			end,
			MSCC4 = MSCC3#'3gpp_ro_Multiple-Services-Credit-Control'{'Result-Code' = [RC]},
			MSCC4;
		F(ServiceRating, [_H | T1]) ->
			F(ServiceRating, T1)
	end,
	NewMSCC = F(H, Container),
	build_mscc(T, [NewMSCC | Acc], Container);
build_mscc([], Acc, _Container) ->
	lists:reverse(Acc).

-spec map_service_rating(ServiceRating, SessionId) -> Result
	when
		ServiceRating :: [{struct, [tuple()]}],
		SessionId :: binary() | undefined,
		Result :: {[map()], ResultCode} | {[], ResultCode},
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
			Acc2 = Acc1#{"grantedUnit" => granted_units(Units)},
			F(T1, Acc2);
		F([{"consumedUnit", {_, Units}} | T1], Acc1) ->
			Acc2 = Acc1#{"consumedUnit" => used_units(Units)},
			F(T1, Acc2);
		F([{"currentTariff", {_, TariffElements}} | T1], Acc1) ->
			Acc2 = Acc1#{"tariffElement" => tariff_element(TariffElements)},
			F(T1, Acc2);
		F([{"resultCode", RC1} | T1], Acc1) ->
			{NewRC1, NewRC2} = result_code(RC1, RC2),
			Acc2 = Acc1#{"resultCode" => NewRC1,
					"finalResultCode" => NewRC2},
			F(T1, Acc2);
		F([{Name, Value} | T1], Acc1) ->
			F(T1, Acc1#{Name => Value});
		F([], Acc1) ->
			Acc1
	end,
	ServiceRatingMap = F(Elements, #{}),
	NewRC2 = maps:get("finalResultCode", ServiceRatingMap),
	map_service_rating(T, NewRC2, [ServiceRatingMap | Acc]);
map_service_rating([], NewRC2, Acc) ->
	{lists:reverse(Acc), NewRC2}.

%% @hidden
tariff_element(Elements) ->
	tariff_element(Elements, #{}).
tariff_element([{"currencyCode", CurrencyCode} | T], Acc) ->
	tariff_element(T, Acc#{"currencyCode" => CurrencyCode});
tariff_element([{"rateElement", {_,[{_, RateElements}]}} | T], Acc) ->
	RateElements1 = rate_elements(RateElements, #{}),
	tariff_element(T, Acc#{"rateElements" => RateElements1});
tariff_element([_H | T], Acc) ->
	tariff_element(T, Acc);
tariff_element([], Acc) ->
	Acc.

%% @hidden
rate_elements([{"unitType", UnitType} | T], Acc) ->
	rate_elements(T, Acc#{"unitType" => unit_type(UnitType)});
rate_elements([{"unitSize", {_, [{"valueDigits", ValueDigits}]}} | T], Acc) ->
	rate_elements(T, Acc#{"unitSize" => ValueDigits});
rate_elements([{"unitSize", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent >= 0 ->
	rate_elements(T, Acc#{"unitSize" => ValueDigits * (Exponent * 10) * 10000});
rate_elements([{"unitSize", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent < 0 ->
	rate_elements(T, Acc#{"unitSize" => ValueDigits div (Exponent * 10) * 10000});
rate_elements([{"unitCost", {_, [{"valueDigits", ValueDigits}]}} | T], Acc) ->
	rate_elements(T, Acc#{"unitCost" => ValueDigits});
rate_elements([{"unitCost", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent >= 0 ->
	rate_elements(T, Acc#{"unitCost" => ValueDigits * (Exponent * 10) * 10000});
rate_elements([{"unitCost", {_, [{"valueDigits", ValueDigits},
		{"exponent", Exponent}]}} | T], Acc) when Exponent < 0 ->
	rate_elements(T, Acc#{"unitCost" => ValueDigits div (Exponent * 10) * 10000});
rate_elements([_H | T], Acc) ->
	rate_elements(T, Acc);
rate_elements([], Acc) ->
	Acc.

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
		'Requested-Service-Unit' = RSU, 'Service-Identifier' = SI,
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
	ServiceRating = case reserved_unit(RSU) of
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
used_unit([]) ->
	[];
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
reserved_unit([]) ->
	[];
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

-spec initial_service_rating(MSCC, ServiceContextId, ServiceInformation,
		Destination, Class) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: string(),
		ServiceInformation :: [tuple()],
		Destination :: string() | undefined,
		Class :: a | b,
		ServiceRating :: [{struct, [tuple()]}].
%% Create list of service elements to be rated.
initial_service_rating(MSCC, ServiceContextId, ServiceInformation, undefined, Class) ->
	SCID = {"serviceContextId", ServiceContextId},
	initial_service_rating(MSCC, SCID, ServiceInformation, undefined, Class, []);
initial_service_rating(MSCC, ServiceContextId, ServiceInformation, Destination, Class) ->
	SCID = {"serviceContextId", ServiceContextId},
	Des = {"destinationId", {array, [{struct, [{"destinationIdType", "DN"},
			{"destinationIdData", Destination}]}]}},
	initial_service_rating(MSCC, SCID, ServiceInformation, Des, Class, []).
%% @hidden
initial_service_rating([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = RSU, 'Service-Identifier' = SI,
		'Rating-Group' = RG} = _MSCC | T], SCID, SInfo, Des, Class, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			[];
		[N2] ->
			[{"ratingGroup", N2} | Acc1]
	end,
	Acc3 = case SInfo of
		[] ->
			Acc2;
		_ ->
			[SInfo | Acc2]
	end,
	Acc4 = case Des of
	 undefined ->
			Acc3;
		_ ->
			[Des | Acc3]
	end,
	case Class of
	a ->
		initial_service_rating(T, SCID, SInfo, Des, Class, [{struct, [SCID | Acc4]}| Acc]);
	b ->
		case reserved_unit(RSU) of
			[] ->
				ServiceRating = {struct, [SCID, {"requestSubType", "RESERVE"} | Acc4]},
				initial_service_rating(T, SCID, SInfo, Des, Class, [ServiceRating | Acc]);
			ReservedUnits ->
				ServiceRating = {struct, [SCID, {"requestedUnit", {struct, ReservedUnits}},
						{"requestSubType", "RESERVE"} | Acc4]},
				initial_service_rating(T, SCID, SInfo, Des, Class, [ServiceRating | Acc])
		end
	end;
initial_service_rating([], _SCID, _SI, _Des, _Class, Acc) ->
	Acc.

-spec update_service_rating(MSCC, ServiceContextId,
		ServiceInformation, Class) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: string(),
		ServiceInformation :: [tuple()],
		Class :: a | b,
		ServiceRating :: [{struct, [tuple()]}].
%% Create list of service elements to be rated.
update_service_rating(MSCC, ServiceContextId, ServiceInformation, Class) ->
	SCID = {"serviceContextId", ServiceContextId},
	update_service_rating1(MSCC, SCID, ServiceInformation, Class, []).
%% @hidden
update_service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = RSU, 'Used-Service-Unit' = USU,
		'Service-Identifier' = SI, 'Rating-Group' = RG} = _MSCC | T], SCID,
		SInfo, Class, Acc) ->
	Acc1 = case SI of
		[] ->
			[];
		[N1] ->
			[{"serviceId", N1}]
		end,
	Acc2 = case RG of
		[] ->
			[];
		[N2] ->
			[{"ratingGroup", N2} | Acc1]
	end,
	Acc3 = case SInfo of
		[] ->
			Acc2;
		_ ->
			[SInfo | Acc2]
	end,
	case Class of
	a ->
		update_service_rating1(T, SCID, SInfo, Class, [{struct, [SCID | Acc3]} | Acc]);
	b ->
		case {used_unit(USU), reserved_unit(RSU)} of
			{[], []} ->
				ServiceRating = {struct, [SCID, {"requestSubType", "RESERVE"} | Acc3]},
				update_service_rating1(T, SCID, SInfo, Class, [ServiceRating | Acc]);
			{ConsumedUnits, []} ->
				ServiceRating = {struct, [SCID, {"consumedUnit", {struct, ConsumedUnits}},
						{"requestSubType", "DEBIT"} | Acc3]},
				update_service_rating1(T, SCID, SInfo, Class, [ServiceRating | Acc]);
			{[], ReservedUnits} ->
				ServiceRating = {struct, [SCID, {"requestedUnit", {struct, ReservedUnits}},
						{"requestSubType", "RESERVE"} | Acc3]},
				update_service_rating1(T, SCID, SInfo, Class, [ServiceRating | Acc])
		end
	end;
update_service_rating1([], _SCID, _SI, _Class, Acc) ->
	Acc.

-spec final_service_rating(MSCC, ServiceContextId,
		Destination, ServiceInformation, Class) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: string(),
		ServiceInformation :: [tuple()],
		Destination :: string() | undefined,
		Class :: a | b,
		ServiceRating :: [{struct, [tuple()]}].
%% Create list of service elements to be rated.
final_service_rating(MSCC, ServiceContextId, undefined, ServiceInformation, Class) ->
	SCID = {"serviceContextId", ServiceContextId},
	final_service_rating1(MSCC, SCID, ServiceInformation, undefined, Class, []);
final_service_rating(MSCC, ServiceContextId, ServiceInformation, Destination, Class) ->
	SCID = {"serviceContextId", ServiceContextId},
	Des = {"destinationId", {array, [{struct, [{"destinationIdType", "DN"},
			{"destinationIdData", Destination}]}]}},
	final_service_rating1(MSCC, SCID, ServiceInformation, Des, Class, []).
%% @hidden
final_service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = USU, 'Service-Identifier' = SI,
		'Rating-Group' = RG} = _MSCC | T], SCID, SInfo, Des, Class, Acc) ->
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
	Acc4 = case Des of
		undefined ->
			Acc3;
		_ ->
			[Des | Acc3]
	end,
	case Class of
	a ->
		final_service_rating1(T, SCID, SInfo, Des, Class, [{struct, [SCID | Acc4]} | Acc]);
	b ->
		case used_unit(USU) of
			[] ->
				ServiceRating = {struct, [SCID, {"requestSubType", "DEBIT"} | Acc4]},
				final_service_rating1(T, SCID, SInfo, Des, Class, [ServiceRating | Acc]);
			ConsumedUnits ->
				ServiceRating = {struct, [SCID, {"consumedUnit", {struct, ConsumedUnits}},
						{"requestSubType", "DEBIT"} | Acc4]},
				final_service_rating1(T, SCID, SInfo, Des, Class, [ServiceRating | Acc])
		end
	end;
final_service_rating1([], _SCID, _SI, _Des, _Class, Acc) ->
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

-spec get_option(Option) -> Result
	when
		Option :: atom(),
		Result :: term().
%% @doc Get the Nrf endpoint uri.
get_option(Option) ->
	Options = case application:get_env(ocs, diameter) of
		{ok, [{acct,[{_, _, Oplist}]}]} ->
			Oplist;
		{ok, [{auth,[{_, _, Oplist}]}]} ->
			Oplist;
		{ok, [{acct,[{_, _, Oplist}]}, {auth,[{_, _, []}]}]} ->
			Oplist;
		{ok, [{auth,[{_, _, []}]}, {acct,[{_, _, Oplist}]}]} ->
			Oplist
	end,
	case lists:keyfind(Option, 1, Options) of
		{Option, Uri} ->
			Uri;
		false ->
			undefined
	end.

