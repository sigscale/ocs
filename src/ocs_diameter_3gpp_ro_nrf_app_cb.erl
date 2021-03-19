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

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  #diameter_packet{}.
-type message() ::  tuple() | list().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

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
		#'3gpp_ro_CCR'{'Session-Id' = SessionId, 'User-Name' = UserName,
				'Auth-Application-Id' = ?RO_APPLICATION_ID,
				'Service-Context-Id' = _SvcContextId,
				'CC-Request-Type' = RequestType,
				'CC-Request-Number' = RequestNum,
				'Subscription-Id' = SubscriptionId} = Request) ->
	try
		SubscriberIds = case SubscriptionId of
			[#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub1},
					#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub2} | _] ->
				{Sub1, Sub2};
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
				SubscriberIds, OHost, DHost, ORealm, DRealm, IpAddress, Port)
	catch
		_:Reason ->
			error_logger:warning_report(["Unable to process DIAMETER request",
					{origin_host, OHost}, {origin_realm, ORealm},
					{request, Request}, {error, Reason}, {stack, erlang:get_stacktrace()}]),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end.
%% @hidden
process_request1(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId, RequestNum,
		SubscriberIds, OHost, _DHost, ORealm, _DRealm, _IpAddress, _Port) ->
	try
		Location = get_service_location(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		case post_request(SubscriberIds, SvcContextId,
				Timestamp, ServiceType, SessionId, MSCC1, Location, intial) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{NewMSCC, ResultCode} = mscc(ServiceElements, SessionId),
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
process_request1(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) when length(MSCC1) > 0 ->
	try
		Location = get_service_location(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		case post_request(SubscriberIds, SvcContextId,
				Timestamp, ServiceType, SessionId, MSCC1, Location, interim) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{NewMSCC, ResultCode} = mscc(ServiceElements, SessionId),
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
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, SubscriberIds, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) ->
	try
		Location = get_service_location(ServiceInformation),
		ServiceType = service_type(SvcContextId),
		Timestamp = case EventTimestamp of
			[{{_, _, _}, {_, _, _}} = TS] ->
				TS;
			_ ->
				calendar:universal_time()
		end,
		case post_request(SubscriberIds, SvcContextId,
				Timestamp, ServiceType, SessionId, MSCC1, Location, final) of
			{ok, JSON} ->
				{struct, RatedStruct} = mochijson:decode(JSON),
				{_, {_, ServiceElements}} = lists:keyfind("serviceRating", 1, RatedStruct),
				{NewMSCC, ResultCode} = mscc(ServiceElements, SessionId),
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
	end.

-spec post_request(SubscriberIds, ServiceContextId, TimeStamp, ServiceType,
		SessionId, MSCC, Location, Flag) -> Result
	when
		SubscriberIds :: tuple(),
		ServiceContextId :: binary(),
		TimeStamp :: tuple(),
		ServiceType :: string(),
		SessionId :: binary(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		Location :: [tuple()],
		Flag :: intial | interim | final,
		Result :: {ok, Body} | {error, Reason},
		Body :: string(),
		Reason :: term().
%% @doc POST rating data to a Nrf Rating Server.
post_request({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
		SessionId, MSCC, Location, intial) ->
	{ok, NrfUri} = application:get_env(nrf_uri),
	Path = NrfUri ++ ?BASE_URI,
	post_request1({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
			SessionId, MSCC, Location, Path);
post_request({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
		SessionId, MSCC, Location, interim) ->
	{ok, NrfUri} = application:get_env(nrf_uri),
	Path = NrfUri ++ ?BASE_URI ++ "/" ++
			get_ref(SessionId) ++ "/" ++ "update",
	post_request1({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
			SessionId, MSCC, Location, Path);
post_request({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
		SessionId, MSCC, Location, final) ->
	{_, NrfUri} = application:get_env(nrf_uri),
	Path = NrfUri ++ ?BASE_URI ++ "/" ++
			get_ref(SessionId) ++ "/" ++ "release",
	post_request1({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
			SessionId, MSCC, Location, Path).
%% @hidden
post_request1({MSISDN, IMSI}, SvcContextId, TimeStamp, ServiceType,
		SessionId, MSCC, Location, Path) ->
	{ok, Profile} = application:get_env(nrf_profile),
	TS = erlang:system_time(?MILLISECOND),
	InvocationTimeStamp = ocs_log:iso8601(TS),
	Sequence = ets:update_counter(counters, nrf_seq, 1),
	MSISDN1 = "msisdn-" ++ binary_to_list(MSISDN),
	IMSI1 = "imsi-" ++ binary_to_list(IMSI),
	Body = {struct,[{"nfConsumerIdentification",
							{struct, [{"nodeFunctionality", "OCF"}]}},
					{"invocationTimeStamp", InvocationTimeStamp},
					{"invocationSequenceNumber", Sequence},
					{"subscriptionId", {array, [MSISDN1, IMSI1]}},
					{"serviceRating",
							{array, service_rating(MSCC, SvcContextId, Location)}}]},
	ContentType = "application/json",
	RequestBody = mochijson:encode(Body),
	Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
	Request = {Path, Headers, ContentType, lists:flatten(RequestBody)},
	Options = [{relaxed, true}],
   case httpc:request(post, Request, Options, [], Profile) of
		{_RequestId, {{_HttpVersion, 201, _ReasonPhrase}, Headers1, Body1}} ->
			{_, Location1} = lists:keyfind("location", 1, Headers1),
			insert_ref(Location1, SessionId),
			{ok, Body1};
		{_RequestId, {{_HttpVersion, 200, _ReasonPhrase}, _Headers, Body1}} ->
			{ok, Body1};
		{_RequestId, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body1}} ->
			{error, StatusCode};
		{_RequestId, {error, Reason}} ->
			{error, Reason}
   end.

-spec get_service_location(ServiceInformation) -> ServiceInformation
	when
		ServiceInformation :: [#'3gpp_ro_Service-Information'{}] |
				[tuple()].
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
	[{"serviceInformation", {struct, [{"sgsnMccMnc", {struct,
			[{"mcc", MCC}, {"mnc", MNC}]}}]}}];
get_service_location1(<<MCC1, MCC2, MCC3, MNC1, MNC2>>) ->
	MCC = [MCC1, MCC2, MCC3],
	MNC = [MNC1, MNC2],
	[{"serviceInformation", {struct, [{"sgsnMccMnc", {struct,
			[{"mcc", MCC}, {"mnc", MNC}]}}]}}].

-spec insert_ref(Location, SessionId) -> Result
	when
		Location :: string(),
		SessionId :: binary(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Insert a rating Data ref.
%% @hidden
insert_ref(Location, SessionId)
		when is_list(Location), is_binary(SessionId) ->
	case catch ets:insert(?NRF_TABLE, {SessionId, Location}) of
		true ->
			ok;
		{'EXIT', Reason} ->
			{error, Reason}
	end.

-spec get_ref(SessionId) -> Result
	when
		SessionId :: binary(),
		Result :: list().
%% @doc Get a rating data ref
%% @hidden
get_ref(SessionId) ->
	Pattern = {SessionId, '$1'},
	case ets:match_object(?NRF_TABLE, Pattern) of
		[{SessionId, RatingDataRef}] ->
			RatingDataRef;
		_ ->
			[]
	end.

-spec mscc(ServiceElements, SessionId) -> Result
	when
		ServiceElements :: list(),
		SessionId :: binary(),
		Result :: {[MSCC], ResultCode} | {error, Reason ::term()},
		MSCC :: #'3gpp_ro_Multiple-Services-Credit-Control'{},
		ResultCode :: pos_integer().
%% @doc Build a list of rated MSCCs.
mscc(ServiceElementsd, SessionId) ->
	mscc(ServiceElementsd, undefined, SessionId, []).
%% @hidden
mscc([{struct, Element} | T], ResultCode, SessionId, Acc) ->
	Acc1 = #'3gpp_ro_Multiple-Services-Credit-Control'{},
	{MSCC, ResultCode1} = mscc1(Element, ResultCode, Acc1),
	mscc(T, SessionId, ResultCode1, [MSCC | Acc]);
mscc([], _, undefined, []) ->
	{[], ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'};
mscc([], _, ResultCode, Acc) ->
	{lists:reverse(Acc), ResultCode}.
%% @hidden
mscc1([{"resultCode", RC} | T], ResultCode, Acc) ->
	ResultCode1 = case RC of
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
	ResultCode2 = case ResultCode of
		undefined ->
			ResultCode1;
		ResultCode ->
			ResultCode
	end,
	Acc1 = Acc#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Result-Code' = [ResultCode1]},
	mscc1(T, ResultCode2, Acc1);
mscc1([{"serviceId", SID} | T], ResultCode, Acc)  when is_integer(SID) ->
	Acc1 = Acc#'3gpp_ro_Multiple-Services-Credit-Control'{
		  'Service-Identifier' = [SID]},
	mscc1(T, ResultCode, Acc1);
mscc1([{"ratingGroup", RG} | T], ResultCode, Acc) when is_integer(RG) ->
	Acc1 = Acc#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Rating-Group' = [RG]},
	mscc1(T, ResultCode, Acc1);
mscc1([{"grantedUnit", {_, [{UT, GSUTotal}]}} | T], ResultCode, Acc)
		when GSUTotal > 0 ->
	GSU = case UT of
		"time" ->
			#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GSUTotal]};
		"totalVolume" ->
			#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [GSUTotal]};
		"uplinkVolume" ->
			#'3gpp_ro_Granted-Service-Unit'{'CC-Input-Octets' = [GSUTotal]};
		"downlinkVolume" ->
			#'3gpp_ro_Granted-Service-Unit'{'CC-Output-Octets' = [GSUTotal]};
		"serviceSpecificUnit" ->
			#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [GSUTotal]}
	end,
	Acc1 = Acc#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GSU]},
	mscc1(T, ResultCode, Acc1);
mscc1([{"consumedUnit", {_, [{UT, USUTotal}]}} | T], ResultCode, Acc)
		when USUTotal> 0 ->
	USU = case UT of
		"time" ->
			#'3gpp_ro_Used-Service-Unit'{'CC-Time' = [USUTotal]};
		"totalVolume" ->
			#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [USUTotal]};
		"uplinkVolume" ->
			#'3gpp_ro_Used-Service-Unit'{'CC-Input-Octets' = [USUTotal]};
		"downlinkVolume" ->
			#'3gpp_ro_Used-Service-Unit'{'CC-Output-Octets' = [USUTotal]};
		"serviceSpecificUnit" ->
			#'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [USUTotal]}
	end,
	Acc1 = Acc#'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU]},
	mscc1(T, ResultCode, Acc1);
mscc1([_H | T], ResultCode, Acc) ->
	mscc1(T, ResultCode, Acc);
mscc1([], ResultCode, Acc) ->
	{Acc, ResultCode}.

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
diameter_error(SessionId, ResultCode, OHost, ORealm, RequestType, RequestNum) ->
	#'3gpp_ro_CCA'{'Session-Id' = SessionId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum}.

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

-spec service_rating(MSCC, ServiceContextId, ServiceInformation) -> ServiceRating
	when
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}],
		ServiceContextId :: binary(),
		ServiceInformation :: [tuple()],
		ServiceRating :: [{struct, [tuple()]}].
%% Create list of service elements to be rated.
service_rating(MSCC, ServiceContextId, ServiceInformation) ->
	SCID = {"serviceContextId", ServiceContextId},
	service_rating1(MSCC, SCID, ServiceInformation, []).
%% @hidden
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Time' = [CCTime]}]} = MSCC | T], SCID, SI, Acc)
		when is_integer(CCTime), CCTime > 0 ->
	RequestedUnits = {"requestedUnit", {struct, [{"time", CCTime}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC),
			SI, RequestedUnits, {"requestSubType", "RESERVE"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Total-Octets' = [CCTotalOctets]}]} = MSCC | T], SCID, SI, Acc)
		when is_integer(CCTotalOctets), CCTotalOctets > 0 ->
	RequestedUnits = {"requestedUnit", {struct, [{"totalVolume", CCTotalOctets}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			RequestedUnits, {"requestSubType", "RESERVE"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Output-Octets' = [CCOutputOctets],
		'CC-Input-Octets' = [CCInputOctets]}]} = MSCC| T], SCID, SI, Acc)
		when is_integer(CCInputOctets), is_integer(CCOutputOctets),
		CCInputOctets > 0, CCOutputOctets > 0 ->
	RequestedUnits = {"requestedUnit", {struct, [{"uplinkVolume", CCInputOctets},
			{"downlinkVolume", CCOutputOctets}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			RequestedUnits, {"requestSubType", "RESERVE"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{
		'CC-Service-Specific-Units' = [CCSpecUnits]}]} = MSCC | T],
		SCID, SI, Acc) when is_integer(CCSpecUnits), CCSpecUnits > 0 ->
	RequestedUnits = {"requestedUnit", {struct, [{"serviceSpecificUnit", CCSpecUnits}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			RequestedUnits, {"requestSubType", "RESERVE"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Requested-Service-Unit' = [#'3gpp_ro_Requested-Service-Unit'{}]}
		= MSCC | T], SCID, SI, Acc) ->
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			{"requestSubType", "RESERVE"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Time' = [CCTime]}]} = MSCC | T], SCID, SI, Acc)
		when is_integer(CCTime), CCTime > 0 ->
	ConsumedUnits = {"consumedUnit", {struct, [{"time", CCTime}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC),
			SI, ConsumedUnits, {"requestSubType", "DEBIT"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Total-Octets' = [CCTotalOctets]}]} = MSCC | T], SCID, SI, Acc)
		when is_integer(CCTotalOctets), CCTotalOctets > 0 ->
	ConsumedUnits = {"consumedUnit", {struct, [{"totalVolume", CCTotalOctets}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			ConsumedUnits, {"requestSubType", "DEBIT"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Output-Octets' = [DownlinkVolume],
		'CC-Input-Octets' = [UplinkVolume]}]} = MSCC | T], SCID, SI, Acc)
		when is_integer(UplinkVolume), is_integer(DownlinkVolume),
		UplinkVolume > 0, DownlinkVolume > 0 ->
	ConsumedUnits = {"consumedUnit", {struct, [{"uplinkVolume", UplinkVolume},
			{"downlinkVolume", DownlinkVolume}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			ConsumedUnits, {"requestSubType", "DEBIT"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Service-Specific-Units' = [CCSpecUnits]}]} = MSCC | T], SCID, SI, Acc)
		when is_integer(CCSpecUnits), CCSpecUnits > 0 ->
	ConsumedUnits = {"consumedUnit", {struct, [{"serviceSpecificUnit", CCSpecUnits}]}},
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			ConsumedUnits, {"requestSubType", "DEBIT"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([#'3gpp_ro_Multiple-Services-Credit-Control'{}
		= MSCC | T], SCID, SI, Acc) ->
	Parameters = lists:flatten([SCID, get_si(MSCC), get_rg(MSCC), SI,
			{"requestSubType", "RESERVE"}]),
	service_rating1(T, SCID, SI, [{struct, Parameters} | Acc]);
service_rating1([], _SCID, _SI, Acc) ->
	Acc.

%% @hidden
get_si(#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = [SI]})
		when is_integer(SI) ->
	{"serviceId", SI};
get_si(_) ->
	[].

%% @hidden
get_rg(#'3gpp_ro_Multiple-Services-Credit-Control'{'Rating-Group' = [RG]})
		when is_integer(RG) ->
	{"ratingGroup", RG};
get_rg(_) ->
	[].

