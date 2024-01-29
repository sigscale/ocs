%%% ocs_diameter_3gpp_ro_application_cb.erl
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
%%% 	for the 3GPP DIAMETER Ro in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS 29.299 Diameter charging applications
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%% 	RFC4006 - DIAMETER Credit-Control Application </a>
%%%
-module(ocs_diameter_3gpp_ro_application_cb).
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

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
		{_, IpAddress, Port} = _ServiceName, {_, Capabilities} = _Peer) ->
	{reply, process_request(IpAddress, Port, Capabilities, Request)};
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
				'Subscription-Id' = SubscriptionIds} = Request) ->
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
		Subscriber = case subscriber_id(SubscriptionIds, SubIdTypes) of
			{_IdType, SubId} ->
				SubId;
			_ ->
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
				Subscriber, OHost, DHost, ORealm, DRealm, IpAddress, Port)
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
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = []} = Request,
		SessionId, RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) ->
	try
		Server = {IpAddress, Port},
		case mnesia:transaction(fun() -> mnesia:dirty_read(service, Subscriber) end) of
			{atomic, [#service{enabled = true}]} ->
				Reply = diameter_answer(SessionId, [],
						?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{atomic, [#service{enabled = false}]} ->
				Reply = diameter_error(SessionId,
						?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{atomic, []} ->
				Reply = diameter_error(SessionId,
						?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{aborted, Reason} ->
				throw(Reason)
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
		Subscriber, OHost, _DHost, ORealm, _DRealm, IpAddress, Port) ->
	try
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
		Amounts = get_mscc(MSCC1),
		case rate(ServiceType, ServiceNetwork, Subscriber,
				Timestamp, Address, Direction, initial, SessionId,
				Amounts) of
			{MSCC2, ResultCode} when is_list(MSCC2) ->
				Reply = diameter_answer(SessionId, MSCC2,
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, Subscriber}, {address, Address},
						{direction, Direction}, {amounts, Amounts}]),
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
		RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) when length(MSCC1) > 0 ->
	try
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
		Amounts = get_mscc(MSCC1),
		case rate(ServiceType, ServiceNetwork, Subscriber,
				Timestamp, Address, Direction, interim, SessionId,
				Amounts) of
			{MSCC2, ResultCode} when is_list(MSCC2) ->
				Reply = diameter_answer(SessionId, MSCC2,
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, Subscriber}, {address, Address},
						{direction, Direction}, {amounts, Amounts}]),
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
					{request, Request}, {error, Reason1}], {stack, StackTrace}),
			diameter_error(SessionId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, RequestType, RequestNum)
	end;
process_request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) ->
	try
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
		case get_mscc(MSCC1) of
			[] ->
				ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				Reply = diameter_answer(SessionId, [],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			Amounts ->
				case rate(ServiceType, ServiceNetwork, Subscriber,
						Timestamp, Address, Direction, final, SessionId,
						Amounts) of
					{MSCC2, ResultCode} when is_list(MSCC2) ->
						Reply = diameter_answer(SessionId, MSCC2,
								ResultCode, OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply;
					{MSCC2, ResultCode, Rated} when is_list(Rated) ->
						Reply = diameter_answer(SessionId, MSCC2,
								ResultCode, OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, Rated),
						Reply;
					{error, Reason} ->
						error_logger:error_report(["Rating Error",
								{module, ?MODULE}, {error, Reason},
								{origin_host, OHost}, {origin_realm, ORealm},
								{type, accounting_event_type(RequestType)},
								{subscriber, Subscriber}, {address, Address},
								{direction, Direction}, {amounts, Amounts}]),
						Reply = diameter_error(SessionId, ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
								OHost, ORealm, RequestType, RequestNum),
						ok = ocs_log:acct_log(diameter, Server,
								accounting_event_type(RequestType), Request, Reply, undefined),
						Reply
				end
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
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [],
		'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) ->
	try
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
		case ocs_rating:rate(diameter, ServiceType, undefined, undefined,
				ServiceNetwork, Subscriber, Timestamp, Address, Direction,
				event, [], [], [{'Session-Id', SessionId}]) of
			{ok, _, {octets, Amount}, Rated} ->
				ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Amount]},
				MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
						'Granted-Service-Unit' = [GSU],
						'Result-Code' = [ResultCode]},
				Reply = diameter_answer(SessionId, [MSCC],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{ok, _, {seconds, Amount}, Rated} ->
				ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [Amount]},
				MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
						'Granted-Service-Unit' = [GSU],
						'Result-Code' = [ResultCode]},
				Reply = diameter_answer(SessionId, [MSCC],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{ok, _, {messages, Amount}, Rated} ->
				ResultCode = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [Amount]},
				MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
						'Granted-Service-Unit' = [GSU],
						'Result-Code' = [ResultCode]},
				Reply = diameter_answer(SessionId, [MSCC],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{out_of_credit, _RedirectServerAddress, _SessionList, Rated} ->
				ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
				Reply = diameter_answer(SessionId, [],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{disabled, _SessionList} ->
				ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED',
				Reply = diameter_answer(SessionId, [],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, service_not_found} ->
				ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
				Reply = diameter_answer(SessionId, [],
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{error, Reason} ->
				{error, Reason}
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
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = MSCC1,
		'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
		'Service-Information' = ServiceInformation,
		'Service-Context-Id' = SvcContextId,
		'Event-Timestamp' = EventTimestamp} = Request, SessionId,
		RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm,
		IpAddress, Port) ->
	try
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
		Amounts = get_mscc(MSCC1),
		case rate(ServiceType, ServiceNetwork, Subscriber,
				Timestamp, Address, Direction, event, SessionId,
				Amounts) of
			{MSCC2, ResultCode} when is_list(MSCC2) ->
				Reply = diameter_answer(SessionId, MSCC2,
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, undefined),
				Reply;
			{MSCC2, ResultCode, Rated} when is_list(Rated) ->
				Reply = diameter_answer(SessionId, MSCC2,
						ResultCode, OHost, ORealm, RequestType, RequestNum),
				ok = ocs_log:acct_log(diameter, Server,
						accounting_event_type(RequestType), Request, Reply, Rated),
				Reply;
			{error, Reason} ->
				error_logger:error_report(["Rating Error",
						{module, ?MODULE}, {error, Reason},
						{origin_host, OHost}, {origin_realm, ORealm},
						{type, accounting_event_type(RequestType)},
						{subscriber, Subscriber}, {address, Address},
						{direction, Direction}, {amounts, Amounts}]),
				Reply = diameter_error(SessionId, ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
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
service_network([#'3gpp_ro_Service-Information'{
		'IMS-Information' = [#'3gpp_ro_IMS-Information'{
		'Access-Network-Information' = ANI}]}])
		when length(ANI) > 0 ->
	access_info(ANI);
service_network(_) ->
	undefined.

%% @hidden
%% 3GPP TS 24.229 7. 2A.4
access_info([H | T]) ->
	case access_info1(string:tokens(binary_to_list(H), ";")) of
		ID when length(ID) > 0 ->
			{MCC, MNC, _ } = ocs_diameter:plmn(ID),
			MCC ++ MNC;
		[] ->
			access_info(T)
	end;
access_info([]) ->
	"000000".
%% @hidden
access_info1(["utran-cell-id-3gpp=" ++ CID | _]) ->
	CID;
access_info1(["cgi-3gpp=" ++ CGI | _]) ->
	CGI;
access_info1(["cgi-3gpp2=" ++ CGI | _]) ->
	CGI;
access_info1(["ci-3gpp2-femto=" ++ CGI | _]) ->
	CGI;
access_info1(["extension-access-info=" ++ CGI | _]) ->
	CGI;
access_info1([_ | T]) ->
   access_info1(T);
access_info1([]) ->
	[].

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
%% @doc Parse out the USU and RSU unit amounts from MSCCs.
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
		when is_integer(CCTime), CCTime >= 0 ->
	[{seconds, CCTime}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Total-Octets' = [CCTotalOctets]} | _]})
		when is_integer(CCTotalOctets), CCTotalOctets >= 0 ->
	[{octets, CCTotalOctets}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Output-Octets' = [CCOutputOctets],
		'CC-Input-Octets' = [CCInputOctets]} | _]})
		when is_integer(CCInputOctets), is_integer(CCOutputOctets),
		CCInputOctets >= 0, CCOutputOctets >= 0 ->
	[{octets, CCInputOctets + CCOutputOctets}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{
		'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
		'CC-Service-Specific-Units' = [CCSpecUnits]} | _]})
		when is_integer(CCSpecUnits), CCSpecUnits >= 0 ->
	[{messages, CCSpecUnits}];
get_usu(#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' = []}) ->
	[].

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

-spec rate(ServiceType, ServiceNetwork, Subscriber, Timestamp,
		Address, Direction, Flag, SessionId, Amounts) -> Result
	when
		ServiceType :: binary(),
		ServiceNetwork :: binary(),
		Subscriber :: binary(),
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
		Result :: {[MSCC], ResultCode} | {[MSCC], ResultCode, Rated}
				| {error, Reason ::term()},
		MSCC :: #'3gpp_ro_Multiple-Services-Credit-Control'{},
		ResultCode :: pos_integer(),
		Rated :: [#rated{}].
%% @doc Rate all the MSCCs.
%% @hidden
rate(ServiceType, ServiceNetwork, Subscriber, Timestamp,
		Address, Direction, Flag, SessionId, Amounts) ->
	rate(ServiceType, ServiceNetwork, Subscriber, Timestamp,
			Address, Direction, Flag, SessionId,
			Amounts, [], undefined, undefined).
%% @hidden
rate(ServiceType, ServiceNetwork, Subscriber,
		Timestamp, Address, Direction, Flag, SessionId,
		[{SI, RG, Debits, Reserves} | T], Acc, ResultCode1, Rated1) ->
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
			ServiceNetwork, Subscriber, Timestamp, Address, Direction, Flag,
			Debits, Reserves, [{'Session-Id', SessionId}]) of
		{ok, _, {seconds, Amount} = _GrantedAmount} when Amount > 0 ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated1);
		{ok, _, {octets, Amount} = _GrantedAmount} when Amount > 0 ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated1);
		{ok, _, {messages, Amount} = _GrantedAmount} when Amount > 0 ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated1);
		{ok, _, {_, 0} = _GrantedAmount} ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc, ResultCode2, Rated1);
		{ok, _, {octets, Amount}, Rated2} when Amount > 0,
				is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated2);
		{ok, _, {octets, Amount}, Rated2} when Amount > 0,
				is_list(Rated2), is_list(Rated1) ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated1 ++ Rated2);
		{ok, _, {seconds, Amount}, Rated2} when Amount > 0,
				is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated2);
		{ok, _, {messages, Amount}, Rated2} when Amount > 0,
				is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated2);
		{ok, _, {messages, Amount}, Rated2} when Amount > 0,
				is_list(Rated2), is_list(Rated1) ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			GSU = #'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [Amount]},
			MSCC = #'3gpp_ro_Multiple-Services-Credit-Control'{
					'Granted-Service-Unit' = [GSU],
					'Service-Identifier' = SI,
					'Rating-Group' = RG,
					'Result-Code' = [ResultCode2]},
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode2, Rated1 ++ Rated2);
		{ok, _, Rated2} when is_list(Rated2), Rated1 == undefined ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc, ResultCode2, Rated2);
		{ok, _, Rated2} when is_list(Rated2), is_list(Rated1) ->
			ResultCode2 = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, Acc, ResultCode2, Rated1 ++ Rated2);
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
					#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Service-Identifier' = SI,
							'Rating-Group' = RG,
							'Result-Code' = [ResultCode3]};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Service-Identifier' = SI,
							'Rating-Group' = RG,
							'Result-Code' = [ResultCode3],
							'Final-Unit-Indication' = fui(RedirectServerAddress)}
			end,
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode3, Rated1);
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
					#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Service-Identifier' = SI,
							'Rating-Group' = RG,
							'Result-Code' = [ResultCode3]};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Service-Identifier' = SI,
							'Rating-Group' = RG,
							'Result-Code' = [ResultCode3],
							'Final-Unit-Indication' = fui(RedirectServerAddress)}
			end,
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode3, Rated2);
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
					#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Service-Identifier' = SI,
							'Rating-Group' = RG,
							'Result-Code' = [ResultCode3]};
				RedirectServerAddress when is_list(RedirectServerAddress) ->
					#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Service-Identifier' = SI,
							'Rating-Group' = RG,
							'Result-Code' = [ResultCode3],
							'Final-Unit-Indication' = fui(RedirectServerAddress)}
			end,
			rate(ServiceType, ServiceNetwork, Subscriber,
					Timestamp, Address, Direction, Flag, SessionId,
					T, [MSCC | Acc], ResultCode3, Rated1 ++ Rated2);
		{disabled, _SessionList} ->
			{Acc, ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED'};
		{error, service_not_found} ->
			{Acc, ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN'};
		{error, Reason} ->
			{error, Reason}
	end;
rate(ServiceType, ServiceNetwork, Subscriber,
		Timestamp, Address, Direction, final, SessionId,
		[], [], undefined, undefined) ->
	case ocs_rating:rate(diameter, ServiceType, undefined, undefined,
			ServiceNetwork, Subscriber, Timestamp, Address, Direction, final,
			[], [], [{'Session-Id', SessionId}]) of
		{ok, _, Rated} ->
			{[], ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', Rated};
		{out_of_credit, _, _SessionList} ->
			{[], ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED', []};
		{out_of_credit, _, _SessionList, Rated} ->
			{[], ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED', Rated};
		{disabled, _SessionList} ->
			{[], ?'DIAMETER_CC_APP_RESULT-CODE_END_USER_SERVICE_DENIED', []};
		{error, Reason} ->
			{error, Reason}
	end;
rate(_, _, _, _, _, _, _, _, [], Acc, ResultCode, undefined) ->
	{lists:reverse(Acc), ResultCode};
rate(_, _, _, _, _, _, _, _, [], Acc, ResultCode, Rated) ->
	{lists:reverse(Acc), ResultCode, Rated}.

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

-spec subscriber_id(SubscriberIdAVPs, SubIdTypes) -> Subscribers
	when
		SubscriberIdAVPs :: [#'3gpp_ro_Subscription-Id'{}],
		SubIdTypes :: [SubIdType] | undefined,
		SubIdType :: imsi | msisdn | nai | sip | private,
		Subscribers :: {IdType, SubId} | [],
		IdType :: integer(),
		SubId :: binary().
%% @doc Get Subscribers From Diameter SubscriberId AVP.
subscriber_id(SubscriberIdAVPs, undefined = _SubIdTypes) ->
	subscriber_id(SubscriberIdAVPs, [msisdn]);
subscriber_id(SubscriberIdAVPs, [H | T])
		when is_atom(H) ->
	IdType = id_type(H),
	case lists:keyfind(IdType, #'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
			SubscriberIdAVPs) of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = SubId} ->
			{IdType, SubId};
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

