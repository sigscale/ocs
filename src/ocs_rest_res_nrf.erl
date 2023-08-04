%% ocs_rest_res_nrf.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_nrf).
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([initial_nrf/2, update_nrf/3, release_nrf/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_cc_application_rfc4006.hrl").
-include("ocs.hrl").
-include("ocs_log.hrl").
-include_lib("inets/include/httpd.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(RO_APPLICATION_ID, 4).

-ifdef(OTP_RELEASE). % >= 21
	-define(CATCH_STACK, _:Reason1:ST).
	-define(SET_STACK, StackTrace = ST).
-else.
	-define(CATCH_STACK, _:Reason1).
	-define(SET_STACK, StackTrace = erlang:get_stacktrace()).
-endif.

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec initial_nrf(ModData, NrfRequest) -> NrfResponse
	when
		NrfRequest :: iolist(),
		ModData :: #mod{},
		NrfResponse :: {ok, Headers, Body} | {error, Status} |
				{error, Status, Problem},
		Headers :: [tuple()],
		Body :: iolist(),
		Status :: 201 | 400 | 404 | 500,
		Problem :: map().
%% @doc Respond to `POST /nrf-rating/v1/ratingdata'.
%%
%%		Rate an intial Nrf Request.
%%
initial_nrf(ModData, NrfRequest) ->
	case authorize_rating(ModData) of
		{error, Status} ->
			{error, Status};
		{ok, authorized} ->
			initial_nrf1(NrfRequest)
	end.
%% @hidden
initial_nrf1(NrfRequest) ->
	RatingDataRef = unique(),
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				Flag = case NrfMap of
					#{"oneTimeEventType" := "IEC"} ->
						event;
					_ ->
						initial
				end,
				case rate(NrfMap, Flag) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = add_rating_ref(RatingDataRef, UpdatedMap),
						NrfResponse = nrf(UpdatedMap),
						{NrfResponse, diameter_request(RatingDataRef, Flag, NrfMap),
								diameter_reply(RatingDataRef, Flag, UpdatedMap)};
					{error, out_of_credit} ->
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, Problem,
								diameter_error_response(out_of_credit, RatingDataRef, initial, NrfMap)};
					{error, service_not_found} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(service_not_found, InvalidParams),
						{error, 404, Problem,
								diameter_error_response(service_not_found, RatingDataRef, initial, NrfMap)};
					{error, invalid_service_type} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(invalid_service_type, InvalidParams),
						{error, 400, Problem,
								diameter_error_response(invalid_service_type, RatingDataRef, initial, NrfMap)};
					{error, Reason} ->
						{error, Reason,
							diameter_error_response(charging_failed, RatingDataRef, initial, NrfMap)}
				end;
			_ ->
				error_logger:warning_report(["Unable to process DIAMETER Nrf request",
						{RatingDataRef, ratingDataRef}, {request, NrfRequest}, {flag, start},
						{error, decode_failed}]),
				Problem = rest_error_response(charging_failed, undefined),
				{error, 400, Problem}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest, LogResponse} ->
			Location = "/ratingdata/" ++ RatingDataRef,
			ReponseBody = mochijson:encode(NrfResponse1),
			Headers = [{content_type, "application/json"}, {location, Location}],
			ok = ocs_log:acct_log(diameter, server(),
					start, LogRequest, LogResponse, undefined),
			{ok, Headers, ReponseBody};
		{error, StatusCode, Problem1, {DiameterRequest, DiameterReply}} ->
			ok = ocs_log:acct_log(diameter, server(),
					start, DiameterRequest, DiameterReply, undefined),
			{error, StatusCode, Problem1};
		{error, _Reason1, {DiameterRequest, DiameterReply}} ->
			ok = ocs_log:acct_log(diameter, server(),
					start, DiameterRequest, DiameterReply, undefined),
			{error, 500}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER Nrf request",
					{RatingDataRef, ratingDataRef}, {request, NrfRequest}, {flag, start},
					{error, Reason1}, {stack, StackTrace}]),
			{error, 500}
	end.
	
-spec update_nrf(ModData, RatingDataRef, NrfRequest) -> NrfResponse
	when
		ModData ::#mod{},
		NrfRequest :: iolist(),
		RatingDataRef :: string(),
		NrfResponse :: {Status, Headers, Body} | {error, Status} |
				{error, Status, Problem},
		Headers :: [tuple()],
		Body :: iolist(),
		Status :: 200 | 400 | 404 | 500,
		Problem :: map().
%% @doc Respond to `POST /nrf-rating/v1/ratingdata/{ratingRef}/update'.
%%		Rate an interim Nrf Request.
update_nrf(ModData, RatingDataRef, NrfRequest) ->
	case authorize_rating(ModData) of
		{error, Status} ->
			{error, Status};
		{ok, authorized} ->
			update_nrf1(RatingDataRef, NrfRequest)
	end.
%% @hidden
update_nrf1(RatingDataRef, NrfRequest) ->
	case lookup_ref(RatingDataRef) of
		true ->
			update_nrf2(RatingDataRef, NrfRequest);
		false ->
			InvalidParams = [#{param => "{" ++ RatingDataRef ++ "}",
					reason => "Unknown rating data reference"}],
			Problem = rest_error_response(unknown_ref, InvalidParams),
			{error, 404, Problem};
		{error, _Reason} ->
			{error, 500}
	end.
%% @hidden
update_nrf2(RatingDataRef, NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				case rate(NrfMap, interim) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						NrfResponse = nrf(UpdatedMap),
						{NrfResponse, diameter_request(RatingDataRef, interim, NrfMap),
								diameter_reply(RatingDataRef, interim, UpdatedMap)};
					{error, out_of_credit} ->
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, Problem,
								diameter_error_response(out_of_credit, RatingDataRef, interim, NrfMap)};
					{error, service_not_found} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(service_not_found, InvalidParams),
						{error, 404, Problem,
								diameter_error_response(service_not_found, RatingDataRef, interim, NrfMap)};
					{error, invalid_service_type} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(invalid_service_type, InvalidParams),
						{error, 400, Problem,
								diameter_error_response(invalid_service_type, RatingDataRef, interim, NrfMap)};
					{error, Reason} ->
						{error, Reason,
								diameter_error_response(charging_failed, RatingDataRef, interim, NrfMap)}
				end;
			_ ->
				error_logger:warning_report(["Unable to process DIAMETER Nrf request",
						{RatingDataRef, ratingDataRef}, {request, NrfRequest}, {flag, interim},
						{error, decode_failed}]),
				Problem = rest_error_response(charging_failed, undefined),
				{error, 400, Problem}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest, LogResponse} ->
			ReponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(diameter, server(),
					start, LogRequest, LogResponse, undefined),
			{200, [], ReponseBody };
		{error, StatusCode, Problem1, {DiameterRequest, DiameterReply}} ->
			ok = ocs_log:acct_log(diameter, server(),
					update, DiameterRequest, DiameterReply, undefined),
			{error, StatusCode, Problem1};
		{error, _Reason1, {DiameterRequest, DiameterReply}} ->
			ok = ocs_log:acct_log(diameter, server(),
					update, DiameterRequest, DiameterReply, undefined),
			{error, 500}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER Nrf request",
					{RatingDataRef, ratingDataRef}, {request, NrfRequest}, {flag, interim},
					{error, Reason1}, {stack, StackTrace}]),
			{error, 500}
	end.

-spec release_nrf(ModData, RatingDataRef, NrfRequest) -> NrfResponse
	when
		ModData ::#mod{},
		NrfRequest :: iolist(),
		RatingDataRef :: string(),
		NrfResponse :: {Status, Headers, Body} | {error, Status} |
				{error, Status, Problem},
		Headers :: [tuple()],
		Body :: iolist(),
		Status :: 200 | 400 | 404 | 500,
		Problem :: map().
%% @doc Respond to `POST /nrf-rating/v1/ratingdata/{ratingRef}/final'.
%%
%%		Rate an final Nrf Request.
%%
release_nrf(ModData, RatingDataRef, NrfRequest) ->
	case authorize_rating(ModData) of
		{error, Status} ->
			{error, Status};
		{ok, authorized} ->
			release_nrf1(RatingDataRef, NrfRequest)
	end.
%% @hidden
release_nrf1(RatingDataRef, NrfRequest) ->
	case lookup_ref(RatingDataRef) of
		true ->
			release_nrf2(RatingDataRef, NrfRequest);
		false ->
			InvalidParams = [#{param => "{" ++ RatingDataRef ++ "}",
					reason => "Unknown rating data reference"}],
			Problem = rest_error_response(unknown_ref, InvalidParams),
			{error, 404, Problem};
		{error, _Reason} ->
			{error, 500}
	end.
%% @hidden
release_nrf2(RatingDataRef, NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				case rate(NrfMap, final) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = remove_ref(RatingDataRef),
						NrfResponse = nrf(UpdatedMap),
						{NrfResponse, diameter_request(RatingDataRef, final, NrfMap),
								diameter_reply(RatingDataRef, final, UpdatedMap)};
					{error, out_of_credit} ->
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, Problem,
								diameter_error_response(out_of_credit, RatingDataRef, final, NrfMap)};
					{error, service_not_found} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(service_not_found, InvalidParams),
						{error, 404, Problem,
								diameter_error_response(service_not_found, RatingDataRef, final, NrfMap)};
					{error, invalid_service_type} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(invalid_service_type, InvalidParams),
						{error, 400, Problem,
								diameter_error_response(invalid_service_type, RatingDataRef, final, NrfMap)};
					{error, Reason} ->
						{error, Reason,
								diameter_error_response(charging_failed, RatingDataRef, final, NrfMap)}
				end;
			_ ->
				error_logger:warning_report(["Unable to process DIAMETER Nrf request",
						{RatingDataRef, ratingDataRef}, {request, NrfRequest},
						{error, decode_failed}]),
				Problem = rest_error_response(charging_failed, undefined),
				{error, 400, Problem}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest, LogResponse} ->
			ReponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(diameter, server(),
					start, LogRequest, LogResponse, undefined),
			{200, [], ReponseBody};
		{error, StatusCode, Problem1, {DiameterRequest, DiameterReply}} ->
			ok = ocs_log:acct_log(diameter, server(),
					stop, DiameterRequest, DiameterReply, undefined),
			{error, StatusCode, Problem1};
		{error, _Reason1, {DiameterRequest, DiameterReply}} ->
			ok = ocs_log:acct_log(diameter, server(),
					stop, DiameterRequest, DiameterReply, undefined),
			{error, 500}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process DIAMETER Nrf request",
					{RatingDataRef, ratingDataRef}, {request, NrfRequest},
					{error, Reason1}, {stack, StackTrace}]),
			{error, 500}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec diameter_error_response(Error, RatingDataRef, RequestType, NrfMap) -> Result
	when
		Error :: term(),
		RatingDataRef :: term(),
		RequestType :: initial | interim | final | event,
		NrfMap :: map(),
		Result :: {DiameterRequest, DiameterReply},
		DiameterRequest :: #'3gpp_ro_CCR'{},
		DiameterReply :: #'3gpp_ro_CCA'{}.
%% @doc Construct a diameter problem report for accounting error logging.
diameter_error_response(out_of_credit, RatingDataRef, RequestType, NrfMap) ->
	ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED',
	diameter_error_response1(ResultCode, RatingDataRef, RequestType, NrfMap);
diameter_error_response(service_not_found, RatingDataRef, RequestType, NrfMap) ->
	ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_USER_UNKNOWN',
	diameter_error_response1(ResultCode, RatingDataRef, RequestType, NrfMap);
diameter_error_response(charging_failed, RatingDataRef, RequestType, NrfMap) ->
	ResultCode = ?'DIAMETER_CC_APP_RESULT-CODE_RATING_FAILED',
	diameter_error_response1(ResultCode, RatingDataRef, RequestType, NrfMap);
diameter_error_response(invalid_service_type, RatingDataRef, RequestType, NrfMap) ->
	ResultCode = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
	diameter_error_response1(ResultCode, RatingDataRef, RequestType, NrfMap).
%% @hidden
diameter_error_response1(ResultCode, RatingDataRef, RequestType, NrfMap) ->
	{diameter_request(RatingDataRef, RequestType, NrfMap),
			diameter_error(RatingDataRef, RequestType, ResultCode, NrfMap)}.

-spec event_type(RequestType) -> EventType
	when
	RequestType :: initial | interim | final | event,
	EventType :: 1..4.
%% @doc Converts an atom to a CC-Request-Type integer.
event_type(initial) ->
	?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST';
event_type(interim) ->
	?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST';
event_type(final) ->
	?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST';
event_type(event) ->
	?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST'.

-spec diameter_request(RatingDataRef, RequestType, NrfRequest) -> DiameterRequest
	when
		RatingDataRef :: string(),
		RequestType :: initial | interim | final | event,
		NrfRequest :: map(),
		DiameterRequest :: #'3gpp_ro_CCR'{}.
%% @doc Convert a NrfRequest map() to a #'3gpp_ro_CCR'{}.
diameter_request(RatingDataRef, RequestType, #{"invocationSequenceNumber" := RequestNum,
		"serviceRating" := [#{"serviceContextId" := SCI} | _],
		"subscriptionId" := SubscriptionIds}) ->
	{ok, DiamterConfig} = application:get_env(ocs, diameter),
	#'3gpp_ro_CCR'{'Session-Id' = RatingDataRef,
			'Origin-Host' = list_to_binary(ohost(DiamterConfig)),
			'Origin-Realm' = list_to_binary(orealm(DiamterConfig)),
			'Destination-Realm' = <<"asia-east1-b.c.sigscale-ocs.internal">>,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = SCI,
			'CC-Request-Type' = event_type(RequestType),
			'CC-Request-Number' = RequestNum,
			'User-Name' = [list_to_binary(get_subscriber(SubscriptionIds))],
			'Event-Timestamp' = calendar:universal_time(),
			'Subscription-Id' = format_sub_ids(SubscriptionIds)}.

-spec diameter_reply(RatingDataRef, RequestType, NrfReply) -> DiameterReply
	when
		RatingDataRef :: string(),
		RequestType :: initial | interim | final | event,
		NrfReply :: map(),
		DiameterReply :: #'3gpp_ro_CCA'{}.
%% @doc Convert a Nrf Resply map() to a #'3gpp_ro_CCA'{}.
diameter_reply(RatingDataRef, RequestType, #{"invocationSequenceNumber" := RequestNum,
		"serviceRating" := ServiceRating}) ->
	{ok, DiamterConfig} = application:get_env(ocs, diameter),
	#'3gpp_ro_CCA'{'Session-Id' = RatingDataRef,
			'Origin-Host' = list_to_binary(ohost(DiamterConfig)),
			'Origin-Realm' = list_to_binary(orealm(DiamterConfig)),
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = event_type(RequestType),
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = mscc(ServiceRating),
			'Result-Code' = result_code(ServiceRating)}.

-spec diameter_error(RatingDataRef, RequestType, ResultCode,
		NrfReply) -> DiameterReply
	when
		RatingDataRef :: string(),
		RequestType :: initial | interim | final | event,
		ResultCode :: pos_integer(),
		NrfReply :: map(),
		DiameterReply :: #'3gpp_ro_CCA'{}.
%% @doc Build a CCA DIAMETER CCA indicating an operation failure.
%% @hidden
diameter_error(RatingDataRef, RequestType, ResultCode,
		#{"invocationSequenceNumber" := RequestNum}) ->
	{ok, DiamterConfig} = application:get_env(ocs, diameter),
	#'3gpp_ro_CCA'{'Session-Id' = RatingDataRef, 'Result-Code' = ResultCode,
			'Origin-Host' = list_to_binary(ohost(DiamterConfig)),
			'Origin-Realm' = list_to_binary(orealm(DiamterConfig)),
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'CC-Request-Type' = event_type(RequestType),
			'CC-Request-Number' = RequestNum}.

%% @hidden
result_code("SUCCESS") ->
	?'DIAMETER_BASE_RESULT-CODE_SUCCESS';
result_code("QUOTA_LIMIT_REACHED") ->
	?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED';
result_code(ServiceRating) ->
	result_code1(ServiceRating, undefined).
%% @hidden
result_code1([#{"resultCode" := "SUCCESS"} | _], _) ->
	?'DIAMETER_BASE_RESULT-CODE_SUCCESS';
result_code1([#{"resultCode" := "QUOTA_LIMIT_REACHED"} | T], undefined) ->
	result_code1(T, ?'DIAMETER_CC_APP_RESULT-CODE_CREDIT_LIMIT_REACHED');
result_code1([_ | T], ResultCode) ->
	result_code1(T, ResultCode);
result_code1([], ResultCode) ->
	ResultCode.

-spec mscc(ServiceRatings) -> MSCC
	when
		ServiceRatings :: [ServiceRating],
		ServiceRating :: map(),
		MSCC :: [#'3gpp_ro_Multiple-Services-Credit-Control'{}].
%% @doc Convert a list of ServiceRating maps to a
%%   list of MSCCs.
mscc(ServiceRating) ->
	mscc(ServiceRating, []).
%% @hidden
mscc([H | T], Acc) ->
	mscc(T, [mscc1(H, #'3gpp_ro_Multiple-Services-Credit-Control'{}) | Acc]);
mscc([], Acc) ->
	Acc.
%% @hidden
mscc1(#{"resultCode" := ResultCode} = ServiceRating, Acc) ->
	Acc1 = case maps:find("serviceId", ServiceRating) of
		{ok, SI} ->
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Service-Identifier' = [SI]};
		_ ->
			Acc
	end,
	Acc2 = case maps:find("ratingGroup", ServiceRating) of
		{ok, RG} ->
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Rating-Group' = [RG]};
		_ ->
			Acc1
	end,
	GSU = case maps:find("requestedUnit", ServiceRating) of
		{ok, #{"totalVolume" := RTV}} when RTV > 0->
			[#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [RTV]}];
		{ok, #{"time" := RTime}} when RTime > 0 ->
			[#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [RTime]}];
		{ok, #{"serviceSpecificUnit" := RSSU}} when RSSU > 0 ->
			[#'3gpp_ro_Granted-Service-Unit'{'CC-Service-Specific-Units' = [RSSU]}];
		_ ->
			[]
	end,
	USU = case maps:find("consumedUnit", ServiceRating) of
		{ok, #{"totalVolume" := CTV}} when CTV > 0 ->
			[#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [CTV]}];
		{ok, #{"time" := CTime}} when CTime > 0 ->
			[#'3gpp_ro_Used-Service-Unit'{'CC-Time' = [CTime]}];
		{ok, #{"serviceSpecificUnit" := CSSU}} when CSSU > 0 ->
			[#'3gpp_ro_Used-Service-Unit'{'CC-Service-Specific-Units' = [CSSU]}];
		_ ->
			[]
	end,
	Acc3 = Acc2#'3gpp_ro_Multiple-Services-Credit-Control'{'Granted-Service-Unit' = GSU,
			'Used-Service-Unit' = USU, 'Result-Code' = [result_code(ResultCode)]},
	Acc3.

-spec remove_ref(RatingDataRef) -> Result
	when
		RatingDataRef :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Remove a rating data ref.
remove_ref(RatingDataRef)
		when is_list(RatingDataRef) ->
	F = fun() ->
			mnesia:delete(nrf_ref, RatingDataRef, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.
	
-spec lookup_ref(RatingDataRef) -> Result
	when
		RatingDataRef :: string(),
		Result :: boolean() | {error, Reason},
		Reason :: term().
%% @doc Look up a rating data ref.
lookup_ref(RatingDataRef)
		when is_list(RatingDataRef) ->
	F = fun() ->
			mnesia:read(nrf_ref, RatingDataRef, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, []} ->
			false;
		{atomic, [#nrf_ref{rating_ref = RatingDataRef}]} ->
			true
	end.

-spec add_rating_ref(RatingDataRef, NrfMap) -> Result
	when
		RatingDataRef :: string(),
		NrfMap :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Add a rating data ref to the rating ref table.
add_rating_ref(RatingDataRef, #{"nodeFunctionality" := NF,
		"subscriptionId" := SubscriptionId} = _NrfMap) ->
	F = fun() ->
			NewRef = #nrf_ref{rating_ref = RatingDataRef,
					node_functionality = NF, subscription_id = SubscriptionId,
					last_modified = erlang:system_time(millisecond)},
			mnesia:write(nrf_ref, NewRef, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec rest_error_response(Error, InvalidParams) -> Result
	when
		Error :: term(),
		InvalidParams :: [map()] | undefined,
		Result :: map().
%% @doc Construct a problem report for an error respponse.
rest_error_response(out_of_credit, undefined) ->
	#{cause => "QUOTA_LIMIT_REACHED",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied due to insufficient credit (usage applied)"};
rest_error_response(service_not_found, InvalidParams) ->
	#{cause => "USER_UNKNOWN",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the subscriber identity is unrecognized",
			invalidParams => InvalidParams};
rest_error_response(charging_failed, undefined) ->
	#{cause => "CHARGING_FAILED",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Incomplete or erroneous session or subscriber information"};
rest_error_response(unknown_ref, InvalidParams) ->
	#{cause => "RATING_DATA_REF_UNKNOWN",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the rating data ref is not recognized",
			invalidParams => InvalidParams};
rest_error_response(invalid_service_type, InvalidParams) ->
	#{cause => "CHARGING_FAILED",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the service context id is not recognized",
			invalidParams => InvalidParams}.

-spec rate(NrfRequest, Flag) -> Result
	when
		NrfRequest :: map(),
		Flag :: initial | interim | final | event,
		Result :: [map()] | {error, Reason},
		Reason :: term().
%% @doc Rate Nrf Service Ratings.
rate(#{"serviceRating" := ServiceRating, "invocationSequenceNumber" := ISN,
		"subscriptionId" := SubscriptionIds}, Flag) ->
	rate(ServiceRating, ISN, SubscriptionIds, Flag, []).
%% @hidden
rate([#{"serviceContextId" := SCI} = H | T],
		ISN, SubscriptionIds, Flag, Acc) ->
	{Map1, ServiceId} = case maps:find("serviceId", H) of
		{ok, SI} ->
			{#{"serviceId" => SI}, SI};
		_ ->
			{#{}, undefined}
	end,
	{Map2, ChargingKey} = case maps:find("ratingGroup", H) of
		{ok, RG} ->
			{Map1#{"ratingGroup" => RG}, RG};
		_ ->
			{Map1, undefined}
	end,
	{Map3, MCCMNC} = case maps:find("msccmnc", H) of
		{ok, #{"serviceInformation" := #{"mcc" := MCC, "mnc" := MNC}}} ->
			{Map2#{"serviceInformation" => #{"mcc" => MCC, "mnc" => MNC}}, MCC ++ MNC};
		_ ->
			{Map2, undefined}
	end,
	Reserves = case maps:find("requestedUnit", H) of
		{ok, #{"totalVolume" := RTV}} when RTV > 0->
			[{octets, RTV}];
		{ok, #{"time" := RTime}} when RTime > 0 ->
			[{seconds, RTime}];
		{ok, #{"serviceSpecificUnit" := RSSU}} when RSSU > 0 ->
			[{messages, RSSU}];
		_ ->
			[]
	end,
	{Debits, Map4} = case maps:find("consumedUnit", H) of
		{ok, #{"totalVolume" := CTV}} when CTV > 0 ->
			{[{octets, CTV}], Map3#{"consumedUnit" => #{"totalVolume" => CTV}}};
		{ok, #{"time" := CTime}} when CTime > 0 ->
			{[{seconds, CTime}], Map3#{"consumedUnit" => #{"time" => CTime}}};
		{ok, #{"serviceSpecificUnit" := CSSU}} when CSSU > 0 ->
			{[{messages, CSSU}], Map3#{"consumedUnit" => #{"serviceSpecificUnit" => CSSU}}};
		_ ->
			{[], Map3}
	end,
	ServiceType = service_type(list_to_binary(SCI)),
	TS = calendar:universal_time(),
	case ocs_rating:rate(diameter, ServiceType, ServiceId, ChargingKey,
			MCCMNC, get_subscriber(SubscriptionIds), TS, undefined, undefined, Flag,
			Debits, Reserves, [{"invocationSequenceNumber", ISN}]) of
		{ok, _, {Type, Amount} = _GrantedAmount} when Amount > 0 ->
			RatedMap = Map4#{"resultCode" => "SUCCESS", "grantedUnit" => #{type(Type) => Amount},
					"serviceContextId"=> SCI},
			rate(T, ISN, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {_, 0} = _GrantedAmount} ->
			RatedMap = Map4#{"resultCode" => "SUCCESS", "serviceContextId" => SCI},
			rate(T, ISN, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {Type, Amount}, _} ->
			RatedMap = Map4#{"resultCode" => "SUCCESS", "grantedUnit" => #{type(Type) => Amount},
					"serviceContextId"=> SCI},
			rate(T, ISN, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, _} ->
			RatedMap = Map4#{"resultCode" => "SUCCESS", "serviceContextId" => SCI},
			rate(T, ISN, SubscriptionIds, Flag, [RatedMap | Acc]);
		{out_of_credit, _, _} ->
			RatedMap = Map4#{"resultCode" => "QUOTA_LIMIT_REACHED",
					"serviceContextId" => SCI},
			rate(T, ISN, SubscriptionIds, Flag, [RatedMap | Acc]);
		{error, Reason} ->
			{error, Reason}
	end;
rate([], _ISN, _Subscriber, _Flag, Acc) ->
	F = fun F([#{"resultCode" := "SUCCESS"} | _T]) ->
			Acc;
		F([_H | T]) ->
			F(T);
		F([]) ->
			{error, out_of_credit}
	end,
	F(Acc).

-spec nrf(Nrf) -> Nrf
	when
		Nrf :: map() | {struct, [tuple()]}.
%% @doc CODEC for Nrf Reponse.
nrf({struct, StructList}) ->
	nrf(StructList, #{});
nrf(NrfRequest) ->
	nrf1(NrfRequest, []).
%% @hidden
nrf1(#{"invocationTimeStamp" := TS} = M, Acc) ->
	nrf2(M, [{"invocationTimeStamp", TS} | Acc]).
nrf2(#{"invocationSequenceNumber" := SeqNum} = M, Acc) ->
	nrf3(M, [{"invocationSequenceNumber", SeqNum} | Acc]).
nrf3(#{"subscriptionId" := SubIds} = M, Acc) ->
	nrf4(M, [{"subscriptionId", {array, SubIds}} | Acc]).
nrf4(#{"nodeFunctionality" := NF} = M, Acc) ->
	nrf5(M, [{"nfConsumerIdentification",
			{struct, [{"nodeFunctionality", NF}]}} | Acc]).
nrf5(#{"serviceRating" := ServiceRating}, Acc) ->
	Acc1 = [{"serviceRating", {array, struct_service_rating(ServiceRating)}} | Acc],
	{struct, Acc1}.
%% @hidden
nrf([{"invocationTimeStamp", TS} | T], Acc) ->
	nrf(T, Acc#{"invocationTimeStamp" => TS});
nrf([{"oneTimeEventType", EventType} | T], Acc) ->
	nrf(T, Acc#{"oneTimeEventType" => EventType});
nrf([{"invocationSequenceNumber", SeqNum} | T], Acc) ->
	nrf(T, Acc#{"invocationSequenceNumber" => SeqNum});
nrf([{"subscriptionId", SubscriptionIds} | T], Acc) ->
	nrf(T, subscriptionId_map(SubscriptionIds, Acc));
nrf([{"nfConsumerIdentification", {struct, [{"nodeFunctionality", NF}]}} | T], Acc) ->
	nrf(T, Acc#{"nodeFunctionality" => NF});
nrf([{"serviceRating", {array, ServiceRating}} | T], Acc) ->
	nrf(T, Acc#{"serviceRating" => map_service_rating(ServiceRating)});
nrf([_H | T], Acc) ->
	nrf(T, Acc);
nrf([], Acc) ->
	Acc.

%% @hidden
subscriptionId_map({array, Ids}, Acc) ->
	subscriptionId_map(Ids, Acc#{"subscriptionId" => []});
subscriptionId_map(["msisdn-" ++ MSISDN | T],
		#{"subscriptionId" := SubscriptionIds} = Acc) ->
	subscriptionId_map(T, Acc#{"subscriptionId" =>
			["msisdn-" ++ MSISDN | SubscriptionIds]});
subscriptionId_map(["imsi-" ++ IMSI | T],
		#{"subscriptionId" := SubscriptionIds} = Acc) ->
	subscriptionId_map(T, Acc#{"subscriptionId" =>
			["imsi-" ++ IMSI | SubscriptionIds]});
subscriptionId_map([_ | T], Acc) ->
	subscriptionId_map(T, Acc);
subscriptionId_map([], #{"subscriptionId" := SubscriptionIds} = Acc) ->
	SubscriptionIds1 = lists:reverse(SubscriptionIds),
	Acc#{"subscriptionId" := SubscriptionIds1}.

-spec get_subscriber(SubscriptionIds) -> Subscriber
	when
		SubscriptionIds :: [Id],
		Id :: string(),
		Subscriber :: string().
%% @hidden Get a subscriber id from list of subscribers.
get_subscriber(["msisdn-" ++ MSISDN | _]) ->
	MSISDN;
get_subscriber(["imsi-" ++ IMSI | _]) ->
	IMSI;
get_subscriber([_ | T]) ->
	get_subscriber(T);
get_subscriber([]) ->
	undefined.

-spec struct_service_rating(ServiceRating) -> Result
	when
		ServiceRating :: [map()],
		Result :: [{struct, [tuple()]}].
%% @doc Convert a Service Rating map to a struct.
struct_service_rating(ServiceRating) ->
	struct_service_rating(ServiceRating, []).
%% @hidden
struct_service_rating([H | T], Acc) ->
	Acc1 = case maps:find("grantedUnit", H) of
		{ok, Units1} ->
			[{"grantedUnit", {struct, maps:to_list(Units1)}}];
		_ ->
			[]
	end,
	Acc2 = case maps:find("consumedUnit", H) of
		{ok, Units2} ->
			[{"consumedUnit", {struct, maps:to_list(Units2)}} | Acc1];
		_ ->
			Acc1
	end,
	Acc3 = case maps:find("serviceContextId", H) of
		{ok, SCI} ->
			[{"serviceContextId", SCI} | Acc2];
		_ ->
			Acc2
	end,
	Acc4 = case maps:find("serviceId", H) of
		{ok, SI} ->
			[{"serviceId", SI} | Acc3];
		_ ->
			Acc3
	end,
	Acc5 = case maps:find("ratingGroup", H) of
		{ok, RG} ->
			[{"ratingGroup", RG} | Acc4];
		_ ->
			Acc4
	end,
	Acc6 = case maps:find("serviceInformation", H) of
		{ok, #{"mcc" := MCC, "mnc" := MNC}} ->
			[{"serviceInformation",
					{struct, [{"sgsnMccMnc", {struct,
					[{"mcc", MCC}, {"mnc", MNC}]}}]}} | Acc5];
		error ->
			Acc5
	end,
	Acc7 = case maps:find("resultCode", H) of
		{ok, RC} ->
			[{"resultCode", RC} | Acc6];
		error ->
			Acc6
	end,
	struct_service_rating(T, [{struct, Acc7} | Acc]);
struct_service_rating([], Acc) ->
	lists:reverse(Acc).

-spec map_service_rating(ServiceRating) -> Result
	when
		ServiceRating :: [{struct, [tuple()]}],
		Result :: [map()].
%% @doc Convert a Service Rating struct to a map.
map_service_rating(ServiceRating) ->
	map_service_rating(ServiceRating, []).
%% @hidden
map_service_rating([{struct, Elements} | T], Acc) ->
	F = fun F([{"requestedUnit", {_, Units}} | T1], Acc1) ->
			Acc2 = Acc1#{"requestedUnit" => units(Units)},
			F(T1, Acc2);
		F([{"consumedUnit", {_, Units}} | T1], Acc1) ->
			Acc2 = Acc1#{"consumedUnit" => units(Units)},
			F(T1, Acc2);
		F([{"serviceInformation", {_, [{_, {_ ,
				[{"mcc", MCC}, {"mnc", MNC}]}}]}} | T1], Acc1) ->
			Acc2 = Acc1#{"serviceInformation" => #{"mcc" => MCC, "mnc" => MNC}},
			F(T1, Acc2);
		F([{Name, Value} | T1], Acc1) ->
			F(T1, Acc1#{Name => Value});
		F([], Acc1) ->
			Acc1
	end,
	ServiceRatingMap = F(Elements, #{}),
	map_service_rating(T, [ServiceRatingMap | Acc]);
map_service_rating([], Acc) ->
	lists:reverse(Acc).

%% @hidden
units(Units) ->
	units(Units, #{}).
%% @hidden
units([{"time", CCTime} | T], Acc) ->
	units(T, Acc#{"time" => CCTime});
units([{"downlinkVolume", DownLinkVolume} | T], Acc) ->
	units(T, Acc#{"downlinkVolume" => DownLinkVolume});
units([{"uplinkVolume", UpLinkVolume} | T], Acc) ->
	units(T, Acc#{"uplinkVolume" => UpLinkVolume});
units([{"totalVolume", TotalVolume} | T], Acc) ->
	units(T, Acc#{"totalVolume" => TotalVolume});
units([{"serviceSpecificUnit", SpecUnits} | T], Acc) ->
	units(T, Acc#{"serviceSpecificUnit" => SpecUnits});
units([], Acc) ->
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

%% @hidden
type(octets) ->
	"totalVolume";
type(seconds) ->
	"time";
type(messages) ->
	"serviceSpecificUnit".

-spec unique() -> Result
	when
		Result :: ID,
		ID :: string().
%% @doc Generate a unique identifier
unique() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ integer_to_list(N).

%% @hidden
format_sub_ids(SubscriptionIds) ->
	format_sub_ids(SubscriptionIds, []).
%% @hidden
format_sub_ids(["msisdn-" ++ MSISDN | T], Acc) ->
	Subscriber = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = 0,
			'Subscription-Id-Data' = list_to_binary(MSISDN)},
	format_sub_ids(T, [Subscriber | Acc]);
format_sub_ids(["imsi-" ++ IMSI | T], Acc) ->
	Subscriber = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = 1,
			'Subscription-Id-Data' = list_to_binary(IMSI)},
	format_sub_ids(T, [Subscriber | Acc]);
format_sub_ids([], Acc) ->
	Acc.

-spec ohost(DiamterConfig) -> OriginHost
	when
		DiamterConfig :: list(),
		OriginHost :: string().
%% @doc Get Origin Host.
ohost(DiamterConfig) ->
	{ok, Hostname} = inet:gethostname(),
	case lists:keyfind('Origin-Host', 1, DiamterConfig) of
		{_, OriginHost}->
			OriginHost ;
		false when length(Hostname) > 0 ->
				Hostname;
		false ->
			<<"ocs">>
	end.

-spec orealm(DiamterConfig) -> OriginRealm
	when
		DiamterConfig :: list(),
		OriginRealm :: string().
%% @doc Get Origin Realm.
orealm(DiamterConfig) ->
	case lists:keyfind('Origin-Realm', 1, DiamterConfig) of
		{_, OriginRealm}->
			OriginRealm;
		false ->
			case inet_db:res_option(domain) of
				S when length(S) > 0 ->
					S;
				_ ->
				"example.net"
			end
	end.

-spec server() -> Result
	when
		Result :: HostName | {error, Reason},
		HostName :: {inet:ip_address(), inet:ip_port()},
		Reason :: term().
%% @doc Get server IP address and Port.
server() ->
	case application:get_env(ocs, diameter) of
		{ok, [{acct, [{{0, 0, 0, 0}, Port, _}]}]} ->
			{get_address(), Port};
		{ok, [{acct, [{{0, 0, 0, 0}, Port, _}]}, _]} ->
			{get_address(), Port};
		{ok, [_, {acct, [{{0, 0, 0, 0}, Port, _}]}]} ->
			{get_address(), Port};
		{ok, [{acct, [{Address, Port, _}]}]} ->
			{Address, Port};
		{ok, [{acct, [{Address, Port, _}]}, _]} ->
			{Address, Port};
		{ok, [_, {acct, [{Address, Port, _}]}]} ->
			{Address, Port}
	end.

-spec get_address() -> Result
	when
		Result :: HostName | {error, Reason},
		HostName :: inet:ip_address(),
		Reason :: term().
%% @doc Get server IP address
get_address() ->
	{ok, Host} = inet:gethostname(),
	case inet:getaddr(Host, inet) of
		{ok, Address} ->
			Address;
		{error, Reason} ->
			{error, Reason}
	end.

-spec authorize_rating(ModData) -> Result
	when
		ModData :: #mod{},
		Result :: {ok, authorized} | {error, Status},
		Status :: term().
%% @doc Do Authorization for Re interface
authorize_rating(#mod{data = Data} = _ModData) ->
	case lists:keyfind(remote_user, 1, Data) of
		{remote_user, RemoteUser} ->
			authorize_rating1(RemoteUser);
		false ->
			{error, 400}
	end.
%% @hidden
authorize_rating1(RemoteUser) ->
	case ocs:get_user(RemoteUser) of
		{ok, #httpd_user{user_data = UserData}} ->
			case lists:keyfind(rating, 1, UserData) of
				{rating, true} ->
					{ok, authorized};
				{rating, false} ->
					{error, 401}
			end;
		{error, no_such_user} ->
			{error, 404}
	end.

