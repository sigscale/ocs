%%% ocs_rest_res_nrf.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([initial_nrf/2, update_nrf/3, release_nrf/3]).

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
		NrfResponse :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /nrf-rating/v1/ratingdata'.
%%
%%		Rate an intial Nrf Request.
%%
initial_nrf(ModData, NrfRequest) ->
	case authorize_rating(ModData) of
		{ok, authorized} ->
			initial_nrf1(ModData, NrfRequest);
		{error, Status} ->
			{error, Status};
		{error, Status, Problem} ->
			{error, Status, Problem}
	end.
%% @hidden
initial_nrf1(ModData, NrfRequest) ->
	RatingDataRef = unique(),
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				{Flag, LogEventType} = case NrfMap of
					#{"oneTimeEvent" := true, "oneTimeEventType" := "IEC"} ->
						{event, event};
					_ ->
						{initial, start}
				end,
				case rate(RatingDataRef, NrfMap, Flag) of
					{ok, ServiceRating, Rated} when Flag == event ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						NrfResponse = nrf(UpdatedMap),
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						{LogEventType, NrfResponse, LogRequest, UpdatedMap, Rated};
					{ok, ServiceRating, _Rated} ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = add_ref(RatingDataRef, UpdatedMap),
						NrfResponse = nrf(UpdatedMap),
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						{LogEventType, NrfResponse, LogRequest, UpdatedMap, undefined};
					{out_of_credit, _ServiceRating, _Rated} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, LogEventType, LogRequest, Problem};
					{error, service_not_found = Reason} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 404, LogEventType, LogRequest, Problem};
					{error, invalid_service_type = Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 400, LogEventType, LogRequest, Problem};
					{error, _Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(charging_failed, undefined),
						{error, 500, LogEventType, LogRequest, Problem}
				end;
			_ ->
				error_logger:warning_report(["Unable to process Nrf request",
						{ratingDataRef, RatingDataRef}, {request, NrfRequest},
						{operation, start}, {error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{LogEventType1, {struct, _} = NrfResponse1, LogRequest1, LogResponse, Rated1} ->
			Location = "/nrf-rating/v1/ratingdata/" ++ RatingDataRef,
			Headers = [{content_type, "application/json"}, {location, Location}],
			ResponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), LogEventType1,
					LogRequest1, LogResponse, Rated1),
			{ok, Headers, ResponseBody};
		{error, StatusCode, LogEventType1, LogRequest1, Problem1} ->
			ok = ocs_log:acct_log(nrf, server(ModData), LogEventType1,
					LogRequest1, Problem1, undefined),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process Nrf request",
					{ratingDataRef, RatingDataRef}, {request, NrfRequest},
					{operation, start}, {error, Reason1}, {stack, StackTrace}]),
			{error, 500}
	end.
	
-spec update_nrf(ModData, RatingDataRef, NrfRequest) -> NrfResponse
	when
		ModData ::#mod{},
		RatingDataRef :: string(),
		NrfRequest :: iolist(),
		NrfResponse :: {200, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /nrf-rating/v1/ratingdata/{RatingDataRef}/update'.
%%		Rate an interim Nrf Request.
update_nrf(ModData, RatingDataRef, NrfRequest) ->
	case authorize_rating(ModData) of
		{ok, authorized} ->
			update_nrf1(ModData, RatingDataRef, NrfRequest);
		{error, Status} ->
			{error, Status};
		{error, Status, Problem} ->
			{error, Status, Problem}
	end.
%% @hidden
update_nrf1(ModData, RatingDataRef, NrfRequest) ->
	case lookup_ref(RatingDataRef) of
		true ->
			update_nrf2(ModData, RatingDataRef, NrfRequest);
		false ->
			InvalidParams = [#{param => "{" ++ RatingDataRef ++ "}",
					reason => "Unknown rating data reference"}],
			Problem = rest_error_response(unknown_ref, InvalidParams),
			{error, 404, Problem};
		{error, _Reason} ->
			{error, 500}
	end.
%% @hidden
update_nrf2(ModData, RatingDataRef, NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				case rate(RatingDataRef, NrfMap, interim) of
					{ok, ServiceRating, _Rated} ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						NrfResponse = nrf(UpdatedMap),
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						{NrfResponse, LogRequest, UpdatedMap};
					{out_of_credit, _ServiceRating, _Rated} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, LogRequest, Problem};
					{error, service_not_found = Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 404, LogRequest, Problem};
					{error, invalid_service_type = Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 400, LogRequest, Problem};
					{error, _Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(charging_failed, undefined),
						{error, 500, LogRequest, Problem}
				end;
			_Other->
				error_logger:warning_report(["Unable to process Nrf request",
						{ratingDataRef, RatingDataRef}, {request, NrfRequest},
						{operation, update}, {error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest1, LogResponse} ->
			Headers = [{content_type, "application/json"}],
			ResponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), update,
					LogRequest1, LogResponse, undefined),
			{200, Headers, ResponseBody};
		{error, StatusCode, LogRequest1, Problem1} ->
			ok = ocs_log:acct_log(nrf, server(ModData), update,
					LogRequest1, Problem1, undefined),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process Nrf request",
					{ratingDataRef, RatingDataRef}, {request, NrfRequest},
					{operation, update}, {error, Reason1}, {stack, StackTrace}]),
			{error, 500}
	end.

-spec release_nrf(ModData, RatingDataRef, NrfRequest) -> NrfResponse
	when
		ModData ::#mod{},
		NrfRequest :: iolist(),
		RatingDataRef :: string(),
		NrfResponse :: {200, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /nrf-rating/v1/ratingdata/{RatingDataRef}/release'.
%%
%%		Rate a final Nrf Request.
%%
release_nrf(ModData, RatingDataRef, NrfRequest) ->
	case authorize_rating(ModData) of
		{ok, authorized} ->
			release_nrf1(ModData, RatingDataRef, NrfRequest);
		{error, Status} ->
			{error, Status};
		{error, Status, Problem} ->
			{error, Status, Problem}
	end.
%% @hidden
release_nrf1(ModData, RatingDataRef, NrfRequest) ->
	case lookup_ref(RatingDataRef) of
		true ->
			release_nrf2(ModData, RatingDataRef, NrfRequest);
		false ->
			InvalidParams = [#{param => "{" ++ RatingDataRef ++ "}",
					reason => "Unknown rating data reference"}],
			Problem = rest_error_response(unknown_ref, InvalidParams),
			{error, 404, Problem};
		{error, _Reason} ->
			{error, 500}
	end.
%% @hidden
release_nrf2(ModData, RatingDataRef, NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				case rate(RatingDataRef, NrfMap, final) of
					{ok, ServiceRating, Rated} ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						NrfResponse = nrf(UpdatedMap),
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						{NrfResponse, LogRequest, UpdatedMap, Rated};
					{out_of_credit, _ServiceRating, Rated} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, LogRequest, Problem, Rated};
					{error, service_not_found = Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 404, LogRequest, Problem, []};
					{error, invalid_service_type = Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 400, LogRequest, Problem, []};
					{error, _Reason} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(charging_failed, undefined),
						{error, 500, LogRequest, Problem, []}
				end;
			_ ->
				error_logger:warning_report(["Unable to process Nrf request",
						{ratingDataRef, RatingDataRef}, {request, NrfRequest},
						{operation, release}, {error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest1, LogResponse, Rated1} ->
			ok = remove_ref(RatingDataRef),
			Headers = [{content_type, "application/json"}],
			ResponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), stop,
					LogRequest1, LogResponse, Rated1),
			{200, Headers, ResponseBody};
		{error, StatusCode, LogRequest1, Problem1, Rated1} ->
			ok = remove_ref(RatingDataRef),
			ok = ocs_log:acct_log(nrf, server(ModData), stop,
					LogRequest1, Problem1, Rated1),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			ok = remove_ref(RatingDataRef),
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			ok = remove_ref(RatingDataRef),
			error_logger:warning_report(["Unable to process Nrf request",
					{ratingDataRef, RatingDataRef}, {request, NrfRequest},
					{operation, release}, {error, Reason1}, {stack, StackTrace}]),
			{error, 500}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec remove_ref(RatingDataRef) -> Result
	when
		RatingDataRef :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Remove a rating data ref.
%% @hidden
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
%% @hidden
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

-spec add_ref(RatingDataRef, NrfMap) -> Result
	when
		RatingDataRef :: string(),
		NrfMap :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Add a rating data ref to the rating ref table.
%% @hidden
add_ref(RatingDataRef,
		#{"nfConsumerIdentification" := #{"nodeFunctionality" := NF},
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
%% @doc Construct a problem report for an error response.
%% @hidden
rest_error_response(out_of_credit, undefined) ->
	#{cause => "QUOTA_LIMIT_REACHED",
			status => 403,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.8#/",
			title => "Request denied due to insufficient credit (usage applied)"};
rest_error_response(service_not_found, InvalidParams) ->
	#{cause => "SUBSCRIPTION_NOT_FOUND",
			status => 404,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.8#/",
			title => "Request denied because the subscriber identity is unrecognized",
			invalidParams => InvalidParams};
rest_error_response(charging_failed, undefined) ->
	#{cause => "CHARGING_FAILED",
			status => 500,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.8#/",
			title => "Incomplete or erroneous session or subscriber information"};
rest_error_response(unknown_ref, InvalidParams) ->
	#{cause => "RATING_DATA_REF_UNKNOWN",
			status => 404,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.8#/",
			title => "Request denied because the rating data ref is not recognized",
			invalidParams => InvalidParams};
rest_error_response(invalid_service_type, InvalidParams) ->
	#{cause => "INVALID_SERVICE_TYPE",
			status => 400,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.8#/",
			title => "Request denied because the service context id is not recognized",
			invalidParams => InvalidParams};
rest_error_response(decode_failed, undefined) ->
	#{cause => "DECODING_FAILED",
			status => 400,
			title => "Request body could not be parsed as valid JSON"};
rest_error_response(no_such_user, Username) ->
	#{cause => "UNKNOWN_USER",
			status => 403,
			type => "https://www.rfc-editor.org/rfc/rfc9110#field.authorization",
			title => "The provided credentials are not recognized",
			details => "Username: " ++ Username};
rest_error_response(bad_password, Username) ->
	#{cause => "FAILED_AUTHENTICATION",
			status => 403,
			type => "https://www.rfc-editor.org/rfc/rfc9110#field.authorization",
			title => "The provided credentials are not authenticated",
			details => "Username: " ++ Username};
rest_error_response(not_authorized, Username) ->
	#{cause => "NOT_AUTHORIZED",
			status => 403,
			type => "https://www.rfc-editor.org/rfc/rfc9110#field.authorization",
			title => "The provided credentials are not authorized",
			details => "Username: " ++ Username};
rest_error_response(httpd_directory_undefined, undefined) ->
	#{cause => "DIRECTORY_UNDEFINED",
			status => 500,
			type => "https://www.erlang.org/doc/apps/inets/httpd#props_auth",
			title => "The inets httpd configuration is missing `directory` for mod_auth"}.

-spec rate(RatingDataRef, NrfRequest, Flag) -> Result
	when
		RatingDataRef :: string(),
		NrfRequest :: map(),
		Flag :: initial | interim | final | event,
		Result :: {RateResult, ServiceRating, Rated} | {error, Reason},
		RateResult :: ok | out_of_credit,
		ServiceRating :: [map()],
		Rated :: ocs_log:acct_rated(),
		Reason :: offer_not_found | product_not_found | service_not_found
				| invalid_service_type | invalid_bundle_product | term().
%% @doc Rate Nrf `ServiceRatingRequest's.
%% @hidden
rate(RatingDataRef, #{"subscriptionId" := SubscriptionIds} = NrfRequest, Flag) ->
	case maps:get("serviceRating", NrfRequest, []) of
		ServiceRating when length(ServiceRating) > 0 ->
			rate(list_to_binary(RatingDataRef),
					Flag, SubscriptionIds, ServiceRating, []);
		[] = _ServiceRating ->
			{error, invalid_service_type}
	end.
%% @hidden
rate(RatingDataRef, Flag, SubscriptionIds,
		[#{"serviceContextId" := SCI} = H | T], Acc) ->
	ChargingKey = case maps:find("ratingGroup", H) of
		{ok, RG} ->
			RG;
		_ ->
			undefined
	end,
	ServiceId = case maps:find("serviceId", H) of
		{ok, SI} ->
			SI;
		_ ->
			undefined
	end,
	{ServiceNetwork, Direction} = case maps:find("serviceInformation", H) of
		{ok, ServiceInformation} ->
			{service_network(ServiceInformation), direction(ServiceInformation)};
		_ ->
			{undefined, undefined}
	end,
	Address = case Direction of
		originate ->
			case maps:find("destinationId", H) of
				{ok, DI} ->
					hd([D || #{"destinationIdType" := "DN",
							"destinationIdData" := D} <- DI]);
				_ ->
					undefined
			end;
		answer ->
			case maps:find("originationId", H) of
				{ok, OI} ->
					hd([D || #{"originationIdType" := "DN",
							"originationIdData" := D} <- OI]);
				_ ->
					undefined
			end;
		undefined ->
			undefined
	end,
	Reserve = case maps:find("requestedUnit", H) of
		{ok, #{"totalVolume" := RA}} when RA > 0->
			[{octets, RA}];
		{ok, #{"time" := RA}} when RA > 0 ->
			[{seconds, RA}];
		{ok, #{"serviceSpecificUnit" := RA}} when RA > 0 ->
			[{messages, RA}];
		_ ->
			[]
	end,
	Debit = case maps:find("consumedUnit", H) of
		{ok, #{"totalVolume" := DA}} when DA > 0 ->
			[{octets, DA}];
		{ok, #{"uplinkVolume" := UL, "downlinkVolume" := DL}} when (UL + DL) > 0 ->
			[{octets, UL + DL}];
		{ok, #{"uplinkVolume" := UL}} when UL > 0 ->
			[{octets, UL}];
		{ok, #{"downlinkVolume" := DL}} when DL > 0 ->
			[{octets, DL}];
		{ok, #{"time" := DA}} when DA > 0 ->
			[{seconds, DA}];
		{ok, #{"serviceSpecificUnit" := DA}} when DA > 0 ->
			[{messages, DA}];
		_ ->
			[]
	end,
	ServiceType = service_type(SCI),
	SessionAttributes = case {ChargingKey, maps:find("uPFID", H)} of
		{undefined, error} ->
			[{nrf_ref, RatingDataRef}];
		{undefined, {ok, UpfId}} ->
			[{nrf_ref, RatingDataRef}, {upfid, UpfId}];
		{RG1, error} ->
			[{nrf_ref, RatingDataRef}, {rg, RG1}];
		{RG1, {ok, UpfId}} ->
			[{nrf_ref, RatingDataRef}, {upfid, UpfId}, {rg, RG1}]
	end,
	Args = {ServiceType, ChargingKey, ServiceId, ServiceNetwork,
			Address, Direction, SessionAttributes, Debit, Reserve},
	rate(RatingDataRef, Flag, SubscriptionIds, T, [Args | Acc]);
rate(RatingDataRef, Flag, SubscriptionIds, [], Acc) ->
	rate1(RatingDataRef, Flag, SubscriptionIds, lists:sort(Acc), []).
%% @hidden
rate1(RatingDataRef, Flag, SubscriptionIds,
		[{ServiceType, ChargingKey, ServiceId, ServiceNetwork,
				Address, Direction, SessionAttributes, Debit1, Reserve1},
		{ServiceType, ChargingKey, ServiceId, ServiceNetwork,
				Address, Direction, SessionAttributes, Debit2, Reserve2}
		| T], Acc) ->
	Debit3 = combine(Debit1, Debit2),
	Reserve3 = combine(Reserve1, Reserve2),
	rate1(RatingDataRef, Flag, SubscriptionIds,
			[{ServiceType, ChargingKey, ServiceId, ServiceNetwork,
					Address, Direction, SessionAttributes, Debit3, Reserve3}
			| T], Acc);
rate1(RatingDataRef, Flag, SubscriptionIds, [Args1, Args2 | T], Acc) ->
	rate1(RatingDataRef, Flag, SubscriptionIds, [Args2 | T], [Args1 | Acc]);
rate1(RatingDataRef, Flag, SubscriptionIds, [Args1], Acc) ->
	rate2(RatingDataRef, Flag, SubscriptionIds, [Args1 | Acc], [], []).
%% @hidden
rate2(RatingDataRef, Flag, SubscriptionIds,
		[{ServiceType, ChargingKey, ServiceId, ServiceNetwork, Address,
		Direction, SessionAttributes, Debits, Reserves} | T], AccS, AccR) ->
	TS = calendar:universal_time(),
	SR1 = #{"serviceContextId" => integer_to_list(ServiceType) ++ "@3gpp.org"},
	SR2 = case is_integer(ChargingKey) of
		true ->
			SR1#{"ratingGroup" => ChargingKey};
		false ->
			SR1#{}
	end,
	SR3 = case is_integer(ServiceId) of
		true ->
			SR2#{"serviceId" => ServiceId};
		false ->
			SR2
	end,
	SR4 = case lists:keyfind(upfid, 1, SessionAttributes) of
		{_, UpfId} ->
			SR3#{"uPFID" =>  UpfId};
		false ->
			SR3
	end,
	case ocs_rating:rate(nrf, ServiceType, ServiceId, ChargingKey,
			ServiceNetwork, subscriber_id(SubscriptionIds), TS, Address, Direction,
			Flag, Debits, Reserves, SessionAttributes) of
		{ok, _Service, {octets, Amount} = _GrantedAmount}
				when Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"totalVolume" => Amount}},
			SR6 = case application:get_env(ocs, nrf_valid_volume) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					SR5#{"validUnits" => Threshold};
				_ ->
					SR5
			end,
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR6 | AccS], AccR);
		{ok, _Service, {seconds, Amount} = _GrantedAmount}
				when Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"time" => Amount}},
			SR6 = case application:get_env(ocs, nrf_valid_time) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					SR5#{"validUnits" => Threshold};
				_ ->
					SR5
			end,
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR6 | AccS], AccR);
		{ok, _Service, {messages, Amount} = _GrantedAmount}
				when Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"serviceSpecificUnit" => Amount}},
			SR6 = case application:get_env(ocs, nrf_valid_unit) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					SR5#{"validUnits" => Threshold};
				_ ->
					SR5
			end,
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR6 | AccS], AccR);
		{ok, _Service, {_, 0} = _GrantedAmount} ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], AccR);
		{ok, _Service, {octets, Amount} = _GrantedAmount, Rated}
				when Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"totalVolume" => Amount}},
			SR6 = case application:get_env(ocs, nrf_valid_volume) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					SR5#{"validUnits" => Threshold};
				_ ->
					SR5
			end,
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR6 | AccS], [Rated | AccR]);
		{ok, _Service, {seconds, Amount} = _GrantedAmount, Rated}
				when Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"time" => Amount}},
			SR6 = case application:get_env(ocs, nrf_valid_time) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					SR5#{"validUnits" => Threshold};
				_ ->
					SR5
			end,
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR6 | AccS], [Rated | AccR]);
		{ok, _, {messages, Amount} = _GrantedAmount, Rated}
				when Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"serviceSpecificUnit" => Amount}},
			SR6 = case application:get_env(ocs, nrf_valid_unit) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					SR5#{"validUnits" => Threshold};
				_ ->
					SR5
			end,
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR6 | AccS], [Rated | AccR]);
		{ok, _Service, Rated} ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], [Rated | AccR]);
		{out_of_credit, _RedirectServerAddress, _SessionList} ->
			SR5 = SR4#{"resultCode" => "QUOTA_LIMIT_REACHED"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], AccR);
		{out_of_credit, _RedirectServerAddress, _SessionList, Rated} ->
			SR5 = SR4#{"resultCode" => "QUOTA_LIMIT_REACHED"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], [Rated | AccR]);
		{disabled, _SessionList} ->
			{error, service_rejected};
		{error, Reason} ->
			{error, Reason}
	end;
rate2(_, _, _, [], AccS, AccR) when length(AccS) > 0 ->
	Rated = lists:flatten(lists:reverse(AccR)),
	F = fun(#{"resultCode" := "SUCCESS"}) ->
				true;
			(_) ->
				false
	end,
	case lists:any(F, AccS) of
		true ->
			{ok, AccS, Rated};
		false ->
			{out_of_credit, AccS, Rated}
	end;
rate2(_, _, _, [], AccS, AccR) ->
	Rated = lists:flatten(lists:reverse(AccR)),
	{ok, AccS, Rated}.

-spec nrf(Nrf) -> Nrf
	when
		Nrf :: map() | {struct, [tuple()]}.
%% @doc CODEC for Nrf body.
%% @hidden
nrf({struct, StructList}) ->
	nrf1(StructList, #{});
nrf(NrfRequest) when is_map(NrfRequest) ->
	nrf1(NrfRequest).
%% @hidden
nrf1(#{"invocationTimeStamp" := TS,
		"invocationSequenceNumber" := SeqNum,
		"subscriptionId" := SubIds,
		"nfConsumerIdentification" := #{"nodeFunctionality" := NF},
		"serviceRating" := ServiceRating}) ->
	{struct, [{"invocationTimeStamp", TS},
			{"invocationSequenceNumber", SeqNum},
			{"subscriptionId", {array, SubIds}},
			{"nfConsumerIdentification",
					{struct, [{"nodeFunctionality", NF}]}},
			{"serviceRating",
					{array, service_rating(ServiceRating)}}]}.
%% @hidden
nrf1([{"invocationTimeStamp", TS} | T], Acc) ->
	nrf1(T, Acc#{"invocationTimeStamp" => TS});
nrf1([{"oneTimeEvent", OneTimeEvent} | T], Acc)
		when is_boolean(OneTimeEvent) ->
	nrf1(T, Acc#{"oneTimeEvent" => OneTimeEvent});
nrf1([{"oneTimeEventType", EventType} | T], Acc) ->
	nrf1(T, Acc#{"oneTimeEventType" => EventType});
nrf1([{"invocationSequenceNumber", SeqNum} | T], Acc) ->
	nrf1(T, Acc#{"invocationSequenceNumber" => SeqNum});
nrf1([{"subscriptionId", SubscriptionIds} | T], Acc) ->
	nrf1(T, subscriptionId_map(SubscriptionIds, Acc));
nrf1([{"nfConsumerIdentification",
		{struct, NfConsumerIdentification}} | T], Acc) ->
	{_, NF} = lists:keyfind("nodeFunctionality",
			1, NfConsumerIdentification),
	nrf1(T, Acc#{"nfConsumerIdentification" => #{"nodeFunctionality" => NF}});
nrf1([{"serviceRating", {array, ServiceRating}} | T], Acc) ->
	nrf1(T, Acc#{"serviceRating" => service_rating(ServiceRating)});
nrf1([_H | T], Acc) ->
	nrf1(T, Acc);
nrf1([], Acc) ->
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

-spec subscriber_id(SubscriptionIds) -> SubscriberIDs
	when
		SubscriptionIds :: [string()],
		SubscriberIDs :: [string()].
%% @hidden Get a filtered list of subscriber IDs in priority order.
subscriber_id(SubscriptionIds) ->
	{ok, IdTypes} = application:get_env(ocs, nrf_sub_id_type),
	subscriber_id(IdTypes, SubscriptionIds, []).
%% @hidden
subscriber_id([imsi | T], SubscriptionIds, Acc) ->
	case get_imsi(SubscriptionIds) of
		IMSI when is_list(IMSI) ->
			subscriber_id(T, SubscriptionIds, [IMSI | Acc]);
		undefined ->
			subscriber_id(T, SubscriptionIds, Acc)
	end;
subscriber_id([msisdn | T], SubscriptionIds, Acc) ->
	case get_msisdn(SubscriptionIds) of
		MSISDN when is_list(MSISDN) ->
			subscriber_id(T, SubscriptionIds, [MSISDN | Acc]);
		undefined ->
		subscriber_id(T, SubscriptionIds, Acc)
	end;
subscriber_id([nai | T], SubscriptionIds, Acc) ->
	case get_nai(SubscriptionIds) of
		NAI when is_list(NAI) ->
			subscriber_id(T, SubscriptionIds, [NAI | Acc]);
		undefined ->
			subscriber_id(T, SubscriptionIds, Acc)
	end;
subscriber_id([gci | T], SubscriptionIds, Acc) ->
	case get_gci(SubscriptionIds) of
		GCI when is_list(GCI) ->
			subscriber_id(T, SubscriptionIds, [GCI | Acc]);
		undefined ->
			subscriber_id(T, SubscriptionIds, Acc)
	end;
subscriber_id([gli | T], SubscriptionIds, Acc) ->
	case get_gli(SubscriptionIds) of
		GLI when is_list(GLI) ->
			subscriber_id(T, SubscriptionIds, [GLI | Acc]);
		undefined ->
			subscriber_id(T, SubscriptionIds, Acc)
	end;
subscriber_id([iccid | T], SubscriptionIds, Acc) ->
	case get_iccid(SubscriptionIds) of
		ICCID when is_list(ICCID) ->
			subscriber_id(T, SubscriptionIds, [ICCID | Acc]);
		undefined ->
			subscriber_id(T, SubscriptionIds, Acc)
	end;
subscriber_id([_ | T], SubscriptionIds, Acc) ->
	subscriber_id(T, SubscriptionIds, Acc);
subscriber_id([], _SubscriptionIds, Acc) ->
	lists:reverse(Acc).

%% @hidden
get_imsi(["imsi-" ++ IMSI | _]) ->
	IMSI;
get_imsi([_ | T]) ->
	get_imsi(T);
get_imsi([]) ->
	undefined.

%% @hidden
get_msisdn(["msisdn-" ++ MSISDN | _]) ->
	MSISDN;
get_msisdn([_ | T]) ->
	get_msisdn(T);
get_msisdn([]) ->
	undefined.

%% @hidden
get_nai(["nai-" ++ IMSI | _]) ->
	IMSI;
get_nai([_ | T]) ->
	get_nai(T);
get_nai([]) ->
	undefined.

%% @hidden
get_gci(["gci-" ++ IMSI | _]) ->
	IMSI;
get_gci([_ | T]) ->
	get_gci(T);
get_gci([]) ->
	undefined.

%% @hidden
get_gli(["gli-" ++ IMSI | _]) ->
	IMSI;
get_gli([_ | T]) ->
	get_gli(T);
get_gli([]) ->
	undefined.

%% @hidden
get_iccid(["iccid-" ++ IMSI | _]) ->
	IMSI;
get_iccid([_ | T]) ->
	get_iccid(T);
get_iccid([]) ->
	undefined.

-spec service_rating(ServiceRating) -> ServiceRating
	when
		ServiceRating :: [map()] | [{struct, [tuple()]}].
%% @doc CODEC for Service Rating.
%% @hidden
service_rating(ServiceRating)
		when is_list(ServiceRating) ->
	service_rating(ServiceRating, []).
%% @hidden
service_rating([H | T], Acc) ->
	service_rating(T, [service_rating1(H) | Acc]);
service_rating([], Acc) ->
	lists:reverse(Acc).
%% @hidden
service_rating1({struct, SR}) ->
	sr_in(SR, #{});
service_rating1(SR) when is_map(SR) ->
	sr_out(SR, []).

%% @hidden
sr_out(#{"resultCode" := RC} = M, Acc) ->
	sr_out1(M, [{"resultCode", RC} | Acc]);
sr_out(M, Acc) ->
	sr_out1(M, Acc).
%% @hidden
sr_out1(#{"serviceInformation" := SI} = M, Acc) ->
	sr_out2(M, [{"serviceInformation", service_information(SI)} | Acc]);
sr_out1(M, Acc) ->
	sr_out2(M, Acc).
%% @hidden
sr_out2(#{"uPFID" := UPFID} = M, Acc) ->
	sr_out3(M, [{"uPFID", UPFID} | Acc]);
sr_out2(M, Acc) ->
	sr_out3(M, Acc).
%% @hidden
sr_out3(#{"validUnits" := VU} = M, Acc) ->
	sr_out4(M, [{"validUnits", VU} | Acc]);
sr_out3(M, Acc) ->
	sr_out4(M, Acc).
%% @hidden
sr_out4(#{"grantedUnit" := Units} = M, Acc) ->
	sr_out5(M, [{"grantedUnit", {struct, maps:to_list(Units)}} | Acc]);
sr_out4(M, Acc) ->
	sr_out5(M, Acc).
%% @hidden
sr_out5(#{"consumedUnit" := Units} = M, Acc) ->
	sr_out6(M, [{"consumedUnit", {struct, maps:to_list(Units)}} | Acc]);
sr_out5(M, Acc) ->
	sr_out6(M, Acc).
%% @hidden
sr_out6(#{"ratingGroup" := RG} = M, Acc) ->
	sr_out7(M, [{"ratingGroup", RG} | Acc]);
sr_out6(M, Acc) ->
	sr_out7(M, Acc).
%% @hidden
sr_out7(#{"serviceId" := SI} = M, Acc) ->
	sr_out8(M, [{"serviceId", SI} | Acc]);
sr_out7(M, Acc) ->
	sr_out8(M, Acc).
%% @hidden
sr_out8(#{"serviceContextId" := SCI} = _M, Acc)
		when is_list(SCI) ->
	{struct, [{"serviceContextId", SCI} | Acc]};
sr_out8(_M, Acc) ->
	{struct, Acc}.

%% @hidden
sr_in([{"serviceContextId", SCI} | T], Acc)
		when is_list(SCI) ->
	sr_in(T, Acc#{"serviceContextId" => SCI});
sr_in([{"serviceId", SI} | T], Acc)
		when is_integer(SI) ->
	sr_in(T, Acc#{"serviceId" => SI});
sr_in([{"ratingGroup", RG} | T], Acc)
		when is_integer(RG) ->
	sr_in(T, Acc#{"ratingGroup" => RG});
sr_in([{"originationId", {array, OI}} | T], Acc) ->
	sr_in(T, Acc#{"originationId" => origination_id(OI)});
sr_in([{"destinationId", {array, DI}} | T], Acc) ->
	sr_in(T, Acc#{"destinationId" => destination_id(DI)});
sr_in([{"requestSubType", "RESERVE"} | T], Acc) ->
	sr_in(T, Acc#{"requestSubType" => "RESERVE"});
sr_in([{"requestSubType", "DEBIT"} | T], Acc) ->
	sr_in(T, Acc#{"requestSubType" => "DEBIT"});
sr_in([{"uPFID", UPFID} | T], Acc)
		when is_list(UPFID) ->
	sr_in(T, Acc#{"uPFID" => UPFID});
sr_in([{"requestedUnit", {struct, Units}} | T], Acc) ->
	sr_in(T, Acc#{"requestedUnit" => maps:from_list(Units)});
sr_in([{"consumedUnit", {struct, Units}} | T], Acc) ->
	sr_in(T, Acc#{"consumedUnit" => maps:from_list(Units)});
sr_in([{"serviceInformation", {struct, SI}} | T], Acc) ->
	sr_in(T, Acc#{"serviceInformation" => service_information(SI)});
sr_in([_Other | T], Acc) ->
	sr_in(T, Acc);
sr_in([], Acc) ->
	Acc.

%% @hidden
origination_id(OriginationId) ->
	origination_id(OriginationId, []).
%% @hidden
origination_id([{struct, OI} | T], Acc) ->
	origination_id(T, [origination_id1(OI, #{}) | Acc]);
origination_id([_H | T], Acc) ->
	origination_id(T, Acc);
origination_id([], Acc) ->
	lists:reverse(Acc).
%% @hidden
origination_id1([{"originationIdType", Type} | T], Acc)
		when is_list(Type) ->
	origination_id1(T, Acc#{"originationIdType" => Type});
origination_id1([{"originationIdData", Data} | T], Acc)
		when is_list(Data) ->
	origination_id1(T, Acc#{"originationIdData" => Data});
origination_id1([_H | T], Acc) ->
	origination_id1(T, Acc);
origination_id1([], Acc) ->
	Acc.

%% @hidden
destination_id(DestinationId) ->
	destination_id(DestinationId, []).
%% @hidden
destination_id([{struct, DI} | T], Acc) ->
	destination_id(T, [destination_id1(DI, #{}) | Acc]);
destination_id([_H | T], Acc) ->
	destination_id(T, Acc);
destination_id([], Acc) ->
	lists:reverse(Acc).
%% @hidden
destination_id1([{"destinationIdType", Type} | T], Acc)
		when is_list(Type) ->
	destination_id1(T, Acc#{"destinationIdType" => Type});
destination_id1([{"destinationIdData", Data} | T], Acc)
		when is_list(Data) ->
	destination_id1(T, Acc#{"destinationIdData" => Data});
destination_id1([_H | T], Acc) ->
	destination_id1(T, Acc);
destination_id1([], Acc) ->
	Acc.

%% @hidden
service_information(ServiceInformation) ->
	si_in(ServiceInformation, #{}).

%% @hidden
si_in([{"servingNodeType", SNT} | T], Acc)
		when is_list(SNT) ->
	si_in(T, Acc#{"servingNodeType" => SNT});
si_in([{"sgsnMccMnc", {struct, MccMnc}} | T], Acc)
		when is_list(MccMnc) ->
	si_in(T, Acc#{"sgsnMccMnc" => maps:from_list(MccMnc)});
si_in([{"userLocationinfo", {struct, ULI}} | T], Acc)
		when is_list(ULI) ->
	si_in(T, Acc#{"userLocationinfo" => user_location_info(ULI)});
si_in([{"visitedNetworkIdentifier", VNI} | T], Acc)
		when is_list(VNI) ->
	si_in(T, Acc#{"visitedNetworkIdentifier" => VNI});
si_in([{"nodeFunctionality", NF} | T], Acc)
		when is_list(NF) ->
	si_in(T, Acc#{"nodeFunctionality" => NF});
si_in([{"roleOfNode", RON} | T], Acc)
		when is_list(RON) ->
	si_in(T, Acc#{"roleOfNode" => RON});
si_in([_ | T], Acc) ->
	si_in(T, Acc);
si_in([], Acc) ->
	Acc.

%% @hidden
user_location_info(ULI) ->
	user_location_info(ULI, #{}).
%% @hidden
user_location_info([{"utraLocation", {struct, Location}} | T], Acc)
		when is_list(Location) ->
	case user_location_info1(Location, #{}) of
		ULI when map_size(ULI) > 0 ->
			Acc#{"utraLocation" => ULI};
		_ULI ->
			user_location_info(T, Acc)
	end;
user_location_info([{"eutraLocation", {struct, Location}} | T], Acc)
		when is_list(Location) ->
	case user_location_info1(Location, #{}) of
		ULI when map_size(ULI) > 0 ->
			Acc#{"eutraLocation" => ULI};
		_ULI ->
			user_location_info(T, Acc)
	end;
user_location_info([{"nrLocation", {struct, Location}} | T], Acc)
		when is_list(Location) ->
	case user_location_info1(Location, #{}) of
		ULI when map_size(ULI) > 0 ->
			Acc#{"nrLocation" => ULI};
		_ULI ->
			user_location_info(T, Acc)
	end;
user_location_info([{"n3gaLocation", {struct, Location}} | T], Acc)
		when is_list(Location) ->
	case user_location_info1(Location, #{}) of
		ULI when map_size(ULI) > 0 ->
			Acc#{"nrLocation" => ULI};
		_ULI ->
			user_location_info(T, Acc)
	end;
user_location_info([_ | T], Acc) ->
	user_location_info(T, Acc);
user_location_info([], Acc) ->
	Acc.

%% @hidden
user_location_info1([{"cgi", {struct, CGI}} | T], Acc)
		when is_list(CGI) ->
	case user_location_info2(CGI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"cgi" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([{"ecgi", {struct, ECGI}} | T], Acc)
		when is_list(ECGI) ->
	case user_location_info2(ECGI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"ecgi" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([{"ncgi", {struct, NCGI}} | T], Acc)
		when is_list(NCGI) ->
	case user_location_info2(NCGI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"ncgi" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([{"tai", {struct, TAI}} | T], Acc)
		when is_list(TAI) ->
	case user_location_info2(TAI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"tai" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([{"sai", {struct, SAI}} | T], Acc)
		when is_list(SAI) ->
	case user_location_info2(SAI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"sai" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([{"rai", {struct, RAI}} | T], Acc)
		when is_list(RAI) ->
	case user_location_info2(RAI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"rai" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([{"n3gppTai", {struct, TAI}} | T], Acc)
		when is_list(TAI) ->
	case user_location_info2(TAI, #{}) of
		PLMN when map_size(PLMN) > 0 ->
			Acc#{"n3gppTai" => PLMN};
		_PLMN ->
			user_location_info1(T, Acc)
	end;
user_location_info1([_ | T], Acc) ->
	user_location_info1(T, Acc);
user_location_info1([], Acc) ->
	Acc.

%% @hidden
user_location_info2([{"plmnid", {struct, PLMN}} | _T], Acc)
		when is_list(PLMN) ->
	Acc#{"plmnid" => maps:from_list(PLMN)};
user_location_info2([_ | T], Acc) ->
	user_location_info2(T, Acc);
user_location_info2([], Acc) ->
	Acc.

-spec service_type(ServiceContextId) -> ServiceType
	when
		ServiceContextId :: string(),
		ServiceType :: pos_integer() | undefined.
%% @doc Convert service context identifier to number.
%% @hidden
service_type(ServiceContextId) ->
	try
		Len1 = length(ServiceContextId),
		{Rest, "@3gpp.org"} = lists:split(Len1 - 9, ServiceContextId),
		Len2 = length(Rest),
		{_Prefix, ServiceType} = lists:split(Len2 - 5, Rest),
		list_to_integer(ServiceType)
	catch
		_ ->
			undefined
	end.

-spec unique() -> Result
	when
		Result :: ID,
		ID :: string().
%% @doc Generate a unique identifier
%% @hidden
unique() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ integer_to_list(N).

-spec server(ModData) -> Result
	when
		ModData :: #mod{},
		Result :: {Address, Port} | undefined,
		Address :: inet:ip_address(),
		Port :: inet:port_number().
%% @doc Get server IP address and Port.
%% @hidden
server(#mod{socket = Socket, socket_type = ip_comm}) ->
	server1(inet:sockname(Socket));
server(#mod{socket = Socket, socket_type = {ip_comm, _Config}}) ->
	server1(inet:sockname(Socket));
server(#mod{socket = Socket, socket_type = {SSL, _Config}})
		when SSL == essl; SSL == ssl ->
	server1(ssl:sockname(Socket)).
%% @hidden
server1({ok, {Address, Port}}) ->
	{Address, Port};
server1({error, _Reason}) ->
	undefined.

-spec authorize_rating(ModData) -> Result
	when
		ModData :: #mod{},
		Result :: {ok, authorized} | {error, Status}
				| {error, Status, Problem},
		Status :: term(),
		Problem :: map().
%% @doc Do Authorization for Re interface.
%% @todo Handle other httpd configurations.
%% @hidden
authorize_rating(#mod{data = Data, config_db = ConfigDb} = _ModData) ->
	case lists:keyfind(remote_user, 1, Data) of
		{remote_user, Username} ->
			Dirs = [element(2, D) || D <- ets:lookup(ConfigDb, directory)],
			authorize_rating1(Username, Dirs,
					lists:keyfind("/nrf-rating", 1, Dirs));
		false ->
			{ok, authorized}
	end.
%% @hidden
authorize_rating1(Username, _Dirs, {Directory, Options}) ->
	authorize_rating3(Username, Directory, Options);
authorize_rating1(Username, Dirs, false) ->
	authorize_rating2(Username, lists:keyfind("/", 1, Dirs)).
%% @hidden
authorize_rating2(Username, {Directory, Options}) ->
	authorize_rating3(Username, Directory, Options);
authorize_rating2(_Username, false) ->
	Problem = rest_error_response(httpd_directory_undefined, undefined),
	{error, 500, Problem}.
%% @hidden
authorize_rating3(Username, Directory, Options) ->
	{_, Address} = lists:keyfind(bind_address, 1, Options),
	{_, Port} = lists:keyfind(port, 1, Options),
	authorize_rating4(Username, Address, Port, Directory).
%% @hidden
authorize_rating4(Username, Address, Port, Directory) ->
	case mod_auth:get_user(Username, Address, Port, Directory) of
		{ok, #httpd_user{user_data = UserData}} ->
			case lists:keyfind(rating, 1, UserData) of
				{rating, true} ->
					{ok, authorized};
				{rating, false} ->
					Problem = rest_error_response(not_authorized, Username),
					{error, 403, Problem}
			end;
		{error, no_such_user = Reason} ->
			Problem = rest_error_response(Reason, Username),
			{error, 403, Problem};
		{error, bad_password = Reason} ->
			Problem = rest_error_response(Reason, Username),
			{error, 403, Problem};
		{error, _Reason} ->
			{error, 500}
	end.

-spec service_network(ServiceInformation) -> ServiceNetwork
	when
		ServiceInformation :: map(),
		ServiceNetwork :: string() | undefined.
%% @doc Get serving PLMN.
%% @hidden
service_network(#{"sgsnMccMnc" := #{"mcc" := MCC, "mnc" := MNC}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"utraLocation"
		:= #{"cgi" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"utraLocation"
		:= #{"sai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"utraLocation"
		:= #{"rai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"eutraLocation"
		:= #{"ecgi" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"eutraLocation"
		:= #{"tai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"nrLocation"
		:= #{"ncgi" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"nrLocation"
		:= #{"tai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"n3gaLocation"
		:= #{"n3gppTai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(_) ->
	undefined.

-spec direction(ServiceInformation) -> Direction
	when
		ServiceInformation :: map(),
		Direction :: answer | originate | undefined.
%% @doc Get call/message direction.
%% @hidden
direction(#{"roleOfNode" := "ORIGINATING"}) ->
	originate;
direction(#{"roleOfNode" := "TERMINATING"}) ->
	answer;
direction(_) ->
	undefined.

% @hidden
combine([{Units, Amount1}], [{Units, Amount2}]) ->
	[{Units, Amount1 + Amount2}];
combine([{Units, Amount}], []) ->
	[{Units, Amount}];
combine([], [{Units, Amount}]) ->
	[{Units, Amount}];
combine([], []) ->
	[].

