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
				Flag = case NrfMap of
					#{"oneTimeEvent" := true, "oneTimeEventType" := "IEC"} ->
						event;
					_ ->
						initial
				end,
				case rate(RatingDataRef, NrfMap, Flag) of
					{ok, ServiceRating, _Rated} ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = add_rating_ref(RatingDataRef, UpdatedMap),
						NrfResponse = nrf(UpdatedMap),
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						{NrfResponse, LogRequest, UpdatedMap};
					{out_of_credit, _ServiceRating, _Rated} ->
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
						Problem = rest_error_response(out_of_credit, undefined),
						{error, 403, LogRequest, Problem};
					{error, service_not_found = Reason} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						LogRequest = NrfMap#{"ratingSessionId" => RatingDataRef},
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
			_ ->
				error_logger:warning_report(["Unable to process Nrf request",
						{ratingDataRef, RatingDataRef}, {request, NrfRequest},
						{operation, start}, {error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest1, LogResponse} ->
			Location = "/nrf-rating/v1/ratingdata/" ++ RatingDataRef,
			Headers = [{content_type, "application/json"}, {location, Location}],
			ResponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), start,
					LogRequest1, LogResponse, undefined),
			{ok, Headers, ResponseBody};
		{error, StatusCode, LogRequest1, Problem1} ->
			ocs_log:acct_log(nrf, server(ModData), start,
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
			ocs_log:acct_log(nrf, server(ModData), update,
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
						ok = remove_ref(RatingDataRef),
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
			Headers = [{content_type, "application/json"}],
			ResponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), stop,
					LogRequest1, LogResponse, Rated1),
			{200, Headers, ResponseBody};
		{error, StatusCode, LogRequest1, Problem1, Rated1} ->
			ocs_log:acct_log(nrf, server(ModData), stop,
					LogRequest1, Problem1, Rated1),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
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
%% @doc Construct a problem report for an error response.
rest_error_response(out_of_credit, undefined) ->
	#{cause => "QUOTA_LIMIT_REACHED",
			status => 403,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.7#/",
			title => "Request denied due to insufficient credit (usage applied)"};
rest_error_response(service_not_found, InvalidParams) ->
	#{cause => "SUBSCRIPTION_NOT_FOUND",
			status => 404,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.7#/",
			title => "Request denied because the subscriber identity is unrecognized",
			invalidParams => InvalidParams};
rest_error_response(charging_failed, undefined) ->
	#{cause => "CHARGING_FAILED",
			status => 500,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.7#/",
			title => "Incomplete or erroneous session or subscriber information"};
rest_error_response(unknown_ref, InvalidParams) ->
	#{cause => "RATING_DATA_REF_UNKNOWN",
			status => 404,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.7#/",
			title => "Request denied because the rating data ref is not recognized",
			invalidParams => InvalidParams};
rest_error_response(invalid_service_type, InvalidParams) ->
	#{cause => "INVALID_SERVICE_TYPE",
			status => 400,
			type => "https://app.swaggerhub.com/apis-docs/SigScale/nrf-rating/1.1.7#/",
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
				{ok, #{"destinationIdData" := DN}} ->
					DN;
				_ ->
					undefined
			end;
		answer ->
			case maps:find("originationId", H) of
				{ok, #{"originationIdData" := DN}} ->
					DN;
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
				when Flag == event, Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], AccR);
		{ok, _Service, {seconds, Amount} = _GrantedAmount}
				when Flag == event, Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], AccR);
		{ok, _Service, {messages, Amount} = _GrantedAmount}
				when Flag == event, Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], AccR);
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
				when Flag == event, Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], [Rated | AccR]);
		{ok, _Service, {seconds, Amount} = _GrantedAmount, Rated}
				when Flag == event, Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], [Rated | AccR]);
		{ok, _Service, {messages, Amount} = _GrantedAmount, Rated}
				when Flag == event, Amount > 0 ->
			SR5 = SR4#{"resultCode" => "SUCCESS"},
			rate2(RatingDataRef, Flag, SubscriptionIds, T,
					[SR5 | AccS], [Rated | AccR]);
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
%% @doc CODEC for Nrf Response.
nrf({struct, StructList}) ->
	nrf1(StructList, #{});
nrf(NrfRequest) when is_map(NrfRequest) ->
	nrf1(NrfRequest).
%% @hidden
nrf1(#{"invocationTimeStamp" := TS,
		"invocationSequenceNumber" := SeqNum,
		"subscriptionId" := SubIds,
		"nodeFunctionality" := NF,
		"serviceRating" := ServiceRating}) ->
	{struct, [{"invocationTimeStamp", TS},
			{"invocationSequenceNumber", SeqNum},
			{"subscriptionId", {array, SubIds}},
			{"nfConsumerIdentification",
					{struct, [{"nodeFunctionality", NF}]}},
			{"serviceRating",
					{array, struct_service_rating(ServiceRating)}}]}.
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
	nrf1(T, Acc#{"nodeFunctionality" => NF});
nrf1([{"serviceRating", {array, ServiceRating}} | T], Acc) ->
	nrf1(T, Acc#{"serviceRating" => map_service_rating(ServiceRating)});
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
	Acc7 = case maps:find("validUnits", H) of
		{ok, VU} ->
			[{"validUnits", VU} | Acc6];
		error ->
			Acc6
	end,
	Acc8 = case maps:find("uPFID", H) of
		{ok, UPFID} ->
			[{"uPFID", UPFID} | Acc7];
		error ->
			Acc7
	end,
	Acc9 = case maps:find("resultCode", H) of
		{ok, RC} ->
			[{"resultCode", RC} | Acc8];
		error ->
			Acc8
	end,
	struct_service_rating(T, [{struct, Acc9} | Acc]);
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
%% @private
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
service_network(#{"userLocationinfo" := #{"nrLocation"
		:= #{"ncgi" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"nrLocation"
		:= #{"tai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"n3gaLocation"
		:= #{"n3gppTai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(#{"userLocationinfo" := #{"eutraLocation"
		:= #{"tai" := #{"plmnid" := #{"mcc" := MCC, "mnc" := MNC}}}}}) ->
	MCC ++ MNC;
service_network(_) ->
	undefined.

-spec direction(ServiceInformation) -> Direction
	when
		ServiceInformation :: map(),
		Direction :: answer | originate | undefined.
%% @doc Get call/message direction.
%% @private
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

