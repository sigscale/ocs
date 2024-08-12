%% ocs_rest_res_nrf.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_nrf).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

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
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = add_rating_ref(RatingDataRef, UpdatedMap),
						NrfResponse = nrf(UpdatedMap),
						{NrfResponse, NrfMap, UpdatedMap};
					{error, out_of_credit = Reason} ->
						Problem = rest_error_response(Reason, undefined),
						{error, 403, NrfMap, Problem};
					{error, service_not_found = Reason} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 404, NrfMap, Problem};
					{error, invalid_service_type = Reason} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 400, NrfMap, Problem};
					{error, _Reason} ->
						Problem = rest_error_response(charging_failed, undefined),
						{error, 500, NrfMap, Problem}
				end;
			_ ->
				error_logger:warning_report(["Unable to process Nrf request",
						{RatingDataRef, ratingDataRef}, {request, NrfRequest}, {flag, start},
						{error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest, LogResponse} ->
			Location = "/ratingdata/" ++ RatingDataRef,
			ReponseBody = mochijson:encode(NrfResponse1),
			Headers = [{content_type, "application/json"}, {location, Location}],
			ok = ocs_log:acct_log(nrf, server(ModData), start,
					LogRequest, LogResponse, undefined),
			{ok, Headers, ReponseBody};
		{error, StatusCode, LogRequest, Problem1} ->
			ok = ocs_log:acct_log(nrf, server(ModData), start,
					LogRequest, Problem1, undefined),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process Nrf request",
					{RatingDataRef, ratingDataRef}, {request, NrfRequest},
					{flag, start}, {error, Reason1}, {stack, StackTrace}]),
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
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						NrfResponse = nrf(UpdatedMap),
						{NrfResponse, NrfMap, UpdatedMap};
					{error, out_of_credit = Reason} ->
						Problem = rest_error_response(Reason, undefined),
						{error, 403, NrfMap, Problem};
					{error, service_not_found = Reason} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 404, NrfMap, Problem};
					{error, invalid_service_type = Reason} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 400, NrfMap, Problem};
					{error, _Reason} ->
						Problem = rest_error_response(charging_failed, undefined),
						{error, 500, NrfMap, Problem}
				end;
			_ ->
				error_logger:warning_report(["Unable to process Nrf request",
						{RatingDataRef, ratingDataRef}, {request, NrfRequest},
						{flag, interim}, {error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest, LogResponse} ->
			ReponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), update,
					LogRequest, LogResponse, undefined),
			{200, [], ReponseBody};
		{error, StatusCode, LogRequest, Problem1} ->
			ok = ocs_log:acct_log(nrf, server(ModData), update,
					LogRequest, Problem1, undefined),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process Nrf request",
					{RatingDataRef, ratingDataRef}, {request, NrfRequest},
					{flag, interim}, {error, Reason1}, {stack, StackTrace}]),
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
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = remove_ref(RatingDataRef),
						NrfResponse = nrf(UpdatedMap),
						{NrfResponse, NrfMap, UpdatedMap};
					{error, out_of_credit = Reason} ->
						Problem = rest_error_response(Reason, undefined),
						{error, 403, NrfMap, Problem};
					{error, service_not_found = Reason} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 404, NrfMap, Problem};
					{error, invalid_service_type = Reason} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = rest_error_response(Reason, InvalidParams),
						{error, 400, NrfMap, Problem};
					{error, _Reason} ->
						Problem = rest_error_response(charging_failed, undefined),
						{error, 500, NrfMap, Problem}
				end;
			_ ->
				error_logger:warning_report(["Unable to process Nrf request",
						{RatingDataRef, ratingDataRef}, {request, NrfRequest},
						{error, decode_failed}]),
				{error, decode_failed}
		end
	of
		{{struct, _} = NrfResponse1, LogRequest, LogResponse} ->
			ReponseBody = mochijson:encode(NrfResponse1),
			ok = ocs_log:acct_log(nrf, server(ModData), stop,
					LogRequest, LogResponse, undefined),
			{200, [], ReponseBody};
		{error, StatusCode,  LogRequest, Problem1} ->
			ok = ocs_log:acct_log(nrf, server(ModData), stop,
					LogRequest, Problem1, undefined),
			{error, StatusCode, Problem1};
		{error, decode_failed = Reason1} ->
			Problem1 = rest_error_response(Reason1, undefined),
			{error, 400, Problem1}
	catch
		?CATCH_STACK ->
			?SET_STACK,
			error_logger:warning_report(["Unable to process Nrf request",
					{RatingDataRef, ratingDataRef}, {request, NrfRequest},
					{error, Reason1}, {stack, StackTrace}]),
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
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied due to insufficient credit (usage applied)"};
rest_error_response(service_not_found, InvalidParams) ->
	#{cause => "SUBSCRIPTION_NOT_FOUND",
			status => 404,
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the subscriber identity is unrecognized",
			invalidParams => InvalidParams};
rest_error_response(charging_failed, undefined) ->
	#{cause => "CHARGING_FAILED",
			status => 500,
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Incomplete or erroneous session or subscriber information"};
rest_error_response(unknown_ref, InvalidParams) ->
	#{cause => "RATING_DATA_REF_UNKNOWN",
			status => 404,
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the rating data ref is not recognized",
			invalidParams => InvalidParams};
rest_error_response(invalid_service_type, InvalidParams) ->
	#{cause => "INVALID_SERVICE_TYPE",
			status => 400,
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
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
		Result :: [map()] | {error, Reason},
		Reason :: term().
%% @doc Rate Nrf `ServiceRatingRequest's.
rate(RatingDataRef, #{"serviceRating" := ServiceRating,
		"subscriptionId" := SubscriptionIds}, Flag) ->
	rate(list_to_binary(RatingDataRef), ServiceRating, SubscriptionIds, Flag, []).
%% @hidden
rate(RatingDataRef, [#{"serviceContextId" := SCI} = H | T],
		SubscriptionIds, Flag, Acc) ->
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
	{Map3, MCCMNC} = case maps:find("mccmnc", H) of
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
	ServiceType = service_type(SCI),
	TS = calendar:universal_time(),
	{Map5, SessionAttributes} = case maps:find("uPFID", H) of
		{ok, UpfId} ->
			case application:get_env(quota_per_upf) of
				{ok, true} ->
					{Map4#{"uPFID" => UpfId},
							[{nrf_ref, RatingDataRef}, {upfid, UpfId}]};
				{ok, false} ->
					{Map4, [{nrf_ref, RatingDataRef}]}
			end;
		_ ->
			{Map4, [{nrf_ref, RatingDataRef}]}
	end,
	case ocs_rating:rate(nrf, ServiceType, ServiceId, ChargingKey,
			MCCMNC, get_subscriber(SubscriptionIds), TS, undefined, undefined,
			Flag, Debits, Reserves, SessionAttributes) of
		{ok, _, {octets, Amount} = _GrantedAmount}
				when Flag == event, Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"consumedUnit" => #{"totalVolume" => Amount},
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {seconds, Amount} = _GrantedAmount}
				when Flag == event, Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"consumedUnit" => #{"time" => Amount},
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {messages, Amount} = _GrantedAmount}
				when Flag == event, Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"consumedUnit" => #{"serviceSpecificUnit" => Amount},
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {octets, Amount} = _GrantedAmount}
				when Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"totalVolume" => Amount},
					"serviceContextId" => SCI},
			RatedMap1 = case application:get_env(nrf_quota_volume) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					RatedMap#{"validUnits" => Threshold};
				_ ->
					RatedMap
			end,
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap1 | Acc]);
		{ok, _, {seconds, Amount} = _GrantedAmount}
				when Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"time" => Amount},
					"serviceContextId" => SCI},
			RatedMap1 = case application:get_env(nrf_quota_time) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					RatedMap#{"validUnits" => Threshold};
				_ ->
					RatedMap
			end,
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap1 | Acc]);
		{ok, _, {messages, Amount} = _GrantedAmount}
				when Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"serviceSpecificUnit" => Amount},
					"serviceContextId" => SCI},
			RatedMap1 = case application:get_env(nrf_quota_unit) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					RatedMap#{"validUnits" => Threshold};
				_ ->
					RatedMap
			end,
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap1 | Acc]);
		{ok, _, {_, 0} = _GrantedAmount} ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {octets, Amount} = _GrantedAmount, _}
				when Flag == event, Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"consumedUnit" => #{"totalVolume" => Amount},
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {seconds, Amount} = _GrantedAmount, _}
				when Flag == event, Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"consumedUnit" => #{"time" => Amount},
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {messages, Amount} = _GrantedAmount, _}
				when Flag == event, Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"consumedUnit" => #{"serviceSpecificUnit" => Amount},
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{ok, _, {octets, Amount} = _GrantedAmount, _}
				when Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"totalVolume" => Amount},
					"serviceContextId" => SCI},
			RatedMap1 = case application:get_env(nrf_quota_volume) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					RatedMap#{"validUnits" => Threshold};
				_ ->
					RatedMap
			end,
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap1 | Acc]);
		{ok, _, {seconds, Amount} = _GrantedAmount, _}
				when Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"time" => Amount},
					"serviceContextId" => SCI},
			RatedMap1 = case application:get_env(nrf_quota_time) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					RatedMap#{"validUnits" => Threshold};
				_ ->
					RatedMap
			end,
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap1 | Acc]);
		{ok, _, {messages, Amount} = _GrantedAmount, _}
				when Amount > 0 ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"grantedUnit" => #{"serviceSpecificUnit" => Amount},
					"serviceContextId" => SCI},
			RatedMap1 = case application:get_env(nrf_quota_unit) of
				{ok, Threshold} when is_integer(Threshold), Amount > Threshold ->
					RatedMap#{"validUnits" => Threshold};
				_ ->
					RatedMap
			end,
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap1 | Acc]);
		{ok, _, _} ->
			RatedMap = Map5#{"resultCode" => "SUCCESS",
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{out_of_credit, _, _} ->
			RatedMap = Map5#{"resultCode" => "QUOTA_LIMIT_REACHED",
					"serviceContextId" => SCI},
			rate(RatingDataRef, T, SubscriptionIds, Flag, [RatedMap | Acc]);
		{error, Reason} ->
			{error, Reason}
	end;
rate(_RatingDataRef, [], _Subscriber, _Flag, Acc) ->
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
nrf1([{"nfConsumerIdentification", {struct, [{"nodeFunctionality", NF}]}} | T], Acc) ->
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
		Result :: {Server, Port},
		Server :: inet:ip_address() | inet:hostname(),
		Port :: inet:port_number().
%% @doc Get server IP address and Port.
server(#mod{init_data = #init_data{sockname = {Port, Host}}}) ->
	{Host, Port}.

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

