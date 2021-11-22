%% ocs_rest_res_nrf.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_nrf).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([initial_nrf/1, update_nrf/2, release_nrf/2]).

-include("ocs.hrl").

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

-spec initial_nrf(NrfRequest) -> NrfResponse
	when
		NrfRequest :: iolist(),
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
initial_nrf(NrfRequest) ->
	RatingDataRef = unique(),
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				Flag = event_type(NrfMap),
				case rate(NrfMap, Flag) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = add_rating_ref(RatingDataRef, UpdatedMap),
						nrf(UpdatedMap);
					{error, out_of_credit} ->
						Problem = error_response(out_of_credit, undefined),
						{error, 403, Problem};
					{error, service_not_found} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = error_response(service_not_found, InvalidParams),
						{error, 404, Problem};
					{error, invalid_service_type} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = error_response(invalid_service_type, InvalidParams),
						{error, 400, Problem};
					{error, Reason} ->
						{error, Reason}
				end;
			_ ->
				Problem = error_response(charging_failed, undefined),
				{error, 400, Problem}
		end
	of
		{struct, _Attributes1} = NrfResponse ->
			Location = "/ratingdata/" ++ RatingDataRef,
			ReponseBody = mochijson:encode(NrfResponse),
			Headers = [{content_type, "application/json"}, {location, Location}],
			{ok, Headers, ReponseBody};
		{error, StatusCode, Problem1} ->
			{error, StatusCode, Problem1};
		{error, _Reason} ->
			{error, 500}
	catch
		_:_Reason ->
			{error, 500}
	end.
	
-spec update_nrf(RatingDataRef, NrfRequest) -> NrfResponse
	when
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
update_nrf(RatingDataRef, NrfRequest) ->
	case lookup_ref(RatingDataRef) of
		true ->
			update_nrf(NrfRequest);
		false ->
			InvalidParams = [#{param => "{" ++ RatingDataRef ++ "}",
					reason => "Unknown rating data reference"}],
			Problem = error_response(unknown_ref, InvalidParams),
			{error, 404, Problem};
		{error, _Reason} ->
			{error, 500}
	end.
update_nrf(NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				case rate(NrfMap, interim) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						nrf(UpdatedMap);
					{error, out_of_credit} ->
						Problem = error_response(out_of_credit, undefined),
						{error, 403, Problem};
					{error, service_not_found} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = error_response(service_not_found, InvalidParams),
						{error, 404, Problem};
					{error, invalid_service_type} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = error_response(invalid_service_type, InvalidParams),
						{error, 400, Problem};
					{error, Reason} ->
						{error, Reason}
				end;
			_ ->
				Problem = error_response(charging_failed, undefined),
				{error, 400, Problem}
		end
	of
		{struct, _Attributes1} = NrfResponse ->
			ReponseBody = mochijson:encode(NrfResponse),
			{200, [], ReponseBody };
		{error, StatusCode, Problem1} ->
			{error, StatusCode, Problem1};
		{error, _Reason} ->
			{error, 500}
	catch
		_:_ ->
			{error, 500}
	end.

-spec release_nrf(RatingDataRef, NrfRequest) -> NrfResponse
	when
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
release_nrf(RatingDataRef, NrfRequest) ->
	case lookup_ref(RatingDataRef) of
		true ->
			release_nrf1(RatingDataRef, NrfRequest);
		false ->
			InvalidParams = [#{param => "{" ++ RatingDataRef ++ "}",
					reason => "Unknown rating data reference"}],
			Problem = error_response(unknown_ref, InvalidParams),
			{error, 404, Problem};
		{error, _Reason} ->
			{error, 500}
	end.
release_nrf1(RatingDataRef, NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf(NrfStruct),
				case rate(NrfMap, final) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						ok = remove_ref(RatingDataRef),
						nrf(UpdatedMap);
					{error, out_of_credit} ->
						Problem = error_response(out_of_credit, undefined),
						{error, 403, Problem};
					{error, service_not_found} ->
						InvalidParams = [#{param => "/subscriptionId",
								reason => "Unknown subscriber identifier"}],
						Problem = error_response(service_not_found, InvalidParams),
						{error, 404, Problem};
					{error, invalid_service_type} ->
						InvalidParams = [#{param => "/serviceContextId",
								reason => "Invalid Service Type"}],
						Problem = error_response(invalid_service_type, InvalidParams),
						{error, 400, Problem};
					{error, Reason} ->
						{error, Reason}
				end;
			_ ->
				Problem = error_response(charging_failed, undefined),
				{error, 400, Problem}
		end
	of
		{struct, _Attributes1} = NrfResponse ->
			ReponseBody = mochijson:encode(NrfResponse),
			{200, [], ReponseBody };
		{error, StatusCode, Problem1} ->
			{error, StatusCode, Problem1};
		{error, _Reason} ->
			{error, 500}
	catch
		_:_ ->
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

-spec error_response(Error, InvalidParams) -> Result
	when
		Error :: term(),
		InvalidParams :: [map()] | undefined,
		Result :: map().
%% @doc Construct a problem report for an error respponse.
error_response(out_of_credit, undefined) ->
	#{cause => "QUOTA_LIMIT_REACHED",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied due to insufficient credit (usage applied)"};
error_response(service_not_found, InvalidParams) ->
	#{cause => "USER_UNKNOWN",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the subscriber identity is unrecognized",
			invalidParams => InvalidParams};
error_response(charging_failed, undefined) ->
	#{cause => "CHARGING_FAILED",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Incomplete or erroneous session or subscriber information"};
error_response(unknown_ref, InvalidParams) ->
	#{cause => "RATING_DATA_REF_UNKNOWN",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the rating data ref is not recognized",
			invalidParams => InvalidParams};
error_response(invalid_service_type, InvalidParams) ->
	#{cause => "CHARGING_FAILED",
			type => "https://app.swaggerhub.com/apis/SigScale/nrf-rating/1.0.0#/",
			title => "Request denied because the service context id is not recognized",
			invalidParams => InvalidParams}.

-spec rate(NrfRequest, Flag) -> Result
	when
		NrfRequest :: map(),
		Flag :: initial | interim | final,
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
	{Map3, MCCMNC} = case maps:find("ratingGroup", H) of
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
	nrf4(M, [{"subscriptionId", {array, subscriptionId_list(SubIds)}} | Acc]).
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
	Acc1 = Acc#{"subscriptionId" =>
			["msisdn-" ++ MSISDN | SubscriptionIds]},
	subscriptionId_map(T, Acc1);
subscriptionId_map(["imsi-" ++ IMSI | T],
		#{"subscriptionId" := SubscriptionIds} = Acc) ->
	Acc1 = Acc#{"subscriptionId" =>
			["imsi-" ++ IMSI | SubscriptionIds]},
	subscriptionId_map(T, Acc1);
subscriptionId_map([_ | T], Acc) ->
	subscriptionId_map(T, Acc);
subscriptionId_map([], #{"subscriptionId" := SubscriptionIds} = Acc) ->
	SubscriptionIds1 = lists:reverse(SubscriptionIds),
	Acc#{"subscriptionId" := SubscriptionIds1}.

%% @hidden
subscriptionId_list(SubscriptionIds) ->
	subscriptionId_list1(SubscriptionIds, []).
%% @hidden
subscriptionId_list1(["msisdn-" ++ MSISDN | T], Acc) ->
	subscriptionId_list1(T, ["msisdn-" ++ MSISDN | Acc]);
subscriptionId_list1(["imsi-" ++ IMSI | T], Acc) ->
	subscriptionId_list1(T, ["imsi-" ++ IMSI| Acc]);
subscriptionId_list1([_| T], Acc) ->
	subscriptionId_list1(T, Acc);
subscriptionId_list1([], Acc) ->
	lists:reverse(Acc).

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

%% @hidden
event_type(#{"oneTimeEventType" := "IEC"}) ->
	event;
event_type(_) ->
	initial.
	
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

