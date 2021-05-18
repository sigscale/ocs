%%% ocs_rest_res_nrf.erl
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
-export([initial_nrf/1]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

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
	["application/json"].

-spec initial_nrf(NrfRequest) -> NrfResponse
	when
		NrfRequest :: iolist(),
		NrfResponse :: {ok, Headers, Body} | {error, Status},
		Headers :: [tuple()],
		Body :: iolist(),
		Status :: 201 | 400 | 500.
%% @doc Respond to `POST /nrf-rating/v1/ratingdata'.
%%		Rate an intial Nrf Request.
initial_nrf(NrfRequest) ->
	try
		case mochijson:decode(NrfRequest) of
			{struct, _Attributes} = NrfStruct ->
				NrfMap = nrf_request_to_map(NrfStruct),
				case rate(NrfMap, initial) of
					ServiceRating when is_list(ServiceRating) ->
						UpdatedMap = maps:update("serviceRating", ServiceRating, NrfMap),
						nrf_response_to_struct(UpdatedMap);
					{error, Reason1} ->
						{error, Reason1}
				end;
			_ ->
				{error, 400}
		end
	of
		{struct, _Attributes1} = NrfResponse ->
			RatingDataRef = ocs:generate_identity(),
			Location = "/ratingdata/" ++ RatingDataRef,
			Body = mochijson:encode(NrfResponse),
			{ok, [{location, Location}], Body};
		{error, Reason2} ->
			{error, Reason2}
	catch
		_:_ ->
			{error, 400}
	end.
	
%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec rate(NrfRequest, Flag) -> Result
	when
		NrfRequest :: map(),
		Flag :: initial | interim | final,
		Result :: [map()] | {error, Reason},
		Reason :: term().
%% @doc Rate Nrf Service Ratings.
rate(#{"serviceRating" := ServiceRating, "invocationSequenceNumber" := ISN,
		"msisdn" := MSISDN}, Flag) ->
	rate(ServiceRating, ISN, MSISDN, Flag, []).
%% @hidden
rate([#{"serviceContextId" := SCI} = H | T],
		ISN, Subscriber, Flag, Acc) ->
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
		{ok, #{"totalVolume" := RTV}} ->
			[{octets, RTV}];
		{ok, #{"time" := RTime}} ->
			[{seconds, RTime}];
		{ok, #{"serviceSpecificUnit" := RSSU}} ->
			[{messages, RSSU}];
		error ->
			undefined
	end,
	Debits = case maps:find("consumedUnit", H) of
		{ok, #{"totalVolume" := CTV}} ->
			[{octets, CTV}];
		{ok, #{"time" := CTime}} ->
			[{seconds, CTime}];
		{ok, #{"serviceSpecificUnit" := CSSU}} ->
			[{messages, CSSU}];
		error ->
			[]
	end,
	ServiceType = service_type(list_to_binary(SCI)),
	TS = calendar:universal_time(),
	case ocs_rating:rate(diameter, ServiceType, ServiceId, ChargingKey,
			MCCMNC, Subscriber, TS, undefined, undefined, Flag,
			Debits, Reserves, [{"invocationSequenceNumber", ISN}]) of
		{ok, _, {Type, Amount} = _GrantedAmount} when Amount > 0 ->
			RatedMap = Map3#{"resultCode" => "SUCCESS", "grantedUnit" => #{type(Type)=> Amount},
					"serviceContextId"=> SCI},
			rate(T, ISN, Subscriber, Flag, [RatedMap | Acc]);
		{ok, _, {_, 0} = _GrantedAmount} ->
			RatedMap = Map3#{"resultCode" => "SUCCESS", "serviceContextId"=> SCI},
			rate(T, ISN, Subscriber, Flag, [RatedMap | Acc]);
		{out_of_credit, _, _} ->
			{error, out_of_credit};
		{disabled, _} ->
			{error, disabled};
		{error, Reason} ->
			{error, Reason}
	end;
rate([], _ISN, _Subscriber, _Flag, Acc) ->
	lists:reverse(Acc).

-spec nrf_response_to_struct(NrfReponse) -> Result
	when
		NrfReponse :: map(),
		Result :: {struct, [tuple()]}.
%% @doc CODEC for Nrf Reponse.
nrf_response_to_struct(NrfRequest) ->
	nrf_response_to_struct1(NrfRequest, []).
%% @hidden
nrf_response_to_struct1(#{"invocationTimeStamp" := TS} = M, Acc) ->
	nrf_response_to_struct2(M, [{"invocationTimeStamp", TS} | Acc]).
nrf_response_to_struct2(#{"invocationSequenceNumber" := SeqNum} = M, Acc) ->
	nrf_response_to_struct3(M, [{"invocationSequenceNumber", SeqNum} | Acc]).
nrf_response_to_struct3(#{"msisdn" := MSISDN, "imsi" := IMSI} = M, Acc) ->
	nrf_response_to_struct4(M, [{"subscriptionId", {array, ["msisdn-" ++ MSISDN,
			"imsi-" ++ IMSI]}} | Acc]).
nrf_response_to_struct4(#{"serviceRating" := ServiceRating}, Acc) ->
	Acc1 = [{"nfConsumerIdentification", {struct,[{"nodeFunctionality","OCF"}]}},
			{"serviceRating", {array, struct_service_rating(ServiceRating)}} | Acc],
	{struct, Acc1}.

-spec nrf_request_to_map(NrfRequest) -> Result
	when
		NrfRequest :: {struct, [tuple()]},
		Result :: map().
%% @doc CODEC for Nrf Request.
nrf_request_to_map({struct, StructList}) ->
	nrf_request_to_map(StructList, #{}).
%% @hidden
nrf_request_to_map([{"invocationTimeStamp", TS} | T], Acc) ->
	nrf_request_to_map(T, Acc#{"invocationTimeStamp" => TS});
nrf_request_to_map([{"invocationSequenceNumber", SeqNum} | T], Acc) ->
	nrf_request_to_map(T, Acc#{"invocationSequenceNumber" => SeqNum});
nrf_request_to_map([{"subscriptionId", {array, ["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]}} | T], Acc) ->
	nrf_request_to_map(T, Acc#{"msisdn" => MSISDN, "imsi" => IMSI});
nrf_request_to_map([{"serviceRating", {array, ServiceRating}} | T], Acc) ->
	nrf_request_to_map(T, Acc#{"serviceRating" => map_service_rating(ServiceRating)});
nrf_request_to_map([_H | T], Acc) ->
	nrf_request_to_map(T, Acc);
nrf_request_to_map([], Acc) ->
	Acc.

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
			[{"grantedUnit", {struct, maps:to_list(Units1)}} | Acc];
		_ ->
			Acc
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

