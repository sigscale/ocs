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

-spec rate_nrf(NrfRequest) -> NrfResponse
	when
		NrfRequest :: iolist(),
		NrfResponse :: {ok, Headers, Body} | {error, Status},
		Headers :: [tuple()],
		Body :: iolist(),
		Status :: 201 | 400 | 500.
%% @doc Respond to `POST /nrf-rating/v1/ratingdata'.
%%		Rate an intial Nrf Request.
rate_nrf(NrfRequest) ->
	try
		nrf_request(NrfRequest)
	of
		NrfMap when is_map(NrfMap) ->
			{ok, [{}], []}
	catch
		error:_Reason ->
         {error, 500};
		throw:_Reason1 ->
			{error, 400}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec nrf_request(NrfRequest) -> NrfRequest
	when
		NrfRequest :: {struct, [tuple()]} | map().
%% @doc CODEC for Nrf Request.
nrf_request({struct, StructList}) -> 
	nrf_request(StructList, #{});
nrf_request(NrfRequest)
		when is_map(NrfRequest) -> 
	nrf_request(NrfRequest, []).
%% @hidden
nrf_request([{"invocationTimeStamp", TS} | T], Acc) ->
	nrf_request(T, Acc#{"invocationTimeStamp" => TS});
nrf_request(#{"invocationTimeStamp" := TS} = M, Acc) ->
	nrf_request(M, [{"invocationTimeStamp", TS} | Acc]);
nrf_request([{"invocationSequenceNumber", SeqNum} | T], Acc) ->
	nrf_request(T, Acc#{"invocationSequenceNumber" => SeqNum});
nrf_request(#{"invocationSequenceNumber" := SeqNum} = M, Acc) ->
	nrf_request(M, [{"invocationSequenceNumber", SeqNum} | Acc]);
nrf_request([{"subscriptionId", {array, [MSISDN, IMSI]}} | T], Acc) ->
	nrf_request(T, Acc#{"msisdn" => MSISDN, "imsi" => IMSI});
nrf_request(#{"msisdn" := MSISDN, "imsi" := IMSI} = M, Acc) ->
	nrf_request(M, [{"subscriptionId", {array, [MSISDN, IMSI]}} | Acc]);
nrf_request([{"serviceRating", {array,[{struct, MSCC}]}} | T], Acc) ->
	nrf_request(T, Acc#{"serviceRating" => map_service_rating(MSCC)});
nrf_request(#{"serviceRating" := ServiceRating}, Acc) ->
	[{"serviceRating", {array, [{struct, struct_service_rating(ServiceRating)}]}} | Acc];
nrf_request([_H | T], Acc) ->
	nrf_request(T, Acc);
nrf_request([], Acc) ->
	Acc.

-spec struct_service_rating(ServiceRating) -> Result
	when
		ServiceRating :: [map()],
		Result :: [{struct, [tuple()]}].
%% @doc Convert a Service Rating map to a struct.
struct_service_rating(ServiceRating) -> 
	struct_service_rating(ServiceRating, []).
%% @hidden
struct_service_rating([ServiceRating | T], Acc) -> 
   F = fun F(#{"grantedUnit" := Units} = M, Acc1) ->
         F(M, [{"grantedUnit", {array, units(Units)}} | Acc1]);
      F(#{"consumedUnit" := Units} = M , Acc1) ->
         F(M, [{"consumedUnit", {array, units(Units)}} | Acc1]);
      F(#{"serviceContextId" := SCI} = M, Acc1) ->
         F(M, [{"serviceContextId", SCI} | Acc1]);
      F(#{"serviceId" := SID} = M, Acc1) ->
         F(M, [{"serviceId", SID} | Acc1]);
      F(#{"ratingGroup" := RG} = M, Acc1) ->
         F(M, [{"ratingGroup", RG} | Acc1]);
      F(#{"requestSubType" := RS} = M, Acc1) ->
         [{"requestSubType", RS} | Acc1]
   end,
   ServiceRatingList = F(ServiceRating, []),
   struct_service_rating(T, [{struct, ServiceRatingList} | Acc]);
struct_service_rating([], Acc) ->
	lists:reverse(Acc).

-spec map_service_rating(ServiceRating) -> Result
	when
		ServiceRating :: {struct, [tuple()]},
		Result :: [map()].
%% @doc Convert a Service Rating struct to a map.
map_service_rating({struct, ServiceRating}) -> 
	map_service_rating(ServiceRating, []).
%% @hidden
map_service_rating([{struct, Elements} | T], Acc) -> 
   F = fun F([{"requestedUnit", {_, Units}} | T1], Acc1) ->
         Acc2 = Acc1#{"grantedUnit" => units(Units)},
         F(T1, Acc2);
      F([{"consumedUnit", {_, Units}} | T1], Acc1) ->
         Acc2 = Acc1#{"consumedUnit" => units(Units)},
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
units(Units) when is_list(Units) ->
	units(Units, #{});
units(Units) when is_map(Units) ->
	units(Units, []).
%% @hidden
units([{"time", CCTime} | T], Acc) ->
	units(T, Acc#{"time" => CCTime});
units(#{"time" := CCTime} = M, Acc) ->
	units(M, [{"time", CCTime} | Acc]);
units([{"downlinkVolume", DownLinkVolume} | T], Acc) ->
	units(T, Acc#{"downlinkVolume" => DownLinkVolume});
units(#{"downlinkVolume" := DownLinkVolume} = M, Acc) ->
	units(M, [{"downlinkVolume", DownLinkVolume} |Acc]);
units([{"uplinkVolume", UpLinkVolume} | T], Acc) ->
	units(T, Acc#{"uplinkVolume" => UpLinkVolume});
units(#{"uplinkVolume" := UpLinkVolume} = M, Acc) ->
	units(M, [{"uplinkVolume", UpLinkVolume} | Acc]);
units([{"totalVolume", TotalVolume} | T], Acc) ->
	units(T, Acc#{"totalVolume" => TotalVolume});
units(#{"totalVolume" := TotalVolume} = M, Acc) ->
	units(M, [{"totalVolume", TotalVolume} | Acc]);
units([{"serviceSpecificUnit", SpecUnits} | T], Acc) ->
	units(T, Acc#{"serviceSpecificUnit" => SpecUnits});
units(#{"serviceSpecificUnit" := SpecUnits} = M, Acc) ->
	[{"serviceSpecificUnit", SpecUnits} | Acc];
units([], Acc) ->
   Acc.
