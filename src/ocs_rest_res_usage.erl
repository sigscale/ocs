%%% ocs_rest_res_usage.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
-module(ocs_rest_res_usage).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_accepted/0,
				content_types_provided/0,
				perform_get_all/0,
				perform_get/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").

-define(IPDR_LOG, ipdr_log).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/hal+json"].

-spec perform_get_all() -> Result
	when
		Result :: {body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /usageManagement/v1/usage'
%% requests.
perform_get_all() ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	case file:list_dir(Directory) of
		{ok, Files} ->
			Body = mochijson:encode({array, Files}),
			{body, Body};
		{error, _Reason} ->
			{error, 500}
	end.

-spec perform_get(Id) -> Result
	when
		Id :: string(),
		Result :: {body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /usageManagement/v1/usage/{id}'
%% requests.
perform_get(Id) ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	Log = ?IPDR_LOG,
	FileName = Directory ++ Id,
	read_ipdr(Log, FileName).
	
%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
read_ipdr(Log, FileName) ->
	case disk_log:open([{name, Log}, {file, FileName},
			{type, halt}, {size, infinity}]) of
		{ok, Log} ->
			read_ipdr1(Log, start, []);
		{repaired, Log, _Recovered, _Bad} ->
			read_ipdr1(Log, start, []);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
read_ipdr1(Log, Continuation, Acc) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			ipdr_to_json(Log, Acc);
		{Continuation2, Records} ->
			NewAcc = Records ++ Acc,
			read_ipdr1(Log, Continuation2, NewAcc)
	end.

%% @hidden
ipdr_to_json(Log, IpdrList) ->
	F = fun(#ipdrDoc{}, {Id, Acc}) ->
				{Id, Acc};
			(#ipdr{} = Ipdr, {Id, Acc}) ->
				UsageSpecification = {struct, [{id, 1},
						{href, "usageManagement/v1/usageSpecification/1"},
						{name, "PublicWLANAccessUsageSpec"}]},
				UsageCharacteristicObjs = usage_characteristics(Ipdr),
				UsageCharacteristic = {array, UsageCharacteristicObjs},
				RespObj = [{struct, [{id, Id},
						{href, "usageManagement/v1/usage/" ++ integer_to_list(Id)},
						{date, "SomeDateTime"}, {type, "PublicWLANAccessUsage"},
						{description, "Description for individual usage content"},
						{status, received},
						{usageSpecification, UsageSpecification},
						{usageCharacteristic, UsageCharacteristic}]}],
						{Id + 1, Acc ++ RespObj};
			(#ipdrDocEnd{}, {Id, Acc}) ->
				{Id, Acc}
	end,
	{_Count, JsonObj} = lists:foldl(F, {1, []}, IpdrList),
	Response = {array, JsonObj},
	Body = mochijson:encode(Response),
	disk_log:close(Log),
	{body, Body}.

%% @hidden
usage_characteristics(#ipdr{} = Ipdr) ->
	usage_characteristics(record_info(fields, ipdr), Ipdr, []).

%% @hidden
usage_characteristics([ipdrCreationTime | T], Ipdr, Acc) ->
	Struct = {struct, [{key, ipdrCreationTime}, {value, Ipdr#ipdr.ipdrCreationTime}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([seqNum | T], Ipdr, Acc) ->
	Struct = {struct, [{key, seqNum}, {value, Ipdr#ipdr.seqNum}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([username | T], Ipdr, Acc) ->
	Struct = {struct, [{key, username}, {value, Ipdr#ipdr.username}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([scIdType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, scIdType}, {value, Ipdr#ipdr.scIdType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([scId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, scId}, {value, Ipdr#ipdr.scId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([homeServiceProviderType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, homeServiceProviderType}, {value, Ipdr#ipdr.homeServiceProviderType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([homeServiceProvider | T], Ipdr, Acc) ->
	Struct = {struct, [{key, homeServiceProvider}, {value, Ipdr#ipdr.homeServiceProvider}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([acctSessionId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, acctSessionId}, {value, Ipdr#ipdr.acctSessionId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([userIpAddress | T], Ipdr, Acc) ->
	Struct = {struct, [{key, userIpAddress}, {value, Ipdr#ipdr.userIpAddress}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([callingStationId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, callingStationId}, {value, Ipdr#ipdr.callingStationId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([nasIpAddress | T], Ipdr, Acc) ->
	Struct = {struct, [{key, nasIpAddress}, {value, Ipdr#ipdr.nasIpAddress}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([calledStationId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, calledStationId}, {value, Ipdr#ipdr.calledStationId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([nasId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, nasId}, {value, Ipdr#ipdr.nasId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([accessProviderType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, accessProviderType}, {value, Ipdr#ipdr.accessProviderType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([accessServiceProvider | T], Ipdr, Acc) ->
	Struct = {struct, [{key, accessServiceProvider}, {value, Ipdr#ipdr.accessServiceProvider}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationName | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationName}, {value, Ipdr#ipdr.locationName}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationId}, {value, Ipdr#ipdr.locationId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationType}, {value, Ipdr#ipdr.locationType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationCountryCode | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationCountryCode}, {value, Ipdr#ipdr.locationCountryCode}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationStateProvince | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationStateProvince}, {value, Ipdr#ipdr.locationStateProvince}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationCity | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationCity}, {value, Ipdr#ipdr.locationCity}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationGeocode | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationGeocode}, {value, Ipdr#ipdr.locationGeocode}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([locationGeocodeType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, locationGeocodeType}, {value, Ipdr#ipdr.locationGeocodeType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([nasPortType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, nasPortType}, {value, Ipdr#ipdr.nasPortType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([paymentType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, paymentType}, {value, Ipdr#ipdr.paymentType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([networkConnectionType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, networkConnectionType}, {value, Ipdr#ipdr.networkConnectionType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([sessionDuration | T], Ipdr, Acc) ->
	Struct = {struct, [{key, sessionDuration}, {value, Ipdr#ipdr.sessionDuration}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([inputOctets | T], Ipdr, Acc) ->
	Struct = {struct, [{key, inputOctets}, {value, Ipdr#ipdr.inputOctets}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([outputOctets | T], Ipdr, Acc) ->
	Struct = {struct, [{key, outputOctets}, {value, Ipdr#ipdr.outputOctets}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([class | T], Ipdr, Acc) ->
	Struct = {struct, [{key, class}, {value, Ipdr#ipdr.class}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([gmtSessionStartDateTime | T], Ipdr, Acc) ->
	Struct = {struct, [{key, gmtSessionStartDateTime}, {value, Ipdr#ipdr.gmtSessionStartDateTime}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([gmtSessionEndDateTime | T], Ipdr, Acc) ->
	Struct = {struct, [{key, gmtSessionEndDateTime}, {value, Ipdr#ipdr.gmtSessionEndDateTime}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([sessionTerminateCause | T], Ipdr, Acc) ->
	Struct = {struct, [{key, sessionTerminateCause}, {value, Ipdr#ipdr.sessionTerminateCause}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([billingClassOfService | T], Ipdr, Acc) ->
	Struct = {struct, [{key, billingClassOfService}, {value, Ipdr#ipdr.billingClassOfService}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([unitOfMeasure | T], Ipdr, Acc) ->
	Struct = {struct, [{key, unitOfMeasure}, {value, Ipdr#ipdr.unitOfMeasure}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([chargeableUnit | T], Ipdr, Acc) ->
	Struct = {struct, [{key, chargeableUnit}, {value, Ipdr#ipdr.chargeableUnit}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([chargeableQuantity | T], Ipdr, Acc) ->
	Struct = {struct, [{key, chargeableQuantity}, {value, Ipdr#ipdr.chargeableQuantity}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([chargeAmount | T], Ipdr, Acc) ->
	Struct = {struct, [{key, chargeAmount}, {value, Ipdr#ipdr.chargeAmount}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([chargeCurrencyType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, chargeCurrencyType}, {value, Ipdr#ipdr.chargeCurrencyType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([otherParty | T], Ipdr, Acc) ->
	Struct = {struct, [{key, otherParty}, {value, Ipdr#ipdr.otherParty}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([taxPercentage | T], Ipdr, Acc) ->
	Struct = {struct, [{key, taxPercentage}, {value, Ipdr#ipdr.taxPercentage}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([taxAmount | T], Ipdr, Acc) ->
	Struct = {struct, [{key, taxAmount}, {value, Ipdr#ipdr.taxAmount}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([taxType | T], Ipdr, Acc) ->
	Struct = {struct, [{key, taxType}, {value, Ipdr#ipdr.taxType}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([intermediaryName | T], Ipdr, Acc) ->
	Struct = {struct, [{key, intermediaryName}, {value, Ipdr#ipdr.intermediaryName}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([serviceName | T], Ipdr, Acc) ->
	Struct = {struct, [{key, serviceName}, {value, Ipdr#ipdr.serviceName}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([relatedIpdrIdList | T], Ipdr, Acc) ->
	Struct = {struct, [{key, relatedIpdrIdList}, {value, Ipdr#ipdr.relatedIpdrIdList}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([tempUserId | T], Ipdr, Acc) ->
	Struct = {struct, [{key, tempUserId}, {value, Ipdr#ipdr.tempUserId}]},
	usage_characteristics(T, Ipdr, [Struct |Acc]);
usage_characteristics([], _Ipdr, Acc) ->
	lists:reverse(Acc).

