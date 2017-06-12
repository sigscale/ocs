%%% ocs_rest_res_usage.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_usage/0, get_usage/1, get_usagespec/0, get_usagespec/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").

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
	["application/json"].

-spec get_usage() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usage'
%% 	requests.
get_usage() ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	case file:list_dir(Directory) of
		{ok, Files} ->
			SortedFiles = lists:reverse(lists:sort(Files)),
			Body = mochijson:encode({array, SortedFiles}),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 500}
	end.

-spec get_usage(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usage/{id}'
%% 	requests.
get_usage(Id) ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	FileName = Directory ++ "/" ++ Id,
	read_ipdr(FileName, MaxItems).
	
-spec get_usagespec() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usageSpecification'
%% 	requests.
get_usagespec() ->
	Headers = [{content_type, "application/json"}],
	Body = mochijson:encode({array, [spec_aaa_usage(),
			spec_aaa_accounting(), spec_public_wlan()]}),
	{ok, Headers, Body}.

-spec get_usagespec(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usageSpecification/{id}'
%% 	requests.
get_usagespec("AAAAccessUsageSpec") ->
	Headers = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_aaa_usage()),
	{ok, Headers, Body};
get_usagespec("AAAAccountingUsageSpec") ->
	Headers = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_aaa_accounting()),
	{ok, Headers, Body};
get_usagespec("PublicWLANAccessUsageSpec") ->
	Headers = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_public_wlan()),
	{ok, Headers, Body};
get_usagespec(_Id) ->
	{error, 404}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
read_ipdr(FileName, MaxItems) ->
	case disk_log:open([{name, make_ref()}, {file, FileName},
			{type, halt}, {size, infinity}]) of
		{ok, Log} ->
			read_ipdr1(Log, start, MaxItems, 0, []);
		{repaired, Log, _Recovered, _Bad} ->
			read_ipdr1(Log, start, MaxItems, 0, []);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
read_ipdr1(Log, Continuation, MaxItems, Count, Acc) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			read_ipdr2(Log, Count, Acc);
		{Continuation2, Records} -> 
			case ipdr_to_json(Count, Records) of
				{NewCount, JsonObj} when NewCount > MaxItems ->
					{NewJsonObj, _} = lists:split(MaxItems - Count, lists:reverse(JsonObj)),
					NewAcc = [NewJsonObj | Acc],
					read_ipdr2(Log, NewCount, NewAcc);
				{NewCount, JsonObj} when NewCount == MaxItems ->
					NewAcc = [JsonObj | Acc],
					read_ipdr2(Log, NewCount, NewAcc);
				{NewCount, JsonObj} ->
					NewAcc = [JsonObj | Acc],
					read_ipdr1(Log, Continuation2, MaxItems, NewCount, NewAcc)
			end
	end.
%% @hidden
read_ipdr2(Log, Count, Acc)->
	NewAcc = lists:flatten(lists:reverse(Acc)),
	Response = {array, NewAcc},
	Body = mochijson:encode(Response),
	disk_log:close(Log),
	ContentRange = "items 1-" ++ integer_to_list(Count) ++ "/*",
   Headers = [{content_range, ContentRange}],
	{ok, Headers, Body}.

%% @hidden
ipdr_to_json(Count, Records) ->
	F = fun(#ipdrDoc{}, {N, Acc}) ->
				{N, Acc};
			(#ipdr{} = Ipdr, {N, Acc}) ->
				UsageSpecification = {struct, [{id, 1},
						{href, "usageManagement/v1/usageSpecification/1"},
						{name, "PublicWLANAccessUsageSpec"}]},
				UsageCharacteristicObjs = usage_characteristics(Ipdr),
				UsageCharacteristic = {array, UsageCharacteristicObjs},
				RespObj = [{struct, [{id, N + 1},
						{href, "usageManagement/v1/usage/" ++ integer_to_list(N + 1)},
						{date, "SomeDateTime"}, {type, "PublicWLANAccessUsage"},
						{description, "Description for individual usage content"},
						{status, received},
						{usageSpecification, UsageSpecification},
						{usageCharacteristic, UsageCharacteristic}]}],
						{N + 1, [RespObj | Acc]};
			(#ipdrDocEnd{}, {N, Acc}) ->
				{N, Acc}
	end,
	lists:foldl(F, {Count, []}, Records).

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


%% @hidden
spec_aaa_usage() ->
	ID = {id, "AAAAccessUsageSpec"},
	Href = {href, "/usageManagement/v1/usageSpecification/AAAAccessUsageSpec"},
	Name = {name, "AAAAccessUsageSpec"},
	Desc = {description, "Specification for SigScale OCS access requests."},
	Start = {startDateTime, "2017-01-01T00:00:00Z"},
	End = {endDateTime, "2017-12-31T23:59:59Z"},
	Valid = {validFor, {struct, [Start, End]}},
	Chars = [spec_timestamp(), spec_protocol(), spec_node(),
			spec_server_address(), spec_server_port(), spec_client_address(),
			spec_client_port(), spec_type_access(),
			spec_attr_username(), spec_attr_nas_ip(),
			spec_attr_nas_port(), spec_attr_service_type(),
			spec_attr_framed_address(), spec_attr_framed_pool(),
			spec_attr_framed_netmask(), spec_attr_framed_routing(),
			spec_attr_filter_id(), spec_attr_framed_mtu(),
			spec_attr_framed_route(), spec_attr_session_timeout(),
			spec_attr_idle_timeout(), spec_attr_termination_action(),
			spec_attr_called_id(), spec_attr_calling_id(), spec_attr_nas_id(),
			spec_attr_nas_port_id(), spec_attr_nas_port_type(),
			spec_attr_nas_port_limit(), spec_attr_data_rate(),
			spec_attr_xmit_rate(), spec_attr_interim_interval(),
			spec_attr_class()],
	Char = {usageSpecCharacteristic, {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_aaa_accounting() ->
	ID = {id, "AAAAccountingUsageSpec"},
	Href = {href, "/usageManagement/v1/usageSpecification/AAAAccountingUsageSpec"},
	Name = {name, "AAAAccountingUsageSpec"},
	Desc = {description, "Specification for SigScale OCS accounting requests."},
	Start = {startDateTime, "2017-01-01T00:00:00Z"},
	End = {endDateTime, "2017-12-31T23:59:59Z"},
	Valid = {validFor, {struct, [Start, End]}},
	Chars = [spec_timestamp(), spec_protocol(), spec_node(),
			spec_server_address(), spec_server_port(), spec_type_accounting(),
			spec_attr_username(), spec_attr_nas_ip(), spec_attr_nas_port(),
			spec_attr_service_type(), spec_attr_framed_address(),
			spec_attr_framed_netmask(), spec_attr_framed_routing(),
			spec_attr_filter_id(), spec_attr_framed_mtu(),
			spec_attr_framed_route(), spec_attr_class(),
			spec_attr_session_timeout(), spec_attr_idle_timeout(),
			spec_attr_termination_action(), spec_attr_called_id(),
			spec_attr_calling_id(), spec_attr_nas_id(),
			spec_attr_nas_port_id(), spec_attr_delay(),
			spec_attr_input_octets(),spec_attr_output_octets(),
			spec_attr_input_giga_words(),spec_attr_output_giga_words(),
			spec_attr_event_timestamp(), spec_attr_session_id(),
			spec_attr_authentic(), spec_attr_session_time(),
			spec_attr_input_packets(), spec_attr_output_packets(),
			spec_attr_cause(), spec_attr_multi_session_id(),
			spec_attr_link_count(), spec_attr_nas_port_type(),
			spec_attr_nas_port_limit()],
	Char = {usageSpecCharacteristic, {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_public_wlan() ->
	ID = {id, "PublicWLANAccessUsageSpec"},
	Href = {href, "/usageManagement/v1/usageSpecification/PublicWLANAccessUsageSpec"},
	Name = {name, "PublicWLANAccessUsageSpec"},
	Desc = {description, "Specification for IPDR Public WLAN Access - WISP Use Case"},
	Start = {startDateTime, "2017-01-01T00:00:00Z"},
	End = {endDateTime, "2017-12-31T23:59:59Z"},
	Valid = {validFor, {struct, [Start, End]}},
	Chars = lists:reverse(spec_public_wlan1([])),
	Char = {usageSpecCharacteristic, {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.
%% @hidden
spec_public_wlan1(Acc) ->
	Name = {name, "userName"},
	Desc = {description, "The end user ID and their domain name (NAI)."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan2(NewAcc).
%% @hidden
spec_public_wlan2(Acc) ->
	Name = {name, "ScIdType"},
	Desc = {description, "Type of Service Consumer ID Used when a more specific Identifier of Service Consumer is necessary. For example, IMSI for GSM subscribers."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 3},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan3(NewAcc).
%% @hidden
spec_public_wlan3(Acc) ->
	Name = {name, "ScId"},
	Desc = {description, "The Service Consumer ID when a more specific identifier of the Service Consumer is required. For example, IMSI for GSM/GPRS subscribers."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan4(NewAcc).
%% @hidden
spec_public_wlan4(Acc) ->
	Name = {name, "homeServiceProviderType"},
	Desc = {description, "Identifies how the home service provider is identified."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 4},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan5(NewAcc).
%% @hidden
spec_public_wlan5(Acc) ->
	Name = {name, "homeServiceProvider"},
	Desc = {description, "The user’s Home Service Provider. May be derived from the NAI of the Username. This field, plus the type, will uniquely identify the provider by the same value that they are known in their industry."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan6(NewAcc).
%% @hidden
spec_public_wlan6(Acc) ->
	Name = {name, "acctSessionId"},
	Desc = {description, "Account session ID assigned by the NAS server. Each session is assigned a unique NAS ID and is therefore used as one of the key criteria in the Settlement Process to identify unique transactions."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan7(NewAcc).
%% @hidden
spec_public_wlan7(Acc) ->
	Name = {name, "userIpAddress"},
	Desc = {description, "IP Address of the end user (calling station). This field must support IPv6 format."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan8(NewAcc).
%% @hidden
spec_public_wlan8(Acc) ->
	Name = {name, "callingStationId"},
	Desc = {description, "MAC Address of the end user's device as formatted in RFC3580, section 3.21. For example, 00-10-A4-23-19-C0"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan9(NewAcc).
%% @hidden
spec_public_wlan9(Acc) ->
	Name = {name, "nasIpAddress"},
	Desc = {description, "The IP address of the local Network Access Server (NAS) (i.e. the access gateway) that provides the service. This field must support IPv6 format."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan10(NewAcc).
%% @hidden
spec_public_wlan10(Acc) ->
	Name = {name, "nasIpAddress"},
	Desc = {description, "The IP address of the local Network Access Server (NAS) (i.e. the access gateway) that provides the service. This field must support IPv6 format."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan11(NewAcc).
%% @hidden
spec_public_wlan11(Acc) ->
	Name = {name, "calledStationId"},
	Desc = {description, "A unique name which identifies the hotspot venue. Radius Defined using the Mac Address and SSID in the format shown in RFC3580 section 3.20. For example: 00-10- A4-23-19-C0:AP1."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan12(NewAcc).
%% @hidden
spec_public_wlan12(Acc) ->
	Name = {name, "nasId"},
	Desc = {description, "Will appear in Access Request record format (depends on WISP network configuration and BSS system). Identifies the access gateway when NAS-IP-Address is insufficient."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan13(NewAcc).
%% @hidden
spec_public_wlan13(Acc) ->
	Name = {name, "accessProviderType"},
	Desc = {description, "Identifies how the serve/visited service provider is identified. For example, Domain Name, PMN code, SID/BID number, or BRI. Need to identify the possible values."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 4},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan14(NewAcc).
%% @hidden
spec_public_wlan14(Acc) ->
	Name = {name, "accessServiceProvider"},
	Desc = {description, "The PWLAN operator providing network access. This field, plus the type, will uniquely identify the provider by the same value that they are known in their industry."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan15(NewAcc).
%% @hidden
spec_public_wlan15(Acc) ->
	Name = {name, "locationName"},
	Desc = {description, "Descriptive Location Name of the user access network. For Example: 'Gate_14_Terminal_C_of_Newark_ Airport'. The source of this data will be from Optional VSA or Derived."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan16(NewAcc).
%% @hidden
spec_public_wlan16(Acc) ->
	Name = {name, "locationName"},
	Desc = {description, "Descriptive Location Name of the user access network. For Example: 'Gate_14_Terminal_C_of_Newark_ Airport'. The source of this data will be from Optional VSA or Derived."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan17(NewAcc).
%% @hidden
spec_public_wlan17(Acc) ->
	Name = {name, "locationId"},
	Desc = {description, "Describes the user’s access area within a given location. For example: Network=ACMEWISP_NewarkAirport"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan18(NewAcc).
%% @hidden
spec_public_wlan18(Acc) ->
	Name = {name, "locationType"},
	Desc = {description, "Contains the location type defined within the access provider’s network. Examples include: airport, hotel, coffee shop, and bookstore."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan19(NewAcc).
%% @hidden
spec_public_wlan19(Acc) ->
	Name = {name, "locationCountryCode"},
	Desc = {description, "ISO country code of the user’s location. 2 character alpha string. Derived. Can be derived from Geocode."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan20(NewAcc).
%% @hidden
spec_public_wlan20(Acc) ->
	Name = {name, "locationStateProvince"},
	Desc = {description, "2 character alpha string. Can be derived from Geocode"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan21(NewAcc).
%% @hidden
spec_public_wlan21(Acc) ->
	Name = {name, "locationCity"},
	Desc = {description, "Derived, can be derived from Geocode"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan22(NewAcc).
%% @hidden
spec_public_wlan22(Acc) ->
	Name = {name, "locationGeocode"},
	Desc = {description, "Content dictated by Type"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan23(NewAcc).
%% @hidden
spec_public_wlan23(Acc) ->
	Name = {name, "locationGeocodeType"},
	Desc = {description, "UTM, OSGB, Lat/Long"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan24(NewAcc).
%% @hidden
spec_public_wlan24(Acc) ->
	Name = {name, "nasPortType"},
	Desc = {description, "Identifier indicating the Port type. Values from RFC2865."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 4},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan25(NewAcc).
%% @hidden
spec_public_wlan25(Acc) ->
	Name = {name, "paymentType"},
	Desc = {description, "Applies only to settlement between Venue and Access provider."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 3},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan26(NewAcc).
%% @hidden
spec_public_wlan26(Acc) ->
	Name = {name, "networkConnectionType"},
	Desc = {description, "Uniquely identifies the network type used. For Example: WA=802.11a, WB=802.11b, WG=802.11G, EN=Ethernet (2 character alpha string) [??]=cdma2000"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan27(NewAcc).
%% @hidden
spec_public_wlan27(Acc) ->
	Name = {name, "sessionDuration"},
	Desc = {description, "Session duration in seconds (already compensated for idle timeout).  Possible source: RADIUS Acct-Session-Time"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 0},
	Value1 = {struct, [Typ1, From]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan28(NewAcc).
%% @hidden
spec_public_wlan28(Acc) ->
	Name = {name, "inputOctets"},
	Desc = {description, "Bytes user received."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 0},
	Value1 = {struct, [Typ1, From]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan29(NewAcc).
%% @hidden
spec_public_wlan29(Acc) ->
	Name = {name, "outputOctets"},
	Desc = {description, "Byes user transmitted."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 0},
	Value1 = {struct, [Typ1, From]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan30(NewAcc).
%% @hidden
spec_public_wlan30(Acc) ->
	Name = {name, "class"},
	Desc = {description, "Home Service Provider specified service class and provided if supported by Access Provider for that session"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan31(NewAcc).
%% @hidden
spec_public_wlan31(Acc) ->
	Name = {name, "gmtSessionStartDateTime"},
	Desc = {description, "The universal GMT date and time the session started with the Service Consumer’s perceived time zone. See ISO 8601."},
	Conf = {configurable, true},
	Typ1 = {valueType, "dateTime"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan32(NewAcc).
%% @hidden
spec_public_wlan32(Acc) ->
	Name = {name, "gmtSessionEndDateTime"},
	Desc = {description, "The universal GMT date and time the session ended with the Service Consumer’s perceived time zone. See ISO 8601."},
	Conf = {configurable, true},
	Typ1 = {valueType, "dateTime"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan33(NewAcc).
%% @hidden
spec_public_wlan33(Acc) ->
	Name = {name, "sessionTerminateCause"},
	Desc = {description, "RFC 3580 specifies, RFC 2866 enumerates"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 7},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan34(NewAcc).
%% @hidden
spec_public_wlan34(Acc) ->
	Name = {name, "billingClassOfService"},
	Desc = {description, "Indicates Service Type. Service level provided to user derived from Max-bandwidth-level. (Added for compatibility with WISPr)"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan35(NewAcc).
%% @hidden
spec_public_wlan35(Acc) ->
	Name = {name, "unitOfMeasure"},
	Desc = {description, "Indicates what is being represented in chargeable units field. The 'Quantity' enum item may be applicable for settlement for Partner content purchase."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 7},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan36(NewAcc).
%% @hidden
spec_public_wlan36(Acc) ->
	Name = {name, "chargeableUnit"},
	Desc = {description, "Indicates what activity the Chargeable_Quantity and Unit of Measure are metering."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 7},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan37(NewAcc).
%% @hidden
spec_public_wlan37(Acc) ->
	Name = {name, "chargeableQuantity"},
	Desc = {description, "Volume of chargeable_unit charged for this record."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 0},
	Value1 = {struct, [Typ1, From]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan38(NewAcc).
%% @hidden
spec_public_wlan38(Acc) ->
	Name = {name, "chargeAmount"},
	Desc = {description, "Amount of the charge, not including taxes."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 0},
	Value1 = {struct, [Typ1, From]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan39(NewAcc).
%% @hidden
spec_public_wlan39(Acc) ->
	Name = {name, "chargeCurrencyType"},
	Desc = {description, "Standard currency abbreviation from ISO 4217."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan40(NewAcc).
%% @hidden
spec_public_wlan40(Acc) ->
	Name = {name, "otherParty"},
	Desc = {description, "Identifies content or other party involved in transaction, if applicable. The party is associated with the charge since types of charges may have involved different parties. For example, the charge for network access is applied to access provider while charge for content applies to content provider."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan41(NewAcc).
%% @hidden
spec_public_wlan41(Acc) ->
	Name = {name, "taxPercentage"},
	Desc = {description, "The tax % applied to the charge. If blank, then the tax amount was a percentage or fixed value applied."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From1 = {valueFrom, 0},
	To1 = {valueTo, 100},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan42(NewAcc).
%% @hidden
spec_public_wlan42(Acc) ->
	Name = {name, "taxAmount"},
	Desc = {description, "The amount of tax. The charge amount does not include tax."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 0},
	Value1 = {struct, [Typ1, From]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan43(NewAcc).
%% @hidden
spec_public_wlan43(Acc) ->
	Name = {name, "taxType"},
	Desc = {description, "Type of tax applied."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 14},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan44(NewAcc).
%% @hidden
spec_public_wlan44(Acc) ->
	Name = {name, "intermediaryName"},
	Desc = {description, "Represents a human-readable PWLAN intermediary name string. Could be a reseller, aggregator, clearinghouse, etc."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan45(NewAcc).
%% @hidden
spec_public_wlan45(Acc) ->
	Name = {name, "serviceName"},
	Desc = {description, "Specifies the service type used. VoIP, Basic Access, Purchased Content, etc. Mention in remark that it’s not the RADIUS service type."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From = {valueFrom, 1},
	To = {valueTo, 6},
	Value1 = {struct, [Typ1, From, To]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan46(NewAcc).
%% @hidden
spec_public_wlan46(Acc) ->
	Name = {name, "relatedIpdrIdList"},
	Desc = {description, "Used to link together multiple related IPDRs when usage scenario and business rules demand so. Can’t change parent IPDR for audit/revenue assurance integrity."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan47(NewAcc).
%% @hidden
spec_public_wlan47(Acc) ->
	Name = {name, "tempUserId"},
	Desc = {description, "Temporary user identification allocated by home SP. This is an ID assigned by the Access Point."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	[{struct, [Name, Desc, Conf, Value]} | Acc].

%% @hidden
spec_timestamp() ->
	Name = {name, "timeStamp"},
	Desc = {description, "Time and date request was processed by OCS."},
	Conf = {configurable, true},
	Typ1 = {valueType, "dateTime"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_protocol() ->
	Name = {name, "protocol"},
	Desc = {description, "AAA protocol used in request."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, true},
	Val1 = {value, "RADIUS"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "DIAMETER"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_node() ->
	Name = {name, "node"},
	Desc = {description, "Time and date request was processed by OCS."},
	Conf = {configurable, true},
	Typ1 = {valueType, "dateTime"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_server_address() ->
	Name = {name, "serverAddress"},
	Desc = {description, "IP address request was received on."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_server_port() ->
	Name = {name, "serverPort"},
	Desc = {description, "IP port request was received on."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_client_address() ->
	Name = {name, "clientAddress"},
	Desc = {description, "IP address request was received from."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_client_port() ->
	Name = {name, "clientPort"},
	Desc = {description, "IP port request was received from."},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_type_access() ->
	Name = {name, "type"},
	Desc = {description, "Type of access event."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, false},
	Val1 = {value, "accept"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "reject"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "change"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2, Value3]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_type_accounting() ->
	Name = {name, "type"},
	Desc = {description, "Type of accounting event."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, false},
	Val1 = {value, "on"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "off"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "start"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Typ4 = {valueType, "string"},
	Def4 = {default, false},
	Val4 = {value, "interim"},
	Value4 = {struct, [Typ4, Def4, Val4]},
	Typ5 = {valueType, "string"},
	Def5 = {default, false},
	Val5 = {value, "event"},
	Value5 = {struct, [Typ5, Def5, Val5]},
	Value = {usageSpecCharacteristicValue,
			{array, [Value1, Value2, Value3, Value4, Value5]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_username() ->
	Name = {name, "username"},
	Desc = {description, "Username/identity of subscriber."},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_ip() ->
	Name = {name, "nasIpAddress"},
	Desc = {description, "NAS-IP-Address attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	From1 = {valueFrom, "0.0.0.0"},
	To1 = {valueTo, "255.255.255.255"},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port() ->
	Name = {name, "nasPort"},
	Desc = {description, "NAS-Port attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_service_type() ->
	Name = {name, "serviceType"},
	Desc = {description, "Service-Type attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, true},
	Val1 = {value, "framed"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "administrative"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "authenticate-only"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2, Value3]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_address() ->
	Name = {name, "framedIpAddress"},
	Desc = {description, "Framed-IP-Address attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	From1 = {valueFrom, "0.0.0.0"},
	To1 = {valueTo, "255.255.255.255"},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_pool() ->
	Name = {name, "framedPool"},
	Desc = {description, "Framed-Pool attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_netmask() ->
	Name = {name, "framedIpNetmask"},
	Desc = {description, "Framed-IP-Netmask attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	From1 = {valueFrom, "0.0.0.0"},
	To1 = {valueTo, "255.255.255.255"},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_routing() ->
	Name = {name, "framedRouting"},
	Desc = {description, "Framed-Routing attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, true},
	Val1 = {value, "none"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "listen-for-routing-packets"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "listen-for-routing-packets"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Typ4 = {valueType, "string"},
	Def4 = {default, false},
	Val4 = {value, "send-and-listen"},
	Value4 = {struct, [Typ4, Def4, Val4]},
	Value = {usageSpecCharacteristicValue,
			{array, [Value1, Value2, Value3, Value4]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_filter_id() ->
	Name = {name, "filterId"},
	Desc = {description, "Filter-Id attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_mtu() ->
	Name = {name, "framedMtu"},
	Desc = {description, "Framed-MTU attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From1 = {valueFrom, 64},
	To1 = {valueTo, 65535},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_route() ->
	Name = {name, "framedRoute"},
	Desc = {description, "Framed-Route attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_class() ->
	Name = {name, "class"},
	Desc = {description, "Class attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_session_timeout() ->
	Name = {name, "sessionTimeout"},
	Desc = {description, "Session-Timeout attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From1 = {valueFrom, 0},
	To1 = {valueTo, 4294967295},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_idle_timeout() ->
	Name = {name, "idleTimeout"},
	Desc = {description, "Idle-Timeout attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	From1 = {valueFrom, 0},
	To1 = {valueTo, 4294967295},
	Value1 = {struct, [Typ1, From1, To1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_termination_action() ->
	Name = {name, "terminationAction"},
	Desc = {description, "Termination-Action attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, false},
	Val1 = {value, "default"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "aaa-request"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_called_id() ->
	Name = {name, "calledStationId"},
	Desc = {description, "Called-Station-Id attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_calling_id() ->
	Name = {name, "callingStationId"},
	Desc = {description, "Calling-Station-Id attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_id() ->
	Name = {name, "nasIdentifier"},
	Desc = {description, "NAS-Identifier attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port_id() ->
	Name = {name, "nasPortId"},
	Desc = {description, "NAS-Port-Id attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port_type() ->
	Name = {name, "nasPortType"},
	Desc = {description, "NAS-Port-Type attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, false},
	Val1 = {value, "virtual"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "sdsl"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "adsl-cap"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Typ4 = {valueType, "string"},
	Def4 = {default, false},
	Val4 = {value, "adsl-dmt"},
	Value4 = {struct, [Typ4, Def4, Val4]},
	Typ5 = {valueType, "string"},
	Def5 = {default, false},
	Val5 = {value, "ethernet"},
	Value5 = {struct, [Typ5, Def5, Val5]},
	Typ6 = {valueType, "string"},
	Def6 = {default, false},
	Val6 = {value, "xdsl"},
	Value6 = {struct, [Typ6, Def6, Val6]},
	Typ7 = {valueType, "string"},
	Def7 = {default, false},
	Val7 = {value, "cable"},
	Value7 = {struct, [Typ7, Def7, Val7]},
	Typ8 = {valueType, "string"},
	Def8 = {default, false},
	Val8 = {value, "wireless-other"},
	Value8 = {struct, [Typ8, Def8, Val8]},
	Typ9 = {valueType, "string"},
	Def9 = {default, false},
	Val9 = {value, "wireless-ieee802.11"},
	Value9 = {struct, [Typ9, Def9, Val9]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2,
			Value3, Value4, Value5, Value6, Value7, Value8, Value9]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port_limit() ->
	Name = {name, "nasPortLimit"},
	Desc = {description, "NAS-Port-Limit attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_delay() ->
	Name = {name, "acctDelayTime"},
	Desc = {description, "Acct-Delay-Time attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_event_timestamp() ->
	Name = {name, "eventTimestamp"},
	Desc = {description, "Event-Timestamp attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "dateTime"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_session_id() ->
	Name = {name, "acctSessionId"},
	Desc = {description, "Acct-Session-Id attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_multi_session_id() ->
	Name = {name, "acctMultiSessionId"},
	Desc = {description, "Acct-Multi-Session-Id attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_link_count() ->
	Name = {name, "acctLinkCount"},
	Desc = {description, "Acct-Link-Count attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_authentic() ->
	Name = {name, "acctAuthentic"},
	Desc = {description, "Acct-Authentic attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, false},
	Val1 = {value, "RADIUS"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "local"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "remote"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2, Value3]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_session_time() ->
	Name = {name, "acctSessionTime"},
	Desc = {description, "Acct-Session-Time attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_input_octets() ->
	Name = {name, "inputOctets"},
	Desc = {description, "Acct-Input-Octets attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_output_octets() ->
	Name = {name, "outputOctets"},
	Desc = {description, "Acct-Output-Octets attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_input_giga_words() ->
	Name = {name, "acctInputGigaWords"},
	Desc = {description, "Acct-Input-Gigawords attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_output_giga_words() ->
	Name = {name, "acctOutputGigaWords"},
	Desc = {description, "Acct-Output-Gigawords attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_input_packets() ->
	Name = {name, "acctInputPackets"},
	Desc = {description, "Acct-Input-Packets attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_output_packets() ->
	Name = {name, "acctOutputPackets"},
	Desc = {description, "Acct-Output-Packets attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_data_rate() ->
	Name = {name, "ascendDataRate"},
	Desc = {description, "Ascend-Data-Rate attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_xmit_rate() ->
	Name = {name, "ascendXmitRate"},
	Desc = {description, "Ascend-Xmit-Rate attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_interim_interval() ->
	Name = {name, "acctInterimInterval"},
	Desc = {description, "Acct-Interim-Interval  attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "number"},
	Value1 = {struct, [Typ1]},
	Value = {usageSpecCharacteristicValue, {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_cause() ->
	Name = {name, "acctTerminateCause"},
	Desc = {description, "Acct-Terminate-Cause attribute"},
	Conf = {configurable, true},
	Typ1 = {valueType, "string"},
	Def1 = {default, false},
	Val1 = {value, "user-request"},
	Value1 = {struct, [Typ1, Def1, Val1]},
	Typ2 = {valueType, "string"},
	Def2 = {default, false},
	Val2 = {value, "lost-carrier"},
	Value2 = {struct, [Typ2, Def2, Val2]},
	Typ3 = {valueType, "string"},
	Def3 = {default, false},
	Val3 = {value, "lost-service"},
	Value3 = {struct, [Typ3, Def3, Val3]},
	Typ4 = {valueType, "string"},
	Def4 = {default, false},
	Val4 = {value, "idle-timeout"},
	Value4 = {struct, [Typ4, Def4, Val4]},
	Typ5 = {valueType, "string"},
	Def5 = {default, false},
	Val5 = {value, "session-timeout"},
	Value5 = {struct, [Typ5, Def5, Val5]},
	Typ6 = {valueType, "string"},
	Def6 = {default, false},
	Val6 = {value, "admin-reset"},
	Value6 = {struct, [Typ6, Def6, Val6]},
	Typ7 = {valueType, "string"},
	Def7 = {default, false},
	Val7 = {value, "admin-reboot"},
	Value7 = {struct, [Typ7, Def7, Val7]},
	Typ8 = {valueType, "string"},
	Def8 = {default, false},
	Val8 = {value, "port-error"},
	Value8 = {struct, [Typ8, Def8, Val8]},
	Typ9 = {valueType, "string"},
	Def9 = {default, false},
	Val9 = {value, "NAS-error"},
	Value9 = {struct, [Typ9, Def9, Val9]},
	Typ10 = {valueType, "string"},
	Def10 = {default, false},
	Val10 = {value, "NAS-request"},
	Value10 = {struct, [Typ10, Def10, Val10]},
	Typ11 = {valueType, "string"},
	Def11 = {default, false},
	Val11 = {value, "NAS-reboot"},
	Value11 = {struct, [Typ11, Def11, Val11]},
	Typ12 = {valueType, "string"},
	Def12 = {default, false},
	Val12 = {value, "port-uneeded"},
	Value12 = {struct, [Typ12, Def12, Val12]},
	Typ13 = {valueType, "string"},
	Def13 = {default, false},
	Val13 = {value, "port-preempted"},
	Value13 = {struct, [Typ13, Def13, Val13]},
	Typ14 = {valueType, "string"},
	Def14 = {default, false},
	Val14 = {value, "port-suspended"},
	Value14 = {struct, [Typ14, Def14, Val14]},
	Typ15 = {valueType, "string"},
	Def15 = {default, false},
	Val15 = {value, "service-unavailable"},
	Value15 = {struct, [Typ15, Def15, Val15]},
	Typ16 = {valueType, "string"},
	Def16 = {default, false},
	Val16 = {value, "user-error"},
	Value16 = {struct, [Typ16, Def16, Val16]},
	Value = {usageSpecCharacteristicValue, {array, [Value1, Value2, Value3,
			Value4, Value5, Value6, Value7, Value8, Value9, Value10, Value11,
			Value12, Value13, Value14, Value15, Value16]}},
	{struct, [Name, Desc, Conf, Value]}.

