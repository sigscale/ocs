%%% ocs_rest_res_usage.erl
%%% vim: ts=3
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
		get_usage/2, get_usage/3, get_usagespec/1, get_usagespec/2,
		get_ipdr/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The ocs_rest_res_usage API
%%----------------------------------------------------------------------

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

-spec get_usage(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usage'
%% 	requests.
%% @hidden
get_usage(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_usage1(NewQuery, Filters, Headers);
		false ->
			get_usage1(Query, [], Headers)
	end.
%% @hidden
get_usage1(Query, Filters, Headers) ->
	case lists:keytake("sort", 1, Query) of
		{value, {_, "-date"}, NewQuery} ->
			get_last(NewQuery, Filters);
		{value, {_, Date}, NewQuery}
				when Date == "date"; Date == "+date" ->
			get_usage2(NewQuery, Filters, Headers);
		false ->
			get_usage2(Query, Filters, Headers);
		_ ->
			{error, 400}
	end.
%% @hidden
get_usage2(Query, Filters, Headers) ->
	case {lists:keyfind("if-match", 1, Headers),
			lists:keyfind("if-range", 1, Headers),
			lists:keyfind("range", 1, Headers)} of
		{{"if-match", Etag}, false, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(Query, Filters, Start, End)
					end;
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", Range}} ->
			case ocs_rest:range(Range) of
				{error, _Reason} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(Query, Filters, undefined, undefined)
	end.

-spec get_usage(Id, Query, Headers) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usage/{id}'
%% 	requests.
get_usage("auth-" ++ _ = Id, [] = _Query, _Headers) ->
	try
		["auth", TimeStamp, Serial] = string:tokens(Id, [$-]),
		TS = list_to_integer(TimeStamp),
		N = list_to_integer(Serial),
		case ocs_log:auth_query(start, TS, TS, '_', '_', '_', '_') of
			{error, _Reason} ->
				{error, 500};
			{_Cont, Events} ->
				case lists:keyfind(N, 2, Events) of
					Event when is_tuple(Event) ->
						{struct, Attr} = usage_aaa_auth(Event, []),
						{_, Date} = lists:keyfind("date", 1, Attr),
						Body = mochijson:encode({struct, Attr}),
						RespHeaders = [{content_type, "application/json"},
								{last_modified, Date}],
						{ok, RespHeaders, Body};
					_Other ->
						{error, 404}
				end
		end
	catch
		_:_Reason1 ->
			{error, 404}
	end;
get_usage("acct-" ++ _ = Id, [] = _Query, _Headers) ->
	try
		["acct", TimeStamp, Serial] = string:tokens(Id, [$-]),
		TS = list_to_integer(TimeStamp),
		N = list_to_integer(Serial),
		case ocs_log:acct_query(start, TS, TS, '_', '_', '_') of
			{error, _Reason} ->
				{error, 500};
			{_Cont, Events} ->
				case lists:keyfind(N, 2, Events) of
					Event when is_tuple(Event) ->
						{struct, Attr} = usage_aaa_acct(Event, []),
						{_, Date} = lists:keyfind("date", 1, Attr),
						Body = mochijson:encode({struct, Attr}),
						RespHeaders = [{content_type, "application/json"},
								{last_modified, Date}],
						{ok, RespHeaders, Body};
					_Other ->
						{error, 404}
				end
		end
	catch
		_:_Reason1 ->
			{error, 404}
	end;
get_usage(Id, [] = _Query, _Headers) ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	FileName = Directory ++ "/" ++ Id,
	read_ipdr(FileName, MaxItems).

-spec get_usagespec(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usageSpecification'
%% 	requests.
%% @todo http transfer usage spec
get_usagespec([] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode({array, [spec_aaa_auth(),
			spec_aaa_acct(), spec_public_wlan()]}),
	{ok, RespHeaders, Body};
get_usagespec(Query) ->
	RespHeaders = [{content_type, "application/json"}],
	case lists:keytake("name", 1, Query) of
		{_, {_, "AAAAccessUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_aaa_auth()]}),
			{ok, RespHeaders, Body};
		{_, {_, "AAAAccessUsageSpec"}, _} ->
			{error, 400};
		{_, {_, "AAAAccountingUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_aaa_acct()]}),
			{ok, RespHeaders, Body};
		{_, {_, "AAAAccountingUsageSpec"}, _} ->
			{error, 400};
		{_, {_, "PublicWLANAccessUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_public_wlan()]}),
			{ok, RespHeaders, Body};
		{_, {_, "PublicWLANAccessUsageSpec"}, _} ->
			{error, 400};
		{_, {_, "HTTPTransferUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_http_transfer()]}),
			{ok, RespHeaders, Body};
		false ->
			{error, 404};
		_ ->
			{error, 400}
	end.

-spec get_usagespec(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usageSpecification/{id}'
%% 	requests.
%% @todo http transfer usage spec
get_usagespec("AAAAccessUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_aaa_auth()),
	{ok, RespHeaders, Body};
get_usagespec("AAAAccessUsageSpec", _Query) ->
	{error, 400};
get_usagespec("AAAAccountingUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_aaa_acct()),
	{ok, RespHeaders, Body};
get_usagespec("AAAAccountingUsageSpec", _Query) ->
	{error, 400};
get_usagespec("PublicWLANAccessUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_public_wlan()),
	{ok, RespHeaders, Body};
get_usagespec("PublicWLANAccessUsageSpec", _Query) ->
	{error, 400};
get_usagespec("HTTPTransferUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_http_transfer()),
	{ok, RespHeaders, Body};
get_usagespec(_Id, _Query) ->
	{error, 404}.

-spec get_ipdr(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /ocs/v1/log/ipdr'
%% 	requests.
get_ipdr([] = _Query) ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	case file:list_dir(Directory) of
		{ok, Files} ->
			SortedFiles = lists:reverse(lists:sort(Files)),
			Body = mochijson:encode({array, SortedFiles}),
			RespHeaders = [{content_type, "application/json"}],
			{ok, RespHeaders, Body};
		{error, _Reason} ->
			{error, 500}
	end;
get_ipdr(_Query) ->
	{error, 400}.

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
				UsageSpecification = {struct, [{"id", 1},
						{"href", "usageManagement/v1/usageSpecification/1"},
						{"name", "PublicWLANAccessUsageSpec"}]},
				UsageCharacteristicObjs = ipdr_characteristics(Ipdr),
				UsageCharacteristic = {array, UsageCharacteristicObjs},
				RespObj = [{struct, [{"id", N + 1},
						{"href", "usageManagement/v1/usage/" ++ integer_to_list(N + 1)},
						{"date", "SomeDateTime"}, {"type", "PublicWLANAccessUsage"},
						{"description", "Description for individual usage content"},
						{"status", "received"},
						{"usageSpecification", UsageSpecification},
						{"usageCharacteristic", UsageCharacteristic}]}],
						{N + 1, [RespObj | Acc]};
			(#ipdrDocEnd{}, {N, Acc}) ->
				{N, Acc}
	end,
	lists:foldl(F, {Count, []}, Records).

%% @hidden
ipdr_characteristics(#ipdr{} = Ipdr) ->
	ipdr_characteristics(record_info(fields, ipdr), Ipdr, []).

%% @hidden
ipdr_characteristics([ipdrCreationTime | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "ipdrCreationTime"}, {"value", Ipdr#ipdr.ipdrCreationTime}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([seqNum | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "seqNum"}, {"value", Ipdr#ipdr.seqNum}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([username | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "username"}, {"value", Ipdr#ipdr.username}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([scIdType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "scIdType"}, {"value", Ipdr#ipdr.scIdType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([scId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "scId"}, {"value", Ipdr#ipdr.scId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([homeServiceProviderType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "homeServiceProviderType"}, {"value", Ipdr#ipdr.homeServiceProviderType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([homeServiceProvider | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "homeServiceProvider"}, {"value", Ipdr#ipdr.homeServiceProvider}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([acctSessionId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "acctSessionId"}, {"value", Ipdr#ipdr.acctSessionId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([userIpAddress | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "userIpAddress"}, {"value", Ipdr#ipdr.userIpAddress}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([callingStationId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "callingStationId"}, {"value", Ipdr#ipdr.callingStationId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([nasIpAddress | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "nasIpAddress"}, {"value", Ipdr#ipdr.nasIpAddress}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([calledStationId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "calledStationId"}, {"value", Ipdr#ipdr.calledStationId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([nasId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "nasId"}, {"value", Ipdr#ipdr.nasId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([accessProviderType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "accessProviderType"}, {"value", Ipdr#ipdr.accessProviderType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([accessServiceProvider | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "accessServiceProvider"}, {"value", Ipdr#ipdr.accessServiceProvider}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationName | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationName"}, {"value", Ipdr#ipdr.locationName}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationId"}, {"value", Ipdr#ipdr.locationId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationType"}, {"value", Ipdr#ipdr.locationType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationCountryCode | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationCountryCode"}, {"value", Ipdr#ipdr.locationCountryCode}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationStateProvince | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationStateProvince"}, {"value", Ipdr#ipdr.locationStateProvince}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationCity | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationCity"}, {"value", Ipdr#ipdr.locationCity}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationGeocode | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationGeocode"}, {"value", Ipdr#ipdr.locationGeocode}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([locationGeocodeType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "locationGeocodeType"}, {"value", Ipdr#ipdr.locationGeocodeType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([nasPortType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "nasPortType"}, {"value", Ipdr#ipdr.nasPortType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([paymentType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "paymentType"}, {"value", Ipdr#ipdr.paymentType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([networkConnectionType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "networkConnectionType"}, {"value", Ipdr#ipdr.networkConnectionType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([sessionDuration | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "sessionDuration"}, {"value", Ipdr#ipdr.sessionDuration}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([inputOctets | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "inputOctets"}, {"value", Ipdr#ipdr.inputOctets}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([outputOctets | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "outputOctets"}, {"value", Ipdr#ipdr.outputOctets}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([class | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "class"}, {"value", Ipdr#ipdr.class}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([gmtSessionStartDateTime | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "gmtSessionStartDateTime"}, {"value", Ipdr#ipdr.gmtSessionStartDateTime}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([gmtSessionEndDateTime | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "gmtSessionEndDateTime"}, {"value", Ipdr#ipdr.gmtSessionEndDateTime}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([sessionTerminateCause | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "sessionTerminateCause"}, {"value", Ipdr#ipdr.sessionTerminateCause}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([billingClassOfService | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "billingClassOfService"}, {"value", Ipdr#ipdr.billingClassOfService}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([unitOfMeasure | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "unitOfMeasure"}, {"value", Ipdr#ipdr.unitOfMeasure}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([chargeableUnit | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "chargeableUnit"}, {"value", Ipdr#ipdr.chargeableUnit}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([chargeableQuantity | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "chargeableQuantity"}, {"value", Ipdr#ipdr.chargeableQuantity}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([chargeAmount | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "chargeAmount"}, {"value", Ipdr#ipdr.chargeAmount}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([chargeCurrencyType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "chargeCurrencyType"}, {"value", Ipdr#ipdr.chargeCurrencyType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([otherParty | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "otherParty"}, {"value", Ipdr#ipdr.otherParty}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([taxPercentage | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "taxPercentage"}, {"value", Ipdr#ipdr.taxPercentage}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([taxAmount | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "taxAmount"}, {"value", Ipdr#ipdr.taxAmount}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([taxType | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "taxType"}, {"value", Ipdr#ipdr.taxType}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([intermediaryName | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "intermediaryName"}, {"value", Ipdr#ipdr.intermediaryName}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([serviceName | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "serviceName"}, {"value", Ipdr#ipdr.serviceName}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([relatedIpdrIdList | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "relatedIpdrIdList"}, {"value", Ipdr#ipdr.relatedIpdrIdList}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([tempUserId | T], Ipdr, Acc) ->
	Struct = {struct, [{"key", "tempUserId"}, {"value", Ipdr#ipdr.tempUserId}]},
	ipdr_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_characteristics([], _Ipdr, Acc) ->
	lists:reverse(Acc).

%% @hidden
usage_aaa_auth({Milliseconds, N, P, Node,
		{ServerIP, ServerPort}, {ClientIP, ClientPort}, EventType,
		RequestAttributes, ResponseAttributes}, Filters) ->
	UsageSpec = {struct, [{"id", "AAAAccessUsageSpec"},
			{"href", "/usageManagement/v1/usageSpecification/AAAAccessUsageSpec"},
			{"name", "AAAAccessUsageSpec"}]},
	Type = "AAAAccessUsage",
	Status = "received",
	ID = "auth-" ++ integer_to_list(Milliseconds) ++ "-"
			++ integer_to_list(N),
	Href = "/usageManagement/v1/usage/" ++ ID,
	Date = ocs_log:iso8601(Milliseconds),
	Protocol = case P of
		radius ->
			"RADIUS";
		diameter ->
			"DIAMETER"
	end,
	ServerAddress = inet:ntoa(ServerIP),
	ClientAddress = inet:ntoa(ClientIP),
	EventChars = [{struct, [{"name", "protocol"}, {"value", Protocol}]},
			{struct, [{"name", "node"}, {"value", atom_to_list(Node)}]},
			{struct, [{"name", "serverAddress"}, {"value", ServerAddress}]},
			{struct, [{"name", "serverPort"}, {"value", ServerPort}]},
			{struct, [{"name", "clientAddress"}, {"value", ClientAddress}]},
			{struct, [{"name", "clientPort"}, {"value", ClientPort}]},
			{struct, [{"name", "type"}, {"value", atom_to_list(EventType)}]}],
	RequestChars = usage_characteristics(RequestAttributes),
	ResponseChars = usage_characteristics(ResponseAttributes),
	UsageChars = EventChars ++ RequestChars ++ ResponseChars,
	Object = {struct, [{"id", ID}, {"href", Href}, {"date", Date}, {"type", Type},
			{"status", Status}, {"usageSpecification", UsageSpec},
			{"usageCharacteristic", {array, UsageChars}}]},
	case Filters of
		[] ->
			Object;
		_ ->
			ocs_rest:fields("id,href," ++ Filters, Object)
	end;
usage_aaa_auth(Events, Filters) when is_list(Events) ->
	usage_aaa_auth(Events, Filters, []).
%% @hidden
usage_aaa_auth([H | T], Filters, Acc) ->
	usage_aaa_auth(T, Filters, [usage_aaa_auth(H, Filters) | Acc]);
usage_aaa_auth([], _Filters, Acc) ->
	lists:reverse(Acc).

%% @hidden
usage_aaa_acct({Milliseconds, N, P, Node,
		{ServerIP, ServerPort}, EventType, Attributes}, Filters) ->
	UsageSpec = {struct, [{"id", "AAAAccountingUsageSpec"},
			{"href", "/usageManagement/v1/usageSpecification/AAAAccountingUsageSpec"},
			{"name", "AAAAccountingUsageSpec"}]},
	Type = "AAAAccountingUsage",
	Status = "received",
	ID = "acct-" ++ integer_to_list(Milliseconds) ++ "-"
			++ integer_to_list(N),
	Href = "/usageManagement/v1/usage/" ++ ID,
	Date = ocs_log:iso8601(Milliseconds),
	Protocol = case P of
		radius ->
			"RADIUS";
		diameter ->
			"DIAMETER"
	end,
	ServerAddress = inet:ntoa(ServerIP),
	EventChars = [{struct, [{"name", "protocol"}, {"value", Protocol}]},
			{struct, [{"name", "node"}, {"value", atom_to_list(Node)}]},
			{struct, [{"name", "serverAddress"}, {"value", ServerAddress}]},
			{struct, [{"name", "serverPort"}, {"value", ServerPort}]},
			{struct, [{"name", "type"}, {"value", atom_to_list(EventType)}]}],
	AttributeChars = usage_characteristics(Attributes),
	UsageChars = EventChars ++ AttributeChars,
	Object = {struct, [{"id", ID}, {"href", Href}, {"date", Date}, {"type", Type},
			{"status", Status}, {"usageSpecification", UsageSpec},
			{"usageCharacteristic", {array, UsageChars}}]},
	case Filters of
		[] ->
			Object;
		_ ->
			ocs_rest:fields("id,href," ++ Filters, Object)
	end;
usage_aaa_acct(Events, Filters) when is_list(Events) ->
	usage_aaa_acct(Events, Filters, []).
%% @hidden
usage_aaa_acct([H | T], Filters, Acc) ->
	usage_aaa_acct(T, Filters, [usage_aaa_acct(H, Filters) | Acc]);
usage_aaa_acct([], _Filters, Acc) ->
	lists:reverse(Acc).

%% @hidden
usage_http_transfer({Host, User, DateTime, Method, URI, HttpStatus}, Filters) ->
	UsageSpec = {struct, [{"id", "HTTPTransferUsageSpec"},
			{"href", "/usageManagement/v1/usageSpecification/HTTPTransferUsageSpec"},
			{"name", "HTTPTransferUsageSpec"}]},
	Type = "HTTPTransferUsage",
	UsageChars = [{struct, [{"name", "host"}, {"value", Host}]},
			{struct, [{"name", "user"}, {"value", User}]},
			{struct, [{"name", "method"}, {"value", Method}]},
			{struct, [{"name", "uri"}, {"value", URI}]},
			{struct, [{"name", "httpStatus"}, {"value", HttpStatus}]}],
	Object = {struct, [{"date", DateTime}, {"type", Type},
			{"usageSpecification", UsageSpec},
			{"usageCharacteristic", {array, UsageChars}}]},
	if
		Filters =:= [] ->
			Object;
		true ->
			ocs_rest:fields("id,href," ++ Filters, Object)
	end;
usage_http_transfer(Events, Filters) when is_list(Events) ->
	usage_http_transfer(Events, Filters, []).
%% @hidden
usage_http_transfer([H | T], Filters, Acc) ->
	usage_http_transfer(T, Filters, [usage_http_transfer(H, Filters) | Acc]);
usage_http_transfer([], _Filters, Acc) ->
	lists:reverse(Acc).

%% @hidden
spec_aaa_auth() ->
	ID = {"id", "AAAAccessUsageSpec"},
	Href = {"href", "/usageManagement/v1/usageSpecification/AAAAccessUsageSpec"},
	Name = {"name", "AAAAccessUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS access requests."},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", "2017-12-31T23:59:59Z"},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [spec_protocol(), spec_node(), spec_server_address(),
			spec_server_port(), spec_client_address(),
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
			spec_attr_port_limit(), spec_attr_data_rate(),
			spec_attr_xmit_rate(), spec_attr_interim_interval(),
			spec_attr_class()],
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_aaa_acct() ->
	ID = {"id", "AAAAccountingUsageSpec"},
	Href = {"href", "/usageManagement/v1/usageSpecification/AAAAccountingUsageSpec"},
	Name = {"name", "AAAAccountingUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS accounting requests."},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", "2017-12-31T23:59:59Z"},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [spec_protocol(), spec_node(), spec_server_address(),
			spec_server_port(), spec_type_accounting(),
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
			spec_attr_cause(), spec_attr_multisession_id(),
			spec_attr_link_count(), spec_attr_nas_port_type(),
			spec_attr_port_limit()],
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_http_transfer() ->
	ID = {"id", "HTTPTransferUsageSpec"},
	Href = {"href", "/usageManagement/v1/usageSpecification/HTTPTransferUsageSpec"},
	Name = {"name", "HTTPTransferUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS http requests."},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", "2017-12-31T23:59:59Z"},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [{struct, [{"todo", "todo"}]}],
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_public_wlan() ->
	ID = {"id", "PublicWLANAccessUsageSpec"},
	Href = {"href", "/usageManagement/v1/usageSpecification/PublicWLANAccessUsageSpec"},
	Name = {"name", "PublicWLANAccessUsageSpec"},
	Desc = {"description", "Specification for IPDR Public WLAN Access - WISP Use Case"},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", "2017-12-31T23:59:59Z"},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = lists:reverse(spec_public_wlan1([])),
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.
%% @hidden
spec_public_wlan1(Acc) ->
	Name = {"name", "userName"},
	Desc = {"description", "The end user ID and their domain name (NAI)."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan2(NewAcc).
%% @hidden
spec_public_wlan2(Acc) ->
	Name = {"name", "ScIdType"},
	Desc = {"description", "Type of Service Consumer ID Used when a more specific Identifier of Service Consumer is necessary. For example, IMSI for GSM subscribers."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 3},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan3(NewAcc).
%% @hidden
spec_public_wlan3(Acc) ->
	Name = {"name", "ScId"},
	Desc = {"description", "The Service Consumer ID when a more specific identifier of the Service Consumer is required. For example, IMSI for GSM/GPRS subscribers."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan4(NewAcc).
%% @hidden
spec_public_wlan4(Acc) ->
	Name = {"name", "homeServiceProviderType"},
	Desc = {"description", "Identifies how the home service provider is identified."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 4},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan5(NewAcc).
%% @hidden
spec_public_wlan5(Acc) ->
	Name = {"name", "homeServiceProvider"},
	Desc = {"description", "The user’s Home Service Provider. May be derived from the NAI of the Username. This field, plus the type, will uniquely identify the provider by the same value that they are known in their industry."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan6(NewAcc).
%% @hidden
spec_public_wlan6(Acc) ->
	Name = {"name", "acctSessionId"},
	Desc = {"description", "Account session ID assigned by the NAS server. Each session is assigned a unique NAS ID and is therefore used as one of the key criteria in the Settlement Process to identify unique transactions."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan7(NewAcc).
%% @hidden
spec_public_wlan7(Acc) ->
	Name = {"name", "userIpAddress"},
	Desc = {"description", "IP Address of the end user (calling station). This field must support IPv6 format."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan8(NewAcc).
%% @hidden
spec_public_wlan8(Acc) ->
	Name = {"name", "callingStationId"},
	Desc = {"description", "MAC Address of the end user's device as formatted in RFC3580, section 3.21. For example, 00-10-A4-23-19-C0"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan9(NewAcc).
%% @hidden
spec_public_wlan9(Acc) ->
	Name = {"name", "nasIpAddress"},
	Desc = {"description", "The IP address of the local Network Access Server (NAS) (i.e. the access gateway) that provides the service. This field must support IPv6 format."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan10(NewAcc).
%% @hidden
spec_public_wlan10(Acc) ->
	Name = {"name", "nasIpAddress"},
	Desc = {"description", "The IP address of the local Network Access Server (NAS) (i.e. the access gateway) that provides the service. This field must support IPv6 format."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan11(NewAcc).
%% @hidden
spec_public_wlan11(Acc) ->
	Name = {"name", "calledStationId"},
	Desc = {"description", "A unique name which identifies the hotspot venue. Radius Defined using the Mac Address and SSID in the format shown in RFC3580 section 3.20. For example: 00-10- A4-23-19-C0:AP1."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan12(NewAcc).
%% @hidden
spec_public_wlan12(Acc) ->
	Name = {"name", "nasId"},
	Desc = {"description", "Will appear in Access Request record format (depends on WISP network configuration and BSS system). Identifies the access gateway when NAS-IP-Address is insufficient."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan13(NewAcc).
%% @hidden
spec_public_wlan13(Acc) ->
	Name = {"name", "accessProviderType"},
	Desc = {"description", "Identifies how the serve/visited service provider is identified. For example, Domain Name, PMN code, SID/BID number, or BRI. Need to identify the possible values."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 4},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan14(NewAcc).
%% @hidden
spec_public_wlan14(Acc) ->
	Name = {"name", "accessServiceProvider"},
	Desc = {"description", "The PWLAN operator providing network access. This field, plus the type, will uniquely identify the provider by the same value that they are known in their industry."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan15(NewAcc).
%% @hidden
spec_public_wlan15(Acc) ->
	Name = {"name", "locationName"},
	Desc = {"description", "Descriptive Location Name of the user access network. For Example: 'Gate_14_Terminal_C_of_Newark_ Airport'. The source of this data will be from Optional VSA or Derived."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan16(NewAcc).
%% @hidden
spec_public_wlan16(Acc) ->
	Name = {"name", "locationName"},
	Desc = {"description", "Descriptive Location Name of the user access network. For Example: 'Gate_14_Terminal_C_of_Newark_ Airport'. The source of this data will be from Optional VSA or Derived."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan17(NewAcc).
%% @hidden
spec_public_wlan17(Acc) ->
	Name = {"name", "locationId"},
	Desc = {"description", "Describes the user’s access area within a given location. For example: Network=ACMEWISP_NewarkAirport"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan18(NewAcc).
%% @hidden
spec_public_wlan18(Acc) ->
	Name = {"name", "locationType"},
	Desc = {"description", "Contains the location type defined within the access provider’s network. Examples include: airport, hotel, coffee shop, and bookstore."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan19(NewAcc).
%% @hidden
spec_public_wlan19(Acc) ->
	Name = {"name", "locationCountryCode"},
	Desc = {"description", "ISO country code of the user’s location. 2 character alpha string. Derived. Can be derived from Geocode."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan20(NewAcc).
%% @hidden
spec_public_wlan20(Acc) ->
	Name = {"name", "locationStateProvince"},
	Desc = {"description", "2 character alpha string. Can be derived from Geocode"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan21(NewAcc).
%% @hidden
spec_public_wlan21(Acc) ->
	Name = {"name", "locationCity"},
	Desc = {"description", "Derived, can be derived from Geocode"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan22(NewAcc).
%% @hidden
spec_public_wlan22(Acc) ->
	Name = {"name", "locationGeocode"},
	Desc = {"description", "Content dictated by Type"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan23(NewAcc).
%% @hidden
spec_public_wlan23(Acc) ->
	Name = {"name", "locationGeocodeType"},
	Desc = {"description", "UTM, OSGB, Lat/Long"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan24(NewAcc).
%% @hidden
spec_public_wlan24(Acc) ->
	Name = {"name", "nasPortType"},
	Desc = {"description", "Identifier indicating the Port type. Values from RFC2865."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 4},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan25(NewAcc).
%% @hidden
spec_public_wlan25(Acc) ->
	Name = {"name", "paymentType"},
	Desc = {"description", "Applies only to settlement between Venue and Access provider."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 3},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan26(NewAcc).
%% @hidden
spec_public_wlan26(Acc) ->
	Name = {"name", "networkConnectionType"},
	Desc = {"description", "Uniquely identifies the network type used. For Example: WA=802.11a, WB=802.11b, WG=802.11G, EN=Ethernet (2 character alpha string) [??]=cdma2000"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan27(NewAcc).
%% @hidden
spec_public_wlan27(Acc) ->
	Name = {"name", "sessionDuration"},
	Desc = {"description", "Session duration in seconds (already compensated for idle timeout).  Possible source: RADIUS Acct-Session-Time"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 0},
	Value1 = {struct, [Typ, From]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan28(NewAcc).
%% @hidden
spec_public_wlan28(Acc) ->
	Name = {"name", "inputOctets"},
	Desc = {"description", "Bytes user received."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 0},
	Value1 = {struct, [Typ, From]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan29(NewAcc).
%% @hidden
spec_public_wlan29(Acc) ->
	Name = {"name", "outputOctets"},
	Desc = {"description", "Byes user transmitted."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 0},
	Value1 = {struct, [Typ, From]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan30(NewAcc).
%% @hidden
spec_public_wlan30(Acc) ->
	Name = {"name", "class"},
	Desc = {"description", "Home Service Provider specified service class and provided if supported by Access Provider for that session"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan31(NewAcc).
%% @hidden
spec_public_wlan31(Acc) ->
	Name = {"name", "gmtSessionStartDateTime"},
	Desc = {"description", "The universal GMT date and time the session started with the Service Consumer’s perceived time zone. See ISO 8601."},
	Conf = {"configurable", true},
	Typ = {"valueType", "DateTime"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan32(NewAcc).
%% @hidden
spec_public_wlan32(Acc) ->
	Name = {"name", "gmtSessionEndDateTime"},
	Desc = {"description", "The universal GMT date and time the session ended with the Service Consumer’s perceived time zone. See ISO 8601."},
	Conf = {"configurable", true},
	Typ = {"valueType", "DateTime"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan33(NewAcc).
%% @hidden
spec_public_wlan33(Acc) ->
	Name = {"name", "sessionTerminateCause"},
	Desc = {"description", "RFC 3580 specifies, RFC 2866 enumerates"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 7},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan34(NewAcc).
%% @hidden
spec_public_wlan34(Acc) ->
	Name = {"name", "billingClassOfService"},
	Desc = {"description", "Indicates Service Type. Service level provided to user derived from Max-bandwidth-level. (Added for compatibility with WISPr)"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan35(NewAcc).
%% @hidden
spec_public_wlan35(Acc) ->
	Name = {"name", "unitOfMeasure"},
	Desc = {"description", "Indicates what is being represented in chargeable units field. The 'Quantity' enum item may be applicable for settlement for Partner content purchase."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 7},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan36(NewAcc).
%% @hidden
spec_public_wlan36(Acc) ->
	Name = {"name", "chargeableUnit"},
	Desc = {"description", "Indicates what activity the Chargeable_Quantity and Unit of Measure are metering."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 7},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan37(NewAcc).
%% @hidden
spec_public_wlan37(Acc) ->
	Name = {"name", "chargeableQuantity"},
	Desc = {"description", "Volume of chargeable_unit charged for this record."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 0},
	Value1 = {struct, [Typ, From]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan38(NewAcc).
%% @hidden
spec_public_wlan38(Acc) ->
	Name = {"name", "chargeAmount"},
	Desc = {"description", "Amount of the charge, not including taxes."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 0},
	Value1 = {struct, [Typ, From]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan39(NewAcc).
%% @hidden
spec_public_wlan39(Acc) ->
	Name = {"name", "chargeCurrencyType"},
	Desc = {"description", "Standard currency abbreviation from ISO 4217."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan40(NewAcc).
%% @hidden
spec_public_wlan40(Acc) ->
	Name = {"name", "otherParty"},
	Desc = {"description", "Identifies content or other party involved in transaction, if applicable. The party is associated with the charge since types of charges may have involved different parties. For example, the charge for network access is applied to access provider while charge for content applies to content provider."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan41(NewAcc).
%% @hidden
spec_public_wlan41(Acc) ->
	Name = {"name", "taxPercentage"},
	Desc = {"description", "The tax % applied to the charge. If blank, then the tax amount was a percentage or fixed value applied."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From1 = {"valueFrom", 0},
	To1 = {"valueTo", 100},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan42(NewAcc).
%% @hidden
spec_public_wlan42(Acc) ->
	Name = {"name", "taxAmount"},
	Desc = {"description", "The amount of tax. The charge amount does not include tax."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 0},
	Value1 = {struct, [Typ, From]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan43(NewAcc).
%% @hidden
spec_public_wlan43(Acc) ->
	Name = {"name", "taxType"},
	Desc = {"description", "Type of tax applied."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 14},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan44(NewAcc).
%% @hidden
spec_public_wlan44(Acc) ->
	Name = {"name", "intermediaryName"},
	Desc = {"description", "Represents a human-readable PWLAN intermediary name string. Could be a reseller, aggregator, clearinghouse, etc."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan45(NewAcc).
%% @hidden
spec_public_wlan45(Acc) ->
	Name = {"name", "serviceName"},
	Desc = {"description", "Specifies the service type used. VoIP, Basic Access, Purchased Content, etc. Mention in remark that it’s not the RADIUS service type."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From = {"valueFrom", 1},
	To = {"valueTo", 6},
	Value1 = {struct, [Typ, From, To]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan46(NewAcc).
%% @hidden
spec_public_wlan46(Acc) ->
	Name = {"name", "relatedIpdrIdList"},
	Desc = {"description", "Used to link together multiple related IPDRs when usage scenario and business rules demand so. Can’t change parent IPDR for audit/revenue assurance integrity."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_public_wlan47(NewAcc).
%% @hidden
spec_public_wlan47(Acc) ->
	Name = {"name", "tempUserId"},
	Desc = {"description", "Temporary user identification allocated by home SP. This is an ID assigned by the Access Point."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	[{struct, [Name, Desc, Conf, Value]} | Acc].

%% @hidden
spec_protocol() ->
	Name = {"name", "protocol"},
	Desc = {"description", "AAA protocol used in request."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", true},
	Val1 = {"value", "RADIUS"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "DIAMETER"},
	Value2 = {struct, [Typ, Def, Val2]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_node() ->
	Name = {"name", "node"},
	Desc = {"description", "Clusternode on which event was processed by OCS."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_server_address() ->
	Name = {"name", "serverAddress"},
	Desc = {"description", "IP address request was received on."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_server_port() ->
	Name = {"name", "serverPort"},
	Desc = {"description", "IP port request was received on."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_client_address() ->
	Name = {"name", "clientAddress"},
	Desc = {"description", "IP address request was received from."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_client_port() ->
	Name = {"name", "clientPort"},
	Desc = {"description", "IP port request was received from."},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_type_access() ->
	Name = {"name", "type"},
	Desc = {"description", "Type of access event."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", false},
	Val1 = {"value", "accept"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "reject"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "change"},
	Value3 = {struct, [Typ, Def, Val3]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2, Value3]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_type_accounting() ->
	Name = {"name", "type"},
	Desc = {"description", "Type of accounting event."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", false},
	Val1 = {"value", "on"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "off"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "start"},
	Value3 = {struct, [Typ, Def, Val3]},
	Val4 = {"value", "interim"},
	Value4 = {struct, [Typ, Def, Val4]},
	Val5 = {"value", "event"},
	Value5 = {struct, [Typ, Def, Val5]},
	Value = {"usageSpecCharacteristicValue",
			{array, [Value1, Value2, Value3, Value4, Value5]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_username() ->
	Name = {"name", "username"},
	Desc = {"description", "Username/identity of subscriber."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_ip() ->
	Name = {"name", "nasIpAddress"},
	Desc = {"description", "NAS-IP-Address attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	From1 = {"valueFrom", "0.0.0.0"},
	To1 = {"valueTo", "255.255.255.255"},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port() ->
	Name = {"name", "nasPort"},
	Desc = {"description", "NAS-Port attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_service_type() ->
	Name = {"name", "serviceType"},
	Desc = {"description", "Service-Type attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", true},
	Val1 = {"value", "framed"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "administrative"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "authenticate-only"},
	Value3 = {struct, [Typ, Def, Val3]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2, Value3]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_address() ->
	Name = {"name", "framedIpAddress"},
	Desc = {"description", "Framed-IP-Address attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	From1 = {"valueFrom", "0.0.0.0"},
	To1 = {"valueTo", "255.255.255.255"},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_pool() ->
	Name = {"name", "framedPool"},
	Desc = {"description", "Framed-Pool attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_netmask() ->
	Name = {"name", "framedIpNetmask"},
	Desc = {"description", "Framed-IP-Netmask attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	From1 = {"valueFrom", "0.0.0.0"},
	To1 = {"valueTo", "255.255.255.255"},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_routing() ->
	Name = {"name", "framedRouting"},
	Desc = {"description", "Framed-Routing attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", true},
	Val1 = {"value", "none"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "send-routing-packets"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "listen-for-routing-packets"},
	Value3 = {struct, [Typ, Def, Val3]},
	Val4 = {"value", "send-and-listen"},
	Value4 = {struct, [Typ, Def, Val4]},
	Value = {"usageSpecCharacteristicValue",
			{array, [Value1, Value2, Value3, Value4]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_filter_id() ->
	Name = {"name", "filterId"},
	Desc = {"description", "Filter-Id attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_mtu() ->
	Name = {"name", "framedMtu"},
	Desc = {"description", "Framed-MTU attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From1 = {"valueFrom", 64},
	To1 = {"valueTo", 65535},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_framed_route() ->
	Name = {"name", "framedRoute"},
	Desc = {"description", "Framed-Route attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_class() ->
	Name = {"name", "class"},
	Desc = {"description", "Class attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_session_timeout() ->
	Name = {"name", "sessionTimeout"},
	Desc = {"description", "Session-Timeout attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From1 = {"valueFrom", 0},
	To1 = {"valueTo", 4294967295},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_idle_timeout() ->
	Name = {"name", "idleTimeout"},
	Desc = {"description", "Idle-Timeout attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	From1 = {"valueFrom", 0},
	To1 = {"valueTo", 4294967295},
	Value1 = {struct, [Typ, From1, To1]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_termination_action() ->
	Name = {"name", "terminationAction"},
	Desc = {"description", "Termination-Action attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", false},
	Val1 = {"value", "default"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "aaa-request"},
	Value2 = {struct, [Typ, Def, Val2]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_called_id() ->
	Name = {"name", "calledStationId"},
	Desc = {"description", "Called-Station-Id attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_calling_id() ->
	Name = {"name", "callingStationId"},
	Desc = {"description", "Calling-Station-Id attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_id() ->
	Name = {"name", "nasIdentifier"},
	Desc = {"description", "NAS-Identifier attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port_id() ->
	Name = {"name", "nasPortId"},
	Desc = {"description", "NAS-Port-Id attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_nas_port_type() ->
	Name = {"name", "nasPortType"},
	Desc = {"description", "NAS-Port-Type attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", false},
	Val1 = {"value", "virtual"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "sdsl"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "adsl-cap"},
	Value3 = {struct, [Typ, Def, Val3]},
	Val4 = {"value", "adsl-dmt"},
	Value4 = {struct, [Typ, Def, Val4]},
	Val5 = {"value", "ethernet"},
	Value5 = {struct, [Typ, Def, Val5]},
	Val6 = {"value", "xdsl"},
	Value6 = {struct, [Typ, Def, Val6]},
	Val7 = {"value", "cable"},
	Value7 = {struct, [Typ, Def, Val7]},
	Val8 = {"value", "wireless-other"},
	Value8 = {struct, [Typ, Def, Val8]},
	Val9 = {"value", "wireless-ieee802.11"},
	Value9 = {struct, [Typ, Def, Val9]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2,
			Value3, Value4, Value5, Value6, Value7, Value8, Value9]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_port_limit() ->
	Name = {"name", "portLimit"},
	Desc = {"description", "Port-Limit attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_delay() ->
	Name = {"name", "acctDelayTime"},
	Desc = {"description", "Acct-Delay-Time attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_event_timestamp() ->
	Name = {"name", "eventTimestamp"},
	Desc = {"description", "Event-Timestamp attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "DateTime"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_session_id() ->
	Name = {"name", "acctSessionId"},
	Desc = {"description", "Acct-Session-Id attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_multisession_id() ->
	Name = {"name", "acctMultiSessionId"},
	Desc = {"description", "Acct-Multi-Session-Id attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_link_count() ->
	Name = {"name", "acctLinkCount"},
	Desc = {"description", "Acct-Link-Count attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_authentic() ->
	Name = {"name", "acctAuthentic"},
	Desc = {"description", "Acct-Authentic attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", false},
	Val1 = {"value", "RADIUS"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "local"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "remote"},
	Value3 = {struct, [Typ, Def, Val3]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2, Value3]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_session_time() ->
	Name = {"name", "acctSessionTime"},
	Desc = {"description", "Acct-Session-Time attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_input_octets() ->
	Name = {"name", "inputOctets"},
	Desc = {"description", "Acct-Input-Octets attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_output_octets() ->
	Name = {"name", "outputOctets"},
	Desc = {"description", "Acct-Output-Octets attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_input_giga_words() ->
	Name = {"name", "acctInputGigawords"},
	Desc = {"description", "Acct-Input-Gigawords attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_output_giga_words() ->
	Name = {"name", "acctOutputGigawords"},
	Desc = {"description", "Acct-Output-Gigawords attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_input_packets() ->
	Name = {"name", "acctInputPackets"},
	Desc = {"description", "Acct-Input-Packets attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_output_packets() ->
	Name = {"name", "acctOutputPackets"},
	Desc = {"description", "Acct-Output-Packets attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_data_rate() ->
	Name = {"name", "ascendDataRate"},
	Desc = {"description", "Ascend-Data-Rate attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_xmit_rate() ->
	Name = {"name", "ascendXmitRate"},
	Desc = {"description", "Ascend-Xmit-Rate attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_interim_interval() ->
	Name = {"name", "acctInterimInterval"},
	Desc = {"description", "Acct-Interim-Interval  attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_cause() ->
	Name = {"name", "acctTerminateCause"},
	Desc = {"description", "Acct-Terminate-Cause attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", false},
	Val1 = {"value", "user-request"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "lost-carrier"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "lost-service"},
	Value3 = {struct, [Typ, Def, Val3]},
	Val4 = {"value", "idle-timeout"},
	Value4 = {struct, [Typ, Def, Val4]},
	Val5 = {"value", "session-timeout"},
	Value5 = {struct, [Typ, Def, Val5]},
	Val6 = {"value", "admin-reset"},
	Value6 = {struct, [Typ, Def, Val6]},
	Val7 = {"value", "admin-reboot"},
	Value7 = {struct, [Typ, Def, Val7]},
	Val8 = {"value", "port-error"},
	Value8 = {struct, [Typ, Def, Val8]},
	Val9 = {"value", "NAS-error"},
	Value9 = {struct, [Typ, Def, Val9]},
	Val10 = {"value", "NAS-request"},
	Value10 = {struct, [Typ, Def, Val10]},
	Val11 = {"value", "NAS-reboot"},
	Value11 = {struct, [Typ, Def, Val11]},
	Val12 = {"value", "port-uneeded"},
	Value12 = {struct, [Typ, Def, Val12]},
	Val13 = {"value", "port-preempted"},
	Value13 = {struct, [Typ, Def, Val13]},
	Val14 = {"value", "port-suspended"},
	Value14 = {struct, [Typ, Def, Val14]},
	Val15 = {"value", "service-unavailable"},
	Value15 = {struct, [Typ, Def, Val15]},
	Val16 = {"value", "callback"},
	Value16 = {struct, [Typ, Def, Val16]},
	Val17 = {"value", "user-error"},
	Value17 = {struct, [Typ, Def, Val17]},
	Val18 = {"value", "host-request"},
	Value18 = {struct, [Typ, Def, Val18]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2, Value3,
			Value4, Value5, Value6, Value7, Value8, Value9, Value10, Value11,
			Value12, Value13, Value14, Value15, Value16, Value17, Value18]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
usage_characteristics(Attributes) ->
	lists:reverse(char_attr_username(Attributes, [])).

%% @hidden
char_attr_username(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?UserName, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "username"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_ip(Attributes, NewAcc).

%% @hidden
char_attr_nas_ip(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?NasIpAddress, Attributes) of
		{ok, Value} ->
			Address = inet:ntoa(Value),
			[{struct, [{"name", "nasIpAddress"}, {"value", Address}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_port(Attributes, NewAcc).

%% @hidden
char_attr_nas_port(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?NasPort, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasPort"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_service_type(Attributes, NewAcc).

%% @hidden
char_attr_service_type(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?ServiceType, Attributes) of
		{ok, Value} ->
			Type = case Value of
				2 ->
					"framed";
				6 ->
					"administrative";
				8 ->
					"authenticate-only";
				N ->
					N
			end,
			[{struct, [{"name", "serviceType"}, {"value", Type}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_address(Attributes, NewAcc).

%% @hidden
char_attr_framed_address(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FramedIpAddress, Attributes) of
		{ok, Value} ->
			Address = inet:ntoa(Value),
			[{struct, [{"name", "framedIpAddress"}, {"value", Address}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_pool(Attributes, NewAcc).

%% @hidden
char_attr_framed_pool(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FramedPool, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "framedPool"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_netmask(Attributes, NewAcc).

%% @hidden
char_attr_framed_netmask(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FramedIpNetmask, Attributes) of
		{ok, Value} ->
			Netmask = inet:ntoa(Value),
			[{struct, [{"name", "framedIpNetmask"}, {"value", Netmask}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_routing(Attributes, NewAcc).

%% @hidden
char_attr_framed_routing(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FramedRouting, Attributes) of
		{ok, Value} ->
			Routing = case Value of
				0 ->
					"none";
				1 ->
					"send-routing-packets";
				2 ->
					"listen-for-routing-packets";
				3 ->
					"send-and-listen";
				N ->
					N
			end,
			[{struct, [{"name", "framedIpNetmask"}, {"value", Routing}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_filter_id(Attributes, NewAcc).

%% @hidden
char_attr_filter_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FilterId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "filterId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_mtu(Attributes, NewAcc).

%% @hidden
char_attr_framed_mtu(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FramedMtu, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "framedMtu"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_route(Attributes, NewAcc).

%% @hidden
char_attr_framed_route(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?FramedRoute, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "framedRoute"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_class(Attributes, NewAcc).

%% @hidden
char_attr_class(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?Class, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "class"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_session_timeout(Attributes, NewAcc).

%% @hidden
char_attr_session_timeout(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?SessionTimeout, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "sessionTimeout"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_idle_timeout(Attributes, NewAcc).

%% @hidden
char_attr_idle_timeout(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?IdleTimeout, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "idleTimeout"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_termination_action(Attributes, NewAcc).

%% @hidden
char_attr_termination_action(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?TerminationAction, Attributes) of
		{ok, Value} ->
			Action = case Value of
				0 ->
					"default";
				1 ->
					"aaa-request";
				N ->
					N
			end,
			[{struct, [{"name", "terminationAction"}, {"value", Action}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_called_id(Attributes, NewAcc).

%% @hidden
char_attr_called_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?CalledStationId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "calledStationId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_calling_id(Attributes, NewAcc).

%% @hidden
char_attr_calling_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?CallingStationId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "callingStationId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_id(Attributes, NewAcc).

%% @hidden
char_attr_nas_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?NasIdentifier, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasIdentifier"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_port_id(Attributes, NewAcc).

%% @hidden
char_attr_port_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?NasPortId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasPortId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_port_type(Attributes, NewAcc).

%% @hidden
char_attr_nas_port_type(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?NasPortType, Attributes) of
		{ok, Value} ->
			Type = case Value of
				5 ->
					"virtual";
				11 ->
					"sdsl";
				12 ->
					"adsl-cap";
				13 ->
					"adsl-dmt";
				16 ->
					"xdsl";
				17 ->
					"cable";
				18 ->
					"wireless-other";
				19 ->
					"wireless-ieee802.11";
				N ->
					N
			end,
			[{struct, [{"name", "nasPortType"}, {"value", Type}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_port_limit(Attributes, NewAcc).

%% @hidden
char_attr_port_limit(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?PortLimit, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "portLimit"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_delay(Attributes, NewAcc).

%% @hidden
char_attr_delay(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctDelayTime, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctDelayTime"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_event_timestamp(Attributes, NewAcc).

%% @hidden
char_attr_event_timestamp(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?EventTimestamp, Attributes) of
		{ok, Value} ->
			DateTime = ocs_log:iso8601(Value * 1000),
			[{struct, [{"name", "eventTimestamp"}, {"value", DateTime}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_session_id(Attributes, NewAcc).

%% @hidden
char_attr_session_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctSessionId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctSessionId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_multisession_id(Attributes, NewAcc).

%% @hidden
char_attr_multisession_id(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctMultiSessionId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctMultiSessionId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_link_count(Attributes, NewAcc).

%% @hidden
char_attr_link_count(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctLinkCount, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctLinkCount"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_authentic(Attributes, NewAcc).

%% @hidden
char_attr_authentic(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctAuthentic, Attributes) of
		{ok, Value} ->
			Type = case Value of
				1 ->
					"RADIUS";
				2 ->
					"local";
				3 ->
					"remote";
				N ->
					N
			end,
			[{struct, [{"name", "acctAuthentic"}, {"value", Type}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_session_time(Attributes, NewAcc).

%% @hidden
char_attr_session_time(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctSessionTime, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctSessionTime"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_input_octets(Attributes, NewAcc).

%% @hidden
char_attr_input_octets(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctInputOctets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "inputOctets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_output_octets(Attributes, NewAcc).

%% @hidden
char_attr_output_octets(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctOutputOctets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "outputOctets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_input_giga_words(Attributes, NewAcc).

%% @hidden
char_attr_input_giga_words(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctInputGigawords, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctInputGigawords"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_output_giga_words(Attributes, NewAcc).

%% @hidden
char_attr_output_giga_words(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctOutputGigawords, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctOutputGigawords"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_input_packets(Attributes, NewAcc).

%% @hidden
char_attr_input_packets(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctInputPackets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctInputPackets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_output_packets(Attributes, NewAcc).

%% @hidden
char_attr_output_packets(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctOutputPackets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctOutputPackets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_data_rate(Attributes, NewAcc).

%% @hidden
char_attr_data_rate(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AscendDataRate, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "ascendDataRate"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_xmit_rate(Attributes, NewAcc).

%% @hidden
char_attr_xmit_rate(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AscendXmitRate, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "ascendXmitRate"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_interim_interval(Attributes, NewAcc).

%% @hidden
char_attr_interim_interval(Attributes, Acc) ->
	NewAcc = case radius_attributes:find(?AcctInterimInterval, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctInterimInterval"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_cause(Attributes, NewAcc).

%% @hidden
char_attr_cause(Attributes, Acc) ->
	case radius_attributes:find(?AcctTerminateCause, Attributes) of
		{ok, Value} ->
			Cause = case Value of
				1 ->
					"user-request";
				2 ->
					"lost-carrier";
				3 ->
					"lost-service";
				4 ->
					"idle-timeout";
				5 ->
					"session-timeout";
				6 ->
					"admin-reset";
				7 ->
					"admin-reboot";
				8 ->
					"port-error";
				9 ->
					"NAS-error";
				10 ->
					"NAS-request";
				11 ->
					"NAS-reboot";
				12 ->
					"port-uneeded";
				13 ->
					"port-preempted";
				14 ->
					"port-suspended";
				15 ->
					"service-unavailable";
				16 ->
					"callback";
				17 ->
					"user-error";
				18 ->
					"host-request";
				N ->
					N
			end,
			[{struct, [{"name", "acctTerminateCause"}, {"value", Cause}]} | Acc];
		{error, not_found} ->
			Acc
	end.

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	{DateStart, DateEnd} = case lists:keyfind("date", 1, Query) of
		{_, DateTime} when length(DateTime) > 3 ->
			range(DateTime);
		false ->
			{1, erlang:system_time(?MILLISECOND)}
	end,
	query_start1(lists:keyfind("type", 1, Query), Query,
			Filters, RangeStart, RangeEnd, DateStart, DateEnd).
%% @hidden
query_start1({_, "AAAAccessUsage"}, Query,
		Filters, RangeStart, RangeEnd, DateStart, DateEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex,
							[{"usageCharacteristic", contains, Contains}]}]}]} ->
						characteristic(Contains, '_', '_', '_', '_')
				end;
			false ->
				{'_', '_', '_', '_'}
		end
	of
		{Protocol, Types, ReqAttrs, RespAttrs} ->
			Args = [DateStart, DateEnd, Protocol, Types, ReqAttrs, RespAttrs],
			MFA = [ocs_log, auth_query, Args],
			case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_ ->
			{error, 400}
	end;
query_start1({_, "AAAAccountingUsage"}, Query,
		Filters, RangeStart, RangeEnd, DateStart, DateEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex,
							[{"usageCharacteristic", contains, Contains}]}]}]} ->
						characteristic(Contains, '_', '_', '_', '_')
				end;
			false ->
				{'_', '_', '_', '_'}
		end
	of
		{Protocol, Types, Attributes, _} ->
			Args = [DateStart, DateEnd, Protocol, Types, Attributes],
			MFA = [ocs_log, acct_query, Args],
			case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_ ->
			{error, 400}
	end;
query_start1({_, "PublicWLANAccessUsage"}, _, _, _, _, _, _) ->
	{error, 404}; % todo?
query_start1({_, "HTTPTransferUsage"}, Query,
		Filters, RangeStart, RangeEnd, _, _) ->
	DateTime = proplists:get_value("datetime", Query, '_'),
	Host = proplists:get_value("host", Query, '_'),
	User = proplists:get_value("user", Query, '_'),
	URI = proplists:get_value("uri", Query, '_'),
	Method = proplists:get_value("method", Query, '_'),
	HTTPStatus = proplists:get_value("httpStatus", Query, '_'),
	case supervisor:start_child(ocs_rest_pagination_sup,
			[[ocs_log, http_query,
			[transfer, DateTime, Host, User, Method, URI, HTTPStatus]]]) of
		{ok, PageServer, Etag} ->
			query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end;
query_start1({_, _}, [], _, _, _, _, _) ->
	{error, 404};
query_start1({_, _}, _, _, _, _, _, _) ->
	{error, 400};
query_start1(false, _, _, _, _, _, _) ->
	{error, 400}.

%% @hidden
query_page(PageServer, Etag, Query, Filters, Start, End) ->
	case lists:keytake("type", 1, Query) of
		{_, {_, "AAAAccessUsage"}, _Query1} ->
			query_page1(PageServer, Etag, fun usage_aaa_auth/2, Filters, Start, End);
		{_, {_, "AAAAccountingUsage"}, _Query1} ->
			query_page1(PageServer, Etag, fun usage_aaa_acct/2, Filters, Start, End);
		{_, {_, "PublicWLANAccessUsage"}, []} ->
			{error, 404}; % todo?
		{_, {_, "HTTPTransferUsage"}, _} ->
			query_page1(PageServer, Etag, fun usage_http_transfer/2, Filters, Start, End);
		{_, {_, _}, []} ->
			{error, 404};
		{_, {_, _}, _} ->
			{error, 400};
		false ->
			{error, 400}
	end.
%% @hidden
query_page1(PageServer, Etag, Decoder, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try Decoder(Events, Filters) of
				JsonObj ->
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end.

%% @hidden
get_last(Query, Filters) ->
	case lists:keytake("type", 1, Query) of
		{_, {_, "AAAAccessUsage"}, []} ->
			get_last1(ocs_auth, fun usage_aaa_auth/2, Filters);
		{_, {_, "AAAAccountingUsage"}, []} ->
			get_last1(ocs_acct, fun usage_aaa_acct/2, Filters);
		{_, {_, "PublicWLANAccessUsage"}, []} ->
			{error, 404}; % todo?
		{_, {_, "HTPPUsage"}, []} ->
			get_last1(ocs_acct, fun usage_http_transfer/2, Filters);
		{_, {_, _}, []} ->
			{error, 404};
		false ->
			{error, 400}
	end.
%% @hidden
get_last1(Log, Decoder, Filters) ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	case ocs_log:last(Log, MaxItems) of
		{error, _} ->
			{error, 500};
		{0, []} ->
			{error, 404};
		{NewCount, Events} ->
			try Decoder(Events, Filters) of
				JsonObj ->
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					ContentRange = "items 1-"
							++ integer_to_list(NewCount) ++ "/*",
					Headers = [{content_type, "application/json"},
							{content_range, ContentRange}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end.

%% @hidden
range([Y1, Y2, Y3, Y4 | T] = DateTime)
		when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
		Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	{ocs_log:iso8601(DateTime), range([Y1, Y2, Y3, Y4], T)}.
%% @hidden
range(Year, []) ->
	EndYear = list_to_integer(Year) + 1,
	End = lists:flatten(io_lib:fwrite("~4.10.0b", [EndYear])),
	ocs_log:iso8601(End) - 1;
range(Year, "-") ->
	range(Year, []);
range(Year, "-0") ->
	ocs_log:iso8601(Year ++ "-10-01") - 1;
range(Year, "-1") ->
	EndYear = list_to_integer(Year) + 1,
	End = lists:flatten(io_lib:fwrite("~4.10.0b", [EndYear])),
	ocs_log:iso8601(End) - 1;
range(Year, [$-, $0, N]) when N >= $1, N =< $8 ->
	ocs_log:iso8601(Year ++ [$-, $0, N + 1]) - 1;
range(Year, "-09") ->
	ocs_log:iso8601(Year ++ "-10") - 1;
range(Year, "-10") ->
	ocs_log:iso8601(Year ++ "-11") - 1;
range(Year, "-11") ->
	ocs_log:iso8601(Year ++ "-12") - 1;
range(Year, "-12") ->
	EndYear = list_to_integer(Year) + 1,
	End = lists:flatten(io_lib:fwrite("~4.10.0b", [EndYear])),
	ocs_log:iso8601(End) - 1;
range(Year, [$-, M1, M2, $-]) ->
	range(Year, [$-, M1, M2]);
range(Year, [$-, M1, M2, $-, $0]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $1, $0]) - 1;
range(Year, [$-, M1, M2, $-, $1]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $2, $0]) - 1;
range(Year, "-02-2") ->
	ocs_log:iso8601(Year ++ "-03") - 1;
range(Year, [$-, M1, M2, $-, $2]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $3, $0]) - 1;
range(Year, "-12-3") ->
	EndYear = list_to_integer(Year) + 1,
	End = lists:flatten(io_lib:fwrite("~4.10.0b", [EndYear])),
	ocs_log:iso8601(End) - 1;
range(Year, [$-, M1, M2, $-, $3]) ->
	Month = list_to_integer([M1, M2]) + 1,
	End = lists:flatten(io_lib:fwrite("-~2.10.0b", [Month])),
	ocs_log:iso8601(Year ++ End) - 1;
range(Year, "-02-29") ->
	ocs_log:iso8601(Year ++ "-03-01") - 1;
range(Year, "-02-28") ->
	case calendar:last_day_of_the_month(list_to_integer(Year), 2) of
		28 ->
			ocs_log:iso8601(Year ++ "-03-01") - 1;
		29 ->
			ocs_log:iso8601(Year ++ "-02-29") - 1
	end;
range(Year, [$-, M1, M2, $-, $0, $9]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $1, $0]) - 1;
range(Year, [$-, M1, M2, $-, $0, D2]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $0, D2 + 1]) - 1;
range(Year, [$-, M1, M2, $-, $1, $9]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $2, $0]) - 1;
range(Year, [$-, M1, M2, $-, $1, D2]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $1, D2 + 1]) - 1;
range(Year, [$-, M1, M2, $-, $2, $9]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $3, $0]) - 1;
range(Year, [$-, $0, $4, $-, $3, $0]) ->
	ocs_log:iso8601(Year ++ [$-, $0, $5, $-, $0, $1]) - 1;
range(Year, [$-, $0, $6, $-, $3, $0]) ->
	ocs_log:iso8601(Year ++ [$-, $0, $7, $-, $0, $1]) - 1;
range(Year, [$-, $0, $9, $-, $3, $0]) ->
	ocs_log:iso8601(Year ++ [$-, $1, $0, $-, $0, $1]) - 1;
range(Year, [$-, $1, $1, $-, $3, $0]) ->
	ocs_log:iso8601(Year ++ [$-, $1, $2, $-, $0, $1]) - 1;
range(Year, [$-, $1, $2, $-, $3, $1]) ->
	EndYear = list_to_integer(Year) + 1,
	End = lists:flatten(io_lib:fwrite("~4.10.0b", [EndYear])),
	ocs_log:iso8601(End) - 1;
range(Year, [$-, M1, M2, $-, $3, $0]) ->
	ocs_log:iso8601(Year ++ [$-, M1, M2, $-, $3, $1]) - 1;
range(Year, [$-, M1, M2, $-, D1, D2, $T]) ->
	range(Year, [$-, M1, M2, $-, D1, D2]);
range(Year, [$-, M1, M2, $-, D1, D2, $T | T]) ->
	range(Year, [$-, M1, M2, $-, D1, D2, $T], T).
%% @hidden
range(Year, Day, [$0]) ->
	ocs_log:iso8601(Year ++ Day ++ "10") - 1;
range(Year, Day, [$1]) ->
	ocs_log:iso8601(Year ++ Day ++ "20") - 1;
range(Year, Day, [$2]) ->
	ocs_log:iso8601(Year ++ Day ++ "24") - 1;
range(Year, Day, "09") ->
	ocs_log:iso8601(Year ++ Day ++ "10") - 1;
range(Year, Day, [$0, N]) ->
	ocs_log:iso8601(Year ++ Day ++ [$0, N + 1]) - 1;
range(Year, Day, "19") ->
	ocs_log:iso8601(Year ++ Day ++ "20") - 1;
range(Year, Day, [$1, N]) ->
	ocs_log:iso8601(Year ++ Day ++ [$1, N + 1]) - 1;
range(Year, Day, [$2, N]) when N >= $0, N =< $3 ->
	ocs_log:iso8601(Year ++ Day ++ [$1, N + 1]) - 1;
range(Year, Day, [H1, H2, $:]) ->
	range(Year, Day, [H1, H2]);
range(Year, Day, [H1, H2, $:, $5]) ->
	Hour = list_to_integer([H1, H2]) + 1,
	End = lists:flatten(io_lib:fwrite("~2.10.0b", [Hour])),
	ocs_log:iso8601(Year ++ Day ++ End) - 1;
range(Year, Day, [H1, H2, $:, M]) ->
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:, M + 1]) - 1;
range(Year, Day, [H1, H2, $:, $5, $9]) ->
	Hour = list_to_integer([H1, H2]) + 1,
	End = lists:flatten(io_lib:fwrite("~2.10.0b", [Hour])),
	ocs_log:iso8601(Year ++ Day ++ End) - 1;
range(Year, Day, [H1, H2, $:, M, $9]) ->
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:, M + 1, $0]) - 1;
range(Year, Day, [H1, H2, $:, M1, M2]) ->
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:, M1, M2 + 1]) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:]) ->
	range(Year, Day, [H1, H2, $:, M1, M2]);
range(Year, Day, [H1, H2, $:, $5, $9, $:, $5]) ->
	Hour = list_to_integer([H1, H2]) + 1,
	End = lists:flatten(io_lib:fwrite("~2.10.0b", [Hour])),
	ocs_log:iso8601(Year ++ Day ++ End) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:, $5]) ->
	Minute = list_to_integer([M1, M2]) + 1,
	End = lists:flatten(io_lib:fwrite("~2.10.0b", [Minute])),
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:] ++ End) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:, N]) ->
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:, M1, M2, $:, N + 1]) - 1;
range(Year, Day, [H1, H2, $:, $5, $9, $:, $5, $9]) ->
	Hour = list_to_integer([H1, H2]) + 1,
	End = lists:flatten(io_lib:fwrite("~2.10.0b", [Hour])),
	ocs_log:iso8601(Year ++ Day ++ End) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:, $5, $9]) ->
	Minute = list_to_integer([M1, M2]) + 1,
	End = lists:flatten(io_lib:fwrite("~2.10.0b", [Minute])),
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:] ++ End) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:, S1, $9]) ->
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:, M1, M2, $:, S1 + 1, $0]) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:, S1, S2]) ->
	ocs_log:iso8601(Year ++ Day ++ [H1, H2, $:, M1, M2, $:, S1, S2 + 1]) - 1;
range(Year, Day, [H1, H2, $:, M1, M2, $:, S1, S2, $.]) ->
	range(Year, Day, [H1, H2, $:, M1, M2, $:, S1, S2]);
range(Year, Day, [_, _, $:, _, _, $:, _, _, $., _] = S) ->
	ocs_log:iso8601(Year ++ Day ++ S ++ "99");
range(Year, Day, [_, _, $:, _, _, $:, _, _, $., _, _] = S) ->
	ocs_log:iso8601(Year ++ Day ++ S ++ "9");
range(Year, Day, [_, _, $:, _, _, $:, _, _, $., _, _ | _] = S) ->
	ocs_log:iso8601(Year ++ Day ++ S).

%% @hidden
characteristic([{complex, L1} | T], Protocol, Types, ReqAttrs, RespAttrs) ->
	case lists:keytake("name", 1, L1) of
		{_, Name, L2} ->
			case lists:keytake("value", 1, L2) of
				{_, Value, []} ->
					characteristic(Name, Value, T,
							Protocol, Types, ReqAttrs, RespAttrs);
				_ ->
					throw({error, 400})
			end;
		false ->
			throw({error, 400})
	end;
characteristic([], Protocol, Types, ReqAttrs, RespAttrs) ->
	{Protocol, rev(Types), rev(ReqAttrs), rev(RespAttrs)}.
%% @hidden
characteristic({"name", exact, "username"}, {"value", exact, UserName},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(UserName) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?UserName, {exact, UserName}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "username"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?UserName, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasIpAddress"}, {"value", exact, NasIp},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(NasIp) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasIpAddress, {exact, NasIp}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasIpAddress"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasIpAddress, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasPort"}, {"value", Op, NasPort},
		T, Protocol, Types, ReqAttrs1, RespAttrs)
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasPort, {Op, list_to_integer(NasPort)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "serviceType"}, {"value", Op, ServiceType},
		T, Protocol, Types, ReqAttrs1, RespAttrs)
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?ServiceType, {Op, list_to_integer(ServiceType)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedIpAddress"}, {"value", exact, FramedIp},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(FramedIp) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedIpAddress, {exact, FramedIp}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedIpAddress"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedIpAddress, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedPool"}, {"value", exact, FramedPool},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(FramedPool) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedPool, {exact, FramedPool}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedPool"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedPool, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedIpNetmask"}, {"value", exact, FramedIpNet},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(FramedIpNet) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedIpNetmask, {exact, FramedIpNet}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedIpNetmask"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedIpNetmask, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedRouting"}, {"value", exact, FramedRouting},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(FramedRouting) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedRouting, {exact, FramedRouting}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedRouting"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedRouting, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "filterId"}, {"value", exact, FilterId},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(FilterId) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FilterId, {exact, FilterId}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "filterId"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FilterId, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedMtu"}, {"value", Op, FramedMtu},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedMtu, {Op, list_to_integer(FramedMtu)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedRoute"}, {"value", exact, FramedRoute},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(FramedRoute) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedRoute, {exact, FramedRoute}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "framedRoute"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?FramedRoute, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "class"}, {"value", exact, Class},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Class) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?Class, {exact, Class}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "class"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?Class, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "sessionTimeout"}, {"value", Op, SessionTimout},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?SessionTimeout, {Op, list_to_integer(SessionTimout)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "idleTimeout"}, {"value", Op, IdleTimeout},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?IdleTimeout, {Op, list_to_integer(IdleTimeout)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "terminationAction"}, {"value", exact, TerminationAction},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(TerminationAction) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?TerminationAction, {exact, TerminationAction}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "terminationAction"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?TerminationAction, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "calledStationId"}, {"value", exact, CalledStation},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(CalledStation) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?CalledStationId, {exact, CalledStation}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "calledStationId"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?CalledStationId, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "callingStationId"}, {"value", exact, CallingStation},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(CallingStation) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?CallingStationId, {exact, CallingStation}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "callingStationId"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?CallingStationId, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasIdentifier"}, {"value", exact, NasId},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(NasId) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasIdentifier, {exact, NasId}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasIdentifier"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasIdentifier, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasPortId"}, {"value", exact, NasPortId},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(NasPortId) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasPortId, {exact, NasPortId}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasPortId"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasPortId, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasPortType"}, {"value", exact, NasPortType},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(NasPortType) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasPortType, {exact, NasPortType}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "nasPortType"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?NasPortType, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "portLimit"}, {"value", Op, PortLimit},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?PortLimit, {Op, list_to_integer(PortLimit)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctDelayTime"}, {"value", Op, AcctDelayTime},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?PortLimit, {Op, list_to_integer(AcctDelayTime)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctSessionId"}, {"value", exact, AcctSessionId},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(AcctSessionId) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctSessionId, {exact, AcctSessionId}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctSessionId"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctSessionId, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctMultiSessionId"}, {"value", exact, AcctMultiSessionId},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(AcctMultiSessionId) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctMultiSessionId, {exact, AcctMultiSessionId}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctMultiSessionId"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctMultiSessionId, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctLinkCount"}, {"value", Op, AcctLinkCount},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctLinkCount, {Op, list_to_integer(AcctLinkCount)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctAuthentic"}, {"value", exact, AcctAuth},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(AcctAuth) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctAuthentic, {exact, AcctAuth}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctAuthentic"}, {"value", like, Like},
		T, Protocol, Types, ReqAttrs1, RespAttrs) when is_list(Like) ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctAuthentic, {like, like(Like)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctSessionTime"}, {"value", Op, AcctSessionTime},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctSessionTime, {Op, list_to_integer(AcctSessionTime)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctInputGigawords"}, {"value", Op, InputGiga},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctInputGigawords, {Op, list_to_integer(InputGiga)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctOutputGigawords"}, {"value", Op, OutputGiga},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctOutputGigawords, {Op, list_to_integer(OutputGiga)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctInputOctets"}, {"value", Op, InputOctets},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctInputPackets, {Op, list_to_integer(InputOctets)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctOutputOctets"}, {"value", Op, OutputOctets},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctOutputPackets, {Op, list_to_integer(OutputOctets)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "ascendDataRate"}, {"value", Op, AscendDataRate},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AscendDataRate, {Op, list_to_integer(AscendDataRate)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "ascendXmitRate"}, {"value", Op, AscendXmitRate},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AscendXmitRate, {Op, list_to_integer(AscendXmitRate)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctInterimInterval"}, {"value", Op, AcctInterimInterval},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctInterimInterval, {Op, list_to_integer(AcctInterimInterval)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs);
characteristic({"name", exact, "acctTerminateCause"}, {"value", Op, AcctTerminateCause},
		T, Protocol, Types, ReqAttrs1, RespAttrs) 
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	ReqAttrs2 = add_char(ReqAttrs1, {?AcctTerminateCause, {Op, list_to_integer(AcctTerminateCause)}}),
	characteristic(T, Protocol, Types, ReqAttrs2, RespAttrs).


%% @hidden
like([H | T]) ->
	like([H | T], []).
%% @hidden
like([H | T], Acc) ->
	case lists:last(H) of
		$% ->
			like(T, [lists:droplast(H) | Acc]);
		_ ->
			like(T, [H | Acc])
	end;
like([], Acc) ->
	lists:reverse(Acc).

%% @hidden
add_char('_', AttributeMatch) ->
	[AttributeMatch];
add_char(Attributes, AttributeMatch) when is_list(Attributes) ->
	[AttributeMatch | Attributes].

%% @hidden
rev('_') ->
	'_';
rev(Attributes) when is_list(Attributes) ->
	lists:reverse(Attributes).

