%%% ocs_rest_res_usage.erl
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
-module(ocs_rest_res_usage).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_usages/2, get_usages/3, get_usages/4, get_usage/3, get_ipdr/2,
		get_usagespec/1, get_usagespec/2]).
-export([usage_aaa_acct/2]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_3gpp_gx_application.hrl").

-define(usageSpecPath, "/usageManagement/v1/usageSpecification/").
-define(usagePath, "/usageManagement/v1/usage/").

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

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
	["application/json", "application/problem+json"].

-spec get_usages(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @equiv get_usages(voip, Query, Headers)
%% @hidden
get_usages(Query, Headers) ->
	get_usages(voip, Query, Headers).

-spec get_usages(Type, Query, Headers) -> Result
	when
		Type :: wlan | voip,
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @equiv get_usages(Type, undefined, Query, Headers)
%% @hidden
get_usages(Type, Query, Headers) ->
	get_usages(Type, undefined, Query, Headers).

-spec get_usages(Type, Id, Query, Headers) -> Result
	when
		Type :: wlan | voip,
		Id :: string() | undefined,
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v1/usage'
%% 	requests.
%% @hidden
get_usages(Type, Id, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_usages1(Type, Id, NewQuery, Filters, Headers);
		false ->
			get_usages1(Type, Id, Query, [], Headers)
	end.
%% @hidden
get_usages1(Type, Id, Query, Filters, Headers) ->
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
							query_start(Type, Id, Query, Filters, Start, End)
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
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case ocs_rest:range(Range) of
				{error, _Reason} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Type, Id, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(Type, Id, Query, Filters, undefined, undefined)
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
	end.

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
			spec_aaa_acct(), spec_public_wlan(),
			spec_voip()]}),
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
		{_, {_, "AAAPolicyUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_aaa_policy_usage_spec()]}),
			{ok, RespHeaders, Body};
		{_, {_, "AAAPolicyUsageSpec"}, _} ->
			{error, 400};
		{_, {_, "PublicWLANAccessUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_public_wlan()]}),
			{ok, RespHeaders, Body};
		{_, {_, "PublicWLANAccessUsageSpec"}, _} ->
			{error, 400};
		{_, {_, "VoIPUsageSpec"}, []} ->
			Body = mochijson:encode({array, [spec_voip()]}),
			{ok, RespHeaders, Body};
		{_, {_, "VoIPUsageSpec"}, _} ->
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
get_usagespec("AAAPolicyUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_aaa_policy_usage_spec()),
	{ok, RespHeaders, Body};
get_usagespec("AAAPolicyUsageSpec", _Query) ->
	{error, 400};
get_usagespec("PublicWLANAccessUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_public_wlan()),
	{ok, RespHeaders, Body};
get_usagespec("PublicWLANAccessUsageSpec", _Query) ->
	{error, 400};
get_usagespec("VoIPUsageSpec", []) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_voip()),
	{ok, RespHeaders, Body};
get_usagespec("VoIPUsageSpec", _Query) ->
	{error, 400};
get_usagespec("HTTPTransferUsageSpec", [] = _Query) ->
	RespHeaders = [{content_type, "application/json"}],
	Body = mochijson:encode(spec_http_transfer()),
	{ok, RespHeaders, Body};
get_usagespec(_Id, _Query) ->
	{error, 404}.

-spec get_ipdr(Type, Query) -> Result
	when
		Type :: wlan | voip,
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /ocs/v1/log/ipdr/{Type}'
%% 	requests.
get_ipdr(Type, [] = _Query) ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	Dir = Directory ++ "/" ++ Type,
	case file:list_dir(Dir) of
		{ok, Files} ->
			SortedFiles = lists:reverse(lists:sort(Files)),
			Body = mochijson:encode({array, SortedFiles}),
			RespHeaders = [{content_type, "application/json"}],
			{ok, RespHeaders, Body};
		{error, _Reason} ->
			{error, 500}
	end;
get_ipdr(_Type, _Query) ->
	{error, 400}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec ipdr_voip_characteristics(IPDRVoIP) -> IPDRVoIP
	when
		IPDRVoIP :: #ipdr_voip{} | [{struct, [tuple()]}].
%% @doc Implements json object for ipdr_voip record
%% @hidden
ipdr_voip_characteristics(#ipdr_voip{} = IPDR) ->
	ipdr_voip_characteristics(record_info(fields, ipdr_voip), IPDR, []).
%% @hidden
ipdr_voip_characteristics([callCompletionCode | T],
		#ipdr_voip{callCompletionCode = CCCode} = IPDR, Acc) ->
	Obj = {struct, [{"name", "callCompletionCode"}, {"value", CCCode}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([hostName | T], #ipdr_voip{hostName = Host} = IPDR, Acc) ->
	Obj = {struct, [{"name", "hostName"}, {"value", Host}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([subscriberId | T],
		#ipdr_voip{subscriberId = SubscriberId} = IPDR, Acc) ->
	Obj = {struct, [{"name", "subscriberId"}, {"value", SubscriberId}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([uniqueCallID | T],
		#ipdr_voip{uniqueCallID = UniqueId} = IPDR, Acc) ->
	Obj = {struct, [{"name", "uniqueCallID"}, {"value", UniqueId}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([disconnectReason | T],
		#ipdr_voip{disconnectReason = DiscReason} = IPDR, Acc) ->
	Obj = {struct, [{"name", "disconnectReason"}, {"value", DiscReason}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([destinationID | T],
		#ipdr_voip{destinationID = DistID} = IPDR, Acc) ->
	Obj = {struct, [{"name", "destinationID"}, {"value", DistID}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([bucketType | T],
		#ipdr_voip{bucketType = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([bucketType | T],
		#ipdr_voip{bucketType = BType} = IPDR, Acc) ->
	Obj = {struct, [{"name", "bucketType"}, {"value", BType}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([bucketValue | T],
		#ipdr_voip{bucketValue = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([bucketValue | T],
		#ipdr_voip{bucketValue = BValue} = IPDR, Acc) ->
	Obj = {struct, [{"name", "bucketValue"}, {"value", BValue}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([tariffType | T],
		#ipdr_voip{tariffType = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([tariffType | T],
		#ipdr_voip{tariffType = TValue} = IPDR, Acc) ->
	Obj = {struct, [{"name", "tariffType"}, {"value", TValue}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([product | T],
		#ipdr_voip{product = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([product | T],
		#ipdr_voip{product = Prod} = IPDR, Acc) ->
	Obj = {struct, [{"name", "product"}, {"value", Prod}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([priceType | T],
		#ipdr_voip{priceType = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([priceType | T],
		#ipdr_voip{priceType = PType} = IPDR, Acc) ->
	Obj = {struct, [{"name", "priceType"}, {"value", PType}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([usageRating | T],
		#ipdr_voip{usageRating = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([usageRating | T],
		#ipdr_voip{usageRating = URate} = IPDR, Acc) ->
	Obj = {struct, [{"name", "usageRating"}, {"value", URate}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([chargeAmount | T],
		#ipdr_voip{chargeAmount = undefined} = IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([chargeAmount | T],
		#ipdr_voip{chargeAmount = CA} = IPDR, Acc) ->
	Obj = {struct, [{"name", "chargeAmount"},
			{"value", ocs_rest:millionths_out(CA)}]},
	ipdr_voip_characteristics(T, IPDR, [Obj |Acc]);
ipdr_voip_characteristics([_ | T], IPDR, Acc) ->
	ipdr_voip_characteristics(T, IPDR, Acc);
ipdr_voip_characteristics([], _IPDR, Acc) ->
	lists:reverse(Acc).

%% @hidden
ipdr_wlan_characteristics(#ipdr_wlan{} = Ipdr) ->
	ipdr_wlan_characteristics(record_info(fields, ipdr_wlan), Ipdr, []).

%% @hidden
ipdr_wlan_characteristics([ipdrCreationTime | T], #ipdr_wlan{ipdrCreationTime = IpdrCreationTime} = Ipdr, Acc)
		when is_list(IpdrCreationTime) ->
	Struct = {struct, [{"name", "ipdrCreationTime"}, {"value", IpdrCreationTime}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([seqNum | T], #ipdr_wlan{seqNum = SeqNum} = Ipdr, Acc)
		when is_integer(SeqNum) ->
	Struct = {struct, [{"name", "seqNum"}, {"value", SeqNum}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([username | T], #ipdr_wlan{username = Username} = Ipdr, Acc)
		when is_list(Username)->
	Struct = {struct, [{"name", "username"}, {"value", Username}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([scIdType | T], #ipdr_wlan{scIdType = ScIdType} = Ipdr, Acc)
		when is_integer(ScIdType) ->
	Struct = {struct, [{"name", "scIdType"}, {"value", ScIdType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([scId | T], #ipdr_wlan{scId = ScId} = Ipdr, Acc)
		when is_list(ScId) ->
	Struct = {struct, [{"name", "scId"}, {"value", ScId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([homeServiceProviderType | T], #ipdr_wlan{homeServiceProviderType
		= HomeServiceProviderType} = Ipdr, Acc) when is_integer(HomeServiceProviderType) ->
	Struct = {struct, [{"name", "homeServiceProviderType"}, {"value", HomeServiceProviderType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([homeServiceProvider | T], #ipdr_wlan{homeServiceProvider
		= HomeServiceProvider} = Ipdr, Acc) when is_list(HomeServiceProvider) ->
	Struct = {struct, [{"name", "homeServiceProvider"}, {"value", Ipdr#ipdr_wlan.homeServiceProvider}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([acctSessionId | T], #ipdr_wlan{acctSessionId = AcctSessionId} = Ipdr, Acc)
		when is_list(AcctSessionId)->
	Struct = {struct, [{"name", "acctSessionId"}, {"value", AcctSessionId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([userIpAddress | T], #ipdr_wlan{userIpAddress = UserIpAddress} = Ipdr, Acc)
		when is_list(UserIpAddress)->
	Struct = {struct, [{"name", "userIpAddress"}, {"value", UserIpAddress}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([callingStationId | T], #ipdr_wlan{callingStationId = CallingStationId} = Ipdr,
		Acc) when is_list(CallingStationId) ->
	Struct = {struct, [{"name", "callingStationId"}, {"value", CallingStationId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([nasIpAddress | T], #ipdr_wlan{nasIpAddress = NasIpAddress} = Ipdr, Acc)
		when is_list(NasIpAddress) ->
	Struct = {struct, [{"name", "nasIpAddress"}, {"value", NasIpAddress}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([calledStationId | T], #ipdr_wlan{calledStationId = CalledStationId} = Ipdr,
		Acc) when is_list(CalledStationId) ->
	Struct = {struct, [{"name", "calledStationId"}, {"value", CalledStationId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([nasId | T], #ipdr_wlan{nasId = NasId} = Ipdr, Acc) when is_list(NasId) ->
	Struct = {struct, [{"name", "nasId"}, {"value", NasId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([accessProviderType | T], #ipdr_wlan{accessProviderType = AccessProviderType}
		= Ipdr, Acc) when is_integer(AccessProviderType) ->
	Struct = {struct, [{"name", "accessProviderType"}, {"value", AccessProviderType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([accessServiceProvider | T], #ipdr_wlan{accessServiceProvider
		= AccessServiceProvider} = Ipdr, Acc) when is_list(AccessServiceProvider) ->
	Struct = {struct, [{"name", "accessServiceProvider"}, {"value", AccessServiceProvider}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationName | T], #ipdr_wlan{locationName = LocationName} = Ipdr, Acc)
		when is_list(LocationName) ->
	Struct = {struct, [{"name", "locationName"}, {"value", LocationName}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationId | T], #ipdr_wlan{locationId = LocationId} = Ipdr, Acc)
		when is_list(LocationId) ->
	Struct = {struct, [{"name", "locationId"}, {"value", LocationId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationType | T], #ipdr_wlan{locationType = LocationType} = Ipdr, Acc) ->
	Struct = {struct, [{"name", "locationType"}, {"value", LocationType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationCountryCode | T], #ipdr_wlan{locationCountryCode =
		LocationCountryCode} = Ipdr, Acc) when is_list(LocationCountryCode) ->
	Struct = {struct, [{"name", "locationCountryCode"}, {"value", LocationCountryCode}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationStateProvince | T], #ipdr_wlan{locationStateProvince =
		LocationStateProvince} = Ipdr, Acc) when is_list(LocationStateProvince) ->
	Struct = {struct, [{"name", "locationStateProvince"}, {"value", LocationStateProvince}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationCity | T], #ipdr_wlan{locationCity = LocationCity} = Ipdr, Acc)
		when is_list(LocationCity) ->
	Struct = {struct, [{"name", "locationCity"}, {"value", LocationCity}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationGeocode | T], #ipdr_wlan{locationGeocode = LocationGeocode} = Ipdr,
		Acc) when is_list(LocationGeocode) ->
	Struct = {struct, [{"name", "locationGeocode"}, {"value", LocationGeocode}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([locationGeocodeType | T], #ipdr_wlan{locationGeocodeType
		= LocationGeocodeType} = Ipdr, Acc) when is_list(LocationGeocodeType) ->
	Struct = {struct, [{"name", "locationGeocodeType"}, {"value", LocationGeocodeType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([nasPortType | T], #ipdr_wlan{nasPortType = NasPortType} = Ipdr, Acc)
		when is_integer(NasPortType) ->
	Struct = {struct, [{"name", "nasPortType"}, {"value", NasPortType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([paymentType | T], #ipdr_wlan{paymentType = PaymentType} = Ipdr, Acc)
		when is_integer(PaymentType) ->
	Struct = {struct, [{"name", "paymentType"}, {"value", PaymentType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([networkConnectionType | T], #ipdr_wlan{networkConnectionType
		= NetworkConnectionType} = Ipdr, Acc) ->
	Struct = {struct, [{"name", "networkConnectionType"}, {"value", NetworkConnectionType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([sessionDuration | T], #ipdr_wlan{sessionDuration = SessionDuration}
		= Ipdr, Acc) when is_integer(SessionDuration) ->
	Struct = {struct, [{"name", "sessionDuration"}, {"value", SessionDuration}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([inputOctets | T], #ipdr_wlan{inputOctets = InputOctets} = Ipdr, Acc)
		when is_integer(InputOctets) ->
	Struct = {struct, [{"name", "inputOctets"}, {"value", InputOctets}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([outputOctets | T], #ipdr_wlan{outputOctets = OutputOctets} = Ipdr, Acc)
		when is_integer(OutputOctets) ->
	Struct = {struct, [{"name", "outputOctets"}, {"value", OutputOctets}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([class | T], #ipdr_wlan{class = Class} = Ipdr, Acc) when is_list(Class) ->
	Struct = {struct, [{"name", "class"}, {"value", Class}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([gmtSessionStartDateTime | T], #ipdr_wlan{gmtSessionStartDateTime =
		GmtSessionStartDateTime} = Ipdr, Acc) when is_list(GmtSessionStartDateTime) ->
	Struct = {struct, [{"name", "gmtSessionStartDateTime"}, {"value", GmtSessionStartDateTime}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([gmtSessionEndDateTime | T], #ipdr_wlan{gmtSessionEndDateTime =
		GmtSessionEndDateTime} = Ipdr, Acc) when is_list(GmtSessionEndDateTime) ->
	Struct = {struct, [{"name", "gmtSessionEndDateTime"}, {"value", GmtSessionEndDateTime}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([sessionTerminateCause | T], #ipdr_wlan{sessionTerminateCause =
		SessionTerminateCause} = Ipdr, Acc) when is_integer(SessionTerminateCause) ->
	Struct = {struct, [{"name", "sessionTerminateCause"}, {"value", SessionTerminateCause}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([billingClassOfService | T], #ipdr_wlan{billingClassOfService =
		BillingClassOfService} = Ipdr, Acc) when is_list(BillingClassOfService) ->
	Struct = {struct, [{"name", "billingClassOfService"}, {"value", BillingClassOfService}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([unitOfMeasure | T], #ipdr_wlan{unitOfMeasure = UnitOfMeasure} = Ipdr, Acc)
		when is_integer(UnitOfMeasure) ->
	Struct = {struct, [{"name", "unitOfMeasure"}, {"value", UnitOfMeasure}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([chargeableUnit | T], #ipdr_wlan{chargeableUnit = ChargeableUnit} = Ipdr,
		Acc) when is_integer(ChargeableUnit) ->
	Struct = {struct, [{"name", "chargeableUnit"}, {"value", ChargeableUnit}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([chargeableQuantity | T], #ipdr_wlan{chargeableQuantity =
		ChargeableQuantity} = Ipdr, Acc) ->
	Struct = {struct, [{"name", "chargeableQuantity"}, {"value", ChargeableQuantity}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([chargeAmount | T], #ipdr_wlan{chargeAmount = ChargeAmount} = Ipdr, Acc)
		when is_integer(ChargeAmount) ->
	Struct = {struct, [{"name", "chargeAmount"}, {"value", ChargeAmount}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([chargeCurrencyType | T], #ipdr_wlan{chargeCurrencyType =
		ChargeCurrencyType} = Ipdr, Acc) when is_list(ChargeCurrencyType)->
	Struct = {struct, [{"name", "chargeCurrencyType"}, {"value", ChargeCurrencyType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([otherParty | T], #ipdr_wlan{otherParty = OtherParty} = Ipdr, Acc)
		when is_list(OtherParty) ->
	Struct = {struct, [{"name", "otherParty"}, {"value", OtherParty}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([taxPercentage | T], #ipdr_wlan{taxPercentage = TaxPercentage} = Ipdr, Acc)
		when is_integer(TaxPercentage) ->
	Struct = {struct, [{"name", "taxPercentage"}, {"value", TaxPercentage}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([taxAmount | T], #ipdr_wlan{taxAmount = TaxAmount} = Ipdr, Acc) ->
	Struct = {struct, [{"name", "taxAmount"}, {"value", TaxAmount}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([taxType | T], #ipdr_wlan{taxType = TaxType} = Ipdr, Acc) when is_integer(TaxType) ->
	Struct = {struct, [{"name", "taxType"}, {"value", TaxType}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([intermediaryName | T], #ipdr_wlan{intermediaryName = IntermediaryName} =
		Ipdr, Acc) when is_list(IntermediaryName) ->
	Struct = {struct, [{"name", "intermediaryName"}, {"value", IntermediaryName}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([serviceName | T], #ipdr_wlan{serviceName = ServiceName} = Ipdr, Acc)
		when is_integer(ServiceName) ->
	Struct = {struct, [{"name", "serviceName"}, {"value", ServiceName}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([relatedIpdrIdList | T], #ipdr_wlan{relatedIpdrIdList = RelatedIpdrIdList}
		= Ipdr, Acc) when is_list(RelatedIpdrIdList) ->
	Struct = {struct, [{"name", "relatedIpdrIdList"}, {"value", RelatedIpdrIdList}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([tempUserId | T], #ipdr_wlan{tempUserId = TempUserId} = Ipdr, Acc)
		when is_list(TempUserId) ->
	Struct = {struct, [{"name", "tempUserId"}, {"value", TempUserId}]},
	ipdr_wlan_characteristics(T, Ipdr, [Struct |Acc]);
ipdr_wlan_characteristics([_ | T], Ipdr, Acc) ->
	ipdr_wlan_characteristics(T, Ipdr, Acc);
ipdr_wlan_characteristics([], _Ipdr, Acc) ->
	lists:reverse(Acc).

%% @hidden
usage_aaa_auth({Milliseconds, N, P, Node,
		{ServerIP, ServerPort}, {ClientIP, ClientPort}, EventType,
		RequestAttributes, ResponseAttributes}, Filters) ->
	UsageSpec = {struct, [{"id", "AAAAccessUsageSpec"},
			{"href", ?usageSpecPath ++ "AAAAccessUsageSpec"},
			{"name", "AAAAccessUsageSpec"}]},
	Type = "AAAAccessUsage",
	Status = "received",
	ID = "auth-" ++ integer_to_list(Milliseconds) ++ "-"
			++ integer_to_list(N),
	Href = ?usagePath ++ ID,
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
usage_ipdr(Event, Filters) when is_record(Event, ipdr_wlan) ->
	UsageSpec = {struct, [{"id", "PublicWLANAccessUsageSpec"},
			{"href", ?usageSpecPath ++ "PublicWLANAccessUsageSpec"},
			{"name", "PublicWLANAccessUsageSpec"}]},
	Type = "PublicWLANAccessUsage",
	Status = "rated",
	ID = integer_to_list(Event#ipdr_wlan.seqNum),
	Href = ?usagePath ++ ID,
	Date = Event#ipdr_wlan.ipdrCreationTime,
	Chars = ipdr_wlan_characteristics(Event),
	Object = {struct, [{"id", ID}, {"href", Href}, {"date", Date}, {"type", Type},
			{"status", Status}, {"usageSpecification", UsageSpec},
			{"usageCharacteristic", {array, Chars}}]},
	case Filters of
		[] ->
			Object;
		_ ->
			ocs_rest:fields("id,href," ++ Filters, Object)
	end;
usage_ipdr(Event, Filters) when is_record(Event, ipdr_voip) ->
	UsageSpec = {struct, [{"id", "VoIPUsageSpec"},
			{"href", ?usageSpecPath ++ "VoIPUsageSpec"},
			{"name", "VoIPUsageSpec"}]},
	Type = "VoIPUsage",
	Status = "rated",
	ID = integer_to_list(Event#ipdr_voip.seqNum),
	Href = ?usagePath ++ ID,
	Date = Event#ipdr_voip.ipdrCreationTime,
	Chars = ipdr_voip_characteristics(Event),
	Object = {struct, [{"id", ID}, {"href", Href}, {"date", Date}, {"type", Type},
			{"status", Status}, {"usageSpecification", UsageSpec},
			{"usageCharacteristic", {array, Chars}}]},
	case Filters of
		[] ->
			Object;
		_ ->
			ocs_rest:fields("id,href," ++ Filters, Object)
	end;
usage_ipdr(Event, Filters) when is_list(Event) ->
	usage_ipdr(Event, Filters, []).
%% @hidden
usage_ipdr([#ipdrDocWLAN{} | T], Filters, Acc) ->
	usage_ipdr(T, Filters, Acc);
usage_ipdr([#ipdrDocVoIP{} | T], Filters, Acc) ->
	usage_ipdr(T, Filters, Acc);
usage_ipdr([#ipdrDocEnd{} | T], Filters, Acc) ->
	usage_ipdr(T, Filters, Acc);
usage_ipdr([H | T], Filters, Acc) ->
	usage_ipdr(T, Filters, [usage_ipdr(H, Filters) | Acc]);
usage_ipdr([], _Filters, Acc) ->
	lists:reverse(Acc).

%% @hidden
usage_aaa_acct(Event, Filters) when is_tuple(Event), size(Event) > 6 ->
	Milliseconds = element(1, Event),
	{ServerIP, ServerPort} = element(5, Event),
	UsageSpec = {struct, [{"id", "AAAAccountingUsageSpec"},
			{"href", ?usageSpecPath ++ "AAAccountingUsageSpec"},
			{"name", "AAAAccountingUsageSpec"}]},
	Type = "AAAAccountingUsage",
	Status = "received",
	ID = "acct-" ++ integer_to_list(Milliseconds) ++ "-"
			++ integer_to_list(element(2, Event)),
	Href = ?usagePath ++ ID,
	Date = ocs_log:iso8601(Milliseconds),
	Protocol = case element(3, Event) of
		radius ->
			"RADIUS";
		diameter ->
			"DIAMETER";
		nrf ->
			"Nrf_Rating"
	end,
	ServerAddress = inet:ntoa(ServerIP),
	EventChars = [{struct, [{"name", "protocol"}, {"value", Protocol}]},
			{struct, [{"name", "node"},
					{"value", atom_to_list(element(4, Event))}]},
			{struct, [{"name", "serverAddress"}, {"value", ServerAddress}]},
			{struct, [{"name", "serverPort"}, {"value", ServerPort}]},
			{struct, [{"name", "type"},
					{"value", atom_to_list(element(6, Event))}]}],
	AttributeChars = usage_characteristics(element(7, Event)),
	UsageChars = EventChars ++ AttributeChars,
	Frated = fun(#rated{tax_excluded_amount = TaxExcluded})
					when is_integer(TaxExcluded) ->
				{struct, [{"taxExcludedRatingAmount", ocs_rest:millionths_out(TaxExcluded)}]};
			(_) ->
				{struct, []}
	end,
	RatedUsage = case size(Event) > 8 of
		true ->
			case element(9, Event) of
				[#rated{} | _] = Rated ->
					[{"ratedProductUsage", {array, lists:map(Frated, Rated)}}];
				[[#rated{} | _] = Rated] ->
					[{"ratedProductUsage", {array, lists:map(Frated, Rated)}}];
				undefined ->
					[]
			end;
		false ->
			[]
	end,
	Object = {struct, [{"id", ID}, {"href", Href}, {"date", Date}, {"type", Type},
			{"status", Status}, {"usageSpecification", UsageSpec},
			{"usageCharacteristic", {array, UsageChars}}] ++ RatedUsage},
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
			{"href", ?usageSpecPath ++ "HTTPTransferUsageSpec"},
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
spec_aaa_policy_usage_spec() ->
	ID = {"id", "AAAPolicyUsageSpec"},
	Href = {"href", ?usageSpecPath ++ "AAAPolicyUsageSpec"},
	Name = {"name", "AAAPolicyUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS Policy
			Usage in Diameter Gx interface."},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", undefined},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [spec_charging_rule_install()],
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_charging_rule_install()  ->
	Name = {"name", "chargingRuleInstall"},
	Desc = {"description", "Charging Rule characteristic in Gx Policy Usage"},
	Conf = {"configurable", true},
	Value1 = {struct, [{"name", "name"},
			{"description", "Policy rule name"},
			{"default", true}, {"valueType", "String"}]},
	Value2 = {struct, [{"name", "precedence"},
			{"description", "Priority order rules are applied in"},
			{"default", true}, {"valueType", "Number"}]},
	Value3 = {struct, [{"name", "predefined"},
			{"description", "Indicate PCEF predefined rule"},
			{"default", false}, {"valueType", "Boolean"}]},
	Value4 = {struct, [{"name", "chargingKey"},
			{"description", "Charging rule"},
			{"default", true}, {"valueType", "Number"}]},
	Value5 = {struct, [{"name", "serviceId"},
			{"description", "Service flow identifier"},
			{"default", false}, {"valueType", "String"}]},
	Value6 =  {struct, [{"name", "qosInformation"},
			{"description", "Quality of Service Information"},
			{"default", false}, {"valueType", "Object"}]},
	Value7 = {struct, [{"name", "flowInformation"},
			{"description", "Service flow template"}, {"default", true},
			{"valueType", "Array"}]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2, Value3,
			Value4, Value5, Value6, Value7]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_aaa_auth() ->
	ID = {"id", "AAAAccessUsageSpec"},
	Href = {"href", ?usageSpecPath ++ "AAAAccessUsageSpec"},
	Name = {"name", "AAAAccessUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS access requests."},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", undefined},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [spec_protocol1(), spec_node(), spec_server_address(),
			spec_server_port(), spec_client_address(),
			spec_client_port(), spec_type_access(), spec_attr_context(),
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
	Href = {"href", ?usageSpecPath ++ "AAAAccountingUsageSpec"},
	Name = {"name", "AAAAccountingUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS accounting requests."},
	Start = {"startDateTime", "2024-11-25T00:00:00Z"},
	End = {"endDateTime", undefined},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [spec_protocol2(), spec_node(), spec_server_address(),
			spec_server_port(), spec_type_accounting(), spec_attr_context(),
			spec_attr_username(),  spec_attr_msisdn(), spec_attr_imsi(),
			spec_attr_nas_id(), spec_attr_nas_ip(), spec_attr_nas_port(),
			spec_attr_service_type(), spec_attr_framed_address(),
			spec_attr_framed_netmask(), spec_attr_framed_routing(),
			spec_attr_filter_id(), spec_attr_framed_mtu(),
			spec_attr_framed_route(), spec_attr_class(),
			spec_attr_session_timeout(), spec_attr_idle_timeout(),
			spec_attr_termination_action(), spec_attr_called_id(),
			spec_attr_calling_id(), spec_attr_delay(),
			spec_attr_nas_port_id(), spec_attr_nas_port_type(),
			spec_attr_input_octets(), spec_attr_output_octets(),
			spec_attr_input_giga_words(),spec_attr_output_giga_words(),
			spec_attr_total_octets(), spec_attr_user_location(),
			spec_attr_event_timestamp(), spec_attr_session_id(),
			spec_attr_authentic(), spec_attr_session_time(),
			spec_attr_input_packets(), spec_attr_output_packets(),
			spec_attr_cause(), spec_attr_multisession_id(),
			spec_attr_link_count(), spec_attr_port_limit()],
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

%% @hidden
spec_http_transfer() ->
	ID = {"id", "HTTPTransferUsageSpec"},
	Href = {"href", ?usageSpecPath ++ "HTTPTransferUsageSpec"},
	Name = {"name", "HTTPTransferUsageSpec"},
	Desc = {"description", "Specification for SigScale OCS http requests."},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", undefined},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = [{struct, [{"todo", "todo"}]}],
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.

spec_voip() ->
	ID = {"id", "VoIPUsageSpec"},
	Href = {"href", ?usageSpecPath ++ "VoIPUsageSpec"},
	Name = {"name", "VoIPUsageSpec"},
	Desc = {"description", ""},
	Start = {"startDateTime", "2018-03-12T00:00:00Z"},
	End = {"endDateTime", undefined},
	Valid = {"validFor", {struct, [Start, End]}},
	Chars = lists:reverse(spec_voip1([])),
	Char = {"usageSpecCharacteristic", {array, Chars}},
	{struct, [ID, Href, Name, Desc, Valid, Char]}.
%% @hidden
spec_voip1(Acc) ->
	Name = {"name", "callCompletionCode"},
	Desc = {"description", "Final call completion code for billing use"},
	Conf = {"configurable", false},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_voip2(NewAcc).
%% @hidden
spec_voip2(Acc) ->
	Name = {"name", "hostName"},
	Desc = {"description", "Name of call management server controlling call processing"},
	Conf = {"configurable", false},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_voip3(NewAcc).
%% @hidden
spec_voip3(Acc) ->
	Name = {"name", "subscriberId"},
	Desc = {"description", "Unique within a service provider network. Tied to a SC or a SE requesting a service"},
	Conf = {"configurable", false},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_voip4(NewAcc).
%% @hidden
spec_voip4(Acc) ->
	Name = {"name", "uniqueCallID"},
	Desc = {"description", "Unique Call ID to identify that different IPDRs generated by different elements are for the same call"},
	Conf = {"configurable", false},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_voip5(NewAcc).
%% @hidden
spec_voip5(Acc) ->
	Name = {"name", "disconnectReason"},
	Desc = {"description", "Reason that call was disconnected based on Call CompletionCode"},
	Conf = {"configurable", false},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	spec_voip6(NewAcc).
spec_voip6(Acc) ->
	Name = {"name", "destinationID"},
	Desc = {"description", "ID of called party"},
	Conf = {"configurable", false},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	NewAcc = [{struct, [Name, Desc, Conf, Value]} | Acc],
	lists:reverse(NewAcc).

%% @hidden
spec_public_wlan() ->
	ID = {"id", "PublicWLANAccessUsageSpec"},
	Href = {"href", ?usageSpecPath ++ "PublicWLANAccessUsageSpec"},
	Name = {"name", "PublicWLANAccessUsageSpec"},
	Desc = {"description", "Specification for IPDR Public WLAN Access - WISP Use Case"},
	Start = {"startDateTime", "2017-01-01T00:00:00Z"},
	End = {"endDateTime", undefined},
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
spec_protocol1() ->
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
spec_protocol2() ->
	Name = {"name", "protocol"},
	Desc = {"description", "AAA protocol used in request."},
	Conf = {"configurable", true},
	Typ = {"valueType", "String"},
	Def = {"default", true},
	Val1 = {"value", "RADIUS"},
	Value1 = {struct, [Typ, Def, Val1]},
	Val2 = {"value", "DIAMETER"},
	Value2 = {struct, [Typ, Def, Val2]},
	Val3 = {"value", "Nrf_Rating"},
	Value3 = {struct, [Typ, Def, Val3]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1, Value2, Value3]}},
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
spec_attr_msisdn() ->
	Name = {"name", "msisdn"},
	Desc = {"description", "Mobile Station International Subscriber Directory Number (MSISDN)."},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Value]}.

%% @hidden
spec_attr_imsi() ->
	Name = {"name", "imsi"},
	Desc = {"description", "International Mobile Subscriber Identity (IMSI)"},
	Typ = {"valueType", "String"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Value]}.

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
	Desc = {"description", "RADIUS Service-Type"},
	Conf = {"configurable", true},
	TypS = {"valueType", "String"},
	TypN = {"valueType", "Number"},
	Val1 = {"value", "framed"},
	Value1 = {struct, [TypS, Val1]},
	Val2 = {"value", "administrative"},
	Value2 = {struct, [TypS, Val2]},
	Val3 = {"value", "authenticate-only"},
	Value3 = {struct, [TypS, Val3]},
	Value4 = {struct, [TypN]},
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
spec_attr_total_octets() ->
	Name = {"name", "totalOctets"},
	Desc = {"description", "CC-Total-Octets attribute"},
	Conf = {"configurable", true},
	Typ = {"valueType", "Number"},
	Value1 = {struct, [Typ]},
	Value = {"usageSpecCharacteristicValue", {array, [Value1]}},
	{struct, [Name, Desc, Conf, Value]}.

%% @hidden
spec_attr_user_location() ->
	Name = {"name", "userLocationInfo"},
	Desc = {"description", "3GPP-User-Location-Info attribute"},
	Conf = {"configurable", true},
	Type = {"valueType", "String"},
	Value1 = {struct, [Type]},
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
spec_attr_context() ->
	Name = {"name", "serviceContextId"},
	Desc = {"description", "Service-Context-Id AVP"},
	Conf = {"configurable", true},
	Type = {"valueType", "String"},
	{struct, [Name, Desc, Conf, Type]}.

%% @hidden
usage_characteristics(Attributes) ->
	lists:reverse(char_attr_username(Attributes, [])).

%% @hidden
char_attr_username(#'3gpp_ro_CCR'{'Subscription-Id' = SubscriptionIdData} = CCR, Acc)
		when length(SubscriptionIdData) > 0 ->
	F = fun(#'3gpp_ro_Subscription-Id'{'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
				'Subscription-Id-Data' = MSISDN}, Acc1) ->
			[{struct, [{"name", "msisdn"}, {"value", binary_to_list(MSISDN)}]} | Acc1];
		(#'3gpp_ro_Subscription-Id'{'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
				'Subscription-Id-Data' = IMSI}, Acc1) ->
			[{struct, [{"name", "imsi"}, {"value", binary_to_list(IMSI)}]} | Acc1];
		(_, Acc1) ->
			Acc1
	end,
	NewAcc = lists:foldl(F, Acc, SubscriptionIdData),
	char_attr_nas_ip(CCR, NewAcc);
char_attr_username(#'3gpp_gx_CCR'{'Subscription-Id' = SubscriptionIdData} = CCR, Acc)
		when length(SubscriptionIdData) > 0 ->
	F = fun(#'3gpp_gx_Subscription-Id'{'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
				'Subscription-Id-Data' = MSISDN}, Acc1) ->
			[{struct, [{"name", "msisdn"}, {"value", binary_to_list(MSISDN)}]} | Acc1];
		(#'3gpp_gx_Subscription-Id'{'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
				'Subscription-Id-Data' = IMSI}, Acc1) ->
			[{struct, [{"name", "imsi"}, {"value", binary_to_list(IMSI)}]} | Acc1];
		(_, Acc1) ->
			Acc1
	end,
	NewAcc = lists:foldl(F, Acc, SubscriptionIdData),
	char_attr_nas_ip(CCR, NewAcc);
char_attr_username(#'3gpp_ro_CCR'{'User-Name' = [Username]} = CCR, Acc)
		when is_binary(Username) ->
	NewAcc = [{struct, [{"name", "username"},
			{"value", binary_to_list(Username)}]} | Acc],
	char_attr_nas_ip(CCR, NewAcc);
char_attr_username(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?UserName, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "username"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_ip(Attributes, NewAcc);
char_attr_username(#{"subscriptionId" := SubscriptionId} = NrfRequest, Acc)
		when length(SubscriptionId) > 0 ->
	F = fun("msisdn-" ++ MSISDN, Acc1) ->
			[{struct, [{"name", "msisdn"}, {"value", MSISDN}]} | Acc1];
		("imsi-" ++  IMSI, Acc1) ->
			[{struct, [{"name", "imsi"}, {"value", IMSI}]} | Acc1];
		(_Other, Acc1) ->
			Acc1
	end,
	NewAcc = lists:foldl(F, Acc, SubscriptionId),
	char_attr_nas_ip(NrfRequest, NewAcc);
char_attr_username(Request, Acc) ->
	char_attr_nas_ip(Request, Acc).

%% @hidden
char_attr_nas_ip(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'PS-Information'
		= [#'3gpp_ro_PS-Information'{'SGSN-Address'
		= [Address]}]}]} = CCR, Acc) when is_tuple(Address) ->
	NewAcc = [{struct, [{"name", "nasIpAddress"},
			{"value", inet:ntoa(Address)}]} | Acc],
	char_attr_context(CCR, NewAcc);
char_attr_nas_ip(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'PS-Information'
		= [#'3gpp_ro_PS-Information'{'SGW-Address'
		= [Address]}]}]} = CCR, Acc) when is_tuple(Address) ->
	NewAcc = [{struct, [{"name", "nasIpAddress"},
			{"value", inet:ntoa(Address)}]} | Acc],
	char_attr_context(CCR, NewAcc);
char_attr_nas_ip(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?NasIpAddress, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasIpAddress"},
					{"value", inet:ntoa(Value)}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_port(Attributes, NewAcc);
char_attr_nas_ip(#{"serviceRating" := [#{"serviceInformation"
		:= #{"pduSessionInformation" := #{"servingNetworkFunctionID"
		:= #{"servingNetworkFunctionInformation"
		:= #{"nFIPv4Address" := Address}}}}} | _]} = NrfRequest, Acc) ->
	NewAcc = [{struct, [{"name", "nasIpAddress"}, {"value", Address}]} | Acc],
	char_attr_context(NrfRequest, NewAcc);
char_attr_nas_ip(Request, Acc) ->
	char_attr_context(Request, Acc).

%% @hidden
char_attr_nas_port(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?NasPort, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasPort"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_service_type(Attributes, NewAcc).

%% @hidden
char_attr_service_type(Attributes, Acc)
		when is_list(Attributes) ->
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

char_attr_context(#'3gpp_ro_CCR'{'Service-Context-Id' = Context} = CCR, Acc)
		when is_binary(Context) ->
	Value = binary_to_list(Context),
	NewAcc = [{struct, [{"name", "serviceContextId"}, {"value", Value}]} | Acc],
	char_attr_called_id(CCR, NewAcc);
char_attr_context(#{"serviceRating"
		:= [#{"serviceContextId" := Context} | _]} = NrfRequest, Acc) ->
	NewAcc = [{struct, [{"name", "serviceContextId"}, {"value", Context}]} | Acc],
	char_attr_called_id(NrfRequest, NewAcc);
char_attr_context(Request, Acc) ->
	char_attr_called_id(Request, Acc).

%% @hidden
char_attr_framed_address(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?FramedIpAddress, Attributes) of
		{ok, Value} ->
			Address = inet:ntoa(Value),
			[{struct, [{"name", "framedIpAddress"}, {"value", Address}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_pool(Attributes, NewAcc).

%% @hidden
char_attr_framed_pool(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?FramedPool, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "framedPool"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_netmask(Attributes, NewAcc).

%% @hidden
char_attr_framed_netmask(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?FramedIpNetmask, Attributes) of
		{ok, Value} ->
			Netmask = inet:ntoa(Value),
			[{struct, [{"name", "framedIpNetmask"}, {"value", Netmask}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_routing(Attributes, NewAcc).

%% @hidden
char_attr_framed_routing(Attributes, Acc)
		when is_list(Attributes) ->
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
char_attr_filter_id(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?FilterId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "filterId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_mtu(Attributes, NewAcc).

%% @hidden
char_attr_framed_mtu(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?FramedMtu, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "framedMtu"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_framed_route(Attributes, NewAcc).

%% @hidden
char_attr_framed_route(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?FramedRoute, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "framedRoute"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_class(Attributes, NewAcc).

%% @hidden
char_attr_class(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?Class, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "class"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_session_timeout(Attributes, NewAcc).

%% @hidden
char_attr_session_timeout(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?SessionTimeout, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "sessionTimeout"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_idle_timeout(Attributes, NewAcc).

%% @hidden
char_attr_idle_timeout(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?IdleTimeout, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "idleTimeout"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_termination_action(Attributes, NewAcc).

%% @hidden
char_attr_termination_action(Attributes, Acc)
		when is_list(Attributes) ->
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
char_attr_called_id(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'IMS-Information'
		= [#'3gpp_ro_IMS-Information'{'Called-Party-Address'
		= [<<"tel:", Called/binary>>]}]}]} = CCR, Acc) ->
	NewAcc = [{struct, [{"name", "calledStationId"},
			{"value", binary_to_list(Called)}]} | Acc],
	char_attr_calling_id(CCR, NewAcc);
char_attr_called_id(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?CalledStationId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "calledStationId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_calling_id(Attributes, NewAcc);
char_attr_called_id(Request, Acc) ->
	char_attr_calling_id(Request, Acc).

%% @hidden
char_attr_calling_id(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'IMS-Information'
		= [#'3gpp_ro_IMS-Information'{'Calling-Party-Address'
		= [<<"tel:", Called/binary>>]}]}]} = CCR, Acc) ->
	NewAcc = [{struct, [{"name", "callingStationId"},
			{"value", binary_to_list(Called)}]} | Acc],
	char_attr_nas_id(CCR, NewAcc);
char_attr_calling_id(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?CallingStationId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "callingStationId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_id(Attributes, NewAcc);
char_attr_calling_id(Request, Acc) ->
	char_attr_nas_id(Request, Acc).

%% @hidden
char_attr_nas_id(#'3gpp_ro_CCR'{'Origin-Host' = Host} = CCR, Acc)
		when is_binary(Host) ->
	NewAcc = [{struct, [{"name", "nasIdentifier"},
			{"value", binary_to_list(Host)}]} | Acc],
	char_attr_event_timestamp(CCR, NewAcc);
char_attr_nas_id(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?NasIdentifier, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasIdentifier"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_port_id(Attributes, NewAcc);
char_attr_nas_id(#{"nfConsumerIdentification"
		:= #{"nFName" := NfName}} = NrfRequest, Acc) ->
	NewAcc = [{struct, [{"name", "nasIdentifier"}, {"value", NfName}]} | Acc],
	char_attr_event_timestamp(NrfRequest, NewAcc);
char_attr_nas_id(#{"nfConsumerIdentification"
		:= #{"nodeFunctionality" := NodeFunctionality}} = NrfRequest, Acc) ->
	NewAcc = [{struct, [{"name", "nasIdentifier"}, {"value", NodeFunctionality}]} | Acc],
	char_attr_event_timestamp(NrfRequest, NewAcc);
char_attr_nas_id(Request, Acc) ->
	char_attr_event_timestamp(Request, Acc).

%% @hidden
char_attr_port_id(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?NasPortId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "nasPortId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_nas_port_type(Attributes, NewAcc).

%% @hidden
char_attr_nas_port_type(Attributes, Acc)
		when is_list(Attributes) ->
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
char_attr_delay(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctDelayTime, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctDelayTime"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_event_timestamp(Attributes, NewAcc).

%% @hidden
char_attr_event_timestamp(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'IMS-Information'
		= [#'3gpp_ro_IMS-Information'{'Time-Stamps'
		= [#'3gpp_ro_Time-Stamps'{'SIP-Request-Timestamp'
		= [TimeStamp]}]}]}]} = CCR, Acc) ->
	Seconds = calendar:datetime_to_gregorian_seconds(TimeStamp) - ?EPOCH,
	NewAcc = [{struct, [{"name", "eventTimestamp"},
			{"value", ocs_log:iso8601(Seconds * 1000)}]} | Acc],
	char_attr_session_id(CCR, NewAcc);
char_attr_event_timestamp(#'3gpp_ro_CCR'{'Event-Timestamp'
		= [TimeStamp]} = CCR, Acc) ->
	Seconds = calendar:datetime_to_gregorian_seconds(TimeStamp) - ?EPOCH,
	NewAcc = [{struct, [{"name", "eventTimestamp"},
			{"value", ocs_log:iso8601(Seconds * 1000)}]} | Acc],
	char_attr_session_id(CCR, NewAcc);
char_attr_event_timestamp(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?EventTimestamp, Attributes) of
		{ok, Value} ->
			DateTime = ocs_log:iso8601(Value * 1000),
			[{struct, [{"name", "eventTimestamp"}, {"value", DateTime}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_session_id(Attributes, NewAcc);
char_attr_event_timestamp(Request, Acc) ->
	char_attr_session_id(Request, Acc).

%% @hidden
char_attr_session_id(#'3gpp_ro_CCR'{'Session-Id' = SessionID} = CCR, Acc)
		when SessionID /= undefined ->
	NewAcc = [{struct, [{"name", "acctSessionId"},
			{"value", binary_to_list(SessionID)}]} | Acc],
	char_attr_session_time(CCR, NewAcc);
char_attr_session_id(#'3gpp_gx_CCR'{'Session-Id' = SessionID} = CCR, Acc)
		when SessionID /= undefined ->
	NewAcc = [{struct, [{"name", "acctSessionId"},
			{"value", binary_to_list(SessionID)}]} | Acc],
	char_attr_session_time(CCR, NewAcc);
char_attr_session_id(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctSessionId, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctSessionId"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_multisession_id(Attributes, NewAcc);
char_attr_session_id(#{"ratingSessionId" := SessionId} = NrfRequest, Acc) ->
	NewAcc = [{struct, [{"name", "acctSessionId"}, {"value", SessionId}]} | Acc],
	char_attr_session_time(NrfRequest, NewAcc);
char_attr_session_id(Request, Acc) ->
	char_attr_session_time(Request, Acc).

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
char_attr_link_count(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctLinkCount, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctLinkCount"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_authentic(Attributes, NewAcc).

%% @hidden
char_attr_authentic(Attributes, Acc)
		when is_list(Attributes) ->
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
char_attr_session_time(#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control'
		= MSCC} = CCR, Acc) ->
	Fold = fun(#'3gpp_ro_Multiple-Services-Credit-Control'{
					'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
					'CC-Time' = [Seconds]}]}, Acc1) ->
				Seconds + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, MSCC) of
		0 ->
			Acc;
		TotalTime ->
			[{struct, [{"name", "acctSessionTime"}, {"value", TotalTime}]} | Acc]
	end,
	char_attr_input_octets(CCR, NewAcc);
char_attr_session_time(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctSessionTime, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctSessionTime"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_input_octets(Attributes, NewAcc);
char_attr_session_time(#{"serviceRating" := ServiceRating} = NrfRequest, Acc)
		when length(ServiceRating) > 0 ->
	Fold = fun(#{"consumedUnit" := #{"time" := Time}}, Acc1) ->
				Time + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, ServiceRating) of
		0 ->
			Acc;
		Time1 ->
			[{struct, [{"name", "acctSessionTime"}, {"value", Time1}]} | Acc]
	end,
	char_attr_input_octets(NrfRequest, NewAcc);
char_attr_session_time(Request, Acc) ->
	char_attr_input_octets(Request, Acc).

%% @hidden
char_attr_input_octets(#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control'
		= MSCC} = CCR, Acc) ->
	Fold = fun(#'3gpp_ro_Multiple-Services-Credit-Control'{
					'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
					'CC-Input-Octets' = [Octets]}]}, Acc1) ->
				Octets + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, MSCC) of
		0 ->
			Acc;
		InputOctets ->
			[{struct, [{"name", "inputOctets"}, {"value", InputOctets}]} | Acc]
	end,
	char_attr_output_octets(CCR, NewAcc);
char_attr_input_octets(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctInputOctets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "inputOctets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_output_octets(Attributes, NewAcc);
char_attr_input_octets(#{"serviceRating" := ServiceRating} = NrfRequest, Acc)
		when length(ServiceRating) > 0 ->
	Fold = fun(#{"consumedUnit" := #{"uplinkVolume" := Volume}}, Acc1) ->
				Volume + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, ServiceRating) of
		0 ->
			Acc;
		Volume1 ->
			[{struct, [{"name", "inputOctets"}, {"value", Volume1}]} | Acc]
	end,
	char_attr_output_octets(NrfRequest, NewAcc);
char_attr_input_octets(Request, Acc) ->
	char_attr_output_octets(Request, Acc).

%% @hidden
char_attr_output_octets(#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control'
		= MSCC} = CCR, Acc) ->
	Fold = fun(#'3gpp_ro_Multiple-Services-Credit-Control'{
					'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
					'CC-Output-Octets' = [Octets]}]}, Acc1) ->
				Octets + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, MSCC) of
		0 ->
			Acc;
		OutputOctets ->
			[{struct, [{"name", "outputOctets"}, {"value", OutputOctets}]} | Acc]
	end,
	char_attr_total_octets(CCR, NewAcc);
char_attr_output_octets(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctOutputOctets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "outputOctets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_input_giga_words(Attributes, NewAcc);
char_attr_output_octets(#{"serviceRating" := ServiceRating} = NrfRequest, Acc)
		when length(ServiceRating) > 0 ->
	Fold = fun(#{"consumedUnit" := #{"downlinkVolume" := Volume}}, Acc1) ->
				Volume + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, ServiceRating) of
		0 ->
			Acc;
		Volume1 ->
			[{struct, [{"name", "outputOctets"}, {"value", Volume1}]} | Acc]
	end,
	char_attr_total_octets(NrfRequest, NewAcc);
char_attr_output_octets(Request, Acc) ->
	char_attr_total_octets(Request, Acc).

%% @hidden
char_attr_total_octets(#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control'
		= MSCC} = CCR, Acc) ->
	Fold = fun(#'3gpp_ro_Multiple-Services-Credit-Control'{
					'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
					'CC-Total-Octets' = [Octets]}]}, Acc1) ->
				Octets + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, MSCC) of
		0 ->
			Acc;
		TotalOctets ->
			[{struct, [{"name", "totalOctets"}, {"value", TotalOctets}]} | Acc]
	end,
	char_attr_cause(CCR, NewAcc);
char_attr_total_octets(#{"serviceRating" := ServiceRating} = NrfRequest, Acc)
		when length(ServiceRating) > 0 ->
	Fold = fun(#{"consumedUnit" := #{"totalVolume" := Volume}}, Acc1) ->
				Volume + Acc1;
			(_, Acc1) ->
				Acc1
	end,
	NewAcc = case lists:foldl(Fold, 0, ServiceRating) of
		0 ->
			Acc;
		Volume1 ->
			[{struct, [{"name", "totalOctets"}, {"value", Volume1}]} | Acc]
	end,
	char_attr_cause(NrfRequest, NewAcc);
char_attr_total_octets(Request, Acc) ->
	char_attr_cause(Request, Acc).

%% @hidden
char_attr_input_giga_words(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctInputGigawords, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctInputGigawords"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_output_giga_words(Attributes, NewAcc).

%% @hidden
char_attr_output_giga_words(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctOutputGigawords, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctOutputGigawords"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_input_packets(Attributes, NewAcc).

%% @hidden
char_attr_input_packets(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctInputPackets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctInputPackets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_output_packets(Attributes, NewAcc).

%% @hidden
char_attr_output_packets(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctOutputPackets, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctOutputPackets"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_data_rate(Attributes, NewAcc).

%% @hidden
char_attr_data_rate(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AscendDataRate, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "ascendDataRate"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_xmit_rate(Attributes, NewAcc).

%% @hidden
char_attr_xmit_rate(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AscendXmitRate, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "ascendXmitRate"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_interim_interval(Attributes, NewAcc).

%% @hidden
char_attr_interim_interval(Attributes, Acc)
		when is_list(Attributes) ->
	NewAcc = case radius_attributes:find(?AcctInterimInterval, Attributes) of
		{ok, Value} ->
			[{struct, [{"name", "acctInterimInterval"}, {"value", Value}]} | Acc];
		{error, not_found} ->
			Acc
	end,
	char_attr_cause(Attributes, NewAcc).

%% @hidden
char_attr_cause(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'IMS-Information'
		= [#'3gpp_ro_IMS-Information'{'Cause-Code'
		= [Cause]}]}]}, Acc) ->
	[{struct, [{"name", "acctTerminateCause"},
			{"value", integer_to_list(Cause)}]} | Acc];
char_attr_cause(Attributes, Acc)
		when is_list(Attributes) ->
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
			NewAcc = [{struct, [{"name", "acctTerminateCause"}, {"value", Cause}]} | Acc],
			char_attr_user_location(Attributes, NewAcc);
		{error, not_found} ->
			char_attr_user_location(Attributes, Acc)
	end;
char_attr_cause(Request, Acc) ->
	char_attr_user_location(Request, Acc).

%% @hidden
char_attr_user_location(#'3gpp_ro_CCR'{'Service-Information'
		= [#'3gpp_ro_Service-Information'{'PS-Information'
		= [#'3gpp_ro_PS-Information'{'3GPP-User-Location-Info'
		= [Info]}]}]}, Acc) when is_binary(Info) ->
	[{struct, [{"name", "userLocationInfo"},
			{"value", base64:encode_to_string(Info)}]} | Acc];
char_attr_user_location(_, Acc) ->
	Acc.

%% @hidden
query_start(Type, Id, Query, Filters, RangeStart, RangeEnd) ->
	{DateStart, DateEnd} = case lists:keyfind("date", 1, Query) of
		{_, DateTime} when length(DateTime) > 3 ->
			ocs_rest:date_range(DateTime);
		false ->
			{1, erlang:system_time(millisecond)}
	end,
	query_start1(Type, lists:keyfind("type", 1, Query), Id, Query,
			Filters, RangeStart, RangeEnd, DateStart, DateEnd).
%% @hidden
query_start1(_Type, {_, "AAAAccessUsage"}, undefined, Query,
		Filters, RangeStart, RangeEnd, DateStart, DateEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex,
							[{"usageCharacteristic", contains, Contains}]}]}]} ->
						characteristic(Contains, '_', '_', '_', '_', 1)
				end;
			false ->
				{'_', '_', '_', '_'}
		end
	of
		{Protocol, Types, Request, Response} ->
			Args = [DateStart, DateEnd, Protocol, Types, Request, Response],
			MFA = [ocs_log, auth_query, Args],
			case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_:_ ->
			{error, 400}
	end;
query_start1(_Type, {_, "AAAAccountingUsage"}, undefined, Query,
		Filters, RangeStart, RangeEnd, DateStart, DateEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex,
							[{"usageCharacteristic", contains, Contains}]}]}]} ->
						characteristic(Contains, '_', '_', '_', '_', 1)
				end;
			false ->
				{'_', '_', '_', '_'}
		end
	of
		{Protocol, Types, Request, _Response} ->
			Args = [DateStart, DateEnd, Protocol, Types, Request],
			MFA = [ocs_log, acct_query, Args],
			case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_:_Reason1 ->
			{error, 400}
	end;
query_start1(Type, false, IpdrFile, Query,
		Filters, RangeStart, RangeEnd, DateStart, DateEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex,
							[{"usageCharacteristic", contains, Contains}]}]}]} ->
						ipdr_chars(Contains, '_')
				end;
			false ->
				'_'
		end
	of
		Chars ->
			Name = {name, {ipdr, erlang:unique_integer([positive])}},
			{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
			Directory1 = Directory ++ "/" ++ Type,
			case IpdrFile of
				IpdrFile when is_list(IpdrFile) ->
					FileName = {file, Directory1 ++ "/" ++ IpdrFile},
					Mode = {mode, read_only},
					{ok, {Log, A}} = disk_log:open([Name, FileName, Mode]),
					Args = [{Log, A}, DateStart, DateEnd, Chars],
					MFA = [ocs_log, ipdr_query, Args],
					case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
						{ok, PageServer, Etag} ->
							query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
						{error, _Reason} ->
							{error, 500}
					end;
				undefined ->
					{error, 404}
			end
	catch
		_:_ ->
			{error, 400}
	after
		case IpdrFile of
			IpdrFile when is_list(IpdrFile) ->
				disk_log:close("log/ipdr/" ++ Type ++ "/" ++ IpdrFile);
			undefined ->
				{error, 404}
		end
	end;
query_start1(_Type, {_, "HTTPTransferUsage"}, undefined, Query,
		Filters, RangeStart, RangeEnd, _DateStart, _DateEnd) ->
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
query_start1(_, {_, _}, _, [], _, _, _, _, _) ->
	{error, 404};
query_start1(_, {_, false}, _, _, _, _, _, _, _) ->
	{error, 400};
query_start1(_, {_, _}, _, _, _, _, _, _, _) ->
	{error, 400}.

%% @hidden
query_page(PageServer, Etag, Query, Filters, Start, End) ->
	case lists:keytake("type", 1, Query) of
		{_, {_, "AAAAccessUsage"}, Query1} ->
			query_page1(PageServer, Etag, auth, Query1, Filters, Start, End);
		{_, {_, "AAAAccountingUsage"}, Query1} ->
			query_page1(PageServer, Etag, acct, Query1, Filters, Start, End);
		{_, {_, "HTTPTransferUsage"}, Query1} ->
			query_page1(PageServer, Etag, http, Query1, Filters, Start, End);
		{_, {_, _}, []} ->
			{error, 404};
		{_, {_, _}, _} ->
			{error, 400};
		false ->
			query_page1(PageServer, Etag, ipdr, Query, Filters, Start, End)
	end.
%% @hidden
query_page1(PageServer, Etag, auth, [] = _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			ContentRange1 = case string:split(ContentRange, "/") of
				[Range, "*"] ->
					LogInfo = disk_log:info(ocs_auth),
					{_, Size} = lists:keyfind(no_items, 1, LogInfo),
					lists:concat([Range, "/",  Size]);
				_Other ->
					ContentRange
			end,
			try usage_aaa_auth(Events, Filters) of
				JsonObj ->
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange1}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end;
query_page1(PageServer, Etag, acct, [] = _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			ContentRange1 = case string:split(ContentRange, "/") of
				[Range, "*"] ->
					LogInfo = disk_log:info(ocs_acct),
					{_, Size} = lists:keyfind(no_items, 1, LogInfo),
					lists:concat([Range, "/",  Size]);
				_Other ->
					ContentRange
			end,
			try usage_aaa_acct(Events, Filters) of
				JsonObj ->
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange1}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end;
query_page1(PageServer, Etag, http, [] = _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			ContentRange1 = case string:split(ContentRange, "/") of
				[Range, "*"] ->
					Log = ocs_log:httpd_logname(transfer),
					LogInfo = disk_log:info(Log),
					{_, Size} = lists:keyfind(no_items, 1, LogInfo),
					lists:concat([Range, "/",  Size]);
				_Other ->
					ContentRange
			end,
			try usage_http_transfer(Events, Filters) of
				JsonObj ->
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange1}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end;
query_page1(PageServer, Etag, auth, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try usage_aaa_auth(Events, Filters) of
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
	end;
query_page1(PageServer, Etag, acct, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try usage_aaa_acct(Events, Filters) of
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
	end;
query_page1(PageServer, Etag, http, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try usage_http_transfer(Events, Filters) of
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
	end;
query_page1(PageServer, Etag, ipdr, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try usage_ipdr(Events, Filters) of
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
ipdr_chars([{complex, L1} | T], Chars) ->
	case lists:keytake("name", 1, L1) of
		{_, Name, L2} ->
			case lists:keytake("value", 1, L2) of
				{_, Value, []} ->
					ipdr_chars(Name, Value, T, Chars);
				_ ->
					throw({error, 400})
			end;
		false ->
			throw({error, 400})
	end;
ipdr_chars([], Chars) ->
	rev(Chars).
%% @hidden
ipdr_chars({"name", exact, "ipdrCreationTime"}, {"value", exact, IpdrCreationTime},
		T, Chars1) when is_list(IpdrCreationTime) ->
	Chars2 = add_char(Chars1, {ipdrCreationTime, {exact, IpdrCreationTime}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "ipdrCreationTime"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {ipdrCreationTime, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "username"}, {"value", exact, UserName},
		T, Chars1) when is_list(UserName) ->
	Chars2 = add_char(Chars1, {username, {exact, UserName}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "username"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {username, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "acctSessionId"}, {"value", exact, AcctSessionId},
		T, Chars1) when is_list(AcctSessionId) ->
	Chars2 = add_char(Chars1, {acctSessionId, {exact, AcctSessionId}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "acctSessionId"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {acctSessionId, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "callingStationId"}, {"value", exact, CallingStationId},
		T, Chars1) when is_list(CallingStationId) ->
	Chars2 = add_char(Chars1, {callingStationId, {exact, CallingStationId}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "callingStationId"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {callingStationId, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "calledStationId"}, {"value", exact, CalledStationId},
		T, Chars1) when is_list(CalledStationId) ->
	Chars2 = add_char(Chars1, {calledStationId, {exact, CalledStationId}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "calledStationId"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {calledStationId, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "nasIpAddress"}, {"value", exact, NasIpAddress},
		T, Chars1) when is_list(NasIpAddress) ->
	Chars2 = add_char(Chars1, {nasIpAddress, {exact, NasIpAddress}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "nasIpAddress"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {nasIpAddress, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "nasId"}, {"value", exact, NasId},
		T, Chars1) when is_list(NasId) ->
	Chars2 = add_char(Chars1, {nasId, {exact, NasId}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "nasId"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {nasId, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "locationType"}, {"value", exact, LocationType},
		T, Chars1) when is_list(LocationType) ->
	Chars2 = add_char(Chars1, {locationType, {exact, LocationType}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "locationType"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {locationType, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "networkConnectionType"}, {"value", exact, NetworkConnectionType},
		T, Chars1) when is_list(NetworkConnectionType) ->
	Chars2 = add_char(Chars1, {networkConnectionType, {exact, NetworkConnectionType}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "networkConnectionType"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {networkConnectionType, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "gmtSessionStartDateTime"}, {"value", exact, GmtSessionStartDateTime},
		T, Chars1) when is_list(GmtSessionStartDateTime) ->
	Chars2 = add_char(Chars1, {gmtSessionStartDateTime, {exact, GmtSessionStartDateTime}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "gmtSessionStartDateTime"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {gmtSessionStartDateTime, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "gmtSessionEndDateTime"}, {"value", exact, GmtSessionEndDateTime},
		T, Chars1) when is_list(GmtSessionEndDateTime) ->
	Chars2 = add_char(Chars1, {gmtSessionEndDateTime, {exact, GmtSessionEndDateTime}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "gmtSessionEndDateTime"}, {"value", like, Like},
		T, Chars1) when is_list(Like) ->
	Chars2 = add_char(Chars1, {gmtSessionEndDateTime, {like, like(Like)}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "chargeableQuantity"}, {"value", Op, ChargeableQuantity},
		T, Chars1)
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	Chars2 = add_char(Chars1, {chargeableQuantity, {Op, ChargeableQuantity}}),
	ipdr_chars(T, Chars2);
ipdr_chars({"name", exact, "taxAmount"}, {"value", Op, TaxAmount},
		T, Chars1)
		when Op == exact; Op == lt; Op == lte; Op == gt; Op == gte ->
	Chars2 = add_char(Chars1, {taxAmount, {Op, TaxAmount}}),
	ipdr_chars(T, Chars2).

-spec characteristic(Filters, Protocols, Types,
		MatchRequest, MatchResponse, VarNum) -> Result
	when
		Filters :: [Filter],
		Filter :: Simple | Complex | Array,
		Simple :: {LHS, Operator, RHS},
		LHS :: string(),
		Operator :: exact | notexact | like | lt | lte | gt | gte,
		RHS :: integer() | string() | boolean() | Complex | Filters,
		Complex :: {complex, Filters},
		Array :: {array, Filters},
		Protocols :: [ocs_log:protocol()] | '_',
		Types :: [ocs_log:acct_type()] | '_',
		MatchRequest :: [tuple()] | '_',
		MatchResponse :: [tuple()] | '_',
		VarNum :: pos_integer(),
		Result :: {Protocols, Types, MatchRequest, MatchResponse}.
%% @doc Construct a log event filter.
%%
%% 	`Filters' is as defined in
%% 	{@link //ocs/ocs_rest_query_parser. ocs_rest_query_parser}.
%%
%% 	`VarNum' is the next available number for a `MatchVariable'.
%%
%% 	`Result' is used in {@link //ocs/ocs_log:acct_query/6. ocs_log:acct_query/6}.
%%
%% @throws {error, 400 | 500}
%% @private
characteristic(Filter, '_' = _Protocols, Types,
		MatchRequest, MatchResponse, VarNum) ->
	Protocols = [radius, diameter, nrf],
	characteristic(Filter, Protocols, Types,
			MatchRequest, MatchResponse, VarNum);
characteristic([{complex, L1} | T] = _Filter,
		Protocols, Types, MatchRequest, MatchResponse, VarNum) ->
	case lists:keytake("name", 1, L1) of
		{value, {_, exact, Name}, L2} ->
			case lists:keytake("value", 1, L2) of
				{value, {_, Op, Value}, []} when Name == "protocol",
						((Op == exact) or (Op == notexact)
								or (Op == like) or (Op == notlike)) ->
					Protocols1 = characteristic_protocol(Op, Value),
					characteristic(T, Protocols1, Types,
							MatchRequest, MatchResponse, VarNum);
				{value, {_, Op, Value}, []} when Name == "type",
						((Op == exact) or (Op == notexact)
								or (Op == like) or (Op == notlike)) ->
					Types1 = characteristic_type(Op, Value),
					characteristic(T, Protocols, Types1,
							MatchRequest, MatchResponse, VarNum);
				{value, {_, Op, Value}, []}
						when Op == exact; Op == notexact;
								Op == like; Op == notlike;
								Op == lt; Op == lte;
								Op == gt; Op == gte ->
						{Protocols1, MatchRequest1, MatchResponse1, VarNum1}
								= characteristic1(Name, Op, Value,
										Protocols, Protocols, MatchRequest,
										MatchResponse, VarNum),
						characteristic(T, Protocols1, Types,
								MatchRequest1, MatchResponse1, VarNum1);
				_Reason ->
					throw({error, 400})
			end;
		false ->
			throw({error, 400})
	end;
characteristic([], Protocol, Types,
		MatchRequest, MatchResponse, _N) ->
	{Protocol, rev(Types), rev(MatchRequest), rev(MatchResponse)}.
%% @hidden
characteristic1(Name, Operator, Value, [Protocol | T], Protocols,
		MatchRequest, MatchResponse, VarNum) ->
	{Protocols1, MatchRequest1, MatchResponse1, VarNum1}
			= characteristic2(Name, Operator, Value, Protocol, Protocols,
					MatchRequest, MatchResponse, VarNum),
	characteristic1(Name, Operator, Value, T, Protocols1,
			MatchRequest1, MatchResponse1, VarNum1);
characteristic1(_Name, _Operator, _Value, [], Protocols,
		MatchRequest, MatchResponse, VarNum) ->
	{Protocols, MatchRequest, MatchResponse, VarNum}.

-spec characteristic2(Name, Operator, Value, Protocol, Protocols,
		MatchRequest, MatchResponse, VarNum) -> Result
	when
		Name :: string(),
		Operator :: exact | notexact | like | notlike | lt | lte | gt | gte,
		Value :: integer() | string() | boolean(),
		Protocol :: ocs_log:protocol(),
		Protocols :: [Protocol],
		MatchRequest :: [tuple()] | '_',
		MatchResponse :: [tuple()] | '_',
		VarNum :: non_neg_integer(),
		Result :: {Protocols, MatchRequest, MatchResponse, VarNum}.
%% @doc Construct a Match for a Characteristic.
%% @throws {error, 400}
%% @hidden
characteristic2("username" = _Name, exact = _Operator, UserName,
		radius, Protocols, Request, Response, VarNum) when is_list(UserName) ->
	Request1 = add_char(Request, {?UserName, {exact, UserName}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("username", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?UserName, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("username", exact, UserName,
		diameter, Protocols, Request, Response, VarNum) when is_list(UserName) ->
	UserName1 = list_to_binary(UserName),
	CCR = #'3gpp_ro_CCR'{'User-Name' = [UserName1], _ = '_'},
	Request1 = merge({CCR, []}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("username", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = list_to_binary(hd(like(Like))),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	VarMatch = build_var_match(VarNum),
	CCR = #'3gpp_ro_CCR'{'User-Name' = [VarMatch], _ = '_'},
	MatchCond = [{'=<', Prefix, VarMatch}, {'>', Prefix1, VarMatch}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("imsi", _Op, _Value, radius, Protocols, Request, Response, VarNum) ->
	Protocols1 = lists:delete(radius, Protocols),
	{Protocols1, Request, Response, VarNum};
characteristic2("imsi", exact, IMSI,
		diameter, Protocols, Request, Response, VarNum) when is_list(IMSI) ->
	IMSI1 = list_to_binary(IMSI),
	VarMatch = build_var_match(VarNum),
	TypeField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
	DataField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Data',
	TypeValue = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
	CCR = #'3gpp_ro_CCR'{'Subscription-Id' = VarMatch,  _ = '_'},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=:=', {element, TypeField, {hd, VarMatch}}, TypeValue},
					{'=:=', {element, DataField, {hd, VarMatch}}, IMSI1}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=:=', {element, TypeField, {hd, {tl, VarMatch}}}, TypeValue},
					{'=:=', {element, DataField, {hd, {tl, VarMatch}}}, IMSI1}}}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("imsi", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = list_to_binary(hd(like(Like))),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	VarMatch = build_var_match(VarNum),
	TypeField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
	DataField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Data',
	TypeValue = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
	CCR = #'3gpp_ro_CCR'{'Subscription-Id' = VarMatch,  _ = '_'},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=:=', {element, TypeField, {hd, VarMatch}}, TypeValue},
					{'=<', Prefix, {element, DataField, {hd, VarMatch}}},
					{'>', Prefix1, {element, DataField, {hd, VarMatch}}}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=:=', {element, TypeField, {hd, {tl, VarMatch}}}, TypeValue},
					{'=<', Prefix, {element, DataField, {hd, {tl, VarMatch}}}},
					{'>', Prefix1, {element, DataField, {hd, {tl, VarMatch}}}}}}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("msisdn", _Op, _Value, radius, Protocols, Request, Response, VarNum) ->
	Protocols1 = lists:delete(radius, Protocols),
	{Protocols1, Request, Response, VarNum};
characteristic2("msisdn", exact, MSISDN,
		diameter, Protocols, Request, Response, VarNum) when is_list(MSISDN) ->
	MSISDN1 = list_to_binary(MSISDN),
	VarMatch = build_var_match(VarNum),
	TypeField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
	DataField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Data',
	TypeValue = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
	CCR = #'3gpp_ro_CCR'{'Subscription-Id' = VarMatch,  _ = '_'},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=:=', {element, TypeField, {hd, VarMatch}}, TypeValue},
					{'=:=', {element, DataField, {hd, VarMatch}}, MSISDN1}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=:=', {element, TypeField, {hd, {tl, VarMatch}}}, TypeValue},
					{'=:=', {element, DataField, {hd, {tl, VarMatch}}}, MSISDN1}}}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("msisdn", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = list_to_binary(hd(like(Like))),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	VarMatch = build_var_match(VarNum),
	TypeField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Type',
	DataField = #'3gpp_ro_Subscription-Id'.'Subscription-Id-Data',
	TypeValue = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
	CCR = #'3gpp_ro_CCR'{'Subscription-Id' = VarMatch,  _ = '_'},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=:=', {element, TypeField, {hd, VarMatch}}, TypeValue},
					{'=<', Prefix, {element, DataField, {hd, VarMatch}}},
					{'>', Prefix1, {element, DataField, {hd, VarMatch}}}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=:=', {element, TypeField, {hd, {tl, VarMatch}}}, TypeValue},
					{'=<', Prefix, {element, DataField, {hd, {tl, VarMatch}}}},
					{'>', Prefix1, {element, DataField, {hd, {tl, VarMatch}}}}}}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("imsi", exact, IMSI,
		nrf, Protocols, Request, Response, VarNum) when is_list(IMSI) ->
	IMSI1 = "imsi-" ++ IMSI,
	VarMatch = build_var_match(VarNum),
	Nrf = #{"subscriptionId" => VarMatch},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=:=', {hd, VarMatch}, IMSI1}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=:=', {hd, {tl, VarMatch}}, IMSI1}}}],
	Request1 = merge({Nrf, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("imsi", like, Like,
		nrf, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = "imsi-" ++ hd(like(Like)),
	Pre = lists:droplast(Prefix),
	Char = lists:last(Prefix) + 1,
	Prefix1 = Pre ++ [Char],
	VarMatch = build_var_match(VarNum),
	Nrf = #{"subscriptionId" => VarMatch},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=<', Prefix, {hd, VarMatch}},
					{'>', Prefix1, {hd, VarMatch}}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=<', Prefix, {hd, {tl, VarMatch}}},
					{'>', Prefix1, {hd, {tl, VarMatch}}}}}],
	Request1 = merge({Nrf, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("msisdn", exact, MSISDN,
		nrf, Protocols, Request, Response, VarNum) when is_list(MSISDN) ->
	MSISDN1 = "msisdn-" ++ MSISDN,
	VarMatch = build_var_match(VarNum),
	Nrf = #{"subscriptionId" => VarMatch},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=:=', {hd, VarMatch}, MSISDN1}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=:=', {hd, {tl, VarMatch}}, MSISDN1}}}],
	Request1 = merge({Nrf, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("msisdn", like, Like,
		nrf, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = "msisdn-" ++ hd(like(Like)),
	Pre = lists:droplast(Prefix),
	Char = lists:last(Prefix) + 1,
	Prefix1 = Pre ++ [Char],
	VarMatch = build_var_match(VarNum),
	Nrf = #{"subscriptionId" => VarMatch},
	MatchCond = [{'or',
			{'andalso',
					{'>', {length, VarMatch}, 0},
					{'=<', Prefix, {hd, VarMatch}},
					{'>', Prefix1, {hd, VarMatch}}},
			{'andalso',
					{'>', {length, VarMatch}, 1},
					{'=<', Prefix, {hd, {tl, VarMatch}}},
					{'>', Prefix1, {hd, {tl, VarMatch}}}}}],
	Request1 = merge({Nrf, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("nasIpAddress", exact, NasIp,
		radius, Protocols, Request, Response, VarNum) when is_list(NasIp) ->
	{ok, NasIp1} = inet:parse_address(NasIp),
	Request1 = add_char(Request, {?NasIpAddress, {exact, NasIp1}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("nasIpAddress", exact, NasIp,
		diameter, Protocols, Request, Response, VarNum) when is_list(NasIp) ->
	{ok, Address} = inet:parse_address(NasIp),
	VarMatch1 = build_var_match(VarNum),
	VarMatch2 = build_var_match(VarNum + 1),
	PSI = #'3gpp_ro_PS-Information'{'SGSN-Address' = VarMatch1,
			'SGW-Address' = VarMatch2, _ = '_'},
	SI = #'3gpp_ro_Service-Information'{'PS-Information' = [PSI], _ = '_'},
	CCR = #'3gpp_ro_CCR'{'Service-Information' = [SI], _ = '_'},
	MatchCond = [{'or', {'==', [Address], [VarMatch1]},
			{'==', [Address], [VarMatch2]}}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 2};
characteristic2("nasPort", Op, NasPort,
		radius, Protocols, Request, Response, VarNum) when 
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(NasPort) ->
	Request1 = add_char(Request, {?NasPort, {Op, NasPort}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("serviceType", Op, ServiceType,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(ServiceType) ->
	Request1 = add_char(Request, {?ServiceType, {Op, ServiceType}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("serviceContextId", exact, Context,
		diameter, Protocols, Request, Response, VarNum) when is_list(Context) ->
	Context1 = list_to_binary(Context),
	CCR = #'3gpp_ro_CCR'{'Service-Context-Id' = Context1, _ = '_'},
	Request1 = merge({CCR, []}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedIpAddress", exact, FramedIp,
		radius, Protocols, Request, Response, VarNum) when is_list(FramedIp) ->
	{ok, FramedIp1} = inet:parse_address(FramedIp),
	Request1 = add_char(Request, {?FramedIpAddress, {exact, FramedIp1}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedPool", exact, FramedPool,
		radius, Protocols, Request, Response, VarNum) when is_list(FramedPool) ->
	Request1 = add_char(Request, {?FramedPool, {exact, FramedPool}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedPool", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?FramedPool, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedIpNetmask", exact, FramedIpNet,
		radius, Protocols, Request, Response, VarNum) when is_list(FramedIpNet) ->
	{ok, FramedIpNet1} = inet:parse_address(FramedIpNet),
	Request1 = add_char(Request, {?FramedIpNetmask, {exact, FramedIpNet1}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("filterId", exact, FilterId,
		radius, Protocols, Request, Response, VarNum) when is_list(FilterId) ->
	Request1 = add_char(Request, {?FilterId, {exact, FilterId}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("filterId", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?FilterId, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedMtu", Op, FramedMtu,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(FramedMtu) ->
	Request1 = add_char(Request, {?FramedMtu, {Op, FramedMtu}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedRoute", exact, FramedRoute,
		radius, Protocols, Request, Response, VarNum) when is_list(FramedRoute) ->
	Request1 = add_char(Request, {?FramedRoute, {exact, FramedRoute}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedRoute", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?FramedRoute, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedRouting", exact, "none",
		radius, Protocols, Request, Response, VarNum) ->
	Request1 = add_char(Request, {?FramedRouting, {exact, 0}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedRouting", exact, "send-routing-packets",
		radius, Protocols, Request, Response, VarNum) ->
	Request1 = add_char(Request, {?FramedRouting, {exact, 1}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedRouting", exact, "listen-for-routing-packets",
		radius, Protocols, Request, Response, VarNum) ->
	Request1 = add_char(Request, {?FramedRouting, {exact, 2}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("framedRouting", exact, "send-and-listen",
		radius, Protocols, Request, Response, VarNum) ->
	Request1 = add_char(Request, {?FramedRouting, {exact, 3}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("class", exact, Class,
		radius, Protocols, Request, Response, VarNum) when is_list(Class) ->
	Request1 = add_char(Request, {?Class, {exact, Class}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("class", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?Class, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("sessionTimeout", Op, SessionTimout,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(SessionTimout) ->
	Request1 = add_char(Request, {?SessionTimeout, {Op, SessionTimout}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("idleTimeout", Op, IdleTimeout,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(IdleTimeout) ->
	Request1 = add_char(Request, {?IdleTimeout, {Op, IdleTimeout}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("terminationAction", exact, "default",
		radius, Protocols, Request, Response, VarNum) ->
	Request1 = add_char(Request, {?TerminationAction, {exact, 0}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("terminationAction", exact, "aaa-request",
		radius, Protocols, Request, Response, VarNum) ->
	Request1 = add_char(Request, {?TerminationAction, {exact, 1}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("calledStationId", exact, CalledStation,
		radius, Protocols, Request, Response, VarNum) when is_list(CalledStation) ->
	Request1 = add_char(Request, {?CalledStationId, {exact, CalledStation}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("calledStationId", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?CalledStationId, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("calledStationId", exact, CalledStation,
		diameter, Protocols, Request, Response, VarNum) when is_list(CalledStation) ->
	CalledStation1 = list_to_binary(CalledStation),
	IMS = #'3gpp_ro_IMS-Information'{
			'Called-Party-Address' = [CalledStation1], _ = '_'},
	SI = #'3gpp_ro_Service-Information'{
			'IMS-Information' = [IMS], _ = '_'},
	CCR = #'3gpp_ro_CCR'{'Service-Information' = [SI], _ = '_'},
	Request1 = merge({CCR, []}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("calledStationId", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = list_to_binary(hd(like(Like))),
	VarMatch = build_var_match(VarNum),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	IMS = #'3gpp_ro_IMS-Information'{
			'Called-Party-Address' = [VarMatch], _ = '_'},
	SI = #'3gpp_ro_Service-Information'{
			'IMS-Information' = [IMS], _ = '_'},
	CCR = #'3gpp_ro_CCR'{'Service-Information' = [SI], _ = '_'},
	MatchCond = [{'=<', Prefix, VarMatch}, {'>', Prefix1, VarMatch}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("callingStationId", exact, CallingStation,
		diameter, Protocols, Request, Response, VarNum) when is_list(CallingStation) ->
	CallingStation1 = list_to_binary(CallingStation),
	IMS = #'3gpp_ro_IMS-Information'{
			'Calling-Party-Address' = [CallingStation1], _ = '_'},
	SI = #'3gpp_ro_Service-Information'{
			'IMS-Information' = [IMS], _ = '_'},
	CCR = #'3gpp_ro_CCR'{'Service-Information' = [SI], _ = '_'},
	Request1 = merge({CCR, []}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("callingStationId", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Prefix = list_to_binary(hd(like(Like))),
	VarMatch = build_var_match(VarNum),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	IMS = #'3gpp_ro_IMS-Information'{
			'Calling-Party-Address' = [VarMatch], _ = '_'},
	SI = #'3gpp_ro_Service-Information'{
			'IMS-Information' = [IMS], _ = '_'},
	CCR = #'3gpp_ro_CCR'{'Service-Information' = [SI], _ = '_'},
	MatchCond = [{'=<', Prefix, VarMatch}, {'>', Prefix1, VarMatch}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("nasIdentifier", exact, NasId,
		radius, Protocols, Request, Response, VarNum) when is_list(NasId) ->
	Request1 = add_char(Request, {?NasIdentifier, {exact, NasId}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("nasIdentifier", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?NasIdentifier, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("nasIdentifier", exact, NasId,
		diameter, Protocols, Request, Response, VarNum) when is_list(NasId) ->
	NasId1 = list_to_binary(NasId),
	CCR = #'3gpp_ro_CCR'{'Origin-Host' = NasId1, _ = '_'},
	Request1 = merge({CCR, []}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("nasIdentifier", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	VarMatch = build_var_match(VarNum),
	Prefix = list_to_binary(hd(like(Like))),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	CCR = #'3gpp_ro_CCR'{'Origin-Host' = VarMatch, _ = '_'},
	MatchCond = [{'=<', Prefix, VarMatch}, {'>', Prefix1, VarMatch}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("nasIdentifier", exact, NasId,
		nrf, Protocols, Request, Response, VarNum) when is_list(NasId) ->
	VarMatch = build_var_match(VarNum),
	Nrf = #{"nfConsumerIdentification" => VarMatch},
	MatchCond = [{'or',
			{'andalso',
					{is_map_key, "nFName", VarMatch},
					{'==', NasId, {map_get, "nFName", VarMatch}}},
			{'andalso',
					{is_map_key, "nodeFunctionality", VarMatch},
					{'==', NasId, {map_get, "nodeFunctionality", VarMatch}}}}],
	Request1 = merge({Nrf, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("nasIdentifier", like, Like,
		nrf, Protocols, Request, Response, VarNum) when is_list(Like) ->
	VarMatch = build_var_match(VarNum),
	Prefix = hd(like(Like)),
	Pre = lists:droplast(Prefix),
	Char = lists:last(Prefix) + 1,
	Prefix1 = Pre ++ [Char],
	Nrf = #{"nfConsumerIdentification" => VarMatch},
	MatchCond = [{'or',
			{'andalso',
					{is_map_key, "nFName", VarMatch},
					{'=<', Prefix, {map_get, "nFName", VarMatch}},
					{'>', Prefix1, {map_get, "nFName", VarMatch}}},
			{'andalso',
					{is_map_key, "nodeFunctionality", VarMatch},
					{'=<', Prefix, {map_get, "nodeFunctionality", VarMatch}},
					{'>', Prefix1, {map_get, "nodeFunctionality", VarMatch}}}}],
	Request1 = merge({Nrf, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("nasPortId", exact, NasPortId,
		radius, Protocols, Request, Response, VarNum) when is_list(NasPortId) ->
	Request1 = add_char(Request, {?NasPortId, {exact, NasPortId}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("nasPortId", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?NasPortId, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("nasPortType", Op, NasPortType,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(NasPortType) ->
	Request1 = add_char(Request, {?NasPortType, {Op, NasPortType}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("portLimit", Op, PortLimit,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(PortLimit) ->
	Request1 = add_char(Request, {?PortLimit, {Op, PortLimit}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctDelayTime", Op, AcctDelayTime,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AcctDelayTime) ->
	Request1 = add_char(Request, {?PortLimit, {Op, AcctDelayTime}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctSessionId", exact, AcctSessionId,
		radius, Protocols, Request, Response, VarNum) when is_list(AcctSessionId) ->
	Request1 = add_char(Request, {?AcctSessionId, {exact, AcctSessionId}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctSessionId", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?AcctSessionId, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctSessionId", exact, AcctSessionId,
		diameter, Protocols, Request, Response, VarNum) when is_list(AcctSessionId) ->
	AcctSessionId1 = list_to_binary(AcctSessionId),
	CCR = #'3gpp_ro_CCR'{'Session-Id' = AcctSessionId1, _ = '_'},
	Request1 = merge({CCR, []}, Request),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctSessionId", like, Like,
		diameter, Protocols, Request, Response, VarNum) when is_list(Like) ->
	VarMatch = build_var_match(VarNum),
	Prefix = list_to_binary(hd(like(Like))),
	Pre = binary:part(Prefix, 0, byte_size(Prefix) - 1),
	Byte = binary:last(Prefix) + 1,
	Prefix1 = <<Pre/binary, Byte>>,
	CCR = #'3gpp_ro_CCR'{'Session-Id' = VarMatch, _ = '_'},
	MatchCond = [{'=<', Prefix, VarMatch}, {'>', Prefix1, VarMatch}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2("acctMultiSessionId", exact, AcctMultiSessionId,
		radius, Protocols, Request, Response, VarNum) when is_list(AcctMultiSessionId) ->
	Request1 = add_char(Request, {?AcctMultiSessionId, {exact, AcctMultiSessionId}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctMultiSessionId", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?AcctMultiSessionId, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctLinkCount", Op, AcctLinkCount,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AcctLinkCount) ->
	Request1 = add_char(Request, {?AcctLinkCount, {Op, AcctLinkCount}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctAuthentic", exact, AcctAuth,
		radius, Protocols, Request, Response, VarNum) when is_list(AcctAuth) ->
	Request1 = add_char(Request, {?AcctAuthentic, {exact, AcctAuth}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctAuthentic", like, Like,
		radius, Protocols, Request, Response, VarNum) when is_list(Like) ->
	Request1 = add_char(Request, {?AcctAuthentic, {like, like(Like)}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctSessionTime", Op, AcctSessionTime,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AcctSessionTime) ->
	Request1 = add_char(Request, {?AcctSessionTime, {Op, AcctSessionTime}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctInputGigawords", Op, InputGiga,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(InputGiga) ->
	Request1 = add_char(Request, {?AcctInputGigawords, {Op, InputGiga}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctOutputGigawords", Op, OutputGiga,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(OutputGiga) ->
	Request1 = add_char(Request, {?AcctOutputGigawords, {Op, OutputGiga}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("inputOctets", Op, InputOctets,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(InputOctets) ->
	Request1 = add_char(Request, {?AcctInputPackets, {Op, InputOctets}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("outputOctets", Op, OutputOctets,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(OutputOctets) ->
	Request1 = add_char(Request, {?AcctOutputPackets, {Op, OutputOctets}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("ascendDataRate", Op, AscendDataRate,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AscendDataRate) ->
	Request1 = add_char(Request, {?AscendDataRate, {Op, AscendDataRate}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("ascendXmitRate", Op, AscendXmitRate,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AscendXmitRate) ->
	Request1 = add_char(Request, {?AscendXmitRate, {Op, AscendXmitRate}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctInterimInterval", Op, AcctInterimInterval,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AcctInterimInterval) ->
	Request1 = add_char(Request, {?AcctInterimInterval, {Op, AcctInterimInterval}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctTerminateCause", Op, AcctTerminateCause,
		radius, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AcctTerminateCause) ->
	Request1 = add_char(Request, {?AcctTerminateCause, {Op, AcctTerminateCause}}),
	{Protocols, Request1, Response, VarNum};
characteristic2("acctTerminateCause", Op, AcctTerminateCause,
		diameter, Protocols, Request, Response, VarNum) when
		((Op == exact) or ((Op == notexact))
		or (Op == lt) or (Op == lte)
		or (Op == gt) or (Op == gte)),
		is_integer(AcctTerminateCause) ->
	VarMatch = build_var_match(VarNum),
	IMS = #'3gpp_ro_IMS-Information'{'Cause-Code' = [VarMatch], _ = '_'},
	SI = #'3gpp_ro_Service-Information'{'IMS-Information' = [IMS], _ = '_'},
	CCR = #'3gpp_ro_CCR'{'Service-Information' = [SI], _ = '_'},
	MatchCond = [{operator(Op), VarMatch, AcctTerminateCause}],
	Request1 = merge({CCR, MatchCond}, Request),
	{Protocols, Request1, Response, VarNum + 1};
characteristic2(_Name, _Operator, _Value,
		_Protocol, _Protocols, _MatchRequest, _MatchResponse, _VarNum) ->
	throw({error, 400}).

%% @hidden
characteristic_protocol(like, [Like]) ->
	case lists:last(Like) of
		$% ->
			Prefix = lists:droplast(Like),
			F = fun({_, String}) ->
				lists:prefix(Prefix, String)
			end,
			Sprotocols = [{radius, "RADIUS"},
					{diameter, "DIAMETER"}, {nrf, "Nrf_Rating"}],
			Fprotocols = lists:filter(F, Sprotocols),
			[Protocol || {Protocol, _} <- Fprotocols];
		_ when Like == "RADIUS" ->
			[radius];
		_ when Like == "DIAMETER" ->
			[diameter];
		_ when Like == "Nrf_Rating" ->
			[nrf];
		_ ->
			throw({error, 400})
	end;
characteristic_protocol(notlike, [Like]) ->
	Protocols = [radius, diameter, nrf],
	case lists:last(Like) of
		$% ->
			Prefix = lists:droplast(Like),
			F = fun({_, String}) ->
				lists:prefix(Prefix, String)
			end,
			Sprotocols = [{radius, "RADIUS"},
					{diameter, "DIAMETER"}, {nrf, "Nrf_Rating"}],
			Fprotocols = lists:filter(F, Sprotocols),
			Protocols -- [Protocol || {Protocol, _} <- Fprotocols];
		_ when Like == "RADIUS" ->
			Protocols -- [radius];
		_ when Like == "DIAMETER" ->
			Protocols -- [diameter];
		_ when Like == "Nrf_Rating" ->
			Protocols -- [nrf];
		_ ->
			throw({error, 400})
	end;
characteristic_protocol(exact, "RADIUS") ->
	[radius];
characteristic_protocol(exact, "DIAMETER") ->
	[diameter];
characteristic_protocol(exact, "Nrf_Rating") ->
	[nrf];
characteristic_protocol(notexact, "RADIUS") ->
	[diameter, nrf];
characteristic_protocol(notexact, "DIAMETER") ->
	[radius, nrf];
characteristic_protocol(notexact, "Nrf_Rating") ->
	[radius, diameter];
characteristic_protocol(_Op, _Value) ->
	throw({error, 400}).

%% @hidden
characteristic_type(like, [Like]) ->
	case lists:last(Like) of
		$% ->
			Prefix = lists:droplast(Like),
			F = fun({_, String}) ->
				lists:prefix(Prefix, String)
			end,
			STypes = [{start, "start"}, {stop, "stop"},
					{interim, "interim"}, {event, "event"},
					{on, "on"}, {off, "off"}],
			FTypes = lists:filter(F, STypes),
			[Type || {Type, _} <- FTypes];
		_ when Like == "start" ->
			[start];
		_ when Like == "interim" ->
			[interim];
		_ when Like == "stop" ->
			[stop];
		_ when Like == "event" ->
			[event];
		_ when Like == "on" ->
			[on];
		_ when Like == "off" ->
			[off];
		_ ->
			throw({error, 400})
	end;
characteristic_type(notlike, [Like]) ->
	Types = [start, interim, stop, event, on, off],
	case lists:last(Like) of
		$% ->
			Prefix = lists:droplast(Like),
			F = fun({_, String}) ->
				lists:prefix(Prefix, String)
			end,
			STypes = [{start, "start"}, {stop, "stop"},
					{interim, "interim"}, {event, "event"},
					{on, "on"}, {off, "off"}],
			FTypes = lists:filter(F, STypes),
			Types -- [Type || {Type, _} <- FTypes];
		_ when Like == "start" ->
			[interim, stop, event, on, off];
		_ when Like == "interim" ->
			[start, stop, event, on, off];
		_ when Like == "stop" ->
			[start, interim, event, on, off];
		_ when Like == "event" ->
			[start, interim, stop, event, on, off];
		_ when Like == "on" ->
			[start, interim, stop, event, off];
		_ when Like == "off" ->
			[start, interim, stop, event, on];
		_ ->
			throw({error, 400})
	end;
characteristic_type(exact, "start") ->
	[start];
characteristic_type(exact, "interim") ->
	[interim];
characteristic_type(exact, "stop") ->
	[stop];
characteristic_type(exact, "event") ->
	[event];
characteristic_type(exact, "on") ->
	[on];
characteristic_type(exact, "off") ->
	[off];
characteristic_type(notexact, "start") ->
	[interim, stop, event, on, off];
characteristic_type(notexact, "interim") ->
	[start, interim, stop, event, on, off];
characteristic_type(notexact, "stop") ->
	[start, interim, event, on, off];
characteristic_type(notexact, "event") ->
	[start, interim, stop, on, off];
characteristic_type(notexact, "on") ->
	[start, interim, stop, event, off];
characteristic_type(notexact, "off") ->
	[start, interim, stop, event, on];
characteristic_type(_Op, _Value) ->
	throw({error, 400}).

build_var_match(N) ->
	VarN = integer_to_list(N),
	list_to_atom([$$] ++ VarN).

%% @hidden
like(Like) ->
	like(Like, []).
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

-spec merge(Match, Matches) -> Matches
	when
		Matches :: [Match] | '_',
		Match :: RadiusMatch | DiameterMatchSpec | NrfMatchSpec,
		RadiusMatch :: {Attribute, AttributeMatch},
		Attribute :: byte(),
		AttributeMatch :: tuple(),
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{}
				| #'3gpp_ro_RAR'{} | #'3gpp_ro_RAA'{}
				| #'3gpp_gx_CCR'{} | #'3gpp_gx_CCA'{}
				| #'3gpp_gx_RAR'{} | #'3gpp_gx_RAA'{},
		NrfMatchSpec :: {NrfMatchHead, MatchConditions},
		NrfMatchHead :: map(),
		MatchConditions :: [tuple()].
%% @doc Merge a `Match' with existing `Matches'.
%% @throws {error, 400}
%% @private
merge(Match, '_') when is_tuple(Match) ->
	[Match];
merge(Match, Matches) ->
	merge(Matches, Match, []).
%% @hidden
merge([{MatchHead2, _} | _] = Matches, {MatchHead1, _} = Match, Acc)
		when element(1, MatchHead1) == element(1, MatchHead2) ->
	merge1(Matches, Match, Acc, merge_record(MatchHead1, MatchHead2));
merge([{MatchHead2, _} | _] = Matches, {MatchHead1, _} = Match, Acc)
		when is_map(MatchHead1), is_map(MatchHead2) ->
	merge1(Matches, Match, Acc, merge_map(MatchHead1, MatchHead2));
merge([H | T], Match, Acc) ->
	merge(T, Match, [H | Acc]);
merge([], Match, Acc) ->
	lists:reverse([Match | Acc]).
%% @hidden
merge1([{_, MatchCond2} | T], {_, MatchCond1}, Acc, MatchHead3) ->
	MatchCond3 = MatchCond2 ++ MatchCond1,
	lists:reverse(Acc) ++ [{MatchHead3, MatchCond3}] ++ T.

%% @hidden
merge_record(MatchHead1, MatchHead2) ->
	MatchList1 = tl(tuple_to_list(MatchHead1)),
	MatchList2 = tl(tuple_to_list(MatchHead2)),
	MatchList3 = merge_record1(MatchList1, MatchList2, []),
	Name = element(1, MatchHead1),
	list_to_tuple([Name | MatchList3]).
%% @hidden
merge_record1([H | T1], [H | T2], Acc) ->
	merge_record1(T1, T2, [H | Acc]);
merge_record1(['_' | T1], [H | T2], Acc) ->
	merge_record1(T1, T2, [H | Acc]);
merge_record1([H | T1], ['_' | T2], Acc) ->
	merge_record1(T1, T2, [H | Acc]);
merge_record1([H1 | T1], [H2 | T2], Acc)
		when is_tuple(H1), is_tuple(H2) ->
	merge_record1(T1, T2, [merge_record(H1, H2) | Acc]);
merge_record1([[H1] | T1], [[H2] | T2], Acc)
		when is_tuple(H1), is_tuple(H2) ->
	merge_record1(T1, T2, [merge_record(H1, H2) | Acc]);
merge_record1([], [], Acc) ->
	lists:reverse(Acc);
merge_record1(_T1, _T2, _Acc) ->
	throw({error, 400}).

%% @hidden
merge_map(MatchHead1, MatchHead2) ->
	F = fun(_Key, _Value1, _Value2) ->
				throw({error, 400})
	end,
	maps:merge_with(F, MatchHead1, MatchHead2).

-spec operator(Operator) -> GuardFunction
	when
		Operator :: exact | notexact | lt | lte | gt | gte,
		GuardFunction ::  '=:=' | '=/=' | '>' | '=<' | '<' | '>='.
%% @doc Map query filter operatot to match specification guard function.
%%
%% 	An `Operator' is as defined in
%% 	{@link //ocs/ocs_rest_query_parser. ocs_rest_query_parser}.
%%
%% 	A `GuardFunction' is a term comparison guard function as
%% 	defined in {@link //stdlib/ets:match_spec(). match_spec()}.
%%
%% @private
operator(exact) ->
	'=:=';
operator(notexact) ->
	'=/=';
operator(lt) ->
	'<';
operator(lte) ->
	'=<';
operator(gt) ->
	'>';
operator(gte) ->
	'>='.

%% @hidden
rev('_') ->
	'_';
rev(Attributes) when is_list(Attributes) ->
	lists:reverse(Attributes).

