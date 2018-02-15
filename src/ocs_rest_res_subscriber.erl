%%% ocs_rest_res_subscriber.erl
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
-module(ocs_rest_res_subscriber).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_subscribers/2, get_subscriber/2, post_subscriber/1,
		patch_subscriber/4, delete_subscriber/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).
-define(subscriberPath, "/ocs/v1/subscriber/").

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

-spec get_subscriber(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber/{id}'
%% requests.
get_subscriber(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, _NewQuery} ->
			get_subscriber1(Id, Filters);
		false ->
			get_subscriber1(Id, [])
	end.
%% @hidden
get_subscriber1(Id, Filters) ->
	try
		case ocs:find_subscriber(Id) of
			{ok, Sub} ->
				Sub;
			{error, not_found} ->
				throw(404);
			{error, _Reason1} ->
				throw(500)
		end
	of
		#subscriber{last_modified = LM} = Subscriber1 ->
			Json = subscriber(Subscriber1),
			FilteredJson = case Filters of
				[] ->
					Json;
				Filters ->
					ocs_rest:fields(Filters, Json)
			end,
			Body = mochijson:encode(FilteredJson),
			Headers = [{content_type, "application/json"},
				{etag, ocs_rest:etag(LM)}],
			{ok, Headers, Body}
	catch
		throw:Status ->
			{error, Status};
		_:_ ->
			{error, 400}
	end.

-spec get_subscribers(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber'
%% requests.
get_subscribers(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_subscriber1(NewQuery, Filters, Headers);
		false ->
			get_subscriber1(Query, [], Headers)
	end.
%% @hidden
get_subscriber1(Query, Filters, Headers) ->
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
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(Query, Filters, undefined, undefined)
	end.

-spec post_subscriber(RequestBody) -> Result 
	when 
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/subscriber' and add a new `subscriber'
%% resource.
post_subscriber(RequestBody) ->
	try 
		#subscriber{name = Name, password = Password,
				attributes = Attributes, enabled = Enabled,
				multisession = Multi, buckets = Buckets1,
				product = Product} =
					subscriber(mochijson:decode(RequestBody)),
		Buckets2 =
			[B#bucket{id = generate_bucket_id()} || B <- Buckets1],
		{ProdID, Chars} = case Product of
			undefined ->
				{undefined, []};
			#product_instance{product = ProdId,
					characteristics = Characteristics} ->
				{ProdId, Characteristics}
		end,
		case catch ocs:add_subscriber(Name, Password,
				ProdID, Chars, Buckets2, Attributes, Enabled, Multi) of
			{ok, #subscriber{name = Id, last_modified = LM} = Subscriber} ->
				Json = subscriber(Subscriber),
				Body = mochijson:encode(Json),
				Location = ?subscriberPath ++ binary_to_list(Id),
				Headers = [{location, Location}, {etag, ocs_rest:etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_:_ ->
			{error, 400}
	end.

-spec patch_subscriber(Id, Etag, ContenType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | list(),
		ContenType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/subscriber/{id}' request and
%% Updates a existing `subscriber''s password or attributes. 
patch_subscriber(Id, Etag, "application/json-patch+json", ReqBody) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Etag1, mochijson:decode(ReqBody)}
	of
		{Etag2, Operations} ->
			case ocs:find_subscriber(Id) of
				{ok, #subscriber{last_modified = Etag3} = Sub} when
						Etag3 == Etag2; Etag2 == undefined; Etag3 == undefined ->
					case catch ocs_rest:patch(Operations, subscriber(Sub)) of
						{struct, _} = Json ->
							patch_subscriber1(Id, Json);
						_ ->
							{error, 400}
					end;
				{ok, _} ->
					{error, 412};
				{error, not_found} ->
					{error, 404};
				{error, _Reason1} ->
					{error, 500}
			end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
patch_subscriber1(Id, Json) ->
	try
		#subscriber{product = Product} = Subscriber = subscriber(Json),
		TS = erlang:system_time(?MILLISECOND),
		N = erlang:unique_integer([positive]),
		LM = {TS, N},
		F = fun() ->
			NewSub = Subscriber#subscriber{last_modified = LM,
				product = Product#product_instance{last_modified = LM}},
			mnesia:write(subscriber, NewSub, write)
		end,
		case mnesia:transaction(F) of
			{atomic, ok} ->
				Body = mochijson:encode(Json),
				Location = ?subscriberPath ++ Id,
				Headers = [{location, Location},
					{etag, ocs_rest:etag(LM)}],
				{ok, Headers, Body};
			{aborted, _} ->
				{error, 500}
		end
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec delete_subscriber(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /ocs/v1/subscriber/{id}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
delete_subscriber(Id) ->
	ok = ocs:delete_subscriber(Id),
	{ok, [], []}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

subscriber({struct, ObjectMembers}) ->
	subscriber(ObjectMembers, #subscriber{});
subscriber(#subscriber{} = Subscriber) ->
	{struct, subscriber(record_info(fields, subscriber),
		Subscriber, [])}.
%% @hidden
subscriber([{"id", Name} | T], Acc) ->
	subscriber(T, Acc#subscriber{name = list_to_binary(Name)});
subscriber([{"password", Password} | T], Acc) ->
	subscriber(T, Acc#subscriber{password = list_to_binary(Password)});
subscriber([{"attributes", {array, Attributes}} | T], Acc) ->
	subscriber(T, Acc#subscriber{attributes = json_to_radius(Attributes)});
subscriber([{"multisession", Multi} | T], Acc) ->
	subscriber(T, Acc#subscriber{multisession = Multi});
subscriber([{"enabled", Enabled} | T], Acc) ->
	subscriber(T, Acc#subscriber{enabled = Enabled});
subscriber([{"product", _} = Product | T], #subscriber{product = undefined} = Acc) ->
	subscriber(T, Acc#subscriber{product = product({struct, [Product]})});
subscriber([{"product", _} = Product | T], #subscriber{product = ProdInst} = Acc) ->
	#product_instance{product = ProdID} = product({struct, [Product]}),
	NewProdInst = ProdInst#product_instance{product = ProdID},
	subscriber(T, Acc#subscriber{product = NewProdInst});
subscriber([{"buckets", {array, Buckets}} | T], Acc) ->
	Buckets2 = [bucket(Bucket) || Bucket <- Buckets],
	subscriber(T, Acc#subscriber{buckets = Buckets2});
subscriber([{"totalBalance", _} | T], Acc) ->
	subscriber(T, Acc);
subscriber([{"characteristics", _} = Chars | T], #subscriber{product = undefined} = Acc) ->
	subscriber(T, Acc#subscriber{product = product({struct, [Chars]})});
subscriber([{"characteristics", _} = Chars | T], #subscriber{product = ProdInst} = Acc) ->
	#product_instance{characteristics = NewChars} = product({struct, [Chars]}),
	NewProdInst = ProdInst#product_instance{characteristics = NewChars},
	subscriber(T, Acc#subscriber{product = NewProdInst});
subscriber([_A | T], Acc) ->
	subscriber(T, Acc);
subscriber([], Acc) ->
	Acc.
%% @hidden
subscriber([name | T], #subscriber{name = Name} = Subscriber, Acc) ->
	Id = {"id", binary_to_list(Name)},
	Href = {"href", ?subscriberPath ++ binary_to_list(Name)},
	subscriber(T, Subscriber, [Href, Id | Acc]);
subscriber([password | T], #subscriber{password = Password} = Subscriber, Acc) ->
	subscriber(T, Subscriber, [{"password", binary_to_list(Password)} | Acc]);
subscriber([attributes | T], #subscriber{attributes = []} = Subscriber, Acc) ->
	subscriber(T, Subscriber, Acc);
subscriber([attributes | T], #subscriber{attributes = undefined} = Subscriber, Acc) ->
	subscriber(T, Subscriber, Acc);
subscriber([attributes | T], #subscriber{attributes = Attributes} = Subscriber, Acc) ->
	subscriber(T, Subscriber, [{"attributes", {array, radius_to_json(Attributes)}} | Acc]);
subscriber([buckets | T], #subscriber{buckets = Buckets} = Subscriber, Acc) ->
	Buckets1 = [bucket(Bucket) || Bucket <- Buckets],
	Buckets2 = [{"totalBalance", accumulated_balance(Buckets)},
		{"buckets", {array, Buckets1}}],
	subscriber(T, Subscriber, Buckets2 ++ Acc);
subscriber([product | T], #subscriber{product = #product_instance{} = Product} = Subscriber, Acc) ->
	{struct, Object} = product(Product),
	subscriber(T, Subscriber, Object ++ Acc);
subscriber([enabled | T], #subscriber{enabled = Enabled} = Subscriber, Acc) ->
	subscriber(T, Subscriber, [{"enabled", Enabled} | Acc]);
subscriber([disconnect | T], Subscriber, Acc) ->
	subscriber(T, Subscriber, Acc);
subscriber([session_attributes | T], Subscriber, Acc) ->
	subscriber(T, Subscriber, Acc);
subscriber([multisession | T], #subscriber{multisession = Multi} = Subscriber, Acc) ->
	subscriber(T, Subscriber, [{"multisession", Multi} | Acc]);
subscriber([_ | T], Subscriber, Acc) ->
	subscriber(T, Subscriber, Acc);
subscriber([], _Subscriber, Acc) ->
	Acc.

-spec product(ProdInst) -> ProdInst
	when
		ProdInst :: {struct, list()} | #product_instance{}.
%% @doc CODEC for product instance.
product({struct, ProdInst}) ->
	product(ProdInst, #product_instance{});
product(#product_instance{} = ProdInst) ->
	product(record_info(fields, product_instance), ProdInst, []).
%% @hidden
product([{"product", ProductID} | T], Acc) ->
	product(T, Acc#product_instance{product = ProductID});
product([{"characteristics", Chars} | T], Acc) ->
	product(T, Acc#product_instance{characteristics = characteristics(Chars)});
product([], Acc) ->
	Acc.
%% @hidden
product([product | T], #product_instance{product = ProdID} = ProdInst, Acc) ->
	product(T, ProdInst, [{"product", ProdID} | Acc]);
product([characteristics | T], #product_instance{characteristics = Chars} = ProdInst, Acc) ->
	product(T, ProdInst, [{"characteristics", characteristics(Chars)} | Acc]);
product([_ | T], ProdInst, Acc) ->
	product(T, ProdInst, Acc);
product([], _ProdInst, Acc) ->
	{struct, Acc}.

-spec characteristics(Characteristics) -> Characteristics
	when
		Characteristics :: {array, list()} | [tuple()].
%% @doc CODEC for Product characteristics.
characteristics({array, Characteristics}) ->
	characteristics(Characteristics, []);
characteristics(Characteristics) ->
	{array, characteristics(Characteristics, [])}.
%% @hidden
characteristics([{struct, [{"subscriberIdentity", Identity}]} | T], Acc) ->
	characteristics(T, [{"subscriberIdentity", Identity} | Acc]);
characteristics([{struct, [{"subscriberPassword", Password}]} | T], Acc) ->
	characteristics(T, [{"subscriberPassword", Password} | Acc]);
characteristics([{struct, [{"balanceTopUpDuration", BalanceTopUpDuration}]} | T], Acc) ->
	characteristics(T, [{"balanceTopUpDuration", topup_duration(BalanceTopUpDuration)} | Acc]);
characteristics([{struct, [{"radiusReserveTime", RadiusReserveTime}]} | T], Acc) ->
	characteristics(T, [{"radiusReserveTime", radius_reserve(RadiusReserveTime)} | Acc]);
characteristics([{struct, [{"radiusReserveOctets", RadiusReserveOctets}]} | T], Acc) ->
	characteristics(T, [{"radiusReserveOctets", radius_reserve(RadiusReserveOctets)} | Acc]);
characteristics([{struct, [{"radiusReserveSessionTime", SessionTime}]} | T], Acc) ->
	characteristics(T, [{"radiusReserveSessionTime", SessionTime} | Acc]);
characteristics([{"subscriberIdentity", Identity} | T], Acc) ->
	characteristics(T, [{struct, [{"subscriberIdentity", Identity}]} | Acc]);
characteristics([{"subscriberPassword", Password} | T], Acc) ->
	characteristics(T, [{struct, [{"subscriberPassword", Password}]} | Acc]);
characteristics([{"radiusReserveTime", RadiusReserveTime} | T], Acc) ->
	characteristics(T, [{struct, [{"radiusReserveTime", radius_reserve(RadiusReserveTime)}]} | Acc]);
characteristics([{"radiusReserveOctets", RadiusReserveOctets} | T], Acc) ->
	characteristics(T, [{struct, [{"radiusReserveOctets", radius_reserve(RadiusReserveOctets)}]} | Acc]);
characteristics([{"balanceTopUpDuration", Chars} | T], Acc) ->
	characteristics(T, [{struct, [{"balanceTopUpDuration", topup_duration(Chars)}]} | Acc]);
characteristics([{"radiusReserveSessionTime", SessionTime} | T], Acc) ->
	characteristics(T, [{struct, [{"radiusReserveSessionTime", SessionTime}]} | Acc]);
characteristics([], Acc) ->
	lists:reverse(Acc).

-spec topup_duration(BalanceTopUpDuration) -> BalanceTopUpDuration
	when
		BalanceTopUpDuration :: {struct, list()} | [tuple()].
%% @doc CODEC for top up duration characteristic
topup_duration({struct, [{"unitOfMeasure", Duration}, {"value", Amount}]}) ->
	[{unitOfMeasure, duration(Duration)}, {value, Amount}];
topup_duration({struct, [{"value", Amount}, {"unitOfMeasure", Duration}]}) ->
	[{unitOfMeasure, duration(Duration)}, {value, Amount}];
topup_duration([{unitOfMeasure, Duration}, {value, Amount}]) ->
	{struct, [{"unitOfMeasure", duration(Duration)}, {"value", Amount}]};
topup_duration([{value, Amount}, {unitOfMeasure, Duration}]) ->
	{struct, [{"unitOfMeasure", duration(Duration)}, {"value", Amount}]}.

-spec radius_reserve(RadiusReserve) -> RadiusReserve
	when
		RadiusReserve :: {struct, list()} | [tuple()].
%% @doc CODEC for top up duration characteristic
radius_reserve({struct, [{"unitOfMeasure", "seconds"}, {"value", Value}]}) ->
	[{type, seconds}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "seconds"}]}) ->
	[{type, seconds}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "minutes"}, {"value", Value}]}) ->
	[{type, minutes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "minutes"}]}) ->
	[{type, minutes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "bytes"}, {"value", Value}]}) ->
	[{type, bytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "bytes"}]}) ->
	[{type, bytes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "kilobytes"}, {"value", Value}]}) ->
	[{type, kilobytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "kilobytes"}]}) ->
	[{type, kilobytes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "megabytes"}, {"value", Value}]}) ->
	[{type, megabytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "megabytes"}]}) ->
	[{type, megabytes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "gigabytes"}, {"value", Value}]}) ->
	[{type, gigabytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "gigabytes"}]}) ->
	[{type, gigabytes}, {value, Value}];
%% @hidden
radius_reserve([{type, seconds}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "seconds"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, seconds}]) ->
	{struct, [{"unitOfMeasure", "seconds"}, {"value", Value}]};
radius_reserve([{type, minutes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "minutes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, minutes}]) ->
	{struct, [{"unitOfMeasure", "minutes"}, {"value", Value}]};
radius_reserve([{type, bytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "bytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, bytes}]) ->
	{struct, [{"unitOfMeasure", "bytes"}, {"value", Value}]};
radius_reserve([{type, kilobytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "kilobytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, kilobytes}]) ->
	{struct, [{"unitOfMeasure", "kilobytes"}, {"value", Value}]};
radius_reserve([{type, megabytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "megabytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, megabytes}]) ->
	{struct, [{"unitOfMeasure", "megabytes"}, {"value", Value}]};
radius_reserve([{type, gigabytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "gigabytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, gigabytes}]) ->
	{struct, [{"unitOfMeasure", "gigabytes"}, {"value", Value}]}.

-spec bucket(Buckets) -> Buckets
	when
		Buckets :: {struct, list()} | #bucket{}.
%% @doc CODEC for buckets
bucket({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	bucket(ObjectMembers, #bucket{});
bucket(#bucket{} = Bucket) ->
	bucket(record_info(fields, bucket), Bucket, []).
%% @hidden
bucket([{"name", Name} | T], Acc) when is_list(Name) ->
	bucket(T, Acc#bucket{name = Name});
bucket([{"id", Id} | T], Acc) when is_list(Id) ->
	bucket(T, Acc#bucket{id = Id});
bucket([{"startDate", SDate} | T], Acc) when is_list(SDate) ->
	bucket(T, Acc#bucket{id = ocs_rest:iso8601(SDate)});
bucket([{"terminationDate", TDate} | T], Acc) when is_list(TDate) ->
	bucket(T, Acc#bucket{id = ocs_rest:iso8601(TDate)});
bucket([{"units", Type} | T], Acc) when is_list(Type) ->
	bucket(T, Acc#bucket{units = units(Type)});
bucket([{"remainAmount", Amount} | T], Acc) ->
	case lists:keyfind("units", 1, T) of
			{_, "cents"} when is_list(Amount) ->
				bucket(T, Acc#bucket{remain_amount = convert(Amount)});
			_ ->
				bucket(T, Acc#bucket{remain_amount = Amount})
	end;
bucket([], Acc) ->
	Acc.
%% @hidden
bucket([id | T], #bucket{id = undefined} = Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([id | T], #bucket{id = ID} = Bucket, Acc) ->
	bucket(T, Bucket, [{"id", ID} | Acc]);
bucket([name | T], Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([units | T], #bucket{units = Type} = Bucket, Acc) ->
	bucket(T, Bucket, [{"units", units(Type)} | Acc]);
bucket([start_date | T], #bucket{start_date = undefined} = Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([start_date | T], #bucket{start_date = SDate} = Bucket, Acc) ->
	bucket(T, Bucket, [{"startDate", ocs_rest:iso8601(SDate)} | Acc]);
bucket([termination_date | T], #bucket{termination_date = undefined} = Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([termination_date | T], #bucket{termination_date = TDate} = Bucket, Acc) ->
	bucket(T, Bucket, [{"terminationDate", ocs_rest:iso8601(TDate)} | Acc]);
bucket([remain_amount | T], #bucket{units = cents, remain_amount = Amount} = Bucket, Acc) ->
	bucket(T, Bucket, [{"remainAmount", convert(Amount)} | Acc]);
bucket([remain_amount | T], #bucket{remain_amount = Amount} = Bucket, Acc) ->
	bucket(T, Bucket, [{"remainAmount", Amount} | Acc]);
bucket([reservations | T], Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([last_modified | T], Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([prices | T], Bucket, Acc) ->
	bucket(T, Bucket, Acc);
bucket([], _Bucket, Acc) ->
	{struct, Acc}.

%% @hidden
duration("seconds") -> "seconds";
duration("minutes") -> "minutes";
duration("days") -> "days";
duration("months") -> "months";
duration("years") -> "years";
duration(seconds) -> "seconds";
duration(minutes) -> "minutes";
duration(days) -> "days";
duration(months) -> "months";
duration(years) -> "years".

%% @hidden
json_to_radius(JsonObjList) ->
	json_to_radius(JsonObjList, []).
%% @hidden
json_to_radius([{struct, [{"name", "ascendDataRate"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T,Acc);
json_to_radius([{struct, [{"name", "ascendDataRate"}, {"value", V}]} | T], Acc) ->
	Attribute = {?VendorSpecific, {?Ascend, {?AscendDataRate, V}}},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct, [{"name", "ascendXmitRate"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T,Acc);
json_to_radius([{struct, [{"name", "ascendXmitRate"}, {"value", V}]} | T], Acc) ->
	Attribute = {?VendorSpecific, {?Ascend, {?AscendXmitRate, V}}},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct,[{"name","sessionTimeout"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T, Acc);
json_to_radius([{struct,[{"name","sessionTimeout"}, {"value", V}]} | T], Acc) ->
	Attribute = {?SessionTimeout, V},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct,[{"name","acctInterimInterval"}, {"value", V}]} | T], Acc) when V == null; V == ""->
	json_to_radius(T,Acc);
json_to_radius([{struct,[{"name","acctInterimInterval"}, {"value", V}]} | T], Acc) ->
	Attribute = {?AcctInterimInterval, V},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct,[{"name","class"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T, Acc);
json_to_radius([{struct,[{"name","class"}, {"value", V}]} | T], Acc) ->
	Attribute = {?Class, V},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct, [{"name", "vendorSpecific"} | VendorSpecific]} | T], Acc) ->
	case vendor_specific(VendorSpecific) of
		[] ->
			json_to_radius(T, Acc);
		Attribute ->
			json_to_radius(T, [Attribute | Acc])
	end;
json_to_radius([], Acc) ->
	Acc.

%% @hidden
radius_to_json(RadiusAttributes) ->
	radius_to_json(RadiusAttributes, []).
%% @hidden
radius_to_json([{?SessionTimeout, V} | T], Acc) ->
	Attribute = {struct, [{"name", "sessionTimeout"}, {"value",  V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?AcctInterimInterval, V} | T], Acc) ->
	Attribute = {struct, [{"name", "acctInterimInterval"}, {"value", V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?Class, V} | T], Acc) ->
	Attribute = {struct, [{"name", "class"}, {"value", V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, {?Ascend, {?AscendDataRate, V}}} | T], Acc) ->
	Attribute = {struct, [{"name", "ascendDataRate"}, {"value",  V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, {?Ascend, {?AscendXmitRate, V}}} | T], Acc) ->
	Attribute = {struct, [{"name", "ascendXmitRate"}, {"value",  V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, _} = H | T], Acc) ->
	Attribute = {struct, vendor_specific(H)},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([_| T], Acc) ->
	radius_to_json(T, Acc);
radius_to_json([], Acc) ->
	Acc.

%% @hidden
vendor_specific(AttrJson) when is_list(AttrJson) ->
	{_, Type} = lists:keyfind("type", 1, AttrJson),
	{_, VendorID} = lists:keyfind("vendorId", 1, AttrJson),
	{_, Key} = lists:keyfind("vendorType", 1, AttrJson),
	case lists:keyfind("value", 1, AttrJson) of
		{_, null} ->
			[];
		{_, Value} ->
			{Type, {VendorID, {Key, Value}}}
	end;
vendor_specific({?VendorSpecific, {VendorID, {VendorType, Value}}}) ->
	AttrObj = [{"name", vendorSpecific},
				{"vendorId", VendorID},
				{"vendorType", VendorType},
				{"value", Value}],
	{struct, AttrObj}.

-spec accumulated_balance(Buckets) ->	AccumulatedBalance
	when
		Buckets					:: [#bucket{}],
		AccumulatedBalance	:: tuple().
%% @doc return accumulated buckets as a json object.
accumulated_balance([]) ->
	[];
accumulated_balance(Buckets) ->
	accumulated_balance1(Buckets, []).
%% @hidden
accumulated_balance1([], AccBalance) ->
	F = fun({octets, {U1, A1}}, AccIn) ->
				Obj = {struct, [{"amount", A1}, {"units", U1}]},
				[Obj | AccIn];
			({cents, {U2, A2}}, AccIn) ->
				Obj = {struct, [{"amount", A2}, {"units", U2}]},
				[Obj | AccIn];
			({seconds, {U3, A3}}, AccIn) ->
				Obj = {struct, [{"amount", A3}, {"units", U3}]},
				[Obj | AccIn]
	end,
	JsonArray = lists:reverse(lists:foldl(F, [], AccBalance)),
	{array, JsonArray};
accumulated_balance1([Bucket | T], AccBalance) ->
	accumulated_balance1(T, accumulated_balance2(Bucket, AccBalance)).
%% @hidden
accumulated_balance2(#bucket{units = octets, remain_amount = Amount}, AccBalance) ->
	accumulated_balance3(octets, "octets", Amount, AccBalance);
accumulated_balance2(#bucket{units = cents, remain_amount = Amount}, AccBalance) ->
	accumulated_balance3(cents, "cents", Amount, AccBalance);
accumulated_balance2(#bucket{units = seconds, remain_amount = Amount}, AccBalance) ->
	accumulated_balance3(seconds, "seconds", Amount, AccBalance).
%% @hidden
accumulated_balance3(Key, Units, Amount, AccBalance) ->
	case lists:keytake(Key, 1, AccBalance) of
		{value, {Key, {Units, Balance}}, Rest} ->
			[{Key, {Units, Amount + Balance}} | Rest];
		false ->
			[{Key, {Units, Amount}} | AccBalance]
	end.

-spec units(Units) -> Units
	when
		Units :: string() | atom().
%% @doc CODEC for bucket type.
units("octets") -> octets;
units("cents") -> cents;
units("seconds") -> seconds;
units(octets) -> "octets";
units(cents) -> "cents";
units(seconds) -> "seconds".

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	case supervisor:start_child(ocs_rest_pagination_sup,
				[[ocs, query_subscriber, []]]) of
		{ok, PageServer, Etag} ->
			query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
query_page(PageServer, Etag, Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Subscribers, ContentRange} ->
			try
				case lists:keytake("sort", 1, Query) of
					{value, {_, "id"}, NewQuery} ->
						{lists:keysort(#subscriber.name, Subscribers), NewQuery};
					{value, {_, "-id"}, NewQuery} ->
						{lists:reverse(lists:keysort(#subscriber.name, Subscribers)), NewQuery};
					{value, {_, "password"}, NewQuery} ->
						{lists:keysort(#subscriber.password, Subscribers), NewQuery};
					{value, {_, "-password"}, NewQuery} ->
						{lists:reverse(lists:keysort(#subscriber.password, Subscribers)), NewQuery};
					{value, {_, "totalBalance"}, NewQuery} ->
						{lists:keysort(#subscriber.buckets, Subscribers), NewQuery};
					{value, {_, "-totalBalance"}, NewQuery} ->
						{lists:reverse(lists:keysort(#subscriber.buckets, Subscribers)), NewQuery};
					{value, {_, "product"}, NewQuery} ->
						{lists:keysort(#subscriber.product, Subscribers), NewQuery};
					{value, {_, "-product"}, NewQuery} ->
						{lists:reverse(lists:keysort(#subscriber.product, Subscribers)), NewQuery};
					{value, {_, "enabled"}, NewQuery} ->
						{lists:keysort(#subscriber.enabled, Subscribers), NewQuery};
					{value, {_, "-enabled"}, NewQuery} ->
						{lists:reverse(lists:keysort(#subscriber.enabled, Subscribers)), NewQuery};
					{value, {_, "multisession"}, NewQuery} ->
						{lists:keysort(#subscriber.multisession, Subscribers), NewQuery};
					{value, {_, "-multisession"}, NewQuery} ->
						{lists:reverse(lists:keysort(#subscriber.multisession, Subscribers)), NewQuery};
					false ->
						{Subscribers, Query};
					_ ->
						throw(400)
				end
			of
				{SortedEvents, _NewQuery} ->
					JsonObj = query_page1(lists:map(fun subscriber/1, SortedEvents), Filters, []),
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
query_page1(Json, [], []) ->
	Json;
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

%% @hidden
generate_bucket_id() ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

%% @hidden
convert(N) when is_list(N) ->
	case string:tokens(N, [$.]) of
		[A] ->
			list_to_integer(A) * 1000000;
		[A, B] when length(B) =< 6 ->
			list_to_integer(A)*1000000 +
					(list_to_integer(B ++ lists:duplicate(6 - length(B), $0)))
	end;
convert(N) when is_integer(N) ->
	M = N div 1000000,
	D = N rem 1000000,
	S = integer_to_list(M) ++ [$.] ++ integer_to_list(D),
	string:strip(S, right, $0).

