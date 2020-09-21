%%% ocs_rest_res_balance.erl
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
-module(ocs_rest_res_balance).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		top_up/2, top_up_service/2, get_balance/1, get_balance_service/1,
		get_balance_log/2, balance_adjustment/1]).

-export([get_bucket/1, get_buckets/2]).
-export([abmf/1, adjustment/1]).
-export([quantity/1]).
-export([bucket/1]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(bucketPath, "/balanceManagement/v1/bucket/").
-define(actionPath, "/balanceManagement/v1/balanceTransfer/").
-define(productInventoryPath, "/productInventoryManagement/v1/product/").
-define(balancePath, "/balanceManagement/v1/").

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

-spec get_balance_log(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers  :: [tuple()],
		Result :: {ok, Headers :: [tuple()],
			Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/log/balance'
%% requests.
get_balance_log(Query, _Headers) ->
	try
		{DateStart, DateEnd} = case lists:keyfind("date", 1, Query) of
			{_, DateTime} when length(DateTime) > 3 ->
				ocs_rest:range(DateTime);
			false ->
				{1, erlang:system_time(?MILLISECOND)}
		end,
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, _Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchType = match_abmf("type", Complex, Query),
						MatchSubscriber = match_abmf("subscriber", Complex, Query),
						MatchBucket = match_abmf("bucket", Complex, Query),
						MatchUnits = match_abmf("units", Complex, Query),
						MatchProducts = match_abmf("product", Complex, Query),
						case ocs_log:abmf_query(start, DateStart, DateEnd, MatchType,
								MatchSubscriber, MatchBucket, MatchUnits, MatchProducts) of
							{error, _} ->
								{error, 500};
							{_Cont, AbmfList} ->
								Json = abmfs(AbmfList, []),
								Body = mochijson:encode(Json),
								HeadersAbmf = [{content_type, "application/json"}],
								{ok, HeadersAbmf, Body}
						end;
					{error, _} ->
						{error, 500}
				end;
			false ->
				case ocs_log:abmf_query(start, DateStart, DateEnd, '_', '_', '_', '_', '_') of
					{error, _} ->
						{error, 500};
					{_Cont1, AbmfList1}  ->
						Json1 = abmfs(AbmfList1, []),
						Body = mochijson:encode(Json1),
						Headers1 = [{content_type, "application/json"}],
						{ok, Headers1, Body}
				end
		end
	catch
		_ ->
			{error, 400}
	end.

-spec get_bucket(BucketId) -> Result
	when
		BucketId :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /balanceManagment/v1/bucket/{id}',
get_bucket(BucketId) ->
	try
		case ocs:find_bucket(BucketId) of
			{ok, Bucket1} ->
				Bucket1;
			{error, Reason} ->
				exit(Reason)
		end
	of
		Bucket ->
			Body = mochijson:encode(bucket(Bucket)),
			Href = ?bucketPath ++ Bucket#bucket.id,
			Headers = [{location, Href},
					{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		_:not_found ->
			{error, 404};
		_:_ ->
			{error, 500}
	end.

-spec get_buckets(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers	:: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /balanceManagment/v1/bucket/',
get_buckets(Query, Headers) -> 
	try
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchId = match("id", Complex, Query),
						MatchProduct = match("product", Complex, Query),
						{Query1, [MatchId, MatchProduct]}
				end;
			false ->
				MatchId = match("id", [], Query),
				MatchProduct = match("product", [], Query),
				{Query, [MatchId, MatchProduct]}
		end
	of
		{Query2, Args} ->
			Codec = fun bucket/1,
			query_filter({ocs, query_bucket, Args}, Codec, Query2, Headers)
	catch
		 _ ->
			{error, 400}
	end.

-spec get_balance_service(Identity) -> Result
	when
		Identity :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% `GET /balanceManagment/v1/service/{id}/accumulatedBalance' request
get_balance_service(Identity) ->
	try
		case ocs:find_service(Identity) of
			{ok, #service{product = ProductRef}} ->
				case ocs:get_buckets(ProductRef) of
					Buckets1 when is_list(Buckets1) ->
						{ProductRef, Buckets1};
					{error, Reason} ->
						throw(Reason)
				end;
			{error, _} ->
				{error, 500}
		end
	of
		{ProductRef1, Buckets2} ->
			F1 = fun(#bucket{units = cents}) -> true; (_) -> false end,
			Buckets3 = lists:filter(F1, Buckets2),
			TotalAmount = lists:sum([B#bucket.remain_amount || B <- Buckets3]),
			F2 = fun(#bucket{id = Id}) ->
					{struct, [{"id", Id}, {"href", ?bucketPath ++ Id}]}
			end,
			Buckets4 = {"buckets", {array, lists:map(F2, Buckets3)}},
			Total = {"totalBalance", {struct,
					[{"amount", ocs_rest:millionths_out(TotalAmount)}]}},
			Id = {"id", Identity},
			Href = {"href", ?balancePath ++ "service/" ++ Identity ++ "/accumulatedBalance"},
			Product = {"product", {array, [{struct, [{"id", ProductRef1}, Href]}]}},
			Json = {struct, [Id, Href, Total, Buckets4, Product]},
			Body  = mochijson:encode(Json),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		_:product_not_found ->
			{error, 404};
		_Error ->
			{error, 400}
	end.
 
-spec get_balance(ProdRef) -> Result
	when
		ProdRef :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%%	`GET /balanceManagment/v1/product/{id}/accumulatedBalance' request
get_balance(ProdRef) ->
	try
		case ocs:get_buckets(ProdRef) of
			Buckets1 when is_list(Buckets1) ->
				Buckets1;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Buckets2 ->
			F1 = fun(#bucket{units = cents}) -> true; (_) -> false end,
			Buckets3 = lists:filter(F1, Buckets2),
			TotalAmount = lists:sum([B#bucket.remain_amount || B <- Buckets3]),
			F2 = fun(#bucket{id = Id}) ->
					{struct, [{"id", Id}, {"href", ?bucketPath ++ Id}]}
			end,
			Buckets4 = {"buckets", {array, lists:map(F2, Buckets3)}},
			Total = {"totalBalance", {struct,
					[{"amount", ocs_rest:millionths_out(TotalAmount)}]}},
			Id = {"id", ProdRef},
			Href = {"href", ?balancePath ++ "product/" ++ ProdRef ++ "/accumulatedBalance"},
			Product = {"product", {array, [{struct, [Id, Href]}]}},
			Json = {struct, [Id, Href, Total, Buckets4, Product]},
			Body  = mochijson:encode(Json),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		_:product_not_found ->
			{error, 404};
		_Error ->
			{error, 400}
	end.

-spec top_up_service(Identity, RequestBody) -> Result
	when
		Identity :: list(),
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /balanceManagement/v1/service/{id}/balanceTopup'
top_up_service(Identity, RequestBody) ->
	try
		bucket(mochijson:decode(RequestBody))
	of
		#bucket{} = Bucket ->
			case ocs:find_service(Identity) of
				{ok, #service{product = ProductRef}} ->
					case ocs:add_bucket(ProductRef, Bucket) of
						{ok, _, #bucket{id = Id} = B11} ->
							Body = mochijson:encode(bucket(B11)),
							Location = ?bucketPath ++ Id,
							Headers = [{location, Location}],
							{ok, Headers, Body};
						{error, _} ->
							{error, 500}
					end;
				{error, _} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end.

-spec top_up(Identity, RequestBody) -> Result
	when
		Identity :: list(),
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /balanceManagement/v1/product/{id}/balanceTopup'
top_up(Identity, RequestBody) ->
	try
		bucket(mochijson:decode(RequestBody))
	of
		#bucket{product = [], units = Units, remain_amount = RM} = B
				when Units /= undefined, RM > 0 ->
			case ocs:add_bucket(Identity, B#bucket{product = [Identity]}) of
				{ok, _, #bucket{id = Id} = B1} ->
					Body = mochijson:encode(bucket(B1)),
					Location = ?bucketPath ++ Id,
					Headers = [{location, Location}],
					{ok, Headers, Body};
				{error, _} ->
					{error, 500}
			end;
		#bucket{product = [Identity], units = Units, remain_amount = RM} = B
				when Units /= undefined, RM > 0 ->
			case ocs:add_bucket(Identity, B) of
				{ok, _, #bucket{id = Id} = B1} ->
					Body = mochijson:encode(bucket(B1)),
					Location = ?bucketPath ++ Id,
					Headers = [{location, Location}],
					{ok, Headers, Body};
				{error, _} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end.

-spec balance_adjustment(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /balanceManagement/v1/balanceAdjustment'
balance_adjustment(RequestBody) ->
	try
		adjustment(mochijson:decode(RequestBody))
	of
		#adjustment{} = Adjustment ->
			ok = ocs:adjustment(Adjustment),
			{ok, [], []};
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec units(Units) -> Units
	when
		Units :: string() | octets | cents | seconds | messages.
%% @doc Return the type of units of the bucket.
units(Units) when is_list(Units) ->
	units1(string:to_lower(Units));
units(Units) when is_atom(Units) ->
	units1(Units).
%% @hidden
units1("octets") -> octets;
units1("cents") -> cents;
units1("seconds") -> seconds;
units1("messages") -> messages;
units1(octets) -> "octets";
units1(cents) -> "cents";
units1(seconds) -> "seconds";
units1(messages) -> "messages".

-spec type(Type) -> Type
	when
		Type :: string() | deduct | reserve | unreserve | transfer | topup | adjustment.
%% @doc Return the type of abmf logs.
type(Type) when is_list(Type) ->
	type1(string:to_lower(Type));
type(Type) when is_atom(Type) ->
	type1(Type).
%% @hidden
type1("deduct") -> deduct;
type1("reserve") -> reserve;
type1("unreserve") -> unreserve;
type1("transfer") -> transfer;
type1("topup") -> topup;
type1("adjustment") -> adjustment;
type1(deduct) -> "deduct";
type1(reserve) -> "reserve";
type1(unreserve) -> "unreserve";
type1(transfer) -> "transfer";
type1(topup) -> "topup";
type1(adjustment) -> "adjustment".

-spec bucket(Bucket) -> Bucket
	when
		Bucket :: #bucket{} | {struct, list()}.
%% @doc CODEC for buckets
bucket({struct, Object}) ->
	bucket(Object, #bucket{});
bucket(#bucket{} = B) ->
	bucket(record_info(fields, bucket), B, []).
%% @hidden
bucket([{"id", ID} | T], Bucket) ->
	bucket(T, Bucket#bucket{id = ID});
bucket([{"name", Name} | T], Bucket) ->
	bucket(T, Bucket#bucket{name = Name});
bucket([{"amount", {struct, _} = Q} | T], Bucket) ->
	#quantity{amount = Amount, units = Units} = quantity(Q),
	bucket(T, Bucket#bucket{units = Units, remain_amount = Amount});
bucket([{"remainedAmount", {struct, _} = Q} | T], Bucket) ->
	#quantity{amount = Amount, units = Units} = quantity(Q),
	bucket(T, Bucket#bucket{units = Units, remain_amount = Amount});
bucket([{"product", {struct, P}} | T], Bucket) ->
	{_, ProdRef} = lists:keyfind("id", 1, P),
	bucket(T, Bucket#bucket{product = [ProdRef]});
bucket([{"validFor", {struct, L}} | T], Bucket) ->
	Bucket1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} ->
			Bucket#bucket{start_date = ocs_rest:iso8601(Start)};
		false ->
			Bucket
	end,
	Bucket2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} ->
			Bucket1#bucket{end_date = ocs_rest:iso8601(End)};
		false ->
			Bucket1
	end,
	bucket(T, Bucket2);
bucket([_ | T], Bucket) ->
	bucket(T, Bucket);
bucket([], Bucket) ->
	Bucket.
%% @hidden
bucket([id | T], #bucket{id = undefined} = B, Acc) ->
	bucket(T, B, Acc);
bucket([id | T], #bucket{id = ID} = B, Acc) ->
	bucket(T, B, [{"id", ID},
			{"href", ?bucketPath ++ ID} | Acc]);
bucket([name | T], #bucket{name = undefined} = B, Acc) ->
	bucket(T, B, Acc);
bucket([name | T], #bucket{name = Name} = B, Acc) ->
	bucket(T, B, [{"name", Name} | Acc]);
bucket([product | T], #bucket{product = [ProdRef]} = B, Acc) ->
	Id = {"id", ProdRef},
	Href = {"href", ?productInventoryPath ++ ProdRef},
	bucket(T, B, [{"product", {struct, [Id, Href]}} | Acc]);
bucket([remain_amount | T],
		#bucket{units = Units, remain_amount = Amount} =
		B, Acc) when is_integer(Amount) ->
	Q = #quantity{amount = Amount, units = Units},
	bucket(T, B, [{"remainedAmount", quantity(Q)} | Acc]);
bucket([reservations | T], #bucket{reservations = []} = B, Acc) ->
	bucket(T, B, Acc);
bucket([reservations | T], #bucket{units = undefined,
		reservations = Reservations} = B, Acc) ->
	Amount = lists:sum([A || {_, _, A, _} <- Reservations]),
	Reserved = [{"amount", Amount}],
	bucket(T, B, [{"reservedAmount", {struct, Reserved}}| Acc]);
bucket([reservations | T], #bucket{reservations = Reservations,
		units = cents} = B, Acc) ->
	Amount = lists:sum([A || {_, _, A, _} <- Reservations]),
	Reserved = [{"amount", ocs_rest:millionths_out(Amount)}, {"units", "cents"}],
	bucket(T, B, [{"reservedAmount", {struct, Reserved}}| Acc]);
bucket([reservations | T], #bucket{reservations = Reservations,
		units = Units} = B, Acc) ->
	Amount = lists:sum([A || {_, _, A, _} <- Reservations]),
	Reserved = [{"amount", Amount}, {"units", units(Units)}],
	bucket(T, B, [{"reservedAmount", {struct, Reserved}}| Acc]);
bucket([start_date | T], #bucket{start_date = undefined,
		end_date = End} = B, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	bucket(T, B, [{"validFor", ValidFor} | Acc]);
bucket([start_date | T], #bucket{start_date = Start,
		end_date = undefined} = B, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	bucket(T, B, [{"validFor", ValidFor} | Acc]);
bucket([start_date | T], #bucket{start_date = Start,
		end_date = End} = B, Acc) when is_integer(Start),
		is_integer(End)->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)},
			{"startDateTime", ocs_rest:iso8601(Start)}]},
	bucket(T, B, [{"validFor", ValidFor} | Acc]);
bucket([_ | T], B, Acc) ->
	bucket(T, B, Acc);
bucket([], _B, Acc) ->
	{struct, lists:reverse(Acc)}.

-spec adjustment(Adjustment) -> Adjustment
	when
		Adjustment :: #adjustment{} | {struct, list()}.
%% @doc CODEC for adjustments
adjustment({struct, Object}) when is_list(Object) ->
	adjustment(Object, #adjustment{});
adjustment(#adjustment{} = A) ->
	adjustment(record_info(fields, adjustment), A, []).
%% @hidden
adjustment([{"id", ID} | T], Adjustment) ->
	adjustment(T, Adjustment#adjustment{id = ID});
adjustment([{"type", Type} | T], Adjustment) ->
	adjustment(T, Adjustment#adjustment{type = Type});
adjustment([{"description", Description} | T], Adjustment) ->
	adjustment(T, Adjustment#adjustment{description = Description});
adjustment([{"reason", Reason} | T], Adjustment) ->
	adjustment(T, Adjustment#adjustment{reason = Reason});
adjustment([{"amount", {struct, _} = Q} | T], Adjustment) ->
	#quantity{amount = Amount, units = Units} = quantity(Q),
	adjustment(T, Adjustment#adjustment{units = Units, amount = Amount});
adjustment([{"product", {struct, P}} | T], Adjustment) ->
	{_, ProdRef} = lists:keyfind("id", 1, P),
	adjustment(T, Adjustment#adjustment{product = ProdRef});
adjustment([{"bucket", {struct, B}} | T], Adjustment) ->
	{_, BucketRef} = lists:keyfind("id", 1, B),
	adjustment(T, Adjustment#adjustment{bucket = BucketRef});
adjustment([{"validFor", {struct, L}} | T], Adjustment) ->
	Adjustment1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} ->
			Adjustment#adjustment{start_date = ocs_rest:iso8601(Start)};
		false ->
			Adjustment
	end,
	Adjustment2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} ->
			Adjustment1#adjustment{end_date = ocs_rest:iso8601(End)};
		false ->
			Adjustment1
	end,
	adjustment(T, Adjustment2);
adjustment([_ | T], Adjustment) ->
	adjustment(T, Adjustment);
adjustment([], Adjustment) ->
	Adjustment.
%% @hidden
adjustment([id | T], #adjustment{id = undefined} = A, Acc) ->
	adjustment(T, A, Acc);
adjustment([id | T], #adjustment{id = ID} = A, Acc) ->
	adjustment(T, A, [{"id", ID},
			{"href", ?bucketPath ++ ID} | Acc]);
%adjustment([name | T], #adjustment{name = undefined} = A, Acc) ->
%	adjustment(T, A, Acc);
adjustment([type | T], #adjustment{type = Type} = A, Acc) ->
	adjustment(T, A, [{"type", Type} | Acc]);
adjustment([description | T], #adjustment{description = Description} = A, Acc) ->
	adjustment(T, A, [{"description", Description} | Acc]);
adjustment([reason | T], #adjustment{reason = Reason} = A, Acc) ->
	adjustment(T, A, [{"reason", Reason} | Acc]);
adjustment([product | T], #adjustment{product = [ProdRef]} = A, Acc) ->
	Id = {"id", ProdRef},
	Href = {"href", ?productInventoryPath ++ ProdRef},
	adjustment(T, A, [{"product", {struct, [Id, Href]}} | Acc]);
adjustment([bucket | T], #adjustment{bucket = [BucketRef]} = A, Acc) ->
	Id = {"id", BucketRef},
	Href = {"href", ?productInventoryPath ++ BucketRef},
	adjustment(T, A, [{"bucket", {struct, [Id, Href]}} | Acc]);
adjustment([amount | T], #adjustment{units = Units, amount = Amount} = A, Acc)
		when is_integer(Amount) ->
	Q = #quantity{amount = Amount, units = Units},
	adjustment(T, A, [{"amount", quantity(Q)} | Acc]);
adjustment([start_date | T], #adjustment{start_date = undefined,
		end_date = End} = A, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	adjustment(T, A, [{"validFor", ValidFor} | Acc]);
adjustment([start_date | T], #adjustment{start_date = Start,
		end_date = undefined} = A, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	adjustment(T, A, [{"validFor", ValidFor} | Acc]);
adjustment([start_date | T], #adjustment{start_date = Start,
		end_date = End} = A, Acc) when is_integer(Start),
		is_integer(End)->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)},
			{"startDateTime", ocs_rest:iso8601(Start)}]},
	adjustment(T, A, [{"validFor", ValidFor} | Acc]);
adjustment([_ | T], A, Acc) ->
	adjustment(T, A, Acc);
adjustment([], _A, Acc) ->
	{struct, lists:reverse(Acc)}.

-spec abmfs(Abmf, Acc) -> Result
	when
		Abmf :: list(),
		Acc :: list(),
		Result :: {array, list()}.
%% @doc CODEC for list of abmf
% @hidden
abmfs([H | T], Acc) ->
	abmfs(T, [abmf(H) | Acc]);
abmfs([], Acc) ->
	{array, lists:reverse(Acc)}.

-spec abmf(Event) -> Json 
	when
		Event :: tuple(),
		Json :: {struct, list()}.
%% @doc CODEC for abmf
%% @hidden
abmf(Events) ->
	abmf0(Events, []).
%       lists:map(fun abmf0/2, Events).
%% @hidden
abmf0(Event, Acc) when element(1, Event) /= undefined ->
	Date = {"date", ocs_log:iso8601(element(1, Event))},
	abmf1(Event, [Date | Acc]);
abmf0(Event, Acc) ->
	abmf1(Event, Acc).
%% @hidden
abmf1(Event, Acc) when element(4, Event) /= undefined ->
	Type = {"type", type(element(4, Event))},
	abmf2(Event, [Type | Acc]);
abmf1(Event, Acc) ->
	abmf2(Event, Acc).
%% @hidden
abmf2(Event, Acc) when element(5, Event) /= undefined,
		is_list(element(5, Event)) ->
	Sub = {"subscriber", {struct,[{"id", element(5, Event)}]}},
	abmf3(Event, [Sub | Acc]);
abmf2(Event, Acc) when element(5, Event) /= undefined ->
	Sub = {"subscriber",
			{struct,[{"id", binary_to_list(element(5, Event))}]}},
	abmf3(Event, [Sub | Acc]);
abmf2(Event, Acc) ->
	abmf3(Event, Acc).
%% @hidden
abmf3(Event, Acc) when element(6, Event) /= undefined ->
	Bucket = element(6, Event),
	Bucket1 = {"bucketBalance", {struct, [{"id", Bucket},
			{"href", ?bucketPath ++ Bucket}]}},
	abmf4(Event, [Bucket1 | Acc]);
abmf3(Event, Acc) ->
	abmf4(Event, Acc).
%% @hidden
abmf4(Event, Acc) when element(7, Event) /= undefined,
		is_integer(element(9, Event)) ->
	Units = element(7, Event),
	Amount = element(9, Event),
	Amount1 = {"amount",	{struct,
			[{"units", units(Units)}, {"amount", Amount}]}},
	abmf5(Event, [Amount1 | Acc]);
abmf4(Event, Acc) ->
	abmf5(Event, Acc).
%% @hidden
abmf5(Event, Acc) when element(7, Event) /= undefined,
		is_integer(element(10, Event)) ->
	Units = element(7, Event),
	Amount = element(10, Event),
	AmountBefore1 = {"amountBefore",
			{struct, [{"units", units(Units)}, {"amount", Amount}]}},
	abmf6(Event, [AmountBefore1 | Acc]);
abmf5(Event, Acc) ->
	abmf6(Event, Acc).
%% @hidden
abmf6(Event, Acc) when element(7, Event) /= undefined,
		is_integer(element(10, Event)) ->
	Units = element(7, Event),
	Amount = element(11, Event),
	AmountAfter = {"amountAfter",
			{struct, [{"units", units(Units)}, {"amount", Amount}]}},
	abmf7(Event, [AmountAfter | Acc]);
abmf6(Event, Acc) ->
	abmf7(Event, Acc).
%% @hidden
abmf7(Event, Acc) when element(8, Event) /= undefined ->
	Product = element(8, Event),
	Product1 = {"product", {struct, [{"id", Product},
			{"href", ?productInventoryPath ++ Product}]}},
	abmf8(Event, [Product1 | Acc]);
abmf7(Event, Acc) ->
	abmf8(Event, Acc).
%% @hidden
abmf8(_Event, Acc) ->
	{struct, lists:reverse(Acc)}.

-spec quantity(Quantity) -> Quantity
	when
		Quantity :: {struct, list()} | #quantity{}.
%% @doc CODEC for quantity type
quantity({struct, [{"amount", Amount}, {"units", "cents"}]}) ->
	#quantity{units = cents, amount = ocs_rest:millionths_in(Amount)};
quantity({struct, [{"units", "cents"}, {"amount", Amount}]}) ->
	#quantity{units = cents, amount = ocs_rest:millionths_in(Amount)};
quantity({struct, [{"amount", Amount}, {"units", Units}]}) when is_list(Amount) ->
	quantity({struct, [{"units", Units}, {"amount", Amount}]});
quantity({struct, [{"amount", Amount}, {"units", Units}]}) ->
	quantity({struct, [{"units", Units}, {"amount", Amount}]});
quantity({struct, [{"units", Units}, {"amount", Amount}]}) when is_list(Amount)->
	Units1 = units(Units),
	case lists:last(Amount) of
		$b when Units1 == octets ->
			N = lists:sublist(Amount, length(Amount) - 1),
			#quantity{units = Units1, amount = list_to_integer(N)};
		$k when Units1 == octets ->
			N = lists:sublist(Amount, length(Amount) - 1),
			#quantity{units = Units1, amount = list_to_integer(N) * 1000};
		$m when Units1 == octets ->
			N = lists:sublist(Amount, length(Amount) - 1),
			#quantity{units = Units1, amount = list_to_integer(N) * 1000000};
		$g when Units1 == octets ->
			N = lists:sublist(Amount, length(Amount) - 1),
			#quantity{units = Units1, amount = list_to_integer(N) * 1000000000};
		$s when Units1 == seconds ->
			N = lists:sublist(Amount, length(Amount) - 1),
			#quantity{units = Units1, amount = list_to_integer(N)};
		_ ->
			#quantity{units = units(Units), amount = list_to_integer(Amount)}
	end;
quantity({struct, [{"units", Units}, {"amount", Amount}]}) ->
	#quantity{units = units(Units), amount = Amount};
quantity(#quantity{} = Quantity) ->
	{struct, quantity(record_info(fields, quantity),
			Quantity, [])}.
%% @hidden
quantity([amount | T], #quantity{units = cents, amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", ocs_rest:millionths_out(Amount)} | Acc]);
quantity([amount | T], #quantity{amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", integer_to_list(Amount) ++ "b"} | Acc]);
quantity([units | T], #quantity{units = undefined} = Q, Acc) ->
	quantity(T, Q, Acc);
quantity([units | T], #quantity{units = Units} = Q, Acc) ->
	quantity(T, Q, [{"units", units(Units)} | Acc]);
quantity([], _Q, Acc) ->
	lists:reverse(Acc).

%% @hidden
query_filter(MFA, Codec, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			query_filter(MFA, Codec, NewQuery, Filters, Headers);
		false ->
			query_filter(MFA, Codec, Query, [], Headers)
	end.
%% @hidden
query_filter(MFA, Codec, Query, Filters, Headers) ->
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
							query_page(Codec, PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(Codec, PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(MFA, Codec, Query, Filters, Start, End)
					end;
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(Codec, PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case ocs_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(MFA, Codec, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(MFA, Codec, Query, Filters, undefined, undefined)
	end.

%% @hidden
query_start({M, F, A}, Codec, Query, Filters, RangeStart, RangeEnd) ->
	case supervisor:start_child(ocs_rest_pagination_sup, [[M, F, A]]) of
		{ok, PageServer, Etag} ->
			query_page(Codec, PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
query_page(Codec, PageServer, Etag, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			JsonObj = query_page1(lists:map(Codec, Result), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
%% @hidden
query_page1(Json, [], []) ->
	Json;
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

%% @hidden
match(Key, Complex, Query) ->
	case lists:keyfind(Key, 1, Complex) of
		{_, like, [Value]} ->
			{like, Value};
		{_, exact, [Value]} ->
			{exact, Value};
		false ->
			case lists:keyfind(Key, 1, Query) of
				{_, Value} ->
					{exact, Value};
				false ->
					'_'
			end
	end.

%% @hidden
match_abmf(Key, Complex, _Query) ->
	case lists:keyfind(Key, 1, Complex) of
		{Obj, like, [Value]} ->
			[{list_to_atom(Obj), {like, [Value]}}];
		{Obj1, exact, Value} when Obj1 == "type" ->
			[{type, {exact, list_to_atom(Value)}}];
		{Obj1, exact, Value} ->
			[{list_to_atom(Obj1), {exact, Value}}];
		false ->
			'_'
	end.

