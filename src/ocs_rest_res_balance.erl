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
		top_up/2, top_up_service/2, get_balance/1, get_balance_log/0]).

-export([get_bucket/1, get_buckets/2]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(bucketPath, "/balanceManagement/v1/bucket/").
-define(actionPath, "/balanceManagement/v1/balanceTransfer/").
-define(productInventoryPath, "/productInventoryManagement/v1/product/").
-define(balancePath, "/balanceManagement/v1/accumulatedBalance/").

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

-spec get_balance_log() -> Result
	when
		Result :: {ok, Headers :: [tuple()],
			Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/log/balance'
%% requests.
get_balance_log() ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	try
		case ocs_log:abmf_open() of
			ok ->
				case ocs_log:last(ocs_abmf, MaxItems) of
					{error, _} ->
						{error, 404};
					{NewCount, Events} ->
						JsonObj = abmf_json(Events),
						JsonArray = {array, JsonObj},
						Body = mochijson:encode(JsonArray),
						ContentRange = "items 1-" ++ integer_to_list(NewCount) ++ "/*",
						Headers = [{content_type, "application/json"},
							{content_range, ContentRange}],
						{ok, Headers, Body}
				end;
			{error, _} ->
				{error, 404}
		end
	catch
		_:_ ->
			{error, 500}
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
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			query_buckets1(NewQuery, Filters, Headers);
		false ->
			query_buckets1(Query, [], Headers)
	end.
%% @hidden
query_buckets1(Query, Filters, Headers) ->
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
 
-spec get_balance(ProdRef) -> Result
	when
		ProdRef :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%%	`GET /balanceManagment/v1/accumulatedBalance/{id}' request
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
			Href = {"href", ?balancePath ++ ProdRef},
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
		#bucket{product = undefined, units = Units, remain_amount = RM} = B
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

% @hidden
abmf_json(Events) ->
	lists:map(fun abmf_json0/1, Events).
% @hidden
abmf_json0(Event) ->
	{struct, abmf_json0(Event, [])}.
%% @hidden
abmf_json0(Event, Acc) when element(1, Event) /= undefined ->
	Date = {"date", ocs_log:iso8601(element(1, Event))},
	abmf_json1(Event, [Date | Acc]);
abmf_json0(Event, Acc) ->
	abmf_json1(Event, Acc).
%% @hidden
abmf_json1(Event, Acc) when element(4, Event) /= undefined ->
	Type = {"type", element(4, Event)},
	abmf_json2(Event, [Type | Acc]);
abmf_json1(Event, Acc) ->
	abmf_json2(Event, Acc).
%% @hidden
abmf_json2(Event, Acc) when element(5, Event) /= undefined,
		is_list(element(5, Event)) ->
	Sub = {"subscriber", {struct,[{"id", element(5, Event)}]}},
	abmf_json3(Event, [Sub | Acc]);
abmf_json2(Event, Acc) when element(5, Event) /= undefined ->
	Sub = {"subscriber",
			{struct,[{"id", binary_to_list(element(5, Event))}]}},
	abmf_json3(Event, [Sub | Acc]);
abmf_json2(Event, Acc) ->
	abmf_json3(Event, Acc).
%% @hidden
abmf_json3(Event, Acc) when element(6, Event) /= undefined ->
	Bucket = element(6, Event),
	Bucket1 = {"bucketBalance", {struct, [{"id", Bucket},
			{"href", ?bucketPath ++ Bucket}]}},
	abmf_json4(Event, [Bucket1 | Acc]);
abmf_json3(Event, Acc) ->
	abmf_json4(Event, Acc).
%% @hidden
abmf_json4(Event, Acc) when element(7, Event) /= undefined,
		is_integer(element(9, Event)) ->
	Units = element(7, Event),
	Amount = element(9, Event),
	Amount1 = {"amount",	{struct,
			[{"units", units(Units)}, {"amount", Amount}]}},
	abmf_json5(Event, [Amount1 | Acc]);
abmf_json4(Event, Acc) ->
	abmf_json5(Event, Acc).
%% @hidden
abmf_json5(Event, Acc) when element(7, Event) /= undefined,
		is_integer(element(10, Event)) ->
	Units = element(7, Event),
	Amount = element(10, Event),
	AmountBefore1 = {"amountBefore",
			{struct, [{"units", units(Units)}, {"amount", Amount}]}},
	abmf_json6(Event, [AmountBefore1 | Acc]);
abmf_json5(Event, Acc) ->
	abmf_json6(Event, Acc).
%% @hidden
abmf_json6(Event, Acc) when element(7, Event) /= undefined,
		is_integer(element(10, Event)) ->
	Units = element(7, Event),
	Amount = element(11, Event),
	AmountAfter = {"amountAfter",
			{struct, [{"units", units(Units)}, {"amount", Amount}]}},
	abmf_json7(Event, [AmountAfter | Acc]);
abmf_json6(Event, Acc) ->
	abmf_json7(Event, Acc).
%% @hidden
abmf_json7(Event, Acc) when element(8, Event) /= undefined ->
	Product = element(8, Event),
	Product1 = {"product", {struct, [{"id", Product},
			{"href", ?productInventoryPath ++ Product}]}},
	abmf_json8(Event, [Product1 | Acc]);
abmf_json7(Event, Acc) ->
	abmf_json8(Event, Acc).
%% @hidden
abmf_json8(_Event, Acc) ->
	lists:reverse(Acc).

-spec quantity(Quantity) -> Quantity
	when
		Quantity :: {struct, list()} | #quantity{}.
%% @doc CODEC for quantity type
quantity({struct, [{"amount", Amount}, {"units", "cents"}]}) ->
	#quantity{units = cents, amount = ocs_rest:millionths_in(Amount)};
quantity({struct, [{"units", "cents"}, {"amount", Amount}]}) ->
	#quantity{units = cents, amount = ocs_rest:millionths_in(Amount)};
quantity({struct, [{"amount", Amount}, {"units", Units}]}) when is_list(Amount) ->
	#quantity{units = units(Units), amount = list_to_integer(Amount)};
quantity({struct, [{"amount", Amount}, {"units", Units}]}) ->
	#quantity{units = units(Units), amount = Amount};
quantity({struct, [{"units", Units}, {"amount", Amount}]}) when is_list(Amount) ->
	#quantity{units = units(Units), amount = list_to_integer(Amount)};
quantity({struct, [{"units", Units}, {"amount", Amount}]}) ->
	#quantity{units = units(Units), amount = Amount};
quantity(#quantity{} = Quantity) ->
	{struct, quantity(record_info(fields, quantity),
			Quantity, [])}.
%% @hidden
quantity([amount | T], #quantity{units = cents, amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", ocs_rest:millionths_out(Amount)} | Acc]);
quantity([amount | T], #quantity{amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", Amount} | Acc]);
quantity([units | T], #quantity{units = undefined} = Q, Acc) ->
	quantity(T, Q, Acc);
quantity([units | T], #quantity{units = Units} = Q, Acc) ->
	quantity(T, Q, [{"units", units(Units)} | Acc]);
quantity([], _Q, Acc) ->
	lists:reverse(Acc).

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]}]},
							{complex, [{"product", like, [Product]}]}]}]} ->
						{{like, Id}, {like, Product}};
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						{{like, Id}, '_'}
				end;
			false ->
				{'_', '_'}
		end
	of
		{MatchId, MatchProduct} ->
			MFA = [ocs, query_bucket, [MatchId, MatchProduct]],
				case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
					{ok, PageServer, Etag} ->
						query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
					{error, _Reason} ->
						{error, 500}
				end
	catch
		 _ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			JsonObj = query_page1(lists:map(fun bucket/1, Events), Filters, []),
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
%% @hidden
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

