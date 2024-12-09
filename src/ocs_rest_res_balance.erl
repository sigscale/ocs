%%% ocs_rest_res_balance.erl
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
-module(ocs_rest_res_balance).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		top_up/2, top_up_service/2, get_balance/1, get_balance/2,
		get_balance_service/1, get_balance_log/2, balance_adjustment/1]).
-export([delete_bucket/1]).
-export([get_bucket/1, get_buckets/2, head_bucket/0]).
-export([abmf/1, adjustment/1, bucket/1, acc_balance/1]).
-export([quantity/1]).

-include("ocs.hrl").

-define(bucketPath, "/balanceManagement/v1/bucket/").
-define(actionPath, "/balanceManagement/v1/balanceTransfer/").
-define(productInventoryPath, "/productInventoryManagement/v2/product/").
-define(serviceInventoryPath, "/serviceInventoryManagement/v2/service/").
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
	["application/json", "application/problem+json"].

-spec get_balance_log(Query, Headers) -> Result
	when
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Headers :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for `GET /ocs/v1/log/balance'
%% requests.
get_balance_log(Query, Headers) ->
	try
		{DateStart, DateEnd} = case lists:keyfind("date", 1, Query) of
			{_, DateTime} when length(DateTime) > 3 ->
				ocs_rest:date_range(DateTime);
			false ->
				{1, erlang:system_time(millisecond)}
		end,
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchType = match_abmf("type", Complex, Query1),
						MatchSubscriber = match_abmf("subscriber", Complex, Query1),
						MatchBucket = match_abmf("bucket", Complex, Query1),
						MatchUnits = match_abmf("units", Complex, Query1),
						MatchProducts = match_abmf("product", Complex, Query1),
						{Query, [DateStart, DateEnd, MatchType, MatchSubscriber, MatchBucket, MatchUnits, MatchProducts]}
				end;
			false ->
				MatchType = match_abmf("type", [], Query),
				MatchSubscriber = match_abmf("subscriber", [], Query),
				MatchBucket = match_abmf("bucket", [], Query),
				MatchUnits = match_abmf("units", [], Query),
				MatchProducts = match_abmf("product", [], Query),
				{Query, [DateStart, DateEnd, MatchType, MatchSubscriber, MatchBucket, MatchUnits, MatchProducts]}
		end
   of
      {Query2, Args} ->
         Codec = fun abmf/1,
         query_filter({ocs_log, abmf_query, Args}, Codec, Query2, Headers)
	catch
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing query"},
			{error, 400, Problem}
	end.

-spec delete_bucket(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `DELETE /balanceManagement/v1/bucket/{id}'
%% 	request to remove a `Balance Bucket'.
delete_bucket(Id) ->
	try ocs:delete_bucket(Id) of
		ok ->
			{ok, [], []}
	catch
		_:{not_found, _} ->
			Problem = #{type => "about:blank",
					title => "Not Found",
					detail => "No such Balance Bucket found"},
			{error, 404, Problem};
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception occurred deleting Balance Bucket"},
			{error, 500, Problem}
	end.

-spec get_bucket(BucketId) -> Result
	when
		BucketId :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for `GET /balanceManagement/v1/bucket/{id}'
%%    requests.
get_bucket(BucketId) ->
	try
		case ocs:find_bucket(BucketId) of
			{ok, Bucket1} ->
				Bucket1;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Bucket ->
			Body = mochijson:encode(bucket(Bucket)),
			Href = ?bucketPath ++ Bucket#bucket.id,
			Headers = [{location, Href},
					{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		not_found ->
			Problem = #{type => "about:blank",
					title => "Not Found",
					detail => "No such Balance Bucket found"},
			{error, 404, Problem};
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception occurred getting Balance Bucket"},
			{error, 500, Problem}
	end.

-spec head_bucket() -> Result
	when
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%%    `HEAD /balanceManagement/v1/bucket'
%%    requests.
head_bucket() ->
	try
		Size = mnesia:table_info(bucket, size),
		LastItem = integer_to_list(Size),
		ContentRange = "items 1-" ++ LastItem ++ "/" ++ LastItem,
		Headers = [{content_range, ContentRange}],
		{ok, Headers, []}
	catch
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception occurred getting Balance Bucket"},
			{error, 500, Problem}
	end.

-spec get_buckets(Query, Headers) -> Result
	when
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Headers	:: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for `GET /balanceManagement/v1/bucket/'
%%    requests.
get_buckets(Query, Headers) ->
	try
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchId = match("id", Complex, Query1),
						MatchProduct = match("product.id", Complex, Query1),
						{Query, [MatchId, MatchProduct]}
				end;
			false ->
				MatchId = match("id", [], Query),
				MatchProduct = match("product.id", [], Query),
				{Query, [MatchId, MatchProduct]}
		end
	of
		{Query2, Args} ->
			Codec = fun bucket/1,
			query_filter({ocs, query_bucket, Args}, Codec, Query2, Headers)
	catch
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing query"},
			{error, 400, Problem}
	end.

-spec get_balance_service(Identity) -> Result
	when
		Identity :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%% `GET /balanceManagement/v1/service/{id}/accumulatedBalance'
%%    requests.
get_balance_service(Identity) ->
	try
		case ocs:find_service(Identity) of
			{ok, #service{product = ProductRef}} ->
				case ocs:get_buckets(ProductRef) of
					Buckets1 when is_list(Buckets1) ->
						{ProductRef, Buckets1};
					{error, product_not_found} ->
						throw(product_not_found)
				end;
			{error, not_found} ->
				throw(service_not_found)
		end
	of
		{ProductRef1, Buckets2} ->
			{TotalBal, IdAcc}
					= calculate_total(Buckets2, {0, 0, 0, 0}, []),
			AccBalance = #acc_balance{id = Identity, total_balance = TotalBal,
					bucket = IdAcc, product = [ProductRef1]},
			Body  = mochijson:encode(acc_balance(AccBalance)),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		service_not_found ->
			Problem = #{type => "about:blank",
					title => "Not Found",
					detail => "No such Service inventory item found"},
			{error, 404, Problem};
		product_not_found ->
			Problem = #{type => "about:blank",
					title => "Not Found",
					detail => "No associated Product inventory item found"},
			{error, 404, Problem};
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception occurred getting accumulated balance for Service"},
			{error, 500, Problem}
	end.

-spec get_balance(ProdRef) -> Result
	when
		ProdRef :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%%	`GET /balanceManagement/v1/product/{id}/accumulatedBalance' request
%%    requests.
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
			{TotalBal, IdAcc}
					= calculate_total(Buckets2, {0, 0, 0, 0}, []),
			AccBalance = #acc_balance{id = ProdRef, product = [ProdRef],
					total_balance = TotalBal, bucket = IdAcc},
			Body  = mochijson:encode(acc_balance(AccBalance)),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		product_not_found ->
			Problem = #{type => "about:blank",
					title => "Not Found",
					detail => "No such Product inventory item found"},
			{error, 404, Problem};
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception occurred getting accumulated balance for Product"},
			{error, 500, Problem}
	end.

-spec get_balance(ProdRef, Query) -> Result
	when
		ProdRef :: string(),
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%%	`GET /balanceManagement/v1/product/{id}/accumulatedBalance'
%% with query request
get_balance(ProdRef, Query) ->
	try
		case ocs:get_buckets(ProdRef) of
			Buckets1 when is_list(Buckets1) ->
				Buckets1;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Buckets2 ->
			{_, Value} = lists:keyfind("totalBalance.units", 1, Query),
			Now = erlang:system_time(millisecond),
			Units = list_to_existing_atom(Value),
			F = fun(#bucket{units = U, end_date = EndDate})
							when EndDate == undefined; EndDate > Now, U == Units ->
						true;
					(_) ->
						false
			end,
			Buckets3 = lists:filter(F, Buckets2),
			TotalAmount = lists:sum([B#bucket.remain_amount || B <- Buckets3]),
			BucketIds = [B#bucket.id || B <- Buckets3],
			AccBalance = #acc_balance{id = ProdRef,
					total_balance = [#quantity{amount = TotalAmount, units = Units}],
					bucket = BucketIds, product = [ProdRef]},
			[{Condition, S}] = Query -- [{"totalBalance.units", Value}],
			ok = send_notification(Condition, TotalAmount,
					ocs_rest:millionths_in(S), AccBalance),
			Body  = mochijson:encode(acc_balance(AccBalance)),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		product_not_found ->
			Problem = #{type => "about:blank",
					title => "Not Found",
					detail => "No such Product inventory item found"},
			{error, 404, Problem};
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing query"},
			{error, 400, Problem}
	end.

-spec top_up_service(Identity, RequestBody) -> Result
	when
		Identity :: string(),
		RequestBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /balanceManagement/v1/service/{id}/balanceTopup'.
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
							Headers = [{content_type, "application/json"},
									{location, Location}],
							{ok, Headers, Body};
						{error, _} ->
							Problem = #{type => "about:blank",
									title => "Internal Server Error",
									detail => "Exception occurred adding Balance Bucket"},
							{error, 500, Problem}
					end;
				{error, not_found} ->
					Problem = #{type => "about:blank",
							title => "Not Found",
							detail => "No such Service inventory item found"},
					{error, 404, Problem}
			end;
		_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing request body"},
			{error, 400, Problem}
	catch
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing request body"},
			{error, 400, Problem}
	end.

-spec top_up(Identity, RequestBody) -> Result
	when
		Identity :: string(),
		RequestBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /balanceManagement/v1/product/{id}/balanceTopup'.
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
					Headers = [{content_type, "application/json"},
							{location, Location}],
					{ok, Headers, Body};
				{error, _} ->
					Problem = #{type => "about:blank",
							title => "Internal Server Error",
							detail => "Exception occurred adding Balance Bucket"},
					{error, 500, Problem}
			end;
		#bucket{product = [Identity], units = Units, remain_amount = RM} = B
				when Units /= undefined, RM > 0 ->
			case ocs:add_bucket(Identity, B) of
				{ok, _, #bucket{id = Id} = B1} ->
					Body = mochijson:encode(bucket(B1)),
					Location = ?bucketPath ++ Id,
					Headers = [{content_type, "application/json"},
							{location, Location}],
					{ok, Headers, Body};
				{error, _} ->
					Problem = #{type => "about:blank",
							title => "Internal Server Error",
							detail => "Exception occurred adding Balance Bucket"},
					{error, 500, Problem}
			end;
		_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing request body"},
			{error, 400, Problem}
	catch
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing request body"},
			{error, 400, Problem}
	end.

-spec balance_adjustment(RequestBody) -> Result
	when
		RequestBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /balanceManagement/v1/balanceAdjustment'.
balance_adjustment(RequestBody) ->
	try adjustment(mochijson:decode(RequestBody)) of
		#adjustment{} = Adjustment ->
			case ocs:adjustment(Adjustment) of
				ok ->
					{ok, [], []};
				{error, not_found} ->
					Problem = #{type => "about:blank",
							title => "Not Found",
							detail => "No such Product/Service inventory item found"},
					{error, 404, Problem};
				{error, _Reason} ->
					Problem = #{type => "about:blank",
							title => "Internal Server Error",
							detail => "Exception occurred applying Balance Adjustment"},
					{error, 500, Problem}
			end;
		_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing request body"},
			{error, 400, Problem}
	catch
		_:_ ->
			Problem = #{type => "about:blank",
					title => "Bad Request",
					detail => "Exception occurred parsing request body"},
			{error, 400, Problem}
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
		Type :: topup | adjustment | delete | deduct
				| reserve | unreserve | transfer | string().
%% @doc Return the type of abmf logs.
type(Type) when is_list(Type) ->
	type1(string:to_lower(Type));
type(Type) when is_atom(Type) ->
	type1(Type).
%% @hidden
type1("topup") -> topup;
type1("adjustment") -> adjustment;
type1("delete") -> delete;
type1("deduct") -> deduct;
type1("reserve") -> reserve;
type1("unreserve") -> unreserve;
type1("transfer") -> transfer;
type1(topup) -> "topup";
type1(adjustment) -> "adjustment";
type1(delete) -> "delete";
type1(deduct) -> "deduct";
type1(reserve) -> "reserve";
type1(unreserve) -> "unreserve";
type1(transfer) -> "transfer".

-spec bucket_status(Status) -> Status
   when
      Status :: atom() | string().
%% @doc CODEC for life cycle status of Bucket instance.
%% @private
bucket_status("Active") -> active;
bucket_status("Expired") -> expired;
bucket_status("Suspended") -> suspended;
bucket_status(active) -> "Active";
bucket_status(expired) -> "Expired";
bucket_status(suspended) -> "Suspended".

-spec bucket(Bucket) -> Bucket
	when
		Bucket :: #bucket{} | {struct, list()}.
%% @doc CODEC for buckets
bucket({struct, Object}) ->
	bucket(Object, #bucket{attributes = #{bucket_type => normal}});
bucket(#bucket{} = B) ->
	bucket(record_info(fields, bucket), ocs:parse_bucket(B), []).
%% @hidden
bucket([{"id", ID} | T], Bucket) when is_list(ID) ->
	bucket(T, Bucket#bucket{id = ID});
bucket([{"name", Name} | T], Bucket) when is_list(Name) ->
	bucket(T, Bucket#bucket{name = Name});
bucket([{"amount", {struct, _} = Q} | T], Bucket) ->
	#quantity{amount = Amount, units = Units} = quantity(Q),
	bucket(T, Bucket#bucket{units = Units, remain_amount = Amount});
bucket([{"lifecycleStatus", Status} | T], Bucket) ->
	bucket(T, Bucket#bucket{status = bucket_status(Status)});
bucket([{"remainedAmount", {struct, _} = Q} | T], Bucket) ->
	#quantity{amount = Amount, units = Units} = quantity(Q),
	bucket(T, Bucket#bucket{units = Units, remain_amount = Amount});
bucket([{"price", Price} | T], Bucket) when is_list(Price) ->
	bucket(T, Bucket#bucket{price = Price});
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
bucket([price | T], #bucket{price = []} = B, Acc) ->
	bucket(T, B, Acc);
bucket([price | T], #bucket{price = Price} = B, Acc) ->
	bucket(T, B, [{"price", Price} | Acc]);
bucket([product | T], #bucket{product = [ProdRef]} = B, Acc) ->
	Id = {"id", ProdRef},
	Href = {"href", ?productInventoryPath ++ ProdRef},
	bucket(T, B, [{"product", {struct, [Id, Href]}} | Acc]);
bucket([remain_amount | T],
		#bucket{units = Units, remain_amount = Amount} =
		B, Acc) when is_integer(Amount) ->
	Q = #quantity{amount = Amount, units = Units},
	bucket(T, B, [{"remainedAmount", quantity(Q)} | Acc]);
bucket([status | T], #bucket{status = Status} = B, Acc)
		when Status /= undefined ->
	StatusString = bucket_status(Status),
	bucket(T, B, [{"lifecycleStatus", StatusString} | Acc]);
bucket([attributes | T], #bucket{attributes = Attributes} = B, Acc)
		when false == is_map_key(reservations, Attributes) ->
	bucket(T, B, Acc);
bucket([attributes | T], #bucket{units = undefined,
		attributes = #{reservations := Reservations}} = B, Acc) ->
	ReservationList = maps:to_list(Reservations),
	Amount = lists:sum([A || {_, #{reserve := A}} <- ReservationList]),
	Reserved = [{"amount", Amount}],
	bucket(T, B, [{"reservedAmount", {struct, Reserved}}| Acc]);
bucket([attributes | T], #bucket{attributes = #{reservations := Reservations},
		units = cents} = B, Acc) ->
	ReservationList = maps:to_list(Reservations),
	Amount = lists:sum([A || {_, #{reserve := A}} <- ReservationList]),
	Reserved = [{"amount", ocs_rest:millionths_out(Amount)}, {"units", "cents"}],
	bucket(T, B, [{"reservedAmount", {struct, Reserved}}| Acc]);
bucket([attributes | T], #bucket{attributes = #{reservations := Reservations},
		units = Units} = B, Acc) ->
	ReservationList = maps:to_list(Reservations),
	Amount = lists:sum([A || {_, #{reserve := A}} <- ReservationList]),
	Reserved = [{"amount", Amount}, {"units", units(Units)}],
	bucket(T, B, [{"reservedAmount", {struct, Reserved}} | Acc]);
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
adjustment([{"service", {struct, S}} | T], Adjustment) ->
	{_, ServiceRef} = lists:keyfind("id", 1, S),
	adjustment(T, Adjustment#adjustment{service = ServiceRef});
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
adjustment([service | T], #adjustment{service = [ServiceRef]} = A, Acc) ->
	Id = {"id", ServiceRef},
	Href = {"href", ?serviceInventoryPath ++ ServiceRef},
	adjustment(T, A, [{"service", {struct, [Id, Href]}} | Acc]);
adjustment([bucket | T], #adjustment{bucket = [BucketRef]} = A, Acc) ->
	Id = {"id", BucketRef},
	Href = {"href", ?bucketPath ++ BucketRef},
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

-spec acc_balance(AccBalance) -> AccBalance
	when
		AccBalance :: #acc_balance{} | {struct, list()}.
%% @doc CODEC for acc_balance
acc_balance({struct, Object}) ->
	acc_balance(Object, #acc_balance{});
acc_balance(#acc_balance{} = AccBalance) ->
	acc_balance(record_info(fields, acc_balance), AccBalance, []).
%% @hidden
acc_balance([{"id", ID} | T], AccBalance) ->
	acc_balance(T, AccBalance#acc_balance{id = ID});
acc_balance([{"name", Name} | T], AccBalance) when is_list(Name) ->
	acc_balance(T, AccBalance#acc_balance{name = Name});
acc_balance([{"totalBalance", {array, Structs}} | T], AccBalance) ->
	acc_balance(T, AccBalance#acc_balance{
			total_balance = [quantity(Struct) || Struct <- Structs]});
acc_balance([{"product", {array, [{struct, ProdRefList}]}} | T], AccBalance) ->
	{_, ProdRef} = lists:keyfind("id", 1, ProdRefList),
	acc_balance(T, AccBalance#acc_balance{product = [ProdRef]});
acc_balance([{"buckets", {array, BucketRefStructs}} | T], AccBalance) ->
	F = fun({struct, BucketRefList}) ->
			{_, BucketId} = lists:keyfind("id", 1, BucketRefList),
			BucketId
	end,
	acc_balance(T, AccBalance#acc_balance{bucket
			= lists:map(F, BucketRefStructs)});
acc_balance([_ | T], AccBalance) ->
	acc_balance(T, AccBalance);
acc_balance([], AccBalance) ->
	AccBalance.
%% @hidden
acc_balance([id | T], #acc_balance{id = ProdRef, product = [ProdRef]} = AccBal,
		Acc) when ProdRef /= undefined ->
	acc_balance(T, AccBal, [{"id", ProdRef}, {"href", ?balancePath
			++ "product/" ++ ProdRef ++ "/accumulatedBalance"} | Acc]);
acc_balance([id | T], #acc_balance{id = ServiceId} = AccBal, Acc)
		when ServiceId /= undefined ->
	acc_balance(T, AccBal, [{"id", ServiceId}, {"href", ?balancePath
			++ "service/" ++ ServiceId ++ "/accumulatedBalance"} | Acc]);
acc_balance([name | T], #acc_balance{name = Name} = AccBal, Acc)
		when is_list(Name) ->
	acc_balance(T, AccBal, [{"name", Name} | Acc]);
acc_balance([total_balance | T], #acc_balance{total_balance = TotalBal} = AccBal,
		Acc) when is_list(TotalBal) ->
	acc_balance(T, AccBal, [{"totalBalance",
			{array, [quantity(Quantity) || Quantity <- TotalBal]}} | Acc]);
acc_balance([product | T], #acc_balance{product = [ProdRef],
		id = ProdRef} = AccBal, Acc) when is_list(ProdRef) ->
	Id = {"id", ProdRef},
	Href = {"href", ?balancePath ++ "product/"
			++ ProdRef ++ "/accumulatedBalance"},
	acc_balance(T, AccBal, [{"product", {array, [{struct, [Id, Href]}]}} | Acc]);
acc_balance([product | T], #acc_balance{product = [ProdRef],
		id = ServiceId} = AccBal, Acc) when is_list(ProdRef) ->
	Id = {"id", ProdRef},
	Href = {"href", ?balancePath ++ "service/"
			++ ServiceId ++ "/accumulatedBalance"},
	acc_balance(T, AccBal, [{"product", {array, [{struct, [Id, Href]}]}} | Acc]);
acc_balance([bucket | T], #acc_balance{bucket = BucketRefs} = AccBal, Acc)
		when is_list(BucketRefs) ->
	F = fun(BucketId) ->
			{struct, [{"id", BucketId}, {"href", ?bucketPath ++ BucketId}]}
	end,
	acc_balance(T, AccBal, [{"buckets",
			{array, lists:map(F, BucketRefs)}} | Acc]);
acc_balance([_ | T], B, Acc) ->
	acc_balance(T, B, Acc);
acc_balance([], _B, Acc) ->
	{struct, lists:reverse(Acc)}.

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
quantity({struct, [{"amount", Amount}, {"units", Units}]}) ->
	quantity({struct, [{"units", Units}, {"amount", Amount}]});
quantity({struct, [{"units", Units}, {"amount", Amount}]}) when is_list(Amount)->
	Units1 = units(Units),
	case lists:suffix("msg", Units) of
		true ->
			N = lists:sublist(Units, length(Units) - 3),
			#quantity{units = messages, amount = list_to_integer(N)};
		false ->
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
			end
	end;
quantity({struct, [{"units", Units}, {"amount", Amount}]}) ->
	#quantity{units = units(Units), amount = Amount};
quantity(#quantity{} = Quantity) ->
	{struct, quantity(record_info(fields, quantity),
			Quantity, [])}.
%% @hidden
quantity([amount | T], #quantity{units = cents, amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", ocs_rest:millionths_out(Amount)} | Acc]);
quantity([amount | T], #quantity{units = octets, amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", integer_to_list(Amount) ++ "b"} | Acc]);
quantity([amount | T], #quantity{units = seconds, amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", integer_to_list(Amount) ++ "s"} | Acc]);
quantity([amount | T], #quantity{units = messages, amount = Amount} = Q, Acc) ->
	quantity(T, Q, [{"amount", integer_to_list(Amount) ++ "msg"} | Acc]);
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
query_page(Codec, PageServer, Etag, [] = _Query, Filters, Start, End) ->
	{ok, Timeout} = application:get_env(ocs, rest_request_timeout),
	try gen_server:call(PageServer, {Start, End}, Timeout) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			ContentRange1 = case string:split(ContentRange, "/") of
				[Range, "*"] ->
					case erlang:fun_info(Codec, name) of
						{_, abmf} ->
							LogInfo = disk_log:info(ocs_abmf),
							{_, Size} = lists:keyfind(no_items, 1, LogInfo),
							lists:concat([Range, "/",  Size]);
						{_, bucket} ->
							Size = mnesia:table_info(bucket, size),
							lists:concat([Range, "/",  Size]);
						_Other ->
							ContentRange
					end;
				_Other ->
					ContentRange
			end,
			JsonObj = query_page1(lists:map(Codec, Result), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange1}],
			{ok, Headers, Body}
	catch
		_:{timeout, _} ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Timeout calling the pagination server"},
			{error, 500, Problem};
		_:_Reason ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception caught while calling the pagination server"},
			{error, 500, Problem}
	end;
query_page(Codec, PageServer, Etag, _Query, Filters, Start, End) ->
	{ok, Timeout} = application:get_env(ocs, rest_request_timeout),
	try gen_server:call(PageServer, {Start, End}, Timeout) of
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
	catch
		_:{timeout, _} ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Timeout calling the pagination server"},
			{error, 500, Problem};
		_:_Reason ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception caught while calling the pagination server"},
			{error, 500, Problem}
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

%% @hidden
send_notification("totalBalance.amount.lt", TotalAmount, Threshold, AccBalance)
		when TotalAmount < Threshold ->
	ocs_event:notify(accumulated, [AccBalance], balance);
send_notification(_, _TotalAmount, _Threshold, _AccBalance) ->
	ok.

%% @hidden
calculate_total(Buckets, TotalAcc, IdAcc) ->
	calculate_total(Buckets, TotalAcc, IdAcc, erlang:system_time(millisecond)).
%% @hidden
calculate_total([#bucket{id = Id, units = octets, remain_amount = RA,
		end_date = EndDate} | T], {TO, TC, TS, TM}, IdAcc, Now) when
		EndDate == undefined; EndDate > Now ->
	Now = erlang:system_time(millisecond),
	calculate_total(T, {TO + RA, TC, TS, TM}, [Id | IdAcc], Now);
calculate_total([#bucket{id = Id, units = cents, remain_amount = RA,
		end_date = EndDate} | T], {TO, TC, TS, TM}, IdAcc, Now) when
		EndDate == undefined; EndDate > Now ->
	calculate_total(T, {TO, TC + RA, TS, TM}, [Id | IdAcc], Now);
calculate_total([#bucket{id = Id, units = seconds, remain_amount = RA,
		end_date = EndDate} | T], {TO, TC, TS, TM}, IdAcc, Now) when
		EndDate == undefined; EndDate > Now ->
	calculate_total(T, {TO, TC, TS + RA, TM}, [Id | IdAcc], Now);
calculate_total([#bucket{id = Id, units = messages, remain_amount = RA,
		end_date = EndDate} | T], {TO, TC, TS, TM}, IdAcc, Now) when
		EndDate == undefined; EndDate > Now ->
	calculate_total(T, {TO, TC, TS, TM + RA}, [Id | IdAcc], Now);
calculate_total([_ | T], {TO, TC, TS, TM}, IdAcc, Now) ->
	calculate_total(T, {TO, TC, TS, TM}, IdAcc, Now);
calculate_total([], {TO, TC, TS, TM}, IdAcc, _Now) ->
	Totals = [{octets, TO}, {cents, TC}, {seconds, TS}, {messages, TM}],
	F = fun({octets, A}) when A > 0 ->
				{true, #quantity{units = octets, amount = A}};
			({cents, A}) when A > 0 ->
				{true, #quantity{units = cents, amount = A}};
			({seconds, A}) when A > 0 ->
				{true, #quantity{units = seconds, amount = A}};
			({messages, A}) when A > 0 ->
				{true, #quantity{units = messages, amount = A}};
			(_) ->
				false
	end,
	{lists:filtermap(F, Totals), IdAcc}.
