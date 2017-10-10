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
		get_subscribers/1, get_subscriber/2, post_subscriber/1,
		patch_subscriber/4, delete_subscriber/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

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
		{value, {_, L}, NewQuery} ->
			get_subscriber(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_subscriber(Id, Query, [])
	end.
%% @hidden
get_subscriber(Id, [] = _Query, Filters) ->
	get_subscriber1(Id, Filters);
get_subscriber(_Id, _Query, _Filters) ->
	{error, 400}.
%% @hidden
get_subscriber1(Id, Filters) ->
	case ocs:find_subscriber(Id) of
		{ok, #subscriber{password = PWBin, attributes = Attributes,
				buckets = Buckets, product = Product, enabled = Enabled,
				multisession = Multi, last_modified = LM}} ->
			Etag = etag(LM),
			Att = radius_to_json(Attributes),
			Att1 = {array, Att},
			Password = binary_to_list(PWBin),
			RespObj1 = [{"id", Id}, {"href", "/ocs/v1/subscriber/" ++ Id}],
			RespObj2 = [{"attributes", Att1}],
			RespObj3 = case Filters == []
				orelse lists:member("password", Filters) of
					true ->
						[{"password", Password}];
					false ->
						[]
				end,
			RespObj4 = case Filters == []
				orelse lists:member("totalBalance", Filters) of
					true ->
						AccBalance = accumulated_balance(Buckets),
						[{"totalBalance", AccBalance}];
					false ->
						[]
				end,
			RespObj5 = case Filters == []
				orelse lists:member("product", Filters) of
					true ->
						[{"product", Product#product_instance.product}];
					false ->
						[]
				end,
			RespObj6 = case Filters == []
				orelse lists:member("enabled", Filters) of
					true ->
						[{"enabled", Enabled}];
					false ->
						[]
				end,
			RespObj7 = case Filters == []
				orelse lists:member("multisession", Filters) of
					true ->
						[{"multisession", Multi}];
					false ->
						[]
				end,
			JsonObj  = {struct, RespObj1 ++ RespObj2 ++ RespObj3
					++ RespObj4 ++ RespObj5 ++ RespObj6 ++ RespObj7},
			Body = mochijson:encode(JsonObj),
			Headers = [{content_type, "application/json"}, {etag, Etag}],
			{ok, Headers, Body};
		{error, not_found} ->
			{error, 404}
	end.

-spec get_subscribers(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber'
%% requests.
get_subscribers(Query) ->
	case ocs:get_subscribers() of
		{error, _} ->
			{error, 404};
		Subscribers ->
			case lists:keytake("fields", 1, Query) of
				{value, {_, L}, NewQuery} ->
					get_subscribers(Subscribers, NewQuery, string:tokens(L, ","));
				false ->
					get_subscribers(Subscribers, Query, [])
			end
	end.
%% @hidden
get_subscribers(Subscribers, Query, Filters) ->
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
		{SortedSubscribers, NextQuery} ->
			get_subscribers1(SortedSubscribers, NextQuery, Filters)
	catch
		throw:400 ->
			{error, 400}
	end.
%% @hidden
get_subscribers1(Subscribers, Query, Filters) ->
	{Id, Query1} = case lists:keytake("id", 1, Query) of
		{value, {_, V1}, Q1} ->
			{V1, Q1};
		false ->
			{[], Query}
	end,
	{Password, Query2} = case lists:keytake("password", 1, Query1) of
		{value, {_, V2}, Q2} ->
			{V2, Q2};
		false ->
			{[], Query1}
	end,
	{Balance, Query3} = case lists:keytake("totalBalance", 1, Query2) of
		{value, {_, V3}, Q3} ->
			{V3, Q3};
		false ->
			{[], Query2}
	end,
	{Product, Query4} = case lists:keytake("product", 1, Query3) of
		{value, {_, V4}, Q4} ->
			{V4, Q4};
		false ->
			{[], Query3}
	end,
	{Enabled, Query5} = case lists:keytake("enabled", 1, Query3) of
		{value, {_, V5}, Q5} ->
			{V5, Q5};
		false ->
			{[], Query4}
	end,
	{Multi, Query6} = case lists:keytake("multisession", 1, Query4) of
		{value, {_, V6}, Q6} ->
			{V6, Q6};
		false ->
			{[], Query5}
	end,
	get_subscribers2(Subscribers, Id, Password, Balance, Product, Enabled, Multi, Query6, Filters).
%% @hidden
get_subscribers2(Subscribers, Id, Password, Balance, Product, Enabled, Multi, [] = _Query, Filters) ->
	F = fun(#subscriber{name = Na, password = Pa, attributes = Attributes, 
			buckets = Bu, product = Prod, enabled = Ena, multisession = Mul}) ->
		Nalist = binary_to_list(Na),
		T1 = lists:prefix(Id, Nalist),
		Palist = binary_to_list(Pa),
		T2 = lists:prefix(Password, Palist),
		Att = radius_to_json(Attributes),
		Att1 = {array, Att},
		T3 = lists:prefix(Balance, Bu),
		T4 = lists:prefix(Product, Prod#product_instance.product),
		T5 = lists:prefix(Enabled, atom_to_list(Ena)),
		T6 = lists:prefix(Multi, atom_to_list(Mul)),
		if
			T1 and T2 and T3 and T4 and T5 and T6 ->
				RespObj1 = [{"id", Nalist}, {"href", "/ocs/v1/subscriber/" ++ Nalist}],
				RespObj2 = [{"attributes", Att1}],
				RespObj3 = case Filters == []
						orelse lists:member("password", Filters) of
					true ->
						[{"password", Palist}];
					false ->
						[]
				end,
				RespObj4 = case Filters == []
						orelse lists:member("totalBalance", Filters) of
					true ->
						AccBalance = accumulated_balance(Bu),
						[{"totalBalance", AccBalance}];
					false ->
						[]
				end,
				RespObj5 = case Filters == []
						orelse lists:member("product", Filters) of
					true ->
						[{"product", Prod#product_instance.product}];
					false ->
						[]
				end,
				RespObj6 = case Filters == []
						orelse lists:member("enabled", Filters) of
					true ->
						[{"enabled", Ena}];
					false ->
						[]
				end,
				RespObj7 = case Filters == []
						orelse lists:member("multisession", Filters) of
					true ->
						[{"multisession", Mul}];
					false ->
						[]
				end,
				{true, {struct, RespObj1 ++ RespObj2 ++ RespObj3
							++ RespObj4 ++ RespObj5 ++ RespObj6 ++ RespObj7}};
			true ->
				false
		end
	end,
	try
		JsonObj = lists:filtermap(F, Subscribers),
		Size = integer_to_list(length(JsonObj)),
		ContentRange = "items 1-" ++ Size ++ "/" ++ Size,
		Body  = mochijson:encode({array, lists:reverse(JsonObj)}),
		{ok, [{content_type, "application/json"},
				{content_range, ContentRange}], Body}
	catch
		_:_Reason ->
			{error, 500}
	end;
get_subscribers2(_, _, _, _, _, _, _, _, _) ->
	{error, 400}.

-spec post_subscriber(RequestBody) -> Result 
	when 
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/subscriber' and add a new `subscriber'
%% resource.
post_subscriber(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		IdIn = proplists:get_value("id", Object),
		PasswordIn = proplists:get_value("password", Object),
		Product = proplists:get_value("product", Object),
		Attributes = case lists:keyfind("attributes", 1, Object) of
			{_, {array, JsonObjList}} ->
				json_to_radius(JsonObjList);
			false ->
				[]
		end,
		{Buckets, BucketRef} = case lists:keyfind("buckets", 1, Object) of
			{"buckets", {array, BktStruct}} ->
				F = fun({struct, Bucket}, AccIn) ->
					{_, Amount} = lists:keyfind("amount", 1, Bucket),
					{_, Units} = lists:keyfind("units", 1, Bucket),
					BucketType = bucket_type(Units),
					_Product = proplists:get_value("product", Bucket, ""),
					BR = #bucket{bucket_type = BucketType, 
						remain_amount = Amount, units = BucketType},
					[BR | AccIn]
				end,
				{lists:reverse(lists:foldl(F, [], BktStruct)), {array, BktStruct}};
			false ->
				undefined
		end,
		Enabled = proplists:get_value("enabled", Object),
		Multi = proplists:get_value("multisession", Object),
		case ocs:add_subscriber(IdIn, PasswordIn, Product, Buckets, Attributes, Enabled, Multi) of
			{ok, #subscriber{name = IdOut, last_modified = LM} = S} ->
				Id = binary_to_list(IdOut),
				Location = "/ocs/v1/subscriber/" ++ Id,
				JAttributes = {array, radius_to_json(S#subscriber.attributes)},
				RespObj = [{id, Id}, {href, Location},
						{password, binary_to_list(S#subscriber.password)},
						{attributes, JAttributes}, {buckets, BucketRef},
						{enabled, S#subscriber.enabled},
						{multisession, S#subscriber.multisession},
						{product, (S#subscriber.product)#product_instance.product}],
				JsonObj  = {struct, RespObj},
				Body = mochijson:encode(JsonObj),
				Headers = [{location, Location}, {etag, etag(LM)}],
				{ok, Headers, Body};
			{error, _} ->
				{error, 400}
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
patch_subscriber(Id, undefined, CType, ReqBody) ->
	patch_subscriber1(Id, undefined, CType, ReqBody);
patch_subscriber(Id, Etag, CType, ReqBody) ->
	try
		Etag1 = etag(Etag),
		patch_subscriber1(Id, Etag1, CType, ReqBody)
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
patch_subscriber1(Id, Etag, "application/json-patch+json", ReqBody) ->
	try
		{array, OpList} = mochijson:decode(ReqBody),
		case execute_json_patch_operations(Id, Etag, OpList) of
			{ok, #subscriber{password = Password,
					attributes = RadAttr, buckets = Buckets,
					enabled = Enabled, multisession = MSession}} ->
				Attributes = {array, radius_to_json(RadAttr)},
				TotalBalance = accumulated_balance(Buckets),
				RespObj =[{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
				{password, Password}, {attributes, Attributes},
				{totalBalance, TotalBalance}, {enabled, Enabled}, {multisession, MSession}],
				JsonObj  = {struct, RespObj},
				RespBody = mochijson:encode(JsonObj),
				Headers = case Etag of
					undefined ->
						[];
					_ ->
						[{etag, etag(Etag)}]
				end,
				{ok, Headers, RespBody};
			{error, Status} ->
				{error, Status}
		end
	catch
		_:_ ->
			{error, 400}
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

-spec etag(V1) -> V2
	when
		V1 :: string() | {N1, N2},
		V2 :: {N1, N2} | string(),
		N1 :: integer(),
		N2 :: integer().
%% @doc Generate a tuple with 2 integers from Etag string
%% value or vice versa.
%% @hidden
etag(V) when is_list(V) ->
	[TS, N] = string:tokens(V, "-"),
	{list_to_integer(TS), list_to_integer(N)};
etag(V) when is_tuple(V) ->
	{TS, N} = V,
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

-spec execute_json_patch_operations(Id, Etag, OpList) ->
		{ok, Subscriber} | {error, Status} when
	Id				:: string() | binary(),
	Etag			:: undefined | tuple(),
	OpList		:: [{struct, [tuple()]}],
	Subscriber	:: #subscriber{},
	Status		:: 412 | 404 | 500.
%% @doc Execute json-patch opearations and return subscriber record
%% @private
execute_json_patch_operations(Id, Etag, OpList) when is_list(Id) ->
	BinId = list_to_binary(Id),
	execute_json_patch_operations(BinId, Etag, OpList);
execute_json_patch_operations(Id, Etag, OpList) ->
	F = fun() ->
		case mnesia:read(subscriber, Id, write) of
			[Entry] when
					Entry#subscriber.last_modified == Etag;
					Etag == undefined ->
				F2 = fun({struct, OpObj}) ->
					case validate_operation(OpObj) of
						{"replace", Path, Value} ->
							ok = patch_replace(Id, Path, Value);
						{"add", Path, Value} ->
							ok = patch_add(Id, Path, Value);
						{error, malformed_request} ->
							throw(malformed_request)
					end
				end,
				lists:foreach(F2, OpList),
				[NewEntry] = mnesia:read(subscriber, Id),
				NewEntry;
			[#subscriber{}] ->
				throw(precondition_failed);
			[] ->
				throw(not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, Subscriber} ->
			{ok, Subscriber};
		{aborted, {throw, malformed_request}} ->
			{error, 400};
		{aborted, {throw, not_found}} ->
			{error, 404};
		{aborted, {throw, precondition_failed}} ->
			{error, 412};
		{aborted, _Reason} ->
			{error,  500}
	end.

-spec validate_operation(Operation) -> Result
	when
		Operation	:: [tuple()],
		Result		:: {Op, Path, Value} | {error, Reason},
		Op				:: string(),
		Path			:: string(),
		Value			:: string() | tuple() | atom(),
		Reason		:: malformed_request.
%% @doc validate elements in an operation object and return
%% `op', `path' and `value' or reason for failed.
validate_operation(Operation) ->
	OpT = lists:keyfind("op", 1, Operation),
	PathT = lists:keyfind("path", 1, Operation),
	ValueT = lists:keyfind("value", 1, Operation),
	case OpT of
		{_, "replace"} ->
			validate_operation1(replace, OpT, PathT, ValueT);
		{_, "add"} ->
			validate_operation1(add, OpT, PathT, ValueT);
		_ ->
			{error, malformed_request}
	end.
%% @hidden
validate_operation1(replace, OpT, {_, Path} = PathT, ValueT) ->
	[Target | _] = string:tokens(Path, "/"),
	Members = ["name", "password", "attributes",
		"enabled", "multisession"],
	case lists:member(Target, Members) of
		true ->
			validate_operation2(OpT, PathT, ValueT);
		false ->
			{error, malformed_request}
	end;
validate_operation1(add, OpT, {_, Path} = PathT, ValueT) ->
	[Target | _] = string:tokens(Path, "/"),
	Members = ["buckets"],
	case lists:member(Target, Members) of
		true ->
			validate_operation2(OpT, PathT, ValueT);
		false ->
			{error, malformed_request}
	end.
%% @hidden
validate_operation2(OpT, PathT, ValueT) ->
	case {OpT, PathT, ValueT} of
		{{_, Op}, {_, Path}, {_, Value}} ->
			{Op, Path, Value};
		_ ->
			{error, malformed_request}
	end.

-spec patch_replace(Id, Path, Value) -> ok
	when
		Id				:: binary(),
		Path			:: string(),
		Value			:: string() | atom() | tuple().
%% @doc replace the give value with given target path.
patch_replace(Id, Path , Value) ->
	[Target] = string:tokens(Path, "/"),
	patch_replace1(Target , Id, Value).
%% @hidden
patch_replace1("name", Id, Value) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	UpdateSubscriber = Subscriber#subscriber{name = list_to_binary(Value)},
	do_write(UpdateSubscriber);
patch_replace1("password", Id, Value) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	UpdateSubscriber =
		Subscriber#subscriber{password = list_to_binary(Value)},
	do_write(UpdateSubscriber);
patch_replace1("attributes", Id, {array, Value}) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	RadAttributes = json_to_radius(Value),
	UpdateSubscriber =
		Subscriber#subscriber{attributes = RadAttributes},
	do_write(UpdateSubscriber);
patch_replace1("enabled", Id, Value) when is_list(Value) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	Enabled = case Value of
		"true" ->
			true;
		"false" ->
			false
	end,
	UpdateSubscriber =
		Subscriber#subscriber{enabled = Enabled},
	do_write(UpdateSubscriber);
patch_replace1("enabled", Id, Value) when is_atom(Value) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	UpdateSubscriber =
		Subscriber#subscriber{enabled = Value},
	do_write(UpdateSubscriber);
patch_replace1("multisession", Id, Value) when is_list(Value) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	MultiSession = case Value of
		"true" ->
			true;
		"false" ->
			false
	end,
	UpdateSubscriber =
		Subscriber#subscriber{multisession = MultiSession},
	do_write(UpdateSubscriber);
patch_replace1("multisession", Id, Value) when is_atom(Value) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	UpdateSubscriber =
		Subscriber#subscriber{multisession = Value},
	do_write(UpdateSubscriber).

-spec patch_add(Id, Path, Value) -> ok
	when
		Id		:: binary(),
		Path	:: string(),
		Value	:: string() | atom() | tuple().
%% @doc add the give value with given target location.
patch_add(Id, Path, Value) ->
	[Target, Location] = string:tokens(Path, "/"),
	patch_add1(Target , Id, Value, Location).
%% @hidden
patch_add1("buckets" , Id, Value, Location) ->
	[Subscriber] = mnesia:read(subscriber, Id),
	OldBuckets = Subscriber#subscriber.buckets,
	patch_add(buckets , Value, Location, OldBuckets, Subscriber).
%% @hidden
patch_add(buckets , Value, "-", OldBuckets, Subscriber) ->
	{struct, BucketObj} = Value,
	{_, Amount} = lists:keyfind("amount", 1, BucketObj),
	{_, Units} =  lists:keyfind("units", 1, BucketObj),
	BucketType = bucket_type(Units),
	Bucket = #bucket{bucket_type = BucketType, 
			remain_amount = Amount, units = BucketType},
	NewBuckets = lists:append(OldBuckets, [Bucket]),
	UpdateSubscriber =
		Subscriber#subscriber{buckets = NewBuckets},
	do_write(UpdateSubscriber);
patch_add(buckets , Value, _Location, OldBuckets, Subscriber) ->
	{struct, BucketObj} = mochijson:decode(Value),
	{_, Amount} = lists:keyfind("amount", 1, BucketObj),
	{_, Units} =  lists:keyfind("units", 1, BucketObj),
	BucketType = bucket_type(Units),
	Bucket = #bucket{bucket_type = BucketType, 
			remain_amount = Amount, units = BucketType},
	NewBuckets = lists:append(OldBuckets, [Bucket]),
	UpdateSubscriber =
		Subscriber#subscriber{buckets = NewBuckets},
	do_write(UpdateSubscriber).

-spec do_write(Record) -> ok
	when
		Record :: #subscriber{}.
%% @hidden
do_write(Record) ->
	mnesia:write(Record).

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
accumulated_balance2(#bucket{bucket_type = octets, remain_amount = Amount}, AccBalance) ->
	accumulated_balance3(octets, "octets", Amount, AccBalance);
accumulated_balance2(#bucket{bucket_type = cents, remain_amount = Amount}, AccBalance) ->
	accumulated_balance3(cents, "cents", Amount, AccBalance);
accumulated_balance2(#bucket{bucket_type = seconds, remain_amount = Amount}, AccBalance) ->
	accumulated_balance3(seconds, "seconds", Amount, AccBalance).
%% @hidden
accumulated_balance3(Key, Units, Amount, AccBalance) ->
	case lists:keytake(Key, 1, AccBalance) of
		{value, {Key, {Units, Balance}}, Rest} ->
			[{Key, {Units, Amount + Balance}} | Rest];
		false ->
			[{Key, {Units, Amount}} | AccBalance]
	end.

-spec bucket_type(SBucketType) -> BucketType
	when
		SBucketType	:: string(),
		BucketType	:: octets | cents | seconds.
%% @doc return the bucket type.
bucket_type(BucketType) ->
	bucket_type1(string:to_lower(BucketType)).
%% @hidden
bucket_type1("octets") ->
	octets;
bucket_type1("cents") ->
	cents;
bucket_type1("seconds") ->
	seconds.

