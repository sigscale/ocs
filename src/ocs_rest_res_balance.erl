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
		top_up/2, get_balance/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

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

-spec get_balance(Identity) -> Result
	when
		Identity :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /balanceManagment/v1/{id}/buckets'
%% reuqest
get_balance(Identity) ->
	try
		case ocs:find_subscriber(Identity) of
			{ok, #subscriber{buckets = Buckets, enabled = true}} ->
				get_balance1(Identity, Buckets, "active");
			{ok, #subscriber{buckets = Buckets, enabled = false}} ->
				get_balance1(Identity, Buckets, "disable");
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_Error ->
			{error, 400}
	end.
%% @hidden
get_balance1(Identity, Buckets, ActStatus) ->
	Id = {"id", Identity},
	Href = {"href", "/balanceManagement/v1/buckets/" ++ Identity},
	BucketType = {bucketType, ""},
	Balance = accumulated_balance(Buckets),
	RemAmount = {"remainedAmount", Balance},
	Status = {"status", ActStatus},
	Object = [Id, Href, BucketType, RemAmount, Status],
	Json = {struct, Object},
	Body  = mochijson:encode(Json),
	{ok, [{content_type, "application/json"}], Body}.

-spec top_up(Identity, RequestBody) -> Result
	when
		Identity :: list(),
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /balanceManagement/v1/{id}/balanceTopups'
%% and top up `subscriber' balance resource
top_up(Identity, RequestBody) ->
	try
		{struct, Object} = mochijson:decode(RequestBody),
		{_, _} = lists:keyfind("type", 1, Object),
		{_, {struct, Channel}} = lists:keyfind("channel", 1, Object),
		{_, _} = lists:keyfind("name", 1, Channel),
		{_, {struct, AmountObj}} = lists:keyfind("amount", 1, Object),
		{_, Units} = lists:keyfind("units", 1, AmountObj),
		{_, Amount} = lists:keyfind("amount", 1, AmountObj),
		BucketType = bucket_type(Units),
		Bucket = #bucket{bucket_type = BucketType, remain_amount =
				#remain_amount{amount = Amount, unit = Units}},
		top_up1(Identity, Bucket)
	catch
		_Error ->
			{error, 400}
	end.
%% @hidden
top_up1(Identity, Bucket) ->
	F = fun()->
		case mnesia:read(subscriber, list_to_binary(Identity), read) of
			[] ->
				not_found;
			[#subscriber{buckets = CrntBuckets, last_modified = LM} = User] ->
				mnesia:write(User#subscriber{buckets = CrntBuckets ++ [Bucket]}),
				LM
		end
	end,
	case mnesia:transaction(F) of
		{atomic, not_found} ->
			{error, 404};
		{atomic, LastMod} ->
			Location = "/balanceManagement/v1/buckets/" ++ Identity,
			Headers = [{location, Location}, {etag, etag(LastMod)}],
			{ok, Headers, []};
		{aborted, _Reason} ->
			{error, 500}
	end.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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
accumulated_balance2(#bucket{bucket_type = octets, remain_amount =
		#remain_amount{unit = Units, amount = Amount}}, AccBalance) ->
	accumulated_balance3(octets, Units, Amount, AccBalance);
accumulated_balance2(#bucket{bucket_type = cents, remain_amount =
		#remain_amount{unit = Units, amount = Amount}}, AccBalance) ->
	accumulated_balance3(cents, Units, Amount, AccBalance);
accumulated_balance2(#bucket{bucket_type = seconds, remain_amount =
		#remain_amount{unit = Units, amount = Amount}}, AccBalance) ->
	accumulated_balance3(seconds, Units, Amount, AccBalance).
%accumulated_balance2([], AccBalance) ->
%	AccBalance.
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
		SBucketType	:: string() | octets | cents | seconds,
		BucketType	:: octets | cents | seconds | string().
%% @doc return the bucket type.
bucket_type(BucketType) when is_list(BucketType) ->
	bucket_type1(string:to_lower(BucketType));
bucket_type(BucketType) when is_atom(BucketType) ->
	bucket_type1(BucketType).
%% @hidden
bucket_type1("octets") -> octets;
bucket_type1("cents") -> cents;
bucket_type1("seconds") -> seconds;
bucket_type1(octets) -> "octets";
bucket_type1(cents) -> "cents";
bucket_type1(seconds) -> "seconds".

