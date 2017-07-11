%%% ocs_rest_res_balance.erl
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
			{ok, _, _, Balance, true, _} ->
				get_balance1(Identity, Balance, "active");
			{ok, _, _, Balance, false, _} ->
				get_balance1(Identity, Balance, "disable");
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_Error ->
			{error, 400}
	end.
%% @hidden
get_balance1(Identity, Balance, ActStatus) ->
	Id = {"id", Identity},
	Href = {"href", "/balanceManagement/v1/buckets/" ++ Identity},
	BucketType = {"bucketType", "octets"},
	Amount = {"amount", Balance},
	Units = {"units", "octect"},
	RemAmount = {"remainedAmount", {struct, [Amount, Units]}},
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
		{_, "buckettype"} = lists:keyfind("type", 1, Object),
		{_, {struct, Channel}} = lists:keyfind("channel", 1, Object),
		{_, "POS"} = lists:keyfind("name", 1, Channel),
		{_, {struct, AmountObj}} = lists:keyfind("amount", 1, Object),
		{_, "octect"} = lists:keyfind("units", 1, AmountObj),
		{_, Ammount} = lists:keyfind("amount", 1, AmountObj),
		{ok, _, _, _, _, LM} = ocs:find_subscriber(Identity),
		Location = "/balanceManagement/v1/buckets/" ++ Identity,
		Headers = [{location, Location}, {etag, etag(LM)}],
		{ok, Headers, []}
	catch
		_Error ->
			{error, 400}
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

