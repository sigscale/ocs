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

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

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
		{StartDate, EndDate} = case lists:keyfind("validFor", 1, Object) of
			{_, {struct, VF}} ->
				SDT = proplists:get_value("startDate", VF),
				EDT = proplists:get_value("endDate", VF),
				case {SDT, EDT} of
					{undefined, undefined} ->
						{undefined, undefined};
					{undefined, EDT} ->
						{undefined, ocs_rest:iso8601(EDT)};
					{SDT, undefined} ->
						{ocs_rest:iso8601(SDT), undefined}
				end;
			false ->
				{undefined, undefined}
		end,
		BucketType = units(Units),
		BID = generate_bucket_id(),
		Bucket = #bucket{id = BID, units = BucketType, remain_amount = Amount,
				start_date = StartDate, termination_date = EndDate},
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
			Headers = [{location, Location}, {etag, ocs_rest:etag(LastMod)}],
			{ok, Headers, []};
		{aborted, _Reason} ->
			{error, 500}
	end.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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
		Units :: string() | octets | cents | seconds.
%% @doc Return the type of units of the bucket.
units(Units) when is_list(Units) ->
	units1(string:to_lower(Units));
units(Units) when is_atom(Units) ->
	units1(Units).
%% @hidden
units1("octets") -> octets;
units1("cents") -> cents;
units1("seconds") -> seconds;
units1(octets) -> "octets";
units1(cents) -> "cents";
units1(seconds) -> "seconds".

%% @hidden
generate_bucket_id() ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

