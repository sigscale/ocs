%%% ocs_rating.erl
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
%%% @doc This library module implements utility functions
%%% 	for handling rating in the {@link //ocs. ocs} application.
%%%
-module(ocs_rating).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([rating/2]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec rating(RequestType, SubscriptionRef) -> Return
	when
		RequestType :: price_request | tariff_request,
		SubscriptionRef :: #subscriber{},
		Return :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
rating(tariff_request, #subscriber{buckets = Buckets,
		product = ProdInst} = SubscriptionRef) ->
	F = fun(#bucket{bucket_type = octets, remain_amount = RA}) when
					RA#remain_amount.amount > 0 -> 
				true;
			(_) -> 
				false
	end,

	rating1(lists:any(F, Buckets), ProdInst, Buckets, SubscriptionRef).
%% @hidden
rating1(false, #product_instance{product = ProdID, termination_date = EDT}, Buckets, SubscriptionRef) ->
	rating2(ocs:find_product(ProdID), EDT, Buckets, SubscriptionRef);
rating1(true, #product_instance{termination_date =
		EDT, product = ProdID}, Buckets, SubscriptionRef) ->
	Now = erlang:system_time(?MILLISECOND),
	if
		EDT > Now ->
			{ok, SubscriptionRef};
		true ->
			rating2(ocs:find_product(ProdID), EDT, Buckets, SubscriptionRef)
	end.
%% @hidden
rating2({ok, #product{price = Prices}}, EDT, Buckets, SubscriptionRef) ->
	rating3(Prices, EDT, Buckets, SubscriptionRef);
rating2({error, Reason}, _EDT, _Buckets, _SubscriptionRef) ->
	{error, Reason}.
%% @hidden
rating3([#price{type = recurring, units = cents,
		amount = Amount, alteration = #alteration{type = recurring,
		units = octets, size = Size}} | T], EDT, Buckets, SubscriptionRef) ->
	Now = erlang:system_time(?MILLISECOND),
	case EDT > Now of
		true ->
			rating4(T, EDT, Amount, Size, Buckets, SubscriptionRef);
		false ->
			rating3(T, EDT, Buckets, SubscriptionRef)
	end;
rating3([#price{type = usage, size = Size, units = octets,
		amount = Amount} | T], EDT, Buckets, SubscriptionRef) ->
	rating4(T, EDT, Amount, Size, Buckets, SubscriptionRef);
rating3([], _, _, _SubscriptionRef) ->
	{error, rating_failed}.
%% @hidden
rating4(Prices, EndTime, Amount, Size, Buckets, #subscriber{product = ProdInst} = SubscriptionRef) ->
	case lists:keytake(cents, #bucket.bucket_type, Buckets) of
		{value, #bucket{remain_amount = 
				#remain_amount{amount = Cents}} = RecuBucket, ReBuckets} when Cents >= Amount ->
			B1 = #bucket{bucket_type = octets, remain_amount = #remain_amount{amount = Size}},
			B2 = RecuBucket#bucket{remain_amount = #remain_amount{amount = Cents - Amount}},
			NewBuckets = [B1, B2 | ReBuckets],
			SDT = get_std(),
			EDT = get_edt(monthly, SDT),
			NewProdInst = ProdInst#product_instance{start_date = SDT, termination_date = EDT},
			NewSubscriptionRef = SubscriptionRef#subscriber{product = NewProdInst, buckets = NewBuckets},
			{ok, NewSubscriptionRef};
		{value, _, _} ->
			rating3(Prices, EndTime, Buckets, SubscriptionRef);
		false ->
			{error, rating_failed}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------
get_std() ->
	erlang:system_time(?MILLISECOND).

get_edt(monthly, SDT) ->
	SDT + 2592000. % 3600*24

