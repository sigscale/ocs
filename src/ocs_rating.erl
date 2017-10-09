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

-spec rating(RequestType, SubscriptionRef) -> Return
	when
		RequestType :: price_request | tariff_request,
		SubscriptionRef :: #subscriber{},
		Return :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
rating(tariff_request, #subscriber{buckets = Buckets,
		product = #product_instance{product = ProdID},} = SubscriptionRef) ->
	F = fun(#bucket{bucket_type = octets, remain_amount = RA}) when
					RA#remain_amount.amount > 0 -> 
				true;
			(_) -> 
				false
	end,

	rating1(lists:any(F, Buckets), ProdID, Buckets, SubscriptionRef).
%% @hidden
rating1(false, ProdID, Buckets, SubscriptionRef) ->
	rating2(ocs:find_product(ProdID), Buckets, SubscriptionRef);
rating1(true, _ProdID, _Buckets, SubscriptionRef) ->
	{ok, SubscriptionRef}.
%% @hidden
rating2({ok, #product{price = Prices}}, Buckets, SubscriptionRef) ->
	case lists:keyfind(usage, #price.type, Prices) of
		#price{} = Price ->
			rating3(Price, Buckets, SubscriptionRef);
		false ->
			{error, rating_failed}
	end;
rating2({error, Reason}, _Buckets, _SubscriptionRef) ->
	{error, Reason}.
%% @hidden
rating3(#price{type = usage, size = Size, units = octets, amount = Amount}, Buckets, SubscriptionRef) ->
	rating4(Amount, Size, Buckets, SubscriptionRef);
rating3(_Price, _Buckets, _SubscriptionRef) ->
	{error, rating_failed}.
%% @hidden
rating4(Amount, Size, Buckets, SubscriptionRef) ->
	case lists:keytake(cents, #bucket.bucket_type, Buckets) of
		{value, #bucket{remain_amount = 
				#remain_amount{amount = Cents}} = RecuBucket, ReBuckets} when Cents >= Amount ->
			B1 = #bucket{bucket_type = octets, remain_amount = #remain_amount{amount = Size}},
			B2 = RecuBucket#bucket{remain_amount = #remain_amount{amount = Cents - Amount}},
			NewBuckets = [B1, B2 | ReBuckets],
			{ok, SubscriptionRef#subscriber{buckets = NewBuckets}};
		_ ->
			{error, rating_failed}
	end.
