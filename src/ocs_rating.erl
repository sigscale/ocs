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

-export([rating/3]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec rating(SubscriberID, UsageSecs, UsageOctets) -> Return
	when
		SubscriberID :: string() | binary(),
		UsageSecs :: integer(),
		UsageOctets :: integer(),
		Return :: ok | {error, Reason},
		Reason :: term().
rating(SubscriberID, UsageSecs, UsageOctets) when is_list(SubscriberID) ->
	rating(list_to_binary(SubscriberID), UsageSecs, UsageOctets);
rating(SubscriberID, UsageSecs, UsageOctets) when is_binary(SubscriberID) ->
	F = fun() ->
			case mnesia:read(subscriber, SubscriberID, write) of
				[#subscriber{buckets = Buckets, product =
						#product_instance{product = ProdID,
						product_characteristics = Chars}} = Subscriber] ->
					Validity = proplists:get_value(validity, Chars),
					case mnesia:read(product, ProdID, read) of
						[#product{price = Prices}] ->
							case lists:keyfind(usage, #price.type, Prices) of
								#price{} = Price ->
									{Charged, NewBuckets} = rating2(Price,
											Validity, UsageSecs, UsageOctets, Buckets),
									Entry = Subscriber#subscriber{buckets = NewBuckets},
									mnesia:write(Entry);
								false ->
									throw(price_not_found)
							end;
						[] ->
							throw(product_not_found)
					end;
				[] ->
					throw(subscriber_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
rating2(#price{type = usage, size = Size, units = octets,
		amount = Amount}, Validity, _UsageSecs, UsageOctets, Buckets) ->
	rating3(Amount, Size, octets, Validity, UsageOctets, Buckets);
rating2(#price{type = usage, size = Size, units = seconds,
		amount = Amount}, Validity, UsageSecs, _UsageOctets, Buckets) ->
	rating3(Amount, Size, seconds, Validity, UsageSecs, Buckets).
%% @hidden
rating3(Price, Size, Validity, Type, Used, Buckets) ->
	case charge(Type, Used, lists:sort(fun sort_buckets/2, Buckets)) of
		{Charged, NewBuckets} when Charged < Used ->
			purchase(Type, Price, Size, Used - Charged, Validity, NewBuckets);
		{Charged, NewBuckets} ->
			{Charged, NewBuckets}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------
-spec charge(Type, Charge, Buckets) -> Result
	when
		Type :: octets | seconds | cents,
		Charge :: integer(),
		Buckets :: [#bucket{}],
		Result :: {Charged, Buckets},
		Charged :: integer().
charge(Type, Charge, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	charge(Type, Charge, Now, Buckets, [], 0).
%% @hidden
charge(Type, Charge, Now, [#bucket{bucket_type = Type,
		termination_date = T1} | T], Acc, Charged) when T1 =/= undefined, T1 =< Now->
	charge(Type, Charge, Now, T, Acc, Charged);
charge(Type, Charge, _Now, [#bucket{bucket_type = Type,
		remain_amount = R} = B | T], Acc, Charged) when R > Charge ->
	NewBuckets = [B#bucket{remain_amount = R - Charge} | T],
	{Charged + Charge, NewBuckets ++ Acc};
charge(Type, Charge, Now, [#bucket{bucket_type = Type,
		remain_amount = R} | T], Acc, Charged) when R =< Charge ->
	charge(Type, Charge - R, Now, T, Acc, Charged + R);
charge(_Type, 0, _Now, Buckets, Acc, Charged) ->
	{Charged, Buckets ++ Acc};
charge(_Type, _Charge, _Now, [], Acc, Charged) ->
	{Charged, Acc};
charge(_Type, _Charge, _Now, Buckets, Acc, Charged) ->
	{Charged, Buckets ++ Acc}.

-spec purchase(Type, Price, Size, Used, Validity, Buckets) -> Result
	when
		Type :: octets | seconds,
		Price :: integer(),
		Size :: integer(),
		Used :: integer(),
		Validity :: integer(),
		Buckets :: [#bucket{}],
		Result :: {Charged, Buckets},
		Charged :: integer().
purchase(Type, Price, Size, Used, Validity, Buckets) ->
	UnitsNeeded = case (Used rem Size) of
		0 ->
			Used div Size;
		_ ->
			(Used div Size) + 1
	end,
	Charge = UnitsNeeded * Price,
	case charge(cents, Charge, Buckets) of
		{Charged, NewBuckets} when Charged < Charge ->
			{Charged, NewBuckets};
		{Charged, NewBuckets} ->
			Remain = UnitsNeeded * Size - Used,
			Bucket = #bucket{bucket_type = Type, remain_amount = Remain,
				termination_date = Validity,
				start_date = erlang:system_time(?MILLISECOND)},
			{Charged, [Bucket | NewBuckets]}
	end.

%% @hidden
sort_buckets(#bucket{termination_date = T1}, #bucket{termination_date = T2}) when
		T1 =< T2->
	true;
sort_buckets(_, _)->
	false.

