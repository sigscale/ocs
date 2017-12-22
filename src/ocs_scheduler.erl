%%% ocs_scheduler.erl
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
%%%
-module(ocs_scheduler).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([start/0, start/1]).
-export([product_charge/1]).

-include("ocs.hrl").

-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec start() -> ok.
%% @equiv start(Interval)
start() ->
	start(1440000).

-spec start(Interval) -> ok
	when
		Interval :: pos_integer().
%% @doc
start(Interval) ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(product, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				timer:apply_interval(Interval, ?MODULE,
					product_charge, [lists:flatten(lists:reverse(Acc))]);
			(_F, {error, Reason}, _Acc) ->
				exit(Reason);
			(F,{Products, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Products | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			exit(Reason);
		{atomic, _Result} ->
			ok
	end.

-spec product_charge(Products) -> ok
	when
		Products :: [#product{}].
%% @doc Scheduler update for all the subscriptions.
product_charge(Products) ->
	F  = fun() -> product_charge(mnesia:first(subscriber), Products) end,
	mnesia:transaction(F),
	ok.
%% @hidden
product_charge('$end_of_table', _Products) ->
	ok;
product_charge(SubscriptionId, Products) ->
	case mnesia:read(subscriber, SubscriptionId) of
		[#subscriber{buckets = Buckets, product =
				#product_instance{product = ProdId,
				payment = Payment} = ProdInst} = S] ->
			Now = erlang:system_time(?MILLISECOND),
			Product = find_product(ProdId, Products),
			LM = {Now, erlang:unique_integer([positive])},
			{PaymentDues, NewBuckets} = product_charge1(Now, Buckets, Product#product.price, Payment),
			NewProdInst = ProdInst#product_instance{payment = PaymentDues},
			Entry = S#subscriber{buckets = NewBuckets,
				product = NewProdInst, last_modified = LM},
			mnesia:write(Entry);
		[] ->
			ok
	end,
	product_charge(mnesia:next(subscriber, SubscriptionId), Products).
%% @hidden
product_charge1(Now, Buckets, Prices, Payments) ->
	product_charge1(Now, Buckets, Prices, [], Payments).
%% @hidden
product_charge1(Now,Buckets, Prices, Acc,
		[{PriceName, DueDate}  = H | T]) when DueDate < Now ->
	Price = find_price(PriceName, Prices),
	case product_charge2(Price, Now, Buckets) of
		{_R, Buckets} ->
			product_charge1(Now, Buckets, Prices, [H | Acc], T);
		{0, B1} ->
			case product_charge2(Price#price.alteration, Now, B1) of
				{_, B1} ->
					B3 = product_charge3(Now, Price#price.name, Price#price.alteration, B1),
					product_charge1(Now, B3, Prices, Acc, T);
				{0, B2} ->
					B3 = product_charge3(Now, Price#price.name, Price#price.alteration, B2),
					product_charge1(Now, B3, Prices, Acc, T);
				{_, B2} ->
					product_charge1(Now, B2, Prices, [H | Acc], T)
			end;
		{_, B1} ->
			product_charge1(Now, B1, Prices, [H | Acc], T)
	end;
product_charge1(Now, Buckets, Prices, Acc, [H | T]) ->
	product_charge1(Now, Buckets, Prices, [H | Acc], T);
product_charge1(_Now, Buckets, _Prices, Acc, []) ->
	{lists:reverse(Acc), lists:reverse(Buckets)}.
%% @hidden
product_charge2(#price{type = recurring,
		units = Type, amount = Amount}, Now, Buckets) ->
	charge(Type, Amount, Now, Buckets);
product_charge2(#alteration{type = recurring,
		amount = Amount}, Now, Buckets) ->
	charge(cents, Amount, Now, Buckets);
product_charge2(_, _, Buckets) ->
	{0, Buckets}.
%% @hidden
product_charge3(_Now, _Price, undefined, Buckets) ->
	Buckets;
product_charge3(Now, Price, #alteration{units = Type, size = Size}, Buckets) ->
	[#bucket{remain_amount = Size, prices = [Price],
		units = Type, start_date = Now} | Buckets].

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec find_product(ProductId, Products) -> Product
	when
		ProductId :: string(),
		Products :: [#product{}],
		Product :: #product{}.
%% @doc lookup product record with given id
%% @hidden
find_product(ProductId, Products) ->
	case lists:keyfind(ProductId, #product.name, Products) of
		#product{} = P ->
			P;
		false ->
			exit(product_not_found)
	end.

-spec find_price(Name, Prices) -> #price{}
	when
		Name :: string(),
		Prices :: [#price{}].
%% @doc return match price record with given name
find_price(Name, [#price{name = Name} = Price | _]) ->
	Price;
find_price(Name, [_ | T]) ->
	find_price(Name, T);
find_price(_Name, []) ->
	exit(price_not_found).

-spec charge(Type, Charge, Now, Buckets) -> Result
	when
		Type :: octets | cents | seconds,
		Charge :: integer(),
		Now :: pos_integer(),
		Buckets :: [#bucket{}],
		Result :: {Remain, Buckets},
		Remain :: integer().
%% @private
charge(Type, Charge, Now, Buckets) ->
	charge(Type, Charge, Now, [], Buckets).
%% @hidden
charge(Type, Charge, Now, Acc, [#bucket{units = Type,
		termination_date = Expire, remain_amount = R} = B | T])
		when ((Expire == undefined) or (Expire >= Now)) andalso R  >= Charge ->
	NewAcc = [B#bucket{remain_amount = R - Charge} | Acc],
	{0, NewAcc ++ T};
charge(Type, Charge, Now, Acc, [#bucket{units = Type,
		termination_date = Expire, remain_amount = R} | T])
		when ((Expire == undefined) or (Expire >= Now)) andalso R  =< Charge ->
	charge(Type, Charge - R, Now, lists:reverse(Acc) ++ T);
charge(_Type, 0, _Now, Acc, Buckets) ->
	{0, lists:reverse(Acc) ++ Buckets};
charge(Type, Charge, Now, Acc, [H | T]) ->
	charge(Type, Charge, Now, [H | Acc], T);
charge(_Type, Charge, _Now, Acc, []) ->
	{Charge, lists:reverse(Acc)}.

