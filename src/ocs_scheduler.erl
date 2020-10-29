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

%% private API
-export([start/0, start/2]).
-export([product_charge/0]).
%% private API
-export([run_recurring/0]).

-include("ocs.hrl").

-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec start() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv start(Interval)
start() ->
	{ok, ScheduledTime} = application:get_env(charging_scheduler_time),
	{ok, Interval} = application:get_env(charging_interval),
	start(ScheduledTime, Interval).

-spec start(ScheduledTime, Interval) -> Result
	when
		ScheduledTime :: tuple(),
		Interval :: pos_integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Starts the schedule of recurring charges.
start(ScheduledTime, Interval) ->
	StartDelay = start_delay(ScheduledTime, Interval),
	case timer:apply_after(StartDelay, ?MODULE, run_recurring, []) of
		{ok, _TRef} ->
			ok;
		{error, Reason} ->
			error_logger:error_report(["Scheduler Failed",
					{module, ?MODULE}, {delay, StartDelay},
					{interval, Interval}, {error, Reason}]),
			{error, Reason}
	end.

-spec product_charge() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Apply recurring charges to all subscriptions.
product_charge() ->
	Now = erlang:system_time(?MILLISECOND),
	product_charge1(get_product(start), Now).
%% @hidden
product_charge1('$end_of_table', _Now) ->
	ok;
product_charge1(ProdRef, Now) ->
	F = fun() ->
			case mnesia:read(product, ProdRef, write) of
				[#product{product = OfferId,
						payment = Payments,
						balance = BucketRefs} = Product] ->
					case mnesia:read(offer, OfferId, read) of
						[#offer{name = OfferId, price = Prices,
								bundle = Bundle} = Offer] when is_list(Bundle) ->
							Fbundle = fun(#bundled_po{name = OfferName}) ->
										case mnesia:read(offer, OfferName, read) of
											[#offer{price = Prices, bundle = []}] ->
												Prices;
											[] ->
												throw(offer_not_found)
										end
							end,
							BundledOfferPrices = lists:map(Fbundle, Bundle),
							case if_recur(Prices ++ BundledOfferPrices) of
								true ->
									case if_dues(Payments, Now) of
										true ->
											Buckets1 = [mnesia:read(bucket, B) || B <- BucketRefs],
											Buckets2 = lists:flatten(Buckets1),
											{NewProduct1, Buckets3} = ocs:subscription(Product, Offer,
													Buckets2, false),
											BucketAdjustments = bucket_adjustment(ProdRef,
													BucketRefs, Buckets2, Buckets3),
											NewBRefs = update_buckets(BucketRefs, Buckets1, Buckets3),
											NewProduct2 = NewProduct1#product{balance = NewBRefs},
											{mnesia:write(NewProduct2), BucketAdjustments};
										false ->
											ok
									end;
								false ->
									ok
							end;
						[] ->
							throw(offer_not_found)
					end;
				[] ->
					throw(product_ref_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			product_charge1(get_product(ProdRef), Now);
		{atomic, {ok, Adjustments}} ->
			ocs_event:notify(charge, Adjustments, balance),
			product_charge1(get_product(ProdRef), Now);
		{aborted, Reason} ->
			error_logger:error_report("Scheduler Update Failed",
					[{module, ?MODULE}, {product_id, ProdRef},
					{time, erlang:system_time(?MILLISECOND)},
					{reason, Reason}]),
			product_charge1(get_product(ProdRef), Now)
	end.

%%----------------------------------------------------------------------
%%  private functions
%%----------------------------------------------------------------------

-spec run_recurring() -> ok.
%% @doc Scheduled function runs recurring charging and reschedules itself.
%% @private
run_recurring() ->
	error_logger:info_report(["Start scheduled charging"]),
	ok = product_charge(),
	error_logger:info_report(["End scheduled charging"]),
	{ok, ScheduledTime} = application:get_env(ocs, charging_scheduler_time),
	{ok, Interval} = application:get_env(ocs, charging_interval),
	StartDelay = start_delay(ScheduledTime, Interval),
	case timer:apply_after(StartDelay, ?MODULE, run_recurring, []) of
		{ok, _TRef} ->
			ok;
		{error, Reason} ->
			error_logger:error_report(["Scheduler Failed",
					{module, ?MODULE}, {delay, StartDelay},
					{interval, Interval}, {error, Reason}])
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @private
bucket_adjustment(ProductRef, OldBucketRefs, OldBuckets, NewBuckets) ->
	NewBucketRefs = [B#bucket.id || B <- NewBuckets],
	Fdel = fun F([BucketId | T], Acc) ->
				#bucket{remain_amount = Amount, units = Units}
						= lists:keyfind(BucketId, #bucket.id, OldBuckets),
				F(T, [#adjustment{type = "recurring",
						reason = "scheduled", amount = -Amount,
						units = Units, product = ProductRef} | Acc]);
			F([], Acc) ->
				Acc
	end,
	DelBucketAdj = Fdel(OldBucketRefs -- NewBucketRefs, []),
	Fupdate = fun F([#bucket{id = Id, remain_amount = Amount,
			units = cents} | T], Acc) ->
				#bucket{remain_amount = PrevAmount}
						= lists:keyfind(Id, #bucket.id, OldBuckets),
				F(T, [#adjustment{type = "recurring",
						reason = "scheduled", amount = Amount - PrevAmount,
						units = cents, product = ProductRef} | Acc]);
			F([#bucket{remain_amount = Amount, units = Units} | T], Acc) ->
				F(T, [#adjustment{type = "recurring",
						reason = "scheduled", amount = Amount,
						units = Units, product = ProductRef} | Acc]);
			F([], Acc) ->
				Acc
	end,
	UpdatedBucketAdj = Fupdate(NewBuckets -- OldBuckets, []),
	DelBucketAdj ++ UpdatedBucketAdj.

%% @private
if_dues([{_, DueDate} | _], Now) when DueDate < Now ->
	true;
if_dues([_ | T], Now) ->
	if_dues(T, Now);
if_dues([], _Now)  ->
	false.

-spec if_recur(Prices) -> Result
	when
		Prices :: [#price{}],
		Result :: boolean().
%% @private
if_recur(Prices) ->
	F = fun(#price{type = recurring}) ->
				true;
			(#price{alteration
					= #alteration{type = recurring}}) ->
				true;
			(_) ->
				false
	end,
	case lists:any(F, Prices) of
		false ->
			false;
		true ->
			true
	end.

%% @private
get_product(start) ->
	ets:first(product);
get_product(SId) ->
	ets:next(product, SId).

%% @private
update_buckets(BRefs, OldB, NewB) ->
	AllNewKeys = [B#bucket.id || B <- NewB],
	UpdatedB = NewB -- OldB,
	update_b(UpdatedB),
	ok = delete_b(BRefs -- AllNewKeys),
	AllNewKeys.

%% @private
update_b([B | T]) ->
	ok = mnesia:write(B),
	update_b(T);
update_b([]) ->
	ok.

%% @private
delete_b([BRef | T]) ->
	ok = mnesia:delete(bucket, BRef, write),
	delete_b(T);
delete_b([]) ->
	ok.

%% @hidden
start_delay(ScheduledTime, Interval) when Interval < 1440 ->
	IntervalSecs = Interval * 60,
	{Date, Time} = erlang:universaltime(),
	case calendar:datetime_to_gregorian_seconds({Date, ScheduledTime})
			- calendar:datetime_to_gregorian_seconds({Date, Time}) of
		Delay when Delay < 0 ->
			(IntervalSecs - ((0 - Delay) rem IntervalSecs)) * 1000;
		0 ->
			IntervalSecs * 1000;
		Delay when Delay > 0 ->
			(Delay rem IntervalSecs) * 1000
	end;
start_delay(ScheduledTime, Interval) when Interval >= 1440 ->
	{Date, Time} = erlang:universaltime(),
	Today = calendar:date_to_gregorian_days(Date),
	Period = Interval div 1440,
	ScheduleDay = calendar:gregorian_days_to_date(Today + Period),
	Next = {ScheduleDay, ScheduledTime},
	Now = calendar:datetime_to_gregorian_seconds({Date, Time}),
	(calendar:datetime_to_gregorian_seconds(Next) - Now) * 1000.

