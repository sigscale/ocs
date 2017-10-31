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

-export([rate/4, rate/5]).
-export([charge/4]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec rate(SubscriberID, Flag, DebitAmount, ReserveAmount) -> Result
	when
		SubscriberID :: string() | binary(),
		Flag :: inital | interim | final,
		DebitAmount :: [{Type, Amount}],
		ReserveAmount :: [{Type, Amount}],
		Type :: octets | seconds,
		Amount :: integer(),
		Result :: {ok, Subscriber, GrantAmount} | {out_of_credit, SessionList} | {error, Reason},
		Subscriber :: #subscriber{},
		GrantAmount :: integer(),
		SessionList :: [tuple()],
		Reason :: term().
%% @equiv rate(SubscriberID, Flag, DebitAmount, ReserveAmount, [])
rate(SubscriberID, Flag, DebitAmount, ReserveAmount) ->
	rate(SubscriberID, Flag, DebitAmount, ReserveAmount, []).

-spec rate(SubscriberID, Flag, DebitAmount, ReserveAmount, SessionIdentification) -> Result
	when
		SubscriberID :: string() | binary(),
		Flag :: inital | interim | final,
		DebitAmount :: [{Type, Amount}],
		ReserveAmount :: [{Type, Amount}],
		SessionIdentification :: [tuple()],
		Type :: octets | seconds,
		Amount :: integer(),
		Result :: {ok, Subscriber, GrantAmount} | {out_of_credit, SessionList} | {error, Reason},
		Subscriber :: #subscriber{},
		GrantAmount :: integer(),
		SessionList :: [tuple()],
		Reason :: term().
%% @doc
rate(SubscriberID, Flag, DebitAmount, ReserveAmount, SessionIdentification) when is_list(SubscriberID)->
	rate(list_to_binary(SubscriberID), Flag, DebitAmount, ReserveAmount, SessionIdentification);
rate(SubscriberID, Flag, DebitAmount, ReserveAmount, SessionIdentification)
		when is_binary(SubscriberID),
		((Flag == inital) or (Flag == interim) or (Flag == final)),
		is_list(DebitAmount), is_list(ReserveAmount), is_list(SessionIdentification) ->
	F = fun() ->
			case mnesia:read(subscriber, SubscriberID, write) of
				[#subscriber{product = #product_instance{product = ProdID}} = Subscriber] ->
					case mnesia:read(product, ProdID, read) of
						[#product{price = Prices}] ->
							rate1(Subscriber, Prices, Flag, DebitAmount, ReserveAmount, SessionIdentification);
						[] ->
							throw(product_not_found)
					end;
				[] ->
					throw(subsriber_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {grant, Sub, GrantAmount}} ->
			{ok, Sub, GrantAmount};
		{atomic, {out_of_credit, SL}} ->
			{out_of_credit, SL};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
rate1(Subscriber, Prices, Flag, [], ReserveAmount, SessionIdentification) ->
	rate2(Subscriber, Prices, Flag, ReserveAmount, SessionIdentification, 0);
rate1(#subscriber{buckets = Buckets, product = #product_instance{characteristics = Chars}} =
		Subscriber, Prices, Flag, DebitAmount, ReserveAmount, SessionIdentification) ->
	try
		Validity = proplists:get_value(validity, Chars),
		#price{units = Type, size = Size, amount = Price} = lists:keyfind(usage, #price.type, Prices),
		{Type, Used} = lists:keyfind(Type, 1, DebitAmount),
		case charge(Type, Used, true, Buckets) of
			{R1, C1, NB1} when R1 > 0 ->
				{R2, _C2, NB2}  = purchase(Type, Price, Size, Used - C1, Validity, true, NB1),
				{R2, NB2};
			{R1, _C1, NB1} ->
				{R1, NB1}
		end
	of
		{RemainCharge, NewBuckets}  when RemainCharge > 0 ->
			rate3(Subscriber#subscriber{buckets = NewBuckets},
					RemainCharge, Flag, ReserveAmount, SessionIdentification);
		{RemainCharge, NewBuckets} ->
			rate2(Subscriber#subscriber{buckets = NewBuckets},
					Prices, Flag, ReserveAmount, SessionIdentification, RemainCharge)
	catch
		_:_ ->
			throw(price_not_found)
	end.
%% @hidden
rate2(Subscriber, _Prices, Flag, [], SessionIdentification, Charged)  ->
	rate3(Subscriber, Charged, Flag, 0, SessionIdentification);
rate2(#subscriber{buckets = Buckets, product = #product_instance{characteristics = Chars}} =
		Subscriber, Prices, Flag, ReserveAmount, SessionIdentification, _Charged) ->
	try
		Validity = proplists:get_value(validity, Chars),
		#price{units = Type, size = Size, amount = Price} = lists:keyfind(usage, #price.type, Prices),
		{Type, Reserve} = lists:keyfind(Type, 1, ReserveAmount),
		case charge(Type, Reserve, false, Buckets) of
			{R1, C1, NB1} when R1 > 0 ->
				{R2, _C2, NB2} = purchase(Type, Price, Size, Reserve - C1, Validity, false, NB1),
				{R2, NB2, Reserve};
			{R1, _C1, NB1} ->
				{R1, NB1, Reserve}
		end
	of
		{RemainCharge, NewBuckets, Amount} ->
			rate3(Subscriber#subscriber{buckets = NewBuckets},
					RemainCharge, Flag, Amount, SessionIdentification)
	catch
		_:_ ->
			throw(price_not_found)
	end.
%% @hidden
rate3(#subscriber{session_attributes = SessionList} = Subscriber,
		Charged, Flag, ReserveAmount, SessionIdentification) ->
	case Charged of
		C1 when C1 > 0 ->
			Entry = Subscriber#subscriber{session_attributes = []},
			ok = mnesia:write(Entry),
			{out_of_credit, SessionList};
		_ ->
			NewSessionList = case Flag of
				inital ->
					update_session(SessionIdentification, SessionList);
				final ->
					remove_session(SessionList, SessionIdentification);
				interim ->
					SessionList
			end,
			Entry = Subscriber#subscriber{session_attributes = NewSessionList},
			ok = mnesia:write(Entry),
			{grant, Entry, ReserveAmount}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec charge(Type, Charge, Final, Buckets) -> Result
	when
		Type :: octets | seconds | cents,
		Charge :: integer(),
		Final :: boolean(),
		Buckets :: [#bucket{}],
		Result :: {RemainingCharge, Charged, NewBuckets},
		RemainingCharge :: integer(),
		Charged :: integer(),
		NewBuckets :: [#bucket{}].
%% @doc Manage balance bucket reservations and debit amounts.
%%
%% 	Subscriber credit is kept in a `Buckets' list where
%% 	each `#bucket{}' has a `Type', an expiration time and
%% 	a remaining balance value. Charges may be made against
%% 	the `Buckets' list in any `Type'. The buckets are
%% 	processed starting with the oldest and expired buckets
%% 	are ignored and removed. Buckets matching `Type' are
%% 	are compared with `Charge'. If `Final' is `true' then
%% 	 `Charge' amount is debited from the buckets. Empty
%% 	buckets are removed.
%%
%% 	Returns `{RemainingCharge, Charged, Buckets}' where
%% 	`Charge' is the total amount debited from the buckets,
%% 	`RemainingCharge' is the left over amount not charged
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
charge(Type, Charge, Final, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	F = fun(#bucket{termination_date = T1},
				#bucket{termination_date = T2}) when T1 =< T2 ->
			true;
		(_, _)->
			false
	end,
	SortedBuckets = lists:sort(F, Buckets),
	charge(Type, Charge, Now, Final, SortedBuckets, [], 0).
%% @hidden
charge(Type, Charge, Now, Final, [#bucket{bucket_type = Type,
		termination_date = T1} | T], Acc, Charged) when T1 =/= undefined, T1 =< Now->
	charge(Type, Charge, Now, Final, T, Acc, Charged);
charge(Type, Charge, _Now, true, [#bucket{bucket_type = Type,
		remain_amount = R} = B | T], Acc, Charged) when R > Charge ->
	NewBuckets = [B#bucket{remain_amount = R - Charge} | T],
	{0, Charged + Charge, NewBuckets ++ Acc};
charge(cents, Charge, _Now, false, [#bucket{bucket_type = cents,
		remain_amount = R} = B | T], Acc, Charged) when R > Charge ->
	NewBuckets = [B#bucket{remain_amount = R - Charge} | T],
	{0, Charged + Charge, NewBuckets ++ Acc};
charge(Type, Charge, _Now, false, [#bucket{bucket_type = Type,
		remain_amount = R} | _] = B, Acc, Charged) when R > Charge ->
	{Charge, Charged + Charge, B ++ Acc};
charge(Type, Charge, Now, true, [#bucket{bucket_type = Type,
		remain_amount = R} | T], Acc, Charged) when R =< Charge ->
	charge(Type, Charge - R, Now, true, T, Acc, Charged + R);
charge(Type, Charge, Now, false, [#bucket{bucket_type = Type,
		remain_amount = R}  = B | T], Acc, Charged) when R =< Charge ->
	charge(Type, Charge - R, Now, false, T, [B | Acc], Charged);
charge(_Type, 0, _Now, _Final, Buckets, Acc, Charged) ->
	{0, Charged, Buckets ++ Acc};
charge(Type, Charge, Now, Final, [H | T], Acc, Charged) ->
	charge(Type, Charge, Now, Final, T, [H | Acc], Charged);
charge(_Type, Charge, _Now, _Final, [], Acc, Charged) ->
	{Charge, Charged, Acc}.

-spec purchase(Type, Price, Size, Used, Validity, Final, Buckets) -> Result
	when
		Type :: octets | seconds,
		Price :: integer(),
		Size :: integer(),
		Used :: integer(),
		Validity :: integer(),
		Final :: boolean(),
		Buckets :: [#bucket{}],
		Result :: {RemainingCharge, Charged, Buckets},
		RemainingCharge :: integer(),
		Charged :: integer().
%% @doc Manage usage pricing and debit monetary amount buckets.
%%
%% 	Subscribers are charged at a monetary rate of `Price' cents
%% 	per `Unit' of `Used' service.  The total number of units
%% 	required and total monetary amount is calculated and 
%% 	debited from available cents buckets as in {@link charge/4}.
%%
%% 	If `Final' is `false' a new `Type' bucket with the total
%% 	number of units required and expiration of `Validity' is
%% 	added to `Buckets'.
%%
%% 	Returns `{RemainingCharge, Charged, Buckets}' where
%% 	`Charge' is the total amount debited from the buckets,
%% 	`RemainingCharge' is the left over amount not charged
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
purchase(Type, Price, Size, Used, Validity, Final, Buckets) ->
	UnitsNeeded = case (Used rem Size) of
		0 ->
			Used div Size;
		_ ->
			(Used div Size) + 1
	end,
	Charge = UnitsNeeded * Price,
	case charge(cents, Charge, Final, Buckets) of
		{RemainCharge, Charged, NewBuckets} when Charged < Charge ->
			{RemainCharge, Charged, NewBuckets};
		{RemainCharge, Charged, NewBuckets} when RemainCharge == 0, Charge == Charged ->
			Remain = case Final of
				true ->
					UnitsNeeded * Size - Used;
				false ->
					UnitsNeeded * Size
			end,
			if
				Remain == 0 ->
					{RemainCharge, Charged, NewBuckets};
				true ->
					Bucket = #bucket{bucket_type = Type, remain_amount = Remain,
						termination_date = Validity,
						start_date = erlang:system_time(?MILLISECOND)},
					{RemainCharge, Charged, [Bucket | NewBuckets]}
			end;
		{RemainCharge, Charged, NewBuckets} ->
			{RemainCharge, Charged, NewBuckets}
	end.

%% @hidden
remove_session(SessionList, [Candidate | T]) ->
	remove_session(remove_session1(SessionList, Candidate), T);
remove_session(SessionList, []) ->
	SessionList.
%% @hidden
remove_session1(SessionList, Candidate) ->
	F = fun({Ts, IsCandidate}, Acc)  ->
				case lists:member(Candidate, IsCandidate) of
					true ->
						Acc;
					false ->
						[{Ts, IsCandidate} | Acc]
				end;
		(IsCandidate, Acc)  ->
				case lists:member(Candidate, IsCandidate) of
					true ->
						Acc;
					false ->
						[IsCandidate | Acc]
				end
	end,
	lists:foldl(F, [], SessionList).


%% @private
update_session(SessionIdentification, SessionList) ->
	update_session(SessionIdentification, SessionList, []).
%% @hidden
update_session(SessionIdentification, [], Acc) ->
	Now = erlang:system_time(?MILLISECOND),
	[{Now, SessionIdentification} | Acc];
update_session(SessionIdentification, [{_, Attributes} = H | T] = S, Acc) ->
	case update_session1(SessionIdentification, Attributes) of
		true ->
			S ++ Acc;
		false ->
			update_session(SessionIdentification, T, [H | Acc])
	end.
%% @hidden
update_session1([], _Attributes) ->
	false;
update_session1([Identifier | T], Attributes) ->
	case lists:member(Identifier, Attributes) of
		true ->
			true;
		false ->
			update_session1(T, Attributes)
	end.
