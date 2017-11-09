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

-export([rate/5, rate/6]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount) -> Result
	when
		Protocol :: radius | diameter,
		SubscriberID :: string() | binary(),
		Flag :: initial | interim | final,
		DebitAmount :: [{Type, Amount}],
		ReserveAmount :: [{Type, Amount}],
		Type :: octets | seconds,
		Amount :: integer(),
		Result :: {ok, Subscriber, GrantedAmount} | {out_of_credit, SessionList} | {error, Reason},
		Subscriber :: #subscriber{},
		GrantedAmount :: integer(),
		SessionList :: [tuple()],
		Reason :: term().
%% @equiv rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount, [])
rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount) ->
	rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount, []).

-spec rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount, SessionIdentification) -> Result
	when
		Protocol :: radius | diameter,
		SubscriberID :: string() | binary(),
		Flag :: initial | interim | final,
		DebitAmount :: [{Type, Amount}],
		ReserveAmount :: [{Type, Amount}],
		SessionIdentification :: [tuple()],
		Type :: octets | seconds,
		Amount :: integer(),
		Result :: {ok, Subscriber, GrantedAmount} | {out_of_credit, SessionList} | {error, Reason},
		Subscriber :: #subscriber{},
		GrantedAmount :: integer(),
		SessionList :: [tuple()],
		Reason :: term().
%% @doc Handle rating and balance management for used and reserved unit amounts.
%%
%% 	Subscriber balance buckets are permanently reduced by the
%% 	amount(s) in `DebitAmount' and `Type' buckets are allocated
%% 	by the amount(s) in `ReserveAmount'. The subscribed product
%% 	determines the price used to calculate the amount to be
%% 	permanently debited from available `cents' buckets.
%%
%% 	Returns `{ok, Subscriber, GrantedAmount}' if successful or
%% 	`{out_of_credit, SessionList}' if the subscriber's balance
%% 	is insufficient to cover the `DebitAmount' and `ReserveAmount'.
%% 	`SessionList' describes the known avtive sessions which
%% 	should be disconnected.
%%
rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount, SessionIdentification) when is_list(SubscriberID)->
	rate(Protocol, list_to_binary(SubscriberID), Flag, DebitAmount, ReserveAmount, SessionIdentification);
rate(Protocol, SubscriberID, Flag, DebitAmount, ReserveAmount, SessionIdentification)
		when ((Protocol == radius) or (Protocol == diameter)), is_binary(SubscriberID),
		((Flag == initial) or (Flag == interim) or (Flag == final)),
		is_list(DebitAmount), is_list(ReserveAmount), is_list(SessionIdentification) ->
	F = fun() ->
			case mnesia:read(subscriber, SubscriberID, write) of
				[#subscriber{product = #product_instance{product = ProdID,
						characteristics = Chars}} = Subscriber] ->
					case mnesia:read(product, ProdID, read) of
						[#product{price = Prices}] ->
							Validity = proplists:get_value(validity, Chars),
							rate1(Protocol, Subscriber, Prices, Validity, Flag,
									DebitAmount, ReserveAmount, SessionIdentification);
						[] ->
							throw(product_not_found)
					end;
				[] ->
					throw(subsriber_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {grant, Sub, GrantedAmount}} ->
			{ok, Sub, GrantedAmount};
		{atomic, {out_of_credit, SL}} ->
			{out_of_credit, SL};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
rate1(Protocol, Subscriber, Prices, Validity, Flag, [], ReserveAmount, SessionIdentification) ->
	rate2(Protocol, Subscriber, Prices, Validity, Flag, ReserveAmount, SessionIdentification);
rate1(Protocol, #subscriber{buckets = Buckets} = Subscriber, Prices, Validity, Flag,
		DebitAmount, ReserveAmount, SessionIdentification) ->
	try
		#price{units = Type, size = Size, amount = Price} = lists:keyfind(usage, #price.type, Prices),
		{Type, Used} = lists:keyfind(Type, 1, DebitAmount),
		case charge(Type, Used, true, Buckets) of
			{R1, _C1, NB1} when R1 > 0 ->
				purchase(Type, Price, Size, R1, Validity, true, NB1);
			{R1, C1, NB1} ->
				{R1, C1, NB1}
		end
	of
		{RemainingCharge, _Charged, NewBuckets}  when RemainingCharge > 0 ->
			rate3(Subscriber#subscriber{buckets = NewBuckets},
					RemainingCharge, Flag, ReserveAmount, SessionIdentification);
		{_RemainingCharge, _Charged, NewBuckets} ->
			rate2(Protocol, Subscriber#subscriber{buckets = NewBuckets},
					Prices, Validity, Flag, ReserveAmount, SessionIdentification)
	catch
		_:_ ->
			throw(price_not_found)
	end.
%% @hidden
rate2(radius, #subscriber{product = #product_instance{characteristics = Chars}} =
		Subscriber, Prices, Validity, initial = Flag, _ReserveAmount, SessionIdentification)  ->
	{Key, Price} = case lists:keyfind(usage, #price.type, Prices) of
		#price{units = seconds} = P ->
			{"radiusReserveTime", P};
		#price{units = octets} = P ->
			{"radiusReserveOctets", P};
		false ->
			throw(price_not_found)
	end,
	{_, RadiusReserve} = lists:keyfind(Key, 1, Chars),
	RadiusReserveAmount = reservation_amount(RadiusReserve),
	#price{units = Type, size = Size, amount = Amount} = Price,
	rate2_1(Subscriber, Type, Amount, Size, RadiusReserveAmount, Validity, Flag, SessionIdentification);
rate2(radius, #subscriber{product = #product_instance{characteristics = Chars}} =
		Subscriber, Prices, Validity, interim = Flag, ReserveAmount, SessionIdentification)  ->
	{Key, Price} = case lists:keyfind(usage, #price.type, Prices) of
		#price{units = seconds} = P ->
			{"radiusReserveTime", P};
		#price{units = octets} = P ->
			{"radiusReserveOctets", P};
		false ->
			throw(price_not_found)
	end,
	{_, RadiusReserve} = lists:keyfind(Key, 1, Chars),
	RadiusReserveAmount = reservation_amount(RadiusReserve),
	#price{units = Type, size = Size, amount = Amount} = Price,
	{Type, Reserve} = lists:keyfind(Type, 1, ReserveAmount),
	TotalReserveAmount = RadiusReserveAmount + Reserve,
	rate2_1(Subscriber, Type, Amount, Size, TotalReserveAmount, Validity, Flag, SessionIdentification);
rate2(_Protocol, Subscriber, _Prices, _Validity, Flag, [], SessionIdentification)  ->
	rate3(Subscriber, 0, Flag, 0, SessionIdentification);
rate2(_Protocol, Subscriber, Prices, Validity, Flag, ReserveAmount, SessionIdentification)  ->
	#price{units = Type, size = Size, amount = Amount} = lists:keyfind(usage, #price.type, Prices),
	{Type, Reserve} = lists:keyfind(Type, 1, ReserveAmount),
	rate2_1(Subscriber, Type, Amount, Size, Reserve, Validity, Flag, SessionIdentification).
%% @hidden
rate2_1(#subscriber{buckets = Buckets} = Subscriber,
		Type, Price, Size, ReserveAmount, Validity, Flag, SessionIdentification) ->
	try
		case charge(Type, ReserveAmount, false, Buckets) of
			{R1, C1, NB1} when R1 > 0 ->
				{R2, C2, NB2} = purchase(Type, Price, Size, R1, Validity, false, NB1),
				{R2, C1 + C2, NB2};
			{R1, C1, NB1} ->
				{R1, C1, NB1}
		end
	of
		{0, ReservedAmount, NewBuckets} ->
			rate3(Subscriber#subscriber{buckets = NewBuckets},
					0, Flag, ReservedAmount, SessionIdentification);
		{RemainingCharge, ReservedAmount, _NewBuckets} ->
			rate3(Subscriber, RemainingCharge,
				Flag, ReservedAmount, SessionIdentification)
	catch
		_:_ ->
			throw(rating_faild)
	end.
%% @hidden
rate3(#subscriber{session_attributes = SessionList} = Subscriber,
		RemainingCharge, _Flag, _ReserveAmount, _SessionIdentification)
		when RemainingCharge > 0 ->
	Entry = Subscriber#subscriber{session_attributes = [],
		enabled = false},
	ok = mnesia:write(Entry),
	{out_of_credit, SessionList};
rate3(#subscriber{enabled = false} = Subscriber,
		_RemainingCharge, _Flag, ReserveAmount, _SessionIdentification) ->
	Entry = Subscriber#subscriber{session_attributes = []},
	ok = mnesia:write(Entry),
	{ok, Subscriber, ReserveAmount};
rate3(#subscriber{session_attributes = SessionList} = Subscriber,
		_RemainingCharge, initial, ReserveAmount, SessionIdentification) ->
	NewSessionList = update_session(SessionIdentification, SessionList),
	Entry = Subscriber#subscriber{session_attributes = NewSessionList},
	ok = mnesia:write(Entry),
	{grant, Entry, ReserveAmount};
rate3(#subscriber{session_attributes = SessionList} = Subscriber,
		_RemainingCharge, final, ReserveAmount, SessionIdentification) ->
	NewSessionList = remove_session(SessionList, SessionIdentification),
	Entry = Subscriber#subscriber{session_attributes = NewSessionList},
	ok = mnesia:write(Entry),
	{grant, Entry, ReserveAmount};
rate3(Subscriber, _RemainingCharge, interim, ReserveAmount, _SessionIdentification) ->
	ok = mnesia:write(Subscriber),
	{grant, Subscriber, ReserveAmount}.

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
%% 	Returns `{RemainingCharge, Charged, NewBuckets}' where
%% 	`Charged' is the total amount debited from the buckets,
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
	{0, Charged + Charge, lists:reverse(Acc) ++ NewBuckets};
charge(Type, Charge, _Now, false, [#bucket{bucket_type = Type,
		remain_amount = R} | _] = B, Acc, Charged) when R > Charge ->
	{0, Charged + Charge, B ++ Acc};
charge(Type, Charge, Now, true, [#bucket{bucket_type = Type,
		remain_amount = R} | T], Acc, Charged) when R =< Charge ->
	charge(Type, Charge - R, Now, true, T, Acc, Charged + R);
charge(Type, Charge, Now, false, [#bucket{bucket_type = Type,
		remain_amount = R}  = B | T], Acc, Charged) when R =< Charge ->
	charge(Type, Charge - R, Now, false, T, [B | Acc], Charged);
charge(_Type, 0, _Now, _Final, Buckets, Acc, Charged) ->
	{0, Charged, lists:reverse(Acc) ++ Buckets};
charge(Type, Charge, Now, Final, [H | T], Acc, Charged) ->
	charge(Type, Charge, Now, Final, T, [H | Acc], Charged);
charge(_Type, Charge, _Now, _Final, [], Acc, Charged) ->
	{Charge, Charged, lists:reverse(Acc)}.

-spec purchase(Type, Price, Size, Used, Validity, Final, Buckets) -> Result
	when
		Type :: octets | seconds,
		Price :: integer(),
		Size :: integer(),
		Used :: integer(),
		Validity :: integer(),
		Final :: boolean(),
		Buckets :: [#bucket{}],
		Result :: {RemainingUnits, UnitsCharged, NewBuckets},
		RemainingUnits :: integer(),
		UnitsCharged :: integer(),
		NewBuckets :: [#bucket{}].
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
%% 	Returns `{RemainingUnits, UnitsCharged, NewBuckets}' where
%% 	`UnitsCharged' is the total amount of units in the newly
%%		created usage bucket, `RemainingUnits' is the left over
%%		amount not charged and `NewBuckets' is the updated bucket list.
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
	case charge(cents, Charge, true, Buckets) of
		{0, Charge, NewBuckets} when Final == true,
				(UnitsNeeded * Size - Used) == 0 ->
			{0, UnitsNeeded * Size, NewBuckets};
		{0, Charge, NewBuckets} when Final == false ->
			Bucket = #bucket{bucket_type = Type,
				remain_amount = UnitsNeeded * Size,
				termination_date = Validity,
				start_date = erlang:system_time(?MILLISECOND)},
			{0, UnitsNeeded * Size, [Bucket | NewBuckets]};
		{0, Charge, NewBuckets} when Final == true ->
			Bucket = #bucket{bucket_type = Type,
				remain_amount = UnitsNeeded * Size - Used,
				termination_date = Validity,
				start_date = erlang:system_time(?MILLISECOND)},
			{0, UnitsNeeded * Size, [Bucket | NewBuckets]};
		{_RemainingCharge, Charged, NewBuckets} ->
			UnitsCharged = Charged div Price,
			{UnitsNeeded - UnitsCharged, UnitsCharged, NewBuckets}
	end.

-spec remove_session(SessionIdentification, SessionList) ->NewSessionList
	when
		SessionIdentification :: [tuple()],
		SessionList :: [tuple()],
		NewSessionList :: [tuple()].
%% @doc Remove session identification attributes set from active sessions list.
%% @private
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

-spec update_session(SessionIdentification, SessionList) ->NewSessionList
	when
		SessionIdentification :: [tuple()],
		SessionList :: [tuple()],
		NewSessionList :: [tuple()].
%% @doc Add new session identification attributes set to active sessions list.
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

%% @hidden
reservation_amount([{type, seconds}, {value, Value}]) -> Value;
reservation_amount([{value, Value}, {type, seconds}]) -> Value;
reservation_amount([{type, minutes}, {value, Value}]) -> Value * 60;
reservation_amount([{value, Value}, {type, minutes}]) -> Value * 60;
reservation_amount([{type, bytes}, {value, Value}]) -> Value;
reservation_amount([{value, Value}, {type, bytes}]) -> Value;
reservation_amount([{type, kilobytes}, {value, Value}]) -> Value * 1000;
reservation_amount([{value, Value}, {type, kilobytes}]) -> Value * 1000;
reservation_amount([{type, megabytes}, {value, Value}]) -> Value * 1000000;
reservation_amount([{value, Value}, {type, megabytes}]) -> Value * 1000000;
reservation_amount([{type, gigabytes}, {value, Value}]) -> Value * 1000000000;
reservation_amount([{value, Value}, {type, gigabytes}]) -> Value * 1000000000.
