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

-export([rate/6, rate/7]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec rate(Protocol, SubscriberID, Destination, Flag, DebitAmount, ReserveAmount) -> Result
	when
		Protocol :: radius | diameter,
		SubscriberID :: string() | binary(),
		Destination :: string(),
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
%% @equiv rate(Protocol, SubscriberID, Destination, Flag, DebitAmount, ReserveAmount, [])
rate(Protocol, SubscriberID, Destination, Flag, DebitAmount, ReserveAmount) ->
	rate(Protocol, SubscriberID, Destination, Flag, DebitAmount, ReserveAmount, []).

-spec rate(Protocol, SubscriberID, Destination,
		Flag, DebitAmount, ReserveAmount, SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		SubscriberID :: string() | binary(),
		Destination :: string(),
		Flag :: initial | interim | final,
		DebitAmount :: [{Type, Amount}],
		ReserveAmount :: [{Type, Amount}],
		SessionAttributes :: [tuple()],
		Type :: octets | seconds,
		Amount :: integer(),
		Result :: {ok, Subscriber, GrantedAmount} | {out_of_credit, SessionList}
				| {disabled, SessionList} | {error, Reason},
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
%% 	Returns `{ok, Subscriber, GrantedAmount}' if successful.
%%
%% 	Returns `{out_of_credit, SessionList}' if the subscriber's
%% 	balance is insufficient to cover the `DebitAmount' and
%% 	`ReserveAmount' or `{disabled, SessionList}' if the subscriber
%% 	is not enabled. In both cases subscriber's balance is debited.
%% 	`SessionList' describes the known active sessions which
%% 	should be disconnected.
%%
rate(Protocol, SubscriberID, Destination,
		Flag, DebitAmount, ReserveAmount, SessionAttributes) when is_list(SubscriberID)->
	rate(Protocol, list_to_binary(SubscriberID), Destination,
		Flag, DebitAmount, ReserveAmount, SessionAttributes);
rate(Protocol, SubscriberID, Destination, Flag, DebitAmount, ReserveAmount, SessionAttributes)
		when ((Protocol == radius) or (Protocol == diameter)), is_binary(SubscriberID),
		((Flag == initial) or (Flag == interim) or (Flag == final)),
		is_list(DebitAmount), is_list(ReserveAmount), is_list(SessionAttributes) ->
	F = fun() ->
			case mnesia:read(subscriber, SubscriberID, write) of
				[#subscriber{product = #product_instance{product = ProdID,
						characteristics = Chars}} = Subscriber] ->
					case mnesia:read(product, ProdID, read) of
						[#product{} = Product] ->
							Validity = proplists:get_value(validity, Chars),
							rate1(Protocol, Subscriber, Destination, Product, Validity, Flag,
									DebitAmount, ReserveAmount, SessionAttributes);
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
		{atomic, {disabled, SL}} ->
			{disabled, SL};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
rate1(Protocol, Subscriber, Destination, #product{specification = "3", char_value_use = CharValueUse,
		price = Prices}, Validity, Flag, DebitAmount, ReserveAmount, SessionAttributes) ->
	case lists:keyfind("destPrefixPriceTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = PriceTable}]} ->
			rate2(Protocol, PriceTable, Subscriber, Destination, Prices,
					Validity, Flag, DebitAmount, ReserveAmount, SessionAttributes);
		false ->
			 case lists:keyfind("destPrefixTariffTable", #char_value_use.name, CharValueUse) of
				#char_value_use{values = [#char_value{value = TariffTable}]} ->
					rate3(Protocol, TariffTable, Subscriber, Destination, Prices,
							Validity, Flag, DebitAmount, ReserveAmount, SessionAttributes);
				_ ->
					throw(table_prefix_not_found)
			end
	end;
rate1(Protocol, Subscriber, _Destiations, #product{price = Prices}, Validity,
		Flag, DebitAmount, ReserveAmount, SessionAttributes) ->
	case lists:keyfind(usage, #price.type, Prices) of
		#price{} = Price ->
			rate4(Protocol, Subscriber, Price, Validity,
				Flag, DebitAmount, ReserveAmount, SessionAttributes);
		false ->
			throw(price_not_found)
	end.
%% @hidden
rate2(Protocol, PriceTable, Subscriber, Destination, Prices,
		Validity, Flag, DebitAmount, ReserveAmount, SessionAttributes) ->
	case catch ocs_gtt:lookup_last(PriceTable, Destination) of
		PriceKey when is_list(PriceKey) ->
			F = fun(#price{char_value_use = CharValueUse}) ->
					case lists:keyfind("ratePrice", #char_value_use.name, CharValueUse) of
						#char_value_use{values = [#char_value{value = PriceKey}]} ->
							true;
						false ->
							false
					end
			end,
			case lists:filter(F, Prices) of
				[Price] ->
					rate4(Protocol, Subscriber, Price, Validity,
							Flag, DebitAmount, ReserveAmount, SessionAttributes);
				[] ->
					throw(price_not_found)
			end;
		_ ->
			throw(rating_failed)
	end.
%% @hidden
rate3(Protocol, TariffTable, Subscriber, Destination, Prices,
		Validity, Flag, DebitAmount, ReserveAmount, SessionAttributes) ->
	case catch ocs_gtt:lookup_last(TariffTable, Destination) of
		Amount when is_integer(Amount) ->
			case lists:keyfind(tariffUsage, #price.type, Prices) of
				#price{} = Price ->
					rate4(Protocol, Subscriber, Price#price{amount = Amount}, Validity,
							Flag, DebitAmount, ReserveAmount, SessionAttributes);
				false ->
					throw(price_not_found)
			end;
		_ ->
			throw(rating_failed)
	end.
%% @hidden
rate4(Protocol, Subscriber, Price, Validity, Flag, [], ReserveAmount, SessionAttributes) ->
	rate5(Protocol, Subscriber, Price, Validity, Flag, ReserveAmount, SessionAttributes);
rate4(Protocol, #subscriber{buckets = Buckets, enabled = Enabled} = Subscriber,
		#price{units = Type, size = Size, amount = Amount} = Price, Validity, Flag,
		DebitAmount, ReserveAmount, SessionAttributes) ->
	try
		{Type, Used} = lists:keyfind(Type, 1, DebitAmount),
		case charge(Type, Used, true, Buckets) of
			{R1, _C1, NB1} when R1 > 0 ->
				purchase(Type, Amount, Size, R1, Validity, true, NB1);
			{R1, C1, NB1} ->
				{R1, C1, NB1}
		end
	of
		{RemainingCharge, _Charged, NewBuckets}
				when Enabled == false; RemainingCharge > 0 ->
			rate6(Subscriber#subscriber{buckets = NewBuckets}, Flag,
					RemainingCharge, ReserveAmount, SessionAttributes);
		{_RemainingCharge, _Charged, NewBuckets} ->
			rate5(Protocol, Subscriber#subscriber{buckets = NewBuckets},
					Price, Validity, Flag, ReserveAmount, SessionAttributes)
	catch
		_:_ ->
			throw(price_not_found)
	end.
%% @hidden
rate5(radius, Subscriber, _Price, _Validity, final,
		_ReserveAmount, SessionAttributes) ->
	rate6(Subscriber, final, 0, 0, SessionAttributes);
rate5(radius, Subscriber, #price{units = Units, size = Size,
		amount = Amount, char_value_use = CharValueUse}, Validity, Flag,
		ReserveAmount, SessionAttributes) ->
	CharName = case Units of
		seconds ->
			"radiusReserveTime";
		octets ->
			"radiusReserveBytes"
	end,
	Reserve = case lists:keyfind(Units, 1, ReserveAmount) of
		{_, R} ->
			R;
		false ->
			0
	end,
	RadiusReserve = case lists:keyfind(CharName,
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = Value}]} ->
			Reserve + Value;
		false ->
			Reserve
	end,
	case RadiusReserve of
		0 ->
			rate6(Subscriber, Flag, 0, 0, SessionAttributes);
		_ ->
			rate5(Subscriber, Units, Amount, Size, RadiusReserve,
					Validity, Flag, SessionAttributes)
	end;
rate5(diameter, Subscriber, _Price, _Validity, Flag, [], SessionAttributes) ->
	rate6(Subscriber, Flag, 0, 0, SessionAttributes);
rate5(diameter, Subscriber, #price{units = Type, size = Size, amount = Amount},
		Validity, Flag, ReserveAmount, SessionAttributes) ->
	{Type, Reserve} = lists:keyfind(Type, 1, ReserveAmount),
	rate5(Subscriber, Type, Amount, Size, Reserve, Validity, Flag, SessionAttributes).
%% @hidden
rate5(#subscriber{buckets = Buckets} = Subscriber,
		Type, Price, Size, ReserveAmount, Validity, Flag, SessionAttributes) ->
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
			rate6(Subscriber#subscriber{buckets = NewBuckets},
					Flag, 0, ReservedAmount, SessionAttributes);
		{RemainingCharge, ReservedAmount, _NewBuckets} ->
			rate6(Subscriber, Flag, RemainingCharge,
				ReservedAmount, SessionAttributes)
	catch
		_:_ ->
			throw(rating_failed)
	end.
%% @hidden
rate6(#subscriber{session_attributes = SessionList} = Subscriber,
		_Flag, RemainingCharge, _ReserveAmount, _SessionAttributes)
		when RemainingCharge > 0 ->
	Entry = Subscriber#subscriber{session_attributes = []},
	ok = mnesia:write(Entry),
	{out_of_credit, SessionList};
rate6(#subscriber{enabled = false,
		session_attributes = SessionList} = Subscriber,
		Flag_, _RemainingCharge, _ReserveAmount, _SessionAttributes) ->
	Entry = Subscriber#subscriber{session_attributes = []},
	ok = mnesia:write(Entry),
	{disabled, SessionList};
rate6(#subscriber{session_attributes = SessionList} = Subscriber,
		initial, _RemainingCharge, ReserveAmount, SessionAttributes) ->
	NewSessionList = update_session(SessionAttributes, SessionList),
	Entry = Subscriber#subscriber{session_attributes = NewSessionList},
	ok = mnesia:write(Entry),
	{grant, Entry, ReserveAmount};
rate6(#subscriber{session_attributes = SessionList} = Subscriber,
		final, _RemainingCharge, ReserveAmount, SessionAttributes) ->
	NewSessionList = remove_session(SessionList, SessionAttributes),
	Entry = Subscriber#subscriber{session_attributes = NewSessionList},
	ok = mnesia:write(Entry),
	{grant, Entry, ReserveAmount};
rate6(Subscriber, interim, _RemainingCharge, ReserveAmount, _SessionAttributes) ->
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
charge(Type, Charge, Now, Final, [#bucket{units = Type,
		termination_date = T1} | T], Acc, Charged) when T1 =/= undefined, T1 =< Now->
	charge(Type, Charge, Now, Final, T, Acc, Charged);
charge(Type, Charge, _Now, true, [#bucket{units = Type,
		remain_amount = R} = B | T], Acc, Charged) when R > Charge ->
	NewBuckets = [B#bucket{remain_amount = R - Charge} | T],
	{0, Charged + Charge, lists:reverse(Acc) ++ NewBuckets};
charge(Type, Charge, _Now, false, [#bucket{units = Type,
		remain_amount = R} | _] = L, Acc, Charged) when R > Charge ->
	{0, Charged + Charge, lists:reverse(Acc) ++ L};
charge(Type, Charge, Now, true, [#bucket{units = Type,
		remain_amount = R} | T], Acc, Charged) when R =< Charge ->
	charge(Type, Charge - R, Now, true, T, Acc, Charged + R);
charge(Type, Charge, Now, false, [#bucket{units = Type,
		remain_amount = R} = B | T], Acc, Charged) when R =< Charge ->
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
			Bucket = #bucket{units = Type,
				remain_amount = UnitsNeeded * Size,
				termination_date = Validity,
				start_date = erlang:system_time(?MILLISECOND)},
			{0, UnitsNeeded * Size, [Bucket | NewBuckets]};
		{0, Charge, NewBuckets} when Final == true ->
			Bucket = #bucket{units = Type,
				remain_amount = UnitsNeeded * Size - Used,
				termination_date = Validity,
				start_date = erlang:system_time(?MILLISECOND)},
			{0, UnitsNeeded * Size, [Bucket | NewBuckets]};
		{_RemainingCharge, Charged, NewBuckets} ->
			UnitsCharged = Charged div Price,
			{UnitsNeeded - UnitsCharged, UnitsCharged, NewBuckets}
	end.

-spec remove_session(SessionAttributes, SessionList) ->NewSessionList
	when
		SessionAttributes :: [tuple()],
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

-spec update_session(SessionAttributes, SessionList) ->NewSessionList
	when
		SessionAttributes :: [tuple()],
		SessionList :: [tuple()],
		NewSessionList :: [tuple()].
%% @doc Add new session identification attributes set to active sessions list.
%% @private
update_session(SessionAttributes, SessionList) ->
	update_session(SessionAttributes, SessionList, []).
%% @hidden
update_session(SessionAttributes, [], Acc) ->
	Now = erlang:system_time(?MILLISECOND),
	[{Now, SessionAttributes} | Acc];
update_session(SessionAttributes, [{_, Attributes} = H | T] = S, Acc) ->
	case update_session1(SessionAttributes, Attributes) of
		true ->
			S ++ Acc;
		false ->
			update_session(SessionAttributes, T, [H | Acc])
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

