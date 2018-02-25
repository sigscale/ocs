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

-export([rate/10]).
-export([authorize/8]).
-export([session_attributes/1]).

-include("ocs.hrl").
-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%% service types for radius
-define(RADIUSLOGIN, 1).
-define(RADIUSFRAMED, 2).
-define(RADIUSVOICE, 12).
%% service types for diameter
-define(DIAMETERDATA, 32251).
-define(DIAMETERVOICE, 32260).


-spec rate(Protocol, ServiceType, SubscriberID, Timestamp,
		Address, Direction, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		ServiceType :: integer() | binary(),
		SubscriberID :: string() | binary(),
		Timestamp :: calendar:datetime(),
		Address :: string(),
		Direction :: answer | originate | undefined,
		Flag :: initial | interim | final,
		DebitAmounts :: [{Type, Amount}],
		ReserveAmounts :: [{Type, Amount}],
		SessionAttributes :: [tuple()],
		Type :: octets | seconds,
		Amount :: integer(),
		Result :: {ok, Subscriber, Rated} | {out_of_credit, SessionList}
				| {disabled, SessionList} | {error, Reason},
		Subscriber :: #subscriber{},
		Rated :: #rated{},
		SessionList :: [{pos_integer(), [tuple()]}],
		Reason :: term().
%% @doc Handle rating and balance management for used and reserved unit amounts.
%%
%% 	Subscriber balance buckets are permanently reduced by the
%% 	amount(s) in `DebitAmounts' and `Type' buckets are allocated
%% 	by the amount(s) in `ReserveAmounts'. The subscribed product
%% 	determines the price used to calculate the amount to be
%% 	permanently debited from available `cents' buckets.
%%
%% 	Returns `{ok, Subscriber, Rated}' if successful. The granted
%% 	amount is found in `Rated#rated.bucket_value'.
%%
%% 	Returns `{out_of_credit, SessionList}' if the subscriber's
%% 	balance is insufficient to cover the `DebitAmounts' and
%% 	`ReserveAmounts' or `{disabled, SessionList}' if the subscriber
%% 	is not enabled. In both cases subscriber's balance is debited.
%% 	`SessionList' describes the known active sessions which
%% 	should be disconnected.
%%
rate(Protocol, ServiceType, SubscriberID, Timestamp, Address, Direction,
		Flag, DebitAmounts, ReserveAmounts, SessionAttributes)
		when is_list(SubscriberID)->
	rate(Protocol, ServiceType, list_to_binary(SubscriberID), Timestamp,
		Address, Direction, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes);
rate(Protocol, ServiceType, SubscriberID, Timestamp, Address, Direction,
		Flag, DebitAmounts, ReserveAmounts, SessionAttributes) when
		((Protocol == radius) or (Protocol == diameter)), is_binary(SubscriberID),
		((Flag == initial) or (Flag == interim) or (Flag == final)),
		is_list(DebitAmounts), is_list(ReserveAmounts), length(SessionAttributes) > 0 ->
	F = fun() ->
			case mnesia:read(subscriber, SubscriberID, write) of
				[#subscriber{buckets = Buckets, product =
						#product_instance{product = ProdID,
						characteristics = Chars}} = Subscriber] ->
					case mnesia:read(product, ProdID, read) of
						[#product{} = Product] ->
							Validity = proplists:get_value(validity, Chars),
							Subscriber1 = Subscriber#subscriber{buckets = due(Buckets)},
							rate1(Protocol, ServiceType, Subscriber1, Timestamp,
									Address, Direction, Product, Validity, Flag,
									DebitAmounts, ReserveAmounts, SessionAttributes);
						[] ->
							throw(product_not_found)
					end;
				[] ->
					throw(subscriber_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {grant, Sub, GrantedAmount}} ->
			{ok, Sub, #rated{bucket_value = GrantedAmount}};
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
rate1(Protocol, ServiceType, Subscriber, Timestamp, Address, Direction,
		#product{specification = undefined, bundle = Bundle},
		Validity, Flag, DebitAmounts, ReserveAmounts, SessionAttributes) ->
	try
		F = fun(#bundled_po{name = ProdId}, Acc) ->
				case mnesia:read(product, ProdId, read) of
					[#product{specification = Spec, status = Status} = P] when
							((Status == active) orelse (Status == undefined))
							and
							(((Protocol == radius)
								and
								(((ServiceType == ?RADIUSFRAMED) orelse (ServiceType == ?RADIUSLOGIN)) and ((Spec == "4") orelse (Spec == "8")))
								orelse
								((ServiceType == ?RADIUSVOICE) and ((Spec == "5") orelse (Spec == "9"))))
							orelse
							((Protocol == diameter)
								and
								((ServiceType == ?DIAMETERDATA) and ((Spec == "4") orelse (Spec == "8")))
								orelse
								((ServiceType == ?DIAMETERVOICE) and ((Spec == "5") orelse (Spec == "9"))))) ->
						[P | Acc];
					_ ->
						Acc
				end
		end,
		[#product{name = ProductName} = Product | _] = lists:foldl(F, [], Bundle),
		rate2(Protocol, Subscriber, Timestamp, Address, Direction, Product,
				Validity, Flag, DebitAmounts, ReserveAmounts, SessionAttributes,
				#rated{product = ProductName})
	catch
		_:_ ->
			throw(invalid_bundle_product)
	end;
rate1(Protocol, _ServiceType, Subscriber, Timestamp, Address, Direction,
		#product{name = ProductName} = Product, Validity, Flag, DebitAmounts,
		ReserveAmounts, SessionAttributes) ->
	rate2(Protocol, Subscriber, Timestamp, Address, Direction, Product,
		Validity, Flag, DebitAmounts, ReserveAmounts, SessionAttributes,
		#rated{product = ProductName}).
%% @hidden
rate2(Protocol, Subscriber, Timestamp, Address, Direction,
		#product{specification = ProdSpec, price = Prices},
		Validity, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes, Rated) when ProdSpec == "5"; ProdSpec == "9" ->
	F = fun(#price{type = tariff, units = seconds}) ->
				true;
			(#price{type = usage, units = seconds}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_tod(Timestamp, FilteredPrices1),
	case filter_prices_dir(Direction, FilteredPrices2) of
		[Price | _] ->
			rate3(Protocol, Subscriber, Address, Price, Validity, Flag,
					DebitAmounts, ReserveAmounts, SessionAttributes, Rated);
		_ ->
			throw(price_not_found)
	end;
rate2(Protocol, Subscriber, Timestamp, _Address, _Direction,
		#product{price = Prices}, Validity, Flag,
		DebitAmounts, ReserveAmounts, SessionAttributes, Rated) ->
	F = fun(#price{type = usage}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	case filter_prices_tod(Timestamp, FilteredPrices1) of
		[Price | _] ->
			rate4(Protocol, Subscriber, Price, Validity, Flag,
					DebitAmounts, ReserveAmounts, SessionAttributes, Rated);
		_ ->
			throw(price_not_found)
	end.
%% @hidden
rate3(Protocol, Subscriber, Address,
		#price{type = tariff, char_value_use = CharValueUse} = Price,
		Validity, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes, Rated) ->
	case lists:keyfind("destPrefixTariffTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = TariffTable}]} ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{Description, Amount} ->
					case Amount of
						N when N >= 0 ->
							rate4(Protocol, Subscriber, Price#price{amount = N},
									Validity, Flag, DebitAmounts, ReserveAmounts,
									SessionAttributes, Rated#rated{price_type = tariff,
									description = Description});
						_N ->
							throw(negative_amount)
					end;
				Other ->
					error_logger:error_report(["Prefix table tariff lookup failed",
							{module, ?MODULE}, {table, Table},
							{address, Address}, {result, Other}]),
					throw(table_lookup_failed)
			end;
		false ->
			throw(undefined_tariff)
	end;
rate3(Protocol, Subscriber, _Address, Price, Validity, Flag,
		DebitAmounts, ReserveAmounts, SessionAttributes, Rated) ->
	rate4(Protocol, Subscriber, Price, Validity, Flag,
			DebitAmounts, ReserveAmounts, SessionAttributes, Rated).
%% @hidden
rate4(_Protocol, #subscriber{enabled = false} = Subscriber, _Price,
		_Validity, initial, _DebitAmounts, _ReserveAmounts,
		SessionAttributes, Rated) ->
	SessionId = get_session_id(SessionAttributes),
	rate6(Subscriber, initial, 0, 0, 0, 0, SessionId, Rated);
rate4(radius, Subscriber, Price, Validity,
		initial, [], [], SessionAttributes, Rated) ->
	rate5(Subscriber, Price, Validity, initial,
			0, get_reserve(Price), SessionAttributes, Rated);
rate4(radius, Subscriber, #price{units = Units} = Price, Validity,
		interim, [], ReserveAmounts, SessionAttributes, Rated) ->
	ReserveAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{_, ReserveUnits} ->
			ReserveUnits + get_reserve(Price);
		false ->
			get_reserve(Price)
	end,
	rate5(Subscriber, Price, Validity, interim,
			0, ReserveAmount, SessionAttributes, Rated);
rate4(_Protocol, Subscriber, #price{units = Units} = Price, Validity,
		Flag, DebitAmounts, ReserveAmounts, SessionAttributes, Rated) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{_, DebitUnits} ->
			DebitUnits;
		false ->
			0
	end,
	ReserveAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{_, ReserveUnits} ->
			ReserveUnits;
		false ->
			0
	end,
	rate5(Subscriber, Price, Validity, Flag,
			DebitAmount, ReserveAmount, SessionAttributes, Rated).
%% @hidden
rate5(#subscriber{buckets = Buckets1} = Subscriber,
		#price{units = Units, size = UnitSize, amount = UnitPrice},
		_Validity, initial, 0, ReserveAmount, SessionAttributes, Rated) ->
	SessionId = get_session_id(SessionAttributes),
	case reserve_session(Units, ReserveAmount, SessionId, Buckets1) of
		{ReserveAmount, Buckets2} ->
			rate6(Subscriber#subscriber{buckets = Buckets2},
					initial, 0, 0, ReserveAmount, ReserveAmount,
					SessionId, Rated);
		{UnitsReserved, Buckets2} ->
			PriceReserveUnits = (ReserveAmount - UnitsReserved),
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case reserve_session(cents, PriceReserve, SessionId, Buckets2) of
				{PriceReserve, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3},
							initial, 0, 0, ReserveAmount,
							UnitsReserved + UnitReserve, SessionId, Rated);
				{PriceReserved, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3},
							initial, 0, 0, ReserveAmount,
							UnitsReserved + (PriceReserved div UnitPrice),
							SessionId, Rated)
			end
	end;
rate5(#subscriber{enabled = false, buckets = Buckets1} = Subscriber,
		#price{units = Units, size = UnitSize, amount = UnitPrice},
		_Validity, interim, DebitAmount, _ReserveAmount,
		SessionAttributes, Rated) ->
	SessionId = get_session_id(SessionAttributes),
	case update_session(Units, DebitAmount, 0, SessionId, Buckets1) of
		{DebitAmount, 0, Buckets2} ->
			rate6(Subscriber#subscriber{buckets = Buckets2}, interim,
					DebitAmount, DebitAmount, 0, 0, SessionId, Rated);
		{UnitsCharged, 0, Buckets2} ->
			PriceChargeUnits = DebitAmount - UnitsCharged,
			{UnitCharge, PriceCharge} = price_units(PriceChargeUnits,
					UnitSize, UnitPrice),
			case update_session(cents, PriceCharge, 0, SessionId, Buckets2) of
				{PriceCharge, 0, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3}, interim,
							DebitAmount, DebitAmount + UnitCharge, 0, 0,
							SessionId, Rated);
				{PriceCharged, 0, Buckets3} ->
					Buckets4 = [#bucket{remain_amount = PriceCharged - PriceCharge,
							units = cents} | Buckets3],
					rate6(Subscriber#subscriber{buckets = Buckets4}, interim, DebitAmount,
							UnitsCharged + (PriceCharged div UnitPrice), 0, 0,
							SessionId, Rated)
			end
	end;
rate5(#subscriber{buckets = Buckets1} = Subscriber,
		#price{units = Units, size = UnitSize, amount = UnitPrice},
		_Validity, interim, DebitAmount, ReserveAmount,
		SessionAttributes, Rated) ->
	SessionId = get_session_id(SessionAttributes),
	case update_session(Units, DebitAmount, ReserveAmount,
			SessionId, Buckets1) of
		{DebitAmount, ReserveAmount, Buckets2} ->
			rate6(Subscriber#subscriber{buckets = Buckets2}, interim,
					DebitAmount, DebitAmount, ReserveAmount,
					ReserveAmount, SessionId, Rated);
		{DebitAmount, UnitsReserved, Buckets2} ->
			PriceReserveUnits = ReserveAmount - UnitsReserved,
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case update_session(cents, 0, PriceReserve, SessionId, Buckets2) of
				{0, PriceReserve, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3}, interim,
							DebitAmount, DebitAmount, ReserveAmount,
							UnitReserve, SessionId, Rated);
				{0, PriceReserved, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3}, interim,
							DebitAmount, DebitAmount, ReserveAmount,
							UnitsReserved + PriceReserved div UnitPrice,
							SessionId, Rated)
			end;
		{UnitsCharged, 0, Buckets2} ->
			PriceChargeUnits = DebitAmount - UnitsCharged,
			{UnitCharge, PriceCharge} = price_units(PriceChargeUnits,
					UnitSize, UnitPrice),
			{UnitReserve, PriceReserve} = price_units(ReserveAmount,
					UnitSize, UnitPrice),
			case update_session(cents,
					PriceCharge, PriceReserve, SessionId, Buckets2) of
				{PriceCharge, PriceReserve, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3}, interim,
							DebitAmount, UnitsCharged + UnitCharge, ReserveAmount,
							UnitReserve, SessionId, Rated);
				{PriceCharge, PriceReserved, Buckets3} ->
					rate6(Subscriber#subscriber{buckets = Buckets3}, interim,
							DebitAmount, UnitsCharged + UnitCharge, ReserveAmount,
							PriceReserved div UnitPrice, SessionId, Rated);
				{PriceCharged, 0, Buckets3} ->
					Buckets4 = [#bucket{remain_amount = PriceCharged - PriceCharge,
							units = cents} | Buckets3],
					rate6(Subscriber#subscriber{buckets = Buckets4}, interim,
							DebitAmount, UnitsCharged + (PriceCharged div UnitPrice),
							ReserveAmount, 0, SessionId, Rated)
			end
	end;
rate5(#subscriber{buckets = Buckets1} = Subscriber,
		#price{units = Units, size = UnitSize, amount = UnitPrice,
		type = PriceType, currency = Currency},
		_Validity, final, DebitAmount, 0, SessionAttributes, Rated1) ->
	Rated2 = Rated1#rated{bucket_type = Units,
			price_type = PriceType, currency = Currency},
	SessionId = get_session_id(SessionAttributes),
	case charge_session(Units, DebitAmount, SessionId, Buckets1) of
		{DebitAmount, Buckets2} ->
			Rated3 = Rated2#rated{usage_rating_tag = included},
			rate6(Subscriber#subscriber{buckets = Buckets2}, final,
					DebitAmount, DebitAmount, 0, 0, SessionId, Rated3);
		{UnitsCharged, Buckets2} ->
			PriceChargeUnits = DebitAmount - UnitsCharged,
			{UnitCharge, PriceCharge} = price_units(PriceChargeUnits,
					UnitSize, UnitPrice),
			Rated3 = Rated2#rated{usage_rating_tag = non_included},
			case charge_session(cents, PriceCharge, SessionId, Buckets2) of
				{PriceCharge, Buckets3} ->
					TotalUnits = UnitsCharged + UnitCharge,
					rate6(Subscriber#subscriber{buckets = Buckets3}, final,
							DebitAmount, TotalUnits, 0, 0, SessionId, Rated3);
				{PriceCharged, Buckets3} ->
					TotalUnits = UnitsCharged + (PriceCharged div UnitPrice),
					Buckets4 = [#bucket{remain_amount = PriceCharged - PriceCharge,
							units = cents} | Buckets3],
					rate6(Subscriber#subscriber{buckets = Buckets4}, final,
					DebitAmount, TotalUnits, 0, 0, SessionId, Rated3)
			end
	end.
%% @hidden
rate6(#subscriber{session_attributes = SessionList,
		buckets = Buckets} = Subscriber1,
		final, Charge, Charged, 0, 0, SessionId, Rated)
		when Charged >= Charge ->
	NewBuckets1 = refund(SessionId, Buckets),
	{Debit, NewBuckets2} = get_debits(SessionId, NewBuckets1),
	Rated1 = Rated#rated{bucket_value = Debit, is_billed = true},
	NewSessionList = remove_session(SessionId, SessionList),
	Subscriber2 = Subscriber1#subscriber{buckets = NewBuckets2,
			session_attributes = NewSessionList},
	ok = mnesia:write(Subscriber2),
	{final, Subscriber2, Rated1};
rate6(#subscriber{session_attributes = SessionList,
		buckets = Buckets} = Subscriber1,
		final, _Charge, _Charged, 0, 0, SessionId, Rated) ->
	NewBuckets1 = refund(SessionId, Buckets),
	{Debit, NewBuckets2} = get_debits(SessionId, NewBuckets1),
	Rated1 = Rated#rated{bucket_value = Debit, is_billed = true},
	Subscriber2 = Subscriber1#subscriber{buckets = NewBuckets2,
			session_attributes = []},
	ok = mnesia:write(Subscriber2),
	{out_of_credit, SessionList, Rated1};
rate6(#subscriber{enabled = false, buckets = Buckets,
		session_attributes = SessionList} = Subscriber1, _Flag,
		_Charge, _Charged, _Reserve, _Reserved, SessionId, _Rated) ->
	NewBuckets = refund(SessionId, Buckets),
	Subscriber2 = Subscriber1#subscriber{buckets = NewBuckets,
			session_attributes = []},
	ok = mnesia:write(Subscriber2),
	{disabled, SessionList};
rate6(#subscriber{session_attributes = SessionList,
		buckets = Buckets} = Subscriber1, _Flag,
		Charge, Charged, Reserve, Reserved, SessionId, _Rated)
		when Charged < Charge; Reserved <  Reserve ->
	NewBuckets = refund(SessionId, Buckets),
	Subscriber2 = Subscriber1#subscriber{buckets = NewBuckets,
			session_attributes = []},
	ok = mnesia:write(Subscriber2),
	{out_of_credit, SessionList};
rate6(#subscriber{session_attributes = SessionList} = Subscriber1,
		initial, 0, 0, _Reserve, Reserved, SessionId, _Rated) ->
	NewSessionList = add_session(SessionId, SessionList),
	Subscriber2 = Subscriber1#subscriber{session_attributes = NewSessionList},
	ok = mnesia:write(Subscriber2),
	{grant, Subscriber2, Reserved};
rate6(Subscriber, interim, _Charge, _Charged, _Reserve, Reserved,
		_SessionId, _Rated) ->
	ok = mnesia:write(Subscriber),
	{grant, Subscriber, Reserved}.

-spec authorize(Protocol, ServiceType, SubscriberId, Password,
		Timestamp, Address, Direction, SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		ServiceType :: binary() | char() | undefined,
		SubscriberId :: binary() | string(),
		Password :: binary(),
		Timestamp :: calendar:datetime(),
		Address :: string() | undefined,
		Direction :: answer | originate | undefined,
		SessionAttributes :: [tuple()],
		Result :: {authorized, Subscriber, Attributes, SessionList}
					| {unauthorized, Reason, SessionList},
		Subscriber :: #subscriber{},
		Attributes :: [tuple()],
		SessionList :: [tuple()],
		Reason :: disabled | bad_password | subscriber_not_found
				| out_of_credit | product_not_found | invalid_bundle_product
				| price_not_found | table_lookup_failed.
%% @doc Authorize access request.
%% 	If authorized returns attributes to be included in `Access-Accept' response.
%%
%% 	When subscriber's product instance includes the `radiusReserveSessionTime'
%% 	characteristic a reservation is attempted for the given value of seconds.
%% 	A `Session-Timeout' attribute will be included with the actual reservation.
%%
authorize(Protocol, ServiceType, SubscriberId, Password, Timestamp,
		Address, Direction, SessionAttributes) when is_list(SubscriberId) ->
	authorize(Protocol, ServiceType, list_to_binary(SubscriberId),
			Password, Timestamp, Address, Direction, SessionAttributes);
authorize(Protocol, ServiceType, SubscriberId, Password, Timestamp,
			Address, Direction, SessionAttributes) when is_list(Password) ->
	authorize(Protocol, ServiceType, SubscriberId, list_to_binary(Password),
			Timestamp, Address, Direction, SessionAttributes);
authorize(Protocol, ServiceType, SubscriberId, Password, Timestamp,
		Address, Direction, SessionAttributes)
		when ((Protocol == radius) or (Protocol == diameter)), is_binary(SubscriberId),
		length(SessionAttributes) > 0 ->
	F = fun() ->
			case mnesia:read(subscriber, SubscriberId) of
				[#subscriber{enabled = false,
						session_attributes = ExistingAttr} = S] ->
					ok = mnesia:write(S#subscriber{session_attributes = []}),
					{unauthorized, disabled, ExistingAttr};
				[#subscriber{password = MTPassword} = S] when
						((Password == <<>>) and (Password =/= MTPassword)) orelse
						(Password == MTPassword) ->
					authorize1(Protocol, ServiceType, S, Timestamp,
							Address, Direction, SessionAttributes);
				[#subscriber{}] ->
					throw(bad_password);
				[] ->
					throw(subscriber_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {authorized, Sub, Attr, SSA}} ->
			{authorized, Sub, Attr, SSA};
		{atomic, {unauthorized, Reason, SSA}} ->
			{unauthorized, Reason, SSA};
		{aborted, {throw, Reason}} ->
			{unauthorized, Reason, []};
		{aborted, Reason} ->
			{unauthorized, Reason, []}
	end.
%% @hidden
authorize1(radius, ServiceType,
		#subscriber{attributes = Attributes,
		product = #product_instance{product = ProdId,
		characteristics = Chars}} = Subscriber, Timestamp,
		Address, Direction, SessionAttributes) ->
	F = fun({'Session-Id', _}) ->
			true;
		({?AcctSessionId, _}) ->
			true;
		(_) ->
			false
	end,
	case lists:any(F, get_session_id(SessionAttributes)) of
		true ->
			case lists:keyfind("radiusReserveSessionTime", 1, Chars) of
				{_, RRST} when is_integer(RRST) ->
					case mnesia:read(product, ProdId, read) of
						[#product{} = P] ->
							authorize2(radius, ServiceType, Subscriber, P,
									Timestamp, Address, Direction, SessionAttributes, RRST);
						[] ->
							throw(product_not_found)
					end;
				false ->
					authorize5(Subscriber, ServiceType, SessionAttributes, Attributes)
			end;
		false ->
			authorize5(Subscriber, ServiceType, SessionAttributes, Attributes)
	end;
authorize1(diameter, ServiceType,
		#subscriber{attributes = Attributes} = Subscriber,
		_Timestamp, _Address, _Direction, SessionAttributes) ->
	authorize5(Subscriber, ServiceType, SessionAttributes, Attributes).
%% @hidden
authorize2(radius = Protocol, ServiceType,
		#subscriber{attributes = Attributes} = Subscriber, Timestamp,
		#product{specification = undefined, bundle = Bundle},
		Address, Direction, SessionAttributes, Reserve)
		when Reserve > 0, Bundle /= [] ->
	try
		F = fun(#bundled_po{name = ProdId}, Acc) ->
				case mnesia:read(product, ProdId, read) of
					[#product{specification = Spec, status = Status} = P] when
							((Status == active) orelse (Status == undefined))
							and
							(((Protocol == radius)
								and
								(((ServiceType == ?RADIUSVOICE) and
								((Spec == "5") orelse (Spec == "9"))) orelse
								(((ServiceType == ?RADIUSFRAMED) orelse (ServiceType == ?RADIUSLOGIN)) and
								((Spec == "4") orelse (Spec == "8")))))) ->
						[P | Acc];
					_ ->
						Acc
				end
		end,
		case lists:foldl(F, [], Bundle) of
			[#product{} = Product | _] ->
				authorize2(Protocol, ServiceType, Subscriber, Product,
						Timestamp, Address, Direction, SessionAttributes, Reserve);
			[] ->
				authorize5(Subscriber, ServiceType, SessionAttributes, Attributes)
		end
	catch
		_:_ ->
			throw(invalid_bundle_product)
	end;
authorize2(radius = Protocol, ServiceType,
		#subscriber{attributes = Attributes} = Subscriber,
		#product{specification = ProdSpec, price = Prices},
		Timestamp, Address, Direction, SessionAttributes,
		Reserve) when (Reserve > 0)
		and ((ProdSpec == "9") orelse (ProdSpec == "5")) ->
	F = fun(#price{type = tariff, units = seconds}) ->
				true;
			(#price{type = usage, units = seconds}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_tod(Timestamp, FilteredPrices1),
	case filter_prices_dir(Direction, FilteredPrices2) of
		[Price | _] ->
			authorize3(Protocol, ServiceType, Subscriber, Address,
					Price, SessionAttributes, Reserve);
		_ ->
			authorize5(Subscriber, ServiceType, SessionAttributes, Attributes)
	end;
authorize2(radius = Protocol, ServiceType,
		#subscriber{attributes = Attributes} = Subscriber,
		#product{specification = ProdSpec, price = Prices},
		Timestamp, _Address, _Direction, SessionAttributes,
		Reserve) when (Reserve > 0)
		and ((ProdSpec == "8") orelse (ProdSpec == "4")) ->
	F = fun(#price{type = usage, units = seconds}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	case filter_prices_tod(Timestamp, FilteredPrices1) of
		[Price | _] ->
			authorize4(Protocol, ServiceType, Subscriber,
					Price, SessionAttributes, Reserve);
		_ ->
			authorize5(Subscriber, ServiceType, SessionAttributes, Attributes)
	end;
authorize2(_Protocol, ServiceType,
		#subscriber{attributes = Attributes} = Subscriber, _Product,
		_Timestamp, _Address, _Direction, SessionAttributes, _Reserve) ->
	authorize5(Subscriber, ServiceType, SessionAttributes, Attributes).
%% @hidden
authorize3(Protocol, ServiceType, Subscriber, Address,
		#price{type = tariff, char_value_use = CharValueUse} = Price,
		SessionAttributes, Reserve) ->
	case lists:keyfind("destPrefixTariffTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = TariffTable}]} ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{_Description, Amount} ->
					case Amount of
						N when N >= 0 ->
							authorize4(Protocol, ServiceType, Subscriber,
								Price#price{amount = N}, SessionAttributes, Reserve);
						_N ->
							throw(negative_amount)
					end;
				Other ->
					error_logger:error_report(["Prefix table tariff lookup failed",
							{module, ?MODULE}, {table, Table},
							{address, Address}, {result, Other}]),
					throw(table_lookup_failed)
			end;
		false ->
			throw(undefined_tariff)
	end;
authorize3(Protocol, ServiceType, Subscriber, _Address,
		Price, SessionAttributes, Reserve) ->
	authorize4(Protocol, ServiceType, Subscriber,
			Price, SessionAttributes, Reserve).
%% @hidden
authorize4(_Protocol, ServiceType, #subscriber{buckets = Buckets1,
		session_attributes = ExistingAttr, attributes = Attr} =
		Subscriber, #price{units = Units, size = UnitSize, amount =
		UnitPrice}, SessionAttributes, Reserve) ->
	SessionId = get_session_id(SessionAttributes),
	case reserve_session(Units, Reserve, SessionId, Buckets1) of
		{Reserve, _Buckets2} ->
			NewAttr = radius_attributes:store(?SessionTimeout, Reserve, Attr),
			authorize5(Subscriber, ServiceType, SessionAttributes, NewAttr);
		{UnitsReserved, Buckets2} ->
			PriceReserveUnits = (Reserve- UnitsReserved),
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case reserve_session(cents, PriceReserve, SessionId, Buckets2) of
				{PriceReserve, _Buckets3}  ->
					SessionTimeout = UnitsReserved + UnitReserve,
					NewAttr = radius_attributes:store(?SessionTimeout, SessionTimeout, Attr),
					authorize5(Subscriber, ServiceType, SessionAttributes, NewAttr);
				{0, _Buckets3}  when UnitsReserved == 0 ->
					{unauthorized, out_of_credit, ExistingAttr};
				{PriceReserved, _Buckets3} ->
					SessionTimeout = UnitsReserved + ((PriceReserved div UnitPrice) * UnitSize),
					NewAttr = radius_attributes:store(?SessionTimeout, SessionTimeout, Attr),
					authorize5(Subscriber, ServiceType, SessionAttributes, NewAttr)
			end
	end.
%% @hidden
authorize5(#subscriber{buckets = Buckets, session_attributes = ExistingAttr} = Subscriber,
		ServiceType, SessionAttributes, Attributes) ->
	F = fun(#bucket{remain_amount = R, units = U})
				when
				((ServiceType == undefined) orelse
				(((ServiceType == ?RADIUSFRAMED) orelse (ServiceType == ?RADIUSLOGIN) orelse (ServiceType == ?DIAMETERDATA)) and
				((U == octets) orelse (U == cents) orelse (U == seconds))) orelse
				(((ServiceType == ?RADIUSVOICE) orelse (ServiceType == ?DIAMETERVOICE)) and
				((U == seconds) orelse (U == cents)))) and (R > 0) ->
			true;
		(_) ->
			false
	end,
	case lists:any(F, Buckets) of
		true ->
			authorize6(Subscriber, SessionAttributes, Attributes);
		false ->
			{unauthorized, out_of_credit, ExistingAttr}
	end.
%% @hidden
authorize6(#subscriber{multisession = false, session_attributes = []}
		= Subscriber, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(?MILLISECOND),
			get_session_id(SessionAttributes)},
	Subscriber1 = Subscriber#subscriber{session_attributes =
		[NewSessionAttributes], disconnect = false},
	ok = mnesia:write(Subscriber1),
	{authorized, Subscriber1, Attributes, []};
authorize6(#subscriber{multisession = false, session_attributes
		= ExistingAttr} = Subscriber, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(?MILLISECOND),
			get_session_id(SessionAttributes)},
	Subscriber1 = Subscriber#subscriber{session_attributes =
		[NewSessionAttributes], disconnect = false},
	ok = mnesia:write(Subscriber1),
	{authorized, Subscriber1, Attributes, ExistingAttr};
authorize6(#subscriber{multisession = true, session_attributes
		= ExistingAttr} = Subscriber, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(?MILLISECOND),
			get_session_id(SessionAttributes)},
	Subscriber1 = Subscriber#subscriber{session_attributes =
		[NewSessionAttributes | ExistingAttr], disconnect = false},
	ok = mnesia:write(Subscriber1),
	{authorized, Subscriber1, Attributes, ExistingAttr}.

-spec session_attributes(Attributes) -> SessionAttributes
	when
		Attributes :: radius_attributes:attributes(),
		SessionAttributes :: radius_attributes:attributes().
%% @doc Extract RADIUS session related attributes.
session_attributes(Attributes) ->
	F = fun({?NasIdentifier, _}) ->
				true;
			({?NasIpAddress, _}) ->
				true;
			({?AcctSessionId, _}) ->
				true;
			({?AcctMultiSessionId, _}) ->
				true;
			({?UserName, _}) ->
				true;
			({?FramedIpAddress, _}) ->
				true;
			({?NasPort, _}) ->
				true;
			({?NasPortType, _}) ->
				true;
			({?CalledStationId, _}) ->
				true;
			({?CallingStationId, _}) ->
				true;
			({?NasPortId, _}) ->
				true;
			({?OriginatingLineInfo, _}) ->
				true;
			({?FramedInterfaceId, _}) ->
				true;
			({?FramedIPv6Prefix, _}) ->
				true;
			(_) ->
				false
	end,
	lists:keysort(1, lists:filter(F, Attributes)).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec reserve_session(Type, Amount, SessionId, Buckets) -> Result
	when
		Type :: octets | seconds | cents,
		Amount :: non_neg_integer(),
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {Reserved, NewBuckets},
		Reserved :: non_neg_integer(),
		NewBuckets :: [#bucket{}].
%% @doc Perform reservation for a session.
%%
%% 	Creates a reservation within bucket(s) of `Type' and
%% 	decrements remaining balance by the same amount(s).
%%
%% 	Expired buckets are removed when no session
%% 	reservations remain.
%%
%% 	Returns `{Reserved, NewBuckets}' where
%% 	`Reserved' is the total amount of reservation(s) made
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
reserve_session(Type, Amount, SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	reserve_session(Type, Amount, Now, SessionId, sort(Buckets), [], 0).
%% @hidden
reserve_session(Type, Amount, Now, SessionId,
		[#bucket{termination_date = Expires, reservations = [],
		remain_amount = Remain} | T], Acc, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0 ->
	reserve_session(Type, Amount, Now, SessionId, T, Acc, Reserved);
reserve_session(Type, Amount, Now, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		reservations = Reservations, termination_date = Expires} = B | T],
		Acc, Reserved) when Remain >= Amount,
		((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, 0, Amount, SessionId},
	NewBuckets = lists:reverse(Acc)
			++ [B#bucket{remain_amount = Remain - Amount,
			reservations = [NewReservation | Reservations]} | T],
	{Reserved + Amount, NewBuckets};
reserve_session(Type, Amount, Now, SessionId,
		[#bucket{units = Type, remain_amount = 0} = B | T], Acc, Reserved) ->
	reserve_session(Type, Amount, Now, SessionId, T, [B | Acc], Reserved);
reserve_session(Type, Amount, Now, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		reservations = Reservations, termination_date = Expires} = B | T],
		Acc, Reserved) when Remain > 0, Remain < Amount,
		((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, 0, Remain, SessionId},
	NewAcc = [B#bucket{remain_amount = 0,
			reservations = [NewReservation | Reservations]} | Acc],
	reserve_session(Type, Amount - Remain, Now,
			SessionId, T, NewAcc, Reserved + Remain);
reserve_session(Type, Amount, Now, SessionId, [H | T], Acc, Reserved) ->
	reserve_session(Type, Amount, Now, SessionId, T, [H | Acc], Reserved);
reserve_session(_, _, _, _, [], Acc, Reserved) ->
	{Reserved, lists:reverse(Acc)}.

-spec update_session(Type, Charge, Reserve, SessionId, Buckets) -> Result
	when
		Type :: octets | seconds | cents,
		Charge :: non_neg_integer(),
		Reserve :: non_neg_integer(),
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {Charged, Reserved, NewBuckets},
		Charged :: non_neg_integer(),
		Reserved :: non_neg_integer(),
		NewBuckets :: [#bucket{}].
%% @doc Perform debit and reservation for a session.
%%
%% 	Finds reservations matching `SessionId'.
%%
%% 	Empty or expired buckets are removed when no session
%% 	reservations remain.
%%
%% 	Returns `{Charged, Reserved, NewBuckets}' where
%% 	`Charged' is the total amount debited from bucket(s),
%% 	`Reserved' is the total amount of reservation(s) made,
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
update_session(Type, Charge, Reserve, SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	update_session(Type, Charge, Reserve, Now, SessionId,
			sort(Buckets), [], 0, 0).
%% @hidden
update_session(Type, Charge, Reserve, Now, SessionId,
		[#bucket{termination_date = Expires, reservations = [],
		remain_amount = Remain} | T], Acc, Charged, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0 ->
	update_session(Type, Charge, Reserve,
			Now, SessionId, T, Acc, Charged, Reserved);
update_session(Type, Charge, Reserve, Now, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		termination_date = Expires, reservations = Reservations} = B | T],
		Acc, Charged, Reserved) ->
	case lists:keytake(SessionId, 4, Reservations) of
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when Remain > 0, Remain >= (Reserve - (ReservedAmount - Charge)),
				((Expires == undefined) or (Now < Expires)) ->
			NewReservation = {Now, DebitedAmount + Charge, Reserve, SessionId},
			NewBuckets = lists:reverse(Acc)
					++ [B#bucket{remain_amount = Remain
					- (Reserve - (ReservedAmount - Charge)),
					reservations = [NewReservation | NewReservations]} | T],
			{Charged + Charge, Reserved + Reserve, NewBuckets};
		{value, {_, DebitedAmount, ReservedAmount, _}, []}
				when Remain >= 0, (Remain + ReservedAmount) =< Charge,
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + Remain + ReservedAmount,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = 0,
					reservations = [NewReservation]} | Acc],
			update_session(Type, Charge - (Remain + ReservedAmount), Reserve,
					Now, SessionId, T, NewAcc, Charged + Remain + ReservedAmount, Reserved);
		{value, {_, DebitedAmount, ReservedAmount, _}, []}
				when ReservedAmount >= Charge, Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + ReservedAmount,
			NewReservedAmount = ReservedAmount - Charge,
			NewReservation = {Now, NewDebitedAmount, NewReservedAmount, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation]} | Acc],
			update_session(Type, 0, Reserve, Now,
					SessionId, T, NewAcc, Charge, Reserved);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount >= Charge, Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + ReservedAmount,
			NewReservedAmount = ReservedAmount - Charge,
			NewReservation = {Now, NewDebitedAmount, NewReservedAmount, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, 0, Reserve, Now, SessionId, T, NewAcc, Charge, Reserved);
		{value, {_, DebitedAmount, ReservedAmount, _}, []}
				when ReservedAmount < Charge, Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + ReservedAmount,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation]} | Acc],
			update_session(Type, Charge - ReservedAmount, Reserve, Now,
					SessionId, T, NewAcc, Charge, Reserved);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount < Charge, Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + ReservedAmount,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, Charge - ReservedAmount, Reserve, Now,
					SessionId, T, NewAcc, Charge, Reserved);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when Remain > 0, (Remain + ReservedAmount) =< Charge,
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + (Remain + ReservedAmount),
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = 0,
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, Charge - (Remain + ReservedAmount), Reserve,
					Now, SessionId, T, NewAcc, Charged + Remain + ReservedAmount, Reserved);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when Remain >= (Charge - ReservedAmount),
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReserve = Remain - (Charge - ReservedAmount),
			NewReservation = {Now, NewDebitedAmount, NewReserve, SessionId},
			NewAcc = [B#bucket{remain_amount = 0,
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, 0, Reserve - NewReserve, Now, SessionId,
					T, NewAcc, Charged + Charge, Reserved + NewReserve);
		_ when Reservations == [], Expires /= undefined, Expires =< Now ->
			update_session(Type, Charge, Reserve, Now, SessionId,
					T, Acc, Charged, Reserved);
		false ->
			update_session(Type, Charge, Reserve, Now, SessionId,
					T, [B | Acc], Charged, Reserved)
	end;
update_session(Type, Charge, Reserve, Now, SessionId,
		[H | T], Acc, Charged, Reserved) ->
	update_session(Type, Charge, Reserve, Now, SessionId,
			T, [H | Acc], Charged, Reserved);
update_session(_, 0, Reserved, _, _, [], Acc, Charged, Reserved) ->
	{Charged, Reserved, lists:reverse(Acc)};
update_session(Type, Charge, Reserve, Now, SessionId, [], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, SessionId, lists:reverse(Acc), [], Charged, Reserved).

%% @hidden
update(Type, Charge, Reserve, Now, SessionId,
		[#bucket{termination_date = Expires, reservations = [],
		remain_amount = Remain} | T], Acc, Charged, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0 ->
	update(Type, Charge, Reserve, Now, SessionId, T, Acc, Charged, Reserved);
update(Type, Charge, Reserve, Now, SessionId, [#bucket{units = Type,
		remain_amount = Remain, termination_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)), Remain > (Charge + Reserve) ->
	NewReservation = {Now, Charge, Reserve, SessionId},
	NewBuckets = [B#bucket{remain_amount = Remain - (Charge + Reserve),
		reservations = [NewReservation | Reservations]} | Acc],
	{Charged + Charge, Reserved + Reserve, lists:reverse(NewBuckets) ++ T};
update(Type, Charge, Reserve, Now, SessionId, [#bucket{units = Type,
		remain_amount = Remain, termination_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain =< Charge ->
	NewReservation = {Now, Remain, 0, SessionId},
	NewAcc = [B#bucket{remain_amount = 0,
			reservations = [NewReservation | Reservations]} | Acc],
	update(Type, Charge - Remain, Reserve, Now,
			SessionId, T, NewAcc, Charged + Remain, Reserved);
update(Type, Charge, Reserve, Now, SessionId, [#bucket{units = Type,
		remain_amount = Remain, termination_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain =< Reserve, Remain > 0 ->
	NewReservation = {Now, 0, Remain, SessionId},
	NewAcc = [B#bucket{remain_amount = 0,
		reservations = [NewReservation | Reservations]} | Acc],
	update(Type, Charge, Reserve - Remain, Now,
			SessionId, T, NewAcc, Charged, Reserved + Remain);
update(_Type, 0, 0, _Now, _SessionId, Buckets, Acc, Charged, Reserved) ->
	{Charged, Reserved, lists:reverse(Acc) ++ Buckets};
update(Type, Charge, Reserve, Now, SessionId, [H | T], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, SessionId, T, [H | Acc], Charged, Reserved);
update(_, _, _,  _, _, [], Acc, Charged, Reserved) ->
	{Charged, Reserved, lists:reverse(Acc)}.

-spec charge_session(Type, Charge, SessionId, Buckets) -> Result
	when
		Type :: octets | seconds | cents,
		Charge :: non_neg_integer(),
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {Charged, NewBuckets},
		Charged :: non_neg_integer(),
		NewBuckets :: [#bucket{}].
%% @doc Peform final charging for a session.
%%
%% 	Finds and removes all reservations matching `SessionId'.
%% 	If the total reservation amounts are less than `Charge'
%% 	the deficit is debited. Any surplus is refunded within
%% 	the bucket containing the reservation.
%%
%% 	Empty buckets are removed. Expired buckets are removed
%% 	when no session reservations remain.
%%
%% 	Returns `{Charged, NewBuckets}' where
%% 	`Charged' is the total amount debited from the buckets
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
charge_session(Type, Charge, SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	charge_session(Type, Charge, Now, SessionId, sort(Buckets), 0, []).
%% @hidden
charge_session(Type, Charge, Now, SessionId,
		[#bucket{units = Type, termination_date = Expires,
		remain_amount = Remain, reservations = Reservations} = B | T],
		Charged, Acc) ->
	case lists:keytake(SessionId, 4, Reservations) of
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount >= Charge,
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = Remain + (ReservedAmount - Charge),
					reservations = [NewReservation | NewReservations]} | Acc],
			charge_session(Type, 0, Now, SessionId, T, Charged + Charge, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, []} 
				when ReservedAmount >= Charge,
				Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = Remain + (ReservedAmount - Charge),
					reservations = [NewReservation]} | Acc],
			charge_session(Type, 0, Now, SessionId, T, Charged + Charge, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount >= Charge,
				Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{reservations = 
					[NewReservation | NewReservations]} | Acc],
			charge_session(Type, 0, Now, SessionId, T, Charged + Charge, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount < Charge, Remain > (Charge - ReservedAmount),
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = Remain - (Charge - ReservedAmount),
					reservations = [NewReservation | NewReservations]} | Acc],
			charge_session(Type, 0, Now, SessionId, T, Charged + Charge, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, []}
				when ReservedAmount < Charge, Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + ReservedAmount,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation]} | Acc],
			charge_session(Type, Charge - ReservedAmount, Now,
					SessionId, T, Charged + ReservedAmount, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount < Charge, Expires /= undefined, Expires =< Now ->
			NewDebitedAmount = DebitedAmount + ReservedAmount,
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation| NewReservations]} | Acc],
			charge_session(Type, Charged + Charge, Now,
					SessionId, T, Charge - ReservedAmount, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, []}
				when ReservedAmount < Charge, Remain >= 0,
				Remain =< (Charge - ReservedAmount),
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + (Remain + ReservedAmount),
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = 0, reservations = [NewReservation]} | Acc], 
			charge_session(Type, Charge - ReservedAmount - Remain, Now,
					SessionId, T, Charged + ReservedAmount + Remain, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations}
				when ReservedAmount < Charge, Remain >= 0,
				Remain =< (Charge - ReservedAmount),
				((Expires == undefined) or (Now < Expires)) ->
			NewDebitedAmount = DebitedAmount + (Remain + ReservedAmount),
			NewReservation = {Now, NewDebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation | NewReservations]} | Acc],
			charge_session(Type, Charge - ReservedAmount - Remain, Now,
					SessionId, T, Charged + ReservedAmount + Remain, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount, _}, NewReservations} when Charge =:= 0,
				((Expires == undefined) or (Now < Expires)) ->
			NewReservation = {Now, DebitedAmount, 0, SessionId},
			NewAcc = [B#bucket{remain_amount = Remain + ReservedAmount,
					reservations = [NewReservation | NewReservations]} | Acc],
			charge_session(Type, 0, Now, SessionId, T, Charged, NewAcc);
		_ when Reservations == [], Expires /= undefined, Expires =< Now ->
			charge_session(Type, Charge, Now, SessionId, T, Charged, Acc);
		false ->
			charge_session(Type, Charge, Now, SessionId, T, Charged, [B | Acc])
	end;
charge_session(Type, Charge, Now, SessionId, [H | T], Charged, Acc) ->
	charge_session(Type, Charge, Now, SessionId, T, Charged, [H | Acc]);
charge_session(_, Charge, _, _, [], Charge, Acc) ->
	{Charge, lists:reverse(Acc)};
charge_session(Type, Charge, Now, SessionId, [], Charged, Acc) ->
	charge(Type, Charge, Now, SessionId, lists:reverse(Acc), [], Charged).

%% @hidden
charge(Type, Charge, Now, SessionId,
		[#bucket{termination_date = Expires, reservations = [],
		remain_amount = Remain} | T], Acc, Charged)
		when Expires /= undefined, Expires =< Now, Remain >= 0 ->
	charge(Type, Charge, Now, SessionId, T, Acc, Charged);
charge(Type, Charge, Now, SessionId, [#bucket{units = Type,
		remain_amount = R, termination_date = Expires,
		reservations = Reservations} = B | T],
		Acc, Charged) when R > Charge,
		((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, Charge, 0, SessionId},
	NewBuckets = [B#bucket{remain_amount = R - Charge,
			reservations = [NewReservation | Reservations]} | T],
	{Charged + Charge, lists:reverse(Acc) ++ NewBuckets};
charge(Type, Charge, Now, SessionId, [#bucket{units = Type,
		remain_amount = R, reservations = [],
		termination_date = Expires} = B | T], Acc, Charged)
		when R =< Charge, 0 =< R, ((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, R, 0, SessionId},
	NewAcc = [B#bucket{remain_amount = R - Charge,
			reservations = [NewReservation]} | Acc],
	charge(Type, Charge - R, Now, SessionId, T, NewAcc, Charged + R);
charge(Type, Charge, Now, SessionId, [#bucket{units = Type,
		remain_amount = R, termination_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged)
		when R =< Charge, 0 =< R, ((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, R, 0, SessionId},
	NewAcc = [B#bucket{remain_amount = 0, 
			reservations = [NewReservation | Reservations]} | Acc],
	charge(Type, Charge - R, Now, SessionId, T, NewAcc, Charged + R);
charge(_Type, 0, _Now, _SessionId, Buckets, Acc, Charged) ->
	{Charged, lists:reverse(Acc) ++ Buckets};
charge(Type, Charge, Now, SessionId, [H | T], Acc, Charged) ->
	charge(Type, Charge, Now, SessionId, T, [H | Acc], Charged);
charge(_, _, _, _, [], Acc, Charged) ->
	{Charged, lists:reverse(Acc)}.

-spec remove_session(SessionId, SessionList) -> NewSessionList
	when
		SessionId :: [tuple()],
		SessionList :: [{pos_integer(), [tuple()]}],
		NewSessionList :: [{pos_integer(), [tuple()]}].
%% @doc Remove session identification attributes set from active sessions list.
%% @private
remove_session(SessionId, SessionList) ->
	lists:keydelete(SessionId, 2, SessionList).

-spec add_session(SessionId, SessionList) -> SessionList
	when
		SessionId:: [tuple()],
		SessionList :: [{pos_integer(), [tuple()]}].
%% @doc Add new session identification attributes set to active sessions list.
%% @private
add_session(SessionId, SessionList) ->
	case lists:keymember(SessionId, 2, SessionList) of
		true ->
			SessionList;
		false ->
			[{erlang:system_time(?MILLISECOND), SessionId} | SessionList]
	end.

-spec get_session_id(SessionAttributes) -> SessionId
	when
		SessionAttributes :: [tuple()],
		SessionId :: [tuple()].
%% @doc Get the session identifier value.
%% 	Returns a list of DIAMETER/RADIUS attributes.
%% @private
get_session_id(SessionAttributes) ->
	case lists:keyfind('Session-Id', 1, SessionAttributes) of
		false ->
			get_session_id1(SessionAttributes, []);
		SessionId ->
			[SessionId]
	end.
%% @hidden
get_session_id1([], Acc) ->
	lists:keysort(1, Acc);
get_session_id1([{?AcctSessionId, _} = AcctSessionId | T], Acc) ->
	get_session_id1(T, [AcctSessionId | Acc]);
get_session_id1([{?NasIdentifier, _} = NasIdentifier | T], Acc) ->
	get_session_id1(T, [NasIdentifier | Acc]);
get_session_id1([{?NasIpAddress, _} = NasIpAddress | T], Acc) ->
	get_session_id1(T, [NasIpAddress | Acc]);
get_session_id1([{?UserName, _} = UserName | T], Acc) ->
	get_session_id1(T, [UserName | Acc]);
get_session_id1([{?NasPort, _} = NasPort | T], Acc) ->
	get_session_id1(T, [NasPort | Acc]);
get_session_id1([{?NasPortId, _} = NasPortId | T], Acc) ->
	get_session_id1(T, [NasPortId | Acc]);
get_session_id1([{?NasPortType, _} = NasPortType | T], Acc) ->
	get_session_id1(T, [NasPortType | Acc]);
get_session_id1([{?FramedIpAddress, _} = FramedIpAddress | T], Acc) ->
	get_session_id1(T, [FramedIpAddress | Acc]);
get_session_id1([{?CallingStationId, _} = CallingStationId | T], Acc) ->
	get_session_id1(T, [CallingStationId | Acc]);
get_session_id1([{?CalledStationId, _} = CalledStationId | T], Acc) ->
	get_session_id1(T, [CalledStationId | Acc]);
get_session_id1([_ | T], Acc) ->
	get_session_id1(T, Acc).

-spec sort(Buckets) -> Buckets
	when
		Buckets :: [#bucket{}].
%% @doc Sort `Buckets' oldest first.
%% @private
sort(Buckets) ->
	F = fun(#bucket{termination_date = T1},
				#bucket{termination_date = T2}) when T1 =< T2 ->
			true;
		(_, _)->
			false
	end,
	lists:sort(F, Buckets).

-spec price_units(Amount, UnitSize, UnitPrice) -> {TotalUnits, TotalPrice}
	when
		Amount :: non_neg_integer(),
		UnitSize :: pos_integer(),
		UnitPrice :: pos_integer(),
		TotalUnits :: pos_integer(),
		TotalPrice :: pos_integer().
%% @doc Calculate total size and price.
price_units(0, _UnitSize, _UnitPrice) ->
	{0, 0};
price_units(Amount, UnitSize, UnitPrice) when (Amount rem UnitSize) == 0 ->
	{Amount, UnitPrice * (Amount div UnitSize)};
price_units(Amount, UnitSize, UnitPrice) ->
	Units = (Amount div UnitSize + 1),
	{Units * UnitSize, UnitPrice * Units}.

-spec get_reserve(Price) -> ReserveAmount
	when
		Price :: #price{},
		ReserveAmount :: pos_integer().
%% @doc Get the reserve amount.
get_reserve(#price{units = seconds,
		char_value_use = CharValueUse} = _Price) ->
	case lists:keyfind("radiusReserveTime",
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = Value}]} ->
			Value;
		false ->
			0
	end;
get_reserve(#price{units = octets,
		char_value_use = CharValueUse} = _Price) ->
	case lists:keyfind("radiusReserveOctets",
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = Value}]} ->
			Value;
		false ->
			0
	end.

-spec refund(SessionId, Buckets) -> Buckets
	when
		Buckets :: [#bucket{}],
		SessionId :: string() | binary().
%% @doc Refund unused reservations.
%% @hidden
refund(SessionId, Buckets) ->
	refund(SessionId, Buckets, []).
%% @hidden
refund(SessionId, [#bucket{reservations = Reservations} = H | T], Acc) ->
	refund(SessionId, H, Reservations, T, [], Acc);
refund(_SessionId, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
refund(SessionId, #bucket{remain_amount = R} = Bucket1,
		[{_, 0, Amount, SessionId} | T1], T2, Acc1, Acc2) ->
	Bucket2 = Bucket1#bucket{remain_amount = R + Amount,
			reservations = lists:reverse(Acc1) ++ T1},
	refund(SessionId, T2, [Bucket2 | Acc2]);
refund(SessionId, #bucket{remain_amount = R} = Bucket1,
		[{TS, Debit, Amount, SessionId} | T1], T2, Acc1, Acc2) ->
	Refuned = {TS, Debit, 0, SessionId},
	NewReservation = [Refuned | T1],
	Bucket2 = Bucket1#bucket{remain_amount = R + Amount,
			reservations = lists:reverse(Acc1) ++ NewReservation},
	refund(SessionId, T2, [Bucket2 | Acc2]);
refund(SessionId, Bucket, [H | T1], T2, Acc1, Acc2) ->
	refund(SessionId, Bucket, T1, T2, [H | Acc1], Acc2);
refund(SessionId, Bucket1, [], T, Acc1, Acc2) ->
	Bucket2 = Bucket1#bucket{reservations = lists:reverse(Acc1)},
	refund(SessionId, T, [Bucket2 | Acc2]).

-spec due(Buckets) -> Buckets
	when
		Buckets :: [#bucket{}].
%% @doc Claim if any dues
%% @hidden
due(Buckets) ->
	F = fun(#bucket{remain_amount = R}) when R =< 0 ->
			true;
	(_) ->
			false
	end,
	{B1, B2} = lists:splitwith(F, Buckets),
	due1(B1, B2).
%% @hidden
due1([H | T], B2) ->
	due1(T, due2(H, B2));
due1([], B2) ->
	B2.
%% @hidden
due2(B1, B2) ->
	due2(B1, B2, []).
%% @hidden
due2(#bucket{units = Units, remain_amount = R1} = B1,
		[#bucket{units = Units, remain_amount = R2}
		| T], Acc) when R1 < 0, R1 + R2 < 0 ->
	due2(B1#bucket{remain_amount = R1 + R2}, T, Acc);
due2(#bucket{units = Units, remain_amount = R1},
		[#bucket{units = Units, remain_amount = R2} = B2
		| T], Acc) when R1 < 0, R1 + R2 >= 0 ->
	NewAcc = [B2#bucket{remain_amount = R1 + R2} | Acc],
	lists:reverse(NewAcc) ++ T;
due2(#bucket{} = B, [H | T], Acc) ->
	due2(B, T, [H | Acc]);
due2(#bucket{} = B, [], Acc) ->
	lists:reverse([B | Acc]).

-spec filter_prices_tod(Timestamp, Prices) -> Prices
	when
		Timestamp :: calendar:datetime(),
		Prices :: [#price{}].
%% @doc Filter prices with `timeOfDayRange'
%% @hidden
filter_prices_tod(Timestamp, Prices) ->
	filter_prices_tod(Timestamp, Prices, []).
%% @hidden
filter_prices_tod(Timestamp, [#price{char_value_use = CharValueUse} = P | T], Acc) ->
	case lists:keyfind("timeOfDayRange", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value =
				#range{lower = Start, upper = End}}]} ->
			case filter_prices_tod1(Timestamp, Start, End) of
				true ->
					filter_prices_tod(Timestamp, T, [P | Acc]);
				false ->
					filter_prices_tod(Timestamp, T, Acc)
			end;
		_ ->
			filter_prices_tod(Timestamp, T, [P | Acc])
	end;
filter_prices_tod(_, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
filter_prices_tod1({_, Time}, #quantity{units = U1, amount = A1},
		#quantity{units = U2, amount = A2}) ->
	filter_prices_tod2(to_seconds(U1, A1), to_seconds(U2, A2),
			calendar:time_to_seconds(Time)).
%% @hidden
filter_prices_tod2(Start, End, Time) when Start =< Time, End > Time ->
	true;
filter_prices_tod2(Start, End, Time) when End < Start,
		((Start =< Time) orelse (End > Time)) ->
	true;
filter_prices_tod2(_, _, _) ->
	false.

%% @hidden
to_seconds("seconds", Seconds) when Seconds >= 0, Seconds < 86400 ->
	Seconds;
to_seconds("minutes", Minutes) when Minutes >= 0, Minutes < 1440 ->
	Minutes * 60;
to_seconds("hours", Hours) when Hours >= 0, Hours < 24 ->
	Hours * 3600.

-spec filter_prices_dir(Direction, Prices) -> Prices
	when
		Direction :: answer | originate | undefined,
		Prices :: [#price{}].
%% @doc Filter prices with `callDirection'.
%% @hidden
filter_prices_dir(undefined, Prices) ->
	Prices;
filter_prices_dir(Direction, Prices) when is_atom(Direction) ->
	filter_prices_dir(Direction, Prices, []).
%% @hidden
filter_prices_dir(Direction, [#price{char_value_use = CharValueUse} = P | T], Acc) ->
	case lists:keyfind("callDirection", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = "answer"}]}
				when Direction == answer ->
			filter_prices_dir(Direction, T, [P | Acc]);
		#char_value_use{values = [#char_value{value = "originate"}]}
				when Direction == originate ->
			filter_prices_dir(Direction, T, [P | Acc]);
		#char_value_use{values = [#char_value{}]} ->
			filter_prices_dir(Direction, T, Acc);
		_ ->
			filter_prices_dir(Direction, T, [P | Acc])
	end;
filter_prices_dir(_, [], Acc) ->
	lists:reverse(Acc).

-spec get_debits(SessionId, Buckets) -> Result
	when
		SessionId :: [tuple()],
		Buckets :: [#bucket{}],
		Result :: {Debit, NewBuckets},
		Debit :: integer(),
		NewBuckets :: [#bucket{}].
%% @doc Get total debited amount and remove all reservations for session.
%% @private
%% 
get_debits(SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	get_debits(Buckets, SessionId, Now, 0, []).
%% @hidden
get_debits([#bucket{remain_amount = 0, reservations = []} | T],
		SessionId, Now, Debit, Acc) ->
	get_debits(T, SessionId, Now, Debit, Acc);
get_debits([#bucket{reservations = [], termination_date = Expires} | T],
		SessionId, Now, Debit, Acc) when Expires < Now ->
	get_debits(T, SessionId, Now, Debit, Acc);
get_debits([#bucket{reservations = []} = B | T], SessionId,
		Now, Debit, Acc) ->
	get_debits(T, SessionId, Now, Debit, [B | Acc]);
get_debits([#bucket{reservations = Reservations} = B | T],
		SessionId, Now, Debit, Acc) ->
	{Debited, NewReservations} =
			get_debits1(SessionId, Reservations, 0, []),
	get_debits(T, SessionId, Now, Debit + Debited,
			[B#bucket{reservations = NewReservations} | Acc]);
get_debits([], _SessionId, _Now, Debited, Acc) ->
	{Debited, lists:reverse(Acc)}.
%% @hidden
get_debits1(SessionId, [{_, Debited, _, SessionId} | T], Debit, Acc) ->
	{Debited + Debit, lists:reverse(Acc) ++ T};
get_debits1(SessionId, [H | T], Debit, Acc) ->
	get_debits1(SessionId, T, Debit, [H | Acc]);
get_debits1(_SessionId, [], Debit, Acc) ->
	{Debit, lists:reverse(Acc)}.

