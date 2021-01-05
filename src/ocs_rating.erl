%%% @end
%%% ocs_rating.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2020 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2020 SigScale Global Inc.').

-export([rate/13]).
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
-define(DIAMETERSMS, 32274).

-record(state,
		{buckets = [] :: [#bucket{}],
		product :: #product{},
		chars = [] :: [tuple()],
		service_type :: integer() | binary(),
		service_id:: integer() | undefined,
		charging_key :: integer() | undefined,
		service_network :: string() | undefined,
		roaming_tb_prefix :: string() | undefined,
		session_id :: [tuple()],
		rated = #rated{} :: #rated{} | [#rated{}]}).

-spec rate(Protocol, ServiceType, ServiceId, ChargingKey,
		ServiceNetwork, SubscriberID, Timestamp, Address, Direction,
		Flag, DebitAmounts, ReserveAmounts, SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		ServiceType :: integer() | binary(),
		ServiceId :: integer() | undefined,
		ChargingKey :: integer() | undefined,
		ServiceNetwork :: string() | binary() | undefined,
		SubscriberID :: string() | binary(),
		Timestamp :: calendar:datetime(),
		Address :: string() | binary() | undefined,
		Direction :: answer | originate | undefined,
		Flag :: initial | interim | final | event,
		DebitAmounts :: [{Type, Amount}],
		ReserveAmounts :: [{Type, Amount}] | undefined,
		SessionAttributes :: [tuple()],
		Type :: octets | seconds | messages,
		Amount :: integer(),
		Result :: {ok, Service, GrantedAmount}
				| {ok, Service, Rated}
				| {ok, Service, GrantedAmount, Rated}
				| {out_of_credit, SessionList}
				| {out_of_credit, SessionList, Rated}
				| {disabled, SessionList}
				| {error, Reason},
		Service :: #service{},
		GrantedAmount :: {Type, Amount},
		Rated :: [#rated{}],
		SessionList :: [{pos_integer(), [tuple()]}],
		Reason :: term().
%% @doc Handle rating and balance management for used and reserved unit amounts.
%%
%% 	Subscriber balance buckets are permanently reduced by the
%% 	amount in `DebitAmounts' and bucket reservations are made of
%% 	the amounts in `ReserveAmounts'. The subscribed product offer
%% 	determines the price used to calculate the amount to be
%% 	permanently debited from available `cents' buckets.
%%
%% 	If empty `ReserveAmounts' are provided in `initial', `interim'
%% 	and `event' requests the `Type' and `Amount' are determined by
%% 	applicable product offer price.
%%
%% 	If successful returns `{ok, Service, GrantedAmount}' for `initial'
%% 	and `interim' updates, `{ok, Service, Rated}' for `final' or
%% 	`{ok, Service, GrantedAmount, Rated}' for `event'.
%%
%% 	If subscriber's balance is insufficient to cover the `DebitAmounts'
%% 	and `ReserveAmounts' returns `{out_of_credit, SessionList}' for interim
%% 	updates and `{out_of_credit, SessionList, Rated}' for final or
%% 	`{disabled, SessionList}' if the subscriber is not enabled. In both
%% 	cases subscriber's balance is debited.  `SessionList' describes the
%% 	known active sessions which should be disconnected.
%%
rate(Protocol, ServiceType, ServiceId, ChargingKey,
		ServiceNetwork, SubscriberID, Timestamp, Address,
		Direction, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes) when is_list(SubscriberID) ->
	rate(Protocol, ServiceType, ServiceId, ChargingKey,
			ServiceNetwork, list_to_binary(SubscriberID),
			Timestamp, Address, Direction, Flag, DebitAmounts,
			ReserveAmounts, SessionAttributes);
rate(Protocol, ServiceType, ServiceId, ChargingKey,
		ServiceNetwork, SubscriberID, Timestamp, Address,
		Direction, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes) when is_binary(ServiceNetwork) ->
	rate(Protocol, ServiceType, ServiceId, ChargingKey,
			binary_to_list(ServiceNetwork), SubscriberID,
			Timestamp, Address, Direction, Flag, DebitAmounts,
			ReserveAmounts, SessionAttributes);
rate(Protocol, ServiceType, ServiceId, ChargingKey,
		ServiceNetwork, SubscriberID, Timestamp, Address,
		Direction, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes) when is_binary(Address) ->
	rate(Protocol, ServiceType, ServiceId, ChargingKey,
			ServiceNetwork, SubscriberID, Timestamp,
			binary_to_list(Address), Direction, Flag,
			DebitAmounts, ReserveAmounts, SessionAttributes);
rate(Protocol, ServiceType, ServiceId, ChargingKey,
		ServiceNetwork, SubscriberID,
		{{_, _, _}, {_, _, _}} = Timestamp, Address,
		Direction, Flag, DebitAmounts, ReserveAmounts,
		SessionAttributes)
		when ((Protocol == radius) or (Protocol == diameter)),
		(is_integer(ChargingKey) or (ChargingKey == undefined)),
		(is_list(ServiceNetwork) or (ServiceNetwork == undefined)),
		is_binary(SubscriberID),
		(is_list(Address) or (Address == undefined)),
		((Direction == answer) or (Direction == originate)
				or (Direction == undefined)),
		((Flag == initial) or (Flag == interim) or (Flag == final) or (Flag == event)),
		is_list(DebitAmounts),
		(is_list(ReserveAmounts) or (ReserveAmounts == undefined)),
		length(SessionAttributes) > 0 ->
	F = fun() ->
			case mnesia:read(service, SubscriberID, sticky_write) of
				[#service{product = ProdRef, session_attributes = SessionList} = Service] ->
					case mnesia:read(product, ProdRef, read) of
						[#product{characteristics = Chars, product = OfferId,
								balance = BucketRefs} = Product] ->
							case mnesia:read(offer, OfferId, read) of
								[#offer{} = Offer] ->
									Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write)
											|| Id <- BucketRefs]),
									F2 = fun(#bucket{units = cents, remain_amount = RM}) when RM < 0 ->
												false;
											(_) ->
												true
									end,
									case lists:all(F2, Buckets) of
										true ->
											State = #state{buckets = Buckets,
													product  = Product,
													chars = Chars,
													service_type = ServiceType,
													service_id = ServiceId,
													charging_key = ChargingKey,
													service_network = ServiceNetwork,
													session_id = get_session_id(SessionAttributes)},
											rate1(Protocol, Service, Buckets,
													Timestamp, Address, Direction, Offer,
													Flag, DebitAmounts, ReserveAmounts, State);
										false ->
											{out_of_credit, SessionList, [], []}
									end;
								[] ->
									throw(offer_not_found)
							end;
						[] ->
							throw(product_not_found)
					end;
				[] ->
					throw(service_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {ok, Sub, Rated, DeletedBuckets, AccBalance}}
				when is_list(Rated) ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Rated};
		{atomic, {ok, Sub, Granted, Rated, DeletedBuckets, AccBalance}}
				when is_list(Rated) ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Granted, Rated};
		{atomic, {out_of_credit, SL, Rated, DeletedBuckets, AccBalance}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, SL, Rated};
		{atomic, {grant, Sub, {_Units, Amount} = Granted, DeletedBuckets,
				AccBalance}} when is_integer(Amount) ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Granted};
		{atomic, {out_of_credit, SL, DeletedBuckets, AccBalance}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, SL};
		{atomic, {disabled, SL, DeletedBuckets, AccBalance}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{disabled, SL};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
rate1(Protocol, Service, Buckets, Timestamp, Address, Direction,
		#offer{specification = undefined, bundle = Bundle}, Flag,
		DebitAmounts, ReserveAmounts, #state{service_type = ServiceType} = State) ->
	try
		F = fun(#bundled_po{name = OfferId}, Acc) ->
				case mnesia:read(offer, OfferId, read) of
					[#offer{specification = Spec, status = Status} = P] when
							((Status == active) orelse (Status == undefined))
							and
							(((Protocol == radius)
								and
								(((ServiceType == ?RADIUSFRAMED) orelse (ServiceType == ?RADIUSLOGIN))
								and ((Spec == "4") orelse (Spec == "8"))) orelse
								((ServiceType == ?RADIUSVOICE) and ((Spec == "5") orelse (Spec == "9"))))
							orelse
							((Protocol == diameter)
								and
								((ServiceType == ?DIAMETERDATA) and ((Spec == "4") orelse (Spec == "8")))
								orelse
								((ServiceType == ?DIAMETERVOICE) and ((Spec == "5") orelse (Spec == "9")))
								orelse
								((ServiceType == ?DIAMETERSMS) and ((Spec == "10") orelse (Spec == "11"))))) ->
						[P | Acc];
					_ ->
						Acc
				end
		end,
		[#offer{name = OfferName} = Offer | _] = lists:foldl(F, [], Bundle),
		rate2(Protocol, Service, Buckets, Timestamp,
				Address, Direction, Offer, Flag, DebitAmounts,
				ReserveAmounts, State#state{rated = #rated{product = OfferName}})
	catch
		_:_ ->
			throw(invalid_bundle_product)
	end;
rate1(Protocol, Service, Buckets, Timestamp, Address,
		Direction, #offer{name = OfferName} = Offer,
		Flag, DebitAmounts, ReserveAmounts, State) ->
	rate2(Protocol, Service, Buckets, Timestamp, Address,
		Direction, Offer, Flag, DebitAmounts, ReserveAmounts,
		State#state{rated = #rated{product = OfferName}}).
%% @hidden
rate2(Protocol, Service, Buckets, Timestamp, Address, Direction,
		#offer{specification = ProdSpec, price = Prices} = Offer,
		Flag, DebitAmounts, ReserveAmounts,
		#state{charging_key = ChargingKey} = State)
		when ProdSpec == "10"; ProdSpec == "11" ->
	F = fun(#price{type = usage, units = messages}) ->
				true;
			(#price{type = usage, units = cents}) ->
				true;
			(#price{type = tariff, units = messages}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	case filter_prices_dir(Direction, FilteredPrices3) of
		[Price | _] ->
			RoamingTable = roaming_table_prefix(Price),
			rate3(Protocol, Service, Buckets, Address,
					Price, Flag, DebitAmounts, ReserveAmounts,
					State#state{roaming_tb_prefix = RoamingTable});
		_ ->
			throw(price_not_found)
	end;
rate2(Protocol, Service, Buckets, Timestamp, Address, Direction,
		#offer{specification = ProdSpec, price = Prices} = Offer,
		Flag, DebitAmounts, ReserveAmounts,
		#state{charging_key = ChargingKey} = State)
		when ProdSpec == "5"; ProdSpec == "9" ->
	F = fun(#price{type = tariff, units = seconds}) ->
				true;
			(#price{type = usage, units = seconds}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	case filter_prices_dir(Direction, FilteredPrices3) of
		[Price | _] ->
			RoamingTable = roaming_table_prefix(Price),
			rate3(Protocol, Service, Buckets, Address,
					Price, Flag, DebitAmounts, ReserveAmounts,
					State#state{roaming_tb_prefix = RoamingTable});
		_ ->
			throw(price_not_found)
	end;
rate2(Protocol, Service, Buckets, Timestamp, _Address, _Direction,
		#offer{price = Prices} = Offer, Flag, DebitAmounts, ReserveAmounts,
		#state{charging_key = ChargingKey} = State) ->
	F = fun(#price{type = tariff, units = octets}) ->
				true;
			(#price{type = usage}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	case filter_prices_tod(Timestamp, FilteredPrices2) of
		[Price | _] ->
			RoamingTable = roaming_table_prefix(Price),
			rate4(Protocol, Service, Buckets, Price,
					Flag, DebitAmounts, ReserveAmounts,
					State#state{roaming_tb_prefix = RoamingTable});
		_ ->
			throw(price_not_found)
	end.
%% @hidden
rate3(Protocol, Service, Buckets, Address,
		#price{type = tariff, char_value_use = CharValueUse} = Price,
		Flag, DebitAmounts, ReserveAmounts, #state{rated = Rated,
		roaming_tb_prefix = RoamingTable, service_network = ServiceNetwork} = State) ->
	case lists:keyfind("destPrefixTariffTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = TariffTable}]}
				when RoamingTable == undefined ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{Description, Amount, _} when is_integer(Amount) ->
					case Amount of
						N when N >= 0 ->
							rate5(Protocol, Service, Buckets,
									Price#price{amount = N}, Flag, DebitAmounts, ReserveAmounts,
									State#state{rated = Rated#rated{price_type = tariff,
											description = Description}});
						_N ->
							throw(negative_amount)
					end;
				Other ->
					error_logger:error_report(["Prefix table tariff lookup failed",
							{module, ?MODULE}, {table, Table},
							{address, Address}, {result, Other}]),
					throw(table_lookup_failed)
			end;
		#char_value_use{values = [#char_value{value = TariffTable}]}
				when ServiceNetwork /= undefined ->
			Table1 = list_to_existing_atom(RoamingTable),
			case catch ocs:find_sn_network(Table1, ServiceNetwork) of
				{_, _, _Description, TabPrefix} ->
						Table2 = list_to_existing_atom(TabPrefix ++ "-" ++ TariffTable),
						case catch ocs_gtt:lookup_last(Table2, Address) of
							{Description1, Amount, _} when is_integer(Amount) ->
								case Amount of
									N when N >= 0 ->
										rate5(Protocol, Service, Buckets,
												Price#price{amount = N}, Flag, DebitAmounts, ReserveAmounts,
												State#state{rated = Rated#rated{price_type = tariff,
												description = Description1}});
									_N ->
										throw(negative_amount)
								end;
							Other ->
								error_logger:error_report(["Prefix table tariff lookup failed",
										{module, ?MODULE}, {table, Table2},
										{address, Address}, {result, Other}]),
								throw(table_lookup_failed)
						end;
				Other ->
					error_logger:error_report(["Service Network table lookup failed",
							{module, ?MODULE}, {table, Table1},
							{address, Address}, {result, Other}]),
					throw(table_lookup_failed)
			end;
		false ->
			throw(undefined_tariff)
	end;
rate3(Protocol, Service, Buckets, _Address,
		Price, Flag, DebitAmounts, ReserveAmounts, State) ->
	rate5(Protocol, Service, Buckets, Price,
			Flag, DebitAmounts, ReserveAmounts, State).
%% @hidden
rate4(Protocol, Service, Buckets,
		#price{type = tariff} = Price, Flag, DebitAmounts, ReserveAmounts,
		#state{roaming_tb_prefix = RoamingTable, service_network = ServiceNetwork,
		rated = Rated} = State)
		when is_list(RoamingTable), is_list(ServiceNetwork) ->
	Table = list_to_existing_atom(RoamingTable),
	case catch ocs_gtt:lookup_last(Table, ServiceNetwork) of
		{Description, Amount, _} when is_integer(Amount) ->
			case Amount of
				N when N >= 0 ->
					rate5(Protocol, Service, Buckets,
							Price#price{amount = N}, Flag, DebitAmounts, ReserveAmounts,
							State#state{rated = Rated#rated{price_type = tariff,
							description = Description}});
				_N ->
					throw(negative_amount)
			end;
		Other ->
			error_logger:error_report(["Prefix table tariff lookup failed",
					{module, ?MODULE}, {table, Table},
					{service_network, ServiceNetwork}, {result, Other}]),
			throw(table_lookup_failed)
	end;
rate4(Protocol, Service, Buckets, Price,
		Flag, DebitAmounts, ReserveAmounts, State) ->
	rate5(Protocol, Service, Buckets, Price,
			Flag, DebitAmounts, ReserveAmounts, State).
%% @hidden
rate5(_Protocol, #service{enabled = false} = Service,
		Buckets, #price{units = Units} = _Price, initial,
		_DebitAmounts, _ReserveAmounts, State) ->
	rate7(Service, Buckets, initial,
			{Units, 0}, {Units, 0}, {Units, 0}, {Units, 0}, State);
rate5(radius, Service, Buckets, #price{units = Units} = Price,
		initial, [], [], State) ->
	rate6(Service, Buckets, Price, initial, {Units, 0}, get_reserve(Price), State);
rate5(radius, Service, Buckets, #price{units = Units} = Price,
		interim, DebitAmounts, ReserveAmounts, State) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, 0}
	end,
	ReserveAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{_, ReserveUnits} ->
			{Units, Amount} = get_reserve(Price),
			{Units, ReserveUnits + Amount};
		false ->
			get_reserve(Price)
	end,
	rate6(Service, Buckets, Price, interim, DebitAmount, ReserveAmount, State);
rate5(_Protocol, Service, Buckets,
		#price{units = Units, size = Size} = Price,
		Flag, DebitAmounts, [], State)
		when ((Flag == initial) or (Flag == interim)) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, 0}
	end,
	ReserveAmount = case Units of
		octets ->
			case application:get_env(ocs, min_reserve_octets) of
				{ok, Value} when Value < Size ->
					{Units, Size};
				{ok, Value} ->
					{Units, Value}
			end;
		seconds ->
			case application:get_env(ocs,min_reserve_seconds) of
				{ok, Value} when Value < Size ->
					{Units, Size};
				{ok, Value} ->
					{Units, Value}
			end;
		messages ->
			case application:get_env(ocs, min_reserve_messages) of
				{ok, Value} when Value < Size ->
					{Units, Size};
				{ok, Value} ->
					{Units, Value}
			end
	end,
	rate6(Service, Buckets, Price, Flag, DebitAmount, ReserveAmount, State);
rate5(_Protocol, Service, Buckets, #price{units = Units, size = Size} = Price,
		event, _DebitAmounts, undefined, State) ->
	DebitAmount = {Units, Size},
	ReserveAmount = {Units, 0},
	rate6(Service, Buckets, Price, event, DebitAmount, ReserveAmount, State);
rate5(_Protocol, Service, Buckets, #price{units = Units, size = Size} = Price,
		event, _DebitAmounts, ReserveAmounts, State) ->
	DebitAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, Size}
	end,
	ReserveAmount = {Units, 0},
	rate6(Service, Buckets, Price, event, DebitAmount, ReserveAmount, State);
rate5(_Protocol, Service, Buckets, #price{units = Units} = Price,
		Flag, DebitAmounts, undefined, State) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, 0}
	end,
	ReserveAmount = {Units, 0},
	rate6(Service, Buckets, Price, Flag, DebitAmount, ReserveAmount, State);
rate5(_Protocol, Service, Buckets, #price{units = Units} = Price,
		Flag, DebitAmounts, ReserveAmounts, State)
		when is_list(ReserveAmounts) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, 0}
	end,
	ReserveAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{Units, ReserveUnits} ->
			{Units, ReserveUnits};
		false ->
			{Units, 0}
	end,
	rate6(Service, Buckets, Price, Flag, DebitAmount, ReserveAmount, State).
%% @hidden
rate6(Service, Buckets,
		#price{units = Units, size = UnitSize, amount = UnitPrice},
		initial, {_, 0}, {Units, Amount} = ReserveAmount,
		#state{session_id = SessionId, service_id = ServiceId,
		charging_key = ChargingKey} = State) ->
	case update_session(Units, 0, Amount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{0, UnitsReserved, Buckets2} when UnitsReserved >= Amount ->
			rate7(Service, Buckets2, initial,
					{Units, 0}, {Units, 0}, ReserveAmount,
					{Units, UnitsReserved}, State);
		{0, UnitsReserved, Buckets2} when UnitsReserved < Amount ->
			PriceReserveUnits = (Amount - UnitsReserved),
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case convert(PriceReserve, Units, UnitReserve,
					ServiceId, ChargingKey, SessionId, Buckets2) of
				{ok, Buckets3} ->
					{0, UnitReserve, Buckets4} = update_session(Units, 0,
							UnitReserve, ServiceId, ChargingKey, SessionId, Buckets3),
					rate7(Service, Buckets4, initial,
							{Units, 0}, {Units, 0}, ReserveAmount,
							{Units, UnitsReserved + UnitReserve}, State);
				false ->
					rate7(Service, Buckets2, initial,
							{Units, 0}, {Units, 0}, ReserveAmount,
							{Units, UnitsReserved}, State)
			end
	end;
rate6(#service{enabled = false} = Service, Buckets,
		#price{units = Units, size = UnitSize, amount = UnitPrice},
		interim, {Units, Amount} = DebitAmount, _ReserveAmount,
		#state{session_id = SessionId, service_id = ServiceId,
		charging_key = ChargingKey} = State) ->
	case update_session(Units, Amount, 0,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Amount, 0, Buckets2} ->
			rate7(Service, Buckets2, interim,
					DebitAmount, DebitAmount, {Units, 0}, {Units, 0}, State);
		{UnitsCharged, 0, Buckets2} when UnitsCharged < Amount ->
			NewChargeUnits = Amount - UnitsCharged,
			{UnitCharge, PriceCharge} = price_units(NewChargeUnits,
					UnitSize, UnitPrice),
			case convert(PriceCharge, Units, UnitCharge,
					ServiceId, ChargingKey, SessionId, Buckets2) of
				{ok, Buckets3} ->
					case update_session(Units, NewChargeUnits, 0,
							ServiceId, ChargingKey, SessionId, Buckets3) of
						{NewChargeUnits, 0, Buckets4} ->
							rate7(Service, Buckets4, interim,
									DebitAmount, {Units, UnitsCharged},
									{Units, 0}, {Units, 0}, State);
						{NewUnitsCharged, 0, Buckets4}
								when NewUnitsCharged < NewChargeUnits->
							Now = erlang:system_time(?MILLISECOND),
							NewReservation = {Now, 0, NewUnitsCharged - NewChargeUnits,
									ServiceId, ChargingKey, SessionId},
							LM = make_lm(),
							Buckets5 = [#bucket{id = make_id(LM), last_modified = LM,
									start_date = Now, end_date = Now,
									remain_amount = 0, reservations = [NewReservation],
									units = Units} | Buckets4],
							rate7(Service, Buckets5, interim,
									DebitAmount, {Units, UnitsCharged + NewUnitsCharged},
									{Units, 0}, {Units, 0}, State)
					end;
				false ->
					Now = erlang:system_time(?MILLISECOND),
					NewReservation = {Now, 0, NewChargeUnits - Amount,
							ServiceId, ChargingKey, SessionId},
					LM = make_lm(),
					Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now, end_date = Now,
							remain_amount = 0, reservations = [NewReservation],
							units = Units} | Buckets2],
					rate7(Service, Buckets4, interim,
							DebitAmount, {Units, UnitsCharged},
							{Units, 0}, {Units, 0}, State)
			end
	end;
rate6(Service, Buckets,
		#price{units = Units, size = UnitSize, amount = UnitPrice},
		interim, {Units, Damount} = DebitAmount, {Units, Ramount} = ReserveAmount,
		#state{session_id = SessionId, service_id = ServiceId,
		charging_key = ChargingKey} = State) ->
	case update_session(Units, Damount, Ramount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Damount, UnitsReserved, Buckets2} when UnitsReserved >= Ramount ->
			rate7(Service, Buckets2, interim, DebitAmount, DebitAmount,
					ReserveAmount, {Units, UnitsReserved}, State);
		{Damount, UnitsReserved, Buckets2} when UnitsReserved < Ramount ->
			NewReserveUnits = Ramount - UnitsReserved,
			{UnitReserve, PriceReserve} = price_units(NewReserveUnits,
					UnitSize, UnitPrice),
			case convert(PriceReserve, Units, UnitReserve,
					ServiceId, ChargingKey, SessionId, Buckets2) of
				{ok, Buckets3} ->
					{0, NewReserveUnits, Buckets4} = update_session(Units,
							0, NewReserveUnits, ServiceId,
							ChargingKey, SessionId, Buckets3),
					rate7(Service, Buckets4, interim,
							DebitAmount, DebitAmount, ReserveAmount,
							{Units, UnitsReserved + UnitReserve}, State);
				false ->
					rate7(Service, Buckets2, interim, DebitAmount, DebitAmount,
							ReserveAmount, {Units, UnitsReserved}, State)
			end;
		{UnitsCharged, 0, Buckets2} when UnitsCharged < Damount ->
			NewChargeUnits = Damount - UnitsCharged,
			{ConvertReserve, PriceReserve} = price_units(NewChargeUnits + Ramount,
					UnitSize, UnitPrice),
			{UnitReserve, _} = price_units(Ramount, UnitSize, UnitPrice),
			case convert(PriceReserve, Units, ConvertReserve,
					ServiceId, ChargingKey, SessionId, Buckets2) of
				{ok, Buckets3} ->
					case update_session(Units, NewChargeUnits, UnitReserve,
							ServiceId, ChargingKey, SessionId, Buckets3) of
						{NewChargeUnits, UnitsReserved, Buckets4}
								when UnitsReserved >= UnitReserve ->
							rate7(Service, Buckets4, interim, DebitAmount,
									{Units, UnitsCharged + NewChargeUnits},
									ReserveAmount, {Units, UnitsReserved}, State);
						{NewUnitsCharged, 0, Buckets4}
								when NewUnitsCharged < NewChargeUnits ->
							Now = erlang:system_time(?MILLISECOND),
							NewReservation = {Now, NewUnitsCharged, 0,
									ServiceId, ChargingKey, SessionId},
							LM = make_lm(),
							Buckets5 = [#bucket{id = make_id(LM), last_modified = LM,
									start_date = Now, end_date = Now,
									remain_amount = NewUnitsCharged - NewChargeUnits,
									reservations = [NewReservation],
									units = Units} | Buckets4],
							rate7(Service, Buckets5, interim, DebitAmount,
									{Units, UnitsCharged + NewUnitsCharged},
									ReserveAmount, {Units, 0}, State)
					end;
				false ->
					Now = erlang:system_time(?MILLISECOND),
					NewReservation = {Now, UnitsCharged, 0,
							ServiceId, ChargingKey, SessionId},
					LM = make_lm(),
					Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now, end_date = Now,
							remain_amount = UnitsCharged - Damount,
							reservations = [NewReservation],
							units = Units} | Buckets2],
					rate7(Service, Buckets4, interim,
							DebitAmount, {Units, Damount - UnitsCharged},
							ReserveAmount, {Units, 0}, State)
			end
	end;
rate6(Service, Buckets1,
		#price{units = Units, size = UnitSize, amount = UnitPrice,
		type = PriceType, currency = Currency}, final,
		{Units, Amount} = DebitAmount, {Units, 0} = ReserveAmount,
		#state{rated = Rated1, session_id = SessionId,
		service_id = ServiceId, charging_key = ChargingKey} = State) ->
	Rated2 = Rated1#rated{price_type = PriceType, currency = Currency},
	case charge_session(Units, Amount,
			ServiceId, ChargingKey, SessionId, Buckets1) of
		{Amount, Buckets2} ->
			{Debits, Buckets3} = get_final(ServiceId,
					ChargingKey, SessionId, Buckets2),
			Rated3 = rated(Debits, Rated2),
			rate7(Service, Buckets3, final, DebitAmount, DebitAmount,
					ReserveAmount, ReserveAmount, State#state{rated = Rated3});
		{UnitsCharged, Buckets2} when UnitsCharged < Amount ->
			{UnitCharge, PriceCharge} = price_units(Amount - UnitsCharged,
					UnitSize, UnitPrice),
			case charge_session(cents, PriceCharge,
					ServiceId, ChargingKey, SessionId, Buckets2) of
				{PriceCharge, Buckets3} ->
					TotalUnits = UnitsCharged + UnitCharge,
					{Debits, Buckets4} = get_final(ServiceId,
							ChargingKey, SessionId, Buckets3),
					Rated3 = rated(Debits, Rated2),
					rate7(Service, Buckets4, final, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, State#state{rated = Rated3});
				{PriceCharged, Buckets3}  when PriceCharged < PriceCharge ->
					TotalUnits = UnitsCharged + (PriceCharged div UnitPrice),
					{Debits, Buckets4} = get_final(ServiceId,
							ChargingKey, SessionId, Buckets3),
					Rated3 = rated(Debits, Rated2),
					Now = erlang:system_time(?MILLISECOND),
					NewReservation = {Now, UnitsCharged, 0,
							ServiceId, ChargingKey, SessionId},
					LM = make_lm(),
					Buckets5 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now,
							remain_amount = PriceCharged - PriceCharge,
							reservations = [NewReservation],
							units = cents} | Buckets4],
					rate7(Service, Buckets5, final, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, State#state{rated = Rated3})
			end
	end;
rate6(Service, Buckets1,
		#price{units = Units, size = UnitSize, amount = UnitPrice,
		type = PriceType, currency = Currency}, event,
		{Units, Amount} = DebitAmount, {Units, 0} = ReserveAmount,
		#state{rated = Rated1, session_id = SessionId,
		service_id = ServiceId, charging_key = ChargingKey} = State) ->
	Rated2 = Rated1#rated{price_type = PriceType, currency = Currency},
	case charge_event(Units, Amount, Buckets1) of
		{Amount, Buckets2} ->
			Rated3 = rated(#{Units => Amount}, Rated2),
			rate7(Service, Buckets2, event, DebitAmount, DebitAmount,
					ReserveAmount, ReserveAmount, State#state{rated = Rated3});
		{UnitsCharged, Buckets2} when UnitsCharged < Amount ->
			{UnitCharge, PriceCharge} = price_units(Amount - UnitsCharged,
					UnitSize, UnitPrice),
			case charge_event(cents, PriceCharge, Buckets2) of
				{PriceCharge, Buckets3} ->
					TotalUnits = UnitsCharged + UnitCharge,
					Rated3 = rated(#{Units => Amount, cents => PriceCharge}, Rated2),
					rate7(Service, Buckets3, event, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, State#state{rated = Rated3});
				{PriceCharged, Buckets3}  when PriceCharged < PriceCharge ->
					TotalUnits = UnitsCharged + (PriceCharged div UnitPrice),
					Rated3 = rated(#{Units => Amount, cents => PriceCharged}, Rated2),
					Now = erlang:system_time(?MILLISECOND),
					NewReservation = {Now, UnitsCharged, 0,
							ServiceId, ChargingKey, SessionId},
					LM = make_lm(),
					Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now,
							remain_amount = PriceCharged - PriceCharge,
							reservations = [NewReservation],
							units = cents} | Buckets3],
					rate7(Service, Buckets4, event, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, State#state{rated = Rated3})
			end
	end.
%% @hidden
rate7(#service{session_attributes = SessionList} = Service1, Buckets, final,
		{Units, Charge}, {Units, Charged}, {Units, 0}, {Units, 0},
		#state{rated = Rated, product = P, session_id = SessionId,
		service_id = ServiceId, charging_key = ChargingKey,
		buckets = OldBuckets}) when Charged >= Charge ->
	{Debits, NewBuckets} = get_final(ServiceId,
			ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	Rated1 = rated(Debits, Rated),
	NewSessionList = remove_session(SessionId, SessionList),
	Service2 = Service1#service{session_attributes = NewSessionList},
	ok = mnesia:write(Service2),
	{ok, Service2, Rated1, DeletedBuckets,
			accumulated_balance(NewBuckets, P#product.id)};
rate7(#service{session_attributes = SessionList} = Service1, Buckets, final,
		{Units, _Charge}, {Units, _Charged}, {Units, 0}, {Units, 0},
		#state{rated = Rated, product = P, session_id = SessionId,
		service_id = ServiceId, charging_key = ChargingKey,
		buckets = OldBuckets}) ->
	{Debits, NewBuckets} = get_final(ServiceId,
			ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	Rated1 = rated(Debits, Rated),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, Rated1, DeletedBuckets,
			accumulated_balance(NewBuckets, P#product.id)};
rate7(#service{enabled = false, session_attributes = SessionList} = Service1,
		Buckets, _Flag, {Units, _Charge}, {Units, _Charged},
		{Units, _Reserve}, {Units, _Reserved},
		#state{session_id = SessionId, service_id = ServiceId,
		charging_key = ChargingKey, buckets = OldBuckets, product = P}) ->
	NewBuckets = refund(ServiceId, ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{disabled, SessionList, DeletedBuckets,
			accumulated_balance(NewBuckets, P#product.id)};
rate7(#service{session_attributes = SessionList} = Service1, Buckets, _Flag,
		{Units, Charge}, {Units, Charged}, {Units, Reserve}, {Units, Reserved},
		#state{session_id = SessionId, service_id = ServiceId,
		charging_key = ChargingKey, buckets = OldBuckets,
		product = P}) when Charged < Charge; Reserved <  Reserve ->
	NewBuckets = refund(ServiceId, ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, DeletedBuckets,
			accumulated_balance(NewBuckets, P#product.id)};
rate7(#service{session_attributes = SessionList} = Service1, Buckets, initial,
		{Units, 0}, {Units, 0}, {Units, _Reserve}, {Units, Reserved},
		#state{buckets = OldBuckets, session_id = SessionId, product = P}) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	NewSessionList = add_session(SessionId, SessionList),
	Service2 = Service1#service{session_attributes = NewSessionList},
	ok = mnesia:write(Service2),
	{grant, Service2, {Units, Reserved}, DeletedBuckets,
			accumulated_balance(Buckets, P#product.id)};
rate7(Service, Buckets, interim, {Units, _Charge}, {Units, _Charged},
		{Units, _Reserve}, {Units, Reserved},
		#state{buckets = OldBuckets, product = P}) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	ok = mnesia:write(Service),
	{grant, Service, {Units, Reserved}, DeletedBuckets,
			accumulated_balance(Buckets, P#product.id)};
rate7(Service, Buckets, event,
		{Units, Charge}, {Units, Charged}, {Units, 0}, {Units, 0},
		#state{rated = Rated, product = P, buckets = OldBuckets})
		when Charged >= Charge ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	ok = mnesia:write(Service),
	{ok, Service, {Units, Charged}, Rated, DeletedBuckets,
			accumulated_balance(Buckets, P#product.id)};
rate7(#service{session_attributes = SessionList} = Service1, Buckets, event,
		{Units, _Charge}, {Units, _Charged}, {Units, 0}, {Units, 0},
		#state{rated = Rated, product = P, buckets = OldBuckets}) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(P#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(P#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, Rated, DeletedBuckets,
			accumulated_balance(Buckets, P#product.id)}.

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
		Subscriber :: #service{},
		Attributes :: [tuple()],
		SessionList :: [tuple()],
		Reason :: disabled | bad_password | service_not_found
				| out_of_credit | offer_not_found | invalid_bundle_product
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
			case mnesia:read(service, SubscriberId) of
				[#service{enabled = false,
						session_attributes = ExistingAttr} = S] ->
					ok = mnesia:write(S#service{session_attributes = []}),
					{unauthorized, disabled, ExistingAttr};
				[#service{password = MTPassword} = S] when
						((Password == <<>>) and (Password =/= MTPassword)) orelse
						(Password == MTPassword) ->
					authorize1(Protocol, ServiceType, S, Timestamp,
							Address, Direction, SessionAttributes);
				[#service{}] ->
					throw(bad_password);
				[] ->
					throw(service_not_found)
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
		#service{attributes = Attributes, product = ProdRef,
		characteristics = Chars} = Service, Timestamp, Address,
		Direction, SessionAttributes) ->
	case mnesia:read(product, ProdRef, read) of
		[#product{product = OfferId, balance = BucketRefs}] ->
			Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write) || Id <- BucketRefs]),
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
							case mnesia:read(offer, OfferId, read) of
								[#offer{} = Offer] ->
									authorize2(radius, ServiceType, Service, Buckets, Offer,
											Timestamp, Address, Direction, SessionAttributes, RRST);
								[] ->
									throw(offer_not_found)
							end;
						false ->
							authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
					end;
				false ->
					authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
			end;
		[] ->
			throw(product_not_found)
	end;
authorize1(diameter, ServiceType,
		#service{attributes = Attributes, product = ProdRef} =
		Service, _Timestamp, _Address, _Direction, SessionAttributes) ->
	case mnesia:read(product, ProdRef, read) of
		[#product{balance = BucketRefs}] ->
			Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write) || Id <- BucketRefs]),
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes);
		[] ->
			throw(product_not_found)
	end.
%% @hidden
authorize2(radius = Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets, Timestamp,
		#offer{specification = undefined, bundle = Bundle}, Address, Direction,
		SessionAttributes, Reserve) when Reserve > 0, Bundle /= [] ->
	try
		F = fun(#bundled_po{name = OfferId}, Acc) ->
				case mnesia:read(offer, OfferId, read) of
					[#offer{specification = Spec, status = Status} = P] when
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
			[#offer{} = Offer | _] ->
				authorize2(Protocol, ServiceType, Service, Buckets, Offer,
						Timestamp, Address, Direction, SessionAttributes, Reserve);
			[] ->
				authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
		end
	catch
		_:_ ->
			throw(invalid_bundle_product)
	end;
authorize2(radius = Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets,
		#offer{specification = ProdSpec, price = Prices}, Timestamp,
		Address, Direction, SessionAttributes, Reserve) when (Reserve > 0)
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
			authorize3(Protocol, ServiceType, Service, Buckets, Address,
					Price, SessionAttributes, Reserve);
		_ ->
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
	end;
authorize2(radius = Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets,
		#offer{specification = ProdSpec, price = Prices}, Timestamp,
		_Address, _Direction, SessionAttributes, Reserve) when (Reserve > 0)
		and ((ProdSpec == "8") orelse (ProdSpec == "4")) ->
	F = fun(#price{type = usage, units = seconds}) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	case filter_prices_tod(Timestamp, FilteredPrices1) of
		[Price | _] ->
			authorize4(Protocol, ServiceType, Service,
					Buckets, Price, SessionAttributes, Reserve);
		_ ->
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
	end;
authorize2(_Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets, _Offer,
		_Timestamp, _Address, _Direction, SessionAttributes, _Reserve) ->
	authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes).
%% @hidden
authorize3(Protocol, ServiceType, Service, Buckets, Address,
		#price{type = tariff, char_value_use = CharValueUse} = Price,
		SessionAttributes, Reserve) ->
	case lists:keyfind("destPrefixTariffTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = TariffTable}]} ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{_Description, Amount, _} when is_integer(Amount) ->
					case Amount of
						N when N >= 0 ->
							authorize4(Protocol, ServiceType, Service, Buckets,
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
authorize3(Protocol, ServiceType, Service,
		Buckets, _Address, Price, SessionAttributes, Reserve) ->
	authorize4(Protocol, ServiceType, Service,
			Buckets, Price, SessionAttributes, Reserve).
%% @hidden
authorize4(_Protocol, ServiceType,
		#service{session_attributes = ExistingAttr, attributes = Attr} = Service,
		Buckets, #price{units = Units, size = UnitSize, amount = UnitPrice},
		SessionAttributes, Reserve) ->
	SessionId = get_session_id(SessionAttributes),
	case update_session(Units, 0, Reserve,
			undefined, undefined, SessionId, Buckets) of
		{0, Reserve, _Buckets2} ->
			NewAttr = radius_attributes:store(?SessionTimeout, Reserve, Attr),
			authorize5(Service, Buckets, ServiceType, SessionAttributes, NewAttr);
		{0, UnitsReserved, Buckets2} ->
			PriceReserveUnits = (Reserve- UnitsReserved),
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case update_session(cents, 0, PriceReserve,
					undefined, undefined, SessionId, Buckets2) of
				{0, PriceReserve, _Buckets3}  ->
					SessionTimeout = UnitsReserved + UnitReserve,
					NewAttr = radius_attributes:store(?SessionTimeout, SessionTimeout, Attr),
					authorize5(Service, Buckets, ServiceType, SessionAttributes, NewAttr);
				{0, 0, _Buckets3}  when UnitsReserved == 0 ->
					{unauthorized, out_of_credit, ExistingAttr};
				{0, PriceReserved, _Buckets3} ->
					SessionTimeout = UnitsReserved + ((PriceReserved div UnitPrice) * UnitSize),
					NewAttr = radius_attributes:store(?SessionTimeout, SessionTimeout, Attr),
					authorize5(Service, Buckets, ServiceType, SessionAttributes, NewAttr)
			end
	end.
%% @hidden
authorize5(#service{session_attributes = ExistingAttr} = Service,
		Buckets, ServiceType, SessionAttributes, Attributes) ->
	F = fun(#bucket{remain_amount = R, units = U})
				when
				((ServiceType == undefined) orelse
				(((ServiceType == ?RADIUSFRAMED) orelse (ServiceType == ?RADIUSLOGIN) orelse (ServiceType == ?DIAMETERDATA)) and
				((U == octets) orelse (U == cents) orelse (U == seconds))) orelse
				(((ServiceType == ?RADIUSVOICE) orelse (ServiceType == ?DIAMETERVOICE)) and
				((U == seconds) orelse (U == cents))) orelse
				((ServiceType == ?DIAMETERSMS) and ((U == messages) orelse (U == cents)))) and (R > 0) ->
			true;
		(_) ->
			false
	end,
	case lists:any(F, Buckets) of
		true ->
			authorize6(Service, SessionAttributes, Attributes);
		false ->
			{unauthorized, out_of_credit, ExistingAttr}
	end.
%% @hidden
authorize6(#service{multisession = false, session_attributes = []}
		= Service, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(?MILLISECOND),
			get_session_id(SessionAttributes)},
	Service1 = Service#service{session_attributes =
		[NewSessionAttributes], disconnect = false},
	ok = mnesia:write(Service1),
	{authorized, Service1, Attributes, []};
authorize6(#service{multisession = false, session_attributes
		= ExistingAttr} = Service, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(?MILLISECOND),
			get_session_id(SessionAttributes)},
	Service1 = Service#service{session_attributes =
		[NewSessionAttributes], disconnect = false},
	ok = mnesia:write(Service1),
	{authorized, Service1, Attributes, ExistingAttr};
authorize6(#service{multisession = true, session_attributes
		= ExistingAttr} = Service, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(?MILLISECOND),
			get_session_id(SessionAttributes)},
	Service1 = Service#service{session_attributes =
		[NewSessionAttributes | ExistingAttr], disconnect = false},
	ok = mnesia:write(Service1),
	{authorized, Service1, Attributes, ExistingAttr}.

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

-spec update_session(Type, Charge, Reserve, ServiceId, ChargingKey,
		SessionId, Buckets) -> Result
	when
		Type :: octets | seconds | cents | messages,
		Charge :: non_neg_integer(),
		Reserve :: non_neg_integer(),
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {Charged, Reserved, NewBuckets},
		Charged :: non_neg_integer(),
		Reserved :: non_neg_integer(),
		NewBuckets :: [#bucket{}].
%% @doc Perform debit and reservation for a session.
%%
%% 	Finds reservations matching `SessionId', `ChargingKey'
%% 	and `ServiceId'.
%%
%% 	Empty or expired buckets are removed when no session
%% 	reservations remain.
%%
%% 	Returns `{Charged, Reserved, NewBuckets}' where
%% 	`Charged' is the total amount debited from bucket(s),
%% 	`Reserved' is the total amount of quota reservation,
%% 	and `NewBuckets' is the updated bucket list.
%%
%% 	The `Reserve' amount is not additive, the `Reserved'
%% 	amount is the new total reservation.
%%
%% 	3GPP RS 32.299 6.3.8 Support of re-authorization:
%% 	"New quota allocations [...] override any remaining held quota"
%%
%% @private
update_session(Type, Charge, Reserve,
		ServiceId, ChargingKey, SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	update_session(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, sort(Buckets), [], 0, 0).
%% @hidden
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{start_date = Start, end_date = Expires,
		reservations = [], remain_amount = Remain} | T],
		Acc, Charged, Reserved) when Expires /= undefined,
		Start =/= Expires, Expires =< Now, Remain >= 0 ->
	update_session(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		reservations = Reservations} = B | T],
		Acc, Charged, Reserved) when length(Reservations) > 0 ->
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	F = fun({_, _, _, ServiceId1, ChargingKey1, SessionId1})
					when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _, _, _, _, _}) ->
				false
	end,
	case lists:partition(F, Reservations) of
		{[{_, DebitedAmount, ReservedAmount,
				ServiceId, ChargingKey, _}], NewReservations}
				when (ReservedAmount - NewCharge) > NewReserve ->
			NewRemain = Remain + ((ReservedAmount - NewCharge) - NewReserve),
			NewReservation = {Now, DebitedAmount + NewCharge, NewReserve,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = NewRemain,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserve);
		{[{_, DebitedAmount, ReservedAmount,
				ServiceId, ChargingKey, _}], NewReservations}
				when (ReservedAmount - NewCharge) >= 0 ->
			NewReserved = ReservedAmount - NewCharge,
			NewReservation = {Now, DebitedAmount + NewCharge, NewReserved,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserved);
		{[{_, DebitedAmount, ReservedAmount,
				ServiceId, ChargingKey, _}], NewReservations}
				when ReservedAmount > 0, NewCharge > ReservedAmount ->
			NewReservation = {Now, DebitedAmount + ReservedAmount, 0,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + ReservedAmount, Reserved);
		_Other ->
			update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, [B | Acc], Charged, Reserved)
	end;
update_session(_, Charge, Reserve, _, _, _, _, [], Acc, Charged, Reserved)
		when Charge =:= Charged, Reserve =:= Reserved ->
	{Charged, Reserved, lists:reverse(Acc)};
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[H | T], Acc, Charged, Reserved) ->
	update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			T, [H | Acc], Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[], Acc, Charged, Reserved) when Reserved < Reserve ->
	update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			lists:reverse(Acc), [], Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			lists:reverse(Acc), [], Charged, Reserved).
%% @hidden
update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Charge > Charged) or (Reserve > Reserved)), Remain > 0 ->
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	F = fun({_, _, _, ServiceId1, ChargingKey1, SessionId1})
					when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _, _, _, _, _}) ->
				false
	end,
	case lists:partition(F, Reservations) of
		{[{_, DebitedAmount, 0, ServiceId, ChargingKey, _}], NewReservations}
				when Remain >= (NewCharge + NewReserve) ->
			NewRemain = Remain - (NewReserve + NewCharge),
			NewReservation = {Now, DebitedAmount + NewCharge, NewReserve,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = NewRemain,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserve);
		{[{_, DebitedAmount, 0, ServiceId, ChargingKey, _}], NewReservations}
					when Remain >= NewCharge ->
			NewReserved = Remain - NewCharge,
			NewReservation = {Now, DebitedAmount + NewCharge, NewReserved,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserved);
		{[{_, DebitedAmount, ReservedAmount, ServiceId, ChargingKey, _}], NewReservations}
					when NewCharge =:= 0, Remain >= NewReserve ->
			NewReservation = {Now, DebitedAmount, ReservedAmount + NewReserve,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = Remain - NewReserve,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged, Reserved + NewReserve);
		{[{_, DebitedAmount, ReservedAmount, ServiceId, ChargingKey, _}], NewReservations}
					when NewCharge =:= 0 ->
			NewReservation = {Now, DebitedAmount, ReservedAmount + Remain,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged, Reserved + Remain);
		{[], _} ->
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, [B | Acc], Charged, Reserved)
	end;
update_session1(_, Charge, Reserve, _, _, _, _, Buckets, Acc, Charged, Reserved)
		when Charge =:= Charged, Reserve =:= Reserved ->
	{Charged, Reserved, lists:reverse(Acc) ++ Buckets};
update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[H | T], Acc, Charged, Reserved) ->
	update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			T, [H | Acc], Charged, Reserved);
update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			lists:reverse(Acc), [], Charged, Reserved).

%% @hidden
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{end_date = Expires, reservations = [],
		remain_amount = Remain} | T], Acc, Charged, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0 ->
	update(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged, Reserved);
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain, end_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain >= ((Charge - Charged) + (Reserve - Reserved)) ->
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	NewReservation = {Now, NewCharge, NewReserve,
			ServiceId, ChargingKey, SessionId},
	NewBuckets = [B#bucket{remain_amount = Remain - (NewCharge + NewReserve),
		last_modified = {Now, erlang:unique_integer([positive])},
		reservations = [NewReservation | Reservations]} | Acc],
	{Charged + NewCharge, Reserved + NewReserve, lists:reverse(NewBuckets) ++ T};
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain, end_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain >= (Charge - Charged) ->
	NewCharge = Charge - Charged,
	NewReserve = Remain - NewCharge,
	NewReservation = {Now, NewCharge, NewReserve,
			ServiceId, ChargingKey, SessionId},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			reservations = [NewReservation | Reservations]} | Acc],
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			T, NewAcc, Charged + NewCharge, Reserved + NewReserve);
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain, end_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain < (Charge - Charged) ->
	NewCharge = Charge - Charged,
	NewReservation = {Now, NewCharge, 0, ServiceId, ChargingKey, SessionId},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			reservations = [NewReservation | Reservations]} | Acc],
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			T, NewAcc, Charged + NewCharge, Reserved);
update(_, Charge, Reserve, _, _, _, _, Buckets, Acc, Charged, Reserved)
		when Charge =:= Charged, Reserve =:= Reserved ->
	{Charged, Reserved, lists:reverse(Acc) ++ Buckets};
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[H | T], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
			SessionId, T, [H | Acc], Charged, Reserved);
update(_, _, _,  _, _, _, _, [], Acc, Charged, Reserved) ->
	{Charged, Reserved, lists:reverse(Acc)}.

-spec charge_session(Type, Charge,
		ServiceId, ChargingKey, SessionId, Buckets) -> Result
	when
		Type :: octets | seconds | cents | messages,
		Charge :: non_neg_integer(),
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {Charged, NewBuckets},
		Charged :: non_neg_integer(),
		NewBuckets :: [#bucket{}].
%% @doc Peform final charging for a session.
%%
%% 	Returns `{Charged, NewBuckets}' where
%% 	`Charged' is the total amount debited from the buckets
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
charge_session(Type, Charge,
		ServiceId, ChargingKey, SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	charge_session(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, sort(Buckets), 0, []).
%% @hidden
charge_session(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{remain_amount = Remain, reservations = [],
		start_date = Start, end_date = Expires} | T], Charged, Acc)
		when Remain >= 0, Expires /= undefined, Expires =/= Start,
		Expires =< Now ->
	charge_session(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, T, Charged, Acc);
charge_session(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		reservations = Reservations} = B | T], Charged, Acc) when Charge > 0 ->
	case lists:keytake(SessionId, 6, Reservations) of
		{value, {_, DebitedAmount, ReservedAmount, 
				ServiceId, ChargingKey, _}, NewReservations}
				when ReservedAmount >= Charge ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservedAmount = ReservedAmount - Charge,
			NewReservation = {Now, NewDebitedAmount, NewReservedAmount,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{reservations = [NewReservation | NewReservations],
					last_modified = {Now, erlang:unique_integer([positive])}} | Acc],
			charge_session(Type, 0, Now, ServiceId, ChargingKey, SessionId,
					T, Charged + Charge, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount,
				ServiceId, ChargingKey, _}, NewReservations}
				when ReservedAmount < Charge, Remain >= (Charge - ReservedAmount) ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservation = {Now, NewDebitedAmount, 0,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = Remain - (Charge - ReservedAmount),
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			charge_session(Type, 0, Now, ServiceId, ChargingKey, SessionId,
					T, Charged + Charge, NewAcc);
		{value, {_, DebitedAmount, ReservedAmount,
				ServiceId, ChargingKey, _}, NewReservations}
				when ReservedAmount < Charge, Remain >= 0,
				Remain < (Charge - ReservedAmount) ->
			NewDebitedAmount = DebitedAmount + (Remain + ReservedAmount),
			NewReservation = {Now, NewDebitedAmount, 0,
					ServiceId, ChargingKey, SessionId},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = [NewReservation | NewReservations]} | Acc],
			charge_session(Type, Charge - ReservedAmount - Remain,
					Now, ServiceId, ChargingKey, SessionId,
					T, Charged + ReservedAmount + Remain, NewAcc);
		_ ->
			charge_session(Type, Charge, Now,
					ServiceId, ChargingKey, SessionId, T, Charged, [B | Acc])
	end;
charge_session(_, 0, _, _, _, _, Buckets, Charged, Acc) ->
	{Charged, lists:reverse(Acc) ++ Buckets};
charge_session(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[H | T], Charged, Acc) ->
	charge_session(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
			T, Charged, [H | Acc]);
charge_session(Type, Charge, Now,
		ServiceId, ChargingKey, SessionId, [], Charged, Acc) ->
	charge(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
			lists:reverse(Acc), [], Charged).

%% @hidden
charge(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{end_date = Expires, reservations = [],
		remain_amount = R} | T], Acc, Charged)
		when R >= 0, Expires /= undefined, Expires =< Now ->
	charge(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged);
charge(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = R, end_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged)
		when Charge > 0, R >= Charge,
		((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, Charge, 0, ServiceId, ChargingKey, SessionId},
	NewBuckets = [B#bucket{remain_amount = R - Charge,
			last_modified = {Now, erlang:unique_integer([positive])},
			reservations = [NewReservation | Reservations]} | T],
	{Charged + Charge, lists:reverse(Acc) ++ NewBuckets};
charge(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = R, end_date = Expires,
		reservations = Reservations} = B | T], Acc, Charged)
		when Charge > 0, R =< Charge, R > 0,
		((Expires == undefined) or (Now < Expires)) ->
	NewReservation = {Now, R, 0, ServiceId, ChargingKey, SessionId},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			reservations = [NewReservation | Reservations]} | Acc],
	charge(Type, Charge - R, Now,
			ServiceId, ChargingKey, SessionId, T, NewAcc, Charged + R);
charge(_Type, 0, _, _, _, _, Buckets, Acc, Charged) ->
	{Charged, lists:reverse(Acc) ++ Buckets};
charge(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[H | T], Acc, Charged) ->
	charge(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
			T, [H | Acc], Charged);
charge(_, _, _, _, _, _, [], Acc, Charged) ->
	{Charged, lists:reverse(Acc)}.

-spec charge_event(Type, Charge, Buckets) -> Result
	when
		Type :: octets | seconds | cents | messages,
		Charge :: non_neg_integer(),
		Buckets :: [#bucket{}],
		Result :: {Charged, NewBuckets},
		Charged :: non_neg_integer(),
		NewBuckets :: [#bucket{}].
%% @doc Peform immediate event charging (IEC).
%%
%% 	Returns `{Charged, NewBuckets}' where
%% 	`Charged' is the total amount debited from the buckets
%% 	and `NewBuckets' is the updated bucket list.
%%
%% @private
charge_event(Type, Charge, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	charge_event(Type, Charge, Now, sort(Buckets), 0, []).
%% @hidden
charge_event(Type, Charge, Now,
		[#bucket{remain_amount = Remain, reservations = [],
		start_date = Start, end_date = Expires} | T], Charged, Acc)
		when Remain >= 0, Expires /= undefined, Expires =/= Start,
		Expires =< Now ->
	charge_event(Type, Charge, Now, T, Charged, Acc);
charge_event(Type, Charge, Now,
		[#bucket{units = Type, remain_amount = Remain} = B | T],
		Charged, Acc) when Charge > 0, Remain >= Charge ->
	NewAcc = [B#bucket{remain_amount = Remain - Charge,
			last_modified = {Now, erlang:unique_integer([positive])}} | Acc],
	{Charged + Charge, lists:reverse(NewAcc) ++ T};
charge_event(Type, Charge, Now,
		[#bucket{units = Type, remain_amount = Remain} = B | T],
		Charged, Acc) when Charge > 0, Remain > 0, Remain < Charge ->
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])}} | Acc],
	charge_event(Type, Charge - Remain,
			Now, T, Charged + Remain, NewAcc);
charge_event(_, 0, _, Buckets, Charged, Acc) ->
	{Charged, lists:reverse(Acc) ++ Buckets};
charge_event(Type, Charge, Now, [H | T], Charged, Acc) ->
	charge_event(Type, Charge, Now, T, Charged, [H | Acc]);
charge_event(_, _, _, [], Charged, Acc) ->
	{Charged, lists:reverse(Acc)}.

-spec convert(Price, Type, Size,
		ServiceId, ChargingKey, SessionId, Buckets) -> Result
	when
		Price :: pos_integer(),
		Type :: octets | seconds | messages,
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		Size :: pos_integer(),
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {ok, Buckets} | false.
%% @doc Convert cents to `Type' bucket(s) of `Size'.
%%
%% 	Tops up existing bucket for session if found,
%% 	otherwise creates session bucket(s) of `Type'.
%% @private
convert(Price, Type, Size, ServiceId, ChargingKey, SessionId, Buckets) ->
	Buckets1 = sort(Buckets),
	Fcents = fun(#bucket{units = cents}) ->
				true;
			(_) ->
				false
	end,
	{CentsBuckets, UnitsBuckets} = lists:partition(Fcents, Buckets1),
	Now = erlang:system_time(?MILLISECOND),
	convert(Price, Type, Size, ServiceId, ChargingKey, SessionId,
			Now, CentsBuckets, UnitsBuckets, []).
%% @hidden
convert(0, Type, Size, ServiceId, ChargingKey, SessionId,
		Now, CentsBuckets, UnitsBuckets, Acc) ->
	convert1(Type, Size, ServiceId, ChargingKey, SessionId,
			Now, lists:reverse(Acc) ++ CentsBuckets, UnitsBuckets, []);
convert(Price, Type, Size, ServiceId, ChargingKey, SessionId, Now,
		[#bucket{remain_amount = R, end_date = Expires,
		reservations = []} | T], UnitsBuckets, Acc)
		when R >= 0, Expires /= undefined, Expires =< Now ->
	convert(Price, Type, Size, ServiceId, ChargingKey, SessionId,
			Now, T, UnitsBuckets, Acc);
convert(Price1, Type, Size, ServiceId, ChargingKey, SessionId, Now,
		[#bucket{remain_amount = R, end_date = Expires,
		reservations = Reservations1} = B1 | T], UnitsBuckets, Acc)
		when Price1 > 0, R > 0, ((Expires == undefined) or (Now < Expires)) ->
	F = fun({_, _, _, ServiceId1, ChargingKey1, SessionId1})
					when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _, _, _, _, _}) ->
				false
	end,
	{Price2, B2} = case lists:partition(F, Reservations1) of
		{[{_, DebitedAmount, ReservedAmount, _, _, _}], Reservations2}
				when R >= Price1 ->
			NewReservations = [{Now, DebitedAmount + Price1, ReservedAmount,
					ServiceId, ChargingKey, SessionId} | Reservations2],
			{0, B1#bucket{remain_amount = R - Price1,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = NewReservations}};
		{[{_, DebitedAmount, ReservedAmount, _, _, _}], Reservations2}
				when R < Price1 ->
			NewReservations = [{Now, DebitedAmount + R, ReservedAmount,
					ServiceId, ChargingKey, SessionId} | Reservations2],
			{Price1 - R, B1#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = NewReservations}};
		{[], _} when R >= Price1 ->
			NewReservations = [{Now, Price1, 0,
					ServiceId, ChargingKey, SessionId} | Reservations1],
			{0, B1#bucket{remain_amount = R - Price1,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = NewReservations}};
		{[], _} when R < Price1 ->
			NewReservations = [{Now, R, 0,
					ServiceId, ChargingKey, SessionId} | Reservations1],
			{Price1 - R, B1#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = NewReservations}}
	end,
	convert(Price2, Type, Size, ServiceId, ChargingKey, SessionId,
			Now, T, UnitsBuckets, [B2 | Acc]);
convert(Price, Type, Size, ServiceId, ChargingKey, SessionId,
		Now, [H | T], UnitsBuckets, Acc) ->
	convert(Price, Type, Size, ServiceId, ChargingKey, SessionId,
			Now, T, UnitsBuckets, [H | Acc]);
convert(Price, _, _, _, _, _, _, [], _, _) when Price > 0 ->
	false.
%% @hidden
convert1(Type, Size, ServiceId, ChargingKey, SessionId, Now, CentsBuckets,
		[#bucket{units = Type, name = "session", remain_amount = R,
		reservations = Reservations} = B | T], Acc) ->
	F = fun({_, _, _, ServiceId1, ChargingKey1, SessionId1})
					when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _, _, _, _, _}) ->
				false
	end,
	case lists:any(F, Reservations) of
		true ->
			NewBucket = B#bucket{remain_amount = R + Size,
					last_modified = {Now, erlang:unique_integer([positive])}},
			NewBuckets = CentsBuckets ++ lists:reverse(Acc) ++ [NewBucket | T],
			{ok, NewBuckets};
		false ->
			convert1(Type, Size, ServiceId, ChargingKey, SessionId,
					Now, CentsBuckets, T, [B | Acc])
	end;
convert1(Type, Size, ServiceId, ChargingKey, SessionId,
		Now, CentsBuckets, [H | T], Acc) ->
	convert1(Type, Size, ServiceId, ChargingKey, SessionId,
			Now, CentsBuckets, T, [H | Acc]);
convert1(Type, Size, ServiceId, ChargingKey, SessionId, Now,
		[#bucket{product = Product} | _] = CentsBuckets, [], Acc) ->
	LM = make_lm(),
	NewBucket = #bucket{id = make_id(LM), last_modified = LM,
			start_date = Now, end_date = Now,
			name = "session", product = Product,
			remain_amount = Size, units = Type,
			reservations = [{Now, 0, 0, ServiceId, ChargingKey, SessionId}]},
	NewBuckets = CentsBuckets ++ [NewBucket | lists:reverse(Acc)],
	{ok, NewBuckets}.

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
	F = fun(#bucket{end_date = T1},
				#bucket{end_date = T2}) when T1 =< T2 ->
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
		ReserveAmount :: {Units, Amount},
		Units :: seconds | octets,
		Amount :: pos_integer().
%% @doc Get the reserve amount.
get_reserve(#price{units = seconds,
		char_value_use = CharValueUse} = _Price) ->
	case lists:keyfind("radiusReserveTime",
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = Value}]} ->
			{seconds, Value};
		false ->
			{seconds, 0}
	end;
get_reserve(#price{units = octets,
		char_value_use = CharValueUse} = _Price) ->
	case lists:keyfind("radiusReserveOctets",
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = Value}]} ->
			{octets, Value};
		false ->
			{octets, 0}
	end.

-spec refund(ServiceId, ChargingKey, SessionId, Buckets) -> Buckets
	when
		Buckets :: [#bucket{}],
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		SessionId :: string() | binary().
%% @doc Refund unused reservations.
%% @hidden
refund(ServiceId, ChargingKey, SessionId, Buckets) ->
	refund(ServiceId, ChargingKey, SessionId, Buckets, []).
%% @hidden
refund(ServiceId, ChargingKey, SessionId,
		[#bucket{reservations = Reservations} = H | T], Acc) ->
	refund(ServiceId, ChargingKey, SessionId,
			H, Reservations, T, [], Acc);
refund(_, _, _, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
refund(ServiceId, ChargingKey, SessionId,
		#bucket{remain_amount = R} = Bucket1,
		[{_, 0, Amount, ServiceId, ChargingKey, SessionId} | T1],
		T2, Acc1, Acc2) ->
	Bucket2 = Bucket1#bucket{remain_amount = R + Amount,
			reservations = lists:reverse(Acc1) ++ T1},
	refund(ServiceId, ChargingKey, SessionId, T2, [Bucket2 | Acc2]);
refund(ServiceId, ChargingKey, SessionId,
		#bucket{remain_amount = R} = Bucket1,
		[{TS, Debit, Amount, ServiceId, ChargingKey, SessionId} | T1],
		T2, Acc1, Acc2) ->
	Refunded = {TS, Debit, 0, ServiceId, ChargingKey, SessionId},
	NewReservation = [Refunded | T1],
	Bucket2 = Bucket1#bucket{remain_amount = R + Amount,
			reservations = lists:reverse(Acc1) ++ NewReservation},
	refund(ServiceId, ChargingKey, SessionId, T2, [Bucket2 | Acc2]);
refund(ServiceId, ChargingKey, SessionId, Bucket,
		[H | T1], T2, Acc1, Acc2) ->
	refund(ServiceId, ChargingKey, SessionId, Bucket,
			T1, T2, [H | Acc1], Acc2);
refund(ServiceId, ChargingKey, SessionId, Bucket1, [], T, Acc1, Acc2) ->
	Bucket2 = Bucket1#bucket{reservations = lists:reverse(Acc1)},
	refund(ServiceId, ChargingKey, SessionId, T, [Bucket2 | Acc2]).

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

-spec filter_prices_key(ChargingKey, Prices) -> Prices
	when
		ChargingKey :: non_neg_integer(),
		Prices :: [#price{}].
%% @doc Filter prices with `chargingKey'.
%% @hidden
filter_prices_key(undefined, Prices) ->
	Prices;
filter_prices_key(ChargingKey, Prices) when is_integer(ChargingKey) ->
	filter_prices_key(ChargingKey, Prices, []).
%% @hidden
filter_prices_key(ChargingKey, [#price{char_value_use = CharValueUse} = P | T], Acc) ->
	case lists:keyfind("chargingKey", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = ChargingKey}]} ->
			filter_prices_key(ChargingKey, T, [P | Acc]);
		#char_value_use{values = [#char_value{}]} ->
			filter_prices_key(ChargingKey, T, Acc);
		_ ->
			filter_prices_key(ChargingKey, T, [P | Acc])
	end;
filter_prices_key(_, [], Acc) ->
	lists:reverse(Acc).

-spec get_final(ServiceId, ChargingKey, SessionId, Buckets) -> Result
	when
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		SessionId :: [tuple()],
		Buckets :: [#bucket{}],
		Result :: {Debits, NewBuckets},
		Debits :: #{},
		NewBuckets :: [#bucket{}].
%% @doc Get total debited and remaining amounts, refund and
%% 	remove all reservations, for session.
%% @private
%%
get_final(ServiceId, ChargingKey, SessionId, Buckets) ->
	Now = erlang:system_time(?MILLISECOND),
	get_final(Buckets, ServiceId, ChargingKey, SessionId, Now, #{}, []).
%% @hidden
get_final([#bucket{remain_amount = 0, reservations = []} | T],
		ServiceId, ChargingKey, SessionId, Now, Debits, Acc) ->
	get_final(T, ServiceId, ChargingKey, SessionId, Now, Debits, Acc);
get_final([#bucket{remain_amount = R, reservations = [],
		end_date = Expires} | T], ServiceId, ChargingKey,
		SessionId, Now, Debits, Acc)
		when R >= 0, Expires /= undefined, Expires < Now ->
	get_final(T, ServiceId, ChargingKey, SessionId, Now, Debits, Acc);
get_final([#bucket{reservations = []} = B | T],
		ServiceId, ChargingKey, SessionId, Now, Debits, Acc) ->
	get_final(T, ServiceId, ChargingKey, SessionId, Now, Debits, [B | Acc]);
get_final([#bucket{units = Units, reservations = Reservations,
		remain_amount = R, end_date = Expires} = B | T],
		ServiceId, ChargingKey, SessionId, Now, Debits, Acc) ->
	N = maps:get(Units, Debits, 0),
	case get_debits(ServiceId, ChargingKey, SessionId,
			Reservations, 0, 0, []) of
		{Debit, 0, []} when R == 0 ->
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit}, Acc);
		{Debit, _Refund, []} when R >= 0, Expires /= undefined, Expires < Now ->
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit}, Acc);
		{Debit, Refund, NewReservations} ->
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit},
					[B#bucket{remain_amount = R + Refund,
					last_modified = {Now, erlang:unique_integer([positive])},
					reservations = NewReservations} | Acc])
	end;
get_final([], _, _, _, _, Debits, Acc) ->
	{Debits, lists:reverse(Acc)}.
%% @hidden
get_debits(ServiceId, ChargingKey, SessionId,
		[{_, Debited, Reserved, ServiceId, ChargingKey, SessionId} | T],
		Debit, Refund, Acc) ->
	get_debits(ServiceId, ChargingKey, SessionId,
			T, Debit + Debited, Refund + Reserved, Acc);
get_debits(ServiceId, ChargingKey, SessionId,
		[H | T], Debit, Refund, Acc) ->
	get_debits(ServiceId, ChargingKey, SessionId,
			T, Debit, Refund, [H | Acc]);
get_debits(_, _, _, [], Debit, Refund, Acc) ->
	{Debit, Refund, lists:reverse(Acc)}.

-spec rated(Debits, Rated) -> Result
	when
		Debits :: #{},
		Rated :: #rated{} | [#rated{}],
		Result :: [Rated].
%% @doc Construct rated product usage.
%% @hidden
rated(Debits, #rated{} = Rated) ->
	rated(Debits, [Rated]);
rated(#{}, Rated) ->
	Rated;
rated(Debits, [#rated{} = Rated | T]) ->
	F = fun(cents, Amount, Acc) ->
				[Rated#rated{bucket_type = cents, bucket_value = Amount,
						usage_rating_tag = non_included, is_billed = true,
						tax_excluded_amount = Amount} | Acc];
			(Units, Amount, Acc) ->
				[Rated#rated{bucket_value = Amount,
						usage_rating_tag = included, is_billed = true,
						bucket_type = Units} | Acc]
	end,
	maps:fold(F, T, Debits).

%% @hidden
update_buckets(BRefs, OldB, NewB) ->
	AllNewKeys = [B#bucket.id || B <- NewB],
	UpdatedB = NewB -- OldB,
	DeletedBRefs = BRefs -- AllNewKeys,
	Fdel = fun F([BucketId | T], Acc) ->
				DelBucket = lists:keyfind(BucketId, #bucket.id, OldB),
				F(T, [DelBucket | Acc]);
			F([], Acc) ->
				Acc
	end,
	DeletedBuckets = Fdel(DeletedBRefs, []),
	update_b(UpdatedB),
	ok = delete_b(DeletedBRefs),
	{AllNewKeys, DeletedBuckets}.

%% @hidden
update_b([B | T])	->
	ok = mnesia:write(B),
	update_b(T);
update_b([])	->
	ok.

%% @hidden
delete_b([BRef | T]) ->
	ok = mnesia:delete(bucket, BRef, write),
	delete_b(T);
delete_b([]) ->
	ok.

%% @hidden
roaming_table_prefix(#price{type = tariff, char_value_use = CharValueUse}) ->
	case lists:keyfind("roamingTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = RoamingTable}]} ->
			RoamingTable;
		false ->
			undefined
	end;
roaming_table_prefix(_) ->
	undefined.

%% @private
make_lm() ->
	{erlang:system_time(?MILLISECOND), erlang:unique_integer([positive])}.

%% @private
make_id({TS, N}) ->
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

%% @private
send_notifications([]) ->
	ok;
send_notifications([DeletedBucket | T]) ->
	ocs_event:notify(depleted, DeletedBucket, balance),
	send_notifications(T).

%% @private
notify_accumulated_balance([]) ->
	ok;
notify_accumulated_balance(AccBlance) ->
	ocs_event:notify(accumulated, AccBlance, balance).

%% @private
accumulated_balance(Buckets, ProdRef) ->
	F = fun(UnitThresholdName) ->
			case application:get_env(ocs, UnitThresholdName) of
				{ok, undefined} ->
					false;
				{ok, Threshold} when is_integer(Threshold) ->
					true
			end
	end,
	UnitThresholdNames = [threshold_cents, threshold_bytes,
			threshold_seconds, threshold_messages],
	accumulated_balance(Buckets, ProdRef, lists:any(F, UnitThresholdNames)).
accumulated_balance(_Buckets, _ProdRef, false) ->
	[];
accumulated_balance(Buckets, ProdRef, true) when is_list(Buckets) ->
	Fcents = fun(#bucket{units = cents}) ->
				true;
			(_) ->
				false
	end,
	{CentsBuckets, AccBalance} = case lists:filter(Fcents, Buckets) of
		[] ->
			{[], []};
		BucketList when is_list(BucketList) ->
			AccBal = build_acc(BucketList, "accumulated cents", cents,
					ProdRef, application:get_env(ocs, threshold_cents), []),
			{BucketList, AccBal}
	end,
	Fbytes = fun(#bucket{units = octets}) ->
				true;
			(_) ->
				false
	end,
	Buckets1 = Buckets -- CentsBuckets,
	{BytesBuckets, AccBalance1} = case lists:filter(Fbytes, Buckets1) of
		[] ->
			{[], AccBalance};
		BucketList1 when is_list(BucketList1) ->
			AccBal1 = build_acc(BucketList1, "accumulated octets", octets,
					ProdRef, application:get_env(ocs, threshold_bytes), AccBalance),
			{BucketList1, AccBal1}
	end,
	Fseconds = fun(#bucket{units = seconds}) ->
				true;
			(_) ->
				false
	end,
	Buckets2 = Buckets1 -- BytesBuckets,
	{SecondsBuckets, AccBalance2} = case lists:filter(Fseconds, Buckets2) of
		[] ->
			{[], AccBalance1};
		BucketList2 when is_list(BucketList2) ->
			AccBal2 = build_acc(BucketList2, "accumulated seconds", seconds,
					ProdRef, application:get_env(ocs, threshold_seconds), AccBalance),
			{BucketList2, AccBal2}
	end,
	Fmessages = fun(#bucket{units = messages}) ->
				true;
			(_) ->
				false
	end,
	Buckets3 = Buckets2 -- SecondsBuckets,
	{_MessagesBuckets, AccBalance3} = case lists:filter(Fmessages, Buckets3) of
		[] ->
			{[], AccBalance2};
		BucketList3 when is_list(BucketList3) ->
			AccBal3 = build_acc(BucketList3, "accumulated messages", messages,
					ProdRef, application:get_env(ocs, threshold_messages), AccBalance),
			{BucketList3, AccBal3}
	end,
	AccBalance3.

%% @private
build_acc(_Buckets, _Name, _Units, _ProdRef, {ok, undefined}, AccBalance) ->
	AccBalance;
build_acc(Buckets, Name, Units, ProdRef, {ok, UnitThreshold}, AccBalance)
		when is_integer(UnitThreshold), UnitThreshold > 0 ->
	case lists:sum([RA || #bucket{remain_amount = RA} <- Buckets]) of
		0 ->
			AccBalance;
		TotalBalance when is_integer(TotalBalance),
				TotalBalance < UnitThreshold ->
			BucketRefs = [Id || #bucket{id = Id} <- Buckets],
			[#acc_balance{id = ProdRef, name = Name, product = [ProdRef],
					units = Units, total_balance = TotalBalance,
					bucket = BucketRefs} | AccBalance];
		_ ->
			AccBalance
	end.

