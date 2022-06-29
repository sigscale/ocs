%%% ocs_rating.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-export([rate/13, charge/13]).
-export([authorize/8]).
-export([session_attributes/1]).
-export([filter_prices_tod/2, filter_prices_key/2]).

-include("ocs.hrl").
-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").

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
				| {out_of_credit, RedirectServerAddress, SessionList}
				| {out_of_credit, RedirectServerAddress, SessionList, Rated}
				| {disabled, SessionList}
				| {ok, {pla_ref, Price}}
				| {error, Reason},
		Price :: #price{},
		Service :: #service{},
		GrantedAmount :: {Type, Amount},
		Rated :: [#rated{}],
		SessionList :: [{pos_integer(), [tuple()]}],
		RedirectServerAddress :: string() | undefined,
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
%% 	and `ReserveAmounts' returns `{out_of_credit, RedirectServerAddressAddress, SessionList}' for interim
%% 	updates and `{out_of_credit, RedirectServerAddressAddress, SessionList, Rated}' for final or
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
						[#product{product = OfferId,
								balance = BucketRefs} = Product] ->
							Now = erlang:system_time(millisecond),
							case mnesia:dirty_read(offer, OfferId) of
								[#offer{char_value_use = CharValueUse, end_date = EndDate, start_date = StartDate} = Offer]
										when ((StartDate =< Now) or (StartDate == undefined)), ((EndDate > Now) or ( EndDate == undefined)) ->
									Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write)
											|| Id <- BucketRefs]),
									F2 = fun(#bucket{units = cents, remain_amount = RM}) when RM < 0 ->
												false;
											(_) ->
												true
									end,
									RedirectServerAddress = case lists:keyfind("redirectServer",
											#char_value_use.name, CharValueUse) of
										#char_value_use{values = [#char_value{value = Value}]}
												when is_list(Value) ->
											Value;
										_Other ->
											undefined
									end,
									case lists:all(F2, Buckets) of
										true ->
											{rate1(Protocol, Service, ServiceId, Product, Buckets,
													Timestamp, Address, Direction, Offer,
													Flag, DebitAmounts, ReserveAmounts, ServiceType,
													get_session_id(SessionAttributes), ChargingKey, ServiceNetwork), RedirectServerAddress};
										false ->
											{out_of_credit, RedirectServerAddress, SessionList, [], []}
									end;
								_ ->
									mnesia:abort(offer_not_found)
							end;
						[] ->
							mnesia:abort(product_not_found)
					end;
				[] ->
					mnesia:abort(service_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {{ok, Sub, Rated, DeletedBuckets, AccBalance}, _}}
				when is_list(Rated); Rated == #rated{} ->
			Rated1 = case Rated of
				Rated when is_list(Rated) ->
					Rated;
				#rated{} = Rated ->
					[Rated]
			end,
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Rated1};
		{atomic, {{ok, Sub, Granted, Rated, DeletedBuckets, AccBalance}, _}}
				when is_list(Rated); Rated == #rated{} ->
			Rated1 = case Rated of
				Rated when is_list(Rated) ->
					Rated;
				#rated{} = Rated ->
					[Rated]
			end,
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Granted, Rated1};
		{atomic, {{out_of_credit, SL, Rated,
				DeletedBuckets, AccBalance}, RedirectServerAddress}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, RedirectServerAddress, SL, Rated};
		{atomic, {{grant, Sub, {_Units, Amount} = Granted, DeletedBuckets,
				AccBalance}, _}} when is_integer(Amount) ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Granted};
		{atomic, {{out_of_credit, SL,
				DeletedBuckets, AccBalance}, RedirectServerAddress}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, RedirectServerAddress, SL};
		{atomic, {{disabled, SL, DeletedBuckets, AccBalance}, _}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{disabled, SL};
		{atomic, {out_of_credit, RedirectServerAddress, SL,
				DeletedBuckets, AccBalance}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, RedirectServerAddress, SL};
		{atomic, {{pla_ref, Price}, _}} ->
			{ok,  {pla_ref, Price}};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
rate1(Protocol, Service, ServiceId, Product, Buckets, Timestamp, Address, Direction,
		#offer{specification = undefined, bundle = Bundle}, Flag,
		DebitAmounts, ReserveAmounts, ServiceType, SessionId, ChargingKey, ServiceNetwork) ->
	try
		F = fun(#bundled_po{name = OfferId}, Acc) ->
				case mnesia:dirty_read(offer, OfferId) of
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
		rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp,
				Address, Direction, Offer, Flag, DebitAmounts,
				ReserveAmounts, SessionId, #rated{product = OfferName},
				ChargingKey, ServiceNetwork)
	catch
		_:_ ->
			mnesia:abort(invalid_bundle_product)
	end;
rate1(Protocol, Service, ServiceId, Product, Buckets,
		Timestamp, Address, Direction,
		#offer{name = OfferName, specification = Spec, status = Status} = Offer,
		Flag, DebitAmounts, ReserveAmounts, ServiceType, SessionId,
		ChargingKey, ServiceNetwork) when
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
	rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp, Address,
			Direction, Offer, Flag, DebitAmounts, ReserveAmounts,
			SessionId, #rated{product = OfferName},
			ChargingKey, ServiceNetwork);
rate1(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
	mnesia:abort(invalid_service_type).
%% @hidden
rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp, Address, Direction,
		#offer{specification = ProdSpec, price = Prices} = _Offer,
		Flag, DebitAmounts, ReserveAmounts, SessionId, Rated,
		ChargingKey, ServiceNetwork)
		when ProdSpec == "10"; ProdSpec == "11" ->
	Now = erlang:system_time(millisecond),
	F = fun(#price{type = usage, units = messages,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = usage, units = cents,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = tariff, units = messages,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = #pla_ref{},
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	case filter_prices_dir(Direction, FilteredPrices3) of
		[#price{type = #pla_ref{}} = Price | _] ->
			 {pla_ref, Price};
		[#price{units = Units, type = PriceType, size = UnitSize, amount = UnitPrice,
				currency = Currency, char_value_use = PriceChars} = Price | _] ->
			RoamingTable = roaming_table_prefix(Price),
			rate3(Protocol, Service, ServiceId, Product, Buckets, Address,
					PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
					Flag, DebitAmounts, ReserveAmounts,
					SessionId, RoamingTable, Rated,
					ChargingKey, ServiceNetwork);
		_ ->
			mnesia:abort(price_not_found)
	end;
rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp, Address, Direction,
		#offer{specification = ProdSpec, price = Prices} = _Offer,
		Flag, DebitAmounts, ReserveAmounts,
		SessionId, Rated, ChargingKey, ServiceNetwork)
		when ProdSpec == "5"; ProdSpec == "9" ->
	Now = erlang:system_time(millisecond),
	F = fun(#price{type = tariff, units = seconds,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = usage, units = seconds,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = #pla_ref{},
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	case filter_prices_dir(Direction, FilteredPrices3) of
		[#price{type = #pla_ref{}} = Price | _] ->
			 {pla_ref, Price};
		[#price{units = Units, type = PriceType, size = UnitSize, amount = UnitPrice,
				currency = Currency, char_value_use = PriceChars} = Price | _] ->
			RoamingTable = roaming_table_prefix(Price),
			rate3(Protocol, Service, ServiceId, Product, Buckets, Address,
					PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
					Flag, DebitAmounts, ReserveAmounts,
					SessionId, RoamingTable,
					Rated, ChargingKey, ServiceNetwork);
		_ ->
			mnesia:abort(price_not_found)
	end;
rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp, _Address, _Direction,
		#offer{price = Prices} = _Offer, Flag, DebitAmounts, ReserveAmounts,
		SessionId, Rated, ChargingKey, ServiceNetwork) ->
	Now = erlang:system_time(millisecond),
	F = fun(#price{type = tariff, units = octets,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = usage,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(#price{type = #pla_ref{},
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or ( EndDate == undefined)) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	case filter_prices_tod(Timestamp, FilteredPrices2) of
		[#price{type = #pla_ref{}} = Price | _] ->
			 {pla_ref, Price};
		[#price{units = Units, type = PriceType, size = UnitSize, amount = UnitPrice,
				currency = Currency, char_value_use = PriceChars} = Price| _] ->
			RoamingTable = roaming_table_prefix(Price),
			rate4(Protocol, Service, ServiceId, Product, Buckets,
					PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
					Flag, DebitAmounts, ReserveAmounts,
					SessionId, RoamingTable, Rated, ChargingKey, ServiceNetwork);
		_ ->
			mnesia:abort(price_not_found)
	end.
%% @hidden
rate3(Protocol, Service, ServiceId, Product, Buckets, Address,
		tariff = PriceType, UnitSize, Units, Currency, _UnitPrice, PriceChars,
		Flag, DebitAmounts, ReserveAmounts, SessionId,
		RoamingTable, Rated, ChargingKey, ServiceNetwork) ->
	case lists:keyfind("destPrefixTariffTable", #char_value_use.name, PriceChars) of
		#char_value_use{values = [#char_value{value = TariffTable}]}
				when RoamingTable == undefined ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{Description, Amount, _} when is_integer(Amount) ->
					case Amount of
						N when N >= 0 ->
							charge1(Protocol, Service, ServiceId, Product, Buckets,
									PriceType, UnitSize, Units, Currency, N, PriceChars,
									Flag, DebitAmounts, ReserveAmounts,
									SessionId, Rated#rated{price_type = tariff,
											description = Description}, ChargingKey);
						_N ->
							mnesia:abort(negative_amount)
					end;
				Other ->
					mnesia:abort(table_lookup_failed)
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
										charge1(Protocol, Service, ServiceId, Product, Buckets,
												PriceType, UnitSize, Units, Currency, N, PriceChars,
												Flag, DebitAmounts, ReserveAmounts,
												SessionId, Rated#rated{price_type = tariff,
												description = Description1}, ChargingKey);
									_N ->
										mnesia:abort(negative_amount)
								end;
							Other ->
								mnesia:abort(table_lookup_failed)
						end;
				Other ->
					error_logger:error_report(["Service Network table lookup failed",
							{module, ?MODULE}, {table, Table1},
							{address, Address}, {result, Other}]),
					mnesia:abort(table_lookup_failed)
			end;
		false ->
			mnesia:abort(undefined_tariff)
	end;
rate3(Protocol, Service, ServiceId, Product, Buckets, _Address,
		PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
		Flag, DebitAmounts, ReserveAmounts, SessionId,
		_RoamingTable, Rated, ChargingKey, _ServiceNetwork) ->
	charge1(Protocol, Service, ServiceId, Product, Buckets,
			PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
			Flag, DebitAmounts, ReserveAmounts, SessionId, Rated, ChargingKey).
%% @hidden
rate4(Protocol, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, _UnitPrice, PriceChars,
		Flag, DebitAmounts, ReserveAmounts,
		SessionId, RoamingTable, Rated, ChargingKey, ServiceNetwork)
		when is_list(RoamingTable), is_list(ServiceNetwork) ->
	Table = list_to_existing_atom(RoamingTable),
	case catch ocs_gtt:lookup_last(Table, ServiceNetwork) of
		{Description, Amount, _} when is_integer(Amount) ->
			case Amount of
				N when N >= 0 ->
					charge1(Protocol, Service, ServiceId, Product, Buckets,
							PriceType, UnitSize, Units, Currency, N, PriceChars,
							Flag, DebitAmounts, ReserveAmounts,
							SessionId, Rated#rated{price_type = tariff,
							description = Description}, ChargingKey);
				_N ->
					mnesia:abort(negative_amount)
			end;
		Other ->
			mnesia:abort(table_lookup_failed)
	end;
rate4(Protocol, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
		Flag, DebitAmounts, ReserveAmounts, SessionId,
		_RoamingTable, Rated, ChargingKey, _ServiceNetwork) ->
	charge1(Protocol, Service, ServiceId, Product, Buckets,
			PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
			Flag, DebitAmounts, ReserveAmounts, SessionId, Rated, ChargingKey).

-spec charge(Protocol, SubscriberID, ServiceId,
		UnitSize, Units, Currency, UnitPrice, PriceChars,
		ChargingKey, Flag, DebitAmounts,
		ReserveAmounts, SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		SubscriberID :: binary(),
		ServiceId :: integer() | undefined,
		ChargingKey :: integer() | undefined,
		UnitSize :: integer() | undefined,
		Currency :: string() | integer(),
		UnitPrice :: integer() | undefined,
		PriceChars :: #char_value_use{} | undefined,
		Units :: octets | seconds | messages,
		Flag :: initial | interim | final | event,
		DebitAmounts :: list(),
		ReserveAmounts :: list() | undefined,
		SessionAttributes :: list(),
		Result :: {ok, Service, GrantedAmount}
				| {ok, Service, Rated}
				| {ok, Service, GrantedAmount, Rated}
				| {out_of_credit, RedirectServerAddress, SessionList}
				| {out_of_credit, RedirectServerAddress, SessionList, Rated}
				| {disabled, SessionList}
				| {error, Reason},
		Service :: #service{},
		GrantedAmount :: {Units, Amount},
		Amount :: integer(),
		Rated :: [#rated{}],
		SessionList :: [{pos_integer(), [tuple()]}],
		RedirectServerAddress :: string() | undefined,
		Reason :: term().
%% @doc Handle balance management for used and reserved unit amounts.
charge(Protocol, SubscriberID, ServiceId,
		UnitSize, Units, Currency, UnitPrice, PriceChars,
		ChargingKey, Flag, DebitAmounts,
		ReserveAmounts, SessionAttributes)
		when ((Protocol == radius) or (Protocol == diameter)),
		is_binary(SubscriberID),
		(is_integer(ChargingKey) or (ChargingKey == undefined)),
		(is_integer(ServiceId) or (ServiceId== undefined)),
		(is_integer(UnitSize) or (UnitSize == undefined)),
		is_integer(UnitPrice),
		(is_atom(Units) or (Units == undefined)),
		(is_list(Currency) or is_integer(Currency)),
		((Flag == initial) or (Flag == interim) or (Flag == final) or (Flag == event)),
		is_list(DebitAmounts),
		(is_list(ReserveAmounts) or (ReserveAmounts == undefined)),
		length(SessionAttributes) > 0 ->
	F = fun() ->
		case mnesia:read(service, SubscriberID, read) of
			[#service{product = ProdRef, session_attributes = SessionList} = Service] ->
				case mnesia:read(product, ProdRef, read) of
					[#product{product = OfferId, balance = BucketRefs} = Product] ->
						case mnesia:read(offer, OfferId, read) of
							[#offer{char_value_use = CharValueUse, name = OfferName} = _Offer] ->
								Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write)
										|| Id <- BucketRefs]),
								F1 = fun(#bucket{units = cents, remain_amount = RM}) when RM < 0 ->
										false;
									(_) ->
										true
								end,
								RedirectServerAddress = case lists:keyfind("redirectServer",
										#char_value_use.name, CharValueUse) of
									#char_value_use{values = [#char_value{value = Value}]}
											when is_list(Value) ->
										Value;
									_Other ->
										undefined
								end,
								case lists:all(F1, Buckets) of
									true ->
										Rated = #rated{product = OfferName, price_type = tariff},
										{charge1(Protocol, Service, ServiceId, Product, Buckets,
												tariff, UnitSize, Units, Currency, UnitPrice, PriceChars,
												Flag, DebitAmounts, ReserveAmounts, get_session_id(SessionAttributes),
												Rated, ChargingKey), RedirectServerAddress};
									false ->
										{out_of_credit, RedirectServerAddress, SessionList, [], []}
								end;
							[] ->
								mnesia:abort(offer_not_found)
						end;
					[] ->
						mnesia:abort(product_not_found)
				end;
			[] ->
				mnesia:abort(service_not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, {{ok, Sub, Rated, DeletedBuckets, AccBalance}, _}}
				when is_list(Rated); Rated == #rated{} ->
			Rated1 = case Rated of
				Rated when is_list(Rated) ->
					Rated;
				#rated{} = Rated ->
					[Rated]
			end,
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Rated1};
		{atomic, {{ok, Sub, Granted, Rated, DeletedBuckets, AccBalance}, _}}
				when is_list(Rated); Rated == #rated{} ->
			Rated1 = case Rated of
				Rated when is_list(Rated) ->
					Rated;
				#rated{} = Rated ->
					[Rated]
			end,
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Granted, Rated1};
		{atomic, {{out_of_credit, SL, Rated,
				DeletedBuckets, AccBalance}, RedirectServerAddress}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, RedirectServerAddress, SL, Rated};
		{atomic, {{grant, Sub, {_Units, Amount} = Granted, DeletedBuckets,
				AccBalance}, _}} when is_integer(Amount) ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{ok, Sub, Granted};
		{atomic, {{out_of_credit, SL,
				DeletedBuckets, AccBalance}, RedirectServerAddress}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, RedirectServerAddress, SL};
		{atomic, {{disabled, SL, DeletedBuckets, AccBalance}, _}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{disabled, SL};
		{atomic, {out_of_credit, RedirectServerAddress, SL,
				DeletedBuckets, AccBalance}} ->
			ok = send_notifications(DeletedBuckets),
			ok = notify_accumulated_balance(AccBalance),
			{out_of_credit, RedirectServerAddress, SL};
		{aborted, Reason} ->
			{error, Reason}
	end.

%% @hidden
%% @private
charge1(_Protocol, #service{enabled = false} = Service, ServiceId, Product,
		Buckets, _PriceType, _UnitSize, Units, _Currency, _UnitPrice, _PriceChars, initial,
		_DebitAmounts, _ReserveAmounts, SessionId, Rated, ChargingKey) ->
	charge3(Service, ServiceId, Product, Buckets, initial,
			{Units, 0}, {Units, 0}, {Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey, Buckets);
charge1(radius, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
		initial, [], [], SessionId, Rated, ChargingKey) ->
	charge2(Service, ServiceId, Product, Buckets,
			PriceType, UnitSize, Units, Currency, UnitPrice, initial, {Units, 0},
			get_reserve(#price{units = Units, char_value_use = PriceChars}),
			SessionId, Rated, ChargingKey);
charge1(radius, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, PriceChars,
		interim, DebitAmounts, ReserveAmounts, SessionId, Rated, ChargingKey) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, 0}
	end,
	ReserveAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{_, ReserveUnits} ->
			{Units, Amount} = get_reserve(#price{units = Units,
					char_value_use = PriceChars}),
			{Units, ReserveUnits + Amount};
		false ->
			get_reserve(#price{units = Units,
					char_value_use = PriceChars})
	end,
	charge2(Service, ServiceId, Product, Buckets, PriceType,
			UnitSize, Units, Currency, UnitPrice, interim, DebitAmount, ReserveAmount,
			SessionId, Rated, ChargingKey);
charge1(_Protocol, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, _PriceChars,
		Flag, DebitAmounts, [], SessionId, Rated, ChargingKey)
		when ((Flag == initial) or (Flag == interim)) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} when UnitPrice > 0 ->
			{Units, DebitUnits};
		_ ->
			{Units, 0}
	end,
	ReserveAmount = case Units of
		octets ->
			case application:get_env(ocs, min_reserve_octets) of
				{ok, Value} when Value < UnitSize ->
					{Units, UnitSize};
				{ok, Value} ->
					{Units, Value}
			end;
		seconds ->
			case application:get_env(ocs,min_reserve_seconds) of
				{ok, Value} when Value < UnitSize ->
					{Units, UnitSize};
				{ok, Value} ->
					{Units, Value}
			end;
		messages ->
			case application:get_env(ocs, min_reserve_messages) of
				{ok, Value} when Value < UnitSize ->
					{Units, UnitSize};
				{ok, Value} ->
					{Units, Value}
			end
	end,
	charge2(Service, ServiceId, Product, Buckets, PriceType,
			UnitSize, Units, Currency, UnitPrice, Flag, DebitAmount,
			ReserveAmount, SessionId, Rated, ChargingKey);
charge1(_Protocol, Service, ServiceId, Product, Buckets, PriceType,
		UnitSize, Units, Currency, UnitPrice, _PriceChars,
		event, _DebitAmounts, undefined, SessionId, Rated, ChargingKey) ->
	DebitAmount = {Units, UnitSize},
	ReserveAmount = {Units, 0},
	charge2(Service, ServiceId, Product, Buckets, PriceType, UnitSize,
			 Units, Currency, UnitPrice, event, DebitAmount, ReserveAmount,
			SessionId, Rated, ChargingKey);
charge1(_Protocol, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, _PriceChars,
		event, _DebitAmounts, ReserveAmounts, SessionId, Rated, ChargingKey) ->
	DebitAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{Units, DebitUnits} ->
			{Units, DebitUnits};
		false ->
			{Units, UnitSize}
	end,
	ReserveAmount = {Units, 0},
	charge2(Service, ServiceId, Product, Buckets, PriceType,
			UnitSize, Units, Currency, UnitPrice, event, DebitAmount, ReserveAmount,
			SessionId, Rated, ChargingKey);
charge1(_Protocol, Service, ServiceId, Product, Buckets, PriceType,
		UnitSize, Units, Currency, UnitPrice, _PriceChars,
		Flag, DebitAmounts, undefined, SessionId, Rated, ChargingKey) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} when UnitPrice > 0 ->
			{Units, DebitUnits};
		_ ->
			{Units, 0}
	end,
	ReserveAmount = {Units, 0},
	charge2(Service, ServiceId, Product, Buckets, PriceType,
			UnitSize, Units, Currency, UnitPrice, Flag, DebitAmount, ReserveAmount,
			SessionId, Rated, ChargingKey);
charge1(_Protocol, Service, ServiceId, Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, _PriceChars,
		Flag, DebitAmounts, ReserveAmounts, SessionId, Rated, ChargingKey)
		when is_list(ReserveAmounts) ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{Units, DebitUnits} when UnitPrice > 0 ->
			{Units, DebitUnits};
		_ ->
			{Units, 0}
	end,
	ReserveAmount = case lists:keyfind(Units, 1, ReserveAmounts) of
		{Units, ReserveUnits} ->
			{Units, ReserveUnits};
		false ->
			{Units, 0}
	end,
	charge2(Service, ServiceId, Product, Buckets, PriceType,
			UnitSize, Units, Currency, UnitPrice, Flag, DebitAmount, ReserveAmount,
			SessionId, Rated, ChargingKey).
%% @hidden
charge2(Service, ServiceId, Product, Buckets,
		_PriceType, UnitSize, Units, _Currency, UnitPrice,
		initial, {_, 0}, {Units, Amount} = ReserveAmount,
		SessionId, Rated, ChargingKey) ->
	case update_session(Units, 0, Amount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{0, UnitsReserved, Buckets2} when UnitsReserved >= Amount ->
			charge3(Service, ServiceId, Product, Buckets2, initial,
					{Units, 0}, {Units, 0}, ReserveAmount,
					{Units, UnitsReserved}, SessionId, Rated, ChargingKey, Buckets);
		{0, UnitsReserved, Buckets2} when UnitsReserved < Amount ->
			PriceReserveUnits = (Amount - UnitsReserved),
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case convert(PriceReserve, Units, UnitReserve,
					ServiceId, ChargingKey, SessionId, Buckets2) of
				{ok, Buckets3} ->
					{0, UnitReserve, Buckets4} = update_session(Units, 0,
							UnitReserve, ServiceId, ChargingKey, SessionId, Buckets3),
					charge3(Service, ServiceId, Product, Buckets4, initial,
							{Units, 0}, {Units, 0}, ReserveAmount,
							{Units, UnitsReserved + UnitReserve}, SessionId, Rated, ChargingKey, Buckets);
				false ->
					charge3(Service, ServiceId, Product, Buckets2, initial,
							{Units, 0}, {Units, 0}, ReserveAmount,
							{Units, UnitsReserved}, SessionId, Rated, ChargingKey, Buckets)
			end
	end;
charge2(#service{enabled = false} = Service, ServiceId, #product{id = ProductId} = Product,
		Buckets, _PriceType, UnitSize, Units, _Currency, UnitPrice,
		interim, {Units, Amount} = DebitAmount, _ReserveAmount,
		SessionId, Rated, ChargingKey) ->
	case update_session(Units, Amount, 0,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Amount, 0, Buckets2} ->
			charge3(Service, ServiceId, Product, Buckets2, interim,
					DebitAmount, DebitAmount, {Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey, Buckets);
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
							charge3(Service, ServiceId, Product, Buckets4, interim,
									DebitAmount, {Units, UnitsCharged},
									{Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey, Buckets);
						{NewUnitsCharged, 0, Buckets4}
								when NewUnitsCharged < NewChargeUnits->
							Now = erlang:system_time(millisecond),
							Reservation = #{ts => Now, debit => 0,
									reserve => NewUnitsCharged - NewChargeUnits,
									service_id => ServiceId, charging_key => ChargingKey},
							Attributes = #{bucket_type => session,
									reservations => #{SessionId => Reservation}},
							LM = make_lm(),
							Buckets5 = [#bucket{id = make_id(LM), last_modified = LM,
									start_date = Now, end_date = Now,
									remain_amount = 0, attributes = Attributes,
									units = Units, product = [ProductId]} | Buckets4],
							charge3(Service, ServiceId, Product, Buckets5, interim,
									DebitAmount, {Units, UnitsCharged + NewUnitsCharged},
									{Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey, Buckets)
					end;
				false ->
					Now = erlang:system_time(millisecond),
					Reservation = #{ts => Now, debit => 0,
							reserve => NewChargeUnits - Amount,
							service_id => ServiceId, charging_key => ChargingKey},
					Attributes = #{bucket_type => session,
							reservations => #{SessionId => Reservation}},
					LM = make_lm(),
					Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now, end_date = Now,
							remain_amount = 0, attributes = Attributes,
							units = Units, product = [ProductId]} | Buckets2],
					charge3(Service, ServiceId, Product, Buckets4, interim,
							DebitAmount, {Units, UnitsCharged},
							{Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey, Buckets)
			end
	end;
charge2(Service, ServiceId, #product{id = ProductId} = Product, Buckets,
		_PriceType, UnitSize, Units, _Currency, UnitPrice,
		interim, {Units, Damount} = DebitAmount, {Units, Ramount} = ReserveAmount,
		SessionId, Rated, ChargingKey) ->
	case update_session(Units, Damount, Ramount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Damount, UnitsReserved, Buckets2} when UnitsReserved >= Ramount ->
			charge3(Service, ServiceId, Product, Buckets2, interim, DebitAmount, DebitAmount,
					ReserveAmount, {Units, UnitsReserved}, SessionId, Rated, ChargingKey, Buckets);
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
					charge3(Service, ServiceId, Product, Buckets4, interim,
							DebitAmount, DebitAmount, ReserveAmount,
							{Units, UnitsReserved + UnitReserve}, SessionId, Rated, ChargingKey, Buckets);
				false ->
					charge3(Service, ServiceId, Product, Buckets2, interim, DebitAmount, DebitAmount,
							ReserveAmount, {Units, UnitsReserved}, SessionId, Rated, ChargingKey, Buckets)
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
							charge3(Service, ServiceId, Product, Buckets4, interim, DebitAmount,
									{Units, UnitsCharged + NewChargeUnits},
									ReserveAmount, {Units, UnitsReserved}, SessionId, Rated, ChargingKey, Buckets);
						{NewUnitsCharged, 0, Buckets4}
								when NewUnitsCharged < NewChargeUnits ->
							Now = erlang:system_time(millisecond),
							Reservation = #{ts => Now, debit => NewUnitsCharged,
									reserve => 0, service_id => ServiceId,
									charging_key => ChargingKey},
							Attributes = #{bucket_type => session,
									reservations => #{SessionId => Reservation}},
							LM = make_lm(),
							Buckets5 = [#bucket{id = make_id(LM), last_modified = LM,
									start_date = Now, end_date = Now,
									remain_amount = NewUnitsCharged - NewChargeUnits,
									attributes = Attributes, units = Units,
									product = [ProductId]} | Buckets4],
							charge3(Service, ServiceId, Product, Buckets5, interim, DebitAmount,
									{Units, UnitsCharged + NewUnitsCharged},
									ReserveAmount, {Units, 0}, SessionId, Rated, ChargingKey, Buckets)
					end;
				false ->
					Now = erlang:system_time(millisecond),
					Reservation = #{ts => Now, debit => UnitsCharged, reserve => 0,
							service_id => ServiceId, charging_key => ChargingKey},
					Attributes = #{bucket_type => session,
							reservations => #{SessionId => Reservation}},
					LM = make_lm(),
					Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now, end_date = Now,
							remain_amount = UnitsCharged - Damount,
							attributes = Attributes, units = Units,
							product = [ProductId]} | Buckets2],
					charge3(Service, ServiceId, Product, Buckets4, interim,
							DebitAmount, {Units, Damount - UnitsCharged},
							ReserveAmount, {Units, 0}, SessionId, Rated, ChargingKey, Buckets)
			end
	end;
charge2(Service, ServiceId, #product{id = ProductId} = Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, final,
		{Units, Amount} = DebitAmount, {Units, 0} = ReserveAmount,
		SessionId, Rated, ChargingKey) ->
	Rated2 = Rated#rated{price_type = PriceType, currency = Currency},
	case charge_session(Units, Amount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Amount, Buckets2} ->
			{Debits, Buckets3} = get_final(ServiceId,
					ChargingKey, SessionId, Buckets2),
			Rated3 = rated(Debits, Rated2),
			charge3(Service, ServiceId, Product, Buckets3, final, DebitAmount, DebitAmount,
					ReserveAmount, ReserveAmount, SessionId, Rated3, ChargingKey, Buckets);
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
					charge3(Service, ServiceId, Product, Buckets4, final, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, SessionId, Rated3, ChargingKey, Buckets);
				{PriceCharged, Buckets3}  when PriceCharged < PriceCharge ->
					TotalUnits = UnitsCharged + (PriceCharged div UnitPrice),
					{Debits, Buckets4} = get_final(ServiceId,
							ChargingKey, SessionId, Buckets3),
					Rated3 = rated(Debits, Rated2),
					Now = erlang:system_time(millisecond),
					Reservation = #{ts => Now, debit => UnitsCharged, reserve => 0,
							service_id => ServiceId, charging_key => ChargingKey},
					Attributes = #{bucket_type => session,
							reservations => #{SessionId => Reservation}},
					LM = make_lm(),
					Buckets5 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now,
							remain_amount = PriceCharged - PriceCharge,
							attributes = Attributes, units = cents,
							product = [ProductId]} | Buckets4],
					charge3(Service, ServiceId, Product, Buckets5, final, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, SessionId, Rated3, ChargingKey, Buckets)
			end
	end;
charge2(Service, ServiceId, #product{id = ProductId} = Product, Buckets,
		PriceType, UnitSize, Units, Currency, UnitPrice, event,
		{Units, Amount} = DebitAmount, {Units, 0} = ReserveAmount,
		SessionId, Rated, ChargingKey) ->
	Rated2 = Rated#rated{price_type = PriceType, currency = Currency},
	case charge_event(Units, Amount, Buckets) of
		{Amount, Buckets2} ->
			Rated3 = rated(#{Units => Amount}, Rated2),
			charge3(Service, ServiceId, Product, Buckets2, event, DebitAmount, DebitAmount,
					ReserveAmount, ReserveAmount, SessionId, Rated3, ChargingKey, Buckets);
		{UnitsCharged, Buckets2} when UnitsCharged < Amount ->
			{UnitCharge, PriceCharge} = price_units(Amount - UnitsCharged,
					UnitSize, UnitPrice),
			case charge_event(cents, PriceCharge, Buckets2) of
				{PriceCharge, Buckets3} ->
					TotalUnits = UnitsCharged + UnitCharge,
					Rated3 = rated(#{Units => Amount, cents => PriceCharge}, Rated2),
					charge3(Service, ServiceId, Product, Buckets3, event, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, SessionId, Rated3, ChargingKey, Buckets);
				{PriceCharged, Buckets3}  when PriceCharged < PriceCharge ->
					TotalUnits = UnitsCharged + (PriceCharged div UnitPrice),
					Rated3 = rated(#{Units => Amount, cents => PriceCharged}, Rated2),
					Now = erlang:system_time(millisecond),
					Reservation = #{ts => Now, debit => UnitsCharged, reserve => 0,
							service_id => ServiceId, charging_key => ChargingKey},
					Attributes = #{bucket_type => session,
							reservations => #{SessionId => Reservation}},
					LM = make_lm(),
					Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now,
							remain_amount = PriceCharged - PriceCharge,
							attributes = Attributes, units = cents,
							product = [ProductId]} | Buckets3],
					charge3(Service, ServiceId, Product, Buckets4, event, DebitAmount, {Units, TotalUnits},
							ReserveAmount, ReserveAmount, SessionId, Rated3, ChargingKey, Buckets)
			end
	end.
%% @hidden
charge3(#service{session_attributes = SessionList} = Service1, ServiceId,
		Product, Buckets, final, {Units, Charge}, {Units, Charged},
		{Units, 0}, {Units, 0}, SessionId,
		Rated, ChargingKey, OldBuckets) when Charged >= Charge ->
	{Debits, NewBuckets} = get_final(ServiceId,
			ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Rated1 = rated(Debits, Rated),
	NewSessionList = remove_session(SessionId, SessionList),
	Service2 = Service1#service{session_attributes = NewSessionList},
	ok = mnesia:write(Service2),
	{ok, Service2, Rated1, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge3(#service{session_attributes = SessionList} = Service1, ServiceId, Product, Buckets, final,
		{Units, _Charge}, {Units, _Charged}, {Units, 0}, {Units, 0},
		SessionId, Rated, ChargingKey, OldBuckets) ->
	{Debits, NewBuckets} = get_final(ServiceId,
			ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Rated1 = rated(Debits, Rated),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, Rated1, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge3(#service{enabled = false, session_attributes = SessionList} = Service1,
		ServiceId, Product, Buckets, _Flag, {Units, _Charge}, {Units, _Charged},
		{Units, _Reserve}, {Units, _Reserved},
		SessionId, _Rated, ChargingKey, OldBuckets) ->
	NewBuckets = refund(ServiceId, ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{disabled, SessionList, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge3(#service{session_attributes = SessionList} = Service1, ServiceId,
		Product, Buckets, _Flag, {Units, Charge}, {Units, Charged},
		{Units, Reserve}, {Units, Reserved},
		SessionId, _Rated, ChargingKey, OldBuckets)
		when Charged < Charge; Reserved <  Reserve ->
	NewBuckets = refund(ServiceId, ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge3(#service{session_attributes = SessionList} = Service1,
		_ServiceId, Product, Buckets, initial,
		{Units, 0}, {Units, 0}, {Units, _Reserve}, {Units, Reserved},
		SessionId, _Rated,  _ChargingKey, OldBuckets) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	case add_session(SessionId, SessionList) of
		SessionList ->
			{grant, Service1, {Units, Reserved}, DeletedBuckets,
					accumulated_balance(Buckets, Product#product.id)};
		NewSessionList ->
			Service2 = Service1#service{session_attributes = NewSessionList},
			ok = mnesia:write(Service2),
			{grant, Service2, {Units, Reserved}, DeletedBuckets,
					accumulated_balance(Buckets, Product#product.id)}
	end;
charge3(Service, _ServiceId, Product, Buckets, interim, {Units, _Charge}, {Units, _Charged},
		{Units, _Reserve}, {Units, Reserved},
		_SessionId, _Rated, _ChargingKey, OldBuckets) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	ok = mnesia:write(Service),
	{grant, Service, {Units, Reserved}, DeletedBuckets,
			accumulated_balance(Buckets, Product#product.id)};
charge3(Service, _ServiceId, Product, Buckets, event,
		{Units, Charge}, {Units, Charged}, {Units, 0}, {Units, 0},
		_SessionId, Rated, _ChargingKey, OldBuckets)
		when Charged >= Charge ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	ok = mnesia:write(Service),
	{ok, Service, {Units, Charged}, Rated, DeletedBuckets,
			accumulated_balance(Buckets, Product#product.id)};
charge3(#service{session_attributes = SessionList} = Service1,
		_ServiceId, Product, Buckets, event,
		{Units, _Charge}, {Units, _Charged}, {Units, 0}, {Units, 0},
		_SessionId, Rated, _ChargingKey, OldBuckets) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, Rated, DeletedBuckets,
			accumulated_balance(Buckets, Product#product.id)}.

-spec authorize(Protocol, ServiceType, SubscriberId, Password,
		Timestamp, Address, Direction, SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		ServiceType :: binary() | char() | undefined,
		SubscriberId :: binary() | string(),
		Password :: binary() | string() | {ChapId :: 0..255,
				ChapPassword :: binary(), Challenge :: binary()},
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
				[#service{password = Password1} = S] when is_tuple(Password) ->
					{ChapId, ChapPassword, Challenge} = Password,
					case crypto:hash(md5, [ChapId, Password1, Challenge]) of
						ChapPassword ->
							authorize1(Protocol, ServiceType, S, Timestamp,
									Address, Direction, SessionAttributes);
						_Other ->
							mnesia:abort(bad_password)
					end;
				[#service{password = MTPassword} = S] when
						((Password == <<>>) and (Password =/= MTPassword)) orelse
						(Password == MTPassword) ->
					authorize1(Protocol, ServiceType, S, Timestamp,
							Address, Direction, SessionAttributes);
				[#service{}] ->
					mnesia:abort(bad_password);
				[] ->
					mnesia:abort(service_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {authorized, Sub, Attr, SSA}} ->
			{authorized, Sub, Attr, SSA};
		{atomic, {unauthorized, Reason, SSA}} ->
			{unauthorized, Reason, SSA};
		{aborted, Reason} ->
			{unauthorized, Reason, []}
	end.
%% @hidden
authorize1(radius, ServiceType,
		#service{attributes = Attributes, product = ProdRef} = Service,
		Timestamp, Address, Direction, SessionAttributes) ->
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
					case mnesia:read(offer, OfferId, read) of
						[#offer{char_value_use = CharValueUse} = Offer] ->
							F2 = fun(#char_value_use{name = "radiusReserveSessionTime",
											values = [#char_value{value = RRST}]})
											when is_integer(RRST) ->
										{true, {seconds, RRST}};
									(#char_value_use{name = "radiusReserveSessionOctets",
											values = [#char_value{value = RRSO}]})
											when is_integer(RRSO) ->
										{true, {octets, RRSO}};
									(_) ->
										false
							end,
							case lists:filtermap(F2, CharValueUse) of
								[{ReserveUnits, Reserve}] ->
									authorize2(radius, ServiceType, Service, Buckets,
										Offer, Timestamp, Address, Direction,
										SessionAttributes, Reserve, ReserveUnits);
								[] ->
									authorize5(Service, Buckets, ServiceType,
											SessionAttributes, Attributes)
							end;
						[] ->
							mnesia:abort(offer_not_found)
					end;
				false ->
					authorize5(Service, Buckets, ServiceType,
							SessionAttributes, Attributes)
			end;
		[] ->
			mnesia:abort(product_not_found)
	end;
authorize1(diameter, ServiceType,
		#service{attributes = Attributes, product = ProdRef} =
		Service, _Timestamp, _Address, _Direction, SessionAttributes) ->
	case mnesia:dirty_read(product, ProdRef) of
		[#product{balance = BucketRefs}] ->
			Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write) || Id <- BucketRefs]),
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes);
		[] ->
			mnesia:abort(product_not_found)
	end.
%% @hidden
authorize2(radius = Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets, Timestamp,
		#offer{specification = undefined, bundle = Bundle}, Address, Direction,
		SessionAttributes, Reserve, ReserveUnits) when Reserve > 0, Bundle /= [] ->
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
						Timestamp, Address, Direction, SessionAttributes,
						Reserve, ReserveUnits);
			[] ->
				authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
		end
	catch
		_:_ ->
			mnesia:abort(invalid_bundle_product)
	end;
authorize2(radius = Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets,
		#offer{specification = ProdSpec, price = Prices}, Timestamp,
		Address, Direction, SessionAttributes, Reserve, ReserveUnits)
		when (Reserve > 0) and ((ProdSpec == "9") orelse (ProdSpec == "5")) ->
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
					Price, SessionAttributes, Reserve, ReserveUnits);
		_ ->
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
	end;
authorize2(radius = Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets,
		#offer{specification = ProdSpec, price = Prices}, Timestamp,
		_Address, _Direction, SessionAttributes, Reserve, ReserveUnits)
		when (Reserve > 0) and ((ProdSpec == "8") orelse (ProdSpec == "4")) ->
	F = fun(#price{type = usage, units = Units}) when Units == ReserveUnits ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	case filter_prices_tod(Timestamp, FilteredPrices1) of
		[Price | _] ->
			authorize4(Protocol, ServiceType, Service,
					Buckets, Price, SessionAttributes, Reserve, ReserveUnits);
		_ ->
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes)
	end;
authorize2(_Protocol, ServiceType,
		#service{attributes = Attributes} = Service, Buckets, _Offer, _Timestamp,
		_Address, _Direction, SessionAttributes, _Reserve, _ReserveUnits) ->
	authorize5(Service, Buckets, ServiceType, SessionAttributes, Attributes).
%% @hidden
authorize3(Protocol, ServiceType, Service, Buckets, Address,
		#price{type = tariff, char_value_use = CharValueUse} = Price,
		SessionAttributes, Reserve, ReserveUnits) ->
	case lists:keyfind("destPrefixTariffTable", #char_value_use.name, CharValueUse) of
		#char_value_use{values = [#char_value{value = TariffTable}]} ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{_Description, Amount, _} when is_integer(Amount) ->
					case Amount of
						N when N >= 0 ->
							authorize4(Protocol, ServiceType, Service, Buckets,
									Price#price{amount = N}, SessionAttributes,
									Reserve, ReserveUnits);
						_N ->
							mnesia:abort(negative_amount)
					end;
				_Other ->
					mnesia:abort(table_lookup_failed)
			end;
		false ->
			mnesia:abort(undefined_tariff)
	end;
authorize3(Protocol, ServiceType, Service,
		Buckets, _Address, Price, SessionAttributes, Reserve, ReserveUnits) ->
	authorize4(Protocol, ServiceType, Service,
			Buckets, Price, SessionAttributes, Reserve, ReserveUnits).
%% @hidden
authorize4(_Protocol, ServiceType,
		#service{session_attributes = ExistingAttr, attributes = Attr} = Service,
		Buckets, #price{units = Units, size = UnitSize, amount = UnitPrice},
		SessionAttributes, Reserve, ReserveUnits) ->
	SessionId = get_session_id(SessionAttributes),
	case update_session(Units, 0, Reserve,
			undefined, undefined, SessionId, Buckets) of
		{0, Reserve, _Buckets2} when ReserveUnits == seconds ->
			NewAttr = radius_attributes:store(?SessionTimeout, Reserve, Attr),
			authorize5(Service, Buckets, ServiceType, SessionAttributes, NewAttr);
		{0, Reserve, _Buckets2} ->
			authorize5(Service, Buckets, ServiceType, SessionAttributes, Attr);
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
	NewSessionAttributes = {erlang:system_time(millisecond),
			get_session_id(SessionAttributes)},
	Service1 = Service#service{session_attributes =
		[NewSessionAttributes], disconnect = false},
	ok = mnesia:write(Service1),
	{authorized, Service1, Attributes, []};
authorize6(#service{multisession = false, session_attributes
		= ExistingAttr} = Service, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(millisecond),
			get_session_id(SessionAttributes)},
	Service1 = Service#service{session_attributes =
		[NewSessionAttributes], disconnect = false},
	ok = mnesia:write(Service1),
	{authorized, Service1, Attributes, ExistingAttr};
authorize6(#service{multisession = true, session_attributes
		= ExistingAttr} = Service, SessionAttributes, Attributes) ->
	NewSessionAttributes = {erlang:system_time(millisecond),
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
	Now = erlang:system_time(millisecond),
	update_session(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, sort(Buckets), [], 0, 0).
%% @hidden
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{start_date = Start, end_date = Expires,
		attributes = #{bucket_type := normal}, remain_amount = Remain} | T],
		Acc, Charged, Reserved) when Expires /= undefined,
		Start =/= Expires, Expires =< Now, Remain >= 0 ->
	update_session(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		attributes = Attributes} = B | T], Acc, Charged, Reserved) ->
	#{reservations := Reservations} = Attributes,
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	F = fun({SessionId1, #{service_id := ServiceId1,
					charging_key := ChargingKey1}}) when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _}) ->
				false
	end,
	case lists:partition(F, maps:to_list(Reservations)) of
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}}],
				NewReservations} when (ReservedAmount - NewCharge) > NewReserve ->
			NewRemain = Remain + ((ReservedAmount - NewCharge) - NewReserve),
			ReservationList = [{SessionId, #{ts => Now,
					debit => DebitedAmount + NewCharge,
					reserve => NewReserve, service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = NewRemain,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserve);
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}}],
				NewReservations} when (ReservedAmount - NewCharge) >= 0 ->
			NewReserved = ReservedAmount - NewCharge,
			ReservationList = [{SessionId, #{ts => Now,
					debit => DebitedAmount + NewCharge,
					reserve => NewReserved, service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserved);
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}}],
				NewReservations} when ReservedAmount > 0,
				NewCharge > ReservedAmount ->
			ReservationList = [{SessionId, #{ts => Now,
					debit => DebitedAmount + ReservedAmount,
					reserve => 0, service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
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
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Charge > Charged) or (Reserve > Reserved)), Remain > 0 ->
	#{reservations := Reservations} = Attributes,
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	F = fun({SessionId1, #{service_id := ServiceId1,
					charging_key := ChargingKey1}}) when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _, _, _, _, _}) ->
				false
	end,
	case lists:partition(F, maps:to_list(Reservations)) of
		{[{_, #{debit := DebitedAmount, reserve := 0, service_id := ServiceId,
				charging_key := ChargingKey}}], NewReservations}
				when Remain >= (NewCharge + NewReserve) ->
			NewRemain = Remain - (NewReserve + NewCharge),
			ReservationList = [{SessionId, #{ts => Now,
					debit => DebitedAmount + NewCharge, reserve => NewReserve,
					service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = NewRemain,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserve);
		{[{_, #{debit := DebitedAmount, reserve := 0, service_id := ServiceId,
				charging_key := ChargingKey}}], NewReservations}
				when Remain >= NewCharge ->
			NewReserved = Remain - NewCharge,
			ReservationList = [{SessionId, #{ts => Now,
					debit => DebitedAmount + NewCharge, reserve => NewReserved,
					service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + NewCharge, Reserved + NewReserved);
		{[{_, #{debit := DebitedAmount, reserve := 0, service_id := ServiceId,
				charging_key := ChargingKey}}], NewReservations}
				when NewCharge > 0 ->
			ReservationList = [{SessionId, #{ts => Now,
					debit => DebitedAmount + Remain, reserve => 0,
					service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged + Remain, Reserved);
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}}],
				NewReservations} when NewCharge =:= 0, Remain >= NewReserve ->
			ReservationList = [{SessionId, #{ts => Now, debit => DebitedAmount,
					reserve => ReservedAmount + NewReserve, service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = Remain - NewReserve,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
					SessionId, T, NewAcc, Charged, Reserved + NewReserve);
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}}],
				NewReservations} when NewCharge =:= 0 ->
			ReservationList = [{SessionId, #{ts => Now, debit => DebitedAmount,
					reserve => ReservedAmount + Remain, service_id => ServiceId,
					charging_key => ChargingKey}} | NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
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
		[#bucket{end_date = Expires, attributes = #{bucket_type := normal},
		remain_amount = Remain} | T], Acc, Charged, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0 ->
	update(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged, Reserved);
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain >= ((Charge - Charged) + (Reserve - Reserved)) ->
	#{reservations := Reservations} = Attributes,
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	NewReservation = Reservations#{SessionId => #{ts => Now, debit => NewCharge,
			reserve => NewReserve, service_id => ServiceId,
			charging_key => ChargingKey}},
	NewBuckets = [B#bucket{remain_amount = Remain - (NewCharge + NewReserve),
		last_modified = {Now, erlang:unique_integer([positive])},
		attributes = Attributes#{reservations => NewReservation}} | Acc],
	{Charged + NewCharge, Reserved + NewReserve, lists:reverse(NewBuckets) ++ T};
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain >= (Charge - Charged) ->
	#{reservations := Reservations} = Attributes,
	NewCharge = Charge - Charged,
	NewReserve = Remain - NewCharge,
	NewReservation = Reservations#{SessionId => #{ts => Now, debit => NewCharge,
			reserve => NewReserve, service_id => ServiceId,
			charging_key => ChargingKey}},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservation}} | Acc],
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
			T, NewAcc, Charged + NewCharge, Reserved + NewReserve);
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain < (Charge - Charged) ->
	#{reservations := Reservations} = Attributes,
	NewCharge = Charge - Charged,
	NewReservation = Reservations#{SessionId => #{ts => Now, debit => NewCharge,
			reserve => 0, service_id => ServiceId, charging_key => ChargingKey}},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservation}} | Acc],
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
	Now = erlang:system_time(millisecond),
	charge_session(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, sort(Buckets), 0, []).
%% @hidden
charge_session(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{remain_amount = Remain, attributes = #{bucket_type := normal},
		start_date = Start, end_date = Expires} | T], Charged, Acc)
		when Remain >= 0, Expires /= undefined, Expires =/= Start,
		Expires =< Now ->
	charge_session(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, T, Charged, Acc);
charge_session(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = Remain,
		attributes = Attributes} = B | T], Charged, Acc) when Charge > 0 ->
	#{reservations := Reservations} = Attributes,
	case maps:take(SessionId, Reservations) of
		{#{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey},
				NewReservations1} when ReservedAmount >= Charge ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservedAmount = ReservedAmount - Charge,
			NewReservations2 = NewReservations1#{SessionId => #{ts => Now,
					debit => NewDebitedAmount, reserve => NewReservedAmount,
					service_id => ServiceId, charging_key => ChargingKey}},
			NewAcc = [B#bucket{attributes = Attributes#{reservations => NewReservations2},
					last_modified = {Now, erlang:unique_integer([positive])}} | Acc],
			charge_session(Type, 0, Now, ServiceId, ChargingKey, SessionId,
					T, Charged + Charge, NewAcc);
		{#{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}, NewReservations1}
				when ReservedAmount < Charge, Remain >= (Charge - ReservedAmount) ->
			NewDebitedAmount = DebitedAmount + Charge,
			NewReservations2 = NewReservations1#{SessionId => #{ts => Now,
					debit => NewDebitedAmount, reserve => 0,
					service_id => ServiceId, charging_key => ChargingKey}},
			NewAcc = [B#bucket{remain_amount = Remain - (Charge - ReservedAmount),
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => NewReservations2}} | Acc],
			charge_session(Type, 0, Now, ServiceId, ChargingKey, SessionId,
					T, Charged + Charge, NewAcc);
		{#{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}, NewReservations1}
				when ReservedAmount < Charge, Remain >= 0,
				Remain < (Charge - ReservedAmount) ->
			NewDebitedAmount = DebitedAmount + (Remain + ReservedAmount),
			NewReservations2 = NewReservations1#{SessionId => #{ts => Now,
			debit => NewDebitedAmount, reserve => 0, service_id => ServiceId,
			charging_key => ChargingKey}},
			NewAcc = [B#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => NewReservations2}} | Acc],
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
	charge_session1(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
			lists:reverse(Acc), [], Charged).

%% @hidden
charge_session1(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{end_date = Expires, attributes = #{bucket_type := normal},
		remain_amount = R} | T], Acc, Charged)
		when R >= 0, Expires /= undefined, Expires =< Now ->
	charge_session1(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged);
charge_session1(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = R, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged)
		when Charge > 0, R >= Charge,
		((Expires == undefined) or (Now < Expires)) ->
	#{reservations := Reservations} = Attributes,
	NewReservations = Reservations#{SessionId => #{ts => Now, debit => Charge,
			reserve => 0, service_id => ServiceId, charging_key => ChargingKey}},
	NewBuckets = [B#bucket{remain_amount = R - Charge,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservations}} | T],
	{Charged + Charge, lists:reverse(Acc) ++ NewBuckets};
charge_session1(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[#bucket{units = Type, remain_amount = R, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged)
		when Charge > 0, R =< Charge, R > 0,
		((Expires == undefined) or (Now < Expires)) ->
	#{reservations := Reservations} = Attributes,
	NewReservations = Reservations#{SessionId => #{ts => Now, debit => R,
			reserve => 0, service_id => ServiceId, charging_key => ChargingKey}},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservations}} | Acc],
	charge_session1(Type, Charge - R, Now,
			ServiceId, ChargingKey, SessionId, T, NewAcc, Charged + R);
charge_session1(_Type, 0, _, _, _, _, Buckets, Acc, Charged) ->
	{Charged, lists:reverse(Acc) ++ Buckets};
charge_session1(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
		[H | T], Acc, Charged) ->
	charge_session1(Type, Charge, Now, ServiceId, ChargingKey, SessionId,
			T, [H | Acc], Charged);
charge_session1(_, _, _, _, _, _, [], Acc, Charged) ->
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
	Now = erlang:system_time(millisecond),
	charge_event(Type, Charge, Now, sort(Buckets), 0, []).
%% @hidden
charge_event(Type, Charge, Now,
		[#bucket{remain_amount = Remain, attributes = #{bucket_type := normal},
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
	Now = erlang:system_time(millisecond),
	convert(Price, Type, Size, ServiceId, ChargingKey, SessionId,
			Now, CentsBuckets, UnitsBuckets, []).
%% @hidden
convert(0, Type, Size, ServiceId, ChargingKey, SessionId,
		Now, CentsBuckets, UnitsBuckets, Acc) ->
	convert1(Type, Size, ServiceId, ChargingKey, SessionId,
			Now, lists:reverse(Acc) ++ CentsBuckets, UnitsBuckets, []);
convert(Price, Type, Size, ServiceId, ChargingKey, SessionId, Now,
		[#bucket{remain_amount = R, end_date = Expires,
		attributes = #{bucket_type := normal}} | T], UnitsBuckets, Acc)
		when R >= 0, Expires /= undefined, Expires =< Now ->
	convert(Price, Type, Size, ServiceId, ChargingKey, SessionId,
			Now, T, UnitsBuckets, Acc);
convert(Price1, Type, Size, ServiceId, ChargingKey, SessionId, Now,
		[#bucket{remain_amount = R, end_date = Expires,
		attributes = Attributes} = B1 | T], UnitsBuckets, Acc)
		when Price1 > 0, R > 0, ((Expires == undefined) or (Now < Expires)) ->
	#{reservations := Reservations1} = Attributes,
	F = fun({SessionId1, #{service_id := ServiceId1, charging_key := ChargingKey1}})
					when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _}) ->
				false
	end,
	{Price2, B2} = case lists:partition(F, maps:to_list(Reservations1)) of
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount}}], Reservations2}
				when R >= Price1 ->
			NewReservations = [{SessionId, #{ts => Now,
					debit => DebitedAmount + Price1, reserve => ReservedAmount,
					service_id => ServiceId,
					charging_key => ChargingKey}} | Reservations2],
			{0, B1#bucket{remain_amount = R - Price1,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => maps:from_list(NewReservations)}}};
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount}}], Reservations2}
				when R < Price1 ->
			NewReservations = [{SessionId, #{ts => Now, debit => DebitedAmount + R,
					reserve => ReservedAmount, service_id => ServiceId,
					charging_key => ChargingKey}} | Reservations2],
			{Price1 - R, B1#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => maps:from_list(NewReservations)}}};
		{[], _} when R >= Price1 ->
			NewReservations = Reservations1#{SessionId => #{ts => Now,
					debit => Price1, reserve => 0, service_id => ServiceId,
					charging_key => ChargingKey}},
			{0, B1#bucket{remain_amount = R - Price1,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => NewReservations}}};
		{[], _} when R < Price1 ->
			NewReservations = Reservations1#{SessionId => #{ts => Now, debit => R,
					reserve => 0, service_id => ServiceId,
					charging_key => ChargingKey}},
			{Price1 - R, B1#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => NewReservations}}}
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
		attributes = #{reservations := Reservations}} = B | T], Acc) ->
	F = fun({SessionId1, #{service_id := ServiceId1,
					charging_key := ChargingKey1}}) when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _}) ->
				false
	end,
	case lists:any(F, maps:to_list(Reservations)) of
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
			attributes = #{reservations => #{SessionId => #{ts => Now, debit => 0,
					reserve => 0, service_id => ServiceId,
					charging_key => ChargingKey}}}},
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
		SessionId :: [tuple()],
		SessionList :: [{pos_integer(), [tuple()]}].
%% @doc Add session identification attributes set to active sessions list.
%%
%% 	If new `SessionId' is a superset of an existing `SessionId' replace it.
%% @private
add_session(SessionId, [] = _SessionList) ->
	[{erlang:system_time(millisecond), SessionId}];
add_session(SessionId, SessionList) ->
	add_session(SessionList, SessionId, []).
%% @hidden
add_session([{TS, CurrentId} = H | T], NewId, Acc) ->
	case CurrentId -- NewId of
		[] ->
			lists:reverse(Acc) ++ [{TS, NewId} | T];
		_ ->
			add_session(T, NewId, [H | Acc])
	end;
add_session([], _, Acc) ->
	lists:reverse(Acc).

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
			session_attributes(SessionAttributes);
		SessionId ->
			[SessionId]
	end.

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
price_units(_Amount, _UnitSize, 0) ->
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
		[#bucket{attributes = #{reservations := Reservations}} = H | T], Acc) ->
	refund(ServiceId, ChargingKey, SessionId,
			H, maps:to_list(Reservations), T, [], Acc);
refund(_, _, _, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
refund(ServiceId, ChargingKey, SessionId,
		#bucket{remain_amount = R, attributes = Attributes} = Bucket1,
		[{SessionId, #{debit := 0, reserve := Amount, service_id := ServiceId,
				charging_key := ChargingKey}} | T1],
		T2, Acc1, Acc2) ->
	NewReservations = maps:from_list(lists:reverse(Acc1) ++ T1),
	Bucket2 = Bucket1#bucket{remain_amount = R + Amount,
			attributes = Attributes#{reservations => NewReservations}},
	refund(ServiceId, ChargingKey, SessionId, T2, [Bucket2 | Acc2]);
refund(ServiceId, ChargingKey, SessionId,
		#bucket{remain_amount = R, attributes = Attributes} = Bucket1,
		[{SessionId, #{ts := TS, debit := Debit, reserve := Amount,
				service_id := ServiceId, charging_key := ChargingKey}} | T1],
		T2, Acc1, Acc2) ->
	Refunded = {SessionId, #{ts => TS, debit => Debit, reserve => 0,
			service_id => ServiceId, charging_key => ChargingKey}},
	NewReservations = maps:from_list(lists:reverse(Acc1) ++ [Refunded | T1]),
	Bucket2 = Bucket1#bucket{remain_amount = R + Amount,
			attributes = Attributes#{reservations => NewReservations}},
	refund(ServiceId, ChargingKey, SessionId, T2, [Bucket2 | Acc2]);
refund(ServiceId, ChargingKey, SessionId, Bucket,
		[H | T1], T2, Acc1, Acc2) ->
	refund(ServiceId, ChargingKey, SessionId, Bucket,
			T1, T2, [H | Acc1], Acc2);
refund(ServiceId, ChargingKey, SessionId,
		#bucket{attributes = Attributes} = Bucket1, [], T, Acc1, Acc2) ->
	NewReservations = maps:from_list(lists:reverse(Acc1)),
	Bucket2 = Bucket1#bucket{attributes =
			Attributes#{reservations => NewReservations}},
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
		#char_value_use{values = [#char_value{value = undefined}]} ->
					filter_prices_tod(Timestamp, T, [P | Acc]);
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
		#char_value_use{values = [#char_value{value = undefined}]} ->
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
		ChargingKey :: non_neg_integer() | undefined,
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
		#char_value_use{values = [#char_value{value = undefined}]} ->
			filter_prices_key(ChargingKey, T, [P | Acc]);
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
		ChargingKey :: integer() | undefined,
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
	Now = erlang:system_time(millisecond),
	get_final(Buckets, ServiceId, ChargingKey, SessionId, Now, #{}, []).
%% @hidden
get_final([#bucket{remain_amount = 0,
		attributes = #{bucket_type := normal}} | T], ServiceId,
		ChargingKey, SessionId, Now, Debits, Acc) ->
	get_final(T, ServiceId, ChargingKey, SessionId, Now, Debits, Acc);
get_final([#bucket{remain_amount = R, attributes = #{bucket_type := normal},
		end_date = Expires} | T], ServiceId,
		ChargingKey, SessionId, Now, Debits, Acc)
		when R >= 0, Expires /= undefined, Expires < Now ->
	get_final(T, ServiceId, ChargingKey, SessionId, Now, Debits, Acc);
get_final([#bucket{attributes = #{bucket_type := normal}} = B | T],
		ServiceId, ChargingKey, SessionId, Now, Debits, Acc) ->
	get_final(T, ServiceId, ChargingKey, SessionId, Now, Debits, [B | Acc]);
get_final([#bucket{units = Units, attributes = Attributes,
		remain_amount = R, start_date = StartDate, end_date = EndDate} = B | T],
		ServiceId, ChargingKey, SessionId, Now, Debits, Acc) ->
	#{reservations := Reservations} = Attributes,
	N = maps:get(Units, Debits, 0),
	case get_debits(ServiceId, ChargingKey, SessionId,
			maps:to_list(Reservations), 0, 0, []) of
		{Debit, 0, []} when R == 0 ->
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit}, Acc);
		{Debit, Refund, []} when StartDate == EndDate ->
			NewAttributes = maps:remove(reservations, Attributes),
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit},
					[B#bucket{name = undefined, end_date = undefined,
					remain_amount = R + Refund,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc]);
		{Debit, _Refund, []} when R >= 0,
				EndDate /= undefined, EndDate < Now ->
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit}, Acc);
		{Debit, Refund, NewReservations} ->
			get_final(T, ServiceId, ChargingKey, SessionId,
					Now, Debits#{Units => N + Debit},
					[B#bucket{remain_amount = R + Refund,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations
							=> maps:from_list(NewReservations)}} | Acc])
	end;
get_final([], _, _, _, _, Debits, Acc) ->
	{Debits, lists:reverse(Acc)}.

%% @hidden
get_debits(undefined, undefined, SessionId,
		[{SessionId, #{debit := Debited, reserve := Reserved}} | T],
		Debit, Refund, Acc) ->
	get_debits(undefined, undefined, SessionId,
			T, Debit + Debited, Refund + Reserved, Acc);
get_debits(ServiceId, ChargingKey, SessionId,
		[{SessionId, #{debit := Debited, reserve := Reserved,
				service_id := ServiceId, charging_key := ChargingKey}} | T],
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
		Debits :: map(),
		Rated :: #rated{} | [#rated{}],
		Result :: [Rated].
%% @doc Construct rated product usage.
%% @hidden
rated(Debits, Rated)
		when map_size(Debits) =:= 0, is_list(Rated) ->
	Rated;
rated(Debits, Rated)
		when map_size(Debits) =:= 0 ->
	[Rated];
rated(Debits, #rated{} = Rated) ->
	rated(Debits, [Rated]);
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
	{erlang:system_time(millisecond), erlang:unique_integer([positive])}.

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
					total_balance = [#quantity{units = Units,
					amount = TotalBalance}], bucket = BucketRefs} | AccBalance];
		_ ->
			AccBalance
	end.

