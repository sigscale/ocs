%%% ocs_rating.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-export([rate/13, charge/11]).
-export([authorize/8]).
-export([session_attributes/1]).
-export([filter_prices_tod/2, filter_prices_key/2]).

-include("ocs.hrl").
-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("stdlib/include/assert.hrl").

-type bucket() :: #bucket{}.

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
		DebitAmounts :: [{UnitType, Amount}],
		ReserveAmounts :: [{UnitType, Amount}] | undefined,
		UnitType :: octets | seconds | messages,
		Amount :: integer(),
		SessionAttributes :: [tuple()],
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
		GrantedAmount :: {UnitType, Amount},
		Rated :: [#rated{}],
		SessionList :: [{pos_integer(), [tuple()]}],
		RedirectServerAddress :: string() | undefined,
		Reason :: offer_not_found | product_not_found
				| service_not_found | invalid_service_type
				| invalid_bundle_product | term().
%% @doc Handle rating and balance management for used and reserved unit amounts.
%%
%% 	Subscriber balance buckets are permanently reduced by the
%% 	amount in `DebitAmounts' and bucket reservations are made
%% 	of the amounts in `ReserveAmounts'. The subscribed
%% 	<i>Product Offering</i>	provides one or more
%% 	<i>Product Offering Price</i> (POP) used to rate the
%% 	service usage.
%%
%% 	The value of `DebitAmounts' is a list of one or more alternate
%% 	measures	of the service usage with different `UnitType'
%% 	(i.e. `octets'	and `seconds') allowing the applied POP to
%% 	determine the `UnitType' selected.
%%
%% 	If `ReserveAmounts' is an empty list, and `Flag' is  `initial',
%% 	`interim' or `event', the `UnitType' and `Amount' are determined
%% 	by applicable POP. If `undefined' no reservation is performed.
%%
%% 	If successful returns `{ok, Service, GrantedAmount}' for `initial'
%% 	and `interim' updates, `{ok, Service, Rated}' for `final' or
%% 	`{ok, Service, GrantedAmount, Rated}' for `event'.
%%
%% 	If subscriber's balance is insufficient to cover the `DebitAmounts'
%% 	and `ReserveAmounts' returns
%% 	`{out_of_credit, RedirectServerAddressAddress, SessionList}'
%% 	for `initial' or `interim' and
%% 	`{out_of_credit, RedirectServerAddressAddress, SessionList, Rated}'
%% 	for `final'. Returns `{disabled, SessionList}' if the subscriber
%% 	is not enabled. The value of `SessionList' describes the
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
				[#service{product = ProdRef} = Service] ->
					case mnesia:read(product, ProdRef, read) of
						[#product{product = OfferId,
								balance = BucketRefs} = Product] ->
							Now = erlang:system_time(millisecond),
							case mnesia:dirty_read(offer, OfferId) of
								[#offer{char_value_use = CharValueUse,
												end_date = EndDate, start_date = StartDate} = Offer]
										when ((StartDate =< Now) or (StartDate == undefined)),
												((EndDate > Now) or (EndDate == undefined)) ->
									Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write)
											|| Id <- BucketRefs]),
									RedirectServerAddress = case lists:keyfind("redirectServer",
											#char_value_use.name, CharValueUse) of
										#char_value_use{values = [#char_value{value = Value}]}
												when is_list(Value) ->
											Value;
										_Other ->
											undefined
									end,
									NewBuckets = lists:map(fun ocs:parse_bucket/1, Buckets),
									RateResult = rate1(Protocol, Service, ServiceId, Product,
											NewBuckets, Timestamp, Address, Direction, Offer,
											Flag, DebitAmounts, ReserveAmounts, ServiceType,
											get_session_id(SessionAttributes), ChargingKey,
											ServiceNetwork),
									{RateResult, RedirectServerAddress};
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
				when is_list(Rated); is_record(Rated, rated) ->
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
				when is_list(Rated); is_record(Rated, rated) ->
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
		error:{badmatch, _} ->
			mnesia:abort(invalid_bundle_product);
		_:Reason ->
			mnesia:abort(Reason)
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
rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp,
		_Address, _Direction,
		#offer{specification = ProdSpec, price = Prices} = _Offer,
		Flag, DebitAmounts, ReserveAmounts, SessionId, Rated,
		ChargingKey, ServiceNetwork)
		when ProdSpec == "4"; ProdSpec == "8" ->
	Now = erlang:system_time(millisecond),
	F = fun(#price{type = tariff, units = octets,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = usage,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = #pla_ref{},
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	rate3(Protocol, Service, ServiceId, Product, Buckets, undefined,
			Flag, DebitAmounts, ReserveAmounts, SessionId,
			Rated, ChargingKey, ServiceNetwork, FilteredPrices3);
rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp,
		Address, Direction,
		#offer{specification = ProdSpec, price = Prices} = _Offer,
		Flag, DebitAmounts, ReserveAmounts, SessionId, Rated,
		ChargingKey, ServiceNetwork)
		when ProdSpec == "5"; ProdSpec == "9" ->
	Now = erlang:system_time(millisecond),
	F = fun(#price{type = tariff, units = seconds,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = usage, units = seconds,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = #pla_ref{},
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	FilteredPrices4 = filter_prices_dir(Direction, FilteredPrices3),
	rate3(Protocol, Service, ServiceId, Product, Buckets, Address,
			Flag, DebitAmounts, ReserveAmounts, SessionId,
			Rated, ChargingKey, ServiceNetwork, FilteredPrices4);
rate2(Protocol, Service, ServiceId, Product, Buckets, Timestamp,
		Address, Direction,
		#offer{specification = ProdSpec, price = Prices} = _Offer,
		Flag, DebitAmounts, ReserveAmounts, SessionId, Rated,
		ChargingKey, ServiceNetwork)
		when ProdSpec == "10"; ProdSpec == "11" ->
	Now = erlang:system_time(millisecond),
	F = fun(#price{type = usage, units = messages,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = usage, units = cents,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = tariff, units = messages,
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(#price{type = #pla_ref{},
					start_date = StartDate, end_date = EndDate})
					when ((StartDate =< Now) or (StartDate == undefined)),
					((EndDate > Now) or (EndDate == undefined)) ->
				true;
			(_) ->
				false
	end,
	FilteredPrices1 = lists:filter(F, Prices),
	FilteredPrices2 = filter_prices_key(ChargingKey, FilteredPrices1),
	FilteredPrices3 = filter_prices_tod(Timestamp, FilteredPrices2),
	FilteredPrices4 = filter_prices_dir(Direction, FilteredPrices3),
	rate3(Protocol, Service, ServiceId, Product, Buckets, Address,
			Flag, DebitAmounts, ReserveAmounts, SessionId,
			Rated, ChargingKey, ServiceNetwork, FilteredPrices4).
%% @hidden
rate3(_Protocol, _Service, _ServiceId, _Product, _Buckets,
		_Address, _Flag, _DebitAmounts, _ReserveAmounts,
		_SessionId, _Rated, _ChargingKey, _ServiceNetwork,
		[#price{type = #pla_ref{}} = Price | _]) ->
	{pla_ref, Price};
rate3(Protocol, Service, ServiceId, Product, Buckets,
		Address, Flag, DebitAmounts, ReserveAmounts,
		SessionId, Rated, ChargingKey, ServiceNetwork, Prices) ->
	charge1(Protocol, Flag, Service, ServiceId, Product,
				Buckets, Prices, DebitAmounts, ReserveAmounts,
				SessionId, ChargingKey, Address, ServiceNetwork).

-spec charge(Protocol, Flag, SubscriberID, ServiceId,
		ChargingKey, ServiceNetwork, Address, Prices,
		DebitAmounts, ReserveAmounts, SessionAttributes) -> Result
	when
		Protocol :: radius | diameter,
		Flag :: initial | interim | final | event,
		SubscriberID :: binary(),
		ServiceId :: integer() | undefined,
		ChargingKey :: integer() | undefined,
		ServiceNetwork :: [$0..$9],
		Address :: [$0..$9],
		Prices :: [#price{}],
		DebitAmounts :: [{UnitType, Amount}],
		ReserveAmounts :: [{UnitType, Amount}] | undefined,
		UnitType :: octets | seconds | messages,
		Amount :: integer(),
		SessionAttributes :: list(),
		Result :: {ok, Service, GrantedAmount}
				| {ok, Service, Rated}
				| {ok, Service, GrantedAmount, Rated}
				| {out_of_credit, RedirectServerAddress, SessionList}
				| {out_of_credit, RedirectServerAddress, SessionList, Rated}
				| {disabled, SessionList}
				| {error, Reason},
		Service :: #service{},
		GrantedAmount :: {UnitType, Amount},
		Amount :: integer(),
		Rated :: [#rated{}],
		SessionList :: [{pos_integer(), [tuple()]}],
		RedirectServerAddress :: string() | undefined,
		Reason :: term().
%% @doc Handle balance management for used and reserved unit amounts.
charge(Protocol, Flag, SubscriberID, ServiceId, ChargingKey,
		ServiceNetwork, Address, Prices,
		DebitAmounts, ReserveAmounts, SessionAttributes)
		when ((Protocol == radius) or (Protocol == diameter)),
		is_binary(SubscriberID),
		(is_integer(ChargingKey) or (ChargingKey == undefined)),
		(is_integer(ServiceId) or (ServiceId== undefined)),
		is_list(Prices),
		((Flag == initial) or (Flag == interim) or (Flag == final) or (Flag == event)),
		is_list(DebitAmounts),
		(is_list(ReserveAmounts) or (ReserveAmounts == undefined)),
		length(SessionAttributes) > 0 ->
	F = fun() ->
		case mnesia:read(service, SubscriberID, read) of
			[#service{product = ProdRef} = Service] ->
				case mnesia:read(product, ProdRef, read) of
					[#product{product = OfferId, balance = BucketRefs} = Product] ->
						case mnesia:read(offer, OfferId, read) of
							[#offer{char_value_use = CharValueUse} = _Offer] ->
								Buckets = lists:flatten([mnesia:read(bucket, Id, sticky_write)
										|| Id <- BucketRefs]),
								RedirectServerAddress = case lists:keyfind("redirectServer",
										#char_value_use.name, CharValueUse) of
									#char_value_use{values = [#char_value{value = Value}]}
											when is_list(Value) ->
										Value;
									_Other ->
										undefined
								end,
								ChargeResult = charge1(Protocol, Flag, Service,
										ServiceId, Product, Buckets, Prices,
										DebitAmounts, ReserveAmounts,
										get_session_id(SessionAttributes),
										ChargingKey, Address, ServiceNetwork),
								{ChargeResult,	RedirectServerAddress};
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
				when is_list(Rated); is_record(Rated, rated) ->
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
				when is_list(Rated); is_record(Rated, rated) ->
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
		{aborted, Reason} ->
			{error, Reason}
	end.

%% @doc Split and order buckets.
%% @hidden
charge1(Protocol, Flag, Service, ServiceId, Product, Buckets,
		[#price{units = Units, size = UnitSize, name = PriceName,
				type = PriceType, currency = Currency} | _ ] = Prices,
		DebitAmounts, ReserveAmounts, SessionId, ChargingKey,
		Address, ServiceNetwork) ->
	F = fun(#bucket{remain_amount = RemainAmount})
					when RemainAmount < 0 ->
				true;
			(_) ->
				false
	end,
	case lists:any(F, Buckets) of
		true ->
			DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
				{Units, DA} ->
					{Units, DA};
				false ->
					{Units, 0}
			end,
			Rated = case Flag of
				final ->
					[#rated{price_type = PriceType,
							price_name = PriceName, currency = Currency}];
				_ ->
					[]
			end,
			ReserveAmount = {Units, reserve_amount(Units,
					UnitSize, ReserveAmounts)},
			charge4(Flag, Service, ServiceId, Product, Buckets,
					DebitAmount, {Units, 0}, ReserveAmount, {Units, 0},
					SessionId, Rated, ChargingKey, Buckets);
		false ->
			{PriceBuckets, OtherBuckets} = split_by_price(Buckets),
			charge2(Protocol, Flag, Service, ServiceId,
					Product, Prices, DebitAmounts, ReserveAmounts,
					{undefined, 0}, {undefined, 0}, SessionId,
					ChargingKey, Address, ServiceNetwork,
					[], PriceBuckets, OtherBuckets, [], Buckets)
	end.

%% @doc Determine POP and amount of debit and reserve.
%% @hidden
charge2(_Protocol, Flag,
		#service{enabled = false} = Service, ServiceId, Product,
		[#price{units = Units} | _ ] = _Prices,
		_DebitAmounts, _ReserveAmounts, _DebitedAmount, _ReservedAmount,
		SessionId, ChargingKey, _Address, _ServiceNetwork, Rated,
		PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	NewBuckets = lists:flatten([PriceBuckets, NewAcc, OtherBuckets]),
	charge4(Flag, Service, ServiceId, Product, NewBuckets,
			{Units, 0}, {Units, 0}, {Units, 0}, {Units, 0},
			SessionId, Rated, ChargingKey, OldBuckets);
charge2(_Protocol, Flag, Service, ServiceId, Product,
		[#price{name = PriceName, type = PriceType,
				currency = Currency} | _ ] = _Prices,
		[{Units, DA1} = DebitAmount] = _DebitAmounts,
		[{Units, RA1} = ReserveAmount] = _ReserveAmounts,
		{Units, DA2} = DebitedAmount, {Units, RA2} = ReservedAmount,
		SessionId, ChargingKey, _Address, _ServiceNetwork, Rated,
		PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when DA2 >= DA1, RA2 >= RA1 ->
	NewBuckets = lists:flatten([PriceBuckets, NewAcc, OtherBuckets]),
	Rated1 = case Rated of
		[] when Flag == final ->
			[#rated{price_type = PriceType,
					price_name = PriceName, currency = Currency}];
		Rated ->
			Rated
	end,
	charge4(Flag, Service, ServiceId, Product, NewBuckets,
			DebitAmount, DebitedAmount, ReserveAmount, ReservedAmount,
			SessionId, Rated1, ChargingKey, OldBuckets);
charge2(Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} | _ ] = Prices,
		DebitAmounts, ReserveAmounts, {undefined, 0}, {undefined, 0},
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	DebitedAmount = {Units, 0},
	ReservedAmount = {Units, 0},
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			DebitAmounts, ReserveAmounts, DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} | _ ] = Prices,
		[] = _DebitAmounts, ReserveAmounts, DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	DebitAmount = {Units, 0},
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			[DebitAmount], ReserveAmounts, DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} | _ ] = Prices,
		DebitAmounts, ReserveAmounts, DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when length(DebitAmounts) > 1 ->
	DebitAmount = case lists:keyfind(Units, 1, DebitAmounts) of
		{_, DA} ->
			{Units, DA};
		false -> % @todo find POP with matching units {Units, 0}
			{Units, 0}
	end,
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			[DebitAmount], ReserveAmounts, DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} | _ ] = Prices,
		DebitAmounts, undefined = _ReserveAmounts, DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	ReserveAmount = {Units, 0},
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			DebitAmounts, [ReserveAmount], DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(radius = Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units,
				char_value_use = CharValueUse} | _ ] = Prices,
		DebitAmounts, [] = _ReserveAmounts, DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when Flag == initial; Flag == interim ->
	ReserveAmount = radius_reserve(Units, CharValueUse),
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			DebitAmounts, [ReserveAmount], DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(diameter = Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units, size = UnitSize} | _ ] = Prices,
		DebitAmounts, [] = _ReserveAmounts, DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when Flag == initial; Flag == interim; Flag == event ->
	RA = reserve_amount(Units, UnitSize),
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			DebitAmounts, [{Units, RA}], DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(_Protocol, Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units,
				size = UnitSize} = Price | _ ] = _Prices,
		[{Units, DA1} = DebitAmount], ReserveAmounts,
		{Units, DA2} = _DebitedAmount, {Units, RA1} = _ReservedAmount,
		SessionId, ChargingKey, _Address, _ServiceNetwork, Rated,
		[] = _PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when Flag == initial; Flag == interim ->
	DA3 = case DA1 > DA2 of
		true ->
			DA1 - DA2;
		false -> % @todo find POP with matching units {Units, 0}
			0
	end,
	RA2 = reserve_amount(Units, UnitSize, ReserveAmounts),
	RA3 = case RA2 > RA1 of
		true ->
			RA2 - RA1;
		false ->
			0
	end,
	{ok, Overflow} = application:get_env(ocs, charge_overflow),
	{{Units, DA4}, {Units, RA4}, NewBuckets1, undefined}
			= charge3(Flag, Service, ServiceId, Product,
			OtherBuckets, Price, {Units, DA3}, {Units, RA3},
			SessionId, ChargingKey, Overflow),
	NewDebitedAmount = {Units, DA2 + DA4},
	NewReservedAmount = {Units, RA1 + RA4},
	NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
	charge4(Flag, Service, ServiceId, Product, NewBuckets2,
			DebitAmount, NewDebitedAmount, {Units, RA2}, NewReservedAmount,
			SessionId, Rated, ChargingKey, OldBuckets);
charge2(Protocol, final = Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} | _ ] = Prices,
		DebitAmounts, ReserveAmounts, DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when ReserveAmounts == undefined; ReserveAmounts == [] ->
	ReserveAmount = {Units, 0},
	charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
			DebitAmounts, [ReserveAmount], DebitedAmount, ReservedAmount,
			SessionId, ChargingKey, Address, ServiceNetwork, Rated,
			PriceBuckets, OtherBuckets, NewAcc, OldBuckets);
charge2(_Protocol, final = Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} = Price | _ ] = _Prices,
		[{Units, DA1} = DebitAmount] = _DebitAmounts,
		[{Units, 0} = ReserveAmount] = _ResererveAmounts,
		{Units, DA2} = _DebitedAmount, {Units, 0} = ReservedAmount,
		SessionId, ChargingKey, _Address, _ServiceNetwork, Rated1,
		[] = _PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	DA3 = case DA1 > DA2 of
		true ->
			DA1 - DA2;
		false ->
			0
	end,
	{ok, Overflow} = application:get_env(ocs, charge_overflow),
	{{Units, DA4}, {Units, 0}, NewBuckets1, Rated2}
			= charge3(Flag, Service, ServiceId, Product,
			OtherBuckets, Price, {Units, DA3}, {Units, 0},
			SessionId, ChargingKey, Overflow),
	NewDebitedAmount = {Units, DA2 + DA4},
	Rated3 = lists:flatten([Rated1, Rated2]),
	NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
	charge4(Flag, Service, ServiceId, Product, NewBuckets2,
			DebitAmount, NewDebitedAmount, ReserveAmount, ReservedAmount,
			SessionId, Rated3, ChargingKey, OldBuckets);
charge2(_Protocol, event = Flag, Service, ServiceId, Product,
		[#price{type = usage, units = Units} = Price | _ ] = _Prices,
		[{Units, 0}], [{Units, DA1} = DebitAmount],
		{Units, DA2} = _DebitedAmount, {Units, 0} = _ReservedAmount,
		SessionId, ChargingKey, _Address, _ServiceNetwork, Rated1,
		[] = _PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	DA3 = case DA1 > DA2 of
		true ->
			DA1 - DA2;
		false ->
			0
	end,
	{ok, Overflow} = application:get_env(ocs, charge_overflow),
	{{Units, DA4}, {Units, 0}, NewBuckets1, Rated2}
			= charge3(Flag, Service, ServiceId, Product,
			OtherBuckets, Price, {Units, DA3}, {Units, 0},
			SessionId, ChargingKey, Overflow),
	NewDebitedAmount = {Units, DA2 + DA4},
	Rated3 = lists:flatten([Rated1, Rated2]),
	NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
	charge4(Flag, Service, ServiceId, Product, NewBuckets2,
			DebitAmount, NewDebitedAmount, {Units, 0}, {Units, 0},
			SessionId, Rated3, ChargingKey, OldBuckets);
charge2(Protocol, initial = Flag, Service, ServiceId, Product,
		[#price{type = tariff, units = Units,
				size = UnitSize} = Price1 | T ] = _Prices,
		[] = DebitAmounts, ReserveAmounts,
		{_, DA} = DebitedAmount, {_, RA1} = ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	DebitAmount = {Units, 0},
	case tariff_rate(Address, ServiceNetwork, Price1) of
		{_Description, InitialUnitSize, InitialUnitPrice, _, _}
				when UnitSize == undefined; UnitSize == InitialUnitSize ->
			ReserveAmount = {Units, reserve_amount(Units,
					InitialUnitSize, ReserveAmounts)},
			RA2 = case ReserveAmount of
				{_, Ra} when Ra > RA1 ->
					Ra - RA1;
				_ ->
					0
			end,
			Price2 = Price1#price{size = InitialUnitSize,
					amount = InitialUnitPrice},
			{ok, Overflow} = application:get_env(ocs, charge_overflow),
cs, 
			{DebitAmount, {Units, RA3}, NewBuckets1, undefined}
					= charge3(Flag, Service, ServiceId, Product, OtherBuckets,
							Price2, DebitAmount, {Units, RA2},
							SessionId, ChargingKey, Overflow),
			NewReservedAmount = {Units, RA1 + RA3},
			NewDebitedAmount = {Units, DA},
			NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
			charge4(Flag, Service, ServiceId, Product, NewBuckets2,
					DebitAmount, NewDebitedAmount, ReserveAmount,
					NewReservedAmount, SessionId, Rated, ChargingKey,
					OldBuckets);
		undefined ->
			charge2(Protocol, Flag, Service, ServiceId, Product,
					T, DebitAmounts, ReserveAmounts,
					DebitedAmount, ReservedAmount,
					SessionId, ChargingKey, Address, ServiceNetwork, Rated,
					PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
	end;
charge2(Protocol, interim = Flag, Service, ServiceId, Product,
		[#price{type = tariff, units = Units,
				size = UnitSize} = Price1 | T ] = _Prices,
		DebitAmounts, ReserveAmounts,
		{_, DA1} = DebitedAmount, {_, RA1} = ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	DA2 = case lists:keyfind(Units, 1, DebitAmounts) of
		{_, Da} ->
			Da;
		false ->
			0
	end,
	DA3 = case DA2 > DA1 of
		true ->
			DA2 - DA1;
		false ->
			0
	end,
	case tariff_rate(Address, ServiceNetwork, Price1) of
		{_Description, InitialUnitSize, InitialUnitPrice, _, _}
				when UnitSize == undefined; UnitSize == InitialUnitSize ->
			ReserveAmount = {Units, reserve_amount(Units,
					InitialUnitSize, ReserveAmounts)},
			RA2 = case ReserveAmount of
				{_, Ra} when Ra > RA1 ->
					Ra - RA1;
				_ ->
					0
			end,
			Price2 = Price1#price{size = InitialUnitSize,
					amount = InitialUnitPrice},
			{ok, Overflow} = application:get_env(ocs, charge_overflow),
			{{Units, DA4}, {Units, RA3}, NewBuckets1, undefined}
					= charge3(Flag, Service, ServiceId, Product,
					OtherBuckets, Price2, {Units, DA3}, {Units, RA2},
					SessionId, ChargingKey, Overflow),
			NewDebitedAmount = {Units, DA1 + DA4},
			NewReservedAmount = {Units, RA1 + RA3},
			NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
			charge4(Flag, Service, ServiceId, Product, NewBuckets2,
					{Units, DA2}, NewDebitedAmount,
					ReserveAmount, NewReservedAmount,
					SessionId, Rated, ChargingKey, OldBuckets);
		undefined ->
			charge2(Protocol, Flag, Service, ServiceId, Product,
					T, DebitAmounts, ReserveAmounts,
					DebitedAmount, ReservedAmount,
					SessionId, ChargingKey, Address, ServiceNetwork, Rated,
					PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
	end;
charge2(Protocol, final = Flag, Service, ServiceId, Product,
		[#price{type = tariff, units = Units,
				size = UnitSize} = Price1 | T ] = _Prices,
		DebitAmounts, ReserveAmounts,
		{_, DA1} = DebitedAmount, ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated1,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
		when ReserveAmounts == undefined; ReserveAmounts == [] ->
	DA2 = case lists:keyfind(Units, 1, DebitAmounts) of
		{_, Da} ->
			Da;
		false ->
			0
	end,
	DA3 = case DA2 > DA1 of
		true ->
			DA2 - DA1;
		false ->
			0
	end,
	ReserveAmount = {Units, 0},
	case tariff_rate(Address, ServiceNetwork, Price1) of
		{_Description, InitialUnitSize, InitialUnitPrice, _, _}
				when UnitSize == undefined; UnitSize == InitialUnitSize ->
			Price2 = Price1#price{size = InitialUnitSize,
					amount = InitialUnitPrice},
			{ok, Overflow} = application:get_env(ocs, charge_overflow),
			{{Units, DA4}, {Units, 0}, NewBuckets1, Rated2}
					= charge3(Flag, Service, ServiceId, Product,
					OtherBuckets, Price2, {Units, DA3},
					ReserveAmount, SessionId, ChargingKey, Overflow),
			NewDebitedAmount = {Units, DA1 + DA4},
			Rated3 = lists:flatten([Rated1, Rated2]),
			NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
			charge4(Flag, Service, ServiceId,
					Product, NewBuckets2,
					{Units, DA2}, NewDebitedAmount,
					ReserveAmount, {Units, 0},
					SessionId, Rated3, ChargingKey, OldBuckets);
		undefined ->
			charge2(Protocol, Flag, Service, ServiceId,
					Product, T, DebitAmounts, ReserveAmounts,
					DebitedAmount, ReservedAmount,
					SessionId, ChargingKey, Address,
					ServiceNetwork, Rated1, PriceBuckets,
					OtherBuckets, NewAcc, OldBuckets)
	end;
charge2(Protocol, event = Flag, Service, ServiceId, Product,
		[#price{type = tariff, units = Units,
				size = UnitSize} = Price1 | T ] = _Prices,
		[] = DebitAmounts, ReserveAmounts,
		{_, DA1} = DebitedAmount1, ReservedAmount1,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated1,
		[] = PriceBuckets, OtherBuckets, NewAcc, OldBuckets) ->
	case tariff_rate(Address, ServiceNetwork, Price1) of
		{_Description, InitialUnitSize, InitialUnitPrice, _, _}
				when UnitSize == undefined; UnitSize == InitialUnitSize ->
			Price2 = Price1#price{size = InitialUnitSize,
					amount = InitialUnitPrice},
			DA2 = reserve_amount(Units, InitialUnitSize, ReserveAmounts),
			DebitAmount = case DA2 - DA1 of
				NewDA2 when NewDA2 > 0 ->
					{Units, NewDA2};
				_NewDA2 ->
					{Units, 0}
			end,
			{ok, Overflow} = application:get_env(ocs, charge_overflow),
			{{Units, DA3}, ReservedAmount2, NewBuckets1, Rated2}
					= charge3(Flag, Service, ServiceId, Product,
					OtherBuckets, Price2, DebitAmount, {Units, 0},
					SessionId, ChargingKey, Overflow),
			NewDebitedAmount = {Units, DA1 + DA3},
			Rated3 = lists:flatten([Rated1, Rated2]),
			NewBuckets2 = lists:flatten([NewAcc, NewBuckets1]),
			charge4(Flag, Service, ServiceId,
					Product, NewBuckets2,
					DebitAmount, NewDebitedAmount,
					{Units, 0}, ReservedAmount2,
					SessionId, Rated3, ChargingKey, OldBuckets);
		undefined ->
			charge2(Protocol, Flag, Service, ServiceId,
					Product, T, DebitAmounts, ReserveAmounts,
					DebitedAmount1, ReservedAmount1, SessionId,
					ChargingKey, Address, ServiceNetwork, Rated1,
					PriceBuckets, OtherBuckets, NewAcc, OldBuckets)
	end;
charge2(_Protocol, _Flag, _Service, _ServiceId, _Product,
		[] = _Prices, _DebitAmounts, _ReserveAmounts,
		_DebitedAmount, _ReservedAmount,
		_SessionId, _ChargingKey, _Address, _ServiceNetwork, _Rated,
		[] = _PriceBuckets, _OtherBuckets, _NewAcc, _OldBuckets) ->
	mnesia:abort(table_lookup_failed);
charge2(Protocol, Flag, Service, ServiceId, Product, Prices,
		DebitAmounts, ReserveAmounts,
		{Units1, DA1} = DebitedAmount, {Units1, RA1} = ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated,
		[#bucket{price = PriceName} | _] = PriceBuckets1,
		OtherBuckets, NewAcc, OldBuckets)
		when Flag == initial; Flag == interim ->
	F = fun(#bucket{price = Pname}) when Pname == PriceName ->
				true;
		(_) ->
				false
	end,
	{Buckets, PriceBuckets2} = lists:partition(F, PriceBuckets1),
	case lists:keyfind(PriceName, #price.name, Prices) of
		#price{type = usage, units = Units2, size = UnitSize} = Price
				when Units1 == undefined; Units1 == Units2 ->
			DebitAmount = case lists:keyfind(Units2, 1, DebitAmounts) of
				Da when is_tuple(Da) ->
					Da;
				false ->
					{Units2, 0}
			end,
			DA2 = case DebitAmount of
				{_, Du} when Du > DA1 ->
					Du - DA1;
				_ ->
					0
			end,
			ReserveAmount = {Units2, reserve_amount(Units2,
					UnitSize, ReserveAmounts)},
			RA2 = case ReserveAmount of
				{_, Ru} when Ru > RA1 ->
					Ru - RA1;
				_ ->
					0
			end,
			case charge3(Flag, Service, ServiceId, Product,
					Buckets, Price, {Units2, DA2}, {Units2, RA2},
					SessionId, ChargingKey, false) of
				{{Units2, DA3}, {Units2, RA3}, NewBuckets1, undefined}
						when DA3 >= DA2, RA3 >= RA2 ->
					NewBuckets2 = lists:flatten([PriceBuckets2,
							NewAcc, NewBuckets1, OtherBuckets]),
					charge4(Flag, Service, ServiceId, Product, NewBuckets2,
							DebitAmount, {Units2, DA1 + DA3},
							ReserveAmount, {Units2, RA1 + RA3},
							SessionId, Rated, ChargingKey, OldBuckets);
				{{Units2, DA3}, {Units2, RA3}, NewBuckets1, undefined} ->
					NewAcc1 = lists:flatten([NewBuckets1, NewAcc]),
					NewPrices = case PriceBuckets2 of
						[] ->
							drop_fixed(Prices);
						_ ->
							Prices
					end,
					charge2(Protocol, Flag, Service,
							ServiceId, Product, NewPrices,
							[DebitAmount], [ReserveAmount],
							{Units2, DA1 + DA3}, {Units2, RA1 + RA3},
							SessionId, ChargingKey, Address,
							ServiceNetwork, Rated, PriceBuckets2,
							OtherBuckets, NewAcc1, OldBuckets)
			end;
		#price{type = tariff, units = Units2, size = UnitSize} = Price1
				when Units1 == undefined; Units1 == Units2 ->
			DebitAmount = case lists:keyfind(Units2, 1, DebitAmounts) of
				Da when is_tuple(Da) ->
					Da;
				false ->
					{Units2, 0}
			end,
			DA2 = case DebitAmount of
				{_, Du} when Du > DA1 ->
					Du - DA1;
				_ ->
					0
			end,
			case tariff_rate(Address, ServiceNetwork, Price1) of
				{_Description, InitialUnitSize, InitialUnitPrice, _, _}
						when UnitSize == undefined;
						UnitSize == InitialUnitSize ->
					ReserveAmount = {Units2, reserve_amount(Units2,
							InitialUnitSize, ReserveAmounts)},
					RA2 = case ReserveAmount of
						{_, Ru} when Ru > RA1 ->
							Ru - RA1;
						_ ->
							0
					end,
					Price2 = Price1#price{size = InitialUnitSize,
							amount = InitialUnitPrice},
					case charge3(Flag, Service, ServiceId, Product,
							Buckets, Price2, {Units2, DA2}, {Units2, RA2},
							SessionId, ChargingKey, false) of
						{{Units2, DA3}, {Units2, RA3}, NewBuckets1, undefined}
								when DA3 >= DA2, RA3 >= RA2 ->
							NewBuckets2 = lists:flatten([PriceBuckets2,
									NewAcc, NewBuckets1, OtherBuckets]),
							charge4(Flag, Service, ServiceId, Product, NewBuckets2,
									DebitAmount, {Units2, DA1 + DA3},
									ReserveAmount, {Units2, RA1 + RA3},
									SessionId, Rated, ChargingKey,
									OldBuckets);
						{{Units2, DA3}, {Units2, RA3},
								NewBuckets1, undefined} ->
							NewAcc1 = lists:flatten([NewBuckets1, NewAcc]),
							NewPrices = case PriceBuckets2 of
								[] ->
									drop_fixed(Prices);
								_ ->
									Prices
							end,
							charge2(Protocol, Flag, Service,
									ServiceId, Product, NewPrices,
									[DebitAmount], [ReserveAmount],
									{Units2, DA1 + DA3}, {Units2, RA1 + RA3},
									SessionId, ChargingKey, Address,
									ServiceNetwork, Rated, PriceBuckets2,
									OtherBuckets, NewAcc1, OldBuckets)
					end;
				undefined ->
					NewAcc1 = lists:flatten([Buckets, NewAcc]),
					NewPrices = case PriceBuckets2 of
						[] ->
							drop_fixed(Prices);
						_ ->
							Prices
					end,
					charge2(Protocol, Flag, Service,
							ServiceId, Product, NewPrices,
							DebitAmounts, ReserveAmounts,
							DebitedAmount, ReservedAmount,
							SessionId, ChargingKey, Address,
							ServiceNetwork, Rated, PriceBuckets2,
							OtherBuckets, NewAcc1, OldBuckets)
			end;
		false ->
			NewAcc1 = lists:flatten([Buckets, NewAcc]),
			NewPrices = case PriceBuckets2 of
				[] ->
					drop_fixed(Prices);
				_ ->
					Prices
			end,
			charge2(Protocol, Flag, Service,
					ServiceId, Product, NewPrices,
					DebitAmounts, ReserveAmounts,
					DebitedAmount, ReservedAmount,
					SessionId, ChargingKey, Address,
					ServiceNetwork, Rated, PriceBuckets2,
					OtherBuckets, NewAcc1, OldBuckets)
	end;
charge2(Protocol, final = Flag, Service, ServiceId, Product, Prices,
		DebitAmounts, ReserveAmounts,
		{Units1, DA1} = DebitedAmount, {_, 0} = ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated1,
		[#bucket{price = PriceName} | _] = PriceBuckets1,
		OtherBuckets, NewAcc, OldBuckets)
		when ReserveAmounts == undefined; ReserveAmounts == [] ->
	F = fun(#bucket{price = Pname}) when Pname == PriceName ->
				true;
		(_) ->
				false
	end,
	{Buckets, PriceBuckets2} = lists:partition(F, PriceBuckets1),
	case lists:keyfind(PriceName, #price.name, Prices) of
		#price{type = usage, units = Units2} = Price
				when Units1 == undefined; Units1 == Units2 ->
			DebitAmount = case lists:keyfind(Units2, 1, DebitAmounts) of
				Da when is_tuple(Da) ->
					Da;
				false ->
					{Units2, 0}
			end,
			DA2 = case DebitAmount of
				{_, Du} when Du > DA1 ->
					Du - DA1;
				_ ->
					0
			end,
			case charge3(Flag, Service, ServiceId,
					Product, Buckets, Price,
					{Units2, DA2}, {Units2, 0},
					SessionId, ChargingKey, false) of
				{{Units2, DA3}, {Units2, 0}, NewBuckets1, Rated2}
								when DA3 >= DA2 ->
					Rated3 = lists:flatten([Rated1, Rated2]),
					NewBuckets2 = lists:flatten([NewBuckets1,
							PriceBuckets2, NewAcc, OtherBuckets]),
					charge4(Flag, Service, ServiceId, Product, NewBuckets2,
							DebitAmount, {Units2, DA1 + DA3},
							{Units2, 0}, {Units2, 0}, SessionId, Rated3,
							ChargingKey, OldBuckets);
				{{Units2, DA3}, {Units2, 0}, NewBuckets1, Rated2}
						when DA3 < DA2 ->
					Rated3 = lists:flatten([Rated1, Rated2]),
					NewAcc1 = lists:flatten([NewBuckets1, NewAcc]),
					NewPrices = case PriceBuckets2 of
						[] ->
							drop_fixed(Prices);
						_ ->
							Prices
					end,
					charge2(Protocol, Flag, Service,
							ServiceId, Product, NewPrices,
							[DebitAmount], ReserveAmounts,
							{Units2, DA1 + DA3}, {Units2, 0},
							SessionId, ChargingKey, Address,
							ServiceNetwork, Rated3, PriceBuckets2,
							OtherBuckets, NewAcc1, OldBuckets)
			end;
		#price{type = tariff, units = Units2, size = UnitSize} = Price1
				when Units1 == undefined; Units1 == Units2 ->
			DebitAmount = case lists:keyfind(Units2, 1, DebitAmounts) of
				Da when is_tuple(Da) ->
					Da;
				false ->
					{Units2, 0}
			end,
			DA2 = case DebitAmount of
				{_, Du} when Du > DA1 ->
					Du - DA1;
				_ ->
					0
			end,
			case tariff_rate(Address, ServiceNetwork, Price1) of
				{_Description, InitialUnitSize, InitialUnitPrice, _, _}
						when UnitSize == undefined;
						UnitSize == InitialUnitSize ->
					Price2 = Price1#price{size = InitialUnitSize,
							amount = InitialUnitPrice},
					case charge3(Flag, Service, ServiceId, Product, Buckets,
							Price2, {Units2, DA2}, {Units2, 0},
							SessionId, ChargingKey, false) of
						{{Units2, DA3}, {Units2, 0}, NewBuckets1, Rated2}
								when DA3 >= DA2 ->
							Rated3 = lists:flatten([Rated1, Rated2]),
							NewBuckets2 = lists:flatten([NewBuckets1,
									PriceBuckets2, NewAcc, OtherBuckets]),
							charge4(Flag, Service, ServiceId, Product, NewBuckets2,
									DebitAmount, {Units2, DA1 + DA3},
									{Units2, 0}, {Units2, 0}, SessionId, Rated3,
									ChargingKey, OldBuckets);
						{{Units2, DA3}, {Units2, 0}, NewBuckets1, Rated2}
								when DA3 < DA2 ->
							Rated3 = lists:flatten([Rated1, Rated2]),
							NewAcc1 = lists:flatten([NewBuckets1, NewAcc]),
							NewPrices = case PriceBuckets2 of
								[] ->
									drop_fixed(Prices);
								_ ->
									Prices
							end,
							charge2(Protocol, Flag, Service,
									ServiceId, Product, NewPrices,
									[DebitAmount], ReserveAmounts,
									{Units2, DA1 + DA3}, ReservedAmount,
									SessionId, ChargingKey, Address,
									ServiceNetwork, Rated3, PriceBuckets2,
									OtherBuckets, NewAcc1, OldBuckets)
					end;
				undefined ->
					NewAcc1 = lists:flatten([Buckets, NewAcc]),
					NewPrices = case PriceBuckets2 of
						[] ->
							drop_fixed(Prices);
						_ ->
							Prices
					end,
					charge2(Protocol, Flag, Service,
							ServiceId, Product, NewPrices,
							DebitAmounts, ReserveAmounts,
							DebitedAmount, ReservedAmount,
							SessionId, ChargingKey, Address,
							ServiceNetwork, Rated1, PriceBuckets2,
							OtherBuckets, NewAcc1, OldBuckets)
			end;
		false ->
			NewAcc1 = lists:flatten([Buckets, NewAcc]),
			NewPrices = case PriceBuckets2 of
				[] ->
					drop_fixed(Prices);
				_ ->
					Prices
			end,
			charge2(Protocol, Flag, Service,
					ServiceId, Product, NewPrices,
					DebitAmounts, ReserveAmounts,
					DebitedAmount, ReservedAmount,
					SessionId, ChargingKey, Address,
					ServiceNetwork, Rated1, PriceBuckets2,
					OtherBuckets, NewAcc1, OldBuckets)
	end;
charge2(Protocol, event = Flag,
		Service, ServiceId, Product, Prices, [], DebitAmounts,
		{Units1, DA1} = DebitedAmount, {Units1, 0} = ReservedAmount,
		SessionId, ChargingKey, Address, ServiceNetwork, Rated1,
		[#bucket{price = PriceName} | _] = PriceBuckets1,
		OtherBuckets, NewAcc, OldBuckets) ->
	F = fun(#bucket{price = Pname}) when Pname == PriceName ->
				true;
		(_) ->
				false
	end,
	{Buckets, PriceBuckets2} = lists:partition(F, PriceBuckets1),
	case lists:keyfind(PriceName, #price.name, Prices) of
		#price{type = usage, units = Units2} = Price
				when Units1 == undefined; Units1 == Units2 ->
			DebitAmount = case lists:keyfind(Units2, 1, DebitAmounts) of
				Da when is_tuple(Da) ->
					Da;
				false ->
					{Units2, 0}
			end,
			DA2 = case DebitAmount of
				{_, Du} when Du > DA1 ->
					Du - DA1;
				_ ->
					0
			end,
			case charge3(Flag, Service, ServiceId, Product, Buckets,
					Price, {Units2, DA2}, {Units2, 0},
					SessionId, ChargingKey, false) of
				{{Units2, DA3}, {Units2, 0}, NewBuckets1, Rated2}
						when DA3 >= DA2 ->
					Rated3 = lists:flatten([Rated1, Rated2]),
					NewBuckets2 = lists:flatten([NewBuckets1,
							PriceBuckets2, NewAcc]),
					charge4(Flag, Service, ServiceId,
							Product, NewBuckets2,
							DebitAmount, {Units2, DA1 + DA3},
							{Units2, 0}, {Units2, 0},
							SessionId, Rated3,
							ChargingKey, OldBuckets);
				{{Units2, DA3}, {Units2, 0}, NewBuckets1, Rated2}
						when DA3 < DA2 ->
					Rated3 = lists:flatten([Rated1, Rated2]),
					NewAcc1 = lists:flatten([NewBuckets1, NewAcc]),
					NewPrices = case PriceBuckets2 of
						[] ->
							drop_fixed(Prices);
						_ ->
							Prices
					end,
					charge2(Protocol, Flag, Service, ServiceId,
							Product, NewPrices, [], [DebitAmount],
							{Units2, DA1 + DA3}, {Units2, 0},
							SessionId, ChargingKey, Address,
							ServiceNetwork, Rated3, PriceBuckets2,
							OtherBuckets, NewAcc1, OldBuckets)
			end;
		false ->
			NewAcc1 = lists:flatten([Buckets, NewAcc]),
			NewPrices = case PriceBuckets2 of
				[] ->
					drop_fixed(Prices);
				_ ->
					Prices
			end,
			charge2(Protocol, Flag, Service, ServiceId,
					Product, NewPrices, [], DebitAmounts,
					DebitedAmount, ReservedAmount,
					SessionId, ChargingKey, Address,
					ServiceNetwork, Rated1, PriceBuckets2,
					OtherBuckets, NewAcc1, OldBuckets)
	end.

%% @doc Apply debit and reserve to provided buckets.
%% @hidden
charge3(initial, _Service, ServiceId, _Product, Buckets,
		#price{units = Units, size = UnitSize,
				amount = UnitPrice} = _Price,
		{_, 0} = _DebitAmount, {Units, Amount} = _ReserveAmount,
		SessionId, ChargingKey, _Overflow) ->
	Rated = undefined,
	case update_session(Units, 0, Amount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{0, UnitsReserved, Buckets1} when UnitsReserved >= Amount ->
			{{Units, 0}, {Units, UnitsReserved}, Buckets1, Rated};
		{0, UnitsReserved, Buckets1} when UnitsReserved < Amount ->
			PriceReserveUnits = (Amount - UnitsReserved),
			{UnitReserve, PriceReserve} = price_units(PriceReserveUnits,
					UnitSize, UnitPrice),
			case convert(PriceReserve, Units, UnitPrice, UnitSize, UnitReserve,
					ServiceId, ChargingKey, SessionId, Buckets1) of
				{ok, Buckets2} ->
					{0, Amount, Buckets3} = update_session(Units, 0,
							Amount, ServiceId, ChargingKey,
							SessionId, Buckets2),
					{{Units, 0}, {Units, Amount}, Buckets3, Rated};
				false ->
					{{Units, 0}, {Units, UnitsReserved}, Buckets1, Rated}
			end
	end;
charge3(interim, #service{enabled = false} = _Service, ServiceId,
		#product{id = ProductId} = _Product, Buckets,
		#price{units = Units, size = UnitSize,
				amount = UnitPrice} = _Price,
		{Units, Amount} = DebitAmount, _ReserveAmount,
		SessionId, ChargingKey, Overflow) ->
	Rated = undefined,
	case update_session(Units, Amount, 0,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Amount, 0, Buckets1} ->
			{DebitAmount, {Units, 0}, Buckets1, Rated};
		{UnitsCharged, 0, Buckets1} when UnitsCharged < Amount ->
			NewChargeUnits = Amount - UnitsCharged,
			{UnitCharge, PriceCharge} = price_units(NewChargeUnits,
					UnitSize, UnitPrice),
			case convert(PriceCharge, Units, UnitPrice, UnitSize,
					UnitCharge, ServiceId, ChargingKey, SessionId,
					Buckets1) of
				{ok, Buckets2} ->
					case update_session(Units, NewChargeUnits, 0,
							ServiceId, ChargingKey,
							SessionId, Buckets2) of
						{NewChargeUnits, 0, Buckets3} ->
							{{Units, UnitsCharged + NewChargeUnits},
									{Units, 0}, Buckets3, Rated};
						{NewUnitsCharged, 0, Buckets3} when Overflow == false,
								NewUnitsCharged < NewChargeUnits ->
							{{Units, UnitsCharged + NewUnitsCharged},
									{Units, 0}, Buckets3, Rated};
						{NewUnitsCharged, 0, Buckets3} when Overflow == true,
								NewUnitsCharged < NewChargeUnits ->
							Now = erlang:system_time(millisecond),
							Reservation = #{ts => Now, reserve => 0,
									debit => NewChargeUnits - NewUnitsCharged,
									service_id => ServiceId, charging_key => ChargingKey},
							Attributes = #{bucket_type => normal,
									reservations => #{SessionId => Reservation}},
							LM = make_lm(),
							Buckets4 = [#bucket{id = make_id(LM), last_modified = LM,
									start_date = Now, end_date = Now,
									remain_amount = NewUnitsCharged - NewChargeUnits,
									attributes = Attributes, units = Units,
									product = [ProductId]} | Buckets3],
							{{Units, UnitsCharged + NewUnitsCharged},
									{Units, 0}, Buckets4, Rated}
					end;
				false when Overflow == false ->
					{{Units, UnitsCharged}, {Units, 0}, Buckets1, Rated};
				false when Overflow == true ->
					Now = erlang:system_time(millisecond),
					Reservation = #{ts => Now, reserve => 0,
							debit => NewChargeUnits,
							service_id => ServiceId, charging_key => ChargingKey},
					Attributes = #{bucket_type => normal,
							reservations => #{SessionId => Reservation}},
					LM = make_lm(),
					Buckets2 = [#bucket{id = make_id(LM), last_modified = LM,
							start_date = Now, end_date = Now,
							remain_amount = UnitsCharged - Amount,
							attributes = Attributes, units = Units,
							product = [ProductId]} | Buckets1],
					{{Units, UnitsCharged}, {Units, 0}, Buckets2, Rated}
			end
	end;
charge3(interim, _Service, ServiceId,
		#product{id = ProductId} = _Product, Buckets,
		#price{units = Units, size = UnitSize,
				amount = UnitPrice} = _Price,
		{Units, Damount} = DebitAmount,
		{Units, Ramount} = ReserveAmount,
		SessionId, ChargingKey, Overflow) ->
	Rated = undefined,
	case update_session(Units, Damount, Ramount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Damount, UnitsReserved, Buckets1} when UnitsReserved >= Ramount ->
			{DebitAmount, {Units, UnitsReserved}, Buckets1, Rated};
		{Damount, UnitsReserved, Buckets1} when UnitsReserved < Ramount ->
			NewReserveUnits = Ramount - UnitsReserved,
			{UnitReserve, PriceReserve} = price_units(NewReserveUnits,
					UnitSize, UnitPrice),
			case convert(PriceReserve, Units, UnitPrice, UnitSize, UnitReserve,
					ServiceId, ChargingKey, SessionId, Buckets1) of
				{ok, Buckets2} ->
					{0, Ramount, Buckets3} = update_session(Units,
							0, Ramount, ServiceId,
							ChargingKey, SessionId, Buckets2),
					{DebitAmount, ReserveAmount, Buckets3, Rated};
				false ->
					{DebitAmount, {Units, UnitsReserved}, Buckets1, Rated}
			end;
		{UnitsCharged, 0, Buckets1} when UnitsCharged < Damount ->
			NewChargeUnits = Damount - UnitsCharged,
			{ConvertReserve, PriceReserve} = price_units(NewChargeUnits + Ramount,
					UnitSize, UnitPrice),
			case convert(PriceReserve, Units, UnitPrice, UnitSize, ConvertReserve,
					ServiceId, ChargingKey, SessionId, Buckets1) of
				{ok, Buckets2} ->
					case update_session(Units, NewChargeUnits, Ramount,
							ServiceId, ChargingKey, SessionId, Buckets2) of
						{NewChargeUnits, UnitsReserved, Buckets3} ->
							{{Units, UnitsCharged + NewChargeUnits},
									{Units, UnitsReserved}, Buckets3, Rated};
						{NewUnitsCharged, 0, Buckets3} when Overflow == false;
								NewUnitsCharged < NewChargeUnits ->
							{{Units, UnitsCharged + NewUnitsCharged},
									{Units, 0}, Buckets3, Rated};
						{NewUnitsCharged, 0, Buckets3} when Overflow == true;
								NewUnitsCharged < NewChargeUnits ->
							Now = erlang:system_time(millisecond),
							Reservation = #{ts => Now, reserve => 0,
									debit => NewChargeUnits - NewUnitsCharged,
									service_id => ServiceId,
									charging_key => ChargingKey},
							Attributes = #{bucket_type => normal,
									reservations => #{SessionId => Reservation}},
							LM = make_lm(),
							Buckets4 = [#bucket{id = make_id(LM),
									last_modified = LM,
									start_date = Now, end_date = Now,
									remain_amount = NewUnitsCharged - NewChargeUnits,
									attributes = Attributes, units = Units,
									product = [ProductId]} | Buckets3],
							{{Units, UnitsCharged + NewUnitsCharged},
									{Units, 0}, Buckets4, Rated}
					end;
				false when Overflow == false ->
					{{Units, UnitsCharged}, {Units, 0}, Buckets1, Rated};
				false when Overflow == true ->
					Now = erlang:system_time(millisecond),
					Reservation = #{ts => Now, reserve => 0,
							debit => NewChargeUnits,
							service_id => ServiceId,
							charging_key => ChargingKey},
					Attributes = #{bucket_type => normal,
							reservations => #{SessionId => Reservation}},
					LM = make_lm(),
					Buckets2 = [#bucket{id = make_id(LM),
							last_modified = LM,
							start_date = Now, end_date = Now,
							remain_amount = UnitsCharged - Damount,
							attributes = Attributes, units = Units,
							product = [ProductId]} | Buckets1],
					{{Units, UnitsCharged}, {Units, 0}, Buckets2, Rated}
			end
	end;
charge3(final, _Service, ServiceId,
		#product{id = ProductId} = _Product, Buckets,
		#price{name = PriceName, type = PriceType,
				units = Units, size = UnitSize,
				amount = UnitPrice, currency = Currency} = _Price,
		{Units, Amount} = DebitAmount, {Units, 0} = ReserveAmount,
		SessionId, ChargingKey, Overflow) ->
	Rated1 = #rated{price_type = PriceType,
			price_name = PriceName, currency = Currency},
	case charge_session(Units, Amount,
			ServiceId, ChargingKey, SessionId, Buckets) of
		{Amount, Buckets1} ->
			{Debits, Buckets2} = get_final(ServiceId,
					ChargingKey, SessionId, {Units, 0}, Buckets1),
			Rated2 = rated(Debits, Rated1),
			{DebitAmount, ReserveAmount, Buckets2, Rated2};
		{UnitsCharged, Buckets1} when UnitsCharged < Amount ->
			{UnitCharge, PriceCharge} = price_units(Amount - UnitsCharged,
					UnitSize, UnitPrice),
			case charge_session(cents, PriceCharge,
					ServiceId, ChargingKey, SessionId, Buckets1) of
				{PriceCharge, Buckets2} ->
					{Debits, Buckets3} = get_final(ServiceId,
							ChargingKey, SessionId, {Units, 0}, Buckets2),
					Rated2 = rated(Debits, Rated1),
					{{Units, UnitsCharged + UnitCharge},
							ReserveAmount, Buckets3, Rated2};
				{PriceCharged, Buckets2} when PriceCharged < PriceCharge ->
					TotalUnits = UnitsCharged + ((PriceCharged div UnitPrice) * 60),
					{Debits, Buckets3} = get_final(ServiceId,
							ChargingKey, SessionId, {Units, 0}, Buckets2),
					Rated2 = rated(Debits, Rated1),
					Buckets4 = case Overflow of
						false ->
							Buckets3;
						true ->
							Now = erlang:system_time(millisecond),
							Reservation = #{ts => Now, reserve => 0,
									debit => PriceCharge - PriceCharged,
									service_id => ServiceId,
									charging_key => ChargingKey},
							Attributes = #{bucket_type => normal,
									reservations => #{SessionId => Reservation}},
							LM = make_lm(),
							[#bucket{id = make_id(LM),
									last_modified = LM, start_date = Now,
									remain_amount = PriceCharged - PriceCharge,
									attributes = Attributes, units = cents,
									product = [ProductId]} | Buckets3]
					end,
					{{Units, TotalUnits}, ReserveAmount, Buckets4, Rated2}
			end
	end;
charge3(event, _Service, ServiceId,
		#product{id = ProductId} = _Product, Buckets,
		#price{name = PriceName, type = PriceType,
				units = Units, size = UnitSize,
				amount = UnitPrice, currency = Currency} = _Price,
		{Units, Amount} = DebitAmount, {Units, 0} = ReserveAmount,
		SessionId, ChargingKey, Overflow) ->
	Rated1 = #rated{price_type = PriceType,
			price_name = PriceName, currency = Currency},
	case charge_event(Units, Amount, Buckets) of
		{Amount, Buckets1} ->
			Rated2 = rated(#{Units => Amount}, Rated1),
			{DebitAmount, ReserveAmount, Buckets1, Rated2};
		{UnitsCharged, Buckets1} when UnitsCharged < Amount ->
			{UnitCharge, PriceCharge} = price_units(Amount - UnitsCharged,
					UnitSize, UnitPrice),
			case charge_event(cents, PriceCharge, Buckets1) of
				{PriceCharge, Buckets2} ->
					Rated2 = rated(#{Units => Amount, cents => PriceCharge}, Rated1),
					{{Units, UnitsCharged + UnitCharge},
							ReserveAmount, Buckets2, Rated2};
				{PriceCharged, Buckets2} when PriceCharged < PriceCharge ->
					TotalUnits = UnitsCharged + ((PriceCharged div UnitPrice) * 60),
					Rated2 = rated(#{Units => Amount, cents => PriceCharged}, Rated1),
					Buckets3 = case Overflow of
						false ->
							Buckets2;
						true ->
							Now = erlang:system_time(millisecond),
							Reservation = #{ts => Now, reserve => 0,
									debit => PriceCharge - PriceCharged,
									service_id => ServiceId,
									charging_key => ChargingKey},
							Attributes = #{bucket_type => normal,
									reservations => #{SessionId => Reservation}},
							LM = make_lm(),
							[#bucket{id = make_id(LM), last_modified = LM,
									start_date = Now,
									remain_amount = PriceCharged - PriceCharge,
									attributes = Attributes, units = cents,
									product = [ProductId]} | Buckets2]
					end,
					{{Units, TotalUnits}, ReserveAmount, Buckets3, Rated2}
			end
	end.

%% @doc Finalize charging.
%% @hidden
charge4(final,
		#service{session_attributes = SessionList} = Service1, ServiceId,
		Product, Buckets, {Units, Charge}, {Units, Charged},
		{Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey,
		OldBuckets) when Charged >= Charge ->
	Refund = {Units, Charged - Charge},
	{Debits, NewBuckets} = get_final(ServiceId,
			ChargingKey, SessionId, Refund, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Rated1 = rated(Debits, Rated),
	NewSessionList = remove_session(SessionId, SessionList),
	Service2 = Service1#service{session_attributes = NewSessionList},
	ok = mnesia:write(Service2),
	{ok, Service2, Rated1, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge4(final,
		#service{session_attributes = SessionList} = Service1, ServiceId,
		Product, Buckets, {Units, _Charge}, {Units, _Charged},
		{Units, 0}, {Units, 0}, SessionId, Rated, ChargingKey, OldBuckets) ->
	{Debits, NewBuckets} = get_final(ServiceId,
			ChargingKey, SessionId, {Units, 0}, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Rated1 = rated(Debits, Rated),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, Rated1, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge4(_Flag,
		#service{enabled = false, session_attributes = SessionList} = Service1,
		ServiceId, Product, Buckets, {Units, _Charge}, {Units, _Charged},
		{Units, _Reserve}, {Units, _Reserved}, SessionId, _Rated,
		ChargingKey, OldBuckets) ->
% @todo charge final reported usage
	NewBuckets = refund(ServiceId, ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{disabled, SessionList, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge4(_Flag,
		#service{session_attributes = SessionList} = Service1, ServiceId,
		Product, Buckets, {Units, Charge}, {Units, Charged},
		{Units, Reserve}, {Units, Reserved}, SessionId, _Rated,
		ChargingKey, OldBuckets)
		when Charged < Charge; Reserved < Reserve ->
	NewBuckets = refund(ServiceId, ChargingKey, SessionId, Buckets),
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, NewBuckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	Service2 = Service1#service{session_attributes = []},
	ok = mnesia:write(Service2),
	{out_of_credit, SessionList, DeletedBuckets,
			accumulated_balance(NewBuckets, Product#product.id)};
charge4(initial,
		#service{session_attributes = SessionList} = Service1,
		_ServiceId, Product, Buckets, {Units, 0},
		{Units, 0}, {Units, _Reserve}, {Units, Reserved},
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
charge4(interim, Service, _ServiceId, Product,
		Buckets, {Units, _Charge}, {Units, _Charged},
		{Units, _Reserve}, {Units, Reserved}, _SessionId, _Rated,
		_ChargingKey, OldBuckets) ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	ok = mnesia:write(Service),
	{grant, Service, {Units, Reserved}, DeletedBuckets,
			accumulated_balance(Buckets, Product#product.id)};
charge4(event, Service, _ServiceId, Product, Buckets,
		{Units, Charge}, {Units, Charged}, {Units, 0}, {Units, 0},
		_SessionId, Rated, _ChargingKey, OldBuckets)
		when Charged >= Charge ->
	{NewBRefs, DeletedBuckets}
			= update_buckets(Product#product.balance, OldBuckets, Buckets),
	ok = mnesia:write(Product#product{balance = NewBRefs}),
	ok = mnesia:write(Service),
	{ok, Service, {Units, Charged}, Rated, DeletedBuckets,
			accumulated_balance(Buckets, Product#product.id)};
charge4(event,
		#service{session_attributes = SessionList} = Service1,
		_ServiceId, Product, Buckets, {Units, _Charge},
			{Units, _Charged}, {Units, 0}, {Units, 0},
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
				| out_of_credit | offer_not_found
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
											values = [CharValue]}) ->
										case CharValue of
											#char_value{units = undefined, value = RRST}
													when is_integer(RRST) ->
												{true, {seconds, RRST}};
											#char_value{units = "seconds", value = RRST}
													when is_integer(RRST) ->
												{true, {seconds, RRST}};
											#char_value{units = "minutes", value = RRST}
													when is_integer(RRST) ->
												{true, {seconds, RRST * 60}}
										end;
									(#char_value_use{name = "radiusReserveSessionOctets",
											values = [CharValue]}) ->
										case CharValue of
											#char_value{units = undefined, value = RRSO}
													when is_integer(RRSO) ->
												{true, {octets, RRSO}};
											#char_value{units = "bytes", value = RRSO}
													when is_integer(RRSO) ->
												{true, {octets, RRSO}};
											#char_value{units = "kilobytes", value = RRSO}
													when is_integer(RRSO) ->
												{true, {octets, RRSO * 1000}};
											#char_value{units = "megabytes", value = RRSO}
													when is_integer(RRSO) ->
												{true, {octets, RRSO * 1000000}};
											#char_value{units = "gigabytes", value = RRSO}
													when is_integer(RRSO) ->
												{true, {octets, RRSO * 1000000000}}
										end;
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
		_:Reason ->
			mnesia:abort(Reason)
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
				{_Description, Amount, _TS}
						when is_integer(Amount), Amount >= 0 ->
					authorize4(Protocol, ServiceType, Service, Buckets,
							Price#price{amount = Amount}, SessionAttributes,
							Reserve, ReserveUnits);
				{_Description, PeriodInitial, RateInitial,
						PeriodAdditional, RateAdditional, _TS}
						when is_integer(PeriodInitial),
						is_integer(RateInitial),
						is_integer(PeriodAdditional),
						is_integer(RateAdditional) ->
					authorize4(Protocol, ServiceType, Service, Buckets,
							Price#price{amount = RateInitial}, SessionAttributes,
							Reserve, ReserveUnits);
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
		#service{session_attributes = ExistingAttr,
		attributes = Attr} = Service, Buckets, #price{units = Units,
		size = UnitSize, amount = UnitPrice},
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

-spec update_session(Type, Charge, Reserve, ServiceId,
		ChargingKey, SessionId, Buckets) -> Result
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
			ServiceId, ChargingKey, SessionId, sort_by_age(Buckets), [], 0, 0).
%% @hidden
update_session(Type, Charge, Reserve, Now, ServiceId,
		ChargingKey, SessionId, [#bucket{end_date = Expires,
		attributes = Attributes, remain_amount = Remain} | T],
		Acc, Charged, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0,
		false == is_map_key(reservations, Attributes) ->
	update_session(Type, Charge, Reserve, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId,
		ChargingKey, SessionId, [H | T], Acc, Charged, Reserved)
		when Charge =:= Charged, Reserve =:= Reserved ->
	update_session(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, [H | Acc], Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId,
		ChargingKey, SessionId, [#bucket{units = Type,
		remain_amount = Remain, attributes = Attributes} = B | T],
		Acc, Charged, Reserved) ->
	Reservations = maps:get(reservations, Attributes, #{}),
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
			update_session(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged + NewCharge,
					Reserved + NewReserve);
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
			update_session(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged + NewCharge,
					Reserved + NewReserved);
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
			update_session(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged + ReservedAmount,
					Reserved);
		_Other ->
			update_session(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, [B | Acc], Charged, Reserved)
	end;
update_session(_, Charge, Reserve, _, _, _, _, [], Acc, Charged, Reserved)
		when Charge =:= Charged, Reserve =:= Reserved ->
	{Charged, Reserved, lists:reverse(Acc)};
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [H | T], Acc, Charged, Reserved) ->
	update_session(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, [H | Acc], Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [], Acc, Charged, Reserved) when Reserved < Reserve ->
	update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
			SessionId, lists:reverse(Acc), [], Charged, Reserved);
update_session(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
			SessionId, lists:reverse(Acc), [], Charged, Reserved).
%% @hidden
update_session1(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = Remain, attributes = Attributes} = B | T],
		Acc, Charged, Reserved) when Remain > 0,
		((Charge > Charged) or (Reserve > Reserved)), Remain > 0 ->
	Reservations = maps:get(reservations, Attributes, #{}),
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
			update_session1(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged + NewCharge,
					Reserved + NewReserve);
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
			update_session1(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged + NewCharge,
					Reserved + NewReserved);
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
			update_session1(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged + Remain,
					Reserved);
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount,
				service_id := ServiceId, charging_key := ChargingKey}}],
				NewReservations} when NewCharge =:= 0, Remain >= NewReserve ->
			ReservationList = [{SessionId, #{ts => Now, debit => DebitedAmount,
					reserve => ReservedAmount + NewReserve,
					service_id => ServiceId, charging_key => ChargingKey}}
					| NewReservations],
			NewAttributes = Attributes#{reservations
					=> maps:from_list(ReservationList)},
			NewAcc = [B#bucket{remain_amount = Remain - NewReserve,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = NewAttributes} | Acc],
			update_session1(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged,
					Reserved + NewReserve);
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
			update_session1(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, NewAcc, Charged,
					Reserved + Remain);
		{[], _} ->
			update_session1(Type, Charge, Reserve, Now, ServiceId,
					ChargingKey, SessionId, T, [B | Acc], Charged, Reserved)
	end;
update_session1(_, Charge, Reserve, _, _, _, _, Buckets, Acc, Charged,
		Reserved) when Charge =:= Charged, Reserve =:= Reserved ->
	{Charged, Reserved, lists:reverse(Acc) ++ Buckets};
update_session1(Type, Charge, Reserve, Now, ServiceId,
		ChargingKey, SessionId, [H | T], Acc, Charged, Reserved) ->
	update_session1(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, [H | Acc], Charged, Reserved);
update_session1(Type, Charge, Reserve, Now, ServiceId,
		ChargingKey, SessionId, [], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
			SessionId, lists:reverse(Acc), [], Charged, Reserved).

%% @hidden
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{end_date = Expires, attributes = Attributes,
		remain_amount = Remain} | T], Acc, Charged, Reserved)
		when Expires /= undefined, Expires =< Now, Remain >= 0,
		false == is_map_key(reservations, Attributes) ->
	update(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, Acc, Charged, Reserved);
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = Remain, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain >= ((Charge - Charged) + (Reserve - Reserved)) ->
	Reservations = maps:get(reservations, Attributes, #{}),
	NewCharge = Charge - Charged,
	NewReserve = Reserve - Reserved,
	NewReservation = Reservations#{SessionId => #{ts => Now,
			debit => NewCharge, reserve => NewReserve,
			service_id => ServiceId, charging_key => ChargingKey}},
	NewBuckets = [B#bucket{remain_amount = Remain - (NewCharge + NewReserve),
		last_modified = {Now, erlang:unique_integer([positive])},
		attributes = Attributes#{reservations => NewReservation}} | Acc],
	{Charged + NewCharge, Reserved + NewReserve,
			lists:reverse(NewBuckets) ++ T};
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = Remain, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain >= (Charge - Charged) ->
	Reservations = maps:get(reservations, Attributes, #{}),
	NewCharge = Charge - Charged,
	NewReserve = Remain - NewCharge,
	NewReservation = Reservations#{SessionId => #{ts => Now,
			debit => NewCharge, reserve => NewReserve,
			service_id => ServiceId, charging_key => ChargingKey}},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservation}}
			| Acc],
	update(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, NewAcc, Charged + NewCharge,
			Reserved + NewReserve);
update(Type, Charge, Reserve, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = Remain, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged, Reserved)
		when ((Expires == undefined) or (Now < Expires)),
		Remain > 0, Remain < (Charge - Charged) ->
	Reservations = maps:get(reservations, Attributes, #{}),
	NewReservation = Reservations#{SessionId => #{ts => Now,
			debit => Remain, reserve => 0, service_id => ServiceId,
			charging_key => ChargingKey}},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservation}}
			| Acc],
	update(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, NewAcc, Charged + Remain,
			Reserved);
update(_, Charge, Reserve, _, _, _, _, Buckets, Acc, Charged,
		Reserved) when Charge =:= Charged, Reserve =:= Reserved ->
	{Charged, Reserved, lists:reverse(Acc) ++ Buckets};
update(Type, Charge, Reserve, Now, ServiceId,
		ChargingKey, SessionId, [H | T], Acc, Charged, Reserved) ->
	update(Type, Charge, Reserve, Now, ServiceId,
			ChargingKey, SessionId, T, [H | Acc], Charged, Reserved);
update(_, _, _, _, _, _, _, [], Acc, Charged, Reserved) ->
	{Charged, Reserved, lists:reverse(Acc)}.

-spec charge_session(Type, Charge, ServiceId,
		ChargingKey, SessionId, Buckets) -> Result
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
			ServiceId, ChargingKey, SessionId, sort_by_age(Buckets), 0, []).
%% @hidden
charge_session(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{remain_amount = Remain,
		attributes = Attributes, start_date = Start,
		end_date = Expires} | T], Charged, Acc)
		when Remain >= 0, Expires /= undefined, Expires =/= Start,
		Expires =< Now, false == is_map_key(reservations, Attributes) ->
	charge_session(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, T, Charged, Acc);
charge_session(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = Remain, attributes = Attributes} = B | T],
		Charged, Acc) when Charge > 0 ->
	Reservations = maps:get(reservations, Attributes, #{}),
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
			charge_session(Type, 0, Now, ServiceId, ChargingKey,
					SessionId, T, Charged + Charge, NewAcc);
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
			charge_session(Type, 0, Now, ServiceId, ChargingKey,
					SessionId, T, Charged + Charge, NewAcc);
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
charge_session(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [H | T], Charged, Acc) ->
	charge_session(Type, Charge, Now, ServiceId, ChargingKey,
			SessionId, T, Charged, [H | Acc]);
charge_session(Type, Charge, Now,
		ServiceId, ChargingKey, SessionId, [], Charged, Acc) ->
	charge_session1(Type, Charge, Now, ServiceId, ChargingKey,
			SessionId, lists:reverse(Acc), [], Charged).

%% @hidden
charge_session1(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{end_date = Expires, attributes = Attributes,
		remain_amount = R} | T], Acc, Charged)
		when R >= 0, Expires /= undefined, Expires =< Now,
		false == is_map_key(reservations, Attributes) ->
	charge_session1(Type, Charge, Now,
			ServiceId, ChargingKey, SessionId, T, Acc, Charged);
charge_session1(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = R, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged)
		when Charge > 0, R >= Charge,
		((Expires == undefined) or (Now < Expires)) ->
	Reservations = maps:get(reservations, Attributes, #{}),
	NewReservations = Reservations#{SessionId => #{ts => Now,
			debit => Charge, reserve => 0, service_id => ServiceId,
			charging_key => ChargingKey}},
	NewBuckets = [B#bucket{remain_amount = R - Charge,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservations}} | T],
	{Charged + Charge, lists:reverse(Acc) ++ NewBuckets};
charge_session1(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [#bucket{units = Type,
		remain_amount = R, end_date = Expires,
		attributes = Attributes} = B | T], Acc, Charged)
		when Charge > 0, R =< Charge, R > 0,
		((Expires == undefined) or (Now < Expires)) ->
	Reservations = maps:get(reservations, Attributes, #{}),
	NewReservations = Reservations#{SessionId => #{ts => Now, debit => R,
			reserve => 0, service_id => ServiceId, charging_key => ChargingKey}},
	NewAcc = [B#bucket{remain_amount = 0,
			last_modified = {Now, erlang:unique_integer([positive])},
			attributes = Attributes#{reservations => NewReservations}} | Acc],
	charge_session1(Type, Charge - R, Now,
			ServiceId, ChargingKey, SessionId, T, NewAcc, Charged + R);
charge_session1(_Type, 0, _, _, _, _, Buckets, Acc, Charged) ->
	{Charged, lists:reverse(Acc) ++ Buckets};
charge_session1(Type, Charge, Now, ServiceId, ChargingKey,
		SessionId, [H | T], Acc, Charged) ->
	charge_session1(Type, Charge, Now, ServiceId, ChargingKey,
			SessionId, T, [H | Acc], Charged);
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
	charge_event(Type, Charge, Now, sort_by_age(Buckets), 0, []).
%% @hidden
charge_event(Type, Charge, Now,
		[#bucket{remain_amount = Remain, attributes = Attributes,
		start_date = Start, end_date = Expires} | T], Charged, Acc)
		when Remain >= 0, Expires /= undefined, Expires =/= Start,
		Expires =< Now, false == is_map_key(reservations, Attributes) ->
	charge_event(Type, Charge, Now, T, Charged, Acc);
charge_event(Type, Charge, Now,
		[#bucket{units = Type,
		remain_amount = Remain} = B | T], Charged, Acc)
		when Charge > 0, Remain >= Charge ->
	NewAcc = [B#bucket{remain_amount = Remain - Charge,
			last_modified = {Now, erlang:unique_integer([positive])}} | Acc],
	{Charged + Charge, lists:reverse(NewAcc) ++ T};
charge_event(Type, Charge, Now,
		[#bucket{units = Type,
		remain_amount = Remain} = B | T], Charged, Acc)
		when Charge > 0, Remain > 0, Remain < Charge ->
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

-spec convert(Price, Type, UnitPrice, UnitSize, TotalSize,
		ServiceId, ChargingKey, SessionId, Buckets) -> Result
	when
		Price :: pos_integer(),
		Type :: octets | seconds | messages,
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		UnitPrice :: pos_integer(),
		UnitSize :: pos_integer(),
		TotalSize :: pos_integer(),
		SessionId :: string() | binary(),
		Buckets :: [#bucket{}],
		Result :: {ok, Buckets} | false.
%% @doc Convert cents to `Type' bucket(s) of `Size'.
%%
%% 	Tops up existing bucket for session if found,
%% 	otherwise creates session bucket(s) of `Type'.
%% @private
convert(Price, Type, UnitPrice, UnitSize, TotalSize,
		ServiceId, ChargingKey, SessionId, Buckets) ->
	Buckets1 = sort_by_age(Buckets),
	Fcents = fun(#bucket{units = cents}) ->
				true;
			(_) ->
				false
	end,
	{CentsBuckets, UnitsBuckets} = lists:partition(Fcents, Buckets1),
	Now = erlang:system_time(millisecond),
	convert(Price, Type, UnitPrice, UnitSize, TotalSize, ServiceId, ChargingKey,
			SessionId, Now, CentsBuckets, UnitsBuckets, [], []).
%% @hidden
convert(0, Type, _UnitPrice, _UnitSize, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, CentsBuckets, UnitsBuckets, Acc, FBAcc) ->
	convert1(Type, TotalSize, ServiceId, ChargingKey, SessionId, Now,
			lists:reverse(Acc) ++ CentsBuckets, UnitsBuckets, FBAcc, []);
convert(Price, Type, UnitPrice, UnitSize, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, [#bucket{remain_amount = R, end_date = Expires,
		attributes = Attributes} | T], UnitsBuckets, Acc, FBAcc) when R >= 0,
		Expires /= undefined, Expires =< Now,
		false == is_map_key(reservations, Attributes) ->
	convert(Price, Type, UnitPrice, UnitSize, TotalSize, ServiceId, ChargingKey,
			SessionId, Now, T, UnitsBuckets, Acc, FBAcc);
convert(Price1, Type, UnitPrice, UnitSize, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, [#bucket{id = BId, remain_amount = R, end_date = Expires,
		attributes = Attributes} = B1 | T], UnitsBuckets, Acc, FBAcc1)
		when Price1 > 0, R > 0, ((Expires == undefined) or (Now < Expires)) ->
	Reservations1 = maps:get(reservations, Attributes, #{}),
	F = fun({SessionId1, #{service_id := ServiceId1, charging_key := ChargingKey1}})
					when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _}) ->
				false
	end,
	{Price2, B2, FBAcc2} = case lists:partition(F, maps:to_list(Reservations1)) of
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount}}], Reservations2}
				when R >= Price1 ->
			NewReservations = [{SessionId, #{ts => Now,
					debit => DebitedAmount + Price1, reserve => ReservedAmount,
					service_id => ServiceId,
					charging_key => ChargingKey}} | Reservations2],
			FromBucket = [#{id => BId, amount => Price1, unit_size => UnitSize,
					unit_price => UnitPrice, expire => Expires}] ++ FBAcc1,
			{0, B1#bucket{remain_amount = R - Price1,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => maps:from_list(NewReservations)}}, FromBucket};
		{[{_, #{debit := DebitedAmount, reserve := ReservedAmount}}], Reservations2}
				when R < Price1 ->
			NewReservations = [{SessionId, #{ts => Now, debit => DebitedAmount + R,
					reserve => ReservedAmount, service_id => ServiceId,
					charging_key => ChargingKey}} | Reservations2],
			FromBucket = [#{id => BId, amount => R, unit_size => UnitSize,
					unit_price => UnitPrice, expire => Expires}] ++ FBAcc1,
			{Price1 - R, B1#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => maps:from_list(NewReservations)}}, FromBucket};
		{[], _} when R >= Price1 ->
			NewReservations = Reservations1#{SessionId => #{ts => Now,
					debit => Price1, reserve => 0, service_id => ServiceId,
					charging_key => ChargingKey}},
			FromBucket = [#{id => BId, amount => Price1, unit_size => UnitSize,
					unit_price => UnitPrice, expire => Expires}] ++ FBAcc1,
			{0, B1#bucket{remain_amount = R - Price1,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => NewReservations}}, FromBucket};
		{[], _} when R < Price1 ->
			NewReservations = Reservations1#{SessionId => #{ts => Now, debit => R,
					reserve => 0, service_id => ServiceId,
					charging_key => ChargingKey}},
			FromBucket = [#{id => BId, amount => R, unit_size => UnitSize,
					unit_price => UnitPrice, expire => Expires}] ++ FBAcc1,
			{Price1 - R, B1#bucket{remain_amount = 0,
					last_modified = {Now, erlang:unique_integer([positive])},
					attributes = Attributes#{reservations => NewReservations}}, FromBucket}
	end,
	convert(Price2, Type, UnitPrice, UnitSize, TotalSize, ServiceId,
			ChargingKey, SessionId, Now, T, UnitsBuckets, [B2 | Acc], FBAcc2);
convert(Price, Type, UnitPrice, UnitSize, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, [H | T], UnitsBuckets, Acc, FBAcc) ->
	convert(Price, Type, UnitPrice, UnitSize, TotalSize, ServiceId, ChargingKey,
			SessionId, Now, T, UnitsBuckets, [H | Acc], FBAcc);
convert(Price, _, _, _, _, _, _, _, _, [], _, _, _) when Price > 0 ->
	false.
%% @hidden
convert1(Type, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, CentsBuckets, [#bucket{units = Type, remain_amount = R,
		attributes = #{bucket_type := session, from_bucket := FromBucket,
				reservations := Reservations}} = B | T], FBAcc, Acc) ->
	F = fun({SessionId1, #{service_id := ServiceId1,
					charging_key := ChargingKey1}}) when ServiceId1 =:= ServiceId,
					ChargingKey1 =:= ChargingKey, SessionId1 == SessionId ->
				true;
			({_, _}) ->
				false
	end,
	case lists:any(F, maps:to_list(Reservations)) of
		true ->
			Attributes = B#bucket.attributes,
			NewBucket = B#bucket{remain_amount = R + TotalSize,
					attributes = Attributes#{from_bucket => FBAcc ++ FromBucket},
					last_modified = {Now, erlang:unique_integer([positive])}},
			NewBuckets = CentsBuckets ++ lists:reverse(Acc) ++ [NewBucket | T],
			{ok, NewBuckets};
		false ->
			convert1(Type, TotalSize, ServiceId, ChargingKey,
					SessionId, Now, CentsBuckets, T, FBAcc, [B | Acc])
	end;
convert1(Type, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, CentsBuckets, [H | T], FBAcc, Acc) ->
	convert1(Type, TotalSize, ServiceId, ChargingKey,
			SessionId, Now, CentsBuckets, T, FBAcc, [H | Acc]);
convert1(Type, TotalSize, ServiceId, ChargingKey,
		SessionId, Now, CentsBuckets, [], FBAcc, Acc) ->
	F = fun(#bucket{attributes = #{reservations := Reservations}}) ->
				case lists:keyfind(SessionId, 1, maps:to_list(Reservations)) of
					{SessionId, _} ->
						true;
					false ->
						false
				end;
			(#bucket{}) ->
				false
	end,
	[#bucket{product = Product, price = PriceName}
			| _] = lists:filter(F, CentsBuckets),
	LM = make_lm(),
	NewBucket = #bucket{id = make_id(LM), last_modified = LM,
			start_date = Now, end_date = Now,
			name = "session", product = Product, price = PriceName,
			remain_amount = TotalSize, units = Type,
			attributes = #{bucket_type => session, from_bucket => FBAcc,
					reservations => #{SessionId => #{ts => Now, debit => 0,
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

-spec split_by_price(Buckets) -> Result
	when
		Buckets :: [bucket()],
		Result :: {PriceBuckets, OtherBuckets},
		PriceBuckets :: [bucket()],
		OtherBuckets :: [bucket()].
%% @doc Split out buckets with price name.
%% @private
split_by_price(Buckets) ->
	F = fun(#bucket{price = Name}) when length(Name) > 0 ->
				true;
			(#bucket{}) ->
				false
	end,
	{PriceBuckets, OtherBuckets} = lists:partition(F, Buckets),
	{sort_by_age(PriceBuckets), sort_by_age(OtherBuckets)}.

-spec sort_by_age(Buckets) -> Buckets
	when
		Buckets :: [#bucket{}].
%% @doc Sort `Buckets' with soonest to expire first.
%% @private
sort_by_age(Buckets) ->
	F = fun(#bucket{end_date = T1}, #bucket{end_date = T2})
					when is_integer(T1), is_integer(T2), T1 =< T2 ->
				true;
			(#bucket{end_date = T1}, #bucket{end_date = undefined})
					when is_integer(T1) ->
				true;
			(_, _)->
				false
	end,
	lists:sort(F, Buckets).

-spec sort_from_bucket(FromBucket) -> FromBucket
	when
		FromBucket :: [bucket_source()].
%% @doc Sort `BucketSource' with soonest to expire first.
%% @private
sort_from_bucket(FromBucket) when is_list(FromBucket) ->
	F = fun(#{expire := T1}, #{expire := T2})
					when is_integer(T1), is_integer(T2), T1 =< T2 ->
				true;
			(#{expire := T1}, #{expire := undefined})
					when is_integer(T1) ->
				true;
			(_, _) ->
				false
	end,
	lists:sort(F, FromBucket).

-spec price_units(Amount, UnitSize, UnitPrice) -> {TotalUnits, TotalPrice}
	when
		Amount :: non_neg_integer(),
		UnitSize :: pos_integer(),
		UnitPrice :: pos_integer(),
		TotalUnits :: pos_integer(),
		TotalPrice :: pos_integer().
%% @doc Calculate total size and price.
%% @private
price_units(0, _UnitSize, _UnitPrice) ->
	{0, 0};
price_units(_Amount, _UnitSize, 0) ->
	{0, 0};
price_units(Amount, UnitSize, UnitPrice) when (Amount rem UnitSize) == 0 ->
	{Amount, UnitPrice * (Amount div UnitSize)};
price_units(Amount, UnitSize, UnitPrice) ->
	Units = (Amount div UnitSize) + 1,
	{Units * UnitSize, UnitPrice * Units}.

-spec radius_reserve(Units, CharValueUse) -> ReserveAmount
	when
		Units :: seconds | octets,
		CharValueUse :: [tuple()],
		ReserveAmount :: {Units, Amount},
		Amount :: pos_integer().
%% @doc Get the reserve amount.
%% @hidden
radius_reserve(seconds, CharValueUse) ->
	case lists:keyfind("radiusReserveTime",
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = CharValue} ->
			case CharValue of
				[#char_value{units = undefined, value = Value}]
						when is_integer(Value) ->
					{seconds, Value};
				[#char_value{units = "seconds", value = Value}]
						when is_integer(Value) ->
					{seconds, Value};
				[#char_value{units = "minutes", value = Value}]
						when is_integer(Value) ->
					{seconds, Value * 60}
			end;
		false ->
			{seconds, 0}
	end;
radius_reserve(octets, CharValueUse) ->
	case lists:keyfind("radiusReserveOctets",
			#char_value_use.name, CharValueUse) of
		#char_value_use{values = CharValue} ->
			case CharValue of
				[#char_value{units = undefined, value = Value}]
						when is_integer(Value) ->
					{octets, Value};
				[#char_value{units = "octets", value = Value}]
						when is_integer(Value) ->
					{octets, Value};
				[#char_value{units = "bytes", value = Value}]
						when is_integer(Value) ->
					{octets, Value};
				[#char_value{units = "kilobytes", value = Value}]
						when is_integer(Value) ->
					{octets, Value * 1000};
				[#char_value{units = "megabytes", value = Value}]
						when is_integer(Value) ->
					{octets, Value * 1000000};
				[#char_value{units = "gigabytes", value = Value}]
						when is_integer(Value) ->
					{octets, Value * 1000000000}
			end;
		false ->
			{octets, 0}
	end.

-spec reserve_amount(Units, UnitSize, ReserveAmounts) -> ReserveAmount
	when
		Units :: seconds | octets | messages,
		UnitSize:: pos_integer(),
		ReserveAmounts :: [{Units, UnitSize}] | undefined,
		ReserveAmount :: pos_integer().
%% @doc Get the reserve amount.
%% @hidden
reserve_amount(Units, UnitSize, [{Units, ReserveSize} | _])
		when ReserveSize > UnitSize ->
	reserve_amount(Units, ReserveSize);
reserve_amount(Units, UnitSize, [{Units, _ReserveSize} | _]) ->
	reserve_amount(Units, UnitSize);
reserve_amount(Units, UnitSize, [_ | T]) ->
	reserve_amount(Units, UnitSize, T);
reserve_amount(Units, UnitSize, []) ->
	reserve_amount(Units, UnitSize);
reserve_amount(_Units, _UnitSize, undefined) ->
	0.
%% @hidden
reserve_amount(octets = _Units, UnitSize) ->
	case application:get_env(ocs, min_reserve_octets) of
		{ok, Value} when Value < UnitSize ->
			UnitSize;
		{ok, Value} ->
			Value
	end;
reserve_amount(seconds = _Units, UnitSize) ->
	case application:get_env(ocs,min_reserve_seconds) of
		{ok, Value} when Value < UnitSize ->
			UnitSize;
		{ok, Value} ->
			Value
	end;
reserve_amount(messages = _Units, UnitSize) ->
	case application:get_env(ocs, min_reserve_messages) of
		{ok, Value} when Value < UnitSize ->
			UnitSize;
		{ok, Value} ->
			Value
	end.

-spec refund(ServiceId, ChargingKey, SessionId, Buckets) -> Buckets
	when
		Buckets :: [#bucket{}],
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		SessionId :: string() | binary().
%% @doc Refund unused reservations.
%% @private
refund(ServiceId, ChargingKey, SessionId, Buckets) ->
	refund(ServiceId, ChargingKey, SessionId, Buckets, []).
%% @hidden
refund(ServiceId, ChargingKey, SessionId,
		[#bucket{attributes = Attributes} = H | T], Acc) ->
	Reservations = maps:get(reservations, Attributes, #{}),
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
filter_prices_tod(_Timestamp, [], Acc) ->
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

-spec get_final(ServiceId, ChargingKey, SessionId,
		Refund, Buckets) -> Result
	when
		ServiceId :: non_neg_integer() | undefined,
		ChargingKey :: non_neg_integer() | undefined,
		SessionId :: [tuple()],
		Refund :: {RefundUnits, RefundAmount},
		RefundUnits :: octets | seconds | messages,
		RefundAmount :: non_neg_integer(),
		Buckets :: [#bucket{}],
		Result :: {Debits, NewBuckets},
		Debits :: #{},
		NewBuckets :: [#bucket{}].
%% @doc Get total debited and remaining amounts, refund and
%% 	remove all reservations, for session.
%% @private
get_final(ServiceId, ChargingKey, SessionId, Refund, Buckets) ->
	F = fun(#bucket{attributes = #{bucket_type := session,
					reservations := Reservations}})
					when is_map_key(SessionId, Reservations) ->
				true;
			(#bucket{}) ->
				false
	end,
	{SessionBuckets, OtherBuckets} = lists:partition(F, Buckets),
	Now = erlang:system_time(millisecond),
	get_final(ServiceId, ChargingKey, SessionId,
			Refund, Now, #{}, SessionBuckets, OtherBuckets, []).
%% @hidden
get_final(ServiceId, ChargingKey, SessionId,
		{Units, RefundAmount} = _Refund, Now, Debits,
		[#bucket{units = Units, remain_amount = Remain,
				attributes = Attributes} = B | T] = _SessionBuckets,
		OtherBuckets, Acc) when RefundAmount > 0,
				is_map_key(from_bucket, Attributes) ->
	get_final(ServiceId, ChargingKey, SessionId,
			{Units, 0}, Now, Debits,
			[B#bucket{remain_amount = Remain + RefundAmount} | T],
			OtherBuckets, Acc);
get_final(ServiceId, ChargingKey, SessionId, Refund, Now, Debits,
		[#bucket{remain_amount = Remain,
				attributes = #{reservations := Reservations1,
						from_bucket := From} = Attributes1,
				units = Units} = B1 | T] = _SessionBuckets,
		OtherBuckets1, Acc) ->
	N = maps:get(Units, Debits, 0),
	case get_debits(ServiceId, ChargingKey, SessionId, Reservations1) of
		{Debited, 0, Reservations2} when Remain == 0,
				map_size(Reservations2) == 0  ->
			get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
					Debits#{Units => N + Debited}, T, OtherBuckets1, Acc);
		{Debited, Reserved, Reservations2} when Reserved > 0,
				map_size(Reservations2) == 0  ->
			OtherBuckets2 = get_final1(Remain + Reserved,
					Now, OtherBuckets1, From),
			get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
					Debits#{Units => N + Debited}, T, OtherBuckets2, Acc);
		{Debited, Reserved, Reservations2}
				when map_size(Reservations2) == 0 ->
			Attributes2 = maps:remove(reservations, Attributes1),
			B2 = B1#bucket{remain_amount = Remain + Reserved,
					attributes = Attributes2},
			get_final(ServiceId, ChargingKey, SessionId,
					Refund, Now, Debits#{Units => N + Debited},
					T, OtherBuckets1, [B2 | Acc]);
		{Debited, Reserved, Reservations2} ->
			Attributes2 = Attributes1#{reservations => Reservations2},
			B2 = B1#bucket{remain_amount = Remain + Reserved,
					attributes = Attributes2},
			get_final(ServiceId, ChargingKey, SessionId,
					Refund, Now, Debits#{Units => N + Debited},
					T, OtherBuckets1, [B2 | Acc])
	end;
get_final(ServiceId, ChargingKey, SessionId, Refund, Now, Debits,
		[], [#bucket{remain_amount = 0,
				attributes = Attributes} = _B | T], Acc)
		when is_map_key(reservations, Attributes) == false ->
	get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
			Debits, [], T, Acc);
get_final(ServiceId, ChargingKey, SessionId, Refund, Now, Debits,
		[], [#bucket{remain_amount = Remain, attributes = Attributes,
		end_date = Expires} | T], Acc)
		when Remain >= 0, Expires /= undefined, Expires < Now,
		is_map_key(reservations, Attributes) == false ->
	get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
			Debits, [], T, Acc);
get_final(ServiceId, ChargingKey, SessionId, Refund, Now, Debits,
		[], [#bucket{attributes = Attributes} = B | T], Acc)
		when is_map_key(reservations, Attributes) == false ->
	get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
			Debits, [], T, [B | Acc]);
get_final(ServiceId, ChargingKey, SessionId, Refund, Now, Debits,
		[], [#bucket{remain_amount = Remain, units = Units,
				attributes = #{reservations := Reservations1} = Attributes1,
				end_date = EndDate} = B1 | T], Acc) ->
	N = maps:get(Units, Debits, 0),
	case get_debits(ServiceId, ChargingKey, SessionId, Reservations1) of
		{Debited, 0, Reservations2} when Remain == 0,
				map_size(Reservations2) == 0 ->
			get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
					Debits#{Units => N + Debited}, [], T, Acc);
		{Debited, Reserved, Reservations2}
				when Remain + Reserved >= 0,
				EndDate /= undefined, EndDate < Now,
				map_size(Reservations2) == 0 ->
			get_final(ServiceId, ChargingKey, SessionId, Refund, Now,
					Debits#{Units => N + Debited}, [], T, Acc);
		{Debited, Reserved, Reservations2}
				when map_size(Reservations2) == 0 ->
			Attributes2 = maps:remove(reservations, Attributes1),
			B2 = B1#bucket{remain_amount = Remain + Reserved,
					attributes = Attributes2},
			get_final(ServiceId, ChargingKey, SessionId,
					Refund, Now, Debits#{Units => N + Debited},
					[], T, [B2 | Acc]);
		{Debited, Reserved, Reservations2} ->
			Attributes2 = Attributes1#{reservations => Reservations2},
			B2 = B1#bucket{remain_amount = Remain + Reserved,
					attributes = Attributes2},
			get_final(ServiceId, ChargingKey, SessionId,
					Refund, Now, Debits#{Units => N + Debited},
					[], T, [B2 | Acc])
	end;
get_final(_, _, _, _, _, Debits, [], [], Acc) ->
	{Debits, lists:reverse(Acc)}.

%% @hidden
get_final1(RefundUnits, Now, Buckets, From) ->
	get_final2(RefundUnits, Now, Buckets, sort_from_bucket(From)).
%% @hidden
get_final2(RefundUnits, Now, Buckets,
		[#{id := Id, amount := Amount, unit_size := UnitSize,
		unit_price := UnitPrice} | T] = _From)
		when RefundUnits >= UnitSize ->
	FromUnits = (Amount div UnitPrice) * UnitSize,
	{RefundedAmount, RefundedUnits} = case FromUnits =< RefundUnits of
		true ->
			{Amount, FromUnits};
		false ->
			RU = RefundUnits div UnitSize,
			{RU * UnitPrice, RU * UnitSize}
	end,
	case lists:keyfind(Id, #bucket.id, Buckets) of
		#bucket{remain_amount = Remain} = B1 ->
			LM = {Now, erlang:unique_integer([positive])},
			B2 = B1#bucket{remain_amount = Remain + RefundedAmount,
					last_modified = LM},
			NewBuckets = lists:keyreplace(Id, #bucket.id, Buckets, B2),
			get_final2(RefundUnits - RefundedUnits,
					Now, NewBuckets, T);
		false ->
			get_final2(RefundUnits, Now, Buckets, T)
	end;
get_final2(RefundUnits, Now, Buckets, [_H | T]) ->
	get_final2(RefundUnits, Now, Buckets, T);
get_final2(0, _, Buckets, _) ->
	Buckets;
get_final2(_, _, Buckets, []) ->
	Buckets.

%% @hidden
get_debits(ServiceId, ChargingKey, SessionId, Reservations) ->
	get_debits(ServiceId, ChargingKey, SessionId,
			maps:to_list(Reservations), 0, 0, []).
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
	{Debit, Refund, maps:from_list(Acc)}.

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
rated(Debits, #rated{} = Rated)
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

-spec tariff_rate(Address, ServiceNetwork, Price) -> Result
	when
		Address :: [$0..$9] | undefined,
		ServiceNetwork :: [$0..$9] | undefined,
		Price :: #price{},
		Result :: {Description, InitialUnitSize, InitialUnitPrice,
				AdditionalUnitSize, AdditionalUnitPrice} | undefined,
		Description :: string() | undefined,
		InitialUnitSize :: pos_integer(),
		InitialUnitPrice :: non_neg_integer(),
		AdditionalUnitSize :: pos_integer(),
		AdditionalUnitPrice :: non_neg_integer().
%% @hidden
tariff_rate(undefined = _Address, ServiceNetwork,
		#price{type = tariff, size = UnitSize,
				char_value_use = PriceChars} = _Price) ->
	RoamingTable = case lists:keyfind("roamingTable",
			#char_value_use.name, PriceChars) of
		#char_value_use{values = [#char_value{value = RT}]} ->
			RT;
		false ->
			undefined
	end,
	case {RoamingTable, ServiceNetwork} of
		{RoamingTable, ServiceNetwork}
				when is_list(RoamingTable), is_list(ServiceNetwork) ->
			Table = list_to_existing_atom(RoamingTable),
			case catch ocs_gtt:lookup_last(Table, ServiceNetwork) of
				{Description, UnitPrice, _TS} ->
					{Description, UnitSize, UnitPrice, UnitSize, UnitPrice};
				undefined ->
					undefined
			end;
		{RoamingTable, undefined} when is_list(RoamingTable) ->
			mnesia:abort(undefined_service_network);
		{undefined, _} ->
			mnesia:abort(undefined_tariff)
	end;
tariff_rate(Address, ServiceNetwork,
		#price{type = tariff, size = UnitSize,
				char_value_use = PriceChars} = _Price)
		when is_list(Address) ->
	RoamingTable = case lists:keyfind("roamingTable",
			#char_value_use.name, PriceChars) of
		#char_value_use{values = [#char_value{value = RT}]} ->
			RT;
		false ->
			undefined
	end,
	DestinationTable = case lists:keyfind("destPrefixTariffTable",
			#char_value_use.name, PriceChars) of
		#char_value_use{values = [#char_value{value = DT}]} ->
			DT;
		false ->
			undefined
	end,
	case {RoamingTable, ServiceNetwork, DestinationTable} of
		{undefined, _, TariffTable} when is_list(TariffTable) ->
			Table = list_to_existing_atom(TariffTable),
			case catch ocs_gtt:lookup_last(Table, Address) of
				{Description, UnitPrice, _TS}
						when is_integer(UnitPrice), UnitPrice >= 0 ->
					{Description, UnitSize, UnitPrice, UnitSize, UnitPrice};
				{Description, PeriodInitial, RateInitial,
						PeriodAdditional, RateAdditional, _TS}
						when UnitSize == undefined,
						is_integer(PeriodInitial),
						is_integer(RateInitial),
						is_integer(PeriodAdditional),
						is_integer(RateAdditional) ->
					{Description, PeriodInitial, RateInitial,
							PeriodAdditional, RateAdditional};
				undefined ->
					undefined
			end;
		{RoamingTable, undefined, _} when is_list(RoamingTable) ->
			mnesia:abort(undefined_service_network);
		{RoamingTable, ServiceNetwork, TariffTable}
				when is_list(RoamingTable), is_list(TariffTable) ->
			Table1 = list_to_existing_atom(RoamingTable),
			case catch ocs_gtt:lookup_last(Table1, ServiceNetwork) of
				{_DescriptionSN, TablePrefix, _TS1}
						when is_list(TablePrefix) ->
					Table2 = list_to_existing_atom(TablePrefix
							++ "-" ++ TariffTable),
					case catch ocs_gtt:lookup_last(Table2, Address) of
						{Description, UnitPrice, _TS2}
								when is_integer(UnitPrice), UnitPrice >= 0 ->
							{Description, UnitSize, UnitPrice, UnitSize, UnitPrice};
						{Description, PeriodInitial, RateInitial,
								PeriodAdditional, RateAdditional, _TS2}
								when UnitSize == undefined,
								is_integer(PeriodInitial),
								is_integer(RateInitial),
								is_integer(PeriodAdditional),
								is_integer(RateAdditional) ->
							{Description, PeriodInitial, RateInitial,
									PeriodAdditional, RateAdditional}
					end;
				undefined ->
					undefined
			end;
		{_, _, undefined} ->
			mnesia:abort(undefined_tariff)
	end.

%% @hidden
drop_fixed(Prices) ->
	Filter = fun(#price{char_value_use = CharValueUse}) ->
			case lists:keyfind("fixedPriceBucket",
					#char_value_use.name, CharValueUse) of
				#char_value_use{values = [#char_value{value = true}]} ->
					false;
				#char_value_use{values = [#char_value{value = false}]} ->
					true;
				false ->
					true
			end
	end,
	lists:filter(Filter, Prices).

