%%% ocs_rest_res_balance.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_product).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).

-export([add_product/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec add_product(ReqData) -> Result when
	ReqData	:: [tuple()],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 500 .
%% @doc Respond to `POST /catalogManagement/v1/product' and
%% add a new `product'
add_product(ReqData) ->
	try
		{struct, Object} = mochijson:decode(ReqData),
		{_, Name} = lists:keyfind("name", 1, Object),
		IsBundle = case lists:keyfind("isBundle", 1, Object) of
			{"isBundle", "true"} -> true;
			_ -> false
		end,
		Status  = case lists:keyfind("lifecycleStatus", 1, Object) of
			{_, FindStatus} ->
				find_status(FindStatus);
			false ->
				"active"
		end,
		Descirption = proplists:get_value("description", Object, ""),
		{_, {array, ProdOfPrice}} = lists:keyfind("productOfferingPrice", 1, Object),
		case product_offering_price(ProdOfPrice) of
			{error, StatusCode} ->
				{error, StatusCode};
			Price ->
				Product = #product{price = Price, name = Name, is_bundle = IsBundle,
				status = Status, description = Descirption},
				case add_product1(Product) of
					ok ->
						add_product2(Object);
					{error, StatusCode} ->
						{error, StatusCode}
				end
		end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
add_product1(Products) ->
	F1 = fun() ->
		ok = mnesia:write(product, Products, write)
	end,
	case mnesia:transaction(F1) of
		{atomic, ok} ->
			ok;
		{aborted, _} ->
			{error, 500}
	end.
%% @hidden
add_product2(JsonResponse) ->
	Id = {id, ""},
	Json = {struct, [Id | JsonResponse]},
	Body = mochijson:encode(Json),
	Location = "/catalogManagement/v1/product", %% ++ id
	Headers = [{location, Location}],
	{ok, Headers, Body}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec product_offering_price(POfPrice) -> Result when
	POfPrice	:: list(),
	Result 	:: Prices | {error, StatusCode},
	Prices	:: [#price{}],
	StatusCode	:: 400.
%% @doc construct list of product
%% @private
product_offering_price([]) ->
	{error, 400};
product_offering_price(POfPrice) ->
	po_price(POfPrice, []).
%% @hidden
po_price([], Prices) ->
	Prices;
po_price([{struct, Object} | T], Prices) ->
	try
		{_, ProdName} = lists:keyfind("name", 1, Object),
		{_,  {struct, VFObj}} = lists:keyfind("validFor", 1, Object),
		{_, ProdSTime} = lists:keyfind("startDateTime", 1, VFObj),
		{_, ProdETime} = lists:keyfind("endDateTime", 1, VFObj),
		{_, ProdPriceTypeS} = lists:keyfind("priceType", 1, Object),
		{_, {struct, ProdPriceObj}} = lists:keyfind("price", 1, Object),
		{_, ProdAmount} = lists:keyfind("taxIncludedAmount", 1, ProdPriceObj),
		{_, CurrencyCode} = lists:keyfind("currencyCode", 1, ProdPriceObj),
		{_, RCPeriodS} = lists:keyfind("recurringChargePeriod", 1, Object),
		ProdDescirption = proplists:get_value("description", Object, ""),
		ProdUOMesasure = proplists:get_value("unitOfMeasure", Object, ""),
		ProdValidity = validity_period(ProdSTime, ProdETime),
		if
			ProdValidity =/= {error, format_error} ->
				ProdPriceType = price_type(ProdPriceTypeS),
				{ProdUnits, ProdSize} = product_unit_of_measure(ProdUOMesasure),
				Size = product_size(ProdUnits, octets, ProdSize),
				RCPeriod = recurring_charge_period(RCPeriodS),
				Price1 = #price{name = ProdName, description = ProdDescirption,
				type = ProdPriceType, units = ProdUnits, size = Size,
					currency = CurrencyCode, period = RCPeriod, validity = ProdValidity,
					amount = ProdAmount},
				case lists:keyfind("productOfferPriceAlteration", 1, Object) of
					false ->
						po_price(T, [Price1 | Prices]);
					{_, {struct, ProdAlterObj}} ->
						case po_alteration(ProdAlterObj) of
							{error, Status} ->
								{error, Status};
							Alteration ->
								Price2 = Price1#price{alteration = Alteration},
								po_price(T, [Price2 | Prices])
						end
				end;
			true ->
				{error, 400}
		end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
po_alteration(ProdAlterObj) ->
	try
		{_, ProdAlterName} = lists:keyfind("name", 1, ProdAlterObj),
		{_, {struct, ProdAlterVFObj}} = lists:keyfind("validFor", 1, ProdAlterObj),
		{_, ProdAlterSTimeISO} = lists:keyfind("startDateTime", 1, ProdAlterVFObj),
		{_, ProdAlterPriceTypeS} = lists:keyfind("priceType", 1, ProdAlterObj),
		{_, ProdAlterUOMeasure} = lists:keyfind("unitOfMeasure", 1, ProdAlterObj),
		{_, {struct, ProdAlterPriceObj}} = lists:keyfind("price", 1, ProdAlterObj),
		{_, ProdAlterAmount} = lists:keyfind("taxIncludedAmount", 1,  ProdAlterPriceObj),
		ProdAlterDescirption = proplists:get_value("description", ProdAlterObj, ""),
		{ProdAlterUnits, ProdAlterSize} = product_unit_of_measure(ProdAlterUOMeasure),
		AlterSize = product_size(ProdAlterUnits, octets, ProdAlterSize),
		ProdAlterSTime = ocs_rest:timestamp(ProdAlterSTimeISO),
		ProdAlterPriceType = price_type(ProdAlterPriceTypeS),
		#alteration{name = ProdAlterName, description = ProdAlterDescirption,
			units = ProdAlterUnits, size = AlterSize, amount = ProdAlterAmount}
	catch
		_:_ ->
			{error, 400}
	end.

-spec validity_period(StartTime, EndTime) -> Result
	when
		StartTime	:: string(),
		EndTime		:: string(),
		Result		:: pos_integer() | {error, Reason},
		Reason		:: term().
%% @doc return validity period of a product in milliseconds.
%% @private
validity_period(ISOSTime, ISOETime) when is_list(ISOSTime),
		is_list(ISOETime) ->
	case {ocs_rest:timestamp(ISOSTime), ocs_rest:timestamp(ISOETime)} of
		{{error, _}, _} ->
			{error, format_error};
		{_, {error, _}} ->
			{error, format_error};
		{STime, ETime} ->
			ETime - STime
	end.

-spec product_unit_of_measure(UnitsOfMeasure) -> Result
	when
		UnitsOfMeasure	:: string(),
		Result			:: {Units, Size},
		Units				:: undefined | unit_of_measure(),
		Size				:: undefined | pos_integer().
%% @doc return units type and size of measurement of a product
%% @private
product_unit_of_measure("") ->
	{undefined, undefined};
product_unit_of_measure(UnitsOfMeasure) ->
	LowerUOM = string:to_lower(UnitsOfMeasure),
	product_unit_of_measure1(LowerUOM).
%% @hidden
product_unit_of_measure1(UnitsOfMeasure) ->
	Suffix = "octets",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{octets, list_to_integer(Size)};
		false ->
			product_unit_of_measure2(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure2(UnitsOfMeasure) ->
	Suffix = "gb",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{gb, list_to_integer(Size)};
		false ->
			product_unit_of_measure3(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure3(UnitsOfMeasure) ->
	Suffix = "mb",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{mb, list_to_integer(Size)};
		false ->
			product_unit_of_measure4(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure4(UnitsOfMeasure) ->
	Suffix = "cents",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{cents, list_to_integer(Size)};
		false ->
			product_unit_of_measure5(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure5(UnitsOfMeasure) ->
	Suffix = "seconds",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{seconds, list_to_integer(Size)};
		false ->
			product_unit_of_measure6(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure6(_UnitsOfMeasure) ->
	{undefined, undefined}.

-spec product_size(UnitsFrom, UnitsTo, Size) -> Result
	when
		UnitsFrom	:: undefined | atom(), % gb | mb | second | cents
		UnitsTo		:: octets,
		Size			:: undefined | pos_integer(),
		Result		:: integer().
%% @private
product_size(UnitsFrom, octets, Size) when
		UnitsFrom == undefined; Size == undefinedi ->
	0;
product_size(gb, octets, Size) -> Size * 1000000000;
product_size(mb, octets, Size) -> Size * 1000000;
product_size(_, _, Size) -> Size.

-spec recurring_charge_period(RCPeriod) -> Result
	when
		RCPeriod	:: string(),
		Result	:: valid_period().
%% @doc return valid period
%% @private
recurring_charge_period("") -> undefined;
recurring_charge_period("yearly") -> yearly;
recurring_charge_period("monthly") -> monthly;
recurring_charge_period("weekly") -> weekly;
recurring_charge_period("daily") -> daily.

-spec find_status(StringStatus) -> Status when
	StringStatus	:: string(),
	Status			:: product_status().
%% @doc return life cycle status of the product
%% @private
find_status("active") -> active;
find_status("created") -> created;
find_status("aborted") -> aborted;
find_status("cancelled") -> cancelled;
find_status("suspended") -> suspended;
find_status("terminate") -> terminate;
find_status("pending_active") -> pending_active;
find_status("pending_terminate") -> pending_terminate.

-spec price_type(StringPriceType) -> PriceType when
	StringPriceType :: string(),
	PriceType		 :: recurring | one_time | usage.
%% @private
price_type("usage") -> usage;
price_type("recurring") -> recurring;
price_type("one_time") -> one_time.

