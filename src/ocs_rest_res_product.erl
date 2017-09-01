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

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

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
product_offering_price(POfPrice) ->
	try
		F = fun({struct, Object}, AccIn) ->
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
				ProdPriceType = price_type(ProdPriceTypeS),
				{ProdUnits, ProdSize} = product_unit_of_measure(ProdUOMesasure),
				RCPeriod = recurring_charge_period(RCPeriodS),
				Price1 = #price{name = ProdName, description = ProdDescirption,
					type = ProdPriceType, units = ProdUnits, size = ProdSize,
					currency = CurrencyCode, period = RCPeriod, validity = ProdValidity,
					amount = ProdAmount},
				case lists:keyfind("productOfferPriceAlteration", 1, Object) of
					false ->
						[Price1 | AccIn];
					{_, {struct, ProdAlterObj}} ->
						{_, ProdAlterName} = lists:keyfind("name", 1, ProdAlterObj),
						{_, {struct, ProdAlterVFObj}} = lists:keyfind("validFor", 1, ProdAlterObj),
						{_, ProdAlterSTimeISO} = lists:keyfind("startDateTime", 1, ProdAlterVFObj),
						{_, ProdAlterPriceTypeS} = lists:keyfind("priceType", 1, ProdAlterObj),
						{_, ProdAlterUOMeasure} = lists:keyfind("unitOfMeasure", 1, ProdAlterObj),
						{_, {struct, ProdAlterPriceObj}} = lists:keyfind("price", 1, ProdAlterObj),
						{_, ProdAlterAmount} = lists:keyfind("taxIncludedAmount", 1,  ProdAlterPriceObj),
						ProdAlterDescirption = proplists:get_value("description", ProdAlterObj, ""),
						{ProdAlterUnits, ProdAlterSize} = product_unit_of_measure(ProdAlterUOMeasure),
						ProdAlterSTime = timestamp(ProdAlterSTimeISO),
						ProdAlterPriceType = price_type(ProdAlterPriceTypeS),
						Alteration = #alteration{name = ProdAlterName, description = ProdAlterDescirption,
							units = ProdAlterUnits , size = ProdAlterSize, amount = ProdAlterAmount},
						Price2 = Price1#price{alteration = Alteration},
						[Price2 | AccIn]
					end
		end,
		AccOut = lists:foldl(F, [], POfPrice),
		lists:reverse(AccOut)
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
	case {timestamp(ISOSTime), timestamp(ISOETime)} of
		{STime, ETime} when is_integer(STime),
				is_integer(ETime) ->
			ETime - STime;
		_ ->
			{error, format_error}
	end.

-spec product_unit_of_measure(UnitsOfMeasure) -> Result
	when
		UnitsOfMeasure	:: string(),
		Result			:: {Units, Size},
		Units				:: unit_of_measure(),
		Size				:: pos_integer().
%% @doc return units type and size of measurement of a product
%% @private
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
	Suffix = "cents",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{cents, list_to_integer(Size)};
		false ->
			product_unit_of_measure4(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure4(UnitsOfMeasure) ->
	Suffix = "seconds",
	case lists:suffix(Suffix, UnitsOfMeasure) of
		true ->
			[Size] = string:tokens(UnitsOfMeasure, Suffix),
			{seconds, list_to_integer(Size)};
		false ->
			product_unit_of_measure5(UnitsOfMeasure)
	end.
%% @hidden
product_unit_of_measure5(_UnitsOfMeasure) ->
	{octets, 0}.

-spec recurring_charge_period(RCPeriod) -> Result
	when
		RCPeriod	:: string(),
		Result	:: valid_period().
%% @doc return valid period
%% @private
recurring_charge_period("") ->
	undefined;
recurring_charge_period("yearly") ->
	yearly;
recurring_charge_period("monthly") ->
	monthly;
recurring_charge_period("weekly") ->
	weekly;
recurring_charge_period("daily") ->
	daily.

-spec find_status(StringStatus) -> Status when
	StringStatus	:: string(),
	Status			:: product_status().
%% @doc return life cycle status of the product
%% @private
find_status("created") ->
	created;
find_status("aborted") ->
	aborted;
find_status("cancelled") ->
	cancelled;
find_status("active") ->
	active;
find_status("pending_active") ->
	pending_active;
find_status("suspended") ->
	suspended;
find_status("terminate") ->
	terminate;
find_status("pending_terminate") ->
	pending_terminate.

-spec price_type(StringPriceType) -> PriceType when
	StringPriceType :: string(),
	PriceType		 :: recurring | one_time | usage.
%% @private
price_type("recurring") ->
	recurring;
price_type("one_time") ->
	one_time;
price_type("usage") ->
	usage.

-spec date(DateTimeFormat) -> Result
	when
		DateTimeFormat	:: pos_integer() | tuple(),
		Result			:: calendar:datetime().
%% @doc Convert timestamp to date and time or
%%	date and time to timeStamp.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH.

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds	:: pos_integer(),
		Result			:: string().
%% @doc Convert timestamp to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars).

-spec timestamp(ISO8610) -> Result
	when
		ISO8610	:: string(),
		Result	:: pos_integer() | {error, Reason},
		Reason	:: term().
%% @doc Convert ISO 8601 format date and time to timestamp.
timestamp(ISO8610) when is_list(ISO8610) ->
	DateFormat = "~4d-~2d-~2d",
	TimeFormat = "T~2d:~2d:~2d.~3dZ",
	case io_lib:fread(DateFormat ++ TimeFormat, ISO8610) of
		{ok, [Y, M, D, H, Min, S, MSR], _} ->
			DateTime = {{Y, M, D}, {H, Min, S + MSR}},
			date(DateTime);
		{error, Reason} ->
			{error, Reason}
	end.
