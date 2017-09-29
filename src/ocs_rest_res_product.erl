%%% ocs_rest_res_product.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_product).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).

-export([add_product_CatMgmt/1, add_product_InvMgmt/1]).
-export([get_product_CatMgmt/1, get_products_CatMgmt/2]).
-export([on_patch_product_CatMgmt/3, merge_patch_product_CatMgmt/3]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json",
	"application/merge-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec add_product_CatMgmt(ReqData) -> Result when
	ReqData	:: [tuple()],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 500 .
%% @doc Respond to `POST /catalogManagement/v1/productOffering' and
%% add a new `product'
add_product_CatMgmt(ReqData) ->
	try
		{struct, Object} = mochijson:decode(ReqData),
		Name = prod_name(erl_term, Object),
		IsBundle = prod_isBundle(erl_term, Object),
		Status = prod_status(erl_term, Object),
		ValidFor = prod_vf(erl_term, Object),
		Descirption = prod_description(erl_term, Object),
		StartDate = prod_sdate(erl_term, Object),
		TerminationDate = prod_tdate(erl_term, Object),
		case prod_offering_price(erl_term, Object) of
			{error, StatusCode} ->
				{error, StatusCode};
			Price ->
				Product = #product{price = Price, name = Name, valid_for = ValidFor,
					is_bundle = IsBundle, status = Status, start_date = StartDate,
					termination_date = TerminationDate, description = Descirption},
				case add_product_CatMgmt1(Product) of
					ok ->
						add_product_CatMgmt2(Name, Object);
					{error, StatusCode} ->
						{error, StatusCode}
				end
		end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
add_product_CatMgmt1(Product) ->
	case ocs:add_product(Product) of
		ok ->
			ok;
		{error, _} ->
			{error, 500}
	end.

%% @hidden
add_product_CatMgmt2(ProdId, JsonResponse) ->
	Id = {id, ProdId},
	Json = {struct, [Id | JsonResponse]},
	Body = mochijson:encode(Json),
	Location = "/catalogManagement/v1/productuOffering/" ++ ProdId,
	Headers = [{location, Location}],
	{ok, Headers, Body}.

-spec add_product_InvMgmt(ReqData) -> Result when
	ReqData	:: [tuple()],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 500 .
%% @doc Respond to `POST /productInventoryManagement/v1/product' and
%% add a new `product'
add_product_InvMgmt(ReqData) ->
	try
		{struct, Object} = mochijson:decode(ReqData),
		Headers = [{content_type, "application/json"}],
		{ok, Headers, []}
	catch
		_:_ ->
			{error, 400}
	end.

-spec get_product_CatMgmt(ProdID) -> Result when
	ProdID	:: string(),
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v1/productOffering/{id}' and
%% retrieve a `product' details
get_product_CatMgmt(ProductID) ->
	case ocs:find_product(ProductID) of
		{ok, Product} ->
			get_product_CatMgmt1(Product);
		{error, not_found} ->
			{error, 404};
		{error, _} ->
			{error, 500}
	end.
%% @hidden
get_product_CatMgmt1(Prod) ->
	ID = prod_id(json, Prod),
	Descirption = prod_description(json, Prod),
	Href = prod_href(json, Prod),
	ValidFor = prod_vf(json, Prod),
	IsBundle = prod_isBundle(json, Prod),
	Name = prod_name(json, Prod),
	Status = prod_status(json, Prod),
	StartDate = prod_sdate(json, Prod),
	TerminationDate = prod_tdate(json, Prod),
	case prod_offering_price(json, Prod) of
		{error, StatusCode} ->
			{error, StatusCode};
		OfferPrice ->
			Json = {struct, [ID, Descirption, Href, StartDate,
				TerminationDate, IsBundle, Name, Status, ValidFor,
				OfferPrice]},
			Body = mochijson:encode(Json),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	end.

-spec get_products_CatMgmt(Query, Headers) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v1/productOffering' and
%% retrieve all `product' details
%% @todo Filtering
get_products_CatMgmt(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_products_CatMgmt1(NewQuery, Filters, Headers);
		false ->
			get_products_CatMgmt1(Query, [], Headers)
	end.
%% @hidden
get_products_CatMgmt1(Query, Filters, Headers) ->
	case {lists:keyfind("if-match", 1, Headers),
			lists:keyfind("if-range", 1, Headers),
			lists:keyfind("range", 1, Headers)} of
		{{"if-match", Etag}, false, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					{ok, MaxItems} = application:get_env(ocs, rest_page_size),
					query_page(PageServer, Etag, Query, Filters, 1, MaxItems)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(Query, Filters, Start, End)
					end;
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", Range}} ->
			case ocs_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Query, Filters, Start, End)
			end;
		{false, false, false} ->
			{ok, MaxItems} = application:get_env(ocs, rest_page_size),
			query_start(Query, Filters, 1, MaxItems)
	end.

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	Name = proplists:get_value("name", 1, Filters),
	Des = proplists:get_value("description", 1, Filters),
	Status = proplists:get_value("status", 1, Filters),
	SDT = proplists:get_value("startDate", 1, Filters),
	EDT = proplists:get_value("endDate", 1, Filters),
	Price = proplists:get_value("price", 1, Filters),
	case supervisor:start_child(ocs_rest_pagination_sup,
				[[ocs, query_product, [Name, Des, Status, SDT, EDT, Price]]]) of
		{ok, PageServer, Etag} ->
			query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

query_page(PageServer, Etag, Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try
				case lists:keytake("sort", 1, Query) of
					{value, {_, "name"}, Q1} ->
						{lists:keysort(#product.name, Events), Q1};
					{value, {_, "-name"}, Q1} ->
						{lists:reverse(lists:keysort(#product.name, Events)), Q1};
					{value, {_, "description"}, Q1} ->
						{lists:keysort(#product.description, Events), Q1};
					{value, {_, "-description"}, Q1} ->
						{lists:reverse(lists:keysort(#product.description, Events)), Q1};
					{value, {_, "status"}, Q1} ->
						{lists:keysort(#product.status, Events), Q1};
					{value, {_, "-status"}, Q1} ->
						{lists:reverse(lists:keysort(#product.status, Events)), Q1};
					{value, {_, "startDate"}, Q1} ->
						{lists:keysort(#product.start_date, Events), Q1};
					{value, {_, "-startDate"}, Q1} ->
						{lists:reverse(lists:keysort(#product.start_date, Events)), Q1};
					{value, {_, "endDate"}, Q1} ->
						{lists:keysort(#product.termination_date, Events), Q1};
					{value, {_, "-endtDate"}, Q1} ->
						{lists:reverse(lists:keysort(#product.termination_date, Events)), Q1};
					{value, {_, "price"}, Q1} ->
						{lists:keysort(#product.price, Events), Q1};
					{value, {_, "-price"}, Q1} ->
						{lists:reverse(lists:keysort(#product.price, Events)), Q1};
					false ->
						{Events, Query};
					_ ->
						throw(400)
				end
			of
				{SortedEvents, _NewQuery} ->
					JsonObj = query_page1(lists:map(fun product_json/1, SortedEvents), Filters, []),
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end.
%% @hidden
query_page1([], _, Acc) ->
	lists:reverse(Acc);
query_page1(Json, [], Acc) ->
	lists:reverse(Json ++ Acc);
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:filter(Filters, H) | Acc]).

product_json(Prod) when is_record(Prod, product)->
	ID = prod_id(json, Prod),
	Descirption = prod_description(json, Prod),
	Href = prod_href(json, Prod),
	ValidFor = prod_vf(json, Prod),
	IsBundle = prod_isBundle(json, Prod),
	Name = prod_name(json, Prod),
	Status = prod_status(json, Prod),
	StartDate = prod_sdate(json, Prod),
	TerminationDate = prod_tdate(json, Prod),
	case prod_offering_price(json, Prod) of
		{error, StatusCode} ->
			throw(StatusCode);
		OfferPrice ->
			{struct, [ID, Descirption, Href, StartDate,
				TerminationDate, IsBundle, Name, Status, ValidFor,
				OfferPrice]}
	end.

-spec on_patch_product_CatMgmt(ProdId, Etag, ReqData) -> Result
	when
		ProdId	:: string(),
		Etag		:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 500 .
%% @doc Respond to `PATCH /catalogManagement/v1/productOffering/{id}' and
%% apply object notation patch for `product'
%% RFC6902 `https://tools.ietf.org/html/rfc6902'
on_patch_product_CatMgmt(ProdId, Etag, ReqData) ->
	try
		{array, OpList} = mochijson:decode(ReqData),
		case exe_jsonpatch_ON(ProdId, Etag, OpList) of
			{error, StatusCode} ->
				{error, StatusCode};
			{ok, Prod} ->
				ID = prod_id(json, Prod),
				Descirption = prod_description(json, Prod),
				Href = prod_href(json, Prod),
				ValidFor = prod_vf(json, Prod),
				IsBundle = prod_isBundle(json, Prod),
				Name = prod_name(json, Prod),
				Status = prod_status(json, Prod),
				StartDate = prod_sdate(json, Prod),
				TerminationDate = prod_tdate(json, Prod),
				case prod_offering_price(json, Prod) of
					{error, StatusCode} ->
						{error, StatusCode};
					OfferPrice ->
						Json = {struct, [ID, Descirption, Href, StartDate,
						TerminationDate, IsBundle, Name, Status, ValidFor,
						OfferPrice]},
						Body = mochijson:encode(Json),
						Headers = [{content_type, "application/json"}],
						{ok, Headers, Body}
				end
		end
	catch
		_:_ ->
			{error, 400}
	end.

-spec merge_patch_product_CatMgmt(ProdId, Etag, ReqData) -> Result
	when
		ProdId	:: string(),
		Etag		:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 500 .
%% @doc Respond to `PATCH /catalogManagement/v1/productOffering/{id}' and
%% apply merge patch for `product'
%% RFC7386 `https://tools.ietf.org/html/rfc7386'
merge_patch_product_CatMgmt(ProdId, Etag, ReqData) ->
	try
		Json = mochijson:decode(ReqData),
		case exe_jsonpatch_merge(ProdId, Etag, Json) of
			{error, Reason} ->
				{error, Reason};
			{ok, Response} ->
				Body = mochijson:encode(Response),
				Headers = [{content_type, "application/json"}],
				{ok, Headers, Body}
		end
	catch
		_:_ ->
			{error, 400}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------
-spec prod_offering_price(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: [#price{}] | list() | {error, Status},
		Status	:: 400.
%% @doc construct list of product
%% @private
prod_offering_price(erl_term, []) ->
	{error, 400};
prod_offering_price(erl_term, Json) ->
	{_, {array, ProdOfPrice}} = lists:keyfind("productOfferingPrice", 1, Json),
	case po_price(erl_term, ProdOfPrice, []) of
		{error, Status} ->
			{error, Status};
		Prices ->
			Prices
	end;
prod_offering_price(json, Product) ->
	case po_price(json, Product#product.price, []) of
		{error, Status} ->
			{error, Status};
		ProdOfPrice ->
			{"productOfferingPrice", {array, ProdOfPrice}}
	end.

-spec po_price(Prefix, ProductOfPrice, Prices) -> Result
	when
		Prefix	:: erl_term | json,
		ProductOfPrice	:: list() | [#price{}],
		Prices	::	list(),
		Result	:: [#price{}] | list() | {error, Status},
		Status	:: 400 | 500.
%% @hidden
po_price(erl_term, [], Prices) ->
	Prices;
po_price(erl_term, [{struct, Object} | T], Prices) ->
	try
		ProdName = prod_price_name(erl_term, Object),
		{_ProdSTime, _ProdETime} = prod_price_vf(erl_term, Object),
		ProdPriceType = prod_price_type(erl_term, Object),
		{_, {struct, ProdPriceObj}} = lists:keyfind("price", 1, Object),
		ProdAmount = prod_price_price_amount(erl_term, ProdPriceObj),
		CurrencyCode = prod_price_price_c_code(erl_term, ProdPriceObj),
		ProdVF = prod_price_vf(erl_term, Object),
		RCPeriod = prod_price_rc_period(erl_term, Object),
		ProdDescirption = prod_price_description(erl_term, Object),
		{ProdUnits, ProdSize} = prod_price_ufm(erl_term, Object),
		Size = product_size(ProdUnits, octets, ProdSize),
		Price1 = #price{name = ProdName, description = ProdDescirption,
				type = ProdPriceType, units = ProdUnits, size = Size, valid_for = ProdVF,
				currency = CurrencyCode, period = RCPeriod, %validity = ProdValidity,
				amount = ProdAmount},
		case lists:keyfind("productOfferPriceAlteration", 1, Object) of
			false ->
				po_price(erl_term, T, [Price1 | Prices]);
			{_, {struct, ProdAlterObj}} ->
				case po_alteration(erl_term, ProdAlterObj) of
					{error, Status} ->
						{error, Status};
					Alteration ->
						Price2 = Price1#price{alteration = Alteration},
						po_price(erl_term, T, [Price2 | Prices])
				end
		end
	catch
		_:_ ->
			{error, 400}
	end;
po_price(json, [], Prices) ->
	Prices;
po_price(json, [Price | T], Prices) when is_record(Price, price) ->
	try
		Name = prod_price_name(json, Price),
		ValidFor = prod_price_vf(json, Price),
		PriceType = prod_price_type(json, Price),
		Amount = prod_price_price_amount(json, Price),
		CurrencyCode = prod_price_price_c_code(json, Price),
		PriceObj = {"price", {struct, [Amount, CurrencyCode]}},
		RCPeriod = prod_price_rc_period(json, Price),
		Description = prod_price_description(json, Price),
		UOMeasure = prod_price_ufm(json, Price),
		if
			Price#price.alteration == undefined ->
				Price1 = {struct, [Name, Description, ValidFor,
					PriceType, PriceObj, UOMeasure, RCPeriod]},
				po_price(json, T, [Price1 | Prices]);
			true ->
				case po_alteration(json, Price#price.alteration) of
					{error, Status} ->
						{error, Status};
					Alteration ->
						Price1 = {struct, [Name, Description, PriceType,
							ValidFor, PriceObj, UOMeasure, RCPeriod, Alteration]},
						po_price(json, T, [Price1 | Prices])
				end
		end
	catch
		_:_ ->
			{error, 500}
	end.

-spec po_alteration(Prefix, ProdAlterObj) -> Result
	when
		Prefix	:: erl_term | json,
		ProdAlterObj :: list() | #alteration{},
		Result	:: #alteration{} | {error, Status},
		Status	:: 400 | 500.
%% @private
po_alteration(erl_term, ProdAlterObj) ->
	try
		ProdAlterName = prod_price_alter_name(erl_term, ProdAlterObj),
		ProdAlterVF = prod_price_alter_vf(erl_term, ProdAlterObj),
		ProdAlterPriceType = prod_price_alter_price_type(erl_term, ProdAlterObj),
		{_, {struct, ProdAlterPriceObj}} = lists:keyfind("price", 1, ProdAlterObj),
		ProdAlterAmount = prod_price_alter_amount(erl_term, ProdAlterPriceObj),
		ProdAlterDescirption = prod_price_alter_description(erl_term, ProdAlterObj),
		{ProdAlterUnits, ProdAlterSize} = prod_price_alter_ufm(erl_term, ProdAlterObj),
		AlterSize = product_size(ProdAlterUnits, octets, ProdAlterSize),
		#alteration{name = ProdAlterName, description = ProdAlterDescirption,
			valid_for = ProdAlterVF, units = ProdAlterUnits, size = AlterSize,
			amount = ProdAlterAmount, type = ProdAlterPriceType}
	catch
		_:_ ->
			{error, 400}
	end;
po_alteration(json, ProdAlter) ->
	try
		Name = prod_price_alter_name(json, ProdAlter),
		ValidFor = prod_price_alter_vf(json, ProdAlter),
		PriceType = prod_price_alter_price_type(json, ProdAlter),
		UFM  = prod_price_alter_ufm(json, ProdAlter),
		Description = prod_price_alter_description(json, ProdAlter),
		Amount = prod_price_alter_amount(json, ProdAlter),
		PriceObj = {struct, [Amount]},
		Price = {"price", PriceObj},
			{"productOfferPriceAlteration",
				{struct, [Name, Description, PriceType, ValidFor, UFM, Price]}}
	catch
		_:_ ->
			{error, 500}
	end.

-spec prod_id(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: string() | tuple().
%% @private
prod_id(json, Product) ->
	{"id", Product#product.name}.

-spec prod_name(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: string() | tuple().
%% @private
prod_name(erl_term, Product) ->
	{_, Name} = lists:keyfind("name", 1, Product),
	Name;
prod_name(json, Prod) ->
	{"name", Prod#product.name}.

-spec prod_description(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: undefined | string() | tuple().
%% @private
prod_description(erl_term, Product) ->
	proplists:get_value("description", Product, undefined);
prod_description(json, Product) ->
	case Product#product.description of
		undefined ->
			{"description", ""};
		Des ->
			{"description", Des}
	end.

-spec prod_href(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: undefined | string() | tuple().
%% @private
prod_href(json, Product) ->
	{"href", "/product/product/" ++ Product#product.name}.

-spec prod_isBundle(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: boolean() | tuple().
%% @private
prod_isBundle(erl_term, Product) ->
	case lists:keyfind("isBundle", 1, Product) of
		{"isBundle", "true"} -> true;
		{"isBundle", true} -> true;
		_ -> false
	end;
prod_isBundle(json, Product) ->
	case Product#product.is_bundle of
		undefined ->
			{"isBundle", ""};
		IsBundle ->
			{"isBundle", IsBundle}
	end.

-spec prod_status(Prefix, Product) -> Result
	when
		Prefix	:: erl_term | json,
		Product	:: list() | #product{},
		Result	:: string() | tuple().
%% @private
prod_status(erl_term, Product) ->
	case lists:keyfind("lifecycleStatus", 1, Product) of
		{_, FindStatus} ->
			find_status(FindStatus);
		false ->
			undefined
	end;
prod_status(json, Product) ->
	case Product#product.status of
		undefined ->
			{"lifecycleStatus", ""};
		Status ->
			{"lifecycleStatus", Status}
	end.

-spec prod_sdate(Prefix, Product) -> Result
	when
		Prefix :: erl_term | json,
		Product :: list() | #product{},
		Result :: undefined | tuple().
%% @private
prod_sdate(erl_term, Product) ->
	case lists:keyfind("startDate", 1, Product) of
		{_, SD} ->
			ocs_rest:iso8601(SD);
		false ->
			undefined
	end;
prod_sdate(json, Product) ->
	case Product#product.start_date of
		undefined ->
			{"startDate", ""};
		SD ->
			{"startDate", ocs_rest:iso8601(SD)}
	end.

-spec prod_tdate(Prefix, Product) -> Result
	when
		Prefix :: erl_term | json,
		Product :: list() | #product{},
		Result :: undefined | tuple().
%% @private
prod_tdate(erl_term, Product) ->
	case lists:keyfind("terminationDate", 1, Product) of
		{_, SD} ->
			ocs_rest:iso8601(SD);
		false ->
			undefined
	end;
prod_tdate(json, Product) ->
	case Product#product.termination_date of
		undefined ->
			{"terminationDate", ""};
		SD ->
			{"terminationDate", ocs_rest:iso8601(SD)}
	end.

-spec prod_vf(Prefix, Product) -> Result
	when
		Prefix :: erl_term | json,
		Product :: list() | #product{},
		Result :: tuple().
%% @private
prod_vf(erl_term, Product) ->
	case lists:keyfind("validFor", 1, Product) of
		{_, {struct, VFObj}} ->
			case {proplists:get_value("startDateTime", VFObj), proplists:get_value("endDateTime", VFObj)} of
				{undefined, undefined} ->
					{undefined, undefined};
				{undefined, EDT} ->
					{undefined, ocs_rest:iso8601(EDT)};
				{SDT, undefined} ->
					{ocs_rest:iso8601(SDT), undefined};
				{SDT, EDT} ->
					{ocs_rest:iso8601(SDT), ocs_rest:iso8601(EDT)}
			end;
		false ->
			{undefined, undefined}
	end;
prod_vf(json, Product) ->
	case Product#product.valid_for of
		{undefined, undefined} ->
			{"validFor", {struct, []}};
		{SDateTime, undefined} ->
			SDT = {"startDateTime", ocs_rest:iso8601(SDateTime)},
			{"validFor", {struct, [SDT]}};
		{undefined, EDateTime} ->
			EDT = {"endDateTime", ocs_rest:iso8601(EDateTime)},
			{"validFor", {struct, [EDT]}};
		{SDateTime, EDateTime} ->
			SDT = {"startDateTime", ocs_rest:iso8601(SDateTime)},
			EDT = {"endDateTime", ocs_rest:iso8601(EDateTime)},
			{"validFor", {struct, [SDT, EDT]}}
	end.

-spec prod_price_name(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: string() | tuple().
%% @private
prod_price_name(erl_term, Price) ->
	{_, Name} = lists:keyfind("name", 1, Price),
	Name;
prod_price_name(json, Price) ->
	{"name", Price#price.name}.

-spec prod_price_description(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: undefined | string() | tuple().
%% @private
prod_price_description(erl_term, Price) ->
	proplists:get_value("description", Price, undefined);
prod_price_description(json, Price) ->
	case Price#price.description of
		undefined ->
			{"description", ""};
		Des ->
			{"description", Des}
	end.

-spec prod_price_vf(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: tuple().
%% @private
prod_price_vf(erl_term, Price) ->
	case lists:keyfind("validFor", 1, Price) of
		{_,  {struct, VFObj}} ->
			case {proplists:get_value("startDateTime", VFObj), proplists:get_value("endDateTime", VFObj)} of
				{undefined, undefined} ->
					{undefined, undefined};
				{undefined, EDT} ->
					{undefined, ocs_rest:iso8601(EDT)};
				{SDT, undefined} ->
					{ocs_rest:iso8601(SDT), undefined};
				{SDT, EDT} ->
					{ocs_rest:iso8601(SDT), ocs_rest:iso8601(EDT)}
			end;
		false ->
			{undefined, undefined}
	end;
prod_price_vf(json, Price) ->
	case Price#price.valid_for of
		{undefined, undefined} ->
			{"validFor",{struct, []}};
		{undefined, EDateTime} ->
			EDT = {"endDateTime", ocs_rest:iso8601(EDateTime)},
			{"validFor",{struct, [EDT]}};
		{SDateTime, undefined} ->
			SDT = {"startDateTime", ocs_rest:iso8601(SDateTime)},
			{"validFor",{struct, [SDT]}};
		{SDateTime, EDateTime} ->
			SDT = {"startDateTime", ocs_rest:iso8601(SDateTime)},
			EDT = {"endDateTime", ocs_rest:iso8601(EDateTime)},
			{"validFor", {struct, [SDT, EDT]}}
	end.

-spec prod_price_type(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: atom() | tuple().
%% @private
prod_price_type(erl_term, Price) ->
	{_, ProdPriceTypeS} = lists:keyfind("priceType", 1, Price),
	price_type(ProdPriceTypeS);
prod_price_type(json, Price) ->
	PPT = price_type(Price#price.type),
	{"priceType", PPT}.

-spec prod_price_price_amount(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: integer() | tuple().
%% @private
prod_price_price_amount(erl_term, PriceObj) ->
	{_, ProdAmount} = lists:keyfind("taxIncludedAmount", 1, PriceObj),
	ProdAmount;
prod_price_price_amount(json, Price) ->
	case Price#price.amount of
		undefined ->
			{"taxIncludedAmount", ""};
		Amount ->
			{"taxIncludedAmount", Amount}
	end.

-spec prod_price_price_c_code(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: string() | tuple().
%% @private
prod_price_price_c_code(erl_term, PriceObj) ->
		{_, CurrencyCode} = lists:keyfind("currencyCode", 1, PriceObj),
		CurrencyCode;
prod_price_price_c_code(json, Price) ->
	{"currencyCode", Price#price.currency}.

-spec prod_price_rc_period(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: undefined | string() | tuple().
%% @private
prod_price_rc_period(erl_term, Price) ->
	case lists:keyfind("recurringChargePeriod", 1, Price) of
		{_, RCPeriod} ->
			rc_period(RCPeriod);
		false ->
			undefined
	end;
prod_price_rc_period(json, Price) ->
	case Price#price.period of
		undefined ->
			{"recurringChargePeriod", ""};
		RCPeriod ->
			{"recurringChargePeriod", rc_period(RCPeriod)}
	end.

-spec prod_price_alter_name(Prefix, PAlter) -> Result
	when
		Prefix :: erl_term | json,
		PAlter :: list() | #alteration{},
		Result :: string() | tuple().
%% @private
prod_price_alter_name(erl_term, PAlter) ->
	{_, Name} = lists:keyfind("name", 1, PAlter),
	Name;
prod_price_alter_name(json, PAlter) ->
	{"name", PAlter#alteration.name}.

-spec prod_price_alter_vf(Prefix, PAlter) -> Result
	when
		Prefix :: erl_term | json,
		PAlter :: list() | #alteration{},
		Result :: integer() | tuple().
%% @private
prod_price_alter_vf(erl_term, PAlter) ->
	case lists:keyfind("validFor", 1, PAlter) of
		{_, {struct, PAlterVF}} ->
			PAlterSTimeISO = proplists:get_value("startDateTime", PAlterVF),
			PAlterETime = proplists:get_value("endDateTime", PAlterVF),
			case {PAlterSTimeISO, PAlterETime} of
				{undefined, undefined} ->
					{undefined, undefined};
				{undefined, EDT} ->
					{undefined, ocs_rest:iso8601(EDT)};
				{SDT, undefined} ->
					{ocs_rest:iso8601(SDT), undefined};
				{SDT, EDT} ->
					{ocs_rest:iso8601(SDT), ocs_rest:iso8601(EDT)}
			end;
		false ->
			{undefined, undefined}
	end;
prod_price_alter_vf(json, PAlter) ->
	ValidFor = PAlter#alteration.valid_for,
	case ValidFor of
		{undefined, undefined} ->
			{"validFor", {struct, []}};
		{SDateTime, undefined} ->
			SDT = {"startDateTime", ocs_rest:iso8601(SDateTime)},
			{"validFor", {struct, [SDT]}};
		{undefined, EDateTime} ->
			EDT = {"startDateTime", ocs_rest:iso8601(EDateTime)},
			{"validFor", {struct, [EDT]}};
		{SDateTime, EDateTime} ->
			SDT = {"startDateTime", ocs_rest:iso8601(SDateTime)},
			EDT = {"endDateTime", ocs_rest:iso8601(EDateTime)},
			{"validFor", {struct, [SDT, EDT]}}
	end.

-spec prod_price_alter_description(Prefix, PAlter) -> Result
	when
		Prefix :: erl_term | json,
		PAlter :: list() | #alteration{},
		Result :: undefined | string() | tuple().
%% @private
prod_price_alter_description(erl_term, PAlter) ->
	proplists:get_value("description", PAlter, undefined);
prod_price_alter_description(json, PAlter) ->
	case PAlter#alteration.description of
		undefined ->
			{"description", ""};
		Des ->
			{"description", Des}
	end.

-spec prod_price_alter_price_type(Prefix, PAlter) -> Result
	when
		Prefix :: erl_term | json,
		PAlter :: list() | #alteration{},
		Result :: undefined | atom().
%% @private
prod_price_alter_price_type(erl_term, PAlter) ->
	case lists:keyfind("priceType", 1, PAlter) of
		{_, PriceType} ->
			price_type(PriceType);
		false ->
			undefined
	end;
prod_price_alter_price_type(json, PAlter) ->
	case PAlter#alteration.type of
		undefined ->
			{"priceType", ""};
		PT ->
			{"priceType", price_type(PT)}
	end.

-spec prod_price_alter_amount(Prefix, PAlter) -> Result
	when
		Prefix :: erl_term | json,
		PAlter :: list() | #alteration{},
		Result :: undefined | integer() | tuple().
%% @private
prod_price_alter_amount(erl_term, PAlterPriceObj) ->
	{_, PAlterAmount} = lists:keyfind("taxIncludedAmount", 1,  PAlterPriceObj),
	PAlterAmount;
prod_price_alter_amount(json, PAlter) ->
	case PAlter#alteration.amount of
		undefined ->
			{"taxIncludedAmount", ""};
		Amount ->
			{"taxIncludedAmount", Amount}
	end.

-spec prod_price_ufm(Prefix, Price) -> Result
	when
		Prefix	:: erl_term | json,
		Price		:: list() | #price{},
		Result	:: {Units, Size} | string(),
		Units		:: undefined | unit_of_measure(),
		Size		:: undefined | pos_integer().
%% @doc return units type and size of measurement of a product
%% @private
prod_price_ufm(erl_term, Price) ->
	UFM = proplists:get_value("unitOfMeasure", Price, undefined),
	prod_price_ufm_et(UFM);
prod_price_ufm(json, Price) ->
	Size = Price#price.size,
	Units = Price#price.units,
	{"unitOfMeasure", prod_price_ufm_json(Units, Size)}.

-spec prod_price_alter_ufm(Prefix, Alter) -> Result
	when
		Prefix	:: erl_term | json,
		Alter		:: list() | #alteration{},
		Result	:: {Units, Size} | string(),
		Units		:: undefined | unit_of_measure(),
		Size		:: undefined | pos_integer().
%% @doc return units type and size of measurement of a alteration
%% @private
prod_price_alter_ufm(erl_term, Alter) ->
	UFM = proplists:get_value("unitOfMeasure", Alter),
	prod_price_ufm_et(UFM);
prod_price_alter_ufm(json, Alter) ->
	Units = Alter#alteration.units,
	Size = product_size(octets, Units, Alter#alteration.size),
	{"unitOfMeasure", prod_price_ufm_json(Units, Size)}.

prod_price_ufm_json(undefined, _) ->
	"";
prod_price_ufm_json(Units, undefined) ->
	Units;
prod_price_ufm_json(Units, Size) when is_number(Size) ->
	prod_price_ufm_json(Units, integer_to_list(Size));
prod_price_ufm_json(octets, Size) when is_list(Size) ->
	Size ++ "b";
prod_price_ufm_json(gb, Size) when is_list(Size) ->
	Size ++ "g";
prod_price_ufm_json(mb, Size) when is_list(Size) ->
	Size ++ "m";
prod_price_ufm_json(cents, Size) when is_list(Size) ->
	Size ++ "c";
prod_price_ufm_json(seconds, Size) when is_list(Size) ->
	Size ++ "s".

prod_price_ufm_et(undefined) ->
	{undefined, undefined};
prod_price_ufm_et(UFM) ->
	LowerUOM = string:to_lower(UFM),
	prod_price_ufm_et1(LowerUOM).
%% @hidden
prod_price_ufm_et1(UFM) ->
	Suffix = "b",
	case lists:suffix(Suffix, UFM) of
		true ->
			[Size] = string:tokens(UFM, Suffix),
			{octets, list_to_integer(Size)};
		false ->
			prod_price_ufm_et2(UFM)
	end.
%% @hidden
prod_price_ufm_et2(UFM) ->
	Suffix = "g",
	case lists:suffix(Suffix, UFM) of
		true ->
			[Size] = string:tokens(UFM, Suffix),
			{gb, list_to_integer(Size)};
		false ->
			prod_price_ufm_et3(UFM)
	end.
%% @hidden
prod_price_ufm_et3(UFM) ->
	Suffix = "m",
	case lists:suffix(Suffix, UFM) of
		true ->
			[Size] = string:tokens(UFM, Suffix),
			{mb, list_to_integer(Size)};
		false ->
			prod_price_ufm_et4(UFM)
	end.
%% @hidden
prod_price_ufm_et4(UFM) ->
	Suffix = "c",
	case lists:suffix(Suffix, UFM) of
		true ->
			[Size] = string:tokens(UFM, Suffix),
			{cents, list_to_integer(Size)};
		false ->
			prod_price_ufm_et5(UFM)
	end.
%% @hidden
prod_price_ufm_et5(UFM) ->
	Suffix = "s",
	case lists:suffix(Suffix, UFM) of
		true ->
			[Size] = string:tokens(UFM, Suffix),
			{seconds, list_to_integer(Size)};
		false ->
			prod_price_ufm_et6(UFM)
	end.
%% @hidden
prod_price_ufm_et6(_UFM) ->
	{undefined, undefined}.

-spec product_size(UnitsFrom, UnitsTo, Size) -> Result
	when
		UnitsFrom	:: undefined | atom(), % gb | mb | second | cents
		UnitsTo		:: octets,
		Size			:: undefined | pos_integer(),
		Result		:: integer().
%% @private
product_size(UnitsFrom, octets, Size) when
		UnitsFrom == undefined; Size == undefined ->
	0;
product_size(gb, octets, Size) -> Size * 1000000000;
product_size(octets, gb, Size) -> Size div 1000000000;
product_size(mb, octets, Size) -> Size * 1000000;
product_size(octets, mb, Size) -> Size div 1000000;
product_size(_, _, Size) -> Size.

-spec rc_period(RCPeriod) -> Result
	when
		RCPeriod	:: string() | valid_period(),
		Result	:: valid_period() | string().
%% @doc return valid period
%% @private
rc_period("") -> undefined;
rc_period("yearly") -> yearly;
rc_period("monthly") -> monthly;
rc_period("weekly") -> weekly;
rc_period("daily") -> daily;
rc_period(undefined) -> "";
rc_period(yearly) -> "yearly";
rc_period(monthly) -> "monthly";
rc_period(weekly) -> "weekly";
rc_period(daily) -> "daily".

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
	StringPriceType :: string() | atom(),
	PriceType		 :: recurring | one_time | usage | string().
%% @private
price_type("usage") -> usage;
price_type("recurring") -> recurring;
price_type("one_time") -> one_time;
price_type(usage) -> "usage";
price_type(recurring) -> "recurring";
price_type(one_time) -> "one_time".

-spec exe_jsonpatch_ON(ProductID, Etag, OperationList) -> Result
	when
		ProductID		:: string() | binary(),
		Etag				:: undefined | tuple(),
		OperationList	:: [tuple()],
		Result			:: list() | {error, StatusCode},
		StatusCode		:: 400 | 404 | 422 | 500.
%% @doc execute object notation json patch
exe_jsonpatch_ON(ProdID, _Etag, OperationList) ->
	F = fun() ->
			case mnesia:read(product, ProdID, write) of
				[Entry] ->
					case ocs_rest:parse(OperationList) of
						{error, invalid_format} ->
							throw(malfored_request);
						Operations ->
							case lists:foldl(fun do_patch/2, Entry, Operations) of
								{error, Reason} ->
									throw(Reason);
								Updatedentry ->
									ok = mnesia:write(Updatedentry),
									Updatedentry
							end
					end;
				[] ->
					throw(not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, Product} ->
			{ok,  Product};
		{aborted, {throw, malfored_request}} ->
			{error, 400};
		{aborted, {throw, not_found}} ->
			{error, 404};
		{aborted, {throw, not_implemented}} ->
			{error, 422};
		{aborted, {throw, unprocessable}} ->
			{error, 422};
		{aborted, _Reason} ->
			{error,  500}
	end.

-spec exe_jsonpatch_merge(ProductID, Etag, Patch) -> Result
	when
		ProductID		:: string() | binary(),
		Etag				:: undefined | tuple(),
		Patch				:: term(),
		Result			:: list() | {error, StatusCode},
		StatusCode		:: 400 | 404 | 422 | 500.
%% @doc execute json merge patch
exe_jsonpatch_merge(ProdID, _Etag, Patch) ->
	F = fun() ->
			case mnesia:read(product, ProdID, write) of
				[Entry] ->
					case target(Entry) of
						{error, Status} ->
							throw(Status);
						Target ->
							Patched = ocs_rest:merge_patch(Target, Patch),
							case target(Patched) of
								{error, SC} ->
									throw(SC);
								NewEntry ->
									ok = mnesia:write(NewEntry),
									Patched
							end
					end;
				[] ->
					throw(not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, Product} ->
			{ok,  Product};
		{aborted, {throw, malfored_request}} ->
			{error, 400};
		{aborted, {throw, not_found}} ->
			{error, 404};
		{aborted, {throw, not_implemented}} ->
			{error, 422};
		{aborted, {throw, unprocessable}} ->
			{error, 422};
		{aborted, _Reason} ->
			{error,  500}
	end.

-spec do_patch(Operation, Product) -> Result
	when
		Operation	:: {Op, Path, Value},
		Product		:: #product{},
		Op				:: replace | add | remove | move | copy | test,
		Path			:: list(),
		Value			:: term(),
		Result		:: #product{} | {error, Reason},
		Reason		:: malfored_request | not_implemented | unprocessable.
%% @doc validate operation and paths for product patch reqeust.
do_patch({replace, Path, Value}, Product) ->
	patch_replace(Path, Value, Product);
do_patch(_, _) ->
	{error, not_implemented}.

patch_replace(["name"], Value, Product) when is_list(Value) ->
	Product#product{name = Value};
patch_replace(["description"], Value, Product) when is_list(Value) ->
	Product#product{description = Value};
patch_replace(["isBundle"], Value, Product) when is_boolean(Value) ->
	Product#product{is_bundle = Value};
patch_replace(["startDate"], Value, Product) when is_list(Value) ->
	SDate = ocs_rest:iso8601(Value),
	Product#product{start_date = SDate};
patch_replace(["terminationDate"], Value, Product) when is_list(Value) ->
	TDate = ocs_rest:iso8601(Value),
	Product#product{termination_date = TDate};
patch_replace(["lifecycleStatus"], Value, Product) when is_list(Value) ->
	S = find_status(Value),
	Product#product{status = S};
patch_replace(["productOfferingPrice" | T], Value, Product) ->
	case patch_replace1(prod_price, T, Value, Product#product.price) of
		{error, Reason} ->
			{error, Reason};
		Price ->
			Product#product{price = Price}
	end;
patch_replace(_, _, _) ->
	{error, unprocessable}.
%% @hidden
patch_replace1(prod_price, [], {array, Values}, _) ->
	try
	F = fun({struct, Obj}, Acc) ->
			F5 = fun(F5, [{"taxIncludedAmount", A} | T], Alter) when is_integer(A) ->
							F5(F5, T, Alter#alteration{amount = A});
						(_, [], Alter) ->
							Alter;
						(_, _, _) ->
							throw(malfored_request)
			end,
			F4 = fun(F4, [{"name", V} | T], Alter) when is_list(V) ->
							NAlter = Alter#alteration{name = V},
							F4(F4, T, NAlter);
						(F4, [{"description", V} | T], Alter) when is_list(V) ->
							NAlter = Alter#alteration{description = V},
							F4(F4, T, NAlter);
						(F4, [{"priceType", V} | T], Alter) when is_list(V) ->
							PT = price_type(V),
							NAlter = Alter#alteration{type = PT},
							F4(F4, T, NAlter);
						(F4, [{"price", {struct, V}} | T], Alter) when is_list(V) ->
							NAlter = F5(F5, V, Alter),
							F4(F4, T, NAlter);
						(F4, [{"unitOfMeasure", V} | T], Alter) when is_list(V) ->
							{AU, AS} = prod_price_ufm_et(V),
							S = product_size(AU, octets, AS),
							NAlter = Alter#alteration{units = AU, size = S},
							F4(F4, T, NAlter);
						(_, [], Alter) ->
							Alter;
						(_, _, _) ->
							throw(malfored_request)
			end,
			F3 = fun(F3, [{"currencyCode", C} | T], Price) when is_list(C)->
							F3(F3, T, Price#price{currency = C});
						(F3, [{"taxIncludedAmount", A} | T], Price) when is_integer(A) ->
							F3(F3, T, Price#price{amount = A});
						(_, [], Price) ->
							Price;
						(_, _, _) ->
							throw(malfored_request)
			end,
			F2 = fun(F2, [{"name", V} | T], Price) when is_list(V)->
							F2(F2, T, Price#price{name = V});
						(F2, [{"description", V} | T], Price) when is_list(V) ->
							F2(F2, T, Price#price{description = V});
						(F2, [{"priceType", V} | T], Price) when is_list(V) ->
							PT = price_type(V),
							F2(F2, T, Price#price{type = PT});
						(F2, [{"unitOfMeasure", V} | T], Price) when is_list(V) ->
							{U, S} = prod_price_ufm_et(V),
							UP = Price#price{units = U, size = S},
							F2(F2, T, UP);
						(F2, [{"recurringChargePeriod", V} | T], Price) when is_list(V) ->
							RcPeriod = rc_period(V),
							UP = Price#price{period = RcPeriod},
							F2(F2, T, UP);
						(F2, [{"price", {struct, V}} | T], Price) when is_list(V) ->
							UP = F3(F3, V, Price),
							F2(F2, T, UP);
						(F2, [{"productOfferPriceAlteration", {struct, V}} | T], Price) when is_list(V) ->
							Alter = F4(F4, V, #alteration{}),
							F2(F2, T, Price#price{alteration = Alter});
						(_, [], Price) ->
							Price;
						(_, _, _) ->
							throw(malfored_request)
			end,
			[F2(F2, Obj, #price{}) | Acc]
	end,
	lists:foldl(F, [], Values)
	catch
		_:_ ->
			{error, malfored_request}
	end;
patch_replace1(prod_price, [$- | T], Values, Prices) when Prices =/= undefined ->
	try
		{Hlist, [LastElement]} = lists:split(length(Prices) - 1, Prices),
		NewValues = case {Values, T} of
			{{struct, V}, []} ->
				V;
			{V, [T1]} when is_list(V) ->
				[{T1, V}];
			{V, [T1, T2]} when is_list(V) ->
				[{T1, {struct, [{T2, V}]}}];
			{V, [T1, T2, T3]} when is_list(V) ->
				[{T1, {struct, [{T2, [{struct, [{T3, V}]}]}]}}]
		end,
		case patch_replace2(NewValues, LastElement) of
			{error, Reason} ->
				{error, Reason};
			NLastElement ->
				Hlist ++ [NLastElement]
		end
	catch
		_:_ ->
			{error, unprocessable}
	end;
patch_replace1(prod_price, [Index | T], Values, Prices) when is_integer(Index), Prices =/= undefined  ->
	try
	if
		Index > length(Prices) ->
			{error, malfored_request};
		true ->
			{BNth, _} = lists:split(Index, Prices),
			Nth = lists:nth(Index + 1, Prices),
			ANth = lists:nthtail(Index + 1, Prices),
			NewValues = case {Values, T} of
				{{struct, V}, []} ->
					V;
				{V, [T1]} when is_list(V) ->
					[{T1, V}];
				{V, [T1, T2]} when is_list(V) ->
					[{T1, {struct, [{T2, V}]}}];
				{V, [T1, T2, T3]} when is_list(V) ->
					[{T1, {struct, [{T2, [{struct, [{T3, V}]}]}]}}]
			end,
			case patch_replace2(NewValues, Nth) of
				{error, Reason} ->
					{error, Reason};
				NewNth ->
					BNth ++ [NewNth] ++ ANth
			end
	end
	catch
		_:_ ->
			{error, unprocessable}
	end;
patch_replace1(_, _, _, _) ->
	{error, unprocessable}.
%% @hidden
patch_replace2([], Price) when is_record(Price, price) ->
	Price;
patch_replace2([{"name", Value} | T], Price) when is_record(Price, price), is_list(Value) ->
	patch_replace2(T, Price#price{name = Value});
patch_replace2([{"description", Value} | T], Price) when is_record(Price, price), is_list(Value) ->
	patch_replace2(T, Price#price{description = Value});
patch_replace2([{"priceType", Value} | T], Price) when is_record(Price, price), is_list(Value) ->
	PT = price_type(Value),
	patch_replace2(T, Price#price{type = PT});
patch_replace2([{"recurringChargePeriod", Value} | T], Price) when is_record(Price, price), is_list(Value) ->
	RCP = rc_period(Value),
	patch_replace2(T, Price#price{period = RCP});
patch_replace2([{"unitOfMeasure", Value} | T], Price) when is_record(Price, price), is_list(Value) ->
	{U, S} = prod_price_ufm_et(Value),
	UPrice = Price#price{units = U, size = S},
	patch_replace2(T, UPrice);
patch_replace2([{"price", {struct, Value}} | T], Price) when is_record(Price, price), is_list(Value) ->
	case patch_replace4(Value, Price) of
		{error, Reason} ->
			{error, Reason};

		UPrice ->
			patch_replace2(T, UPrice)
	end;
patch_replace2([{"productOfferPriceAlteration", {struct, Value}} | T], #price{alteration = Alter} = Price)
		when is_record(Price, price) and is_list(Value) ->
	case patch_replace3(Value, Alter) of
		{error, Reason} ->
			{error, Reason};
		UAlter ->
			patch_replace2(T, Price#price{alteration = UAlter})
	end;
patch_replace2(_, _) ->
	{error, unprocessable}.
%% @hidden
patch_replace3([], Alter) ->
	Alter;
patch_replace3([{"name", Value} | T], Alter) when is_list(Value) ->
	patch_replace3(T, Alter#alteration{name = Value});
patch_replace3([{"description", Value} | T], Alter) when is_list(Value) ->
	patch_replace3(T, Alter#alteration{description = Value});
patch_replace3([{"priceType", Value} | T], Alter) when is_list(Value) ->
	PT = price_type(Value),
	patch_replace3(T, Alter#alteration{type = PT});
patch_replace3([{"unitOfMeasure", Value} | T], Alter) when is_list(Value) ->
	{AU, AS} = prod_price_ufm_et(Value),
	S = product_size(AU, octets, AS),
	UAlter = Alter#alteration{units = AU, size = S},
	patch_replace3(T, UAlter);
patch_replace3([{"price", {struct, Value}} | T], Alter) when is_list(Value) ->
	case patch_replace4(Value, Alter) of
		{error, Reason} ->
			{error, Reason};
		UAlter ->
			patch_replace3(T, UAlter)
	end;
patch_replace3(_, _) ->
	{error, unprocessable}.
%% @hidden
patch_replace4([], PorA) ->
	PorA;
patch_replace4([{"currencyCode", Value} | T], Price) when is_record(Price, price) ->
	patch_replace4(T, Price#price{currency = Value});
patch_replace4([{"taxIncludedAmount", Value} | T], Price) when is_record(Price, price), is_integer(Value) ->
	patch_replace4(T, Price#price{amount = Value});
patch_replace4([{"taxIncludedAmount", Value} | T], Alter) when is_record(Alter, alteration), is_integer(Value) ->
	patch_replace4(T, #alteration{amount = Value});
patch_replace4(_, _) ->
	{error, unprocessable}.

target(Prod) when is_record(Prod, product) ->
	ID = prod_id(json, Prod),
	Descirption = prod_description(json, Prod),
	Href = prod_href(json, Prod),
	ValidFor = prod_vf(json, Prod),
	IsBundle = prod_isBundle(json, Prod),
	Name = prod_name(json, Prod),
	Status = prod_status(json, Prod),
	StartDate = prod_sdate(json, Prod),
	TerminationDate = prod_tdate(json, Prod),
	case prod_offering_price(json, Prod) of
		{error, StatusCode} ->
			{error, StatusCode};
		OfferPrice ->
			{struct, [ID, Descirption, Href, StartDate,
				TerminationDate, IsBundle, Name, Status, ValidFor,
				OfferPrice]}
	end;
target({struct, Object}) ->
	try
		Name = prod_name(erl_term, Object),
		IsBundle = prod_isBundle(erl_term, Object),
		Status = prod_status(erl_term, Object),
		ValidFor = prod_vf(erl_term, Object),
		Descirption = prod_description(erl_term, Object),
		StartDate = prod_sdate(erl_term, Object),
		TerminationDate = prod_tdate(erl_term, Object),
		case prod_offering_price(erl_term, Object) of
			{error, StatusCode} ->
				{error, StatusCode};
			Price ->
				#product{price = Price, name = Name, valid_for = ValidFor,
					is_bundle = IsBundle, status = Status, start_date = StartDate,
					termination_date = TerminationDate, description = Descirption}
		end
	catch
		_:_ ->
			{error, 400}
	end.
