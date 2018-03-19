%% ocs_rest_res_product.erl
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
-export([add_product_offering/1, add_product_inventory/1]).
-export([get_product_offering/1, get_product_offerings/2,
		patch_product_offering/3, get_product_inventory/1,
		get_product_inventories/2, patch_product_inventory/3]).
-export([get_catalog/2, get_catalogs/1]).
-export([get_category/2, get_categories/1]).
-export([get_product_spec/2, get_product_specs/1]).
-export([add_pla/1, get_pla/1, get_plas/2, patch_pla/3]).
-export([get_pla_spec/2, get_pla_specs/1]).
-export([delete_product_offering/1, delete_product_inventory/1, delete_pla/1]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(catalogPath, "/catalogManagement/v2/catalog/").
-define(categoryPath, "/catalogManagement/v2/category/").
-define(productSpecPath, "/catalogManagement/v2/productSpecification/").
-define(offeringPath, "/catalogManagement/v2/productOffering/").
-define(plaPath, "/catalogManagement/v2/pla/").
-define(plaSpecPath, "/catalogManagement/v2/plaSpecification/").
-define(inventoryPath, "/productInventoryManagement/v2/product/").

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

-spec add_product_offering(ReqData) -> Result when
	ReqData	:: [tuple()],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 500 .
%% @doc Respond to `POST /catalogManagement/v2/productOffering'.
%% 	Add a new Product Offering.
add_product_offering(ReqData) ->
	try
		case ocs:add_product(offer(mochijson:decode(ReqData))) of
			{ok, ProductOffering} ->
				ProductOffering;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Offer ->
			Body = mochijson:encode(offer(Offer)),
			Etag = ocs_rest:etag(Offer#product.last_modified),
			Href = ?offeringPath ++ Offer#product.name,
			Headers = [{location, Href}, {etag, Etag}],
			{ok, Headers, Body}
	catch
		throw:validation_failed ->
			{error, 400};
		throw:_Reason1 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec add_product_inventory(ReqData) -> Result when
	ReqData	:: [tuple()],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 500 .
%% @doc Respond to `POST /productInventoryManagemen/v2/product'.
%% 	Add a new instance of a Product Offering subscription.
add_product_inventory(ReqData) ->
	try
		#subscriber{name = SubscriberID,
				password = Password, product = #product_instance{product = ProdId,
				characteristics = Chars}} = inventory(mochijson:decode(ReqData)),
		case ocs:add_subscriber(SubscriberID, Password, ProdId, Chars) of
			{ok, Subscriber} ->
				Subscriber;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Subscription ->
			Body = mochijson:encode(inventory(Subscription)),
			Etag = ocs_rest:etag(Subscription#subscriber.last_modified),
			Href = ?inventoryPath ++ binary_to_list(Subscription#subscriber.name),
			Headers = [{location, Href}, {etag, Etag}],
			{ok, Headers, Body}
	catch
		throw:validation_failed ->
			{error, 400};
		throw:_Reason1 ->
			{error, 500};
		_:_Reason1 ->
			{error, 400}
	end.

-spec add_pla(ReqData) -> Result when
	ReqData :: [tuple()],
	Result   :: {ok, Headers, Body} | {error, Status},
	Headers  :: [tuple()],
	Body     :: iolist(),
	Status   :: 400 | 500 .
%% @doc Respond to `POST /catalogManagement/v2/pla'.
%%    Add a new Product Offering.
add_pla(ReqData) ->
	try
		case ocs:add_pla(pla(mochijson:decode(ReqData))) of
			{ok, PricingLogic} ->
				PricingLogic;
			{error, Reason} ->
				throw(Reason)
		end
	of
		PriceAlgo ->
			Body = mochijson:encode(pla(PriceAlgo)),
			Etag = ocs_rest:etag(PriceAlgo#pla.last_modified),
			Href = ?plaPath ++ PriceAlgo#pla.name,
			Headers = [{location, Href}, {etag, Etag}],
			{ok, Headers, Body}
	catch
		throw:validation_failed ->
			{error, 400};
		throw:_Reason1 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec get_product_offering(ID) -> Result when
	ID			:: string(),
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/productOffering/{id}'.
%% 	Retrieve a Product Offering.
get_product_offering(ID) ->
	try
		case ocs:find_product(ID) of
			{ok, ProductOffering} ->
				ProductOffering;
			{error, not_found} ->
				{throw, 404};
			{error, _Reason1} ->
				{throw, 500}
		end
	of
		Offer ->
			Body = mochijson:encode(offer(Offer)),
			Etag = ocs_rest:etag(Offer#product.last_modified),
			Href = ?offeringPath ++ Offer#product.name,
			Headers = [{location, Href}, {etag, Etag},
					{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		throw:_Reason2 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec get_product_inventory(ID) -> Result when
	ID			:: string(),
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /productInventoryManagement/v2/product/{id}'.
%% 	Retrieve a Product Inventory.
get_product_inventory(ID) ->
	try
		case ocs:find_subscriber(ID) of
			{ok, Subscriber} ->
				Subscriber;
			{error, not_found} ->
				{throw, 404};
			{error, _Reason1} ->
				{throw, 500}
		end
	of
		Subscription ->
			Body = mochijson:encode(inventory(Subscription)),
			Etag = ocs_rest:etag(Subscription#subscriber.last_modified),
			Href = ?inventoryPath ++ binary_to_list(Subscription#subscriber.name),
			Headers = [{location, Href}, {etag, Etag},
					{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		throw:_Reason2 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec get_product_offerings(Query, Headers) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/productOffering'.
%% 	Retrieve all Product Offerings.
%% @todo Filtering
get_product_offerings(Query, Headers) ->
	Name =  proplists:get_value("name", Query),
	Des = proplists:get_value("description", Query),
	Status = case lists:keyfind("lifecycleStatus", 1, Query) of
		false ->
			undefined;
		{_, S} ->
			product_status(S)
	end,
	SDT = proplists:get_value("startDate", Query),
	EDT = proplists:get_value("endDate", Query),
	Price = proplists:get_value("price", Query),
	M = ocs,
	F = query_product,
	A = [Name, Des, Status, SDT, EDT, Price],
	Codec = fun offer/1,
	query_filter({M, F, A}, Codec, Query, Headers).

-spec get_pla(ID) -> Result when
	ID	:: string(),
	Result :: {ok, Headers, Body} | {error, Status},
	Headers :: [tuple()],
	Body :: iolist(),
	Status :: 400 | 404 | 500.
%% @doc Respond to `GET /catalogManagement/v2/pla/{id}'.
%%    Retrieve a pricing logic algorothm.
get_pla(ID) ->
	try
		case ocs:find_pla(ID) of
			{ok, PricingLogicAlgorithm} ->
				PricingLogicAlgorithm;
			{error, not_found} ->
				{throw, 404};
			{error, _Reason} ->
				{throw, 500}
		end
	of
		LogicAlgo ->
			Body = mochijson:encode(pla(LogicAlgo)),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		throw:_Reason1 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec get_plas(Query, Headers) ->Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result :: {ok, Headers, Body} | {error, Status},
	Headers :: [tuple()],
	Body :: iolist(),
	Status :: 400 | 404 | 412 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/pla'.
%%    Retrieve all pricing logic algorithms.
get_plas(_Query, _Headers) -> 
	try
		case ocs:get_plas() of
			PricingLogicAlgorithms
				when is_list(PricingLogicAlgorithms) ->
				PricingLogicAlgorithms;
			{error, not_found} ->
				throw(404);
			{error, _Reason} ->
				throw(500)
		end
	of
		Logic ->
			Body = mochijson:encode({array, [pla(P) || P <- Logic]}),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		throw:_Reason1 ->
         {error, 500};
      _:_ ->
         {error, 400}
   end.

-spec get_product_inventories(Query, Headers) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `GET /productInventoryManagement/v2/product'.
%% 	Retrieve all Product Inventories.
%% @todo Filtering
get_product_inventories(Query, Headers) ->
	M = ocs,
	F = query_subscriber,
	A = [],
	Codec = fun inventory/1,
	query_filter({M, F, A}, Codec, Query, Headers).

-spec get_catalog(Id, Query) -> Result when
	Id :: string(),
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/catalog/{id}'.
%% 	Retrieve a catalog .
get_catalog("1", [] =  _Query) ->
	Headers = [{content_type, "application/json"}],
	Body = mochijson:encode(product_catalog()),
	{ok, Headers, Body};
get_catalog(_Id,  [] = _Query) ->
	{error, 404};
get_catalog(_Id, _Query) ->
	{error, 400}.

-spec get_catalogs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/catalog'.
%% 	Retrieve all catalogs .
get_catalogs([] =  _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [product_catalog()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_catalogs(_Query) ->
	{error, 400}.

-spec get_category(Id, Query) -> Result when
	Id :: string(),
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/category/{id}'.
%% 	Retrieve a category.
get_category("1", [] =  _Query) ->
	Headers = [{content_type, "application/json"}],
	Body = mochijson:encode(prepaid_category()),
	{ok, Headers, Body};
get_category(_Id,  [] = _Query) ->
	{error, 404};
get_category(_Id, _Query) ->
	{error, 400}.

-spec get_categories(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagement/v2/catalog'.
%% 	Retrieve all catalogs .
get_categories([] =  _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [prepaid_category()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_categories(_Query) ->
	{error, 400}.

-spec get_product_spec(Id, Query) -> Result when
	Id :: string(),
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManegment/v2/productSpecification/{id}'.
%% 	Retrieve a product specification.
get_product_spec(ID, [] = _Query) ->
	case product_spec(ID) of
		{error, StatusCode} ->
			{error, StatusCode};
		ProductSpec ->
			Headers = [{content_type, "application/json"}],
			Body = mochijson:encode(ProductSpec),
			{ok, Headers, Body}
	end;
get_product_spec(_Id, _Query) ->
	{error, 400}.

-spec get_product_specs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManagment/v2/productSpecification'.
%% 	Retrieve all product specifications.
get_product_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [spec_prod_network(),
					spec_prod_fixed_quantity_pkg(),
					spec_prod_rated_plan(),
					spec_prod_data(), spec_prod_voice(),
					spec_prod_prepaid(), spec_prod_postpaid(),
					spec_prod_prepaid_data(), spec_prod_prepaid_voice()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_product_specs(_Query) ->
	{error, 400}.

-spec patch_product_offering(ProdId, Etag, ReqData) -> Result
	when
		ProdId	:: string(),
		Etag		:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `PATCH /catalogManagement/v2/productOffering/{id}'.
%% 	Update a Product Offering using JSON patch method
%% 	<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>.
patch_product_offering(ProdId, Etag, ReqData) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Etag1, mochijson:decode(ReqData)}
	of
		{Etag2, {array, _} = Operations} ->
			F = fun() ->
					case mnesia:read(product, ProdId, write) of
						[Product1] when
								Product1#product.last_modified == Etag2;
								Etag2 == undefined ->
							case catch ocs_rest:patch(Operations, offer(Product1)) of
								{struct, _} = Product2  ->
									Product3 = offer(Product2),
									TS = erlang:system_time(?MILLISECOND),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									Product4 = Product3#product{last_modified = LM},
									ok = mnesia:write(Product4),
									{Product2, LM};
								_ ->
									throw(bad_request)
							end;
						[#product{}] ->
							throw(precondition_failed);
						[] ->
							throw(not_found)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, {Product, Etag3}} ->
					Location = ?offeringPath ++ ProdId,
					Headers = [{location, Location}, {etag, ocs_rest:etag(Etag3)}],
					Body = mochijson:encode(Product),
					{ok, Headers, Body};
				{aborted, {throw, bad_request}} ->
					{error, 400};
				{aborted, {throw, not_found}} ->
					{error, 404};
				{aborted, {throw, precondition_failed}} ->
					{error, 412};
				{aborted, _Reason} ->
					{error, 500}
			end
	catch
		_:_ ->
			{error, 400}
	end.

-spec get_pla_specs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /catalogManegment/v2/plaSpecification'.
%% 	Retrieve all pricing logic algorithm specifications.
get_pla_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [spec_pla_once(), spec_pla_recurring(),
			spec_pla_usage(), spec_pla_tariff()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_pla_specs(_Query) ->
	{error, 400}.

-spec get_pla_spec(Id, Query) -> Result when
	Id :: string(),
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /catalogManegment/v2/plaSpecification/{id}'.
%% 	Retrieve a pricing logic algorithm specification.
get_pla_spec(ID, [] = _Query) ->
	case pla_spec(ID) of
		{error, StatusCode} ->
			{error, StatusCode};
		PLASpec ->
			Headers = [{content_type, "application/json"}],
			Body = mochijson:encode(PLASpec),
			{ok, Headers, Body}
	end;
get_pla_spec(_Id, _Query) ->
	{error, 400}.

-spec patch_product_inventory(SubId, Etag, ReqData) -> Result
	when
		SubId	:: string(),
		Etag		:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `PATCH /catalogManagement/v2/productOffering/{id}'.
%% 	Update a Product Offering using JSON patch method
%% 	<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>.
patch_product_inventory(SubId, Etag, ReqData) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Etag1, mochijson:decode(ReqData)}
	of
		{Etag2, {array, _} = Operations} ->
			F = fun() ->
					case mnesia:read(subscriber, SubId, write) of
						[Subscriber1] when
								Subscriber1#subscriber.last_modified == Etag2;
								Etag2 == undefined ->
							case catch ocs_rest:patch(Operations, inventory(Subscriber1)) of
								{struct, _} = Subscriber2  ->
									Subscriber3 = inventory(Subscriber2),
									TS = erlang:system_time(?MILLISECOND),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									Subscriber4 = Subscriber3#subscriber{last_modified = LM},
									ok = mnesia:write(Subscriber4),
									{Subscriber2, LM};
								_ ->
									throw(bad_request)
							end;
						[#subscriber{}] ->
							throw(precondition_failed);
						[] ->
							throw(not_found)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, {Subscriber, Etag3}} ->
					Location = "/productInventoryManagement/v1/product/" ++ SubId,
					Headers = [{location, Location}, {etag, ocs_rest:etag(Etag3)}],
					Body = mochijson:encode(Subscriber),
					{ok, Headers, Body};
				{aborted, {throw, bad_request}} ->
					{error, 400};
				{aborted, {throw, not_found}} ->
					{error, 404};
				{aborted, {throw, precondition_failed}} ->
					{error, 412};
				{aborted, _Reason} ->
					{error, 500}
			end
	catch
		_:_ ->
			{error, 400}
	end.

-spec patch_pla(Id, Etag, ReqData) -> Result
	when
		Id	:: string(),
		Etag		:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `PATCH /catalogManagement/v2/pla/{id}'.
%% 	Update a pricing logic algorithm using JSON patch method
patch_pla(Id, Etag, ReqData) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Etag1, mochijson:decode(ReqData)}
	of
		{Etag2, {array, _} = Operations} ->
			F = fun() ->
					case mnesia:read(pla, Id, write) of
						[Pla1] when
								Pla1#pla.last_modified == Etag2;
								Etag2 == undefined ->
							case catch ocs_rest:patch(Operations, pla(Pla1)) of
								{struct, _} = Pla2  ->
									Pla3 = pla(Pla2),
									TS = erlang:system_time(?MILLISECOND),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									Pla4 = Pla3#pla{last_modified = LM},
									ok = mnesia:write(Pla4),
									{Pla2, LM};
								_ ->
									throw(bad_request)
							end;
						[#pla{}] ->
							throw(precondition_failed);
						[] ->
							throw(not_found)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, {Pla, Etag3}} ->
					Location = ?plaPath ++ Id,
					Headers = [{location, Location}, {etag, ocs_rest:etag(Etag3)}],
					Body = mochijson:encode(Pla),
					{ok, Headers, Body};
				{aborted, {throw, bad_request}} ->
					{error, 400};
				{aborted, {throw, not_found}} ->
					{error, 404};
				{aborted, {throw, precondition_failed}} ->
					{error, 412};
				{aborted, _Reason} ->
					{error, 500}
			end
	catch
		_:_ ->
			{error, 400}
	end.

-spec delete_product_offering(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /catalogManagement/v1/productOffering/{id}'
%% 	request to remove a `Product Offering'.
delete_product_offering(Id) ->
	ok = ocs:delete_product(Id),
	{ok, [], []}.

-spec delete_product_inventory(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /productInventoryManagement/v1/product/{id}'
%% 	request to remove a `Product Invenotry'.
delete_product_inventory(Id) ->
	ok = ocs:delete_subscriber(Id),
	{ok, [], []}.

-spec delete_pla(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /catalogManagement/v2/pla/{id}'
%% 	request to remove a `Pla'.
delete_pla(Id) when is_list(Id) ->
	case catch list_to_existing_atom(Id) of
		{'EXIT', _Reason} ->
			{error, 404};
		TableName when is_atom(TableName) ->
			ok = ocs:delete_pla(Id),
			{ok, [], []}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec product_spec(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Get Product Specification by ID.
product_spec("1") ->
	spec_prod_network();
product_spec("2") ->
	spec_prod_fixed_quantity_pkg();
product_spec("3") ->
	spec_prod_rated_plan();
product_spec("4") ->
	spec_prod_data();
product_spec("5") ->
	spec_prod_voice();
product_spec("6") ->
	spec_prod_prepaid();
product_spec("7") ->
	spec_prod_postpaid();
product_spec("8") ->
	spec_prod_prepaid_data();
product_spec("9") ->
	spec_prod_prepaid_voice();
product_spec(_) ->
	{error, 404}.

%% @hidden
product_catalog() ->
	Id = {"id", "1"},
	Href = {"href", ?catalogPath "1"},
	Type = {"type", "Product Catalog"},
	Name = {"name", "SigScale OCS"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-04T00:00:00Z"},
	Category = {"category", {array, [prepaid_category()]}},
	{struct, [Id, Href, Type, Name, Status, Version, LastUpdate, Category]}.

%% @hidden
prepaid_category() ->
	Id = {"id", "1"},
	Href = {"href", ?categoryPath "1"},
	Name = {"name", "Prepaid"},
	Description = {"description", "Services provided with realtime credit management"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-04T00:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	IsRoot = {"isRoot", true},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, IsRoot]}.

%% @hidden
spec_prod_network() ->
	Id = {"id", "1"},
	Href = {"href", ?productSpecPath "1"},
	Name = {"name", "NetworkProductSpec"},
	Description = {"description", "Represents the common behaviour and description of an installed network product that will be provisioned in the network and that enables usages."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-01T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	Chars = {"productSpecCharacteristic", {array, characteristic_product_network()}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars]}.

%% @hidden
spec_prod_fixed_quantity_pkg() ->
	Id = {"id", "2"},
	Href = {"href", ?productSpecPath "2"},
	Name = {"name", "FixedQuantityPackageProductSpec"},
	Description = {"description", "Defines buckets of usage from which Usages will debit the bucket."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-06T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status]}.

%% @hidden
spec_prod_rated_plan() ->
	Id = {"id", "3"},
	Href = {"href", ?productSpecPath "3"},
	Name = {"name", "RatedPlanProductSpec"},
	Description = {"description", "Defines criteria to be used to gain special usage tariffs like the period (day, evening) or phone number."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-06T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	Chars = {"productSpecCharacteristic", {array, characteristic_product_rated_plan()}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars]}.

%% @hidden
spec_prod_data() ->
	Id = {"id", "4"},
	Href = {"href", ?productSpecPath "4"},
	Name = {"name", "DataProductSpec"},
	Description = {"description", "Defines characteristics specific to data service."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-11-14T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	DepType = {"type", "dependency"},
	DepId = {"id", "1"},
	DepHref = {"href", ?productSpecPath "1"},
	Depend = {struct, [DepId, DepHref, DepType]},
	Dependency = {"productSpecificationRelationship", {array, [Depend]}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Dependency]}.

%% @hidden
spec_prod_voice() ->
	Id = {"id", "5"},
	Href = {"href", ?productSpecPath "5"},
	Name = {"name", "VoiceProductSpec"},
	Description = {"description", "Defines characteristics specific to voice calling."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-12-21T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	DepType = {"type", "dependency"},
	DepId1 = {"id", "1"},
	DepHref1 = {"href", ?productSpecPath "1"},
	Depend1 = {struct, [DepId1, DepHref1, DepType]},
	DepId2 = {"id", "3"},
	DepHref2 = {"href", ?productSpecPath "3"},
	Depend2 = {struct, [DepId2, DepHref2, DepType]},
	Dependency = {"productSpecificationRelationship", {array, [Depend1, Depend2]}},
	Chars = {"productSpecCharacteristic", {array, characteristic_product_voice()}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars, Dependency]}.

%% @hidden
spec_prod_prepaid() ->
	Id = {"id", "6"},
	Href = {"href", ?productSpecPath "6"},
	Name = {"name", "PrepaidProductSpec"},
	Description = {"description", "Defines characteristics specific to prepaid charging."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-12-21T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	Chars = {"productSpecCharacteristic", {array, characteristic_product_prepaid()}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars]}.

%% @hidden
spec_prod_postpaid() ->
	Id = {"id", "7"},
	Href = {"href", ?productSpecPath "7"},
	Name = {"name", "PostpaidProductSpec"},
	Description = {"description", "Defines characteristics specific to postpaid charging."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-12-21T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status]}.

%% @hidden
spec_prod_prepaid_data() ->
	Id = {"id", "8"},
	Href = {"href", ?productSpecPath "8"},
	Name = {"name", "PrepaidDataProductSpec"},
	Description = {"description", "Defines characteristics specific to prepaid data."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-12-21T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	DepType = {"type", "dependency"},
	DepId1 = {"id", "4"},
	DepHref1 = {"href", ?productSpecPath "4"},
	Depend1 = {struct, [DepId1, DepHref1, DepType]},
	DepId2 = {"id", "6"},
	DepHref2 = {"href", ?productSpecPath "6"},
	Depend2 = {struct, [DepId2, DepHref2, DepType]},
	Dependency = {"productSpecificationRelationship", {array, [Depend1, Depend2]}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Dependency]}.

%% @hidden
spec_prod_prepaid_voice() ->
	Id = {"id", "9"},
	Href = {"href", ?productSpecPath "9"},
	Name = {"name", "PrepaidVoiceProductSpec"},
	Description = {"description", "Defines characteristics specific to prepaid voice."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-12-21T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	DepType = {"type", "dependency"},
	DepId1 = {"id", "5"},
	DepHref1 = {"href", ?productSpecPath "5"},
	Depend1 = {struct, [DepId1, DepHref1, DepType]},
	DepId2 = {"id", "6"},
	DepHref2 = {"href", ?productSpecPath "6"},
	Depend2 = {struct, [DepId2, DepHref2, DepType]},
	Dependency = {"productSpecificationRelationship", {array, [Depend1, Depend2]}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Dependency]}.

%% @hidden
characteristic_product_network() ->
	Name1 = {"name", "subscriberIdentity"},
	Description1 = {"description",
			"Uniquely identifies subscriber (e.g. MSISDN, IMSI, username)."},
	Config1 = {"configurable", true},
	Type1 = {"valueType", "String"},
	Value1 = {"productSpecCharacteristicValue", {array, [{struct, [Type1]}]}},
	Char1 = {struct, [Name1, Description1, Config1, Type1, Value1]},
	Name2 = {"name", "subscriberPassword"},
	Description2 = {"description", "Shared secret used in authentication."},
	Config2 = {"configurable", true},
	Type2 = {"valueType", "String"},
	Value2 = {"productSpecCharacteristicValue", {array, [{struct, [Type2]}]}},
	Char2 = {struct, [Name2, Description2, Config2, Type2, Value2]},
	Name3 = {"name", "radiusReserveTime"},
	Description3 = {"description",
		"Number of seconds to reserve on RADIUS Accounting-Start "
		"and add to reported duration on Accounting-Interim reservation."},
	Config3 = {"configurable", false},
	Type3 = {"valueType", "Number"},
	Type31 = {struct, [{"unitOfMeasure", "seconds"}, {"valueType", "Number"}]},
	Type32 = {struct, [{"unitOfMeasure", "minutes"}, {"valueType", "Number"}]},
	Value3 = {"productSpecCharacteristicValue", {array, [Type31, Type32]}},
	Char3 = {struct, [Name3, Description3, Config3, Type3, Value3]},
	Name4 = {"name", "radiusReserveOctets"},
	Description4 = {"description",
		"Number of octets to reserve on RADIUS Accounting-Start "
		"and add to reported octets used on Accounting-Interim reservation."},
	Config4 = {"configurable", false},
	Type4 = {"valueType", "Number"},
	Type41 = {struct, [{"unitOfMeasure", "bytes"}, {"valueType", "Number"}]},
	Type42 = {struct, [{"unitOfMeasure", "kilobytes"}, {"valueType", "Number"}]},
	Type43 = {struct, [{"unitOfMeasure", "megabytes"}, {"valueType", "Number"}]},
	Type44 = {struct, [{"unitOfMeasure", "gigabytes"}, {"valueType", "Number"}]},
	Value4 = {"productSpecCharacteristicValue",
			{array, [Type41, Type42, Type43, Type44]}},
	Char4 = {struct, [Name4, Description4, Config4, Type4, Value4]},
	Name5 = {"name", "radiusReserveSessionTime"},
	Description5 = {"description", "Number of seconds for one session"},
	Config5 = {"configurable", true},
	Type5 = {"valueType", "Number"},
	Value5 = {"productSpecCharacteristicValue", {array, [{struct, [Type5]}]}},
	Char5 = {struct, [Name5, Description5, Config5, Type5, Value5]},
	[Char1, Char2, Char3, Char4, Char5].

%% @hidden
characteristic_product_rated_plan() ->
	Name1 = {"name", "timeOfDayRange"},
	Description1 = {"description", "Start and End of time of day range"},
	ValueType1 = {"valueType", "Range"},
	Char1 = {struct, [Name1, Description1, ValueType1]},
	[Char1].

%% @hidden
characteristic_product_prepaid() ->
	Name1 = {"name", "balanceTopUpDuration"},
	Description1 = {"description", "Validity period of balance top-ups."},
	Config1 = {"configurable", false},
	Type1 = {"valueType", "Number"},
	Type11 = {struct, [{"unitOfMeasure", "seconds"}, {"valueType", "Number"}]},
	Type12 = {struct, [{"unitOfMeasure", "minutes"}, {"valueType", "Number"}]},
	Type13 = {struct, [{"unitOfMeasure", "days"}, {"valueType", "Number"}]},
	Type14 = {struct, [{"unitOfMeasure", "months"}, {"valueType", "Number"}]},
	Type15 = {struct, [{"unitOfMeasure", "years"}, {"valueType", "Number"}]},
	Value1 = {"productSpecCharacteristicValue",
			{array, [Type11, Type12, Type13, Type14, Type15]}},
	Char1 = {struct, [Name1, Description1, Config1, Type1, Value1]},
	[Char1].

%% @hidden
characteristic_product_voice() ->
	Name1 = {"name", "destPrefixTariffTable"},
	Description1 = {"description", "Table of Prefix, Description, Tariff rate"},
	ValueType1 = {"valueType", "String"},
	Char1 = {struct, [Name1, Description1, ValueType1]},
	Name2 = {"name", "callDirection"},
	Description2 = {"description", "Constrain price to incoming or outgoing calls"},
	ValueType2 = {"valueType", "String"},
	Char2 = {struct, [Name2, Description2, ValueType2]},
	[Char1, Char2].

-spec pla_spec(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Get PLA specification by ID.
pla_spec("1") ->
	spec_pla_once();
pla_spec("2") ->
	spec_pla_recurring();
pla_spec("3") ->
	spec_pla_usage();
pla_spec("4") ->
	spec_pla_tariff();
pla_spec(_) ->
	{error, 404}.

%% @hidden
spec_pla_once() ->
	Id = {"id", "1"},
	Href = {"href", ?plaSpecPath "1"},
	Name = {"name", "OneTimePLASpec"},
	Description = {"description", "Interface specification for a function that rates one time events."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status]}.

%% @hidden
spec_pla_recurring() ->
	Id = {"id", "2"},
	Href = {"href", ?plaSpecPath "2"},
	Name = {"name", "RecurringPLASpec"},
	Description = {"description", "Interface specification for a function that rates recurring events."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status]}.

%% @hidden
spec_pla_usage() ->
	Id = {"id", "3"},
	Href = {"href", ?plaSpecPath "3"},
	Name = {"name", "UsagePLASpec"},
	Description = {"description", "Interface specification for a function that rates usage events."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	Chars = {"usageSpecCharacteristic", {array, []}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars]}.

%% @hidden
spec_pla_tariff() ->
	Id = {"id", "4"},
	Href = {"href", ?plaSpecPath "4"},
	Name = {"name", "PrefixTariffTablePLASpec"},
	Description = {"description", "Destination prefix table lookup of tariff amount."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	Chars = {"usageSpecCharacteristic", {array, []}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars]}.

-spec pla_chars(Characteristics) -> Characteristics
	when
		Characteristics :: {array, list()} | [tuple()].
%% @doc CODEC for Pricing Logic Algorithm characteristics.
pla_chars({array, L} = _Characteristics) ->
	pla_chars(L, []);
pla_chars(Characteristics) when is_list(Characteristics) ->
	{array, pla_chars(Characteristics, [])}.
%% @hidden
pla_chars([], Acc) ->
	lists:reverse(Acc).

-spec offer_status(Status) -> Status
	when
		Status :: atom() | string().
%% @doc CODEC for life cycle status of Product instance.
%% @private
offer_status("In Study") -> in_study;
offer_status("In Design") -> in_design;
offer_status("In Test") -> in_test;
offer_status("Active") -> active;
offer_status("Rejected") -> rejected;
offer_status("Launched") -> launched;
offer_status("Retired") -> retired;
offer_status("Obsolete") -> obsolete;
offer_status(in_study) -> "In Study";
offer_status(in_design) -> "In Design";
offer_status(in_test) -> "In Test";
offer_status(active) -> "Active";
offer_status(rejected) -> "Rejected";
offer_status(launched) -> "Launched";
offer_status(retired) -> "Retired";
offer_status(obsolete) -> "Obsolete".

-spec product_status(Status) -> Status
	when
		Status :: atom() | string().
%% @doc CODEC for life cycle status of Product Offering.
%% @private
product_status("Created") -> created;
product_status("Pending Active") -> pending_active;
product_status("Aborted") -> aborted;
product_status("Cancelled") -> cancelled;
product_status("Active") -> active;
product_status("Suspended") -> suspended;
product_status("Pending Terminate") -> pending_terminate;
product_status("Terminated") -> terminated;
product_status(created) -> "Created";
product_status(pending_active) -> "Pending Active";
product_status(aborted) -> "Aborted";
product_status(cancelled) -> "Cancelled";
product_status(active) -> "Active";
product_status(suspended) -> "Suspended";
product_status(pending_terminate) -> "Pending Terminate";
product_status(terminated) -> "Terminated".

-spec price_type(Type) -> Type
	when
		Type :: string() | usage | recurring | one_time | tariff.
%% @doc CODEC for Price Type.
%% @private
price_type("usage") -> usage;
price_type("recurring") -> recurring;
price_type("one_time") -> one_time;
price_type("tariff") -> tariff;
price_type(usage) -> "usage";
price_type(recurring) -> "recurring";
price_type(one_time) -> "one_time";
price_type(tariff) -> "tariff".

-spec price_period(Period) -> Period
	when
		Period :: string() | hourly | daily | weekly | monthly | yearly.
%% @doc CODEC for Recurring Charge Period.
%% @private
price_period(hourly) -> "hourly";
price_period(daily) -> "daily";
price_period(weekly) -> "weekly";
price_period(monthly) -> "monthly";
price_period(yearly) -> "yearly";
price_period("hourly") -> hourly;
price_period("daily") -> daily;
price_period("weekly") -> weekly;
price_period("monthly") -> monthly;
price_period("yearly") -> yearly.

-spec pla(Pla) -> Pla
	when
		Pla :: #pla{} | {struct, [tuple()]}.
%% @doc CODEC for Pricing login algorithm.
%% @private
pla(#pla{} = Pla) ->
	pla(record_info(fields,pla), Pla, []);
pla({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	pla(ObjectMembers, #pla{}).
%% @hidden
pla([name | T], #pla{name = Name} = P, Acc) when is_list(Name) ->
	pla(T, P, [{"name", Name} | Acc]);
pla([description | T], #pla{description = Description} = P, Acc)
		when is_list(Description) ->
	pla(T, P, [{"description", Description} | Acc]);
pla([status | T], #pla{status = Status} = P, Acc)
		when Status /= undefined ->
	StatusPla = product_status(Status),
	pla(T, P, [{"status", StatusPla} | Acc]);
pla([start_date | T], #pla{start_date = Start,
		end_date = undefined} = P, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	pla(T, P, [{"validFor", ValidFor} | Acc]);
pla([start_date | T], #pla{start_date = undefined,
		end_date = End} = P, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	pla(T, P, [{"validFor", ValidFor} | Acc]);
pla([start_date | T], #pla{start_date = Start,
		end_date = End} = P, Acc) when is_integer(Start), is_integer(End) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)},
			{"endDateTime", ocs_rest:iso8601(End)}]},
	pla(T, P, [{"validFor", ValidFor} | Acc]);
pla([end_date | T], P, Acc) ->
	pla(T, P, Acc);
pla([specification | T], #pla{specification = Spec} = P, Acc) ->
	pla(T, P, [{"plaSpecId", Spec} | Acc]);
pla([characteristics | T], #pla{characteristics = Chars} = P, Acc) ->
	pla(T, P, [{"plaSpecCharacteristicValue", pla_chars(Chars)} | Acc]);
pla([last_modified | T], #pla{last_modified = {Last, _}} = P, Acc)
		when is_integer(Last) ->
	pla(T, P, [{"lastUpdate", ocs_rest:iso8601(Last)} | Acc]);
pla([_H | T], P, Acc) ->
	pla(T, P, Acc);
pla([], #pla{name = Name} = _P, Acc) ->
	{struct, [{"id", Name} | lists:reverse(Acc)]}.
%% @hidden
pla([{"name", Name} | T], Acc) when is_list(Name) ->
	pla(T, Acc#pla{name = Name});
pla([{"description", Description} | T], Acc) when is_list(Description) ->
	pla(T, Acc#pla{description = Description});
pla([{"status", Status} | T], Acc) when is_list(Status) ->
	pla(T, Acc#pla{status = product_status(Status)});
pla([{"validFor", {struct, L}} | T], Acc) ->
	Acc1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} ->
			Acc#pla{start_date = ocs_rest:iso8601(Start)};
		false -> 
			Acc
	end,
	Acc2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} ->
			Acc1#pla{end_date = ocs_rest:iso8601(End)};
		false -> 
			Acc
	end,
	pla(T, Acc2);
pla([{"plaSpecId", Spec} | T], Acc) when is_list(Spec) ->
	pla(T, Acc#pla{specification = Spec});
pla([{"plaSpecCharacteristicValue", {array, Chars}} | T], Acc)
		when is_list(Chars)->
	pla(T, Acc#pla{characteristics = pla_chars({array, Chars})});
pla([{"lastUpdate", LastUpdate} | T], Acc) when is_list(LastUpdate) ->
	pla(T, Acc);
pla([_ | T], Acc) ->
	pla(T, Acc);
pla([], Acc) ->
	Acc.

-spec offer(Product) -> Product
	when
		Product :: #product{} | {struct, [tuple()]}.
%% @doc CODEC for Product Offering.
%% @private
offer(#product{} = Product) ->
	offer(record_info(fields, product), Product, []);
offer({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	offer(ObjectMembers, #product{}).
%% @hidden
offer([name | T], #product{name = Name} = P, Acc) when is_list(Name) ->
	offer(T, P, [{"name", Name} | Acc]);
offer([description | T], #product{description = Description} = P,
	Acc) when is_list(Description) ->
	offer(T, P, [{"description", Description} | Acc]);
offer([specification | T],
		#product{specification = ProdSpecId} = P, Acc) when is_list(ProdSpecId) ->
	{struct, L} = product_spec(ProdSpecId),
	{_, Id} = lists:keyfind("id", 1, L),
	{_, Href} = lists:keyfind("href", 1, L),
	Name = proplists:get_value("name", L),
	Spec = {struct, [{"id", Id}, {"href", Href}, {"name", Name}]},
	offer(T, P, [{"productSpecification", Spec} | Acc]);
offer([bundle | T],
		#product{bundle = Bundle} = P, Acc) when length(Bundle) > 0 ->
	Array = [bundled_po(B) || B <- Bundle],
	offer(T, P, [{"bundledProductOffering", {array, Array}} | Acc]);
offer([status | T], #product{status = Status} = P, Acc)
		when Status /= undefined ->
	StatusString = offer_status(Status),
	offer(T, P, [{"lifecycleStatus", StatusString} | Acc]);
offer([start_date | T], #product{start_date = Start,
		end_date = undefined} = P, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	offer(T, P, [{"validFor", ValidFor} | Acc]);
offer([start_date | T], #product{start_date = undefined,
		end_date = End} = P, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	offer(T, P, [{"validFor", ValidFor} | Acc]);
offer([start_date | T], #product{start_date = Start,
		end_date = End} = P, Acc) when is_integer(Start), is_integer(End) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)},
			{"endDateTime", ocs_rest:iso8601(End)}]},
	offer(T, P, [{"validFor", ValidFor} | Acc]);
offer([end_date | T], P, Acc) ->
	offer(T, P, Acc);
offer([price | T], #product{price = Prices1} = P, Acc)
		when is_list(Prices1) ->
	Prices2 = [price(Price) || Price <- Prices1],
	offer(T, P, [{"productOfferingPrice", {array, Prices2}} | Acc]);
offer([char_value_use | T], #product{char_value_use = CharValueUses} = P, Acc) ->
	offer(T, P, [{"prodSpecCharValueUse", char_value_uses(CharValueUses)} | Acc]);
offer([last_modified | T], #product{last_modified = {Last, _}} = P, Acc)
		when is_integer(Last) ->
	offer(T, P, [{"lastUpdate", ocs_rest:iso8601(Last)} | Acc]);
offer([_H | T], P, Acc) ->
	offer(T, P, Acc);
offer([], #product{name = Name, bundle = [] ,
		specification = S}, Acc) when S /= undefined ->
	H = [{"id", Name}, {"href", ?offeringPath ++ Name}, {"isBundle", false}],
	{struct, H ++ lists:reverse(Acc)};
offer([], #product{name = Name, bundle = L,
		specification = undefined}, Acc) when length(L) > 0 ->
	H = [{"id", Name}, {"href", ?offeringPath ++ Name}, {"isBundle", true}],
	{struct, H ++ lists:reverse(Acc)}.
%% @hidden
offer([{"id", ID} | T], Acc) when is_list(ID) ->
	offer(T, Acc);
offer([{"href", URI} | T], Acc) when is_list(URI) ->
	offer(T, Acc);
offer([{"name", Name} | T], Acc) when is_list(Name) ->
	offer(T, Acc#product{name = Name});
offer([{"description", Description} | T], Acc) when is_list(Description) ->
	offer(T, Acc#product{description = Description});
offer([{"validFor", {struct, L}} | T], Acc) ->
	Acc1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} ->
			Acc#product{start_date = ocs_rest:iso8601(Start)};
		false ->
			Acc
	end,
	Acc2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} ->
			Acc1#product{end_date = ocs_rest:iso8601(End)};
		false ->
			Acc
	end,
	offer(T, Acc2);
offer([{"lifecycleStatus", Status} | T], Acc) when is_list(Status) ->
	offer(T, Acc#product{status = offer_status(Status)});
offer([{"productSpecification", {struct, L}} | T], Acc) when is_list(L) ->
	Acc1 = case lists:keyfind("id", 1, L) of
		{_, ID} when is_list(ID) ->
			Acc#product{specification = ID};
		false ->
			Acc
	end,
	offer(T, Acc1);
offer([{"bundledProductOffering", {array, Array}} | T], Acc)
		when is_list(Array) ->
	Bundle = [bundled_po(B) || B <- Array],
	offer(T, Acc#product{bundle = Bundle});
offer([{"isCustomerVisible", Visible} | T], Acc) when is_boolean(Visible) ->
	offer(T, Acc);
offer([{"productOfferingPrice", {array, Prices1}} | T], Acc) when is_list(Prices1) ->
	Prices2 = [price(Price) || Price <- Prices1],
	offer(T, Acc#product{price = Prices2});
offer([{"prodSpecCharValueUse", {array, _} = CharValueUses} | T], Acc) ->
	offer(T, Acc#product{char_value_use = char_value_uses(CharValueUses)});
offer([{"lastUpdate", LastUpdate} | T], Acc) when is_list(LastUpdate) ->
	offer(T, Acc);
offer([_ | T], Acc) ->
	offer(T, Acc);
offer([], #product{bundle = [], specification = S} = Acc)
		when S /= undefined ->
	Acc;
offer([], #product{bundle = L, specification = undefined} = Acc)
		when length(L) > 0 ->
	Acc.

-spec bundled_po(Bundled) -> Bundled
	when
		Bundled :: #bundled_po{} | {struct, list()}.
bundled_po(#bundled_po{} = B) ->
	bundled_po(record_info(fields, bundled_po), B, []);
bundled_po({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	bundled_po(ObjectMembers, #bundled_po{}).
%% @hidden
bundled_po([name | T], #bundled_po{name = Name} = B, Acc)
		when is_list(Name) ->
	Header = [{"href", ?offeringPath ++ Name}, {"name", Name}, {"id", Name}],
	bundled_po(T, B, Header ++ Acc);
bundled_po([status | T], #bundled_po{status = Status} = B, Acc)
		when Status /= undefined ->
	bundled_po(T, B, [{"lifecycleStatus", offer_status(Status)} | Acc]);
bundled_po([default | T], #bundled_po{default = undefined,
		lower_limit = undefined, upper_limit = undefined} = B, Acc) ->
	bundled_po(T, B, Acc);
bundled_po([default | T], #bundled_po{default= N1,
		upper_limit = N2, lower_limit = N3} = B, Acc) ->
	O1 = case N1 of
		undefined ->
			[];
		N1 when is_integer(N1) ->
			[{"numberRelOfferDefault", N1}]
	end,
	O2 = case N2 of
		undefined ->
			O1;
		N2 when is_integer(N2) ->
			[{"numberRelOfferUpperLimit", N2} | O1]
	end,
	O3 = case N3 of
		undefined ->
			O2;
		N3 when is_integer(N3) ->
			[{"numberRelOfferLowerLimit", N3} | O2]
	end,
	bundled_po(T, B, [{"bundledProductOfferingOption", {struct, O3}} | Acc]);
bundled_po([_H | T], B, Acc) ->
	bundled_po(T, B, Acc);
bundled_po([], _, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
bundled_po([{"name", Name} | T], Acc) when is_list(Name) ->
	bundled_po(T, Acc#bundled_po{name = Name});
bundled_po([{"lifecycleStatus", Status} | T], Acc)
		when is_list(Status) ->
	bundled_po(T, Acc#bundled_po{status = offer_status(Status)});
bundled_po([{"bundledProductOfferingOption", {struct, L}} | T], Acc)
		when is_list(L) ->
	LowerLimit = proplists:get_value("numberRelOfferLowerLimit", L),
	UpperLimit = proplists:get_value("numberRelOfferUpperLimit", L),
	Default = proplists:get_value("numberRelOfferDefault", L),
	NewAcc = Acc#bundled_po{lower_limit = LowerLimit,
			upper_limit = UpperLimit, default = Default},
	bundled_po(T, NewAcc);
bundled_po([_H | T], Acc) ->
	bundled_po(T, Acc);
bundled_po([], Acc) ->
	Acc.

-spec price(Price) -> Price
	when
		Price :: #price{} | {struct, list()}.
%% @doc CODEC for Product Offering Price.
%% @private
price(#price{} = Price) ->
	price(record_info(fields, price), Price, []);
price({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	price(ObjectMembers, #price{}).
%% @hidden
price([name| T], #price{name = Name} = P, Acc) when is_list(Name) ->
	price(T, P, [{"name", Name} | Acc]);
price([description | T], #price{description = Description} = P, Acc)
		when is_list(Description) ->
	price(T, P, [{"description", Description} | Acc]);
price([start_date | T], #price{start_date = Start,
		end_date = undefined} = P, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	price(T, P, [{"validFor", ValidFor} | Acc]);
price([start_date | T], #price{start_date = undefined,
		end_date = End} = P, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	price(T, P, [{"validFor", ValidFor} | Acc]);
price([start_date | T], #price{start_date = Start,
		end_date = End} = P, Acc) when is_integer(Start), is_integer(End) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)},
			{"endDateTime", ocs_rest:iso8601(End)}]},
	price(T, P, [{"validFor", ValidFor} | Acc]);
price([end_date | T], P, Acc) ->
	price(T, P, Acc);
price([type | T], #price{type = Type} = P, Acc)
		when Type /= undefined ->
	price(T, P, [{"priceType", price_type(Type)} | Acc]);
price([period | T], #price{period = Period} = P, Acc)
	when Period /= undefined ->
	price(T, P, [{"recurringChargePeriod", price_period(Period)} | Acc]);
price([units | T], #price{units = octets, size = Size} = P, Acc)
		when is_integer(Size) ->
	price(T, P, [{"unitOfMeasure", integer_to_list(Size) ++ "b"} | Acc]);
price([units | T], #price{units = seconds, size = Size} = P, Acc)
		when is_integer(Size) ->
	price(T, P, [{"unitOfMeasure", integer_to_list(Size) ++ "s"} | Acc]);
price([amount | T], #price{amount = Amount, currency = Currency} = P, Acc)
		when is_integer(Amount), is_list(Currency) ->
	Price = {struct, [{"taxIncludedAmount", ocs_rest:decimal(Amount)},
			{"currencyCode", Currency}]},
	price(T, P, [{"price", Price} | Acc]);
price([amount | T], #price{amount = Amount} = P, Acc)
		when is_integer(Amount) ->
	Price = {struct, [{"taxIncludedAmount", ocs_rest:decimal(Amount)}]},
	price(T, P, [{"price", Price} | Acc]);
price([char_value_use | T], #price{char_value_use = CharValueUses} = P, Acc) ->
	price(T, P, [{"prodSpecCharValueUse", char_value_uses(CharValueUses)} | Acc]);
price([alteration | T], #price{alteration = Alteration} = P, Acc)
		when is_record(Alteration, alteration) ->
	price(T, P, [{"productOfferPriceAlteration", alteration(Alteration)} | Acc]);
price([_ | T], P, Acc) ->
	price(T, P, Acc);
price([], _P, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
price([{"id", _ID} | T], Acc) ->
	price(T, Acc);
price([{"href", _URI} | T], Acc) ->
	price(T, Acc);
price([{"name", Name} | T], Acc) when is_list(Name) ->
	price(T, Acc#price{name = Name});
price([{"description", Description} | T], Acc) when is_list(Description) ->
	price(T, Acc#price{description = Description});
price([{"validFor", {struct, L}} | T], Acc) when is_list(L) ->
	Acc1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} ->
			Acc#price{start_date = ocs_rest:iso8601(Start)};
		false ->
			Acc
	end,
	Acc2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} ->
			Acc1#price{end_date = ocs_rest:iso8601(End)};
		false ->
			Acc
	end,
	price(T, Acc2);
price([{"priceType", Type} | T], Acc) when is_list(Type) ->
	price(T, Acc#price{type = price_type(Type)});
price([{"unitOfMeasure", UnitOfMeasure} | T], Acc)
		when is_list(UnitOfMeasure) ->
	case lists:last(UnitOfMeasure) of
		$b ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			price(T, Acc#price{units = octets, size = list_to_integer(N)});
		$k ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			price(T, Acc#price{units = octets,
					size = list_to_integer(N) * 1000});
		$m ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			price(T, Acc#price{units = octets,
					size = list_to_integer(N) * 1000000});
		$g ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			price(T, Acc#price{units = octets,
					size = list_to_integer(N) * 1000000000});
		$s ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			price(T, Acc#price{units = seconds, size = list_to_integer(N)});
		_ ->
			price(T, Acc#price{size = list_to_integer(UnitOfMeasure)})
	end;
price([{"price", {struct, L}} | T], Acc) when is_list(L) ->
	Acc1 = case lists:keyfind("taxIncludedAmount", 1, L) of
		{_, Amount} when is_integer(Amount) ->
			Acc#price{amount = Amount * 1000000};
		{_, Amount} when is_list(Amount) ->
			Acc#price{amount = ocs_rest:decimal(Amount)};
		_ ->
			Acc
	end,
	Acc2 = case lists:keyfind("currencyCode", 1, L) of
		{_, Currency} when is_list(Currency) ->
			Acc1#price{currency = Currency};
		_ ->
			Acc1
	end,
	price(T, Acc2);
price([{"recurringChargePeriod", Period} | T], Acc) when is_list(Period) ->
	price(T, Acc#price{period = price_period(Period)});
price([{"prodSpecCharValueUse", {array, _} = CharValueUses} | T], Acc) ->
	price(T, Acc#price{char_value_use = char_value_uses(CharValueUses)});
price([{"productOfferPriceAlteration", {struct, L} = Alteration} | T], Acc)
		when is_list(L) ->
	price(T, Acc#price{alteration = alteration(Alteration)});
price([], Acc) ->
	Acc.

-spec alteration(Alteration) -> Alteration
	when
		Alteration :: #alteration{} | {struct, [tuple()]}.
%% @doc CODEC for Product Offering Price Alteration.
%% @private
alteration(#alteration{} = A) ->
	alteration(record_info(fields, alteration), A, []);
alteration({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	alteration(ObjectMembers, #alteration{}).
%% @hidden
alteration([name| T], #alteration{name = Name} = A, Acc) when is_list(Name) ->
	alteration(T, A, [{"name", Name} | Acc]);
alteration([description | T], #alteration{description = Description} = A, Acc)
		when is_list(Description) ->
	alteration(T, A, [{"description", Description} | Acc]);
alteration([start_date | T], #alteration{start_date = Start,
		end_date = undefined} = A, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	alteration(T, A, [{"validFor", ValidFor} | Acc]);
alteration([start_date | T], #alteration{start_date = undefined,
		end_date = End} = A, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	alteration(T, A, [{"validFor", ValidFor} | Acc]);
alteration([start_date | T], #alteration{start_date = Start,
		end_date = End} = A, Acc) when is_integer(Start), is_integer(End) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)},
			{"endDateTime", ocs_rest:iso8601(End)}]},
	alteration(T, A, [{"validFor", ValidFor} | Acc]);
alteration([end_date | T], A, Acc) ->
	alteration(T, A, Acc);
alteration([type | T], #alteration{type = Type} = A, Acc)
		when Type /= undefined ->
	alteration(T, A, [{"priceType", price_type(Type)} | Acc]);
alteration([period | T], #alteration{period = Period} = A, Acc)
		when Period /= undefined ->
	alteration(T, A, [{"recurringChargePeriod", price_period(Period)} | Acc]);
alteration([units | T], #alteration{units = octets, size = Size} = A, Acc)
		when is_integer(Size) ->
	alteration(T, A, [{"unitOfMeasure", integer_to_list(Size) ++ "b"} | Acc]);
alteration([units | T], #alteration{units = seconds, size = Size} = A, Acc)
		when is_integer(Size) ->
	alteration(T, A, [{"unitOfMeasure", integer_to_list(Size) ++ "s"} | Acc]);
alteration([amount | T], #alteration{amount = Amount, currency = Currency} = A, Acc)
		when is_integer(Amount), is_list(Currency) ->
	Price = {struct, [{"taxIncludedAmount", ocs_rest:decimal(Amount)},
			{"currencyCode", Currency}]},
	alteration(T, A, [{"price", Price} | Acc]);
alteration([amount | T], #alteration{units = cents, amount = Amount} = A, Acc)
		when is_integer(Amount) ->
	Price = {struct, [{"taxIncludedAmount", ocs_rest:decimal(Amount)}]},
	alteration(T, A, [{"price", Price} | Acc]);
alteration([_ | T], A, Acc) ->
	alteration(T, A, Acc);
alteration([], _A, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
alteration([{"id", _ID} | T], Acc) ->
	alteration(T, Acc);
alteration([{"href", _URI} | T], Acc) ->
	alteration(T, Acc);
alteration([{"name", Name} | T], Acc) when is_list(Name) ->
	alteration(T, Acc#alteration{name = Name});
alteration([{"description", Description} | T], Acc) when is_list(Description) ->
	alteration(T, Acc#alteration{description = Description});
alteration([{"validFor", {struct, L}} | T], Acc) when is_list(L) ->
	Acc1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} when is_list(Start) ->
			Acc#alteration{start_date = ocs_rest:iso8601(Start)};
		false ->
			Acc
	end,
	Acc2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} when is_list(End) ->
			Acc1#alteration{end_date = ocs_rest:iso8601(End)};
		false ->
			Acc1
	end,
	alteration(T, Acc2);
alteration([{"priceType", Type} | T], Acc) ->
	alteration(T, Acc#alteration{type = price_type(Type)});
alteration([{"unitOfMeasure", UnitOfMeasure} | T], Acc) ->
	case lists:last(UnitOfMeasure) of
		$b ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			alteration(T, Acc#alteration{units = octets,
					size = list_to_integer(N)});
		$k ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			alteration(T, Acc#alteration{units = octets,
					size = list_to_integer(N) * 1000});
		$m ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			alteration(T, Acc#alteration{units = octets,
					size = list_to_integer(N) * 1000000});
		$g ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			alteration(T, Acc#alteration{units = octets,
					size = list_to_integer(N) * 1000000000});
		$s ->
			N = lists:sublist(UnitOfMeasure, length(UnitOfMeasure) - 1),
			alteration(T, Acc#alteration{units = seconds, size = list_to_integer(N)});
		_ ->
			alteration(T, Acc#alteration{size = list_to_integer(UnitOfMeasure)})
	end;
alteration([{"price", {struct, L}} | T], Acc) ->
	Acc1 = case lists:keyfind("taxIncludedAmount", 1, L) of
		{_, Amount} when is_integer(Amount) ->
			Acc#alteration{amount = Amount * 1000000};
		{_, Amount} when is_list(Amount) ->
			Acc#alteration{amount = ocs_rest:decimal(Amount)};
		_ ->
			Acc
	end,
	Acc2 = case lists:keyfind("currencyCode", 1, L) of
		{_, Currency} when is_list(Currency) ->
			Acc1#alteration{currency = Currency};
		_ ->
			Acc1
	end,
	alteration(T, Acc2);
alteration([{"recurringChargePeriod", Period} | T], Acc)
		when is_list(Period) ->
	alteration(T, Acc#alteration{period = price_period(Period)});
alteration([], Acc) ->
	Acc.

-spec char_value_uses(CharValueUses) -> CharValueUses
	when
		CharValueUses :: [#char_value_use{}] | {array, [tuple()]}.
%% @doc CODEC for ProductSpecCharValueUses.
%% @private
char_value_uses(CharValueUses) when is_list(CharValueUses) ->
	{array, char_value_uses(CharValueUses, [])};
char_value_uses({array, CharValueUses}) when is_list(CharValueUses) ->
	char_value_uses(CharValueUses, []).
%% @hidden
char_value_uses([H | T], Acc) ->
	char_value_uses(T, [char_value_use(H) | Acc]);
char_value_uses([], Acc) ->
	lists:reverse(Acc).

-spec char_value_use(CharValueUse) -> CharValueUse
	when
		CharValueUse :: #char_value_use{} | {struct, [tuple()]}.
%% @doc CODEC for ProductSpecCharValueUse.
%% @private
char_value_use(#char_value_use{} = C) ->
	char_value_use(record_info(fields, char_value_use), C, []);
char_value_use({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	char_value_use(ObjectMembers, #char_value_use{}).
%% @hidden
char_value_use([name | T], #char_value_use{name = undefined} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([name | T], #char_value_use{name = Name} = C, Acc)
		when is_list(Name) ->
	char_value_use(T, C, [{"name", Name} | Acc]);
char_value_use([description | T],
		#char_value_use{description = undefined} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([description | T],
		#char_value_use{description = Description} = C, Acc)
		when is_list(Description) ->
	char_value_use(T, C, [{"description", Description} | Acc]);
char_value_use([type | T], #char_value_use{type = undefined} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([type | T], #char_value_use{type = Type} = C, Acc)
		when is_list(Type) ->
	char_value_use(T, C, [{"valueType", Type} | Acc]);
char_value_use([min | T], #char_value_use{min = undefined} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([min | T], #char_value_use{min = Min} = C, Acc)
		when is_integer(Min) ->
	char_value_use(T, C, [{"minCardinality", Min} | Acc]);
char_value_use([max | T], #char_value_use{max = undefined} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([max | T], #char_value_use{max = Max} = C, Acc)
		when is_integer(Max) ->
	char_value_use(T, C, [{"maxCardinality", Max} | Acc]);
char_value_use([specification | T],
		#char_value_use{specification = Spec} = P, Acc) when is_list(Spec) ->
	{struct, L} = product_spec(Spec),
	{_, Id} = lists:keyfind("id", 1, L),
	{_, Href} = lists:keyfind("href", 1, L),
	Spec1 = {struct, [{"id", Id}, {"href", Href}]},
	char_value_use(T, P, [{"productSpecification", Spec1} | Acc]);
char_value_use([start_date | T], #char_value_use{start_date = undefined,
		end_date = undefined} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([start_date | T],
		#char_value_use{start_date = Start, end_date = End} = C, Acc)
		when is_integer(Start), is_integer(End) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)},
			{"endDateTime", ocs_rest:iso8601(End)}]},
	char_value_use(T, C, [{"validFor", ValidFor} | Acc]);
char_value_use([start_date | T], #char_value_use{start_date = Start,
		end_date = undefined} = C, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	char_value_use(T, C, [{"validFor", ValidFor} | Acc]);
char_value_use([start_date | T], #char_value_use{start_date = undefined,
		end_date = End} = C, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	char_value_use(T, C, [{"validFor", ValidFor} | Acc]);
char_value_use([end_date | T], #char_value_use{} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([values | T], #char_value_use{values = []} = C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([values | T], #char_value_use{values = Values} = C, Acc)
		when is_list(Values) ->
	char_value_use(T, C, [{"productSpecCharacteristicValue",
			char_values(Values)} | Acc]);
char_value_use([_ | T], C, Acc) ->
	char_value_use(T, C, Acc);
char_value_use([], _, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
char_value_use([{"name", Name} | T], Acc) when is_list(Name) ->
	char_value_use(T, Acc#char_value_use{name = Name});
char_value_use([{"description", Description} | T], Acc)
		when is_list(Description) ->
	char_value_use(T, Acc#char_value_use{description = Description});
char_value_use([{"valueType", ValueType} | T], Acc)
		when is_list(ValueType) ->
	char_value_use(T, Acc#char_value_use{type = ValueType});
char_value_use([{"minCardinality", MinCardinality} | T], Acc)
		when is_integer(MinCardinality) ->
	char_value_use(T, Acc#char_value_use{min = MinCardinality});
char_value_use([{"maxCardinality", MaxCardinality} | T], Acc)
		when is_integer(MaxCardinality) ->
	char_value_use(T, Acc#char_value_use{max = MaxCardinality});
char_value_use([{"productSpecification", {struct, L}} | T], Acc) when is_list(L) ->
	Acc1 = case lists:keyfind("id", 1, L) of
		{_, ID} when is_list(ID) ->
			Acc#char_value_use{specification = ID};
		false ->
			Acc
	end,
	char_value_use(T, Acc1);
char_value_use([{"validFor", {struct, L}} | T], Acc) when is_list(L) ->
	NewAcc = case {lists:keyfind("startDateTime", 1, L),
			lists:keyfind("endDateTime", 1, L)} of
		{{_, Start}, false} ->
			Acc#char_value_use{start_date = ocs_rest:iso8601(Start)};
		{{_, Start}, {_, End}} ->
			Acc#char_value_use{start_date = ocs_rest:iso8601(Start),
					end_date = ocs_rest:iso8601(End)};
		{false, {_, End}} ->
			Acc#char_value_use{end_date = ocs_rest:iso8601(End)}
	end,
	char_value_use(T, NewAcc);
char_value_use([{"productSpecCharacteristicValue", {array, _} = Values} | T], Acc) ->
	char_value_use(T, Acc#char_value_use{values = char_values(Values)});
char_value_use([{"productSpecification", _Spec} | T], Acc) ->
	char_value_use(T, Acc);
char_value_use([], Acc) ->
	Acc.

-spec char_values(CharValues) -> CharValues
	when
		CharValues :: [#char_value{}] | {array, [tuple()]}.
%% @doc CODEC for ProductSpecCharacteristicValues.
%% @private
char_values(CharValues) when is_list(CharValues) ->
	{array, char_values(CharValues, [])};
char_values({array, CharValues}) when is_list(CharValues) ->
	char_values(CharValues, []).
%% @hidden
char_values([H | T], Acc) ->
	char_values(T, [char_value(H) | Acc]);
char_values([], Acc) ->
	lists:reverse(Acc).

-spec char_value(CharValue) -> CharValue
	when
		CharValue :: #char_value{} | {struct, [tuple()]}.
%% @doc CODEC for ProductSpecCharacteristicValue.
%% @private
char_value(#char_value{} = V) ->
	char_value(record_info(fields, char_value), V, []);
char_value({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	char_value(ObjectMembers, #char_value{}).
%% @hidden
char_value([default | T], #char_value{default = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([default | T], #char_value{default = Default} = V, Acc)
		when is_boolean(Default) ->
	char_value(T, V, [{"default", Default} | Acc]);
char_value([units | T], #char_value{units = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([units | T], #char_value{units = Units} = V, Acc)
		when is_list(Units) ->
	char_value(T, V, [{"unitOfMeasure", Units} | Acc]);
char_value([units | T], #char_value{units = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([start_date | T], #char_value{start_date = undefined,
		end_date = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([start_date | T],
		#char_value{start_date = Start, end_date = End} = V, Acc)
		when is_integer(Start), is_integer(End) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)},
			{"endDateTime", ocs_rest:iso8601(End)}]},
	char_value(T, V, [{"validFor", ValidFor} | Acc]);
char_value([start_date | T], #char_value{start_date = Start,
		end_date = undefined} = V, Acc) when is_integer(Start) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(Start)}]},
	char_value(T, V, [{"validFor", ValidFor} | Acc]);
char_value([start_date | T], #char_value{start_date = undefined,
		end_date = End} = V, Acc) when is_integer(End) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(End)}]},
	char_value(T, V, [{"validFor", ValidFor} | Acc]);
char_value([end_date | T], #char_value{} = V, Acc) ->
	char_value(T, V, Acc);
char_value([value | T], #char_value{value = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([value | T], #char_value{value = Value} = V, Acc) ->
	char_value(T, V, [{"value", char_value_type(Value)} | Acc]);
char_value([from | T], #char_value{from = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([from | T], #char_value{from = From} = V, Acc) ->
	char_value(T, V, [{"valueFrom", From} | Acc]);
char_value([to | T], #char_value{to = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([to | T], #char_value{to = To} = V, Acc) ->
	char_value(T, V, [{"valueTo", To} | Acc]);
char_value([type | T], #char_value{type = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([type | T], #char_value{type = Type} = V, Acc)
		when is_list(Type) ->
	char_value(T, V, [{"valueType", Type} | Acc]);
char_value([interval | T], #char_value{interval = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([interval | T], #char_value{interval = Interval} = V, Acc)
		when Interval == open; Interval == closed,
		Interval == closed_bottom; Interval == closed_top ->
	char_value(T, V, [{"rangeInterval", atom_to_list(Interval)} | Acc]);
char_value([regex | T], #char_value{regex = undefined} = V, Acc) ->
	char_value(T, V, Acc);
char_value([regex | T], #char_value{regex = {_, RegEx}} = V, Acc) ->
	char_value(T, V, [{"regex", RegEx} | Acc]);
char_value([], _, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
char_value([{"default", Default} | T], Acc) when is_boolean(Default) ->
	char_value(T, Acc#char_value{default = Default});
char_value([{"unitOfMeasure", Units} | T], Acc) when is_list(Units) ->
	char_value(T, Acc#char_value{units = Units});
char_value([{"validFor", {struct, L}} | T], Acc) when is_list(L) ->
	NewAcc = case {lists:keyfind("startDateTime", 1, L),
			lists:keyfind("endDateTime", 1, L)} of
		{{_, Start}, false} when is_list(Start) ->
			Acc#char_value{start_date = ocs_rest:iso8601(Start)};
		{{_, Start}, {_, End}} when is_list(Start), is_list(End) ->
			Acc#char_value{start_date = ocs_rest:iso8601(Start),
					end_date = ocs_rest:iso8601(End)};
		{false, {_, End}} when is_list(End) ->
			Acc#char_value{end_date = ocs_rest:iso8601(End)}
	end,
	char_value(T, NewAcc);
char_value([{"value", Value} | T], Acc)
		when is_integer(Value); is_float(Value);
		is_list(Value); is_boolean(Value) ->
	char_value(T, Acc#char_value{value = Value});
char_value([{"value", {struct, _} = Value} | T], Acc) ->
	char_value(T, Acc#char_value{value = char_value_type(Value)});
char_value([{"valueFrom", From} | T], Acc)
		when is_integer(From); is_list(From) ->
	char_value(T, Acc#char_value{from = From});
char_value([{"valueTo", To} | T], Acc)
		when is_integer(To); is_list(To) ->
	char_value(T, Acc#char_value{to = To});
char_value([{"valueType", Type} | T], Acc) when is_list(Type) ->
	char_value(T, Acc#char_value{type = Type});
char_value([{"rangeInterval", "open"} | T], Acc) ->
	char_value(T, Acc#char_value{interval = open});
char_value([{"rangeInterval", "closed"} | T], Acc) ->
	char_value(T, Acc#char_value{interval = closed});
char_value([{"rangeInterval", "closedBottom"} | T], Acc) ->
	char_value(T, Acc#char_value{interval = closed_bottom});
char_value([{"rangeInterval", "closedTop"} | T], Acc) ->
	char_value(T, Acc#char_value{interval = closed_top});
char_value([{"regex", RegEx} | T], Acc) when is_list(RegEx) ->
	{ok, MP} = re:compile(RegEx),
	char_value(T, Acc#char_value{regex = {MP, RegEx}});
char_value([], Acc) ->
	Acc.

%% @hidden
char_value_type({struct, [{"lowerValue", {struct, _} = LV},
		{"upperValue", {struct, _} = UV}]}) ->
	#range{lower = char_value_type(LV), upper = char_value_type(UV)};
char_value_type({struct, [{"upperValue", {struct, _} = UV},
		{"lowerValue", {struct, _} = LV}]}) ->
	#range{lower = char_value_type(LV), upper = char_value_type(UV)};
char_value_type({struct, [{"amount", V1}, {"units", V2}]})
		when is_integer(V1), is_list(V2) ->
	#quantity{amount = V1, units = V2};
char_value_type({struct, [{"units", V2}, {"amount", V1}]})
		when is_integer(V1), is_list(V2) ->
	#quantity{amount = V1, units = V2};
char_value_type({struct, [{"numerator", {struct, _} = NV},
		{"denominator", {struct, _} = DV}]}) ->
	#rate{numerator = char_value_type(NV),
		denominator = char_value_type(DV)};
char_value_type({struct, [{"denominator", {struct, _} = DV},
		{"numerator", {struct, _} = NV}]}) ->
	#rate{numerator = char_value_type(NV),
		denominator = char_value_type(DV)};
char_value_type(#quantity{units = Units, amount = Amount}) ->
	{struct, [{"units", Units}, {"amount", Amount}]};
char_value_type(#range{lower = Lower, upper = Upper}) ->
	{struct, [{"lowerValue", char_value_type(Lower)},
			{"upperValue", char_value_type(Upper)}]};
char_value_type(#rate{numerator = Numerator, denominator = Denominator}) ->
	{struct, [{"numerator", char_value_type(Numerator)},
			{"denominator", char_value_type(Denominator)}]};
char_value_type(Value) when is_integer(Value); is_list(Value) ->
	Value.
	
-spec inventory(Subscription) -> Subscription
	when
		Subscription :: #subscriber{} | {struct, [tuple()]}.
%% @doc CODEC for Product Inventory.
inventory({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	ProductInstance  = instance({struct, ObjectMembers}),
	F = fun(Key) ->
			case proplists:get_value(Key, ProductInstance#product_instance.characteristics) of
				undefined ->
					undefined;
				Value ->
					list_to_binary(Value)
			end
	end,
	Username = F("subscriberIdentity"),
	Password = F("subscriberPassword"),
	#subscriber{name = Username, password = Password, product = ProductInstance};
inventory(#subscriber{name = Username, password = Password,
		product = #product_instance{characteristics = Chars}  = ProductInstance}) ->
	F1 = fun(CChars, _, undefined) ->
			CChars;
		(CChars, Key, Value) ->
			lists:keystore(Key, 1, CChars, {Key, binary_to_list(Value)})
	end,
	Chars1 = F1(Chars, "subscriberIdentity", Username),
	Chars2 = F1(Chars1, "subscriberPassword", Password),
	{struct, Json1} = instance(ProductInstance#product_instance{characteristics = Chars2}),
	F2 = fun(Key) ->
			case proplists:get_value(Key, ProductInstance#product_instance.characteristics) of
				undefined ->
					binary_to_list(Username);
				Value ->
					Value
			end
	end,
	Username1 = F2("subscriberIdentity"),
	Id = {"id", Username1},
	Href = {"href", ?inventoryPath ++ Username1},
	Name = {"name", Username1},
	{struct, [Id, Href, Name | Json1]}.

-spec instance(Instance) -> Instance
	when
		Instance :: #product_instance{} | {struct, [tuple()]}.
%% @doc CODEC for Product Inventory.
instance({struct, ObjectMembers}) ->
	instance(ObjectMembers, #product_instance{});
instance(ProductInstance) ->
	{struct, instance(record_info(fields, product_instance), ProductInstance, [])}.
%% @hidden
instance([{"characteristic", Chars} | T], Acc) ->
	NewChars = instance_chars(Chars),
	instance(T, Acc#product_instance{characteristics = NewChars});
instance([{"productOffering", {struct, Offer}} | T], Acc) ->
	instance(T, Acc#product_instance{product = product({struct, Offer})});
instance([{"status", Status} | T], Acc) ->
	instance(T, Acc#product_instance{status = product_status(Status)});
instance([{"startDate", SDate} | T], Acc) ->
	instance(T, Acc#product_instance{start_date = ocs_rest:iso8601(SDate)});
instance([{"terminationDate", TDate} | T], Acc) ->
	instance(T, Acc#product_instance{termination_date = ocs_rest:iso8601(TDate)});
instance([_ | T], Acc) ->
	instance(T, Acc);
instance([], Acc) ->
	Acc.
%% @hidden
instance([product | T], #product_instance{product = ProdID} = ProductInstance, Acc) ->
	Offer = {"productOffering", product(ProdID)},
	instance(T, ProductInstance, [Offer | Acc]);
instance([characteristics | T], #product_instance{characteristics = Chars} = ProductInstance, Acc) ->
	Characteristics = {"characteristic", instance_chars(Chars)},
	instance(T, ProductInstance, [Characteristics | Acc]);
instance([status | T], #product_instance{status = undefined} = ProductInstance, Acc) ->
	instance(T, ProductInstance,  Acc);
instance([status | T], #product_instance{status = Status} = ProductInstance, Acc) ->
	NewAcc = [{"status", product_status(Status)}  | Acc],
	instance(T, ProductInstance,  NewAcc);
instance([start_date | T], #product_instance{start_date = undefined} = ProductInstance, Acc) ->
	instance(T, ProductInstance,  Acc);
instance([start_date | T], #product_instance{start_date = SDate} = ProductInstance, Acc) ->
	NewAcc = [{"startDate", ocs_rest:iso8601(SDate)}  | Acc],
	instance(T, ProductInstance,  NewAcc);
instance([termination_date | T], #product_instance{termination_date = undefined} = ProductInstance, Acc) ->
	instance(T, ProductInstance,  Acc);
instance([termination_date | T], #product_instance{termination_date = TDate} = ProductInstance, Acc) ->
	NewAcc = [{"terminationDate", ocs_rest:iso8601(TDate)}  | Acc],
	instance(T, ProductInstance,  NewAcc);
instance([_ | T], ProductInstance, Acc) ->
	instance(T, ProductInstance,  Acc);
instance([], _ProductInstance, Acc) ->
	lists:reverse(Acc).

-spec instance_chars(Characteristics) -> Characteristics
	when
		Characteristics :: {array, list()} | [tuple()].
%% @doc CODEC for Product Inventory characteristics.
instance_chars({array, Characteristics}) ->
	instance_chars(Characteristics, []);
instance_chars(Characteristics) ->
	{array, instance_chars(Characteristics, [])}.
%% @hidden
instance_chars([{struct, [{"name", Name}, {"value", Value}]} | T], Acc) ->
	instance_chars(T, [{Name, Value} | Acc]);
instance_chars([{struct, [{"value", Value}, {"name", Name}]} | T], Acc) ->
	instance_chars(T, [{Name, Value} | Acc]);
instance_chars([], Acc) ->
	lists:reverse(Acc).

%% @hidden
product({struct, Offer}) ->
	{_, ProdId} = lists:keyfind("id", 1, Offer),
	ProdId;
product(ProdID) ->
	ID = {"id", ProdID},
	Href = {"href", ?offeringPath ++ ProdID},
	Name = {"name", ProdID},
	{struct, [ID, Href, Name]}.

%% @hidden
query_filter(MFA, Codec, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			query_filter(MFA, Codec, NewQuery, Filters, Headers);
		false ->
			query_filter(MFA, Codec, Query, [], Headers)
	end.
%% @hidden
query_filter(MFA, Codec, Query, Filters, Headers) ->
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
							query_page(Codec, PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(Codec, PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(MFA, Codec, Query, Filters, Start, End)
					end;
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(Codec, PageServer, Etag, Query, Filters, Start, End)
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
					query_start(MFA, Codec, Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(MFA, Codec, Query, Filters, undefined, undefined)
	end.

%% @hidden
query_start({M, F, A}, Codec, Query, Filters, RangeStart, RangeEnd) ->
	case supervisor:start_child(ocs_rest_pagination_sup,
				[[M, F, A]]) of
		{ok, PageServer, Etag} ->
			query_page(Codec, PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
query_page(Codec, PageServer, Etag, Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			try
				case lists:keytake("sort", 1, Query) of
					{value, {_, "name"}, Q1} ->
						{lists:keysort(#product.name, Result), Q1};
					{value, {_, "-name"}, Q1} ->
						{lists:reverse(lists:keysort(#product.name, Result)), Q1};
					{value, {_, "description"}, Q1} ->
						{lists:keysort(#product.description, Result), Q1};
					{value, {_, "-description"}, Q1} ->
						{lists:reverse(lists:keysort(#product.description, Result)), Q1};
					{value, {_, "lifecycleStatus"}, Q1} ->
						{lists:keysort(#product.status, Result), Q1};
					{value, {_, "-lifecycleStatus"}, Q1} ->
						{lists:reverse(lists:keysort(#product.status, Result)), Q1};
					{value, {_, "startDate"}, Q1} ->
						{lists:keysort(#product.start_date, Result), Q1};
					{value, {_, "-startDate"}, Q1} ->
						{lists:reverse(lists:keysort(#product.start_date, Result)), Q1};
					{value, {_, "endDate"}, Q1} ->
						{lists:keysort(#product.end_date, Result), Q1};
					{value, {_, "-endDate"}, Q1} ->
						{lists:reverse(lists:keysort(#product.end_date, Result)), Q1};
					{value, {_, "price"}, Q1} ->
						{lists:keysort(#product.price, Result), Q1};
					{value, {_, "-price"}, Q1} ->
						{lists:reverse(lists:keysort(#product.price, Result)), Q1};
					false ->
						{Result, Query};
					_ ->
						throw(400)
				end
			of
				{SortedResult, _NewQuery} ->
					JsonObj = query_page1(lists:map(Codec, SortedResult), Filters, []),
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
query_page1(Json, [], []) ->
	Json;
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

