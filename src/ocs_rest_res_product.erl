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
-export([add_product_offering/1, add_product_inventory/1]).
-export([get_product_offering/1, get_product_offerings/2,
		patch_product_offering/3, get_product_inventory/1,
		get_product_inventories/2, patch_product_inventory/3]).
-export([get_catalog/2, get_catalogs/1]).
-export([get_category/2, get_categories/1]).
-export([get_product_spec/2, get_product_specs/1]).
-export([delete_product_offering/1, delete_product_inventory/1]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(catalogPath, "/catalogManagement/v2/catalog/").
-define(categoryPath, "/catalogManagement/v2/category/").
-define(specificationPath, "/catalogManagement/v2/productSpecification/").
-define(offeringPath, "/catalogManagement/v2/productOffering/").
-define(inventoryPath, "/inventoryManagement/v2/productOffering/").

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

-spec get_product_inventories(Query, Headers) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `GET /productInventoryManagement/v2/productOffering'.
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
%% @doc Respond to `GET /catalogManegment/v2/productSpecification'.
%% 	Retrieve all product specifications.
get_product_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [spec_product_network(),
					spec_product_fixed_quantity_pkg(),
					spec_product_rate_plan(),
					spec_product_wlan()]},
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

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec product_spec(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Get Product Specification by ID.
product_spec("1") ->
	spec_product_network();
product_spec("2") ->
	spec_product_fixed_quantity_pkg();
product_spec("3") ->
	spec_product_rate_plan();
product_spec("4") ->
	spec_product_wlan();
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
spec_product_network() ->
	Id = {"id", "1"},
	Href = {"href", ?specificationPath "1"},
	Name = {"name", "NetworkProductSpec"},
	Description = {"description", "Represents the common behaviour and description of an installed network product that will be provisioned in the network and that enables usages."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-06T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status]}.

%% @hidden
spec_product_fixed_quantity_pkg() ->
	Id = {"id", "2"},
	Href = {"href", ?specificationPath "2"},
	Name = {"name", "FixedQuantityPackageProductSpec"},
	Description = {"description", "Defines buckets of usage from which Usages will debit the bucket."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-06T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status]}.

%% @hidden
spec_product_rate_plan() ->
	Id = {"id", "3"},
	Href = {"href", ?specificationPath "3"},
	Name = {"name", "RatedPlanProductSpec"},
	Description = {"description", "Defines criteria to be used to gain special usage tariffs like the period (day, evening) or phone number."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-10-06T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	Chars = {"productSpecCharacteristic", {array, characteristic_product_rate_plane()}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars]}.

%% @hidden
spec_product_wlan() ->
	Id = {"id", "4"},
	Href = {"href", ?specificationPath "4"},
	Name = {"name", "WLANProductSpec"},
	Description = {"description", "Defines characteristics specific to pulic Wi-Fi use."},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2017-11-14T12:00:00Z"},
	Status = {"lifecycleStatus", "Active"},
	DepType = {"type", "dependency"},
	DepId = {"id", "1"},
	DepHref = {"href", ?specificationPath "1"},
	Depend = {struct, [DepId, DepHref, DepType]},
	Dependency = {"productSpecificationRelationship", {array, [Depend]}},
	Chars = {"productSpecCharacteristic", {array, characteristic_product_wlan()}},
	{struct, [Id, Name, Href, Description, Version, LastUpdate, Status, Chars, Dependency]}.

%% @hidden
characteristic_product_wlan() ->
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
	Name3 = {"name", "balanceTopUpDuration"},
	Description3 = {"description", "Validity period of balance top-ups."},
	Config3 = {"configurable", false},
	Type3 = {"valueType", "Number"},
	Type31 = {struct, [{"unitOfMeasure", "seconds"}, {"valueType", "integer"}]},
	Type32 = {struct, [{"unitOfMeasure", "minutes"}, {"valueType", "integer"}]},
	Type33 = {struct, [{"unitOfMeasure", "days"}, {"valueType", "integer"}]},
	Type34 = {struct, [{"unitOfMeasure", "months"}, {"valueType", "integer"}]},
	Type35 = {struct, [{"unitOfMeasure", "years"}, {"valueType", "integer"}]},
	Value3 = {"productSpecCharacteristicValue",
			{array, [Type31, Type32, Type33, Type34, Type35]}},
	Char3 = {struct, [Name3, Description3, Config3, Type3, Value3]},
	Name4 = {"name", "radiusReserveTime"},
	Description4 = {"description",
		"Number of seconds to reserve on RADIUS Accouning-Start "
		"and add to reported duration on Accounting-Interim reservation."},
	Config4 = {"configurable", false},
	Type4 = {"valueType", "Number"},
	Type41 = {struct, [{"unitOfMeasure", "seconds"}, {"valueType", "integer"}]},
	Type42 = {struct, [{"unitOfMeasure", "minutes"}, {"valueType", "integer"}]},
	Value4 = {"productSpecCharacteristicValue", {array, [Type41, Type42]}},
	Char4 = {struct, [Name4, Description4, Config4, Type4, Value4]},
	Name5 = {"name", "radiusReserveOctets"},
	Description5 = {"description",
		"Number of octets to reserve on RADIUS Accouning-Start "
		"and add to reported octetes used on Accounting-Interim reservation."},
	Config5 = {"configurable", false},
	Type5 = {"valueType", "Number"},
	Type51 = {struct, [{"unitOfMeasure", "bytes"}, {"valueType", "integer"}]},
	Type52 = {struct, [{"unitOfMeasure", "kilobytes"}, {"valueType", "integer"}]},
	Type53 = {struct, [{"unitOfMeasure", "megabytes"}, {"valueType", "integer"}]},
	Type54 = {struct, [{"unitOfMeasure", "gigabytes"}, {"valueType", "integer"}]},
	Value5 = {"productSpecCharacteristicValue",
			{array, [Type51, Type52, Type53, Type54]}},
	Char5 = {struct, [Name5, Description5, Config5, Type5, Value5]},
	[Char1, Char2, Char3, Char4, Char5].

%% @hidden
characteristic_product_rate_plane() ->
	Name1 = {"name", "destPrefixPriceTable"},
	Description1 = {"description", "Table of Prefix, Description, Price Lable"},
	ValueType1 = {"valueType", "string"},
	Char1 = {struct, [Name1, Description1, ValueType1]},
	Name2 = {"name", "destPrefixTariffTable"},
	Description2 = {"description", "Table of Prefix, Description, Tariff rate"},
	ValueType2 = {"valueType", "string"},
	Char2 = {struct, [Name2, Description2, ValueType2]},
	Name3 = {"name", "timeOfDayRange"},
	Description3 = {"description", "Time range of the day"},
	ValueType3 = {"valueType", "string"},
	Char3 = {struct, [Name3, Description3, ValueType3]},
	Name4 = {"name", "ratePrice"},
	Description4 = {"description", ""},
	ValueType4 = {"valueType", "string"},
	Char4 = {struct, [Name4, Description4, ValueType4]},
	[Char1, Char2, Char3, Char4].

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
		Type :: string() | usage | recurring | one_time.
%% @doc CODEC for Price Type.
%% @private
price_type("usage") -> usage;
price_type("recurring") -> recurring;
price_type("one_time") -> one_time;
price_type(usage) -> "usage";
price_type(recurring) -> "recurring";
price_type(one_time) -> "one_time".

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
offer([is_bundle | T], #product{is_bundle = IsBundle} = P, Acc)
		when is_boolean(IsBundle) ->
	offer(T, P, [{"isBundle", IsBundle} | Acc]);
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
offer([], #product{name = Name}, Acc) ->
	H = [{"id", Name}, {"href", ?offeringPath ++ Name}],
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
offer([{"isBundle", Bundle} | T], Acc) when is_boolean(Bundle) ->
	offer(T, Acc#product{is_bundle = Bundle});
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
offer([{"isCustomerVisible", Visible} | T], Acc) when is_boolean(Visible) ->
	offer(T, Acc);
offer([{"productOfferingPrice", {array, Prices1}} | T], Acc) when is_list(Prices1) ->
	Prices2 = [price(Price) || Price <- Prices1],
	offer(T, Acc#product{price = Prices2});
offer([{"prodSpecCharValueUse", {array, _} = CharValueUses} | T], Acc) ->
	offer(T, Acc#product{char_value_use = char_value_uses(CharValueUses)});
offer([{"lastUpdate", LastUpdate} | T], Acc) when is_list(LastUpdate) ->
	offer(T, Acc);
offer([], Acc) ->
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
price([type | T], #price{type = one_time, units = cents} = P, Acc) ->
	price(T, P, [{"priceType", price_type(one_time)} | Acc]);
price([type | T], #price{type = recurring, period = Period,
		units = cents} = P, Acc) when Period /= undefined ->
	Recurring = [{"priceType", price_type(recurring)},
			{"recurringChargePeriod", price_period(Period)}],
	price(T, P, Recurring ++ Acc);
price([type | T], #price{type = usage, units = octets,
		size = Size} = P, Acc) when is_integer(Size) ->
	UsageType = [{"priceType", price_type(usage)},
			{"unitOfMeasure", integer_to_list(Size) ++ "b"}],
	price(T, P, UsageType ++ Acc);
price([type | T], #price{type = usage, units = seconds,
		size = Size} = P, Acc) when is_integer(Size) ->
%	UsageType = [{"priceType", price_type(usage)},
%			{"unitOfMeasure", integer_to_list(Size) ++ "s"}],
%	price(T, P, UsageType ++ Acc);
	price(T, P, Acc);
price([period | T], P, Acc) ->
	price(T, P, Acc);
price([units | T], P, Acc) ->
	price(T, P, Acc);
price([size | T], P, Acc) ->
	price(T, P, Acc);
price([amount | T], #price{amount = Amount, currency = Currency} = P, Acc)
		when is_integer(Amount), is_list(Currency) ->
	Price = {struct, [{"taxIncludedAmount", Amount},
			{"currencyCode", Currency}]},
	price(T, P, [{"price", Price} | Acc]);
price([amount | T], #price{amount = Amount} = P, Acc)
		when is_integer(Amount) ->
	Price = {struct, [{"taxIncludedAmount", Amount}]},
	price(T, P, [{"price", Price} | Acc]);
price([currency | T], P, Acc) ->
	price(T, P, Acc);
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
	case price_type(Type) of
		Type1 when Type1 == one_time; Type1 == recurring ->
			price(T, Acc#price{type = Type1, units = cents});
		Type1 ->
			price(T, Acc#price{type = Type1})
	end;
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
			Acc#price{amount = Amount};
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
alteration([type | T], #alteration{type = one_time, units = cents} = A, Acc) ->
	alteration(T, A, [{"priceType", price_type(one_time)} | Acc]);
alteration([type | T], #alteration{type = recurring, period = Period,
		units = cents} = A, Acc) when Period /= undefined ->
	Recurring = [{"priceType", price_type(recurring)},
			{"recurringChargePeriod", price_period(Period)}],
	alteration(T, A, Recurring ++ Acc);
alteration([type | T], #alteration{type = usage, units = octets,
		size = Size} = A, Acc) when is_integer(Size) ->
	UsageType = [{"priceType", price_type(usage)},
			{"unitOfMeasure", integer_to_list(Size) ++ "b"}],
	alteration(T, A, UsageType ++ Acc);
alteration([type | T], #alteration{type = usage, units = seconds,
		size = Size} = A, Acc) when is_integer(Size) ->
	UsageType = [{"priceType", price_type(usage)},
			{"unitOfMeasure", integer_to_list(Size) ++ "s"}],
	alteration(T, A, UsageType ++ Acc);
alteration([period | T], A, Acc) ->
	alteration(T, A, Acc);
alteration([units | T], A, Acc) ->
	alteration(T, A, Acc);
alteration([size | T], A, Acc) ->
	alteration(T, A, Acc);
alteration([amount | T], #alteration{amount = Amount, currency = Currency} = A, Acc)
		when is_integer(Amount), is_list(Currency) ->
	Price = {struct, [{"taxIncludedAmount", Amount},
			{"currencyCode", Currency}]},
	alteration(T, A, [{"price", Price} | Acc]);
alteration([amount | T], #alteration{amount = Amount} = A, Acc)
		when is_integer(Amount) ->
	Price = {struct, [{"taxIncludedAmount", Amount}]},
	alteration(T, A, [{"price", Price} | Acc]);
alteration([currency | T], A, Acc) ->
	alteration(T, A, Acc);
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
			Acc#alteration{amount = Amount};
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
	char_value(T, V, [{"value", Value} | Acc]);
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
					undefined;
				Value ->
					Value
			end
	end,
	Username1 = F2("subscriberIdentity"),
	Id = {"id", Username1},
	Href = {"href", "product/product/" ++ Username1},
	{struct, lists:sort([Id, Href | Json1])}.


-spec instance(Instance) -> Instance
	when
		Instance :: #product_instance{} | {struct, [tuple()]}.
%% @doc CODEC for Product Inventory.
instance({struct, ObjectMembers}) ->
	instance(ObjectMembers, #product_instance{});
instance(ProductInstance) ->
	{struct, instance(record_info(fields, product_instance), ProductInstance, [])}.
%% @hidden
instance([{"characteristics", Chars} | T], Acc) ->
	NewChars = characteristics(Chars),
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
	Characteristics = {"characteristics", characteristics(Chars)},
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

-spec characteristics(Characteristics) -> Characteristics
	when
		Characteristics :: {array, list()} | [tuple()].
%% @doc CODEC for Product Inventory characteristics.
characteristics({array, Characteristics}) ->
	characteristics(Characteristics, []);
characteristics(Characteristics) ->
	{array, characteristics(Characteristics, [])}.
%% @hidden
characteristics([{struct, [{"subscriberIdentity", Identity}]} | T], Acc) ->
	characteristics(T, [{"subscriberIdentity", Identity} | Acc]);
characteristics([{struct, [{"subscriberPassword", Password}]} | T], Acc) ->
	characteristics(T, [{"subscriberPassword", Password} | Acc]);
characteristics([{struct, [{"balanceTopUpDuration", BalanceTopUpDuration}]} | T], Acc) ->
	characteristics(T, [{"balanceTopUpDuration", topup_duration(BalanceTopUpDuration)} | Acc]);
characteristics([{struct, [{"radiusReserveTime", RadiusReserveTime}]} | T], Acc) ->
	characteristics(T, [{"radiusReserveTime", radius_reserve_time(RadiusReserveTime)} | Acc]);
characteristics([{struct, [{"radiusReserveOctets", RadiusReserveOctets}]} | T], Acc) ->
	characteristics(T, [{"radiusReserveOctets", radius_reserve_octets(RadiusReserveOctets)} | Acc]);
characteristics([{"subscriberIdentity", Identity} | T], Acc) ->
	characteristics(T, [{struct, [{"subscriberIdentity", Identity}]} | Acc]);
characteristics([{"subscriberPassword", Password} | T], Acc) ->
	characteristics(T, [{struct, [{"subscriberPassword", Password}]} | Acc]);
characteristics([{"radiusReserveTime", RadiusReserveTime} | T], Acc) ->
	characteristics(T, [{struct, [{"radiusReserveTime", radius_reserve_time(RadiusReserveTime)}]} | Acc]);
characteristics([{"radiusReserveOctets", RadiusReserveOctets} | T], Acc) ->
	characteristics(T, [{struct, [{"radiusReserveOctets", radius_reserve_octets(RadiusReserveOctets)}]} | Acc]);
characteristics([{"balanceTopUpDuration", Chars} | T], Acc) ->
	characteristics(T, [{struct, [{"balanceTopUpDuration", topup_duration(Chars)}]} | Acc]);
characteristics([], Acc) ->
	lists:reverse(Acc).

-spec topup_duration(BalanceTopUpDuration) -> BalanceTopUpDuration
	when
		BalanceTopUpDuration :: {struct, list()} | [tuple()].
%% @doc CODEC for top up duration characteristic
%% @private
topup_duration({struct, [{"unitOfMeasure", Duration}, {"value", Amount}]}) ->
	[{unitOfMeasure, duration(Duration)}, {value, Amount}];
topup_duration({struct, [{"value", Amount}, {"unitOfMeasure", Duration}]}) ->
	[{unitOfMeasure, duration(Duration)}, {value, Amount}];
topup_duration([{unitOfMeasure, Duration}, {value, Amount}]) ->
	{struct, [{"unitOfMeasure", duration(Duration)}, {"value", Amount}]};
topup_duration([{value, Amount}, {unitOfMeasure, Duration}]) ->
	{struct, [{"unitOfMeasure", duration(Duration)}, {"value", Amount}]}.

%% @hidden
product({struct, Offer}) ->
	{_, ProdId} = lists:keyfind("name", 1, Offer),
	ProdId;
product(ProdID) ->
	ID = {"id", ProdID},
	Href = {"href", ?offeringPath ++ ProdID},
	Name = {"name", ProdID},
	{struct, [ID, Href, Name]}.

-spec radius_reserve_octets(RadiusReserve) -> RadiusReserve
	when
		RadiusReserve :: {struct, list()} | pos_integer().
%% @doc CODEC for radius reservation octets characteristic.
%% @private
radius_reserve_octets({struct, L}) ->
	radius_reserve_octets(L, undefined, 0);
radius_reserve_octets(N) when N =/= 0, N rem 1000000000 =:= 0 ->
	{struct, [{"unitOfMeasure", "gigabytes"}, {"value", N}]};
radius_reserve_octets(N) when N =/= 0, N rem 1000000 =:= 0 ->
	{struct, [{"unitOfMeasure", "megabytes"}, {"value", N}]};
radius_reserve_octets(N) when N =/= 0, N rem 1000 =:= 0 ->
	{struct, [{"unitOfMeasure", "kilobytes"}, {"value", N}]};
radius_reserve_octets(N) when N =/= 0 ->
	{struct, [{"unitOfMeasure", "bytes"}, {"value", N}]}.
%% @hidden
radius_reserve_octets([{"unitOfMeasure", Units} | T], undefined, Value) ->
	radius_reserve_octets(T, Units, Value);
radius_reserve_octets([{"value", Value} | T], Units, 0) when is_integer(Value) ->
	radius_reserve_octets(T, Units, Value);
radius_reserve_octets([], "bytes", Value) ->
	Value;
radius_reserve_octets([], "kilobytes", Value) ->
	Value * 1000;
radius_reserve_octets([], "megabytes", Value) ->
	Value * 1000000;
radius_reserve_octets([], "gigabytes", Value) ->
	Value * 1000000000.

-spec radius_reserve_time(RadiusReserve) -> RadiusReserve
	when
		RadiusReserve :: {struct, list()} | pos_integer().
%% @doc CODEC for radius reservation time characteristic.
%% @private
radius_reserve_time({struct, L}) ->
	radius_reserve_time(L, undefined, 0);
radius_reserve_time(N) when N =/= 0, N rem 60 =:= 0 ->
	{struct, [{"unitOfMeasure", "minutes"}, {"value", N}]};
radius_reserve_time(N) when N =/= 0 ->
	{struct, [{"unitOfMeasure", "seconds"}, {"value", N}]}.
%% @hidden
radius_reserve_time([{"unitOfMeasure", Units} | T], undefined, Value) ->
	radius_reserve_time(T, Units, Value);
radius_reserve_time([{"value", Value} | T], Units, 0) when is_integer(Value) ->
	radius_reserve_time(T, Units, Value);
radius_reserve_time([], "seconds", Value) ->
	Value;
radius_reserve_time([], "minutes", Value) ->
	Value * 60.

%% @hidden
duration("seconds") -> "seconds";
duration("minutes") -> "minutes";
duration("days") -> "days";
duration("months") -> "months";
duration("years") -> "years";
duration(seconds) -> "seconds";
duration(minutes) -> "minutes";
duration(days) -> "days";
duration(months) -> "months";
duration(years) -> "years".

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
	query_page1(T, Filters, [ocs_rest:filter(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

