%% ocs_rest_res_resource.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2018 SigScale Global Inc.
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
-module(ocs_rest_res_resource).
-copyright('Copyright (c) 2016 - 2020 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_resource_spec/1, get_resource_specs/1]).
-export([get_resource_category/1, get_resource_categories/1]).
-export([get_resource_candidate/1, get_resource_candidates/1]).
-export([get_resource_catalog/1, get_resource_catalogs/1]).
-export([get_resource_inventory/2, add_resource_inventory/2, patch_resource_inventory/4,
			delete_resource_inventory/2]).
-export([get_pla_specs/1]).
-export([pla/1, add_pla/1, get_pla/1, patch_pla/3, get_plas/2, delete_pla/1]).
-export([gtt/2]).

-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(specPath, "/resourceCatalogManagement/v2/resourceSpecification/").
-define(candidatePath, "/resourceCatalogManagement/v2/resourceCandidate/").
-define(catalogPath, "/resourceCatalogManagement/v2/resourceCatalog/").
-define(categoryPath, "/resourceCatalogManagement/v2/resourceCategory/").
-define(inventoryPath, "/resourceInventoryManagement/v1/logicalResource/").
-define(plaPath, "/resourceInventoryManagement/v2/pla/").
-define(plaSpecPath, "/resourceCatalogManagement/v2/plaSpecification/").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_resource_spec(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Get Resource Specification by ID.
get_resource_spec("1") ->
	ResourceSpec = tariff_table_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec(_) ->
	{error, 404}.

-spec get_resource_specs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceSpecification'.
%% 	Retrieve all Resource specifications.
get_resource_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_spec()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_specs(_Query) ->
	{error, 400}.

-spec get_resource_category(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCategory/{id}'.
%% 	Retrieve a Resource category by Id.
get_resource_category("1") ->
	ResourceCatagory = tariff_table_category(),
	Body = mochijson:encode(ResourceCatagory),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_category(_) ->
	{error, 404}.

-spec get_resource_categories(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCategory'.
%% 	Retrieve all Resource categories.
get_resource_categories([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_category()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_categories(_Query) ->
	{error, 400}.

-spec get_resource_candidate(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCandidate/{id}'.
%%		Get Resource Candidate by ID.
get_resource_candidate("1") ->
	ResourceCandidate = tariff_table_candidate(),
	Body = mochijson:encode(ResourceCandidate),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_candidate(_) ->
	{error, 404}.

-spec get_resource_candidates(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /catalogManagement/v2/resourceCandidate'.
%% 	Retrieve all Resource candidate.
get_resource_candidates([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_candidate()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_candidates(_Query) ->
	{error, 400}.

-spec get_resource_catalog(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCatalog/{id}'.
%%		Get Resource Catalog by ID.
get_resource_catalog("1") ->
	ResourceCatalog = tariff_table_catalog(),
	Body = mochijson:encode(ResourceCatalog),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_catalog(_) ->
	{error, 404}.

-spec get_resource_catalogs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCatalog'.
%% 	Retrieve all Resource catalogs.
get_resource_catalogs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_catalog()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_catalogs(_Query) ->
	{error, 400}.

-spec get_resource_inventory(Id, Query) -> Result when
	Id :: string(),
	Query :: [{Key :: string(), Value :: string()}],
	Result   :: {ok, Headers, Body} | {error, Status},
	Headers  :: [tuple()],
	Body     :: iolist(),
	Status   :: 400 | 404 | 500.
%% @doc Respond to `GET /resourceInventoryManagement/v1/logicalResource/{id}'.
%%    Retrieve all logical resource from inventory management.
get_resource_inventory(Id, [] = _Query) ->
	try
		Name = list_to_existing_atom(Id),
		case ocs:query_table(start, Name, undefined, undefined, undefined, undefined) of
			{eof, Page} ->
				L = get_resource_inventory1(Id, Page, []),
				Body = mochijson:encode({array, L}),
				Headers = [{content_type, "application/json"}],
				{ok, Headers, Body};
			{error, not_found} ->
				{error, 404};
			{error, _Reason} ->
				{error, 500}
		end
	catch
		error:badarg ->
			{error, 404};
		_:_Reason1 ->
			{error, 500}
	end.
get_resource_inventory1(Id, [#gtt{num = Prefix, value = Value} | T], Acc) ->
		Gtt = gtt(Id, {Prefix, element(1, Value), element(2, Value)}),
		get_resource_inventory1(Id, T, [Gtt | Acc]);
get_resource_inventory1(_, [], Acc) ->
		lists:reverse(Acc).

-spec add_resource_inventory(Table, ReqData) -> Result when
	Table :: string(),
	ReqData :: [tuple()],
	Result   :: {ok, Headers, Body} | {error, Status},
	Headers  :: [tuple()],
	Body     :: iolist(),
	Status   :: 400 | 500 .
%% @doc Respond to
%% 	`POST /resourceInventoryManagement/v1/logicalResource/{table}'.
%%    Add a new row in logical resource inventory management.
add_resource_inventory(Table, ReqData) ->
	try
		{P, D, R} = gtt(Table, mochijson:decode(ReqData)),
		Name = list_to_existing_atom(Table),
		case catch ocs_gtt:insert(Name, P, {D, R}) of
			{ok, #gtt{num = Prefix1, value = Value}} ->
				{Prefix1, element(1, Value),
						element(2, Value), element(size(Value), Value)};
			{'EXIT', {no_exists, _}} ->
				throw(not_found);
			{'EXIT', Reason} ->
				throw(Reason)
		end
	of
		{Prefix2, Desc, Rate, LM} ->
			Body = mochijson:encode(gtt(Table, {Prefix2, Desc, Rate})),
			Etag = ocs_rest:etag(LM),
			Headers = [{content_type, "application/json"}, {etag, Etag}],
			{ok, Headers, Body}
	catch
		throw:not_found ->
			{error, 404};
		throw:_Reason1 ->
			{error, 500};
		error:badarg ->
			{error, 400};
		_:_ ->
			{error, 400}
	end.

-spec add_pla(ReqData) -> Result when
	ReqData :: [tuple()],
	Result   :: {ok, Headers, Body} | {error, Status},
	Headers  :: [tuple()],
	Body     :: iolist(),
	Status   :: 400 | 500 .
%% @doc Respond to `POST /resourceInventoryManagement/v2/pla'.
%%    Add a new Pricing Logic Algorithm (PLA).
add_pla(ReqData) ->
	try
		case ocs:add_pla(ocs_rest_res_resource:pla(mochijson:decode(ReqData))) of
			{ok, PricingLogic} ->
				PricingLogic;
			{error, Reason} ->
				throw(Reason)
		end
	of
		PriceAlgo ->
			Body = mochijson:encode(ocs_rest_res_resource:pla(PriceAlgo)),
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


-spec patch_pla(Id, Etag, ReqData) -> Result
	when
		Id	:: string(),
		Etag		:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 412 | 500 .
%% @doc Respond to `PATCH /resourceInventoryManagement/v2/pla/{id}'.
%% 	Update a Pricing Logic Algorithm (PLA) using JSON patch method.
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
							case catch ocs_rest:patch(Operations, ocs_rest_res_resource:pla(Pla1)) of
								{struct, _} = Pla2  ->
									Pla3 = ocs_rest_res_resource:pla(Pla2),
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

-spec delete_pla(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /resourceInventoryManagement/v2/pla/{id}'
%%    request to remove a Pricing Logic Algorithm (PLA).
delete_pla(Id) when is_list(Id) ->
	case catch list_to_existing_atom(Id) of
		{'EXIT', _Reason} ->
			{error, 404};
		TableName when is_atom(TableName) ->
			ok = ocs:delete_pla(Id),
			{ok, [], []}
	end.

-spec get_pla(ID) -> Result when
	ID	:: string(),
	Result :: {ok, Headers, Body} | {error, Status},
	Headers :: [tuple()],
	Body :: iolist(),
	Status :: 400 | 404 | 500.
%% @doc Respond to `GET /resourceInventoryManagement/v2/pla/{id}'.
%%    Retrieve a Pricing Logic Algorothm (PLA).
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
			Body = mochijson:encode(ocs_rest_res_resource:pla(LogicAlgo)),
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
%% @doc Respond to `GET /resourceInventoryManagement/v2/pla'.
%%    Retrieve all Pricing Logic Algorithms (PLA).
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


-spec get_pla_specs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
%% @doc Respond to `GET /resourceCatalogManagement/v2/plaSpecification'.
%% 	Retrieve all Pricing Logic Algorithm (PLA) specifications.
get_pla_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [spec_pla_once(), spec_pla_recurring(),
			spec_pla_usage(), spec_pla_tariff()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_pla_specs(_Query) ->
	{error, 400}.

-spec patch_resource_inventory(Table, Id, Etag, ReqData) -> Result
	when
		Table :: string(),
		Id	:: string(),
		Etag	:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 500 .
%% @doc Respond to `PATCH /resourceInventoryManagement/v1/logicalResource/{table}/{id}'.
%% 	Update a table row using JSON patch method.
patch_resource_inventory(Table, Id, _Etag, ReqData) ->
	try
		Table1 = list_to_existing_atom(Table),
		{Table1, mochijson:decode(ReqData)}
	of
		{Table2, Operations} ->
			case catch ocs_gtt:lookup_last(Table2, Id) of
				{'EXIT', _} ->
					throw(not_found);
				V1 ->
					case catch ocs_rest:patch(Operations,
							gtt(Table, {Id, element(1, V1), element(2, V1)})) of
						{struct, _} = Res  ->
							{Id, Description, Rate} = gtt(Id, Res),
							{ok, #gtt{value = V}} = ocs_gtt:insert(Table2, Id, {Description, Rate}),
							Location = ?inventoryPath ++ Table ++ Id,
							Headers = [{location, Location},
									{etag, ocs_rest:etag(element(size(V), V))}],
							Body = mochijson:encode(Res),
							{ok, Headers, Body};
						_ ->
							throw(bad_request)
					end
			end
	catch
		throw:not_found ->
			{error, 404};
		throw:bad_request ->
			{error, 400};
		_:_ ->
			{error, 500}
	end.

-spec delete_resource_inventory(Table, Id) -> Result
   when
		Table :: string(),
      Id :: string(),
      Result :: {ok, Headers :: [tuple()], Body :: iolist()}
            | {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /resourceInventoryManagement/v1/logicalResource/{table}/{id}''
%%    request to remove a table row.
delete_resource_inventory(Table, Id) ->
	try
		Name = list_to_existing_atom(Table),
		ocs_gtt:delete(Name, Id)
	of
		ok ->
			{ok, [], []}
	catch
		error:badarg ->
			{error, 404};
		_:_ ->
			{error, 400}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
tariff_table_spec() ->
	Id = {"id", "1"},
	Href = {"href", ?specPath "1"},
	Name = {"name", "TariffTableSpec"},
	Description = {"description", "Voice call rating tariff table"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Category = {"category", "TariffTable"},
	Chars = {array, [{struct, [{"name", "prefix"},
			{"description", "Call address prefix"},
			{"valueType", "MatrixCharacteristicSpec"},
			{"resourceSpecCharacteristicValue", {array, [{struct,
			[{"seqNum", 1}, {"valueType", "String"}]}]}}]},
			{struct, [{"name", "description"},
			{"description", "Prefix description"},
			{"valueType", "MatrixCharacteristicSpec"},
			{"resourceSpecCharacteristicValue", {array, [{struct,
			[{"seqNum", 2}, {"valueType", "String"}]}]}}]},
			{struct, [{"name", "rate"},
			{"description", "Rated price for address"},
			{"valueType", "MatrixCharacteristicSpec"},
			{"resourceSpecCharacteristicValue", {array, [{struct,
			[{"seqNum", 3}, {"valueType", "Number"}]}]}}]}]},
	Characteristic = {"resourceSpecCharacteristic" , Chars},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, Category, Characteristic]}.

%% @hidden
tariff_table_category() ->
	Id = {"id", "1"},
	Href = {"href", ?categoryPath "1"},
	Name = {"name", "TariffTableCategory"},
	Description = {"description", "Voice call rating tariff tables"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	IsRoot = {"isRoot", true},
	Candidate = {"resourceCandidate", {array, [{struct, [{"id", "1"}, {"href", ?candidatePath "1"},
			{"version", "1.0"}, {"name", "TariffTableCandidate"}]}]}},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, IsRoot, Candidate]}.

%% @hidden
tariff_table_candidate() ->
	Id = {"id", "1"},
	Href = {"href", ?candidatePath "1"},
	Name = {"name", "TariffTableCandidate"},
	Description = {"description", "Voice call rating tariff table"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	Category = {"category", {array, [{struct, [{"id", "1"}, {"href", ?categoryPath "1"},
			{"version", "1.0"}, {"name", "TariffTableCandidate"}]}]}},
	ResourceSpec = {"resourceSpecification", {struct, [{"id", "1"}, {"href", ?specPath "1"},
			{"name", "TariffTableSpec"}]}},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, Category, ResourceSpec]}.

%% @hidden
tariff_table_catalog() ->
	Id = {"id", "1"},
	Href = {"href", ?catalogPath "1"},
	Name = {"name", "TariffTableCatalog"},
	Description = {"description", "Voice call rating tariff table"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2018-01-10"},
	Status = {"lifecycleStatus", "Active"},
	Category = {"category", {array, [{struct, [{"id", "1"}, {"href", ?categoryPath "1"},
		{"version", "1.0"}, {"name", "TariffTableCategory"}]}]}},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, Category]}.

-spec gtt(Name, Gtt) -> Gtt
	when
		Name :: string(),
		Gtt :: {Prefix, Description, Rate} | {struct, [tuple()]},
		Prefix :: string(),
		Description :: string(),
		Rate :: non_neg_integer().
%% @doc CODEC for gtt.
%% @private
gtt(Name, {Prefix, Description, Rate} = _Gtt) ->
	SpecId = {"id", "1"},
   SpecHref = {"href", ?specPath "1"},
   SpecName = {"name", "TariffTableSpec"},
	{struct, [{"id", Prefix},
			{"href", ?inventoryPath ++ Name ++ "/" ++ Prefix},
			{"resourceSpecification", {struct, [SpecId, SpecHref, SpecName]}},
			{"resourceCharacteristic", {array, [{struct, [{"name", "prefix"},
			{"value", {struct, [{"seqNum", 1}, {"value", Prefix}]}}]},
			{struct, [{"name", "description"},
			{"value", {struct, [{"seqNum", 2}, {"value", Description}]}}]},
			{struct, [{"name", "rate"},
			{"value", {struct, [{"seqNum", 3}, {"value", ocs_rest:millionths_out(Rate)}]}}]}]}}]};
gtt(_, {struct, ObjectMembers}) when is_list(ObjectMembers) ->
	gtt1(ObjectMembers, {undefined, [], undefined}).
%% @hidden
gtt1([{"resourceCharacteristic", {array, L}} | T], Acc) ->
   gtt1(T, gtt2(L, Acc));
gtt1([_ | T], Acc) ->
	gtt1(T, Acc);
gtt1([], {Prefix, Desc, Rate} = _Acc)
		when is_list(Prefix)->
   {Prefix, Desc, ocs_rest:millionths_in(Rate)}.
%% @hidden
gtt2([{struct, L} | T], {Prefix, Desc, Rate} = _Acc) ->
	case lists:keytake("name", 1, L) of
		{value, {"name", "prefix"}, L1} ->
			{_, {struct, L2}} = lists:keyfind("value", 1, L1),
			{_, Prefix1} = lists:keyfind("value", 1, L2),
			gtt2(T, {Prefix1, Desc, Rate});
		{value, {"name", "description"}, L1} ->
			{_, {struct, L2}} = lists:keyfind("value", 1, L1),
			{_, Desc1} = lists:keyfind("value", 1, L2),
			gtt2(T, {Prefix, Desc1, Rate});
		{value, {"name", "rate"}, L1} ->
			{_, {struct, L2}} = lists:keyfind("value", 1, L1),
			{_, Rate1} = lists:keyfind("value", 1, L2),
			gtt2(T, {Prefix, Desc, Rate1})
	end;
gtt2([], Acc) ->
	Acc.

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
	StatusPla = pla_status(Status),
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
	pla(T, Acc#pla{status = pla_status(Status)});
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

-spec pla_status(Status) -> Status
	when
		Status :: created | active | cancelled | terminated | string().
%% @doc CODEC for life cycle status of PLA.
%% @private
pla_status(created) -> "Created";
pla_status(cancelled) -> "Cancelled";
pla_status(active) -> "Active";
pla_status(terminated) -> "Terminated";
pla_status("Created") -> created;
pla_status("Active") -> active;
pla_status("Cancelled") -> cancelled;
pla_status("Terminated") -> terminated.

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

