%% ocs_rest_res_resource.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_resource).
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_resource_spec/1, get_resource_specs/1]).
-export([get_resource_category/1, get_resource_categories/1]).
-export([get_resource_candidate/1, get_resource_candidates/1]).
-export([get_resource_catalog/1, get_resource_catalogs/1]).
-export([get_resource/1, get_resource/2, add_resource/1, patch_resource/3,
		delete_resource/1, head_resource/0]).
-export([get_pla_specs/1]).
-export([resource/1]).

-include("ocs.hrl").

-define(specPath, "/resourceCatalogManagement/v2/resourceSpecification/").
-define(candidatePath, "/resourceCatalogManagement/v2/resourceCandidate/").
-define(catalogPath, "/resourceCatalogManagement/v2/resourceCatalog/").
-define(categoryPath, "/resourceCatalogManagement/v2/resourceCategory/").
-define(inventoryPath, "/resourceInventoryManagement/v1/resource/").
-define(plaPath, "/resourceInventoryManagement/v2/pla/").
-define(plaSpecPath, "/resourceCatalogManagement/v2/plaSpecification/").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: [ContentType],
		ContentType :: string().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: [ContentType],
		ContentType :: string().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec get_resource_spec(ID) -> Result
	when
		ID :: string(),
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 404.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceSpecification/{id}'.
%%		Retrieve a resource specification.
get_resource_spec("1") ->
	ResourceSpec = tariff_table_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("2") ->
	ResourceSpec = tariff_row_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("3") ->
	ResourceSpec = policy_table_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("4") ->
	ResourceSpec = policy_row_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("5") ->
	ResourceSpec = tariff_periods_table_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("6") ->
	ResourceSpec = tariff_periods_row_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("7") ->
	ResourceSpec = roaming_table_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec("8") ->
	ResourceSpec = roaming_row_spec(),
	Body = mochijson:encode(ResourceSpec),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_spec(_) ->
	{error, 404}.

-spec get_resource_specs(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceSpecification'.
%% 	Retrieve all resource specifications.
get_resource_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_spec(), tariff_row_spec(),
			policy_table_spec(), policy_row_spec()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_specs(_Query) ->
	{error, 400}.

-spec get_resource_category(ID) -> Result
	when
		ID :: string(),
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 404.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCategory/{id}'.
%% 	Retrieve a Resource category by Id.
get_resource_category("1") ->
	ResourceCatagory = tariff_table_category(),
	Body = mochijson:encode(ResourceCatagory),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_category("2") ->
	ResourceCatagory = policy_table_category(),
	Body = mochijson:encode(ResourceCatagory),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_category(_) ->
	{error, 404}.

-spec get_resource_categories(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCategory'.
%% 	Retrieve all Resource categories.
get_resource_categories([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_category(), policy_table_category()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_categories(_Query) ->
	{error, 400}.

-spec get_resource_candidate(ID) -> Result
	when
		ID :: string(),
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 404.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCandidate/{id}'.
%%		Get Resource Candidate by ID.
get_resource_candidate("1") ->
	ResourceCandidate = tariff_table_candidate(),
	Body = mochijson:encode(ResourceCandidate),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_candidate(_) ->
	{error, 404}.

-spec get_resource_candidates(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400.
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
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 404.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCatalog/{id}'.
%%		Get Resource Catalog by ID.
get_resource_catalog("1") ->
	ResourceCatalog = tariff_table_catalog(),
	Body = mochijson:encode(ResourceCatalog),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body};
get_resource_catalog(_) ->
	{error, 404}.

-spec get_resource_catalogs(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400.
%% @doc Respond to `GET /resourceCatalogManagement/v2/resourceCatalog'.
%% 	Retrieve all Resource catalogs.
get_resource_catalogs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [tariff_table_catalog()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_resource_catalogs(_Query) ->
	{error, 400}.

-spec get_resource(Id) -> Result
	when
		Id :: string(),
		Result   :: {ok, Headers, Body} | {error, Status},
		Headers  :: [tuple()],
		Body     :: iolist(),
		Status   :: 404 | 500.
%% @doc Respond to `GET /resourceInventoryManagement/v1/resource/{id}'.
%%    Retrieve resource from inventory management.
get_resource(Id) ->
	try
		case ocs:get_resource(Id) of
			{ok, #resource{last_modified = LM} = Resource} ->
				Headers = [{content_type, "application/json"},
						{etag, ocs_rest:etag(LM)}],
				Body = mochijson:encode(resource(Resource)),
				{ok, Headers, Body};
			{error, not_found} ->
				{error, 404};
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_:_Reason1 ->
			{error, 500}
	end.

-spec head_resource() -> Result
	when
		Result   :: {ok, Headers, Body} | {error, Status},
		Headers  :: [tuple()],
		Body     :: iolist(),
		Status   :: 500.
%% @doc Body producing function for
%% 	`HEAD /resourceInventoryManagement/v1/resource'
%% 	requests.
head_resource() ->
	try
		Size = mnesia:table_info(resource, size),
		LastItem = integer_to_list(Size),
		ContentRange = "items 1-" ++ LastItem ++ "/" ++ LastItem,
		Headers = [{content_range, ContentRange}],
		{ok, Headers, []}
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec get_resource(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result   :: {ok, ReplyHeaders, Body} | {error, Status},
		ReplyHeaders  :: [tuple()],
		Body     :: iolist(),
		Status   :: 400.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceInventoryManagement/v1/resource'
%% 	requests.
get_resource(Query, Headers) ->
	try
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchId = match("id", Complex, Query),
						MatchCategory = match("category", Complex, Query),
						MatchSpecId = match("resourceSpecification.id", Complex, Query),
						MatchRelName = match("resourceRelationship", Complex, []),
						{Query1, [MatchId, MatchCategory, MatchSpecId, MatchRelName]}
				end;
			false ->
					MatchId = match("id", [], Query),
					MatchCategory = match("category", [], Query),
					MatchSpecId = match("resourceSpecification.id", [], Query),
					MatchRelName = match("resourceRelationship.resource.name", [], Query),
					{Query, [MatchId, MatchCategory, MatchSpecId, MatchRelName]}
		end
	of
		{Query2, QueryArgs} ->
			Codec = fun resource/1,
			query_filter({ocs, query_resource, QueryArgs}, Codec, Query2, Headers)
	catch
		_ ->
			{error, 400}
	end.

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
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case ocs_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(MFA, Codec, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(MFA, Codec, Query, Filters, undefined, undefined)
	end.

%% @hidden
query_page(Codec, PageServer, Etag, Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			JsonObj = query_page1(lists:map(Codec, Result), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
%% @hidden
query_page1(Json, [], []) ->
	Json;
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

%% @hidden
query_start({M, F, A}, Codec, Query, Filters, RangeStart, RangeEnd) ->
	case supervisor:start_child(ocs_rest_pagination_sup, [[M, F, A]]) of
		{ok, PageServer, Etag} ->
			query_page(Codec, PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

-spec add_resource(RequestBody) -> Result
	when
		RequestBody :: string(),
		Result   :: {ok, Headers, Body} | {error, Status},
		Headers  :: [tuple()],
		Body     :: iolist(),
		Status   :: 400 | 500.
%% @doc Respond to
%% 	`POST /resourceInventoryManagement/v1/resource'.
%%    Add a new resource in inventory.
add_resource(RequestBody) ->
	try
		ocs:add_resource(resource(mochijson:decode(RequestBody)))
	of
		{ok, #resource{href = Href, last_modified = LM} = Resource} ->
			ReplyHeaders = [{content_type, "application/json"},
					{location, Href}, {etag, ocs_rest:etag(LM)}],
			ReplyBody = mochijson:encode(resource(Resource)),
			{ok, ReplyHeaders, ReplyBody};
		{error, table_not_found} ->
			Problem = #{type => "/doc/ocs.html#add_resource-1",
					title => "Table not found",
					detail => "The underying mnesia table must first be created"},
			{error, 400, Problem};
		{error, table_exists} ->
			Problem = #{type => "/doc/ocs.html#add_resource-1",
					title => "Table already exists",
					detail => "The table resource already exists in the inventory"},
			{error, 400, Problem};
		{error, missing_char} ->
			Problem = #{type => "/doc/ocs.html#add_resource-1",
					title => "Missing resource characteristic(s)",
					detail => "A mandatory resource characteristic was missing"},
			{error, 400, Problem};
		{error, _Reason} ->
			Problem = #{type => "/doc/ocs.html#add_resource-1",
					title => "Server error",
					detail => "An error occurred adding the resource"},
			{error, 500, Problem}
	catch
		_:_Reason ->
			{error, 400}
	end.

-spec get_pla_specs(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400.
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

-spec delete_resource(Id) -> Result
   when
      Id :: string(),
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404.
%% @doc Respond to `DELETE /resourceInventoryManagement/v1/resource/{id}''
%%    request to remove a table row.
delete_resource(Id) ->
	case ocs:delete_resource(Id) of
		ok ->
			{ok, [], []};
		{error, not_found} ->
			{error, 404};
		{error, Reason} ->
			{error, 500}
	end.

-spec patch_resource(Id, Etag, ReqData) -> Result
	when
		Id	:: string(),
		Etag	:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 412 | 500.
%% @doc Respond to `PATCH /resourceInventoryManagement/v1/resource/{id}'.
%% 	Update a table row using JSON patch method.
patch_resource(Id, Etag, ReqData) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Id, Etag1, mochijson:decode(ReqData)}
	of
		{Id, Etag2, {array, _} = Operations} ->
			F = fun() ->
					case mnesia:read(resource, Id, write) of
						[#resource{last_modified = LM1} = Resource1]
								when LM1 == Etag2; Etag2 == undefined ->
							case catch ocs_rest:patch(Operations,
									resource(Resource1)) of
								{struct, _} = Resource2 ->
									Resource3 = resource(Resource2),
									Resource4 = Resource3#resource{last_modified = LM1},
									ocs:update_resource(Resource4);
								_ ->
									mnesia:abort(bad_request)
							end;
						[#resource{}] ->
							{error, stale};
						[] ->
							{error, not_found}
					end
			end,
			case mnesia:transaction(F) of
				{atomic, {ok, #resource{href = Href,
						last_modified = LM2} = Resource3}} ->
					ReplyHeaders = [{content_type, "application/json"},
							{location, Href}, {etag, ocs_rest:etag(LM2)}],
					ReplyBody = mochijson:encode(resource(Resource3)),
					{ok, ReplyHeaders, ReplyBody};
				{atomic, {error, not_found}} ->
					Problem = #{type => "/doc/ocs.html#update_resource-1",
							title => "Resource not found",
							detail => "The id was not found in the resource table"},
					{error, 404, Problem};
				{atomic, {error, not_allowed}} ->
					Problem = #{type => "/doc/ocs.html#update_resource-1",
							title => "Resource update not allowed",
							detail => "An attribute or characteristic which may not be modified"},
					{error, 400, Problem};
				{atomic, {error, missing_char}} ->
					Problem = #{type => "/doc/ocs.html#update_resource-1",
							title => "Missing resource characteristic(s)",
							detail => "A mandatory resource characteristic was missing"},
					{error, 400, Problem};
				{atomic, {error, stale}} ->
					Problem = #{type => "/doc/ocs.html#update_resource-1",
							title => "Resource modified after read",
							detail => "ETag value did not match last modified time"},
					{error, 412, Problem};
				{atomic, {error, _Reason}} ->
					Problem = #{type => "/doc/ocs.html#update_resource-1",
							title => "Server error",
							detail => "An error occurred patching the resource"},
					{error, 500, Problem};
				{aborted, bad_request} ->
					{error, 400};
				{aborted, _Reason} ->
					{error, 500}
			end
	catch
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
	Name = {"name", "TariffTable"},
	Description = {"description", "Voice call rating tariff table"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2020-01-13"},
	Category = {"category", "TariffTable"},
	{struct, [Id, Href, Name, Description, Version, Status,
			LastUpdate, Category]}.

%% @hidden
tariff_row_spec() ->
	Id = {"id", "2"},
	Href = {"href", ?specPath "2"},
	Name = {"name", "TariffTableRow"},
	Description = {"description", "Voice call rating tariff row"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2020-01-13"},
	Category = {"category", "TariffRow"},
	Chars = {array,
			[{struct,
					[{"name", "prefix"},
					{"description", "Call address prefix"},
					{"valueType", "String"}]},
			{struct,
					[{"name", "description"},
					{"description", "Prefix description"},
					{"valueType", "String"}]},
			{struct,
					[{"name", "rate"},
					{"description", "Rated price for address"},
					{"valueType", "Number"}]}]},
	Characteristic = {"resourceSpecCharacteristic", Chars},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate,
			Category, Characteristic]}.

%% @hidden
policy_table_spec() ->
	Id = {"id", "3"},
	Href = {"href", ?specPath "3"},
	Name = {"name", "PolicyTable"},
	Description = {"description", "Policy table"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2021-04-13"},
	Category = {"category", "PolicyTable"},
	{struct, [Id, Href, Name, Description, Version, Status,
			LastUpdate, Category]}.

%% @hidden
policy_row_spec() ->
	Id = {"id", "4"},
	Href = {"href", ?specPath "4"},
	Name = {"name", "PolicyTableRow"},
	Description = {"description", "Policy table row"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2021-01-15"},
	Category = {"category", "PolicyRow"},
	Chars = {array,
			[{struct,
					[{"name", "name"},
					{"description", "Policy rule name"},
					{"minCardinality", 1},
					{"valueType", "String"}]},
			{struct,
					[{"name", "qosInformation"},
					{"description", "Quality of Service Information"},
					{"minCardinality", 0},
					{"valueType", "Object"}]},
			{struct,
					[{"name", "chargingKey"},
					{"description", "Charging rule"},
					{"minCardinality", 0},
					{"valueType", "Number"}]},
			{struct,
					[{"name", "flowInformation"},
					{"description", "Service flow template"},
					{"minCardinality", 1},
					{"valueType", "Array"}]},
			{struct,
					[{"name", "precedence"},
					{"description", "Priority order rules are applied in"},
					{"minCardinality", 1}, {"valueType", "Number"}]},
			{struct,
					[{"name", "serviceId"},
					{"description", "Service flow identifier"},
					{"minCardinality", 0},
					{"valueType", "String"}]},
			{struct,
					[{"name", "predefined"},
					{"description", "Indicate PCEF predefined rule"},
					{"minCardinality", 0},
					{"valueType", "Boolean"}]}]},
	Characteristic = {"resourceSpecCharacteristic", Chars},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate,
			Category, Characteristic]}.

%% @hidden
tariff_periods_table_spec() ->
	Id = {"id", "5"},
	Href = {"href", ?specPath "5"},
	Name = {"name", "TariffPeriodsTable"},
	Description = {"description", "Voice call rating, with initial and additional periods, tariff table"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2023-03-29"},
	Category = {"category", "TariffTable"},
	{struct, [Id, Href, Name, Description, Version, Status,
			LastUpdate, Category]}.

%% @hidden
tariff_periods_row_spec() ->
	Id = {"id", "6"},
	Href = {"href", ?specPath "6"},
	Name = {"name", "TariffPeriodsTableRow"},
	Description = {"description", "Voice call rating, with initial and additional periods, tariff row"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2023-03-29"},
	Category = {"category", "TariffRow"},
	Chars = {array,
			[{struct,
					[{"name", "prefix"},
					{"description", "Destination address prefix"},
					{"valueType", "String"}]},
			{struct,
					[{"name", "description"},
					{"description", "Prefix description"},
					{"valueType", "String"}]},
			{struct,
					[{"name", "periodInitial"},
					{"description", "Length of initial period in seconds"},
					{"valueType", "Number"}]},
			{struct,
					[{"name", "rateInitial"},
					{"description", "Rated price for the initial period"},
					{"valueType", "Number"}]},
			{struct,
					[{"name", "periodAdditional"},
					{"description", "Length of additional period in seconds"},
					{"valueType", "Number"}]},
			{struct,
					[{"name", "rateAdditional"},
					{"description", "Rated price for each additional period"},
					{"valueType", "Number"}]}]},
	Characteristic = {"resourceSpecCharacteristic", Chars},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate,
			Category, Characteristic]}.

%% @hidden
roaming_table_spec() ->
	Id = {"id", "7"},
	Href = {"href", ?specPath "7"},
	Name = {"name", "RoamingTable"},
	Description = {"description", "Visited network roaming tariffs table"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2023-04-01"},
	Category = {"category", "TariffTable"},
	{struct, [Id, Href, Name, Description, Version, Status,
			LastUpdate, Category]}.

%% @hidden
roaming_row_spec() ->
	Id = {"id", "8"},
	Href = {"href", ?specPath "8"},
	Name = {"name", "RoamingTableRow"},
	Description = {"description", "Visited network roaming tariff row"},
	Status = {"lifecycleStatus", "Active"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2023-04-01"},
	Category = {"category", "TariffRow"},
	Chars = {array,
			[{struct,
					[{"name", "prefix"},
					{"description", "Visited network MCCMNC prefix"},
					{"valueType", "String"}]},
			{struct,
					[{"name", "description"},
					{"description", "Prefix description"},
					{"valueType", "String"}]},
			{struct,
					[{"name", "unitSize"},
					{"description", "Unit size for rate"},
					{"valueType", "Number"}]}]},
			{struct,
					[{"name", "rate"},
					{"description", "Rated price per unit"},
					{"valueType", "Number"}]}]},
			{struct,
					[{"name", "tariff"},
					{"description", "Tariff table name suffix"},
					{"valueType", "String"}]}]},
	Characteristic = {"resourceSpecCharacteristic", Chars},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate,
			Category, Characteristic]}.

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
policy_table_category() ->
	Id = {"id", "2"},
	Href = {"href", ?categoryPath "2"},
	Name = {"name", "PolicyTableCategory"},
	Description = {"description", "Voice call rating policy tables"},
	Version = {"version", "1.0"},
	LastUpdate = {"lastUpdate", "2021-01-20"},
	Status = {"lifecycleStatus", "Active"},
	IsRoot = {"isRoot", true},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, IsRoot]}.

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

-spec resource(Resource) -> Resource
	when
		Resource :: resource() | {struct, list()}.
%% @doc CODEC for `Resource'.
resource({struct, Object}) ->
	resource(Object, #resource{});
resource(#resource{} = Resource) ->
	resource(record_info(fields, resource), Resource, []).
%% @hidden
resource([{"id", Id} | T], Acc) when is_list(Id) ->
	resource(T, Acc#resource{id = Id});
resource([{"href", Href} | T], Acc) when is_list(Href) ->
	resource(T, Acc#resource{href = Href});
resource([{"name", Name} | T], Acc) when is_list(Name) ->
	resource(T, Acc#resource{name = Name});
resource([{"description", Description} | T], Acc) when is_list(Description) ->
	resource(T, Acc#resource{description = Description});
resource([{"category", Category} | T], Acc) when is_list(Category) ->
	resource(T, Acc#resource{category = Category});
resource([{"@type", Type} | T], Acc) when is_list(Type) ->
	resource(T, Acc#resource{class_type = Type});
resource([{"@baseType", Type} | T], Acc) when is_list(Type) ->
	resource(T, Acc#resource{base_type = Type});
resource([{"@schemaLocation", Schema} | T], Acc) when is_list(Schema) ->
	resource(T, Acc#resource{schema = Schema});
resource([{"version", Version} | T], Acc) when is_list(Version) ->
	resource(T, Acc#resource{version = Version});
resource([{"validFor", {struct, L}} | T], Resource) ->
	Resource1 = case lists:keyfind("startDateTime", 1, L) of
		{_, Start} ->
			Resource#resource{start_date = ocs_rest:iso8601(Start)};
		false ->
			Resource
	end,
	Resource2 = case lists:keyfind("endDateTime", 1, L) of
		{_, End} ->
			Resource1#resource{end_date = ocs_rest:iso8601(End)};
		false ->
			Resource1
	end,
	resource(T, Resource2);
resource([{"lastUpdate", DateTime} | T], Acc) when is_list(DateTime) ->
	LM = {ocs_rest:iso8601(DateTime), erlang:unique_integer([positive])},
	resource(T, Acc#resource{last_modified = LM});
resource([{"lifecycleState", State} | T], Acc) when is_list(State) ->
	resource(T, Acc#resource{state = State});
resource([{"lifecycleSubState", SubState} | T], Acc) when is_list(SubState) ->
	resource(T, Acc#resource{substate = SubState});
resource([{"resourceRelationship", {array, _} = ResRel} | T], Acc) ->
	resource(T, Acc#resource{related = resource_rel(ResRel)});
resource([{"resourceSpecification", SpecRef} | T], Acc) when is_tuple(SpecRef) ->
	resource(T, Acc#resource{specification = specification_ref(SpecRef)});
resource([{"resourceCharacteristic", ResChar} | T], Acc)
		when is_tuple(ResChar) ->
	resource(T, Acc#resource{characteristic = resource_char(ResChar)});
resource([_ | T], Acc) ->
	resource(T, Acc);
resource([], Acc) ->
	Acc.
%% @hidden
resource([id | T], #resource{id = Id} = R, Acc) when is_list(Id) ->
	resource(T, R, [{"id", Id} | Acc]);
resource([href | T], #resource{href = Href} = R, Acc)
		when is_list(Href) ->
	resource(T, R, [{"href", Href} | Acc]);
resource([name | T], #resource{name = Name} = R, Acc)
		when is_list(Name) ->
	resource(T, R, [{"name", Name} | Acc]);
resource([description | T],
		#resource{description = Description} = R, Acc)
		when is_list(Description) ->
	resource(T, R, [{"description", Description} | Acc]);
resource([category | T], #resource{category = Category} = R, Acc)
		when is_list(Category) ->
	resource(T, R, [{"category", Category} | Acc]);
resource([class_type | T], #resource{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource(T, R, [{"@type", Type} | Acc]);
resource([base_type | T], #resource{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource(T, R, [{"@baseType", Type} | Acc]);
resource([schema | T], #resource{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource(T, R, [{"@schemaLocation", Schema} | Acc]);
resource([version | T], #resource{version = Version} = R, Acc)
		when is_list(Version) ->
	resource(T, R, [{"version", Version} | Acc]);
resource([start_date | T], #resource{start_date = StartDate,
		end_date = undefined} = R, Acc) when is_integer(StartDate) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(StartDate)}]},
	resource(T, R, [{"validFor", ValidFor} | Acc]);
resource([start_date | T], #resource{start_date = undefined,
		end_date = EndDate} = R, Acc) when is_integer(EndDate) ->
	ValidFor = {struct, [{"endDateTime", ocs_rest:iso8601(EndDate)}]},
	resource(T, R, [{"validFor", ValidFor} | Acc]);
resource([start_date | T], #resource{start_date = StartDate,
		end_date = EndDate} = R, Acc) when is_integer(StartDate),
		is_integer(EndDate) ->
	ValidFor = {struct, [{"startDateTime", ocs_rest:iso8601(StartDate)},
			{"endDateTime", ocs_rest:iso8601(EndDate)}]},
	resource(T, R, [{"validFor", ValidFor} | Acc]);
resource([last_modified | T], #resource{last_modified = {TS, _}} = R, Acc)
		when is_integer(TS) ->
	resource(T, R, [{"lastUpdate", ocs_rest:iso8601(TS)} | Acc]);
resource([state | T], #resource{state = State} = R, Acc)
		when State /= undefined ->
	resource(T, R, [{"lifecycleState", State} | Acc]);
resource([substate | T], #resource{substate = SubState} = R, Acc)
		when SubState /= undefined ->
	resource(T, R, [{"lifecycleSubState", SubState} | Acc]);
resource([related | T], #resource{related = ResRel} = R, Acc)
		when is_list(ResRel), length(ResRel) > 0 ->
	resource(T, R, [{"resourceRelationship",
			{array, resource_rel(ResRel)}} | Acc]);
resource([specification | T], #resource{specification = SpecRef} = R, Acc)
		when is_record(SpecRef, specification_ref) ->
	resource(T, R, [{"resourceSpecification",
			specification_ref(SpecRef)} | Acc]);
resource([characteristic | T], #resource{characteristic = ResChar} = R, Acc)
		when is_list(ResChar), length(ResChar) > 0 ->
	resource(T, R, [{"resourceCharacteristic",
			{array, resource_char(ResChar)}} | Acc]);
resource([_ | T], R, Acc) ->
	resource(T, R, Acc);
resource([], _, Acc) ->
	{struct, lists:reverse(Acc)}.

-spec resource_rel(ResourceRelationship) -> ResourceRelationship
	when
		ResourceRelationship :: [resource_rel()] | {array, list()}.
%% @doc CODEC for `ResourceRelationship'.
resource_rel([#resource_rel{} | _] = List) ->
	Fields = record_info(fields, resource_rel),
	[resource_rel(Fields, R, []) || R <- List];
resource_rel({array, [{struct, _} | _] = StructList}) ->
	[resource_rel(Object, #resource_rel{}) || {struct, Object} <- StructList].
%% @hidden
resource_rel([{"resource", {struct, AttrList}} | T], Acc) ->
	resource_rel(T, resource_rel1(AttrList, Acc));
resource_rel([{"relationshipType", RelType} | T], Acc) when is_list(RelType) ->
	resource_rel(T, Acc#resource_rel{type = RelType});
resource_rel([_ | T], Acc) ->
	resource_rel(T, Acc);
resource_rel([], ResourceRel) ->
	ResourceRel.
%% @hidden
resource_rel([id | T], #resource_rel{id = Id, href = Href, name = Name,
		referred_type = RefType} = R, Acc) when is_list(Id), is_list(Href),
		is_list(Name), is_list(RefType) ->
	resource_rel(T, R, [{"resource", {struct, [{"id", Id}, {"href", Href},
			{"name", Name}, {"@referredType", RefType}]}} | Acc]);
resource_rel([id | T], #resource_rel{id = Id, href = Href, name = Name} = R,
		Acc) when is_list(Id), is_list(Href), is_list(Name) ->
	resource_rel(T, R, [{"resource", {struct, [{"id", Id}, {"href", Href},
			{"name", Name}]}} | Acc]);
resource_rel([type | T], #resource_rel{type = Type} = R, Acc)
		when is_list(Type) ->
	resource_rel(T, R, [{"relationshipType", Type} | Acc]);
resource_rel([_ | T], R, Acc) ->
	resource_rel(T, R, Acc);
resource_rel([], _, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
resource_rel1([{"id", Id} | T], Acc) when is_list(Id) ->
	resource_rel1(T, Acc#resource_rel{id = Id});
resource_rel1([{"href", Href} | T], Acc) when is_list(Href) ->
	resource_rel1(T, Acc#resource_rel{href = Href});
resource_rel1([{"name", Name} | T], Acc) when is_list(Name) ->
	resource_rel1(T, Acc#resource_rel{name = Name});
resource_rel1([{"@referredType", RefType} | T], Acc) when is_list(RefType) ->
	resource_rel1(T, Acc#resource_rel{referred_type = RefType});
resource_rel1([_ | T], Acc) ->
	resource_rel1(T, Acc);
resource_rel1([], ResourceRel) ->
	ResourceRel.

-spec resource_char(ResourceCharacteristic) -> ResourceCharacteristic
	when
		ResourceCharacteristic :: [resource_char()] | {array, list()}.
%% @doc CODEC for `ResourceCharacteristic'.
resource_char({array, [{struct, _} | _] = StructList}) ->
	[resource_char(Object, #resource_char{}) || {struct, Object} <- StructList];
resource_char([#resource_char{} | _] = List) ->
	[resource_char1(R) || R <- List].
%% @hidden
resource_char([{"name", Name} | T], Acc) when is_list(Name) ->
	resource_char(T, Acc#resource_char{name = Name});
resource_char([{"value", Value} | T], Acc) when is_list(Value) ->
	resource_char(T, Acc#resource_char{value = Value});
resource_char([{"value", Value} | T], Acc) when is_integer(Value) ->
	resource_char(T, Acc#resource_char{value = Value});
resource_char([{"value", Value} | T], Acc) when is_float(Value) ->
	resource_char(T, Acc#resource_char{value = Value});
resource_char([{"value", Value} | T], Acc) when is_boolean(Value) ->
	resource_char(T, Acc#resource_char{value = Value});
resource_char([{"value", {struct, QosList}} | T],
		#resource_char{name = "qosInformation"} = Acc) when is_list(QosList) ->
	resource_char(T, Acc#resource_char{value = qos(QosList, #{})});
resource_char([{"value", {array, Flows}} | T],
		#resource_char{name = "flowInformation"} = Acc) when is_list(Flows) ->
	resource_char(T, Acc#resource_char{value = flow(Flows, [])});
resource_char([], Acc) ->
	Acc.
%% @hidden
resource_char1(#resource_char{name = Name, value = Value})
		when is_integer(Value) ->
	{struct, [{"name", Name}, {"value", Value}]};
resource_char1(#resource_char{name = Name, value = Value})
		when is_float(Value) ->
	{struct, [{"name", Name}, {"value", Value}]};
resource_char1(#resource_char{name = Name, value = Value})
		when is_boolean(Value) ->
	{struct, [{"name", Name}, {"value", Value}]};
resource_char1(#resource_char{name = "qosInformation",
		value = #{"maxRequestedBandwidthDL" := MaxDL,
		"maxRequestedBandwidthUL" := MaxUL, "qosClassIdentifier" := Class}})
		when is_integer(MaxDL), is_integer(MaxUL), is_integer(Class) ->
	{struct, [{"name", "qosInformation"}, {"value", {struct,
			[{"maxRequestedBandwidthDL", MaxDL}, {"maxRequestedBandwidthUL",
			MaxUL}, {"qosClassIdentifier", Class}]}}]};
resource_char1(#resource_char{name = "flowInformation", value = Flows})
		when is_list(Flows) ->
	{struct, [{"name", "flowInformation"},
			{"value", {array, flow(Flows, [])}}]};
resource_char1(#resource_char{name = Name, value = Value})
		when is_list(Value) ->
	{struct, [{"name", Name}, {"value", Value}]}.

%% @hidden
qos([{Field, Value} | T], Acc) when is_integer(Value),
		Field == "qosClassIdentifier"; Field == "maxRequestedBandwidthUL";
		Field == "maxRequestedBandwidthDL" ->
	qos(T, Acc#{Field => Value});
qos([], Acc) ->
	Acc.

-spec flow(Flows, Acc) -> Flows
	when
		Flows :: [Flow],
		Flow :: {struct, [{Name :: string(), Value :: string()}]} | map(),
		Acc :: [Flow].
%% @hidden
flow([#{"flowDescription" := Description, "flowDirection" := Direction} | T], Acc)
		when is_list(Description), is_integer(Direction) ->
	flow(T, [{struct, [{"flowDescription", Description},
			{"flowDirection", flow_dir(Direction)}]} | Acc]);
flow([{struct, [{"flowDescription", Description},
		{"flowDirection", Direction}]} | T], Acc)
		when is_list(Description), is_list(Direction) ->
	flow(T, [#{"flowDescription" => Description,
			"flowDirection" => flow_dir(Direction)} | Acc]);
flow([{struct, [{"flowDirection", Direction},
		{"flowDescription", Description}]} | T], Acc)
		when is_list(Description), is_list(Direction) ->
	flow(T, [#{"flowDescription" => Description,
			"flowDirection" => flow_dir(Direction)} | Acc]);
flow([], Acc) ->
	lists:reverse(Acc).

-spec flow_dir(Direction) -> Direction
	when
		Direction :: 0..3 | string().
%% @hidden
flow_dir(0) ->
	"unspecified";
flow_dir(1) ->
	"down";
flow_dir(2) ->
	"up";
flow_dir(3) ->
	"both";
flow_dir("unspecified") ->
	0;
flow_dir("down") ->
	1;
flow_dir("up") ->
	2;
flow_dir("both") ->
	3.

-spec specification_ref(ResourceSpecificationRef) -> ResourceSpecificationRef
	when
		ResourceSpecificationRef :: specification_ref() | {struct, list()}.
%% @doc CODEC for `ResourceSpecificationRef'.
specification_ref({struct, Object}) ->
	specification_ref(Object, #specification_ref{});
specification_ref(#specification_ref{} = ResourceSpecificationRef) ->
	specification_ref(record_info(fields, specification_ref),
			ResourceSpecificationRef, []).
%% @hidden
specification_ref([{"id", Id} | T], Acc) when is_list(Id) ->
	specification_ref(T, Acc#specification_ref{id = Id});
specification_ref([{"href", Href} | T], Acc) when is_list(Href) ->
	specification_ref(T, Acc#specification_ref{href = Href});
specification_ref([{"name", Name} | T], Acc) when is_list(Name) ->
	specification_ref(T, Acc#specification_ref{name = Name});
specification_ref([{"version", Version} | T], Acc) when is_list(Version) ->
	specification_ref(T, Acc#specification_ref{version = Version});
specification_ref([_ | T], Acc) ->
	specification_ref(T, Acc);
specification_ref([], Acc) ->
	Acc.
%% @hidden
specification_ref([id | T], #specification_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	specification_ref(T, R, [{"id", Id} | Acc]);
specification_ref([href | T], #specification_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	specification_ref(T, R, [{"href", Href} | Acc]);
specification_ref([name | T], #specification_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	specification_ref(T, R, [{"name", Name} | Acc]);
specification_ref([version | T], #specification_ref{version = Version} = R, Acc)
		when is_list(Version) ->
	specification_ref(T, R, [{"version", Version} | Acc]);
specification_ref([_ | T], R, Acc) ->
	specification_ref(T, R, Acc);
specification_ref([], _, Acc) ->
	{struct, lists:reverse(Acc)}.

%% @hidden
match(Key, Complex, Query) ->
	match1(Key, lists:keyfind(Key, 1, Complex), Query).
%% @hidden
match1(Key, {_, like, [Value]}, _Query) ->
	{like, Value};
match1(Key, {_, exact, [Value]}, _Query) ->
	{exact, Value};
match1(Key, {_, contains, [{complex, Complex}]}, _Query) ->
	match4(Key, Complex);
match1(Key, false, Query) ->
	match2(lists:keyfind(Key, 1, Query)).
%% @hidden
match2({_, Value}) ->
	match3(string:lexemes(Value, [$,]), []);
match2(false) ->
	'_'.
%% @hidden
match3([H | T], Acc) ->
	match3(T, [{exact, H} | Acc]);
match3([], Acc) ->
	lists:reverse(Acc).
%% @hidden
match4("resourceRelationship", Complex) ->
	match5(lists:keyfind("resource.name", 1, Complex));
match4(_Key, _Complex) ->
	'_'.
%% @hidden
match5({_, exact, [Value]}) ->
	{exact, Value};
match5({_, like, [Value]}) ->
	{like, Value};
match5(false) ->
	'_'.

