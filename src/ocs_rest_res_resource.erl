%% ocs_rest_res_resource.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2022 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2022 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_resource_spec/1, get_resource_specs/1]).
-export([get_resource_category/1, get_resource_categories/1]).
-export([get_resource_candidate/1, get_resource_candidates/1]).
-export([get_resource_catalog/1, get_resource_catalogs/1]).
-export([get_resource/1, get_resource/2, add_resource/1, patch_resource/3,
		delete_resource/1, head_resource/0]).
-export([get_pla_specs/1]).
-export([resource/1, gtt/2]).

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
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec get_resource_spec(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
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
get_resource_spec(_) ->
	{error, 404}.

-spec get_resource_specs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500.
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
		Result :: {struct, [tuple()]} | {error, 404}.
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
	Object = {array, [tariff_table_category(), policy_table_category()]},
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

-spec get_resource(Id) -> Result
	when
		Id :: string(),
		Result   :: {ok, Headers, Body} | {error, Status},
		Headers  :: [tuple()],
		Body     :: iolist(),
		Status   :: 400 | 404 | 500.
%% @doc Respond to `GET /resourceInventoryManagement/v1/resource/{id}'.
%%    Retrieve resource from inventory management.
get_resource(Id) ->
	try
		string:tokens(Id, "-")
	of
		[Table, Prefix] ->
			{Description, Rate, LM} = ocs_gtt:lookup_first(Table, Prefix),
			Headers = [{content_type, "application/json"},
					{etag, ocs_rest:etag(LM)}],
			Body = mochijson:encode(gtt(Table, {Prefix, Description, Rate})),
			{ok, Headers, Body};
		_ ->
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
		error:badarg ->
			{error, 404};
		_:_Reason1 ->
			{error, 500}
	end.

-spec head_resource() -> Result
	when
		Result :: {ok, [], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
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
	

-spec get_resource(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
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
						MatchRelName
								= match("resourceRelationship.resource.name", Complex, Query),
						MatchPrefix = match("resourceCharacteristic.prefix", Complex, Query),
						{Query1, [MatchId, MatchCategory, MatchSpecId, MatchRelName, MatchPrefix]}
				end;
			false ->
					MatchId = match("id", [], Query),
					MatchCategory = match("category", [], Query),
					MatchSpecId = match("resourceSpecification.id", [], Query),
					MatchRelName
							= match("resourceRelationship.resource.name", [], Query),
					MatchPrefix = match("resourceCharacteristic.prefix", [], Query),
					{Query, [MatchId, MatchCategory, MatchSpecId, MatchRelName, MatchPrefix]}
		end
	of
		{Query2, [_, _, {exact, "2"}, {exact, Table}, MatchPrefix1]} ->
			Codec = fun gtt/2,
			query_filter({ocs_gtt, query, [list_to_existing_atom(Table), MatchPrefix1]}, Codec, Query2, Headers);
		{Query2, [ResId, ResName, SpecId, RelName, _]} ->
			Codec = fun resource/1,
			query_filter({ocs, query_resource, [ResId, ResName, SpecId, RelName]}, Codec, Query2, Headers)
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
		{[#gtt{} | _] = Result, ContentRange} ->
			case lists:keyfind("resourceRelationship.resource.name", 1,
					Query) of
				{_, Table} ->
					Objects = [gtt(Table, {Prefix, Description, Rate})
							|| #gtt{num = Prefix, value = {Description, Rate, _}}
							<- Result],
					JsonObj = query_page1(Objects, Filters, []),
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange}],
					{ok, Headers, Body};
				false ->
					{error, 400}
			end;
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
		RequestBody :: [tuple()],
		Result   :: {ok, Headers, Body} | {error, Status},
		Headers  :: [tuple()],
		Body     :: iolist(),
		Status   :: 400 | 500 .
%% @doc Respond to
%% 	`POST /resourceInventoryManagement/v1/resource'.
%%    Add a new resource in inventory.
add_resource(RequestBody) ->
	try
		resource(mochijson:decode(RequestBody))
	of
		#resource{name = Name, specification = #specification_ref{id = SpecId}}
				= Resource when SpecId == "1"; SpecId == "3" ->
			F = fun F(eof, Acc) ->
						lists:flatten(Acc);
					F(Cont1, Acc) ->
						{Cont2, L} = ocs:query_resource(Cont1, '_', {exact, Name},
								{exact, SpecId}, '_'),
						F(Cont2, [L | Acc])
			end,
			case F(start, []) of
				[] ->
					add_resource1(ocs:add_resource(Resource));
				[#resource{} | _] ->
					{error, 400}
			end;
		#resource{specification = #specification_ref{id = "2"},
				related = [#resource_rel{name = Table}],
				characteristic = Chars} = Resource1 ->
			F = fun(CharName) ->
						case lists:keyfind(CharName, #resource_char.name, Chars) of
							#resource_char{value = Value} ->
								Value;
							false ->
								{error, 400}
						end
			end,
			Prefix = F("prefix"),
			{ok, #gtt{value = {_, _, LM}}} = ocs_gtt:insert(Table, Prefix,
					{F("description"), ocs_rest:millionths_in(F("rate"))}),
			Id = Table ++ "-" ++ Prefix,
			Href = "/resourceInventoryManagement/v1/resource/" ++ Id,
			Resource2 = Resource1#resource{id = Id, href = F("prefix"),
					last_modified = LM},
			Headers = [{content_type, "application/json"},
					{location, Href}, {etag, ocs_rest:etag(LM)}],
			Body = mochijson:encode(resource(Resource2)),
			{ok, Headers, Body};
		#resource{specification = #specification_ref{id = "4"}} = Resource ->
			add_resource1(ocs:add_resource(Resource))
	catch
		_:_Reason ->
			{error, 400}
	end.
%% @hidden
add_resource1({ok, #resource{href = Href, last_modified = LM} = Resource}) ->
	Headers = [{content_type, "application/json"},
			{location, Href}, {etag, ocs_rest:etag(LM)}],
	Body = mochijson:encode(resource(Resource)),
	{ok, Headers, Body};
add_resource1({error, _Reason}) ->
	{error, 400}.

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

-spec delete_resource(Id) -> Result
   when
      Id :: string(),
      Result :: {ok, Headers :: [tuple()], Body :: iolist()}
            | {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /resourceInventoryManagement/v1/resource/{id}''
%%    request to remove a table row.
delete_resource(Id) ->
	try
		case string:tokens(Id, "-") of
			[Table, Prefix] ->
				Name = list_to_existing_atom(Table),
				ok = ocs_gtt:delete(Name, Prefix),
				{ok, [], []};
			[Id] ->
				delete_resource1(ocs:get_resource(Id))
		end
	catch
		error:badarg ->
			{error, 404};
		_:_ ->
			{error, 400}
	end.
%% @hidden
delete_resource1({ok, #resource{id = Id, name = Name,
		specification = #specification_ref{id = "3"}}}) ->
	F = fun F(eof, Acc) ->
				Acc;
			F(Cont1, Acc) ->
				case ocs:query_resource(Cont1, '_', '_',
						{exact, "4"}, {exact, Name}) of
					{error, _Reason} ->
						{error, 400};
					{Cont2, L} ->
						Fid = fun(#resource{id = RId}) ->
									RId
						end,
						F(Cont2, lists:map(Fid, L) ++ Acc)
				end
	end,
	case F(start, []) of
		[] ->
			delete_resource2([Id]);
		[ResId | _] = ResIdList when is_list(ResId) ->
			delete_resource2(ResIdList ++ [Id])
	end;
delete_resource1({ok, #resource{id = Id}}) ->
	delete_resource2([Id]);
delete_resource1({error, _Reason}) ->
	{error, 400}.
%% @hidden
delete_resource2([Id | T]) ->
	case ocs:delete_resource(Id) of
		ok ->
			delete_resource2(T);
		{error, _Reason} ->
			{error, 400}
	end;
delete_resource2([]) ->
	{ok, [], []}.

-spec patch_resource(Id, Etag, ReqData) -> Result
	when
		Id	:: string(),
		Etag	:: undefined | list(),
		ReqData	:: [tuple()],
		Result	:: {ok, Headers, Body} | {error, Status},
		Headers	:: [tuple()],
		Body		:: iolist(),
		Status	:: 400 | 404 | 500 .
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
		case string:tokens(Id, "-") of
			[Id] ->
				{Id, Etag1, mochijson:decode(ReqData)};
			[Table, Prefix] ->
				Table1 = list_to_existing_atom(Table),
				{Table1, Prefix, Etag1, mochijson:decode(ReqData)}
		end
	of
		{Id, Etag2, {array, _} = Operations} ->
			F = fun() ->
					case mnesia:read(resource, Id, write) of
						[Resource1] when
								Resource1#resource.last_modified == Etag2;
								Etag2 == undefined ->
							case catch ocs_rest:patch(Operations,
									resource(Resource1)) of
								{struct, _} = Resource2 ->
									Resource3 = resource(Resource2),
									TS = erlang:system_time(millisecond),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									Resource4 = Resource3#resource{last_modified = LM},
									ok = mnesia:write(Resource4),
									{Resource2, LM};
								_ ->
									throw(bad_request)
							end;
						[#resource{}] ->
							throw(precondition_failed);
						[] ->
							throw(not_found)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, {Resource, Etag3}} ->
					Location = "/resourceInventoryManagement/v1/resource/" ++ Id,
					Headers = [{content_type, "application/json"},
							{location, Location}, {etag, ocs_rest:etag(Etag3)}],
					Body = mochijson:encode(Resource),
					{ok, Headers, Body};
				{aborted, {throw, bad_request}} ->
					{error, 400};
				{aborted, {throw, not_found}} ->
					{error, 404};
				{aborted, {throw, precondition_failed}} ->
					{error, 412};
				{aborted, _Reason} ->
					{error, 500}
			end;
		{Table2, Prefix1, Etag2, {array, _} = Operations} ->
			case catch ocs_gtt:lookup_last(Table2, Prefix1) of
				{'EXIT', _} ->
					throw(not_found);
				{Description, Rate, LastModified1} when LastModified1 == Etag2;
						Etag2 == undefined ->
					case catch ocs_rest:patch(Operations,
							gtt(atom_to_list(Table2), {Prefix1, Description, Rate})) of
						{struct, _} = Res ->
							{Prefix1, Description1, Rate1} = gtt(Prefix1, Res),
							{ok, #gtt{value = {_, _, LastModified2}}}
									= ocs_gtt:insert(Table2, Prefix1,
											{Description1, Rate1}),
							Location = ?inventoryPath ++ atom_to_list(Table2)
									++ "-" ++ Prefix1,
							Headers = [{content_type, "application/json"},
									{location, Location}, {etag, ocs_rest:etag(LastModified2)}],
							Body = mochijson:encode(Res),
							{ok, Headers, Body};
						_ ->
							throw(bad_request)
					end
			end
	catch
		throw:not_found ->
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
	Chars = {array, [{struct, [{"name", "prefix"},
			{"description", "Call address prefix"}, {"valueType", "String"}]},
			{struct, [{"name", "description"},
			{"description", "Prefix description"}, {"valueType", "String"}]},
			{struct, [{"name", "rate"},
			{"description", "Rated price for address"},
			{"valueType", "Number"}]}]},
	Characteristic = {"resourceSpecCharacteristic", Chars},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate,
			Category, Characteristic]}.

%%@doc policy and charging control rules.
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

%%@doc policy and charging control rule.
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
	Chars = {array, [{struct, [{"name", "name"},
			{"description", "Policy rule name"},
			{"minCardinality", 1}, {"valueType", "String"}]},
			{struct, [{"name", "qosInformation"},
			{"description", "Quality of Service Information"},
			{"minCardinality", 0},
			{"valueType", "Object"}]},
			{struct, [{"name", "chargingKey"},
			{"description", "Charging rule"},
			{"minCardinality", 0}, {"valueType", "Number"}]},
			{struct, [{"name", "flowInformation"},
			{"description", "Service flow template"}, {"minCardinality", 1},
			{"valueType", "Array"}]},
			{struct, [{"name", "precedence"},
			{"description", "Priority order rules are applied in"},
			{"minCardinality", 1}, {"valueType", "Number"}]},
			{struct, [{"name", "serviceId"},
			{"description", "Service flow identifier"},
			{"minCardinality", 0}, {"valueType", "String"}]},
			{struct, [{"name", "predefined"},
			{"description", "Indicate PCEF predefined rule"},
			{"minCardinality", 0}, {"valueType", "Boolean"}]}]},
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

-spec gtt(Table, Gtt) -> Gtt
	when
		Table :: string(),
		Gtt :: {Prefix, Description, Rate} | {struct, [tuple()]},
		Prefix :: string(),
		Description :: string(),
		Rate :: non_neg_integer().
%% @doc CODEC for gtt.
%% @private
gtt(Table, {Prefix, Description, Rate} = _Gtt) ->
	Id = Table ++ "-" ++ Prefix,
	SpecId = {"id", "2"},
   SpecHref = {"href", ?specPath "2"},
   SpecName = {"name", "TariffTableRow"},
	{struct, [{"id", Id},
			{"href", ?inventoryPath ++ Id},
			{"resourceSpecification", {struct, [SpecId, SpecHref, SpecName]}},
			{"resourceCharacteristic", {array,
			[{struct, [{"name", "prefix"}, {"value", Prefix}]},
			{struct, [{"name", "description"}, {"value", Description}]},
			{struct, [{"name", "rate"}, {"value", ocs_rest:millionths_out(Rate)}]}]}}]};
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
		{value, {"name", "prefix"}, [{"value", Prefix1}]} ->
			gtt2(T, {Prefix1, Desc, Rate});
		{value, {"name", "description"}, [{"value", Desc1}]} ->
			gtt2(T, {Prefix, Desc1, Rate});
		{value, {"name", "rate"}, [{"value", Rate1}]} ->
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
	case lists:keyfind(Key, 1, Complex) of
		{_, like, [Value]} ->
			{like, Value};
		{_, exact, [Value]} ->
			{exact, Value};
		false ->
			case lists:keyfind(Key, 1, Query) of
				{_, Value} ->
					{exact, Value};
				false ->
					'_'
			end
	end.

