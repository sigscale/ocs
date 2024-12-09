%%% ocs_rest_res_service.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2024 SigScale Global Inc.
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
-module(ocs_rest_res_service).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([add_inventory/1, get_inventory/1, get_inventories/2,
		delete_inventory/1, patch_inventory/3, head_inventory/0]).
-export([get_service_specs/1, get_service_spec/2]).
-export([inventory/1]).

-include("ocs.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include_lib("radius/include/radius.hrl").

-define(serviceSpecPath, "/serviceCatalogManagement/v2/serviceSpecification/").
-define(serviceInventoryPath, "/serviceInventoryManagement/v2/service/").

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

-spec add_inventory(RequestBody) ->
	Result when
		RequestBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /serviceInventoryManagement/v2/service'.
%% 	Add a new Service Inventory.
add_inventory(RequestBody) ->
	try
		#service{name = Identity, password = Password,
			attributes = Attributes, product = ProductRef,
			state = State, enabled = Enabled, multisession = MultiSession,
			characteristics = Chars} =
			inventory(mochijson:decode(RequestBody)),
		case ocs:add_service(Identity, Password, State, ProductRef,
				Chars, Attributes, Enabled, MultiSession) of
			{ok, Service1} ->
				Service1;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Service ->
			Body = mochijson:encode(inventory(Service)),
			Href = ?serviceInventoryPath ++ binary_to_list(Service#service.name),
			Etag = ocs_rest:etag(Service#service.last_modified),
			Headers = [{content_type, "application/json"},
					{location, Href}, {etag, Etag}],
			{ok, Headers, Body}
	catch
		throw:product_not_found ->
			{error, 400};
		throw:service_exists ->
			{error, 400};
		throw:_ ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec get_inventory(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `GET /serviceInventoryManagement/v2/service/{id}'.
%% 	Retrieve a service inventories.
get_inventory(Id) ->
	try
		case ocs:find_service(Id) of
			{ok, Service1} ->
				Service1;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Service ->
			Body = mochijson:encode(inventory(Service)),
			Etag = ocs_rest:etag(Service#service.last_modified),
			Href = ?serviceInventoryPath ++ binary_to_list(Service#service.name),
			Headers = [{content_type, "application/json"},
					{location, Href}, {etag, Etag},
					{content_type, "application/json"}],
			{ok, Headers, Body}
	catch
		throw:not_found ->
			{error, 404};
		_:_ ->
			{error, 500}
	end.

-spec head_inventory() -> Result
   when
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%%    `HEAD /serviceInventoryManagement/v2/service'
%%    requests.
head_inventory() ->
   try
      Size = mnesia:table_info(client, size),
      LastItem = integer_to_list(Size),
      ContentRange = "items 1-" ++ LastItem ++ "/" ++ LastItem,
      Headers = [{content_range, ContentRange}],
      {ok, Headers, []}
   catch
      _:_Reason ->
         {error, 500}
   end.

-spec get_inventories(Query, RequestHeaders) -> Result
	when
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `GET /serviceInventoryManagement/v2/service'.
%% 	Retrieve all Service Inventories.
get_inventories(Query, RequestHeaders) ->
	try
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchId = match("id", Complex, Query1),
						MatchProduct = match("product", Complex, Query1),
						{Query, [MatchId, MatchProduct]}
				end;
			false ->
				MatchId = match("id", [], Query),
				MatchProduct = match("product", [], Query),
				{Query, [MatchId, MatchProduct]}
		end
	of
		{Query2, Args} ->
			Codec = fun inventory/1,
			query_filter({ocs, query_service, Args}, Codec, Query2, RequestHeaders)
	catch
		_ ->
			{error, 400}
	end.

-spec delete_inventory(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `DELETE /serviceInventoryManagement/v2/service/{id}'
%% 	request to remove a `Service Inventory'.
delete_inventory(Id) ->
	case catch ocs:delete_service(Id) of
		ok ->
			{ok, [], []};
		{'EXIT', _} ->
			{error, 500}
	end.

-spec patch_inventory(ServiceId, Etag, RequestBody) -> Result
	when
		ServiceId :: string(),
		Etag :: undefined | string(),
		RequestBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `PATCH /serviceInventoryManagement/v2/service/{id}'.
%% 	Update a Service Inventory using JSON patch method
%% 	<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>.
patch_inventory(ServiceId, Etag, RequestBody) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Etag1, mochijson:decode(RequestBody)}
	of
		{Etag2, {array, _} = Operations} ->
			F = fun() ->
					case mnesia:read(service, list_to_binary(ServiceId), write) of
						[#service{product = ProductRef} = Service1] when
								Service1#service.last_modified == Etag2;
								Etag2 == undefined,
								ProductRef =/= undefined ->
							case catch ocs_rest:patch(Operations, inventory(Service1)) of
								{struct, _} = Service2 ->
									TS = erlang:system_time(millisecond),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									case inventory(Service2) of
										#service{product = undefined} = Service3 ->
											Service4 = Service3#service{last_modified = LM,
													product = undefined},
											{ok, #product{service = Services} = Product}
													= ocs:find_product(ProductRef),
											Product1 = Product#product{service = Services -- [list_to_binary(ServiceId)],
													last_modified = LM},
											ok = mnesia:write(product, Product1, write),
											ok = mnesia:write(Service4),
											{Service2, LM};
										#service{product = ProductRef1} = Service3
												when ProductRef1 =/= ProductRef,
												ProductRef1 =/= undefined->
											Service4 = Service3#service{last_modified = LM,
													product = ProductRef1},
											{ok, #product{service = Services} = Product}
													= ocs:find_product(ProductRef1),
											{ok, #product{service = OldServices} = OldProduct}
													= ocs:find_product(ProductRef),
											Product1 = Product#product{service = Services ++ [list_to_binary(ServiceId)],
													last_modified = LM},
											Product2 = OldProduct#product{service = OldServices -- [list_to_binary(ServiceId)],
													last_modified = LM},
											ok = mnesia:write(product, Product1, write),
											ok = mnesia:write(product, Product2, write),
											ok = mnesia:write(Service4),
											{Service2, LM};
										#service{product = ProductRef} = Service3 ->
											Service4 = Service3#service{last_modified = LM},
											ok = mnesia:write(Service4),
											{Service2, LM}
									end;
								_ ->
									throw(bad_request)
							end;
						[Service1] when
								Service1#service.last_modified == Etag2;
								Etag2 == undefined ->
							case catch ocs_rest:patch(Operations, inventory(Service1)) of
								{struct, _} = Service2 ->
									Service3 = inventory(Service2),
									TS = erlang:system_time(millisecond),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									Service4 = Service3#service{last_modified = LM},
									ok = mnesia:write(Service4),
									{Service2, LM}
							end;
						[#service{}] ->
							throw(precondition_failed);
						[] ->
							throw(not_found)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, {Service, Etag3}} ->
					Location = ?serviceInventoryPath ++ ServiceId,
					Headers = [{content_type, "application/json"},
							{location, Location}, {etag, ocs_rest:etag(Etag3)}],
					Body = mochijson:encode(Service),
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

-spec get_service_specs(Query) -> Result
	when
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `GET /serviceCatalogManagement/v2/serviceSpecification/'.
%% 	Retrieve all service specifications.
get_service_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [ocs_service_spec()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_service_specs(_Query) ->
	{error, 400}.

-spec get_service_spec(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `GET /serviceCatalogManagement/v2/serviceSpecification/{id}'.
%% 	Retrieve a service specification.
get_service_spec(ID, [] = _Query) ->
	case service_spec(ID) of
		{error, StatusCode} ->
			{error, StatusCode};
		ServiceSpec ->
			Headers = [{content_type, "application/json"}],
			Body = mochijson:encode(ServiceSpec),
			{ok, Headers, Body}
	end;
get_service_spec(_ID, _Query) ->
	{error, 400}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec service_spec(ID) -> Result
	when
		ID :: string(),
		Result :: {struct, [tuple()]} | {error, 404}.
%% @doc Get Service Specification by ID
service_spec("1") ->
	ocs_service_spec();
service_spec(_) ->
	{error, 404}.

%% @hidden
ocs_service_spec() ->
	Id = {"id", "1"},
	Href = {"href", ?serviceSpecPath ++ "1"},
	Name = {"name", "OCSServiceSpec"},
	Description = {"description", ""},
	Version = {"version", "1.0"},
	Status = {"lifecycleStatus", "Active"},
	LastUpdate = {"lastUpdate", "2018-02-22T00:00:00Z"},
	Type = {"type", "RFS"},
	Chars = {"serviceSpecCharacteristic", {array, service_spec_chars()}},
	{struct, [Id, Href, Name, Description, Version, Status, LastUpdate, Type, Chars]}.

%% @hidden
service_spec_chars() ->
	Name1 = {"name", "serviceIdentity"},
	Description1 = {"description",
			"Uniquely identifies service (e.g. MSISDN, IMSI, username)."},
	Config1 = {"configurable", true},
	Type1 = {"valueType", "String"},
	Value1 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type1]}]}},
	Char1 = {struct, [Name1, Description1, Config1, Type1, Value1]},
	Name2 = {"name", "servicePassword"},
	Description2 = {"description", "Shared secret used in authentication."},
	Config2 = {"configurable", true},
	Type2 = {"valueType", "String"},
	Value2 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type2]}]}},
	Char2 = {struct, [Name2, Description2, Config2, Type2, Value2]},
	Name3 = {"name", "acctSessionInterval"},
	Description3 = {"description", "Number of seconds for interim updated"},
	Config3 = {"configurable", false},
	Type3 = {"valueType", "Number"},
	Value3 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type3]}]}},
	Char3 = {struct, [Name3, Description3, Config3, Type3, Value3]},
	Name4 = {"name", "sessionTimeout"},
	Description4 = {"description", "Number of seconds for one session"},
	Config4 = {"configurable", false},
	Type4 = {"valueType", "Number"},
	Value4 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type4]}]}},
	Char4 = {struct, [Name4, Description4, Config4, Type4, Value4]},
	Name5 = {"name", "multiSession"},
	Description5 = {"description", "Multiple concurrent sessions allowed"},
	Config5 = {"configurable", true},
	Type5 = {"valueType", "boolean"},
	Value5 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type5]}]}},
	Char5 = {struct, [Name5, Description5, Config5, Type5, Value5]},
	Name6 = {"name", "class"},
	Description6 = {"description", "Class"},
	Config6 = {"configurable", true},
	Type6 = {"valueType", "String"},
	Value6 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type6]}]}},
	Char6 = {struct, [Name6, Description6, Config6, Type6, Value6]},
	Name7 = {"name", "serviceAkak"},
	Description7 = {"description", "AKA K"},
	Config7 = {"configurable", true},
	Type7 = {"valueType", "String"},
	Value7 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type7]}]}},
	Char7 = {struct, [Name7, Description7, Config7, Type7, Value7]},
	Name8 = {"name", "serviceAkaOPc"},
	Description8 = {"description", "AKA OPc"},
	Config8 = {"configurable", true},
	Type8 = {"valueType", "String"},
	Value8 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type8]}]}},
	Char8 = {struct, [Name8, Description8, Config8, Type8, Value8]},
	[Char1, Char2, Char3, Char4, Char5, Char6, Char7, Char8].

-spec inventory(Service) -> Service
	when
		Service :: #service{} | {struct, list()}.
%% @doc CODEC for service inventory
inventory({struct, Service}) ->
	inventory(Service, #service{});
inventory(#service{} = Service) ->
	inventory(record_info(fields, service), Service, [], []).
%% @hidden
inventory([{"id", Id}| T], Acc) ->
	inventory(T, Acc#service{name = list_to_binary(Id)});
inventory([{"state", State}| T], Acc) ->
	inventory(T, Acc#service{state = service_state(State)});
inventory([{"isServiceEnabled", Enabled}| T], Acc) ->
	inventory(T, Acc#service{enabled = Enabled});
inventory([{"product", ProductRef}| T], Acc) ->
	inventory(T, Acc#service{product = ProductRef});
inventory([{"serviceCharacteristic", Characteristics}| T], Acc) ->
	Chars = service_chars(Characteristics),
	F1 = fun(Key, Chars1) ->
			case lists:keyfind(Key, 1, Chars1) of
				{_, Value} ->
					Value;
				false ->
					undefined
			end
	end,
	Identity = case lists:keyfind("serviceIdentity", 1, Chars) of
		{_, V1} ->
			list_to_binary(V1);
		false ->
			undefined
	end,
	Password = case lists:keyfind("servicePassword", 1, Chars) of
		{_, V2} ->
			list_to_binary(V2);
		false ->
			case lists:keyfind("serviceAkaK", 1, Chars) of
				{_, V3} when length(V3) =:= 32 ->
					K = << <<N:4>> || N <- [list_to_integer([C], 16) || C <- V3] >>,
					case lists:keyfind("serviceAkaOPc", 1, Chars) of
						{_, V4} when length(V4) =:= 32 ->
							OPc = << <<N:4>> || N <- [list_to_integer([C], 16) || C <- V4] >>,
							#aka_cred{k = K, opc = OPc};
						false ->
							undefined
					end;
				false ->
					undefined
			end
	end,
	MultiSession = F1("multiSession", Chars),
	A1 = case F1("sessionTimeout", Chars) of
		undefined ->
			[];
		SessionTimeout ->
			[{?SessionTimeout, SessionTimeout}]
	end,
	A2  = case F1("acctSessionInterval", Chars) of
		undefined ->
			A1;
		SessionInterval ->
			[{?AcctInterimInterval, SessionInterval} | A1]
	end,
	A3  = case F1("class", Chars) of
		undefined ->
			A2;
		Class ->
			[{?Class, Class} | A2]
	end,
	F2 = fun(Key1, Chars1, AccIn) ->
			case lists:keyfind(Key1, 1, Chars1) of
				false ->
					AccIn;
				Result ->
					[Result | AccIn]
			end
	end,
	C0 = F2("radiusReserveTime", Chars, []),
	C1 = F2("radiusReserveOctets", Chars, C0),
	C2 = F2("ReserveSessionTime", Chars, C1),
	NewAcc = Acc#service{name = Identity, password = Password,
			multisession = MultiSession, attributes = A3, characteristics = C2},
	inventory(T, NewAcc);
inventory([{"category", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"description", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"endDate", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"hasStarted", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"href", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"isStateful", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"name", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"orderDate", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"startDate", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"startMode", StartMode}| T], Acc) ->
	_Mode = start_mode(StartMode),
	inventory(T, Acc);
inventory([{"status", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"type", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"supportingResource", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"serviceRelationship", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"place", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"note", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"supportingService", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"serviceSpecification", _}| T], Acc) ->
	inventory(T, Acc);
inventory([{"relatedPary", {array, _}}| T], Acc) ->
	inventory(T, Acc);
inventory([{"@type", _} | T], Acc) ->
	inventory(T, Acc);
inventory([{"@baseType", _} | T], Acc) ->
	inventory(T, Acc);
inventory([{"@schemaLocation", _} | T], Acc) ->
	inventory(T, Acc);
inventory([], Acc) ->
	Acc.
%% @hidden
inventory([product | T], #service{product = undefined} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, Acc);
inventory([product | T], #service{product = ProductRef} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, [{"product", ProductRef} | Acc]);
inventory([state | T], #service{state = State} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, [{"state", State} | Acc]);
inventory([enabled | T], #service{enabled = Enabled} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, [{"isServiceEnabled", Enabled} | Acc]);
inventory([name | T], #service{name = undefined} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, Acc);
inventory([name | T], #service{name = Identity} = Service,
		Chars, Acc) when is_binary(Identity) ->
	SId = {"serviceIdentity", binary_to_list(Identity)},
	Id = {"id", binary_to_list(Identity)},
	Href = {"href", ?serviceInventoryPath ++ binary_to_list(Identity)},
	NewChars = lists:keystore("serviceIdentity", 1, Chars, SId),
	inventory(T, Service, NewChars, [Id, Href | Acc]);
inventory([name | T], #service{name = Identity} = Service, Chars, Acc) ->
	Id = {"id", Identity},
	Href = {"href", ?serviceInventoryPath ++ Identity},
	SId = {"serviceIdentity", Identity},
	NewChars = lists:keystore("serviceIdentity", 1, Chars, SId),
	inventory(T, Service, NewChars, [Id, Href | Acc]);
inventory([password | T], #service{password = undefined} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, Acc);
inventory([password | T], #service{password = Password} = Service,
		Chars, Acc) when is_binary(Password) ->
	SPwd = {"servicePassword", binary_to_list(Password)},
	NewChars = lists:keystore("servicePassword", 1, Chars, SPwd),
	inventory(T, Service, NewChars, Acc);
inventory([password | T],
		#service{password = #aka_cred{k = K, opc = OPc}} = Service,
		Chars, Acc) ->
	NewChars = lists:keystore("serviceAkaK", 1, Chars,
			{"serviceAkaK", binary_to_hex(K)}),
	NextChars = lists:keystore("serviceAkaOPc", 1, NewChars,
			{"serviceAkaOPc", binary_to_hex(OPc)}),
	inventory(T, Service, NextChars, Acc);
inventory([multisession | T], #service{multisession = MultiSession} =
		Service, Chars, Acc) ->
	MS = {"multiSession", MultiSession},
	NewChars = lists:keystore("multiSession", 1, Chars, MS),
	inventory(T, Service, NewChars, Acc);
inventory([attributes | T], #service{attributes = []} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, Acc);
inventory([attributes | T], #service{attributes = Attributes} = Service, Chars, Acc) ->
	C1 = case lists:keyfind(?AcctInterimInterval, 1, Attributes) of
		{_, AcctSessionInterval} ->
			[{"acctSessionInterval", AcctSessionInterval}];
		false ->
			[]
	end,
	C2 = case lists:keyfind(?SessionTimeout, 1, Attributes) of
		{_, SessionTimeout} ->
			[{"sessionTimeout", SessionTimeout} | C1];
		false ->
			C1
	end,
	NewChars = C2 ++ Chars,
	inventory(T, Service, NewChars, Acc);
inventory([characteristics | T],
		#service{characteristics = []} = Service, Chars, Acc) ->
	inventory(T, Service, Chars, Acc);
inventory([characteristics | T], #service{characteristics = Chars2} =
		Service, Chars1, Acc) ->
	NewChars = Chars1 ++ Chars2,
	inventory(T, Service, NewChars, Acc);
inventory([_ | T], Service, Chars, Acc) ->
	inventory(T, Service, Chars, Acc);
inventory([], _Service, [], Acc) ->
	{struct, lists:reverse(Acc)};
inventory([], _Service, Chars, Acc) ->
	Obj = [{"serviceCharacteristic", service_chars(Chars)},
			{"@schemaLocation",
					"/schema/serviceInventoryManagement.schema.json"
					"#/definitions/Service"},
			{"@type", "Service"}, {"type", "RFS"} | Acc],
	{struct, lists:reverse(Obj)}.

-spec start_mode(Mode) -> Mode
	when
		Mode :: 0..5 | atom().
%% @doc Codec for Service start mode
start_mode(0) -> unknown;
start_mode(1) -> automatically_managed_environment;
start_mode(2) -> automatically_owning_device;
start_mode(3) -> manually_provider_of_service;
start_mode(4) -> manually_customer_of_service;
start_mode(5) -> any_of_the_above;
start_mode(unknown) -> 0;
start_mode(automatically_managed_environment) -> 1;
start_mode(automatically_owning_device) -> 2;
start_mode(manually_provider_of_service) -> 3;
start_mode(manually_customer_of_service) -> 4;
start_mode(any_of_the_above) -> 5.

-spec service_state(State) -> State
	when
		State :: atom() | string().
%% @doc CODEC for life cycle status of Product instance.
%% @private
service_state(feasibilityChecked) -> "feasibilityChecked";
service_state(designed) -> "designed";
service_state(reserved) -> "reserved";
service_state(active) -> "active";
service_state(inactive) -> "inactive";
service_state(terminated) -> "terminated";
service_state("feasibilityChecked") -> feasibilityChecked;
service_state("designed") -> designed;
service_state("reserved") -> reserved;
service_state("active") -> active;
service_state("inactive") -> inactive;
service_state("terminated") -> terminated.

-spec service_chars(ServiceChars) -> ServiceChars
	when
		ServiceChars :: [tuple()] | {array, list()}.
%% @doc CODEC for service characteristics
service_chars({array, L}) ->
	service_chars(L, []);
service_chars(Chars) when is_list(Chars) ->
	{array, service_chars(Chars, [])}.
%% @hidden
service_chars([{struct, [{"name", "serviceAkaK"}, {"value", K}]} | T], Acc) ->
	service_chars(T, [{"serviceAkaK", K} | Acc]);
service_chars([{struct, [{"value", K}, {"name", "serviceAkaK"}]} | T], Acc) ->
	service_chars(T, [{"serviceAkaK", K} | Acc]);
service_chars([{struct, [{"name", "serviceAkaOPc"}, {"value", OPc}]} | T], Acc) ->
	service_chars(T, [{"serviceAkaOPc", OPc} | Acc]);
service_chars([{struct, [{"value", OPc}, {"name", "serviceAkaOPc"}]} | T], Acc) ->
	service_chars(T, [{"serviceAkaOPc", OPc} | Acc]);
service_chars([{struct, [{"name", "radiusReserveTime"}, {"value", RadiusReserveTime}]} | T], Acc) ->
	service_chars(T, [{"radiusReserveTime", radius_reserve(RadiusReserveTime)} | Acc]);
service_chars([{struct, [{"value", RadiusReserveTime}, {"name", "radiusReserveTime"}]} | T], Acc) ->
	service_chars(T, [{"radiusReserveTime", radius_reserve(RadiusReserveTime)} | Acc]);
service_chars([{struct, [{"name", "radiusReserveOctets"}, {"value", RadiusReserveOctets}]} | T], Acc) ->
	service_chars(T, [{"radiusReserveOctets", radius_reserve(RadiusReserveOctets)} | Acc]);
service_chars([{struct, [{"value", RadiusReserveOctets} ,{"name", "radiusReserveOctets"}]} | T], Acc) ->
	service_chars(T, [{"radiusReserveOctets", radius_reserve(RadiusReserveOctets)} | Acc]);
service_chars([{struct, [{"name", Name}, {"value", Value}]} | T], Acc) ->
	service_chars(T, [{Name, Value} | Acc]);
service_chars([{struct, [{"value", Value}, {"name", Name}]} | T], Acc) ->
	service_chars(T, [{Name, Value} | Acc]);
service_chars([{"radiusReserveTime", Value} | T], Acc) ->
	Char = {struct, [{"name", "radiusReserveTime"},
			{"value", radius_reserve(Value)}]},
	service_chars(T, [Char | Acc]);
service_chars([{"radiusReserveOctets", Value} | T], Acc) ->
	Char = {struct, [{"name", "radiusReserveOctets"},
			{"value", radius_reserve(Value)}]},
	service_chars(T, [Char | Acc]);
service_chars([{Name, Value} | T], Acc) ->
	Char = {struct, [{"name", Name}, {"value", Value}]},
	service_chars(T, [Char | Acc]);
service_chars([], Acc) ->
	lists:reverse(Acc).

-spec radius_reserve(RadiusReserve) -> RadiusReserve
	when
		RadiusReserve :: {struct, list()} | [tuple()].
%% @doc CODEC for top up duration characteristic
radius_reserve({struct, [{"unitOfMeasure", "seconds"}, {"value", Value}]}) ->
	[{type, seconds}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "seconds"}]}) ->
	[{type, seconds}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "minutes"}, {"value", Value}]}) ->
	[{type, minutes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "minutes"}]}) ->
	[{type, minutes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "bytes"}, {"value", Value}]}) ->
	[{type, bytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "bytes"}]}) ->
	[{type, bytes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "kilobytes"}, {"value", Value}]}) ->
	[{type, kilobytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "kilobytes"}]}) ->
	[{type, kilobytes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "megabytes"}, {"value", Value}]}) ->
	[{type, megabytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "megabytes"}]}) ->
	[{type, megabytes}, {value, Value}];
radius_reserve({struct, [{"unitOfMeasure", "gigabytes"}, {"value", Value}]}) ->
	[{type, gigabytes}, {value, Value}];
radius_reserve({struct, [{"value", Value}, {"unitOfMeasure", "gigabytes"}]}) ->
	[{type, gigabytes}, {value, Value}];
%% @hidden
radius_reserve([{type, seconds}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "seconds"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, seconds}]) ->
	{struct, [{"unitOfMeasure", "seconds"}, {"value", Value}]};
radius_reserve([{type, minutes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "minutes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, minutes}]) ->
	{struct, [{"unitOfMeasure", "minutes"}, {"value", Value}]};
radius_reserve([{type, bytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "bytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, bytes}]) ->
	{struct, [{"unitOfMeasure", "bytes"}, {"value", Value}]};
radius_reserve([{type, kilobytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "kilobytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, kilobytes}]) ->
	{struct, [{"unitOfMeasure", "kilobytes"}, {"value", Value}]};
radius_reserve([{type, megabytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "megabytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, megabytes}]) ->
	{struct, [{"unitOfMeasure", "megabytes"}, {"value", Value}]};
radius_reserve([{type, gigabytes}, {value, Value}]) ->
	{struct, [{"unitOfMeasure", "gigabytes"}, {"value", Value}]};
radius_reserve([{value, Value}, {type, gigabytes}]) ->
	{struct, [{"unitOfMeasure", "gigabytes"}, {"value", Value}]}.

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
query_start({M, F, A}, Codec, Query, Filters, RangeStart, RangeEnd) ->
	case supervisor:start_child(ocs_rest_pagination_sup, [[M, F, A]]) of
		{ok, PageServer, Etag} ->
			query_page(Codec, PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
query_page(Codec, PageServer, Etag, [] = _Query, Filters, Start, End) ->
	{ok, Timeout} = application:get_env(ocs, rest_request_timeout),
	try gen_server:call(PageServer, {Start, End}, Timeout) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			ContentRange1 = case string:split(ContentRange, "/") of
				[Range, "*"] ->
					case erlang:fun_info(Codec, name) of
						{_, inventory} ->
							Size = mnesia:table_info(service, size),
							lists:concat([Range, "/",  Size]);
						_Other ->
							ContentRange
					end;
				_Other ->
					ContentRange
			end,
			JsonObj = query_page1(lists:map(Codec, Result), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange1}],
			{ok, Headers, Body}
	catch
		_:{timeout, _} ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Timeout calling the pagination server"},
			{error, 500, Problem};
		_:_Reason ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception caught while calling the pagination server"},
			{error, 500, Problem}
	end;
query_page(Codec, PageServer, Etag, _Query, Filters, Start, End) ->
	{ok, Timeout} = application:get_env(ocs, rest_request_timeout),
	try gen_server:call(PageServer, {Start, End}, Timeout) of
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
	catch
		_:{timeout, _} ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Timeout calling the pagination server"},
			{error, 500, Problem};
		_:_Reason ->
			Problem = #{type => "about:blank",
					title => "Internal Server Error",
					detail => "Exception caught while calling the pagination server"},
			{error, 500, Problem}
	end.
%% @hidden
query_page1(Json, [], []) ->
	Json;
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

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

%% @hidden
binary_to_hex(B) ->
	binary_to_hex(B, []).
%% @hidden
binary_to_hex(<<N:4, Rest/bits>>, Acc) when N >= 10 ->
	binary_to_hex(Rest, [N - 10 + $a | Acc]);
binary_to_hex(<<N:4, Rest/bits>>, Acc) ->
	binary_to_hex(Rest, [N + $0 | Acc]);
binary_to_hex(<<>>, Acc) ->
	lists:reverse(Acc).

