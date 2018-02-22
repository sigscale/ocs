%%% ocs_rest_res_service.erl
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
-module(ocs_rest_res_service).
-copyright('Copyright (c) 2016 - 2018 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([add_inventory/1]).
-export([get_service_specs/1, get_service_spec/2]).

-include_lib("inets/include/mod_auth.hrl").
-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(servicePath, "catalogManagement/v2/serivce").
-define(serviceSpecPath, "catalogManagement/v2/serviceSpecification").
-define(serviceInventoryPath, "serviceInventoryManagement/v2/serviceSpecification").

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

-spec add_inventory(ReqData) -> Result when
	ReqData	:: [tuple()],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 500.
%% @doc Respond to `POST /serviceInventoryManagement/v2/service'.
%% 	Add a new Service Inventory.
add_inventory(ReqData) ->
	try
		#service{name = Identity, password = Password,
			attributes = Attributes, product = ProductRef,
			enabled = Enabled, multisession = MultiSession} =
			service(mochijson:decode(ReqData)),
		case ocs:add_service(Identity, Password,
				ProductRef, Attributes, Enabled, MultiSession) of
			{ok, Service1} ->
				Service1;
			{error, Reason} ->
				throw(Reason)
		end
	of
		Service ->
			Body = mochijson:encode(service(Service)),
			Href = ?serviceInventoryPath ++ Service#service.name,
			Etag = ocs_rest:etag(Service#service.last_modified),
			Headers = [{location, Href}, {etag, Etag}],
			{ok, Headers, Body}
	catch
		throw:_ ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.

-spec get_service_specs(Query) -> Result when
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManegment/v2/serviceSpecification/'.
%% 	Retrieve all service specifications.
get_service_specs([] = _Query) ->
	Headers = [{content_type, "application/json"}],
	Object = {array, [default_spec()]},
	Body = mochijson:encode(Object),
	{ok, Headers, Body};
get_service_specs(_Query) ->
	{error, 400}.

-spec get_service_spec(Id, Query) -> Result when
	Id :: string(),
	Query :: [{Key :: string(), Value :: string()}],
	Result	:: {ok, Headers, Body} | {error, Status},
	Headers	:: [tuple()],
	Body		:: iolist(),
	Status	:: 400 | 404 | 500 .
%% @doc Respond to `GET /catalogManegment/v2/serviceSpecification/{id}'.
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
	default_spec();
service_spec(_) ->
	{error, 404}.

%% @hidden
default_spec() ->
	Id = {"id", "1"},
	Href = {"href", ?serviceSpecPath ++ "1"},
	Name = {"name", ""},
	Description = {"description", ""},
	Version = {"version", "1.0"},
	Status = {"lifecycleStatus", "Active"},
	LastUpdate = {"lastUpdate", "2018-02-22T00:00:00Z"},
	Type = {"type", ""},
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
	Value3 = {"serviceSpecCharacteristicValue", {array, [Type3]}},
	Char3 = {struct, [Name3, Description3, Config3, Type3, Value3]},
	Name4 = {"name", "sessionTimeout"},
	Description4 = {"description", "Number of seconds for one session"},
	Config4 = {"configurable", false},
	Type4 = {"valueType", "Number"},
	Value4 = {"serviceSpecCharacteristicValue", {array, [Type4]}},
	Char4 = {struct, [Name4, Description4, Config4, Type4, Value4]},
	Name5 = {"name", "multiSession"},
	Description5 = {"description", ""},
	Config5 = {"configurable", true},
	Type5 = {"valueType", "boolean"},
	Value5 = {"serviceSpecCharacteristicValue", {array, [{struct, [Type5]}]}},
	Char5 = {struct, [Name5, Description5, Config5, Type5, Value5]},
	[Char1, Char2, Char3, Char4, Char5].

-spec service(Service) -> Service
	when
		Service :: #service{} | {struct, list()}.
%% @doc CODEC for service inventory 
service({struct, Service}) ->
	service(Service, #service{});
service(#service{} = Service) ->
	service(record_info(fields, service), Service, [], []).
%% @hidden
service([{"category", _}| T], Acc) ->
	service(T, Acc);
service([{"description", _}| T], Acc) ->
	service(T, Acc);
service([{"endDate", _}| T], Acc) ->
	service(T, Acc);
service([{"hasStarted", _}| T], Acc) ->
	service(T, Acc);
service([{"href", _}| T], Acc) ->
	service(T, Acc);
service([{"id", Id}| T], Acc) ->
	service(T, Acc#service{name = list_to_binary(Id)});
service([{"isServiceEnabled", Enabled}| T], Acc) ->
	service(T, Acc#service{enabled = Enabled});
service([{"isStateful", _}| T], Acc) ->
	service(T, Acc);
service([{"name", _}| T], Acc) ->
	service(T, Acc);
service([{"orderDate", _}| T], Acc) ->
	service(T, Acc);
service([{"startDate", _}| T], Acc) ->
	service(T, Acc);
service([{"startMode", StartMode}| T], Acc) ->
	_Mode = start_mode(StartMode),
	service(T, Acc);
service([{"status", _}| T], Acc) ->
	service(T, Acc);
service([{"type", _}| T], Acc) ->
	service(T, Acc);
service([{"supportingResource", _}| T], Acc) ->
	service(T, Acc);
service([{"serviceRelationship", _}| T], Acc) ->
	service(T, Acc);
service([{"place", _}| T], Acc) ->
	service(T, Acc);
service([{"supportingService", _}| T], Acc) ->
	service(T, Acc);
service([{"serviceSpecification", _}| T], Acc) ->
	service(T, Acc);
service([{"relatedPary", {array, _}}| T], Acc) ->
	service(T, Acc);
service([{"serviceCharacteristic", Characteristics}| T], Acc) ->
	Chars = service_chars(Characteristics),
	F = fun(Key, Chars1) ->
			case lists:keyfind(Key, 1, Chars1) of
				{_, Value} ->
					Value;
				false ->
					undefined
			end
	end,
	Identity = F("serviceIdentity", Chars),
	Password = F("servicePassword", Chars),
	MultiSession = F("multiSession", Chars),
	A1 = case F("sessionTimeout", Chars) of
		undefined ->
			[];
		SessionTimeout ->
			[{?SessionTimeout, SessionTimeout}]
	end,
	A2  = case F("acctSessionInterval", Chars) of
		undefined ->
			A1;
		SessionInterval ->
			[{?AcctInterimInterval, SessionInterval} | A1]
	end,
	NewAcc = Acc#service{name = Identity, password = Password,
		multisession = MultiSession, attributes = A2},
	service(T, NewAcc);
service([], Acc) ->
	Acc.
%% @hidden
service([name | T], #service{name = undefined} = Service, Chars, Acc) ->
	service(T, Service, Chars, Acc);
service([name | T], #service{name = Identity} = Service,
		Chars, Acc) when is_binary(Identity) ->
	SId = {"serviceIdentity", binary_to_list(Identity)},
	NewChars = lists:keystore("serviceIdentity", 1, SId, Chars),
	service(T, Service, NewChars, Acc);
service([name | T], #service{name = Identity} = Service,
		Chars, Acc) when is_list(Identity) ->
	SId = {"serviceIdentity", Identity},
	NewChars = lists:keystore("serviceIdentity", 1, SId, Chars),
	service(T, Service, NewChars, Acc);
service([password | T], #service{name = undefined} = Service, Chars, Acc) ->
	service(T, Service, Chars, Acc);
service([password | T], #service{password = Password} = Service,
		Chars, Acc) when is_binary(Password) ->
	SPwd = {"servicePassword", binary_to_list(Password)},
	NewChars = lists:keystore("servicePassword", 1, SPwd, Chars),
	Id = {"id", binary_to_list(Password)},
	Href = {"href", ?serviceInventoryPath ++ binary_to_list(Password)},
	service(T, Service, NewChars, [Id, Href | Acc);
service([password | T], #service{password = Password} = Service,
		Chars, Acc) when is_list(Password) ->
	SPwd = {"serviceIdentity", Password},
	NewChars = lists:keystore("servicePassword", 1, SPwd, Chars),
	Id = {"id", binary_to_list(Password)},
	Href = {"href", ?serviceInventoryPath ++ Password},
	service(T, Service, NewChars, [Id, Href | Acc);
service([product | T], #service{} = Service, Chars, Acc) ->
	service(T, Service, Chars, Acc);
service([enabled | T], #service{enabled = Enabled} = Service, Chars, Acc) ->
	service(T, Service, Chars, [{"isServiceEnabled", Enabled} | Acc]);
service([multisession | T], #service{multisession = MultiSession} =
		Service, Chars, Acc) ->
	MS = {"multiSession", MultiSession},
	NewChars = lists:keystore("multiSession", 1, MS, Chars),
	service(T, Service, NewChars, Acc);
service([attributes | T], #service{attributes = []} = Service, Chars, Acc) ->
	service(T, Service, Chars, Acc);
service([attributes | T], #service{attributes = Attributes} = Service, Chars, Acc) ->
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
	service(T, Service, NewChars, Acc);
service([], _Service, Chars, Acc) ->
	NewAcc = [{"serviceCharacteristic", service_chars(Chars)} | Acc],
	{struct, lists:reverse(NewAcc)}.

-spec start_mode(Mode) -> Mode
	when
		Mode :: 0..5 | atom().
%% @doc Codec for Service start mode
start_mode(0) -> unknown;
start_mode(1) -> automatically_managed_environment;
start_mode(2) -> automatically_owning_device;
start_mode(3) -> manullay_provider_of_service;
start_mode(4) -> manullay_customer_of_service;
start_mode(5) -> any_of_the_above;
start_mode(unknown) -> 0;
start_mode(automatically_managed_environment) -> 1;
start_mode(automatically_owning_device) -> 2;
start_mode(manullay_provider_of_service) -> 3;
start_mode(manullay_customer_of_service) -> 4;
start_mode(any_of_the_above) -> 5.

-spec service_chars(ServiceChars) -> ServiceChars
	when
		ServiceChars :: [tuple()] | {array, list()}.
%% @doc CODEC for service charateristics
service_chars({array, L}) ->
	service_chars(L, []);
service_chars(Chars) when is_list(Chars) ->
	{array, service_chars(Chars, [])}.
%% @hidden
service_chars([{struct, [{"name", Name}, {"value", Value}]} | T], Acc) ->
	service_chars(T, [{Name, Value} | Acc]);
service_chars([{struct, [{"value", Value}, {"name", Name}]} | T], Acc) ->
	service_chars(T, [{Name, Value} | Acc]);
service_chars([{Name, Value} | T], Acc) ->
	Char = {struct, [{"name", Name}, {"value", Value}]},
	service_chars(T, [Char | Acc]);
service_chars([], Acc) ->
	lists:reverse(Acc).

