%%% ocs_rest_hub_product.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 SigScale Global Inc.
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
%%%
-module(ocs_rest_hub_product).
-copyright('Copyright (c) 2020 SigScale Global Inc.').

-include("ocs.hrl").

-export([content_types_accepted/0, content_types_provided/0, post_hub/2,
		delete_hub/1, get_product_hubs/0, get_product_hub/1,
		post_hub_catalog/2, delete_hub_catalog/1, get_catalog_hubs/0]).
-export([hub/1]).

-define(PathProductHub, "/productInventory/v2/hub/").
-define(PathCatalogHub, "/productCatalog/v2/hub/").

%%----------------------------------------------------------------------
%%  The hub public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/merge-patch+json",
	"application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec delete_hub(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% Delete by id.
%% @doc Respond to `POST /productInventory/v2/hub/{id}'
delete_hub(Id) ->
	{gen_fsm:send_all_state_event({global, Id}, shutdown), [], []}.

-spec post_hub(ReqBody, Authorization) -> Result
	when
		ReqBody :: list(),
		Authorization :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% Hub event to disk.
%% @doc Respond to `POST /productInventory/v2/hub/'
post_hub(ReqBody, Authorization) ->
	try
		case hub(mochijson:decode(ReqBody)) of
			#hub{callback = Callback, query = undefined} = HubRecord ->
				case supervisor:start_child(ocs_rest_hub_sup,
						[[], Callback, ?PathProductHub, Authorization]) of
					{ok, _PageServer, Id} ->
						Body = mochijson:encode(hub(HubRecord#hub{id = Id})),
						Headers = [{content_type, "application/json"},
								{location, ?PathProductHub ++ Id}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 500}
				end;
			#hub{callback = Callback, query = Query} = HubRecord ->
				case supervisor:start_child(ocs_rest_hub_sup,
						[Query, Callback, ?PathProductHub, Authorization]) of
					{ok, _PageServer, Id} ->
						Body = mochijson:encode(hub(HubRecord#hub{id = Id})),
						Headers = [{content_type, "application/json"},
								{location, ?PathProductHub ++ Id}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 500}
				end
		end
	catch
		_:_ ->
			{error, 400}
	end.

-spec get_product_hubs() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /productInventory/v2/hub/'
get_product_hubs() ->
	get_product_hubs(supervisor:which_children(ocs_rest_hub_sup), []).
%% @hidden
get_product_hubs([{_, Pid, _, _} | T], Acc) ->
	case gen_fsm:sync_send_all_state_event(Pid, get) of
		#hub{href = ?PathProductHub ++ _} = Hub ->
			get_product_hubs(T, [Hub | Acc]);
		_Hub ->
			get_product_hubs(T, Acc)
	end;
get_product_hubs([], Acc) ->
	Body = mochijson:encode({array, [hub(Hub) || Hub <- Acc]}),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body}.

-spec get_product_hub(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /productInventory/v2/hub/{id}'
%% 	requests.
get_product_hub(Id) ->
	case global:whereis_name(Id) of
		Fsm when is_pid(Fsm) ->
			case gen_fsm:sync_send_all_state_event(Fsm, get) of
				#hub{id = Id} = Hub ->
					Body = mochijson:encode(hub(Hub)),
					Headers = [{content_type, "application/json"}],
					{ok, Headers, Body};
				_ ->
					{error, 404}
			end;
		undefined ->
			{error, 404}
	end.

-spec delete_hub_catalog(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% Delete by id.
%% @doc Respond to `POST /productCatalog/v2/hub/{id}'
delete_hub_catalog(Id) ->
	{gen_fsm:send_all_state_event({global, Id}, shutdown), [], []}.

-spec post_hub_catalog(ReqBody, Authorization) -> Result
	when
		ReqBody :: list(),
		Authorization :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% Hub event to disk.
%% @doc Respond to `POST /productCatalog/v2/hub'
post_hub_catalog(ReqBody, Authorization) ->
	try
		case hub(mochijson:decode(ReqBody)) of
			#hub{callback = Callback, query = undefined} = HubRecord ->
				case supervisor:start_child(ocs_rest_hub_sup,
						[[], Callback, Authorization]) of
					{ok, _PageServer, Id} ->
						Body = mochijson:encode(hub(HubRecord#hub{id = Id})),
						Headers = [{content_type, "application/json"},
								{location, ?PathCatalogHub ++ Id}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 500}
				end;
			#hub{callback = Callback, query = Query} = HubRecord ->
				case supervisor:start_child(ocs_rest_hub_sup,
						[Query, Callback, Authorization]) of
					{ok, _PageServer, Id} ->
						Body = mochijson:encode(hub(HubRecord#hub{id = Id})),
						Headers = [{content_type, "application/json"},
								{location, ?PathCatalogHub ++ Id}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 500}
				end
		end
	catch
		_:_ ->
			{error, 400}
	end.

-spec get_catalog_hubs() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /productCatalog/v2/hub/'
get_catalog_hubs() ->
	get_catalog_hubs(supervisor:which_children(ocs_rest_hub_sup), []).
%% @hidden
get_catalog_hubs([{_, Pid, _, _} | T], Acc) ->
	case gen_fsm:sync_send_all_state_event(Pid, get) of
		#hub{href = ?PathCatalogHub ++ _} = Hub ->
			get_catalog_hubs(T, [Hub | Acc]);
		_Hub ->
			get_catalog_hubs(T, Acc)
	end;
get_catalog_hubs([], Acc) ->
	Body = mochijson:encode({array, [hub(Hub) || Hub <- Acc]}),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body}.
	
%%----------------------------------------------------------------------
%%  The internal functions
%%----------------------------------------------------------------------

-spec hub(Hub) -> Hub
	when
		Hub :: #hub{} | #{}.
%% @doc CODEC for hub.
hub({struct, Object}) when is_list(Object) ->
	hub(Object, #hub{});
hub(#hub{} = Hub) ->
	hub(record_info(fields, hub), Hub, []).
%% @hidden
hub([{"callback", Callback} | T], Hub) when is_list(Callback) ->
	hub(T, Hub#hub{callback = Callback});
hub([{"query", Query} | T], Hub) when is_list(Query) ->
	hub(T, Hub#hub{query = Query});
hub([_ | T], Hub) ->
	hub(T, Hub);
hub([], Hub) ->
	Hub.
%% @hidden
hub([callback | T], #hub{callback = Callback} = H, Acc)
		when is_list(Callback) ->
	hub(T, H, [{"callback", Callback} | Acc]);
hub([id | T], #hub{id = Id} = H, Acc) when is_list(Id) ->
	hub(T, H, [{"id", Id} | Acc]);
hub([href | T], #hub{href = Href} = H, Acc) when is_list(Href) ->
	hub(T, H, [{"href", Href} | Acc]);
hub([query | T], #hub{query = Query} = H, Acc) when length(Query) > 0 ->
	hub(T, H, [{"query", Query} | Acc]);
hub([_ | T], H, Acc) ->
	hub(T, H, Acc);
hub([], _H, Acc) ->
	{struct, lists:reverse(Acc)}.

