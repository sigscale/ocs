%%% ocs_rest_res_hub_role.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022 - 2024 SigScale Global Inc.
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
-module(ocs_rest_res_hub_role).
-copyright('Copyright (c) 2022 - 2024 SigScale Global Inc.').

-include("ocs.hrl").

-export([content_types_accepted/0, content_types_provided/0, post_hub/1,
		delete_hub/1, get_hubs/0, get_hub/1]).
-export([hub/1]).

-define(PathRoleHub, "/partyRoleManagement/v4/hub/").

%%----------------------------------------------------------------------
%%  The hub public API
%%----------------------------------------------------------------------

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
	["application/json", "application/problem+json"].

-spec post_hub(ReqBody) -> Result
	when
		ReqBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% Create hub listener for role.
%% @doc Respond to `POST /partyRoleManagement/v4/hub'
post_hub(ReqBody) ->
	try
		case hub(mochijson:decode(ReqBody)) of
			#hub{callback = Callback, query = undefined} = HubRecord ->
				post_hub1(supervisor:start_child(ocs_rest_hub_sup,
						[[], Callback, ?PathRoleHub]), HubRecord);
			#hub{callback = Callback,
					query = Query} = HubRecord when is_list(Query) ->
				post_hub1(supervisor:start_child(ocs_rest_hub_sup,
						[Query, Callback, ?PathRoleHub]), HubRecord)
		end
	catch
		_:500 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.
post_hub1({ok, _HubFsm, Id}, HubRecord) ->
	Href = ?PathRoleHub ++ Id,
	Body = mochijson:encode(hub(HubRecord#hub{id = Id, href = Href})),
	Headers = [{content_type, "application/json"}, {location, Href}],
	{ok, Headers, Body};
post_hub1({error, _Reason}, _HubRecord) ->
	{error, 500}.

-spec delete_hub(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% Delete by id.
%% @doc Respond to `POST /partyRoleManagement/v4/hub/{id}'
delete_hub(Id) ->
	try
		gen_fsm:sync_send_all_state_event({global, Id}, delete)
	of
		ok ->
			{ok, [], []}
	catch
		_:_ ->
			{error, 500}
	end.

-spec get_hubs() -> Result
	when
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%% 	`GET|HEAD /partyRoleManagement/v4/hub/'
get_hubs() ->
	get_hubs(supervisor:which_children(ocs_rest_hub_sup), []).
%% @hidden
get_hubs([{_, Pid, _, _} | T], Acc) when is_pid(Pid) ->
	try
		gen_fsm:sync_send_all_state_event(Pid, get)
	of
		#hub{href = ?PathRoleHub ++ _} = Hub ->
			get_hubs(T, [Hub | Acc]);
		_Hub ->
			get_hubs(T, Acc)
	catch
		_:_Reason ->
			get_hubs(T, Acc)
	end;
get_hubs([], Acc) ->
	Body = mochijson:encode({array, [hub(Hub) || Hub <- Acc]}),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body}.

-spec get_hub(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%% 	`GET|HEAD /partyRoleManagement/v4/hub/{id}'
get_hub(Id) ->
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
hub([query | T], #hub{query = Query} = H, Acc)
		when is_list(Query), length(Query) > 0 ->
	hub(T, H, [{"query", Query} | Acc]);
hub([_ | T], H, Acc) ->
	hub(T, H, Acc);
hub([], _H, Acc) ->
	{struct, lists:reverse(Acc)}.

