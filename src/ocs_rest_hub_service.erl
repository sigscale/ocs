%%% ocs_rest_hub_service.erl
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
-module(ocs_rest_hub_service).
-copyright('Copyright (c) 2020 SigScale Global Inc.').

-include("ocs.hrl").

-export([content_types_accepted/0, content_types_provided/0, post_hub/1,
		delete_hub/1]).
-export([hub/1]).

-define(PathServiceHub, "/serviceInventory/v2/hub/").

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
%% @doc Delete by id.
delete_hub(Id) ->
	{gen_fsm:send_all_state_event({global, Id}, shutdown), [], []}.

-spec post_hub(ReqBody) -> Result
	when
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Hub event to disk.
post_hub(ReqBody) ->
	try
		case hub(mochijson:decode(ReqBody)) of
			#hub{callback = Callback, query = undefined} = HubRecord ->
				case supervisor:start_child(ocs_rest_hub_sup, [null, Callback]) of
					{ok, _PageServer, Id} ->
						Body = mochijson:encode(hub(HubRecord#hub{id = Id})),
						Headers = [{content_type, "application/json"},
								{location, ?PathServiceHub ++ Id}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 500}
				end;
			#hub{callback = Callback, query = Query} = HubRecord ->
				case supervisor:start_child(ocs_rest_hub_sup, [Query, Callback]) of
					{ok, _PageServer, Id} ->
						Body = mochijson:encode(hub(HubRecord#hub{id = Id})),
						Headers = [{content_type, "application/json"},
								{location, ?PathServiceHub ++ Id}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 500}
				end
		end
	catch
		_:_ ->
			{error, 400}
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
hub([query | T], #hub{query = undefined} = H, Acc) ->
	hub(T, H, [{"query", null} | Acc]);
hub([query | T], #hub{query = Query} = H, Acc) when is_list(Query) ->
	hub(T, H, [{"query", Query} | Acc]);
hub([_ | T], H, Acc) ->
	hub(T, H, Acc);
hub([], _H, Acc) ->
	{struct, lists:reverse(Acc)}.

