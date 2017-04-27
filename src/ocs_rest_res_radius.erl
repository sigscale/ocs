%%% ocs_rest_res_radius.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
%%%   for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_radius).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_accepted/0,
            content_types_provided/0,
            perform_get_all/0]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").

-define(RADAUTH, radius_auth).

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
	["application/json"].

-spec perform_get_all() -> Result
	when
		Result :: {ok, Headers :: [string()],
				Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/log/access'
%% requests.
perform_get_all() ->
	Log = ?RADAUTH,
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	read_auth_log(Log, start, MaxItems, []).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
read_auth_log(Log, Cont, MaxItems, Acc) when length(Acc) < MaxItems ->
	AccLen = length(Acc),
	case disk_log:chunk(Log, Cont) of
		eof ->
			read_auth_log1(MaxItems, Acc);
		{_Cont1, Events} when (length(Events) + AccLen) >= MaxItems ->
			{NewEvents, _} = lists:split(MaxItems - AccLen, Events),
			NewAcc = [radius_auth_json(NewEvents) | Acc],
			read_auth_log1(MaxItems, NewAcc);
		{Cont1, Events} ->
			NewAcc = [radius_auth_json(Events) | Acc],
			read_auth_log(Log, Cont1, MaxItems, NewAcc)
	end.
%% @hidden
read_auth_log1(MaxItems, Acc) -> 
	JsonArray = {array, lists:flatten(lists:reverse(Acc))},
	Body = mochijson:encode(JsonArray),
	ContentRange = "items 1-" ++ integer_to_list(MaxItems) ++ "/*",
	Headers = [{content_range, ContentRange}],
	{ok, Headers, Body}.

% @hidden
radius_auth_json(Events) ->
	F = fun({TimeStamp, Node, Client, Server, Type, ReqAttrs, _RespAttrs}, Acc) ->
		{ClientAdd, ClientPort} = Client,
		ClientIp = inet:ntoa(ClientAdd),
		{ServerAdd, ServerPort} = Server,
		ServerIp = inet:ntoa(ServerAdd),
		Username = radius_attributes:fetch(?UserName, ReqAttrs),
		JsonObj = {struct, [{"timeStamp", TimeStamp}, {"node", Node}, 
				{"clientAddress", ClientIp}, {"clientPort", ClientPort}, 
				{"serverAddress", ServerIp}, {"serverPort", ServerPort}, 
				{"type", Type}, {"username", Username}]},
		[JsonObj | Acc]
	end,
	lists:foldl(F, [], Events).

