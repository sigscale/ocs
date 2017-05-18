%%% ocs_rest_res_accounting.erl
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
-module(ocs_rest_res_accounting).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_accepted/0,
            content_types_provided/0,
            perform_get_all/0]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	[].

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
%% @doc Body producing function for `GET /ocs/v1/log/accounting'
%% requests.
perform_get_all() ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	case ocs_log:last(ocs_acct, MaxItems) of
		{error, _} -> 
			{error, 404};
		{NewCount, Events} -> 
			JsonObj = radius_auth_json(Events),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			ContentRange = "items 1-" ++ integer_to_list(NewCount) ++ "/*",
			Headers = [{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

% @hidden
radius_auth_json(Events) ->
	F = fun({Milliseconds, _Proto, Node, Client, Server, Type, Attr}, Acc) ->
					TimeStamp = ocs_log:iso8601(Milliseconds),
					{ClientAdd, ClientPort} = Client,
					ClientIp = inet:ntoa(ClientAdd),
					{ServerAdd, ServerPort} = Server,
					ServerIp = inet:ntoa(ServerAdd),
					Username = radius_attributes:fetch(?UserName, Attr),
					JsonObj = {struct, [{"timeStamp", TimeStamp}, {"node", Node},
							{"clientAddress", ClientIp}, {"clientPort", ClientPort},
							{"serverAddress", ServerIp}, {"serverPort", ServerPort},
							{"type", Type}, {"username", Username}]},
					[JsonObj | Acc];
		(_, Acc) ->
			%% TODO support for DIAMETER
			Acc
	end,
	lists:foldl(F, [], Events).

