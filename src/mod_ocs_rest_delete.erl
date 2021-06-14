%%% mod_ocs_rest_delete.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
-module(mod_ocs_rest_delete).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-spec do(ModData) -> Result when
	ModData :: #mod{},
	Result :: {proceed, OldData} | {proceed, NewData} | {break, NewData} | done,
	OldData :: list(),
	NewData :: [{response,{StatusCode,Body}}] | [{response,{response,Head,Body}}]
			| [{response,{already_sent,StatusCode,Size}}],
	StatusCode :: integer(),
	Body :: iolist() | nobody | {Fun, Arg},
	Head :: [HeaderOption],
	HeaderOption :: {Option, Value} | {code, StatusCode},
	Option :: accept_ranges | allow
			| cache_control | content_MD5
			| content_encoding | content_language
			| content_length | content_location
			| content_range | content_type | date
			| etag | expires | last_modified
			| location | pragma | retry_after
			| server | trailer | transfer_encoding,
	Value :: string(),
	Size :: term(),
	Fun :: fun((Arg) -> sent| close | Body),
	Arg :: [term()].
%% @doc Erlang web server API callback function.
do(#mod{method = Method, request_uri = Uri, data = Data} = ModData) ->
	case Method of
		"DELETE" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							{_, Resource} = lists:keyfind(resource, 1, Data),
							Path = http_uri:decode(Uri),
							do_delete(Resource, ModData, string:tokens(Path, "/"));
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
do_delete(Resource, ModData, ["ocs", "v1", "client", Identity]) ->
	do_response(ModData, Resource:delete_client(Identity));
do_delete(Resource, ModData, ["ocs", "v1", "subscriber", Identity]) ->
	do_response(ModData, Resource:delete_service(Identity));
do_delete(Resource, ModData, ["catalogManagement", "v2", "productOffering", Identity]) ->
	do_response(ModData, Resource:delete_offer(Identity));
do_delete(Resource, ModData, ["catalogManagement", "v2", "pla", Identity]) ->
	do_response(ModData, Resource:delete_pla(Identity));
do_delete(Resource, ModData, ["partyManagement", "v1", "individual", Identity]) ->
	do_response(ModData, Resource:delete_user(Identity));
do_delete(Resource, ModData, ["partyManagement", "v1", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub(Identity));
do_delete(Resource, ModData, ["productInventoryManagement", "v2", "product", Identity]) ->
	do_response(ModData, Resource:delete_inventory(Identity));
do_delete(Resource, ModData, ["productInventory", "v2", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub(Identity));
do_delete(Resource, ModData, ["serviceInventoryManagement", "v2", "service", Identity]) ->
	do_response(ModData, Resource:delete_inventory(Identity));
do_delete(Resource, ModData, ["serviceInventory", "v2", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub(Identity));
do_delete(Resource, ModData, ["resourceInventoryManagement", "v1", "resource", Identity]) ->
	do_response(ModData, Resource:delete_resource(Identity));
do_delete(Resource, ModData, ["resourceInventory", "v1", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub(Identity));
do_delete(Resource, ModData, ["balanceManagement", "v1", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub(Identity));
do_delete(Resource, ModData, ["balanceManagement", "v1", "bucket", Identity]) ->
	do_response(ModData, Resource:delete_bucket(Identity));
do_delete(Resource, ModData, ["productCatalog", "v2", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub_catalog(Identity));
do_delete(Resource, ModData, ["productCatalogManagement", "v2", "productOffering", Identity]) ->
	do_response(ModData, Resource:delete_offer(Identity));
do_delete(Resource, ModData, ["usageManagement", "v1", "hub", Identity]) ->
	do_response(ModData, Resource:delete_hub(Identity));
do_delete(_Resource, #mod{data = Data} = _ModData, _) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{proceed, [{response, {404, Response}} | Data]}.

%% @hidden
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	NewHeaders = Headers ++ [{content_length, Size}],
	send(ModData, 204, NewHeaders, ResponseBody),
	{proceed, [{response,{already_sent, 204, Size}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 202}) ->
	Response = "<h2>HTTP Error 202 - Accepted</h2>",
	{proceed, [{response, {202, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 400}) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	{proceed, [{response, {400, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 403}) ->
	Response = "<h2>HTTP Error 403 - Forbidden</h2>",
	{proceed, [{response, {403, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 500}) ->
	Response = "<h2>HTTP Error 500 - Server Error</h2>",
	{proceed, [{response, {500, Response}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = ModData,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(ModData, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

