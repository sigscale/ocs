%%% mod_ocs_rest_post.erl
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
-module(mod_ocs_rest_post).
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
do(#mod{method = Method, parsed_header = Headers, request_uri = Uri,
		entity_body = Body, data = Data} = ModData) ->
	case Method of
		"POST" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							{_, Resource} = lists:keyfind(resource, 1, Data),
							Path = http_uri:decode(Uri),
							content_type_available(Headers, Path, Body, Resource, ModData);
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
content_type_available(Headers, Uri, Body,
		Resource, #mod{data = Data} = ModData) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, RequestingType} ->
			AvailableTypes = Resource:content_types_provided(),
			case lists:member(RequestingType, AvailableTypes) of
				true ->
					do_post(Resource, ModData, Body, string:tokens(Uri, "/"));
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{proceed, [{response, {415, Response}} | Data]}
			end;
		_ ->
			do_post(Resource, ModData, Body, string:tokens(Uri, "/"))
	end.

%% @hidden
do_post(Resource, ModData, Body, ["ocs", "v1", "client"]) ->
	do_response(ModData, Resource:post_client(Body));
do_post(Resource, ModData, Body, ["ocs", "v1", "subscriber"]) ->
	do_response(ModData, Resource:post_subscriber(Body));
do_post(Resource, ModData, Body, ["partyManagement", "v1", "individual"]) ->
	do_response(ModData, Resource:post_user(Body));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["partyManagement", "v1", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub(Body, Authorization));
do_post(Resource, ModData, Body, ["balanceManagement", "v1", "product", Id, "balanceTopup"]) ->
	do_response(ModData, Resource:top_up(Id, Body));
do_post(Resource, ModData, Body, ["balanceManagement", "v1", "service", Id, "balanceTopup"]) ->
	do_response(ModData, Resource:top_up_service(Id, Body));
do_post(Resource, ModData, Body, ["balanceManagement", "v1", "balanceAdjustment"]) ->
	do_response(ModData, Resource:balance_adjustment(Body));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["balanceManagement", "v1", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub(Body, Authorization));
do_post(Resource, ModData, Body, ["catalogManagement", "v2", "productOffering"]) ->
	do_response(ModData, Resource:add_offer(Body));
do_post(Resource, ModData, Body, ["productCatalogManagement", "v2", "productOffering"]) ->
	do_response(ModData, Resource:add_offer(Body));
do_post(Resource, ModData, Body, ["productCatalogManagement", "v2", "syncOffer"]) ->
	do_response(ModData, Resource:sync_offer(Body));
do_post(Resource, ModData, Body, ["productInventoryManagement", "v2", "product"]) ->
	do_response(ModData, Resource:add_inventory(Body));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["productInventory", "v2", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub(Body, Authorization));
do_post(Resource, ModData, Body, ["serviceInventoryManagement", "v2", "service"]) ->
	do_response(ModData, Resource:add_inventory(Body));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["serviceInventory", "v2", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub(Body, Authorization));
do_post(Resource, ModData, Body, ["catalogManagement", "v2", "pla"]) ->
	do_response(ModData, Resource:add_pla(Body));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["productCatalog", "v2", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub_catalog(Body, Authorization));
do_post(Resource, ModData, Body, ["resourceInventoryManagement", "v1", "resource"]) ->
	do_response(ModData, Resource:add_resource(Body));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["resourceInventory", "v1", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub(Body, Authorization));
do_post(Resource, #mod{parsed_header = Headers} = ModData, Body,
		["usageManagement", "v1", "hub"]) ->
	{_, Authorization} = lists:keyfind("authorization", 1, Headers),
	do_response(ModData, Resource:post_hub(Body, Authorization));
do_post(Resource, ModData, Body, ["nrf-rating", "v1", "ratingdata"]) ->
	do_response(ModData, Resource:initial_nrf(Body));
do_post(Resource, ModData, Body, ["nrf-rating", "v1", "ratingdata", RatingDataRef, "update"]) ->
	do_response(ModData, Resource:update_nrf(RatingDataRef, Body));
do_post(Resource, ModData, Body, ["nrf-rating", "v1", "ratingdata", RatingDataRef, "release"]) ->
	do_response(ModData, Resource:release_nrf(RatingDataRef, Body)).

%% @hidden
do_response(#mod{data = Data} = ModData, {ok, [] = Headers,
		[] = ResponseBody}) ->
	NewHeaders = Headers ++ [{content_length, "0"}],
	send(ModData, 204, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 204, "0"}} | Data]};
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	Accept = proplists:get_value(accept, Data),
	NewHeaders = Headers ++ [{content_length, Size}, {content_type, Accept}],
	send(ModData, 201, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 201, Size}} | Data]};
do_response(#mod{data = Data} = ModData, {200, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	Accept = proplists:get_value(accept, Data),
	NewHeaders = Headers ++ [{content_length, Size}, {content_type, Accept}],
	send(ModData, 200, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 201, Size}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 400}) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	{proceed, [{response, {400, Response}} | Data]};
do_response(#mod{data = Data} = ModData, {error, 400, ResponseBody}) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers = [{content_length, Size},
			{content_type, "application/problem+json"}],
	send(ModData, 400, Headers, ResponseBody),
	{proceed, [{response, {400, Response}} | Data]};
do_response(#mod{data = Data} = ModData, {error, 403, ResponseBody}) ->
	Response = "<h2>HTTP Error 403 - Forbidden</h2>",
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers = [{content_length, Size},
			{content_type, "application/problem+json"}],
	send(ModData, 403, Headers, ResponseBody),
	{proceed, [{response, {403, Response}} | Data]};
do_response(#mod{data = Data} = ModData, {error, 404, ResponseBody}) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers = [{content_length, Size},
			{content_type, "application/problem+json"}],
	send(ModData, 404, Headers, ResponseBody),
	{proceed, [{response, {404, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 404}) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{proceed, [{response, {404, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 500}) ->
	Response = "<h2>HTTP Error 500 - Server Error</h2>",
	{proceed, [{response, {500, Response}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = Info,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(Info, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

