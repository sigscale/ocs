%%% mod_ocs_rest_post.erl
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
%%%
-module(mod_ocs_rest_post).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

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
							content_type_available(Headers, Uri, Body, Resource, ModData);
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
content_type_available(Headers, Uri, Body, Resource, ModData) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, RequestingType} ->
			AvailableTypes = Resource:content_types_provided(),
			case lists:member(RequestingType, AvailableTypes) of
				true ->
					do_post(Resource, ModData, Body, string:tokens(Uri, "/"));
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{break, [{response, {415, Response}}]}
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
do_post(Resource, ModData, Body, ["balanceManagement", "v1", Id, "balanceTopups"]) ->
	do_response(ModData, Resource:top_up(Id, Body));
do_post(Resource, ModData, Body, ["catalogManagement", "v1", "productOffering"]) ->
	do_response(ModData, Resource:add_product_CatMgmt(Body)).

%% @hidden
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	Accept = proplists:get_value(accept, Data),
	NewHeaders = Headers ++ [{content_length, Size}, {content_type, Accept}],
	send(ModData, 201, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent,201, Size}}]};
do_response(_ModData, {error, 400}) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	{break, [{response, {400, Response}}]};
do_response(_ModData, {error, 404}) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{break, [{response, {404, Response}}]};
do_response(_ModData, {error, 500}) ->
	Response = "<h2>HTTP Error 500 - Server Error</h2>",
	{break, [{response, {500, Response}}]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = Info,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(Info, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

