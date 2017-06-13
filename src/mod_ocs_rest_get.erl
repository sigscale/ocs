%%% mod_ocs_rest_get.erl
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
-module(mod_ocs_rest_get).
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
	Body :: list() | nobody | {Fun, Arg},
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
				data = Data} = ModData) ->
	case Method of
		"GET" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							{_, Resource} = lists:keyfind(resource, 1, Data),
							content_type_available(Headers, Uri, Resource, ModData);
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
content_type_available(Headers, Uri, Resource, ModData) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, RequestingType} ->
			AvailableTypes = Resource:content_types_provided(),
			case lists:member(RequestingType, AvailableTypes) of
				true ->
					parse_query(Resource, ModData, string:tokens(Uri, "?"));
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{break, [{response, {415, Response}}]}
			end;
		_ ->
			parse_query(Resource, ModData, string:tokens(Uri, "?"))
	end.

%% @hidden
parse_query(Resource, ModData, [Path, Query]) ->
	parse_query1(Resource, ModData, Path, string:tokens(Query, "&"), []);
parse_query(Resource, ModData, [Path]) ->
	do_get(Resource, ModData, string:tokens(Path, "/"), []);
parse_query(_, _, _) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{break, [{response, {404, Response}}]}.
%% @hidden
parse_query1(Resource, ModData, Path, [H | T], Acc) ->
	parse_query2(Resource, ModData, Path, T, string:tokens(H, "="), Acc);
parse_query1(Resource, ModData, Path, [], Acc) ->
	do_get(Resource, ModData, string:tokens(Path, "/"), lists:reverse(Acc)).
%% @hidden
parse_query2(Resource, ModData, Path, QueryList, [Key, Val], Acc) ->
	parse_query1(Resource, ModData, Path, QueryList, [{Key, Val} | Acc]);
parse_query2(_, _, _, _, _, _) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	{break, [{response, {400, Response}}]}.

%% @hidden
do_get(Resource, ModData, ["ocs", "v1", "client"], []) ->
	do_response(ModData, Resource:get_client());
do_get(Resource, ModData, ["ocs", "v1", "client", Id], []) ->
	do_response(ModData, Resource:get_client(Id));
do_get(Resource, ModData, ["ocs", "v1", "subscriber"], []) ->
	do_response(ModData, Resource:get_subscriber());
do_get(Resource, ModData, ["ocs", "v1", "subscriber", Id], []) ->
	do_response(ModData, Resource:get_subscriber(Id));
do_get(Resource, ModData, ["usageManagement", "v1", "usage"], Query) ->
	do_response(ModData, Resource:get_usage(Query));
do_get(Resource, ModData,
		["usageManagement", "v1", "usageSpecification"], []) ->
	do_response(ModData, Resource:get_usagespec());
do_get(Resource, ModData,
		["usageManagement", "v1", "usageSpecification", Id], []) ->
	do_response(ModData, Resource:get_usagespec(Id));
do_get(Resource, ModData, ["ocs", "v1", "log", "access"], []) ->
	do_response(ModData, Resource:get_access([]));
do_get(Resource, ModData, ["ocs", "v1", "log", "accounting"], []) ->
	do_response(ModData, Resource:get_accounting());
do_get(Resource, ModData, ["ocs", "v1", "log", "http"], []) ->
	do_response(ModData, Resource:get_http());
do_get(_, _, _, _) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{break, [{response, {404, Response}}]}.

%% @hidden
do_response(ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	NewHeaders = Headers ++ [{content_length, Size}],
	send(ModData, 200, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 200, Size}}]};
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
send(#mod{socket = Socket, socket_type = SocketType} = ModData,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(ModData, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

