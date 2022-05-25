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
%%% @doc Handle received HTTP POST requests.
%%%
%%% 	This is an {@link //inets/httpd. httpd} callback module handling
%%% 	HTTP POST operations. The HTTP resources are managed in modules named
%%% 	`ocs_rest_res_*'.
%%%
%%% 	<h2><a name="callbacks">Resource Handler Functions</a></h2>
%%% 	The resource handler modules should implement callback functions
%%% 	in the pattern described in the example below.
%%%
%%% 	<h3 class="function">
%%% 		<a>post_&lt;Collection&gt;/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>post_&lt;Collection&gt;([Id], RequestBody, [...]) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Id = string()</tt></li>
%%% 			<li><tt>RequestBody = string()</tt></li>
%%% 			<li><tt>Result = {ok, Headers, ResponseBody}
%%% 					| {error, StatusCode}
%%% 					| {error, StatusCode, Problem}</tt></li>
%%% 			<li><tt>ResponseBody = io_list()</tt></li>
%%% 			<li><tt>StatusCode = 200..599</tt></li>
%%% 			<li><tt>Problem = #{type := uri(), title := string(),
%%% 					code := string(), cause => string(), detail => string(),
%%% 					invalidParams => [#{param := string(), reason => string()}],
%%% 					status => 200..599}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Resource handlers for HTTP POST operations on REST Collections.
%%%
%%% 	Response `Headers' must include `content_type' if `ResponseBody' is
%%% 	not en empty list. An optional `Problem' report may be provided in
%%% 	error responses which shall be formatted by
%%% 	{@link //ocs/ocs_rest:format_problem/2. format_problem/2} and included
%%% 	in the response body.
%%%
%%% @end
%%%
-module(mod_ocs_rest_post).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE > 23).
		-define(URI_DECODE(URI), uri_string:percent_decode(URI)).
	-else.
		-define(URI_DECODE(URI), http_uri:decode(URI)).
	-endif.
-else.
	-define(URI_DECODE(URI), http_uri:decode(URI)).
-endif.

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
do(#mod{method = Method, request_uri = Uri,
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
							Path = ?URI_DECODE(Uri),
							do_post(Resource, ModData, Body, string:tokens(Path, "/"));
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
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
do_post(Resource, ModData, Body, ["partyRoleManagement", "v4", "partyRole"]) ->
	do_response(ModData, Resource:post_role(Body));
do_post(Resource, ModData, Body, ["partyRoleManagement", "v4", "hub"]) ->
	do_response(ModData, Resource:post_hub(Body));
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
	do_response(ModData, Resource:initial_nrf(ModData, Body));
do_post(Resource, ModData, Body, ["nrf-rating", "v1", "ratingdata", RatingDataRef, "update"]) ->
	do_response(ModData, Resource:update_nrf(ModData, RatingDataRef, Body));
do_post(Resource, ModData, Body, ["nrf-rating", "v1", "ratingdata", RatingDataRef, "release"]) ->
	do_response(ModData, Resource:release_nrf(ModData, RatingDataRef, Body)).

%% @hidden
do_response(#mod{data = Data} = ModData,
		{ok, Headers, [] = ResponseBody}) ->
	ResponseHeaders = [{content_length, "0"} | Headers],
	send(ModData, 204, ResponseHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 204, "0"}} | Data]};
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size} | Headers],
	send(ModData, 201, ResponseHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 201, Size}} | Data]};
do_response(#mod{data = Data} = ModData, {200, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size} | Headers],
	send(ModData, 200, ResponseHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 201, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
			data = Data} = ModData, {error, 400}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1",
			title => "Bad Request",
			detail => "The server cannot or will not process the request"
					" due to something that is perceived to be a client error.",
			code => "", status => 400},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 400, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 400, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 403}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.3",
			title => "Forbidden",
			detail => "the server understood the request but refuses to authorize it.",
			code => "", status => 403},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 403, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 403, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 404}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 404, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 412}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7232#section-4.2",
			title => "Precondition Failed",
			detail => "One or more conditions given in the request header"
					" fields evaluated to false",
			code => "", status => 412},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 412, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 412, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 416}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7233#section-4.4",
			title => "Range Not Satisfiable",
			detail => "None of the ranges in the request's Range header"
					" field overlap the current extent of the selected resource",
			code => "", status => 416},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 416, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 416, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 500}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.6.1",
			title => "Internal Server Error",
			detail => "The server encountered an unexpected condition that"
					" prevented it from fulfilling the request.",
			code => "", status => 500},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 500, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 500, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, StatusCode, Problem}) when is_map(Problem),
		StatusCode >= 400, StatusCode =< 599 ->
	Problem1 = case maps:is_key(code, Problem) of
		true ->
			Problem#{status => StatusCode};
		false ->
			Problem#{code => "", status => StatusCode}
	end,
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem1, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, StatusCode, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, StatusCode, Size}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = Info,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(Info, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

