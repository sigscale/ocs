%%% mod_ocs_rest_patch.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
%%% @doc Handle received HTTP PATCH requests.
%%%
%%% 	This is an {@link //inets/httpd. httpd} callback module handling
%%% 	HTTP PATCH operations. The HTTP resources are managed in modules named
%%% 	`ocs_rest_res_*'.
%%%
%%% 	<h2><a name="callbacks">Resource Handler Functions</a></h2>
%%% 	The resource handler modules should implement callback functions
%%% 	in the pattern described in the example below.
%%%
%%% 	<h3 class="function">
%%% 		<a>patch_&lt;Resource&gt;/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>patch_&lt;Resource&gt;(Id, ContentType, RequestBody, [...]) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Id = string()</tt></li>
%%% 			<li><tt>ContentType = string()</tt></li>
%%% 			<li><tt>RequestBody = string()</tt></li>
%%% 			<li><tt>Result = {ok, Headers, ResponseBody}
%%% 					| {error, StatusCode}
%%% 					| {error, StatusCode, Problem}</tt></li>
%%% 			<li><tt>ResponseBody = io_list()</tt></li>
%%% 			<li><tt>StatusCode = 400..599</tt></li>
%%% 			<li><tt>Problem = #{type := uri(), title := string(),
%%% 					code := string(), cause => string(), detail => string(),
%%% 					invalidParams => [#{param := string(), reason => string()}],
%%% 					status => 400..599}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Resource handlers for HTTP PATCH operations on REST Resources.
%%%
%%% 	Response `Headers' must include `content_type' if `ResponseBody' is
%%% 	not en empty list. An optional `Problem' report may be provided in
%%% 	error responses which shall be formatted by
%%% 	{@link //ocs/ocs_rest:format_problem/2. format_problem/2} and included
%%% 	in the response body.
%%%
%%% @end
%%%
-module(mod_ocs_rest_patch).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

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
		"PATCH" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							{_, Resource} = lists:keyfind(resource, 1, Data),
							parse_query(Resource, ModData, Uri);
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
parse_query(Resource, ModData, Uri) ->
	parse_query1(Resource, ModData,
			uri_string:percent_decode(uri_string:parse(Uri))).
%% @hidden
parse_query1(Resource,
		#mod{parsed_header = RequestHeaders,
				entity_body = Body, data = Data} = ModData,
		#{path := Path, query := Query}) ->
	{_, ContentType} = lists:keyfind(content_type, 1, Data),
	do_patch(ContentType, Resource, ModData, Body,
			get_etag(RequestHeaders),
			string:lexemes(Path, [$/]),
			uri_string:dissect_query(Query));
parse_query1(Resource,
		#mod{parsed_header = RequestHeaders,
				entity_body = Body, data = Data} = ModData,
		#{path := Path}) ->
	{_, ContentType} = lists:keyfind(content_type, 1, Data),
	do_patch(ContentType, Resource, ModData, Body,
			get_etag(RequestHeaders),
			string:lexemes(Path, [$/]),
			[]);
parse_query1(_Resource,
		#mod{parsed_header = RequestHeaders, data = Data} = ModData, _) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/"
					"rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody}
			= ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 404, Size}} | Data]}.

get_etag(Headers) ->
	case lists:keyfind("if-match", 1, Headers) of
		{_, Etag} ->
			Etag;
		false ->
			undefined
	end.

%% @hidden
do_patch(ContentType, Resource, ModData, Body, Etag,
		["ocs", "v1", "client", Identity], _Query) ->
	do_response(ModData,
			Resource:patch_client(Identity, Etag, ContentType, Body));
do_patch(ContentType, Resource, ModData, Body, Etag,
		["ocs", "v1", "subscriber", Identity], _Query) ->
	do_response(ModData,
			Resource:patch_subscriber(Identity, Etag, ContentType, Body));
do_patch(ContentType, Resource, ModData, Body, Etag,
		["partyManagement", "v1", "individual", Identity],
		_Query) ->
	do_response(ModData,
			Resource:patch_user(Identity, Etag, ContentType, Body));
do_patch("application/json-patch+json", Resource, ModData, Body, Etag,
		["catalogManagement", "v2", "productOffering", ProdId],
		_Query) ->
	do_response(ModData,
			Resource:patch_offer(ProdId, Etag, Body));
do_patch("application/merge-patch+json", Resource, ModData, Body, Etag,
		["catalogManagement", "v2", "productOffering", ProdId],
		_Query) ->
	do_response(ModData,
			Resource:merge_patch_offer(ProdId, Etag, Body));
do_patch("application/json-patch+json", Resource, ModData, Body, Etag,
		["productInventoryManagement", "v2", "product", SubId],
		_Query) ->
	do_response(ModData,
			Resource:patch_product(SubId, Etag, Body));
do_patch("application/merge-patch+json", Resource, ModData, Body, Etag,
		["productInventoryManagement", "v2", "product", SubId],
		_Query) ->
	do_response(ModData,
			Resource:patch_product(SubId, Etag, Body));
do_patch("application/json-patch+json", Resource, ModData, Body, Etag,
		["catalogManagement", "v2", "pla", ProdId], _Query) ->
	do_response(ModData,
			Resource:patch_pla(ProdId, Etag, Body));
do_patch("application/json-patch+json", Resource, ModData, Body, Etag,
		["resourceInventoryManagement", "v1", "resource", Id],
		_Query) ->
	do_response(ModData,
			Resource:patch_resource(Id, Etag, Body));
do_patch("application/json-patch+json", Resource, ModData, Body, Etag,
		["serviceInventoryManagement", "v2", "service", ServiceId],
		_Query) ->
	do_response(ModData,
			Resource:patch_service(ServiceId, Etag, Body));
do_patch("application/json-patch+json", Resource, ModData, Body, Etag,
		["productCatalogManagement", "v2", "productOffering", ProdId],
		_Query) ->
	do_response(ModData,
			Resource:patch_offer(ProdId, Etag, Body));
do_patch("application/merge-patch+json", Resource, ModData, Body, Etag,
		["productCatalogManagement", "v2", "productOffering", ProdId],
		_Query) ->
	do_response(ModData,
			Resource:merge_patch_offer(ProdId, Etag, Body));
do_patch(_ContentType, _Resource,
		#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		_Body, _Etag, _Path, _Query) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType1, ResponseBody}
			= ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType1}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response,{already_sent, 404, Size}} | Data]}.

%% @hidden
do_response(#mod{data = Data} = ModData,
		{ok, Headers, []}) ->
	ResponseHeaders = [{content_length, "0"} | Headers],
	send(ModData, 204, ResponseHeaders, []),
	{proceed, [{response, {already_sent, 204, "0"}} | Data]};
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size} | Headers],
	send(ModData, 200, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 200, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 400}) ->
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
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 403}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.3",
			title => "Forbidden",
			detail => "the server understood the request but refuses to authorize it.",
			code => "", status => 403},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 403, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 403, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 404}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 404, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 409}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.8",
			title => "Conflict",
			detail => "The request could not be completed due to a conflict"
					" with the current state of the target resource.",
			code => "", status => 409},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 409, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 409, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 412}) ->
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
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 422}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc4918#section-11.2",
			title => "Unprocessable Entity",
			detail => "Unable to process the contained instructions.",
			code => "", status => 422},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 422, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 422, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		{error, 500}) ->
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
		{error, StatusCode, Problem})
		when is_map(Problem), StatusCode >= 400, StatusCode =< 599 ->
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

