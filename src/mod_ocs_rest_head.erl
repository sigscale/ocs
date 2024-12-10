%%% mod_ocs_rest_head.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2024 SigScale Global Inc.
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
%%% @doc Handle received HTTP GET requests.
%%%
%%% 	This is an {@link //inets/httpd. httpd} callback module handling
%%% 	HTTP GET operations. The HTTP resources are managed in modules named
%%% 	`ocs_rest_res_*'.
%%%
%%% 	<h2><a name="callbacks">Resource Handler Functions</a></h2>
%%% 	The resource handler modules should implement callback functions
%%% 	in the pattern described in the example below.
%%%
%%% 	<h3 class="function">
%%% 		<a>get_&lt;Collection&gt;/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>get_&lt;Collection&gt;(Query, Headers) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Query = [{Param, Value}]</tt></li>
%%% 			<li><tt>Param = string()</tt></li>
%%% 			<li><tt>Value = string()</tt></li>
%%% 			<li><tt>Headers = [{Option, Value}</tt></li>
%%% 			<li><tt>Option = accept_ranges | allow | cache_control
%%% 					| content_MD5 | content_encoding | content_language
%%% 					| content_length | content_location | content_range
%%% 					| content_type | date | etag | expires | last_modified
%%% 					| location | pragma | retry_after | server | trailer
%%% 					| transfer_encoding</tt></li>
%%% 			<li><tt>Result = {ok, Headers, ResponseBody}
%%% 					| {error, StatusCode}
%%% 					| {error, StatusCode, Problem}
%%% 					| {error, StatusCode, ResponseHeaders, ResponseBody}}
%%% 			</tt></li>
%%% 			<li><tt>ResponseBody = io_list()</tt></li>
%%% 			<li><tt>StatusCode = 200..599</tt></li>
%%% 			<li><tt>ResponseHeaders = [{Option, Value}</tt></li>
%%% 			<li><tt>Problem = #{type := uri(), title := string(),
%%% 					code := string(), cause => string(), detail => string(),
%%% 					invalidParams => [#{param := string(), reason => string()}],
%%% 					status => 200..599}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Resource handlers for HTTP GET operations on REST Collections.
%%%
%%% 	Response `Headers' must include `content_type' if `ResponseBody' is
%%% 	not en empty list. An optional `Problem' report may be provided in
%%% 	error responses which shall be formatted by
%%% 	{@link //ocs/ocs_rest:format_problem/2. format_problem/2} and included
%%% 	in the response body.
%%%
%%% 	<h3 class="function">
%%% 		<a>get_&lt;Resource&gt;/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>get_&lt;Resource&gt;(Id, Query, [Headers, ...]) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Id = string()</tt></li>
%%% 			<li><tt>Query = [{Param, Value}]</tt></li>
%%% 			<li><tt>Param = string()</tt></li>
%%% 			<li><tt>Value = string()</tt></li>
%%% 			<li><tt>Headers = [{Option, Value}</tt></li>
%%% 			<li><tt>Option = accept_ranges | allow | cache_control
%%% 					| content_MD5 | content_encoding | content_language
%%% 					| content_length | content_location | content_range
%%% 					| content_type | date | etag | expires | last_modified
%%% 					| location | pragma | retry_after | server | trailer
%%% 					| transfer_encoding</tt></li>
%%% 			<li><tt>Result = {ok, Headers, ResponseBody}
%%% 					| {error, StatusCode}
%%% 					| {error, StatusCode, Problem}
%%% 					| {error, StatusCode, ResponseHeaders, ResponseBody}}
%%% 			</tt></li>
%%% 			<li><tt>ResponseBody = io_list()</tt></li>
%%% 			<li><tt>StatusCode = 200..599</tt></li>
%%% 			<li><tt>ResponseHeaders = [{Option, Value}</tt></li>
%%% 			<li><tt>Problem = #{type := uri(), title := string(),
%%% 					code := string(), cause => string(), detail => string(),
%%% 					invalidParams => [#{param := string(), reason => string()}],
%%% 					status => 200..599}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Resource handlers for HTTP GET operations on REST Resources.
%%%
%%% 	Response `Headers' must include `content_type' if `ResponseBody' is
%%% 	not en empty list. An optional `Problem' report may be provided in
%%% 	error responses which shall be formatted by
%%% 	{@link //ocs/ocs_rest:format_problem/2. format_problem/2} and included
%%% 	in the response body.
%%%
%%% @end
%%%
-module(mod_ocs_rest_head).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

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
do(#mod{method = Method, request_uri = Uri, data = Data} = ModData) ->
	case Method of
		"HEAD" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							case lists:keyfind(resource, 1, Data) of
								false ->
									{proceed, Data};
								{_, Resource} ->
									parse_query(Resource, ModData, Uri)
							end;
						_Response ->
							{proceed,  Data}
					end
			end;
		_Other ->
			{proceed, Data}
	end.

%% @hidden
parse_query(Resource, ModData, Uri) ->
	parse_query1(Resource, ModData,
			uri_string:percent_decode(uri_string:parse(Uri))).
%% @hidden
parse_query1(Resource, ModData, #{path := Path, query := Query}) ->
	do_head(Resource, ModData, string:lexemes(Path, [$/]),
			uri_string:dissect_query(Query));
parse_query1(Resource, ModData, #{path := Path}) ->
	do_head(Resource, ModData, string:lexemes(Path, [$/]), []);
parse_query1(_, #mod{parsed_header = RequestHeaders,
		data = Data} = ModData, _UriMap) ->
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

%% @hidden
do_head(Resource, ModData,
		["balanceManagement", "v1", "bucket"], _Query) ->
	do_response(ModData, Resource:head_bucket());
do_head(Resource, ModData,
		["ocs", "v1", "client"], _Query) ->
	do_response(ModData, Resource:head_client());
do_head(Resource, ModData,
		["serviceInventoryManagement", "v2", "service"], _Query) ->
	do_response(ModData, Resource:head_inventory());
do_head(Resource, ModData,
		["resourceInventoryManagement", "v1", "resource"], _Query) ->
	do_response(ModData, Resource:head_resource());
do_head(Resource, ModData,
		["catalogManagement", "v2", "productOffering"], _Query) ->
	do_response(ModData, Resource:head_offer());
do_head(Resource, ModData,
		["productCatalogManagement", "v2", "productOffering"], _Query) ->
	do_response(ModData, Resource:head_offer());
do_head(Resource, ModData,
		["productInventoryManagement", "v2", "product"], _Query) ->
	do_response(ModData, Resource:head_product());
do_head(Resource, ModData,
		["partyManagement", "v1", "individual"], _Query) ->
	do_response(ModData, Resource:head_user());
do_head(_Resource,
		#mod{parsed_header = RequestHeaders, data = Data} = ModData,
		_Path, _Query) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response,{already_sent, 404, Size}} | Data]}.

%% @hidden
do_response(#mod{data = Data} = ModData,
		{ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size} | Headers],
	send(ModData, 204, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 204, Size}} | Data]};
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
		{error, 416}) ->
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
	{proceed, [{response, {already_sent, StatusCode, Size}} | Data]};
do_response(#mod{data = Data} = ModData,
		{error, StatusCode, Headers, ResponseBody})
		when is_list(ResponseBody), StatusCode >= 400, StatusCode =< 599 ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size} | Headers],
	send(ModData, StatusCode, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, StatusCode, Size}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = ModData,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(ModData, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

