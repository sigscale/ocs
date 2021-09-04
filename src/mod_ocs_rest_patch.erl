%%% mod_ocs_rest_patch.erl
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
-module(mod_ocs_rest_patch).
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
		"PATCH" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							Path = http_uri:decode(Uri),
							{_, Resource} = lists:keyfind(resource, 1, Data),
							{_, ContentType} = lists:keyfind(content_type, 1, Data),
							content_type_available(Headers, ContentType, Body, Path,
									Resource, ModData);
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
content_type_available(Headers, ContentType, Body,
		Uri, Resource, #mod{data = Data} = ModData) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, RequestingType} ->
			AvailableTypes = Resource:content_types_provided(),
			case lists:member(RequestingType, AvailableTypes) of
				true ->
					Etag = get_etag(Headers),
					do_patch(ContentType, Body, Resource, ModData, Etag,
							string:tokens(Uri, "/"));
				false ->
					Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.13",
							title => "Unsupported Media Type",
							detail => "The client provided Content-Type which the"
									" the server does not support.",
							code => "", status => 415},
					{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
					Headers1 = lists:keystore(content_type, 1, Headers,
							{content_type, ContentType}),
					Size = integer_to_list(iolist_size(ResponseBody)),
					Headers2 = [{content_length, Size} | Headers1],
					send(ModData, 415, Headers2, ResponseBody),
					{proceed, [{response, {already_sent, 415, Size}} | Data]}
			end;
		_ ->
			do_patch(ContentType, Uri, Body, Resource, undefined, ModData)
	end.

%% @hidden
get_etag(Headers) ->
	case lists:keyfind("if-match", 1, Headers) of
		{_, Etag} ->
			Etag;
		false ->
			undefined
	end.

%% @hidden
do_patch(ContentType, Body, Resource, ModData, Etag,
		["ocs", "v1", "client", Identity]) ->
	do_response(ModData, Resource:patch_client(Identity, Etag, ContentType, Body));
do_patch(ContentType, Body, Resource, ModData, Etag,
		["ocs", "v1", "subscriber", Identity]) ->
	do_response(ModData, Resource:patch_subscriber(Identity, Etag, ContentType,
			Body));
do_patch(ContentType, Body, Resource, ModData, Etag,
		["partyManagement", "v1", "individual", Identity]) ->
	do_response(ModData, Resource:patch_user(Identity, Etag, ContentType,
			Body));
do_patch("application/json-patch+json", Body, Resource, ModData, Etag,
		["catalogManagement", "v2", "productOffering", ProdId]) ->
	do_response(ModData, Resource:patch_offer(ProdId, Etag, Body));
do_patch("application/merge-patch+json", Body, Resource, ModData, Etag,
		["catalogManagement", "v2", "productOffering", ProdId]) ->
	do_response(ModData, Resource:merge_patch_offer(ProdId, Etag, Body));
do_patch("application/json-patch+json", Body, Resource, ModData, Etag,
		["productInventoryManagement", "v2", "product", SubId]) ->
	do_response(ModData, Resource:patch_inventory(SubId, Etag, Body));
do_patch("application/merge-patch+json", Body, Resource, ModData, Etag,
		["productInventoryManagement", "v2", "product", SubId]) ->
	do_response(ModData, Resource:patch_inventory(SubId, Etag, Body));
do_patch("application/json-patch+json", Body, Resource, ModData, Etag,
		["catalogManagement", "v2", "pla", ProdId]) ->
	do_response(ModData, Resource:patch_pla(ProdId, Etag, Body));
do_patch("application/json-patch+json", Body, Resource, ModData, Etag,
		["resourceInventoryManagement", "v1", "resource", Id]) ->
	do_response(ModData, Resource:patch_resource(Id, Etag, Body));
do_patch("application/json-patch+json", Body, Resource, ModData, Etag,
		["serviceInventoryManagement", "v2", "service", ServiceId]) ->
	do_response(ModData, Resource:patch_inventory(ServiceId, Etag, Body));
do_patch("application/json-patch+json", Body, Resource, ModData, Etag,
		["productCatalogManagement", "v2", "productOffering", ProdId]) ->
	do_response(ModData, Resource:patch_offer(ProdId, Etag, Body));
do_patch("application/merge-patch+json", Body, Resource, ModData, Etag,
		["productCatalogManagement", "v2", "productOffering", ProdId]) ->
	do_response(ModData, Resource:merge_patch_offer(ProdId, Etag, Body));
do_patch(#mod{parsed_header = Headers,
		data = Data} = ModData, _, _, _, _, _) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 404, Headers2, ResponseBody),
	{proceed, [{response,{already_sent, 404, Size}} | Data]}.

%% @hidden
do_response(#mod{data = Data} = ModData, {ok, Headers, []}) ->
	Size = integer_to_list(iolist_size([])),
	NewHeaders = [{content_length, Size} | Headers],
	send(ModData, 204, NewHeaders, []),
	{proceed,[{response,{already_sent,200, Size}} | Data]};
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	NewHeaders = [{content_length, Size} | Headers],
	send(ModData, 200, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent,200, Size}} | Data]};
do_response(#mod{parsed_header = Headers,
			data = Data} = ModData, {error, 400}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1",
			title => "Bad Request",
			detail => "The server cannot or will not process the request"
					" due to something that is perceived to be a client error.",
			code => "", status => 400},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 400, Headers2, ResponseBody),
	{proceed, [{response, {already_sent, 400, Size}} | Data]};
do_response(#mod{parsed_header = Headers,
		data = Data} = ModData, {error, 404}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 404, Headers2, ResponseBody),
	{proceed, [{response, {already_sent, 404, Size}} | Data]};
do_response(#mod{parsed_header = Headers,
		data = Data} = ModData, {error, 409}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.8",
			title => "Conflict",
			detail => "The request could not be completed due to a conflict"
					" with the current state of the target resource.",
			code => "", status => 409},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 409, Headers2, ResponseBody),
	{proceed, [{response, {already_sent, 409, Size}} | Data]};
do_response(#mod{parsed_header = Headers,
		data = Data} = ModData, {error, 412}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7232#section-4.2",
			title => "Precondition Failed",
			detail => "One or more conditions given in the request header"
					" fields evaluated to false",
			code => "", status => 412},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 412, Headers2, ResponseBody),
	{proceed, [{response, {already_sent, 412, Size}} | Data]};
do_response(#mod{parsed_header = Headers,
		data = Data} = ModData, {error, 422}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc4918#section-11.2",
			title => "Unprocessable Entity",
			detail => "Unable to process the contained instructions.",
			code => "", status => 422},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 422, Headers2, ResponseBody),
	{proceed, [{response, {already_sent, 422, Size}} | Data]};
do_response(#mod{parsed_header = Headers,
		data = Data} = ModData, {error, 500}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.6.1",
			title => "Internal Server Error",
			detail => "The server encountered an unexpected condition that"
					" prevented it from fulfilling the request.",
			code => "", status => 500},
	{ContentType, ResponseBody} = ocs_rest:format_problem(Problem, Headers),
	Headers1 = lists:keystore(content_type, 1, Headers,
			{content_type, ContentType}),
	Size = integer_to_list(iolist_size(ResponseBody)),
	Headers2 = [{content_length, Size} | Headers1],
	send(ModData, 500, Headers2, ResponseBody),
	{proceed, [{response, {already_sent, 500, Size}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = Info,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(Info, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

