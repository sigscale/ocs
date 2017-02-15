%%% mod_ocs_rest_accepted_content.erl
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
%%%
-module(mod_ocs_rest_accepted_content).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

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
				data = Data} = _ModData) ->
	case proplists:get_value(status, Data) of
		{_StatusCode, _PhraseArgs, _Reason} ->
			{proceed, Data};
		undefined ->
			case proplists:get_value(response, Data) of
				undefined ->
					case string:tokens(Uri, "/") of
						[_, "v1", Resource] ->
							do_accept(Headers, Method, Resource, Data);
						[_, "v1", Resource, _Id] ->
							do_accept(Headers, Method, Resource, Data);
							_ ->
								Response = "<h2>HTTP Error 400 - Bad Request</h2>",
								{break, [{response, {400, Response}}]}
					end;
				_Response ->
					{proceed,  Data}
			end
	end.

%% @hidden
do_accept(Headers, Method, Resource, Data) ->
	ResourceName = list_to_atom("ocs_rest_res_" ++ Resource),
	case lists:keyfind("content-type", 1, Headers) of
		{_, ProvidedType} ->
			AcceptedTypes = ResourceName:content_types_accepted(),
			case lists:member(ProvidedType, AcceptedTypes) of
				true ->
					{proceed, [{resource, ResourceName} | Data]};
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{break, [{response, {415, Response}}]}
			end;
		false when Method == "DELETE"; Method == "GET" ->
			{proceed, [{resource, ResourceName} | Data]};
		false ->
			Response = "<h2>HTTP Error 400 - Bad Request</h2>",
			{break, [{response, {400, Response}}]}
	end.

