%%% mod_ocs_rest_accepted_content.erl
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
-module(mod_ocs_rest_accepted_content).
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
		data = Data} = _ModData) ->
	case proplists:get_value(status, Data) of
		{_StatusCode, _PhraseArgs, _Reason} ->
			{proceed, Data};
		undefined ->
			case proplists:get_value(response, Data) of
				undefined ->
					case string:tokens(Uri, "/?") of
						["ocs", "v1", "client"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_client, Data);
						["ocs", "v1", "client", _Id] ->
							check_content_type_header(Headers, Method, ocs_rest_res_client, Data);
						["ocs", "v1", "subscriber"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_subscriber, Data);
						["ocs", "v1", "subscriber", _Id] ->
							check_content_type_header(Headers, Method, ocs_rest_res_subscriber, Data);
						["ocs", "v1", "log", "ipdr"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_usage, Data);
						["ocs", "v1", "log", "access"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_access, Data);
						["ocs", "v1", "log", "accounting"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_accounting, Data);
						["ocs", "v1", "log", "http"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_http, Data);
						["usageManagement", "v1", "usage"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_usage, Data);
						["usageManagement", "v1", "usage" | _] ->
							check_content_type_header(Headers, Method, ocs_rest_res_usage, Data);
						["usageManagement", "v1", "usageSpecification" | _] ->
							check_content_type_header(Headers, Method, ocs_rest_res_usage, Data);
						["partyManagement", "v1", "individual"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_user, Data);
						["partyManagement", "v1", "individual", _Id] ->
							check_content_type_header(Headers, Method, ocs_rest_res_user, Data);
						["balanceManagement", "v1", _Id, "balanceTopups"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_balance, Data);
						["balanceManagement", "v1", _Id, "buckets"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_balance, Data);
						["catalogManagement", "v1", "productOffering"] ->
							check_content_type_header(Headers, Method, ocs_rest_res_product, Data);
						["productInventoryManagement", "v1", "product", _] ->
							check_content_type_header(Headers, Method, ocs_rest_res_product, Data);
						_ ->
							{proceed, Data}
					end;
				_ ->
					{proceed,  Data}
			end
	end.

%% @hidden
check_content_type_header(Headers, Method, Module, Data) ->
	case lists:keyfind("content-type", 1, Headers) of
		{_, ProvidedType} ->
			AcceptedTypes = Module:content_types_accepted(),
			case lists:member(ProvidedType, AcceptedTypes) of
				true ->
					check_accept_header(Headers, Module, [{resource, Module},
							{content_type,  ProvidedType} | Data]);
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{break, [{response, {415, Response}}]}
			end;
		false when Method == "DELETE"; Method == "GET" ->
			check_accept_header(Headers, Module, [{resource, Module} | Data]);
		false ->
			Response = "<h2>HTTP Error 400 - Bad Request</h2>",
			{break, [{response, {400, Response}}]}
	end.

%% @hidden
check_accept_header(Headers, Module, Data) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, AcceptType} ->
			Representations = Module:content_types_provided(),
			case lists:member(AcceptType, Representations) of
				true ->
					{proceed, [{accept, AcceptType} | Data]};
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{break, [{response, {415, Response}}]}
			end;
		false ->
			{proceed, Data}
	end.

