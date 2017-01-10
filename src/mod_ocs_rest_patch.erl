%%% mod_ocs_rest_patch.erl
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
-module(mod_ocs_rest_patch).
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
do(#mod{method = Method, request_uri = Uri, entity_body = Body, 
				data = Data} = ModData) ->
	case Method of
		"PATCH" ->
			case string:tokens(Uri, "/") of
				["ocs", "v1", "subscriber", Identity] ->
					case ocs_rest_res_subscriber:update_subscriber(Identity, Body) of
						{error, ErrorCode} ->
							{break, [{response,	{ErrorCode, "<h1>Not Found</h1>"}}]};
						{body, RespBody} ->
							send_response(ModData, RespBody)
					end;
				_ ->	
					{break, [{response,	{404, "<h1>NOT FOUND</h1>"}}]}
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
send_response(Info, ResponseBody)->
	    Size = integer_to_list(iolist_size(ResponseBody)),
	    Headers = [{content_length, Size}],
	    send(Info, 200, Headers, ResponseBody),
	    {proceed,[{response,{already_sent,200, Size}}]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = Info,
     StatusCode, Headers, ResponseBody) ->
    httpd_response:send_header(Info, StatusCode, Headers),
    httpd_socket:deliver(SocketType, Socket, ResponseBody).

