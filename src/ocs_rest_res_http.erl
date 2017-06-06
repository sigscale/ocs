%%% ocs_rest_res_http.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2017 SigScale Global Inc.
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
%%% @doc This library module implements resource handling functions
%%%   for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_http).
-copyright('Copyright (c) 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, get_http/0]).

-include("ocs_log.hrl").

-record(event,
		{host :: string(),
		user :: string() | undefined,
		date :: string() | undefined,
		method :: string() | undefined,
		uri :: string() | undefined,
		httpStatus :: integer() | undefined}).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	[].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_http() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/log/http'
%% requests.
get_http() ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	Log = ocs_log:httpd_logname(transfer),
	read_http_log(Log, MaxItems).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
read_http_log(Log, MaxItems) ->
	case ocs_log:last(Log, MaxItems) of
		{error, _} ->
			{error, 500};
		{NumItems, Events} ->
			JsonObjs = json(Events),
			JsonArray = {array, JsonObjs},
			Body = mochijson:encode(JsonArray),
			ContentRange = "items 1-" ++ integer_to_list(NumItems) ++ "/*",
			Headers = [{"content_type", "application/json"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

% @hidden
json(Events) ->
	F = fun(B, Acc) ->
				E = parse(B),
				JsonObj = {struct,
						[{"datetime", E#event.date},
						{"host", E#event.host}, 
						{"user", E#event.user},
						{"method", E#event.method},
						{"uri", E#event.uri},
						{"httpStatus", E#event.httpStatus}]},
				[JsonObj | Acc]
	end,
	lists:reverse(lists:foldl(F, [], Events)).

% @hidden
parse(Event) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Host:Offset/binary, 32, $-, 32, Rest/binary>> = Event,
	parse1(Rest, #event{host = binary_to_list(Host)}).
% @hidden
parse1(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<User:Offset/binary, 32, $[, Rest/binary>> = Event,
	parse2(Rest, Acc#event{user = binary_to_list(User)}).
% @hidden
parse2(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<$]>>),
	<<Date:Offset/binary, $], 32, $", Rest/binary>> = Event,
	parse3(Rest, Acc#event{date = binary_to_list(Date)}).
% @hidden
parse3(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Method:Offset/binary, 32, Rest/binary>> = Event,
	parse4(Rest, Acc#event{method = binary_to_list(Method)}).
% @hidden
parse4(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<URI:Offset/binary, 32, Rest/binary>> = Event,
	parse5(Rest, Acc#event{uri = binary_to_list(URI)}).
% @hidden
parse5(Event, Acc) ->
	{Offset, 2} = binary:match(Event, <<$", 32>>),
	<<_Http:Offset/binary, $", 32, Rest/binary>> = Event,
	parse6(Rest, Acc).
% @hidden
parse6(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Status:Offset/binary, 32, _Rest/binary>> = Event,
	Acc#event{httpStatus = binary_to_integer(Status)}.

