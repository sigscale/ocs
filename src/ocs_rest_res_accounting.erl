%%% ocs_rest_res_accounting.erl
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
%%% @doc This library module implements resource handling functions
%%%   for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_accounting).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_accounting/0]).

-include_lib("radius/include/radius.hrl").
-include("ocs_log.hrl").

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

-spec get_accounting() -> Result
	when
		Result :: {ok, Headers :: [tuple()],
				Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/log/accounting'
%% requests.
get_accounting() ->
	{ok, MaxItems} = application:get_env(ocs, rest_page_size),
	case ocs_log:last(ocs_acct, MaxItems) of
		{error, _} -> 
			{error, 404};
		{NewCount, Events} -> 
			JsonObj = radius_acct_json(Events),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			ContentRange = "items 1-" ++ integer_to_list(NewCount) ++ "/*",
			Headers = [{content_type, "application/json"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

% @hidden
radius_acct_json(Events) ->
	F = fun({Milliseconds, _N, radius, Node, Server, Type, Attr}, Acc) ->
			TimeStamp = ocs_log:iso8601(Milliseconds),
			{ServerAdd, ServerPort} = Server,
			ServerIp = inet:ntoa(ServerAdd),
			Obj0 = [{"type", Type}, {"node", Node}, {"serverPort", ServerPort},
					{"serverAddress", ServerIp}, {"timeStamp", TimeStamp}],
			Obj1 = case radius_attributes:find(?UserName, Attr) of
				{ok, Username} ->
					[{"username", Username} | Obj0];
				{error, not_found} ->
					Obj0
			end,
			Obj2 = case radius_attributes:find(?NasIdentifier, Attr) of
				{ok, Identifier} ->
					[{"nasIdentifier", Identifier} | Obj1];
				{error, not_found} ->
					Obj1
			end,
			Obj3 = case {radius_attributes:find(?AcctInputOctets, Attr),
					radius_attributes:find(?AcctInputGigawords, Attr)} of
				{{ok, Octets}, {ok, Giga}} ->
					[{"acctInputoctets", Octets + (Giga * 4294967296)} | Obj2];
				{{ok, Octets}, _} ->
					[{"acctInputoctets", Octets} | Obj2];
				{_, _} ->
					Obj2
			end,
			Obj4 = case {radius_attributes:find(?AcctOutputOctets, Attr),
					radius_attributes:find(?AcctOutputGigawords, Attr)} of
				{{ok, OctetsOut}, {ok, GigaOut}} ->
					[{"acctOutputoctets", OctetsOut + (GigaOut * 4294967296)}| Obj3];
				{{ok, OctetsOut}, _} ->
					[{"acctOutputoctets", OctetsOut} | Obj3];
				{_, _} ->
					Obj3
			end,
			Obj5 = case radius_attributes:find(?AcctSessionTime, Attr) of
				{ok, SessionTime} ->
					[{"acctSessiontime", SessionTime} | Obj4];
				{error, not_found} ->
					Obj4
			end,
			[{struct, lists:reverse(Obj5)} | Acc];
		(_, Acc) ->
			%% TODO support for DIAMETER
			Acc
	end,
	lists:reverse(lists:foldl(F, [], Events)).

