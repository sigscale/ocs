%%% ocs_rest_res_accounting.erl
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
			Headers = [{content_range, ContentRange}],
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
			Username = radius_attributes:fetch(?UserName, Attr),
			Idenifier = radius_attributes:fetch(?NasIdentifier, Attr),
			AcctInput = case {radius_attributes:find(?AcctInputOctets, Attr), radius_attributes:find(?AcctInputGigawords, Attr)} of
				{{ok, Octets}, {ok, Giga}} ->
					[{"acctInputoctets", Octets + (Giga * 4294967296)}];
				{{ok, Octets}, _} ->
					[{"acctInputoctets", Octets}];
				{_, _} ->
					[]
			end,
			AcctOutput = case {radius_attributes:find(?AcctOutputOctets, Attr), radius_attributes:find(?AcctOutputGigawords, Attr)} of
				{{ok, OctetsOut}, {ok, GigaOut}} ->
					[{"acctOutputoctets", OctetsOut + (GigaOut * 4294967296)}];
				{{ok, OctetsOut}, _} ->
					[{"acctOutputoctets", OctetsOut}];
				{_, _} ->
					[]
			end,
			Duration = case radius_attributes:find(?AcctSessionTime, Attr) of
				{ok, SessionTime} ->
					[{"acctSessiontime", SessionTime}];
				{error, not_found} ->
					[]	
			end,
			JsonObj = {struct, [{"timeStamp", TimeStamp}, {"node", Node},
					{"serverAddress", ServerIp}, {"serverPort", ServerPort},
					{"type", Type}, {"username", Username},
					{"nasIdentifier", Idenifier}] ++ Duration ++ AcctInput ++ AcctOutput},
			[JsonObj | Acc];
		(_, Acc) ->
			%% TODO support for DIAMETER
			Acc
	end,
	lists:reverse(lists:foldl(F, [], Events)).

