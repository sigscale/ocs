%%% ocs_rest_res_client.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_client).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_accepted/0,
				content_types_provided/0,
				find_client/1,
				%find_clients/0,
				add_client/1,
				delete_client/1]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-define(VendorID, 529).
-define(AscendDataRate, 197).
-define(AscendXmitRate, 255).

-spec content_types_accepted() -> ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/hal+json"].

-spec find_client(Ip :: string()) ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client/{address}'
%% requests.
find_client(Ip) ->
	case inet:parse_address(Ip) of
		{ok, Address} ->
			find_client1(Address);
		{error, einval} ->
			{error, 400}
	end.
%% @hidden
find_client1(Address) ->
	case ocs:find_client(Address) of
		{ok, Secret} ->
			Id = inet:ntoa(Address),
			RespObj = [{id, Id}, {href, "/ocs/v1/client/" ++ Id}, {secret, Secret}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			{body, Body};
		{error, not_found} ->
			{error, 404}
	end.

%-spec find_clients() ->
%	{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%%% @doc Body producing function for `GET /ocs/v1/client'
%%% requests.
%find_clients() ->
%	case ocs:get_clients() of
%		{error, _} ->
%			{error, 404};
%		Clients ->
%			Response = find_clients1(Clients),
%			Body  = mochijson:encode(Response),
%			{body, Body}
%	end.
%%% @hidden
%find_clients1(Clients) ->
%			F = fun(#radius_client{address= Address, secret = Secret}, Acc) ->
%				RespObj = [{struct, [{address, inet:ntoa(Address)}, {secret, Secret}]}],
%				RespObj ++ Acc
%			end,
%			JsonObj = lists:foldl(F, [], Clients),
%			{array, JsonObj}.

-spec add_client(RequestBody :: list()) ->
	{Location :: string(), Body :: iolist()}
	| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/client' and add a new `client'
%% resource.
add_client(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		{_, Id} = lists:keyfind("id", 1, Object),
		{_, Secret} = lists:keyfind("secret", 1, Object),
		add_client1(Id, Secret)
	catch
		_Error ->
			{error, 400}
	end.
%% @hidden
add_client1(Id, Secret) ->
	try
	case catch ocs:add_client(Id, Secret) of
		ok ->
			Location = "/ocs/v1/client/" ++ Id,
			RespObj = [{id, Id}, {href, Location}, {secret, Secret}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			{Location, Body};
		{error, _Reason} ->
			{error, 400}
	end catch
		throw:_ ->
			{error, 400}
	end.

-spec delete_client(Ip :: list()) ->
	ok .
%% @doc Respond to `DELETE /ocs/v1/client/{address}' request and deletes
%% a `client' resource. If the deletion is succeeded return true.
delete_client(Ip) ->
	{ok, Address} = inet:parse_address(Ip), 
	ok = ocs:delete_client(Address),
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

