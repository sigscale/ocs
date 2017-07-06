%%% ocs_rest_res_client.erl
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
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_client).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_client/0, get_client/1, post_client/1,
		patch_client/3, delete_client/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_client() -> Result
	when
		Result ::{ok, Headers :: [tuple()],
				Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client'
%% requests.
get_client() ->
	case ocs:get_clients() of
		{error, _} ->
			{error, 500};
		Clients ->
			get_client0(Clients)
	end.
%% @hidden
get_client0(Clients) ->
	F = fun(#client{address= Address, identifier = Identifier, port = Port,
				protocol = Protocol, secret = Secret}, Acc) ->
			Id = inet:ntoa(Address),
			RespObj1 = [{id, Id}, {href, "/ocs/v1/client/" ++ Id}],
			RespObj2 = case Identifier of
				<<>> ->
					[];
				Identifier ->
					[{identifier, binary_to_list(Identifier)}]
			end,
			RespObj3 = [{"port", Port},
					{protocol, string:to_upper(atom_to_list(Protocol))},
					{secret, Secret}],
			[{struct, RespObj1 ++ RespObj2 ++ RespObj3} | Acc]
	end,
	try
		JsonObj = lists:foldl(F, [], Clients),
		Body  = mochijson:encode({array, lists:reverse(JsonObj)}),
		{ok, [{content_type, "application/json"}], Body}
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec get_client(Ip) -> Result
	when
		Ip :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client/{id}'
%% requests.
get_client(Ip) ->
	case inet:parse_address(Ip) of
		{ok, Address} ->
			get_client1(Address);
		{error, einval} ->
			{error, 400}
	end.
%% @hidden
get_client1(Address) ->
	case ocs:find_client(Address) of
		{ok, #client{port = Port, identifier = Identifier,
				protocol = Protocol, secret = Secret, last_modified = LM}} ->
			Id = inet:ntoa(Address),
			Etag = etag(LM),
			RespObj1 = [{id, Id}, {href, "/ocs/v1/client/" ++ Id}],
			RespObj2 = case Identifier of
				<<>> ->
					[];
				Identifier ->
					[{identifier, binary_to_list(Identifier)}]
			end,
			RespObj3 = [{"port", Port},
					{protocol, string:to_upper(atom_to_list(Protocol))},
					{secret, Secret}],
			JsonObj  = {struct, RespObj1 ++ RespObj2 ++ RespObj3},
			Body = mochijson:encode(JsonObj),
			Headers = [{content_type, "application/json"}, {etag, Etag}],
			{ok, Headers, Body};
		{error, not_found} ->
			{error, 404}
	end.

-spec post_client(RequestBody) -> Result 
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/client' and add a new `client'
%% resource.
post_client(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		{_, Id} = lists:keyfind("id", 1, Object),
		Port = proplists:get_value("port", Object, 3799),
		Protocol = case proplists:get_value("protocol", Object, "radius") of
			RADIUS when RADIUS =:= "radius"; RADIUS =:= "RADIUS" ->
				radius;
			DIAMETER when DIAMETER =:= "diameter"; DIAMETER =:= "DIAMETER" ->
				diameter
		end,
		Secret = proplists:get_value("secret", Object, ocs:generate_password()),
		ok = ocs:add_client(Id, Port, Protocol, Secret),
		{ok, #client{last_modified = LM}} = ocs:find_client(Id),
		Location = "/ocs/v1/client/" ++ Id,
		RespObj = [{id, Id}, {href, Location}, {"port", Port},
				{protocol, string:to_upper(atom_to_list(Protocol))}, {secret, Secret}],
		JsonObj  = {struct, RespObj},
		Body = mochijson:encode(JsonObj),
		Headers = [{location, Location}, {etag, etag(LM)}],
		{ok, Headers, Body}
	catch
		_Error ->
			{error, 400}
	end.

-spec patch_client(Ip, Etag, ReqBody) -> Result 
	when
		Ip :: string(),
		Etag :: undefined | string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/client/{id}' request and
%% Updates a existing `client''s password or attributes.
patch_client(Ip, undefined, ReqBody) ->
	patch_client0(Ip, undefined, ReqBody);
patch_client(Ip, Etag, ReqBody) ->
	case inet:parse_address(Ip) of
		{ok, Address} ->
			try
				Etag1 = etag(Etag), 
				patch_client0(Address, Etag1, ReqBody)
			catch
				_:_ ->
					{error, 400}
			end;
		{error, einval} ->
			{error, 400}
	end.
%% @hidden
patch_client0(Id, Etag, ReqBody) ->
	case ocs:find_client(Id) of
		{ok, #client{port = CurrPort, protocol = CurrProtocol,
				secret = CurrSecret, last_modified = CurrentEtag}}
				when CurrentEtag == Etag; Etag == undefined ->
			try
				{struct, Object} = mochijson:decode(ReqBody),
				case Object of
					[{"secret", NewPassword}] ->
						Protocol_Atom = string:to_upper(atom_to_list(CurrProtocol)),
						patch_client1(Id, CurrPort, Protocol_Atom, NewPassword, Etag);
					[{"port", NewPort},{"protocol", RADIUS}] 
							when RADIUS =:= "radius"; RADIUS =:= "RADIUS" ->
						patch_client2(Id, NewPort, radius, CurrSecret, Etag);
					[{"port", NewPort},{"protocol", DIAMETER}]
							when DIAMETER =:= "diameter"; DIAMETER =:= "DIAMETER" ->
						patch_client2(Id, NewPort, diameter, CurrSecret, Etag)
				end
			catch
				throw : _ ->
					{error, 400}
			end;
		{ok, #client{last_modified = _NonMatchingEtag}} ->
			{error, 412};
		{error, _Reason} ->
			{error, 404}
	end.
%% @hidden
patch_client1(Id, Port, Protocol, NewPassword, Etag) ->
	IDstr = inet:ntoa(Id),
	ok = ocs:update_client(Id, NewPassword),
	RespObj =[{id, IDstr}, {href, "/ocs/v1/client/" ++ IDstr},
			{"port", Port}, {protocol, Protocol}, {secret, NewPassword}],
	JsonObj  = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	Headers = case Etag of
		undefined ->
			[];
		_ ->
			[{etag, etag(Etag)}]
	end,
	{ok, Headers, RespBody}.
%% @hidden
patch_client2(Id, Port, Protocol, Secret, Etag) ->
	ok = ocs:update_client(Id, Port, Protocol),
	RespObj =[{id, Id}, {href, "/ocs/v1/client/" ++ Id},
			{"port", Port}, {protocol, Protocol}, {secret, Secret}],
	JsonObj  = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	Headers = case Etag of
		undefined ->
			[];
		_ ->
			[{etag, Etag}]
	end,
	{ok, Headers, RespBody}.

-spec delete_client(Ip) -> Result
	when
		Ip :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /ocs/v1/client/{address}' request and deletes
%% a `client' resource. If the deletion is successful return true.
delete_client(Ip) ->
	case inet:parse_address(Ip) of
		{ok, Address} ->
			ocs:delete_client(Address),
			{ok, [], []};
		{error, einval} ->
			{error, 400}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec etag(V1) -> V2
	when
		V1 :: string() | {N1, N2},
		V2 :: {N1, N2} | string(),
		N1 :: integer(),
		N2 :: integer().
%% @doc Generate a tuple with 2 integers from Etag string
%% value or vice versa.
%% @hidden
etag(V) when is_list(V) ->
	[TS, N] = string:tokens(V, "-"),
	{list_to_integer(TS), list_to_integer(N)};
etag(V) when is_tuple(V) ->
	{TS, N} = V,
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

