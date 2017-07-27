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
		get_clients/1, get_client/2, post_client/1,
		patch_client/4, delete_client/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_clients(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result ::{ok, Headers :: [tuple()],
				Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client'
%% requests.
get_clients(Query) ->
	case ocs:get_clients() of
		{error, _} ->
			{error, 500};
		Clients ->
			case lists:keytake("fields", 1, Query) of
				{value, {_, L}, NewQuery} ->
					get_clients(Clients, NewQuery, string:tokens(L, ","));
				false ->
					get_clients(Clients, Query, [])
			end
	end.
%% @hidden
get_clients(Clients, Query, Filters) ->
	try
		case lists:keytake("sort", 1, Query) of
			{value, {_, "id"}, NewQuery} ->
				{lists:keysort(#client.address, Clients), NewQuery};
			{value, {_, "-id"}, NewQuery} ->
				{lists:reverse(lists:keysort(#client.address, Clients)), NewQuery};
			{value, {_, "identifier"}, NewQuery} ->
				{lists:keysort(#client.identifier, Clients), NewQuery};
			{value, {_, "-identifier"}, NewQuery} ->
				{lists:reverse(lists:keysort(#client.identifier, Clients)), NewQuery};
			{value, {_, "port"}, NewQuery} ->
				{lists:keysort(#client.port, Clients), NewQuery};
			{value, {_, "-port"}, NewQuery} ->
				{lists:reverse(lists:keysort(#client.port, Clients)), NewQuery};
			{value, {_, "protocol"}, NewQuery} ->
				{lists:keysort(#client.protocol, Clients), NewQuery};
			{value, {_, "-protocol"}, NewQuery} ->
				{lists:reverse(lists:keysort(#client.protocol, Clients)), NewQuery};
			{value, {_, "secret"}, NewQuery} ->
				{lists:keysort(#client.secret, Clients), NewQuery};
			{value, {_, "-secret"}, NewQuery} ->
				{lists:reverse(lists:keysort(#client.secret, Clients)), NewQuery};
			false ->
				{Clients, Query};
			_ ->
				throw(400)
		end
	of
		{SortedClients, NextQuery} ->
			get_clients1(SortedClients, NextQuery, Filters)
	catch
		throw:400 ->
			{error, 400}
	end.
%% @hidden
get_clients1(Clients, [] = _Query, Filters) ->
	F = fun(#client{address= Address, identifier = Identifier, port = Port,
				protocol = Protocol, secret = Secret}, Acc) ->
			Id = inet:ntoa(Address),
			RespObj1 = [{"id", Id}, {"href", "/ocs/v1/client/" ++ Id}],
			RespObj2 = case Identifier of
				<<>> ->
					[];
				Identifier ->
					[{"identifier", binary_to_list(Identifier)}]
			end,
			RespObj3 = [{"port", Port},
					{"protocol", string:to_upper(atom_to_list(Protocol))},
					{"secret", Secret}],
			RespObj = {struct, RespObj1 ++ RespObj2 ++ RespObj3},
			case Filters of
				[] ->
					[RespObj | Acc];
				_ ->
					[ocs_rest:filter(["id", "href"] ++ Filters, RespObj) | Acc]
			end
	end,
	try
		JsonObj = lists:foldl(F, [], Clients),
		Body = mochijson:encode({array, lists:reverse(JsonObj)}),
		{ok, [{content_type, "application/json"}], Body}
	catch
		_:_Reason ->
			{error, 500}
	end;
get_clients1(_Clients, _Query, _Filters) ->
	{error, 400}.

-spec get_client(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client/{id}'
%% requests.
get_client(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_client(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_client(Id, Query, [])
	end.
%% @hidden
get_client(Id, [] = _Query, Filters) ->
	case inet:parse_address(Id) of
		{ok, Address} ->
			get_client1(Address, Filters);
		{error, einval} ->
			{error, 400}
	end;
get_client(_Id, _Query, _Filters) ->
	{error, 400}.
%% @hidden
get_client1(Address, Filters) ->
	case ocs:find_client(Address) of
		{ok, #client{port = Port, identifier = Identifier,
				protocol = Protocol, secret = Secret, last_modified = LM}} ->
			Id = inet:ntoa(Address),
			Etag = etag(LM),
			RespObj1 = [{"id", Id}, {"href", "/ocs/v1/client/" ++ Id}],
			RespObj2 = case Identifier of
				<<>> ->
					[];
				Identifier ->
					[{"identifier", binary_to_list(Identifier)}]
			end,
			RespObj3 = [{"port", Port},
					{"protocol", string:to_upper(atom_to_list(Protocol))},
					{"secret", Secret}],
			JsonObj  = {struct, RespObj1 ++ RespObj2 ++ RespObj3},
			Body = case Filters of
				[] ->
					mochijson:encode(JsonObj);
				_ ->
					FilteredObj = ocs_rest:filter(["id", "href"] ++ Filters, JsonObj),
					mochijson:encode(FilteredObj)
			end,
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
		RespObj = [{"id", Id}, {"href", Location}, {"port", Port},
				{"protocol", string:to_upper(atom_to_list(Protocol))}, {"secret", Secret}],
		JsonObj  = {struct, RespObj},
		Body = mochijson:encode(JsonObj),
		Headers = [{location, Location}, {etag, etag(LM)}],
		{ok, Headers, Body}
	catch
		_Error ->
			{error, 400}
	end.

-spec patch_client(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/client/{id}' request and
%% Updates a existing `client''s password or attributes.
patch_client(Id, undefined, CType, ReqBody) ->
	patch_client0(Id, undefined, CType, ReqBody);
patch_client(Id, Etag, CType, ReqBody) ->
	try
		Etag1 = etag(Etag), 
		patch_client0(Id, Etag1, CType, ReqBody)
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
patch_client0(Id, Etag, CType, ReqBody) ->
	case inet:parse_address(Id) of
		{ok, Address} ->
			patch_client1(Address, Etag, CType, ReqBody);
		{error, einval} ->
			{error, 400}
	end.
%% @hidden
patch_client1(Id, Etag, CType, ReqBody) ->
	case ocs:find_client(Id) of
		{ok, #client{port = Port, protocol = Protocol, secret = Secret,
				last_modified = CurrentEtag}}
				when CurrentEtag == Etag; Etag == undefined ->
			patch_client2(Id, Etag, CType, ReqBody, Port, Protocol, Secret);
		{ok, #client{last_modified = _NonMatchingEtag}} ->
			{error, 412};
		{error, _Reason} ->
			{error, 404}
	end.
%% @hidden
patch_client2(Id, Etag, "application/json", ReqBody, CurrPort,
		CurrProtocol, CurrSecret) ->
	try
		{struct, Object} = mochijson:decode(ReqBody),
		case Object of
			[{"secret", NewPassword}] ->
				Protocol_Atom = string:to_upper(atom_to_list(CurrProtocol)),
				patch_client3(Id, CurrPort, Protocol_Atom, NewPassword, Etag);
			[{"port", NewPort},{"protocol", RADIUS}]
					when RADIUS =:= "radius"; RADIUS =:= "RADIUS" ->
				patch_client4(Id, NewPort, radius, CurrSecret, Etag);
			[{"port", NewPort},{"protocol", DIAMETER}]
					when DIAMETER =:= "diameter"; DIAMETER =:= "DIAMETER" ->
				patch_client4(Id, NewPort, diameter, CurrSecret, Etag)
		end
	catch
		throw : _ ->
			{error, 400}
	end;
patch_client2(Id, Etag, "application/json-patch+json", ReqBody, CurrPort,
		CurrProtocol, CurrSecret) ->
	try
		{array, Operation}= mochijson:decode(ReqBody),
		case Operation of
			[{struct, [{"op", "replace"}, {"path", Path}, {"value", V}]}] ->
				case Path of
					"/secret" ->
						case V of
							V when is_list(V) ->
								Protocol_Atom = string:to_upper(atom_to_list(CurrProtocol)),
								patch_client3(Id, CurrPort, Protocol_Atom, V, Etag);
							_ ->
								{error, 422}
						end;
				_ ->
						{error, 409}
				end;
			[{struct, [{"op", "replace"}, {"path", Path1}, {"value", V1}]},
					{struct, [{"op", "replace"}, {"path", Path2}, {"value", V2}]}] ->
				case {Path1, Path2} of
					{"/port", "/protocol"} when V2 =:= "radius"; V2 =:= "RADIUS" ->
						patch_client4(Id, V1, radius, CurrSecret, Etag);
					{"/port", "/protocol"} when V2 =:= "diameter"; V2 =:= "DIAMETER" ->
						patch_client4(Id, V1, diameter, CurrSecret, Etag);
					{"/protocol", "/path"} when V1 =:= "radius"; V1 =:= "RADIUS" ->
						patch_client4(Id, V2, radius, CurrSecret, Etag);
					{"/protocol", "/path"} when V1 =:= "diameter"; V1 =:= "DIAMETER" ->
						patch_client4(Id, V2, diameter, CurrSecret, Etag);
					_ ->
						{error, 422}
				end;
			_ ->
				{error, 422}
		end
	catch
		 _: _ ->
			{error, 400}
	end.
%% @hidden
patch_client3(Id, Port, Protocol, NewPassword, Etag) ->
	IDstr = inet:ntoa(Id),
	ok = ocs:update_client(Id, NewPassword),
	RespObj =[{"id", IDstr}, {"href", "/ocs/v1/client/" ++ IDstr},
			{"port", Port}, {"protocol", Protocol}, {"secret", NewPassword}],
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
patch_client4(Id, Port, Protocol, Secret, Etag) ->
	IDstr = inet:ntoa(Id),
	Protocolstr = string:to_upper(atom_to_list(Protocol)),
	ok = ocs:update_client(Id, Port, Protocol),
	RespObj =[{"id", IDstr}, {"href", "/ocs/v1/client/" ++ IDstr},
			{"port", Port}, {"protocol", Protocolstr}, {"secret", Secret}],
	JsonObj  = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	Headers = case Etag of
		undefined ->
			[];
		_ ->
			[{etag, etag(Etag)}]
	end,
	{ok, Headers, RespBody}.

-spec delete_client(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /ocs/v1/client/{address}' request and deletes
%% a `client' resource. If the deletion is successful return true.
delete_client(Id) ->
	case inet:parse_address(Id) of
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

