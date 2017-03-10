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
				perform_get/1,
				perform_get_all/0,
				perform_post/1,
				perform_patch/2,
				perform_delete/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-define(VendorID, 529).
-define(AscendDataRate, 197).
-define(AscendXmitRate, 255).

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
	["application/json", "application/hal+json"].

-spec perform_get(Ip) -> Result
	when
		Ip :: string(),
		Result :: {body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client/{id}'
%% requests.
perform_get(Ip) ->
	case inet:parse_address(Ip) of
		{ok, Address} ->
			perform_get1(Address);
		{error, einval} ->
			{error, 400}
	end.
%% @hidden
perform_get1(Address) ->
	case ocs:find_client(Address) of
		{ok, DiscPort, Protocol, Secret} ->
			Id = inet:ntoa(Address),
			RespObj = [{id, Id}, {href, "/ocs/v1/client/" ++ Id},
					{"disconnectPort", DiscPort},
					{protocol, string:to_upper(atom_to_list(Protocol))}, {secret, Secret}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			{body, Body};
		{error, not_found} ->
			{error, 404}
	end.

-spec perform_get_all() -> Result 
	when
		Result ::{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/client'
%% requests.
perform_get_all() ->
	case ocs:get_clients() of
		{error, _} ->
			{error, 404};
				Clients ->
				Response = perform_get_all1(Clients),
				Body  = mochijson:encode(Response),
			{body, Body}
	end.
%% @hidden
perform_get_all1(Clients) ->
	F = fun(#client{address= Address, disconnect_port = DiscPort,
			protocol = Protocol, secret = Secret}, Acc) ->
		Id = inet:ntoa(Address),
		RespObj = [{struct, [{id, Id}, {href, "/ocs/v1/client/" ++ Id},
			{"disconnectPort", DiscPort},
			{protocol, string:to_upper(atom_to_list(Protocol))}, {secret, Secret}]}],
		RespObj ++ Acc
	end,
	JsonObj = lists:foldl(F, [], Clients),
	{array, JsonObj}.

-spec perform_post(RequestBody) -> Result 
	when
		RequestBody :: list(),
		Result :: {Location :: string(), Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/client' and add a new `client'
%% resource.
perform_post(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		{_, Id} = lists:keyfind("id", 1, Object),
		{_, DiscPort} = lists:keyfind("disconnectPort", 1, Object),
		{_, Protocol} = lists:keyfind("protocol", 1, Object),
		{_, Secret} = lists:keyfind("secret", 1, Object),
		Protocol_Atom = list_to_atom(string:to_lower(Protocol)),
		perform_post1(Id, DiscPort, Protocol_Atom, Secret)
	catch
		_Error ->
			{error, 400}
	end.
%% @hidden
perform_post1(Id, DiscPort, Protocol, Secret) ->
	try
	case catch ocs:add_client(Id, DiscPort, Protocol, Secret) of
		ok ->
			Location = "/ocs/v1/client/" ++ Id,
			RespObj = [{id, Id}, {href, Location}, {"disconnectPort", DiscPort},
					{protocol, string:to_upper(atom_to_list(Protocol))}, {secret, Secret}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			{Location, Body};
		{error, _Reason} ->
			{error, 400}
	end catch
		throw:_ ->
			{error, 400}
	end.

-spec perform_patch(Id, ReqBody) -> Result 
	when
		Id :: list(),
		ReqBody :: list(),
		Result :: {body, Body :: iolist()} | {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/client/{id}' request and
%% Updates a existing `client''s password or attributes.
perform_patch(Id, ReqBody) ->
	{ok, Address} = inet:parse_address(Id),
	case ocs:find_client(Address) of
		{ok, CurrDiscPort, CurrProtocol, CurrSecret} ->
			try
				{struct, Object} = mochijson:decode(ReqBody),
				case Object of
					[{"secret", NewPassword}] ->
						Protocol_Atom = string:to_upper(atom_to_list(CurrProtocol)),
						perfrom_patch1(Id, CurrDiscPort, Protocol_Atom, NewPassword);
					[{"disconnectPort", NewDiscPort},{"protocol", NewProtocol}] ->
						NewProtocolAtom = list_to_atom(string:to_lower(NewProtocol)),
						perform_patch2(Id, NewDiscPort, NewProtocolAtom, CurrSecret)
				end
			catch
				throw : _ ->
					{error, 400}
			end;
		{error, _Reason} ->
			{error, 404}
	end.
%% @hidden
perfrom_patch1(Id, DiscPort, Protocol, NewPassword) ->
	ok = ocs:update_client_password(Id, NewPassword),
	RespObj =[{id, Id}, {href, "/ocs/v1/client/" ++ Id},
			{"disconnectPort", DiscPort}, {protocol, Protocol}, {secret, NewPassword}],
	JsonObj  = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	{body, RespBody}.

%% @hidden
perform_patch2(Id, DiscPort, Protocol, Secret) ->
	ok = ocs:update_client_attributes(Id, DiscPort, Protocol),
	RespObj =[{id, Id}, {href, "/ocs/v1/client/" ++ Id},
			{"disconnectPort", DiscPort}, {protocol, Protocol}, {secret, Secret}],
	JsonObj  = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	{body, RespBody}.

-spec perform_delete(Ip) -> ok 
	when
		Ip :: list().
%% @doc Respond to `DELETE /ocs/v1/client/{address}' request and deletes
%% a `client' resource. If the deletion is succeeded return true.
perform_delete(Ip) ->
	{ok, Address} = inet:parse_address(Ip), 
	ok = ocs:delete_client(Address),
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

