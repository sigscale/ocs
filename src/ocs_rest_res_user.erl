%%% ocs_rest_res_user.erl
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
-module(ocs_rest_res_user).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_user/1, get_user/0, post_user/1, put_user/1, delete_user/1]).

-include_lib("radius/include/radius.hrl").
-include_lib("inets/include/mod_auth.hrl").
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

-spec get_user() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /partyManagement/v1/individual'
%% requests.
get_user() ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	case mod_auth:list_users(Port, "/") of
		{error, _} ->
			{error, 500};
		{ok, Users} ->
			get_user1(Users, Port, [])
	end.
%% @hidden
get_user1([H | T], Port, Acc) ->
	case mod_auth:get_user(H, Port, "/") of
		{ok, #httpd_user{username = Id, user_data = UserData}} ->
			Identity = {struct, [{"name", "username"}, {"value", Id}]},
			Characteristic = case lists:keyfind(locale, 1, UserData) of
				{_, Lang} ->
					LocaleAttr = {struct, [{"name", "locale"}, {"value", Lang}]},
					{array, [LocaleAttr, Identity]};
				false ->
					{array, []}
			end,
			RespObj = [{"id", Id}, {"href", "/partyManagement/v1/individual/" ++ Id},
				{"characteristic", Characteristic}],
			JsonObj  = {struct, RespObj},
			get_user1(T, Port, [JsonObj | Acc]);
		{error, _Reason} ->
			get_user1(T, Port, Acc)
	end;
get_user1([], Port, Acc) ->
	Body = mochijson:encode({array, lists:reverse(Acc)}),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body}.

-spec get_user(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /partyManagement/v1/individual/{id}'
%% requests.
get_user(Id) ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	case mod_auth:get_user(Id, Port, "/") of
		{ok, #httpd_user{username = Id, password = Password, user_data = UserData}} ->
			Identity = {struct, [{"name", "username"}, {"value", Id}]},
			PasswordAttr = {struct, [{"name", "password"}, {"value", Password}]},
			Characteristic = case lists:keyfind(locale, 1, UserData) of
				{_, Lang} ->
					LocaleAttr = {struct, [{"name", "locale"}, {"value", Lang}]},
					{array, [PasswordAttr, LocaleAttr, Identity]};
				false ->
					{array, [PasswordAttr]}
			end,
			RespObj = [{"id", Id}, {"href", "/partyManagement/v1/individual/" ++ Id},
				{"characteristic", Characteristic}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end.

-spec post_user(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /partyManagement/v1/individual' and add a new `User'
%% resource.
post_user(RequestBody) ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	try
		{struct, Object} = mochijson:decode(RequestBody),
		{_, ID} = lists:keyfind("id", 1, Object),
		{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
			F1 = fun(_F, [{struct, [{"name", "password"}, {"value", Pass}]} | _]) ->
						Pass;
					(_F, [{struct, [{"value", Pass}, {"name", "password"}]} | _]) ->
						Pass;
					(F, [_ | T]) ->
						F(F,T)
			end,
		Password = F1(F1, Characteristic),
			F2 = fun(_F, [{struct, [{"name", "locale"}, {"value", Locale}]} | _]) ->
						Locale;
					(_F, [{struct, [{"value", Locale}, {"name", "locale"}]} | _]) ->
						Locale;
					(F, [_ | T]) ->
						F(F,T)
			end,
		Locale = F2(F2, Characteristic),
		case mod_auth:add_user(ID, Password, [{locale, Locale}] , Port, "/") of
			true ->
				Location = "/partyManagement/v1/individual/" ++ ID,
				PasswordAttr = {struct, [{"name", "password"}, {"value", Password}]},
				LocaleAttr = {struct, [{"name", "locale"}, {"value", Locale}]},
				Char = {array, [PasswordAttr, LocaleAttr]},
				RespObj = [{"id", ID}, {"href", Location}, {"characteristic", Char}],
				JsonObj  = {struct, RespObj},
				Body = mochijson:encode(JsonObj),
				Headers = [{location, Location}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec put_user(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Respond to `PUT /partyManagement/v1/individual' and Update a `User'
%% resource.
put_user(RequestBody) ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	try
		{struct, Object} = mochijson:decode(RequestBody),
		{_, ID} = lists:keyfind("id", 1, Object),
		{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
			F1 = fun(_F, [{struct, [{"name", "password"}, {"value", Pass}]} | _]) ->
						Pass;
					(_F, [{struct, [{"value", Pass}, {"name", "password"}]} | _]) ->
						Pass;
					(F, [_ | T]) ->
						F(F,T)
			end,
		Password = F1(F1, Characteristic),
			F2 = fun(_F, [{struct, [{"name", "locale"}, {"value", Locale}]} | _]) ->
						Locale;
					(_F, [{struct, [{"value", Locale}, {"name", "locale"}]} | _]) ->
						Locale;
					(F, [_ | T]) ->
						F(F,T)
			end,
		Locale = F2(F2, Characteristic),
		case mod_auth:delete_user(ID, Port, "/") of
			true ->
				case mod_auth:add_user(ID, Password, [{locale, Locale}] , Port, "/") of
					true ->
						Location = "/partyManagement/v1/individual/" ++ ID,
						PasswordAttr = {struct, [{"name", "password"}, {"value", Password}]},
						LocaleAttr = {struct, [{"name", "locale"}, {"value", Locale}]},
						Char = {array, [PasswordAttr, LocaleAttr]},
						RespObj = [{"id", ID}, {"href", Location}, {"characteristic", Char}],
						JsonObj  = {struct, RespObj},
						Body = mochijson:encode(JsonObj),
						Headers = [{location, Location}],
						{ok, Headers, Body};
					{error, _Reason} ->
						{error, 400}
				end;
			{error, _Reason} ->
				throw(delete)
		end
	catch
		throw:delete ->
erlang:display({?MODULE, ?LINE}),
			{error, 500};
		_:_Reason1 ->
erlang:display({?MODULE, ?LINE}),
			{error, 400}
	end.

-spec delete_user(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /ocs/v1/subscriber/{id}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
delete_user(Id) ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	case mod_auth:delete_user(Id, Port, "/") of
		true ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.        

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

