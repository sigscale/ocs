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

-export([content_types_accepted/0, content_types_provided/0, get_params/0,
		get_user/1, get_user/0, post_user/1, put_user/3, delete_user/1]).

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
	{Port, Address, Directory, _Group} = get_params(),
	case mod_auth:list_users(Address, Port, Directory) of
		{error, _} ->
			{error, 500};
		{ok, Users} ->
			get_user1(Users, Address, Port, [])
	end.
%% @hidden
get_user1([H | T], Address, Port, Acc) ->
	{Port, Address, Directory, _Group} = get_params(),
	case mod_auth:get_user(H, Address, Port, Directory) of
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
			get_user1(T, Address, Port, [JsonObj | Acc]);
		{error, _Reason} ->
			get_user1(T, Address, Port, Acc)
	end;
get_user1([], _Address, _Port, Acc) ->
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
	{Port, Address, Directory, _Group} = get_params(),
	case mod_auth:get_user(Id, Address, Port, Directory) of
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
			LastModified = lists:keyfind(last_modified, 1, UserData),
			RespObj = [{"id", Id}, {"href", "/partyManagement/v1/individual/" ++ Id},
				{"characteristic", Characteristic}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			Headers = [{content_type, "application/json"}, {etag, etag(LastModified)}],
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
	{Port, Address, Directory, Group} = get_params(),
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
		LastModified = {erlang:system_time(milli_seconds),
				erlang:unique_integer([positive])},
		case mod_auth:add_user(ID, Password, [{locale, Locale},
				{last_modified, LastModified}] , Address, Port, Directory) of
			true ->
				case mod_auth:add_group_member(Group, ID, Address, Port, Directory) of
					true ->
						Location = "/partyManagement/v1/individual/" ++ ID,
						PasswordAttr = {struct, [{"name", "password"}, {"value", Password}]},
						LocaleAttr = {struct, [{"name", "locale"}, {"value", Locale}]},
						Char = {array, [PasswordAttr, LocaleAttr]},
						RespObj = [{"id", ID}, {"href", Location}, {"characteristic", Char}],
						JsonObj  = {struct, RespObj},
						Body = mochijson:encode(JsonObj),
						Headers = [{location, Location}, {etag, etag(LastModified)}],
						{ok, Headers, Body}
				end;
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec put_user(ID, Etag, RequestBody) -> Result
	when
		ID :: string(),
		Etag :: undefined | string(),
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Respond to `PUT /partyManagement/v1/individual' and Update a `User'
%% resource.
put_user(ID, undefined, RequestBody) ->
	put_user1(ID, undefined, RequestBody);
put_user(ID, Etag, RequestBody) ->
	try
		Etag1 = etag(Etag),
		put_user1(ID, Etag1, RequestBody)
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
put_user1(ID, Etag, RequestBody) ->
	{Port, _Address, Directory, _Group} = get_params(),
	try
		{struct, Object} = mochijson:decode(RequestBody),
		{_, ID} = lists:keyfind("id", 1, Object),
		{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
		F1 = fun(_F, [{struct, [{"name", "password"}, {"value", Password}]} | _]) ->
					Password;
				(_F, [{struct, [{"value", Password}, {"name", "password"}]} | _]) ->
					Password;
				(F, [_ | T]) ->
					F(F,T);
				(_F, []) ->
					undefined
		end,
		Password = F1(F1, Characteristic),
		F2 = fun(_F, [{struct, [{"name", "locale"}, {"value", Locale}]} | _]) ->
					Locale;
				(_F, [{struct, [{"value", Locale}, {"name", "locale"}]} | _]) ->
					Locale;
				(F, [_ | T]) ->
					F(F,T);
				(_F, []) ->
					undefined
		end,
		Locale = F2(F2, Characteristic),
		case {Password, Locale} of
			{undefined, undefined} ->
				{error, 400};
			{Password, Locale} when is_list(Password) ->
				put_user2(ID, Etag, Password, Locale);
			{undefined, Locale} when is_list(Locale) ->
				case mod_auth:get_user(ID, Port, Directory) of
					{ok, #httpd_user{password = OPassword}} ->
						put_user2(ID, Etag, OPassword, Locale);
					{error, no_such_user} ->
						{error, 404};
					{error, _Reason} ->
						{error, 500}
				end
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.
%% @hidden
put_user2(ID, Etag, Password, undefined) ->
	put_user2(ID, Etag, Password, "en");
put_user2(ID, Etag, Password, Locale) ->
	{Port, Address, Directory, _} = get_params(),
	case mod_auth:get_user(ID, Address, Port, Directory) of
		{ok, #httpd_user{user_data = Data}} ->
			case lists:keyfind(last_modified, 1, Data) of
				{_, LastModified} when Etag == undefined; Etag == LastModified ->
				put_user3(ID, Password, Locale);
				{_, _NonEtagMatch} ->
					{error, 412};
				false ->
					{error, 500}
			end;
		{error, _} ->
			{error, 404}
	end.
%% @hidden
put_user3(ID, Password, Locale) ->
	{Port, Address, Directory, _Group} = get_params(),
	case mod_auth:delete_user(ID, Address, Port, Directory) of
		true ->
			case mod_auth:add_user(ID, Password, [{locale, Locale}] , Address, Port, Directory) of
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
					{error, 500}
			end;
		{error, _Reason} ->
			{error, 500}
	end.

-spec delete_user(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /ocs/v1/subscriber/{id}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
delete_user(Id) ->
	{Port, Address, Directory, _Group} = get_params(),
	case mod_auth:delete_user(Id, Address, Port, Directory) of
		true ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.        

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec get_params() -> {Port :: integer(), Address :: string(), Directory :: string(),
		Group :: string()}.
get_params() ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	case lists:keyfind(require_group, 1, AuthObj) of
		{require_group, [Group | _T]} ->
			{Port, Address, Directory, Group};
		false ->
			exit(not_found)
	end.

-spec etag(V1) -> V2
	when
		V1 :: {N1, N2} | string(),
		V2 :: string() | {N1, N2},
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

