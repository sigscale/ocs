%%% ocs_rest_res_user.erl
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
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_user).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, get_params/0,
		get_user/2, get_users/2, post_user/1, put_user/3, patch_user/4,
		delete_user/1]).

-include_lib("inets/include/mod_auth.hrl").
-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

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

-spec get_users(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /partyManagement/v1/individual'
%% requests.
get_users(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_users1(NewQuery, Filters, Headers);
		false ->
			get_users1(Query, [], Headers)
	end.
%% @hidden
get_users1(Query, Filters, Headers) ->
	case {lists:keyfind("if-match", 1, Headers),
			lists:keyfind("if-range", 1, Headers),
			lists:keyfind("range", 1, Headers)} of
		{{"if-match", Etag}, false, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(Query, Filters, Start, End)
					end;
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", Range}} ->
			case ocs_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(Query, Filters, undefined, undefined)
	end.

-spec get_user(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /partyManagement/v1/individual/{id}'
%% requests.
get_user(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_user(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_user(Id, Query, [])
	end.
%% @hidden
get_user(Id, [] = _Query, _Filters) ->
	case ocs:get_user(Id) of
		{ok, #httpd_user{user_data = UserData}} ->
			Characteristic1 = case lists:keyfind(locale, 1, UserData) of
				{_, Lang} ->
					[{struct, [{"name", "locale"}, {"value", Lang}]}];
				false ->
					[{struct, [{"name", "locale"}, {"value", "en"}]}]
			end,
			Characteristic2 = [{struct, [{"name", "username"},
					{"value", Id}]} | Characteristic1],
			User = {struct, [{"id", Id},
					{"href", "/partyManagement/v1/individual/" ++ Id},
					{"characteristic", {array, Characteristic2}}]},
			Headers1 = case lists:keyfind(last_modified, 1, UserData) of
				{_, LastModified} ->
					[{etag, etag(LastModified)}];
				false ->
					[]
			end,
			Headers2 = [{content_type, "application/json"} | Headers1],
			Body = mochijson:encode(User),
			{ok, Headers2, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_user(_, _, _) ->
	{error, 400}.

-spec post_user(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /partyManagement/v1/individual' and add a new `User'
%% resource.
post_user(RequestBody) ->
	try
		{struct, Object} = mochijson:decode(RequestBody),
		{_, ID} = lists:keyfind("id", 1, Object),
		{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
		Filter = fun(Filter, [{struct, FilterObj} | T], ObjName) ->
							case lists:keyfind("name", 1, FilterObj) of
									{_, ObjName} ->
										proplists:get_value("value", FilterObj);
									_ ->
										Filter(Filter, T, ObjName)
							end;
						(_, [], _) ->
							undefined
		end,
		Password = Filter(Filter, Characteristic, "password"),
		Locale = Filter(Filter, Characteristic, "locale"),
		case ocs:add_user(ID, Password, Locale) of
			{ok, LastModified} ->
				Location = "/partyManagement/v1/individual/" ++ ID,
				IDAttr = {struct, [{"name", "username"}, {"value", ID}]},
				PWDAttr = {struct, [{"name", "password"}, {"value", Password}]},
				LocaleAttr = {struct, [{"name", "locale"}, {"value", Locale}]},
				Char = {array, [IDAttr, PWDAttr, LocaleAttr]},
				RespObj = [{"id", ID}, {"href", Location}, {"characteristic", Char}],
				JsonObj  = {struct, RespObj},
				Body = mochijson:encode(JsonObj),
				Headers = [{location, Location}, {etag, etag(LastModified)}],
				{ok, Headers, Body};
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
put_user(ID, Etag, RequestBody) ->
	try
		{struct, Object} = mochijson:decode(RequestBody),
		{_, ID} = lists:keyfind("id", 1, Object),
		{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
		Filter = fun(Filter, [{struct, FilterObj} | T], ObjName) ->
							case lists:keyfind("name", 1, FilterObj) of
									{_, ObjName} ->
										proplists:get_value("value", FilterObj);
									_ ->
										Filter(Filter, T, ObjName)
							end;
						(_, [], _) ->
							undefined
		end,
		Password = Filter(Filter, Characteristic, "password"),
		Locale = Filter(Filter, Characteristic, "locale"),
		put_user1(ID, Password, Locale, Etag)
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
put_user1(Id, Password, Locale, Etag) when is_list(Etag) ->
	put_user1(Id, Password, Locale, etag(Etag));
put_user1(Id, Password, Locale, Etag) ->
	try
		case ocs:get_user(Id) of
			{ok, #httpd_user{user_data = UD}} ->
				case lists:keyfind(last_modified, 1, UD) of
					{_, LM} when Etag =:= undefined; Etag == LM ->
						case ocs:update_user(Id, Password, Locale) of
							{ok, NLM} ->
								put_user2(Id, Password, Locale, NLM);
							{error, _} ->
								{error, 500}
						end;
					{_, _} ->
						{error, 412};
					false ->
						{error, 500}
				end;
			{error, no_such_user} ->
				{error, 400};
			{error, _} ->
				{error, 500}
		end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
put_user2(ID, Password, Locale, LM) when is_tuple(LM) ->
	put_user2(ID, Password, Locale, etag(LM));
put_user2(ID, Password, Locale, LM) ->
	try
		Location = "/partyManagement/v1/individual/" ++ ID,
		PwdObj = {struct, [{"name", "password"}, {"value", Password}]},
		LocaleObj = {struct, [{"name", "locale"}, {"value", Locale}]},
		Characteristic = {array, [PwdObj, LocaleObj]},
		RespObj = [{"id", ID}, {"href", Location},
				{"characteristic", Characteristic}],
		JsonObj  = {struct, RespObj},
		Body = mochijson:encode(JsonObj),
		Headers = [{location, Location}, {etag, LM}],
		{ok, Headers, Body}
	catch
		_:_ ->
			{error, 500}
	end.

-spec patch_user(ID, Etag, ContenType, ReqBody) -> Result
	when
		ID :: string(),
		Etag :: undefined | list(),
		ContenType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /partyManagement/v1/individual/{id}' request and
%% update an existing `user's characteristics.
patch_user(ID, undefined, CType, ReqBody) ->
	patch_user1(ID, undefined, CType, ReqBody);
patch_user(ID, Etag, CType, ReqBody) ->
	try
		Etag1 = etag(Etag),
		patch_user1(ID, Etag1, CType, ReqBody)
	catch
		_:_Reason ->
			{error, 400}
	end.
%% @hidden
patch_user1(ID, Etag, "application/json-patch+json", ReqBody) ->
	try
		{array, Ops} = mochijson:decode(ReqBody),
		case process_json_patch(Ops, ID) of
			{error, Code} ->
				{error, Code};
			{ID, Password, Locale, LM} when Etag == LM; Etag == undefined ->
				case ocs:update_user(ID, Password, Locale) of
					{ok, LastModified} ->
						Location = "/partyManagement/v1/individual/" ++ ID,
						Headers = [{location, Location}, {etag, etag(LastModified)}],
						{ok, Headers, []};
					{error, _} ->
						{error, 500}
				end;
			{_, _, _, _} ->
				{error, 412}
		end
	catch
		_:_Reason ->
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
	case ocs:delete_user(Id) of
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

-spec process_json_patch(Operations, ID) -> Result
	when
		Operations :: [{struct, OpObject}],
		ID :: string(),
		OpObject :: [{Key, Value}],
		Key :: term(),
		Value :: term(),
		Result :: {Username, Password, Locale, LastModified} | {error, Code},
		Username :: string(),
		Password :: string(),
		Locale :: string(),
		LastModified :: {integer(), integer()},
		Code :: integer().
%% @doc Process a json-patch document and return list of characteristics
%% for a user .
%% @hidden
process_json_patch(Ops, ID) ->
	process_json_patch1(ocs_rest:parse(Ops), ID, []).
%% @hidden
process_json_patch1([{replace, ["characteristic" | _], {struct, Obj}} | T], ID, Acc) ->
	case lists:keyfind("name", 1, Obj) of
		{_, "locale"} ->
			{_, Locale} = lists:keyfind("value", 1, Obj),
			process_json_patch1(T, ID, [{locale, Locale} | Acc]);
		false ->
			{error, 400}
	end;
process_json_patch1([{add, ["characteristic" | _], {struct, Obj}} | T], ID, Acc) ->
	case lists:keyfind("name", 1, Obj) of
		{_, "password"} ->
			{_, Password} = lists:keyfind("value", 1, Obj),
			process_json_patch1(T, ID, [{password, Password} | Acc]);
		false ->
			{error, 400}
	end;
process_json_patch1([{_, _, _} | _], _, _) ->
	{error, 422};
process_json_patch1([], ID, Acc) ->
	case ocs:get_user(ID) of
		{ok, #httpd_user{password = OPassword, user_data = UserData}} ->
			LastModified = case lists:keyfind(last_modified, 1, UserData) of
				{_, LM} ->
					LM;
				false ->
					{erlang:system_time(?MILLISECOND), erlang:unique_integer([positive])}
			end,
			OLocale = case lists:keyfind(locale, 1, UserData) of
				{_, Loc} ->
					Loc;
				false ->
					"en"
			end,
			Password = case lists:keyfind(password, 1, Acc) of
				false ->
					OPassword;
				{_, NewPassword} ->
					NewPassword
			end,
			Locale = case lists:keyfind(locale, 1, Acc) of
				false ->
					OLocale;
				{_, NewLocale} ->
					NewLocale
			end,
			{ID, Password, Locale, LastModified};
		{error, no_such_user} ->
			{error, 404};
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	Id =  proplists:get_value("id", Query),
	Locale =  proplists:get_value("locale", Query),
	case supervisor:start_child(ocs_rest_pagination_sup,
				[[ocs, query_users, [Id, Locale]]]) of
		{ok, PageServer, Etag} ->
			query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->

			{error, 500}
	end.

%% @hidden
query_page(PageServer, Etag, Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			try
				case lists:keytake("sort", 1, Query) of
					{value, {_, "id"}, Q1} ->
						{lists:keysort(#httpd_user.username, Events), Q1};
					{value, {_, "-id"}, Q1} ->
						{lists:reverse(lists:keysort(#httpd_user.username, Events)), Q1};
					false ->
						{Events, Query};
					_ ->
						throw(400)
				end
			of
				{SortedEvents, _NewQuery} ->
					JsonObj = query_page1(lists:map(fun user_body/1, SortedEvents), Filters, []),
					JsonArray = {array, JsonObj},
					Body = mochijson:encode(JsonArray),
					Headers = [{content_type, "application/json"},
							{etag, Etag}, {accept_ranges, "items"},
							{content_range, ContentRange}],
					{ok, Headers, Body}
			catch
				throw:{error, Status} ->
					{error, Status}
			end
	end.
%% @hidden
query_page1([], _, Acc) ->
	lists:reverse(Acc);
query_page1(Json, [], Acc) ->
	lists:reverse(Json ++ Acc);
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:filter(Filters, H) | Acc]).

%% @hidden
user_body(#httpd_user{username = {User, _, _, _}, user_data = Characteristic}) ->
	C1 = [{struct, [{"name", "username"}, {"value", User}]}],
	C2 = case lists:keyfind(locale, 1, Characteristic) of
		{_, Locale} ->
			[{struct, [{"name", "locale"}, {"value", Locale}]} | C1];
		false ->
			C1
	end,
	{struct, [{"id", User},
				{"href", "/partyManagement/v1/individual/" ++ User},
				{"characteristic", {array, C2}}]}.

