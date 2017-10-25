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
		get_user/2, get_users/2, post_user/1, patch_user/4,
		delete_user/1, user/1]).

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
		{ok, #httpd_user{user_data = UserData} = User} ->
			{struct, UserObjectWithPwd} = user(User),
			{_, {array, Chars}} = lists:keyfind("characteristic", 1, UserObjectWithPwd),
			F = fun({struct, Obj2}) ->
					case lists:keyfind("name", 1, Obj2) of
						{_, "password"} ->
							false;
						_ ->
							true
					end
			end,
			CharObj = {"characteristic", {array, lists:filter(F, Chars)}},
			NewChars = lists:keyreplace("characteristic", 1, UserObjectWithPwd, CharObj),
			UserObject = {struct, NewChars},
			Headers1 = case lists:keyfind(last_modified, 1, UserData) of
				{_, LastModified} ->
					[{etag, ocs_rest:etag(LastModified)}];
				false ->
					[]
			end,
			Headers2 = [{content_type, "application/json"} | Headers1],
			Body = mochijson:encode(UserObject),
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
		User = user(mochijson:decode(RequestBody)),
		{Username, _, _, _} = User#httpd_user.username,
		Password = User#httpd_user.password,
		Locale = case lists:keyfind(locale, 1, User#httpd_user.user_data) of
			{_, Loc} ->
				Loc;
			false ->
				"en"
		end,
		case ocs:add_user(Username, Password, Locale) of
			{ok, LastModified} ->
				Body = mochijson:encode(user(User)),
				Location = "/partyManagement/v1/individual/" ++ Username,
				Headers = [{location, Location}, {etag, ocs_rest:etag(LastModified)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec patch_user(ID, Etag, ContentType, ReqBody) -> Result
	when
		ID :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /partyManagement/v1/individual/{id}' request and
%% update an existing `user''s characteristics.
patch_user(ID, Etag, "application/json-patch+json", ReqBody) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		{Etag1, mochijson:decode(ReqBody)}
	of
		{Etag2, {array, Operations}} ->
			{Port, Address, Directory, _Group} = get_params(),
			Username = {ID, Address, Port, Directory},
			F = fun() ->
					case mnesia:read(httpd_user, Username, write) of
						[#httpd_user{user_data = UserData1} = User1] ->
							case lists:keyfind(last_modified, 1, UserData1) of
								{_, Etag3} when Etag3 == Etag2; Etag3 == undefined ->
									case catch ocs_rest:patch(Operations, user(User1)) of
										#httpd_user{user_data = UserData2} = User2 ->
											TS = erlang:system_time(?MILLISECOND),
											N = erlang:unique_integer([positive]),
											UserData3 = [{etag, {TS, N}} | UserData2],
											User3 = User2#httpd_user{user_data = UserData3},
											ok = mnesia:write(User3),
											User3;
										_Reason ->
											throw(bad_request)
									end;
								_ ->
									throw(precondition_failed)
							end;
						[] ->
							throw(not_found)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, #httpd_user{user_data = UserData4}} ->
					case lists:keyfind(etag, 1, UserData4) of
						{error, _} ->
							{error, 500};
						{_, Etag4} ->
							Location = "/partyManagement/v1/individual/" ++ ID,
							Headers = [{location, Location}, {etag, ocs_rest:etag(Etag4)}],
							{ok, Headers, []}
					end;
				{aborted, {throw, bad_request}} ->
					{error, 400};
				{aborted, {throw, not_found}} ->
					{error, 404};
				{aborted, {throw, precondition_failed}} ->
					{error, 412}
			end
	catch
		_:_ ->
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

-spec get_params() -> Result
	when
		Result :: {Port, Address, Directory, Group},
		Port :: integer(),
		Address :: string(),
		Directory :: string(),
		Group :: string().
%% @doc Get {@link //inets/httpd. httpd} configuration parameters.
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

-spec user(User) -> User
	when
		User :: #httpd_user{} | {struct, [tuple()]}.
%% @doc CODEC for HTTP server users.
user(#httpd_user{username = {ID, _, _, _}} = HttpdUser) ->
	user(HttpdUser#httpd_user{username  = ID});
user(#httpd_user{username = ID, password = Password, user_data = Characteristic}) ->
	C1 = [{struct, [{"name", "username"}, {"value", ID}]},
			{struct, [{"name", "password"}, {"value", Password}]}],
	C2 = case lists:keyfind(locale, 1, Characteristic) of
		{_, Locale} ->
			[{struct, [{"name", "locale"}, {"value", Locale}]} | C1];
		false ->
			C1
	end,
	{struct, [{"id", ID},
				{"href", "/partyManagement/v1/individual/" ++ ID},
				{"characteristic", {array, C2}}]};
user({struct, L}) when is_list(L) ->
	user(L, #httpd_user{user_data = []}).
%% @hidden
user([{"id", ID} | T], Acc) when is_list(ID) ->
	{Port, Address, Directory, _Group} = get_params(),
	Username = {ID, Address, Port, Directory},
	user(T, Acc#httpd_user{username = Username});
user([{"href", Href} | T], Acc) when is_list(Href) ->
	user(T, Acc);
user([{"characteristic", {array, Chars}} | T], Acc) when is_list(Chars) ->
	user(T, user1(Chars, Acc));
user([], Acc) ->
	Acc.
%% @hidden
user1([{struct, [{"name", "username"}, {"value", Username}]} | T], Acc)
		when is_list(Username) ->
	{Port, Address, Directory, _Group} = get_params(),
	user1(T, Acc#httpd_user{username = {Username, Address, Port, Directory}});
user1([{struct, [{"value", Username}, {"name", "username"}]} | T], Acc)
		when is_list(Username) ->
	{Port, Address, Directory, _Group} = get_params(),
	user1(T, Acc#httpd_user{username = {Username, Address, Port, Directory}});
user1([{struct, [{"name", "password"}, {"value", Password}]} | T], Acc)
		when is_list(Password) ->
	user1(T, Acc#httpd_user{password = Password});
user1([{struct, [{"value", Password}, {"name", "password"}]} | T], Acc)
		when is_list(Password) ->
	user1(T, Acc#httpd_user{password = Password});
user1([{struct, [{"value", Locale}, {"name", "locale"}]} | T],
		#httpd_user{user_data = Data} = Acc) when is_list(Locale) ->
	user1(T, Acc#httpd_user{user_data = [{locale, Locale} | Data]});
user1([{struct, [{"name", "locale"}, {"value", Locale}]} | T],
		#httpd_user{user_data = Data} = Acc) when is_list(Locale) ->
	user1(T, Acc#httpd_user{user_data = [{locale, Locale} | Data]});
user1([], Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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
					JsonObj = query_page1(lists:map(fun user/1, SortedEvents), Filters, []),
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
query_page1([], Filters, Acc) ->
	query_page2(Acc, Filters, []);
query_page1([{struct, Object} | T], Filters, Acc) ->
	{_, {array, Chars}} = lists:keyfind("characteristic", 1, Object),
	F = fun({struct, Obj}) ->
			case lists:keyfind("name", 1, Obj) of
				{_, "password"} ->
					false;
				_ ->
					true
			end
	end,
	CharObj = {"characteristic", {array, lists:filter(F, Chars)}},
	NewChars = lists:keyreplace("characteristic", 1, Object, CharObj),
	query_page1(T, Filters, [{struct, NewChars} | Acc]).
%% @hidden
query_page2([], _, Acc) ->
	lists:reverse(Acc);
query_page2(Json, [], Acc) ->
	lists:reverse(Json ++ Acc);
query_page2([H | T], Filters, Acc) ->
	query_page2(T, Filters, [ocs_rest:filter(Filters, H) | Acc]).

