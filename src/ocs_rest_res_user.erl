%%% ocs_rest_res_user.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2024 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, get_params/0,
		get_user/2, get_users/2, head_user/0, post_user/1, patch_user/4,
		delete_user/1, user/1]).

-include_lib("radius/include/radius.hrl").
-include_lib("inets/include/mod_auth.hrl").
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
	["application/json", "application/problem+json"].

-spec head_user() -> Result
	when
		Result :: {ok, [], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`HEAD /partyManagement/v1/individual'
%% 	requests.
head_user() ->
	try
		Size = mnesia:table_info(httpd_user, size),
		LastItem = integer_to_list(Size),
		ContentRange = "items 1-" ++ LastItem ++ "/" ++ LastItem,
		Headers = [{content_range, ContentRange}],
		{ok, Headers, []}
	catch
		_:_Reason ->
			{error, 500}
	end.

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
			Headers = case lists:keyfind(last_modified, 1, UserData) of
				{_, LastModified} ->
					[{content_type, "application/json"}, {etag, ocs_rest:etag(LastModified)}];
				false ->
					[{content_type, "application/json"}]
			end,
			Body = mochijson:encode(UserObject),
			{ok, Headers, Body};
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
		UserData = User#httpd_user.user_data,
		case ocs:add_user(Username, Password, UserData) of
			{ok, LastModified} ->
				Body = mochijson:encode(user(User)),
				Location = "/partyManagement/v1/individual/" ++ Username,
				Headers = [{content_type, "application/json"},
						{location, Location}, {etag, ocs_rest:etag(LastModified)}],
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
		{Etag2, Operations} ->
			case ocs:get_user(ID) of
				{ok, #httpd_user{user_data = UserData1} = User1} ->
					case lists:keyfind(last_modified, 1, UserData1) of
						{_, Etag3} when Etag3 == Etag2; Etag2 == undefined; Etag3 == undefined ->
							case catch ocs_rest:patch(Operations, user(User1)) of
								{struct, _} = Result ->
									#httpd_user{user_data = UserData2,
										password = Password} = user(Result),
									case catch ocs:update_user(ID, Password, UserData2) of
										{ok, Etag4} ->
											Location = "/partyManagement/v1/individual/" ++ ID,
											Headers = [{content_type, "application/json"},
													{location, Location}, {etag, ocs_rest:etag(Etag4)}],
											{ok, Headers, []};
										{error, _} ->
											{error, 500}
									end;
								_ ->
									{error, 400}
							end;
						_ ->
							{error, 412}
					end;
				{error, _Reason} ->
					{error, 400}
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
%% @doc	Respond to `DELETE /partyManagement/v1/individual/{id}' request and
%% 	delete an existing `user'.
delete_user(Id) ->
	case ocs:delete_user(Id) of
		ok ->
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
	F = fun({directory, {Directory, Auth}})
			when is_list(Auth), length(Auth) > 0 ->
				case lists:keyfind(require_group, 1, Auth) of
					{require_group, [Group | _T]} ->
						{true, {Port, Address, Directory, Group}};
					false ->
						false
				end;
			(_) ->
				false
	end,
	case lists:filtermap(F, HttpdObj) of
		[{Port, Address, Directory, Group}] ->
			{Port, Address, Directory, Group};
		[] ->
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
	C3 = case lists:keyfind(rating, 1, Characteristic) of
		{_, Rating} ->
			[{struct, [{"name", "rating"}, {"value", Rating}]} | C1];
		false ->
			C2
	end,
	{struct, [{"id", ID},
				{"href", "/partyManagement/v1/individual/" ++ ID},
				{"characteristic", {array, C3}}]};
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
user1([{struct, [{"name", "password"}, {"value", Password}]} | T], Acc)
		when is_list(Password) ->
	user1(T, Acc#httpd_user{password = Password});
user1([{struct, [{"name", "locale"}, {"value", Locale}]} | T],
		#httpd_user{user_data = Data} = Acc) when is_list(Locale) ->
	user1(T, Acc#httpd_user{user_data = [{locale, Locale} | Data]});
user1([{struct, [{"name", "rating"}, {"value", Rating}]} | T],
		#httpd_user{user_data = Data} = Acc) when is_boolean(Rating) ->
	user1(T, Acc#httpd_user{user_data = [{rating, Rating} | Data]});
user1([], Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]},
							{"characteristic", contains, Contains}]}]}]} ->
						[{"language", {like, [Locale]}}] = char(Contains, '_'),
						{{like, Id}, {like, Locale}};
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						{{like, Id}, '_'}
				end;
			false ->
				{'_', '_'}
		end
	of
		{MatchId, MatchLocale} ->
			MFA = [ocs, query_users, [MatchId, MatchLocale]],
			case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			JsonObj = query_page1(lists:map(fun user/1, Events), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
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
	query_page2(T, Filters, [ocs_rest:fields(Filters, H) | Acc]).

%% @hidden
char([{complex, L1} | T], Chars) ->
	case lists:keytake("name", 1, L1) of
		{_, Name, L2} ->
			case lists:keytake("value", 1, L2) of
				{_, Value, []} ->
					char(Name, Value, T, Chars);
				_ ->
					throw({error, 400})
			end;
		false ->
			throw({error, 400})
	end;
char([], Chars) ->
	rev(Chars).
%% @hidden
char({"name", exact, "language"}, {"value", exact, Lang}, T, Chars)
			when is_list(Lang) ->
		Obj = add_char(Chars, {"language", {exact, Lang}}),
		char(T, Obj);
char({"name", exact, "language"}, {"value", like, Like}, T, Chars)
			when is_list(Like) ->
		Obj = add_char(Chars, {"language", {like, Like}}),
		char(T, Obj).

%% @hidden
add_char('_', AttributeMatch) ->
	[AttributeMatch];
add_char(Attributes, AttributeMatch) when is_list(Attributes) ->
	[AttributeMatch | Attributes].

%% @hidden
rev('_') ->
	'_';
rev(Attributes) when is_list(Attributes) ->
	lists:reverse(Attributes).

