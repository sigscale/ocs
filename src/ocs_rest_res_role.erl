%%% ocs_rest_res_role.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
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
%%% 	Handle `Role' collection.
%%%
-module(ocs_rest_res_role).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, post_role/1,
		delete_role/1, get_roles/2, get_role/2]).

-include_lib("inets/include/mod_auth.hrl").
-include("ocs.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec post_role(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `Role' collection.
%% 	Respond to `POST /partyRoleManagement/v4/partyRole' request.
post_role(RequestBody) ->
	try
		Role = role(mochijson:decode(RequestBody)),
		{Name, _, _, _} = Role#httpd_user.username,
		UserData = [{locale, "en"}],
		case ocs:add_user(Name, [], UserData) of
			{ok, LastModified} ->
				Body = mochijson:encode(role(Role)),
				Location = "/partyRoleManagement/v4/partyRole/" ++ Name,
				Headers = [{content_type, "application/json"},
						{location, Location}, {etag, ocs_rest:etag(LastModified)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:500 ->
			{error, 500};
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_role(Name) -> Result
	when
		Name :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `Role' resource.
%% 	Respond to `DELETE /partyRoleManagement/v4/partyRole/{Name}' request.
delete_role(Name) when is_list(Name) ->
	case ocs:delete_user(Name) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec get_roles(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `Role' collection.
%% 	Respond to `GET /partyRoleManagement/v4/partyRole/' request.
get_roles(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_roles1(NewQuery, Filters, Headers);
		false ->
			get_roles1(Query, [], Headers)
	end.
%% @hidden
get_roles1(Query, Filters, Headers) ->
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

-spec get_role(Name, Query) -> Result
	when
		Name :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `Role' resource.
%% 	Respond to `GET /partyRoleManagement/v4/partyRole/{Name}' request.
get_role(Name, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_role(Name, NewQuery, string:tokens(L, ","));
		false ->
			get_role(Name, Query, [])
	end.
%% @hidden
get_role(Name, [] = _Query, _Filters) ->
	case ocs:get_user(Name) of
		{ok, #httpd_user{user_data = UserData} = Role} ->
			Headers = case lists:keyfind(last_modified, 1, UserData) of
				{_, LastModified} ->
					[{content_type, "application/json"}, {etag, ocs_rest:etag(LastModified)}];
				false ->
					[{content_type, "application/json"}]
			end,
			Body = mochijson:encode(role(Role)),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_role(_, _, _) ->
	{error, 400}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec get_params() -> Result
	when
		Result :: {Port :: integer(), Address :: string(),
				Directory :: string(), Group :: string()}
				| {error, Reason :: term()}.
%% @doc Returns configurations details for currently running
%% {@link //inets. httpd} service.
%% @hidden
get_params() ->
	get_params(inets:services_info()).
%% @hidden
get_params({error, Reason}) ->
	{error, Reason};
get_params(ServicesInfo) ->
	get_params1(lists:keyfind(httpd, 1, ServicesInfo)).
%% @hidden
get_params1({httpd, _, HttpdInfo}) ->
	{_, Address} = lists:keyfind(bind_address, 1, HttpdInfo),
	{_, Port} = lists:keyfind(port, 1, HttpdInfo),
	get_params2(Address, Port, application:get_env(inets, services));
get_params1(false) ->
	{error, httpd_not_started}.
%% @hidden
get_params2(Address, Port, {ok, Services}) ->
	get_params3(Address, Port, lists:keyfind(httpd, 1, Services));
get_params2(_, _, undefined) ->
	{error, inet_services_undefined}.
%% @hidden
get_params3(Address, Port, {httpd, Httpd}) ->
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
	case lists:filtermap(F, Httpd) of
		[{Port, Address, Directory, Group}] ->
			{Port, Address, Directory, Group};
		[] ->
			exit(not_found)
	end.

-spec role(Role) -> Role
	when
		Role :: #httpd_user{} | {struct, [tuple()]}.
%% @doc CODEC for HTTP server users.
role(#httpd_user{username = {Name, _, _, _}} = User) when is_list(Name) ->
	role(User#httpd_user{username = Name});
role(#httpd_user{username = Name}) when is_list(Name) ->
	{struct, [{"id", Name}, {"name", Name},
			{"href", "/partyRoleManagement/v4/partyRole/" ++ Name},
			{"@type", "PartyRole"}]};
role({struct, L}) when is_list(L) ->
	role(L, #httpd_user{user_data = []}).
%% @hidden
role([{"name", Name} | T], Acc) when is_list(Name) ->
	case get_params() of
		{Port, Address, Directory, _Group} ->
			role(T, Acc#httpd_user{username = {Name, Address, Port, Directory}});
		{error, _Reason} ->
			{error, 500}
	end;
role([{"@type", Type} | T], #httpd_user{user_data = UserData} = Acc)
		when is_list(Type) ->
	role(T, Acc#httpd_user{user_data = [{type, Type} | UserData]});
role([], Acc) ->
	Acc.

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		{Port, Address, Directory, _Group} = get_params(),
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						Username = {Id ++ '_', Address, Port, Directory},
						{#httpd_user{username = Username, _ = '_'}, []};
					{ok, [{array, [{complex, [{"id", exact, [Id]}]}]}]} ->
						Username = {Id ++ '_', Address, Port, Directory},
						{#httpd_user{username = Username, _ = '_'}, []}
				end;
			false ->
				{'_', '_'}
		end
	of
		{MatchHead, MatchConditions} ->
			MFA = [ocs, query_users, [MatchHead, MatchConditions]],
			case supervisor:start_child(ocs_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_:_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}, infinity) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			JsonObj = query_page1(lists:map(fun role/1, Events), Filters, []),
			Body = mochijson:encode({array, JsonObj}),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
%% @hidden
query_page1([], _, Acc) ->
	lists:reverse(Acc);
query_page1(Json, [], Acc) ->
	lists:reverse(Json ++ Acc);
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]).

