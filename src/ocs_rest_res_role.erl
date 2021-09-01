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

-export([content_types_accepted/0, content_types_provided/0, post_role/1]).

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
	["application/json"].

-spec post_role(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `Role' collection.
%% 	Respond to `POST /partyRoleManagement/v4/partyRole' request.
post_role(RequestBody) ->
	case get_params() of
		{Port, Address, Directory, _Group} ->
			try
				Role = role(mochijson:decode(RequestBody)),
				LM = {erlang:system_time(?MILLISECOND),
						erlang:unique_integer([positive])},
				Name = Role#httpd_user.username,
				true = mod_auth:add_user(Name, [],
						Role#httpd_user.user_data, Address, Port, Directory),
				Body = mochijson:encode(role(Role)),
				Location = "/partyRoleManagement/v4/partyRole/" ++ Name,
				Headers = [{location, Location}, {etag, ocs_rest:etag(LM)}],
				{ok, Headers, Body}
			catch
				_:_Reason1 ->
					{error, 400}
			end;
		{error, _Reason} ->
			{error, 500}
	end.

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
	get_params4(Address, Port, lists:keyfind(directory, 1, Httpd));
get_params3(_, _, false) ->
	{error, httpd_service_undefined}.
%% @hidden
get_params4(Address, Port, {directory, {Directory, Auth}}) ->
	get_params5(Address, Port, Directory,
			lists:keyfind(require_group, 1, Auth));
get_params4(_, _, false) ->
	{error, httpd_directory_undefined}.
%% @hidden
get_params5(Address, Port, Directory, {require_group, [Group | _]}) ->
	{Port, Address, Directory, Group};
get_params5(_, _, _, false) ->
	{error, httpd_group_undefined}.

-spec role(Role) -> Role
	when
		Role :: #httpd_user{} | {struct, [tuple()]}.
%% @doc CODEC for HTTP server users.
role(#httpd_user{username = Name, user_data = UserData}) ->
	F = fun(Key) ->
			case lists:keyfind(Key, 1, UserData) of
				{Key, Value} ->
					Value;
				false ->
					false
			end
	end,
	{struct, [{"id", Name}, {"name", Name}, {"@type", F(type)}, {"validFor", {struct,
					[{"startDateTime", ocs_rest:iso8601(F(start_date))},
					{"endDateTime", ocs_rest:iso8601(F(end_date))}]}},
			{"href", "/partyRoleManagement/v4/partyRole/" ++ Name}]};
role({struct, L}) when is_list(L) ->
	role(L, #httpd_user{user_data = []}).
%% @hidden
role([{"name", Name} | T], Acc) when is_list(Name) ->
	role(T, Acc#httpd_user{username = Name});
role([{"@type", Type} | T], #httpd_user{user_data = UserData} = Acc)
		when is_list(Type) ->
	role(T, Acc#httpd_user{user_data = [{type, Type} | UserData]});
role([{"validFor", {struct, ValidFor}} | T], Acc) when is_list(ValidFor) ->
	role(T, valid_for(ValidFor, Acc));
role([], Acc) ->
	Acc.
%% @hidden
valid_for([{"startDateTime", StartDate} | T],
		#httpd_user{user_data = UserData} = Acc) when is_list(StartDate) ->
	valid_for(T, Acc#httpd_user{user_data = [{start_date,
			ocs_rest:iso8601(StartDate)} | UserData]});
valid_for([{"endDateTime", EndDate} | T],
		#httpd_user{user_data = UserData} = Acc) when is_list(EndDate) ->
	valid_for(T, Acc#httpd_user{user_data = [{end_date,
			ocs_rest:iso8601(EndDate)} | UserData]});
valid_for([], Acc) ->
	Acc.

