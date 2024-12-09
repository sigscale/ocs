%%% ocs_rest_res_client.erl
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
-module(ocs_rest_res_client).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_clients/2, get_client/2, post_client/1,
		patch_client/4, delete_client/1, head_client/0]).

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

-spec head_client() -> Result
   when
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for
%%    `HEAD /ocs/v1/client'
%%    requests.
head_client() ->
   try
      Size = mnesia:table_info(client, size),
      LastItem = integer_to_list(Size),
      ContentRange = "items 1-" ++ LastItem ++ "/" ++ LastItem,
      Headers = [{content_range, ContentRange}],
      {ok, Headers, []}
   catch
      _:_Reason ->
         {error, 500}
   end.

-spec get_clients(Query, Headers) -> Result
	when
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Headers :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Body producing function for `GET /ocs/v1/client'
%% requests.
get_clients(Query, Headers) ->
	try
		case lists:keytake("filter", 1, Query) of
			{value, {_, String}, Query1} ->
				{ok, Tokens, _} = ocs_rest_query_scanner:string(String),
				case ocs_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, Complex}]}]} ->
						MatchAddress = match("id", Complex, Query1),
						MatchId = match("identifier", Complex, Query1),
						MatchPort = match("port", Complex, Query1),
						MatchProtocol = match("protocol", Complex, Query1),
						MatchSecret = match("secret", Complex, Query1),
						{Query, [MatchAddress, MatchId, MatchPort, MatchProtocol, MatchSecret]}
				end;
			false ->
				MatchAddress = match("id", [], Query),
				MatchId = match("identifier", [], Query),
				MatchPort = match("port", [], Query),
				MatchProtocol = match("protocol", [], Query),
				MatchSecret = match("secret", [], Query),
				{Query, [MatchAddress, MatchId, MatchPort, MatchProtocol, MatchSecret]}
		end
	of
		{Query2, Args} ->
			Codec = fun client/1,
			query_filter({ocs, query_clients, Args}, Codec, Query2, Headers)
	catch
		_ ->
			{error, 400}
	end.

-spec get_client(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key, Value}],
		Key :: string(),
		Value :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
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
		{ok, #client{last_modified = LM} = Client} ->
			Json = client(Client),
			FilteredJson = case Filters of
				Filters when Filters =/= [], Client =/= #client{} ->
					ocs_rest:fields("id,href," ++ Filters, Json);
				_ ->
					Json
			end,
			Body = mochijson:encode(FilteredJson),
			Headers = [{content_type, "application/json"},
				{etag, ocs_rest:etag(LM)}],
			{ok, Headers, Body};
		{error, not_found} ->
			{error, 404}
	end.

-spec post_client(RequestBody) -> Result
	when
		RequestBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc Respond to `POST /ocs/v1/client' and add a new `client'
%% resource.
post_client(RequestBody) ->
	try
		Client = client(mochijson:decode(RequestBody)),
		#client{address = Address, port = Port, protocol = Protocol,
				secret = Secret, password_required = PasswordReq,
				trusted = Trusted} = Client,
		{ok, #client{last_modified = Etag} = Client1} =
				ocs:add_client(Address, Port, Protocol, Secret, PasswordReq, Trusted),
		Id = inet:ntoa(Address),
		Location = "/ocs/v1/client/" ++ Id,
		JsonObj  = client(Client1),
		Body = mochijson:encode(JsonObj),
		Headers = [{content_type, "application/json"},
				{location, Location}, {etag, ocs_rest:etag(Etag)}],
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
		ReqBody :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
%% @doc	Respond to `PATCH /ocs/v1/client/{id}' request and
%% Updates a existing `client''s password or attributes.
patch_client(Id, Etag, CType, ReqBody) ->
	try
		Etag1 = case Etag of
			undefined ->
				undefined;
			Etag ->
				ocs_rest:etag(Etag)
		end,
		Address0 = case inet:parse_address(Id) of
			{ok, IpAddress} ->
				IpAddress;
			{error, einval} ->
				{error, 400}
		end,
		{Address0, Etag1, mochijson:decode(ReqBody)}
	of
		{Address, Etag2, Operations} ->
			patch_client1(Address, Etag2, CType, Operations)
	catch
		_:_ ->
			{error, 400}
	end.
patch_client1(Id, Etag, CType, Operations) ->
	case ocs:find_client(Id) of
		{ok, #client{last_modified = CurrentEtag} = Client}
				when CurrentEtag == Etag; Etag == undefined ->
			patch_client2(Id, Etag, CType, Client, Operations);
		{ok, #client{}} ->
			{error, 412};
		{error, _Reason} ->
			{error, 404}
	end.
%% @hidden
patch_client2(Id, Etag, "application/json", #client{port = Port,
		protocol = Protocol, secret = Secret}, {struct, Object}) ->
	try
		case Object of
			[{"secret", NewPassword}] ->
				Protocol1 = string:to_upper(atom_to_list(Protocol)),
				patch_client3(Id, Port, Protocol1, NewPassword, Etag);
			[{"port", NewPort},{"protocol", RADIUS}]
					when RADIUS =:= "radius"; RADIUS =:= "RADIUS" ->
				patch_client4(Id, NewPort, radius, Secret, Etag);
			[{"port", NewPort},{"protocol", DIAMETER}]
					when DIAMETER =:= "diameter"; DIAMETER =:= "DIAMETER" ->
				patch_client4(Id, NewPort, diameter, Secret, Etag)
		end
	catch
		throw : _ ->
			{error, 400}
	end;
patch_client2(Address, _Etag, "application/json-patch+json", Client, Operations) ->
	try
		Json = ocs_rest:patch(Operations, client(Client)),
		Client1 = client(Json),
		F = fun() ->
			case mnesia:read(client, Address, write) of
				[_] ->
					TS = erlang:system_time(millisecond),
					N = erlang:unique_integer([positive]),
					LM = {TS, N},
					ok = mnesia:write(Client1#client{last_modified = LM}),
					LM;
				[] ->
					throw(not_found)
			end
		end,
		case mnesia:transaction(F) of
			{atomic, LastModified} ->
				ID = inet:ntoa(Address),
				Location = "/ocs/v1/client" ++ ID,
				Headers = [{content_type, "application/json"},
					{location, Location}, {etag, ocs_rest:etag(LastModified)}],
				Body = mochijson:encode(Json),
				{ok, Headers, Body};
			{aborted, {throw, not_found}} ->
				{error, 404};
			{aborted, _Reason} ->
				{error, 500}
		end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
patch_client3(Id, Port, Protocol, NewPassword, Etag) ->
	IDstr = inet:ntoa(Id),
	ok = ocs:update_client(Id, NewPassword),
	RespObj = [{"id", IDstr}, {"href", "/ocs/v1/client/" ++ IDstr},
			{"port", Port}, {"protocol", Protocol}, {"secret", NewPassword}],
	JsonObj = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	Headers = case Etag of
		undefined ->
			[{content_type, "application/json"}];
		_ ->
			[{content_type, "application/json"}, {etag, ocs_rest:etag(Etag)}]
	end,
	{ok, Headers, RespBody}.
%% @hidden
patch_client4(Id, Port, Protocol, Secret, Etag) ->
	IDstr = inet:ntoa(Id),
	Protocolstr = string:to_upper(atom_to_list(Protocol)),
	ok = ocs:update_client(Id, Port, Protocol),
	RespObj = [{"id", IDstr}, {"href", "/ocs/v1/client/" ++ IDstr},
			{"port", Port}, {"protocol", Protocolstr}, {"secret", Secret}],
	JsonObj = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	Headers = case Etag of
		undefined ->
			[{content_type, "application/json"}];
		_ ->
			[{content_type, "application/json"},{etag, ocs_rest:etag(Etag)}]
	end,
	{ok, Headers, RespBody}.

-spec delete_client(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, StatusCode}
				| {error, StatusCode, Problem},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist(),
		StatusCode :: 400..599,
		Problem :: ocs_rest:problem().
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

%% @hidden
query_filter(MFA, Codec, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			query_filter(MFA, Codec, NewQuery, Filters, Headers);
		false ->
			query_filter(MFA, Codec, Query, [], Headers)
	end.
%% @hidden
query_filter(MFA, Codec, Query, Filters, Headers) ->
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
							query_page(Codec, PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(Codec, PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(MFA, Codec, Query, Filters, Start, End)
					end;
				PageServer ->
					case ocs_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(Codec, PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case ocs_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(MFA, Codec, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(MFA, Codec, Query, Filters, undefined, undefined)
	end.

%% @hidden
query_start({M, F, A}, Codec, Query, Filters, RangeStart, RangeEnd) ->
	case supervisor:start_child(ocs_rest_pagination_sup, [[M, F, A]]) of
		{ok, PageServer, Etag} ->
			query_page(Codec, PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
		{error, _Reason} ->
			{error, 500}
	end.

%% @hidden
query_page(Codec, PageServer, Etag, [] = _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			ContentRange1 = case string:split(ContentRange, "/") of
				[Range, "*"] ->
					case erlang:fun_info(Codec, name) of
						{_, client} ->
							Size = mnesia:table_info(client, size),
							lists:concat([Range, "/",  Size]);
						_Other ->
							ContentRange
					end;
				_Other ->
					ContentRange
			end,
			JsonObj = query_page1(lists:map(Codec, Result), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange1}],
			{ok, Headers, Body}
	end;
query_page(Codec, PageServer, Etag, _Query, Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}) of
		{error, Status} ->
			{error, Status};
		{Result, ContentRange} ->
			JsonObj = query_page1(lists:map(Codec, Result), Filters, []),
			JsonArray = {array, JsonObj},
			Body = mochijson:encode(JsonArray),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
%% @hidden
query_page1(Json, [], []) ->
	Json;
query_page1([H | T], Filters, Acc) ->
	query_page1(T, Filters, [ocs_rest:fields(Filters, H) | Acc]);
query_page1([], _, Acc) ->
	lists:reverse(Acc).

%% @hidden
match(Key, Complex, Query) ->
	case lists:keyfind(Key, 1, Complex) of
		{_, like, [Value]} ->
			{like, Value};
		{_, exact, [Value]} ->
			{exact, Value};
		false ->
			case lists:keyfind(Key, 1, Query) of
				{_, Value} ->
					{exact, Value};
				false ->
					'_'
			end
	end.

-spec client(Client) -> Result
	when
		Client :: #client{} | {struct, list()},
		Result :: {struct, list()} | #client{}.
%% @private
%% Codec function for client
client(#client{} = Client) ->
	client(record_info(fields, client), Client, []);
client({struct, ObjectMembers}) when is_list(ObjectMembers) ->
	client(ObjectMembers, #client{}).
%% @hidden
client([address | T], #client{address = Address} = C, Acc)
		when is_tuple(Address) ->
	Id = inet:ntoa(Address),
	Header = [{"href","/ocs/v1/client/" ++ Id}, {"id", Id}],
	client(T, C, Acc ++ Header);
client([port | T], #client{port = Port} = C, Acc)
		when is_integer(Port) ->
	client(T, C, [{"port", Port} | Acc]);
client([secret | T], #client{secret = Secret} = C, Acc)
		when is_binary(Secret) ->
	client(T, C, [{"secret", binary_to_list(Secret)} | Acc]);
client([protocol | T], #client{protocol = radius} = C, Acc) ->
	client(T, C, [{"protocol", "RADIUS"} | Acc]);
client([protocol | T], #client{protocol = diameter} = C, Acc) ->
	client(T, C, [{"protocol", "DIAMETER"} | Acc]);
client([identifier| T], #client{identifier = Identifier} = C, Acc)
		when is_binary(Identifier) ->
	client(T, C, [{"identifier", binary_to_list(Identifier)} | Acc]);
client([password_required | T], #client{password_required = false} = C, Acc) ->
	client(T, C, [{"passwordRequired", false} | Acc]);
client([trusted| T], #client{trusted = Trusted} = C, Acc)
		when is_boolean(Trusted) ->
	client(T, C, [{"trusted", Trusted} | Acc]);
client([_ | T], Client, Acc) ->
	client(T, Client, Acc);
client([], _, Acc) ->
	{struct, lists:reverse(Acc)}.
%% @hidden
client([{"id", Id} | T], Acc) when is_list(Id) ->
	{ok, Address} = inet_parse:address(Id),
	client(T, Acc#client{address = Address});
client([{"href", _} | T], Acc) ->
	client(T, Acc);
client([{"port", Port} | T], Acc)
		when is_integer(Port) ->
	client(T, Acc#client{port = Port});
client([{"identifier", Identifier} | T], Acc)
		when is_list(Identifier) ->
	client(T, Acc#client{identifier = list_to_binary(Identifier)});
client([{"protocol", "radius"} | T], Acc) ->
	client(T, Acc#client{protocol = radius});
client([{"protocol", "RADIUS"} | T], Acc) ->
	client(T, Acc#client{protocol = radius});
client([{"protocol", "diameter"} | T], Acc) ->
	client(T, Acc#client{protocol = diameter});
client([{"protocol", "DIAMETER"} | T], Acc) ->
	client(T, Acc#client{protocol = diameter});
client([{"secret", Secret} | T], Acc) when is_list(Secret) ->
	client(T, Acc#client{secret = list_to_binary(Secret)});
client([{"passwordRequired", PwdReq} | T], Acc)
		when is_boolean(PwdReq) ->
	client(T, Acc#client{password_required = PwdReq});
client([{"trusted", Trusted} | T], Acc)
		when is_boolean(Trusted) ->
	client(T, Acc#client{trusted = Trusted});
client([], Acc) ->
	Acc.

