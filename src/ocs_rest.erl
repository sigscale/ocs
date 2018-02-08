%%% ocs_rest.erl
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
%%% @doc This library module implements utility functions
%%% 	for REST servers in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([date/1, iso8601/1, etag/1]).
-export([pointer/1, patch/2]).
-export([parse_query/1, fields/2, range/1]).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The ocs_rest public API
%%----------------------------------------------------------------------

-spec date(DateTimeFormat) -> Result
	when
		DateTimeFormat	:: pos_integer() | tuple(),
		Result			:: calendar:datetime() | non_neg_integer().
%% @doc Convert iso8610 to date and time or
%%		date and time to timeStamp.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH,
	Seconds * 1000.

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds	:: pos_integer() | string(),
		Result			:: string() | pos_integer().
%% @doc Convert iso8610 to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0b",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars);
iso8601(ISODateTime) when is_list(ISODateTime) ->
	case string:rchr(ISODateTime, $T) of
		0 ->
			iso8601(ISODateTime, []);
		N ->
			iso8601(lists:sublist(ISODateTime, N - 1),
				lists:sublist(ISODateTime,  N + 1, length(ISODateTime)))
	end.
%% @hidden
iso8601(Date, Time) when is_list(Date), is_list(Time) ->
	D = iso8601_date(string:tokens(Date, ",-"), []),
	{H, Mi, S, Ms} = iso8601_time(string:tokens(Time, ":."), []),
	date({D, {H, Mi, S}}) + Ms.
%% @hidden
iso8601_date([[Y1, Y2, Y3, Y4] | T], _Acc) ->
	Y = list_to_integer([Y1, Y2, Y3, Y4]),
	iso8601_date(T, Y);
iso8601_date([[M1, M2] | T], Y) when is_integer(Y) ->
	M = list_to_integer([M1, M2]),
	iso8601_date(T, {Y, M});
iso8601_date([[D1, D2] | T], {Y, M}) ->
	D = list_to_integer([D1, D2]),
	iso8601_date(T, {Y, M, D});
iso8601_date([], {Y, M}) ->
	{Y, M, 1};
iso8601_date([], {Y, M, D}) ->
	{Y, M, D}.
%% @hidden
iso8601_time([H1 | T], []) ->
	H = list_to_integer(H1),
	iso8601_time(T, H);
iso8601_time([M1 | T], H) when is_integer(H) ->
	Mi = list_to_integer(M1),
	iso8601_time(T, {H, Mi});
iso8601_time([S1 | T], {H, Mi}) ->
	S = list_to_integer(S1),
	iso8601_time(T, {H, Mi, S});
iso8601_time([], {H, Mi}) ->
	{H, Mi, 0, 0};
iso8601_time([Ms1 | T], {H, Mi, S}) ->
	Ms = list_to_integer(Ms1),
	iso8601_time(T, {H, Mi, S, Ms});
iso8601_time([], {H, Mi, S}) ->
	{H, Mi, S, 0};
iso8601_time([], {H, Mi, S, Ms}) ->
	{H, Mi, S, Ms};
iso8601_time([], []) ->
	{0,0,0,0}.

-spec etag(Etag) -> Etag
	when
		Etag :: string() | {TS, N},
		TS :: pos_integer(),
		N :: pos_integer().
%% @doc Map unique timestamp and HTTP ETag.
etag({TS, N} = _Etag) when is_integer(TS), is_integer(N)->
	integer_to_list(TS) ++ "-" ++ integer_to_list(N);
etag(Etag) when is_list(Etag) ->
	[TS, N] = string:tokens(Etag, "-"),
	{list_to_integer(TS), list_to_integer(N)}.

-spec parse_query(Query) -> Result
	when
		Query :: string(),
		Result :: [{Key, Value}],
		Key :: string(),
		Value :: string().
%% @doc Parse the query portion of a URI.
%% @throws {error, 400}
parse_query("?" ++ Query) ->
	parse_query(Query);
parse_query(Query) when is_list(Query) ->
	parse_query(string:tokens(Query, "&"), []).
%% @hidden
parse_query([H | T], Acc) ->
	parse_query(T, parse_query1(H, string:chr(H, $=), Acc));
parse_query([], Acc) ->
	lists:reverse(Acc).
%% @hidden
parse_query1(_Field, 0, _Acc) ->
	throw({error, 400});
parse_query1(Field, N, Acc) ->
	Key = lists:sublist(Field, N - 1),
	Value = lists:sublist(Field, N + 1, length(Field)),
	[{http_uri:decode(Key), http_uri:decode(Value)} | Acc].

-spec fields(Filters, JsonObject) -> Result
	when
		Filters :: string(),
		JsonObject :: tuple(),
		Result :: tuple().
%% @doc Filter a JSON object.
%%
%% 	Parses the right hand side of a `fields=' portion of a query
%% 	string and applies those filters on a `JSON' object.
%%
%% 	Each filter in `Filters' is the name of a member in the JSON
%% 	encoded `JsonObject'. A filter may refer to a complex type by
%% 	use of the "dot" path separator character (e.g. `"a.b.c"').
%% 	Where an intermediate node on a complex path is an array all
%% 	matching array members will be included. To filter out objects
%% 	an `=value', suffix may be added which will include only
%% 	objects with a member matching the name and value. Multiple
%% 	values may be provided with `=(value1,value2)'.
%%
%% 	Returns a new JSON object with only the matching items.
%%
%% 	Example:
%% 	```
%% 	1> In = {struct,[{"a",{array,[{struct,[{"name","bob"},{"value",6}]},
%% 	1> {"b",7},{struct,[{"name","sue"},{"value",5},{"other", 8}]}]}},{"b",1}]},
%% 	1> ocs_rest:fields("b,a.name=sue,a.value", In).
%% 	{struct, [{"a",{array,[{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]}
%% 	'''
%%
%% @throws {error, 400}
%%
fields(Filters, JsonObject) when is_list(Filters) ->
	Filters1 = case lists:member($(, Filters) of
		true ->
			expand(Filters, []);
		false ->
			Filters
	end,
	Filters2 = string:tokens(Filters1, ","),
	Filters3 = [string:tokens(F, ".") || F <- Filters2],
	Filters4 = lists:usort(Filters3),
	fields1(Filters4, JsonObject, []).

-spec range(Range) -> Result
	when
		Range :: RHS | {Start, End},
		RHS :: string(),
		Result :: {ok, {Start, End}} | {ok, RHS} | {error, 400},
		Start :: pos_integer(),
		End :: pos_integer().
%% @doc Parse or create a `Range' request header.
%% 	`RHS' should be the right hand side of an
%% 	RFC7233 `Range:' header conforming to TMF630
%% 	(e.g. "items=1-100").
%% @private
range(Range) when is_list(Range) ->
	try
		["items", S, E] = string:tokens(Range, "= -"),
		{ok, {list_to_integer(S), list_to_integer(E)}}
	catch
		_:_ ->
			{error, 400}
	end;
range({Start, End}) when is_integer(Start), is_integer(End) ->
	{ok, "items=" ++ integer_to_list(Start) ++ "-" ++ integer_to_list(End)}.

-spec pointer(Path) -> Pointer
	when
		Path :: string(),
		Pointer :: [string()].
%% @doc Decode JSON Pointer.
%% 	Apply the decoding rules of <a href="http://tools.ietf.org/html/rfc6901">RFC6901</a>. 
%% 	`Path' is a JSON string as used in the `"path"' member of a
%% 	JSON Patch ((<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>)
%% 	operation. `Pointer' is a list of member name strings in a path.
pointer(Pointer) ->
	pointer(Pointer, [], []).
%% @hidden
pointer([$/ | T], [], Acc) ->
	pointer(T, [], Acc);
pointer([$/ | T], Acc1, Acc2) ->
	pointer(T, [], [lists:reverse(Acc1) | Acc2]);
pointer([$- | T], Acc1, Acc2) ->
	pointer1(T, Acc1, Acc2);
pointer([H | T], Acc1, Acc2) ->
	pointer(T, [H | Acc1], Acc2);
pointer([], Acc1, Acc2) ->
	lists:reverse([lists:reverse(Acc1) | Acc2]).
%% @hidden
pointer1([$1 | T], Acc1, Acc2) ->
	pointer(T, [$/ | Acc1], Acc2);
pointer1([$0 | T], Acc1, Acc2) ->
	pointer(T, [$- | Acc1], Acc2);
pointer1(T, Acc1, Acc2) ->
	pointer(T, [$- | Acc1], Acc2).

-spec patch(Patch, Resource) -> Resource
	when
		Patch :: {array, [{struct, [tuple()]}]},
		Resource :: {struct, list()} | {array, list()}.
%% @doc Apply a JSON `Patch' (<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>).
%% 	Modifies the `Resource' by applying the operations listed in `Patch'.
%% 	`Operation' may be `"add"', `"remove"', or `"replace"'.
%% @hidden
patch({array, L} = _Patch, Resource) ->
	patch1(L, Resource, []).
%% @hidden
patch1([{struct, L} | T], Resource, Acc) ->
	{_, Op} = lists:keyfind("op", 1, L),
	{_, Path} = lists:keyfind("path", 1, L),
	Operation = case lists:keyfind("value", 1, L) of
		{_, Value} ->
			{Op, Path, Value};
		false ->
			{Op, Path}
	end,
	patch1(T, Resource, [Operation | Acc]);
patch1([], Resource, Acc) ->
	patch2(lists:reverse(Acc), Resource).
%% @hidden
patch2([{"add", Path, Value} | T] = _Patch, Resource) ->
	patch2(T, patch_add(pointer(Path), Value, Resource));
patch2([{"remove", Path} | T], Resource) ->
	patch2(T, patch_remove(pointer(Path), Resource));
patch2([{"replace", Path, Value} | T], Resource) ->
	patch2(T, patch_replace(pointer(Path), Value, Resource));
patch2([], Resource) ->
	Resource.
%% @hidden
patch_add(Path, Value, {struct, L}) ->
	{struct, patch_add(Path, Value, L, [])};
patch_add(["-"], Value, {array, L}) ->
	{array, L ++ [Value]};
patch_add([H | T], Value, {array, L}) ->
	case list_to_integer(H) of
		N when T == [], N < length(L) ->
			Left = lists:sublist(L, N),
			Right = lists:sublist(L, N + 1, length(L)),
			{array, Left ++ [Value | Right]};
		N when N < length(L) ->
			Left = lists:sublist(L, N),
			Element = patch_add(T, Value, lists:nth(N + 1, L)),
			Right = lists:sublist(L, N + 2, length(L)),
			{array, Left ++ [Element | Right]}
	end.
%% @hidden
patch_add([Name | []], Value, [], Acc) ->
	lists:reverse([{Name, Value} | Acc]);
patch_add([Name | []], Value, [{Name, _} | T], Acc) ->
	lists:reverse(Acc) ++ [{Name, Value} | T];
patch_add([Name | T1], Value1, [{Name, Value2} | T2], Acc) ->
	Value3 = patch_add(T1, Value1, Value2),
	lists:reverse(Acc) ++ [{Name, Value3} | T2];
patch_add(Path, Value, [H | T], Acc) ->
	patch_add(Path, Value, T, [H | Acc]).
%% @hidden
patch_remove(Path, {struct, L}) ->
	{struct, patch_remove(Path, L, [])};
patch_remove([H | T], {array, L}) ->
	case list_to_integer(H) of
		N when T == [], N < length(L) ->
			Left = lists:sublist(L, N),
			Right = lists:sublist(L, N + 2, length(L)),
			{array, Left ++ Right};
		N when N < length(L) ->
			Left = lists:sublist(L, N),
			Element = patch_remove(T, lists:nth(N + 1, L)),
			Right = lists:sublist(L, N + 2, length(L)),
			{array, Left ++ [Element | Right]}
	end.
%% @hidden
patch_remove([Name | []], [{Name, _} | T], Acc) ->
	lists:reverse(Acc) ++ T;
patch_remove([Name | T1], [{Name, Value1} | T2], Acc) ->
	Value2 = patch_remove(T1, Value1),
	lists:reverse(Acc) ++ [{Name, Value2} | T2];
patch_remove(Path, [H | T], Acc) ->
	patch_remove(Path, T, [H | Acc]).
%% @hidden
patch_replace(Path, Value, {struct, L}) ->
	{struct, patch_replace(Path, Value, L, [])};
patch_replace([H | T], Value, {array, L}) ->
	case list_to_integer(H) of
		N when T == [], N < length(L) ->
			Left = lists:sublist(L, N),
			Right = lists:sublist(L, N + 2, length(L)),
			{array, Left ++ [Value | Right]};
		N when N < length(L) ->
			Left = lists:sublist(L, N),
			Right = lists:sublist(L, N + 2, length(L)),
			Element = patch_replace(T, Value, lists:nth(N + 1, L)),
			{array, Left ++ [Element | Right]}
	end.
%% @hidden
patch_replace([Name | []], Value, [{Name, _} | T], Acc) ->
	lists:reverse(Acc) ++ [{Name, Value} | T];
patch_replace([Name | T1], Value1, [{Name, Value2} | T2], Acc) ->
	Value3 = patch_replace(T1, Value1, Value2),
	lists:reverse(Acc) ++ [{Name, Value3} | T2];
patch_replace(Path, Value, [H | T], Acc) ->
	patch_replace(Path, Value, T, [H | Acc]).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
fields1(Filters, {array, L}, Acc) ->
	{array, fields2(Filters, L, Acc)};
fields1(Filters, {struct, L}, Acc) ->
	{struct, fields3(Filters, L, false, true, Acc)}.

-spec fields2(Filters, Elements, Acc) -> Result
	when
		Filters :: [list(string())],
		Elements :: [Value],
		Value :: integer() | string() | {struct, Members} | {array, Elements},
		Members :: [{Key, Value}],
		Key :: string(),
		Acc :: [Value],
		Result :: [Value].
%% @doc Process each array element.
%% @hidden
fields2(Filters, [{Type, Value} | T], Acc)
		when Type == struct; Type == array ->
	case fields1(Filters, {Type, Value}, []) of
		{struct, []} ->
			fields2(Filters, T, Acc);
		Object ->
			fields2(Filters, T, [Object | Acc])
	end;
fields2(Filters, [_ | T], Acc) ->
	fields2(Filters, T, Acc);
fields2(_, [], Acc) ->
	lists:reverse(Acc).

-spec fields3(Filters, Members, IsValueMatch, ValueMatched, Acc) -> Result
	when
		Filters :: [list(string())],
		Members :: [{Key, Value}],
		Key :: string(),
		Value :: integer() | string() | {struct, Members} | {array, Elements},
		Elements :: [Value],
		IsValueMatch :: boolean(),
		ValueMatched :: boolean(),
		Acc :: [{Key, Value}],
		Result :: [{Key, Value}].
%% @doc Process each object member.
%% @hidden
fields3(Filters, [{Key1, Value1} | T], IsValueMatch, ValueMatched, Acc) ->
	case fields4(Filters, {Key1, Value1}, false) of
		{false, false} ->
			fields3(Filters, T, IsValueMatch, ValueMatched, Acc);
		{true, false} when IsValueMatch == false ->
			fields3(Filters, T, true, false, Acc);
		{true, false} ->
			fields3(Filters, T, true, ValueMatched, Acc);
		{false, {_, {_, []}}} ->
			fields3(Filters, T, IsValueMatch, ValueMatched, Acc);
		{false, {Key2, Value2}} ->
			fields3(Filters, T, IsValueMatch, ValueMatched, [{Key2, Value2} | Acc]);
		{true, {Key2, Value2}} ->
			fields3(Filters, T, true, true, [{Key2, Value2} | Acc])
	end;
fields3(Filters, [_ | T], IsValueMatch, ValueMatched, Acc) ->
	fields3(Filters, T, IsValueMatch, ValueMatched, Acc);
fields3(_, [], _, false, _) ->
	[];
fields3(_, [], _, true, Acc) ->
	lists:reverse(Acc).

-spec fields4(Filters, Member, IsValueMatch) -> Result
	when
		Filters :: [list(string())],
		Member :: {Key, Value},
		IsValueMatch :: boolean(),
		Key :: string(),
		Value :: integer() | string() | {struct, [Member]} | {array, [Value]},
		Result :: {ValueMatch, MatchResult},
		ValueMatch :: boolean(),
		MatchResult :: false | Member.
%% @doc Apply filters to an object member.
%% @hidden
fields4([[Key] | _], {Key, Value}, IsValueMatch) ->
	{IsValueMatch, {Key, Value}};
fields4([[S] | T], {Key, Value}, IsValueMatch) ->
	case split(S) of
		{Key, Value} ->
			{true, {Key, Value}};
		{Key, _} ->
			fields4(T, {Key, Value}, true);
		_ ->
			fields4(T, {Key, Value}, IsValueMatch)
	end;
fields4([[Key | _ ] | _] = Filters1, {Key, {Type, L}}, IsValueMatch)
		when Type == struct; Type == array ->
	F1 = fun([K | _]) when K =:= Key ->
				true;
			(_) ->
				false
	end,
	Filters2 = lists:takewhile(F1, Filters1),
	F2 = fun([_ | T]) -> T end,
	Filters3 = lists:map(F2, Filters2),
	{IsValueMatch, {Key, fields1(Filters3, {Type, L}, [])}};
fields4([_ | T], {Key, Value}, IsValueMatch) ->
	fields4(T, {Key, Value}, IsValueMatch);
fields4([], _, IsValueMatch) ->
	{IsValueMatch, false}.

%% @hidden
expand("=(" ++ T, Acc) ->
	{Key, NewAcc} = expand1(Acc, []),
	expand2(T, Key, [], NewAcc);
expand([H | T], Acc) ->
	expand(T, [H | Acc]);
expand([], Acc) ->
	lists:reverse(Acc).
%% @hidden
expand1([$, | _] = T, Acc) ->
	{lists:reverse(Acc), T};
expand1([H | T], Acc) ->
	expand1(T, [H | Acc]);
expand1([], Acc) ->
	{lists:reverse(Acc), []}.
%% @hidden
expand2([$) | T], Key, Acc1, Acc2) ->
	Expanded = Acc1 ++ "=" ++ Key,
	expand(T, Expanded ++ Acc2);
expand2([$, | T], Key, Acc1, Acc2) ->
	Expanded = "," ++ Acc1 ++ "=" ++ Key,
	expand2(T, Key, [], Expanded ++ Acc2);
expand2([H | T], Key, Acc1, Acc2) ->
	expand2(T, Key, [H | Acc1], Acc2);
expand2([], _, _, _) ->
	throw({error, 400}).

%% @hidden
split(S) ->
	split(S, []).
%% @hidden
split([$= | T], Acc) ->
	{lists:reverse(Acc), T};
split([H | T], Acc) ->
	split(T, [H | Acc]);
split([], Acc) ->
	lists:reverse(Acc).

