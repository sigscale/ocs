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
-export([parse/1, merge_patch/2]).
-export([parse_query/1, filter/2, range/1]).

-record(pob, {op, path, value}).

-type op_values() :: string() | integer() | boolean()
		| {array, [{struct, [tuple()]}]} | {struct, [tuple()]}.

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

-spec filter(Filters, JsonObject) -> Result
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
%% 	use of the "dot" path seperator character (e.g. `"a.b.c"').
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
%% 	1> ocs_rest:filter("b,a.name=sue,a.value", In).
%% 	{struct, [{"a",{array,[{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]}
%% 	'''
%%
%% @throws {error, 400}
%%
filter(Filters, JsonObject) when is_list(Filters) ->
	Filters1 = case lists:member($(, Filters) of
		true ->
			expand(Filters, []);
		false ->
			Filters
	end,
	Filters2 = string:tokens(Filters1, ","),
	Filters3 = [string:tokens(F, ".") || F <- Filters2],
	Filters4 = lists:usort(Filters3),
	filter1(Filters4, JsonObject, []).

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


-spec parse(Oplists) -> Result
	when
		Oplists		:: [{struct, OPLObject}],
		OPLObject	:: [{Key, Value}],
		Result		:: [Operations] | {error, invalid_format},
		Key			:: string(),
		Value			:: op_values(),
		Operations	:: {OP, Path, Value},
		OP				:: replace | add | remove | move | copy | test,
		Path			:: list().
parse(Oplists) ->
	parse1(lists:map(fun decode_operations/1, Oplists), []).
%% @hidden
parse1([], Acc) ->
	Acc;
parse1([#pob{op = "replace", path = Path, value = Value} | T], Acc) ->
	parse1(T, [{replace, parse_path(Path), Value} | Acc]);
parse1([#pob{op = "add", path = Path, value = Value} | T], Acc) ->
	parse1(T, [{add, parse_path(Path), Value} | Acc]);
parse1([#pob{op = "remove", path = Path, value = Value} | T], Acc) ->
	parse1(T, [{remove, parse_path(Path), Value} | Acc]);
parse1([#pob{op = "move", path = Path, value = Value} | T], Acc) ->
	parse1(T, [{move, parse_path(Path), Value} | Acc]);
parse1([#pob{op = "copy", path = Path, value = Value} | T], Acc) ->
	parse1(T, [{copy, parse_path(Path), Value} | Acc]);
parse1([#pob{op = "test", path = Path, value = Value} | T], Acc) ->
	parse1(T, [{test, parse_path(Path), Value} | Acc]);
parse1(_, _) ->
	{error, invalid_format}.

-spec merge_patch(Target, Patch) -> Result
	when
		Target :: {struct, list()},
		Patch		:: {struct, list()} | {array, list()},
		Result :: {struct, list()} | {array, list()}.
%% @doc Psudo code implementation for RFC7386 Section 2
merge_patch(Target, Patch) ->
	case is_object(Patch) of
		true ->
			Target1 = case is_object(Target) of
				true ->
					Target;
				false ->
					{struct, []}
			end,
			do_merge(get_patch_keys(Patch), Patch, Target1);
		false ->
			Patch
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
filter1(Filters, {array, L}, Acc) ->
	{array, filter2(Filters, L, Acc)};
filter1(Filters, {struct, L}, Acc) ->
	{struct, filter3(Filters, L, false, true, Acc)}.

-spec filter2(Filters, Elements, Acc) -> Result
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
filter2(Filters, [{Type, Value} | T], Acc)
		when Type == struct; Type == array ->
	case filter1(Filters, {Type, Value}, []) of
		{struct, []} ->
			filter2(Filters, T, Acc);
		Object ->
			filter2(Filters, T, [Object | Acc])
	end;
filter2(Filters, [_ | T], Acc) ->
	filter2(Filters, T, Acc);
filter2(_, [], Acc) ->
	lists:reverse(Acc).

-spec filter3(Filters, Members, IsValueMatch, ValueMatched, Acc) -> Result
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
filter3(Filters, [{Key1, Value1} | T], IsValueMatch, ValueMatched, Acc) ->
	case filter4(Filters, {Key1, Value1}, false) of
		{false, false} ->
			filter3(Filters, T, IsValueMatch, ValueMatched, Acc);
		{true, false} when IsValueMatch == false ->
			filter3(Filters, T, true, false, Acc);
		{true, false} ->
			filter3(Filters, T, true, ValueMatched, Acc);
		{false, {_, {_, []}}} ->
			filter3(Filters, T, IsValueMatch, ValueMatched, Acc);
		{false, {Key2, Value2}} ->
			filter3(Filters, T, IsValueMatch, ValueMatched, [{Key2, Value2} | Acc]);
		{true, {Key2, Value2}} ->
			filter3(Filters, T, true, true, [{Key2, Value2} | Acc])
	end;
filter3(Filters, [_ | T], IsValueMatch, ValueMatched, Acc) ->
	filter3(Filters, T, IsValueMatch, ValueMatched, Acc);
filter3(_, [], _, false, _) ->
	[];
filter3(_, [], _, true, Acc) ->
	lists:reverse(Acc).

-spec filter4(Filters, Member, IsValueMatch) -> Result
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
filter4([[Key] | _], {Key, Value}, IsValueMatch) ->
	{IsValueMatch, {Key, Value}};
filter4([[S] | T], {Key, Value}, IsValueMatch) ->
	case split(S) of
		{Key, Value} ->
			{true, {Key, Value}};
		{Key, _} ->
			filter4(T, {Key, Value}, true);
		_ ->
			filter4(T, {Key, Value}, IsValueMatch)
	end;
filter4([[Key | _ ] | _] = Filters1, {Key, {Type, L}}, IsValueMatch)
		when Type == struct; Type == array ->
	F1 = fun([K | _]) when K =:= Key ->
				true;
			(_) ->
				false
	end,
	Filters2 = lists:takewhile(F1, Filters1),
	F2 = fun([_ | T]) -> T end,
	Filters3 = lists:map(F2, Filters2),
	{IsValueMatch, {Key, filter1(Filters3, {Type, L}, [])}};
filter4([_ | T], {Key, Value}, IsValueMatch) ->
	filter4(T, {Key, Value}, IsValueMatch);
filter4([], _, IsValueMatch) ->
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

parse_path(Path) ->
	lists:map(fun is_integer_then_convert/1, string:tokens(Path, "/")).

is_integer_then_convert(Token) ->
	case re:run(Token, ["^[0-9]+$"]) of
		{match, _} ->
			list_to_integer(Token);
		_ ->
			Token
	end.

decode_operations({struct, Operation}) ->
	F = fun({"op", OP}, AccIn) ->
				AccIn#pob{op = OP};
		   ({"path", Path}, AccIn) ->
				AccIn#pob{path = Path};
		   ({"value", Value}, AccIn) ->
				AccIn#pob{value = Value}
	end,
	lists:foldl(F, #pob{}, Operation).

do_merge([], _, Target) ->
	Target;
do_merge([Key | T], Patch, Target) ->
	Value = get_value(Key, Patch),
	Target1 = case Value =:= null of
		true ->
			delete(Key, Target);
		false ->
			set_value(Key, Value, Target)
	end,
	do_merge(T, Patch, Target1).

delete(Key, {struct, L}) when is_list(L) ->
	case lists:keytake(Key, 1, L) of
		{value, _, Target} ->
			{struct, Target};
		false ->
			{struct, L}
	end.

get_patch_keys({struct, L}) when is_list(L) -> proplists:get_keys(L);
get_patch_keys(_) -> [].

get_value(Key, {struct, L}) when is_list(L) ->
	proplists:get_value(Key, L);
get_value(_, _) ->
	throw(not_found).

set_value(Key, Value, {struct, L}) when is_list(L) ->
	{struct, lists:keyreplace(Key, 1, L, {Key, Value})};
set_value(_Key, {struct, _}  = Patch, {struct, _} = Target) ->
	merge_patch(Target, Patch).

is_object({struct, L}) when is_list(L) -> true;
is_object(_) -> false.
