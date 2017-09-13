%%% ocs_rest.erl
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

-export([parse_query/1, filter/2, range/1]).

%%----------------------------------------------------------------------
%%  The ocs_rest public API
%%----------------------------------------------------------------------

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
		Range :: string(),
		Result :: {Start, End} | {error, 400},
		Start :: pos_integer(),
		End :: pos_integer().
%% @doc Parse Range request header.
%% @private
range(Range) when is_list(Range) ->
	try
		["item", S, E] = string:tokens(Range, " -"),
		{list_to_integer(S), list_to_integer(E)}
	catch
		_:_ ->
			{error, 400}
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

