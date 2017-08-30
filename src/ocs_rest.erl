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
%% 	matching array members will be included. To filter out array
%% 	members an `=value', suffix may be added which will include only
%% 	objects with a member matching the name and value. Multiple
%% 	values may be provided with `=(value1,value2)'.
%%
%% 	Returns a new JSON object with only the matching items.
%%
%% 	Example:
%% 	```
%% 	1> In = {struct,[{"a",{array,[{struct,[{"name","bob"},{"value",6}]},
%% 	1> {"b",7},{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]},
%% 	1> ocs_rest:filter("b,a.name=sue", In).
%% 	{struct, [{"a",{array,[{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]}
%% 	'''
%%
%% @throws {error, 400}
%%
filter(Filters, {struct, L} = _Object) when is_list(Filters) ->
	Filters1 = case lists:member($(, Filters) of
		true ->
			expand(Filters, []);
		false ->
			Filters
	end,
	Filters2 = [string:tokens(F, ".") || F <- Filters1],
	{struct, filter1(lists:usort(Filters2), L, [])}.

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
%% put filters in order so we can process (once) sequentially
filter1([Filter | T], L, []) ->
	[Name | RevPath] = lists:reverse(Filter),
	filter1(T, L, [{[Name], RevPath}]);
filter1([Filter | T1], L, [{Names, RevPath} | T2] = Acc) ->
	case lists:reverse(Filter) of
		[Name | RevPath] ->
			filter1(T1, L, [{[Name | Names], RevPath} | T2]);
		[Name | NewRevPath] ->
			filter1(T1, L, [{[Name], NewRevPath} | Acc])
	end;
filter1([], L, Acc) ->
	Filters = [{lists:reverse(Path), lists:reverse(Names)}
			|| {Names, Path} <- lists:reverse(Acc)],
	filter2(Filters, L, []).
%% @hidden
%% filter fields from object
filter2([{[], [H | T1]} | T2] = _Filters, L, Acc) ->
	case lists:keyfind(H, 1, L) of
		false ->
			filter2([{[], T1} | T2], L, Acc);
		KV ->
			filter2([{[], T1} | T2], L, [KV | Acc])
	end;
%% depth first traversal for complex filters
filter2([{[H | _], _} | T] = Filters, L1, Acc) ->
	case lists:keyfind(H, 1, L1) of
		false ->
			filter2(T, L1, Acc);
		{H, {Type, L2}} when Type == struct; Type == array ->
			% remove parent from filter path
			F1 = fun({[Prefix | Suffix], Names}) when Prefix == H ->
						{true, {Suffix, Names}};
					(_) ->
						false
			end,
			SubFilters = lists:filtermap(F1, Filters),
			% filter descendants
			NewAcc = [{H, filter3(Type, SubFilters, L2)} | Acc],
			% skip filters applied above
			F2 = fun({[Prefix | _], _}) when Prefix == H ->
						true;
					(_P) ->
						false
			end,
			NewFilters = lists:dropwhile(F2, Filters),
			filter2(NewFilters, L1, NewAcc);
		_ ->
			throw({error, 400})
	end;
filter2([{[], []} | T], L, Acc) ->
	filter2(T, L, Acc);
filter2([], _, Acc) ->
	lists:reverse(Acc).
%% @hidden
filter3(struct, Filters, L) ->
	{struct, filter2(Filters, L, [])};
filter3(array, Filters, L) ->
	{array, filter4(Filters, L, [])}.
%% @hidden
%% handle complex filters on arrays
filter4(Filters, [{struct, L} | T], Acc) ->
	case filter5(Filters, L, []) of
		false ->
			filter4(Filters, T, Acc);
		NewFilters ->
			filter4(NewFilters, T, [{struct, filter2(NewFilters, L, [])} | Acc])
	end;
filter4(Filters, [{array, L} | T], Acc) ->
	filter4(Filters, T, [{array, filter4(Filters, L, [])} | Acc]);
filter4(Filters, [{Key, _Value} = KV | T], Acc) ->
	case lists:member(Key, Filters) of
		true ->
			filter4(Filters, T, [KV | Acc]);
		false ->
			filter4(Filters, T, Acc)
	end;
filter4(_, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
%% test filters to see if any are match filters
%% if match filters fail ignore entire structure (L)
%% if match(s) succeed remove match portions ("=foo")
filter5([{[], Filters} | T], L, Acc) ->
	case filter6(Filters, L, undefined, []) of
		false ->
			false;
		NewFilters ->
			filter5(T, L, [{[], NewFilters} | Acc])
	end;
filter5([H | T], L, Acc) ->
	case lists:keyfind(H, 1, L) of
		false ->
			filter5(T, L, Acc);
		KV ->
			filter5(T, L, [KV | Acc])
	end;
filter5([], _, Acc) ->
	lists:reverse(Acc).

%% @hidden
%% check for value matches
%% returns false if value match fails
%% returns new filter with value match portions removed
filter6([H | T], L, Flag, Acc) ->
	case string:tokens(H, "=") of
		[Key, Value] ->
			case lists:keyfind(Key, 1, L) of
				{_, Value} ->
					filter6(T, L, true, [Key | Acc]);
				{_, _} when Flag == undefined ->
					filter6(T, L, false, [Key | Acc]);
				_ ->
					filter6(T, L, Flag, [Key | Acc])
			end;
		[Key] ->
			filter6(T, L, Flag, [Key | Acc]);
		_ ->
			throw({error, 400})
	end;
filter6([], _, false, _Acc) ->
	false;
filter6([], _, _, Acc) ->
	lists:usort(lists:reverse(Acc)).

%% @hidden
%% expand parenthesized value lists
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

