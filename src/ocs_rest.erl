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
	Fsort = fun(A, B) when length(A) =< length(B) ->
				true;
			(_, _) ->
				false
	end,
	Filters5 = lists:sort(Fsort, Filters4),
	filter1(Filters5, JsonObject, []).

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
filter1([Filter | T], JSON, []) ->
	[Name | RevPath] = lists:reverse(Filter),
	filter1(T, JSON, [{[Name], RevPath}]);
filter1([Filter | T1], JSON, [{Names, RevPath} | T2] = Acc) ->
	case lists:reverse(Filter) of
		[Name | RevPath] ->
			filter1(T1, JSON, [{[Name | Names], RevPath} | T2]);
		[Name | NewRevPath] ->
			filter1(T1, JSON, [{[Name], NewRevPath} | Acc])
	end;
filter1([], {Type, _} = JSON, Acc) when Type == struct; Type == array ->
	Filters = [{lists:reverse(Path), lists:reverse(Names)}
			|| {Names, Path} <- lists:reverse(Acc)],
	filter2(Filters, JSON).
%% @hidden
filter2(Filters, {struct, L1}) ->
	{struct, filter3(Filters, L1, [])};
filter2(Filters, {array, L1}) ->
	{array, filter4(Filters, L1, [])}.
%% @hidden
%% filter fields from object
filter3([{[], [H | T1]} | T2] = _Filters, L, Acc) ->
	case lists:keyfind(H, 1, L) of
		false ->
			filter3([{[], T1} | T2], L, Acc);
		KV ->
			filter3([{[], T1} | T2], L, [KV | Acc])
	end;
%% depth first traversal for complex filters
filter3([{[H | _], _} | T] = Filters, L1, Acc) ->
	case lists:keyfind(H, 1, L1) of
		false ->
			filter3(T, L1, Acc);
		{H, {Type, _} = JSON} when Type == struct; Type == array ->
			% remove parent from filter path
			F1 = fun({[Prefix | Suffix], Names}) when Prefix == H ->
						{true, {Suffix, Names}};
					(_) ->
						false
			end,
			SubFilters = lists:filtermap(F1, Filters),
			% filter descendants
			NewAcc = case filter2(SubFilters, JSON) of
				{_, []} ->
					Acc;
				NewJSON ->
					[{H, NewJSON} | Acc]
			end,
			% skip filters applied above
			F2 = fun({[Prefix | _], _}) when Prefix == H ->
						true;
					(_P) ->
						false
			end,
			NewFilters = lists:dropwhile(F2, Filters),
			filter3(NewFilters, L1, NewAcc);
		_ ->
			throw({error, 400})
	end;
filter3([{[], []} | T], L, Acc) ->
	filter3(T, L, Acc);
filter3([], _, Acc) ->
	lists:reverse(Acc).
%% @hidden
filter4(Filters, [{struct, L1} | T], Acc) ->
	NewAcc = case filter5(Filters, L1, []) of
		[] ->
			Acc;
		L2 ->
			[{struct, L2} | Acc]
	end,
	filter4(Filters, T, NewAcc);
filter4(Filters, [{array, L1} | T], Acc) ->
	NewAcc = case filter4(Filters, L1, []) of
		[] ->
			Acc;
		L2 ->
			[{array, L2} | Acc]
	end,
	filter4(Filters, T, NewAcc);
filter4(Filters, [{Key, _Value} = KV | T], Acc) ->
	case lists:member(Key, Filters) of
		true ->
			filter4(Filters, T, [KV | Acc]);
		false ->
			filter4(Filters, T, Acc)
	end;
filter4(Filters, [_ | T], Acc) ->
	filter4(Filters, T, Acc);
filter4(_, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
filter5([{[], Names} | T], L, Acc) ->
	filter6(Names, true, L, []);
filter5([], _, Acc) ->
	lists:reverse(Acc).
%% @hidden
%% check for value matches
filter6([H | T], Flag, L, Acc) ->
	filter7(string:tokens(H, "="), T, Flag, L, Acc);
filter6([], true, L, Acc) ->
	filter8(L, Acc, []);
filter6([], false, _, _Acc) ->
	[].
%% @hidden
filter7([Key, _], T, true, L, [Key | _] = Acc) ->
	filter6(T, true, L, Acc);
filter7([Key, Value], T, false, L, [Key | _] = Acc) ->
	case lists:keyfind(Key, 1, L) of
		{_, Value} ->
			filter6(T, true, L, Acc);
		{_, _} ->
			filter6(T, false, L, Acc)
	end;
filter7([Key, Value], T, true, L, Acc) ->
	case lists:keyfind(Key, 1, L) of
		{_, Value} ->
			filter6(T, true, L, [Key | Acc]);
		{_, _} ->
			filter6(T, false, L, [Key | Acc]);
		false ->
			[]
	end;
filter7(_, _, false, _, _) ->
	[];
filter7([Key], T, true, L, Acc) ->
	filter6(T, true, L, [Key | Acc]);
filter7(_, _, _, _, _) ->
	throw({error, 400}).
%% @hidden
filter8([{Key, _} = H | T], Keys, Acc) ->
	case lists:member(Key, Keys) of
		true ->
			filter8(T, Keys, [H | Acc]);
		false ->
			filter8(T, Keys, Acc)
	end;
filter8([], _, Acc) ->
	lists:reverse(Acc).

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

