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
		Filters :: [string()],
		JsonObject :: tuple(),
		Result :: tuple().
%% @doc Filter a JSON object.
%%
%% 	Each filter in `Filters' is the name of a member in the JSON
%% 	encoded `JsonObject'. A filter may refer to a complex type by
%% 	use of the "dot" path seperator character (e.g. `"a.b.c"').
%% 	Where an intermediate node on a complex path is an array
%% 	containing all matching array members will be included. To
%% 	filter out array members an `=value' suffix may be added.
%%
%% 	Returns a new JSON object with only the matching items.
%%
%% 	Example:
%% 	```
%% 	1> In = {struct,[{"a",{array,[{struct,[{"name","bob"},{"value",6}]},
%% 	1> {"b",7},{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]},
%% 	1> ocs_rest:filter(["b", "a.name=sue"], In).
%% 	{struct, [{"a",{array,[{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]}
%% 	'''
%%
%% @throws {error, 400}
%%
filter(Filters, {struct, L} = _Object) when is_list(Filters) ->
	Filters1 = [string:tokens(F, ".") || F <- Filters],
	{struct, filter1(lists:usort(Filters1), L, [])}.

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
filter2([{[], [H | T1]} | T2] = _Filters, L1, Acc) ->
	case lists:keyfind(H, 1, L1) of
		false ->
			filter2([{[], T1} | T2], L1, Acc);
		KV ->
			filter2([{[], T1} | T2], L1, [KV | Acc])
	end;
filter2([{[H | _], _} | T] = Filters, L1, Acc) ->
	case lists:keyfind(H, 1, L1) of
		false ->
			filter2(T, L1, Acc);
		{H, {Type, L2}} when Type == struct; Type == array ->
			F1 = fun({[Prefix | Suffix], Names}) when Prefix == H ->
						{true, {Suffix, Names}};
					(_) ->
						false
			end,
			SubFilters = lists:filtermap(F1, Filters),
			NewAcc = [{H, filter3(Type, SubFilters, L2)} | Acc],
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
filter4(Filters, [{struct, L} | T], Acc) ->
erlang:display({?MODULE, ?LINE, L}),
	case filter5(Filters, L, []) of
		false ->
erlang:display({?MODULE, ?LINE, false}),
			filter4(Filters, T, Acc);
		NewFilters ->
erlang:display({?MODULE, ?LINE, NewFilters}),
			filter4(NewFilters, T, [{struct, filter2(NewFilters, L, [])} | Acc])
	end;
filter4(Filters, [{array, L} | T], Acc) ->
	filter4(Filters, T, [{array, filter4(Filters, L, [])} | Acc]);
filter4(Filters, [_ | T], Acc) ->
	filter4(Filters, T, Acc);
filter4(_, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
filter5([{[], [Filter]} = H | T], L, Acc) ->
erlang:display({?MODULE, ?LINE, Filter}),
	case string:tokens(Filter, "=") of
		[Key, Value] ->
			case lists:keyfind(Key, 1, L) of
				{_, Value} ->
erlang:display({?MODULE, ?LINE, Value}),
					filter5(T, L, [{[], [Key]} | Acc]);
				_Other ->
erlang:display({?MODULE, ?LINE, _Other}),
					false
			end;
		_Other1 ->
erlang:display({?MODULE, ?LINE, _Other1}),
			filter5(T, L, [H | Acc])
	end;
filter5([H | T], L, Acc) ->
erlang:display({?MODULE, ?LINE, H}),
	filter5(T, L, [H | Acc]);
filter5([], _, Acc) ->
erlang:display({?MODULE, ?LINE, Acc}),
	lists:reverse(Acc).

