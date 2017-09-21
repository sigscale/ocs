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

-export([filter/2]).
-export([date/1, iso8601/1]).
-export([parse/1, merge_patch/2]).

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
%%
%% 	Returns a new JSON object with only the matching items.
%% 
filter(Filters, {struct, L} = _Object) when is_list(Filters) ->
	Filters1 = [string:tokens(F, ".") || F <- Filters],
	Filters2 = filter1(lists:usort(Filters1), []),
	{struct, filter2(Filters2, L, [])}.


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
filter1([Filter | T], []) ->
	[Name | RevPath] = lists:reverse(Filter),
	filter1(T, [{[Name], RevPath}]);
filter1([Filter | T1], [{Names, RevPath} | T2] = Acc) ->
	case lists:reverse(Filter) of
		[Name | RevPath] ->
			filter1(T1, [{[Name | Names], RevPath} | T2]);
		[Name | NewRevPath] ->
			filter1(T1, [{[Name], NewRevPath} | Acc])
	end;
filter1([], Acc) ->
	[{lists:reverse(Path), lists:reverse(Names)}
			|| {Names, Path} <- lists:reverse(Acc)].

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
		{H, {struct, L2}} ->
			F1 = fun({[Prefix | Suffix], Names}) when Prefix == H ->
						{true, {Suffix, Names}};
					(_) ->
						false
			end,
			SubFilters = lists:filtermap(F1, Filters),
			NewAcc = [{H, {struct, filter2(SubFilters, L2, [])}} | Acc],
			F2 = fun({[Prefix | _], _}) when Prefix == H ->
						true;
					(_) ->
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
