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
-export([date/1, iso8601/1, timestamp/1]).

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
		Result			:: calendar:datetime().
%% @doc Convert timestamp to date and time or
%%		date and time to timeStamp.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH.

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds	:: pos_integer(),
		Result			:: string().
%% @doc Convert timestamp to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars).

-spec timestamp(ISO8610) -> Result
	when
		ISO8610	:: string(),
		Result	:: pos_integer() | {error, Reason},
		Reason	:: term().
%% @doc Convert ISO 8601 format date and time to timestamp.
timestamp(ISO8610) when is_list(ISO8610) ->
	DateFormat = "~4d-~2d-~2d",
	TimeFormat = "T~2d:~2d:~2d.~3dZ",
	case io_lib:fread(DateFormat ++ TimeFormat, ISO8610) of
		{ok, [Y, M, D, H, Min, S, MSR], _} ->
			DateTime = {{Y, M, D}, {H, Min, S + MSR}},
			date(DateTime);
		{error, Reason} ->
			{error, Reason}
	end.
