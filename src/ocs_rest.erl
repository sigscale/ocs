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

%%----------------------------------------------------------------------
%%  The ocs_rest public API
%%----------------------------------------------------------------------

-spec filter(Filters, JsonObject) -> Result
	when
		Filters :: [list(string())],
		JsonObject :: tuple(),
		Result :: tuple().
%% @doc Filters a JSON object.
%% 	Returns a new JSON object with only the matching items.
%% 
filter(Filters, {struct, L} = _Object) ->
	Filters1 = filter1(lists:usort(Filters), []),
	{struct, filter2(Filters1, L, [])}.

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

