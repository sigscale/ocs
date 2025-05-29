%%% ocs_gtt.erl
%%%---------------------------------------------------------------------
%%% @author Vance Shipley <vances@sigscale.com>
%%% @copyright 2017 - 2025 SigScale Global Inc.
%%% @end
%%%---------------------------------------------------------------------
%%% @doc Global Title Table.
%%% 	This module implements generic prefix matching tables for digit
%%% 	strings using an {@link //mnesia} backing store.  Prefix matching
%%% 	may be done effeciently because each unique prefix is stored
%%% 	in the table.  A lookup for `"1519"' may be done in up to four
%%%	steps.  First find `"1"' as a key, if the key is not found the
%%% 	prefix does not exist in the table.  If the value for the key
%%% 	is undefined lookup `"15"' and if that key's value is undefined
%%% 	lookup `"151"' This continues until either the key is not found
%%% 	or the value is not undefined.
%%%
%%% 	The example below shows the table contents after an initial entry
%%%	of the form:<br />
%%%	``1> ocs_gtt:insert(global_title, "1519240", "Bell Mobility").''
%%%	```
%%%		{gtt, [1], undefined}
%%%		{gtt, [1,5], undefined}
%%%		{gtt, [1,5,1], undefined}
%%%		{gtt, [1,5,1,9], undefined}
%%%		{gtt, [1,5,1,9,2], undefined}
%%%		{gtt, [1,5,1,9,2,4], undefined}
%%%		{gtt, [1,5,1,9,2,4,0], "Bell Mobility"}
%%% 	'''
%%%
%%% 	<strong>Note:</strong> <emp>There is no attempt made to clean the
%%% 	table properly when a prefix is deleted.</emp>
%%%
%%% @todo Implement garbage collection.
%%% @end
%%%
-module(ocs_gtt).
-copyright('Copyright (c) 2017 - 2025 SigScale Global Inc.').
-author('vances@sigscale.com').

%% export API
-export([new/2, new/3, insert/2, insert/3, delete/2, lookup_first/2,
		lookup_last/2, lookup_all/2, list/0, list/2, clear_table/1,
		query/3]).

-export_type([gtt/0]).

-define(CHUNKSIZE, 100).

-include("ocs.hrl").

-type gtt() :: #gtt{}.

%%----------------------------------------------------------------------
%%  The GTT API
%%----------------------------------------------------------------------

-spec new(Table, Options) -> ok
	when
		Table :: atom() | string(),
		Options :: [{Copies, Nodes}],
		Copies :: disc_copies | disc_only_copies | ram_copies,
		Nodes :: [atom()].
%% @doc Create a new table.
%%  	The `Options' define table definitions used in {@link //mnesia}.
%% @see //mnesia/mnesia:create_table/2
%%
new(Table, []) ->
	Nodes = [node() | nodes()],
	new(Table, [{disc_copies, Nodes}]);
new(Table, Options) when is_list(Table) ->
	new(list_to_atom(Table), Options);
new(Table, Options) when is_list(Options) ->
	case mnesia:create_table(Table, Options ++
			[{attributes, record_info(fields, gtt)},
			{user_properties, [{ocs, true}]},
			{record_name, gtt}]) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec new(Table, Options, Items) -> ok
	when
		Table :: atom() | string(),
		Options :: [{Copies, Nodes}],
		Copies :: disc_copies | disc_only_copies | ram_copies,
		Nodes :: [atom()],
		Items :: [{Number, Value}],
		Number :: string() | integer(),
		Value :: term().
%% @doc Create a new table and populate it from the supplied list of items.
%% 	This is the quickest way to build a new table as it performs
%% 	all the insertions within one optimized transaction context.
%%
%%  	The `Options' define table definitions used in {@link //mnesia}.
%% @see //mnesia/mnesia:create_table/2
%%
new(Table, [], Items) ->
	Nodes = [node() | nodes()],
	new(Table, [{disc_copies, Nodes}], Items);
new(Table, Options, Items) when is_list(Table) ->
	new(list_to_atom(Table), Options, Items);
new(Table, Options, Items) when is_list(Options), is_list(Items) ->
	mnesia:create_table(Table, Options ++
			[{attributes, record_info(fields, gtt)},
			{user_properties, [{ocs, true}]},
			{record_name, gtt}]),
	Threshold = mnesia:system_info(dump_log_write_threshold) - 1,
	Ftran = fun(F, [{Number, Value} | T], N) when is_integer(Number) ->
				F(F, [{integer_to_list(Number), Value} | T], N);
			(F, [{Number, _Value} | _T] = L, N) when length(Number) > N ->
				mnesia:dump_log(),
				F(F, L, Threshold);
			(F, [{Number, Value} | T], N) ->
				{Writes, _} = insert(Table, Number, Value, []),
				F(F, T, N - Writes);
			(_F, [], _N) ->
				ok
	end,
	case {lists:keysearch(disc_copies, 1, Options),
			lists:keysearch(disc_only_copies, 1, Options)} of
		{DiscCopies, DiscOnlyCopies} when
				DiscCopies == false, DiscOnlyCopies == false;
				DiscCopies == false, DiscOnlyCopies == [];
				DiscCopies == [] , DiscOnlyCopies == false;
				DiscCopies == [] , DiscOnlyCopies == [] ->
			mnesia:ets(Ftran, [Ftran, Items, Threshold]);
		_ ->
			mnesia:sync_dirty(Ftran, [Ftran, Items, Threshold])
	end.

-spec insert(Table, Number, Value) -> Result
	when
		Table :: atom() | string(),
		Number :: string(),
		Value :: term(),
		Result :: {ok, gtt()}.
%% @doc Insert a table entry.
%%
insert(Table, Number, Value) when is_list(Table) ->
	insert(list_to_existing_atom(Table), Number, Value);
insert(Table, Number, Value) when is_atom(Table), is_list(Number) ->
	F = fun() -> insert(Table, Number, Value, []) end,
	case mnesia:transaction(F) of
		{atomic, {_NumWrites, Gtt}} ->
			{ok, Gtt};
		{aborted, Reason} ->
			exit(Reason)
	end.
	
-spec insert(Table, Items) -> ok
	when
		Table :: atom() | string(),
		Items :: [{Number, Value}],
		Number :: string(),
		Value :: term().
%% @doc Insert a list of table entries.
%% 	The entries are inserted as a transaction, either all entries
%% 	are added to the table or, if an entry insertion fails, none at
%% 	all.
%%
insert(Table, Items) when is_list(Table) ->
	insert(list_to_existing_atom(Table), Items);
insert(Table, Items) when is_atom(Table), is_list(Items)  ->
	InsFun = fun({Number, Value}) -> insert(Table, Number, Value) end,
	TransFun = fun() -> lists:foreach(InsFun, Items) end,
	mnesia:transaction(TransFun),
	ok.

-spec delete(Table, Number) -> ok
	when
		Table :: atom() | string(),
		Number :: string().
%% @doc Delete a table entry.
%%
delete(Table, Number) when is_list(Table) ->
	delete(list_to_existing_atom(Table), Number);
delete(Table, Number) when is_atom(Table), is_list(Number) ->
	Fun = fun() ->
			case mnesia:read(Table, Number) of
				[#gtt{}] ->
					mnesia:delete(Table, Number, write);
				[] ->
					mnesia:abort(not_found)
			end
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.
	
-spec lookup_first(Table, Number) -> Result
	when
		Table :: atom() | string(),
		Number :: string(),
		Result :: Value | undefined,
		Value :: term().
%% @doc Lookup the value of the first matching table entry.
%%
lookup_first(Table, Number) when is_list(Table) ->
	lookup_first(list_to_existing_atom(Table), Number);
lookup_first(Table, [Digit | Rest]) when is_atom(Table) ->
	Fun1 = fun(F, [H|T], [#gtt{num = Prefix, value = undefined}]) ->
				F(F, T, mnesia:read(Table, Prefix ++ [H], read));
			(_, _, [#gtt{value = Result}]) ->
				Result;
			(_, _, []) ->
				undefined
	end,
	Fun2 = fun() -> Fun1(Fun1, Rest, mnesia:read(Table, [Digit], read)) end,
	mnesia:ets(Fun2).

-spec lookup_last(Table, Number) -> Result
	when
		Table :: atom() | string(),
		Number :: string(),
		Result :: Value | undefined,
		Value :: term().
%% @doc Lookup the value of the longest matching table entry.
%%
lookup_last(Table, Number) when is_list(Table) ->
	lookup_last(list_to_existing_atom(Table), Number);
lookup_last(Table, Number) when is_atom(Table) ->
	Fun1 = fun(F, [_|T], []) ->
				F(F, T, mnesia:read(Table, lists:reverse(T), read));
			(F, [_|T], [#gtt{value = undefined}]) ->
				F(F, T, mnesia:read(Table, lists:reverse(T), read));
			(_, _, [#gtt{value = Result}]) ->
				Result;
			(_, [], _) ->
				undefined
	end,
	Fun2 = fun() ->
				Fun1(Fun1, lists:reverse(Number), mnesia:read(Table, Number, read))
	end,
	mnesia:ets(Fun2).

-spec lookup_all(Table, Number) -> Result
	when
		Table :: atom() | string(),
		Number :: string(),
		Result :: [Value],
		Value :: term().
%% @doc Lookup the values of matching table entries.
%%
lookup_all(Table, Number) when is_list(Table) ->
	lookup_all(list_to_existing_atom(Table), Number);
lookup_all(Table, [Digit | Rest]) when is_atom(Table) ->
	Fun1 = fun(F, [H|T], [#gtt{num = Prefix, value = undefined}], Acc) ->
				F(F, T, mnesia:read(Table, Prefix ++ [H], read), Acc);
			(F, [H|T], [#gtt{num = Prefix} = Entry], Acc) ->
				F(F, T, mnesia:read(Table, Prefix ++ [H], read), [Entry | Acc]);
			(_, _, [], Acc) ->
				lists:reverse(Acc)	
	end,
	Fun2 = fun() -> Fun1(Fun1, Rest, mnesia:read(Table, [Digit], read), []) end,
	mnesia:ets(Fun2).

-spec list() -> Tables
	when
		Tables :: [Table],
		Table :: atom().
%% @doc List all tables.
list() ->
	list_tables(mnesia:system_info(tables), []).
%% @hidden
list_tables([H | T], Acc) ->
	case mnesia:table_info(H, record_name) of
		gtt ->
			list_tables(T, [H | Acc]);
		_ ->
			list_tables(T, Acc)
	end;
list_tables([], Acc) ->
	lists:reverse(Acc).

-spec list(Cont, Table) -> Result
	when
		Cont :: start,
		Table :: atom(),
		Result :: {Cont1, [gtt()]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc List all gtt entries.
list(start, Table) when is_atom(Table) ->
	MatchSpec = [{#gtt{value = '$2', _ = '_'},
			[{'/=', '$2', undefined}], ['$_']}],
	F = fun() ->
		mnesia:select(Table, MatchSpec, ?CHUNKSIZE, read)
	end,
	list(mnesia:ets(F));
list(Cont, _Table) ->
	F = fun() ->
		mnesia:select(Cont)
	end,
	list(mnesia:ets(F)).
%% @hidden
list({[#gtt{} | _] = Gtts, Cont}) ->
	{Cont, Gtts};
list('$end_of_table') ->
	{eof, []}.

-spec query(Cont, Table, MatchPrefix) -> Result
	when
		Cont :: start | any(),
		Table :: atom(),
		MatchPrefix :: Match,
		Match :: {exact, string()} | {like, string()} | '_',
		Result :: {Cont1, [gtt()]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Paginated filtered query of table.
query(Cont, Table, '_') ->
	MatchHead = #gtt{_ = '_'},
	MatchSpec = [{MatchHead, [], ['$_']}],
	query1(Cont, Table, MatchSpec);
query(Cont, Table, {Op, String})
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead = case lists:last(String) of
		$% when Op == like ->
			#gtt{num = lists:droplast(String) ++ '_', _ = '_'};
		_ ->
			#gtt{num = String, _ = '_'}
	end,
	MatchSpec = [{MatchHead, [], ['$_']}],
	query1(Cont, Table, MatchSpec).
%% @hidden
query1(start, Table, MatchSpec) when is_atom(Table) ->
	F = fun() ->
		mnesia:select(Table, MatchSpec, ?CHUNKSIZE, read)
	end,
	query2(mnesia:ets(F));
query1(Cont, _Table, _MatchSpec) ->
	F = fun() ->
		mnesia:select(Cont)
	end,
	query2(mnesia:ets(F)).
%% @hidden
query2({[#gtt{} | _] = Gtts, Cont}) ->
	query3(Gtts, Cont, []);
query2('$end_of_table') ->
	{eof, []}.
%% @hidden
query3([#gtt{value = undefined} | T], Cont, Acc) ->
   query3(T, Cont, Acc);
query3([#gtt{} = Gtt | T], Cont, Acc) ->
   query3(T, Cont, [Gtt | Acc]);
query3([], Cont, Acc) ->
   {Cont, Acc}. 

-spec clear_table(Table) -> ok
	when
		Table :: atom() | string().
%% @doc Clear a table.
%%
clear_table(Table) when is_list(Table) ->
	clear_table(list_to_existing_atom(Table));
clear_table(Table) when is_atom(Table) ->
	case mnesia:clear_table(Table) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec insert(Table, Number, Value, []) -> {NumWrites, gtt()}
	when
		Table :: atom(),
		Number :: list() | integer(),
		Value :: term(),
		NumWrites :: integer().
%% @hidden
%%
insert(Table, Number, Value, []) when is_integer(Number) ->
	insert(Table, integer_to_list(Number), Value, 0, []);
insert(Table, Number, Value, []) ->
	insert(Table, Number, Value, 0, []).
%% @hidden
insert(Table, [H | []], Value, NumWrites, Acc) ->
	Number =  Acc ++ [H],
	LM = {erlang:system_time(millisecond),
			erlang:unique_integer([positive])},
	Value1 = erlang:insert_element(tuple_size(Value) + 1, Value, LM),
	Gtt = #gtt{num = Number, value = Value1},
	mnesia:write(Table, Gtt, write),
	{NumWrites + 1, Gtt};
insert(Table, [H | T], Value, NumWrites, Acc) ->
	Number =  Acc ++ [H],
	case mnesia:read(Table, Number, write) of
		[#gtt{}] ->
			insert(Table, T, Value, NumWrites, Number);
		[] ->
			mnesia:write(Table, #gtt{num = Number}, write),
			insert(Table, T, Value, NumWrites + 1, Number)
	end.

