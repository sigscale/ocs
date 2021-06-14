%%% ocs_gtt.erl
%%%---------------------------------------------------------------------
%%% @author Vance Shipley <vances@sigscale.com>
%%% @copyright 2017 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2017 - 2021 SigScale Global Inc.').
-author('vances@sigscale.com').

%% export API
-export([new/2, new/3, insert/2, insert/3, delete/2, lookup_first/2,
		lookup_last/2, lookup_all/2, list/0, list/2, backup/2, restore/2,
		import/1, clear_table/1]).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(CHUNKSIZE, 100).

-include("ocs.hrl").

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
		Result :: {ok, #gtt{}}.
%% @doc Insert a table entry.
%% 
insert(Table, Number, Value) when is_list(Table) ->
	insert(list_to_existing_atom(Table), Number, Value);
insert(Table, Number, Value) when is_atom(Table), is_list(Number) ->
	F = fun() -> insert(Table, Number, Value, []) end,
	case mnesia:transaction(F) of
		{atomic, {_NumWrites, Gtt}} ->
			ocs_event:notify(insert_gtt, {Table, Gtt}, resource),
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
				[#gtt{} = Gtt] ->
					{mnesia:delete(Table, Number, write), Gtt};
				[] ->
					mnesia:abort(not_found)
			end
	end,
	case mnesia:transaction(Fun) of
		{atomic, {ok, Gtt}} ->
			ocs_event:notify(delete_gtt, {Table, Gtt}, resource),
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.
	
-spec lookup_first(Table, Number) -> Value
	when
		Table :: atom() | string(),
		Number :: string(),
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

-spec lookup_last(Table, Number) -> Value
	when
		Table :: atom() | string(),
		Number :: string(),
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

-spec lookup_all(Table, Number) -> Value
	when
		Table :: atom() | string(),
		Number :: string(),
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

-spec backup(Tables, File) -> ok
	when
		Tables :: Table | [Table],
		Table :: atom() | string(),
		File :: string().
%% @doc Create a backup of the named table(s) in `File.BUPTMP'.
%% 
backup([H | _ ] = Tables, File) when is_list(H) ->
	backup([list_to_existing_atom(T) || T <- Tables], File);
backup(Tables, File) when is_list(Tables), is_integer(hd(Tables)) ->
	backup(list_to_existing_atom(Tables), File);
backup(Tables, File) when is_atom(Tables) ->
	backup([Tables], File);
backup(Tables, File) when is_list(Tables), is_list(File) ->
	case mnesia:activate_checkpoint([{max, Tables}]) of
		{ok, Name, _Nodes} ->
			case mnesia:backup_checkpoint(Name, File) of
				ok ->
					mnesia:deactivate_checkpoint(Name),
					ok;
				{error,Reason} ->
					exit(Reason)
			end;
		{error,Reason} ->
			exit(Reason)
	end.

-spec restore(Tables, File) -> {ok,  RestoredTabs}
	when
		Tables :: Table | [Table],
		Table :: atom() | string(),
		File :: string(),
		RestoredTabs :: [atom()].
%% @doc Restore the named table(s) from the backup in `File.BUPTMP'.
%% 
restore([H | _ ] = Tables, File) when is_list(H) ->
	restore([list_to_existing_atom(T) || T <- Tables], File);
restore(Tables, File) when is_list(Tables), is_integer(hd(Tables)) ->
	restore(list_to_existing_atom(Tables), File);
restore(Tables, File) when is_atom(Tables) ->
	restore([Tables], File);
restore(Tables, File) when is_list(Tables), is_list(File) ->
	case mnesia:restore(File, [{clear_tables, Tables}]) of
		{atomic, RestoredTabs} ->
			{ok,  RestoredTabs};
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec import(File) -> ok
	when
		File :: string().
%% @doc Import table from file.
%%
%% 	Create a new table, or overwrite existing table, from
%% 	the contents of a comma separated values (CSV) format
%% 	file of the form `Prefix,Description,Rate'.
%% 	Where `Prefix' is a string of digits, `Description'
%% 	is a string and `Rate' is a decimal currency amount
%% 	with no more than six digits to the right of the
%% 	decimal point.
%%
%% 	Example:
%% 	```
%% 	1416,"Toronto, Ontario, Canada",0.0123
%% 	1212,"New York, New York, United States",0.011
%% 	1800,Toll Free,0
%% 	94,Sri Lanka,1.15
%% 	'''
%%
import(File) ->
	case file:read_file(File) of
		{ok, Binary} ->
			Basename = filename:basename(File),
			Table = list_to_atom(string:sub_string(Basename,
					1, string:rchr(Basename, $.) - 1)),
			import1(Table, Binary, unicode:bom_to_encoding(Binary));
		{error, Reason} ->
			exit(file:format_error(Reason))
	end.
%% @hidden
import1(Table, Binary, {latin1, 0}) ->
	import2(Table, Binary);
import1(Table, Binary, {utf8, Offset}) ->
	Length = size(Binary) - Offset,
	import2(Table, binary:part(Binary, Offset, Length)).
%% @hidden
import2(Table, Records) ->
	case mnesia:create_table(Table,
			[{disc_copies, [node() | nodes()]},
			{attributes, record_info(fields, gtt)},
			{record_name, gtt}]) of
		{atomic, ok} ->
			import3(Table, Records);
		{aborted, {already_exists, Table}} ->
			case mnesia:clear_table(Table) of
				{atomic, ok} ->
					import3(Table, Records);
				{aborted, Reason} ->
					exit(Reason)
			end;
		{aborted, Reason} ->
			exit(Reason)
	end.
%% @hidden
import3(Table, Records) ->
	Split = binary:split(Records, [<<"\n">>, <<"\r">>, <<"\r\n">>], [global]),
	F = fun() ->
				import4(Table, Split, [])
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.
%% @hidden
import4(Table, [], Acc) ->
	F = fun({Key, Value}) ->
				{_NumWrites, #gtt{}} = insert(Table, Key, Value, []),
				ok
	end,
	lists:foreach(F, lists:flatten(Acc));
import4(Table, [<<>> | T], Acc) ->
	import4(Table, T, Acc);
import4(Table, [Chunk | Rest], Acc) ->
	case binary:split(Chunk, [<<"\"">>], [global]) of
		[Chunk] ->
			NewAcc = [import5(binary:split(Chunk, [<<",">>], [global]), []) | Acc],
			import4(Table, Rest, NewAcc);
		SplitChunks ->
			F = fun(<<$, , T/binary>>, AccIn) ->
						[T | AccIn];
					(<<>>, AccIn) ->
						[<<>> | AccIn];
					(C, AccIn) ->
						case binary:at(C, size(C) - 1) of
							$, ->
								[binary:part(C, 0, size(C) - 1) | AccIn];
							_ ->
								[C | AccIn]
						end
			end,
			AccOut = lists:foldl(F, [], SplitChunks),
			NewAcc = [import5(lists:reverse(AccOut), []) | Acc],
			import4(Table, Rest, NewAcc)
	end.
%% @hidden
import5([<<>> | T], Acc) ->
	import5(T, [undefined | Acc]);
import5([H | T], Acc) ->
	import5(T, [binary_to_list(H) | Acc]);
import5([], Acc) when length(Acc) == 3 ->
	import6(lists:reverse(Acc));
import5([], _Acc) ->
	[].
%% @hidden
import6([Key, Desc, Rate]) ->
	Tuple  = {Desc, ocs_rest:millionths_in(Rate)},
	case is_key_number(Key) of
		true->
			{Key, Tuple};
		false ->
			exit(invalid_key)
	end.

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
		Result :: {Cont1, [#gtt{}]} | {error, Reason},
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

-spec insert(Table, Number, Value, []) -> {NumWrites, #gtt{}}
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
	LM = {erlang:system_time(?MILLISECOND),
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

%% @hidden
is_key_number([$0 | T]) ->
	is_key_number(T);
is_key_number([$1 | T]) ->
	is_key_number(T);
is_key_number([$2 | T]) ->
	is_key_number(T);
is_key_number([$3 | T]) ->
	is_key_number(T);
is_key_number([$4 | T]) ->
	is_key_number(T);
is_key_number([$5 | T]) ->
	is_key_number(T);
is_key_number([$6 | T]) ->
	is_key_number(T);
is_key_number([$7 | T]) ->
	is_key_number(T);
is_key_number([$8 | T]) ->
	is_key_number(T);
is_key_number([$9 | T]) ->
	is_key_number(T);
is_key_number([]) ->
	true;
is_key_number(_) ->
	false.

