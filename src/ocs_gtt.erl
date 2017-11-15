%%% ocs_gtt.erl
%%%---------------------------------------------------------------------
%%% @author Vance Shipley <vances@sigscale.com>
%%% @copyright 2017 SigScale Global Inc.
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
%%%	``1> gtt:insert(global_title, "1519240", "Bell Mobility").''
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
-copyright('Copyright (c) SigScale Global Inc.').
-author('vances@sigscale.com').

%% export API
-export([new/2, new/3, insert/2, insert/3, delete/2, lookup_first/2,
		lookup_last/2, lookup_all/2, backup/2, restore/2]).

-export([import_price/1]).

-record(gtt, {num, value}).

%%----------------------------------------------------------------------
%%  The GTT API
%%----------------------------------------------------------------------

%% @spec (Table, Options) -> ok
%% 	Table = atom()
%% 	Options = [{Copies, Nodes}]
%% 	Copies = disc_copies | disc_only_copies | ram_copies
%% 	Nodes = [atom()]
%%
%% @doc Create a new table.
%%  	The `Options' define table definitions used in {@link //mnesia}.
%% @see //mnesia/mnesia:create_table/2
%%
new(Table, Options) when is_list(Options) ->
	case mnesia:create_table(Table, Options ++
			[{attributes, record_info(fields, gtt)},
			{record_name, gtt}]) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Table, Options, Items) -> ok
%% 	Table = atom()
%% 	Options = [{Copies, Nodes}]
%% 	Copies = disc_copies | disc_only_copies | ram_copies
%% 	Nodes = [atom()]
%% 	Items = [{Number, Value}]
%% 	Number = string() | integer()
%% 	Value = term()
%%
%% @doc Create a new table and populate it from the supplied list of items.
%% 	This is the quickest way to build a new table as it performs
%% 	all the insertions within one optimized transaction context.
%%
%%  	The `Options' define table definitions used in {@link //mnesia}.
%% @see //mnesia/mnesia:create_table/2
%%
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
				Writes = insert(Table, [], Number, Value),
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

%% @spec (Table, Number, Value) -> ok
%% 	Table = atom()
%% 	Number = string() | integer()
%% 	Value = term()
%%
%% @doc Insert a table entry.
%% 
insert(Table, Number, Value) when is_integer(Number) ->
	insert(Table, integer_to_list(Number), Value);
insert(Table, Number, Value) 
		when is_atom(Table),
		is_list(Number) ->
	F = fun() -> insert(Table, [], Number, Value) end,
	case mnesia:transaction(F) of
		{atomic, N} when N > 0 ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.
	
%% @spec (Table, Items) -> ok
%% 	Table = atom()
%% 	Items = [{Number, Value}]
%% 	Number = string() | integer()
%% 	Value = term()
%%
%% @doc Insert a list of table entries.
%% 	The entries are inserted as a transaction, either all entries
%% 	are added to the table or, if an entry insertion fails, none at
%% 	all.
%% 
insert(Table, Items) when is_atom(Table), is_list(Items)  ->
	InsFun = fun({Number, Value}) -> insert(Table, [], Number, Value) end,
	TransFun = fun() -> lists:foreach(InsFun, Items) end,
	mnesia:transaction(TransFun),
	ok.

%% @spec (Table, Number) -> ok
%% 	Table = atom()
%% 	Number = string() | integer()
%%
%% @doc Delete a table entry.
%% 
delete(Table, Number) when is_integer(Number) ->
	delete(Table, integer_to_list(Number));
delete(Table, Number) when is_atom(Table), is_list(Number) ->
	Fun = fun() -> mnesia:delete(Table, Number, write) end,
	mnesia:transaction(Fun).
	
%% @spec (Table, Number) -> Value
%% 	Table = atom()
%% 	Number = string() | integer()
%% 	Value = term()
%%
%% @doc Lookup the value of the first matching table entry.
%% 
lookup_first(Table, Number) when is_integer(Number) ->
	lookup_first(Table, integer_to_list(Number));
lookup_first(Table, [Digit | Rest]) when is_atom(Table) ->
	Fun1 = fun(F, [H|T], [#gtt{num = Prefix, value = undefined}]) ->
				F(F, T, mnesia:read(Table, Prefix ++ [H], read));
			(_, _, [#gtt{value = Result}]) ->
				Result;
			(_, _, []) ->
				undefined
	end,
	Fun2 = fun() -> Fun1(Fun1, Rest, mnesia:read(Table, [Digit], read)) end,
	case mnesia:transaction(Fun2) of
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Table, Number) -> Value
%% 	Table = atom()
%% 	Number = string() | integer()
%% 	Value = term()
%%
%% @doc Lookup the value of the longest matching table entry.
%% 
lookup_last(Table, Number) when is_integer(Number) ->
	lookup_last(Table, integer_to_list(Number));
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
	case mnesia:transaction(Fun2) of
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Table, Number) -> Value
%% 	Table = atom()
%% 	Number = list() | integer()
%% 	Value = term()
%%
%% @doc Lookup the values of matching table entries.
%% 
lookup_all(Table, Number) when is_integer(Number) ->
	lookup_all(Table, integer_to_list(Number));
lookup_all(Table, [Digit | Rest]) when is_atom(Table) ->
	Fun1 = fun(F, [H|T], [#gtt{num = Prefix, value = undefined}], Acc) ->
				F(F, T, mnesia:read(Table, Prefix ++ [H], read), Acc);
			(F, [H|T], [#gtt{num = Prefix} = Entry], Acc) ->
				F(F, T, mnesia:read(Table, Prefix ++ [H], read), [Entry | Acc]);
			(_, _, [], Acc) ->
				lists:reverse(Acc)	
	end,
	Fun2 = fun() -> Fun1(Fun1, Rest, mnesia:read(Table, [Digit], read), []) end,
	case mnesia:transaction(Fun2) of
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Tables, File) -> ok
%% 	Tables = atom() | [atom()]
%% 	File = string()
%% 
%% @doc Create a backup of the named table(s) in `File.BUPTMP'.
%% 
backup(Table, File) when is_atom(Table) ->
	backup([Table], File);
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

%% @spec (Tables, File) -> {ok,  RestoredTabs}
%% 	Tables = atom() | [atom()]
%% 	File = string()
%% 	RestoredTabs = [atom()]
%% 
%% @doc Restore the named table(s) from the backup in `File.BUPTMP'.
%% 
restore(Table, File) when is_atom(Table) ->
	restore([Table], File);
restore(Tables, File) when is_list(Tables), is_list(File) ->
	case mnesia:restore(File, [{clear_tables, Tables}]) of
		{atomic, RestoredTabs} ->
			{ok,  RestoredTabs};
		{aborted, Reason} ->
			exit(Reason)
	end.

import_price(File) ->
	import(File, price_t).

-spec import(File, Recordname) -> ok
	when
		File :: string(),
		Recordname :: atom().
%% @doc
import(File, Recordname) ->
	case file:read_file(File) of
		{ok, Records} ->
			Basename = filename:basename(File),
			Table = list_to_atom( string:sub_string(Basename,
				1, string:rchr(Basename, $.) - 1)),
			import(Table, Recordname, Records);
		{error, Reason} ->
			exit(file:format_error(Reason))
	end.
%% @hidden
import(Table, Recordname, Records) ->
	ok = new(Table, []),
	F = fun() ->
			import(Table, Recordname,
				binary:split(Records, [<<"\n">>], [global]), [])
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.
%% @hidden
import(Table, _, [<<>>], Acc) ->
	F = fun(#gtt{} = G) ->
		mnesia:write(Table, G, write)
	end,
	lists:foreach(F, Acc);
import(Table, Recordname, [Chunk | Rest], Acc) ->
	H = list_to_tuple([Recordname | string:tokens(binary_to_list(Chunk), ",\"\\")]),
	import(Table, Recordname, Rest, [#gtt{num = element(2, H), value = H} | Acc]).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @spec (Table, Acc, Number, Value) -> Writes
%% 	Table = atom()
%% 	Acc = list()
%% 	Number = list() | integer()
%% 	Value = term()
%% 	Writes = integer()
%% @hidden
%%
insert(Table, [], Number, Value) when is_integer(Number) ->
	insert(Table, [], integer_to_list(Number), Value, 0);
insert(Table, [], Number, Value) ->
	insert(Table, [], Number, Value, 0).
%% @hidden
insert(Table, P, [H | []], Value, N) ->
	Number =  P ++ [H],
	mnesia:write(Table, #gtt{num = Number, value = Value}, write),
	N + 1;
insert(Table, P, [H | T], Value, N) ->
	Number =  P ++ [H],
	case mnesia:read(Table, Number, write) of
		[#gtt{}] ->
			insert(Table, Number, T, Value, N);
		[] ->
			mnesia:write(Table, #gtt{num = Number}, write),
			insert(Table, Number, T, Value, N + 1)
	end.

