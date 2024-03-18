#!/usr/bin/env escript
%% vim: syntax=erlang
%%! -sname snapshot

main(Args) ->
	case options(Args) of
		#{help := true} = _Options ->
			usage();
		#{node := Node} = Options ->
			connect(list_to_atom(Node), Options);
		#{hostname := Hostname} = Options ->
			connect(list_to_atom("ocs@" ++ Hostname), Options)
	end.

connect(Node, Options) when is_atom(Node) ->
	connect(Node, Options, net_kernel:connect_node(Node)).

connect(Node, Options, true) ->
	backup(Node, Options);
connect(_Node, _Options, false) ->
	io:fwrite("connection failed~n"),
	halt(1).

backup(Node, #{path := Path} = _Options)
		when is_atom(Node), is_list(Path) ->
	try
		erpc:call(Node, mnesia, backup, [Path])
	of
		ok ->
			io:fwrite("Backup written to ~s~n", [Path]),
			ok;
		{error, {'EXIT', {error, {file_error, _, Reason}}}} ->
			io:fwrite("Backup failed with reason: ~w~n", [Reason]),
			halt(1);
		{error, Reason} ->
			io:fwrite("Backup failed with reason: ~w~n", [Reason]),
			halt(1)
	catch
		exit:{exception, Reason} ->
			io:fwrite("Backup failed with EXIT reason: ~w~n", [Reason]),
			halt(1);
		exit:{exception, Reason, _StackTrace} ->
			io:fwrite("Backup failed with EXIT reason: ~w~n", [Reason]),
			halt(1);
		error:{erpc, Reason} ->
			io:fwrite("Backup failed with RPC reason: ~w~n", [Reason]),
			halt(1)
	end;
backup(Node, #{} = Options) ->
	backup(Node, Options#{path => "mnesia.bup"}).

usage() ->
	usage(net:gethostname()).

usage({ok, Hostname}) ->
	usage1(Hostname);
usage({error, Reason}) ->
	throw(Reason).

usage1(Hostname) ->
	Option1 = " [--file mnesia.bup]",
	Option2 = " [--node ocs@" ++ Hostname ++ "]",
	Options = [Option1, Option2],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	case net:gethostname() of
		{ok, Hostname} ->
			options(Args, #{hostname => Hostname});
		{error, Reason} ->
			throw(Reason)
	end.

options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--file", Path | T], Acc) ->
	options(T, Acc#{path => Path});
options(["--node", Node | T], Acc) ->
	options(T, Acc#{node => Node});
options([_H | _T], _Acc) ->
	usage();
options([], Acc) ->
	Acc.

