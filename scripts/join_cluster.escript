#!/usr/bin/env escript
%% vim: syntax=erlang

main([Node]) ->
	case catch mnesia:system_info(db_nodes) of
		[] ->
			case ocs_app:join(list_to_atom(Node)) of
				{ok, Tables} ->
					io:fwrite("{ok, ~p}~n", [Tables]);
				{error, Reason} ->
					io:fwrite("error: ~w~n", [Reason]),
					erlang:halt(1)
			end;
		Nodes ->
			case mnesia:set_master_nodes(Nodes) of
				ok ->
					case ocs_app:join(list_to_atom(Node)) of
						{ok, Tables} ->
							io:fwrite("{ok, ~p}~n", [Tables]);
						{error, Reason} ->
							io:fwrite("error: ~w~n", [Reason]),
							erlang:halt(1)
					end;
				{error, Reason} ->
					io:fwrite("error: ~w~n", [Reason]),
					erlang:halt(1)
			end
	end.
