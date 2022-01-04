#!/usr/bin/env escript
%% vim: syntax=erlang

main([]) ->
	case catch mnesia:system_info(db_nodes) of
		[] ->
			case ocs_app:install() of
				{ok, Tables} ->
					case mnesia:stop() of
						stopped ->
							io:fwrite("{ok, ~p}~n", [Tables]);
						{error, Reason} ->
							io:fwrite("error: ~w~n", [Reason]),
							erlang:halt(1)
					end;
				{error, Reason} ->
					stopped = mnesia:stop(),
					io:fwrite("error: ~w~n", [Reason]),
					erlang:halt(1)
			end;
		Nodes ->
			case mnesia:set_master_nodes(Nodes) of
				ok ->
					case ocs_app:install() of
						{ok, Tables} ->
							case mnesia:stop() of
								stopped ->
									io:fwrite("{ok, ~p}~n", [Tables]);
								{error, Reason} ->
									io:fwrite("error: ~w~n", [Reason]),
									erlang:halt(1)
							end;
						{error, Reason} ->
							stopped = mnesia:stop(),
							io:fwrite("error: ~w~n", [Reason]),
							erlang:halt(1)
					end;
				{error, Reason} ->
					stopped = mnesia:stop(),
					io:fwrite("error: ~w~n", [Reason]),
					erlang:halt(1)
			end
	end.
