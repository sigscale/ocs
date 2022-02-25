#!/usr/bin/env escript
%% vim: syntax=erlang

main([Type, LogFile, Format]) ->
	application:load(ocs),
	case ocs_log:ipdr_file(list_to_atom(Type), LogFile, list_to_atom(Format)) of
			ok ->
				io:fwrite("ok~n");
			{error, Reason} ->
				io:fwrite("error: ~w~n", [Reason]),
				erlang:halt(1)
	end;
main(_) ->
	usage().

usage() ->
	io:fwrite("usage: ~s Type LogFile Format
		Type = wlan | vopi
    		LogFile = export log name
    		Format = xml | xdr | csv ~n", [escript:script_name()]),
	halt(1).

