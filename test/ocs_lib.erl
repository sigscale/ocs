%%% ocs_lib.erl
%%%
-module(ocs_lib).

-export([initialize_db/0, start/0, stop/0]).

initialize_db() ->
	case mnesia:system_info(is_running) of
		no ->
			ok = application:start(mnesia),
			initialize_db();
		S when S == starting; S == stopping ->
			receive
				after 1000 ->
					initialize_db()
			end;
		yes ->
			case mnesia:wait_for_tables([radius_client, subscriber], 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					ok = 	mnesia:create_schema([node()]),
					ok = mnesia:start(),
					{ok, [radius_client, subscriber]} = ocs_app:install([node()]),
					initialize_db();
				ok ->
					ok
			end
	end.

start() ->
	start([crypto, asn1, public_key, ssl, inets, compiler,
			syntax_tools, xmerl, mochiweb, webmachine, radius, ocs]).

start([H | T]) ->
	case application:start(H) of
		ok  ->
			start(T);
		{error, {already_started, H}} ->
			start(T);
		{error, Reason} ->
			{error, Reason}
	end;
start([]) ->
	ok.

stop() ->
	case application:stop(ocs) of
		ok ->
			case application:stop(radius) of
				ok ->
					ok;
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

