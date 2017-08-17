%%% ocs_test_lib.erl
%%%
-module(ocs_test_lib).

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
			case mnesia:wait_for_tables([client, subscriber], 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, Tables} = ocs_app:install(),
					F = fun(T) ->
						case T of
							T when T == client; T == subscriber;
									T == httpd_user; T == httpd_group ->
								true;
							_ ->
								false
						end
					end,
					lists:all(F, Tables),
					initialize_db();
				ok ->
					ok
			end
	end.

start() ->
	start([crypto, inets, asn1, public_key, ssl, xmerl, compiler,
		syntax_tools, mochiweb, radius, diameter, ocs]).

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
					case application:stop(diameter) of
						ok ->
							ok;
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

