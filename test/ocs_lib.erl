%%% ocs_lib.erl
%%%
-module(ocs_lib).

-export([initialize_db/0, start/0]).

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
			case mnesia_wait_for_tables([radius_client, radius_user], 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					ok = 	mnesia:create_schema([node()]),
					ok = mnesia:start(),
					{ok, [radius_client, radius_user]} = ocs_app:install([node()]),
					initialize_db();
				ok ->
					ok
			end
	end.

start() ->
	webmachine_util:ensure_all_started(webmachine),
	case application:start(ocs) of
		ok ->
			ok;
		{error, {already_started, _}} ->
			ok;
		{error, Reason} ->
			{error, Reason]
	end.
