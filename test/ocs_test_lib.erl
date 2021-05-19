%%% ocs_test_lib.erl
%%% vim: ts=3
%%%
-module(ocs_test_lib).

-export([initialize_db/0, start/0, stop/0]).
-export([load/1, unload/1]).
-export([ipv4/0, port/0, mac/0]).
-export([add_offer/0]).
-export([write_csv/2]).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-include("ocs.hrl").

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
			Tables = [httpd_group, httpd_user, nrf_ref, session, bucket, offer,
					resource, product, service, client],
			case mnesia:wait_for_tables([client, service, offer], 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, Tables} = ocs_app:install(),
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
			ok = unload(ocs),
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

load(Application) ->
	case application:load(Application) of
		ok ->
			ok;
		{error, {already_loaded, Application}} ->
			ok = unload(Application),
			load(Application);
		{error, {running, Application}} ->
			ok = application:stop(Application),
			ok = unload(Application),
			load(Application)
	end.

unload(Application) ->
	case application:unload(Application) of
		ok ->
			ok;
		{error, {running, Application}} ->
			ok = application:stop(Application),
			unload(Application);
		{error, {not_loaded, Application}} ->
			ok
	end.

add_offer() ->
	Price1 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly, amount = 1995,
			alteration = #alteration{name = ocs:generate_password(),
					type = recurring, period = monthly,
					units = octets, size = 2000000000, amount = 0}},
	Price2 = #price{name = ocs:generate_password(),
			type = usage, units = octets,
			size = 100000000, amount = 100},
	Prices = [Price1, Price2],
	OfferName = ocs:generate_password(),
	Offer = #offer{name = OfferName, status = active,
			specification = 8, price = Prices},
	case ocs:add_offer(Offer) of
		{ok, _Offer1} ->
			{ok, OfferName};
		{error, Reason} ->
			{error, Reason}
	end.

ipv4() ->
	{10, rand:uniform(256) - 1, rand:uniform(256) - 1, rand:uniform(254)}.

port() ->
	rand:uniform(66559) + 1024.

mac() ->
	mac(6, []).
mac(0, Acc) ->
	lists:flatten(io_lib:fwrite("~.16B:~.16B:~.16B:~.16B:~.16B:~.16B", Acc));
mac(N, Acc) ->
	mac(N - 1, [rand:uniform(256) - 1 | Acc]).

write_csv(File, [H | T]) ->
	write_csv1(File, H),
	write_csv(File, T);
write_csv(_, []) ->
	ok.
%% @hidden
write_csv1(File, Tuple) when is_tuple(Tuple)->
	write_csv1(File, tuple_to_list(Tuple));
write_csv1(File, [H | []]) ->
	file:write_file(File, io_lib:fwrite("~p\n", [H]), [append]);
write_csv1(File, [H | T]) ->
	file:write_file(File, io_lib:fwrite("~p,", [H]), [append]),
	write_csv1(File, T).

