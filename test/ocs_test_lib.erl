%%% ocs_test_lib.erl
%%% vim: ts=3
%%%
-module(ocs_test_lib).

-export([initialize_db/0, start/0, start/1, stop/0, stop/1]).
-export([load/1, unload/1]).
-export([rand_name/0, rand_name/1, rand_dn/0, rand_dn/1]).
-export([ipv4/0, port/0, mac/0, uuid/0]).
-export([add_offer/0]).
-export([write_csv/2]).

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
	stop([ocs, radius, diameter, inets]).

stop([H | T]) ->
	case application:stop(H) of
		ok ->
			ok = unload(H),
			stop(T);
		{error, Reason} ->
			{error, Reason}
	end;
stop([]) ->
	ok.

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

%% @doc Returns 5-13 random printable characters.
rand_name() ->
	rand_name(rand:uniform(8) + 5).

%% @doc Returns N random printable characters.
rand_name(N) ->
	UpperCase = lists:seq(65, 90),
	LowerCase = lists:seq(97, 122),
	Digits = lists:seq(48, 57),
	Special = [$#, $%, $+, $-, $.],
	CharSet = lists:flatten([UpperCase, LowerCase, Digits, Special]),
	rand_name(N, CharSet, []).
rand_name(0, _CharSet, Acc) ->
	Acc;
rand_name(N, CharSet, Acc) ->
	Char = lists:nth(rand:uniform(length(CharSet)), CharSet),
	rand_name(N - 1, CharSet, [Char | Acc]).

%% @doc Returns ten random digits.
rand_dn() ->
	rand_dn(10).

%% @doc Returns N random digits.
rand_dn(N) ->
	rand_dn(N, []).
rand_dn(0, Acc) ->
	Acc;
rand_dn(N, Acc) ->
	rand_dn(N - 1, [47 + rand:uniform(10) | Acc]).

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

uuid() ->
	R1 = rand:uniform(16#ffffffffffff),
	R2 = rand:uniform(16#fff),
	R3 = rand:uniform(16#ffffffff),
	R4 = rand:uniform(16#3fffffff),
	B = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>,
	<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = B,
	L = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
			[TL, TM, THV, CSR, CSL, N]),
	lists:flatten(L).

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

