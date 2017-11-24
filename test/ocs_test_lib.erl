%%% ocs_test_lib.erl
%%% vim: ts=3
%%%
-module(ocs_test_lib).

-export([initialize_db/0, start/0, stop/0]).
-export([ipv4/0, port/0, mac/0]).
-export([add_product/0]).
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
			case mnesia:wait_for_tables([client, subscriber, product], 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, Tables} = ocs_app:install(),
					F = fun(T) ->
						case T of
							T when T == client; T == subscriber;
									T == httpd_user; T == httpd_group;
									T == product ->
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

add_product() ->
	Price1 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly,
			amount = 1995, size = undefined,
			units = undefined,
			alteration = #alteration{name = ocs:generate_password(),
					type = usage, units = octets,
					size = 2000000000, amount = 295}},
	Price2 = #price{name = ocs:generate_password(),
			type = usage, units = octets,
			size = 1000000000, amount = 100},
	Prices = [Price1, Price2],
	ProductName = ocs:generate_password(),
	Product = #product{name = ProductName,
			is_bundle = false,
			status = active,
			price = Prices},
	case ocs:add_product(Product) of
		{ok, _Product1} ->
			{ok, ProductName};
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
write_csv(File, []) ->
	ok.
%% @hidden
write_csv1(File, Tuple) when is_tuple(Tuple)->
	write_csv1(File, tuple_to_list(Tuple));
write_csv1(File, [H | []]) ->
	file:write_file(File, io_lib:fwrite("~p\n", [H]), [append]);
write_csv1(File, [H | T]) ->
	file:write_file(File, io_lib:fwrite("~p,", [H]), [append]),
	write_csv1(File, T).

