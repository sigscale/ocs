%%% ocs_test_lib.erl
%%%
-module(ocs_test_lib).

-export([initialize_db/0, start/0, stop/0]).
-export([add_product/0]).

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
	SD = erlang:system_time(?MILLISECOND),
	TD = erlang:system_time(?MILLISECOND)  + 2678400000,
	Price1 = #price{name = "Family-Pack",
			description = "monthlyprice",
			valid_for = {SD, TD},
			type = recurring,
			currency = "MXV",
			period = monthly,
			amount = 230},
	Price2 = #price{name = "usage",
			description = "usage definition for family pack",
			valid_for = {SD, TD},
			type = usage,
			currency = "MXV",
			size = 0,
			amount = 5,
			alteration = #alteration{name = "Usage",
											valid_for = {SD,undefined},
											type = usage,
											units = octets,
											size = 20000,
											amount = 0}},
	Prices = [Price1, Price2],
	Product = #product{name = "Wi-Fi",
			valid_for = {SD, TD},
			is_bundle = false,
			status = active,
			start_date = SD,
			termination_date = TD,
			description = "monthly subscription Web Family pack",
			price = Prices},

	F = fun() ->
			mnesia:write(product, Product, write)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{ok, "Wi-Fi"};
		_ ->
			{error, aborted}
	end.

