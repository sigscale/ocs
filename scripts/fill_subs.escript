#!/usr/bin/env escript
%% vim: syntax=erlang
%%! -sname fill

-include_lib("ocs/include/ocs.hrl").

-define(DEFAULT_OFFER, "Voice & Data (1G)").

main(Args) ->
	case options(Args) of
		#{help := true} = _Options ->
			usage();
		Options ->
			Host = element(2, inet:gethostname()),
			Nodename = maps:get(node, Options, "ocs@" ++ Host),
			Node = list_to_atom(Nodename),
			N = list_to_integer(maps:get(subs, Options, "1000")),
			Offer = maps:get(offer, Options, ?DEFAULT_OFFER),
			MccMnc = maps:get(mccmnc, Options, "001001"),
			fill(Node, Offer, MccMnc, N, 0)
	end.

fill(_Node, _Offer, _MccMnc, 0, _MSIN) ->
	ok;
fill(Node, Offer, MccMnc, N, MSIN)
		when is_list(Offer), is_integer(N), N > 0, N < 1000000000,
		is_list(MccMnc), length(MccMnc) >= 5, length(MccMnc) =< 6 ->
	try
		fill1(Node, Offer, MccMnc, N, MSIN, erpc:call(Node,
				ocs, add_product, [Offer, []]))
	catch
		Error:Reason ->
			io:fwrite("~w: ~w~n", [Error, Reason]),
			usage()
	end;
fill(_Node, _Offer, _MccMnc, _N, _MSIN) ->
	usage().

fill1(Node, Offer, MccMnc, N, MSIN,
		{ok, #product{id = ProductRef}}) ->
	Width = case length(MccMnc) of
		5 ->
			10;
		6 ->
			9
	end,
	IMSI = lists:flatten(io_lib:fwrite("~s~*.10.0b",
			[MccMnc, Width, MSIN])),
	fill2(Node, Offer, MccMnc, N, MSIN,
			erpc:call(Node, ocs, add_service,
			[IMSI, undefined, ProductRef]));
fill1(_Node, _Offer, _MccMnc, _N, _MSIN,
		{error, Reason}) ->
	io:fwrite("~w: ~w~n", [error, Reason]),
	halt(1).

fill2(Node, Offer, MccMnc, N, MSIN,
		{ok, #service{name = ServiceRef,
		product = ProductRef}}) ->
	fill3(Node, Offer, MccMnc, N, MSIN, ServiceRef,
			erpc:call(Node, ocs, find_product,
			[ProductRef]));
fill2(_Node, _Offer, _MccMnc, _N, _MSIN,
		{error, Reason}) ->
	io:fwrite("~w: ~w~n", [error, Reason]),
	halt(1).

fill3(Node, Offer, MccMnc, N, MSIN, ServiceRef,
		{ok, Product}) ->
	fill4(Node, Offer, MccMnc, N, MSIN,
			erpc:call(Node, ocs, update_product,
			[Product#product{service = [ServiceRef]}]));
fill3(_Node, _Offer, _MccMnc, _N, _MSIN, _ServiceRef,
		{error, Reason}) ->
	io:fwrite("~w: ~w~n", [error, Reason]),
	halt(1).

fill4(Node, Offer, MccMnc, N, MSIN,
		{ok, #product{id = ProductRef}}) ->
	End = case calendar:universal_time() of
		{{Year, 12, Day}, Time} ->
			ocs_rest:date({{Year + 1, 1, Day}, Time});
		{{Year, Month, Day}, Time} ->
			ocs_rest:date({{Year, Month + 1, Day}, Time})
	end,
	Bucket = #bucket{units = cents,
			remain_amount = 1000000000,
			attributes = #{bucket_type => normal},
			end_date = End},
	fill5(Node, Offer, MccMnc, N, MSIN,
			erpc:call(Node, ocs, add_bucket,
			[ProductRef, Bucket]));
fill4(_Node, _Offer, _MccMnc, _N, _MSIN,
		{error, Reason}) ->
	io:fwrite("~w: ~w~n", [error, Reason]),
	halt(1).

fill5(Node, Offer, MccMnc, N, MSIN,
		{ok, _, #bucket{}}) ->
	fill(Node, Offer, MccMnc, N - 1, MSIN + 1);
fill5(_Node, _Offer, _MccMnc, _N, _MSIN,
		{error, Reason}) ->
	io:fwrite("~w: ~w~n", [error, Reason]),
	halt(1).

usage() ->
	Option1 = " [--subs 1000]",
	Option2 = " [--offer " ++ [$"] ++ ?DEFAULT_OFFER ++ [$"] ++ "]",
	Option3 = " [--mccmnc 001001]",
	Option4 = " [--node ocs@" ++ element(2, inet:gethostname()) ++ "]",
	Options = [Option1, Option2, Option3, Option4],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	options(Args, #{}).
options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--subs", Subs | T], Acc) ->
	options(T, Acc#{subs => Subs});
options(["--offer", Offer| T], Acc) ->
	options(T, Acc#{offer => Offer});
options(["--mccmnc", MccMnc | T], Acc) ->
	options(T, Acc#{mccmnc => MccMnc});
options(["--node", Node | T], Acc) ->
	options(T, Acc#{node => Node});
options([_H | _T], _Acc) ->
	usage();
options([], Acc) ->
	Acc.

