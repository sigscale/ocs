%%% ocs_scheduler.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
-module(ocs_scheduler).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([start/0, start/1]).
-export([product_charge/0]).

-include("ocs.hrl").

-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-spec start() -> ok.
%% @equiv start(Interval)
start() ->
	start(1440000).

-spec start(Interval) -> ok
	when
		Interval :: pos_integer().
%% @doc
start(Interval) ->
	timer:apply_interval(Interval, ?MODULE, product_charge, []),
	ok.

-spec product_charge() -> ok.
%% @doc Scheduler update for all the subscriptions.
product_charge() ->
	F  = fun() ->
		product_charge(mnesia:first(service))
	end,
	mnesia:transaction(F),
	ok.
%% @hidden
product_charge('$end_of_table') ->
	ok;
product_charge(SubscriptionId) ->
	case mnesia:read(service, SubscriptionId) of
		[#service{product =
				#product_instance{product = ProdId,
				characteristics = Chars}} = Subscriber]
				when ProdId /= undefined ->
			Now = erlang:system_time(?MILLISECOND),
			LM = {Now, erlang:unique_integer([positive])},
			case mnesia:read(product, ProdId, read) of
				[Product] ->
					Subscriber1 = Subscriber#service{last_modified= LM},
					Subscriber2 = ocs:subscription(Subscriber1, Product, Chars, false),
					mnesia:write(Subscriber2);
				[] ->
					ok
			end;
		[] ->
			ok
	end,
	product_charge(mnesia:next(service, SubscriptionId)).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

