%%% ocs_charging_SUITE.erl
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
%%%  @doc Test suite for charging in {@link //ocs. ocs} application.
%%%
-module(ocs_charging_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for charging in OCS"}]},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[add_once, add_once_bundle, add_once_allowance,
			add_once_allowance_bundle, add_recurring,
			add_recurring_bundle, add_recurring_allowance,
			add_recurring_usage_allowance,
			add_recurring_allowance_bundle].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

add_once() ->
	[{userdata, [{doc, "One time charges at subscription instantiation"}]}].

add_once(_Config) ->
	ProductName = ocs:generate_password(),
	Amount1 = 1900,
	Price1 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount1},
	Amount2 = 100,
	Price2 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount2},
	Prices = [Price1, Price2],
	Product = #offer{name = ProductName, status = active,
			specification = 8, price = Prices},
	{ok, _} = ocs:add_offer(Product),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Amount3 = Amount1 + Amount2,
	Buckets1 = [#bucket{units = cents, remain_amount = Amount3}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, ProductName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, []} = lists:keytake(cents,
			#bucket.units, Buckets2).

add_once_bundle() ->
	[{userdata, [{doc, "One time charges instantiating product bundle"}]}].

add_once_bundle(_Config) ->
	ProductName1 = ocs:generate_password(),
	Amount1 = 1900,
	Price1 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount1},
	Product1 = #offer{name = ProductName1, status = active,
			specification = 8, price = [Price1]},
	{ok, _} = ocs:add_offer(Product1),
	ProductName2 = ocs:generate_password(),
	Amount2 = 100,
	Price2 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount2},
	Product2 = #offer{name = ProductName2, status = active,
			specification = 9, price = [Price2]},
	{ok, _} = ocs:add_offer(Product2),
	BundleName = ocs:generate_password(),
	Bundled1 = #bundled_po{name = ProductName1, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	Bundled2 = #bundled_po{name = ProductName2, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	Amount3 = 1000,
	Price3 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount3},
	Bundle = #offer{name = BundleName, status = active,
			bundle = [Bundled1, Bundled2], price = [Price3]},
	{ok, _} = ocs:add_offer(Bundle),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Amount4 = Amount1 + Amount2 + Amount3,
	Buckets1 = [#bucket{units = cents, remain_amount = Amount4}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, BundleName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, []} = lists:keytake(cents,
			#bucket.units, Buckets2).

add_once_allowance() ->
	[{userdata, [{doc, "One time allowances at subscription instantiation"}]}].

add_once_allowance(_Config) ->
	ProductName = ocs:generate_password(),
	UnitSize = 100000000000,
	Alteration = #alteration{name = ocs:generate_password(),
			type = one_time, units = octets, size = UnitSize, amount = 0},
	Amount = 1000,
	Price = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount, alteration = Alteration},
	Product = #offer{name = ProductName, status = active,
			specification = 8, price = [Price]},
	{ok, _} = ocs:add_offer(Product),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Buckets1 = [#bucket{units = cents, remain_amount = Amount}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, ProductName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, Buckets3} = lists:keytake(cents,
			#bucket.units, Buckets2),
	{_, #bucket{remain_amount = UnitSize}, []} = lists:keytake(octets,
			#bucket.units, Buckets3).

add_once_allowance_bundle() ->
	[{userdata, [{doc, "One time allowances at instantiation of product bundle"}]}].

add_once_allowance_bundle(_Config) ->
	ProductName1 = ocs:generate_password(),
	UnitSize1 = 100000000000,
	Alteration1 = #alteration{name = ocs:generate_password(),
			type = one_time, units = octets, size = UnitSize1, amount = 0},
	Amount1 = 1000,
	Price1 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount1, alteration = Alteration1},
	Product1 = #offer{name = ProductName1, status = active,
			specification = 8, price = [Price1]},
	{ok, _} = ocs:add_offer(Product1),
	ProductName2 = ocs:generate_password(),
	UnitSize2 = 36000,
	Alteration2 = #alteration{name = ocs:generate_password(),
			type = one_time, units = seconds, size = UnitSize2, amount = 0},
	Amount2 = 1000,
	Price2 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount2, alteration = Alteration2},
	Product2 = #offer{name = ProductName2, status = active,
			specification = 9, price = [Price2]},
	{ok, _} = ocs:add_offer(Product2),
	BundleName = ocs:generate_password(),
	Bundled1 = #bundled_po{name = ProductName1, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	Bundled2 = #bundled_po{name = ProductName2, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	UnitSize3 = 2000000000,
	Alteration3 = #alteration{name = ocs:generate_password(),
			type = one_time, units = octets, size = UnitSize3, amount = 0},
	Amount3 = 100,
	Price3 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount3, alteration = Alteration3},
	UnitSize4 = 3600,
	Alteration4 = #alteration{name = ocs:generate_password(),
			type = one_time, units = seconds, size = UnitSize4, amount = 0},
	Amount4 = 100,
	Price4 = #price{name = ocs:generate_password(),
			type = one_time, amount = Amount4, alteration = Alteration4},
	Bundle = #offer{name = BundleName, status = active,
			bundle = [Bundled1, Bundled2], price = [Price3, Price4]},
	{ok, _} = ocs:add_offer(Bundle),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Amount5 = Amount1 + Amount2 + Amount3 + Amount4,
	Buckets1 = [#bucket{units = cents, remain_amount = Amount5}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, BundleName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, Buckets3} = lists:keytake(cents,
			#bucket.units, Buckets2),
	F = fun(#bucket{units = Units, remain_amount = Amount}, {Units, N}) ->
				{Units, N + Amount};
			(_, Acc) ->
				Acc
	end,
	UnitSize5 = UnitSize1 + UnitSize3,
	{_, UnitSize5} = lists:foldl(F, {octets, 0}, Buckets3),
	UnitSize6 = UnitSize2 + UnitSize4,
	{_, UnitSize6} = lists:foldl(F, {seconds, 0}, Buckets3).

add_recurring() ->
	[{userdata, [{doc, "Recurring charges at subscription instantiation"}]}].

add_recurring(_Config) ->
	ProductName = ocs:generate_password(),
	Amount1 = 2995,
	Price1 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly, amount = Amount1},
	Amount2 = 5,
	Price2 = #price{name = ocs:generate_password(),
			type = recurring, period = hourly, amount = Amount2},
	Prices = [Price1, Price2],
	Product = #offer{name = ProductName, status = active,
			specification = 8, price = Prices},
	{ok, _} = ocs:add_offer(Product),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Amount3 = Amount1 + Amount2,
	Buckets1 = [#bucket{units = cents, remain_amount = Amount3}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, ProductName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, []} = lists:keytake(cents,
			#bucket.units, Buckets2).

add_recurring_bundle() ->
	[{userdata, [{doc, "Recurring charges instantiating product bundle"}]}].

add_recurring_bundle(_Config) ->
	ProductName1 = ocs:generate_password(),
	Amount1 = 2995,
	Price1 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly, amount = Amount1},
	Product1 = #offer{name = ProductName1, status = active,
			specification = 8, price = [Price1]},
	{ok, _} = ocs:add_offer(Product1),
	Amount2 = 5,
	Price2 = #price{name = ocs:generate_password(),
			type = recurring, period = hourly, amount = Amount2},
	ProductName2 = ocs:generate_password(),
	Product2 = #offer{name = ProductName2, status = active,
			specification = 9, price = [Price2]},
	{ok, _} = ocs:add_offer(Product2),
	BundleName = ocs:generate_password(),
	Bundled1 = #bundled_po{name = ProductName1, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	Bundled2 = #bundled_po{name = ProductName2, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	Amount3 = 1000,
	Price3 = #price{name = ocs:generate_password(),
			type = recurring, period = yearly, amount = Amount3},
	Bundle = #offer{name = BundleName, status = active,
			bundle = [Bundled1, Bundled2], price = [Price3]},
	{ok, _} = ocs:add_offer(Bundle),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Amount4 = Amount1 + Amount2 + Amount3,
	Buckets1 = [#bucket{units = cents, remain_amount = Amount4}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, BundleName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, []} = lists:keytake(cents,
			#bucket.units, Buckets2).

add_recurring_allowance() ->
	[{userdata, [{doc, "Recurring allowances at subscription instantiation"}]}].

add_recurring_allowance(_Config) ->
	ProductName = ocs:generate_password(),
	UnitSize = 100000000000,
	Alteration = #alteration{name = ocs:generate_password(),
			type = recurring, period = monthly,
			units = octets, size = UnitSize, amount = 0},
	Amount = 1000,
	Price = #price{name = ocs:generate_password(),
			type = recurring, period = monthly,
			amount = Amount, alteration = Alteration},
	Product = #offer{name = ProductName, status = active,
			specification = 8, price = [Price]},
	{ok, _} = ocs:add_offer(Product),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Buckets1 = [#bucket{units = cents, remain_amount = Amount}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, ProductName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, Buckets3} = lists:keytake(cents,
			#bucket.units, Buckets2),
	{_, #bucket{remain_amount = UnitSize}, []} = lists:keytake(octets,
			#bucket.units, Buckets3).

add_recurring_usage_allowance() ->
	[{userdata, [{doc, "Recurring allowances attached to usage price
			at subscription instantiation"}]}].

add_recurring_usage_allowance(_Config) ->
	ProductName = ocs:generate_password(),
	UnitSize = 100000000000,
	Amount = 1000,
	Alteration = #alteration{name = ocs:generate_password(),
			type = recurring, period = monthly,
			units = octets, size = UnitSize, amount = Amount},
	Price = #price{name = ocs:generate_password(),
			type = usage, units = octets, size = 1000000000,
			amount = 100, alteration = Alteration},
	Product = #offer{name = ProductName, status = active,
			specification = 8, price = [Price]},
	{ok, _} = ocs:add_offer(Product),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Buckets1 = [#bucket{units = cents, remain_amount = Amount}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, ProductName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, Buckets3} = lists:keytake(cents,
			#bucket.units, Buckets2),
	{_, #bucket{remain_amount = UnitSize}, []} = lists:keytake(octets,
			#bucket.units, Buckets3).

add_recurring_allowance_bundle() ->
	[{userdata, [{doc, "Recurring allowances at instantiation of product bundle"}]}].

add_recurring_allowance_bundle(_Config) ->
	ProductName1 = ocs:generate_password(),
	UnitSize1 = 100000000000,
	Alteration1 = #alteration{name = ocs:generate_password(),
			type = recurring, period = monthly,
			units = octets, size = UnitSize1, amount = 0},
	Amount1 = 1000,
	Price1 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly,
			amount = Amount1, alteration = Alteration1},
	Product1 = #offer{name = ProductName1, status = active,
			specification = 8, price = [Price1]},
	{ok, _} = ocs:add_offer(Product1),
	ProductName2 = ocs:generate_password(),
	UnitSize2 = 36000,
	Alteration2 = #alteration{name = ocs:generate_password(),
			type = recurring, period = monthly,
			units = seconds, size = UnitSize2, amount = 0},
	Amount2 = 1000,
	Price2 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly,
			amount = Amount2, alteration = Alteration2},
	Product2 = #offer{name = ProductName2, status = active,
			specification = 9, price = [Price2]},
	{ok, _} = ocs:add_offer(Product2),
	BundleName = ocs:generate_password(),
	Bundled1 = #bundled_po{name = ProductName1, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	Bundled2 = #bundled_po{name = ProductName2, status = active,
			lower_limit = 0, upper_limit = 1, default = 1},
	UnitSize3 = 2000000000,
	Alteration3 = #alteration{name = ocs:generate_password(),
			type = recurring, period = monthly,
			units = octets, size = UnitSize3, amount = 0},
	Amount3 = 100,
	Price3 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly,
			amount = Amount3, alteration = Alteration3},
	UnitSize4 = 3600,
	Alteration4 = #alteration{name = ocs:generate_password(),
			type = recurring, period = monthly,
			units = seconds, size = UnitSize4, amount = 0},
	Amount4 = 100,
	Price4 = #price{name = ocs:generate_password(),
			type = recurring, period = monthly,
			amount = Amount4, alteration = Alteration4},
	Bundle = #offer{name = BundleName, status = active,
			bundle = [Bundled1, Bundled2], price = [Price3, Price4]},
	{ok, _} = ocs:add_offer(Bundle),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Amount5 = Amount1 + Amount2 + Amount3 + Amount4,
	Buckets1 = [#bucket{units = cents, remain_amount = Amount5}],
	{ok, #service{buckets = Buckets2}} = ocs:add_service(Identity,
			Password, BundleName, [], Buckets1),
	{_, #bucket{remain_amount = 0}, Buckets3} = lists:keytake(cents,
			#bucket.units, Buckets2),
	F = fun(#bucket{units = Units, remain_amount = Amount}, {Units, N}) ->
				{Units, N + Amount};
			(_, Acc) ->
				Acc
	end,
	UnitSize5 = UnitSize1 + UnitSize3,
	{_, UnitSize5} = lists:foldl(F, {octets, 0}, Buckets3),
	UnitSize6 = UnitSize2 + UnitSize4,
	{_, UnitSize6} = lists:foldl(F, {seconds, 0}, Buckets3).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

