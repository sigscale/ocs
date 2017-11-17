%%% ocs_rating_SUITE.erl
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
%%%  @doc Test suite for rating API of the {@link //ocs. ocs} application.
%%%
-module(ocs_rating_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for rating API in OCS"}]},
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
	[rate_octets_with_debiting_scenario_1, rate_octets_with_debiting_scenario_2,
	rate_octets_with_debiting_scenario_3, rate_octets_with_debiting_scenario_4,
	rate_octets_with_debiting_scenario_5, rate_octets_with_debiting_scenario_6,
	rate_octets_with_debiting_scenario_7, rate_octets_with_reservation_scenario_1,
	rate_octets_with_reservation_scenario_2, rate_octets_with_reservation_scenario_3,
	rate_octets_with_reservation_scenario_4, rate_octets_with_reservation_scenario_5,
	rate_octets_with_reservation_scenario_6, rate_octets_with_debit_and_reservation_scenario_1,
	rate_octets_with_debit_and_reservation_scenario_2, rate_octets_with_debit_and_reservation_scenario_3,
	rate_octets_with_debit_and_reservation_scenario_4, rate_octets_with_debit_and_reservation_scenario_5].

	%[rate_octets_with_reservation_scenario_1].
%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
rate_octets_with_debiting_scenario_1() ->
	[{userdata, [{doc, "Rate cents buckets to octets
		and deduct usage from rated buckets, This senario
		describ debit amount equal to package size"}]}].

rate_octets_with_debiting_scenario_1(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 200,
	Debit = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], []),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	false = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets).

rate_octets_with_debiting_scenario_2() ->
	[{userdata, [{doc, "Rate cents buckets to octets
		and deduct usage from rated buckets, This senario
		describ debit amount less than package size"}]}].

rate_octets_with_debiting_scenario_2(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 200,
	Debit = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], []),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	#bucket{remain_amount = OctetsRemain} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets),
	OctetsRemain = PackageSize - Debit.

rate_octets_with_debiting_scenario_3() ->
	[{userdata, [{doc, "Rate cents buckets to octets
		and deduct usage from rated buckets, This senario
		describ debit amount equal to bucket remain amount and
		package size"}]}].

rate_octets_with_debiting_scenario_3(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 100,
	Debit = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], []),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

rate_octets_with_debiting_scenario_4() ->
	[{userdata, [{doc, "Rate cents buckets to octets
		and deduct usage from rated buckets, This senario
		describ out of credit"}]}].

rate_octets_with_debiting_scenario_4(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 10,
	Debit = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], []),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

rate_octets_with_debiting_scenario_5() ->
	[{userdata, [{doc, "This senario describ when out of
		credit remove session attributes from subscriber record"}]}].

rate_octets_with_debiting_scenario_5(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 10,
	Debit = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	SessionAttributes = [{erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate@sigscale"}, {?NasIpAddress, "10.0.0.1"}]}],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, session_attributes = SessionAttributes,
			product = #product_instance{product = ProdID, characteristics = Chars}},
	mnesia:dirty_write(subscriber, Subscriber),
	Destination = ocs:generate_password(),
	{out_of_credit, SessionAttributes} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], []),
	{ok, #subscriber{session_attributes = []}} = ocs:find_subscriber(SubscriberID).

rate_octets_with_debiting_scenario_6() ->
	[{userdata, [{doc, "This senario describ when final call
		remove given session attributes from subscriber record"}]}].

rate_octets_with_debiting_scenario_6(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 1000,
	Debit = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	SA1 = {erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate1@sigscale"}, {?NasIpAddress, "10.0.0.1"}]},
	SA2 = {erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020304"},
		{?NasIdentifier, "rate2@sigscale"}, {?NasIpAddress, "10.0.0.2"}]},
	Identification = [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate1@sigscale"}, {?NasIpAddress, "10.0.0.1"}],
	SessionAttributes = [SA1, SA2],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, session_attributes = SessionAttributes,
			product = #product_instance{product = ProdID, characteristics = Chars}},
	mnesia:dirty_write(subscriber, Subscriber),
	Destination = ocs:generate_password(),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], [], Identification),
	{ok, #subscriber{session_attributes = [SA2]}} = ocs:find_subscriber(SubscriberID).

rate_octets_with_debiting_scenario_7() ->
	[{userdata, [{doc, "This senario describ when g call for the
		rate function include the session attributes"}]}].

rate_octets_with_debiting_scenario_7(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	Buckets = [#bucket{bucket_type = cents, remain_amount = 200,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionAttributes = [{?AcctSessionId, "1020303"}, {?NasIdentifier, "rate@sigscale"},
		{?NasIpAddress, "10.0.0.1"}],
	Destination = ocs:generate_password(),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [{octets, 100}], [], SessionAttributes),
	{ok, #subscriber{session_attributes = SessionList}} = ocs:find_subscriber(SubscriberID),
	{_, SessionAttributes} = lists:nth(1, SessionList).

rate_octets_with_reservation_scenario_1() ->
	[{userdata, [{doc, "Rate cents buckets to octets
		and This senario describ reservation amount equal
		to package size"}]}].

rate_octets_with_reservation_scenario_1(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 200,
	Reservation = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, Reservation} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	#bucket{remain_amount = PackageSize} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets).

rate_octets_with_reservation_scenario_2() ->
	[{userdata, [{doc, "Rate cents buckets to octets
		and This senario describ reservation amount less
		than the package size"}]}].

rate_octets_with_reservation_scenario_2(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 200,
	Reservation = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	%% 1st Reservation
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	#bucket{remain_amount = PackageSize} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets),
	%% 2nd Reservation
	Reservation2 = 300,
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation2}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	#bucket{remain_amount = PackageSize} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets),
	%% 3rd Reservation
	Reservation3 = 700,
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation3}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	#bucket{remain_amount = PackageSize} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets).

rate_octets_with_reservation_scenario_3() ->
	[{userdata, [{doc, "This senario describ reservation amount
		equal to bucket remain amount and package size"}]}].

rate_octets_with_reservation_scenario_3(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 100,
	Reservation = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	false = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	#bucket{remain_amount = PackageSize} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets).

rate_octets_with_reservation_scenario_4() ->
	[{userdata, [{doc, "This senario describ out of
		credit situation in reservation"}]}].

rate_octets_with_reservation_scenario_4(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 10,
	Reservation = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	Destination = ocs:generate_password(),
	{out_of_credit, []} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = RemAmount} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets).

rate_octets_with_reservation_scenario_5() ->
	[{userdata, [{doc, "This senario describ when out of
		credit remove session attributes from subscriber record"}]}].

rate_octets_with_reservation_scenario_5(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 10,
	Reservation = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	SessionAttributes = [{erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate@sigscale"}, {?NasIpAddress, "10.0.0.1"}]}],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, session_attributes = SessionAttributes,
			product = #product_instance{product = ProdID, characteristics = Chars}},
	Destination = ocs:generate_password(),
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{out_of_credit, SessionAttributes} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation}]),
	{ok, #subscriber{session_attributes = []}} = ocs:find_subscriber(SubscriberID).

rate_octets_with_reservation_scenario_6() ->
	[{userdata, [{doc, "This senario describ when g call for the
		rate function include the session attributes"}]}].

rate_octets_with_reservation_scenario_6(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	Buckets = [#bucket{bucket_type = cents, remain_amount = 200,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionAttributes = [{?AcctSessionId, "1020303"}, {?NasIdentifier, "rate@sigscale"},
		{?NasIpAddress, "10.0.0.1"}],
	Destination = ocs:generate_password(),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, 100}], SessionAttributes),
	{ok, #subscriber{session_attributes = SessionList}} = ocs:find_subscriber(SubscriberID),
	{_, SessionAttributes} = lists:nth(1, SessionList).

rate_octets_with_debit_and_reservation_scenario_1() ->
	[{userdata, [{doc, "Senario describ doing first debit given usage
		and check for reservation, Whole senoario base on sufficient amount"}]}].

rate_octets_with_debit_and_reservation_scenario_1(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 201,
	Debit = 1000,
	Reservation = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 1} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	#bucket{remain_amount = PackageSize} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets).

rate_octets_with_debit_and_reservation_scenario_2() ->
	[{userdata, [{doc, "Senario describ doing first debit given
		usage and check for reservation, debit amount is less than package size and
		reservation amount less than avaliable remain amount, Whole senoario base on
		sufficient amount"}]}].

rate_octets_with_debit_and_reservation_scenario_2(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 201,
	Debit = 100,
	Reservation = 100,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	#bucket{remain_amount = OctetsRemain} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets),
	OctetsRemain = PackageSize - Debit.

rate_octets_with_debit_and_reservation_scenario_3() ->
	[{userdata, [{doc, "Senario describ doing first debit given
		usage and check for reservation, debit amount is equal to the
		package size and reservation amount grater than avaliable remain amount"}]}].

rate_octets_with_debit_and_reservation_scenario_3(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 199,
	Debit = 1000,
	Reservation = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination,
			interim, [{octets, Debit}], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice,
	false = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets).

rate_octets_with_debit_and_reservation_scenario_4() ->
	[{userdata, [{doc, "Senario of suffient amount for dibit the
		usage but not for the reservation"}]}].

rate_octets_with_debit_and_reservation_scenario_4(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 200,
	Debit = 1500,
	Reservation = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}]),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = OctetsRemain} = lists:keyfind(octets, #bucket.bucket_type, RatedBuckets),
	OctetsRemain = (PackageSize * 2) - Debit,
	false = lists:keyfind(cents, #bucket.bucket_type, RatedBuckets).

rate_octets_with_debit_and_reservation_scenario_5() ->
	[{userdata, [{doc, "Senario of insuffient amount for
			dibit and the reservation"}]}].

rate_octets_with_debit_and_reservation_scenario_5(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 50,
	Debit = 1500,
	Reservation = 1000,
	Buckets = [#bucket{bucket_type = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	Destination = ocs:generate_password(),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}]),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

