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
	[initial_reservation_available_remain_amount, initial_reservation_out_of_credit,
	initial_reservation_overhead, initial_reservation_multiple_buckets,
	initial_reservation_expiry_buckets, initial_reservation_ignore_expired_buckets,
	initial_reservation_add_session_attributes, interim_reservation_avaialbe_remain_amount,
	interim_reservations_within_package_size, interim_reservation_available_remain_amount,
	interim_reservation_out_of_credit, interim_reservation_remove_session_attributes,
	interim_reservation_multiple_buckets_with_sufficient_amount,
	interim_reservation_multiple_buckets_out_of_credit,
	interim_debiting_exact_remain_amount, interim_debiting_below_package_size,
	interim_debiting_out_of_credit, octets_debiting_scenario_5, octets_debiting_scenario_6,
	octets_debit_and_reservation_scenario_1, octets_debit_and_reservation_scenario_2,
	octets_debit_and_reservation_scenario_3, octets_debit_and_reservation_scenario_4,
	octets_debit_and_reservation_scenario_5].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
initial_reservation_available_remain_amount() ->
	[{userdata, [{doc, "Remaining amount exactly equal to total reservation price"}]}].

initial_reservation_available_remain_amount(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = Reservations} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	{_, PackagePrice, _} = lists:keyfind(SessionId, 3, Reservations).

initial_reservation_out_of_credit() ->
	[{userdata, [{doc, "Insufficient amount for initial reservation"}]}].

initial_reservation_out_of_credit(_Config) ->
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
	RemAmount = 10,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(radius, SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0} = lists:keyfind(cents, #bucket.units, RatedBuckets).

initial_reservation_add_session_attributes() ->
	[{userdata, [{doc, "Add session attributes in subscriber record"}]}].

initial_reservation_add_session_attributes(_Config) ->
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
	RemAmount = 1000,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'AcctSession-Id', list_to_binary(ocs:generate_password())}],
	NasIp = [{?NasIpAddress, "192.168.1.150"}],
	NasId = [{?NasIpAddress, ocs:generate_password()}],
	SessionAttr = SessionId ++ NasIp ++ NasId,
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, #subscriber{session_attributes = SessionList}, _} =
		ocs_rating:rate(radius, SubscriberID, Destination,
		initial, [], [{octets, PackageSize}], SessionAttr),
	{ok, #subscriber{session_attributes = SessionList}} = ocs:find_subscriber(SubscriberID),
	[{_, SessionAttr}] = SessionList.

initial_reservation_overhead() ->
	[{userdata, [{doc, "Reserved amount grater than requested reservation amount"}]}].

initial_reservation_overhead(_Config) ->
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
	Reservation = 1500,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(radius, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{reservations = Reservations} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	{_, Reserved, _} = lists:keyfind(SessionId, 3, Reservations),
	F = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	Reserved = F(Reservation),
	true = (Reserved * PackagePrice) > Reservation.

initial_reservation_multiple_buckets() ->
	[{userdata, [{doc, "Reservation with mulitple buckets"}]}].

initial_reservation_multiple_buckets(_Config) ->
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
	Reservation = 1000,
	B1 = #bucket{units = cents, remain_amount = 50,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	B2 = #bucket{units = cents, remain_amount = 50,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	Buckets = [B1, B2],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, Reservation} = ocs_rating:rate(radius, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	GetAllReservations = fun(Type, Bs) ->
		F1 = fun(#bucket{units = T, reservations = Res}, R) when T == Type ->
					Res ++  R;
				(_, R) ->
					R
		end,
		lists:foldl(F1, [], Bs)
	end,
	GetReservedAmount = fun(Rese) ->
		F2  = fun(F2, [{_, R, _} | T], Acc) ->
					F2(F2, T, R + Acc);
				(_F2, [], Acc) ->
					Acc
		end,
		F2(F2, Rese, 0)
	end,
	Reservations = GetAllReservations(cents, RatedBuckets),
	Reserved = GetReservedAmount(Reservations),
	F = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	Reserved = F(Reservation).

initial_reservation_expiry_buckets() ->
	[{userdata, [{doc, "remove expired buckets"}]}].

initial_reservation_expiry_buckets(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) - 2592000000}],
	RemAmount = 100,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND) -  (2 * 2592000000),
		termination_date = erlang:system_time(?MILLISECOND) - 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

initial_reservation_ignore_expired_buckets() ->
	[{userdata, [{doc, "Ignore expired buckets"}]}].

initial_reservation_ignore_expired_buckets(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) - 2592000000}],
	RemAmount = rand:uniform(10000),
	Reservation = rand:uniform(1000),
	ExpiredBucket = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND) -  (2 * 2592000000),
		termination_date = erlang:system_time(?MILLISECOND) - 2592000000},
	Bucket = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	Buckets = [ExpiredBucket, Bucket],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = [#bucket{reservations = Reservations}]}} = ocs:find_subscriber(SubscriberID),
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
				(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	{_, Reserved, _} = lists:keyfind(SessionId, 3, Reservations),
	Reserved = F(Reservation).

interim_reservation_avaialbe_remain_amount() ->
	[{userdata, [{doc, "Reservation amount equal to package size"}]}].

interim_reservation_avaialbe_remain_amount(_Config) ->
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
	InitialReservation = 100,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, SubscriberID, Destination, initial, [], [{octets, InitialReservation}], SessionId),
	Interim_1_Reservation = PackageSize + InitialReservation,
	{ok, _, _} = ocs_rating:rate(radius, SubscriberID, Destination, interim, [], [{octets, Interim_1_Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = Reservations} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	{_, Reserved, _} = lists:keyfind(SessionId, 3, Reservations),
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reserved = F(Interim_1_Reservation).

interim_reservations_within_package_size() ->
	[{userdata, [{doc, "Reservation amounts less
		than the package size"}]}].

interim_reservations_within_package_size(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	%% 1st Reservation
	Reservation1 = 100,
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation1}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain1, reservations = Reservations1} =
			lists:keyfind(cents, #bucket.units, RatedBuckets1),
	CentsRemain1 = RemAmount - F(Reservation1),
	{_, Reserved1, _} = lists:keyfind(SessionId, 3, Reservations1),
	Reserved1 = F(Reservation1),
	%% 2nd Reservation
	Reservation2 = 300,
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation2}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} =
			lists:keyfind(cents, #bucket.units, RatedBuckets2),
	CentsRemain2 = RemAmount - F(Reservation2),
	{_, Reserved2, _} = lists:keyfind(SessionId, 3, Reservations2),
	Reserved2 = F(Reservation2),
	%% 3rd Reservation
	Reservation3 = 700,
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation3}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets3}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain3, reservations = Reservations3} =
			lists:keyfind(cents, #bucket.units, RatedBuckets3),
	CentsRemain3 = RemAmount - F(Reservation3),
	{_, Reserved3, _} = lists:keyfind(SessionId, 3, Reservations3),
	Reserved3 = F(Reservation3).

interim_reservation_available_remain_amount() ->
	[{userdata, [{doc, "Reservation amount equal to bucket remain amount and package size"}]}].

interim_reservation_available_remain_amount(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = 2 * PackagePrice,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, PackageSize}], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = Reservations} =
		lists:keyfind(cents, #bucket.units, RatedBuckets),
	{_, PackagePrice, _} = lists:keyfind(SessionId, 3, Reservations).

interim_reservation_out_of_credit() ->
	[{userdata, [{doc, "Out of credit on reservation"}]}].

interim_reservation_out_of_credit(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 110,
	Reservation = 1000,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Destination = ocs:generate_identity(),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, 2 * Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0} = lists:keyfind(cents, #bucket.units, RatedBuckets).

interim_reservation_remove_session_attributes() ->
	[{userdata, [{doc, "Out of credit remove session
			attributes from subscriber record"}]}].

interim_reservation_remove_session_attributes(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 110,
	Reservation1 = 100,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, product = #product_instance{product = ProdID, characteristics = Chars}},
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NasIp = [{?NasIpAddress, "10.0.0.1"}],
	NasId = [{?NasIdentifier, "rate@sigscale"}],
	SessionAttributes = SessionId ++ NasIp ++ NasId,
	Destination = ocs:generate_identity(),
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation1}], SessionAttributes),
	Reservation2 = 1100,
	{out_of_credit, SessionList} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation2}], SessionAttributes),
	[{_, SessionAttributes}] = SessionList,
	{ok, #subscriber{session_attributes = []}} = ocs:find_subscriber(SubscriberID).

interim_reservation_multiple_buckets_with_sufficient_amount() ->
	[{userdata, [{doc, "Reservation with miltiple buckets"}]}].

interim_reservation_multiple_buckets_with_sufficient_amount(_Config) ->
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
	RemAmount = 110,
	Reservation1 = 1000,
	B1 = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	B2 = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	Buckets = [B1, B2],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, product = #product_instance{product = ProdID, characteristics = Chars}},
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NasIp = [{?NasIpAddress, "10.0.0.1"}],
	NasId = [{?NasIdentifier, ocs:generate_password() ++ "@sigscalelab"}],
	SessionAttributes = SessionId ++ NasIp ++ NasId,
	Destination = ocs:generate_identity(),
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation1}], SessionAttributes),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	F1 = fun(F1, Type, [#bucket{units = Type, reservations = Res} | T], R) when Res =/= [] ->
			{_, Reserved, _} = lists:keyfind(SessionId, 3, Res),
			F1(F1, Type, T, Reserved + R);
		(F1, Type, [_ | T], R) ->
			F1(F1, Type, T, R);
		(_, _, [], R) ->
			R
	end,
	F2 = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
				(Reserve div PackageSize) * PackagePrice;
			(Reserve) ->
				(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reserved1 = F1(F1, cents, RatedBuckets1, 0),
	Reserved1 = F2(Reservation1),
	Reservation2 = 1100,
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation2}], SessionAttributes),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	Reserved2 = F1(F1, cents, RatedBuckets2, 0),
	Reserved2 = F2(Reservation2).

interim_reservation_multiple_buckets_out_of_credit() ->
	[{userdata, [{doc, "Out of credit with mulitple cents buckets"}]}].

interim_reservation_multiple_buckets_out_of_credit(_Config) ->
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
	RemAmount = 110,
	Reservation1 = 1000,
	B1 = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	B2 = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	Buckets = [B1, B2],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, product = #product_instance{product = ProdID, characteristics = Chars}},
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NasIp = [{?NasIpAddress, "10.0.0.1"}],
	NasId = [{?NasIdentifier, ocs:generate_password() ++ "@sigscalelab"}],
	SessionAttributes = SessionId ++ NasIp ++ NasId,
	Destination = ocs:generate_identity(),
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation1}], SessionAttributes),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	F1 = fun(F1, Type, [#bucket{units = Type, reservations = Res} | T], R) when Res =/= [] ->
			{_, Reserved, _} = lists:keyfind(SessionId, 3, Res),
			F1(F1, Type, T, Reserved + R);
		(F1, Type, [_ | T], R) ->
			F1(F1, Type, T, R);
		(_, _, [], R) ->
			R
	end,
	F2 = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
				(Reserve div PackageSize) * PackagePrice;
			(Reserve) ->
				(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reserved1 = F1(F1, cents, RatedBuckets1, 0),
	Reserved1 = F2(Reservation1),
	Reservation2 = 2100,
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [], [{octets, Reservation2}], SessionAttributes),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	F3 = fun(#bucket{remain_amount = R}, Acc) -> R + Acc end,
	0 = lists:foldl(F3, 0, RatedBuckets2).

interim_debiting_exact_remain_amount() ->
	[{userdata, [{doc, "Debit amount equal to package size"}]}].

interim_debiting_exact_remain_amount(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, PackageSize}], [], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = []} = lists:keyfind(cents, #bucket.units, RatedBuckets).

interim_debiting_below_package_size() ->
	[{userdata, [{doc, "Debit amount less than package size"}]}].

interim_debiting_below_package_size(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price]},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000001}],
	RemAmount = 200,
	Debit = 100,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice.

interim_debiting_out_of_credit() ->
	[{userdata, [{doc, "Not sufficient amount to debit"}]}].

interim_debiting_out_of_credit(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], [], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

octets_debiting_scenario_5() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

octets_debiting_scenario_5(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	SessionAttributes = [{erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate@sigscale"}, {?NasIpAddress, "10.0.0.1"}]}],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, session_attributes = SessionAttributes,
			product = #product_instance{product = ProdID, characteristics = Chars}},
	mnesia:dirty_write(subscriber, Subscriber),
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], [], SessionId),
	{ok, #subscriber{session_attributes = []}} = ocs:find_subscriber(SubscriberID).

octets_debiting_scenario_6() ->
	[{userdata, [{doc, "Final call remove given session
			attributes from subscriber record"}]}].

octets_debiting_scenario_6(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	SA1 = {erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate1@sigscale"}, {?NasIpAddress, "10.0.0.1"}]},
	SA2 = {erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020304"},
		{?NasIdentifier, "rate2@sigscale"}, {?NasIpAddress, "10.0.0.2"}]},
	SessionAttributes = [SA1, SA2],
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, session_attributes = SessionAttributes,
			product = #product_instance{product = ProdID, characteristics = Chars}},
	mnesia:dirty_write(subscriber, Subscriber),
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, final, [{octets, Debit}], [], SessionId),
	{ok, #subscriber{session_attributes = [SA2]}} = ocs:find_subscriber(SubscriberID).

octets_debit_and_reservation_scenario_1() ->
	[{userdata, [{doc, "Debit given usage and check for reservation,
			Whole senoario base on sufficient amount"}]}].

octets_debit_and_reservation_scenario_1(_Config) ->
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
	RemAmount = 300,
	Debit = 1000,
	Reservation = 1000,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain1, reservations = Reservations1} = lists:keyfind(cents, #bucket.units, RatedBuckets1),
	F1 = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	F3 = fun(Deb, Reserve, 0) ->
				(Deb + Reserve);
			(Deb, Reserve, Reserved) ->
				case Deb rem Reserved of
					0 ->
						Reserve;
					_ ->
						(PackagePrice + Reserve)
				end
	end,
	F2 = fun(Deb, Reserve, Reserved) -> F3(F1(Deb), F1(Reserve), Reserved) end,
	CentsRemain1 = RemAmount - F1(Reservation),
	{_, Reservation1, _} = lists:keyfind(SessionId, 3, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain2 = CentsRemain1 - F1(Debit),
	{_, Reservation2, _} = lists:keyfind(SessionId, 3, Reservations2),
	Reservation2 = F2(Debit, Reservation, Reservation1).

octets_debit_and_reservation_scenario_2() ->
	[{userdata, [{doc, "Debit amount is less than package size and
		reservation amount less than avaliable remain amount, Whole senoario base on
		sufficient amount"}]}].

octets_debit_and_reservation_scenario_2(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain1, reservations = Reservations1} = lists:keyfind(cents, #bucket.units, RatedBuckets1),
	F1 = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	F3 = fun(Deb, Reserve, 0) ->
				(Deb + Reserve);
			(Deb, Reserve, Reserved) ->
				case Deb rem Reserved of
					0 ->
						Reserve;
					_ ->
						(PackagePrice + Reserve)
				end
	end,
	F2 = fun(Deb, Reserve, Reserved) -> F3(F1(Deb), F1(Reserve), Reserved) end,
	CentsRemain1 = RemAmount - F1(Reservation),
	{_, Reservation1, _} = lists:keyfind(SessionId, 3, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain2 = CentsRemain1 - F1(Debit),
	{_, Reservation2, _} = lists:keyfind(SessionId, 3, Reservations2),
	Reservation2 = F2(Debit, Reservation, Reservation1).

octets_debit_and_reservation_scenario_3() ->
	[{userdata, [{doc, "Debit amount is equal to the
		package size and reservation amount grater than avaliable remain amount"}]}].

octets_debit_and_reservation_scenario_3(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain1, reservations = Reservations1} = lists:keyfind(cents, #bucket.units, RatedBuckets1),
	F1 = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	F3 = fun(Deb, Reserve, 0) ->
				(Deb + Reserve);
			(Deb, Reserve, Reserved) ->
				case Deb rem Reserved of
					0 ->
						Reserve;
					_ ->
						(PackagePrice + Reserve)
				end
	end,
	F2 = fun(Deb, Reserve, Reserved) -> F3(F1(Deb), F1(Reserve), Reserved) end,
	CentsRemain1 = RemAmount - F1(Reservation),
	{_, Reservation1, _} = lists:keyfind(SessionId, 3, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0} = lists:keyfind(cents, #bucket.units, RatedBuckets2).

octets_debit_and_reservation_scenario_4() ->
	[{userdata, [{doc, "Suffient amount for dibit the
		usage but not for the reservation"}]}].

octets_debit_and_reservation_scenario_4(_Config) ->
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
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain1, reservations = Reservations1} = lists:keyfind(cents, #bucket.units, RatedBuckets1),
	F1 = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	F3 = fun(Deb, Reserve, 0) ->
				(Deb + Reserve);
			(Deb, Reserve, Reserved) ->
				case Deb rem Reserved of
					0 ->
						Reserve;
					_ ->
						(PackagePrice + Reserve)
				end
	end,
	F2 = fun(Deb, Reserve, Reserved) -> F3(F1(Deb), F1(Reserve), Reserved) end,
	CentsRemain1 = RemAmount - F1(Reservation),
	{_, Reservation1, _} = lists:keyfind(SessionId, 3, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{out_of_credit, _} = ocs_rating:rate(diameter, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

octets_debit_and_reservation_scenario_5() ->
	[{userdata, [{doc, "Insuffient amount for dibit and the reservation"}]}].

octets_debit_and_reservation_scenario_5(_Config) ->
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
	RemAmount = 150,
	Debit = 2500,
	Reservation = 1000,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, SubscriberID, Destination, initial, [], [{octets, Reservation}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(radius, SubscriberID, Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

