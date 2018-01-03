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
	[initial_exact_fit, initial_insufficient,
	initial_insufficient_multisession, initial_overhead,
	initial_multiple_buckets, initial_expire_buckets,
	initial_ignore_expired_buckets, initial_add_session,
	interim_reserve, initial_negative_balance,
	interim_reserve_within_unit_size,
	interim_reserve_available,
	interim_reserve_out_of_credit, interim_reserve_remove_session,
	interim_reserve_multiple_buckets_available,
	interim_reserve_multiple_buckets_out_of_credit,
	interim_debit_exact_balance, interim_debit_under_unit_size,
	interim_debit_out_of_credit, interim_debit_remove_session,
	interim_debit_and_reserve_available,
	interim_debit_and_reserve_insufficient1,
	interim_debit_and_reserve_insufficient2,
	interim_debit_and_reserve_insufficient3,
	interim_debit_and_reserve_insufficient4,
	final_remove_session, final_refund].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
initial_exact_fit() ->
	[{userdata, [{doc, "Cents balance exactly equal to reservation price"}]}].

initial_exact_fit(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, Subscriber2, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	#subscriber{buckets = [#bucket{remain_amount = 0,
			reservations = [{_, PackagePrice, _}]}]} = Subscriber2.

initial_insufficient() ->
	[{userdata, [{doc, "Insufficient cents balance for initial reservation"}]}].

initial_insufficient(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 13,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(radius, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = [#bucket{units = cents, remain_amount = RemAmount,
			reservations = []}]}} = ocs:find_subscriber(SubscriberID).

initial_insufficient_multisession() ->
	[{userdata, [{doc, "Insufficient cents balance on initial reservation of additional session"}]}].

initial_insufficient_multisession(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 13,
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		reservations = [{erlang:system_time(?MILLISECOND), 100, SessionId1}],
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, #subscriber{buckets = Buckets1}} =
		ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(radius, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = Buckets1}} = ocs:find_subscriber(SubscriberID).

initial_add_session() ->
	[{userdata, [{doc, "Add a session"}]}].

initial_add_session(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = 2,
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, #subscriber{session_attributes = [{_, SessionAttr}]},
			PackageSize} = ocs_rating:rate(radius, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, PackageSize}], SessionAttr).

initial_overhead() ->
	[{userdata, [{doc, "Reserved amount greater than requested reservation amount"}]}].

initial_overhead(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
			units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount1 = 233,
	Reservation = 1555,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount1,
			start_date = erlang:system_time(?MILLISECOND),
			termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, #subscriber{buckets = [#bucket{remain_amount = RemAmount2}]},
			Reserved} = ocs_rating:rate(radius, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
	F = fun(A) when (A rem PackageSize) == 0 ->
				(A div PackageSize) * PackagePrice;
			(A) ->
				(A div PackageSize + 1) * PackagePrice
	end,
	RemAmount2 = RemAmount1 - F(Reservation),
	0 = Reserved rem PackageSize,
	true = Reserved > Reservation.

initial_multiple_buckets() ->
	[{userdata, [{doc, "Reservation over multiple cents buckets"}]}].

initial_multiple_buckets(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	Reservation = 1111,
	B1 = #bucket{units = cents, remain_amount = 50,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	B2 = #bucket{units = cents, remain_amount = 100,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	B3 = #bucket{units = cents, remain_amount = 75,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	Buckets = [B1, B2, B3],
	Balance1 = lists:sum([R || #bucket{remain_amount = R} <- Buckets]),
	Destination = ocs:generate_identity(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, #subscriber{buckets = RatedBuckets}, Reserved} =
			ocs_rating:rate(radius, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
	Balance2 = lists:sum([R || #bucket{remain_amount = R} <- RatedBuckets]),
	F = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	Balance2 = Balance1 - F(Reservation),
	0 = Reserved rem PackageSize,
	true = Reserved > Reservation.

initial_expire_buckets() ->
	[{userdata, [{doc, "Remove expired buckets"}]}].

initial_expire_buckets(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

initial_ignore_expired_buckets() ->
	[{userdata, [{doc, "Ignore expired buckets with sessions"}]}].

initial_ignore_expired_buckets(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) - 2592000000}],
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ExpiredBucket = #bucket{units = cents, remain_amount = 1000,
		reservations = [{erlang:system_time(?MILLISECOND) - 3666000, 123, SessionId1}],
		start_date = erlang:system_time(?MILLISECOND) - (2 * 2592000000),
		termination_date = erlang:system_time(?MILLISECOND) - 2592000000},
	RemAmount = 565,
	CurrentBucket = #bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000},
	Buckets1 = [ExpiredBucket, CurrentBucket],
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID,
			Password, ProdID, Chars, Buckets1),
	Destination = ocs:generate_identity(),
	Reservation = rand:uniform(PackageSize),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = <<"32251@3gpp.org">>,
	{ok, #subscriber{buckets = Buckets2},
			PackageSize} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId2),
	2 = length(Buckets2).

initial_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and ignore"}]}].

initial_negative_balance(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price1 = #price{name = "subscription", type = recurring,
			period = monthly, amount = 2000},
	Price2 = #price{name = "usage", type = usage,
			units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price1, Price2],
			specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) - 2592000000}],
	{ok, #subscriber{buckets = Buckets1}} = ocs:add_subscriber(SubscriberID,
			Password, ProdID, Chars, []),
	Balance = lists:sum([R || #bucket{remain_amount = R} <- Buckets1]),
	Destination = ocs:generate_identity(),
	Reservation = rand:uniform(PackageSize),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = <<"32251@3gpp.org">>,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = Buckets2}} = ocs:find_subscriber(SubscriberID),
	Balance = lists:sum([R || #bucket{remain_amount = R} <- Buckets2]).

interim_reserve() ->
	[{userdata, [{doc, "Reservation amount equal to package size"}]}].

interim_reserve(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price],
		specification = 4},
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
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID,
			Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, _Subscriber2, PackageSize} = ocs_rating:rate(radius, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, InitialReservation}],
			SessionId),
	InterimReservation = PackageSize + InitialReservation,
	{ok, #subscriber{buckets = [#bucket{remain_amount = 0,
			reservations = [{_, Reserved, _}]}]},
			_Reservation} = ocs_rating:rate(radius, ServiceType, SubscriberID,
			Destination, interim, [], [{octets, InterimReservation}], SessionId),
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reserved = F(InterimReservation).

interim_reserve_within_unit_size() ->
	[{userdata, [{doc, "Reservation amounts less than package size"}]}].

interim_reserve_within_unit_size(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price],
			specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	%% 1st Reservation
	Reservation1 = 100,
	{ok, #subscriber{buckets = [#bucket{remain_amount = CentsRemain1,
			reservations = [{_, Reserved1, _}]}]}, PackageSize}
			= ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation1}], SessionId),
	CentsRemain1 = RemAmount - F(Reservation1),
	Reserved1 = F(Reservation1),
	%% 2nd Reservation
	Reservation2 = 300,
	{ok, #subscriber{buckets = [#bucket{remain_amount = CentsRemain2,
			reservations = [{_, Reserved2, _}]}]}, PackageSize}
			= ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [], [{octets, Reservation2}], SessionId),
	CentsRemain2 = RemAmount - F(Reservation2),
	Reserved2 = F(Reservation2),
	%% 3rd Reservation
	Reservation3 = 700,
	{ok, #subscriber{buckets = [#bucket{remain_amount = CentsRemain3,
			reservations = [{_, Reserved3, _}]}]}, PackageSize}
			= ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [], [{octets, Reservation3}], SessionId),
	CentsRemain3 = RemAmount - F(Reservation3),
	Reserved3 = F(Reservation3).

interim_reserve_available() ->
	[{userdata, [{doc, "Reservation amount equal to balance and package size"}]}].

interim_reserve_available(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	Buckets = [#bucket{units = cents, remain_amount = 2 * PackagePrice,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID,
			Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _Subscriber2, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = [#bucket{remain_amount = 0,
			reservations = [{_, PackagePrice, _}]}]}, PackageSize}
			= ocs_rating:rate(diameter, ServiceType, SubscriberID, Destination,
			interim, [{octets, PackageSize}], [{octets, PackageSize}], SessionId).

interim_reserve_out_of_credit() ->
	[{userdata, [{doc, "Out of credit on reservation"}]}].

interim_reserve_out_of_credit(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product1 = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _Product2} = ocs:add_product(Product1),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 110,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID,
			Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Destination = ocs:generate_identity(),
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _Subscriber2, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, PackageSize}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [], [{octets, 2 * PackageSize}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0} = lists:keyfind(cents, #bucket.units, RatedBuckets).

interim_reserve_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_reserve_remove_session(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation1}], SessionAttributes),
	Reservation2 = 1100,
	{out_of_credit, SessionList} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, Reservation2}],
			SessionAttributes),
	[{_, SessionAttributes}] = SessionList,
	{ok, #subscriber{session_attributes = []}} = ocs:find_subscriber(SubscriberID).

interim_reserve_multiple_buckets_available() ->
	[{userdata, [{doc, "Reservation with multiple buckets"}]}].

interim_reserve_multiple_buckets_available(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation1}], SessionAttributes),
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
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [], [{octets, Reservation2}], SessionAttributes),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	Reserved2 = F1(F1, cents, RatedBuckets2, 0),
	Reserved2 = F2(Reservation2).

interim_reserve_multiple_buckets_out_of_credit() ->
	[{userdata, [{doc, "Out of credit with multiple cents buckets"}]}].

interim_reserve_multiple_buckets_out_of_credit(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = 4,
	ok = mnesia:dirty_write(subscriber, Subscriber),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation1}], SessionAttributes),
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
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [], [{octets, Reservation2}], SessionAttributes),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	F3 = fun(#bucket{remain_amount = R}, Acc) -> R + Acc end,
	0 = lists:foldl(F3, 0, RatedBuckets2).

interim_debit_exact_balance() ->
	[{userdata, [{doc, "Debit amount equal to package size"}]}].

interim_debit_exact_balance(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, PackageSize}], [], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = []} = lists:keyfind(cents, #bucket.units, RatedBuckets).

interim_debit_under_unit_size() ->
	[{userdata, [{doc, "Debit amount less than package size"}]}].

interim_debit_under_unit_size(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, Debit}], [], SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice.

interim_debit_out_of_credit() ->
	[{userdata, [{doc, "Insufficient amount to debit"}]}].

interim_debit_out_of_credit(_Config) ->
	AccountAmount = 200,
	PackageAmount = 100,
	PackageSize = 1000,
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackageAmount},
	Product1 = #product{name = ProdID, price = [Price], specification = 4},
	{ok, _Product2} = ocs:add_product(Product1),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	Buckets = [#bucket{units = cents, remain_amount = AccountAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	{ok, _Subscriber1} = ocs:add_subscriber(SubscriberID,
			Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _Subscriber2, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [{octets, PackageSize}],
			SessionId),
	{ok, _Subscriber3, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, interim, [{octets, PackageSize}],
			[{octets, PackageSize}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, interim, [{octets, PackageSize}],
			[{octets, PackageSize}], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

interim_debit_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_debit_remove_session(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, interim, [{octets, Debit}], [], SessionId),
	{ok, #subscriber{session_attributes = []}} = ocs:find_subscriber(SubscriberID).

interim_debit_and_reserve_available() ->
	[{userdata, [{doc, "Debit given usage and check for reservation, sufficient balance exists"}]}].

interim_debit_and_reserve_available(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain1, reservations = Reservations1} =
			lists:keyfind(cents, #bucket.units, RatedBuckets1),
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
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, Debit}], [{octets, Reservation}],
			SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain2 = CentsRemain1 - F1(Debit),
	{_, Reservation2, _} = lists:keyfind(SessionId, 3, Reservations2),
	Reservation2 = F2(Debit, Reservation, Reservation1).

interim_debit_and_reserve_insufficient1() ->
	[{userdata, [{doc, "Debit amount less than package size and reservation amount
			less than available balance, sufficient balance exists"}]}].

interim_debit_and_reserve_insufficient1(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
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
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, Debit}], [{octets, Reservation}],
			SessionId),
	{ok, #subscriber{buckets = RatedBuckets}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain2 = CentsRemain1 - F1(Debit),
	{_, Reservation2, _} = lists:keyfind(SessionId, 3, Reservations2),
	Reservation2 = F2(Debit, Reservation, Reservation1).

interim_debit_and_reserve_insufficient2() ->
	[{userdata, [{doc, "Debit amount equal to package size and
			reservation amount greater than available balance"}]}].

interim_debit_and_reserve_insufficient2(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price],
			specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
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
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0} = lists:keyfind(cents, #bucket.units, RatedBuckets2).

interim_debit_and_reserve_insufficient3() ->
	[{userdata, [{doc, "Suffient balance for debit but not reservation"}]}].

interim_debit_and_reserve_insufficient3(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
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
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = []}} = ocs:find_subscriber(SubscriberID).

interim_debit_and_reserve_insufficient4() ->
	[{userdata, [{doc, "Insuffient amount for debit and reservation"}]}].

interim_debit_and_reserve_insufficient4(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price],
			specification = 4},
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
	ServiceType = 2,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, Reservation}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #subscriber{buckets = [#bucket{remain_amount = -150,
			units = cents}]}} = ocs:find_subscriber(SubscriberID).

final_remove_session() ->
	[{userdata, [{doc, "Final call remove session attributes from subscriber record"}]}].

final_remove_session(_Config) ->
	ProdID = ocs:generate_password(),
	Price = #price{name = "overage", type = usage,
		units = octets, size = 1000, amount = 100},
	Product = #product{name = ProdID, price = [Price], specification = 4},
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
	SA2 = {erlang:system_time(?MILLISECOND),
		[{'Session-Id', list_to_binary(ocs:generate_password())},
		{?NasIdentifier, "rate2@sigscale"}, {?NasIpAddress, "10.0.0.2"}]},
	Subscriber = #subscriber{name = SubscriberID, password = Password,
			buckets = Buckets, session_attributes = [SA1],
			product = #product_instance{product = ProdID, characteristics = Chars}},
	mnesia:dirty_write(subscriber, Subscriber),
	Destination = ocs:generate_identity(),
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			SubscriberID, Destination, initial, [], [], [SA2]),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, final, [{octets, Debit}], [], [SA2]),
	{ok, #subscriber{session_attributes = [SA1]}} = ocs:find_subscriber(SubscriberID).

final_refund() ->
	[{userdata, [{doc, "Refund unused amount of reservation"}]}].

final_refund(_Config) ->
	ProdID = ocs:generate_password(),
	PackagePrice = 100,
	PackageSize = 1000,
	Price = #price{name = "overage", type = usage,
		units = octets, size = PackageSize, amount = PackagePrice},
	Product = #product{name = ProdID, price = [Price],
		specification = 4},
	{ok, _} = ocs:add_product(Product),
	SubscriberID = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	Chars = [{validity, erlang:system_time(?MILLISECOND) + 2592000000}],
	RemAmount = 100,
	Buckets = [#bucket{units = cents, remain_amount = RemAmount,
		start_date = erlang:system_time(?MILLISECOND),
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}],
	Destination = ocs:generate_identity(),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = <<"32251@3gpp.org">>,
	{ok, _} = ocs:add_subscriber(SubscriberID, Password, ProdID, Chars, Buckets),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, PackageSize}], SessionId1),
	{ok, #subscriber{buckets = RatedBuckets1}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = Reserved1} = lists:keyfind(cents, #bucket.units, RatedBuckets1),
	[{_, PackagePrice, SessionId1}] = Reserved1,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, final, [{octets, 0}], [], SessionId1),
	{ok, #subscriber{buckets = RatedBuckets2}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = PackagePrice, reservations = []} = lists:keyfind(cents, #bucket.units, RatedBuckets2),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, SubscriberID,
			Destination, initial, [], [{octets, PackageSize}], SessionId2),
	{ok, #subscriber{buckets = RatedBuckets3}} = ocs:find_subscriber(SubscriberID),
	#bucket{remain_amount = 0, reservations = Reserved2} = lists:keyfind(cents, #bucket.units, RatedBuckets3),
	[{_, PackagePrice, SessionId2}] = Reserved2.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

