%%% ocs_rating_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs.hrl").
-include("ocs_log.hrl").
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
	initial_ignore_expired_buckets,
	initial_negative_balance, initial_add_session,
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
	interim_debit_and_reserve_charging_key,
	interim_out_of_credit_voice, final_remove_session, remove_session_after_multiple_interims,
	final_refund_octets, final_refund_seconds,
	final_voice, final_multiple_buckets,
	reserve_data, reserve_voice, interim_voice, time_of_day,
	authorize_voice, authorize_voice_with_partial_reservation,
	authorize_incoming_voice, authorize_outgoing_voice,
	authorize_default_voice, authorize_data_1, authorize_data_2,
	authorize_data_with_partial_reservation, authorize_negative_balance,
	unauthorize_bad_password, unauthorize_bad_password, reserve_sms, debit_sms,
	roaming_table_data, roaming_table_voice, roaming_table_sms, final_empty_mscc].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

initial_exact_fit() ->
	[{userdata, [{doc, "Cents balance exactly equal to reservation price"}]}].

initial_exact_fit(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 500000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, #bucket{remain_amount = 0, reservations = Rs}} = ocs:find_bucket(BId),
	[{_, PackagePrice, 0, _, _, SessionId}] = Rs.

initial_insufficient() ->
	[{userdata, [{doc, "Insufficient cents balance for initial reservation"}]}].

initial_insufficient(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice - 1,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{out_of_credit, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, #bucket{units = cents, remain_amount = RemAmount,
			reservations = []}} = ocs:find_bucket(BId).

initial_insufficient_multisession() ->
	[{userdata, [{doc, "Insufficient cents balance on initial reservation of additional session"}]}].

initial_insufficient_multisession(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 13,
	B1 = bucket(cents, RemAmount),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	R = [{erlang:system_time(?MILLISECOND), 100,
			undefined, undefined, undefined, SessionId1}],
	B2 = B1#bucket{reservations = R},
	BId = add_bucket(ProdRef, B2),
	Timestamp = calendar:local_time(),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{out_of_credit, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined,
			ServiceId, Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId2),
	{ok, #bucket{remain_amount = RemAmount, reservations = R}} = ocs:find_bucket(BId).

initial_add_session() ->
	[{userdata, [{doc, "Add a session"}]}].

initial_add_session(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 1000,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	ServiceType = 2,
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.1.150"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttr = [NasId, NasIp, AcctSessionId, {?ServiceType, ServiceType}],
	{ok, #service{session_attributes = [{_, SessionId}]},
			{PackageUnits, PackageSize}} = ocs_rating:rate(radius,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionAttr),
	{ok, #bucket{reservations = R}} = ocs:find_bucket(BId),
	[{_, PackagePrice, 0, _, _, SessionId}] = R.

initial_overhead() ->
	[{userdata, [{doc, "Reserved amount greater than requested reservation amount"}]}].

initial_overhead(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = 233,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	Reservation = 1555,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, #service{}, {PackageUnits, Reserved}} = ocs_rating:rate(radius,
			ServiceType, undefined, undefined, undefined, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, Reservation}],
			SessionId),
	{ok, #bucket{remain_amount = RemAmount2,
			reservations = [CReservation]}} = ocs:find_bucket(BId),
	F = fun(A) when (A rem PackageSize) == 0 ->
				(A div PackageSize) * PackagePrice;
			(A) ->
				(A div PackageSize + 1) * PackagePrice
	end,
	RemAmount2 = RemAmount1 - F(Reservation),
	0 = Reserved rem PackageSize,
	true = Reserved > Reservation,
	element(3, CReservation) == F(Reservation).

initial_multiple_buckets() ->
	[{userdata, [{doc, "Reservation over multiple cents buckets"}]}].

initial_multiple_buckets(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, (UnitPrice div 2) + rand:uniform(UnitPrice div 2)),
	_BId1 = add_bucket(ProdRef, B1),
	B2 = bucket(cents, (UnitPrice div 2) + rand:uniform(UnitPrice div 2)),
	_BId2 = add_bucket(ProdRef, B2),
	B3 = bucket(cents, (UnitPrice div 2) + rand:uniform(UnitPrice div 2)),
	_BId3 = add_bucket(ProdRef, B3),
	Balance1 = lists:sum([R || #bucket{remain_amount = R} <- [B1, B2, B3]]),
	Timestamp = calendar:local_time(),
	ServiceType = 2,
	Reservation = (UnitSize div 2) + rand:uniform(UnitSize div 2),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, #service{}, {PackageUnits,  Reserved}} = ocs_rating:rate(radius,
			ServiceType, undefined, undefined, undefined, ServiceId, Timestamp,
			undefined, undefined, initial, [],
			[{PackageUnits, Reservation}], SessionId),
	[#product{balance = BucketRefs}] = mnesia:dirty_read(product, ProdRef),
	RatedBuckets = lists:flatten([mnesia:dirty_read(bucket, Id) || Id <- BucketRefs]),
	Balance2 = lists:sum([B#bucket.remain_amount || B <- RatedBuckets,
			B#bucket.units == cents]),
	F = fun(A) when (A rem UnitSize) == 0 ->
			(A div UnitSize) * UnitPrice;
		(A) ->
			(A div UnitSize + 1) * UnitPrice
	end,
	Balance2 = Balance1 - F(Reservation),
	0 = Reserved rem UnitSize,
	true = Reserved > Reservation,
	#bucket{reservations = [{_, 0, Reserved, _, _, SessionId}],
			remain_amount = 0} = lists:keyfind(octets,
			#bucket.units, RatedBuckets).

initial_expire_buckets() ->
	[{userdata, [{doc, "Remove expired buckets"}]}].

initial_expire_buckets(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	B2= B1#bucket{start_date = erlang:system_time(?MILLISECOND) -  (2 * 2592000000),
		end_date = erlang:system_time(?MILLISECOND) - 2592000000},
	BId = add_bucket(ProdRef, B2),
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, #product{balance = []}} = ocs:find_product(ProdRef),
	{error, not_found} = ocs:find_bucket(BId).

initial_ignore_expired_buckets() ->
	[{userdata, [{doc, "Ignore expired buckets with sessions"}]}].

initial_ignore_expired_buckets(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Now = erlang:system_time(?MILLISECOND),
	RemAmount1 = rand:uniform(PackagePrice * 10),
	Reservations = [{Now - 3666000, rand:uniform(PackagePrice * 3),
			0, undefined, undefined, SessionId1}],
	ExpiredBucket = #bucket{units = cents,
			remain_amount = RemAmount1, reservations = Reservations,
			start_date = Now - (2 * 2592000000), end_date = Now - 2592000000},
	BId1 = add_bucket(ProdRef, ExpiredBucket),
	RemAmount2 = PackagePrice + rand:uniform(PackagePrice * 10),
	CurrentBucket = bucket(cents, RemAmount2),
	_BId2 = add_bucket(ProdRef, CurrentBucket),
	Timestamp = calendar:local_time(),
	Reservation = rand:uniform(PackageSize),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, Reservation}],
			SessionId2),
	{ok, #bucket{remain_amount = RemAmount1}} = ocs:find_bucket(BId1).

initial_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and ignore"}]}].

initial_negative_balance(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	PackageAmount = 100 + rand:uniform(2000),
	P1 = #price{name = "subscription", type = recurring,
			period = monthly, amount = PackageAmount},
	P2 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1, P2], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Timestamp = calendar:local_time(),
	Reservation = rand:uniform(PackageSize),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp,
			undefined, undefined, initial,
			[], [{PackageUnits, Reservation}], SessionId).

interim_reserve() ->
	[{userdata, [{doc, "Reservation amount equal to package size"}]}].

interim_reserve(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = (2 * PackagePrice) + rand:uniform(PackagePrice * 6),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	ServiceType = 32251,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _Service, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, #service{}, _Reservation} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, PackageSize}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	{ok, #bucket{remain_amount = RemAmount2,
			reservations = R}} = ocs:find_bucket(BId),
	[{_, PackagePrice, 0, _, _, SessionId}] = R.

interim_reserve_within_unit_size() ->
	[{userdata, [{doc, "Reservation amounts less than package size"}]}].

interim_reserve_within_unit_size(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice * 10,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reservation1 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined,
			undefined, initial, [], [{PackageUnits, Reservation1}], SessionId),
	{ok, #bucket{remain_amount = RemAmount2,
			reservations = R1}} = ocs:find_bucket(BId),
	[{_, PackagePrice, _, _, _, SessionId}] = R1,
	RemAmount2 = RemAmount1 - F(Reservation1),
	Reservation2 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [], [{PackageUnits, Reservation2}], SessionId),
	{ok, #bucket{remain_amount = RemAmount3,
			reservations = R2}} = ocs:find_bucket(BId),
	[{_, Reserved1, _, _, _, SessionId}] = R2,
	Reserved1 = F(Reservation2),
	RemAmount3 = RemAmount1 - F(Reservation2),
	Reservation3 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 120), undefined, undefined,
			interim, [], [{PackageUnits, Reservation3}], SessionId),
	{ok, #bucket{remain_amount = RemAmount4,
			reservations = R3}} = ocs:find_bucket(BId),
	[{_, Reserved2, 0, _, _, SessionId}] = R3,
	RemAmount4 = RemAmount1 - F(Reservation3),
	Reserved2 = F(Reservation3).

interim_reserve_available() ->
	[{userdata, [{doc, "Reservation amount equal to balance and package size"}]}].

interim_reserve_available(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _Service2, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, #bucket{remain_amount = 0,
			reservations = R}} = ocs:find_bucket(BId),
	[{_, PackagePrice, 0, _, _, SessionId}] = R.

interim_reserve_out_of_credit() ->
	[{userdata, [{doc, "Out of credit on reservation"}]}].

interim_reserve_out_of_credit(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ReserveSize = rand:uniform(UnitSize),
	{ok, _Service2, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, ReserveSize}], SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, UnitSize * 2}], SessionId),
	Remain = StartingAmount - UnitPrice,
	{ok, #bucket{remain_amount = Remain}} = ocs:find_bucket(BId).

interim_reserve_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_reserve_remove_session(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	Reservation1 = rand:uniform(UnitSize),
	ServiceType = 32251,
	SessionId = {'Session-Id', list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "10.0.0.1"},
	NasId = {?NasIdentifier, "rate@sigscale"},
	SessionAttributes  = [NasId, NasIp, SessionId],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionAttributes),
	Reservation2 = rand:uniform(UnitSize) + UnitSize,
	{out_of_credit, _, SessionList} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, initial, [],
			[{PackageUnits, Reservation2}], SessionAttributes),
	[{_, [SessionId]}] = SessionList,
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_reserve_multiple_buckets_available() ->
	[{userdata, [{doc, "Reservation with multiple buckets"}]}].

interim_reserve_multiple_buckets_available(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = (PackagePrice div 3) + rand:uniform(PackagePrice div 3),
	B1 = bucket(cents, RemAmount1),
	_BId1 = add_bucket(ProdRef, B1),
	RemAmount2 = (PackagePrice div 3) + rand:uniform(PackagePrice div 2),
	B2 = bucket(cents, RemAmount2),
	_BId2 = add_bucket(ProdRef, B2),
	RemAmount3 = PackagePrice + rand:uniform(PackagePrice div 3),
	B3 = bucket(cents, RemAmount3),
	_BId3 = add_bucket(ProdRef, B3),
	ServiceType = 32251,
	Reservation1 = rand:uniform(PackageSize),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	Reservation2 = rand:uniform(PackageSize),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, Reservation2}],
			SessionId).


interim_reserve_multiple_buckets_out_of_credit() ->
	[{userdata, [{doc, "Out of credit with multiple cents buckets"}]}].

interim_reserve_multiple_buckets_out_of_credit(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = (PackagePrice div 2),
	B1 = bucket(cents, RemAmount1),
	_BId1 = add_bucket(ProdRef, B1),
	RemAmount2 = (PackagePrice div 2),
	B2 = bucket(cents, RemAmount2),
	_BId2 = add_bucket(ProdRef, B2),
	RemAmount3 = (PackagePrice div 2),
	B3 = bucket(cents, RemAmount3),
	_BId3 = add_bucket(ProdRef, B3),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, PackageSize}],
			[], SessionId).

interim_debit_exact_balance() ->
	[{userdata, [{doc, "Debit amount equal to package size"}]}].

interim_debit_exact_balance(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackageSize),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined,
			undefined, interim, [{PackageUnits, PackageSize}], [], SessionId).

interim_debit_under_unit_size() ->
	[{userdata, [{doc, "Debit amount less than package size"}]}].

interim_debit_under_unit_size(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = (2 * PackagePrice) + 1,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			interim, [{PackageUnits, PackageSize div 3}], [{PackageUnits, 0}], SessionId).

interim_debit_out_of_credit() ->
	[{userdata, [{doc, "Insufficient amount to debit"}]}].

interim_debit_out_of_credit(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _Service1, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + 1}], [{PackageUnits, 0}], SessionId).

interim_debit_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_debit_remove_session(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = rand:uniform(PackagePrice - 1),
	_BId = add_bucket(ProdRef, bucket(cents, RemAmount)),
	SessionAttributes = [{erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate@sigscale"}, {?NasIpAddress, "10.0.0.1"}]}],
	ServiceType = 2,
	Timestamp = calendar:local_time(),
	{ok, _,  _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionAttributes),
	{ok, #service{session_attributes = [SessionAttributes]}} = ocs:find_service(ServiceId),
	{out_of_credit, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			interim, [{PackageUnits, PackagePrice}], [{PackageUnits, 0}], SessionAttributes),
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_debit_and_reserve_available() ->
	[{userdata, [{doc, "Debit given usage and check for reservation, sufficient balance exists"}]}].

interim_debit_and_reserve_available(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice * 100,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Reservation = rand:uniform(PackageSize),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	{ok, #bucket{reservations = [{_, PackagePrice, _, _, _, SessionId}],
			remain_amount = RemAmount2}} = ocs:find_bucket(BId),
	Debit = rand:uniform(Reservation),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, Reservation}], SessionId),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient1() ->
	[{userdata, [{doc, "Debit amount less than package size and reservation amount
			less than available balance, insufficient balance exists"}]}].

interim_debit_and_reserve_insufficient1(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice + rand:uniform(PackagePrice - 1),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Reservation1 = rand:uniform(PackageSize),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	Debit = rand:uniform(PackageSize - 1),
	Reservation2 = PackageSize * 2,
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, Reservation2}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient2() ->
	[{userdata, [{doc, "Debit amount equal to unit size and
			reservation amount greater than available balance"}]}].

interim_debit_and_reserve_insufficient2(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = (2 * PackagePrice) + rand:uniform(PackagePrice - 1),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Reservation1 = rand:uniform(PackageSize),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	Reservation2 = PackageSize + rand:uniform(PackageSize),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, PackageSize}],
			[{PackageUnits, Reservation2}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient3() ->
	[{userdata, [{doc, "Suffient balance for debit but not reservation"}]}].

interim_debit_and_reserve_insufficient3(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice + rand:uniform(PackagePrice - 1),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Reservation1 = rand:uniform(PackageSize),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	UsedUnits = rand:uniform(Reservation1),
	Reservation2 = PackageSize,
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, UsedUnits}],
			[{PackageUnits, Reservation2}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient4() ->
	[{userdata, [{doc, "Insuffient amount for debit and reservation"}]}].

interim_debit_and_reserve_insufficient4(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice + rand:uniform(PackagePrice - 1),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Debit = PackagePrice * 2,
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, PackageSize}], SessionId).

interim_debit_and_reserve_charging_key() ->
	[{userdata, [{doc, "Multiple charging keys (Rating-Group) in session"}]}].

interim_debit_and_reserve_charging_key(_Config) ->
	PackagePrice = rand:uniform(100),
	PackageSize = 5000000 + rand:uniform(100000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice * 100 + rand:uniform(PackagePrice),
	B1 = bucket(cents, RemAmount1),
	BId1 = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	RatingGroup1 = 32,
	RatingGroup2 = 6400,
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, {PackageUnits, G1}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, ServiceId,
			Timestamp, undefined, undefined, initial, [], [], SessionId),
	{ok, _, {PackageUnits, G2}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, ServiceId,
			Timestamp, undefined, undefined, initial, [], [], SessionId),
	RemAmount2 = RemAmount1 - (PackagePrice * 2),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId1),
	F2 = fun(#bucket{name = "session"} = B, Acc) -> [B | Acc]; (_, Acc) -> Acc end,
	BucketList = lists:foldl(F2, [], ocs:get_buckets(ProdRef)),
	2 = length(BucketList),
	Debit1 = rand:uniform(PackageSize),
	Debit2 = rand:uniform(PackageSize),
	{ok, _, {PackageUnits, G3}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, interim, [{PackageUnits, Debit1}],
			[], SessionId),
	{ok, _, {PackageUnits, G4}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, interim, [{PackageUnits, Debit2}],
			[], SessionId),
	RemAmount3 = RemAmount2 - (PackagePrice * 2),
	{ok, #bucket{remain_amount = RemAmount3}} = ocs:find_bucket(BId1),
	[#bucket{reservations = _Reservations1},
			#bucket{reservations = _Reservations2}] = lists:foldl(F2, [], ocs:get_buckets(ProdRef)),
	Debit3 = rand:uniform(PackageSize),
	Debit4 = rand:uniform(PackageSize),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, final, [{PackageUnits, Debit3}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, final, [{PackageUnits, Debit4}],
			[], SessionId),
	{ok, #bucket{remain_amount = RemAmount3}} = ocs:find_bucket(BId1),
	1 = length(ocs:get_buckets(ProdRef)).

interim_out_of_credit_voice() ->
	[{userdata, [{doc, "Voice call out of credit during call"}]}].

interim_out_of_credit_voice(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	P1 = price(usage, seconds, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ReserveUnits1 = rand:uniform(UnitSize),
	ServiceType = 32260,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{seconds, ReserveUnits1}], SessionId),
	UsedUnits = (UnitSize * 2) + rand:uniform(UnitSize),
	ReserveUnits2 = rand:uniform(UnitSize),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{seconds, UsedUnits}], [{seconds, ReserveUnits2}], SessionId),
	Remain = StartingAmount - UnitPrice,
	{ok, #bucket{remain_amount = Remain}} = ocs:find_bucket(BId).

final_remove_session() ->
	[{userdata, [{doc, "Final call remove session attributes from subscriber record"}]}].

final_remove_session(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 1000,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Debit = 100,
	AcctSessionId1 = {?AcctSessionId, "1020303"},
	NasId1 = {?NasIdentifier, "rate1@sigscale"},
	NasIp1 = {?NasIpAddress, "10.0.0.1"},
	SessionId1 = lists:keysort(1, [AcctSessionId1, NasIp1, NasId1]),
	SA1 = {erlang:system_time(?MILLISECOND), SessionId1},
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	F = fun() ->
		[Service] = mnesia:read(service, list_to_binary(ServiceId), read),
		mnesia:write(Service#service{session_attributes = [SA1]})
	end,
	{atomic, ok} = mnesia:transaction(F),
	SA2 = [{264, "10.0.0.2"}, {296, "example.com"}] ++ SessionId2,
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], [SA2]),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined,
			undefined, final, [{PackageUnits, Debit}], [], [SA2]),
	{ok, #service{session_attributes = [SA1]}} = ocs:find_service(ServiceId).

remove_session_after_multiple_interims() ->
	[{userdata, [{doc, "Remove session after multiple interims that exceed the reserved amount"}]}].

remove_session_after_multiple_interims(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 1000,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 60), undefined,
			undefined, interim, [{PackageUnits, PackageSize + rand:uniform(PackageSize div 2)}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 120), undefined,
			undefined, interim, [{PackageUnits, PackageSize + rand:uniform(PackageSize div 3)}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 180), undefined,
			undefined, interim, [{PackageUnits, PackageSize + rand:uniform(PackageSize div 3)}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 240), undefined,
			undefined, final, [{PackageUnits, PackageSize + rand:uniform(PackageSize div 2)}], [], SessionId),
	[#bucket{units = cents, reservations = []}] = ocs:get_buckets(ProdRef).

final_refund_octets() ->
	[{userdata, [{doc, "Refund unused amount of octets reservation"}]}].

final_refund_octets(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitSize * 4,
	B1 = bucket(octets, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	Reserve1 = rand:uniform(UnitSize),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reserve1}], SessionId1),
	UsedUnits1 = rand:uniform(Reserve1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits1}], [], SessionId1),
	Remain1 = StartingAmount - UsedUnits1,
	{ok, #bucket{remain_amount = Remain1}} = ocs:find_bucket(BId),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 120),
			undefined, undefined, initial, [],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits2 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 180),
			undefined, undefined, interim, [{PackageUnits, UsedUnits2}],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits3 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 240),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits3}], [], SessionId2),
	Remain2 = Remain1 - UsedUnits2 - UsedUnits3,
	{ok, #bucket{remain_amount = Remain2}} = ocs:find_bucket(BId).

final_refund_seconds() ->
	[{userdata, [{doc, "Refund unused amount of seconds reservation"}]}].

final_refund_seconds(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = seconds,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitSize * 4,
	B1 = bucket(seconds, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	Reserve1 = rand:uniform(UnitSize),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reserve1}], SessionId1),
	UsedUnits1 = rand:uniform(Reserve1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits1}], [], SessionId1),
	Remain1 = StartingAmount - UsedUnits1,
	{ok, #bucket{remain_amount = Remain1}} = ocs:find_bucket(BId),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 120),
			undefined, undefined, initial, [],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits2 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 180),
			undefined, undefined, interim, [{PackageUnits, UsedUnits2}],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits3 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 240),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits3}], [], SessionId2),
	Remain2 = Remain1 - UsedUnits2 - UsedUnits3,
	{ok, #bucket{remain_amount = Remain2}} = ocs:find_bucket(BId).

final_multiple_buckets() ->
	[{userdata, [{doc, "Debit applied to one of several available buckets"}]}].

final_multiple_buckets(_Config) ->
	UnitPrice = 10 + rand:uniform(90),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Balance = 10000000,
	NumBuckets = 3,
	F1 = fun F(0) ->
				ok;
			F(N) ->
				add_bucket(ProdRef, bucket(cents, Balance)),
				F(N - 1)
	end,
	F1(NumBuckets),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined, initial,
			[], [{PackageUnits, UnitSize * 3}], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim,
			[{PackageUnits, UnitSize * 3}], [{PackageUnits, UnitSize * 3}], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 120),
			undefined, undefined, final,
			[{PackageUnits, UnitSize * 3}], [], SessionId),
	NewBalance = (Balance * NumBuckets) - (6 * UnitPrice),
	F2 = fun(#bucket{remain_amount = N}, Acc) -> Acc + N end,
	NewBalance = lists:foldl(F2, 0, ocs:get_buckets(ProdRef)).

final_voice() ->
	[{userdata, [{doc, "Final RADIUS accounting request for voice call"}]}].

final_voice(_Config) ->
	UnitPrice = rand:uniform(10),
	UnitSize = 60,
	ReserveTime = rand:uniform(10) * UnitSize,
	P1 = #price{name = "Calls", type = usage,
			units = seconds, size = UnitSize, amount = UnitPrice,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]}]},
	Chars = [{"radiusReserveSessionTime", 3600}],
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, Chars),
	ServiceId = add_service(ProdRef),
	StartingAmount = (((ReserveTime * 4) div UnitSize) * UnitPrice)
			+ rand:uniform(UnitPrice),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.1.150"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttributes = [NasIp, NasId, AcctSessionId],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress,
			originate, initial, [], [], SessionAttributes),
	UsedSeconds1 = rand:uniform(ReserveTime - 1),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress,
			originate, interim, [{seconds, UsedSeconds1}], [], SessionAttributes),
	UsedSeconds2 = rand:uniform(ReserveTime - 1),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress,
			undefined, final, [{seconds, UsedSeconds2}], [], SessionAttributes),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	RemainAmount = StartingAmount - (((ReserveTime div UnitSize)
			* 2) * UnitPrice).

reserve_data() ->
	[{userdata, [{doc, "Reservation for data session"}]}].

reserve_data(_Config) ->
	DataAmount = 2,
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = octets, value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceAmount = 2,
	VoiceSize = 60,
	ReserveTime = 300,
	VoicePrice = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]}]},
	VoiceOfferId = add_offer([VoicePrice], 9),
	BundleOffer = #offer{name = ocs:generate_identity(),
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, #offer{name = BundleOfferId}} = ocs:add_offer(BundleOffer),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = 2579,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	Timestamp = calendar:local_time(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined,
			undefined, initial, [], [], SessionId),
	{ok, #bucket{remain_amount = Amount}} = ocs:find_bucket(BId),
	ReservedUnits = case (ReserveOctets rem DataSize) of
		0 ->
			ReserveOctets div DataSize;
		_ ->
			ReserveOctets div DataSize + 1
	end,
	Amount = StartingAmount - ReservedUnits * DataAmount.

reserve_voice() ->
	[{userdata, [{doc, "Reservation for voice call"}]}].

reserve_voice(_Config) ->
	DataAmount = 2,
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = octets, value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceAmount = 2,
	VoiceSize = 60,
	ReserveTime = 300,
	VoicePrice = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]}]},
	VoiceOfferId = add_offer([VoicePrice], 9),
	BundleOfferId = ocs:generate_password(),
	BundleProduct = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleProduct),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = 2567,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress,
			undefined, initial, [], [], SessionId),
	{ok, #bucket{remain_amount = Amount}} = ocs:find_bucket(BId),
	ReservedUnits = case (ReserveTime rem VoiceSize) of
		0 ->
			ReserveTime div VoiceSize;
		_ ->
			ReserveTime div VoiceSize + 1
	end,
	Amount = StartingAmount - ReservedUnits * VoiceAmount.

reserve_incoming_voice() ->
	[{userdata, [{doc, "Reservation for incoming voice call"}]}].

reserve_incoming_voice(_Config) ->
	DataAmount = 2,
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = octets, value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceSize = 60,
	VoiceAmountOut = 2,
	ReserveTime = 300,
	VoicePrice1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmountOut,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]},
			#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "originate"}]}]},
	VoiceAmountIn = 1,
	VoicePrice2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmountIn,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]},
			#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	VoiceOfferId = add_offer([VoicePrice1, VoicePrice2], 9),
	BundleOfferId  = ocs:generate_password(),
	BundleProduct = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleProduct),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress,
			answer, initial, [], [], SessionId),
	{ok, #bucket{remain_amount = Amount}} = ocs:find_bucket(BId),
	ReservedUnits = case (ReserveTime rem VoiceSize) of
		0 ->
			ReserveTime div VoiceSize;
		_ ->
			ReserveTime div VoiceSize + 1
	end,
	Amount = StartingAmount - ReservedUnits * VoiceAmountIn.

interim_voice() ->
	[{userdata, [{doc, "Interim reservation for voice call"}]}].

interim_voice(_Config) ->
	DataAmount = rand:uniform(10),
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = octets, value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceAmount = rand:uniform(10),
	VoiceSize = 60,
	ReserveTime = VoiceSize * rand:uniform(10),
	VoicePrice = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]}]},
	VoiceOfferId = add_offer([VoicePrice], 9),
	BundleOfferId = ocs:generate_password(),
	BundleOffer = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleOffer),
	StartingAmount = 1000,
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress,
			undefined, initial, [], [], SessionId),
	UsedOctets = rand:uniform(ReserveOctets - 1),
	UsedSeconds = rand:uniform(ReserveTime - 1),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, CallAddress, undefined,
			interim, [{octets, UsedOctets}, {seconds, UsedSeconds}], [], SessionId),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	RemainAmount = StartingAmount - (((ReserveTime div VoiceSize)
			* VoiceAmount) * 2).

time_of_day() ->
	[{userdata, [{doc, "Time of day price matching"}]}].

time_of_day(_Config) ->
	PeakAmount = 10,
	OffPeakAmount = 5,
	DataSize = 1000000,
	PeakPrice = #price{name = "Peak", type = usage,
			units = octets, size = DataSize, amount = PeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			min = 1, max = 1, values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 480, units = "minutes"},
			upper = #quantity{amount = 1380, units = "minutes"}}}]}]},
	OffPeakPrice = #price{name = "OffPeak", type = usage,
			units = octets, size = DataSize, amount = OffPeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			min = 1, max = 1, values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 1380, units = "minutes"},
			upper = #quantity{amount = 480, units = "minutes"}}}]}]},
	DataOfferId = add_offer([PeakPrice, OffPeakPrice], 8),
	ProdRef = add_product(DataOfferId),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceId = add_service(ProdRef),
	ServiceType = 2,
	{Date, _} = calendar:local_time(),
	Timestamp1 = {Date, {7, 59, 59}},
	SessionId1 = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp1, undefined,
			undefined, initial, [], [], SessionId1),
	UsedOctets1 = rand:uniform(DataSize * 6),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp1, undefined,
			undefined, final, [{octets, UsedOctets1}], [], SessionId1),
	{ok, #bucket{remain_amount = Amount1}} = ocs:find_bucket(BId),
	UsedUnits1 = case UsedOctets1 rem DataSize of
		0 ->
			UsedOctets1 div DataSize;
		_ ->
			UsedOctets1 div DataSize + 1
	end,
	Amount1 = StartingAmount - UsedUnits1 * OffPeakAmount,
	Timestamp2 = {Date, {12, 13, 14}},
	SessionId2 = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp2, undefined,
			undefined, initial, [], [], SessionId2),
	UsedOctets2 = rand:uniform(DataSize * 6),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp2, undefined, undefined,
			final, [{octets, UsedOctets2}], [], SessionId2),
	{ok, #bucket{remain_amount = Amount2}} = ocs:find_bucket(BId),
	UsedUnits2 = case UsedOctets2 rem DataSize of
		0 ->
			UsedOctets2 div DataSize;
		_ ->
			UsedOctets2 div DataSize + 1
	end,
	Amount2 = Amount1 - UsedUnits2 * PeakAmount.

authorize_voice() ->
	[{userdata, [{doc, "Authorize voice call"}]}].

authorize_voice(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, 60} = lists:keyfind(?SessionTimeout, 1, Attr),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_voice_with_partial_reservation() ->
	[{userdata, [{doc, "Authorize voice call with and set the
			session time for available partial reservation amount"}]}].

authorize_voice_with_partial_reservation(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	RemAmount = 20,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = RemAmount * PackageSize,
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_incoming_voice() ->
	[{userdata, [{doc, "Authorize incoming voice call"}]}].

authorize_incoming_voice(_Config) ->
	OutPrice = 2,
	OutSize = 60,
	Price1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "originate"}]}]},
	InPrice = 1,
	InSize = 60,
	Price2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	OfferId = add_offer([Price1, Price2], 9),
	ReserveTime = 3600,
	Chars = [{"radiusReserveSessionTime", ReserveTime}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, answer, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	{ok, #bucket{remain_amount = StartingAmount}} = ocs:find_bucket(BId).

authorize_outgoing_voice() ->
	[{userdata, [{doc, "Authorize outgoing voice call"}]}].

authorize_outgoing_voice(_Config) ->
	InPrice = 1,
	InSize = 60,
	Price1 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	OutPrice = 2,
	OutSize = 60,
	Price2 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "originate"}]}]},
	OfferId = add_offer([Price1, Price2], 9),
	ReserveTime = 3600,
	Chars = [{"radiusReserveSessionTime", ReserveTime}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, originate, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	{ok, #bucket{remain_amount = StartingAmount}} = ocs:find_bucket(BId).

authorize_default_voice() ->
	[{userdata, [{doc, "Authorize default outgoing voice call"}]}].

authorize_default_voice(_Config) ->
	OutPrice = 2,
	OutSize = 60,
	Price1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice},
	InPrice = 1,
	InSize = 60,
	Price2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	OfferId = add_offer([Price1, Price2], 9),
	ReserveTime = 3600,
	Chars = [{"radiusReserveSessionTime", ReserveTime}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	{ok, #bucket{remain_amount = StartingAmount}} = ocs:find_bucket(BId).

authorize_data_1() ->
	[{userdata, [{doc, "Athorize data access when price rated on seconds"}]}].

authorize_data_1(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	Password = ocs:generate_password(),
	ServiceId = list_to_binary(ocs:generate_identity()),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefined, SessionId),
	{?SessionTimeout, 60} = lists:keyfind(?SessionTimeout, 1, Attr),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_data_2() ->
	[{userdata, [{doc, "Athorize data access when price rated on octets"}]}].

authorize_data_2(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, _Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefined, SessionId),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_data_with_partial_reservation() ->
	[{userdata, [{doc, "Athorize data access when price
			rated on seconds with partial reservation"}]}].

authorize_data_with_partial_reservation(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = 20,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefibed, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = RemAmount * PackageSize,
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and deny"}]}].

authorize_negative_balance(_Config) ->
	P1 = price(usage, octets, 1000, 1),
	OfferId = add_offer([P1], 9),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	Timestamp = calendar:local_time(),
	B1 = bucket(cents, -100),
	_BId = add_bucket(ProdRef, B1),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	{unauthorized, out_of_credit, _} = ocs_rating:authorize(radius, 12,
			ServiceId, Password, Timestamp, "5551234", originate, [AcctSessionId]).

unauthorize_bad_password() ->
	[{userdata, [{doc, "Unauthorize if the passwrod wrong"}]}].

unauthorize_bad_password(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{unauthorized, bad_password, []} = ocs_rating:authorize(radius,
			ServiceType, ServiceId, "bogus", Timestamp,
			undefined, undefined, SessionId).

unauthorize_out_of_credit() ->
	[{userdata, [{doc, "Unauthorize if insufficient balance"}]}].

unauthorize_out_of_credit(_Config) ->
	PackagePrice = 1,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, Chars),
	Timestamp = calendar:local_time(),
	B1 = bucket(octets, 100),
	_BId = add_bucket(ProdRef, B1),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	ServiceType = 12,
	{unauthorized, out_of_credit, []} = ocs_rating:authorize(radius,
			ServiceType, undefined, ServiceId, Password, Timestamp,
			CallAddress, undefined, SessionId).

reserve_sms() ->
	[{userdata, [{doc, "Reservation for SMS"}]}].

reserve_sms(_Config) ->
	PackagePrice = 1,
	PackageSize = 1,
	P1 = price(usage, messages, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Timestamp = calendar:local_time(),
	RemAmount = rand:uniform(100) * 5,
	B1 = bucket(messages, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NumOfEvents = rand:uniform(10),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{messages, NumOfEvents}], SessionId),
	{ok, #bucket{remain_amount = R, reservations = Rs}} = ocs:find_bucket(BId),
	R = RemAmount - (PackagePrice * NumOfEvents),
	[{_, _, Reserved, _, _, _}] = Rs,
	Reserved = PackagePrice * NumOfEvents.

debit_sms() ->
	[{userdata, [{doc, "Debit for SMS"}]}].

debit_sms(_Config) ->
	PackagePrice = 1,
	PackageSize = 1,
	P1 = price(usage, messages, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Timestamp = calendar:local_time(),
	RemAmount = rand:uniform(100) * 5,
	B1 = bucket(messages, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NumOfEvents = rand:uniform(10),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{messages, NumOfEvents}],
		SessionId),
	{ok, #bucket{remain_amount = R1, reservations = Rs1}} = ocs:find_bucket(BId),
	R1 = RemAmount - (PackagePrice * NumOfEvents),
	[{_, _, Reserved, _, _, _}] = Rs1,
	Reserved = PackagePrice * NumOfEvents,
	{ok, _, Rated} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			final, [{messages, NumOfEvents}], [],
		SessionId),
	R2 = RemAmount - (PackagePrice * NumOfEvents),
	{ok, #bucket{remain_amount = R2, reservations = []}} = ocs:find_bucket(BId),
	[#rated{bucket_type = messages, bucket_value = NumOfEvents}] = Rated.

roaming_table_data() ->
	[{userdata, [{doc, "Data rating for roaming with prefix table"}]}].

roaming_table_data(Config) ->
	PrivDir = ?config(priv_dir, Config),
	RoamingTable = ocs:generate_identity(),
	CsvFile = PrivDir ++ RoamingTable ++ ".csv",
	{ok, File} = file:open(CsvFile, [write]),
	F = fun F(0) ->
			ok;
		F(N) ->
			SN = ocs:generate_identity(),
			ok = file:write(File, SN ++ "," ++ ocs:generate_password() ++
					"," ++ integer_to_list(rand:uniform(100)) ++ "\n"),
			F(N - 1)
	end,
	ok = F(10),
	SNPrefix = ocs:generate_identity(),
	ok = file:write(File, SNPrefix ++ "," ++ ocs:generate_password() ++
			"," ++ integer_to_list(rand:uniform(100)) ++ "\n"),
	ok = ocs_gtt:import(CsvFile),
	PackageSize = 200,
	PackageUnits = octets,
	CharValueUse = [#char_value_use{name = "roamingTable",
		values = [#char_value{value = RoamingTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], "4"),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	Password = ocs:generate_password(),
	ServiceId = list_to_binary(ocs:generate_identity()),
	{ok, _Service1} = ocs:add_service(ServiceId, Password, ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _A, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SNPrefix, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, PackageSize}], SessionId),
	ok = file:close(File).

roaming_table_voice() ->
	[{userdata, [{doc, "Voice rating for roaming with prefix table"}]}].

roaming_table_voice(Config) ->
	PrivDir = ?config(priv_dir, Config),
	RoamingTable = ocs:generate_identity(),
	DestPrefixTable = ocs:generate_identity(),
	DesPrefix = ocs:generate_identity(),
	CsvFile1 = PrivDir ++ RoamingTable ++ ".csv",
	{ok, File1} = file:open(CsvFile1, [write]),
	F1 = fun F1(0) ->
			ok;
		F1(N) ->
			SN = ocs:generate_identity(),
			ok = file:write(File1, SN ++ "," ++ ocs:generate_password() ++ "," ++
					ocs:generate_identity() ++ "\n"),
			F1(N - 1)
	end,
	ok = F1(10),
	SNPrefix = ocs:generate_identity(),
	ok = file:write(File1, SNPrefix ++ "," ++ ocs:generate_password() ++ ","
			++ DesPrefix ++ "\n"),
	ok = ocs:import(CsvFile1, voice),
	CsvFile2 = PrivDir ++ DesPrefix ++ "-" ++ DestPrefixTable ++ ".csv",
	{ok, File2} = file:open(CsvFile2, [write]),
	F2 = fun F2(0) ->
				ok;
			F2(N) ->
				Prefix = ocs:generate_identity(),
				ok = file:write(File2, Prefix ++ "," ++ ocs:generate_password() ++ ","
						++ integer_to_list(rand:uniform(200)) ++ "\n"),
				F2(N - 2)
	end,
	ok = F2(20),
	ok = file:write(File2, DesPrefix ++ "," ++ ocs:generate_password() ++ ","
						++ integer_to_list(rand:uniform(200)) ++ "\n"),
	ok = ocs_gtt:import(CsvFile2),
	PackageSize = 10,
	PackageUnits = seconds,
	CharValueUse = [#char_value_use{name = "roamingTable", values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "destPrefixTariffTable", values = [#char_value{value = DestPrefixTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize, amount = 0,
			char_value_use = CharValueUse},
	OfferId = add_offer([P1], "5"),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	Password = ocs:generate_password(),
	ServiceId = DesPrefix ++ ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password, ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32260,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _A, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SNPrefix, ServiceId, Timestamp,
			ServiceId, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	ok = file:close(File1),
	ok = file:close(File2).

roaming_table_sms() ->
	[{userdata, [{doc, "SMS rating for roaming with prefix table"}]}].

roaming_table_sms(Config) ->
	PrivDir = ?config(priv_dir, Config),
	RoamingTable = ocs:generate_identity(),
	DestPrefixTable = ocs:generate_identity(),
	DesPrefix = ocs:generate_identity(),
	CsvFile1 = PrivDir ++ RoamingTable ++ ".csv",
	{ok, File1} = file:open(CsvFile1, [write]),
	F1 = fun F1(0) ->
			ok;
		F1(N) ->
			SN = ocs:generate_identity(),
			ok = file:write(File1, SN ++ "," ++ ocs:generate_password() ++ "," ++
					ocs:generate_identity() ++ "\n"),
			F1(N - 1)
	end,
	ok = F1(10),
	SNPrefix = ocs:generate_identity(),
	ok = file:write(File1, SNPrefix ++ "," ++ ocs:generate_password() ++ ","
			++ DesPrefix ++ "\n"),
	ok = ocs:import(CsvFile1, voice),
	CsvFile2 = PrivDir ++ DesPrefix ++ "-" ++ DestPrefixTable ++ ".csv",
	{ok, File2} = file:open(CsvFile2, [write]),
	F2 = fun F2(0) ->
				ok;
			F2(N) ->
				Prefix = ocs:generate_identity(),
				ok = file:write(File2, Prefix ++ "," ++ ocs:generate_password() ++ ","
						++ integer_to_list(rand:uniform(200)) ++ "\n"),
				F2(N - 2)
	end,
	ok = F2(20),
	ok = file:write(File2, DesPrefix ++ "," ++ ocs:generate_password() ++ ","
						++ integer_to_list(rand:uniform(200)) ++ "\n"),
	ok = ocs_gtt:import(CsvFile2),
	PackageSize = 10,
	PackageUnits = messages,
	CharValueUse = [#char_value_use{name = "roamingTable", values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "destPrefixTariffTable", values = [#char_value{value = DestPrefixTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize, amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], "10"),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	Password = ocs:generate_password(),
	ServiceId = DesPrefix ++ ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password, ProdRef, Chars),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _A, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SNPrefix, ServiceId, Timestamp,
			ServiceId, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	ok = file:close(File1),
	ok = file:close(File2).

final_empty_mscc() ->
	[{userdata, [{doc, "Rate a final call with an empty MSCC and remove session"}]}].

final_empty_mscc(_Config) ->
	PackagePrice = 10 + rand:uniform(90),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 1000,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 60), undefined,
			undefined, final, [{PackageUnits, PackageSize * 3}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined,
			undefined, final, [], [], SessionId),
	[#bucket{units = cents, reservations = []}] = ocs:get_buckets(ProdRef).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------
%% @hidden
price(Type, Units, Size, Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, units = Units,
			size = Size, amount = Amount}.

%% @hidden
bucket(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
		start_date = erlang:system_time(?MILLISECOND),
		end_date = erlang:system_time(?MILLISECOND) + 2592000000}.

%% @hidden
add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, Spec) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId.

%% @hidden
add_product(OfferId) ->
	add_product(OfferId, []).
add_product(OfferId, Chars) ->
	add_product(OfferId, [], Chars).
add_product(OfferId, ServiceRefs, Chars) ->
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, ServiceRefs, Chars),
	ProdRef.

%% @hidden
add_service(ProdRef) ->
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} =
			ocs:add_service(ServiceId, ocs:generate_password(),
			ProdRef, []),
	ServiceId.

%% @hidden
add_bucket(ProdRef, Bucket) ->
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.

-spec units_cost(UsedUnits, UnitSize, UnitPrice) -> Cost
	when
		UsedUnits :: non_neg_integer(),
		UnitSize :: pos_integer(),
		UnitPrice :: pos_integer(),
		Cost :: pos_integer().
%% @hidden
units_cost(UsedUnits, UnitSize, UnitPrice) ->
	case UsedUnits rem UnitSize of
		0 ->
			(UsedUnits div UnitSize) * UnitPrice;
		_ ->
			((UsedUnits div UnitSize) + 1) * UnitPrice
	end.

