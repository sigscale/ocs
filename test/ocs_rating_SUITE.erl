%%% ocs_rating_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-behaviour(ct_suite).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs.hrl").
-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").

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
	interim_out_of_credit_voice,
	interim_out_of_credit_negative, interim_out_of_credit_negative1,
	final_remove_session, remove_session_after_multiple_interims,
	final_refund_octets, final_refund_seconds,
	final_voice, final_multiple_buckets,
	reserve_data, reserve_voice, interim_voice, time_of_day,
	authorize_voice, authorize_voice_with_partial_reservation,
	authorize_incoming_voice, authorize_outgoing_voice,
	authorize_default_voice, authorize_data_1, authorize_data_2,
	authorize_data_with_partial_reservation, authorize_negative_balance,
	unauthorize_bad_password, unauthorize_bad_password,
	reserve_sms, debit_sms, roaming_table_data, roaming_table_voice,
	roaming_table_sms_ecur, roaming_table_sms_iec, roaming_table_sms_iec_rsu,
	final_empty_mscc, final_empty_mscc_multiple_services,
	initial_invalid_service_type, refund_unused_reservation,
	refund_partially_used_reservation, tariff_prices,
	allowance_bucket].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

initial_exact_fit() ->
	[{userdata, [{doc, "Cents balance exactly equal to reservation price"}]}].

initial_exact_fit(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 500000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	{ok, #bucket{remain_amount = 0, attributes = Attr}} = ocs:find_bucket(BId),
	#{reservations := #{SessionId := #{debit := PackagePrice,
			reserve := 0}}} = Attr.

initial_insufficient() ->
	[{userdata, [{doc, "Insufficient cents balance for initial reservation"}]}].

initial_insufficient(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
			attributes = #{bucket_type := normal}}} = ocs:find_bucket(BId).

initial_insufficient_multisession() ->
	[{userdata, [{doc, "Insufficient cents balance on initial reservation of additional session"}]}].

initial_insufficient_multisession(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Timestamp = calendar:local_time(),
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(radius,
			ServiceType, undefined, undefined, undefined,
			ServiceId, Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId1),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{out_of_credit, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined,
			ServiceId, Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId2).

initial_add_session() ->
	[{userdata, [{doc, "Add a session"}]}].

initial_add_session(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	ServiceType = 2,
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.7.17"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttr = [NasId, NasIp, AcctSessionId, {?ServiceType, ServiceType}],
	{ok, #service{session_attributes = [{_, SessionId}]},
			{PackageUnits, PackageSize}} = ocs_rating:rate(radius,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionAttr),
	{ok, #bucket{attributes = Attr}} = ocs:find_bucket(BId),
	#{reservations := #{SessionId := #{debit := PackagePrice,
			reserve := 0}}} = Attr.

initial_overhead() ->
	[{userdata, [{doc, "Reserved amount greater than requested reservation amount"}]}].

initial_overhead(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = ocs_rest:millionths_in(233),
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
			attributes = #{reservations := CReservation}}} = ocs:find_bucket(BId),
	F = fun(A) when (A rem PackageSize) == 0 ->
				(A div PackageSize) * PackagePrice;
			(A) ->
				(A div PackageSize + 1) * PackagePrice
	end,
	DebitAmount = F(Reservation),
	RemAmount2 = RemAmount1 - DebitAmount,
	0 = Reserved rem PackageSize,
	true = Reserved > Reservation,
	#{SessionId := #{debit := DebitAmount}} = CReservation.

initial_multiple_buckets() ->
	[{userdata, [{doc, "Reservation over multiple cents buckets"}]}].

initial_multiple_buckets(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
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
	#bucket{attributes = #{reservations := #{SessionId := #{debit := 0,
			reserve := Reserved}}}, remain_amount = 0}
			= lists:keyfind(octets, #bucket.units, RatedBuckets).

initial_expire_buckets() ->
	[{userdata, [{doc, "Remove expired buckets"}]}].

initial_expire_buckets(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = ocs_rest:millionths_in(100),
	B1 = bucket(cents, RemAmount),
	B2= B1#bucket{start_date = erlang:system_time(millisecond) -  (2 * 2592000000),
		end_date = erlang:system_time(millisecond) - 2592000000},
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Now = erlang:system_time(millisecond),
	RemAmount1 = rand:uniform(PackagePrice * 10),
	Reservations = #{SessionId1 => #{ts => Now - 3666000,
			debit => rand:uniform(PackagePrice * 3), reserve => 0}},
	ExpiredBucket = #bucket{units = cents, remain_amount = RemAmount1,
			attributes = #{bucket_type => normal, reservations => Reservations},
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	PackageAmount = 100000000 + rand:uniform(2000000),
	P1 = #price{name = "subscription", type = recurring,
			period = monthly, amount = PackageAmount},
	P2 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1, P2], 8),
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
	PackagePrice = rand:uniform(100000090),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
			attributes = #{reservations := R}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := PackagePrice, reserve := 0}} = R.

interim_reserve_within_unit_size() ->
	[{userdata, [{doc, "Reservation amounts less than package size"}]}].

interim_reserve_within_unit_size(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
			attributes = #{reservations := R1}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := PackagePrice}} = R1,
	RemAmount2 = RemAmount1 - F(Reservation1),
	Reservation2 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [], [{PackageUnits, Reservation2}], SessionId),
	{ok, #bucket{remain_amount = RemAmount3,
			attributes = #{reservations := R2}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := Reserved1}} = R2,
	Reserved1 = F(Reservation2),
	RemAmount3 = RemAmount1 - F(Reservation2),
	Reservation3 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 120), undefined, undefined,
			interim, [], [{PackageUnits, Reservation3}], SessionId),
	{ok, #bucket{remain_amount = RemAmount4,
			attributes = #{reservations := R3}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := Reserved2, reserve := 0}} = R3,
	RemAmount4 = RemAmount1 - F(Reservation3),
	Reserved2 = F(Reservation3).

interim_reserve_available() ->
	[{userdata, [{doc, "Reservation amount equal to balance and package size"}]}].

interim_reserve_available(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
			attributes = #{reservations := R}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := PackagePrice, reserve := 0}} = R.

interim_reserve_out_of_credit() ->
	[{userdata, [{doc, "Out of credit on reservation"}]}].

interim_reserve_out_of_credit(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
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
	UnitPrice = rand:uniform(1000000),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	Reservation1 = rand:uniform(UnitSize),
	ServiceType = 2,
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasId = {?NasIdentifier, ocs:generate_password()},
	NasIp = {?NasIpAddress, "192.168.2.125"},
	SessionAttributes1 = [NasIp, NasId, AcctSessionId],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionAttributes1),
	Reservation2 = rand:uniform(UnitSize) + UnitSize,
	{out_of_credit, _, [{_, SessionAttributes2}]} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, initial, [],
			[{PackageUnits, Reservation2}], SessionAttributes1),
	[] = SessionAttributes1 -- SessionAttributes2,
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_reserve_multiple_buckets_available() ->
	[{userdata, [{doc, "Reservation with multiple buckets"}]}].

interim_reserve_multiple_buckets_available(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = rand:uniform(PackagePrice - 1),
	_BId = add_bucket(ProdRef, bucket(cents, RemAmount)),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.4.123"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttributes1 = [NasIp, NasId, AcctSessionId],
	ServiceType = 2,
	Timestamp = calendar:local_time(),
	{ok, _,  _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionAttributes1),
	{ok, #service{session_attributes = [{_, SessionAttributes2}]}} = ocs:find_service(ServiceId),
	[] = SessionAttributes1 -- SessionAttributes2,
	{out_of_credit, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			interim, [{PackageUnits, PackageSize}], [], SessionAttributes1),
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_debit_and_reserve_available() ->
	[{userdata, [{doc, "Debit given usage and check for reservation, sufficient balance exists"}]}].

interim_debit_and_reserve_available(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice * 100,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Reservation = rand:uniform(PackageSize) + PackageSize,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation}], SessionId),
	RemAmount2 = RemAmount1 - (PackagePrice * 2),
	DebitReserve = PackagePrice * 2,
	{ok, #bucket{attributes = #{reservations
					:= #{SessionId := #{debit := DebitReserve}}},
			remain_amount = RemAmount2}} = ocs:find_bucket(BId),
	Debit = PackageSize,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, Reservation}], SessionId),
	RemAmount3 = RemAmount2 - PackagePrice,
	{ok, #bucket{remain_amount = RemAmount3}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient1() ->
	[{userdata, [{doc, "Debit amount less than package size and reservation amount
			less than available balance, insufficient balance exists"}]}].

interim_debit_and_reserve_insufficient1(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	PackagePrice = rand:uniform(100000000),
	PackageSize = 5000000 + rand:uniform(100000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
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
	F2 = fun(#bucket{attributes = #{bucket_type := session}} = B, Acc) ->
				[B | Acc];
			(_, Acc) ->
				Acc
	end,
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
	[#bucket{attributes = #{reservations := _Reservations1}},
			#bucket{attributes = #{reservations := _Reservations2}}]
			= lists:foldl(F2, [], ocs:get_buckets(ProdRef)),
	Debit3 = rand:uniform(PackageSize),
	Debit4 = rand:uniform(PackageSize),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, final, [{PackageUnits, Debit3}],
			undefined, SessionId),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, final, [{PackageUnits, Debit4}],
			undefined, SessionId),
	Total1 = case (Debit1 + Debit3 rem PackageSize) of
		0 ->
			((Debit1 + Debit3) div PackageSize) * PackagePrice;
		_ ->
			(((Debit1 + Debit3) div PackageSize) + 1) * PackagePrice
	end,
	Total2 = case (Debit2 + Debit4 rem PackageSize) of
		0 ->
			((Debit2 + Debit4) div PackageSize) * PackagePrice;
		_ ->
			(((Debit2 + Debit4) div PackageSize) + 1) * PackagePrice
	end,
	RemAmount3 = RemAmount1 - Total1 - Total2,
	{ok, #bucket{remain_amount = RemAmount3}} = ocs:find_bucket(BId1),
	1 = length(ocs:get_buckets(ProdRef)).

interim_out_of_credit_voice() ->
	[{userdata, [{doc, "Voice call out of credit during call"}]}].

interim_out_of_credit_voice(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 60,
	P1 = price(usage, seconds, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice * 2,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32260,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	UsedUnits = UnitSize + rand:uniform(UnitSize),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{seconds, UsedUnits}], [], SessionId),
	RemainAmount = StartingAmount - UnitPrice,
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId).

interim_out_of_credit_negative() ->
	[{userdata, [{doc, "Credit overrun leaves negative balance (radius)"}]}].

interim_out_of_credit_negative(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 1000000 + rand:uniform(9000000),
	P1 = #price{name = "Usage", type = usage, units = octets,
			size = UnitSize, amount = UnitPrice,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "bytes", value = UnitSize}]}]},
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.35.101"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttributes = [NasIp, NasId, AcctSessionId],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			undefined, undefined,
			initial, [], [], SessionAttributes),
	UsedUnits1 = UnitSize + rand:uniform(UnitSize),
	{out_of_credit, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			interim, [{octets, UsedUnits1}], [], SessionAttributes),
	UsedUnits2 = rand:uniform(UnitSize),
	{out_of_credit, _, _, _} = ocs_rating:rate(radius, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			final, [{octets, UsedUnits2}], undefined, SessionAttributes),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	true = RemainAmount < 0.

interim_out_of_credit_negative1() ->
	[{userdata, [{doc, "Credit overrun leaves negative balance (diameter)"}]}].

interim_out_of_credit_negative1(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 5000000 + rand:uniform(9000000),
	P1 = #price{name = "Usage", type = usage, units = octets,
			size = UnitSize, amount = UnitPrice},
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			undefined, undefined,
			initial, [], [], SessionId),
	UsedUnits1 = UnitSize + rand:uniform(UnitSize),
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			interim, [{octets, UsedUnits1}], [], SessionId),
	UsedUnits2 = rand:uniform(UnitSize),
	{out_of_credit, _, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			final, [{octets, UsedUnits2}], undefined, SessionId),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	true = RemainAmount < 0.

final_remove_session() ->
	[{userdata, [{doc, "Final call remove session attributes from subscriber record"}]}].

final_remove_session(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Debit = 100,
	AcctSessionId1 = {?AcctSessionId, "1020303"},
	NasId1 = {?NasIdentifier, "rate1@sigscale"},
	NasIp1 = {?NasIpAddress, "10.0.0.1"},
	SessionId1 = lists:keysort(1, [AcctSessionId1, NasIp1, NasId1]),
	SA1 = {erlang:system_time(millisecond), SessionId1},
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
			undefined, final, [{PackageUnits, Debit}], undefined, [SA2]),
	{ok, #service{session_attributes = [SA1]}} = ocs:find_service(ServiceId).

remove_session_after_multiple_interims() ->
	[{userdata, [{doc, "Remove session after multiple interims that exceed the reserved amount"}]}].

remove_session_after_multiple_interims(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined, undefined,
			undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 2)}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 120),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 3)}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 180),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 3)}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS1 + 240),
			undefined, undefined, final,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 2)}],
			undefined, SessionId),
	[#bucket{units = cents,
			attributes = #{bucket_type := normal}}] = ocs:get_buckets(ProdRef).

final_refund_octets() ->
	[{userdata, [{doc, "Refund unused amount of octets reservation"}]}].

final_refund_octets(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
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
			[{PackageUnits, UsedUnits1}], undefined, SessionId1),
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
			[{PackageUnits, UsedUnits3}], undefined, SessionId2),
	Remain2 = Remain1 - UsedUnits2 - UsedUnits3,
	{ok, #bucket{remain_amount = Remain2}} = ocs:find_bucket(BId).

final_refund_seconds() ->
	[{userdata, [{doc, "Refund unused amount of seconds reservation"}]}].

final_refund_seconds(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 5000000 + rand:uniform(9000000),
	PackageUnits = seconds,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
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
			[{PackageUnits, UsedUnits1}], undefined, SessionId1),
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
			[{PackageUnits, UsedUnits3}], undefined, SessionId2),
	Remain2 = Remain1 - UsedUnits2 - UsedUnits3,
	{ok, #bucket{remain_amount = Remain2}} = ocs:find_bucket(BId).

final_multiple_buckets() ->
	[{userdata, [{doc, "Debit applied to one of several available buckets"}]}].

final_multiple_buckets(_Config) ->
	UnitPrice = rand:uniform(1000000),
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
			[{PackageUnits, UnitSize * 3}], undefined, SessionId),
	NewBalance = (Balance * NumBuckets) - (6 * UnitPrice),
	F2 = fun(#bucket{remain_amount = N}, Acc) -> Acc + N end,
	NewBalance = lists:foldl(F2, 0, ocs:get_buckets(ProdRef)).

final_voice() ->
	[{userdata, [{doc, "Final RADIUS accounting request for voice call"}]}].

final_voice(_Config) ->
	UnitPrice = rand:uniform(1000000),
	UnitSize = 60,
	ReserveTime = rand:uniform(10) * UnitSize,
	P1 = #price{name = "Calls", type = usage,
			units = seconds, size = UnitSize, amount = UnitPrice,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, []),
	ServiceId = add_service(ProdRef),
	StartingAmount = (((ReserveTime * 4) div UnitSize) * UnitPrice)
			+ rand:uniform(UnitPrice),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.5.175"},
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
			undefined, final, [{seconds, UsedSeconds2}], undefined, SessionAttributes),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	TotalUsed = UsedSeconds1 + UsedSeconds2,
	case (TotalUsed rem UnitSize) of
		0 ->
			RemainAmount = StartingAmount - ((TotalUsed div UnitSize) * UnitPrice);
		_N ->
			RemainAmount = StartingAmount - (((TotalUsed div UnitSize) + 1) * UnitPrice)
	end.

reserve_data() ->
	[{userdata, [{doc, "Reservation for data session"}]}].

reserve_data(_Config) ->
	DataAmount = 2000000,
	DataSize = rand:uniform(10000000),
	ReserveMB = rand:uniform(10),
	ReserveOctets = 1000000 * ReserveMB,
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "megabytes", value = ReserveMB}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceAmount = 2000000,
	VoiceSize = 60,
	ReserveTime = 300,
	VoicePrice = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	VoiceOfferId = add_offer([VoicePrice], 9),
	BundleOffer = #offer{name = ocs:generate_identity(),
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, #offer{name = BundleOfferId}} = ocs:add_offer(BundleOffer),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = 2579000000,
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
	DataAmount = rand:uniform(1000000),
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceAmount = rand:uniform(100000),
	VoiceSize = 60,
	Reserve = rand:uniform(4000),
	VoicePrice = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "seconds", value = Reserve}]}]},
	VoiceOfferId = add_offer([VoicePrice], 9),
	BundleOfferId = ocs:generate_password(),
	BundleProduct = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleProduct),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = (Reserve div VoiceSize) * VoiceAmount * 2,
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
	Reserved = case (Reserve rem VoiceSize) of
		0 ->
			Reserve div VoiceSize;
		_ ->
			(Reserve div VoiceSize) + 1
	end,
	Amount = StartingAmount - (Reserved * VoiceAmount).

reserve_incoming_voice() ->
	[{userdata, [{doc, "Reservation for incoming voice call"}]}].

reserve_incoming_voice(_Config) ->
	DataAmount = 2000000,
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceSize = 60,
	VoiceAmountOut = 2000000,
	ReserveTime = 300,
	VoicePrice1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmountOut,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]},
			#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "originate"}]}]},
	VoiceAmountIn = 1,
	VoicePrice2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmountIn,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]},
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
	StartingAmount = 1000000000,
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
	DataAmount = rand:uniform(1000000),
	DataSize = rand:uniform(10000000),
	ReserveOctets = 1000000 * rand:uniform(10),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	VoiceAmount = rand:uniform(100000),
	VoiceSize = 60,
	ReserveTime = VoiceSize * rand:uniform(10),
	VoicePrice = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	VoiceOfferId = add_offer([VoicePrice], 9),
	BundleOfferId = ocs:generate_password(),
	BundleOffer = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleOffer),
	StartingAmount = (ReserveTime div VoiceSize) * VoiceAmount * 2,
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
	ReserveAmount = ((ReserveTime div VoiceSize) * VoiceAmount),
	UsedAmount = case (UsedSeconds rem VoiceSize) of
		0 ->
			(UsedSeconds div VoiceSize) * VoiceAmount;
		_N ->
			((UsedSeconds div VoiceSize) + 1) * VoiceAmount
	end,
	RemainAmount = StartingAmount - ReserveAmount - UsedAmount.

time_of_day() ->
	[{userdata, [{doc, "Time of day price matching"}]}].

time_of_day(_Config) ->
	PeakAmount = 10000000,
	OffPeakAmount = 5000000,
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
	StartingAmount = 1000000000,
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
			undefined, final, [{octets, UsedOctets1}], undefined, SessionId1),
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
			final, [{octets, UsedOctets2}], undefined, SessionId2),
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
	PackagePrice = 1000000,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 60, default = true}]}],
	OfferId = add_offer([P1], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	RemAmount = ocs_rest:millionths_in(100),
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
	[{userdata, [{doc, "Authorize voice call with "
			"session time for available partial reservation amount"}]}].

authorize_voice_with_partial_reservation(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 60,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 300, default = true}]}],
	OfferId = add_offer([P1], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	RemAmount = PackagePrice * rand:uniform(5),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = (RemAmount div PackagePrice) * PackageSize,
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_incoming_voice() ->
	[{userdata, [{doc, "Authorize incoming voice call"}]}].

authorize_incoming_voice(_Config) ->
	OutPrice = 2000000,
	OutSize = 60,
	Price1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "originate"}]}]},
	InPrice = 1000000,
	InSize = 60,
	Price2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	ReserveTime = 3600,
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = ReserveTime, default = true}]}],
	OfferId = add_offer([Price1, Price2], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	StartingAmount = 1000000000,
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
	InPrice = 1000000,
	InSize = 60,
	Price1 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	OutPrice = 2000000,
	OutSize = 60,
	Price2 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "originate"}]}]},
	ReserveTime = 3600,
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = ReserveTime, default = true}]}],
	OfferId = add_offer([Price1, Price2], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	StartingAmount = 1000000000,
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
	OutPrice = 2000000,
	OutSize = 60,
	Price1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice},
	InPrice = 1000000,
	InSize = 60,
	Price2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			min = 1, max = 1, values = [#char_value{default = true,
			value = "answer"}]}]},
	ReserveTime = 3600,
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = ReserveTime, default = true}]}],
	OfferId = add_offer([Price1, Price2], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	StartingAmount = 1000000000,
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
	PackagePrice = 1000000,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 60, default = true}]}],
	OfferId = add_offer([P1], 8, CharValueUse),
	ProdRef = add_product(OfferId),
	Password = ocs:generate_password(),
	ServiceId = list_to_binary(ocs:generate_identity()),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(100),
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
	PackagePrice = 1000000,
	PackageSize = 2,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 60, default = true}]}],
	OfferId = add_offer([P1], 8, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(100),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, _Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefined, SessionId),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_data_with_partial_reservation() ->
	[{userdata, [{doc, "Athorize data access when price "
			"rated on seconds with partial reservation"}]}].

authorize_data_with_partial_reservation(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 60,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 300, default = true}]}],
	OfferId = add_offer([P1], 8, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = PackagePrice * rand:uniform(5),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefibed, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = (RemAmount div PackagePrice) * PackageSize,
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and deny"}]}].

authorize_negative_balance(_Config) ->
	P1 = price(usage, octets, 1000000000, 1),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 60, default = true}]}],
	OfferId = add_offer([P1], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(), CharValueUse,
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	B1 = bucket(cents, -100),
	_BId = add_bucket(ProdRef, B1),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	{unauthorized, out_of_credit, _} = ocs_rating:authorize(radius, 12,
			ServiceId, Password, Timestamp, "5551234", originate, [AcctSessionId]).

unauthorize_bad_password() ->
	[{userdata, [{doc, "Unauthorize if the passwrod wrong"}]}].

unauthorize_bad_password(_Config) ->
	PackagePrice = 1000000,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId, []),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(100),
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
	PackagePrice = 1000000,
	PackageSize = 2,
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, []),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
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
	PackagePrice = rand:uniform(1000000),
	PackageSize = 1,
	P1 = price(usage, messages, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Timestamp = calendar:local_time(),
	RemAmount = rand:uniform(10) + 10,
	B1 = bucket(messages, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NumEvents = rand:uniform(10),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{messages, NumEvents}], SessionId),
	{ok, #bucket{remain_amount = R,
			attributes = #{reservations := Rs}}} = ocs:find_bucket(BId),
	R = RemAmount - NumEvents,
	#{SessionId := #{reserve := Reserved}} = Rs,
	Reserved = NumEvents.

debit_sms() ->
	[{userdata, [{doc, "Debit for SMS"}]}].

debit_sms(_Config) ->
	PackagePrice = rand:uniform(1000000),
	PackageSize = 1,
	P1 = price(usage, messages, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Timestamp = calendar:local_time(),
	RemAmount = rand:uniform(10) + 10,
	B1 = bucket(messages, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	NumEvents = rand:uniform(10),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{messages, NumEvents}],
		SessionId),
	{ok, #bucket{remain_amount = R1,
			attributes = #{reservations := Rs1}}} = ocs:find_bucket(BId),
	R1 = RemAmount - NumEvents,
	{ok, _, Rated} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			final, [{messages, NumEvents}], undefined,
		SessionId),
	R2 = RemAmount - NumEvents,
	{ok, #bucket{remain_amount = R2}} = ocs:find_bucket(BId).

roaming_table_data() ->
	[{userdata, [{doc, "Data rating for roaming with prefix table"}]}].

roaming_table_data(Config) ->
	RoamingTable = ocs:generate_identity(),
	ok = ocs_gtt:new(RoamingTable, [{disc_copies, [node()]}]),
	F = fun F(0) ->
				ok;
			F(N) ->
				SNPrefix = ocs:generate_identity(),
				Description = ocs:generate_password(),
				UnitPrice = rand:uniform(1000000) * 5,
				Value1 = {Description, UnitPrice},
				{ok, #gtt{}} = ocs_gtt:insert(RoamingTable, SNPrefix, Value1),
				F(N - 1)
	end,
	ok = F(100),
	{_Cont, GTTs} = ocs_gtt:list(start, list_to_existing_atom(RoamingTable)),
	#gtt{num = SN, value = Value2} = lists:nth(rand:uniform(length(GTTs)), GTTs),
	PackagePrice = element(2, Value2),
	PackageSize = 5000000 + rand:uniform(5000000),
	PackageUnits = octets,
	CharValueUse = [#char_value_use{name = "roamingTable",
		values = [#char_value{value = RoamingTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			undefined, undefined, initial, [], [], SessionId),
	DebitUnits1 = rand:uniform(PackageSize),
	{ok, #service{}, _Granted} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 30000),
			undefined, undefined, interim, [{PackageUnits, DebitUnits1}], [],
			SessionId),
	DebitUnits2 = rand:uniform(PackageSize),
	{ok, #service{}, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 90000),
			undefined, undefined, final, [{PackageUnits, DebitUnits2}], undefined,
			SessionId),
	DebitTotal = case ((DebitUnits1 + DebitUnits2) rem PackageSize) of
		0 ->
			((DebitUnits1 + DebitUnits2) div PackageSize) * PackagePrice;
		_ ->
			(((DebitUnits1 + DebitUnits2) div PackageSize) + 1) * PackagePrice
	end,
	RemAmount2 = RemAmount1 - DebitTotal,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

roaming_table_voice() ->
	[{userdata, [{doc, "Voice rating for roaming with prefix table"}]}].

roaming_table_voice(Config) ->
	RoamingTable = ocs:generate_identity(),
	ok = ocs_gtt:new(RoamingTable, [{disc_copies, [node()]}]),
	DestinationTable = ocs:generate_identity(),
	TablePrefix1 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix1 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix2 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix2 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix3 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix3 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefixes = [TablePrefix1, TablePrefix2, TablePrefix3],
	F1 = fun F1(0) ->
				ok;
			F1(N) ->
				SNPrefix = ocs:generate_identity(),
				Description1 = ocs:generate_password(),
				TablePrefix4 = lists:nth(rand:uniform(3), TablePrefixes),
				Value1 = {Description1, TablePrefix4},
				{ok, #gtt{}} = ocs_gtt:insert(RoamingTable, SNPrefix, Value1),
				F1(N - 1)
	end,
	ok = F1(100),
	{_, GTTs1} = ocs_gtt:list(start, list_to_existing_atom(RoamingTable)),
	#gtt{num = SN, value = Value2} = lists:nth(rand:uniform(length(GTTs1)), GTTs1),
	TablePrefix5 = element(2, Value2),
	RatingTable = list_to_existing_atom(TablePrefix5 ++ "-" ++ DestinationTable),
	F2 = fun F2(0) ->
				ok;
			F2(N) ->
				DestPrefix = ocs:generate_identity(),
				Description2 = ocs:generate_password(),
				UnitPrice = rand:uniform(1000000),
				Value3 = {Description2, UnitPrice},
				{ok, #gtt{}} = ocs_gtt:insert(RatingTable, DestPrefix, Value3),
				F2(N - 2)
	end,
	ok = F2(20),
	{_, GTTs2} = ocs_gtt:list(start, RatingTable),
	#gtt{num = Address, value = Value4} = lists:nth(rand:uniform(length(GTTs2)), GTTs2),
	PackagePrice = element(2, Value4),
	PackageSize = 60 + rand:uniform(60),
	PackageUnits = seconds,
	CharValueUse = [#char_value_use{name = "roamingTable",
					values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = DestinationTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32260,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	DebitUnits1 = rand:uniform(PackageSize),
	{ok, #service{}, _Granted} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 30000),
			Address, undefined, interim, [{PackageUnits, DebitUnits1}], [],
			SessionId),
	DebitUnits2 = rand:uniform(PackageSize),
	{ok, #service{}, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 90000),
			Address, undefined, final, [{PackageUnits, DebitUnits2}], undefined,
			SessionId),
	DebitTotal = case ((DebitUnits1 + DebitUnits2) rem PackageSize) of
		0 ->
			((DebitUnits1 + DebitUnits2) div PackageSize) * PackagePrice;
		_ ->
			(((DebitUnits1 + DebitUnits2) div PackageSize) + 1) * PackagePrice
	end,
	RemAmount2 = RemAmount1 - DebitTotal,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

roaming_table_sms_ecur() ->
	[{userdata, [{doc, "SMS ECUR rating for roaming with prefix table"}]}].

roaming_table_sms_ecur(Config) ->
	RoamingTable = ocs:generate_identity(),
	ok = ocs_gtt:new(RoamingTable, [{disc_copies, [node()]}]),
	DestinationTable = ocs:generate_identity(),
	TablePrefix1 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix1 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix2 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix2 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix3 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix3 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefixes = [TablePrefix1, TablePrefix2, TablePrefix3],
	F1 = fun F1(0) ->
				ok;
			F1(N) ->
				SNPrefix = ocs:generate_identity(),
				Description1 = ocs:generate_password(),
				TablePrefix4 = lists:nth(rand:uniform(3), TablePrefixes),
				Value1 = {Description1, TablePrefix4},
				{ok, #gtt{}} = ocs_gtt:insert(RoamingTable, SNPrefix, Value1),
				F1(N - 1)
	end,
	ok = F1(10),
	{_, GTTs1} = ocs_gtt:list(start, list_to_existing_atom(RoamingTable)),
	#gtt{num = SN, value = Value2} = lists:nth(rand:uniform(length(GTTs1)), GTTs1),
	TablePrefix5 = element(2, Value2),
	RatingTable = list_to_existing_atom(TablePrefix5 ++ "-" ++ DestinationTable),
	F2 = fun F2(0) ->
				ok;
			F2(N) ->
				DestPrefix = ocs:generate_identity(),
				Description2 = ocs:generate_password(),
				UnitPrice = rand:uniform(1000000),
				Value3 = {Description2, UnitPrice},
				{ok, #gtt{}} = ocs_gtt:insert(RatingTable, DestPrefix, Value3),
				F2(N - 2)
	end,
	ok = F2(20),
	{_, GTTs2} = ocs_gtt:list(start, RatingTable),
	#gtt{num = Address, value = Value4} = lists:nth(rand:uniform(length(GTTs2)), GTTs2),
	PackagePrice = element(2, Value4),
	PackageSize = rand:uniform(5),
	PackageUnits = messages,
	CharValueUse = [#char_value_use{name = "roamingTable",
					values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = DestinationTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], 10),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	DebitUnits1 = rand:uniform(PackageSize),
	{ok, #service{}, _Granted} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 30000),
			Address, undefined, interim, [{PackageUnits, DebitUnits1}], [],
			SessionId),
	DebitUnits2 = rand:uniform(PackageSize),
	{ok, #service{}, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 90000),
			Address, undefined, final, [{PackageUnits, DebitUnits2}], undefined,
			SessionId),
	DebitTotal = case ((DebitUnits1 + DebitUnits2) rem PackageSize) of
		0 ->
			((DebitUnits1 + DebitUnits2) div PackageSize) * PackagePrice;
		_ ->
			(((DebitUnits1 + DebitUnits2) div PackageSize) + 1) * PackagePrice
	end,
	RemAmount2 = RemAmount1 - DebitTotal,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

roaming_table_sms_iec() ->
	[{userdata, [{doc, "SMS IEC rating for roaming with prefix table"}]}].

roaming_table_sms_iec(Config) ->
	RoamingTable = ocs:generate_identity(),
	ok = ocs_gtt:new(RoamingTable, [{disc_copies, [node()]}]),
	DestinationTable = ocs:generate_identity(),
	TablePrefix1 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix1 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix2 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix2 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix3 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix3 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefixes = [TablePrefix1, TablePrefix2, TablePrefix3],
	F1 = fun F1(0) ->
				ok;
			F1(N) ->
				SNPrefix = ocs:generate_identity(),
				Description1 = ocs:generate_password(),
				TablePrefix4 = lists:nth(rand:uniform(3), TablePrefixes),
				Value1 = {Description1, TablePrefix4},
				{ok, #gtt{}} = ocs_gtt:insert(RoamingTable, SNPrefix, Value1),
				F1(N - 1)
	end,
	ok = F1(10),
	{_, GTTs1} = ocs_gtt:list(start, list_to_existing_atom(RoamingTable)),
	#gtt{num = SN, value = Value2} = lists:nth(rand:uniform(length(GTTs1)), GTTs1),
	TablePrefix5 = element(2, Value2),
	RatingTable = list_to_existing_atom(TablePrefix5 ++ "-" ++ DestinationTable),
	F2 = fun F2(0) ->
				ok;
			F2(N) ->
				DestPrefix = ocs:generate_identity(),
				Description2 = ocs:generate_password(),
				UnitPrice = rand:uniform(1000000),
				Value3 = {Description2, UnitPrice},
				{ok, #gtt{}} = ocs_gtt:insert(RatingTable, DestPrefix, Value3),
				F2(N - 2)
	end,
	ok = F2(20),
	{_, GTTs2} = ocs_gtt:list(start, RatingTable),
	#gtt{num = Address, value = Value4} = lists:nth(rand:uniform(length(GTTs2)), GTTs2),
	PackagePrice = element(2, Value4),
	PackageSize = rand:uniform(4),
	PackageUnits = messages,
	CharValueUse = [#char_value_use{name = "roamingTable",
					values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = DestinationTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], 10),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	DebitUnits = {PackageUnits, PackageSize},
	{ok, #service{}, DebitUnits, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, event, [], [], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

roaming_table_sms_iec_rsu() ->
	[{userdata, [{doc, "SMS IEC RSU rating for roaming with prefix table"}]}].

roaming_table_sms_iec_rsu(Config) ->
	RoamingTable = ocs:generate_identity(),
	ok = ocs_gtt:new(RoamingTable, [{disc_copies, [node()]}]),
	DestinationTable = ocs:generate_identity(),
	TablePrefix1 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix1 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix2 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix2 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefix3 = ocs:generate_identity(),
	ok = ocs_gtt:new(TablePrefix3 ++ "-" ++ DestinationTable,
			[{disc_copies, [node()]}]),
	TablePrefixes = [TablePrefix1, TablePrefix2, TablePrefix3],
	F1 = fun F1(0) ->
				ok;
			F1(N) ->
				SNPrefix = ocs:generate_identity(),
				Description1 = ocs:generate_password(),
				TablePrefix4 = lists:nth(rand:uniform(3), TablePrefixes),
				Value1 = {Description1, TablePrefix4},
				{ok, #gtt{}} = ocs_gtt:insert(RoamingTable, SNPrefix, Value1),
				F1(N - 1)
	end,
	ok = F1(10),
	{_, GTTs1} = ocs_gtt:list(start, list_to_existing_atom(RoamingTable)),
	#gtt{num = SN, value = Value2} = lists:nth(rand:uniform(length(GTTs1)), GTTs1),
	TablePrefix5 = element(2, Value2),
	RatingTable = list_to_existing_atom(TablePrefix5 ++ "-" ++ DestinationTable),
	F2 = fun F2(0) ->
				ok;
			F2(N) ->
				DestPrefix = ocs:generate_identity(),
				Description2 = ocs:generate_password(),
				UnitPrice = rand:uniform(1000000),
				Value3 = {Description2, UnitPrice},
				{ok, #gtt{}} = ocs_gtt:insert(RatingTable, DestPrefix, Value3),
				F2(N - 2)
	end,
	ok = F2(20),
	{_, GTTs2} = ocs_gtt:list(start, RatingTable),
	#gtt{num = Address, value = Value4} = lists:nth(rand:uniform(length(GTTs2)), GTTs2),
	PackagePrice = element(2, Value4),
	PackageSize = 1,
	PackageUnits = messages,
	CharValueUse = [#char_value_use{name = "roamingTable",
					values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = DestinationTable}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], 10),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service(ServiceId, undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	DebitUnits = rand:uniform(4),
	DebitAmount = {PackageUnits, DebitUnits},
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, #service{}, DebitAmount, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, event, [], [DebitAmount],
			SessionId),
	RemAmount2 = RemAmount1 - (DebitUnits * PackagePrice),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

final_empty_mscc() ->
	[{userdata, [{doc, "Rate a final call with an empty MSCC and check whether sessions are removed"}]}].

final_empty_mscc(_Config) ->
	Alteration = #alteration{name = "Allowance", units = octets,
			size = 150000000, amount = 0, type = recurring, period = monthly},
	P1 = #price{name = "Subscription", type = recurring, amount = 19900000000,
			period = monthly},
	P2 = #price{name = "Overage", type = usage, units = octets,
			size = 100000000, amount = 990000000, alteration = Alteration},
	OfferId = add_offer([P1, P2], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Adjustment = #adjustment{amount = 19900000000, product = ProdRef, units = cents},
	ok = ocs:adjustment(Adjustment),
	RemAmount = ocs_rest:millionths_in(20000),
	B1 = #bucket{units = cents, remain_amount = RemAmount,
			start_date = erlang:system_time(millisecond),
			attributes = #{bucket_type => normal}},
	_BId = add_bucket(ProdRef, B1),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			6400, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 60), undefined,
			undefined, interim, [{octets, 300000000}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, calendar:gregorian_seconds_to_datetime(TS1 + 120), undefined,
			undefined, interim, [{octets, 15000000}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId, Timestamp, undefined,
			undefined, final, [], undefined, SessionId),
	BucketList = ocs:get_buckets(ProdRef),
	F1 = fun(#bucket{attributes = #{reservations := Reservation}})
					when is_map(Reservation) ->
				I1 = maps:iterator(Reservation),
				F2 = fun({Key, _Value, _I2}) when Key == SessionId ->
							false;
						({_Key, _Value, I2}) ->
							maps:next(I2);
						(none) ->
							true
				end,
				F2(maps:next(I1));
			(#bucket{}) ->
				true
	end,
	true = lists:all(F1, BucketList).

final_empty_mscc_multiple_services() ->
	[{userdata, [{doc, "Rate final with an empty MSCC after rating interims with different services and check whether sessions are removed"}]}].

final_empty_mscc_multiple_services(_Config) ->
	Alteration = #alteration{name = "Allowance", units = octets,
			size = 150000000, amount = 0, type = recurring, period = monthly},
	P1 = #price{name = "Subscription", type = recurring, amount = 19900000000,
			period = monthly},
	P2 = #price{name = "Overage", type = usage, units = octets,
			size = 100000000, amount = 990000000, alteration = Alteration},
	OfferId = add_offer([P1, P2], 8),
	ProdRef = add_product(OfferId),
	ServiceId1 = add_service(ProdRef),
	Adjustment = #adjustment{amount = 19900000000, product = ProdRef, units = cents},
	ok = ocs:adjustment(Adjustment),
	RemAmount = ocs_rest:millionths_in(20000),
	B1 = #bucket{units = cents, remain_amount = RemAmount,
			start_date = erlang:system_time(millisecond),
			attributes = #{bucket_type => normal}},
	_BId = add_bucket(ProdRef, B1),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	Timestamp1 = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId1, Timestamp1, undefined, undefined,
			initial, [], [], SessionId1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			32, undefined, undefined, ServiceId1, calendar:gregorian_seconds_to_datetime(TS1 + 60), undefined,
			undefined, interim, [{octets, 300000000}], [], SessionId1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			65, undefined, undefined, ServiceId1, calendar:gregorian_seconds_to_datetime(TS1 + 120), undefined,
			undefined, interim, [{octets, 15000000}], [], SessionId1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId1, Timestamp1, undefined,
			undefined, final, [], undefined, SessionId1),
	BucketList = ocs:get_buckets(ProdRef),
	F1 = fun(#bucket{attributes = #{reservations := Reservation}})
					when is_map(Reservation) ->
				I1 = maps:iterator(Reservation),
				F2 = fun({Key, _Value, _I2}) when Key == SessionId1 ->
							false;
						({_Key, _Value, I2}) ->
							maps:next(I2);
						(none) ->
							true
				end,
				F2(maps:next(I1));
			(#bucket{}) ->
				true
	end,
	true = lists:all(F1, BucketList).

initial_invalid_service_type() ->
	[{userdata, [{doc, "Check the validity of a service type during rating"}]}].

initial_invalid_service_type(_Config) ->
	ServiceId = ocs:generate_identity(),
	UnitSize = 1000000 + rand:uniform(10000),
	Amount = rand:uniform(100000000),
	P1 = price(usage, octets, UnitSize, Amount),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	{ok, #service{}} = ocs:add_service(ServiceId, undefined, ProdRef, []),
	Balance = 1000000 + rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	Timestamp = calendar:local_time(),
	ServiceType = undefined,
	{error, invalid_service_type} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [], SId).

refund_unused_reservation() ->
	[{userdata, [{doc, "Refund unused reservation"}]}].

refund_unused_reservation(_Config) ->
	PackagePrice = 10000000,
	PackageSize = 60,
	PackageUnits = seconds,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemainAmount = ocs_rest:millionths_in(100),
	B = bucket(cents, RemainAmount),
	BId = add_bucket(ProdRef, B),
	ServiceType = 32260,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, _, [#rated{} | _]} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined, final, [], undefined, SessionId),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId).

refund_partially_used_reservation() ->
	[{userdata, [{doc, "Refund partially used reservation"}]}].

refund_partially_used_reservation(_Config) ->
	PackagePrice = 10000000,
	PackageSize = 60,
	PackageUnits = seconds,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemainAmount1 = ocs_rest:millionths_in(5),
	B1 = bucket(cents, RemainAmount1),
	BId1 = add_bucket(ProdRef, B1),
	RemainAmount2 = ocs_rest:millionths_in(20),
	B2 = bucket(cents, RemainAmount2),
	BId2 = add_bucket(ProdRef, B2),
	ServiceType = 32260,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, #service{}, _Reservation} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, _, [#rated{} | _]} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			final, [{PackageUnits, 50}], undefined, SessionId),
	{error, not_found} = ocs:find_bucket(BId1),
	RemainAmount3 = RemainAmount1 + RemainAmount2 - PackagePrice,
	{ok, #bucket{remain_amount = RemainAmount3}} = ocs:find_bucket(BId2).

tariff_prices() ->
	[{userdata, [{doc, "Price discrimination by tariff table match"}]}].

tariff_prices(_Config) ->
	TariffTable1 = ocs:generate_identity(),
	TariffTable2 = ocs:generate_identity(),
	TariffTable3 = ocs:generate_identity(),
	F = fun F(0, _, Items) ->
				Items;
			F(N, P, Items) ->
				Rate = rand:uniform(1000000),
				Description = ocs:generate_identity(),
				Value = {Description, Rate},
				Number = [P | ocs:generate_identity()],
				Item = {Number, Value},
				F(N - 1, P, [Item | Items])
	end,
	ok = ocs_gtt:new(TariffTable1, [], F(100, $1, [])),
	ok = ocs_gtt:new(TariffTable2, [], F(100, $2, [])),
	ok = ocs_gtt:new(TariffTable3, [], F(100, $3, [])),
	CharValueUse1 = [#char_value_use{name = "destPrefixTariffTable",
			values = [#char_value{value = TariffTable1}]}],
	CharValueUse2 = [#char_value_use{name = "destPrefixTariffTable",
			values = [#char_value{value = TariffTable2}]}],
	CharValueUse3 = [#char_value_use{name = "destPrefixTariffTable",
			values = [#char_value{value = TariffTable3}]}],
	P1 = #price{type = tariff, units = seconds, size = 60,
			char_value_use = CharValueUse1},
	P2 = #price{type = tariff, units = seconds, size = 120,
			char_value_use = CharValueUse2},
	P3 = #price{type = tariff, units = seconds, size = 300,
			char_value_use = CharValueUse3},
	OfferId = add_offer([P1, P2, P3], 9),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service} = ocs:add_service(ServiceId, undefined, ProdRef),
	RemAmount1 = ocs_rest:millionths_in(1000),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	ServiceType = 32260,
	UnTariffedDestination =  [$9 | ocs:generate_identity()],
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{error,table_lookup_failed} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			calendar:local_time(), UnTariffedDestination, undefined,
			initial, [], [], SessionId1),
	{_Cont, Items} = ocs_gtt:list(start, list_to_existing_atom(TariffTable3)),
	#gtt{num = Destination, value = Value} = lists:last(Items), 
	Rate = element(2, Value),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, {seconds, 300}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			calendar:local_time(), Destination, undefined,
			initial, [], [], SessionId2),
	RemainAmount2 = RemAmount1 - Rate,
	{ok, #bucket{remain_amount = RemainAmount2}} = ocs:find_bucket(BId).

allowance_bucket() ->
	[{userdata, [{doc, "Allowance bucket, created by Alteration, pinned to a Price"}]}].

allowance_bucket(_Config) ->
	{ok, UnitSize} = application:get_env(ocs, min_reserve_octets),
	AllowanceSize = (5 * UnitSize) + (rand:uniform(5) * UnitSize),
	PeakAmount = rand:uniform(1000000),
	OffPeakAmount = rand:uniform(1000000),
	Allowance = #alteration{name = "Allowance", units = octets,
			size = AllowanceSize, amount = 0, type = recurring,
			period = monthly},
	P1 = #price{name = "Peak", type = usage,
			units = octets, size = UnitSize, amount = PeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			min = 1, max = 1, values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 480, units = "minutes"},
			upper = #quantity{amount = 1380, units = "minutes"}}}]}]},
	P2 = #price{name = "OffPeak", type = usage,
			units = octets, size = UnitSize, amount = OffPeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			min = 1, max = 1, values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 1380, units = "minutes"},
			upper = #quantity{amount = 480, units = "minutes"}}}]},
			#char_value_use{name = "fixedPriceBucket",
			values = [#char_value{value = true}]}],
			alteration = Allowance},
	OfferId = add_offer([P1, P2], 8),
	ServiceId = ocs:generate_identity(),
	{ok, #service{name = ServiceRef}} = ocs:add_service(ServiceId,
			undefined, undefined),
	{ok, #product{id = ProdRef, balance = [BId1]}}
			= ocs:add_product(OfferId, [ServiceRef]),
	RemAmount1 = (10 *  UnitSize) + rand:uniform(UnitSize),
	BId2 = add_bucket(ProdRef, bucket(octets, RemAmount1)),
	ServiceType = 32251,
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			{date(), {20, 0, 0}}, undefined, undefined,
			initial, [], [], SessionId1),
	DayAmount1 = rand:uniform(UnitSize) + UnitSize,
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			{date(), {21, 0, 0}}, undefined, undefined,
			interim, [{octets, DayAmount1}], [], SessionId1),
	DayAmount2 = rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			{date(), {22, 0, 0}}, undefined, undefined,
			final, [{octets, DayAmount2}], undefined, SessionId1),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			{date(), {2, 0, 0}}, undefined, undefined,
			initial, [], [], SessionId2),
	NightAmount1 = rand:uniform(UnitSize) + UnitSize,
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			{date(), {3, 0, 0}}, undefined, undefined,
			interim, [{octets, NightAmount1}], [], SessionId2),
	NightAmount2 = rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, ServiceId,
			{date(), {4, 0, 0}}, undefined, undefined,
			final, [{octets, NightAmount2}], undefined, SessionId2),
	RemainAmount2 = RemAmount1 - DayAmount1 - DayAmount2,
	{ok, #bucket{remain_amount = RemainAmount2}} = ocs:find_bucket(BId2),
	RemainAmount3 = AllowanceSize - NightAmount1 - NightAmount2,
	{ok, #bucket{remain_amount = RemainAmount3}} = ocs:find_bucket(BId1).

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
	bucket(Units, RA, []).
%% @hidden
bucket(Units, RA, PriceName) ->
	#bucket{units = Units, remain_amount = RA, price = PriceName,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000,
			attributes = #{bucket_type => normal}}.

%% @hidden
add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, Spec) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId.
%% @hidden
add_offer(Prices, Spec, CharValueUse) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec), CharValueUse);
add_offer(Prices, Spec, CharValueUse) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec, char_value_use = CharValueUse},
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

