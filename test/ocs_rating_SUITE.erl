%%% ocs_rating_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2024 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

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
	radius_reserve_data, radius_reserve_voice, radius_interim_voice, time_of_day,
	authorize_voice, authorize_voice_with_partial_reservation,
	authorize_incoming_voice, authorize_outgoing_voice,
	authorize_default_voice, authorize_data_1, authorize_data_2,
	authorize_data_with_partial_reservation, authorize_negative_balance,
	unauthorize_bad_password, unauthorize_bad_password,
	reserve_sms, debit_sms, iec_out_of_credit,
	roaming_table_data, roaming_table_voice,
	roaming_table_sms_ecur, roaming_table_sms_iec, roaming_table_sms_iec_rsu,
	final_empty_mscc, final_empty_mscc_multiple_services,
	initial_invalid_service_type, refund_unused_reservation,
	refund_partially_used_reservation, tariff_prices,
	allowance_bucket, tariff_bucket_voice,
	tariff_bucket_iec, tariff_bucket_ecur,
	scur_5g_data_initial].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

initial_exact_fit() ->
	[{userdata, [{doc, "Cents balance exactly equal to reservation price"}]}].

initial_exact_fit(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	SessionId = session_id(Protocol),
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = 0, attributes = Attr}} = ocs:find_bucket(BId),
	#{reservations := #{SessionId := #{debit := PackagePrice,
			reserve := 0}}} = Attr.

initial_insufficient() ->
	[{userdata, [{doc, "Insufficient cents balance for initial reservation"}]}].

initial_insufficient(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice - 1,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{units = cents, remain_amount = RemAmount,
			attributes = #{bucket_type := normal}}} = ocs:find_bucket(BId).

initial_insufficient_multisession() ->
	[{userdata, [{doc, "Insufficient cents balance on initial reservation of additional session"}]}].

initial_insufficient_multisession(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId1 = session_id(Protocol),
	Timestamp = calendar:local_time(),
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined,
			[ServiceId], Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId1),
	SessionId2 = session_id(Protocol),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined,
			[ServiceId], Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId2).

initial_add_session() ->
	[{userdata, [{doc, "Add a session"}]}].

initial_add_session(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.7.17"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttr = [NasId, NasIp, AcctSessionId, {?ServiceType, ServiceType}],
	{ok, #service{session_attributes = [{_, SessionId}]},
			{PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionAttr),
	ok = mnesia:sync_log(),
	{ok, #bucket{attributes = Attr}} = ocs:find_bucket(BId),
	#{reservations := #{SessionId := #{debit := PackagePrice,
			reserve := 0}}} = Attr.

initial_overhead() ->
	[{userdata, [{doc, "Reserved amount greater than requested reservation amount"}]}].

initial_overhead(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = ocs_rest:millionths_in(PackagePrice * 10),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	Reservation = MinReserve - rand:uniform(MinReserve - 1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	{ok, #service{}, {PackageUnits, Reserved}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId], Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, Reservation}],
			SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2,
			attributes = #{reservations := CReservation}}} = ocs:find_bucket(BId),
	F = fun(A) when (A rem PackageSize) == 0 ->
				(A div PackageSize) * PackagePrice;
			(A) ->
				(A div PackageSize + 1) * PackagePrice
	end,
	DebitAmount = F(Reserved),
	RemAmount2 = RemAmount1 - DebitAmount,
	0 = Reserved rem PackageSize,
	true = Reserved > Reservation,
	#{SessionId := #{debit := DebitAmount}} = CReservation.

initial_multiple_buckets() ->
	[{userdata, [{doc, "Reservation over multiple cents buckets"}]}].

initial_multiple_buckets(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Reservation = (UnitSize div 2) + rand:uniform(UnitSize div 2),
	SessionId = session_id(Protocol),
	{ok, #service{}, {PackageUnits,  Reserved}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId], Timestamp,
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
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	ok = mnesia:sync_log(),
	{ok, #product{balance = []}} = ocs:find_product(ProdRef),
	{error, not_found} = ocs:find_bucket(BId).

initial_ignore_expired_buckets() ->
	[{userdata, [{doc, "Ignore expired buckets with sessions"}]}].

initial_ignore_expired_buckets(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Protocol = protocol(),
	SessionId1 = session_id(Protocol),
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
	SessionId2 = session_id(Protocol),
	ServiceType = service_type(Protocol, data),
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId], Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, PackageSize}], SessionId2),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount1}} = ocs:find_bucket(BId1).

initial_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and ignore"}]}].

initial_negative_balance(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	PackageAmount = 100000000 + rand:uniform(2000000),
	P1 = #price{name = "subscription", type = recurring,
			period = monthly, amount = PackageAmount},
	P2 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1, P2], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	Timestamp = calendar:local_time(),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp,
			undefined, undefined, initial, [], [], SessionId).

interim_reserve() ->
	[{userdata, [{doc, "Reservation amount equal to package size"}]}].

interim_reserve(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(100000090),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	{ok, _Service, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, #service{}, _Reservation} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, PackageSize}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2,
			attributes = #{reservations := R}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := PackagePrice, reserve := 0}} = R.

interim_reserve_within_unit_size() ->
	[{userdata, [{doc, "Reservation amounts less than package size"}]}].

interim_reserve_within_unit_size(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reservation1 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp, undefined,
			undefined, initial, [], [{PackageUnits, Reservation1}], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2,
			attributes = #{reservations := R1}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := PackagePrice}} = R1,
	RemAmount2 = RemAmount1 - F(Reservation1),
	Reservation2 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [], [{PackageUnits, Reservation2}], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount3,
			attributes = #{reservations := R2}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := Reserved1}} = R2,
	Reserved1 = F(Reservation2),
	RemAmount3 = RemAmount1 - F(Reservation2),
	Reservation3 = rand:uniform(PackageSize - 1),
	{ok, #service{}, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 120), undefined, undefined,
			interim, [], [{PackageUnits, Reservation3}], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount4,
			attributes = #{reservations := R3}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := Reserved2, reserve := 0}} = R3,
	RemAmount4 = RemAmount1 - F(Reservation3),
	Reserved2 = F(Reservation3).

interim_reserve_available() ->
	[{userdata, [{doc, "Reservation amount equal to balance and package size"}]}].

interim_reserve_available(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _Service2, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId], Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [],
			[{PackageUnits, PackageSize}], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = 0,
			attributes = #{reservations := R}}} = ocs:find_bucket(BId),
	#{SessionId := #{debit := PackagePrice, reserve := 0}} = R.

interim_reserve_out_of_credit() ->
	[{userdata, [{doc, "Out of credit on reservation"}]}].

interim_reserve_out_of_credit(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	ReserveSize = rand:uniform(UnitSize),
	{ok, _Service2, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, ReserveSize}], SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, UnitSize * 2}], SessionId),
	Remain = StartingAmount - UnitPrice,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Remain}} = ocs:find_bucket(BId).

interim_reserve_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_reserve_remove_session(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionAttributes1= session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionAttributes1),
	Reservation2 = rand:uniform(UnitSize) + UnitSize,
	{out_of_credit, _, [{_, SessionAttributes2}]} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, initial, [],
			[{PackageUnits, Reservation2}], SessionAttributes1),
	[] = SessionAttributes1 -- SessionAttributes2,
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_reserve_multiple_buckets_available() ->
	[{userdata, [{doc, "Reservation with multiple buckets"}]}].

interim_reserve_multiple_buckets_available(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Reservation1 = rand:uniform(PackageSize),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	Reservation2 = rand:uniform(PackageSize),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, Reservation2}],
			SessionId).

interim_reserve_multiple_buckets_out_of_credit() ->
	[{userdata, [{doc, "Out of credit with multiple cents buckets"}]}].

interim_reserve_multiple_buckets_out_of_credit(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, PackageSize}],
			[], SessionId).

interim_debit_exact_balance() ->
	[{userdata, [{doc, "Debit amount equal to package size"}]}].

interim_debit_exact_balance(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice * 2),
	_BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined,
			undefined, interim, [{PackageUnits, PackageSize}], [], SessionId).

interim_debit_under_unit_size() ->
	[{userdata, [{doc, "Debit amount less than package size"}]}].

interim_debit_under_unit_size(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = (2 * PackagePrice) + 1,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			interim, [{PackageUnits, PackageSize div 3}], [{PackageUnits, 0}], SessionId).

interim_debit_out_of_credit() ->
	[{userdata, [{doc, "Insufficient amount to debit"}]}].

interim_debit_out_of_credit(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	_BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _Service1, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId], Timestamp,
			undefined, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + 1}], [{PackageUnits, 0}], SessionId).

interim_debit_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_debit_remove_session(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice + rand:uniform(PackagePrice),
	_BId = add_bucket(ProdRef, bucket(cents, RemAmount)),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionAttributes1 = session_id(Protocol),
	Timestamp = calendar:local_time(),
	{ok, _,  _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionAttributes1),
	{ok, #service{session_attributes = [{_, SessionAttributes2}]}} = ocs:find_service(ServiceId),
	[] = SessionAttributes1 -- SessionAttributes2,
	DebitAmounts = [{PackageUnits, (PackageSize * 2) + rand:uniform(PackageSize)}],
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			interim, [{PackageUnits, PackageSize}], [], SessionAttributes1),
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_debit_and_reserve_available() ->
	[{userdata, [{doc, "Debit given usage and check for reservation, sufficient balance exists"}]}].

interim_debit_and_reserve_available(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice * 100,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Reservation = rand:uniform(PackageSize) + PackageSize,
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation}], SessionId),
	RemAmount2 = RemAmount1 - (PackagePrice * 2),
	DebitReserve = PackagePrice * 2,
	ok = mnesia:sync_log(),
	{ok, #bucket{attributes = #{reservations
					:= #{SessionId := #{debit := DebitReserve}}},
			remain_amount = RemAmount2}} = ocs:find_bucket(BId),
	Debit = PackageSize,
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, Reservation}], SessionId),
	RemAmount3 = RemAmount2 - PackagePrice,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount3}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient1() ->
	[{userdata, [{doc, "Debit amount less than package size and reservation amount
			less than available balance, insufficient balance exists"}]}].

interim_debit_and_reserve_insufficient1(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice + rand:uniform(PackagePrice - 1),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Reservation1 = rand:uniform(PackageSize),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	Debit = rand:uniform(PackageSize - 1),
	Reservation2 = PackageSize * 2,
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, Reservation2}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient2() ->
	[{userdata, [{doc, "Debit amount equal to unit size and
			reservation amount greater than available balance"}]}].

interim_debit_and_reserve_insufficient2(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = (2 * PackagePrice) + rand:uniform(PackagePrice - 1),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Reservation1 = rand:uniform(PackageSize),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	Reservation2 = PackageSize + rand:uniform(PackageSize),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, PackageSize}],
			[{PackageUnits, Reservation2}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient3() ->
	[{userdata, [{doc, "Suffient balance for debit but not reservation"}]}].

interim_debit_and_reserve_insufficient3(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice + rand:uniform(PackagePrice - 1),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Reservation1 = rand:uniform(PackageSize),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reservation1}], SessionId),
	UsedUnits = rand:uniform(Reservation1),
	Reservation2 = PackageSize,
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{PackageUnits, UsedUnits}],
			[{PackageUnits, Reservation2}], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId).

interim_debit_and_reserve_insufficient4() ->
	[{userdata, [{doc, "Insuffient amount for debit and reservation"}]}].

interim_debit_and_reserve_insufficient4(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = PackagePrice + rand:uniform(PackagePrice - 1),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Debit = PackagePrice * 2,
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [{PackageUnits, Debit}],
			[{PackageUnits, PackageSize}], SessionId).

interim_debit_and_reserve_charging_key() ->
	[{userdata, [{doc, "Multiple charging keys (Rating-Group) in session"}]}].

interim_debit_and_reserve_charging_key(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(100000000),
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
	SessionId = session_id(diameter),
	{ok, _, {PackageUnits, G1}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, [ServiceId],
			Timestamp, undefined, undefined, initial, [], [], SessionId),
	{ok, _, {PackageUnits, G2}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, [ServiceId],
			Timestamp, undefined, undefined, initial, [], [], SessionId),
	RemAmount2 = RemAmount1 - (PackagePrice * 2),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount2}} = ocs:find_bucket(BId1),
	F2 = fun(#bucket{attributes = #{bucket_type := session}} = B, Acc) ->
				[B | Acc];
			(_, Acc) ->
				Acc
	end,
	ok = mnesia:sync_log(),
	BucketList = lists:foldl(F2, [], ocs:get_buckets(ProdRef)),
	2 = length(BucketList),
	Debit1 = rand:uniform(PackageSize),
	Debit2 = rand:uniform(PackageSize),
	{ok, _, {PackageUnits, G3}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, interim, [{PackageUnits, Debit1}],
			[], SessionId),
	{ok, _, {PackageUnits, G4}} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, interim, [{PackageUnits, Debit2}],
			[], SessionId),
	ok = mnesia:sync_log(),
	[#bucket{attributes = #{reservations := _Reservations1}},
			#bucket{attributes = #{reservations := _Reservations2}}]
			= lists:foldl(F2, [], ocs:get_buckets(ProdRef)),
	Debit3 = rand:uniform(PackageSize),
	Debit4 = rand:uniform(PackageSize),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup1, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, final, [{PackageUnits, Debit3}],
			undefined, SessionId),
	{ok, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, RatingGroup2, undefined, [ServiceId],
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
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount3}} = ocs:find_bucket(BId1),
	ok = mnesia:sync_log(),
	1 = length(ocs:get_buckets(ProdRef)).

interim_out_of_credit_voice() ->
	[{userdata, [{doc, "Voice call out of credit during call"}]}].

interim_out_of_credit_voice(_Config) ->
	{ok, UnitSize} = application:get_env(ocs, min_reserve_seconds),
	UnitPrice = rand:uniform(1000000),
	P1 = price(usage, seconds, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice * 2,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	UsedUnits = (UnitSize * 2) + rand:uniform(UnitSize),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{seconds, UsedUnits}], [], SessionId).

interim_out_of_credit_negative() ->
	[{userdata, [{doc, "Credit overrun leaves negative balance (radius)"}]}].

interim_out_of_credit_negative(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
	P1 = #price{name = "Usage", type = usage, units = octets,
			size = UnitSize, amount = UnitPrice,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			values = [#char_value{default = true,
			units = "bytes", value = UnitSize}]}]},
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			undefined, undefined,
			initial, [], [], SessionId),
	UsedUnits1 = UnitSize + rand:uniform(UnitSize - 1),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			interim, [{octets, UsedUnits1}], [], SessionId),
	UsedUnits2 = UnitSize + rand:uniform(UnitSize - 1),
	{out_of_credit, _, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			final, [{octets, UsedUnits2}], undefined, SessionId),
	ok = mnesia:sync_log(),
	F = fun(#bucket{remain_amount = Remain}) when Remain < 0 ->
				true;
			(#bucket{remain_amount = Remain}) when Remain >= 0 ->
				false
	end,
	true = lists:any(F, ocs:get_buckets(ProdRef)).

interim_out_of_credit_negative1() ->
	[{userdata, [{doc, "Credit overrun leaves negative balance (diameter)"}]}].

interim_out_of_credit_negative1(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
	P1 = #price{name = "Usage", type = usage, units = octets,
			size = UnitSize, amount = UnitPrice},
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitPrice + rand:uniform(UnitPrice - 1),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			undefined, undefined,
			initial, [], [], SessionId),
	UsedUnits1 = UnitSize + rand:uniform(UnitSize),
	{out_of_credit, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			interim, [{octets, UsedUnits1}], [], SessionId),
	UsedUnits2 = rand:uniform(UnitSize),
	{out_of_credit, _, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined,
			final, [{octets, UsedUnits2}], undefined, SessionId),
	ok = mnesia:sync_log(),
	F = fun(#bucket{remain_amount = Remain}) when Remain < 0 ->
				true;
			(#bucket{remain_amount = Remain}) when Remain >= 0 ->
				false
	end,
	true = lists:any(F, ocs:get_buckets(ProdRef)).

final_remove_session() ->
	[{userdata, [{doc, "Final call remove session attributes from subscriber record"}]}].

final_remove_session(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], [SA2]),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp, undefined,
			undefined, final, [{PackageUnits, Debit}], undefined, [SA2]),
	{ok, #service{session_attributes = [SA1]}} = ocs:find_service(ServiceId).

remove_session_after_multiple_interims() ->
	[{userdata, [{doc, "Remove session after multiple interims that exceed the reserved amount"}]}].

remove_session_after_multiple_interims(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined, undefined,
			undefined, [ServiceId], calendar:gregorian_seconds_to_datetime(TS1 + 60),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 2)}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS1 + 120),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 3)}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS1 + 180),
			undefined, undefined, interim,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 3)}],
			[], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS1 + 240),
			undefined, undefined, final,
			[{PackageUnits, PackageSize + rand:uniform(PackageSize div 2)}],
			undefined, SessionId),
	ok = mnesia:sync_log(),
	[#bucket{units = cents,
			attributes = #{bucket_type := normal}}] = ocs:get_buckets(ProdRef).

final_refund_octets() ->
	[{userdata, [{doc, "Refund unused amount of octets reservation"}]}].

final_refund_octets(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitSize * 4,
	B1 = bucket(octets, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	Reserve1 = rand:uniform(UnitSize),
	SessionId1 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reserve1}], SessionId1),
	UsedUnits1 = rand:uniform(Reserve1),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits1}], undefined, SessionId1),
	Remain1 = StartingAmount - UsedUnits1,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Remain1}} = ocs:find_bucket(BId),
	SessionId2 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 120),
			undefined, undefined, initial, [],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits2 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 180),
			undefined, undefined, interim, [{PackageUnits, UsedUnits2}],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits3 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 240),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits3}], undefined, SessionId2),
	Remain2 = Remain1 - UsedUnits2 - UsedUnits3,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Remain2}} = ocs:find_bucket(BId).

final_refund_seconds() ->
	[{userdata, [{doc, "Refund unused amount of seconds reservation"}]}].

final_refund_seconds(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
	PackageUnits = seconds,
	P1 = price(usage, PackageUnits, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = UnitSize * 4,
	B1 = bucket(seconds, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	Reserve1 = rand:uniform(UnitSize),
	SessionId1 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{PackageUnits, Reserve1}], SessionId1),
	UsedUnits1 = rand:uniform(Reserve1),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits1}], undefined, SessionId1),
	Remain1 = StartingAmount - UsedUnits1,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Remain1}} = ocs:find_bucket(BId),
	SessionId2 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 120),
			undefined, undefined, initial, [],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits2 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 180),
			undefined, undefined, interim, [{PackageUnits, UsedUnits2}],
			[{PackageUnits, UnitSize}], SessionId2),
	UsedUnits3 = rand:uniform(UnitSize div 2),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 240),
			undefined, undefined, final,
			[{PackageUnits, UsedUnits3}], undefined, SessionId2),
	Remain2 = Remain1 - UsedUnits2 - UsedUnits3,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Remain2}} = ocs:find_bucket(BId).

final_multiple_buckets() ->
	[{userdata, [{doc, "Debit applied to one of several available buckets"}]}].

final_multiple_buckets(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	UnitSize = MinReserve + rand:uniform(MinReserve - 1),
	UnitPrice = rand:uniform(1000000),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined, initial,
			[], [{PackageUnits, UnitSize * 3}], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim,
			[{PackageUnits, UnitSize * 3}], [{PackageUnits, UnitSize * 3}], SessionId),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 120),
			undefined, undefined, final,
			[{PackageUnits, UnitSize * 3}], undefined, SessionId),
	NewBalance = (Balance * NumBuckets) - (6 * UnitPrice),
	F2 = fun(#bucket{remain_amount = N}, Acc) -> Acc + N end,
	ok = mnesia:sync_log(),
	NewBalance = lists:foldl(F2, 0, ocs:get_buckets(ProdRef)).

final_voice() ->
	[{userdata, [{doc, "Final RADIUS accounting request for voice call"}]}].

final_voice(_Config) ->
	{ok, UnitSize} = application:get_env(ocs, min_reserve_seconds),
	UnitPrice = rand:uniform(1000000),
	ReserveTime = rand:uniform(10) * UnitSize,
	P1 = #price{name = "Calls", type = usage,
			units = seconds, size = UnitSize, amount = UnitPrice,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, []),
	ServiceId = add_service(ProdRef),
	StartingAmount = (((ReserveTime * 4) div UnitSize) * UnitPrice)
			+ rand:uniform(UnitPrice),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress,
			originate, initial, [], [], SessionId),
	UsedSeconds1 = rand:uniform(ReserveTime - 1),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress,
			originate, interim, [{seconds, UsedSeconds1}], [], SessionId),
	UsedSeconds2 = rand:uniform(ReserveTime - 1),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress,
			undefined, final, [{seconds, UsedSeconds2}], undefined, SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	TotalUsed = UsedSeconds1 + UsedSeconds2,
	case (TotalUsed rem UnitSize) of
		0 ->
			RemainAmount = StartingAmount - ((TotalUsed div UnitSize) * UnitPrice);
		_N ->
			RemainAmount = StartingAmount - (((TotalUsed div UnitSize) + 1) * UnitPrice)
	end.

radius_reserve_data() ->
	[{userdata, [{doc, "Reservation for RADIUS data session"}]}].

radius_reserve_data(_Config) ->
	{ok, MinReserveData} = application:get_env(ocs, min_reserve_octets),
	DataSize = MinReserveData + rand:uniform(MinReserveData - 1),
	DataAmount = rand:uniform(1000000),
	ReserveOctets = DataSize + rand:uniform(DataSize - 1),
	P1 = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([P1], 8),
	{ok, MinReserveVoice} = application:get_env(ocs, min_reserve_seconds),
	VoiceSize = MinReserveVoice + rand:uniform(MinReserveVoice - 1),
	VoiceAmount = rand:uniform(1000000),
	ReserveTime = VoiceSize + rand:uniform(VoiceSize - 1),
	P2 = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	VoiceOfferId = add_offer([P2], 9),
	BundleOffer = #offer{name = ocs:generate_identity(),
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, #offer{name = BundleOfferId}} = ocs:add_offer(BundleOffer),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = (ReserveOctets div DataSize) * DataAmount * 2,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	Timestamp = calendar:local_time(),
	SessionId = session_id(radius),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined,
			undefined, initial, [], [], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Amount}} = ocs:find_bucket(BId),
	ReservedUnits = case (ReserveOctets rem DataSize) of
		0 ->
			ReserveOctets div DataSize;
		_ ->
			ReserveOctets div DataSize + 1
	end,
	Amount = StartingAmount - (ReservedUnits * DataAmount).

radius_reserve_voice() ->
	[{userdata, [{doc, "Reservation for RADIUS voice call"}]}].

radius_reserve_voice(_Config) ->
	{ok, MinReserveData} = application:get_env(ocs, min_reserve_octets),
	DataSize = MinReserveData + rand:uniform(MinReserveData - 1),
	DataAmount = rand:uniform(1000000),
	ReserveOctets = DataSize + rand:uniform(DataSize - 1),
	P1 = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([P1], 8),
	{ok, MinReserveVoice} = application:get_env(ocs, min_reserve_seconds),
	VoiceSize = MinReserveVoice + rand:uniform(MinReserveVoice - 1),
	VoiceAmount = rand:uniform(1000000),
	ReserveTime = VoiceSize + rand:uniform(VoiceSize - 1),
	P2 = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	VoiceOfferId = add_offer([P2], 9),
	BundleOfferId = ocs:generate_password(),
	BundleProduct = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleProduct),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = (ReserveTime div VoiceSize) * VoiceAmount * 2,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress,
			undefined, initial, [], [], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Amount}} = ocs:find_bucket(BId),
	Reserved = case (ReserveTime rem VoiceSize) of
		0 ->
			ReserveTime div VoiceSize;
		_ ->
			(ReserveTime div VoiceSize) + 1
	end,
	Amount = StartingAmount - (Reserved * VoiceAmount).

radius_reserve_incoming() ->
	[{userdata, [{doc, "Reservation for RADIUS incoming voice call"}]}].

radius_reserve_incoming(_Config) ->
	{ok, MinReserveData} = application:get_env(ocs, min_reserve_octets),
	DataSize = MinReserveData + rand:uniform(MinReserveData - 1),
	DataAmount = rand:uniform(1000000),
	ReserveOctets = DataSize + rand:uniform(DataSize - 1),
	DataPrice = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([DataPrice], 8),
	{ok, MinReserveVoice} = application:get_env(ocs, min_reserve_seconds),
	VoiceSize = MinReserveVoice + rand:uniform(MinReserveVoice - 1),
	VoiceAmountOut = rand:uniform(1000000),
	ReserveTime = VoiceSize + rand:uniform(VoiceSize - 1),
	VoicePrice1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmountOut,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]},
			#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "originate"}]}]},
	VoiceAmountIn = rand:uniform(1000000),
	VoicePrice2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmountIn,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]},
			#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "answer"}]}]},
	VoiceOfferId = add_offer([VoicePrice1, VoicePrice2], 9),
	BundleOfferId  = ocs:generate_password(),
	BundleProduct = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleProduct),
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	StartingAmount = (ReserveTime div VoiceSize) * VoiceAmountIn * 2,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress,
			answer, initial, [], [], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Amount}} = ocs:find_bucket(BId),
	ReservedUnits = case (ReserveTime rem VoiceSize) of
		0 ->
			ReserveTime div VoiceSize;
		_ ->
			ReserveTime div VoiceSize + 1
	end,
	Amount = StartingAmount - (ReservedUnits * VoiceAmountIn).

radius_interim_voice() ->
	[{userdata, [{doc, "Interim reservation for RADIUS voice call"}]}].

radius_interim_voice(_Config) ->
	{ok, MinReserveData} = application:get_env(ocs, min_reserve_octets),
	DataSize = MinReserveData + rand:uniform(MinReserveData - 1),
	DataAmount = rand:uniform(1000000),
	ReserveOctets = DataSize + rand:uniform(DataSize - 1),
	P1 = #price{name = "Data", type = usage,
			units = octets, size = DataSize, amount = DataAmount,
			char_value_use = [#char_value_use{name = "radiusReserveOctets",
			values = [#char_value{default = true,
			units = "bytes", value = ReserveOctets}]}]},
	DataOfferId = add_offer([P1], 8),
	{ok, MinReserveVoice} = application:get_env(ocs, min_reserve_seconds),
	VoiceSize = MinReserveVoice + rand:uniform(MinReserveVoice - 1),
	VoiceAmount = rand:uniform(1000000),
	ReserveTime = VoiceSize + rand:uniform(VoiceSize - 1),
	P2 = #price{name = "Calls", type = usage,
			units = seconds, size = VoiceSize, amount = VoiceAmount,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			values = [#char_value{default = true,
			units = "seconds", value = ReserveTime}]}]},
	VoiceOfferId = add_offer([P2], 9),
	BundleOfferId = ocs:generate_password(),
	BundleOffer = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleOffer),
	StartingAmount = (ReserveTime div VoiceSize) * VoiceAmount * 6,
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress,
			undefined, initial, [], [], SessionId),
	UsedOctets = rand:uniform(ReserveOctets - 1),
	UsedSeconds = rand:uniform(ReserveTime - 1),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, CallAddress, undefined,
			interim, [{octets, UsedOctets}, {seconds, UsedSeconds}], [], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemainAmount}} = ocs:find_bucket(BId),
	ReservationTime = ReserveTime + UsedSeconds,
	ReservationAmount = case (ReservationTime rem VoiceSize) of
		0 ->
			(ReservationTime div VoiceSize) * VoiceAmount;
		_ ->
			((ReservationTime div VoiceSize) + 1) * VoiceAmount
	end,
	RemainAmount = StartingAmount - ReservationAmount.

time_of_day() ->
	[{userdata, [{doc, "Time of day price matching"}]}].

time_of_day(_Config) ->
	PeakAmount = 10000000,
	OffPeakAmount = 5000000,
	DataSize = 1000000,
	PeakPrice = #price{name = "Peak", type = usage,
			units = octets, size = DataSize, amount = PeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 480, units = "minutes"},
			upper = #quantity{amount = 1380, units = "minutes"}}}]}]},
	OffPeakPrice = #price{name = "OffPeak", type = usage,
			units = octets, size = DataSize, amount = OffPeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 1380, units = "minutes"},
			upper = #quantity{amount = 480, units = "minutes"}}}]}]},
	DataOfferId = add_offer([PeakPrice, OffPeakPrice], 8),
	ProdRef = add_product(DataOfferId),
	StartingAmount = 1000000000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceId = add_service(ProdRef),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	{Date, _} = calendar:local_time(),
	Timestamp1 = {Date, {7, 59, 59}},
	SessionId1 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp1, undefined,
			undefined, initial, [], [], SessionId1),
	UsedOctets1 = rand:uniform(DataSize * 6),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp1, undefined,
			undefined, final, [{octets, UsedOctets1}], undefined, SessionId1),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = Amount1}} = ocs:find_bucket(BId),
	UsedUnits1 = case UsedOctets1 rem DataSize of
		0 ->
			UsedOctets1 div DataSize;
		_ ->
			UsedOctets1 div DataSize + 1
	end,
	Amount1 = StartingAmount - UsedUnits1 * OffPeakAmount,
	Timestamp2 = {Date, {12, 13, 14}},
	SessionId2 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp2, undefined,
			undefined, initial, [], [], SessionId2),
	UsedOctets2 = rand:uniform(DataSize * 6),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp2, undefined, undefined,
			final, [{octets, UsedOctets2}], undefined, SessionId2),
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	RemAmount = ocs_rest:millionths_in(100),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, 60} = lists:keyfind(?SessionTimeout, 1, Attr),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_voice_with_partial_reservation() ->
	[{userdata, [{doc, "Authorize voice call with "
			"session time for available partial reservation amount"}]}].

authorize_voice_with_partial_reservation(_Config) ->
	{ok, PackageSize} = application:get_env(ocs, min_reserve_seconds),
	PackagePrice = rand:uniform(1000000),
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 300, default = true}]}],
	OfferId = add_offer([P1], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	RemAmount = PackagePrice * rand:uniform(5),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = (RemAmount div PackagePrice) * PackageSize,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_incoming_voice() ->
	[{userdata, [{doc, "Authorize incoming voice call"}]}].

authorize_incoming_voice(_Config) ->
	{ok, OutSize} = application:get_env(ocs, min_reserve_seconds),
	OutPrice = 2000000,
	Price1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "originate"}]}]},
	InPrice = 1000000,
	{ok, InSize} = application:get_env(ocs, min_reserve_seconds),
	Price2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "answer"}]}]},
	ReserveTime = 3600,
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = ReserveTime, default = true}]}],
	OfferId = add_offer([Price1, Price2], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	StartingAmount = 1000000000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, CallAddress, answer, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = StartingAmount}} = ocs:find_bucket(BId).

authorize_outgoing_voice() ->
	[{userdata, [{doc, "Authorize outgoing voice call"}]}].

authorize_outgoing_voice(_Config) ->
	{ok, InSize} = application:get_env(ocs, min_reserve_seconds),
	InPrice = 1000000,
	Price1 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "answer"}]}]},
	OutPrice = 2000000,
	{ok, OutSize} = application:get_env(ocs, min_reserve_seconds),
	Price2 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "originate"}]}]},
	ReserveTime = 3600,
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = ReserveTime, default = true}]}],
	OfferId = add_offer([Price1, Price2], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	StartingAmount = 1000000000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, CallAddress, originate, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = StartingAmount}} = ocs:find_bucket(BId).

authorize_default_voice() ->
	[{userdata, [{doc, "Authorize default outgoing voice call"}]}].

authorize_default_voice(_Config) ->
	{ok, OutSize} = application:get_env(ocs, min_reserve_seconds),
	OutPrice = 2000000,
	Price1 = #price{name = "Outgoing Calls", type = usage,
			units = seconds, size = OutSize, amount = OutPrice},
	InPrice = 1000000,
	{ok, InSize} = application:get_env(ocs, min_reserve_seconds),
	Price2 = #price{name = "Incoming Calls", type = usage,
			units = seconds, size = InSize, amount = InPrice,
			char_value_use = [#char_value_use{name = "callDirection",
			values = [#char_value{default = true,
			value = "answer"}]}]},
	ReserveTime = 3600,
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = ReserveTime, default = true}]}],
	OfferId = add_offer([Price1, Price2], 9, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	StartingAmount = 1000000000,
	B1 = bucket(cents, StartingAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(100),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = session_id(radius),
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, undefined, undefined, SessionId),
	{?SessionTimeout, 60} = lists:keyfind(?SessionTimeout, 1, Attr),
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(100),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = session_id(radius),
	{authorized, _, _Attr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, undefined, undefined, SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

authorize_data_with_partial_reservation() ->
	[{userdata, [{doc, "Athorize data access when price "
			"rated on seconds with partial reservation"}]}].

authorize_data_with_partial_reservation(_Config) ->
	{ok, PackageSize} = application:get_env(ocs, min_reserve_seconds),
	PackagePrice = rand:uniform(1000000),
	P1 = price(usage, seconds, PackageSize, PackagePrice),
	CharValueUse = [#char_value_use{name = "radiusReserveSessionTime",
			values = [#char_value{value = 300, default = true}]}],
	OfferId = add_offer([P1], 8, CharValueUse),
	ProdRef = add_product(OfferId),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = PackagePrice * rand:uniform(5),
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = session_id(radius),
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			[ServiceId], Password, Timestamp, undefined, undefibed, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = (RemAmount div PackagePrice) * PackageSize,
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	B1 = bucket(cents, -100),
	_BId = add_bucket(ProdRef, B1),
	SessionId = session_id(radius),
	{unauthorized, out_of_credit, _} = ocs_rating:authorize(radius, 12,
			[ServiceId], Password, Timestamp, "5551234", originate, [SessionId]).

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
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = ocs_rest:millionths_in(100),
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = session_id(radius),
	{unauthorized, bad_password, []} = ocs_rating:authorize(radius,
			ServiceType, [ServiceId], "bogus", Timestamp,
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
	{ok, _Service1} = ocs:add_service([ServiceId], Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	B1 = bucket(octets, 100),
	_BId = add_bucket(ProdRef, B1),
	CallAddress = ocs:generate_identity(),
	SessionId = session_id(radius),
	ServiceType = 12,
	{unauthorized, out_of_credit, []} = ocs_rating:authorize(radius,
			ServiceType, undefined, [ServiceId], Password, Timestamp,
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
	SessionId = session_id(radius),
	NumEvents = rand:uniform(10),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{messages, NumEvents}], SessionId),
	ok = mnesia:sync_log(),
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
	SessionId = session_id(diameter),
	NumEvents = rand:uniform(10),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [{messages, NumEvents}],
		SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = R1,
			attributes = #{reservations := Rs1}}} = ocs:find_bucket(BId),
	R1 = RemAmount - NumEvents,
	{ok, _, Rated} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			final, [{messages, NumEvents}], undefined,
		SessionId),
	R2 = RemAmount - NumEvents,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = R2}} = ocs:find_bucket(BId).

iec_out_of_credit() ->
	[{userdata, [{doc, "Balance remains after failed immediate event charging (IEC)"}]}].

iec_out_of_credit(_Config) ->
	PackagePrice = 1000000 + rand:uniform(1000000),
	PackageSize = 1,
	P1 = price(usage, messages, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 11),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount = 1,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	DebitAmount = {messages, 1},
	SessionId = session_id(diameter),
	{out_of_credit, _, _, _} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			calendar:local_time(), undefined, undefined, event,
			[], [DebitAmount], SessionId),
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemAmount}} = ocs:find_bucket(BId).

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
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackageUnits = octets,
	CharValueUse = [#char_value_use{name = "roamingTable",
					values = [#char_value{value = RoamingTable}]},
			#char_value_use{name = "radiusReserveOctets",
					values = [#char_value{default = true,
					units = "bytes", value = PackageSize}]}],
	P1 = #price{type = tariff, units = PackageUnits, size = PackageSize,
			amount = 0, char_value_use = CharValueUse},
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId, []),
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} = ocs:add_service([ServiceId], undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId = session_id(Protocol),
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			undefined, undefined, initial, [], [], SessionId),
	DebitUnits1 = rand:uniform(PackageSize),
	{ok, #service{}, _Granted} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 30000),
			undefined, undefined, interim, [{PackageUnits, DebitUnits1}], [],
			SessionId),
	DebitUnits2 = rand:uniform(PackageSize),
	{ok, #service{}, _Rated} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, SN, [ServiceId],
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
	ok = mnesia:sync_log(),
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
	{ok, MinReserve} = application:get_env(ocs, min_reserve_seconds),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
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
	{ok, _Service1} = ocs:add_service([ServiceId], undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	SessionId = session_id(Protocol),
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	DebitUnits1 = rand:uniform(PackageSize),
	{ok, #service{}, _Granted} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 30000),
			Address, undefined, interim, [{PackageUnits, DebitUnits1}], [],
			SessionId),
	DebitUnits2 = rand:uniform(PackageSize),
	{ok, #service{}, _Rated} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, SN, [ServiceId],
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
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = session_id(diameter),
	{ok, #service{}, {PackageUnits, PackageSize}} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, initial, [], [{PackageUnits, PackageSize}],
			SessionId),
	DebitUnits1 = rand:uniform(PackageSize),
	{ok, #service{}, _Granted} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 30000),
			Address, undefined, interim, [{PackageUnits, DebitUnits1}], [],
			SessionId),
	DebitUnits2 = rand:uniform(PackageSize),
	{ok, #service{}, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, [ServiceId],
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
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	SessionId = session_id(diameter),
	DebitUnits = {PackageUnits, PackageSize},
	{ok, #service{}, DebitUnits, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, event, [], [], SessionId),
	RemAmount2 = RemAmount1 - PackagePrice,
	ok = mnesia:sync_log(),
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
	{ok, _Service1} = ocs:add_service([ServiceId], undefined, ProdRef),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	RemAmount1 = ocs_rest:millionths_in(1000),
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32274,
	DebitUnits = rand:uniform(4),
	DebitAmount = {PackageUnits, DebitUnits},
	SessionId = session_id(diameter),
	{ok, #service{}, DebitAmount, _Rated} = ocs_rating:rate(diameter,
			ServiceType, undefined, undefined, SN, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS),
			Address, undefined, event, [], [DebitAmount],
			SessionId),
	RemAmount2 = RemAmount1 - (DebitUnits * PackagePrice),
	ok = mnesia:sync_log(),
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
	SessionId = session_id(diameter),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			6400, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, [ServiceId], calendar:gregorian_seconds_to_datetime(TS1 + 60), undefined,
			undefined, interim, [{octets, 300000000}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, [ServiceId], calendar:gregorian_seconds_to_datetime(TS1 + 120), undefined,
			undefined, interim, [{octets, 15000000}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp, undefined,
			undefined, final, [], undefined, SessionId),
	ok = mnesia:sync_log(),
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
	SessionId = session_id(diameter),
	ServiceType = 32251,
	Timestamp1 = calendar:local_time(),
	TS1 = calendar:datetime_to_gregorian_seconds(Timestamp1),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, [ServiceId1], Timestamp1, undefined, undefined,
			initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			32, undefined, undefined, [ServiceId1], calendar:gregorian_seconds_to_datetime(TS1 + 60), undefined,
			undefined, interim, [{octets, 300000000}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			65, undefined, undefined, [ServiceId1], calendar:gregorian_seconds_to_datetime(TS1 + 120), undefined,
			undefined, interim, [{octets, 15000000}], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType,
			undefined, undefined, undefined, [ServiceId1], Timestamp1, undefined,
			undefined, final, [], undefined, SessionId),
	ok = mnesia:sync_log(),
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

initial_invalid_service_type() ->
	[{userdata, [{doc, "Check the validity of a service type during rating"}]}].

initial_invalid_service_type(_Config) ->
	ServiceId = ocs:generate_identity(),
	UnitSize = 1000000 + rand:uniform(10000),
	Amount = rand:uniform(100000000),
	P1 = price(usage, octets, UnitSize, Amount),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	{ok, #service{}} = ocs:add_service([ServiceId], undefined, ProdRef, []),
	Balance = 1000000 + rand:uniform(1000000000),
	B1 = bucket(octets, Balance),
	_BId = add_bucket(ProdRef, B1),
	Ref = erlang:ref_to_list(make_ref()),
	SId = diameter:session_id(Ref),
	Timestamp = calendar:local_time(),
	Protocol = protocol(),
	ServiceType = undefined,
	{error, invalid_service_type} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	Timestamp = calendar:local_time(),
	SessionId = session_id(Protocol),
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, _, [#rated{} | _]} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			Timestamp, undefined, undefined, final, [], undefined, SessionId),
	ok = mnesia:sync_log(),
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
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	Timestamp = calendar:local_time(),
	TS = calendar:datetime_to_gregorian_seconds(Timestamp),
	SessionId = session_id(Protocol),
	{ok, _, {PackageUnits, PackageSize}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			Timestamp, undefined, undefined, initial, [],
			[{PackageUnits, PackageSize}], SessionId),
	{ok, #service{}, _Reservation} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{PackageUnits, PackageSize}], SessionId),
	{ok, _, [#rated{} | _]} = ocs_rating:rate(Protocol, ServiceType,
			undefined, undefined, undefined, [ServiceId], Timestamp,
			undefined, undefined, final,
			[{PackageUnits, 50}], undefined, SessionId),
	ok = mnesia:sync_log(),
	{error, not_found} = ocs:find_bucket(BId1),
	RemainAmount3 = RemainAmount1 + RemainAmount2 - PackagePrice,
	ok = mnesia:sync_log(),
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
	{ok, _Service} = ocs:add_service([ServiceId], undefined, ProdRef),
	RemAmount1 = ocs_rest:millionths_in(1000),
	BId = add_bucket(ProdRef, bucket(cents, RemAmount1)),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	UnTariffedDestination =  [$9 | ocs:generate_identity()],
	SessionId1 = session_id(Protocol),
	{error,table_lookup_failed} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			calendar:local_time(), UnTariffedDestination, undefined,
			initial, [], [], SessionId1),
	{_Cont, Items} = ocs_gtt:list(start, list_to_existing_atom(TariffTable3)),
	#gtt{num = Destination, value = Value} = lists:last(Items), 
	Rate = element(2, Value),
	SessionId2 = session_id(Protocol),
	{ok, _, {seconds, 300}} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			calendar:local_time(), Destination, undefined,
			initial, [], [], SessionId2),
	RemainAmount2 = RemAmount1 - Rate,
	ok = mnesia:sync_log(),
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
			values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 480, units = "minutes"},
			upper = #quantity{amount = 1380, units = "minutes"}}}]}]},
	P2 = #price{name = "OffPeak", type = usage,
			units = octets, size = UnitSize, amount = OffPeakAmount,
			char_value_use = [#char_value_use{name = "timeOfDayRange",
			values = [#char_value{default = true,
			value = #range{lower = #quantity{amount = 1380, units = "minutes"},
			upper = #quantity{amount = 480, units = "minutes"}}}]},
			#char_value_use{name = "fixedPriceBucket",
			values = [#char_value{value = true}]}],
			alteration = Allowance},
	OfferId = add_offer([P1, P2], 8),
	ServiceId = ocs:generate_identity(),
	{ok, #service{name = ServiceRef}} = ocs:add_service([ServiceId],
			undefined, undefined),
	{ok, #product{id = ProdRef, balance = [BId1]}}
			= ocs:add_product(OfferId, [ServiceRef]),
	RemAmount1 = (10 *  UnitSize) + rand:uniform(UnitSize),
	BId2 = add_bucket(ProdRef, bucket(octets, RemAmount1)),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, data),
	SessionId1 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			{date(), {20, 0, 0}}, undefined, undefined,
			initial, [], [], SessionId1),
	DayAmount1 = rand:uniform(UnitSize) + UnitSize,
	{ok, _, _} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			{date(), {21, 0, 0}}, undefined, undefined,
			interim, [{octets, DayAmount1}], [], SessionId1),
	DayAmount2 = rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			{date(), {22, 0, 0}}, undefined, undefined,
			final, [{octets, DayAmount2}], undefined, SessionId1),
	SessionId2 = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			{date(), {2, 0, 0}}, undefined, undefined,
			initial, [], [], SessionId2),
	NightAmount1 = rand:uniform(UnitSize) + UnitSize,
	{ok, _, _} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			{date(), {3, 0, 0}}, undefined, undefined,
			interim, [{octets, NightAmount1}], [], SessionId2),
	NightAmount2 = rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(Protocol,
			ServiceType, undefined, undefined, undefined, [ServiceId],
			{date(), {4, 0, 0}}, undefined, undefined,
			final, [{octets, NightAmount2}], undefined, SessionId2),
	RemainAmount2 = RemAmount1 - DayAmount1 - DayAmount2,
	ok = mnesia:sync_log(),
	{ok, #bucket{remain_amount = RemainAmount2}} = ocs:find_bucket(BId2),
	RemainAmount3 = AllowanceSize - NightAmount1 - NightAmount2,
	{ok, #bucket{remain_amount = RemainAmount3}} = ocs:find_bucket(BId1).

tariff_bucket_voice() ->
	[{userdata, [{doc, "Topup bucket, pinned to tariff Price (Voice)"}]}].

tariff_bucket_voice(_Config) ->
	{ok, UnitSize} = application:get_env(ocs, min_reserve_seconds),
	Table1 = ocs:generate_identity(),
	Table2 = ocs:generate_identity(),
	Table3 = ocs:generate_identity(),
	F = fun F(N, Items) when N >= $0, N =< $9 ->
				Description = ocs:generate_identity(),
				Rate = (rand:uniform(5) * 1000000) + rand:uniform(1000000),
				Value = {Description, Rate},
				Number = [N],
				Item = {Number, Value},
				F(N - 1, [Item | Items]);
			F(_N, Items) ->
				Items
	end,
	ok = ocs_gtt:new(Table1, [], F($9, [])),
	ok = ocs_gtt:new(Table2, [], F($9, [])),
	ok = ocs_gtt:new(Table3, [], F($9, [])),
	CharValueUse1 = [#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = Table1}]},
			#char_value_use{name = "fixedPriceBucket",
					values = [#char_value{value = true}]}],
	CharValueUse2 = [#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = Table2}]},
			#char_value_use{name = "fixedPriceBucket",
					values = [#char_value{value = true}]}],
	CharValueUse3 = [#char_value_use{name = "destPrefixTariffTable",
			values = [#char_value{value = Table3}]}],
	PriceName1 = ocs:generate_identity(),
	PriceName2 = ocs:generate_identity(),
	PriceName3 = ocs:generate_identity(),
	P1 = #price{name = PriceName1, type = tariff,
			units = seconds, size = UnitSize,
			char_value_use = CharValueUse1},
	P2 = #price{name = PriceName2, type = tariff,
			units = seconds, size = UnitSize,
			char_value_use = CharValueUse2},
	P3 = #price{name = PriceName3, type = tariff,
			units = seconds, size = UnitSize,
			char_value_use = CharValueUse3},
	OfferId = add_offer([P1, P2, P3], 9),
	ServiceId = ocs:generate_identity(),
	{ok, #service{name = ServiceRef}} = ocs:add_service([ServiceId],
			undefined, undefined),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [ServiceRef]),
	Destination = [$5 | ocs:generate_identity()],
	{_, Rate1, _} = ocs_gtt:lookup_last(Table1, Destination),
	{_, Rate2, _} = ocs_gtt:lookup_last(Table2, Destination),
	{_, Rate3, _} = ocs_gtt:lookup_last(Table3, Destination),
	Reserve = UnitSize * rand:uniform(5),
	Units1 = Reserve + rand:uniform(Reserve),
	Amount1 = case Units1 rem UnitSize of
		0 ->
			(Units1 div UnitSize) * Rate1;
		_ ->
			((Units1 div UnitSize) + 1) * Rate1
	end,
	Units2 = rand:uniform(600),
	Amount2 = case Units2 rem UnitSize of
		0 ->
			(Units2 div UnitSize) * Rate2;
		_ ->
			((Units2 div UnitSize) + 1) * Rate2
	end,
	Amount3 = ocs_rest:millionths_in(1000),
	Today = calendar:date_to_gregorian_days(date()),
	LastMonth = calendar:gregorian_days_to_date(Today - 30),	
	LastWeek = calendar:gregorian_days_to_date(Today - 7),	
	NextWeek = calendar:gregorian_days_to_date(Today + 7),	
	NextMonth = calendar:gregorian_days_to_date(Today + 30),	
	Bucket1 = #bucket{units = cents,
			remain_amount = Amount1, price = PriceName1,
			start_date = ocs_rest:date({LastMonth, time()}),
			end_date = ocs_rest:date({NextWeek, time()}),
			attributes = #{bucket_type => normal}},
	Bucket2 = #bucket{units = cents,
			remain_amount = Amount2, price = PriceName2,
			start_date = ocs_rest:date({LastWeek, time()}),
			end_date = ocs_rest:date({NextMonth, time()}),
			attributes = #{bucket_type => normal}},
	Bucket3 = bucket(cents, Amount3),
	{ok, _, #bucket{id = BId1}} = ocs:add_bucket(ProdRef, Bucket1),
	{ok, _, #bucket{id = BId2}} = ocs:add_bucket(ProdRef, Bucket2),
	{ok, _, #bucket{id = BId3}} = ocs:add_bucket(ProdRef, Bucket3),
	Protocol = protocol(),
	ServiceType = service_type(Protocol, voice),
	SessionId = session_id(Protocol),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, initial,
			[], [{seconds, Reserve}], SessionId),
	Debit1 = Reserve - UnitSize + rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, interim,
			[{seconds, Debit1}], [{seconds, Reserve}], SessionId),
	Debit2 = Reserve - UnitSize + rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, interim,
			[{seconds, Debit2}], [{seconds, Reserve}], SessionId),
	Debit3 = Reserve - UnitSize + rand:uniform(UnitSize),
	{ok, _, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, interim,
			[{seconds, Debit3}], [{seconds, Reserve}], SessionId),
	Debit4 = Reserve - UnitSize + rand:uniform(UnitSize),
	{ok, _, Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, final,
			[{seconds, Debit4}], undefined, SessionId),
	Debits = Debit1 + Debit2 + Debit3 + Debit4,
	DebitedUnits = case Debits rem UnitSize of
		0 ->
			Debits;
		_ ->
			Debits + UnitSize - (Debits rem UnitSize)
	end,
	ok = mnesia:sync_log(),
	{DebitedCents1, DebitedUnits1}  = case ocs:find_bucket(BId1) of
		{ok, #bucket{remain_amount = Remain1}} ->
			D1 = Amount1 - Remain1,
			0 = D1 rem Rate1,
			{D1, (D1 div Rate1) * UnitSize};
		{error, not_found} ->
			{Amount1, (Amount1 div Rate1) * UnitSize}
	end,
	{DebitedCents2, DebitedUnits2}  = case ocs:find_bucket(BId2) of
		{ok, #bucket{remain_amount = Remain2}} ->
			D2 = Amount2 - Remain2,
			0 = D2 rem Rate2,
			{D2, (D2 div Rate2) * UnitSize};
		{error, not_found} ->
			{Amount2, (Amount2 div Rate2) * UnitSize}
	end,
	{DebitedCents3, DebitedUnits3}  = case ocs:find_bucket(BId3) of
		{ok, #bucket{remain_amount = Remain3}} ->
			D3 = Amount3 - Remain3,
			0 = D3 rem Rate3,
			{D3, (D3 div Rate3) * UnitSize};
		{error, not_found} ->
			{Amount3, (Amount3 div Rate3) * UnitSize}
	end,
	DebitedUnits = DebitedUnits1 + DebitedUnits2 + DebitedUnits3,
	DebitedCents = DebitedCents1 + DebitedCents2 + DebitedCents3.

tariff_bucket_iec() ->
	[{userdata, [{doc, "Topup bucket, pinned to tariff Price (SMS IEC)"}]}].

tariff_bucket_iec(_Config) ->
	Table1 = ocs:generate_identity(),
	Table2 = ocs:generate_identity(),
	Table3 = ocs:generate_identity(),
	F = fun F(N, Items) when N >= $0, N =< $9 ->
				Description = ocs:generate_identity(),
				Rate = rand:uniform(4) * 1000000,
				Value = {Description, Rate},
				Number = [N],
				Item = {Number, Value},
				F(N - 1, [Item | Items]);
			F(_N, Items) ->
				Items
	end,
	ok = ocs_gtt:new(Table1, [], F($9, [])),
	ok = ocs_gtt:new(Table2, [], F($9, [])),
	ok = ocs_gtt:new(Table3, [], F($9, [])),
	CharValueUse1 = [#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = Table1}]},
			#char_value_use{name = "fixedPriceBucket",
					values = [#char_value{value = true}]}],
	CharValueUse2 = [#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = Table2}]},
			#char_value_use{name = "fixedPriceBucket",
					values = [#char_value{value = true}]}],
	CharValueUse3 = [#char_value_use{name = "destPrefixTariffTable",
			values = [#char_value{value = Table3}]}],
	PriceName1 = ocs:generate_identity(),
	PriceName2 = ocs:generate_identity(),
	PriceName3 = ocs:generate_identity(),
	P1 = #price{name = PriceName1, type = tariff,
			units = messages, size = 1,
			char_value_use = CharValueUse1},
	P2 = #price{name = PriceName2, type = tariff,
			units = messages, size = 1,
			char_value_use = CharValueUse2},
	P3 = #price{name = PriceName3, type = tariff,
			units = messages, size = 1,
			char_value_use = CharValueUse3},
	OfferId = add_offer([P1, P2, P3], 11),
	ServiceId = ocs:generate_identity(),
	{ok, #service{name = ServiceRef}} = ocs:add_service([ServiceId],
			undefined, undefined),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [ServiceRef]),
	Destination = [$5 | ocs:generate_identity()],
	{_, Rate1, _} = ocs_gtt:lookup_last(Table1, Destination),
	{_, Rate2, _} = ocs_gtt:lookup_last(Table2, Destination),
	{_, Rate3, _} = ocs_gtt:lookup_last(Table3, Destination),
	Amount1 = ocs_rest:millionths_in(rand:uniform(4)),
	Amount2 = ocs_rest:millionths_in(rand:uniform(4)),
	Amount3 = ocs_rest:millionths_in(1000),
	Today = calendar:date_to_gregorian_days(date()),
	LastMonth = calendar:gregorian_days_to_date(Today - 30),	
	LastWeek = calendar:gregorian_days_to_date(Today - 7),	
	NextWeek = calendar:gregorian_days_to_date(Today + 7),	
	NextMonth = calendar:gregorian_days_to_date(Today + 30),	
	Bucket1 = #bucket{units = cents,
			remain_amount = Amount1, price = PriceName1,
			start_date = ocs_rest:date({LastMonth, time()}),
			end_date = ocs_rest:date({NextWeek, time()}),
			attributes = #{bucket_type => normal}},
	Bucket2 = #bucket{units = cents,
			remain_amount = Amount2, price = PriceName2,
			start_date = ocs_rest:date({LastWeek, time()}),
			end_date = ocs_rest:date({NextMonth, time()}),
			attributes = #{bucket_type => normal}},
	Bucket3 = bucket(cents, Amount3),
	{ok, _, #bucket{id = BId1}} = ocs:add_bucket(ProdRef, Bucket1),
	{ok, _, #bucket{id = BId2}} = ocs:add_bucket(ProdRef, Bucket2),
	{ok, _, #bucket{id = BId3}} = ocs:add_bucket(ProdRef, Bucket3),
	Protocol = diameter,
	ServiceType = service_type(diameter, message),
	SessionId = session_id(Protocol),
	DebitUnits = {messages, 1},
	{ok, _, DebitUnits, _Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, event, [], [], SessionId),
	{ok, _, DebitUnits, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, event, [], [], SessionId),
	{ok, _, DebitUnits, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, event, [], [], SessionId),
	{ok, _, DebitUnits, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, event, [], [], SessionId),
	{ok, _, DebitUnits, _} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, event, [], [], SessionId),
	ok = mnesia:sync_log(),
	{DebitedCents1, DebitedUnits1} = case ocs:find_bucket(BId1) of
		{ok, #bucket{remain_amount = Remain1}} ->
			D1 = Amount1 - Remain1,
			0 = D1 rem Rate1,
			{D1, D1 div Rate1};
		{error, not_found} ->
			{Amount1, Amount1 div Rate1}
	end,
	{DebitedCents2, DebitedUnits2}  = case ocs:find_bucket(BId2) of
		{ok, #bucket{remain_amount = Remain2}} ->
			D2 = Amount2 - Remain2,
			0 = D2 rem Rate2,
			{D2, D2 div Rate2};
		{error, not_found} ->
			{Amount2, Amount2 div Rate2}
	end,
	{DebitedCents3, DebitedUnits3}  = case ocs:find_bucket(BId3) of
		{ok, #bucket{remain_amount = Remain3}} ->
			D3 = Amount3 - Remain3,
			0 = D3 rem Rate3,
			{D3, D3 div Rate3};
		{error, not_found} ->
			{Amount3, Amount3 div Rate3}
	end,
	DebitedCents = (DebitedUnits1 * Rate1) + (DebitedUnits2 * Rate2)
			+ (DebitedUnits3 * Rate3),
	DebitedCents = DebitedCents1 + DebitedCents2 + DebitedCents3.

tariff_bucket_ecur() ->
	[{userdata, [{doc, "Topup bucket, pinned to tariff Price (SMS ECUR)"}]}].

tariff_bucket_ecur(_Config) ->
	Table1 = ocs:generate_identity(),
	Table2 = ocs:generate_identity(),
	Table3 = ocs:generate_identity(),
	F = fun F(N, Items) when N >= $0, N =< $9 ->
				Description = ocs:generate_identity(),
				Rate = rand:uniform(4) * 1000000,
				Value = {Description, Rate},
				Number = [N],
				Item = {Number, Value},
				F(N - 1, [Item | Items]);
			F(_N, Items) ->
				Items
	end,
	ok = ocs_gtt:new(Table1, [], F($9, [])),
	ok = ocs_gtt:new(Table2, [], F($9, [])),
	ok = ocs_gtt:new(Table3, [], F($9, [])),
	CharValueUse1 = [#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = Table1}]},
			#char_value_use{name = "fixedPriceBucket",
					values = [#char_value{value = true}]}],
	CharValueUse2 = [#char_value_use{name = "destPrefixTariffTable",
					values = [#char_value{value = Table2}]},
			#char_value_use{name = "fixedPriceBucket",
					values = [#char_value{value = true}]}],
	CharValueUse3 = [#char_value_use{name = "destPrefixTariffTable",
			values = [#char_value{value = Table3}]}],
	PriceName1 = ocs:generate_identity(),
	PriceName2 = ocs:generate_identity(),
	PriceName3 = ocs:generate_identity(),
	P1 = #price{name = PriceName1, type = tariff,
			units = messages, size = 1,
			char_value_use = CharValueUse1},
	P2 = #price{name = PriceName2, type = tariff,
			units = messages, size = 1,
			char_value_use = CharValueUse2},
	P3 = #price{name = PriceName3, type = tariff,
			units = messages, size = 1,
			char_value_use = CharValueUse3},
	OfferId = add_offer([P1, P2, P3], 11),
	ServiceId = ocs:generate_identity(),
	{ok, #service{name = ServiceRef}} = ocs:add_service([ServiceId],
			undefined, undefined),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [ServiceRef]),
	Destination = [$5 | ocs:generate_identity()],
	{_, Rate1, _} = ocs_gtt:lookup_last(Table1, Destination),
	{_, Rate2, _} = ocs_gtt:lookup_last(Table2, Destination),
	{_, Rate3, _} = ocs_gtt:lookup_last(Table3, Destination),
	Amount1 = ocs_rest:millionths_in(rand:uniform(4)),
	Amount2 = ocs_rest:millionths_in(rand:uniform(4)),
	Amount3 = ocs_rest:millionths_in(1000),
	Today = calendar:date_to_gregorian_days(date()),
	LastMonth = calendar:gregorian_days_to_date(Today - 30),	
	LastWeek = calendar:gregorian_days_to_date(Today - 7),	
	NextWeek = calendar:gregorian_days_to_date(Today + 7),	
	NextMonth = calendar:gregorian_days_to_date(Today + 30),	
	Bucket1 = #bucket{units = cents,
			remain_amount = Amount1, price = PriceName1,
			start_date = ocs_rest:date({LastMonth, time()}),
			end_date = ocs_rest:date({NextWeek, time()}),
			attributes = #{bucket_type => normal}},
	Bucket2 = #bucket{units = cents,
			remain_amount = Amount2, price = PriceName2,
			start_date = ocs_rest:date({LastWeek, time()}),
			end_date = ocs_rest:date({NextMonth, time()}),
			attributes = #{bucket_type => normal}},
	Bucket3 = bucket(cents, Amount3),
	{ok, _, #bucket{id = BId1}} = ocs:add_bucket(ProdRef, Bucket1),
	{ok, _, #bucket{id = BId2}} = ocs:add_bucket(ProdRef, Bucket2),
	{ok, _, #bucket{id = BId3}} = ocs:add_bucket(ProdRef, Bucket3),
	Protocol = diameter,
	ServiceType = service_type(diameter, message),
	SessionId = session_id(Protocol),
	DebitUnits = {messages, 1},
	{ok, _, DebitUnits} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, initial, [], [], SessionId),
	{ok, _, _Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, final, [], [], SessionId),
	{ok, _, DebitUnits} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, initial, [], [], SessionId),
	{ok, _, _Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, final, [], [], SessionId),
	{ok, _, DebitUnits} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, initial, [], [], SessionId),
	{ok, _, _Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, final, [], [], SessionId),
	{ok, _, DebitUnits} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, initial, [], [], SessionId),
	{ok, _, _Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, final, [], [], SessionId),
	{ok, _, DebitUnits} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, initial, [], [], SessionId),
	{ok, _, _Rated} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], calendar:local_time(),
			Destination, originate, final, [], [], SessionId),
	ok = mnesia:sync_log(),
	{DebitedCents1, DebitedUnits1} = case ocs:find_bucket(BId1) of
		{ok, #bucket{remain_amount = Remain1}} ->
			D1 = Amount1 - Remain1,
			0 = D1 rem Rate1,
			{D1, D1 div Rate1};
		{error, not_found} ->
			{Amount1, Amount1 div Rate1}
	end,
	{DebitedCents2, DebitedUnits2}  = case ocs:find_bucket(BId2) of
		{ok, #bucket{remain_amount = Remain2}} ->
			D2 = Amount2 - Remain2,
			0 = D2 rem Rate2,
			{D2, D2 div Rate2};
		{error, not_found} ->
			{Amount2, Amount2 div Rate2}
	end,
	{DebitedCents3, DebitedUnits3}  = case ocs:find_bucket(BId3) of
		{ok, #bucket{remain_amount = Remain3}} ->
			D3 = Amount3 - Remain3,
			0 = D3 rem Rate3,
			{D3, D3 div Rate3};
		{error, not_found} ->
			{Amount3, Amount3 div Rate3}
	end,
	DebitedCents = (DebitedUnits1 * Rate1) + (DebitedUnits2 * Rate2)
			+ (DebitedUnits3 * Rate3),
	DebitedCents = DebitedCents1 + DebitedCents2 + DebitedCents3.

scur_5g_data_initial() ->
	[{userdata, [{doc, "5G Data connectivity charging (initial)"}]}].

scur_5g_data_initial(_Config) ->
	{ok, MinReserve} = application:get_env(ocs, min_reserve_octets),
	PackageSize = MinReserve + rand:uniform(MinReserve - 1),
	PackagePrice = rand:uniform(1000000),
	PackageUnits = octets,
	P1 = price(usage, PackageUnits, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 8),
	ProdRef = add_product(OfferId),
	ServiceId = add_service(ProdRef),
	RemAmount1 = PackagePrice * 100,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	Protocol = nrf,
	ServiceType = 32255,
	SessionId = session_id(nrf),
	{ok, _, GrantedAmount} = ocs_rating:rate(Protocol, ServiceType, undefined,
			undefined, undefined, [ServiceId], Timestamp, undefined, undefined,
			initial, [], [], SessionId),
	{PackageUnits, PackageSize} = GrantedAmount,
	ok = mnesia:sync_log(),
	{ok, #bucket{units = cents, remain_amount = RemAmount2,
			attributes = #{bucket_type := normal}}} = ocs:find_bucket(BId),
	RemAmount2 == RemAmount1 - PackagePrice.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @hidden
price(Type, octets, Size, Amount) ->
	CharValue = #char_value{units = "bytes", value = Size},
	CharName = "radiusReserveOctets",
	CharValueUse = #char_value_use{name = CharName, values = [CharValue]},
	#price{name = ocs:generate_identity(),
			type = Type, units = octets,
			size = Size, amount = Amount,
			char_value_use = [CharValueUse]};
price(Type, seconds, Size, Amount) ->
	CharValue = #char_value{units = "seconds", value = Size},
	CharName = "radiusReserveTime",
	CharValueUse = #char_value_use{name = CharName, values = [CharValue]},
	#price{name = ocs:generate_identity(),
			type = Type, units = seconds,
			size = Size, amount = Amount,
			char_value_use = [CharValueUse]};
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
			ocs:add_service([ServiceId], ocs:generate_password(),
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

%% @hidden
protocol() ->
	protocol(rand:uniform(3)).
%% @hidden
protocol(1) ->
	radius;
protocol(2) ->
	diameter;
protocol(3) ->
	nrf.

%% @hidden
service_type(radius, data) ->
	2;
service_type(radius, voice) ->
	12;
service_type(nrf, data) ->
	32255;
service_type(diameter, data) ->
	32251;
service_type(_, voice) ->
	32260;
service_type(_, message) ->
	32274.

%%@hidden
session_id(radius) ->
	SessionId = list_to_binary(ocs:generate_password()),
	AcctSessionId = {?AcctSessionId, SessionId},
	Address = inet:ntoa({192, 168, rand:uniform(256) - 1, rand:uniform(254)}),
	NasIp = {?NasIpAddress, Address},
	NasId = {?NasIdentifier, ocs:generate_password()},
	[NasIp, NasId, AcctSessionId];
session_id(diameter) ->
	SessionId = diameter:session_id(ocs:generate_password()),
	[{'Session-Id', SessionId}];
session_id(nrf) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	RatingDataRef = integer_to_list(TS) ++ integer_to_list(N),
	[{nrf_ref, RatingDataRef}].

