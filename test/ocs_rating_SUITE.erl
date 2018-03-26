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
	interim_out_of_credit_voice,
	final_remove_session, final_refund, final_voice,
	reserve_data, reserve_voice, interim_voice, time_of_day,
	authorize_voice, authorize_voice_with_partial_reservation,
	authorize_incoming_voice, authorize_outgoing_voice,
	authorize_default_voice, authorize_data_1, authorize_data_2,
	authorize_data_with_partial_reservation, authorize_negative_balance,
	unauthorize_bad_password, unauthorize_bad_password, reserve_sms, debit_sms].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
initial_exact_fit() ->
	[{userdata, [{doc, "Cents balance exactly equal to reservation price"}]}].

initial_exact_fit(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	B1 = bucket(cents, PackagePrice),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, Timestamp, undefined, undefined, initial, [],
			[{octets, PackageSize}], SessionId),
	{ok, #bucket{remain_amount = 0,
			reservations = [{_, 0, PackagePrice, _}]}} = ocs:find_bucket(BId).

initial_insufficient() ->
	[{userdata, [{doc, "Insufficient cents balance for initial reservation"}]}].

initial_insufficient(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 13,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{out_of_credit, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, undefined, undefined, initial, [], [{octets, PackageSize}], SessionId),
	{ok, #bucket{units = cents, remain_amount = RemAmount,
			reservations = []}} = ocs:find_bucket(BId).

initial_insufficient_multisession() ->
	[{userdata, [{doc, "Insufficient cents balance on initial reservation of additional session"}]}].

initial_insufficient_multisession(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 13,
	B1 = bucket(cents, RemAmount),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	OldReservation = [{erlang:system_time(?MILLISECOND), 100, SessionId1}],
	B2 = B1#bucket{reservations = OldReservation},
	BId = add_bucket(ProdRef, B2),
	Timestamp = calendar:local_time(),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{out_of_credit, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, undefined, undefined, initial, [], [{octets, PackageSize}], SessionId2),
	{ok, #bucket{remain_amount = RemAmount, reservations = OldReservation}} = ocs:find_bucket(BId).

initial_add_session() ->
	[{userdata, [{doc, "Add a session"}]}].

initial_add_session(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 1000,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	ServiceType = 2,
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.1.150"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionId = lists:keysort(1, [AcctSessionId, NasIp, NasId]),
	SessionAttr = [NasId, NasIp, AcctSessionId, {?ServiceType, ServiceType}],
	{ok, #service{session_attributes = [{_, SessionId}]}, PackageSize} =
			ocs_rating:rate(radius, ServiceType, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{octets, PackageSize}], SessionAttr),
	{ok, #bucket{reservations = [{_, 0, PackagePrice, _}]}} = ocs:find_bucket(BId).

initial_overhead() ->
	[{userdata, [{doc, "Reserved amount greater than requested reservation amount"}]}].

initial_overhead(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount1 = 233,
	B1 = bucket(cents, RemAmount1),
	BId = add_bucket(ProdRef, B1),
	Timestamp = calendar:local_time(),
	Reservation = 1555,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 2,
	{ok, #service{}, Reserved} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, undefined, undefined, initial, [], [{octets, Reservation}],
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
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	B1 = bucket(cents, 50),
	_BId1 = add_bucket(ProdRef, B1),
	B2 = bucket(cents, 100),
	_BId2 = add_bucket(ProdRef, B2),
	B3 = bucket(cents, 75),
	_BId3 = add_bucket(ProdRef, B3),
	Balance1 = lists:sum([R || #bucket{remain_amount = R} <- [B1, B2, B3]]),
	Timestamp = calendar:local_time(),
	ServiceType = 2,
	Reservation = 1111,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, #service{}, Reserved} =
			ocs_rating:rate(radius, ServiceType, ServiceId, Timestamp,
			undefined, undefined, initial, [], [{octets, Reservation}], SessionId),
	[#product{balance = BucketRefs}] = mnesia:dirty_read(product, ProdRef),
	RatedBuckets = lists:flatten([mnesia:dirty_read(bucket, Id) || Id <- BucketRefs]),
	Balance2 = lists:sum([R || #bucket{remain_amount = R} <- RatedBuckets]),
	F = fun(A) when (A rem PackageSize) == 0 ->
			(A div PackageSize) * PackagePrice;
		(A) ->
			(A div PackageSize + 1) * PackagePrice
	end,
	Balance2 = Balance1 - F(Reservation),
	0 = Reserved rem PackageSize,
	true = Reserved > Reservation,
	Balance3 = lists:sum([R || #bucket{reservations = [{_, 0, R, SId}]}
			<- RatedBuckets, SId == SessionId]),
	Balance3 = F(Reservation).

initial_expire_buckets() ->
	[{userdata, [{doc, "Remove expired buckets"}]}].

initial_expire_buckets(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	B2= B1#bucket{start_date = erlang:system_time(?MILLISECOND) -  (2 * 2592000000),
		termination_date = erlang:system_time(?MILLISECOND) - 2592000000},
	BId = add_bucket(ProdRef, B2),
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			Timestamp, undefined, undefined, initial, [], [{octets, PackageSize}],
			SessionId),
	{ok, #product{balance = []}} = ocs:find_product(ProdRef),
	{error, not_found} = ocs:find_bucket(BId).

initial_ignore_expired_buckets() ->
	[{userdata, [{doc, "Ignore expired buckets with sessions"}]}].

initial_ignore_expired_buckets(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ExpiredBucket = #bucket{units = cents, remain_amount = 1000,
		reservations = [{erlang:system_time(?MILLISECOND) - 3666000, 123, SessionId1}],
		start_date = erlang:system_time(?MILLISECOND) - (2 * 2592000000),
		termination_date = erlang:system_time(?MILLISECOND) - 2592000000},
	RemAmount = 565,
	_BId1 = add_bucket(ProdRef, ExpiredBucket),
	CurrentBucket = bucket(cents, RemAmount),
	_BId2 = add_bucket(ProdRef, CurrentBucket),
	Timestamp = calendar:local_time(),
	Reservation = rand:uniform(PackageSize),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{ok, #service{}, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, Timestamp, undefined, undefined, initial, [],
			[{octets, Reservation}], SessionId2),
	{ok, #product{balance = BucketRefs}} = ocs:find_product(ProdRef),
	2 = length(BucketRefs).

initial_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and ignore"}]}].

initial_negative_balance(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = "subscription", type = recurring,
			period = monthly, amount = 2000},
	P2 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1, P2], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Timestamp = calendar:local_time(),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	Balance = lists:sum([R || #bucket{remain_amount = R} <- Buckets1]),
	Reservation = rand:uniform(PackageSize),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			Timestamp, undefined, undefined, initial, [], [{octets, Reservation}],
			SessionId),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	Buckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	Balance = lists:sum([R || #bucket{remain_amount = R} <- Buckets2]).

interim_reserve() ->
	[{userdata, [{doc, "Reservation amount equal to package size"}]}].

interim_reserve(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 200,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	ServiceType = 2,
	InitialReservation = 100,
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _Service, PackageSize} = ocs_rating:rate(radius, ServiceType,
			ServiceId, TS, undefined, undefined, initial, [],
			[{octets, InitialReservation}], SessionId),
	InterimReservation = PackageSize + InitialReservation,
	{ok, #service{}, _Reservation} = ocs_rating:rate(radius, ServiceType,
			ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{octets, InterimReservation}], SessionId),
	{ok, #bucket{remain_amount = 0,
			reservations = [{_, 0, Reserved, _}]}} = ocs:find_bucket(BId),
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	Reserved = F(InterimReservation).

interim_reserve_within_unit_size() ->
	[{userdata, [{doc, "Reservation amounts less than package size"}]}].

interim_reserve_within_unit_size(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 200,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	F = fun(Reserve) when (Reserve rem PackageSize) == 0 ->
			(Reserve div PackageSize) * PackagePrice;
		(Reserve) ->
			(Reserve div PackageSize + 1) * PackagePrice
	end,
	%% 1st Reservation
	Reservation1 = 100,
	{ok, #service{}, PackageSize} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [{octets, Reservation1}], SessionId),
	{ok, #bucket{remain_amount = CentsRemain1,
			reservations = [{_, 0, Reserved1, _}]}} = ocs:find_bucket(BId),
	CentsRemain1 = RemAmount - F(Reservation1),
	Reserved1 = F(Reservation1),
	%% 2nd Reservation
	Reservation2 = 300,
	{ok, #service{}, PackageSize} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined, interim,
			[], [{octets, Reservation2}], SessionId),
	{ok, #bucket{remain_amount = CentsRemain2,
			reservations = [{_, 0, Reserved2, _}]}} = ocs:find_bucket(BId),
	CentsRemain2 = RemAmount - F(Reservation2),
	Reserved2 = F(Reservation2),
	%% 3rd Reservation
	Reservation3 = 700,
	{ok, #service{}, PackageSize} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 120), undefined, undefined, interim,
			[], [{octets, Reservation3}], SessionId),
	{ok, #bucket{remain_amount = CentsRemain3,
			reservations = [{_, 0, Reserved3, _}]}} = ocs:find_bucket(BId),
	CentsRemain3 = RemAmount - F(Reservation3),
	Reserved3 = F(Reservation3).

interim_reserve_available() ->
	[{userdata, [{doc, "Reservation amount equal to balance and package size"}]}].

interim_reserve_available(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	B1 = bucket(cents, (2 * PackagePrice)),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _Service2, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, TS, undefined, undefined, initial, [],
			[{octets, PackageSize}], SessionId),
	{ok, #service{}, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [{octets, PackageSize}],
			[{octets, PackageSize}], SessionId),
	{ok, #bucket{remain_amount = 0,
			reservations = [{_, PackagePrice, PackagePrice, _}]}} =
			ocs:find_bucket(BId).

interim_reserve_out_of_credit() ->
	[{userdata, [{doc, "Out of credit on reservation"}]}].

interim_reserve_out_of_credit(_Config) ->
	UnitPrice = 100,
	UnitSize = 1000000,
	P1 = price(usage, octets, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	StartingAmount = 110,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ReserveSize = UnitSize,
	{ok, _Service2, _} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, TS, undefined, undefined, initial, [],
			[{octets, ReserveSize}], SessionId),
	DebitSize = UnitSize + 1,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{octets, DebitSize}],
			[{octets, ReserveSize}], SessionId),
	{ok, #product{balance = BucketRefs}} = ocs:find_product(ProdRef),
	RatedBuckets = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs]),
	RemainAmount = case DebitSize rem UnitSize of
		0 ->
			StartingAmount - (DebitSize div UnitSize) * UnitPrice;
		_ ->
			StartingAmount - (DebitSize div UnitSize + 1) * UnitPrice
	end,
	RemainAmount = lists:sum([R || #bucket{units = cents, remain_amount = R} <- RatedBuckets]),
	F = fun(Reservations) -> lists:sum([D || {_, D, _, _} <- Reservations]) end,
	StartingAmount = lists:sum([F(R) || #bucket{reservations = R} <- RatedBuckets]).

interim_reserve_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_reserve_remove_session(_Config) ->
	P1 = price(usage, octets, 1000, 100),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 110,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	Reservation1 = 100,
	ServiceType = 32251,
	SessionId = {'Session-Id', list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "10.0.0.1"},
	NasId = {?NasIdentifier, "rate@sigscale"},
	SessionAttributes  = [NasId, NasIp, SessionId],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId, TS,
			undefined, undefined, initial, [], [{octets, Reservation1}],
			SessionAttributes),
	Reservation2 = 1100,
	{out_of_credit, SessionList} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, initial, [], [{octets, Reservation2}],
			SessionAttributes),
	[{_, [SessionId]}] = SessionList,
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_reserve_multiple_buckets_available() ->
	[{userdata, [{doc, "Reservation with multiple buckets"}]}].

interim_reserve_multiple_buckets_available(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, 1000, 100),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 110,
	B1 = bucket(cents, RemAmount),
	_BId1 = add_bucket(ProdRef, B1),
	B2 = bucket(cents, RemAmount),
	_BId2 = add_bucket(ProdRef, B2),
	ServiceType = 32251,
	Reservation1 = 1000,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = {'Session-Id', list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "10.0.0.1"},
	NasId = {?NasIdentifier, ocs:generate_password() ++ "@sigscalelab"},
	SessionAttributes = [NasIp, NasId, SessionId],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId, TS,
			undefined, undefined, initial, [], [{octets, Reservation1}],
			SessionAttributes),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	RatedBuckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	F1 = fun(F1, Type, [#bucket{units = Type, reservations = Res} | T], R) when Res =/= [] ->
			{_, _, Reserved, _} = lists:keyfind([SessionId], 4, Res),
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
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [], [{octets, Reservation2}], SessionAttributes),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	RatedBuckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	Reserved2 = F1(F1, cents, RatedBuckets2, 0),
	Reserved2 = F2(Reservation2).

interim_reserve_multiple_buckets_out_of_credit() ->
	[{userdata, [{doc, "Out of credit with multiple cents buckets"}]}].

interim_reserve_multiple_buckets_out_of_credit(_Config) ->
	UnitPrice = 100,
	UnitSize = 1000000,
	P1 = price(usage, octets, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	StartAmount1 = 110,
	B1 = bucket(cents, StartAmount1),
	_BId1 = add_bucket(ProdRef, B1),
	StartAmount2 = 50,
	B2 = bucket(cents, StartAmount2),
	_BId2 = add_bucket(ProdRef, B2),
	ServiceType = 2,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = {'Session-Id', list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "10.0.0.1"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	SessionAttributes = [NasIp, SessionId, NasId],
	Reservation1 = UnitSize,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId, TS,
			undefined, undefined, initial, [], [{octets, Reservation1}],
			SessionAttributes),
	Reservation2 = UnitSize,
	DebitAmount = Reservation1,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{octets, DebitAmount}], [{octets, Reservation2}],
			SessionAttributes),
	{ok, #product{balance = BucketRefs}} = ocs:find_product(ProdRef),
	Buckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs]),
	StartAmount3 = StartAmount1 + StartAmount2,
	RemainAmount = case DebitAmount rem UnitPrice of
		0 ->
			StartAmount3 - (DebitAmount div UnitSize) * UnitPrice;
		_ ->
			StartAmount3 - (DebitAmount div UnitSize + 1) * UnitPrice
	end,
	RemainAmount = lists:foldl(fun(#bucket{remain_amount = A}, Acc) -> A + Acc end, 0, Buckets2).

interim_debit_exact_balance() ->
	[{userdata, [{doc, "Debit amount equal to package size"}]}].

interim_debit_exact_balance(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{octets, PackageSize}], [], SessionId),
	{ok, #product{balance = BucketRefs}} = ocs:find_product(ProdRef),
	RatedBuckets = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs]),
	#bucket{remain_amount = 0, reservations = Reservations} =
			lists:keyfind(cents, #bucket.units, RatedBuckets),
	F = fun(F, [{_, D, _, _} | T], Debit) ->
				F(F, T, D + Debit);
			(_F, [], Debit) ->
				Debit
	end,
	PackagePrice = F(F, Reservations, 0).

interim_debit_under_unit_size() ->
	[{userdata, [{doc, "Debit amount less than package size"}]}].

interim_debit_under_unit_size(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 200,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Debit = 100,
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [], SessionId),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{octets, Debit}], [], SessionId),
	{ok, #product{balance = BucketRefs}} = ocs:find_product(ProdRef),
	RatedBuckets = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs]),
	#bucket{remain_amount = CentsRemain} = lists:keyfind(cents, #bucket.units, RatedBuckets),
	CentsRemain = RemAmount - PackagePrice.

interim_debit_out_of_credit() ->
	[{userdata, [{doc, "Insufficient amount to debit"}]}].

interim_debit_out_of_credit(_Config) ->
	AccountAmount = 200,
	PackageAmount = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackageAmount),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	B1 = bucket(cents, AccountAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _Service1, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, TS, undefined, undefined, initial, [], [{octets, PackageSize}],
			SessionId),
	{ok, _Service2, PackageSize} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [{octets, PackageSize}],
			[{octets, PackageSize}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, interim, [{octets, PackageSize}],
			[{octets, PackageSize}], SessionId),
	{ok, #bucket{reservations = Reservations}} = ocs:find_bucket(BId),
	[{_, AccountAmount, 0, _}] = Reservations.

interim_debit_remove_session() ->
	[{userdata, [{doc, "Out of credit remove session attributes from subscriber record"}]}].

interim_debit_remove_session(_Config) ->
	P1 = price(usage, octets, 1000, 100),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 10,
	_BId = add_bucket(ProdRef, bucket(cents, RemAmount)),
	SessionAttributes = [{erlang:system_time(?MILLISECOND), [{?AcctSessionId, "1020303"},
		{?NasIdentifier, "rate@sigscale"}, {?NasIpAddress, "10.0.0.1"}]}],
	F = fun() ->
		[Service] = mnesia:read(service, list_to_binary(ServiceId), read),
		mnesia:write(Service#service{session_attributes = SessionAttributes})
	end,
	{atomic, ok} = mnesia:transaction(F),
	ServiceType = 32251,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	Debit = 100,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType,
			ServiceId, Timestamp, undefined, undefined, interim,
			[{octets, Debit}], [], SessionId),
	{ok, #service{session_attributes = []}} = ocs:find_service(ServiceId).

interim_debit_and_reserve_available() ->
	[{userdata, [{doc, "Debit given usage and check for reservation, sufficient balance exists"}]}].

interim_debit_and_reserve_available(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 300,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Debit = 1000,
	ServiceType = 32251,
	Reservation = 1000,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [{octets, Reservation}],
			SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	RatedBuckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
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
	{_, 0, Reservation1, _} = lists:keyfind(SessionId, 4, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{octets, Debit}], [{octets, Reservation}],
			SessionId),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	RatedBuckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} =
			lists:keyfind(cents, #bucket.units, RatedBuckets2),
	CentsRemain2 = CentsRemain1 - F1(Debit),
	{_, Debit2, Reservation2, _} = lists:keyfind(SessionId, 4, Reservations2),
	Debit2 = F1(Debit),
	Reservation2 = F2(Debit, Reservation, Reservation1).

interim_debit_and_reserve_insufficient1() ->
	[{userdata, [{doc, "Debit amount less than package size and reservation amount
			less than available balance, sufficient balance exists"}]}].

interim_debit_and_reserve_insufficient1(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 201,
	_BId = add_bucket(ProdRef, bucket(cents, RemAmount)),
	Debit = 100,
	Reservation = 100,
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId, TS,
			undefined, undefined, initial, [], [{octets, Reservation}], SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	RatedBuckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = CentsRemain1,
			reservations = Reservations1} = lists:keyfind(cents,
			#bucket.units, RatedBuckets1),
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
	{_, 0, Reservation1, _} = lists:keyfind(SessionId, 4, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, interim, [{octets, Debit}], [{octets, Reservation}],
			SessionId),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	RatedBuckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	#bucket{remain_amount = CentsRemain2, reservations = Reservations2} =
			lists:keyfind(cents, #bucket.units, RatedBuckets2),
	CentsRemain2 = CentsRemain1 - F1(Debit),
	{_, Debit2, Reservation2, _} = lists:keyfind(SessionId, 4, Reservations2),
	Debit2 = F1(Debit),
	Reservation2 = F2(Debit, Reservation, Reservation1).

interim_debit_and_reserve_insufficient2() ->
	[{userdata, [{doc, "Debit amount equal to unit size and
			reservation amount greater than available balance"}]}].

interim_debit_and_reserve_insufficient2(_Config) ->
	UnitPrice = 100,
	UnitSize = 1000000,
	P1 = price(usage, octets, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	StartAmount = 199,
	B1 = bucket(cents, StartAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [{octets, UnitSize}], SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{octets, UnitSize}], [{octets, UnitSize}], SessionId),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	RatedBuckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	RemainAmount = StartAmount - UnitPrice,
	#bucket{remain_amount = RemainAmount} = lists:keyfind(cents, #bucket.units, RatedBuckets2).

interim_debit_and_reserve_insufficient3() ->
	[{userdata, [{doc, "Suffient balance for debit but not reservation"}]}].

interim_debit_and_reserve_insufficient3(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 200,
	B1 = bucket(cents, RemAmount),
	BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	Debit = 1500,
	Reservation = 1000,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [{octets, Reservation}], SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	RatedBuckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
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
	{_, 0, Reservation1, _} = lists:keyfind(SessionId, 4, Reservations1),
	Reservation1 = F2(0, Reservation, 0),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #bucket{reservations = [InterimReservation]}} = ocs:find_bucket(BId),
	element(2, InterimReservation) == F1(Debit).

interim_debit_and_reserve_insufficient4() ->
	[{userdata, [{doc, "Insuffient amount for debit and reservation"}]}].

interim_debit_and_reserve_insufficient4(_Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = price(usage, octets, PackageSize, PackagePrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	RemAmount = 150,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	Debit = 2500,
	Reservation = 1000,
	ServiceType = 2,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [{octets, Reservation}],
			SessionId),
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{octets, Debit}], [{octets, Reservation}], SessionId),
	{ok, #product{balance = BucketRefs}} = ocs:find_product(ProdRef),
	RatedBuckets = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs]),
	-150 = lists:sum([R || #bucket{remain_amount = R, units = cents} <- RatedBuckets]),
	F = fun(Res1) -> lists:sum([D || {_, D, _, _} <- Res1]) end,
	RemAmount = lists:sum([F(R) || #bucket{reservations = R, units = cents} <- RatedBuckets]).

interim_out_of_credit_voice() ->
	[{userdata, [{doc, "Voice call out of credit during call"}]}].

interim_out_of_credit_voice(_Config) ->
	UnitPrice = 10,
	UnitSize = 60,
	P1 = price(usage, seconds, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	StartingAmount = 11,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ReserveUnits = 60,
	ServiceType = 32260,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [{seconds, ReserveUnits}],
			SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = Amount1} = lists:keyfind(cents, #bucket.units, Buckets1),
	ReservedUnits = case (ReserveUnits rem UnitSize) of
		0 ->
			ReserveUnits div UnitSize;
		_ ->
			ReserveUnits div UnitSize + 1
	end,
	Amount1 = StartingAmount - ReservedUnits * UnitPrice,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined, undefined,
			interim, [{seconds, ReserveUnits}], [{seconds, ReserveUnits}], SessionId),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	Buckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	#bucket{remain_amount = Amount2, reservations = [InterimReservation]}
			= lists:keyfind(cents, #bucket.units, Buckets2),
	UnitPrice = element(2, InterimReservation),
	Amount2 = StartingAmount - ReservedUnits * UnitPrice.

final_remove_session() ->
	[{userdata, [{doc, "Final call remove session attributes from subscriber record"}]}].

final_remove_session(_Config) ->
	P1 = price(usage, octets, 1000, 100),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
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
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			TS, undefined, undefined, initial, [], [], [SA2]),
	{ok, _, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60), undefined,
			undefined, final, [{octets, Debit}], [], [SA2]),
	{ok, #service{session_attributes = [SA1]}} = ocs:find_service(ServiceId).

final_refund() ->
	[{userdata, [{doc, "Refund unused amount of reservation"}]}].

final_refund(_Config) ->
	UnitPrice = 100,
	UnitSize = 1000000,
	P1 = price(usage, octets, UnitSize, UnitPrice),
	OfferId = add_offer([P1], 4),
	ProdRef = add_product(OfferId),
	ServiceId =  add_service(ProdRef),
	StartingAmount = UnitPrice,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 32251,
	TS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SessionId1 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId, TS,
			undefined, undefined, initial, [], [{octets, UnitSize}], SessionId1),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	RatedBuckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = 0, reservations = Reserved1} =
			lists:keyfind(cents, #bucket.units, RatedBuckets1),
	[{_, _, UnitPrice, SessionId1}] = Reserved1,
	{ok, _, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, final, [{octets, 0}], [], SessionId1),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	RatedBuckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	#bucket{remain_amount = UnitPrice, reservations = []} =
			lists:keyfind(cents, #bucket.units, RatedBuckets2),
	SessionId2 = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
			calendar:gregorian_seconds_to_datetime(TS + 60),
			undefined, undefined, initial, [], [{octets, UnitSize}], SessionId2),
	{ok, #product{balance = BucketRefs3}} = ocs:find_product(ProdRef),
	RatedBuckets3 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs3]),
	#bucket{remain_amount = 0, reservations = Reserved2} = lists:keyfind(cents, #bucket.units, RatedBuckets3),
	[{_, 0, UnitPrice, SessionId2}] = Reserved2.

final_voice() ->
	[{userdata, [{doc, "Final RADIUS accounting request for voice call"}]}].

final_voice(_Config) ->
	UnitPrice = 5,
	UnitSize = 60,
	ReserveTime = 300,
	P1 = #price{name = "Calls", type = usage,
			units = seconds, size = UnitSize, amount = UnitPrice,
			char_value_use = [#char_value_use{name = "radiusReserveTime",
			min = 1, max = 1, values = [#char_value{default = true,
			units = seconds, value = ReserveTime}]}]},
	Chars = [{"radiusReserveSessionTime", 3600}],
	OfferId = add_offer([P1], 9),
	ProdRef = add_product(OfferId, Chars),
	ServiceId =  add_service(ProdRef),
	StartingAmount = 305,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	AcctSessionId = {?AcctSessionId, list_to_binary(ocs:generate_password())},
	NasIp = {?NasIpAddress, "192.168.1.150"},
	NasId = {?NasIdentifier, ocs:generate_password()},
	Attributes = [NasIp, NasId, AcctSessionId],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, originate, initial, [], [], Attributes),
	UsedSeconds1 = rand:uniform(ReserveTime),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, originate, interim,
			[], [{seconds, UsedSeconds1}], Attributes),
	UsedSeconds2 = rand:uniform(ReserveTime),
	{ok, _, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, undefined, final,
			[{seconds, UsedSeconds1 + UsedSeconds2}], [], Attributes),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = RemainAmount,
			reservations = []} = lists:keyfind(cents, #bucket.units, Buckets1),
	UsedUnits = case (UsedSeconds1 + UsedSeconds2) rem UnitSize of
		0 ->
			(UsedSeconds1 + UsedSeconds2) div UnitSize;
		_ ->
			(UsedSeconds1 + UsedSeconds2) div UnitSize + 1
	end,
	RemainAmount = StartingAmount - UsedUnits * UnitPrice.

reserve_data() ->
	[{userdata, [{doc, "Reservation for data session"}]}].

reserve_data(_Config) ->
	DataAmount = 2,
	DataSize = 1000000,
	ReserveOctets = 1000000000,
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
	ServiceId =  add_service(ProdRef),
	StartingAmount = 2579,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 2,
	Timestamp = calendar:local_time(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, undefined, undefined, initial, [], [], SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = Amount} = lists:keyfind(cents, #bucket.units, Buckets1),
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
	DataSize = 1000000,
	ReserveOctets = 1000000000,
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
	ServiceId =  add_service(ProdRef),
	StartingAmount = 2567,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, undefined, initial, [], [], SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = Amount} = lists:keyfind(cents, #bucket.units, Buckets1),
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
	DataSize = 1000000,
	ReserveOctets = 1000000000,
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
	ServiceId =  add_service(ProdRef),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, answer, initial, [], [], SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = Amount} = lists:keyfind(cents, #bucket.units, Buckets1),
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
	DataAmount = 2,
	DataSize = 1000000,
	ReserveOctets = 1000000000,
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
	BundleOffer = #offer{name = BundleOfferId,
			bundle = [#bundled_po{name = DataOfferId},
					#bundled_po{name = VoiceOfferId}]},
	{ok, _} = ocs:add_offer(BundleOffer),
	StartingAmount = 1000,
	ProdRef = add_product(BundleOfferId),
	ServiceId = add_service(ProdRef),
	B1 = bucket(cents, StartingAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, undefined, initial, [], [], SessionId),
	UsedOctets = rand:uniform(ReserveOctets),
	UsedSeconds = rand:uniform(ReserveTime),
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp, CallAddress, undefined, interim, [],
			[{octets, UsedOctets}, {seconds, UsedSeconds}], SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = Amount} = lists:keyfind(cents, #bucket.units, Buckets1),
	ReservedUnits = case ((ReserveTime + UsedSeconds) rem VoiceSize) of
		0 ->
			(ReserveTime + UsedSeconds) div VoiceSize;
		_ ->
			(ReserveTime + UsedSeconds) div VoiceSize + 1
	end,
	Amount = StartingAmount - ReservedUnits * VoiceAmount.

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
	_BId = add_bucket(ProdRef, B1),
	ServiceId = add_service(ProdRef),
	ServiceType = 2,
	{Date, _} = calendar:local_time(),
	Timestamp1 = {Date, {7, 59, 59}},
	SessionId1 = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp1, undefined, undefined, initial, [], [], SessionId1),
	UsedOctets1 = rand:uniform(DataSize * 6),
	{ok, _, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp1, undefined, undefined, final,
			[{octets, UsedOctets1}], [], SessionId1),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = Amount1} = lists:keyfind(cents, #bucket.units, Buckets1),
	UsedUnits1 = case UsedOctets1 rem DataSize of
		0 ->
			UsedOctets1 div DataSize;
		_ ->
			UsedOctets1 div DataSize + 1
	end,
	Amount1 = StartingAmount - UsedUnits1 * OffPeakAmount,
	Timestamp2 = {Date, {12, 13, 14}},
	SessionId2 = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{ok, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp2, undefined, undefined, initial, [], [], SessionId2),
	UsedOctets2 = rand:uniform(DataSize * 6),
	{ok, _, _, _} = ocs_rating:rate(radius, ServiceType, ServiceId,
			Timestamp2, undefined, undefined, final,
			[{octets, UsedOctets2}], [], SessionId2),
	{ok, #product{balance = BucketRefs2}} = ocs:find_product(ProdRef),
	Buckets2 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs2]),
	#bucket{remain_amount = Amount2} = lists:keyfind(cents, #bucket.units, Buckets2),
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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	_BId = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, 60} = lists:keyfind(?SessionTimeout, 1, Attr),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = RemAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	RemAmount = 20,
	B1 = bucket(cents, RemAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = RemAmount * PackageSize,
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = RemAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, answer, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = StartingAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, originate, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = StartingAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	StartingAmount = 1000,
	B1 = bucket(cents, StartingAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 12,
	Timestamp = calendar:local_time(),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, RespAttr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, CallAddress, undefined, SessionId),
	{?SessionTimeout, ReserveTime} = lists:keyfind(?SessionTimeout, 1, RespAttr),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = StartingAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefined, SessionId),
	{?SessionTimeout, 60} = lists:keyfind(?SessionTimeout, 1, Attr),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = RemAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, _Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefined, SessionId),
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{units = cents, remain_amount = RemAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = 20,
	B1 = bucket(cents, RemAmount),
	_Bid = add_bucket(ProdRef, B1),
	ServiceType = 2,
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	{authorized, _, Attr, _} = ocs_rating:authorize(radius, ServiceType,
			ServiceId, Password, Timestamp, undefined, undefibed, SessionId),
	{?SessionTimeout, SessionTimeout} = lists:keyfind(?SessionTimeout, 1, Attr),
	SessionTimeout = RemAmount * PackageSize,
	{ok, #product{balance = BucketRefs1}} = ocs:find_product(ProdRef),
	Buckets1 = lists:flatten([mnesia:dirty_read(bucket, Id)
			|| Id <- BucketRefs1]),
	#bucket{remain_amount = RemAmount} = lists:keyfind(cents, #bucket.units, Buckets1).

authorize_negative_balance() ->
	[{userdata, [{doc, "Handle negative balance and deny"}]}].

authorize_negative_balance(_Config) ->
	P1 = price(usage, octets, 1000, 1),
	OfferId = add_offer([P1], 9),
	Chars = [{"radiusReserveSessionTime", 60}],
	ProdRef = add_product(OfferId, Chars),
	ServiceId = list_to_binary(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	B1 = bucket(cents, -100),
	_Bid = add_bucket(ProdRef, B1),
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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	RemAmount = 100,
	B1 = bucket(cents, RemAmount),
	_Bid = add_bucket(ProdRef, B1),
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
	{ok, _Service1} = ocs:add_service(ServiceId, Password,  ProdRef, []),
	Timestamp = calendar:local_time(),
	B1 = bucket(octets, 100),
	_Bid = add_bucket(ProdRef, B1),
	CallAddress = ocs:generate_identity(),
	SessionId = [{?AcctSessionId, list_to_binary(ocs:generate_password())}],
	ServiceType = 12,
	{unauthorized, out_of_credit, []} = ocs_rating:authorize(radius,
			ServiceType, ServiceId, Password, Timestamp,
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
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
		Timestamp, {127,0,0,1}, undefined, initial, [], [{messages, NumOfEvents}],
		SessionId),
	{ok, #bucket{remain_amount = R1, reservations = Rs1}} = ocs:find_bucket(BId),
	R1 = RemAmount - (PackagePrice * NumOfEvents),
	[{_, _, Reserved, _}] = Rs1,
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
	{ok, _, _} = ocs_rating:rate(diameter, ServiceType, ServiceId,
		Timestamp, undefined, undefined, initial, [], [{messages, NumOfEvents}],
		SessionId),
	{ok, #bucket{remain_amount = R1, reservations = Rs1}} = ocs:find_bucket(BId),
	R1 = RemAmount - (PackagePrice * NumOfEvents),
	[{_, _, Reserved, _}] = Rs1,
	Reserved = PackagePrice * NumOfEvents,
	{ok, _, _, Rated} = ocs_rating:rate(diameter, ServiceType, ServiceId,
		Timestamp, undefined, undefined, final, [{messages, NumOfEvents}], [],
		SessionId),
	{ok, #bucket{remain_amount = R2, reservations = []}} = ocs:find_bucket(BId),
	[#rated{bucket_type = messages, bucket_value = NumOfEvents}] = Rated,
	R2 = RemAmount - (PackagePrice * NumOfEvents).

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
		termination_date = erlang:system_time(?MILLISECOND) + 2592000000}.

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
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, Chars),
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

