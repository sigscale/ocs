%%% ocs_api_SUITE.erl
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
%%%  @doc Test suite for public API of the {@link //ocs. ocs} application.
%%%
-module(ocs_api_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("inets/include/mod_auth.hrl").

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
	[{userdata, [{doc, "Test suite for public API in OCS"}]},
	{require, radius_shared_secret}, {default_config, radius_shared_secret, "abc345"},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	{ok, ProdID} = ocs_test_lib:add_offer(),
	[{product_id, ProdID} | Config].

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
	[client, get_all_clients, update_client_password, delete_client,
	add_service, delete_service, add_offer, find_offer, get_offers,
	delete_offer, add_user, get_user, delete_user, add_bucket,
	find_bucket, delete_bucket, get_buckets, positive_adjustment,
	negative_adjustment_high, negative_adjustment_equal, negative_adjustment_low,
	add_product, find_product, delete_product, query_product, add_offer_event,
	delete_offer_event, gtt_insert_event, gtt_delete_event, add_resource_event,
	delete_pla_event, add_service_event, delete_service_event,
	add_product_event, delete_product_event, add_bucket_event,
	delete_bucket_event, product_charge_event, rating_deleted_bucket_event,
	accumulated_balance_event, add_policy_table, delete_policy_table,
	add_policy, get_policy, get_policies, delete_policy,
	add_resource, get_resources, get_resource, delete_resource].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

client() ->
	[{userdata, [{doc, "Add client to database"}]}].

client(Config) ->
	{ok, [{auth, AuthInstance}, {acct, _AcctInstance}]} = application:get_env(ocs, radius),
	Address = case AuthInstance of
		[{{0, 0, 0, 0}, _, _}] ->
			{127, 0, 0, 1};
		[{Addr, _, _}] ->
			Addr
	end,
	SharedSecret = ct:get_config(radius_shared_secret, Config),
	Protocol = ct:get_config(protocol),
	{ok, _} = ocs:add_client(Address, 3799, Protocol, SharedSecret, true),
	{ok, #client{port = 3799, protocol = Protocol,
			secret = BinSharedSecret}} = ocs:find_client(Address),
	SharedSecret = binary_to_list(BinSharedSecret).

get_all_clients() ->
	[{userdata, [{doc, "Retrieve  all clients from  database"}]}].

get_all_clients(_Config) ->
	A1 = {10,2,45,67},
	A2 = {10,2,45,68},
	A3 = {10,2,45,69},
	Secret1 = "Enid blyton 1",
	Secret2 = "Enid blyton 2",
	Secret3 = "Enid blyton 3",
	Protocol = ct:get_config(protocol),
	{ok, _} = ocs:add_client(A1, 3799, Protocol, Secret1, true),
	{ok, _} = ocs:add_client(A2, 3799, Protocol, Secret2, true),
	{ok, _} = ocs:add_client(A3, 13799, Protocol, Secret3, true),
	Clients = ocs:get_clients(),
	F = fun(#client{address = A, port = LP, protocol = P, secret = S} = _R) ->
		{ok, #client{port = LP, protocol = P, secret = S}} = ocs:find_client(A)
	end,
	lists:foreach(F, Clients).

update_client_password() ->
	[{userdata, [{doc, "Update password in client record in database"}]}].

update_client_password(_Config) ->
	Address = "192.168.90.23",
	Password = "gentoo",
	Protocol = ct:get_config(protocol),
	{ok, _} = ocs:add_client(Address, 3799, Protocol, Password, true),
	PasswordBin = list_to_binary(Password),
	{ok, #client{port = 3799, protocol = Protocol,
			secret = PasswordBin}} = ocs:find_client(Address),
	NewPassword = "GentooNewxD",
	ok = ocs:update_client(Address, NewPassword),
	NewPasswordBin = list_to_binary(NewPassword),
	{ok, #client{port = 3799, protocol = Protocol,
			secret = NewPasswordBin}} = ocs:find_client(Address).


delete_client() ->
	[{userdata, [{doc, "Delete  a client from database"}]}].

delete_client(Config) ->
	{ok, [{auth, AuthInstance}, {acct, _AcctInstance}]} = application:get_env(ocs, radius),
	Address = case AuthInstance of
		[{{0, 0, 0, 0}, _, _}] ->
			{127, 0, 0, 1};
		[{Addr, _, _}] ->
			Addr
	end,
	SharedSecret = ct:get_config(radius_shared_secret, Config),
	Protocol = ct:get_config(protocol),
	{ok, _} = ocs:add_client(Address, 3799, Protocol, SharedSecret, true),
	ok = ocs:delete_client(Address),
	{error, not_found} = ocs:find_client(Address).

add_service() ->
	[{userdata, [{doc, "Add service to database"}]}].

add_service(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:add(?SessionTimeout, 3600, Attribute0),
	Attribute2 = radius_attributes:add(?AcctInterimInterval, 60, Attribute0),
	MSISDN = ocs:generate_identity(),
	IMSI = "001001" ++ ocs:generate_identity(),
	Password = ocs:generate_password(),
	K = crypto:strong_rand_bytes(16),
	OPc = crypto:strong_rand_bytes(16),
	Credentials = #aka_cred{k = K, opc = OPc},
	{ok, _} = ocs:add_service(MSISDN, Password, undefined, [], Attribute1),
	{ok, _} = ocs:add_service(IMSI, Credentials, undefined, [], Attribute2),
	{ok, #service{name = BinMSISDN, password = BinPassword,
			attributes = Attribute1}} = ocs:find_service(MSISDN),
	MSISDN = binary_to_list(BinMSISDN),
	Password = binary_to_list(BinPassword),
	{ok, #service{password = #aka_cred{k = K, opc = OPc},
			attributes = Attribute2}} = ocs:find_service(IMSI).

delete_service() ->
	[{userdata, [{doc, "Delete service from the database"}]}].

delete_service(Config) ->
	OfferId = ?config(product_id, Config),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:add(?SessionTimeout, 3600, Attribute0),
	ServiceId = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, _} = ocs:add_service(ServiceId, Password, ProdRef, Attribute),
	{ok, _} = ocs:find_service(ServiceId),
	{ok, #product{service = [ServiceRef]}} = ocs:find_product(ProdRef),
	ServiceRef = list_to_binary(ServiceId),
	ok = ocs:delete_service(ServiceId),
	{error, _} = ocs:find_service(ServiceId),
	{ok, #product{service = []}} = ocs:find_product(ProdRef).

add_offer() ->
	[{userdata, [{doc, "Add a product offering."}]}].

add_offer(_Config) ->
	SD = erlang:system_time(?MILLISECOND),
	Price1 = #price{name = "Installation",
			description = "One time installation fee.",
			start_date = SD, type = one_time, amount = 2995},
	Price2 = #price{name = "Subscription",
			description = "Monthly package subscription charge.",
			start_date = SD, type = recurring, period = monthly,
			amount = 1250, alteration = #alteration{name = "Allowance",
					start_date = SD, type = usage, units = octets,
					size = 100000000000, amount = 0}},
	Price3 = #price{name = "Overage",
			description = "Usage over and above allowance.",
			start_date = SD, type = usage, size = 1000000000,
			amount = 100, units = octets},
	Prices = [Price1, Price2, Price3],
	Product = #offer{name = "Silver Surfer",
			description = "Medium use residential subscription.",
			start_date = SD, status = active,
			price = Prices},
	{ok, _Product1} = ocs:add_offer(Product).

find_offer() ->
	[{userdata, [{doc, "Find a product offering."}]}].

find_offer(_Config) ->
	SD = erlang:system_time(?MILLISECOND),
	ED = SD + 108000000,
	Price1 = #price{name = "P1", description = "D1",
			start_date = SD, end_date = ED,
			type = recurring, currency = "C1",
			period = monthly, amount = 1330},
	Price2 = #price{name = "P2", description = "D2",
			start_date = SD, end_date = ED,
			type = usage, currency = "C2",
			size = 1000000, amount = 6, units = octets,
			alteration = #alteration{name = "A2",
					start_date = SD, end_date = ED,
					type = one_time, units = octets,
					size = 150000000, amount = 0}},
	Prices = [Price1, Price2],
	ProductName = "PD1",
	Product = #offer{name = ProductName, description = "PDD1",
			start_date = SD, end_date = ED, status = active,
			price = Prices},
	{ok, _Product1} = ocs:add_offer(Product),
	{ok, #offer{name = ProductName}} = ocs:find_offer(ProductName).

get_offers() ->
	[{userdata, [{doc, "Get all products from product table database"}]}].

get_offers(_Config) ->
	F1 = fun(_, 0, Acc) ->
				Acc;
			(F, N, Acc) ->
		ProductName = ocs:generate_password(),
		Price1 = #price{name = ocs:generate_password(),
				type = recurring, period = daily, amount = 1330},
		Price2 = #price{name = ocs:generate_password(),
				type = usage, size = 1000000, amount = 6, units = octets,
				alteration = #alteration{name = ocs:generate_password(),
						type = one_time, units = octets,
						size = 150000000, amount = 0}},
		Prices = [Price1, Price2],
		Product = #offer{name = ProductName,
				status = active, price = Prices},
		{ok, _Product1} =  ocs:add_offer(Product),
		F(F, N - 1, [ProductName | Acc])
	end,
	NewProducts = F1(F1, 3, []),
	[] = NewProducts -- [Name || #offer{name = Name} <- ocs:get_offers()].

delete_offer() ->
	[{userdata, [{doc, "Remove a product from product table"}]}].

delete_offer(_Config) ->
	Price1 = #price{name = "Daily price",
			type = recurring, period = daily, amount = 330},
	Price2 = #price{name = "Monthly price", units = octets,
			type = usage, size = 1000000, amount = 6},
	Prices = [Price1, Price2],
	ProductName = "Mobile-Internet",
	Product = #offer{name = ProductName,
			description = "Monthly subscription for mobile internet",
			status = active, price = Prices},
	{ok, _Product1} = ocs:add_offer(Product),
	ok = ocs:delete_offer(ProductName),
	{error, not_found} = ocs:find_offer(ProductName).

ignore_delete_offer() ->
	[{userdata, [{doc, "ignore remove offer if exist product
			inventory related to offer reference"}]}].

ignore_delete_offer(_Config) ->
	Price1 = #price{name = ocs:generate_identity(),
			type = recurring, period = daily, amount = rand:uniform(500)},
	Price2 = #price{name = ocs:generate_identity(), units = octets,
			type = usage, size = rand:uniform(1000000), amount = rand:uniform(100)},
	Prices = [Price1, Price2],
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId,
			description = "Monthly subscription for mobile internet",
			status = active, price = Prices},
	{ok, _Offer} = ocs:add_offer(Offer),
	{ok, #product{}} = ocs:add_product(OfferId, [], []),
	{'EXIT', unable_to_delete} = (catch ocs:delete_offer(OfferId)),
	{ok, #product{}} = ocs:find_offer(OfferId).

add_user() ->
	[{userdata, [{doc, "Create a new user"}]}].

add_user(_Config) ->
	User = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Locale = "en",
	{ok, {TS, N}} = ocs:add_user(User, Password, Locale),
	true = is_integer(TS),
	true = is_integer(N),
	{Port, Address, Dir, _} = get_params(),
	{ok, #httpd_user{username = User, password = Password,
			user_data = UserData}} = mod_auth:get_user(User, Address, Port, Dir),
	{_, Locale} = lists:keyfind(locale, 1, UserData),
	{_, {_, _}} = lists:keyfind(last_modified, 1, UserData).

get_user() ->
	[{userdata, [{doc, "Look up a user from table"}]}].

get_user(_Config) ->
	User = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Locale = "en",
	{ok, LastModified} = ocs:add_user(User, Password, Locale),
	{ok, #httpd_user{username = User, password = Password,
			user_data = UserData}} = ocs:get_user(User),
	{_, Locale} = lists:keyfind(locale, 1, UserData),
	{_, LastModified} = lists:keyfind(last_modified, 1, UserData).

delete_user() ->
	[{userdata, [{doc, "Remove user from table"}]}].

delete_user(_Config) ->
	User = ocs:generate_identity(),
	Password = ocs:generate_password(),
	Locale = "en",
	{ok, _} = ocs:add_user(User, Password, Locale),
	{ok, _} = ocs:get_user(User),
	ok = ocs:delete_user(User),
	{error, no_such_user} = ocs:get_user(User).

add_product() ->
	[{userdata, [{doc, "Add new product inventory"}]}].

add_product(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), units = octets,
			type = usage, size = rand:uniform(10000), amount = rand:uniform(100)},
	Prices = [Price1],
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId,
			status = active, price = Prices},
	{ok, _Offer1} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef} = P} = ocs:add_product(OfferId, []),
	{atomic, [P]} = mnesia:transaction(fun() -> mnesia:read(product, ProdRef, read) end).

find_product() ->
	[{userdata, [{doc, "Lookup product with given product reference"}]}].

find_product(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), units = octets,
			type = usage, size = rand:uniform(10000), amount = rand:uniform(100)},
	Prices = [Price1],
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId,
			status = active, price = Prices},
	{ok, _Offer1} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	{ok, #product{}} = ocs:find_product(ProdRef).

delete_product() ->
	[{userdata, [{doc, "Remove product from table"}]}].

delete_product(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), units = octets,
			type = usage, size = rand:uniform(10000), amount = rand:uniform(100)},
	Prices = [Price1],
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId,
			status = active, price = Prices},
	{ok, _Offer1} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	{ok, #product{}} = ocs:find_product(ProdRef),
	ok = ocs:delete_product(ProdRef),
	{error, not_found} = ocs:find_product(ProdRef).

query_product() ->
	[{userdata, [{doc, "Query product"}]}].

query_product(_Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Price1 = #price{name = ocs:generate_identity(), units = octets,
						type = usage, size = rand:uniform(10000), amount = rand:uniform(100)},
				Prices = [Price1],
				OfferId = ocs:generate_identity(),
				Offer = #offer{name = OfferId,
						status = active, price = Prices},
				{ok, _Offer1} = ocs:add_offer(Offer),
				{ok, P} = ocs:add_product(OfferId, []),
				P1 = P#product{service = [list_to_binary(ocs:generate_identity())
						|| _ <- lists:seq(1, 5)]},
				mnesia:dirty_write(product, P1),
				F(N -1, [P1 | Acc])
	end,
	Products = F(rand:uniform(1000), []),
	#product{id = Id, product = Offer,
			service = Services} = lists:nth(rand:uniform(length(Products)), Products),
	{_, [#product{id = Id, product = Offer,
			service = Services}]} = ocs:query_product(start, {like, Id},{like, Offer},
					{like, binary_to_list(lists:nth(rand:uniform(length(Services)), Services))}).

ignore_delete_product() ->
	[{userdata, [{doc, "ignore remove product if exist service
			related to product reference"}]}].

ignore_delete_product(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), units = octets,
			type = usage, size = rand:uniform(10000), amount = rand:uniform(100)},
	Prices = [Price1],
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId,
			status = active, price = Prices},
	{ok, _Offer1} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	{ok, #product{}} = ocs:find_product(ProdRef),
	{ok, #service{}} = ocs:add_service(ocs:generate_identity(),
			ocs:generate_password(), ProdRef, []),
	{'EXIT', service_exists} = (catch ocs:delete_product(ProdRef)),
	{ok, #product{}} = ocs:find_product(ProdRef).

add_bucket() ->
	[{userdata, [{doc, "Add new bucket"}]}].

add_bucket(_Config) ->
	Price1 = #price{name = ocs:generate_identity(),
			type = usage, units = octets,
			size = rand:uniform(100000000),
			amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets,
			remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket1),
	{atomic, [B1]} = mnesia:transaction(fun() -> mnesia:read(bucket, BId, read) end),
	true = B1#bucket.remain_amount == Bucket1#bucket.remain_amount,
	true = B1#bucket.units == Bucket1#bucket.units.

find_bucket() ->
	[{userdata, [{doc, "Lookup existing bucket"}]}].

find_bucket(_Config) ->
	Price1 = #price{name = ocs:generate_identity(),
			type = usage, units = octets,
			size = rand:uniform(100000000),
			amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets,
			remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket1),
	{ok, #bucket{}} = ocs:find_bucket(BId).

get_buckets() ->
	[{userdata, [{doc, "Get all the buckets for given Product Reference"}]}].

get_buckets(_Config) ->
	Price1 = #price{name = ocs:generate_identity(),
			type = usage, units = octets,
			size = rand:uniform(100000000),
			amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	AddBuckets = fun() ->
			B1 = #bucket{units = octets,
			remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
			{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, B1),
			BId
	end,
	BIds = [AddBuckets() || _ <- lists:seq(1, 100)],
	Buckets = ocs:get_buckets(ProdRef),
	[] = BIds -- [B2#bucket.id || B2 <- Buckets].

delete_bucket() ->
	[{userdata, [{doc, "Delete bucket form table"}]}].

delete_bucket(_Config) ->
	Price1 = #price{name = ocs:generate_identity(),
			type = usage, units = octets,
			size = rand:uniform(100000000),
			amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets,
			remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket1),
	{ok, #bucket{}} = ocs:find_bucket(BId),
	{ok, #product{balance = [BId]}} = ocs:find_product(ProdRef),
	ok = ocs:delete_bucket(BId),
	{error, not_found} = ocs:find_bucket(BId),
	{ok, #product{balance = []}} = ocs:find_product(ProdRef).

positive_adjustment() ->
	[{userdata, [{doc, "Applying positive adjustment"}]}].

positive_adjustment(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = rand:uniform(100000000), amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1], specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets, remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BucketRef1}} = ocs:add_bucket(ProdRef, Bucket1),
	Amount = rand:uniform(10000000),
	Units = cents,
	Adjustment = #adjustment{units = Units, amount = Amount,
			start_date = erlang:system_time(?MILLISECOND), product = ProdRef,
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	ok = ocs:adjustment(Adjustment),
	{atomic, [#product{balance = BucketRefs}]} = mnesia:transaction(fun() ->
			mnesia:read(product, ProdRef, read) end),
	[BucketRef2] = BucketRefs -- [BucketRef1],
	{ok, #bucket{units = cents,
			remain_amount = Amount}} = ocs:find_bucket(BucketRef2).

negative_adjustment_high() ->
	[{userdata, [{doc, "Applying high negative adjustment"}]}].

negative_adjustment_high(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = rand:uniform(100000000), amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets, remain_amount = 1000,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket1),
	Bucket2 = #bucket{units = octets, remain_amount = 3000,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket2),
	Adjustment = #adjustment{units = octets, amount = -5000,
			start_date = erlang:system_time(?MILLISECOND), product = ProdRef,
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	ok = ocs:adjustment(Adjustment),
	{atomic, [#product{balance = [BId]}]} = mnesia:transaction(fun() ->
			mnesia:read(product, ProdRef, read) end),
	{atomic, [Bucket3]} = mnesia:transaction(fun() -> mnesia:read(bucket, BId, read) end),
	octets = Bucket2#bucket.units,
	1000 + 3000 - 5000 = Bucket3#bucket.remain_amount.

negative_adjustment_equal() ->
	[{userdata, [{doc, "Applying equal negative adjustment"}]}].

negative_adjustment_equal(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = rand:uniform(100000000), amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets, remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket1),
	Units = cents,
	Bucket2 = #bucket{units = Units, remain_amount = 2000,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket2),
	Bucket3 = #bucket{units = octets, remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket3),
	Adjustment = #adjustment{units = Units, amount = -2000,
			start_date = erlang:system_time(?MILLISECOND), product = ProdRef,
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	ok = ocs:adjustment(Adjustment),
	{atomic, [#product{balance = BalanceRefs}]} = mnesia:transaction(fun() ->
			mnesia:read(product, ProdRef, read) end),
	2 = length(BalanceRefs).

negative_adjustment_low() ->
	[{userdata, [{doc, "Applying low negative adjustment"}]}].

negative_adjustment_low(_Config) ->
	Price1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = rand:uniform(100000000), amount = rand:uniform(100)},
	OfferId = ocs:generate_identity(),
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets, remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket1),
	Units = cents,
	Bucket2 = #bucket{units = Units, remain_amount = 3000,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket2),
	Bucket3 = #bucket{units = octets, remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket3),
	Adjustment = #adjustment{units = Units, amount = -1000,
			start_date = erlang:system_time(?MILLISECOND), product = ProdRef,
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	ok = ocs:adjustment(Adjustment),
	Buckets = ocs:get_buckets(ProdRef),
	3 = length(Buckets),
	[#bucket{remain_amount = RA}] = [B || #bucket{units = U} = B <- Buckets, U == Units],
	2000 = RA.

add_offer_event() ->
	[{userdata, [{doc, "Event received on adding offer"}]}].

add_offer_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	{ok, OfferName} = ocs_test_lib:add_offer(),
	receive
		{create_offer, Offer, product} ->
			OfferName = Offer#offer.name
	end.

delete_offer_event() ->
	[{userdata, [{doc, "Event received on deleting offer"}]}].

delete_offer_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	{ok, OfferName} = ocs_test_lib:add_offer(),
	receive
		{create_offer, Offer1, product} ->
			OfferName = Offer1#offer.name
	end,
	ok = ocs:delete_offer(OfferName),
	receive
		{delete_offer, Offer2, product} ->
			OfferName = Offer2#offer.name
	end.

gtt_insert_event() ->
	[{userdata, [{doc, "Event received on inserting logical resource"}]}].

gtt_insert_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	[Table | _] = ocs_gtt:list(),
	Prefix = "1519240",
	Description = "Bell Mobility",
	Amount = 10000,
	{ok, #gtt{}} = ocs_gtt:insert(Table, Prefix, {Description, Amount}),
	receive
		{insert_gtt, {Table, #gtt{num = Prefix, value = Value}}, resource} ->
			{Description, Amount, _} = Value
	end.

gtt_delete_event() ->
	[{userdata, [{doc, "Event received on deleting logical resource"}]}].

gtt_delete_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	[Table | _] = ocs_gtt:list(),
	Prefix = "1519241",
	Description = "Bell Mobility",
	Amount = 10000,
	{ok, #gtt{}} = ocs_gtt:insert(Table, Prefix, {Description, Amount}),
	receive
		{insert_gtt, {Table, #gtt{num = Prefix, value = Value1}}, resource} ->
		{Description, Amount, _} = Value1
	end,
	ok = ocs_gtt:delete(Table, Prefix),
	receive
		{delete_gtt, {Table, #gtt{num = Prefix, value = Value2}}, resource} ->
			{Description, Amount, _} = Value2
	end.

add_resource_event() ->
	[{userdata, [{doc, "Event received on adding resource"}]}].

add_resource_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Name = "Example",
	Status = created,
	TariffResource = #resource{name = Name,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + rand:uniform(10000000000),
			state = Status, specification = #specification_ref{id = "4",
			href = "/resourceCatalogManagement/v3/resourceSpecification/4",
			name = "Example spec", version = "1.0"}},
	{ok, #resource{}} = ocs:add_resource(TariffResource),
	receive
		{create_resource, #resource{id = Id, name = Name, state = Status,
				specification = #specification_ref{}}, resource} ->
			true = is_list(Id)
	end.

delete_pla_event() ->
	[{userdata, [{doc, "Event received on deleting pla"}]}].

delete_pla_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	SD = erlang:system_time(?MILLISECOND),
	ED = erlang:system_time(?MILLISECOND) + rand:uniform(10000000000),
	Status = created,
	Name = "test_notification1",
	Pla = #resource{name = Name, start_date = SD,
			end_date = ED, state = Status},
	{ok, #resource{}} = ocs:add_pla(Pla),
	receive
		{create_pla, #resource{name = Name, state = Status,
				last_modified = LM}, resource} ->
			true = is_tuple(LM)
	end,
	ok = ocs:delete_pla(Name),
	receive
		{delete_pla, #resource{name = Name, state = Status}, resource} ->
			true
	end.

add_service_event() ->
	[{userdata, [{doc, "Event received on adding service"}]}].

add_service_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, #service{}} = ocs:add_service(Identity, Password),
	receive
		{create_service, #service{name = Name, password = Pass}, service} ->
			Name = list_to_binary(Identity),
			Pass = list_to_binary(Password)
	end.

delete_service_event() ->
	[{userdata, [{doc, "Event received on deleting service"}]}].

delete_service_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, #service{}} = ocs:add_service(Identity, Password),
	receive
		{create_service, #service{name = Name1, password = Pass1}, service} ->
			Name1 = list_to_binary(Identity),
			Pass1 = list_to_binary(Password)
	end,
	ok = ocs:delete_service(Identity),
	receive
		{delete_service, #service{name = Name2, password = Pass2}, service} ->
			Name2 = list_to_binary(Identity),
			Pass2 = list_to_binary(Password)
	end.

add_product_event() ->
	[{userdata, [{doc, "Event received on adding service"}]}].

add_product_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Price = #price{name = ocs:generate_identity(),
			type = usage, units = octets, size = 1000, amount = 100},
	OfferName = ocs:generate_identity(),
	Offer1 = #offer{name = OfferName,
			price = [Price], specification = 4},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer1),
	receive
		{create_offer, Offer2, product} ->
			OfferName = Offer2#offer.name
	end,
	{ok, #product{id = ProductId}} = ocs:add_product(OfferId, [], []),
	receive
		{create_product, Product, product} ->
			ProductId = Product#product.id,
			OfferId = Product#product.product
	end.

delete_product_event() ->
	[{userdata, [{doc, "Event received on deleting service"}]}].

delete_product_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Price = #price{name = ocs:generate_identity(),
			type = usage, units = octets, size = 1000, amount = 100},
	OfferName = ocs:generate_identity(),
	Offer1 = #offer{name = OfferName,
			price = [Price], specification = 4},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer1),
	receive
		{create_offer, Offer2, product} ->
			OfferName = Offer2#offer.name
	end,
	{ok, #product{id = ProductId}} = ocs:add_product(OfferId, [], []),
	receive
		{create_product, Product1, product} ->
			ProductId = Product1#product.id,
			OfferId = Product1#product.product
	end,
	ok = ocs:delete_product(ProductId),
	receive
		{delete_product, Product2, product} ->
			ProductId = Product2#product.id,
			OfferId = Product2#product.product
	end.

add_bucket_event() ->
	[{userdata, [{doc, "Event received on adding bucket"}]}].

add_bucket_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Price = #price{name = ocs:generate_identity(),
			type = usage, units = octets, size = 1000, amount = 100},
	Offer1 = #offer{name = ocs:generate_identity(),
			price = [Price], specification = 4},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer1),
	receive
		{create_offer, Offer2, product} ->
			OfferId = Offer2#offer.name
	end,
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	receive
		{create_product, Product1, product} ->
			ProdRef = Product1#product.id,
			OfferId = Product1#product.product
	end,
	Amount = 100,
	Units = cents,
	Bucket1 = #bucket{units = Units, remain_amount = Amount,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BucketId}} = ocs:add_bucket(ProdRef, Bucket1),
	receive
		{create_bucket, Bucket2, balance} ->
			BucketId = Bucket2#bucket.id,
			Units = Bucket2#bucket.units,
			Amount = Bucket2#bucket.remain_amount
	end.

delete_bucket_event() ->
	[{userdata, [{doc, "Event received on deleting bucket"}]}].

delete_bucket_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	Price = #price{name = ocs:generate_identity(),
			type = usage, units = octets, size = 1000, amount = 100},
	Offer1 = #offer{name = ocs:generate_identity(),
			price = [Price], specification = 4},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer1),
	receive
		{create_offer, Offer2, product} ->
			OfferId = Offer2#offer.name
	end,
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	receive
		{create_product, Product1, product} ->
			ProdRef = Product1#product.id,
			OfferId = Product1#product.product
	end,
	Amount = 100,
	Units = cents,
	Bucket1 = #bucket{units = Units, remain_amount = Amount,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BucketId}} = ocs:add_bucket(ProdRef, Bucket1),
	receive
		{create_bucket, Bucket2, balance} ->
			BucketId = Bucket2#bucket.id
	end,
	ok = ocs:delete_bucket(BucketId),
	receive
		{delete_bucket, Bucket3, balance} ->
			BucketId = Bucket3#bucket.id,
			Units = Bucket3#bucket.units,
			Amount = Bucket3#bucket.remain_amount
	end.

product_charge_event() ->
	[{userdata, [{doc, "Event received on product subscription charge"}]}].

product_charge_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	SD = erlang:system_time(?MILLISECOND),
	UnitSize = 100000000000,
	Alteration = #alteration{name = ocs:generate_identity(), start_date = SD,
			type = usage, period = undefined,
			units = octets, size = UnitSize, amount = 0},
	Amount = 1250,
	Price = #price{name = ocs:generate_identity(), start_date = SD,
			type = recurring, period = monthly,
			amount = Amount, alteration = Alteration},
	OfferId = add_offer([Price], 4),
	receive
		{create_offer, Offer, product} ->
			OfferId = Offer#offer.name
	end,
	{ok, #product{id = ProdId} = P} = ocs:add_product(OfferId, []),
	receive
		{create_product, Product, product} ->
			ProdId = Product#product.id,
			OfferId = Product#product.product
	end,
	Expired = erlang:system_time(?MILLISECOND) - 3599000,
	ok = mnesia:dirty_write(product, P#product{payment =
			[{Price#price.name, Expired}]}),
	B1 = #bucket{units = cents, remain_amount = 1000,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BId1}} = ocs:add_bucket(ProdId, B1),
	receive
		{create_bucket, Bucket1, balance} ->
			BId1 = Bucket1#bucket.id
	end,
	B2 = #bucket{units = cents, remain_amount = 1000,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BId2}} = ocs:add_bucket(ProdId, B2),
	receive
		{create_bucket, Bucket2, balance} ->
			BId2 = Bucket2#bucket.id
	end,
	ok = ocs_scheduler:product_charge(),
	receive
		{charge, Adjustments, balance} ->
			Fcents = fun(#adjustment{amount = Value, units = cents}) ->
						{true, Value};
					(_) ->
						false
			end,
			CentsAmount = Amount * -1,
			CentsAmount = lists:sum(lists:filtermap(Fcents, Adjustments)),
			#adjustment{amount = UnitSize, units = octets}
					= lists:keyfind(octets, #adjustment.units, Adjustments)
	end.

rating_deleted_bucket_event() ->
	[{userdata, [{doc, "Event received on deletion of bucket during rating"}]}].

rating_deleted_bucket_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	receive
		{create_offer, Offer, product} ->
			OfferId = Offer#offer.name
	end,
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	receive
		{create_product, Product, product} ->
			ProdRef = Product#product.id,
			OfferId = Product#product.product
	end,
	{ok, #service{name = ServiceId}} = ocs:add_service(ocs:generate_identity(),
			ocs:generate_password(), ProdRef, []),
	receive
		{create_service, #service{name = Name}, service} ->
			ServiceId = Name
	end,
	Units = cents,
	Bucket1 = #bucket{units = Units, remain_amount = PackagePrice,
			start_date = erlang:system_time(?MILLISECOND) - (2 * 2592000000),
			end_date = erlang:system_time(?MILLISECOND) - 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket1),
	receive
		{create_bucket, Bucket2, balance} ->
			BId = Bucket2#bucket.id
	end,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{out_of_credit, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{octets, PackageSize}], SessionId),
	receive
		{depleted, #bucket{units = Units, remain_amount = RA}, balance} ->
			PackagePrice = RA
	end.

accumulated_balance_event() ->
	[{userdata, [{doc, "Event received on accumulated balance after rating"}]}].

accumulated_balance_event(_Config) ->
	ok = gen_event:add_handler(ocs_event, test_event, [self()]),
	PackagePrice = 5000000,
	PackageSize = 100000000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	receive
		{create_offer, Offer, product} ->
			OfferId = Offer#offer.name
	end,
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	receive
		{create_product, Product, product} ->
			ProdRef = Product#product.id,
			OfferId = Product#product.product
	end,
	{ok, #service{name = ServiceId}} = ocs:add_service(ocs:generate_identity(),
			ocs:generate_password(), ProdRef, []),
	receive
		{create_service, #service{name = Name}, service} ->
			ServiceId = Name
	end,
	RA1 = 50000000,
	BId1 = add_bucket(ProdRef, cents, RA1),
	receive
		{create_bucket, Bucket1, balance} ->
			BId1 = Bucket1#bucket.id
	end,
	RA2 = 500000000,
	BId2 = add_bucket(ProdRef, octets, RA2),
	receive
		{create_bucket, Bucket2, balance} ->
			BId2 = Bucket2#bucket.id
	end,
	RA3 = 100000000,
	BId3 = add_bucket(ProdRef, cents, RA3),
	receive
		{create_bucket, Bucket3, balance} ->
			BId3 = Bucket3#bucket.id
	end,
	ok = application:set_env(ocs, threshold_bytes, 500000000),
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{octets, PackageSize}], SessionId),
	receive
		{accumulated, [#acc_balance{total_balance = BytesTotalAmount,
				units = octets}], balance} ->
			BytesTotalAmount = RA2 - PackageSize
	end.

add_policy_table() ->
	[{userdata, [{doc, "Add a new policy table"}]}].

add_policy_table(_Config) ->
	TableName = test_policy,
	ok = ocs:add_policy_table(atom_to_list(TableName)),
	{atomic, policy} = mnesia:transaction(
			fun() -> mnesia:table_info(TableName, record_name) end).

delete_policy_table() ->
	[{userdata, [{doc, "Delete a policy table"}]}].

delete_policy_table(_Config) ->
	TableName = test_policy2,
	ok = ocs:add_policy_table(atom_to_list(TableName)),
	ok = ocs:delete_policy_table(TableName),
	{aborted, {no_exists, TableName, _}} = mnesia:transaction(
			fun() -> mnesia:table_info(TableName, all) end).

add_policy() ->
	[{userdata, [{doc, "Add a new policy in a specified table"}]}].

add_policy(_Config) ->
	TableName = test_policy3,
	ok = ocs:add_policy_table(atom_to_list(TableName)),
	PolicyName = "internal",
	QosInformation = #{"QoS-Class-Identifier" => 9,
			"Max-Requested-Bandwidth-UL" => 1000000000,
			"Max-Requested-Bandwidth-DL" => 1000000000},
	FlowInformationUp1 = #{"Flow-Description" =>
			"permit in ip from any to 10/8", "Flow-Direction" => 2},
	FlowInformationDown1 = #{"Flow-Description" =>
			"permit out ip from 10/8 to any", 'Flow-Direction' => 1},
	Policy = #policy{name = PolicyName,
			qos = QosInformation, charging_rule = 1,
			flow = [FlowInformationUp1, FlowInformationDown1], precedence = 2},
	{ok, #policy{}} = ocs:add_policy(atom_to_list(TableName), Policy),
	{atomic, [P]} = mnesia:transaction(
			fun() -> mnesia:read(TableName, PolicyName, read) end),
	PolicyName = P#policy.name,
	true = is_map(P#policy.qos),
	true = is_integer(P#policy.charging_rule),
	true = is_list(P#policy.flow),
	true = is_integer(P#policy.precedence).

get_policy() ->
	[{userdata, [{doc, "Lookup policy in a specified table with given policy"
			"reference"}]}].

get_policy(_Config) ->
	TableName = "policy",
	PolicyName = "external",
	QosInformation = #{"QoS-Class-Identifier" => 9,
			"Max-Requested-Bandwidth-UL" => 1000000000,
			"Max-Requested-Bandwidth-DL" => 1000000000},
	FlowInformationUp1 = #{"Flow-Description" =>
			"permit in ip from any to 172.16/12", "Flow-Direction" => 2},
	FlowInformationDown1 = #{"Flow-Description" =>
			"permit out ip from 172.16/12 to any", 'Flow-Direction' => 1},
	Policy = #policy{name = PolicyName,
			qos = QosInformation, charging_rule = 32,
			flow = [FlowInformationUp1, FlowInformationDown1], precedence = 1},
	{ok, #policy{}} = ocs:add_policy(TableName, Policy),
	{ok, #policy{}} = ocs:get_policy(TableName, PolicyName).

get_policies() ->
	[{userdata, [{doc, "List all the policies in a specified table"}]}].

get_policies(_Config) ->
	TableName = "policy",
	QosInformation = #{"QoS-Class-Identifier" => 9,
			"Max-Requested-Bandwidth-UL" => 1000000000,
			"Max-Requested-Bandwidth-DL" => 1000000000},
	PolicyName1 = "internal",
	FlowInformationUp1 = #{"Flow-Description" => "permit in ip from any to 10/8",
			"Flow-Direction" => 2},
	FlowInformationDown1 = #{"Flow-Description" =>
			"permit out ip from 10/8 to any", 'Flow-Direction' => 1},
	Policy1 = #policy{name = PolicyName1,
			qos = QosInformation, charging_rule = 1,
			flow = [FlowInformationUp1, FlowInformationDown1], precedence = 2},
	{ok, #policy{}} = ocs:add_policy(TableName, Policy1),
	PolicyName2 = "external",
	FlowInformationUp2 = #{"Flow-Description" =>
			"permit in ip from any to 172.16/12", "Flow-Direction" => 2},
	FlowInformationDown2 = #{"Flow-Description" =>
			"permit out ip from 172.16/12 to any", 'Flow-Direction' => 1},
	Policy2 = #policy{name = PolicyName2,
			qos = QosInformation, charging_rule = 32,
			flow = [FlowInformationUp2, FlowInformationDown2], precedence = 1},
	{ok, #policy{}} = ocs:add_policy(TableName, Policy2),
	Policies = ocs:get_policies(TableName),
	true = length(Policies) >= 2,
	F = fun(#policy{}) ->
				true;
			(_) ->
				false
	end,
	true = lists:all(F, Policies).

delete_policy() ->
	[{userdata, [{doc, "Remove a policy from the table"}]}].

delete_policy(_Config) ->
	TableName = "policy",
	PolicyName = "internal",
	QosInformation = #{"QoS-Class-Identifier" => 9,
			"Max-Requested-Bandwidth-UL" => 1000000000,
			"Max-Requested-Bandwidth-DL" => 1000000000},
	FlowInformationUp1 = #{"Flow-Description" => "permit in ip from any to 10/8",
			"Flow-Direction" => 2},
	FlowInformationDown1 = #{"Flow-Description" =>
			"permit out ip from 10/8 to any", 'Flow-Direction' => 1},
	Policy = #policy{name = PolicyName,
			qos = QosInformation, charging_rule = 1,
			flow = [FlowInformationUp1, FlowInformationDown1], precedence = 2},
	{ok, #policy{}} = ocs:add_policy(TableName, Policy),
	ok = ocs:delete_policy(TableName, PolicyName),
	{error, not_found} = ocs:get_policy(TableName, PolicyName).

add_resource() ->
	[{userdata, [{doc, "Add new resource"}]}].

add_resource(_Config) ->
	Resource = #resource{
			name = "Example",
			description = "Example voice tariff",
			category = "tariff",
			state = "created", start_date = erlang:system_time(?MILLISECOND),
			related = [#resource_rel{id = "1000000000-01",
					name = "example rel",
					href = "/resourceInventory/v4/resource/1000000000-01",
					type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v3/resourceSpecification/4",
					name = "Example spec", version = "1.0"}},
	{ok, #resource{id = ResouceId}} = ocs:add_resource(Resource),
	{atomic, [Resource1]} = mnesia:transaction(fun() ->
			mnesia:read(resource, ResouceId, read)
	end),
	true = is_list(Resource1#resource.id),
	true = is_tuple(Resource1#resource.last_modified).

get_resources() ->
	[{userdata, [{doc, "List all the resources in the table"}]}].

get_resources(_Config) ->
	Resource = #resource{name = "Example",
			description = "Example voice tariff", category = "tariff",
			state = "created", start_date = erlang:system_time(?MILLISECOND),
			related = [#resource_rel{id = "1000000000-01", name = "example rel",
					href = "/resourceInventory/v4/resource/1000000000-01",
					type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v3/resourceSpecification/4",
					name = "Example spec", version = "1.0"}},
	{ok, #resource{}} = ocs:add_resource(Resource),
	Resources = ocs:get_resources(),
	true = length(Resources) >= 1,
	F = fun(#resource{}) ->
				true;
			(_) ->
				false
	end,
	true = lists:all(F, Resources).

get_resource() ->
	[{userdata, [{doc, "Get resource by identifier"}]}].

get_resource(_Config) ->
	Resource = #resource{name = "Example",
			description = "Example voice tariff", category = "tariff",
			state = "created", start_date = erlang:system_time(?MILLISECOND),
			related = [#resource_rel{id = "1000000000-01", name = "example rel",
					href = "/resourceInventory/v4/resource/1000000000-01",
					type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v3/resourceSpecification/4",
					name = "Example spec", version = "1.0"}},
	{ok, #resource{id = ResouceId}} = ocs:add_resource(Resource),
	{ok, #resource{id = ResouceId} = Resource1} = ocs:get_resource(ResouceId),
	true = is_list(Resource1#resource.id),
	true = is_tuple(Resource1#resource.last_modified).

delete_resource() ->
	[{userdata, [{doc, "Remove a resource from the table"}]}].

delete_resource(_Config) ->
	Resource = #resource{name = "Example",
			description = "Example voice tariff", category = "tariff",
			state = "created", start_date = erlang:system_time(?MILLISECOND),
			related = [#resource_rel{id = "1000000000-01", name = "example rel",
					href = "/resourceInventory/v4/resource/1000000000-01",
					type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v3/resourceSpecification/4",
					name = "Example spec", version = "1.0"}},
	{ok, #resource{id = ResouceId}} = ocs:add_resource(Resource),
	ok = ocs:delete_resource(ResouceId),
	{error, not_found} = ocs:get_resource(ResouceId).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, Spec) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId.

%% @hidden
add_bucket(ProdRef, Units, RA) ->
	Bucket = #bucket{units = Units, remain_amount = RA,
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.

get_params() ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	case lists:keyfind(require_group, 1, AuthObj) of
		{require_group, [Group | _T]} ->
			{Port, Address, Directory, Group};
		false ->
			exit(not_found)
	end.
