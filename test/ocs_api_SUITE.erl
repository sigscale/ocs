%%% ocs_api_SUITE.erl
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
%%%  @doc Test suite for public API of the {@link //ocs. ocs} application.
%%%
-module(ocs_api_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

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
	add_product, find_product, delete_product, query_product].

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
	Offer = #offer{name = OfferId, price = [Price1],
			specification = "4"},
	{ok, #offer{}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	Bucket1 = #bucket{units = octets, remain_amount = rand:uniform(10000000),
			start_date = erlang:system_time(?MILLISECOND),
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket1),
	Amount = rand:uniform(10000000),
	Units = cents,
	Adjustment = #adjustment{units = Units, amount = Amount,
			start_date = erlang:system_time(?MILLISECOND), product = ProdRef,
			end_date = erlang:system_time(?MILLISECOND) + 2592000000},
	ok = ocs:adjustment(Adjustment),
	{atomic, [#product{balance = BalanceRefs}]} = mnesia:transaction(fun() ->
			mnesia:read(product, ProdRef, read) end),
	BId = lists:last(BalanceRefs),
	{atomic, [Bucket2]} = mnesia:transaction(fun() -> mnesia:read(bucket, BId, read) end),
	Units = Bucket2#bucket.units,
	Amount = Bucket2#bucket.remain_amount.

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

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

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
