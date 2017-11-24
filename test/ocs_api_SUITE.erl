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
	{ok, ProdID} = ocs_test_lib:add_product(),
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
	add_subscriber, update_password, update_attributes, delete_subscriber,
	add_product, find_product, get_products, delete_product, add_user,
	get_user, delete_user].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

client() ->
	[{userdata, [{doc, "Add client to database"}]}].

client(Config) ->
	{ok, [{auth, AuthInstance}, {acct, _AcctInstance}]} = application:get_env(ocs, radius),
	[{Address, _, _}] = AuthInstance,
	SharedSecret = ct:get_config(radius_shared_secret, Config),
	Protocol = ct:get_config(protocol),
	{ok, _} = ocs:add_client(Address, 3799, Protocol, SharedSecret),
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
	{ok, _} = ocs:add_client(A1, 3799, Protocol, Secret1),
	{ok, _} = ocs:add_client(A2, 3799, Protocol, Secret2),
	{ok, _} = ocs:add_client(A3, 13799, Protocol, Secret3),
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
	{ok, _} = ocs:add_client(Address, 3799, Protocol, Password),
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
	[{Address, _, _}] = AuthInstance,
	SharedSecret = ct:get_config(radius_shared_secret, Config),
	Protocol = ct:get_config(protocol),
	{ok, _} = ocs:add_client(Address, 3799, Protocol, SharedSecret),
	ok = ocs:delete_client(Address),
	{error, not_found} = ocs:find_client(Address).

add_subscriber() ->
	[{userdata, [{doc, "Add subscriber to database"}]}].

add_subscriber(Config) ->
	ProdID = ?config(product_id, Config),
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:add(?SessionTimeout, 3600, Attribute0),
	Attribute2 = radius_attributes:add(?AcctInterimInterval, 60, Attribute0),
	Password1 = ocs:generate_password(),
	Password2 = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber("tomba", Password1, ProdID, [], [], Attribute1),
	{ok, _} = ocs:add_subscriber("android", Password2, ProdID, [], [], Attribute2),
	{ok, #subscriber{password = BinPassword1, attributes = Attribute1,
			product = #product_instance{product = ProdID}}} = 
			ocs:find_subscriber("tomba"),
	Password1 = binary_to_list(BinPassword1),
	{ok, #subscriber{password = BinPassword2, attributes = Attribute2,
			product = #product_instance{product = ProdID}}} = ocs:find_subscriber("android"),
	Password2 = binary_to_list(BinPassword2).

delete_subscriber() ->
	[{userdata, [{doc, "Delete subscriber from the database"}]}].

delete_subscriber(Config) ->
	ProdID = ?config(product_id, Config),
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:add(?SessionTimeout, 3600, Attribute0),
	Subscriber = "deleteandroid",
	Password = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(Subscriber, Password, ProdID, [], [], Attribute),
	{ok, _} = ocs:find_subscriber(Subscriber),
	ok = ocs:delete_subscriber(Subscriber),
	{error, _} = ocs:find_subscriber(Subscriber).

update_password() ->
	[{userdata, [{doc, "Update subscriber password to database"}]}].

update_password(Config) ->
	ProdID = ?config(product_id, Config),
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:add(?SessionTimeout, 3600, Attribute0),
	Subscriber = "android",
	OldPassword = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(Subscriber, OldPassword, ProdID, [], [], Attribute),
	{ok, #subscriber{password = BinOldPassword, attributes = Attribute}} =
			ocs:find_subscriber(Subscriber),
	OldPassword = binary_to_list(BinOldPassword),
	NewPassword = ocs:generate_password(),
	ok = ocs:update_password(Subscriber, NewPassword),
	{ok, #subscriber{password = BinNewPassword}} = ocs:find_subscriber(Subscriber),
	NewPassword = binary_to_list(BinNewPassword).

update_attributes() ->
	[{userdata, [{doc, "Update subscriber attributes to database"}]}].

update_attributes(Config) ->
	ProdID = ?config(product_id, Config),
	Password = ocs:generate_password(),
	Username = "tomba1",
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:add(?SessionTimeout, 3600, Attribute0),
	{ok, _} = ocs:add_subscriber(Username, Password, ProdID, [], [], Attribute1),
	{ok, #subscriber{attributes = Attribute1}} = ocs:find_subscriber(Username),
	Attribute2 = radius_attributes:add(?AcctInterimInterval, 60, Attribute0),
	ok = ocs:update_attributes(Username, Attribute2),
	{ok, #subscriber{attributes = Attribute2}} = ocs:find_subscriber(Username).

add_product() ->
	[{userdata, [{doc, "Add a product offering."}]}].

add_product(_Config) ->
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
	Product = #product{name = "Silver Surfer",
			description = "Medium use residential subscription.",
			start_date = SD, is_bundle = false,
			status = active, price = Prices},
	{ok, _Product1} = ocs:add_product(Product).

find_product() ->
	[{userdata, [{doc, "Find a product offering."}]}].

find_product(_Config) ->
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
					type = usage, units = octets,
					size = 150000000, amount = 0}},
	Prices = [Price1, Price2],
	ProductName = "PD1",
	Product = #product{name = ProductName, description = "PDD1",
			start_date = SD, end_date = ED, is_bundle = false,
			status = active, price = Prices},
	{ok, _Product1} = ocs:add_product(Product),
	{ok, #product{name = ProductName}} = ocs:find_product(ProductName).

get_products() ->
	[{userdata, [{doc, "Get all products from product table database"}]}].

get_products(_Config) ->
	F1 = fun(_, 0, Acc) ->
				Acc;
			(F, N, Acc) ->
		ProductName = ocs:generate_password(),
		Price1 = #price{name = ocs:generate_password(),
				type = recurring, period = daily, amount = 1330},
		Price2 = #price{name = ocs:generate_password(),
				type = usage, size = 1000000, amount = 6, units = octets,
				alteration = #alteration{name = ocs:generate_password(),
						type = usage, units = octets,
						size = 150000000, amount = 0}},
		Prices = [Price1, Price2],
		Product = #product{name = ProductName, is_bundle = false,
				status = active, price = Prices},
		{ok, _Product1} =  ocs:add_product(Product),
		F(F, N - 1, [ProductName | Acc])
	end,
	NewProducts = F1(F1, 3, []),
	[] = NewProducts -- [Name || #product{name = Name} <- ocs:get_products()].

delete_product() ->
	[{userdata, [{doc, "Remove a product from product table"}]}].

delete_product(_Config) ->
	Price1 = #price{name = "Daily price",
			type = recurring, period = daily, amount = 330},
	Price2 = #price{name = "Monthly price", units = octets,
			type = usage, size = 1000000, amount = 6},
	Prices = [Price1, Price2],
	ProductName = "Mobile-Internet",
	Product = #product{name = ProductName,
			description = "Monthly subscription for mobile internet",
			is_bundle = false, status = active, price = Prices},
	{ok, _Product1} = ocs:add_product(Product),
	ok = ocs:delete_product(ProductName),
	{error, not_found} = ocs:find_product(ProductName).

add_user() ->
	[{userdata, [{doc, "Create a new user"}]}].

add_user(_Config) ->
	User = "staff_billing",
	Password = ocs:generate_password(),
	Locale = "en",
	{ok, {E1, E2}} = ocs:add_user(User, Password, Locale),
	true = is_integer(E1),
	true = is_integer(E2),
	{Port, Address, Dir, _} = get_params(),
	{ok, #httpd_user{username = User, password = Password,
	user_data = UserData}} = mod_auth:get_user(User, Address, Port, Dir),
	{_, Locale} = lists:keyfind(locale, 1, UserData),
	{_, {E1, E2}} = lists:keyfind(last_modified, 1, UserData).

get_user() ->
	[{userdata, [{doc, "Look up a user from table"}]}].

get_user(_Config) ->
	User = "customer_care",
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
	User = "staff_3",
	Password = ocs:generate_password(),
	Locale = "en",
	{ok, _} = ocs:add_user(User, Password, Locale),
	{ok, _} = ocs:get_user(User),
	ok = ocs:delete_user(User),
	{error, no_such_user} = ocs:get_user(User).

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
