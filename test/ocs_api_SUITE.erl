%%% ocs_api_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
%%%  Test suite for the ocs API.
%%%
-module(ocs_api_SUITE).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "This suite tests the application's API."}]},
	{require, radius_shared_secret}, {default_config, radius_shared_secret, "abc345"},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_lib:initialize_db(),
	ok = ocs_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	{ok, IP} = application:get_env(ocs, radius_auth_addr),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, IP}, binary]),
	[{socket, Socket} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	ok =  gen_udp:close(Socket).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[client, subscriber, update_password, update_subscriber_attributes,
	delete_subscriber, decrement_balance].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

client() ->
	[{userdata, [{doc, "Add radius_client to database"}]}].

client(Config) ->
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_secret, Config),
	ok = ocs:add_client(Address, SharedSecret),
	{ok, BinSharedSecret} = ocs:find_client(Address),
	SharedSecret = binary_to_list(BinSharedSecret).

subscriber() ->
	[{userdata, [{doc, "Add subscriber to database"}]}].

subscriber(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	Attribute2 = radius_attributes:store(?NasPortId,"wlan2", Attribute0),
	BinAttribute1 = radius_attributes:codec(Attribute1),
	BinAttribute2 = radius_attributes:codec(Attribute2),
	ok = ocs:add_subscriber("tomba", "abcd234", BinAttribute1),
	ok = ocs:add_subscriber("android", "vyz789", BinAttribute2),
	{ok, <<"abcd234">>, BinAttribute1, _Balance} = ocs:find_subscriber("tomba"),
	{ok, <<"vyz789">>, BinAttribute2, _Balance} = ocs:find_subscriber("android").

delete_subscriber() ->
	[{userdata, [{doc, "Delete subscriber from the database"}]}].

delete_subscriber(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	BinAttribute = radius_attributes:codec(Attribute),
	Subscriber = "deleteandroid",
	Password = ocs:generate_password(),
	ok = ocs:add_subscriber(Subscriber, Password,BinAttribute),
	{ok, _BinPassword, _BinAttribute, _Balance} = ocs:find_subscriber(Subscriber),
	ok = ocs:delete_subscriber(Subscriber, Password),
	{error, _} = ocs:find_subscriber(Subscriber).

update_password() ->
	[{userdata, [{doc, "Update subscriber password to database"}]}].

update_password(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	BinAttribute = radius_attributes:codec(Attribute),
	Subscriber = "android",
	OldPassword = ocs:generate_password(),
	ok = ocs:add_subscriber(Subscriber, OldPassword, BinAttribute),
	{ok, BinOldPassword, BinAttribute, _Balance} = ocs:find_subscriber(Subscriber),
	OldPassword = binary_to_list(BinOldPassword),
	NewPassword = ocs:generate_password(),
	ok = ocs:update_password(Subscriber, OldPassword, NewPassword),
	{ok, BinNewPassword, BinAttribute, _Balance} = ocs:find_subscriber(Subscriber),
	NewPassword = binary_to_list(BinNewPassword).

decrement_balance() ->
	[{userdata, [{doc, "Decrement subscriber's balance based on usage"}]}].

decrement_balance(_Config) ->
	BinAttribute = <<>>,
	Subscriber = "android",
	InitialBalance= 10000,
	Password = ocs:generate_password(),
	ok = ocs:add_subscriber(Subscriber, Password, BinAttribute, InitialBalance),
	Usage1 = 7645,
	{ok , NewBalance1} = ocs:decrement_balance(Subscriber, Usage1),
	NewBalance1 = InitialBalance - Usage1,
	Usage2 = 84,
	{ok , NewBalance2} = ocs:decrement_balance(Subscriber, Usage2),
	NewBalance2 = NewBalance1 - Usage2.

update_subscriber_attributes() ->
	[{userdata, [{doc, "Update subscriber attributes to database"}]}].

update_subscriber_attributes(_Config) ->
	Password = ocs:generate_password(),
	Username = "tomba1",
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	BinAttribute1 = radius_attributes:codec(Attribute1),
	ok = ocs:add_subscriber(Username, Password, BinAttribute1),
	{ok, _BinPassword, BinAttribute1, _Balance} = ocs:find_subscriber(Username),
	Attribute2 = radius_attributes:store(?NasPortId,"wlan1", Attribute0),
	BinAttribute2 = radius_attributes:codec(Attribute2),
	ok = ocs:update_subscriber_attributes(Username, Password, BinAttribute2),
	{ok, _BinPassword, BinAttribute2, _Balance} = ocs:find_subscriber(Username).


%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

