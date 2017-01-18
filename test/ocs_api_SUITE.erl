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
%%%  @doc Test suite for public API of the {@link //ocs. ocs} application.
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
-include("ocs.hrl").
-include_lib("common_test/include/ct.hrl").

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
	[client, get_all_clients, delete_client, subscriber, update_password,
	update_attributes, delete_subscriber].

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

get_all_clients() ->
	[{userdata, [{doc, "Retrieve  all radius_clients from  database"}]}].

get_all_clients(Config) ->
	A1 = {10,2,45,67},
	A2 = {10,2,45,68},
	A3 = {10,2,45,69},
	Secret1 = "Enid blyton 1",
	Secret2 = "Enid blyton 2",
	Secret3 = "Enid blyton 3",
	ok = ocs:add_client(A1, Secret1),
	ok = ocs:add_client(A2, Secret2),
	ok = ocs:add_client(A3, Secret3),
	Clients = ocs:get_clients(),
	F = fun(#radius_client{address = Addr, secret = Sec} = _R) ->
		{ok, Sec} = ocs:find_client(Addr)
	end,
	lists:foreach(F, Clients).

delete_client() ->
	[{userdata, [{doc, "Delete  a radius_client from database"}]}].

delete_client(Config) ->
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_secret, Config),
	ok = ocs:add_client(Address, SharedSecret),
	ok = ocs:delete_client(Address),
	{error, not_found} = ocs:find_client(Address).

subscriber() ->
	[{userdata, [{doc, "Add subscriber to database"}]}].

subscriber(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:store(?NasPortId, "wlan0", Attribute0),
	Attribute2 = radius_attributes:store(?NasPortId, "wlan2", Attribute0),
	Password1 = ocs:generate_password(),
	Password2 = ocs:generate_password(),
	ok = ocs:add_subscriber("tomba", Password1, Attribute1),
	ok = ocs:add_subscriber("android", Password2, Attribute2),
	{ok, BinPassword1, Attribute1, _Balance, _Enabled} = ocs:find_subscriber("tomba"),
	Password1 = binary_to_list(BinPassword1),
	{ok, BinPassword2, Attribute2, _Balance, _Enabled} = ocs:find_subscriber("android"),
	Password2 = binary_to_list(BinPassword2).

delete_subscriber() ->
	[{userdata, [{doc, "Delete subscriber from the database"}]}].

delete_subscriber(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	Subscriber = "deleteandroid",
	Password = ocs:generate_password(),
	ok = ocs:add_subscriber(Subscriber, Password, Attribute),
	{ok, _BinPassword, _Attribute, _Balance, _Enabled} = ocs:find_subscriber(Subscriber),
	ok = ocs:delete_subscriber(Subscriber),
	{error, _} = ocs:find_subscriber(Subscriber).

update_password() ->
	[{userdata, [{doc, "Update subscriber password to database"}]}].

update_password(_Config) ->
	Attribute0 = radius_attributes:new(),
	Attribute = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	Subscriber = "android",
	OldPassword = ocs:generate_password(),
	ok = ocs:add_subscriber(Subscriber, OldPassword, Attribute),
	{ok, BinOldPassword, Attribute, _Balance, _Enabled} = ocs:find_subscriber(Subscriber),
	OldPassword = binary_to_list(BinOldPassword),
	NewPassword = ocs:generate_password(),
	ok = ocs:update_password(Subscriber, NewPassword),
	{ok, BinNewPassword, _BinAttribute, _Balance, _Enabled} = ocs:find_subscriber(Subscriber),
	NewPassword = binary_to_list(BinNewPassword).

update_attributes() ->
	[{userdata, [{doc, "Update subscriber attributes to database"}]}].

update_attributes(_Config) ->
	Password = ocs:generate_password(),
	Username = "tomba1",
	Attribute0 = radius_attributes:new(),
	Attribute1 = radius_attributes:store(?NasPortId,"wlan0", Attribute0),
	ok = ocs:add_subscriber(Username, Password, Attribute1),
	{ok, _BinPassword, Attribute1, _Balance, _Enabled} = ocs:find_subscriber(Username),
	Attribute2 = radius_attributes:store(?NasPortId,"wlan1", Attribute0),
	ok = ocs:update_attributes(Username, Attribute2),
	{ok, _BinPassword, Attribute2, _Balance, _Enabled} = ocs:find_subscriber(Username).


%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

