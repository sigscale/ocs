%%% ocs_simple_auth_SUITE.erl
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
%%%  Test suite for the ocs simple authentication.
%%%
-module(ocs_simple_auth_SUITE).
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
	[{userdata, [{doc, "This suite tests the PSK Authentication."}]},
	{timetrap, {minutes, 1}},
	{require, radius_username}, {default_config, radius_username, "ocs"},
	{require, radius_password}, {default_config, radius_password, "ocs123"},
	{require, radius_shared_secret},{default_config, radius_shared_secret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_lib:initialize_db(),
	ok = ocs_lib:start(),
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_secret),
	ok = ocs:add_client(AuthAddress, SharedSecret),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}] ++ Config.

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
	ok = 	gen_udp:close(Socket).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[simple_authentication].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

simple_authentication() ->
	[{userdata, [{doc, "Send AccessAccept to the peer"}]}].

simple_authentication(Config) ->
	Id = 1,
	PeerID = "789EDOB8B823",
	PeerPassword = "222abc33de",
	ok = ocs:add_subscriber(PeerID, PeerPassword, [], 10000),
	Authenticator = radius:authenticator(),
	SharedSecret = ct:get_config(radius_shared_secret),
	UserPassword = radius_attributes:hide(SharedSecret, Authenticator, PeerPassword),	
	{ok, AuthAddress} = application:get_env(ocs, radius_auth_addr),
	{ok, AuthPort} = application:get_env(ocs, radius_auth_port),
	Socket = ?config(socket, Config), 
	A0 = radius_attributes:new(),
	A1 = radius_attributes:store(?ServiceType, 2, A0),
	A2 = radius_attributes:store(?NasPortId, "wlan3", A1),
	A3 = radius_attributes:store(?NasPortType, 19, A2),
	A4 = radius_attributes:store(?UserName, PeerID, A3),
	A5 = radius_attributes:store(?AcctSessionId, "826005e4", A4),
	A6 = radius_attributes:store(?CallingStationId, "78-9E-DO-B8-B8-23", A5),
	A7 = radius_attributes:store(?CalledStationId, "WPA-PSK", A6),
	A8 = radius_attributes:store(?UserPassword, UserPassword, A7),
	A9 = radius_attributes:store(?NasIdentifier, "{14988,{9,\"sigscale.lk\"}}", A8),
	AccessReqest = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
			attributes = A9},
	AccessReqestPacket= radius:codec(AccessReqest),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, AccessReqestPacket),
	{ok, {AuthAddress, AuthPort, AccessAcceptPacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id} = radius:codec(AccessAcceptPacket).

