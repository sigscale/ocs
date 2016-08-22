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
	{timetrap, {minutes, 1}},
	{require, radius_username}, {default_config, radius_username, "ocs"},
	{require, radius_password}, {default_config, radius_password, "ocs123"},
	{require, radius_auth_port}, {default_config, radius_auth_port, 8613},
	{require, radius_auth_addr}, {default_config, radius_auth_addr, {127,0,0,1}},
	{require, radius_shared_scret},{default_config, radius_shared_scret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	AuthAddress = ct:get_config(radius_auth_addr),
	SharedSecret = ct:get_config(radius_shared_scret),
	ok = ocs_lib:initialize_db(),
	ok = ocs_lib:start(),
	ok = ocs:add_client(AuthAddress, SharedSecret),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = application:stop(ocs),
	ok = application:stop(radius),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	IP = ct:get_config(radius_auth_addr),
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
	[eap_id_request].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
eap_id_request() ->
   [{userdata, [{doc, "Send an EAP-PWD-ID request to peer"}]}].

eap_id_request(Config) ->
	Id = 0,
	AuthAddress = ct:get_config(radius_auth_addr),
	AuthPort = ct:get_config(radius_auth_port),
	Socket = ?config(socket, Config), 
	UserName = ct:get_config(radius_username),
	SharedSecret = ct:get_config(radius_shared_scret),
	Authenticator = radius:authenticator(SharedSecret, Id),
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?UserName, UserName, AttributeList0),
	AttributeList2 = radius_attributes:store(?NasIdentifier, "tomba", AttributeList1),
	AttributeList3 = radius_attributes:store(?NasPortId,"wlan0", AttributeList2),
	AttributeList4 = radius_attributes:store(?CallingStationId,"de:ad:be:ef:ca:fe", AttributeList3),
	AttributeList5 = radius_attributes:store(?MessageAuthenticator,
		list_to_binary(lists:duplicate(16,0)), AttributeList4),
	Request1 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = AttributeList5},
	RequestPacket1 = radius:codec(Request1),
	MsgAuth = crypto:hmac(md5, SharedSecret, RequestPacket1),
	AttributeList6 = radius_attributes:store(?MessageAuthenticator, MsgAuth, AttributeList5),
	Request2 = #radius{code = ?AccessRequest, id = Id, authenticator = Authenticator,
		attributes = AttributeList6},
	RequestPacket2 = radius:codec(Request2),
	ok = gen_udp:send(Socket, AuthAddress, AuthPort, RequestPacket2),
	{ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
	#eap_packet{code = ?Request, identifier = Id, data = Data} = ocs_eap_codec:eap_pwd(Packet),
	#eap_pwd{type = ?PWD, length = false, more = false, pwd_exch = id,
		data = IDReqBody} = ocs_eap_codec:eap_pwd(Data),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1, token =_Token, pwd_prep = none,
		identity = _HostName} = ocs_eap_codec:eap_pwd_id(IDReqBody).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

