%%% ocs_codec_eap_SUITE.erl
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
-module(ocs_codec_eap_SUITE).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ocs_eap_codec.hrl").
-include("/usr/lib/erlang/lib/radius-1.2/include/radius.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = application:start(radius),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
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
	[decode_packet, encode_packet, eap_id_request].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

decode_packet() ->
	[{userdata, [{doc, "Decode an EAP packet"}]}].

decode_packet(_Config) ->
	ct:fail(not_implemented).

encode_packet() ->
	[{userdata, [{doc, "Encode an EAP packet"}]}].

encode_packet(_Config) ->
	ct:fail(not_implemented).
	
eap_id_request() ->
	[{userdata, [{doc, "Send an EAP-PWD-ID request to peer"}]}].

eap_id_request(_Config) ->
	SharedSecret = "aabbccc998877",
	User = "john",
	Authenticator = radius:authenticator(SharedSecret, 0),
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(1, User, AttributeList0),
	Id = 0,
	Request = #radius{code = 1, id = Id, authenticator =Authenticator,
		attributes = AttributeList1},
	RequestPacket = radius:codec(Request),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip,{127, 0, 0, 1}}, binary]),
	ok = gen_udp:send(Socket, {127,0,0,1}, 7812, RequestPacket),
	{ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
	#eap_pwd{code = ?Request, identifier = Id, length = Length, type = ?PWD, l_bit = false,
		m_bit = false, pwd_exch = 16#1, data = IDReqBody} = ocs_eap_codec:eap_pwd(Packet),
	#eap_pwd_id{group_desc = 19, random_fun = 16#1, prf = 16#1, token =_Token, pwd_prep = 16#0,
		identity = _HostName}= ocs_eap_codec:eap_pwd_id(IDReqBody).


%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

