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
	[{timetrap, {minutes, 1}},
	{require, ocs_auth_port}, {default_config, ocs_auth_port, 1813}].

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
	[decode_packet, encode_packet, ecc_computations].

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
	
ecc_computations() ->
	[{userdata, [{doc, "Check ECC computations"}]}].

ecc_computations(_Config) ->
Password = <<"Secret">>,
U_rs = crypto:rand_uniform(1, ?R),
U_rp = crypto:rand_uniform(1, ?R),
RandomP = <<U_rp:256>>,
RandomS = <<U_rs:256>>,
TokenS = crypto:rand_bytes(4),
TokenP = crypto:rand_bytes(4),
PWE_S = ocs_eap_pwd:compute_pwe(TokenS, <<"Peer">>, <<"Server">>, Password),
PWE_P = ocs_eap_pwd:compute_pwe(TokenP, <<"Peer">>, <<"Server">>, Password),
{Scalar_S, Element_S }= ocs_eap_pwd:compute_scalar(RandomS, PWE_S),
{Scalar_P, Element_P }= ocs_eap_pwd:compute_scalar(RandomP, PWE_P),
KS = ocs_eap_pwd:compute_ks(RandomS, PWE_S, Scalar_P, Element_P),
KP = ocs_eap_pwd:compute_ks(RandomP, PWE_P, Scalar_S, Element_S),  
Confirm_S = ocs_eap_pwd:h([KS, Element_S, Scalar_S, Element_P, Scalar_P, <<16#19>>,<<16#1>>,<<16#1>>]),
Confirm_P = ocs_eap_pwd:h([KP, Element_P, Scalar_P, Element_S, Scalar_S, <<16#19>>,<<16#1>>,<<16#1>>]),
MK_S = ocs_eap_pwd:h([KS, Confirm_P, Confirm_S]),
MK_P = ocs_eap_pwd:h([KP, Confirm_P, Confirm_S]),
MK_P = MK_S.

eap_id_request() ->
	[{userdata, [{doc, "Send an EAP-PWD-ID request to peer"}]}].

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

