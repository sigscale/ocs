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
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").

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
S_rand = crypto:rand_uniform(1, ?R),
P_rand = crypto:rand_uniform(1, ?R),
P_rand_bin = <<P_rand:256>>,
S_rand_bin = <<S_rand:256>>,
Token = crypto:rand_bytes(4),
S_pwe = ocs_eap_pwd:compute_pwe(Token, <<"Peer">>, <<"Server">>, Password),
P_pwe = ocs_eap_pwd:compute_pwe(Token, <<"Peer">>, <<"Server">>, Password),
{S_scalar, S_element} = ocs_eap_pwd:compute_scalar(S_rand_bin, S_pwe),
{P_scalar, P_element} = ocs_eap_pwd:compute_scalar(P_rand_bin, P_pwe),
Ks = ocs_eap_pwd:compute_ks(S_rand_bin, S_pwe, P_scalar, P_element),
Kp = ocs_eap_pwd:compute_ks(P_rand_bin, P_pwe, S_scalar, S_element),  
S_confirm = ocs_eap_pwd:h([Ks, S_element, S_scalar, P_element, P_scalar, <<19:16>>, <<16#1>>, <<16#1>>]),
P_confirm = ocs_eap_pwd:h([Kp, P_element, P_scalar, S_element, S_scalar, <<19:16>>, <<16#1>>, <<16#1>>]),
MK_S = ocs_eap_pwd:h([Ks, P_confirm, S_confirm]),
MK_P = ocs_eap_pwd:h([Kp, P_confirm, S_confirm]),
MK_P = MK_S.

eap_id_request() ->
	[{userdata, [{doc, "Send an EAP-PWD-ID request to peer"}]}].

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

