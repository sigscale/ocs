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
	[encode_eap_id, ecc_computations,
	ecc_computations_diff_shared_key, ecc_computations_invalid_curve_point].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

encode_eap_id() ->
	[{userdata, [{doc, "Encode/decode an EAP-ID/request packet"}]}].

encode_eap_id(_Config) ->
	Code = ?Request,
	Id = 3,
	Token = crypto:rand_bytes(4),
	Identity = "Server-M",
	RecBody = #eap_pwd_id{group_desc = 19, random_fun = 2, prf = 2, token = Token,
		pwd_prep = none, identity = Identity},
	Body_bin = ocs_eap_codec:eap_pwd_id(RecBody),
	RecHeader = #eap_pwd{length = false, more = false, pwd_exch = id,
		data = Body_bin},
	Header_bin =  ocs_eap_codec:eap_pwd(RecHeader),
	RecPacket = #eap_packet{code = Code, type = ?PWD, identifier = Id, data = Header_bin},
	Packet_bin = ocs_eap_codec:eap_packet(RecPacket),
	#eap_packet{code = Code, type = ?PWD, identifier = Id, data = Res_Header_bin}
		= ocs_eap_codec:eap_packet(Packet_bin),
	#eap_pwd{length = false, more = false, pwd_exch = id,
		data = Res_Body_bin} = ocs_eap_codec:eap_pwd(Res_Header_bin),
	#eap_pwd_id{group_desc = 19, random_fun = 2, prf = 2, token = Token,
		pwd_prep = none, identity = Identity} = ocs_eap_codec:eap_pwd_id(Res_Body_bin).

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
	S_confirm = ocs_eap_pwd:h([Ks, S_element, S_scalar, P_element, P_scalar,
		<<19:16>>, <<16#1>>, <<16#1>>]),
	P_confirm = ocs_eap_pwd:h([Kp, P_element, P_scalar, S_element, S_scalar,
		<<19:16>>, <<16#1>>, <<16#1>>]),
	MK_S = ocs_eap_pwd:h([Ks, P_confirm, S_confirm]),
	MK_P = ocs_eap_pwd:h([Kp, P_confirm, S_confirm]),
	MK_P = MK_S.

ecc_computations_diff_shared_key() ->
	[{userdata, [{doc, "Check ECC computations with an invalid shared key"}]}].

ecc_computations_diff_shared_key(_Config) ->
	S_Password = <<"Secret S">>,
	P_Password = <<"Secret P">>,
	S_rand = crypto:rand_uniform(1, ?R),
	P_rand = crypto:rand_uniform(1, ?R),
	P_rand_bin = <<P_rand:256>>,
	S_rand_bin = <<S_rand:256>>,
	Token = crypto:rand_bytes(4),
	S_pwe = ocs_eap_pwd:compute_pwe(Token, <<"Peer">>, <<"Server">>, S_Password),
	P_pwe = ocs_eap_pwd:compute_pwe(Token, <<"Peer">>, <<"Server">>, P_Password),
	{S_scalar, S_element} = ocs_eap_pwd:compute_scalar(S_rand_bin, S_pwe),
	{P_scalar, P_element} = ocs_eap_pwd:compute_scalar(P_rand_bin, P_pwe),
	Ks = ocs_eap_pwd:compute_ks(S_rand_bin, S_pwe, P_scalar, P_element),
	Kp = ocs_eap_pwd:compute_ks(P_rand_bin, P_pwe, S_scalar, S_element),
	Ks /= Kp.

ecc_computations_invalid_curve_point() ->
<<<<<<< Updated upstream
	[{userdata, [{doc, "Check ECC computations with an invalid points on elliptic curve"}]}].
=======
	[{userdata, [{doc, "Check ECC computations with an invalid random number"}]}].
>>>>>>> Stashed changes

ecc_computations_invalid_curve_point(_Config) ->
	Password = <<"Secret S">>,
	S_rand = crypto:rand_uniform(1, ?R),
	P_rand = crypto:rand_uniform(?R+1, ?R + 769),
	P_rand_bin = <<P_rand:256>>,
	S_rand_bin = <<S_rand:256>>,
	Token = crypto:rand_bytes(4),
	S_pwe = ocs_eap_pwd:compute_pwe(Token, <<"Peer">>, <<"Server">>, Password),
	P_pwe = ocs_eap_pwd:compute_pwe(Token, <<"Peer">>, <<"Server">>, Password),
	{S_scalar, S_element} = ocs_eap_pwd:compute_scalar(S_rand_bin, S_pwe),
	{P_scalar, P_element} = ocs_eap_pwd:compute_scalar(P_rand_bin, P_pwe),
	Ks = ocs_eap_pwd:compute_ks(S_rand_bin, S_pwe, P_scalar, P_element),
	Kp = ocs_eap_pwd:compute_ks(P_rand_bin, P_pwe, S_scalar, S_element),
	S_confirm = ocs_eap_pwd:h([Ks, S_element, S_scalar, P_element, P_scalar,
		<<19:16>>, <<16#1>>, <<16#1>>]),
	P_confirm = ocs_eap_pwd:h([Kp, P_element, P_scalar, S_element, S_scalar,
		<<19:16>>, <<16#1>>, <<16#1>>]),
	MK_S = ocs_eap_pwd:h([Ks, P_confirm, S_confirm]),
	MK_P = ocs_eap_pwd:h([Kp, P_confirm, S_confirm]),
	MK_P /= MK_S.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

