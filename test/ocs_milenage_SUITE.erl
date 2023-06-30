%%% ocs_milenage_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2023 SigScale Global Inc.
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
%%%  @doc Test suite for MILENAGE 3GPP Authentication and Key Generation
%%% 	Functions.
%%%
%%%--------------------------------------------------------------------
%%% 3GPP TS 35.208 4. Conformance test data for MILENAGE
%%%--------------------------------------------------------------------
%%%
-module(ocs_milenage_SUITE).
-copyright('Copyright (c) 2018 - 2023 SigScale Global Inc.').

%% Note: This directive should only be used in test suites.
-compile(export_all).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").

-record(test_set,
		{k, rand, sqn, amf, op, opc, f1, 'f1*', f2, f5, f3, f4, 'f5*'}).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "MILENAGE 3GPP Authentication and Key Generation Functions."}]},
	{timetrap, {seconds, 8}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	crypto:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	application:stop(crypto).

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config) ->
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
	[f0, f1, 'f1*', f2, f3, f4, f5, 'f5*', opc, f2345].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

f0() ->
	[{userdata, [{doc, "MILENAGE f0 algorithm."}]}].

f0(_Config) ->
	bit_size(ocs_milenage:f0()) == 128.

f1() ->
	[{userdata, [{doc, "MILENAGE f1 algorithm."}]}].

f1(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, sqn = SQN,
					amf = AMF, f1 = F1}) ->
				F1 = ocs_milenage:f1(OPc, K, RAND, SQN, AMF)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

'f1*'() ->
	[{userdata, [{doc, "MILENAGE f1* algorithm."}]}].

'f1*'(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, sqn = SQN,
					amf = AMF, 'f1*' = F1star}) ->
				F1star = ocs_milenage:'f1*'(OPc, K, RAND, SQN, AMF)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).
	
f2() ->
	[{userdata, [{doc, "MILENAGE f2 algorithm."}]}].

f2(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, f2 = F2}) ->
				F2 = ocs_milenage:f2(OPc, K, RAND)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

f3() ->
	[{userdata, [{doc, "MILENAGE f3 algorithm."}]}].

f3(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, f3 = F3}) ->
				F3 = ocs_milenage:f3(OPc, K, RAND)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

f4() ->
	[{userdata, [{doc, "MILENAGE f4 algorithm."}]}].

f4(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, f4 = F4}) ->
				F4 = ocs_milenage:f4(OPc, K, RAND)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

f5() ->
	[{userdata, [{doc, "MILENAGE f5 algorithm."}]}].

f5(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, f5 = F5}) ->
				F5 = ocs_milenage:f5(OPc, K, RAND)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

'f5*'() ->
	[{userdata, [{doc, "MILENAGE f5* algorithm."}]}].

'f5*'(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND, 'f5*' = F5star}) ->
				F5star = ocs_milenage:'f5*'(OPc, K, RAND)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

f2345() ->
	[{userdata, [{doc, "MILENAGE f2,f3,f4 & f5 algorithms."}]}].

f2345(_Config) ->
	F = fun(#test_set{opc = OPc, k = K, rand = RAND,
					f2 = F2, f3 = F3, f4 = F4, f5 = F5}) ->
				{F2, F3, F4, F5} = ocs_milenage:f2345(OPc, K, RAND)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

opc() ->
	[{userdata, [{doc, "MILENAGE Operator Variant algorithm."}]}].

opc(_Config) ->
	F = fun(#test_set{k = K, op = OP, opc = OPc}) ->
				OPc = ocs_milenage:opc(OP, K)
	end,
	lists:foreach(F, [test_set(N) || N <- lists:seq(1, 20)]).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

test_set(1) ->
	#test_set{
		k = <<70,91,92,232,177,153,180,159,170,95,10,46,226,56,166,188>>,
		rand = <<35,85,60,190,150,55,168,157,33,138,230,77,174,71,191,53>>,
		sqn = <<255,155,180,208,182,7>>,
		amf = <<185,185>>,
		op = <<205,194,2,213,18,62,32,246,43,109,103,106,199,44,179,24>>,
		opc = <<205,99,203,113,149,74,159,78,72,165,153,78,55,160,43,175>>,
		f1 = <<74,159,250,195,84,223,175,179>>,
		'f1*' = <<1,207,175,158,196,232,113,233>>,
		f2 = <<165,66,17,213,227,186,80,191>>,
		f5 = <<170,104,156,100,131,112>>,
		f3 = <<180,11,169,163,197,139,42,5,187,240,217,135,178,27,248,203>>,
		f4 = <<247,105,188,215,81,4,70,4,18,118,114,113,28,109,52,65>>,
		'f5*' = <<69,30,139,236,164,59>>};
test_set(2) ->
	#test_set{
		k = <<70,91,92,232,177,153,180,159,170,95,10,46,226,56,166,188>>,
		rand = <<35,85,60,190,150,55,168,157,33,138,230,77,174,71,191,53>>,
		sqn = <<255,155,180,208,182,7>>,
		amf = <<185,185>>,
		op = <<205,194,2,213,18,62,32,246,43,109,103,106,199,44,179,24>>,
		opc = <<205,99,203,113,149,74,159,78,72,165,153,78,55,160,43,175>>,
		f1 = <<74,159,250,195,84,223,175,179>>,
		'f1*' = <<1,207,175,158,196,232,113,233>>,
		f2 = <<165,66,17,213,227,186,80,191>>,
		f5 = <<170,104,156,100,131,112>>,
		f3 = <<180,11,169,163,197,139,42,5,187,240,217,135,178,27,248,203>>,
		f4 = <<247,105,188,215,81,4,70,4,18,118,114,113,28,109,52,65>>,
		'f5*' = <<69,30,139,236,164,59>>};
test_set(3) ->
	#test_set{
		k = <<254,200,107,166,235,112,126,208,137,5,117,123,27,180,75,143>>,
		rand = <<159,124,141,2,26,204,244,219,33,60,207,240,199,247,26,106>>,
		sqn = <<157,2,119,89,95,252>>,
		amf = <<114,92>>,
		op = <<219,197,154,220,182,249,160,239,115,84,119,183,250,223,131,116>>,
		opc = <<16,6,2,15,10,71,139,246,182,153,241,92,6,46,66,179>>,
		f1 = <<156,171,195,233,155,175,114,129>>,
		'f1*' = <<149,129,75,162,179,4,67,36>>,
		f2 = <<128,17,196,140,12,33,78,210>>,
		f5 = <<51,72,77,194,19,107>>,
		f3 = <<93,189,187,41,84,232,243,205,230,101,176,70,23,154,80,152>>,
		f4 = <<89,169,45,59,71,106,4,67,72,112,85,207,136,178,48,123>>,
		'f5*' = <<222,172,221,132,140,198>>};
test_set(4) ->
	#test_set{
		k = <<158,89,68,174,169,75,129,22,92,130,251,249,243,45,183,81>>,
		rand = <<206,131,219,197,74,192,39,74,21,124,23,248,13,1,123,214>>,
		sqn = <<11,96,74,129,236,168>>,
		amf = <<158,9>>,
		op = <<34,48,20,197,128,102,148,192,7,202,30,238,245,127,0,79>>,
		opc = <<166,74,80,122,225,162,169,139,184,142,180,33,1,53,220,135>>,
		f1 = <<116,165,130,32,203,168,76,73>>,
		'f1*' = <<172,44,199,74,150,135,24,55>>,
		f2 = <<243,101,205,104,60,217,46,150>>,
		f5 = <<240,185,192,138,208,46>>,
		f3 = <<226,3,237,179,151,21,116,245,169,75,13,97,184,22,52,93>>,
		f4 = <<12,69,36,173,234,192,65,196,221,131,13,32,133,79,196,107>>,
		'f5*' = <<96,133,168,108,111,99>>};
test_set(5) ->
	#test_set{
		k = <<74,177,222,176,92,166,206,176,81,252,152,231,125,2,106,132>>,
		rand = <<116,176,205,96,49,161,200,51,155,43,108,226,184,196,161,134>>,
		sqn = <<232,128,161,181,128,182>>,
		amf = <<159,7>>,
		op = <<45,22,197,205,31,223,107,34,56,53,132,227,190,242,168,216>>,
		opc = <<220,240,124,189,81,133,82,144,185,42,7,169,137,30,82,62>>,
		f1 = <<73,231,133,221,18,98,110,242>>,
		'f1*' = <<158,133,121,3,54,187,63,162>>,
		f2 = <<88,96,252,27,206,53,30,126>>,
		f5 = <<49,225,26,96,145,24>>,
		f3 = <<118,87,118,107,55,61,28,33,56,243,7,227,222,146,66,249>>,
		f4 = <<28,66,233,96,216,155,143,169,159,39,68,224,112,140,203,83>>,
		'f5*' = <<254,37,85,229,74,169>>};
test_set(6) ->
	#test_set{
		k = <<108,56,161,22,172,40,12,69,79,89,51,46,227,92,140,79>>,
		rand = <<238,100,102,188,150,32,44,90,85,122,187,239,248,186,191,99>>,
		sqn = <<65,75,152,34,33,129>>,
		amf = <<68,100>>,
		op = <<27,160,10,26,124,103,0,172,140,63,243,233,106,208,135,37>>,
		opc = <<56,3,239,83,99,185,71,198,170,162,37,229,143,174,57,52>>,
		f1 = <<7,138,223,180,136,36,26,87>>,
		'f1*' = <<128,36,107,141,1,134,188,241>>,
		f2 = <<22,200,35,63,5,160,172,40>>,
		f5 = <<69,176,246,154,176,108>>,
		f3 = <<63,140,117,135,254,142,75,35,58,246,118,174,222,48,186,59>>,
		f4 = <<167,70,108,193,230,178,161,51,125,73,211,182,110,149,215,180>>,
		'f5*' = <<31,83,205,43,17,19>>};
test_set(7) ->
	#test_set{
		k = <<45,96,157,77,176,172,91,240,210,192,222,38,112,20,222,13>>,
		rand = <<25,74,167,86,1,56,150,183,75,74,42,59,10,244,83,158>>,
		sqn = <<107,246,148,56,194,228>>,
		amf = <<95,103>>,
		op = <<70,10,72,56,84,39,170,57,38,74,172,142,252,158,115,232>>,
		opc = <<195,90,10,176,188,191,201,37,44,175,241,95,36,239,189,224>>,
		f1 = <<189,7,211,0,59,158,92,195>>,
		'f1*' = <<188,182,194,252,173,21,34,80>>,
		f2 = <<140,37,161,108,217,24,161,223>>,
		f5 = <<126,100,85,243,76,243>>,
		f3 = <<76,208,132,96,32,248,250,7,49,221,71,203,220,107,228,17>>,
		f4 = <<136,171,128,164,21,241,92,115,113,18,84,161,211,136,246,150>>,
		'f5*' = <<220,109,208,30,143,21>>};
test_set(8) ->
	#test_set{
		k = <<165,48,167,254,66,143,173,16,130,196,94,221,252,225,56,132>>,
		rand = <<58,76,43,50,69,197,14,181,199,29,8,99,147,149,118,77>>,
		sqn = <<246,63,93,118,135,132>>,
		amf = <<185,14>>,
		op = <<81,28,108,78,131,227,140,137,177,197,216,221,230,36,38,250>>,
		opc = <<39,149,62,73,188,138,246,220,198,231,48,235,128,40,107,227>>,
		f1 = <<83,118,31,189,103,155,11,173>>,
		'f1*' = <<33,173,253,51,74,16,231,206>>,
		f2 = <<166,50,65,225,255,195,229,171>>,
		f5 = <<136,25,108,71,152,111>>,
		f3 = <<16,240,91,171,117,169,154,95,187,152,169,194,135,103,156,59>>,
		f4 = <<249,236,8,101,235,50,242,35,105,202,222,64,197,156,58,68>>,
		'f5*' = <<201,135,163,210,49,21>>};
test_set(9) ->
	#test_set{
		k = <<217,21,28,240,72,150,226,88,48,191,46,8,38,123,131,96>>,
		rand = <<247,97,229,233,61,96,63,235,115,14,39,85,108,184,162,202>>,
		sqn = <<71,238,1,153,130,10>>,
		amf = <<145,19>>,
		op = <<117,252,34,51,164,66,148,238,142,109,226,92,67,83,210,107>>,
		opc = <<196,201,62,255,232,160,129,56,194,3,212,194,124,228,227,217>>,
		f1 = <<102,204,75,228,72,98,175,31>>,
		'f1*' = <<122,75,141,122,135,83,242,70>>,
		f2 = <<74,144,178,23,26,200,58,118>>,
		f5 = <<130,160,245,40,122,113>>,
		f3 = <<113,35,107,113,41,249,178,42,183,126,167,165,76,150,218,34>>,
		f4 = <<144,82,126,186,165,88,137,104,219,65,114,115,37,160,77,158>>,
		'f5*' = <<82,125,191,65,243,95>>};
test_set(10) ->
	#test_set{
		k = <<160,226,151,27,104,34,232,211,84,161,140,194,53,98,78,203>>,
		rand = <<8,239,248,40,177,63,219,86,39,34,198,92,127,48,169,178>>,
		sqn = <<219,92,6,100,129,224>>,
		amf = <<113,107>>,
		op = <<50,55,146,250,202,33,251,77,93,111,19,193,69,169,210,193>>,
		opc = <<130,162,111,34,187,169,233,72,143,148,154,16,217,142,156,196>>,
		f1 = <<148,133,254,36,98,28,185,246>>,
		'f1*' = <<188,227,37,206,3,226,233,185>>,
		f2 = <<75,194,33,45,134,36,145,10>>,
		f5 = <<162,248,88,170,158,93>>,
		f3 = <<8,206,246,208,4,236,97,71,26,60,60,218,4,129,55,250>>,
		f4 = <<237,3,24,202,93,235,146,6,39,47,110,143,166,75,164,17>>,
		'f5*' = <<116,231,111,187,236,56>>};
test_set(11) ->
	#test_set{
		k = <<13,166,247,186,134,213,234,200,161,156,245,99,172,88,100,45>>,
		rand = <<103,154,196,219,172,215,210,51,255,157,104,6,244,20,156,227>>,
		sqn = <<110,35,49,214,146,173>>,
		amf = <<34,74>>,
		op = <<75,154,38,250,69,158,58,203,255,54,244,1,93,227,189,193>>,
		opc = <<13,177,7,31,135,103,86,44,164,58,10,100,196,30,141,8>>,
		f1 = <<40,49,215,174,144,136,228,146>>,
		'f1*' = <<155,46,22,149,17,53,213,35>>,
		f2 = <<111,195,15,238,109,18,53,35>>,
		f5 = <<76,83,154,38,225,250>>,
		f3 = <<105,177,202,231,199,66,157,151,94,36,92,172,176,90,81,124>>,
		f4 = <<116,242,78,140,38,223,88,225,179,141,125,205,79,27,127,189>>,
		'f5*' = <<7,134,30,18,105,40>>};
test_set(12) ->
	#test_set{
		k = <<119,180,88,67,200,142,88,193,13,32,38,132,81,94,212,48>>,
		rand = <<76,71,235,48,118,220,85,254,81,6,203,32,52,184,205,120>>,
		sqn = <<254,26,135,49,0,93>>,
		amf = <<173,37>>,
		op = <<191,50,134,199,165,20,9,206,149,114,77,80,59,254,110,112>>,
		opc = <<212,131,175,174,86,36,9,163,38,181,187,11,32,196,215,98>>,
		f1 = <<8,51,45,126,159,72,69,112>>,
		'f1*' = <<237,65,183,52,72,157,82,7>>,
		f2 = <<174,250,53,123,234,194,168,122>>,
		f5 = <<48,255,37,205,173,246>>,
		f3 = <<144,140,67,240,86,156,184,247,75,201,113,231,6,195,108,95>>,
		f4 = <<194,81,223,13,136,141,217,50,155,207,70,101,91,34,110,64>>,
		'f5*' = <<232,78,208,212,103,126>>};
test_set(13) ->
	#test_set{
		k = <<114,155,23,114,146,112,221,135,204,223,27,254,41,180,233,187>>,
		rand = <<49,28,76,146,151,68,214,117,183,32,243,183,233,177,203,208>>,
		sqn = <<200,92,76,246,89,22>>,
		amf = <<91,178>>,
		op = <<208,76,156,53,189,34,98,250,129,13,41,36,208,54,253,19>>,
		opc = <<34,140,47,47,6,172,50,104,169,230,22,238,22,219,75,161>>,
		f1 = <<255,121,79,226,248,39,235,248>>,
		'f1*' = <<36,254,77,198,30,135,75,82>>,
		f2 = <<152,219,189,9,155,59,64,141>>,
		f5 = <<83,128,209,88,207,227>>,
		f3 = <<68,192,242,60,84,147,207,210,65,228,143,25,126,29,16,18>>,
		f4 = <<12,159,184,22,19,136,76,37,53,221,14,171,243,180,64,216>>,
		'f5*' = <<135,172,59,85,159,182>>};
test_set(14) ->
	#test_set{
		k = <<211,45,210,62,137,220,102,35,84,202,18,235,121,221,50,250>>,
		rand = <<207,125,10,177,217,67,6,149,11,241,32,24,251,212,104,135>>,
		sqn = <<72,65,7,229,106,67>>,
		amf = <<181,230>>,
		op = <<254,117,144,91,157,164,125,53,98,54,208,49,78,9,195,46>>,
		opc = <<210,42,75,65,128,165,50,87,8,165,255,112,217,246,126,199>>,
		f1 = <<207,25,214,43,106,128,152,102>>,
		'f1*' = <<93,38,149,55,228,94,44,230>>,
		f2 = <<175,74,65,30,17,57,242,194>>,
		f5 = <<33,122,244,146,114,173>>,
		f3 = <<90,248,107,128,237,183,13,245,41,44,193,18,28,186,213,12>>,
		f4 = <<127,77,106,231,68,14,24,120,154,139,117,173,63,66,240,58>>,
		'f5*' = <<144,14,16,28,103,126>>};
test_set(15) ->
	#test_set{
		k = <<175,124,101,225,146,114,33,222,89,17,135,162,197,152,122,83>>,
		rand = <<31,15,133,120,70,79,213,155,100,190,210,208,148,54,181,122>>,
		sqn = <<61,98,123,1,65,141>>,
		amf = <<132,246>>,
		op = <<12,122,203,141,149,183,212,163,28,90,202,109,38,52,90,136>>,
		opc = <<164,207,92,129,85,192,138,126,255,65,142,84,67,185,142,85>>,
		f1 = <<195,124,174,120,5,100,32,50>>,
		'f1*' = <<104,205,9,164,82,216,219,124>>,
		f2 = <<123,255,165,194,244,31,188,5>>,
		f5 = <<131,127,215,183,68,25>>,
		f3 = <<63,140,63,60,207,118,37,191,119,252,148,188,253,34,253,38>>,
		f4 = <<171,203,174,143,212,97,21,233,150,26,85,208,218,95,32,120>>,
		'f5*' = <<86,233,122,96,144,177>>};
test_set(16) ->
	#test_set{
		k = <<91,215,236,211,211,18,122,65,209,37,57,190,212,231,207,113>>,
		rand = <<89,183,95,20,37,28,117,3,29,11,203,172,28,44,4,199>>,
		sqn = <<162,152,174,137,41,220>>,
		amf = <<208,86>>,
		op = <<249,103,247,96,56,185,32,169,205,37,225,12,8,180,153,36>>,
		opc = <<118,8,157,60,15,243,239,220,110,54,114,29,79,206,183,71>>,
		f1 = <<195,242,92,217,67,9,16,126>>,
		'f1*' = <<176,200,186,52,54,101,175,204>>,
		f2 = <<126,63,68,199,89,31,111,69>>,
		f5 = <<91,225,20,149,82,93>>,
		f3 = <<212,43,45,97,94,73,160,58,194,117,165,174,249,122,248,146>>,
		f4 = <<11,63,141,2,79,230,191,175,170,152,43,143,130,227,25,194>>,
		'f5*' = <<77,106,52,161,228,235>>};
test_set(17) ->
	#test_set{
		k = <<108,209,198,206,177,224,30,20,241,184,35,22,169,11,127,61>>,
		rand = <<246,155,120,243,0,160,86,139,206,159,12,185,60,75,228,201>>,
		sqn = <<180,252,229,254,176,89>>,
		amf = <<228,187>>,
		op = <<7,139,252,169,86,70,89,236,216,133,30,132,230,197,155,72>>,
		opc = <<162,25,220,55,241,220,125,102,115,139,88,67,199,153,242,6>>,
		f1 = <<105,169,8,105,194,104,203,123>>,
		'f1*' = <<46,15,220,249,253,28,250,106>>,
		f2 = <<112,246,189,185,173,33,82,95>>,
		f5 = <<28,64,138,133,139,62>>,
		f3 = <<110,218,249,158,91,217,248,93,95,54,217,28,18,114,251,75>>,
		f4 = <<214,28,133,60,40,13,217,196,111,41,123,174,195,134,222,23>>,
		'f5*' = <<170,74,229,45,170,48>>};
test_set(18) ->
	#test_set{
		k = <<183,58,144,203,207,58,251,98,45,186,131,197,138,132,21,223>>,
		rand = <<177,32,241,193,160,16,42,47,80,125,213,67,222,104,40,31>>,
		sqn = <<241,232,165,35,163,109>>,
		amf = <<71,27>>,
		op = <<182,114,4,126,0,59,185,82,220,166,203,138,240,229,183,121>>,
		opc = <<223,12,103,134,143,162,95,116,139,112,68,198,231,194,69,184>>,
		f1 = <<235,215,3,65,188,212,21,176>>,
		'f1*' = <<18,53,159,93,130,34,12,20>>,
		f2 = <<71,157,210,92,32,121,45,99>>,
		f5 = <<174,253,170,93,221,153>>,
		f3 = <<102,25,93,190,208,49,50,116,197,202,119,102,97,95,162,94>>,
		f4 = <<102,190,199,7,235,42,252,71,109,116,8,168,242,146,123,54>>,
		'f5*' = <<18,236,43,135,251,177>>};
test_set(19) ->
	#test_set{
		k = <<81,34,37,2,20,195,62,114,58,93,213,35,252,20,95,192>>,
		rand = <<129,233,43,108,14,224,225,46,188,235,168,217,42,153,223,165>>,
		sqn = <<22,243,179,247,15,194>>,
		amf = <<195,171>>,
		op = <<201,232,118,50,134,181,185,255,189,245,110,18,151,208,136,123>>,
		opc = <<152,29,70,76,124,82,235,110,80,54,35,73,132,173,11,207>>,
		f1 = <<42,92,35,209,94,227,81,213>>,
		'f1*' = <<98,218,227,133,63,58,249,210>>,
		f2 = <<40,215,176,242,162,236,61,229>>,
		f5 = <<173,161,90,235,123,184>>,
		f3 = <<83,73,251,224,152,100,159,148,143,93,46,151,58,129,192,15>>,
		f4 = <<151,68,135,26,211,43,249,187,209,221,92,229,78,62,46,90>>,
		'f5*' = <<212,97,188,21,71,93>>};
test_set(20) ->
	#test_set{
		k = <<144,220,164,237,164,91,83,207,15,18,215,201,195,188,106,137>>,
		rand = <<159,221,199,32,146,198,173,3,107,110,70,71,137,49,91,120>>,
		sqn = <<32,248,19,189,65,65>>,
		amf = <<97,223>>,
		op = <<63,252,254,91,123,17,17,88,153,32,211,82,142,132,230,85>>,
		opc = <<203,156,204,196,185,37,142,109,202,71,96,55,159,184,37,129>>,
		f1 = <<9,219,148,234,180,248,20,158>>,
		'f1*' = <<162,148,104,170,151,117,181,39>>,
		f2 = <<169,81,0,226,118,9,82,205>>,
		f5 = <<131,207,213,77,185,19>>,
		f3 = <<181,242,218,3,136,59,105,249,107,245,46,2,158,217,172,69>>,
		f4 = <<180,114,19,104,188,22,234,103,135,92,85,152,104,139,176,239>>,
		'f5*' = <<79,32,57,57,45,220>>}.

