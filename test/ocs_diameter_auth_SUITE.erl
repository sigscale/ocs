%%% ocs_diameter_auth_SUITE.erl
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
%%%  @doc Test suite for authentication with DIAMETER protocol in
%%%  {@link //ocs. ocs}
%%%
-module(ocs_diameter_auth_SUITE).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for authentication with DIAMETER protocol in OCS"}]},
	{timetrap, {seconds, 8}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	{ok, [{auth, AuthInstance}, {acct, _AcctInstance}]} = application:get_env(ocs, diameter),
	[{Address, Port, _}] = AuthInstance,
	SvcName1 = diameter_client_base_service,
	true = diameter:subscribe(SvcName1),
	ok = diameter:start_service(SvcName1, base_service_opts()),
	{ok, _Ref1} = connect(SvcName1, Address, Port, diameter_tcp),
	receive
		#diameter_event{service = SvcName1, info = start} ->
			NewConfig = [{svc_name1, SvcName1}] ++ Config,
			SvcName2 = diameter_client_nas_service,
			true = diameter:subscribe(SvcName2),
			ok = diameter:start_service(SvcName2, nas_service_opts()),
			{ok, _Ref2} = connect(SvcName2, Address, Port, diameter_tcp),
			receive
				#diameter_event{service = SvcName2, info = start} ->
					[{svc_name2, SvcName2}] ++ NewConfig;
				_ ->
					{skip, diameter_client_nas_service_not_started}
			end;
		_ ->
			{skip, diameter_client_base_service_not_started}
	end.


-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	SvcName1 = ?config(svc_name1, Config),
	SvcName2 = ?config(svc_name2, Config),
	ok = diameter:stop_service(SvcName1),
	ok = diameter:stop_service(SvcName2),
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
	[nas_authentication].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
nas_authentication() ->
	[{userdata, [{doc, "DIAMETER Authentication in NAS environment"}]}].

nas_authentication(Config) ->
erlang:display({xxxxxx, ?MODULE, ?FUNCTION_NAME, diameter:services()}),
	SvcName1 = ?config(svc_name1, Config),
	SId = diameter:session_id(atom_to_list(SvcName1)),
	RAR = #diameter_base_RAR{'Session-Id' = SId, 'Auth-Application-Id' = 1,
		'Re-Auth-Request-Type' = 0},
erlang:display({xxxxxx, ?MODULE, ?FUNCTION_NAME, ?LINE}),
	Answer = diameter:call(SvcName1, base_app_test, RAR, []).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

diameter_client_response() ->
	receive
		{connected, Ref} ->
			{connected, Ref};
		{ok, #diameter_packet{} = Msg}->
			Msg;
		{error, Reason} ->
			{error, Reason}
	end.

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(diameter_client_base_service = SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]);
connect(diameter_client_nas_service = SvcName, _Address, _Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | [{transport_module, Transport}]]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
base_service_opts() ->
	[{'Origin-Host', "client.testdomain.com"},
		{'Origin-Realm', "testdomain.com"},
		{'Vendor-Id', 0},
		{'Product-Name', "Test Client"},
		{'Auth-Application-Id', [0]},
		{string_decode, false},
		{application, [{alias, base_app_test},
				{dictionary, diameter_gen_base_rfc6733},
				{module, diameter_test_client_cb}]}].

%% @hidden
nas_service_opts() ->
	[{'Origin-Host', "client.testdomain.com"},
		{'Origin-Realm', "testdomain.com"},
		{'Vendor-Id', 0},
		{'Product-Name', "Test Client"},
		{'Auth-Application-Id', [1]},
		{string_decode, false},
		{application, [{alias, nas_app_test},
				{dictionary, diameter_gen_nas_application_rfc7155},
				{module, diameter_test_client_cb}]}].

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans},
		{transport_config, [{raddr, RemAddr},
		{rport, RemPort},
		{reuseaddr, true}
		| [{ip, LocalAddr}]]}].

