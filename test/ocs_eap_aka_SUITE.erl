%%% ocs_eap_aka_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
%%%  @doc Test suite for authentication using Extensible Authentication
%%% 	Protocol (EAP) using only a password (EAP-AKA)
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_eap_aka_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("../include/diameter_gen_eap_application_rfc4072.hrl").
-include_lib("kernel/include/inet.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(EAP_APPLICATION_ID, 5).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for authentication with EAP-AKA in OCS"}]},
	{timetrap, {seconds, 8}},
	{require, radius_username}, {default_config, radius_username, "ocs"},
	{require, radius_password}, {default_config, radius_password, "ocs123"},
	{require, radius_shared_secret},{default_config, radius_shared_secret, "xyzzy5461"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	RadiusPort = rand:uniform(64511) + 1024,
	Options = [{eap_method_prefer, akap}, {eap_method_order, [akap]}],
	RadiusAppVar = [{auth, [{{127,0,0,1}, RadiusPort, Options}]}],
	ok = application:set_env(ocs, radius, RadiusAppVar),
	DiameterPort = rand:uniform(64511) + 1024,
	DiameterAppVar = [{auth, [{{127,0,0,1}, DiameterPort, Options}]}],
	ok = application:set_env(ocs, diameter, DiameterAppVar),
	ok = ocs_test_lib:start(),
	{ok, ProdID} = ocs_test_lib:add_offer(),
	{ok, DiameterConfig} = application:get_env(ocs, diameter),
	{auth, [{Address, Port, _} | _]} = lists:keyfind(auth, 1, DiameterConfig),
	ok = diameter:start_service(?MODULE, client_service_opts()),
	true = diameter:subscribe(?MODULE),
	{ok, _Ref} = connect(?MODULE, Address, Port, diameter_tcp),
	receive
		#diameter_event{service = ?MODULE, info = Info}
				when element(1, Info) == up ->
			[{product_id, ProdID}, {diameter_client, Address} | Config];
		_ ->
			{skip, diameter_client_service_not_started}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = diameter:stop_service(?MODULE),
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	{ok, RadiusConfig} = application:get_env(ocs, radius),
	{auth, [{RadIP, RadPort, _} | _]} = lists:keyfind(auth, 1, RadiusConfig),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, RadIP}, binary]),
	SharedSecret = ct:get_config(radius_shared_secret),
	Protocol = radius,
	{ok, _} = ocs:add_client(RadIP, RadPort, Protocol, SharedSecret, true),
	NasId = atom_to_list(node()),
	[{nas_id, NasId}, {socket, Socket}, {radius_client, RadIP} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	RadClient = ?config(radius_client, Config),
	ok = ocs:delete_client(RadClient),
	ok = gen_udp:close(Socket).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[]. 

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @hidden
client_service_opts() ->
	OriginHost = ocs:generate_password() ++ "@siscale.org",
	OriginRealm = ocs:generate_password() ++ "@siscale.org",
	[{'Origin-Host', OriginHost},
		{'Origin-Realm', OriginRealm},
		{'Vendor-Id', 10415},
		{'Product-Name', "SigScale Test Client (auth)"},
		{'Auth-Application-Id', [?BASE_APPLICATION_ID, ?EAP_APPLICATION_ID]},
		{string_decode, false},
		{application, [{alias, base_app_test},
				{dictionary, diameter_gen_base_rfc6733},
				{module, diameter_test_client_cb}]},
		{application, [{alias, eap_app_test},
				{dictionary, diameter_gen_eap_application_rfc4072},
				{module, diameter_test_client_cb}]}].

%% @doc Add a transport capability to diameter service.
%% @hidden
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect(SvcName, [{connect_timer, 30000} | transport_opts(Address, Port, Transport)]).

%% @hidden
connect(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans}, {transport_config,
			[{raddr, RemAddr}, {rport, RemPort},
			{reuseaddr, true}, {ip, LocalAddr}]}].

