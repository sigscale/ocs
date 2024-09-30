%%% ocs_diameter_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
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
%%%  @doc Test suite for DIAMETER operations
%%%  		in the {@link //ocs. ocs} application.
%%%
-module(ocs_diameter_SUITE).
-copyright('Copyright (c) 2016 - 2024 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% test cases
-export([acct_server/0, acct_server/1,
		origin_host/0, origin_host/1,
		origin_realm/0, origin_realm/1,
		listen_tcp/0, listen_tcp/1,
		listen_sctp/0, listen_sctp/1,
		connect_tcp/0, connect_tcp/1,
		connect_sctp/0, connect_sctp/1]).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for DIAMETER operations in OCS"}]},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	RemoteAddress = {127,0,0,1},
	{ok, _} = ocs:add_client(RemoteAddress, undefined, diameter, undefined, true),
	[{ct_diameter_address, RemoteAddress} | Config].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(connect_tcp, Config) ->
	ok = application:start(diameter),
	Host = "ct",
	Realm = "exemple.net",
	Port = rand:uniform(64511) + 1024,
	ServiceOptions = service_options(Host, Realm),
	ok = diameter:start_service(?MODULE, ServiceOptions),
	Address = proplists:get_value(ct_diameter_address, Config),
	TransportConfig = [{ip, Address}, {port, Port}, {reuseaddr, true}],
	TransportOptions = [{transport_config, TransportConfig}],
	{ok, _Ref} = diameter:add_transport(?MODULE, {listen, TransportOptions}),
	true = diameter:subscribe(?MODULE),
	[{ct_diameter_port, Port} | Config];
init_per_testcase(connect_sctp, Config) ->
	ok = application:start(diameter),
	Host = "ct",
	Realm = "exemple.net",
	Port = rand:uniform(64511) + 1024,
	ServiceOptions = service_options(Host, Realm),
	ok = diameter:start_service(?MODULE, ServiceOptions),
	Address = proplists:get_value(ct_diameter_address, Config),
	TransportConfig = [{transport_module, diameter_sctp},
			{ip, Address}, {port, Port}, {reuseaddr, true}],
	TransportOptions = [{transport_config, TransportConfig}],
	{ok, _Ref} = diameter:add_transport(?MODULE, {listen, TransportOptions}),
	true = diameter:subscribe(?MODULE),
	[{ct_diameter_port, Port} | Config];
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(connect_tcp, Config) ->
	diameter:stop_service(?MODULE),
	application:stop(diameter),
	ocs_test_lib:stop(),
	proplists:delete(ct_diameter_port, Config);
end_per_testcase(connect_sctp, Config) ->
	diameter:stop_service(?MODULE),
	application:stop(diameter),
	ocs_test_lib:stop(),
	proplists:delete(ct_diameter_port, Config);
end_per_testcase(_TestCase, _Config) ->
	ocs_test_lib:stop().

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[acct_server, origin_host, origin_realm, listen_tcp, listen_sctp,
			connect_tcp, connect_sctp].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

acct_server() ->
	[{userdata, [{doc, "Start an accounting server."}]}].

acct_server(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_acct_service, Address, Port},
	ok = application:load(ocs),
	Acct = {Address, Port, []},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	[Service] = diameter:services().
	
origin_host() ->
	[{userdata, [{doc, "Set Origin-Host on diameter service."}]}].

origin_host(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_acct_service, Address, Port},
	OriginHost1 = lists:concat([?FUNCTION_NAME, ".example.net"]),
	ok = application:load(ocs),
	Options = [{'Origin-Host', OriginHost1}],
	Acct = {Address, Port, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	OriginHost2 = diameter:service_info(Service, 'Origin-Host'),
	OriginHost1 = binary_to_list(OriginHost2).

origin_realm() ->
	[{userdata, [{doc, "Set Origin-Realm on diameter service."}]}].

origin_realm(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_acct_service, Address, Port},
	OriginRealm1 = "example.net",
	ok = application:load(ocs),
	Options = [{'Origin-Realm', OriginRealm1}],
	Acct = {Address, Port, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	OriginRealm2 = diameter:service_info(Service, 'Origin-Realm'),
	OriginRealm1 = binary_to_list(OriginRealm2).

listen_tcp() ->
	[{userdata, [{doc, "Explicit TCP listening diameter service."}]}].

listen_tcp(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_acct_service, Address, Port},
	ok = application:load(ocs),
	TransportOptions1 = [{transport_module, diameter_tcp}],
	Options = [{listen, TransportOptions1}],
	Acct = {Address, Port, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	[Transport] = diameter:service_info(Service, transport),
	{_, listen} = lists:keyfind(type, 1, Transport),
	{_, TransportOptions2} = lists:keyfind(options, 1, Transport),
	{_, diameter_tcp} = lists:keyfind(transport_module, 1, TransportOptions2).
	
listen_sctp() ->
	[{userdata, [{doc, "STCP listening diameter service."}]}].

listen_sctp(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_acct_service, Address, Port},
	ok = application:load(ocs),
	TransportOptions1 = [{transport_module, diameter_sctp}],
	Options = [{listen, TransportOptions1}],
	Acct = {Address, Port, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	[Transport] = diameter:service_info(Service, transport),
	{_, listen} = lists:keyfind(type, 1, Transport),
	{_, TransportOptions2} = lists:keyfind(options, 1, Transport),
	{_, diameter_sctp} = lists:keyfind(transport_module, 1, TransportOptions2).
	
connect_tcp() ->
	[{userdata, [{doc, "Connecting TCP client diameter service."}]}].

connect_tcp(Config) ->
	RemotePort = proplists:get_value(ct_diameter_port, Config),
	RemoteAddress = proplists:get_value(ct_diameter_address, Config),
	LocalAddress = {127,0,0,1},
	LocalPort = 0,
	Service = {ocs_diameter_acct_service, LocalAddress, LocalPort},
	ok = application:load(ocs),
	TransportConfig = [{raddr, RemoteAddress}, {rport, RemotePort}],
	TransportOptions1 = [{transport_config, TransportConfig}],
	Options = [{connect, TransportOptions1}],
	Acct = {LocalAddress, LocalPort, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	receive
		#diameter_event{service = Service, info = Info}
				when element(1, Info) == up ->
			ok
	after
		5000 ->
			{fail, timeout}
	end.
	
connect_sctp() ->
	[{userdata, [{doc, "Connecting SCTP client diameter service."}]}].

connect_sctp(Config) ->
	RemotePort = proplists:get_value(ct_diameter_port, Config),
	RemoteAddress = proplists:get_value(ct_diameter_address, Config),
	LocalAddress = {127,0,0,1},
	LocalPort = 0,
	Service = {ocs_diameter_acct_service, LocalAddress, LocalPort},
	ok = application:load(ocs),
	TransportConfig = [{raddr, RemoteAddress}, {rport, RemotePort}],
	TransportOptions1 = [{transport_module, diameter_sctp},
			{transport_config, TransportConfig}],
	Options = [{connect, TransportOptions1}],
	Acct = {LocalAddress, LocalPort, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	receive
		#diameter_event{service = Service, info = Info}
				when element(1, Info) == up ->
			ok
	after
		5000 ->
			{fail, timeout}
	end.
	
%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

service_options(Host, Realm) ->
	[{'Origin-Host', Host},
			{'Origin-Realm', Realm},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Server (Acct)"},
			{'Auth-Application-Id', [?RO_APPLICATION_ID]},
			{string_decode, false},
			{restrict_connections, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_server_cb}]},
			{application, [{alias, cc_app_test},
					{dictionary, diameter_gen_3gpp_ro_application},
					{module, diameter_test_server_cb}]}].

