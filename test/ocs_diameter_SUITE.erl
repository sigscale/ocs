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
		auth_server/0, auth_server/1,
		acct_origin_host/0, acct_origin_host/1,
		auth_origin_host/0, auth_origin_host/1,
		acct_origin_realm/0, acct_origin_realm/1,
		auth_origin_realm/0, auth_origin_realm/1,
		acct_listen_tcp/0, acct_listen_tcp/1,
		auth_listen_tcp/0, auth_listen_tcp/1,
		acct_listen_sctp/0, acct_listen_sctp/1,
		auth_listen_sctp/0, auth_listen_sctp/1,
		acct_connect_tcp/0, acct_connect_tcp/1,
		auth_connect_tcp/0, auth_connect_tcp/1,
		acct_connect_sctp/0, acct_connect_sctp/1,
		auth_connect_sctp/0, auth_connect_sctp/1]).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").

-define(BASE_APPLICATION_ID, 0).
-define(RO_APPLICATION_ID, 4).
-define(EAP_APPLICATION_ID, 5).
-define(SWm_APPLICATION_ID, 16777264).
-define(STa_APPLICATION_ID, 16777250).
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
init_per_testcase(acct_connect_tcp, Config) ->
	ok = application:start(diameter),
	Host = "ct",
	Realm = "exemple.net",
	Port = rand:uniform(64511) + 1024,
	ServiceName = make_ref(),
	ServiceOptions = service_options(acct, Host, Realm),
	ok = diameter:start_service(ServiceName, ServiceOptions),
	Address = proplists:get_value(ct_diameter_address, Config),
	TransportConfig = [{ip, Address}, {port, Port}, {reuseaddr, true}],
	TransportOptions = [{transport_config, TransportConfig}],
	{ok, _Ref} = diameter:add_transport(ServiceName,
			{listen, TransportOptions}),
	true = diameter:subscribe(ServiceName),
	[{rservice, ServiceName}, {rport, Port} | Config];
init_per_testcase(auth_connect_tcp, Config) ->
	ok = application:start(diameter),
	Host = "ct",
	Realm = "exemple.net",
	Port = rand:uniform(64511) + 1024,
	ServiceName = make_ref(),
	ServiceOptions = service_options(auth, Host, Realm),
	ok = diameter:start_service(ServiceName, ServiceOptions),
	Address = proplists:get_value(ct_diameter_address, Config),
	TransportConfig = [{ip, Address}, {port, Port}, {reuseaddr, true}],
	TransportOptions = [{transport_config, TransportConfig}],
	{ok, _Ref} = diameter:add_transport(ServiceName,
			{listen, TransportOptions}),
	true = diameter:subscribe(ServiceName),
	[{rservice, ServiceName}, {rport, Port} | Config];
init_per_testcase(acct_connect_sctp, Config) ->
	ok = application:start(diameter),
	Host = "ct",
	Realm = "exemple.net",
	Port = rand:uniform(64511) + 1024,
	ServiceName = make_ref(),
	ServiceOptions = service_options(acct, Host, Realm),
	ok = diameter:start_service(ServiceName, ServiceOptions),
	Address = proplists:get_value(ct_diameter_address, Config),
	TransportConfig = [{transport_module, diameter_sctp},
			{ip, Address}, {port, Port}, {reuseaddr, true}],
	TransportOptions = [{transport_config, TransportConfig}],
	{ok, _Ref} = diameter:add_transport(ServiceName,
			{listen, TransportOptions}),
	true = diameter:subscribe(ServiceName),
	[{rservice, ServiceName}, {rport, Port} | Config];
init_per_testcase(auth_connect_sctp, Config) ->
	ok = application:start(diameter),
	Host = "ct",
	Realm = "exemple.net",
	Port = rand:uniform(64511) + 1024,
	ServiceName = make_ref(),
	ServiceOptions = service_options(auth, Host, Realm),
	ok = diameter:start_service(ServiceName, ServiceOptions),
	Address = proplists:get_value(ct_diameter_address, Config),
	TransportConfig = [{transport_module, diameter_sctp},
			{ip, Address}, {port, Port}, {reuseaddr, true}],
	TransportOptions = [{transport_config, TransportConfig}],
	{ok, _Ref} = diameter:add_transport(ServiceName,
			{listen, TransportOptions}),
	true = diameter:subscribe(ServiceName),
	[{rservice, ServiceName}, {rport, Port} | Config];
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config)
		when TestCase == acct_connect_tcp;
		TestCase == auth_connect_tcp;
		TestCase == acct_connect_sctp;
		TestCase == auth_connect_sctp ->
	ServiceName = proplists:get_value(rservice, Config),
	diameter:unsubscribe(ServiceName),
	diameter:stop_service(ServiceName),
	application:stop(diameter),
	ocs_test_lib:stop(),
	Config1 = proplists:delete(rservice, Config),
	Config2 = proplists:delete(rport, Config1),
	Config2;
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
	[acct_server, auth_server, acct_origin_host, auth_origin_host,
			acct_origin_realm, auth_origin_realm, acct_listen_tcp,
			auth_listen_sctp, acct_listen_sctp, auth_listen_sctp,
			acct_connect_tcp, auth_connect_tcp, acct_connect_sctp,
			auth_connect_sctp].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

acct_server() ->
	[{userdata, [{doc, "Start an acct server."}]}].

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

auth_server() ->
	[{userdata, [{doc, "Start an auth server."}]}].

auth_server(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_auth_service, Address, Port},
	ok = application:load(ocs),
	Auth = {Address, Port, []},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	[Service] = diameter:services().

acct_origin_host() ->
	[{userdata, [{doc, "Set Origin-Host on diameter acct service."}]}].

acct_origin_host(_Config) ->
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

auth_origin_host() ->
	[{userdata, [{doc, "Set Origin-Host on diameter auth service."}]}].

auth_origin_host(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_auth_service, Address, Port},
	OriginHost1 = lists:concat([?FUNCTION_NAME, ".example.net"]),
	ok = application:load(ocs),
	Options = [{'Origin-Host', OriginHost1}],
	Auth = {Address, Port, Options},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	OriginHost2 = diameter:service_info(Service, 'Origin-Host'),
	OriginHost1 = binary_to_list(OriginHost2).

acct_origin_realm() ->
	[{userdata, [{doc, "Set Origin-Realm on diameter acct service."}]}].

acct_origin_realm(_Config) ->
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

auth_origin_realm() ->
	[{userdata, [{doc, "Set Origin-Realm on diameter auth service."}]}].

auth_origin_realm(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_auth_service, Address, Port},
	OriginRealm1 = "example.net",
	ok = application:load(ocs),
	Options = [{'Origin-Realm', OriginRealm1}],
	Auth = {Address, Port, Options},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	OriginRealm2 = diameter:service_info(Service, 'Origin-Realm'),
	OriginRealm1 = binary_to_list(OriginRealm2).

acct_listen_tcp() ->
	[{userdata, [{doc, "Explicit TCP listening diameter acct service."}]}].

acct_listen_tcp(_Config) ->
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

auth_listen_tcp() ->
	[{userdata, [{doc, "Explicit TCP listening diameter auth service."}]}].

auth_listen_tcp(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_auth_service, Address, Port},
	ok = application:load(ocs),
	TransportOptions1 = [{transport_module, diameter_tcp}],
	Options = [{listen, TransportOptions1}],
	Auth = {Address, Port, Options},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	[Transport] = diameter:service_info(Service, transport),
	{_, listen} = lists:keyfind(type, 1, Transport),
	{_, TransportOptions2} = lists:keyfind(options, 1, Transport),
	{_, diameter_tcp} = lists:keyfind(transport_module, 1, TransportOptions2).

acct_listen_sctp() ->
	[{userdata, [{doc, "STCP listening diameter acct service."}]}].

acct_listen_sctp(_Config) ->
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

auth_listen_sctp() ->
	[{userdata, [{doc, "STCP listening diameter auth service."}]}].

auth_listen_sctp(_Config) ->
	Address = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	Service = {ocs_diameter_auth_service, Address, Port},
	ok = application:load(ocs),
	TransportOptions1 = [{transport_module, diameter_sctp}],
	Options = [{listen, TransportOptions1}],
	Auth = {Address, Port, Options},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	[Transport] = diameter:service_info(Service, transport),
	{_, listen} = lists:keyfind(type, 1, Transport),
	{_, TransportOptions2} = lists:keyfind(options, 1, Transport),
	{_, diameter_sctp} = lists:keyfind(transport_module, 1, TransportOptions2).

acct_connect_tcp() ->
	[{userdata, [{doc, "Connecting TCP client diameter acct service."}]}].

acct_connect_tcp(Config) ->
	RemoteService = proplists:get_value(rservice, Config),
	RemotePort = proplists:get_value(rport, Config),
	RemoteAddress = proplists:get_value(ct_diameter_address, Config),
	LocalAddress = {127,0,0,1},
	LocalPort = 0,
	ok = application:load(ocs),
	TransportConfig = [{raddr, RemoteAddress}, {rport, RemotePort}],
	TransportOptions1 = [{transport_config, TransportConfig}],
	Options = [{connect, TransportOptions1}],
	Acct = {LocalAddress, LocalPort, Options},
	AcctConfig = [{acct, [Acct]}],
	ok = application:set_env(ocs, diameter, AcctConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	receive
		#diameter_event{service = RemoteService, info = Info}
				when element(1, Info) == up ->
			ok
	after
		5000 ->
			{fail, timeout}
	end.

auth_connect_tcp() ->
	[{userdata, [{doc, "Connecting TCP client diameter auth service."}]}].

auth_connect_tcp(Config) ->
	RemoteService = proplists:get_value(rservice, Config),
	RemotePort = proplists:get_value(rport, Config),
	RemoteAddress = proplists:get_value(ct_diameter_address, Config),
	LocalAddress = {127,0,0,1},
	LocalPort = 0,
	ok = application:load(ocs),
	TransportConfig = [{raddr, RemoteAddress}, {rport, RemotePort}],
	TransportOptions1 = [{transport_config, TransportConfig}],
	Options = [{connect, TransportOptions1}],
	Auth = {LocalAddress, LocalPort, Options},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	receive
		#diameter_event{service = RemoteService, info = Info}
				when element(1, Info) == up ->
			ok
	after
		5000 ->
			{fail, timeout}
	end.

acct_connect_sctp() ->
	[{userdata, [{doc, "Connecting SCTP client diameter acct service."}]}].

acct_connect_sctp(Config) ->
	RemoteService = proplists:get_value(rservice, Config),
	RemotePort = proplists:get_value(rport, Config),
	RemoteAddress = proplists:get_value(ct_diameter_address, Config),
	LocalAddress = {127,0,0,1},
	LocalPort = 0,
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
		#diameter_event{service = RemoteService, info = Info}
				when element(1, Info) == up ->
			ok
	after
		5000 ->
			{fail, timeout}
	end.

auth_connect_sctp() ->
	[{userdata, [{doc, "Connecting SCTP client diameter auth service."}]}].

auth_connect_sctp(Config) ->
	RemoteService = proplists:get_value(rservice, Config),
	RemotePort = proplists:get_value(rport, Config),
	RemoteAddress = proplists:get_value(ct_diameter_address, Config),
	LocalAddress = {127,0,0,1},
	LocalPort = 0,
	ok = application:load(ocs),
	TransportConfig = [{raddr, RemoteAddress}, {rport, RemotePort}],
	TransportOptions1 = [{transport_module, diameter_sctp},
			{transport_config, TransportConfig}],
	Options = [{connect, TransportOptions1}],
	Auth = {LocalAddress, LocalPort, Options},
	AuthConfig = [{auth, [Auth]}],
	ok = application:set_env(ocs, diameter, AuthConfig, [{persistent, true}]),
	ok = ocs_test_lib:start(),
	receive
		#diameter_event{service = RemoteService, info = Info}
				when element(1, Info) == up ->
			ok
	after
		5000 ->
			{fail, timeout}
	end.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

service_options(acct, Host, Realm) ->
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
					{module, diameter_test_server_cb}]}];
service_options(auth, Host, Realm) ->
	[{'Origin-Host', Host},
			{'Origin-Realm', Realm},
			{'Vendor-Id', ?IANA_PEN_SigScale},
			{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
			{'Product-Name', "SigScale Test Server (Auth)"},
			{'Auth-Application-Id',
					[?BASE_APPLICATION_ID,
					?EAP_APPLICATION_ID,
					?SWm_APPLICATION_ID,
					?STa_APPLICATION_ID]},
			{string_decode, false},
			{restrict_connections, false},
			{application, [{alias, base_app_test},
					{dictionary, diameter_gen_base_rfc6733},
					{module, diameter_test_client_cb}]},
			{application, [{alias, eap_app_test},
					{dictionary, diameter_gen_eap_application_rfc4072},
					{module, diameter_test_server_cb}]},
			{application, [{alias, swm_app_test},
					{dictionary, diameter_gen_3gpp_swm_application},
					{module, diameter_test_server_cb}]},
			{application, [{alias, sta_app_test},
					{dictionary, diameter_gen_3gpp_sta_application},
					{module, diameter_test_server_cb}]}].

