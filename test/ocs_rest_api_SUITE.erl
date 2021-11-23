%%% ocs_rest_api_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
%%%  @doc Test suite for REST API
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_api_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include_lib("../include/diameter_gen_3gpp_ro_application.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PathBalanceHub, "/balanceManagement/v1/hub/").
-define(PathProductHub, "/productInventory/v2/hub/").
-define(PathServiceHub, "/serviceInventory/v2/hub/").
-define(PathUserHub, "/partyManagement/v1/hub/").
-define(PathCatalogHub, "/productCatalog/v2/hub/").
-define(PathResourceHub, "/resourceInventory/v1/hub/").
-define(PathUsageHub, "/usageManagement/v1/hub/").
-define(PathRole, "/partyRoleManagement/v4/").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for REST API in OCS"}]},
	{timetrap, {minutes, 1}},
	{require, rest},
	{default_config, rest, [{user, "bss"},
			{password, "nfc9xgp32xha"}, {group, "all"}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	{ok, Services} = application:get_env(inets, services),
	Fport = fun FPort([{httpd, L} | T]) ->
				case lists:keyfind(server_name, 1, L) of
					{_, "rest"} ->
						H1 = lists:keyfind(bind_address, 1, L),
						P1 = lists:keyfind(port, 1, L),
						{H1, P1};
					_ ->
						FPort(T)
				end;
			FPort([_ | T]) ->
				FPort(T)
	end,
	RestUser = ct:get_config({rest, user}),
	RestPass = ct:get_config({rest, password}),
	_RestGroup = ct:get_config({rest, group}),
	{Host, Port} = case Fport(Services) of
		{{_, H2}, {_, P2}} when H2 == "localhost"; H2 == {127,0,0,1} ->
			{ok, _} = ocs:add_user(RestUser, RestPass, "en"),
			{"localhost", P2};
		{{_, H2}, {_, P2}} ->
			{ok, _} = ocs:add_user(RestUser, RestPass, "en"),
			case H2 of
				H2 when is_tuple(H2) ->
					{inet:ntoa(H2), P2};
				H2 when is_list(H2) ->
					{H2, P2}
			end;
		{false, {_, P2}} ->
			{ok, _} = ocs:add_user(RestUser, RestPass, "en"),
			{"localhost", P2}
	end,
	{ok, ProductID} = ocs_test_lib:add_offer(),
	Config1 = [{port, Port}, {product_id, ProductID} | Config],
	HostUrl = "https://" ++ Host ++ ":" ++ integer_to_list(Port),
	[{host_url, HostUrl} | Config1].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = ocs_test_lib:stop(),
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(oauth_authentication, Config) ->
	ok = set_inet_mod(),
	application:stop(inets),
	application:start(inets),
	Config;
init_per_testcase(TestCase, Config) when TestCase == notify_create_bucket;
		TestCase == notify_delete_bucket;
		TestCase == notify_rating_deleted_bucket;
		TestCase == notify_accumulated_balance_threshold;
		TestCase == query_accumulated_balance_notification;
		TestCase == query_bucket_notification; TestCase == notify_product_charge;
		TestCase == notify_create_product; TestCase == notify_delete_product;
		TestCase == query_product_notification;
		TestCase == notify_create_service; TestCase == notify_delete_service;
		TestCase == query_service_notification;
		TestCase == notify_create_offer; TestCase == notify_delete_offer;
		TestCase == query_offer_notification;
		TestCase == notify_insert_gtt; TestCase == notify_delete_gtt;
		TestCase == query_gtt_notification;
		TestCase == notify_add_resource; TestCase == notify_delete_resource;
		TestCase == query_resource_notification;
		TestCase == notify_diameter_acct_log ->
	gen_event:delete_handler(ocs_event, ocs_event, []),
	true = register(TestCase, self()),
	case inets:start(httpd, [{port, 0},
			{server_name, ocs:generate_identity()},
			{server_root, "./"},
			{document_root, ?config(data_dir, Config)},
			{modules, [mod_esi]},
			{erl_script_alias, {"/listener", [?MODULE]}}]) of
		{ok, Pid} ->
			[{port, Port}] = httpd:info(Pid, [port]),
			Config1 = lists:keystore(listener_port, 1, Config, {listener_port, Port}),
			lists:keystore(listener_pid, 1, Config1, {listener_pid, Pid});
		{error, Reason} ->
			{error, Reason}
	end;
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(oauth_authentication, _Config) ->
	ok = set_inet_mod(),
	application:stop(inets),
	application:start(inets);
end_per_testcase(TestCase, Config) when TestCase == notify_create_bucket;
		TestCase == notify_delete_bucket;
		TestCase == notify_rating_deleted_bucket;
		TestCase == notify_accumulated_balance_threshold;
		TestCase == query_accumulated_balance_notification;
		TestCase == query_bucket_notification; TestCase == notify_product_charge;
		TestCase == notify_create_product; TestCase == notify_delete_product;
		TestCase == query_product_notification;
		TestCase == notify_create_service; TestCase == notify_delete_service;
		TestCase == query_service_notification;
		TestCase == notify_create_offer; TestCase == notify_delete_offer;
		TestCase == query_offer_notification;
		TestCase == notify_insert_gtt; TestCase == notify_delete_gtt;
		TestCase == query_gtt_notification;
		TestCase == notify_add_resource; TestCase == notify_delete_resource;
		TestCase == query_resource_notification;
		TestCase == notify_diameter_acct_log ->
	case lists:keyfind(listener_pid, 1, Config) of
		{listener_pid, Pid} ->
			case inets:stop(httpd, Pid) of
				ok ->
					ok;
				{error, Reason} ->
					{error, Reason}
			end;
		false ->
			{error, pid_not_found}
	end;
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
	[get_balance_range, authenticate_user_request, unauthenticate_user_request,
	add_user, get_user, delete_user,
	update_user_characteristics_json_patch,
	add_client, add_client_without_password, get_client, get_client_id,
	get_client_bogus, get_client_notfound, get_all_clients,
	get_client_range, get_clients_filter, delete_client,
	update_client_password_json_patch,
	update_client_attributes_json_patch,
	add_offer, get_offer, delete_offer, ignore_delete_offer, update_offer,
	add_service_inventory, add_service_inventory_without_password,
	get_service_inventory, get_all_service_inventories, add_service_aka,
	get_service_not_found, get_service_range, delete_service,
	update_service, get_usagespecs, get_usagespecs_query, get_usagespec,
	get_auth_usage, get_auth_usage_id, get_auth_usage_filter,
	get_auth_usage_range, get_acct_usage, get_acct_usage_id,
	get_acct_usage_filter, get_acct_usage_range, get_ipdr_usage,
	top_up, get_balance, get_balance_service, query_buckets,
	simultaneous_updates_on_client_failure, get_product, add_product,
	add_product_sms, update_product_realizing_service, delete_product,
	ignore_delete_product, query_product, filter_product,
	post_hub_balance, delete_hub_balance, get_balance_hubs, get_balance_hub,
	notify_create_bucket, notify_delete_bucket, notify_rating_deleted_bucket,
	notify_accumulated_balance_threshold, query_accumulated_balance_notification,
	query_bucket_notification, notify_product_charge,
	post_hub_product, delete_hub_product, get_product_hubs, get_product_hub,
	notify_create_product, notify_delete_product,
	query_product_notification, post_hub_service, delete_hub_service,
	get_service_hubs, get_service_hub,
	notify_create_service, notify_delete_service, query_service_notification,
	post_hub_user, delete_hub_user, get_user_hubs, get_user_hub,
	post_hub_catalog, delete_hub_catalog, get_catalog_hubs, get_catalog_hub,
	notify_create_offer, notify_delete_offer, query_offer_notification,
	post_hub_inventory, delete_hub_inventory, get_inventory_hubs,
	get_inventory_hub,
	notify_insert_gtt, notify_delete_gtt, query_gtt_notification,
	notify_add_resource, notify_delete_resource, query_resource_notification,
	post_hub_usage, get_usage_hubs, get_usage_hub, delete_hub_usage,
	notify_diameter_acct_log, get_tariff_resource, get_tariff_resources,
	post_tariff_resource, delete_tariff_resource, update_tariff_resource,
	post_policy_resource, query_policy_resource, arbitrary_char_service,
	delete_policy_table, oauth_authentication,
	post_hub_role, delete_hub_role, get_role_hubs, get_role_hub,
	post_role, delete_role, get_roles, get_role].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

authenticate_user_request() ->
	[{userdata, [{doc, "Authorized user request to the server"}]}].

authenticate_user_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ "/usageManagement/v1/usage", [Accept, auth_header()]},
	{ok, _Result} = httpc:request(get, Request, [], []).

unauthenticate_user_request() ->
	[{userdata, [{doc, "Authorized user request to the server"}]}].

unauthenticate_user_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ocs:generate_identity(),
	RestPass = ocs:generate_password(),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/usageManagement/v1/usage", [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 401, _}, _, _} = Result.

add_user() ->
	[{userdata, [{doc,"Add user in rest interface"}]}].

add_user(Config) ->
	ContentType = "application/json",
	ID = "King",
	Username = ID,
	Password = "KingKong",
	Locale = "en",
	PasswordAttr = {struct, [{"name", "password"}, {"value", Password}]},
	LocaleAttr = {struct, [{"name", "locale"}, {"value", Locale}]},
	UsernameAttr = {struct, [{"name", "username"}, {"value", Username}]},
	CharArray = {array, [UsernameAttr, PasswordAttr, LocaleAttr]},
	JSON = {struct, [{"id", ID}, {"characteristic", CharArray}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/partyManagement/v1/individual", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/partyManagement/v1/individual/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, {array, _Characteristic}} = lists:keyfind("characteristic", 1, Object),
	{ok, #httpd_user{username = Username, password = Password,
			user_data = UserData}} = ocs:get_user(ID),
	{_, Locale} = lists:keyfind(locale, 1, UserData).

get_user() ->
	[{userdata, [{doc,"get user in rest interface"}]}].

get_user(Config) ->
	ID = "Prince",
	Password = "Frog",
	Locale = "es",
	{ok, _} = ocs:add_user(ID, Password, Locale),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request2 = {HostUrl ++ "/partyManagement/v1/individual/" ++ ID, [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, _URI2} = lists:keyfind("href", 1, Object),
	{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
	F = fun(_F, [{struct, [{"name", Name}, {"value", Value}]} | _T], Name) ->
				{ok, Value};
			(_F, [{struct, [{"value", Value}, {"name", Name}]} | _T], Name) ->
				{ok, Value};
			(F, [_ | T], Name) ->
				F(F, T, Name);
			(_F, [], _Name) ->
				{error, not_found}
	end,
	{ok, ID} = F(F, Characteristic, "username"),
	{error, not_found} = F(F, Characteristic, "password"),
	{ok, Locale} = F(F, Characteristic, "locale").

delete_user() ->
	[{userdata, [{doc,"Delete user in rest interface"}]}].

delete_user(Config) ->
	ID = "Queen",
	Password = "QueenBee",
	Locale = "en",
	{ok, _} = ocs:add_user(ID, Password, Locale),
	HostUrl = ?config(host_url, Config),
	Request1 = {HostUrl ++ "/partyManagement/v1/individual/" ++ ID, [auth_header()]},
	{ok, Result1} = httpc:request(delete, Request1, [], []),
	{{"HTTP/1.1", 204, _NoContent}, _Headers1, []} = Result1,
	{error, no_such_user} = ocs:get_user(ID).

update_user_characteristics_json_patch() ->
	[{userdata, [{doc,"Use HTTP PATCH to update users's characteristics using
			json-patch media type"}]}].

update_user_characteristics_json_patch(Config) ->
	Username = "ryanstiles",
	Password = "wliaycaducb46",
	Locale = "en",
	{ok, _} = ocs:add_user(Username, Password, Locale),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request2 = {HostUrl ++ "/partyManagement/v1/individual/" ++ Username,
			[Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers1),
	{_, Etag} = lists:keyfind("etag", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, {array, Characteristic}} = lists:keyfind("characteristic", 1, Object),
	ContentType = "application/json-patch+json",
	NewPassword = ocs:generate_password(),
	NewPwdObj = {struct, [{"name", "password"}, {"value", NewPassword}]},
	NewLocale = "es",
	NewLocaleObj = {struct, [{"name", "locale"}, {"value", NewLocale}]},
	F1 = fun(_F, [{struct, [{"name", Name}, _]} | _T], Name, N) ->
				integer_to_list(N);
			(_F, [{struct, [_, {"name", Name}]} | _T], Name, N) ->
				integer_to_list(N);
			(F, [_ | T], Name, N) ->
				F(F, T, Name, N + 1);
			(_F, [], _Name, _N) ->
				"-"
	end,
	IndexPassword= F1(F1, Characteristic, "password", 0),
	IndexLocale = F1(F1, Characteristic, "locale", 0),
	JSON = {array, [
			{struct, [{op, "add"}, {path, "/characteristic/" ++ IndexPassword}, {value, NewPwdObj}]},
			{struct, [{op, "replace"}, {path, "/characteristic/" ++ IndexLocale}, {value, NewLocaleObj}]}]},
	PatchBody = lists:flatten(mochijson:encode(JSON)),
	PatchBodyLen = size(list_to_binary(PatchBody)),
	RestPort = ?config(port, Config),
	PatchReq = ["PATCH ", URI, " HTTP/1.1",$\r,$\n,
			"Content-Type:" ++ ContentType, $\r,$\n, "Accept:application/json",$\r,$\n,
			"Authorization:"++ basic_auth(),$\r,$\n,
			"Host:localhost:" ++ integer_to_list(RestPort),$\r,$\n,
			"If-Match:" ++ Etag,$\r,$\n,
			"Content-Length:" ++ integer_to_list(PatchBodyLen),$\r,$\n,
			$\r, $\n,
			PatchBody],
	{ok, SslSock} = ssl:connect({127,0,0,1}, RestPort,  [binary, {active, false}], infinity),
	ok = ssl:send(SslSock, list_to_binary(PatchReq)),
	Timeout = 1500,
	F2 = fun(_F, _Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			(F, Sock, {ok, Bin}, Acc) ->
					F(F, Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F2(F2, SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[Headers2, <<>>] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	<<"HTTP/1.1 204", _/binary>> = Headers2,
	ok = ssl:close(SslSock),
	{ok, #httpd_user{username = Username, password = NewPassword,
			user_data = UserData}} = ocs:get_user(Username),
	{_, NewLocale} = lists:keyfind(locale, 1, UserData).

add_client() ->
	[{userdata, [{doc,"Add client in rest interface"}]}].

add_client(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.9",
	Port = 3799,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/ocs/v1/client/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, Port} = lists:keyfind("port", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object).

add_client_without_password() ->
	[{userdata, [{doc,"Add client without password"}]}].

add_client_without_password(Config) ->
	ContentType = "application/json",
	JSON = {struct, [{"id", "10.5.55.10"}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, _Headers, ResponseBody} = Result,
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, 3799} = lists:keyfind("port", 1, Object),
	{_, "RADIUS"} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object),
	12 = length(Secret).

get_client() ->
	[{userdata, [{doc,"get client in rest interface"}]}].

get_client(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.9",
	Port = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	{URI2, _} = httpd_util:split_path(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers1),
	ContentLength = integer_to_list(length(Body1)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI2} = lists:keyfind("href", 1, Object),
	{_, Port} = lists:keyfind("port", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object).

get_client_id() ->
	[{userdata, [{doc,"get client with identifier"}]}].

get_client_id(Config) ->
	ID = "10.2.53.19",
	Identifier = "nas-01-23-45",
	Secret = "ps5mhybc297m",
	{ok, _} = ocs:add_client(ID, Secret),
	{ok, Address} = inet:parse_address(ID),
	Fun = fun() ->
				[C1] = mnesia:read(client, Address, write),
				C2 = C1#client{identifier = list_to_binary(Identifier)},
				mnesia:write(C2)
	end,
	{atomic, ok} = mnesia:transaction(Fun),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ "/ocs/v1/client/" ++ ID, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, _, Body} = Result,
	{struct, Object} = mochijson:decode(Body),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, Identifier} = lists:keyfind("identifier", 1, Object).

get_client_bogus() ->
	[{userdata, [{doc, "get client bogus in rest interface"}]}].

get_client_bogus(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ID = "beefbeefcafe",
	Request = {HostUrl ++ "/ocs/v1/client/" ++ ID, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 400, _BadRequest}, _Headers, _Body} = Result.

get_client_notfound() ->
	[{userdata, [{doc, "get client notfound in rest interface"}]}].

get_client_notfound(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ID = "10.2.53.20",
	Request = {HostUrl ++ "/ocs/v1/client/" ++ ID, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 404, _}, _Headers, _Body} = Result.

get_all_clients() ->
	[{userdata, [{doc,"get all clients in rest interface"}]}].

get_all_clients(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.8",
	Port = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c344npqc",
	JSON = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	Request1 = {HostUrl ++ "/ocs/v1/client", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	Request2 = {HostUrl ++ "/ocs/v1/client", [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers1),
	ContentLength = integer_to_list(length(Body1)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers1),
	{array, ClientsList} = mochijson:decode(Body1),
	Pred1 = fun({struct, Param}) ->
		case lists:keyfind("id", 1, Param) of
			{_, ID} ->
				true;
			{_, _ID} ->
				false
		end
	end,
	[{struct, ClientVar}] = lists:filter(Pred1, ClientsList),
	{_, URI1} = lists:keyfind("href", 1, ClientVar),
	{_, Port} = lists:keyfind("port", 1, ClientVar),
	{_, Protocol} = lists:keyfind("protocol", 1, ClientVar),
	{_, Secret} = lists:keyfind("secret", 1, ClientVar).

get_client_range() ->
	[{userdata, [{doc,"Get range of items in the client collection"}]}].

get_client_range(Config) ->
	{ok, PageSize} = application:get_env(ocs, rest_page_size),
	Fadd = fun(_F, 0) ->
				ok;
			(F, N) ->
				Address = {10, rand:uniform(255),
						rand:uniform(255), rand:uniform(254)},
				Secret = ocs:generate_password(),
				{ok, _} = ocs:add_client(Address, Secret),
				F(F, N - 1)
	end,
	NumAdded = (PageSize * 2) + (PageSize div 2) + 17,
	ok = Fadd(Fadd, NumAdded),
	RangeSize = case PageSize > 25 of
		true ->
			rand:uniform(PageSize - 10) + 10;
		false ->
			PageSize - 1
	end,
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RequestHeaders1 = [Accept, auth_header()],
	Request1 = {HostUrl ++ "/ocs/v1/client", RequestHeaders1},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, ResponseHeaders1, Body1} = Result1,
	{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders1),
	true = is_etag_valid(Etag),
	{_, AcceptRanges1} = lists:keyfind("accept-ranges", 1, ResponseHeaders1),
	true = lists:member("items", string:tokens(AcceptRanges1, ", ")),
	{_, Range1} = lists:keyfind("content-range", 1, ResponseHeaders1),
	["items", "1", RangeEndS1, "*"] = string:tokens(Range1, " -/"),
	RequestHeaders2 = RequestHeaders1 ++ [{"if-match", Etag}],
	PageSize = list_to_integer(RangeEndS1),
	{array, Clients1} = mochijson:decode(Body1),
	PageSize = length(Clients1),
	Fget = fun(F, RangeStart2, RangeEnd2) ->
				RangeHeader = [{"range",
						"items " ++ integer_to_list(RangeStart2)
						++ "-" ++ integer_to_list(RangeEnd2)}],
				RequestHeaders3 = RequestHeaders2 ++ RangeHeader,
				Request2 = {HostUrl ++ "/ocs/v1/client", RequestHeaders3},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, ResponseHeaders2, Body2} = Result2,
				{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders2),
				{_, AcceptRanges2} = lists:keyfind("accept-ranges", 1, ResponseHeaders2),
				true = lists:member("items", string:tokens(AcceptRanges2, ", ")),
				{_, Range} = lists:keyfind("content-range", 1, ResponseHeaders2),
				["items", RangeStartS, RangeEndS, EndS] = string:tokens(Range, " -/"),
				RangeStart2 = list_to_integer(RangeStartS),
				case EndS of
					"*" ->
						RangeEnd2 = list_to_integer(RangeEndS),
						RangeSize = (RangeEnd2 - (RangeStart2 - 1)),
						{array, Clients2} = mochijson:decode(Body2),
						RangeSize = length(Clients2),
						NewRangeStart = RangeEnd2 + 1,
						NewRangeEnd = NewRangeStart + (RangeSize - 1),
						F(F, NewRangeStart, NewRangeEnd);
					EndS when RangeEndS == EndS ->
						list_to_integer(EndS)
				end
	end,
	CollectionSize = length(ocs:get_clients()),
	CollectionSize = Fget(Fget, PageSize + 1, PageSize + RangeSize).

get_clients_filter() ->
	[{userdata, [{doc,"Get clients with filters"}]}].

get_clients_filter(Config) ->
	{ok, _} = ocs:add_client("10.0.123.100", 3799, radius, "ziggyzaggy", true),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Filters = "?filter=%22%5B%7Bid.like=%5B1%25%5D%7D%5D%22",
	Url = HostUrl ++ "/ocs/v1/client" ++ Filters,
	Request = {Url, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, ClientsList} = mochijson:decode(Body),
	Fall = fun({struct, L}) ->
				lists:keymember("id", 1, L)
						and lists:keymember("href", 1, L)
						and lists:keymember("port", 1, L)
						and lists:keymember("protocol", 1, L)
						and lists:keymember("identifier", 1, L)
						and lists:keymember("secret", 1, L)
	end,
	true = lists:all(Fall, ClientsList).

delete_client() ->
	[{userdata, [{doc,"Delete client in rest interface"}]}].

delete_client(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.9",
	Port = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON1 = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/ocs/v1/client", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	{URI2, _} = httpd_util:split_path(URI1),
	Request2 = {HostUrl ++ URI2, [auth_header()]},
	{ok, Result1} = httpc:request(delete, Request2, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers1, []} = Result1,
	{_, "0"} = lists:keyfind("content-length", 1, Headers1).

add_offer() ->
	[{userdata, [{doc,"Create a new product offering."}]}].

add_offer(Config) ->
	CatalogHref = "/productCatalogManagement/v2",
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	ReqList = product_offer(),
	ReqBody = lists:flatten(mochijson:encode({struct, ReqList})),
	Request1 = {HostUrl ++ CatalogHref ++ "/productOffering",
			[Accept, auth_header()], ContentType, ReqBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, _Href} = lists:keyfind("location", 1, Headers).

get_offer() ->
	[{userdata, [{doc,"Get offer for given Offer Id"}]}].

get_offer(Config) ->
	CatalogHref = "/productCatalogManagement/v2",
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	ReqList = product_offer(),
	ReqBody = lists:flatten(mochijson:encode({struct, ReqList})),
	Request1 = {HostUrl ++ CatalogHref ++ "/productOffering",
			[Accept, auth_header()], ContentType, ReqBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, Href} = lists:keyfind("location", 1, Headers),
	Request2 = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Response} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, RespBody} = Response,
	{_, ContentType} = lists:keyfind("content-type", 1, Headers1),
	{struct, RespList} = mochijson:decode(RespBody),
	true = lists:keymember("id", 1, RespList),
	true = lists:keymember("href", 1, RespList),
	true = (lists:keyfind("name", 1, ReqList) == lists:keyfind("name", 1, RespList)),
	true = (lists:keyfind("version", 1, ReqList) == lists:keyfind("version", 1, RespList)),
	true = (lists:keyfind("isBundle", 1, ReqList) == lists:keyfind("isBundle", 1, RespList)),
	true = (lists:keyfind("lifecycleStatus", 1, ReqList) == lists:keyfind("lifecycleStatus", 1, RespList)),
	case {lists:keyfind("validFor", 1, ReqList), lists:keyfind("validFor", 1, RespList)} of
		{{_, {struct, ValidFor1}}, {_, {struct, ValidFor2}}} ->
			true = (lists:keyfind("startDateTime", 1, ValidFor1) == lists:keyfind("startDateTime", 1, ValidFor2)),
			true = (lists:keyfind("endDateTime", 1, ValidFor1) == lists:keyfind("endDateTime", 1, ValidFor2));
		{false, false} ->
			true
	end,
	{_, {struct, ProdSpec1}} = lists:keyfind("productSpecification", 1, ReqList),
	{_, {struct, ProdSpec2}} = lists:keyfind("productSpecification", 1, RespList),
	true = (lists:keyfind("id", 1, ProdSpec1) == lists:keyfind("id", 1, ProdSpec2)),
	true = (lists:keyfind("href", 1, ProdSpec1) == lists:keyfind("href", 1, ProdSpec2)),
	{_, {array, POP1}} = lists:keyfind("productOfferingPrice", 1, ReqList),
	{_, {array, POP2}} = lists:keyfind("productOfferingPrice", 1, RespList),
	F1 = fun({{struct, L1}, {struct, L2}}) ->
		true = (lists:keyfind("name", 1, L1) == lists:keyfind("name", 1 , L2)),
		true = (lists:keyfind("description", 1, L1) == lists:keyfind("description", 1 , L2)),
		case {lists:keyfind("validFor", 1, L1), lists:keyfind("validFor", 1, L2)} of
			{{_, {struct, V1}}, {_, {struct, V2}}} ->
				true = (lists:keyfind("startDateTime", 1, V1) == lists:keyfind("startDateTime", 1, V2)),
				true = (lists:keyfind("endDateTime", 1, V1) == lists:keyfind("endDateTime", 1, V2));
			{false, false} ->
				true
		end,
		case {lists:keyfind("price", 1, L1), lists:keyfind("price", 1, L2)} of
			{{_, {struct, P1}}, {_, {struct, P2}}} ->
				true = (lists:keyfind("taxIncludedAmount", 1, P1) == lists:keyfind("taxIncludedAmount", 1, P2)),
				true = (lists:keyfind("dutyFreeAmount", 1, P1) == lists:keyfind("dutyFreeAmount", 1, P2)),
				true = (lists:keyfind("taxRate", 1, P1) == lists:keyfind("taxRate", 1, P2)),
				true = (lists:keyfind("currencyCode", 1, P1) == lists:keyfind("currencyCode", 1, P2));
			{false, false} ->
				true
		end,
		Fm = fun(U) ->
				case lists:last(U) of
					$b ->
						list_to_integer(lists:sublist(U, length(U) - 1));
					$k ->
						list_to_integer(lists:sublist(U, length(U) - 1)) * 1000;
					$m ->
						list_to_integer(lists:sublist(U, length(U) - 1)) * 1000000;
					$g ->
						list_to_integer(lists:sublist(U, length(U) - 1)) * 1000000000;
					_ ->
						list_to_integer(U)
				end
		end,
		case {lists:keyfind("unitOfMeasure", 1, L1), lists:keyfind("unitOfMeasure", 1, L2)} of
			{{_, UoM}, {_, UoM}} ->
				true;
			{{_, UoM1}, {_, UoM2}} ->
				true = (Fm(UoM1) == Fm(UoM2));
			{false, false} ->
				true
		end,
		true = (lists:keyfind("recurringChargePeriod", 1, L1) == lists:keyfind("recurringChargePeriod", 1, L2)),
		case {lists:keyfind("productOfferPriceAlteration", 1, L1), lists:keyfind("productOfferPriceAlteration", 1, L2)} of
			{{_, {struct, A1}}, {_, {struct, A2}}} ->
				true = (lists:keyfind("name", 1, A1) == lists:keyfind("name", 1, A2)),
				true = (lists:keyfind("description", 1, A1) == lists:keyfind("description", 1, A2)),
				case {lists:keyfind("validFor", 1, A1), lists:keyfind("validFor", 1, A2)} of
					{{_, {struct, AV1}}, {_, {struct, AV2}}} ->
						true = (lists:keyfind("startDateTime", 1, AV1) == lists:keyfind("startDateTime", 1, AV2)),
						true = (lists:keyfind("endDateTime", 1, AV1) == lists:keyfind("endDateTime", 1, AV2));
					{false, false} ->
						true
				end,
				true = (lists:keyfind("priceType", 1, A1) == lists:keyfind("priceType", 1, A2)),
				case {lists:keyfind("price", 1, A1), lists:keyfind("price", 1, A2)} of
					{{_, {struct, AP1}}, {_, {struct, AP2}}} ->
						true = (lists:keyfind("taxIncludedAmount", 1, AP1) == lists:keyfind("taxIncludedAmount", 1, AP2)),
						true = (lists:keyfind("dutyFreeAmount", 1, AP1) == lists:keyfind("dutyFreeAmount", 1, AP2)),
						true = (lists:keyfind("taxRate", 1, AP1) == lists:keyfind("taxRate", 1, AP2)),
						true = (lists:keyfind("currencyCode", 1, AP1) == lists:keyfind("currencyCode", 1, AP2)),
						true = (lists:keyfind("percentage", 1, AP1) == lists:keyfind("percentage", 1, AP2));
					{false, false} ->
						true
				end,
				case {lists:keyfind("unitOfMeasure", 1, A1), lists:keyfind("unitOfMeasure", 1, A2)} of
					{{_, AUoM}, {_, AUoM}} ->
						true;
					{{_, AUoM1}, {_, AUoM2}} ->
						true = (Fm(AUoM1) == Fm(AUoM2));
					{false, false} ->
						true
				end,
				true = (lists:keyfind("recurringChargePeriod", 1, A1) == lists:keyfind("recurringChargePeriod", 1, A2));
			{false, false} ->
				true
		end
	end,
	true = lists:all(F1, lists:zip(POP1, POP2)).

update_offer() ->
	[{userdata, [{doc,"Use PATCH for update product offering entity"}]}].

update_offer(Config) ->
	CatalogHref = "/productCatalogManagement/v2",
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	RestPort = ?config(port, Config),
	ReqList = product_offer(),
	ReqBody = lists:flatten(mochijson:encode({struct, ReqList})),
	Request1 = {HostUrl ++ CatalogHref ++ "/productOffering",
			[Accept, auth_header()], ContentType, ReqBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, _Href} = lists:keyfind("location", 1, Headers),
	{_, Etag} = lists:keyfind("etag", 1, Headers),
	{struct, Product1} = mochijson:decode(ResponseBody),
	RestPort = ?config(port, Config),
	{_, ProductName} = lists:keyfind("name", 1, Product1),
	SslSock = ssl_socket_open({127,0,0,1}, RestPort),
	PatchContentType = "application/json-patch+json",
	Json = {array, [product_description(), product_status(),
			prod_price_name(), prod_price_description(),
			prod_price_ufm(), prod_price_type(), pp_alter_description(),
			pp_alter_type(), pp_alter_ufm(), prod_price_rc_period()]},
	Body = lists:flatten(mochijson:encode(Json)),
	{Headers2, _Response2} = patch_request(SslSock,
			RestPort, PatchContentType, Etag, basic_auth(), ProductName, Body),
	<<"HTTP/1.1 200", _/binary>> = Headers2,
	ok = ssl_socket_close(SslSock).

delete_offer() ->
	[{userdata, [{doc,"Delete offer for given Offer Id"}]}].

delete_offer(Config) ->
	P1 = price(usage, octets, rand:uniform(1000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	{ok, #offer{}} = ocs:find_offer(OfferId),
	HostUrl = ?config(host_url, Config),
	URI = "/productCatalogManagement/v2/productOffering/" ++ OfferId,
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers, []} = Result,
	{_, "0"} = lists:keyfind("content-length", 1, Headers),
	{error, not_found} = ocs:find_offer(OfferId).

ignore_delete_offer() ->
	[{userdata, [{doc,"Ignore delete offer for given Offer Id,
			if any product related to offer"}]}].

ignore_delete_offer(Config) ->
	P1 = price(usage, octets, rand:uniform(1000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	{ok, #offer{}} = ocs:find_offer(OfferId),
	_ProdRef = product_add(OfferId),
	HostUrl = ?config(host_url, Config),
	URI = "/productCatalogManagement/v2/productOffering/" ++ OfferId,
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 403, _Forbidden}, _Headers, _} = Result,
	{ok, #offer{}} = ocs:find_offer(OfferId).

add_product() ->
	[{userdata, [{doc,"Create a new product inventory."}]}].

add_product(Config) ->
	P1 = price(one_time, undefined, undefined, rand:uniform(100)),
	P2 = price(usage, octets, rand:uniform(1000000), rand:uniform(500)),
	OfferId = offer_add([P1, P2], 4),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	InventoryHref = "/productInventoryManagement/v2",
	ProdOffer = {"productOffering", {struct,[{"id", OfferId}, {"name", OfferId},
			{"href","/productCatalogManagement/v2/productOffering/" ++ OfferId}]}},
	StartDate = {"startDate", ocs_rest:iso8601(erlang:system_time(millisecond))},
	EndDate = {"terminationDate", ocs_rest:iso8601(erlang:system_time(millisecond) + 10000000)},
	Inventory = {struct, [ProdOffer, StartDate, EndDate]},
	ReqBody = lists:flatten(mochijson:encode(Inventory)),
	Request1 = {HostUrl ++ InventoryHref ++ "/product",
			[Accept, auth_header()], ContentType, ReqBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, Href} = lists:keyfind("location", 1, Headers),
	InventoryId = lists:last(string:tokens(Href, "/")),
	{ok, #product{product = OfferId}} = ocs:find_product(InventoryId).

get_product() ->
	[{userdata, [{doc,"Get product inventory
			with given product inventory reference"}]}].

get_product(Config) ->
	Amount1 = 200,
	P1 = price(one_time, undefined, undefined, Amount1),
	P2 = price(usage, octets, rand:uniform(1000000), rand:uniform(500)),
	OfferId = offer_add([P1, P2], 4),
	ProdRef = product_add(OfferId),
	Amount2 = 1000,
	B1 = b(cents, Amount2),
	B2 = #bucket{units = cents, remain_amount = 5000,
			start_date = erlang:system_time(millisecond) - (2 * 2592000000),
			end_date = erlang:system_time(millisecond) - 2592000000},
	{_, _, #bucket{}} = ocs:add_bucket(ProdRef, B1),
	{_, _, #bucket{}} = ocs:add_bucket(ProdRef, B2),
	ServiceId = service_add(ProdRef),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ "/productInventoryManagement/v2/product/" ++ ProdRef,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ProdRef} = lists:keyfind("id", 1, Object),
	{_, "/productInventoryManagement/v2/product/" ++ ProdRef}
			= lists:keyfind("href", 1, Object),
	{_, {struct, ProductOffering}} = lists:keyfind("productOffering", 1, Object),
	{_, OfferId} = lists:keyfind("id", 1, ProductOffering),
	{_, "/productCatalogManagement/v2/productOffering/" ++ OfferId}
			= lists:keyfind("href", 1, ProductOffering),
	{_, {array, RealizeingServices}}
			= lists:keyfind("realizingService", 1, Object),
	{_, {array, [{struct, BalanceList}]}} = lists:keyfind("balance", 1, Object),
	{_, {struct, TotalBalanceList}}
			= lists:keyfind("totalBalance", 1, BalanceList),
	{_, CentsBalance} = lists:keyfind("amount", 1, TotalBalanceList),
	CentsBalance = ocs_rest:millionths_out(Amount2 - Amount1),
	F = fun({struct, [{"id", SId},
					{"href","/serviceInventoryManagement/v2/service/" ++ SId}]})
					when ServiceId == SId ->
				true;
			(_) ->
				false
	end,
	true = lists:all(F, RealizeingServices).

update_product_realizing_service() ->
	[{userdata, [{doc,"Use PATCH for update product inventory realizing services"}]}].

update_product_realizing_service(Config) ->
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	ServiceId = ocs:generate_identity(),
	{ok, #service{}}	= ocs:add_service(ServiceId, ocs:generate_password(), undefined, []),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request2 = {HostUrl ++ "/productInventoryManagement/v2/product/" ++ ProdRef,
			[Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, _} = Result1,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers1),
	{_, Etag} = lists:keyfind("etag", 1, Headers1),
	NewRSObj = {struct, [{"id", ServiceId},
			{"href", "/serviceInventoryManagement/v2/service/"++ ServiceId}]},
	JSON = {array, [{struct, [{op, "add"},
			{path, "/realizingService/-"},
			{value, NewRSObj}]}]},
	Body = lists:flatten(mochijson:encode(JSON)),
	Length= size(list_to_binary(Body)),
	Port = ?config(port, Config),
	SslSock = ssl_socket_open({127,0,0,1}, Port),
	ContentType = "application/json-patch+json",
	Timeout = 1500,
	PatchURI = "/productInventoryManagement/v2/product/" ++ ProdRef,
	Request =
			["PATCH ", PatchURI, " HTTP/1.1",$\r,$\n,
			"Content-Type:"++ ContentType, $\r,$\n,
			"Accept:application/json",$\r,$\n,
			"Authorization:"++ basic_auth(),$\r,$\n,
			"Host:localhost:" ++ integer_to_list(Port),$\r,$\n,
			"Content-Length:" ++ integer_to_list(Length),$\r,$\n,
			"If-match:" ++ Etag,$\r,$\n,
			$\r,$\n,
			Body],
	ok = ssl:send(SslSock, Request),
	F2 = fun F2(_Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			F2(Sock, {ok, Bin}, Acc) ->
					F2(Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F2(SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[Headers, ResponseBody] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	<<"HTTP/1.1 200", _/binary>> = Headers,
	{struct, PatchObj} = mochijson:decode(ResponseBody),
	{_, {array, RealizeingServices}} = lists:keyfind("realizingService", 1, PatchObj),
	F3 = fun({struct, Obj}) ->
			try
				{_, ServiceId} = lists:keyfind("id", 1, Obj),
				{_, "/serviceInventoryManagement/v2/service/" ++ ServiceId} = lists:keyfind("href", 1, Obj),
				true
			catch
				_:_ ->
					false
			end
	end,
	true = lists:all(F3, RealizeingServices),
	ok = ssl_socket_close(SslSock).

delete_product() ->
	[{userdata, [{doc,"Delete product inventory"}]}].

delete_product(Config) ->
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	{_, #product{}} = ocs:find_product(ProdRef),
	URI = "/productInventoryManagement/v2/product/" ++ ProdRef,
	HostUrl = ?config(host_url, Config),
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers, []} = Result,
	{_, "0"} = lists:keyfind("content-length", 1, Headers),
	{error, not_found} = ocs:find_product(ProdRef).

ignore_delete_product() ->
	[{userdata, [{doc,"Ignore delete product inventory if
			any service related with product instance"}]}].

ignore_delete_product(Config) ->
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ServiceId = service_add(undefined),
	{ok, #product{id = ProdRef}} =
			ocs:add_product(OfferId, [list_to_binary(ServiceId)]),
	URI = "/productInventoryManagement/v2/product/" ++ ProdRef,
	HostUrl = ?config(host_url, Config),
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 403, _Forbidden}, _Headers, _} = Result,
	{ok, #product{}} = ocs:find_product(ProdRef).

query_product() ->
	[{userdata, [{doc, "Query product entry in product table"}]}].

query_product(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Price1 = price(one_time, undefined, undefined, rand:uniform(100)),
				Prices = [Price1],
				OfferId = ocs:generate_identity(),
				Offer = #offer{name = OfferId,
						status = active, price = Prices, specification = "PrepaidVoiceProductSpec"},
				{ok, _Offer1} = ocs:add_offer(Offer),
				{ok, P} = ocs:add_product(OfferId, []),
				P1 = P#product{service = [list_to_binary(ocs:generate_identity()) || _ <- lists:seq(1, 5)]},
				mnesia:dirty_write(product, P1),
				F(N -1, [P1 | Acc])
	end,
	Products = F(rand:uniform(100), []),
	#product{id = Id, service = Services, product = Offer} = lists:nth(rand:uniform(length(Products)), Products),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Query = "id=" ++ Id ++ "&productOffering=" ++ Offer ++ 
		"&service=" ++  binary_to_list(lists:nth(rand:uniform(length(Services)), Services)),
	Request = {HostUrl ++ "/productInventoryManagement/v2/product?" ++ Query,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{array, [{struct, Object}]} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, Object),
	{_, "/productInventoryManagement/v2/product/" ++ Id} = lists:keyfind("href", 1, Object),
	{_, {struct, ProdOffer}} = lists:keyfind("productOffering", 1, Object),
	{_, Offer} = lists:keyfind("id", 1, ProdOffer).

filter_product() ->
	[{userdata, [{doc, "Filter product inventory ids"}]}].

filter_product(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Price1 = price(one_time, undefined, undefined, rand:uniform(100)),
				Prices = [Price1],
				OfferId = ocs:generate_identity(),
				Offer = #offer{name = OfferId,
						status = active, price = Prices, specification = "PrepaidVoiceProductSpec"},
				{ok, _Offer1} = ocs:add_offer(Offer),
				{ok, P} = ocs:add_product(OfferId, []),
				F(N -1, [{P#product.id} | Acc])
	end,
	ProdRefs1 = F(1, []),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Filter = "?filter=%22%5B%7Bid.like=%5B1%25%5D%7D%5D%22",
	Url = HostUrl ++ "/productInventoryManagement/v2/product" ++ Filter,
	Request = {Url, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{array, Objects} = mochijson:decode(ResponseBody),
	ProdRefs2 = [Id || {struct, [{"id", Id}]} <- Objects],
	ProdRefs3 = ProdRefs1 -- ProdRefs2.

add_product_sms(Config) ->
	CatalogHref = "/productCatalogManagement/v2",
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	ProdId = ocs:generate_identity(),
	ProdName = {"name", ProdId},
	ProdDescirption = {"description", ocs:generate_password()},
	IsBundle = {"isBundle", false},
	IsCustomerVisible = {"isCustomerVisible", true},
	Status = {"lifecycleStatus", "Active"},
	StartTime = {"startDateTime", ocs_rest:iso8601(erlang:system_time(millisecond))},
	EndTime = {"endDateTime", ocs_rest:iso8601(erlang:system_time(millisecond)  + 2678400000)},
	ValidFor = {"validFor", {struct, [StartTime, EndTime]}},
	ProdSpecID = {"id", "11"},
	ProdSpecHref = {"href", CatalogHref ++ "/productSpecification/11"},
	ProdSpec = {"productSpecification", {struct, [ProdSpecID, ProdSpecHref]}},
	POPName = {"name", "usage"},
	POPDescription = {"description", ocs:generate_password()},
	POPStratDateTime = {"startDateTime", ocs_rest:iso8601(erlang:system_time(millisecond))},
	POPEndDateTime = {"endDateTime", ocs_rest:iso8601(erlang:system_time(millisecond)  + 2678400000)},
	POPValidFor = {"validFor", {struct, [POPStratDateTime, POPEndDateTime]}},
	POPPriceType = {"priceType", "usage"},
	POPUOMeasure = {"unitOfMeasure", "10msg"},
	POPPriceTaxInclude = {"taxIncludedAmount",
			integer_to_list(rand:uniform(1000)) ++ "." ++ integer_to_list(rand:uniform(999999))},
	POPPriceCurrency = {"currencyCode", "USD"},
	POPPrice = {"price", {struct, [POPPriceTaxInclude, POPPriceCurrency]}},
	ProdOfferPrice1 = {struct, [POPName, POPDescription, POPValidFor, POPPriceType,
			POPPrice, POPUOMeasure]},
	ProdOfferPrice = {"productOfferingPrice", {array, [ProdOfferPrice1]}},
	ReqList = [ProdName, ProdDescirption, IsBundle, IsCustomerVisible, ValidFor, ProdSpec, Status, ProdOfferPrice],
	ReqBody = lists:flatten(mochijson:encode({struct, ReqList})),
	Request1 = {HostUrl ++ CatalogHref ++ "/productOffering",
			[Accept, auth_header()], ContentType, ReqBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, _Href} = lists:keyfind("location", 1, Headers).

add_service_inventory() ->
	[{userdata, [{doc,"Add service inventory"}]}].

add_service_inventory(Config) ->
	OfferId = ?config(product_id, Config),
	{ok, #product{}} = ocs:add_product(OfferId, []),
	ID = ocs:generate_identity(),
	Password = ocs:generate_password(),
	State = {"state", active},
	IsServiceEnabled = {"isServiceEnabled", true},
	Char1= {struct, [{"name", "acctSessionInterval"}, {"value", rand:uniform(500)}]},
	Char2 = {struct, [{"name", "sessionTimeout"}, {"value", rand:uniform(2500)}]},
	Char3 = {struct, [{"name", "serviceIdentity"}, {"value", ID}]},
	Char4 = {struct, [{"name", "servicePassword"}, {"value", Password}]},
	Char5 = {struct, [{"name", "multiSession"}, {"value", true}]},
	Char6 = {struct, [{"name", "radiusReserveOctets"},
			{"value", {struct, [{"unitOfMeasure", "bytes"},
			{"value", rand:uniform(100000)}]}}]},
	SortedChars = lists:sort([Char1, Char2, Char3, Char4, Char5, Char6]),
	Characteristics = {"serviceCharacteristic", {array, SortedChars}},
	JSON = {struct, [State, IsServiceEnabled, Characteristics]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service",
			[Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _} = lists:keyfind("etag", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/serviceInventoryManagement/v2/service/" ++ ID, _} = httpd_util:split_path(URI),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Object),
	SortedChars = lists:sort(Chars).

add_service_inventory_without_password() ->
	[{userdata, [{doc,"Add service inventory with out servicePassword characteristic"}]}].

add_service_inventory_without_password(Config) ->
	OfferId = ?config(product_id, Config),
	{ok, #product{}} = ocs:add_product(OfferId, []),
	ID = ocs:generate_identity(),
	State = {"state", active},
	IsServiceEnabled = {"isServiceEnabled", true},
	Char1= {struct, [{"name", "acctSessionInterval"}, {"value", rand:uniform(500)}]},
	Char2 = {struct, [{"name", "sessionTimeout"}, {"value", rand:uniform(2500)}]},
	Char3 = {struct, [{"name", "serviceIdentity"}, {"value", ID}]},
	Char4 = {struct, [{"name", "multiSession"}, {"value", true}]},
	SortedChars = lists:sort([Char1, Char2, Char3, Char4]),
	Characteristics = {"serviceCharacteristic", {array, SortedChars}},
	JSON = {struct, [State, IsServiceEnabled, Characteristics]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service",
			[Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _} = lists:keyfind("etag", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/serviceInventoryManagement/v2/service/" ++ ID, _} = httpd_util:split_path(URI),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Object),
	F = fun({struct, [{"name", "servicePassword"}, {"value", Password}]}) ->
					12 == length(Password);
			({struct, [{"value", Password}, {"name", "servicePassword"}]}) ->
					12 == length(Password);
			(_) ->
				false
	end,
	lists:any(F, Chars).

add_service_aka() ->
	[{userdata, [{doc,"Add service with IMSI and AKA"}]}].

add_service_aka(Config) ->
	IMSI = "001001" ++ ocs:generate_identity(),
	K = binary_to_hex(crypto:strong_rand_bytes(16)),
	OPc = binary_to_hex(crypto:strong_rand_bytes(16)),
	Credentials = #aka_cred{k = K, opc = OPc},
	State = {"state", active},
	IsServiceEnabled = {"isServiceEnabled", true},
	Char1 = {struct, [{"name", "serviceIdentity"}, {"value", IMSI}]},
	Char2 = {struct, [{"name", "serviceAkaK"}, {"value", K}]},
	Char3 = {struct, [{"name", "serviceAkaOpc"}, {"value", OPc}]},
	Char4 = {struct, [{"name", "multiSession"}, {"value", true}]},
	SortedChars = lists:sort([Char1, Char2, Char3, Char4]),
	Characteristics = {"serviceCharacteristic", {array, SortedChars}},
	JSON = {struct, [State, IsServiceEnabled, Characteristics]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service",
			[Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _} = lists:keyfind("etag", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/serviceInventoryManagement/v2/service/" ++ IMSI, _} = httpd_util:split_path(URI),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", IMSI} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Object),
	SortedChars1 = lists:sort(Chars).

get_service_inventory() ->
	[{userdata, [{doc,"get service invetory for spefici service id"}]}].

get_service_inventory(Config) ->
	OfferId = ?config(product_id, Config),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, []),
	ID = ocs:generate_identity(),
	Password = ocs:generate_password(),
	State = active,
	SessionTimeout = rand:uniform(2500),
	AcctInterimInterval = rand:uniform(500),
	Attributes = [{?SessionTimeout, SessionTimeout},
			{?AcctInterimInterval, AcctInterimInterval}],
	{ok, #service{}} = ocs:add_service(ID, Password, State, ProdRef, [], Attributes, true, false),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service/" ++ ID,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _} = lists:keyfind("etag", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/serviceInventoryManagement/v2/service/" ++ ID, _} = httpd_util:split_path(URI),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{"state", _} = lists:keyfind("state", 1, Object),
	{"isServiceEnabled", true} = lists:keyfind("isServiceEnabled", 1, Object),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Object),
	F = fun({struct, [{"name", "serviceIdentity"}, {"value", ID1}]}) when ID1 == ID ->
				true;
			({struct, [{"name", "servicePassword"}, {"value", Password1}]}) when Password1 == Password ->
				true;
			({struct, [{"name", "multiSession"}, {"value", false}]}) ->
				true;
			({struct, [{"name", "acctSessionInterval"}, {"value", AcctInterimInterval1}]})
					when AcctInterimInterval1 == AcctInterimInterval ->
				true;
			({struct, [{"name", "sessionTimeout"}, {"value", SessionTimeout1}]})
					when SessionTimeout1 == SessionTimeout ->
				true;
			({struct, [{"value", ID1}, {"name", "serviceIdentity"}]}) when ID1 == ID ->
				true;
			({struct, [{"value", Password1}, {"name", "servicePassword"}]}) when Password1 == Password ->
				true;
			({struct, [{"value", false}, {"name", "multiSession"}]}) ->
				true;
			({struct, [{"value", AcctInterimInterval1}, {"name", "acctSessionInterval"}]})
					when AcctInterimInterval1 == AcctInterimInterval ->
				true;
			({struct, [{"value", SessionTimeout1}, {"name", "sessionTimeout"}]})
					when SessionTimeout1 == SessionTimeout ->
				true;
			(_) ->
				false
	end,
	true = lists:all(F, Chars).

get_service_not_found() ->
	[{userdata, [{doc, "Get service notfound for given service id"}]}].

get_service_not_found(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ID = ocs:generate_identity(),
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service/" ++ ID, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 404, _NotFound}, _Headers, _Body} = Result.

get_all_service_inventories() ->
	[{userdata, [{doc,"Get all service inventories"}]}].

get_all_service_inventories(Config) ->
	OfferId = ?config(product_id, Config),
	{ok, #product{id = ProdRef1}} = ocs:add_product(OfferId, []),
	{ok, #product{id = ProdRef2}} = ocs:add_product(OfferId, []),
	{ok, #product{id = ProdRef3}} = ocs:add_product(OfferId, []),
	{ok, #product{id = ProdRef4}} = ocs:add_product(OfferId, []),
	{ok, #product{id = ProdRef5}} = ocs:add_product(OfferId, []),
	ID1 = ocs:generate_identity(),
	ID2 = ocs:generate_identity(),
	ID3 = ocs:generate_identity(),
	ID4 = ocs:generate_identity(),
	ID5 = ocs:generate_identity(),
	Password1 = ocs:generate_password(),
	Password2 = ocs:generate_password(),
	Password3 = ocs:generate_password(),
	Password4 = ocs:generate_password(),
	Password5 = ocs:generate_password(),
	SessionTimeout1 = rand:uniform(2500),
	SessionTimeout2 = rand:uniform(2500),
	SessionTimeout3 = rand:uniform(2500),
	SessionTimeout4 = rand:uniform(2500),
	SessionTimeout5 = rand:uniform(2500),
	AcctInterimInterval1 = rand:uniform(500),
	AcctInterimInterval2 = rand:uniform(500),
	AcctInterimInterval3 = rand:uniform(500),
	AcctInterimInterval4 = rand:uniform(500),
	AcctInterimInterval5 = rand:uniform(500),
	Attributes1 = [{?SessionTimeout, SessionTimeout1}, {?AcctInterimInterval, AcctInterimInterval1}],
	Attributes2 = [{?SessionTimeout, SessionTimeout2}, {?AcctInterimInterval, AcctInterimInterval2}],
	Attributes3 = [{?SessionTimeout, SessionTimeout3}, {?AcctInterimInterval, AcctInterimInterval3}],
	Attributes4 = [{?SessionTimeout, SessionTimeout4}, {?AcctInterimInterval, AcctInterimInterval4}],
	Attributes5 = [{?SessionTimeout, SessionTimeout5}, {?AcctInterimInterval, AcctInterimInterval5}],
	{ok, #service{}} = ocs:add_service(ID1, Password1, active, ProdRef1, [], Attributes1, true, false),
	{ok, #service{}} = ocs:add_service(ID2, Password2, active, ProdRef2, [], Attributes2, true, false),
	{ok, #service{}} = ocs:add_service(ID3, Password3, active, ProdRef3, [], Attributes3, true, false),
	{ok, #service{}} = ocs:add_service(ID4, Password4, active, ProdRef4, [], Attributes4, true, false),
	{ok, #service{}} = ocs:add_service(ID5, Password5, active, ProdRef5, [], Attributes5, true, false),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service/", [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _} = lists:keyfind("etag", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, Objects} = mochijson:decode(ResponseBody),
	F2 = fun(Obj, ID, Password, Attributes) ->
					{"state", _} = lists:keyfind("state", 1, Obj),
					{"isServiceEnabled", true} = lists:keyfind("isServiceEnabled", 1, Obj),
					{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Obj),
						F3 = fun({struct, [{"name", "serviceIdentity"}, {"value", ID6}]}) when ID6 == ID ->
									true;
								({struct, [{"name", "servicePassword"}, {"value", Password6}]}) when Password6 == Password ->
									true;
								({struct, [{"name", "multiSession"}, {"value", false}]}) ->
									true;
								({struct, [{"name", "acctSessionInterval"}, {"value", AcctInterimInterval6}]}) ->
										case lists:keyfind(?AcctInterimInterval, 1, Attributes) of
											{_, AcctInterimInterval6} ->
												true;
											_ ->
												false
										end;
								({struct, [{"name", "sessionTimeout"}, {"value", SessionTimeout6}]}) ->
										case lists:keyfind(?SessionTimeout, 1, Attributes) of
											{_, SessionTimeout6} ->
												true;
											_ ->
												false
										end;
								({struct, [{"value", ID6}, {"name", "serviceIdentity"}]}) when ID6 == ID ->
									true;
								({struct, [{"value", Password6}, {"name", "servicePassword"}]}) when Password6 == Password ->
									true;
								({struct, [{"value", false}, {"name", "multiSession"}]}) ->
									true;
								({struct, [{"value", AcctInterimInterval6}, {"name", "acctSessionInterval"}]}) ->
										case lists:keyfind(?AcctInterimInterval, 1, Attributes) of
											{_, AcctInterimInterval6} ->
												true;
											_ ->
												false
										end;
								({struct, [{"value", SessionTimeout6}, {"name", "sessionTimeout"}]}) ->
										case lists:keyfind(?SessionTimeout, 1, Attributes) of
											{_, SessionTimeout6} ->
												true;
											_ ->
												false
										end;
								(_) ->
									false
						end,
						true = lists:all(F3, Chars)
	end,
	F = fun({struct, Object}) ->
				case lists:keyfind("id", 1, Object) of
					{_, ID6} when ID6 == ID1 ->
						F2(Object, ID1, Password1, Attributes1);
					{_, ID6} when ID6 == ID2 ->
						F2(Object, ID2, Password2, Attributes2);
					{_, ID6} when ID6 == ID3 ->
						F2(Object, ID3, Password3, Attributes3);
					{_, ID6} when ID6 == ID4 ->
						F2(Object, ID4, Password4, Attributes4);
					{_, ID6} when ID6 == ID5 ->
						F2(Object, ID5, Password5, Attributes5);
					_ ->
						true
				end
	end,
	true = lists:all(F, Objects).

get_service_range() ->
	[{userdata, [{doc,"Get range of items in the service collection"}]}].

get_service_range(Config) ->
	{ok, PageSize} = application:get_env(ocs, rest_page_size),
	P1 = price(usage, octets, rand:uniform(1000000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	Fadd = fun(_F, 0) ->
				ok;
			(F, N) ->
				Identity = ocs:generate_identity(),
				Password = ocs:generate_password(),
				{ok, _} = ocs:add_service(Identity, Password, ProdRef, []),
				F(F, N - 1)
	end,
	NumAdded = (PageSize * 2) + (PageSize div 2) + 17,
	ok = Fadd(Fadd, NumAdded),
	RangeSize = case PageSize > 25 of
		true ->
			rand:uniform(PageSize - 10) + 10;
		false ->
			PageSize - 1
	end,
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RequestHeaders1 = [Accept, auth_header()],
	Request1 = {HostUrl ++ "/serviceInventoryManagement/v2/service/", RequestHeaders1},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, ResponseHeaders1, Body1} = Result1,
	{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders1),
	true = is_etag_valid(Etag),
	{_, AcceptRanges1} = lists:keyfind("accept-ranges", 1, ResponseHeaders1),
	true = lists:member("items", string:tokens(AcceptRanges1, ", ")),
	{_, Range1} = lists:keyfind("content-range", 1, ResponseHeaders1),
	["items", "1", RangeEndS1, "*"] = string:tokens(Range1, " -/"),
	RequestHeaders2 = RequestHeaders1 ++ [{"if-match", Etag}],
	PageSize = list_to_integer(RangeEndS1),
	{array, Service1} = mochijson:decode(Body1),
	PageSize = length(Service1),
	Fget = fun Fget(RangeStart2, RangeEnd2) ->
				RangeHeader = [{"range",
						"items " ++ integer_to_list(RangeStart2)
						++ "-" ++ integer_to_list(RangeEnd2)}],
				RequestHeaders3 = RequestHeaders2 ++ RangeHeader,
				Request2 = {HostUrl ++ "/serviceInventoryManagement/v2/service/", RequestHeaders3},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, ResponseHeaders2, Body2} = Result2,
				{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders2),
				{_, AcceptRanges2} = lists:keyfind("accept-ranges", 1, ResponseHeaders2),
				true = lists:member("items", string:tokens(AcceptRanges2, ", ")),
				{_, Range} = lists:keyfind("content-range", 1, ResponseHeaders2),
				["items", RangeStartS, RangeEndS, EndS] = string:tokens(Range, " -/"),
				RangeStart2 = list_to_integer(RangeStartS),
				case EndS of
					"*" ->
						RangeEnd2 = list_to_integer(RangeEndS),
						RangeSize = (RangeEnd2 - (RangeStart2 - 1)),
						{array, Service2} = mochijson:decode(Body2),
						RangeSize = length(Service2),
						NewRangeStart = RangeEnd2 + 1,
						NewRangeEnd = NewRangeStart + (RangeSize - 1),
						Fget(NewRangeStart, NewRangeEnd);
					EndS when RangeEndS == EndS ->
						list_to_integer(EndS)
				end
	end,
	CollectionSize = length(ocs:get_services()),
	CollectionSize = Fget(PageSize + 1, PageSize + RangeSize).

delete_service() ->
	[{userdata, [{doc,"Delete subscriber in rest interface"}]}].

delete_service(Config) ->
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	ServiceId = service_add(ProdRef),
	{ok, #service{}} = ocs:find_service(ServiceId),
	URI = "/serviceInventoryManagement/v2/service/" ++ ServiceId,
	HostUrl = ?config(host_url, Config),
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers, []} = Result,
	{_, "0"} = lists:keyfind("content-length", 1, Headers).

update_service() ->
	[{userdata, [{doc,"Use HTTP PATCH to update service characteristics
			using json-patch media type"}]}].

update_service(Config) ->
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	ServiceId = service_add(ProdRef),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request2 = {HostUrl ++ "/serviceInventoryManagement/v2/service/" ++ ServiceId,
			[Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers1),
	{_, Etag} = lists:keyfind("etag", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{_, {array, Characteristic}} = lists:keyfind("serviceCharacteristic", 1, Object),
	NewPassword = ocs:generate_password(),
	NewPwdObj = {struct, [{"name", "servicePassword"}, {"value", NewPassword}]},
	F1 = fun F1([{struct, [{"name", Name}, _]} | _T], Name, N) ->
				integer_to_list(N);
			F1([{struct, [_, {"name", Name}]} | _T], Name, N) ->
				integer_to_list(N);
			F1([_ | T], Name, N) ->
				F1(T, Name, N + 1);
			F1([], _Name, _N) ->
				"-"
	end,
	IndexPassword= F1(Characteristic, "servicePassword", 0),
	JSON = {array, [{struct, [{op, "replace"},
			{path, "/serviceCharacteristic/" ++ IndexPassword},
			{value, NewPwdObj}]}]},
	Body = lists:flatten(mochijson:encode(JSON)),
	Length= size(list_to_binary(Body)),
	Port = ?config(port, Config),
	SslSock = ssl_socket_open({127,0,0,1}, Port),
	ContentType = "application/json-patch+json",
	Timeout = 1500,
	PatchURI = "/serviceInventoryManagement/v2/service/" ++ ServiceId,
	Request =
			["PATCH ", PatchURI, " HTTP/1.1",$\r,$\n,
			"Content-Type:"++ ContentType, $\r,$\n,
			"Accept:application/json",$\r,$\n,
			"Authorization:"++ basic_auth(),$\r,$\n,
			"Host:localhost:" ++ integer_to_list(Port),$\r,$\n,
			"Content-Length:" ++ integer_to_list(Length),$\r,$\n,
			"If-match:" ++ Etag,$\r,$\n,
			$\r,$\n,
			Body],
	ok = ssl:send(SslSock, Request),
	F2 = fun F2(_Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			F2(Sock, {ok, Bin}, Acc) ->
					F2(Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F2(SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[Headers, ResponseBody] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	{struct, PatchObj} = mochijson:decode(ResponseBody),
	{_, {array, PatchChars}} = lists:keyfind("serviceCharacteristic", 1, PatchObj),
	F3 = fun({struct, [{"name","serviceIdentity"},{"value", ServiceId1}]})
						when ServiceId1 == ServiceId ->
					true;
			({struct,[{"name","servicePassword"},{"value", Password1}]})
						when Password1 == NewPassword ->
					true;
			({struct,[{"name","multiSession"},{"value", false}]}) ->
				true;
			(_) ->
				false
	end,
	true = lists:all(F3, PatchChars),
	<<"HTTP/1.1 200", _/binary>> = Headers,
	ok = ssl_socket_close(SslSock).

get_usagespecs() ->
	[{userdata, [{doc,"Get usageSpecification collection"}]}].

get_usagespecs(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	Request = {HostUrl ++ "/usageManagement/v1/usageSpecification", [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, [{struct, UsageSpec} | _]} = mochijson:decode(Body),
	{_, _} = lists:keyfind("id", 1, UsageSpec),
	{_, _} = lists:keyfind("href", 1, UsageSpec),
	{_, _} = lists:keyfind("name", 1, UsageSpec),
	{_, _} = lists:keyfind("validFor", 1, UsageSpec),
	{_, _} = lists:keyfind("usageSpecCharacteristic", 1, UsageSpec).

get_usagespecs_query() ->
	[{userdata, [{doc,"Get usageSpecification collection with query"}]}].

get_usagespecs_query(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	Path = HostUrl ++ "/usageManagement/v1/usageSpecification",
	Request1 = {Path, [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, UsageSpecs} = mochijson:decode(Body1),
	F1 = fun({struct, UsageSpec1}) ->
				{_, Type1} = lists:keyfind("name", 1, UsageSpec1),
				Type1
	end,
	Types = lists:map(F1, UsageSpecs),
	F2 = fun(Type2) ->
				Request2 = {Path ++ "?name=" ++ Type2, [Accept, auth_header()]},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, Headers2, Body2} = Result2,
				{_, AcceptValue} = lists:keyfind("content-type", 1, Headers2),
				ContentLength2 = integer_to_list(length(Body2)),
				{_, ContentLength2} = lists:keyfind("content-length", 1, Headers2),
				{array, [{struct, UsageSpec2}]} = mochijson:decode(Body2),
				{_, _} = lists:keyfind("id", 1, UsageSpec2),
				{_, _} = lists:keyfind("href", 1, UsageSpec2),
				{_, Type2} = lists:keyfind("name", 1, UsageSpec2),
				{_, _} = lists:keyfind("validFor", 1, UsageSpec2),
				{_, _} = lists:keyfind("usageSpecCharacteristic", 1, UsageSpec2)
	end,
	lists:foreach(F2, Types).

get_usagespec() ->
	[{userdata, [{doc,"Get a TMF635 usage specification"}]}].

get_usagespec(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	Request1 = {HostUrl ++ "/usageManagement/v1/usageSpecification", [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, UsageSpecs} = mochijson:decode(Body1),
	F1 = fun({struct, UsageSpec1}) ->
				{_, Id} = lists:keyfind("id", 1, UsageSpec1),
				Href = "/usageManagement/v1/usageSpecification/" ++ Id,
				{_, Href} = lists:keyfind("href", 1, UsageSpec1),
				Href
	end,
	Uris = lists:map(F1, UsageSpecs),
	F2 = fun(Uri) ->
				Request2 = {HostUrl ++ Uri, [Accept, auth_header()]},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, Headers2, Body2} = Result2,
				{_, AcceptValue} = lists:keyfind("content-type", 1, Headers2),
				ContentLength2 = integer_to_list(length(Body2)),
				{_, ContentLength2} = lists:keyfind("content-length", 1, Headers2),
				{struct, UsageSpec2} = mochijson:decode(Body2),
				{_, _} = lists:keyfind("id", 1, UsageSpec2),
				{_, _} = lists:keyfind("href", 1, UsageSpec2),
				{_, _} = lists:keyfind("name", 1, UsageSpec2),
				{_, _} = lists:keyfind("validFor", 1, UsageSpec2),
				{_, _} = lists:keyfind("usageSpecCharacteristic", 1, UsageSpec2)
	end,
	lists:foreach(F2, Uris).

get_auth_usage() ->
	[{userdata, [{doc,"Get a TMF635 auth usage"}]}].

get_auth_usage(Config) ->
	ClientAddress = {192, 168, 159, 158},
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "DE:AD:BE:EF:CA:FE"}, {?AcctSessionId, "8250020b"},
			{?CallingStationId, "FE-ED-BE-EF-FE-FE"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?NasIdentifier, "ap-1.sigscale.net"},
			{?NasIpAddress, ClientAddress}, {?NasPort, 21}],
	ResAttrs = [{?SessionTimeout, 3600}, {?IdleTimeout, 300},
			{?AcctInterimInterval, 300},
			{?AscendDataRate, 4000000}, {?AscendXmitRate, 64000},
			{?ServiceType, 2}, {?FramedIpAddress, {10,2,56,78}},
			{?FramedIpNetmask, {255,255,0,0}}, {?FramedPool, "nat"},
			{?FramedRouting, 2}, {?FilterId, "firewall-1"},
			{?FramedMtu, 1492}, {?FramedRoute, "192.168.100.0/24 10.2.1.1 1"},
			{?Class, "silver"}, {?TerminationAction, 1}, {?PortLimit, 1}],
	ok = ocs_log:auth_log(radius, {{0,0,0,0}, 1812},
			{ClientAddress, 4598}, accept, ReqAttrs, ResAttrs),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage&sort=-date",
	Request = {RequestUri, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, Usages} = mochijson:decode(Body),
	{struct, Usage} = lists:last(Usages),
	{_, _} = lists:keyfind("id", 1, Usage),
	{_, _} = lists:keyfind("href", 1, Usage),
	{_, _} = lists:keyfind("date", 1, Usage),
	{_, "AAAAccessUsage"} = lists:keyfind("type", 1, Usage),
	{_, "received"} = lists:keyfind("status", 1, Usage),
	{_, {struct, UsageSpecification}} = lists:keyfind("usageSpecification", 1, Usage),
	{_, _} = lists:keyfind("id", 1, UsageSpecification),
	{_, _} = lists:keyfind("href", 1, UsageSpecification),
	{_, "AAAAccessUsageSpec"} = lists:keyfind("name", 1, UsageSpecification),
	{_, {array, UsageCharacteristic}} = lists:keyfind("usageCharacteristic", 1, Usage),
	F = fun({struct, [{"name", "protocol"}, {"value", Protocol}]})
					when Protocol == "RADIUS"; Protocol == "DIAMETER" ->
				true;
			({struct, [{"name", "node"}, {"value", Node}]}) when is_list(Node) ->
				true;
			({struct, [{"name", "serverAddress"}, {"value", Address}]}) when is_list(Address) ->
				true;
			({struct, [{"name", "serverPort"}, {"value", Port}]}) when is_integer(Port) ->
				true;
			({struct, [{"name", "clientAddress"}, {"value", Address}]}) when is_list(Address) ->
				true;
			({struct, [{"name", "clientPort"}, {"value", Port}]}) when is_integer(Port) ->
				true;
			({struct, [{"name", "type"}, {"value", Type}]})
					when Type == "accept"; Type == "reject"; Type == "change" ->
				true;
			({struct, [{"name", "username"}, {"value", Username}]}) when is_list(Username) ->
				true;
			({struct, [{"name", "nasIpAddress"}, {"value", NasIpAddress}]}) when is_list(NasIpAddress) ->
				true;
			({struct, [{"name", "nasPort"}, {"value", Port}]}) when is_integer(Port) ->
				true;
			({struct, [{"name", "serviceType"}, {"value", Type}]}) when is_list(Type) ->
				true;
			({struct, [{"name", "framedIpAddress"}, {"value", Address}]}) when is_list(Address) ->
				true;
			({struct, [{"name", "framedPool"}, {"value", Pool}]}) when is_list(Pool) ->
				true;
			({struct, [{"name", "framedIpNetmask"}, {"value", Netmask}]}) when is_list(Netmask) ->
				true;
			({struct, [{"name", "framedRouting"}, {"value", Routing}]}) when is_list(Routing) ->
				true;
			({struct, [{"name", "filterId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "framedMtu"}, {"value", Mtu}]}) when is_integer(Mtu) ->
				true;
			({struct, [{"name", "framedRoute"}, {"value", Route}]}) when is_list(Route) ->
				true;
			({struct, [{"name", "class"}, {"value", Class}]}) when is_list(Class) ->
				true;
			({struct, [{"name", "sessionTimeout"}, {"value", Timeout}]}) when is_integer(Timeout) ->
				true;
			({struct, [{"name", "idleTimeout"}, {"value", Timeout}]}) when is_integer(Timeout) ->
				true;
			({struct, [{"name", "terminationAction"}, {"value", Action}]}) when is_list(Action) ->
				true;
			({struct, [{"name", "calledStationId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "callingStationId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "nasIdentifier"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "nasPortId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "nasPortType"}, {"value", Type}]}) when is_list(Type) ->
				true;
			({struct, [{"name", "portLimit"}, {"value", Limit}]}) when is_integer(Limit) ->
				true;
			({struct, [{"name", "ascendDataRate"}, {"value", Rate}]}) when is_integer(Rate) ->
				true;
			({struct, [{"name", "ascendXmitRate"}, {"value", Rate}]}) when is_integer(Rate) ->
				true;
			({struct, [{"name", "acctInterimInterval"}, {"value", Interval}]}) when is_integer(Interval) ->
				true
	end,
	true = lists:any(F, UsageCharacteristic).

get_auth_usage_id() ->
	[{userdata, [{doc,"Get a single TMF635 auth usage"}]}].

get_auth_usage_id(Config) ->
	ReqAttrs = [{?UserName, "ED:DA:EB:FE:AC:EF"},
			{?CallingStationId, "ED:DA:EB:FE:AC:EF"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?NasIdentifier, "ap-1.sigscale.net"}],
	ResAttrs = [{?SessionTimeout, 3600}],
	ok = ocs_log:auth_log(radius, {{0,0,0,0}, 1812},
			{{192,168,178,167}, 4599}, accept, ReqAttrs, ResAttrs),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri1 = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage",
	Request1 = {RequestUri1, [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, Usages} = mochijson:decode(Body1),
	{struct, Usage} = lists:last(Usages),
	{_, Id} = lists:keyfind("id", 1, Usage),
	{_, Href} = lists:keyfind("href", 1, Usage),
	RequestUri2 = HostUrl ++ Href,
	Request2 = {RequestUri2, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, Body2} = Result2,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(Body2)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, Usage} = mochijson:decode(Body2),
	{_, Id} = lists:keyfind("id", 1, Usage),
	{_, Href} = lists:keyfind("href", 1, Usage).

get_auth_usage_filter() ->
	[{userdata, [{doc,"Get filtered TMF635 auth usage"}]}].

get_auth_usage_filter(Config) ->
	ClientAddress = {192, 168, 199, 198},
	ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?UserName, "DE:AD:BE:EF:CA:FE"}, {?AcctSessionId, "82510ed5"},
			{?CallingStationId, "FE-EA-EE-EF-FA-FA"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?NasIdentifier, "ap-1.sigscale.net"},
			{?NasIpAddress, ClientAddress}, {?NasPort, 1}],
	ResAttrs = [{?SessionTimeout, 3600}, {?IdleTimeout, 300},
			{?AcctInterimInterval, 300},
			{?AscendDataRate, 4000000}, {?AscendXmitRate, 64000},
			{?ServiceType, 2}, {?FramedIpAddress, {10,2,74,45}},
			{?FramedIpNetmask, {255,255,0,0}}, {?FramedPool, "nat"},
			{?FramedRouting, 2}, {?FilterId, "firewall-1"},
			{?FramedMtu, 1492}, {?FramedRoute, "192.168.100.0/24 10.2.1.1 1"},
			{?Class, "silver"}, {?TerminationAction, 1}, {?PortLimit, 1}],
	ok = ocs_log:auth_log(radius, {{0,0,0,0}, 1812},
			{ClientAddress, 4589}, accept, ReqAttrs, ResAttrs),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage&sort=-date&fields=date,status,usageCharacteristic",
	Request = {RequestUri, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, Usages} = mochijson:decode(Body),
	{struct, Usage} = lists:last(Usages),
	{_, _, Usage1} = lists:keytake("id", 1, Usage),
	{_, _, Usage2} = lists:keytake("href", 1, Usage1),
	{_, _, Usage3} = lists:keytake("date", 1, Usage2),
	{_, _, Usage4} = lists:keytake("status", 1, Usage3),
	{_, {_, {array, _UsageCharacteristic}}, []} = lists:keytake("usageCharacteristic", 1, Usage4).

get_auth_usage_range() ->
	[{userdata, [{doc,"Get range of items in the usage collection"}]}].

get_auth_usage_range(Config) ->
	{ok, PageSize} = application:get_env(ocs, rest_page_size),
	Flog = fun(_F, 0) ->
				ok;
			(F, N) ->
				ClientAddress = ocs_test_lib:ipv4(),
				ClientPort = ocs_test_lib:port(),
				ReqAttrs = [{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
						{?UserName, ocs:generate_identity()},
						{?CallingStationId, ocs_test_lib:mac()},
						{?CalledStationId, ocs_test_lib:mac()},
						{?NasIpAddress, ClientAddress}, {?NasPort, ClientPort}],
				ResAttrs = [{?SessionTimeout, 3600}, {?IdleTimeout, 300}],
				ok = ocs_log:auth_log(radius, {{0,0,0,0}, 1812},
						{ClientAddress, ClientPort}, accept, ReqAttrs, ResAttrs),
				F(F, N - 1)
	end,
	NumLogged = (PageSize * 2) + (PageSize div 2) + 17,
	ok = Flog(Flog, NumLogged),
	RangeSize = case PageSize > 100 of
		true ->
			rand:uniform(PageSize - 10) + 10;
		false ->
			PageSize - 1
	end,
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RequestHeaders1 = [Accept, auth_header()],
	Request1 = {HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage", RequestHeaders1},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, ResponseHeaders1, Body1} = Result1,
	{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders1),
	true = is_etag_valid(Etag),
	{_, AcceptRanges1} = lists:keyfind("accept-ranges", 1, ResponseHeaders1),
	true = lists:member("items", string:tokens(AcceptRanges1, ", ")),
	{_, Range1} = lists:keyfind("content-range", 1, ResponseHeaders1),
	["items", "1", RangeEndS1, "*"] = string:tokens(Range1, " -/"),
	RequestHeaders2 = RequestHeaders1 ++ [{"if-match", Etag}],
	PageSize = list_to_integer(RangeEndS1),
	{array, Usages1} = mochijson:decode(Body1),
	PageSize = length(Usages1),
	Fget = fun(F, RangeStart2, RangeEnd2) ->
				RangeHeader = [{"range",
						"items " ++ integer_to_list(RangeStart2)
						++ "-" ++ integer_to_list(RangeEnd2)}],
				RequestHeaders3 = RequestHeaders2 ++ RangeHeader,
				Request2 = {HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage", RequestHeaders3},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, ResponseHeaders2, Body2} = Result2,
				{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders2),
				{_, AcceptRanges2} = lists:keyfind("accept-ranges", 1, ResponseHeaders2),
				true = lists:member("items", string:tokens(AcceptRanges2, ", ")),
				{_, Range} = lists:keyfind("content-range", 1, ResponseHeaders2),
				["items", RangeStartS, RangeEndS, EndS] = string:tokens(Range, " -/"),
				RangeStart2 = list_to_integer(RangeStartS),
				case EndS of
					"*" ->
						RangeEnd2 = list_to_integer(RangeEndS),
						RangeSize = (RangeEnd2 - (RangeStart2 - 1)),
						{array, Usages2} = mochijson:decode(Body2),
						RangeSize = length(Usages2),
						NewRangeStart = RangeEnd2 + 1,
						NewRangeEnd = NewRangeStart + (RangeSize - 1),
						F(F, NewRangeStart, NewRangeEnd);
					EndS when RangeEndS == EndS ->
						list_to_integer(EndS)
				end
	end,
	End = Fget(Fget, PageSize + 1, PageSize + RangeSize),
	End >= NumLogged.

get_acct_usage() ->
	[{userdata, [{doc,"Get a TMF635 acct usage"}]}].

get_acct_usage(Config) ->
	ClientAddress = {192, 168, 159, 158},
	Attrs = [{?UserName, "DE:AD:BE:EF:CA:FE"}, {?AcctSessionId, "825df837"},
			{?ServiceType, 2}, {?NasPortId, "wlan1"}, {?NasPortType, 19},
			{?CallingStationId, "FE-ED-BE-EF-FE-FE"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?NasIdentifier, "ap-1.sigscale.net"},
			{?NasIpAddress, ClientAddress}, {?NasPort, 21},
			{?SessionTimeout, 3600}, {?IdleTimeout, 300},
			{?FramedIpAddress, {10,2,56,78}},
			{?FramedIpNetmask, {255,255,0,0}}, {?FramedPool, "nat"},
			{?FramedRouting, 2}, {?FilterId, "firewall-1"},
			{?FramedMtu, 1492}, {?FramedRoute, "192.168.100.0/24 10.2.1.1 1"},
			{?Class, "silver"}, {?PortLimit, 1},
			{?AcctDelayTime, 5}, {?EventTimestamp, erlang:system_time(second)},
			{?AcctMultiSessionId, "8250731f"}, {?AcctLinkCount, 2},
			{?AcctAuthentic, 1}, {?AcctSessionTime, 3021},
			{?AcctInputOctets, 1702487}, {?AcctOutputOctets, 301629083},
			{?AcctInputGigawords, 1}, {?AcctOutputGigawords, 2},
			{?AcctInputPackets, 3021}, {?AcctOutputPackets, 125026},
			{?AcctTerminateCause, 5}],
	ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1813}, stop, Attrs, undefined, undefined),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage&sort=-date",
	Request = {RequestUri, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, Usages} = mochijson:decode(Body),
	{struct, Usage} = lists:last(Usages),
	{_, _} = lists:keyfind("id", 1, Usage),
	{_, _} = lists:keyfind("href", 1, Usage),
	{_, _} = lists:keyfind("date", 1, Usage),
	{_, "AAAAccountingUsage"} = lists:keyfind("type", 1, Usage),
	{_, "received"} = lists:keyfind("status", 1, Usage),
	{_, {struct, UsageSpecification}} = lists:keyfind("usageSpecification", 1, Usage),
	{_, _} = lists:keyfind("id", 1, UsageSpecification),
	{_, _} = lists:keyfind("href", 1, UsageSpecification),
	{_, "AAAAccountingUsageSpec"} = lists:keyfind("name", 1, UsageSpecification),
	{_, {array, UsageCharacteristic}} = lists:keyfind("usageCharacteristic", 1, Usage),
	F = fun({struct, [{"name", "protocol"}, {"value", Protocol}]})
					when Protocol == "RADIUS"; Protocol == "DIAMETER" ->
				true;
			({struct, [{"name", "node"}, {"value", Node}]}) when is_list(Node) ->
				true;
			({struct, [{"name", "serverAddress"}, {"value", Address}]}) when is_list(Address) ->
				true;
			({struct, [{"name", "serverPort"}, {"value", Port}]}) when is_integer(Port) ->
				true;
			({struct, [{"name", "type"}, {"value", Type}]}) when Type == "start";
					Type == "stop"; Type == "on"; Type == "off"; Type == "interim" ->
				true;
			({struct, [{"name", "username"}, {"value", Username}]}) when is_list(Username) ->
				true;
			({struct, [{"name", "nasIpAddress"}, {"value", NasIpAddress}]}) when is_list(NasIpAddress) ->
				true;
			({struct, [{"name", "nasPort"}, {"value", Port}]}) when is_integer(Port) ->
				true;
			({struct, [{"name", "serviceType"}, {"value", Type}]}) when is_list(Type) ->
				true;
			({struct, [{"name", "framedIpAddress"}, {"value", Address}]}) when is_list(Address) ->
				true;
			({struct, [{"name", "framedPool"}, {"value", Pool}]}) when is_list(Pool) ->
				true;
			({struct, [{"name", "framedIpNetmask"}, {"value", Netmask}]}) when is_list(Netmask) ->
				true;
			({struct, [{"name", "framedRouting"}, {"value", Routing}]}) when is_list(Routing) ->
				true;
			({struct, [{"name", "filterId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "framedMtu"}, {"value", Mtu}]}) when is_integer(Mtu) ->
				true;
			({struct, [{"name", "framedRoute"}, {"value", Route}]}) when is_list(Route) ->
				true;
			({struct, [{"name", "class"}, {"value", Class}]}) when is_list(Class) ->
				true;
			({struct, [{"name", "sessionTimeout"}, {"value", Timeout}]}) when is_integer(Timeout) ->
				true;
			({struct, [{"name", "idleTimeout"}, {"value", Timeout}]}) when is_integer(Timeout) ->
				true;
			({struct, [{"name", "calledStationId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "callingStationId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "nasIdentifier"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "nasPortId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "nasPortType"}, {"value", Type}]}) when is_list(Type) ->
				true;
			({struct, [{"name", "portLimit"}, {"value", Limit}]}) when is_integer(Limit) ->
				true;
			({struct, [{"name", "acctDelayTime"}, {"value", Time}]}) when is_integer(Time) ->
				true;
			({struct, [{"name", "eventTimestamp"}, {"value", DateTime}]}) when is_list(DateTime) ->
				true;
			({struct, [{"name", "acctSessionId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "acctMultiSessionId"}, {"value", Id}]}) when is_list(Id) ->
				true;
			({struct, [{"name", "acctLinkCount"}, {"value", Count}]}) when is_integer(Count) ->
				true;
			({struct, [{"name", "acctAuthentic"}, {"value", Type}]}) when is_list(Type) ->
				true;
			({struct, [{"name", "acctSessionTime"}, {"value", Time}]}) when is_integer(Time) ->
				true;
			({struct, [{"name", "inputOctets"}, {"value", Octets}]}) when is_integer(Octets) ->
				true;
			({struct, [{"name", "outputOctets"}, {"value", Octets}]}) when is_integer(Octets) ->
				true;
			({struct, [{"name", "acctInputGigawords"}, {"value", Words}]}) when is_integer(Words) ->
				true;
			({struct, [{"name", "acctOutputGigawords"}, {"value", Words}]}) when is_integer(Words) ->
				true;
			({struct, [{"name", "acctInputPackets"}, {"value", Packets}]}) when is_integer(Packets) ->
				true;
			({struct, [{"name", "acctOutputPackets"}, {"value", Packets}]}) when is_integer(Packets) ->
				true;
			({struct, [{"name", "acctTerminateCause"}, {"value", Cause}]}) when is_list(Cause) ->
				true
	end,
	true = lists:all(F, UsageCharacteristic).

get_acct_usage_id() ->
	[{userdata, [{doc,"Get a single TMF635 acct usage"}]}].

get_acct_usage_id(Config) ->
	Attrs = [{?UserName, "ED:DA:EB:FE:AC:EF"},
			{?CallingStationId, "ED:DA:EB:FE:AC:EF"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?NasIdentifier, "ap-1.sigscale.net"},
			{?AcctSessionTime, 3600}, {?AcctInputOctets, 756012},
			{?AcctOutputOctets, 312658643}, {?AcctTerminateCause, 5}], 
	ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1812}, stop, Attrs, undefined, undefined),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri1 = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage",
	Request1 = {RequestUri1, [Accept, auth_header()]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, Usages} = mochijson:decode(Body1),
	{struct, Usage} = lists:last(Usages),
	{_, Href} = lists:keyfind("href", 1, Usage),
	RequestUri2 = HostUrl ++ Href,
	Request2 = {RequestUri2, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, Body2} = Result2,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(Body2)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, Usage} = mochijson:decode(Body2),
	{_, _Id} = lists:keyfind("id", 1, Usage),
	{_, Href} = lists:keyfind("href", 1, Usage).

get_acct_usage_filter() ->
	[{userdata, [{doc,"Get filtered TMF635 acct usage"}]}].

get_acct_usage_filter(Config) ->
	Attrs = [{?UserName, "ED:DD:B8:F6:4C:8A"},
			{?CallingStationId, "ED:DA:EB:98:84:A2"},
			{?CalledStationId, "CA-FE-CA-FE-CA-FE:AP 1"},
			{?NasIdentifier, "ap-1.sigscale.net"},
			{?AcctSessionTime, 3600}, {?AcctInputOctets, 890123},
			{?AcctOutputOctets, 482634213}, {?AcctTerminateCause, 5}], 
	ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1812}, stop, Attrs, undefined, undefined),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage&sort=-date&fields=date,status,usageCharacteristic",
	Request = {RequestUri, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, Usages} = mochijson:decode(Body),
	{struct, Usage} = lists:last(Usages),
	{_, _, Usage1} = lists:keytake("id", 1, Usage),
	{_, _, Usage2} = lists:keytake("href", 1, Usage1),
	{_, _, Usage3} = lists:keytake("date", 1, Usage2),
	{_, _, Usage4} = lists:keytake("status", 1, Usage3),
	{_, {_, {array, _UsageCharacteristic}}, []} = lists:keytake("usageCharacteristic", 1, Usage4).

get_balance_range() ->
	[{userdata, [{doc,"Get range of items in the usage collection"}]}].

get_balance_range(Config) ->
	{ok, PageSize} = application:get_env(ocs, rest_page_size),
	Flog = fun(_F, 0) ->
				ok;
			(F, N) ->
				ok = ocs_log:abmf_open(),
				Start = erlang:system_time(millisecond),
				Subscriber = list_to_binary(ocs:generate_identity()),
				Type = transfer,
				BucketId = integer_to_list(Start) ++ "-"
							++ integer_to_list(erlang:unique_integer([positive])),
				Units = case rand:uniform(3) of
					1 -> cents;
					2 -> octets;
					3 -> seconds
				end,
				CurrentAmount = rand:uniform(100000000),
				Transfer = rand:uniform(50000),
				BucketAmount = Transfer,
				BeforeAmount = CurrentAmount,
				AfterAmount = CurrentAmount - Transfer,
				ProdId = ocs:generate_password(),
				ok = ocs_log:abmf_log(Type, Subscriber, BucketId, Units,
					ProdId, BucketAmount, BeforeAmount, AfterAmount,
					undefined, undefined, undefined, undefined, undefined,
         		undefined, undefined),
				F(F, N - 1)
	end,
	NumLogged = (PageSize * 2) + (PageSize div 2) + 17,
	ok = Flog(Flog, NumLogged),
	RangeSize = case PageSize > 25 of
		true ->
			rand:uniform(PageSize - 10) + 10;
		false ->
			PageSize - 1
	end,
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RequestHeaders1 = [Accept, auth_header()],
	Request1 = {HostUrl ++ "/ocs/v1/log/balance", RequestHeaders1},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, ResponseHeaders1, Body1} = Result1,
	{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders1),
	true = is_etag_valid(Etag),
	{_, AcceptRanges1} = lists:keyfind("accept-ranges", 1, ResponseHeaders1),
	true = lists:member("items", string:tokens(AcceptRanges1, ", ")),
	{_, Range1} = lists:keyfind("content-range", 1, ResponseHeaders1),
	["items", "1", RangeEndS1, "*"] = string:tokens(Range1, " -/"),
	RequestHeaders2 = RequestHeaders1 ++ [{"if-match", Etag}],
	PageSize = list_to_integer(RangeEndS1),
	{array, Usages1} = mochijson:decode(Body1),
	PageSize = length(Usages1),
	Fget = fun(F, RangeStart2, RangeEnd2) ->
				RangeHeader = [{"range",
						"items " ++ integer_to_list(RangeStart2)
						++ "-" ++ integer_to_list(RangeEnd2)}],
				RequestHeaders3 = RequestHeaders2 ++ RangeHeader,
				Request2 = {HostUrl ++ "/ocs/v1/log/balance", RequestHeaders3},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, ResponseHeaders2, Body2} = Result2,
				{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders2),
				{_, AcceptRanges2} = lists:keyfind("accept-ranges", 1, ResponseHeaders2),
				true = lists:member("items", string:tokens(AcceptRanges2, ", ")),
				{_, Range} = lists:keyfind("content-range", 1, ResponseHeaders2),
				["items", RangeStartS, RangeEndS, EndS] = string:tokens(Range, " -/"),
				RangeStart2 = list_to_integer(RangeStartS),
				case EndS of
					"*" ->
						RangeEnd2 = list_to_integer(RangeEndS),
						RangeSize = (RangeEnd2 - (RangeStart2 - 1)),
						{array, Usages2} = mochijson:decode(Body2),
						RangeSize = length(Usages2),
						NewRangeStart = RangeEnd2 + 1,
						NewRangeEnd = NewRangeStart + (RangeSize - 1),
						F(F, NewRangeStart, NewRangeEnd);
					EndS when RangeEndS == EndS ->
						list_to_integer(EndS)
				end
	end,
	End = Fget(Fget, PageSize + 1, PageSize + RangeSize),
	End >= NumLogged.

get_acct_usage_range() ->
	[{userdata, [{doc,"Get range of items in the usage collection"}]}].

get_acct_usage_range(Config) ->
	{ok, PageSize} = application:get_env(ocs, rest_page_size),
	Flog = fun(_F, 0) ->
				ok;
			(F, N) ->
				ClientAddress = ocs_test_lib:ipv4(),
				ClientPort = ocs_test_lib:port(),
				Attrs = [{?UserName, ocs:generate_identity()},
						{?CallingStationId, ocs_test_lib:mac()},
						{?CalledStationId, ocs_test_lib:mac()},
						{?NasIpAddress, ClientAddress},
						{?NasPort, ClientPort},
						{?AcctSessionTime, 3600},
						{?AcctInputOctets, rand:uniform(100000000)},
						{?AcctOutputOctets, rand:uniform(10000000000)},
						{?AcctTerminateCause, 5}], 
				ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1812}, stop, Attrs, undefined, undefined),
				F(F, N - 1)
	end,
	NumLogged = (PageSize * 2) + (PageSize div 2) + 17,
	ok = Flog(Flog, NumLogged),
	RangeSize = case PageSize > 100 of
		true ->
			rand:uniform(PageSize - 10) + 10;
		false ->
			PageSize - 1
	end,
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RequestHeaders1 = [Accept, auth_header()],
	Request1 = {HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage", RequestHeaders1},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, ResponseHeaders1, Body1} = Result1,
	{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders1),
	true = is_etag_valid(Etag),
	{_, AcceptRanges1} = lists:keyfind("accept-ranges", 1, ResponseHeaders1),
	true = lists:member("items", string:tokens(AcceptRanges1, ", ")),
	{_, Range1} = lists:keyfind("content-range", 1, ResponseHeaders1),
	["items", "1", RangeEndS1, "*"] = string:tokens(Range1, " -/"),
	RequestHeaders2 = RequestHeaders1 ++ [{"if-match", Etag}],
	PageSize = list_to_integer(RangeEndS1),
	{array, Usages1} = mochijson:decode(Body1),
	PageSize = length(Usages1),
	Fget = fun(F, RangeStart2, RangeEnd2) ->
				RangeHeader = [{"range",
						"items " ++ integer_to_list(RangeStart2)
						++ "-" ++ integer_to_list(RangeEnd2)}],
				RequestHeaders3 = RequestHeaders2 ++ RangeHeader,
				Request2 = {HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage", RequestHeaders3},
				{ok, Result2} = httpc:request(get, Request2, [], []),
				{{"HTTP/1.1", 200, _OK}, ResponseHeaders2, Body2} = Result2,
				{_, Etag} = lists:keyfind("etag", 1, ResponseHeaders2),
				{_, AcceptRanges2} = lists:keyfind("accept-ranges", 1, ResponseHeaders2),
				true = lists:member("items", string:tokens(AcceptRanges2, ", ")),
				{_, Range} = lists:keyfind("content-range", 1, ResponseHeaders2),
				["items", RangeStartS, RangeEndS, EndS] = string:tokens(Range, " -/"),
				RangeStart2 = list_to_integer(RangeStartS),
				case EndS of
					"*" ->
						RangeEnd2 = list_to_integer(RangeEndS),
						RangeSize = (RangeEnd2 - (RangeStart2 - 1)),
						{array, Usages2} = mochijson:decode(Body2),
						RangeSize = length(Usages2),
						NewRangeStart = RangeEnd2 + 1,
						NewRangeEnd = NewRangeStart + (RangeSize - 1),
						F(F, NewRangeStart, NewRangeEnd);
					EndS when RangeEndS == EndS ->
						list_to_integer(EndS)
				end
	end,
	End = Fget(Fget, PageSize + 1, PageSize + RangeSize),
	End >= NumLogged.

get_ipdr_usage() ->
	[{userdata, [{doc,"Get a TMF635 IPDR usage"}]}].

get_ipdr_usage(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=PublicWLANAccessUsage",
	Request = {RequestUri, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, {array, [{struct, Usage}]}} = mochijson:decode(Body),
	{_, _} = lists:keyfind("id", 1, Usage),
	{_, _} = lists:keyfind("href", 1, Usage),
	{_, _} = lists:keyfind("date", 1, Usage),
	{_, "PublicWLANAccessUsage"} = lists:keyfind("type", 1, Usage),
	{_, _} = lists:keyfind("description", 1, Usage),
	{_, "recieved"} = lists:keyfind("status", 1, Usage),
	{struct, UsageSpecification} = lists:keyfind("usageSpecification", 1, Usage),
	{_, _} = lists:keyfind("id", 1, UsageSpecification),
	{_, _} = lists:keyfind("href", 1, UsageSpecification),
	{_, "PublicWLANAccessUsageSpec"} = lists:keyfind("name", 1, UsageSpecification),
	{array, UsageCharacteristic} = lists:keyfind("usageCharacteristic", 1, Usage),
	F = fun({struct, [{"name", "userName"},{"value", UserName}]}) when is_list(UserName)->
				true;
			({struct, [{"name", "acctSessionId"},{"value", AcctSessionId}]}) when is_list(AcctSessionId) ->
				true;
			({struct, [{"name", "userIpAddress"},{"value", UserIpAddress}]}) when is_list(UserIpAddress) ->
				true;
			({struct, [{"name", "callingStationId"},{"value", CallingStationId}]}) when is_list(CallingStationId) ->
				true;
			({struct, [{"name", "calledStationId"},{"value", CalledStationId}]}) when is_list(CalledStationId) ->
				true;
			({struct, [{"name", "nasIpAddress"},{"value", NasIpAddress}]}) when is_list(NasIpAddress) ->
				true;
			({struct, [{"name", "nasId"},{"value", NasId}]}) when is_list(NasId) ->
				true;
			({struct, [{"name", "sessionDuration"},{"value", SessionDuration}]}) when is_integer(SessionDuration) ->
				true;
			({struct, [{"name", "inputOctets"},{"value", InputOctets}]}) when is_integer(InputOctets) ->
				true;
			({struct, [{"name", "outputOctets"},{"value", OutputOctets}]}) when is_integer(OutputOctets) ->
				true;
			({struct, [{"name", "sessionTerminateCause"},{"value", SessionTerminateCause}]}) when is_integer(SessionTerminateCause) ->
				true
	end,
	true = lists:all(F, UsageCharacteristic).

top_up() ->
	[{userdata, [{doc,"TMF654 Prepay Balance Management API :
			Top-up add a new bucket"}]}].

top_up(Config) ->
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	ContentType = "application/json",
	RequestURI = HostUrl ++ "/balanceManagement/v1/product/" ++ ProdRef ++ "/balanceTopup",
	BucketType = {"type", ocs:generate_identity()},
	Channel = {"channel", {struct, [{"name", ocs:generate_identity()}]}},
	RechargeAmount = rand:uniform(10000000),
	Amount = {"amount", {struct, [{"units", octets}, {"amount", RechargeAmount}]}},
	Product = {"product", {struct, [{"id", ProdRef}]}},
	SDT = erlang:system_time(millisecond),
	EDT = erlang:system_time(millisecond) + rand:uniform(10000000000),
	ValidFor = {"validFor",
			{struct, [{"startDateTime", ocs_rest:iso8601(SDT)},
			{"endDateTime", ocs_rest:iso8601(EDT)}]}},
	JSON = {struct, [BucketType, Channel, Amount, Product, ValidFor]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	Request = {RequestURI, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, Href} = lists:keyfind("location", 1, Headers),
	BucketId = lists:last(string:tokens(Href, "/")),
	{ok, #bucket{units = octets, remain_amount = RechargeAmount,
			start_date = SDT, end_date = EDT,
			product = [ProdRef]}} = ocs:find_bucket(BucketId).

get_balance() ->
	[{userdata, [{doc,"TMF654 Prepay Balance Management API :
			Get accumulated balance for given product instance"}]}].

get_balance(Config) ->
	HostUrl = ?config(host_url, Config),
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	B1 = b(cents, 10000),
	B2 = b(octets, 150000000),
	B3 = #bucket{units = cents, remain_amount = 500,
			start_date = erlang:system_time(millisecond) - (2 * 2592000000),
			end_date = erlang:system_time(millisecond) - 2592000000},
	{_, _, #bucket{id = BId1}} = ocs:add_bucket(ProdRef, B1),
	{_, _, #bucket{id = BId2}} = ocs:add_bucket(ProdRef, B2),
	{_, _, #bucket{}} = ocs:add_bucket(ProdRef, B3),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	CentsBal = ocs_rest:millionths_out(B1#bucket.remain_amount),
	OctetsBal = integer_to_list(B2#bucket.remain_amount) ++ "b",
	Path = "/balanceManagement/v1/product/" ++ ProdRef ++ "/accumulatedBalance",
	GETURI = HostUrl ++ Path,
	GETRequest = {GETURI, [Accept, auth_header()]},
	{ok, GETResult} = httpc:request(get, GETRequest, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = GETResult,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, PrePayBalance} = mochijson:decode(Body),
	{_, {array, TAStructs}} = lists:keyfind("totalBalance", 1, PrePayBalance),
	{_, {array, [{struct, Product}]}} = lists:keyfind("product", 1, PrePayBalance),
	{_, {array, Buckets}} = lists:keyfind("buckets", 1, PrePayBalance),
	{_, ProdRef} = lists:keyfind("id", 1, Product),
	{_, Path} =
			lists:keyfind("href", 1, Product),
	F1 = fun({struct, B}) ->
		case lists:keyfind("id", 1, B) of
			{_, Id} when Id == BId1; Id == BId2 ->
				true;
			_ ->
				false
		end
	end,
	true = lists:all(F1, Buckets),
	F2 = fun({struct, ObjList}) ->
		case lists:keyfind("amount", 1, ObjList) of
			{_, Amount} when Amount == CentsBal; Amount == OctetsBal ->
				true;
			_ ->
				false
		end
	end,
	true = lists:all(F2, TAStructs).

get_balance_service() ->
	[{userdata, [{doc,"TMF654 Prepay Balance Management API :
			Get accumulated balance for given service identifier"}]}].

get_balance_service(Config) ->
	HostUrl = ?config(host_url, Config),
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId = offer_add([P1], 4),
	ProdRef = product_add(OfferId),
	B1 = b(cents, 10000),
	B2 = b(octets, 150000000),
	B3 = #bucket{units = cents, remain_amount = 500,
			start_date = erlang:system_time(millisecond) - (2 * 2592000000),
			end_date = erlang:system_time(millisecond) - 2592000000},
	{_, _, #bucket{id = BId1}} = ocs:add_bucket(ProdRef, B1),
	{_, _, #bucket{id = BId2}} = ocs:add_bucket(ProdRef, B2),
	{_, _, #bucket{}} = ocs:add_bucket(ProdRef, B3),
	ServiceId = service_add(ProdRef),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	CentsBal = ocs_rest:millionths_out(B1#bucket.remain_amount),
	OctetsBal = integer_to_list(B2#bucket.remain_amount) ++ "b",
	Path = "/balanceManagement/v1/service/" ++ ServiceId ++ "/accumulatedBalance",
	GETURI = HostUrl ++ Path,
	GETRequest = {GETURI, [Accept, auth_header()]},
	{ok, GETResult} = httpc:request(get, GETRequest, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = GETResult,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, PrePayBalance} = mochijson:decode(Body),
	{_, {array, TAStructs}} = lists:keyfind("totalBalance", 1, PrePayBalance),
	{_, {array, [{struct, Product}]}} = lists:keyfind("product", 1, PrePayBalance),
	{_, {array, Buckets}} = lists:keyfind("buckets", 1, PrePayBalance),
	{_, ProdRef} = lists:keyfind("id", 1, Product),
	{_, Path} =
			lists:keyfind("href", 1, Product),
	F1 = fun({struct, B}) ->
		case lists:keyfind("id", 1, B) of
			{_, Id} when Id == BId1; Id == BId2 ->
				true;
			_ ->
				false
		end
	end,
	true = lists:all(F1, Buckets),
	F2 = fun({struct, ObjList}) ->
		case lists:keyfind("amount", 1, ObjList) of
			{_, Amount} when Amount == CentsBal; Amount == OctetsBal ->
				true;
			_ ->
				false
		end
	end,
	true = lists:all(F2, TAStructs).

query_buckets() ->
	[{userdata, [{doc,"query buckets based on product id"}]}].

query_buckets(Config) ->
	HostUrl = ?config(host_url, Config),
	P1 = price(usage, octets, rand:uniform(10000), rand:uniform(100)),
	OfferId1 = offer_add([P1], 4),
	ProdRef1 = product_add(OfferId1),
	B1 = b(cents, 10000),
	B2 = b(octets, 150000000),
	{_, _, #bucket{id = BId1}} = ocs:add_bucket(ProdRef1, B1),
	{_, _, #bucket{id = BId2}} = ocs:add_bucket(ProdRef1, B2),
	P2 = price(one_time, undefined, undefined, 1000),
	OfferId2 = offer_add([P2], 4),
	ProdRef2 = product_add(OfferId2),
	B3 = b(cents, 450000),
	{_, _, #bucket{}} = ocs:add_bucket(ProdRef2, B3),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	Path = "/balanceManagement/v1/bucket" ++ "?product.id=" ++ ProdRef1,
	GETRequest = {HostUrl ++ Path, [Accept, auth_header()]},
	{ok, GETResult} = httpc:request(get, GETRequest, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = GETResult,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, BucketStructs} = mochijson:decode(Body),
	F1 = fun({struct, B}) ->
		case lists:keyfind("id", 1, B) of
			{_, Id} when Id == BId1; Id == BId2 ->
				true;
			_ ->
				false
		end
	end,
	true = lists:all(F1, BucketStructs).

simultaneous_updates_on_client_failure() ->
	[{userdata, [{doc,"Simulataneous HTTP PATCH requests on client resource must fail
			if the resource is already being updated by someone else"}]}].

simultaneous_updates_on_client_failure(Config) ->
	ContentType = "application/json",
	ID = "10.3.53.91",
	Port = 3699,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _Etag} = lists:keyfind("etag", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/ocs/v1/client/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, Port} = lists:keyfind("port", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object),
	RestPort = ?config(port, Config),
	{ok, SslSock} = ssl:connect({127,0,0,1}, RestPort,  [binary, {active, false}], infinity),
	NewSecret = ocs:generate_password(),
	PatchBody =  "{\"secret\" : \""  ++ NewSecret ++ "\"}",
	PatchBodyLen = size(list_to_binary(PatchBody)),
	PatchUri = "/ocs/v1/client/" ++ ID,
	TS = integer_to_list(erlang:system_time(milli_seconds)),
	N = integer_to_list(erlang:unique_integer([positive])),
	NewEtag = TS ++ "-" ++ N,
	PatchReq = ["PATCH ", PatchUri, " HTTP/1.1",$\r,$\n,
			"Content-Type:application/json", $\r,$\n, "Accept:application/json",$\r,$\n,
			"If-match:" ++ NewEtag,$\r,$\n,"Authorization:"++ basic_auth(),$\r,$\n,
			"Host:localhost:" ++ integer_to_list(RestPort),$\r,$\n,
			"Content-Length:" ++ integer_to_list(PatchBodyLen),$\r,$\n,
			$\r,$\n,
			PatchBody],
	ok = ssl:send(SslSock, list_to_binary(PatchReq)),
	Timeout = 1500,
	F = fun(_F, _Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			(F, Sock, {ok, Bin}, Acc) ->
					F(F, Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F(F, SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[H, _ErroMsg] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	<<"HTTP/1.1 412", _/binary>> = H,
	ok = ssl:close(SslSock).

update_client_password_json_patch() ->
	[{userdata, [{doc,"Use HTTP PATCH to update client's password using
			json-patch media type"}]}].

update_client_password_json_patch(Config) ->
	ContentType = "application/json",
	ID = "10.21.65.83",
	Port = 3781,
	Protocol = "RADIUS",
	Secret = ocs:generate_password(),
	JSON = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, Etag} = lists:keyfind("etag", 1, Headers),
	true = is_etag_valid(Etag),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/ocs/v1/client/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, Port} = lists:keyfind("port", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object),
	RestPort = ?config(port, Config),
	{ok, SslSock} = ssl:connect({127,0,0,1}, RestPort,  [binary, {active, false}], infinity),
	NewContentType = "application/json-patch+json",
	NewSecret = ocs:generate_password(),
	JSON1 = {array, [{struct, [{op, "replace"}, {path, "/secret"}, {value, NewSecret}]}]},
	PatchBody = lists:flatten(mochijson:encode(JSON1)),
	PatchBodyLen = size(list_to_binary(PatchBody)),
	PatchUri = "/ocs/v1/client/" ++ ID,
	PatchReq = ["PATCH ", PatchUri, " HTTP/1.1",$\r,$\n,
			"Content-Type:"++ NewContentType, $\r,$\n, "Accept:application/json",$\r,$\n,
			"If-match:" ++ Etag,$\r,$\n,"Authorization:"++ basic_auth(),$\r,$\n,
			"Host:localhost:" ++ integer_to_list(RestPort),$\r,$\n,
			"Content-Length:" ++ integer_to_list(PatchBodyLen),$\r,$\n,
			$\r,$\n,
			PatchBody],
	ok = ssl:send(SslSock, list_to_binary(PatchReq)),
	Timeout = 1500,
	F = fun(_F, _Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			(F, Sock, {ok, Bin}, Acc) ->
					F(F, Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F(F, SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[Headers1, ResponseBody1] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	<<"HTTP/1.1 200", _/binary>> = Headers1,
	{struct, Object1} = mochijson:decode(ResponseBody1),
	{_, ID} = lists:keyfind("id", 1, Object1),
	{_, URI} = lists:keyfind("href", 1, Object1),
	{_, Port} = lists:keyfind("port", 1, Object1),
	{_, Protocol} = lists:keyfind("protocol", 1, Object1),
	{_, NewSecret} = lists:keyfind("secret", 1, Object1),
	ok = ssl:close(SslSock).

update_client_attributes_json_patch() ->
	[{userdata, [{doc,"Use HTTP PATCH to update client's attributes using
			json-patch media type"}]}].

update_client_attributes_json_patch(Config) ->
	ContentType = "application/json",
	ID = "103.73.94.4",
	Port = 2768,
	Protocol = "RADIUS",
	Secret = ocs:generate_password(),
	JSON = {struct, [{"id", ID}, {"port", Port}, {"protocol", Protocol},
		{"secret", Secret}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, Etag} = lists:keyfind("etag", 1, Headers),
	true = is_etag_valid(Etag),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/ocs/v1/client/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, Port} = lists:keyfind("port", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object),
	RestPort = ?config(port, Config),
	{ok, SslSock} = ssl:connect({127,0,0,1}, RestPort,  [binary, {active, false}], infinity),
	NewContentType = "application/json-patch+json",
	NewPort = 8745,
	NewProtocol = "DIAMETER",
	JSON1 = {array, [{struct, [{op, "replace"}, {path, "/port"}, {value, NewPort}]},
			{struct, [{op, "replace"}, {path, "/protocol"}, {value, NewProtocol}]}]},
	PatchBody = lists:flatten(mochijson:encode(JSON1)),
	PatchBodyLen = size(list_to_binary(PatchBody)),
	PatchUri = "/ocs/v1/client/" ++ ID,
	PatchReq = ["PATCH ", PatchUri, " HTTP/1.1",$\r,$\n,
			"Content-Type:"++ NewContentType, $\r,$\n, "Accept:application/json",$\r,$\n,
			"If-match:" ++ Etag,$\r,$\n,"Authorization:"++ basic_auth(),$\r,$\n,
			"Host:localhost:" ++ integer_to_list(RestPort),$\r,$\n,
			"Content-Length:" ++ integer_to_list(PatchBodyLen),$\r,$\n,
			$\r,$\n,
			PatchBody],
	ok = ssl:send(SslSock, list_to_binary(PatchReq)),
	Timeout = 1500,
	F = fun(_F, _Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			(F, Sock, {ok, Bin}, Acc) ->
					F(F, Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F(F, SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[Headers1, ResponseBody1] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	<<"HTTP/1.1 200", _/binary>> = Headers1,
	{struct, Object1} = mochijson:decode(ResponseBody1),
	{_, ID} = lists:keyfind("id", 1, Object1),
	{_, URI} = lists:keyfind("href", 1, Object1),
	{_, NewPort} = lists:keyfind("port", 1, Object1),
	{_, NewProtocol} = lists:keyfind("protocol", 1, Object1),
	{_, Secret} = lists:keyfind("secret", 1, Object1),
	ok = ssl:close(SslSock).

post_hub_balance() ->
	[{userdata, [{doc, "Register hub listener for balance"}]}].

post_hub_balance(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathBalanceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

delete_hub_balance() ->
	[{userdata, [{doc, "Unregister hub listener for balance"}]}].

delete_hub_balance(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathBalanceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_balance_hubs() ->
	[{userdata, [{doc, "Get balance hub listeners"}]}].

get_balance_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathBalanceHub ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_balance_hub() ->
	[{userdata, [{doc, "Get balance hub listener by identifier"}]}].

get_balance_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathBalanceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

notify_create_bucket() ->
	[{userdata, [{doc, "Receive balance creation notification."}]}].

notify_create_bucket(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifycreatebucket",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Price = #price{name = ocs:generate_identity(),
			type = usage, units = octets, size = 1000, amount = 100},
	Offer = #offer{name = ocs:generate_identity(),
			price = [Price], specification = 4},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	Bucket = #bucket{units = cents, remain_amount = 100,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000},
	{ok, _, #bucket{}} = ocs:add_bucket(ProdRef, Bucket),
	Balance = receive
		Receive ->
			{struct, BalanceEvent} = mochijson:decode(Receive),
			{_, "BucketBalanceCreationNotification"}
					= lists:keyfind("eventType", 1, BalanceEvent),
			{_, {struct, BalanceList}} = lists:keyfind("event", 1, BalanceEvent),
			BalanceList
	end,
	{_, {struct, RemainAmount}} = lists:keyfind("remainedAmount", 1, Balance),
	{_, "cents"} = lists:keyfind("units", 1, RemainAmount),
	{_, MillionthsOut} = lists:keyfind("amount", 1, RemainAmount),
	100 = ocs_rest:millionths_in(MillionthsOut),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_delete_bucket() ->
	[{userdata, [{doc, "Notify deletion of bucket"}]}].

notify_delete_bucket(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydeletebucket",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	BId = add_bucket(ProdRef, cents, 100000000),
	receive
		Receive1 ->
			{struct, BalanceEvent} = mochijson:decode(Receive1),
			{_, {struct, BalanceList}} = lists:keyfind("event", 1, BalanceEvent),
			{_, BId} = lists:keyfind("id", 1, BalanceList)
	end,
	ok = ocs:delete_bucket(BId),
	receive
		Receive2 ->
			{struct, BalDelEvent} = mochijson:decode(Receive2),
			{_, "BucketBalanceDeletionEvent"}
					= lists:keyfind("eventType", 1, BalDelEvent)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_rating_deleted_bucket() ->
	[{userdata, [{doc, "Notify deletion of bucket during rating"}]}].

notify_rating_deleted_bucket(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifyratingdeletedbucket",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	{ok, #service{name = ServiceId}} = ocs:add_service(ocs:generate_identity(),
			ocs:generate_password(), ProdRef, []),
	Bucket = #bucket{units = cents, remain_amount = 100,
			start_date = erlang:system_time(millisecond) - (2 * 2592000000),
			end_date = erlang:system_time(millisecond) - 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	receive
		Receive1 ->
			{struct, BalanceEvent} = mochijson:decode(Receive1),
			{_, {struct, BalanceList}} = lists:keyfind("event", 1, BalanceEvent),
			{_, BId} = lists:keyfind("id", 1, BalanceList)
	end,
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{out_of_credit, _, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{octets, PackageSize}], SessionId),
	DeletedBalance = receive
		Receive2 ->
			{struct, BalDelEvent} = mochijson:decode(Receive2),
			{_, "BucketBalanceDeletionEvent"}
					= lists:keyfind("eventType", 1, BalDelEvent),
			{_, {struct, DeletedBalList}} = lists:keyfind("event", 1, BalDelEvent),
			DeletedBalList
	end,
	{_, {struct, RemainAmount}}
			= lists:keyfind("remainedAmount", 1, DeletedBalance),
	{_, MillionthsOut} = lists:keyfind("amount", 1, RemainAmount),
	PackagePrice = ocs_rest:millionths_in(MillionthsOut),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_accumulated_balance_threshold() ->
	[{userdata, [{doc, "Notify accumulated balance while rating if total balance"
			" is less than unit threshold"}]}].

notify_accumulated_balance_threshold(Config) ->
	PackagePrice = 5000000,
	PackageSize = 100000000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	{ok, #service{name = ServiceId}} = ocs:add_service(ocs:generate_identity(),
			ocs:generate_password(), ProdRef, []),
	_BId1 = add_bucket(ProdRef, cents, 50000000),
	RA = 500000000,
	_BId2 = add_bucket(ProdRef, octets, RA),
	_BId3 = add_bucket(ProdRef, cents, 100000000),
	Threshold = 500000000,
	ok = application:set_env(ocs, threshold_bytes, 500000000),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifyaccumulatedbalancethreshold",
	Query = "totalBalance.units=octets&totalBalance.amount.lt="
			++ integer_to_list(Threshold),
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Timestamp = calendar:local_time(),
	SessionId = [{'Session-Id', list_to_binary(ocs:generate_password())}],
	ServiceType = 32251,
	{ok, #service{}, _} = ocs_rating:rate(diameter, ServiceType, undefined,
			undefined, undefined, ServiceId, Timestamp, undefined, undefined,
			initial, [], [{octets, PackageSize}], SessionId),
	receive
		Input7 ->
			{struct, AccBalanceEvent} = mochijson:decode(Input7),
			{_, "AccumulatedBalanceCreationNotification"}
					= lists:keyfind("eventType", 1, AccBalanceEvent),
			{_, {array, [{struct, AccList}]}}
					= lists:keyfind("event", 1, AccBalanceEvent),
			case lists:keyfind("totalBalance", 1, AccList) of
				{_, {array, [{struct, [{"amount", Amount}, {"units", "octets"}]}]}} ->
					OctetsAmount = list_to_integer(lists:droplast(Amount)),
					OctetsAmount = RA - PackageSize;
				{_, {array, [{struct, [{"units", "octets"}, {"amount", Amount}]}]}} ->
					OctetsAmount = list_to_integer(lists:droplast(Amount)),
					OctetsAmount = RA - PackageSize
			end
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

query_accumulated_balance_notification() ->
	[{userdata, [{doc, "Query accumulated balance notification"}]}].

query_accumulated_balance_notification(Config) ->
	PackagePrice = 5000000,
	PackageSize = 100000000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	RA1 = 50000000,
	_BId1 = add_bucket(ProdRef, cents, RA1),
	RA2 = 100000000,
	BId2 = add_bucket(ProdRef, cents, RA2),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/queryaccumulatedbalancenotification",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Threshold = 100,
	Query = "?totalBalance.units=cents&totalBalance.amount.lt="
			++ integer_to_list(Threshold),
	Request2 = {HostUrl ++ "/balanceManagement/v1/product/" ++ ProdRef
			++ "/accumulatedBalance" ++ Query, [Accept, auth_header()]},
	{ok, {{_, 200, _}, _, _}} = httpc:request(get, Request2, [], []),
	ok = ocs:delete_bucket(BId2),
	receive
		Input5 ->
			{struct, BalanceDelEvent} = mochijson:decode(Input5),
			{_, {struct, BalanceDelList}}
					= lists:keyfind("event", 1, BalanceDelEvent),
			{_, BId2} = lists:keyfind("id", 1, BalanceDelList)
	end,
	{ok, {{_, 200, _}, _, _}} = httpc:request(get, Request2, [], []),
	AccBalance = receive
		Input6 ->
			{struct, AccBalanceEvent} = mochijson:decode(Input6),
			{_, "AccumulatedBalanceCreationNotification"}
					= lists:keyfind("eventType", 1, AccBalanceEvent),
			{_, {array, [{struct, AccBalList}]}}
					= lists:keyfind("event", 1, AccBalanceEvent),
			AccBalList
	end,
	Request3 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request3, [], []),
	{_, {array,[{struct, Q}]}} = lists:keyfind("totalBalance", 1, AccBalance),
	{_, Amount} = lists:keyfind("amount", 1, Q),
	list_to_integer(Amount) < Threshold.

query_bucket_notification() ->
	[{userdata, [{doc, "Query bucket notification"}]}].

query_bucket_notification(Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	BId1 = add_bucket(ProdRef, cents, 100000000),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/querybucketnotification",
	Query = "eventType=BucketBalanceDeletionEvent&id=" ++ BId1,
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	_BId2 = add_bucket(ProdRef, octets, 100000000),
	ok = ocs:delete_bucket(BId1),
	receive
		Receive ->
			{struct, BalDelEvent} = mochijson:decode(Receive),
			{_, "BucketBalanceDeletionEvent"}
					= lists:keyfind("eventType", 1, BalDelEvent),
			{_, {struct, StructList}}
					= lists:keyfind("event", 1, BalDelEvent),
			{_, BId1} = lists:keyfind("id", 1, StructList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_product_charge() ->
	[{userdata, [{doc, "Receive product charged notification"}]}].

notify_product_charge(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathBalanceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifyproductcharge",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathBalanceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	SD = erlang:system_time(millisecond),
	Alteration = #alteration{name = ocs:generate_identity(), start_date = SD,
			type = usage, period = undefined,
			units = octets, size = 100000000000, amount = 0},
	Price = #price{name = ocs:generate_identity(), start_date = SD,
			type = recurring, period = monthly,
			amount = 1250000000, alteration = Alteration},
	OfferId = add_offer([Price], 4),
	{ok, #product{id = ProdId} = P} = ocs:add_product(OfferId, []),
	Expired = erlang:system_time(millisecond) - 3599000,
	ok = mnesia:dirty_write(product, P#product{payment =
			[{Price#price.name, Expired}]}),
	B1 = #bucket{units = cents, remain_amount = 1000000000,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000},
	{ok, _, #bucket{id = BId1}} = ocs:add_bucket(ProdId, B1),
	receive
		Input1 ->
			{struct, BucketEvent1} = mochijson:decode(Input1),
			{_, "BucketBalanceCreationNotification"}
					= lists:keyfind("eventType", 1, BucketEvent1),
			{_, {struct, BucketList1}} = lists:keyfind("event", 1, BucketEvent1),
			{_, BId1} = lists:keyfind("id", 1, BucketList1)
	end,
	B2 = #bucket{units = cents, remain_amount = 1000000000,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000},
	{ok, _, #bucket{id = BId2}} = ocs:add_bucket(ProdId, B2),
	receive
		Input2 ->
			{struct, BucketEvent2} = mochijson:decode(Input2),
			{_, "BucketBalanceCreationNotification"}
					= lists:keyfind("eventType", 1, BucketEvent2),
			{_, {struct, BucketList2}} = lists:keyfind("event", 1, BucketEvent2),
			{_, BId2} = lists:keyfind("id", 1, BucketList2)
	end,
	ok = ocs_scheduler:product_charge(),
	AdjustmentStructs = receive
		Input ->
			{struct, AdjustmentEvent} = mochijson:decode(Input),
			{_, "BalanceAdjustmentCreationNotification"}
					= lists:keyfind("eventType", 1, AdjustmentEvent),
			{_, {array, AdjStructList}}
					= lists:keyfind("event", 1, AdjustmentEvent),
			AdjStructList
	end,
	Fcents = fun({struct, AdjustmentList}) ->
				case lists:keyfind("amount", 1, AdjustmentList) of
					{_, {struct, [{"amount", Amount}, {"units","cents"}]}} ->
						{true, list_to_integer(Amount)};
					{_, {struct, [{"units","cents"}, {"amount", Amount}]}} ->
						{true, list_to_integer(Amount)};
					_ ->
						false
				end
	end,
	-1250 = lists:sum(lists:filtermap(Fcents, AdjustmentStructs)),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

post_hub_product() ->
	[{userdata, [{doc, "Register hub listener for product"}]}].

post_hub_product(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathProductHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

delete_hub_product() ->
	[{userdata, [{doc, "Unregister hub listener for product"}]}].

delete_hub_product(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathProductHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_product_hubs() ->
	[{userdata, [{doc, "Get product inventory hub listeners"}]}].

get_product_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathProductHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathProductHub ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_product_hub() ->
	[{userdata, [{doc, "Get product inventory hub listener by identifier"}]}].

get_product_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathProductHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

notify_create_product() ->
	[{userdata, [{doc, "Receive product creation notification."}]}].

notify_create_product(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathProductHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifycreateproduct",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathProductHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Price = #price{name = ocs:generate_identity(),
			type = usage, units = octets, size = 1000, amount = 100},
	Offer = #offer{name = ocs:generate_identity(),
			price = [Price], specification = 4},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	receive
		Input1 ->
			{struct, OfferEvent} = mochijson:decode(Input1),
			{_, "ProductOfferingCreationNotification"}
					= lists:keyfind("eventType", 1, OfferEvent)
	end,
	{ok, #product{id = ProductId}} = ocs:add_product(OfferId, [], []),
	Product = receive
		Input2 ->
			{struct, ProductEvent} = mochijson:decode(Input2),
			{_, "ProductCreationNotification"}
					= lists:keyfind("eventType", 1, ProductEvent),
			{_, {struct, ProductList}} = lists:keyfind("event", 1, ProductEvent),
			ProductList
	end,
	{_, ProductId} = lists:keyfind("id", 1, Product),
	{_, {struct, OfferStruct}} = lists:keyfind("productOffering", 1, Product),
	{_, OfferId} = lists:keyfind("id", 1, OfferStruct),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_delete_product() ->
	[{userdata, [{doc, "Notify deletion of product"}]}].

notify_delete_product(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathProductHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydeleteproduct",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathProductHub ++ SubId} = lists:keyfind("location", 1, Headers),
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	receive
		Input1 ->
			{struct, OfferEvent} = mochijson:decode(Input1),
			{_, "ProductOfferingCreationNotification"}
					= lists:keyfind("eventType", 1, OfferEvent)
	end,
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], []),
	receive
		Input2 ->
			{struct, ProductEvent} = mochijson:decode(Input2),
			{_, {struct, ProductList}} = lists:keyfind("event", 1, ProductEvent),
			{_, ProdRef} = lists:keyfind("id", 1, ProductList)
	end,
	ok = ocs:delete_product(ProdRef),
	ProductStuct2 = receive
		Input3 ->
			{struct, ProductDelEvent} = mochijson:decode(Input3),
			{_, "ProductRemoveNotification"}
					= lists:keyfind("eventType", 1, ProductDelEvent),
			{_, ProductStuct1}
					= lists:keyfind("event", 1, ProductDelEvent),
			ProductStuct1
	end,
	#product{} = ocs_rest_res_product:inventory(ProductStuct2),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

query_product_notification() ->
	[{userdata, [{doc, "Query product notification"}]}].

query_product_notification(Config) ->
	PackagePrice = 100,
	PackageSize = 1000,
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = PackageSize, amount = PackagePrice},
	OfferId = add_offer([P1], 4),
	{ok, #product{id = ProdRef1}} = ocs:add_product(OfferId, [], []),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathProductHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/queryproductnotification",
	Query = "eventType=ProductRemoveNotification&id=" ++ ProdRef1,
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathProductHub ++ SubId} = lists:keyfind("location", 1, Headers),
	{ok, #product{id = _ProdRef2}} = ocs:add_product(OfferId, [], []),
	ok = ocs:delete_product(ProdRef1),
	receive
		Receive ->
			{struct, ProductDelEvent} = mochijson:decode(Receive),
			{_, "ProductRemoveNotification"}
					= lists:keyfind("eventType", 1, ProductDelEvent),
			{_, {struct, StructList}}
					= lists:keyfind("event", 1, ProductDelEvent),
			{_, ProdRef1} = lists:keyfind("id", 1, StructList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

post_hub_service() ->
	[{userdata, [{doc, "Register hub listener for service"}]}].

post_hub_service(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathServiceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

notify_create_service() ->
	[{userdata, [{doc, "Receive service creation notification."}]}].

notify_create_service(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathServiceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifycreateservice",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathServiceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, #service{}} = ocs:add_service(Identity, Password),
	Service = receive
		Input ->
			{struct, ServiceEvent} = mochijson:decode(Input),
			{_, "ServiceCreationNotification"}
					= lists:keyfind("eventType", 1, ServiceEvent),
			{_, {struct, ServiceList}} = lists:keyfind("event", 1, ServiceEvent),
			ServiceList
	end,
	{_, Identity} = lists:keyfind("id", 1, Service),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Service),
	F = fun({struct, [{"name", "servicePassword"}, {"value", Value}]}) ->
				{true, Value};
			({struct, [{"value", Value}, {"name", "servicePassword"}]}) ->
				{true, Value};
			(_) ->
				false
	end,
	[Password] = lists:filtermap(F, Chars),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_delete_service() ->
	[{userdata, [{doc, "Notify deletion of service"}]}].

notify_delete_service(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathServiceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydeleteservice",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathServiceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Identity = ocs:generate_identity(),
	Password = ocs:generate_password(),
	{ok, #service{}} = ocs:add_service(Identity, Password),
	receive
		Input1 ->
			{struct, ServiceEvent} = mochijson:decode(Input1),
			{_, {struct, ServiceList}} = lists:keyfind("event", 1, ServiceEvent),
			{_, Identity} = lists:keyfind("id", 1, ServiceList)
	end,
	ok = ocs:delete_service(Identity),
	receive
		Input2 ->
			{struct, ServiceDelEvent} = mochijson:decode(Input2),
			{_, "ServiceDeleteNotification"}
					= lists:keyfind("eventType", 1, ServiceDelEvent)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

delete_hub_service() ->
	[{userdata, [{doc, "Unregister hub listener for service"}]}].

delete_hub_service(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathServiceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_service_hubs() ->
	[{userdata, [{doc, "Get service inventory hub listeners"}]}].

get_service_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathServiceHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathServiceHub ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_service_hub() ->
	[{userdata, [{doc, "Get service inventory hub listener by identifier"}]}].

get_service_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathServiceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

query_service_notification() ->
	[{userdata, [{doc, "Query service notification"}]}].

query_service_notification(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathServiceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/queryservicenotification",
	Identity = ocs:generate_identity(),
	Query = "eventType=ServiceDeleteNotification&id=" ++ Identity,
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathServiceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Password = ocs:generate_password(),
	{ok, #service{}} = ocs:add_service(Identity, Password),
	ok = ocs:delete_service(Identity),
	Service = receive
		Input ->
			{struct, ServiceDelEvent} = mochijson:decode(Input),
			{_, "ServiceDeleteNotification"}
					= lists:keyfind("eventType", 1, ServiceDelEvent),
			{_, {struct, ServiceList}}
					= lists:keyfind("event", 1, ServiceDelEvent),
			ServiceList
	end,
	{_, Identity} = lists:keyfind("id", 1, Service),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Service),
	F = fun({struct, [{"name", "servicePassword"}, {"value", Value}]}) ->
				{true, Value};
			({struct, [{"value", Value}, {"name", "servicePassword"}]}) ->
				{true, Value};
			(_) ->
				false
	end,
	[Password] = lists:filtermap(F, Chars),
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

post_hub_user() ->
	[{userdata, [{doc, "Register hub listener for service"}]}].

post_hub_user(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathUserHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

delete_hub_user() ->
	[{userdata, [{doc, "Unregister hub listener for user"}]}].

delete_hub_user(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathUserHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_user_hubs() ->
	[{userdata, [{doc, "Get party management hub listeners"}]}].

get_user_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathUserHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathUserHub ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_user_hub() ->
	[{userdata, [{doc, "Get party management hub listener by identifier"}]}].

get_user_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathUserHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

post_hub_catalog() ->
	[{userdata, [{doc, "Register hub listener for catalog"}]}].

post_hub_catalog(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathCatalogHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

delete_hub_catalog() ->
	[{userdata, [{doc, "Unregister hub listener for catalog"}]}].

delete_hub_catalog(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathCatalogHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_catalog_hubs() ->
	[{userdata, [{doc, "Get product catalog hub listeners"}]}].

get_catalog_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalogHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathCatalogHub ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_catalog_hub() ->
	[{userdata, [{doc, "Get product catalog hub listener by identifier"}]}].

get_catalog_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathCatalogHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

notify_create_offer() ->
	[{userdata, [{doc, "Receive offer creation notification."}]}].

notify_create_offer(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalogHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifycreateoffer",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathCatalogHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Price1 = price(one_time, undefined, undefined, 1000),
	Price2 = price(usage, octets, 1000000000, 100),
	OfferId = add_offer([Price1, Price2], 4),
	receive
		Input ->
			{struct, OfferEvent} = mochijson:decode(Input),
			{_, "ProductOfferingCreationNotification"}
					= lists:keyfind("eventType", 1, OfferEvent),
			{_, {struct, OfferList}} = lists:keyfind("event", 1, OfferEvent),
			{_, OfferId} = lists:keyfind("id", 1, OfferList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_delete_offer() ->
	[{userdata, [{doc, "Notify deletion of offer"}]}].

notify_delete_offer(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalogHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydeleteoffer",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathCatalogHub ++ SubId} = lists:keyfind("location", 1, Headers),
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = 1000, amount = 100},
	OfferId = add_offer([P1], 4),
	receive
		Input1 ->
			{struct, OfferEvent} = mochijson:decode(Input1),
			{_, "ProductOfferingCreationNotification"}
					= lists:keyfind("eventType", 1, OfferEvent)
	end,
	ok = ocs:delete_offer(OfferId),
	receive
		Input2 ->
			{struct, OfferDelEvent} = mochijson:decode(Input2),
			{_, "ProductOfferingRemoveNotification"}
					= lists:keyfind("eventType", 1, OfferDelEvent)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

query_offer_notification() ->
	[{userdata, [{doc, "Query offer notification"}]}].

query_offer_notification(Config) ->
	P1 = #price{name = ocs:generate_identity(), type = usage, units = octets,
			size = 1000, amount = 100},
	OfferId1 = add_offer([P1], 4),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalogHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/queryoffernotification",
	Query = "eventType=ProductOfferingRemoveNotification&id=" ++ OfferId1,
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathCatalogHub ++ SubId} = lists:keyfind("location", 1, Headers),
	_OfferId2 = add_offer([P1], 4),
	ok = ocs:delete_offer(OfferId1),
	receive
		Input2 ->
			{struct, OfferDelEvent} = mochijson:decode(Input2),
			{_, "ProductOfferingRemoveNotification"}
					= lists:keyfind("eventType", 1, OfferDelEvent),
			{_, {struct, StructList}}
					= lists:keyfind("event", 1, OfferDelEvent),
			{_, OfferId1} = lists:keyfind("id", 1, StructList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

post_hub_inventory() ->
	[{userdata, [{doc, "Register hub listener for inventory"}]}].

post_hub_inventory(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathResourceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

delete_hub_inventory() ->
	[{userdata, [{doc, "Unregister hub listener for catalog"}]}].

delete_hub_inventory(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathResourceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_inventory_hubs() ->
	[{userdata, [{doc, "Get resource inventory hub listeners"}]}].

get_inventory_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathResourceHub ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_inventory_hub() ->
	[{userdata, [{doc, "Get resource inventory hub listener by identifier"}]}].

get_inventory_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathResourceHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

notify_insert_gtt() ->
	[{userdata, [{doc, "Receive resource creation notification."}]}].

notify_insert_gtt(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifyinsertgtt",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathResourceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Table = "tariff_table9",
	ok = ocs_gtt:new(Table, []),
	receive
		Input ->
		{struct, GttTableEvent} = mochijson:decode(Input),
		{_, "ResourceCreationNotification"}
				= lists:keyfind("eventType", 1, GttTableEvent),
		{_, {struct, GttTableList}} = lists:keyfind("event", 1, GttTableEvent),
		{_, Table} = lists:keyfind("name", 1, GttTableList)
	end,		
	Prefix = "1519240",
	Description = "Bell Mobility",
	Amount = 10000,
	{ok, #gtt{}} = ocs_gtt:insert(Table, Prefix, {Description, Amount}),
	receive
		Input1 ->
			{struct, GttEvent} = mochijson:decode(Input1),
			{_, "ResourceCreationNotification"}
					= lists:keyfind("eventType", 1, GttEvent),
			{_, {struct, GttList}} = lists:keyfind("event", 1, GttEvent),
			ResId = Table ++ "-" ++ Prefix,
			{_, ResId} = lists:keyfind("id", 1, GttList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_delete_gtt() ->
	[{userdata, [{doc, "Receive resource deletion notification."}]}].

notify_delete_gtt(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydeletegtt",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathResourceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Table = "tariff_table7",
	ok = ocs_gtt:new(Table, []),
	receive
		Input ->
		{struct, GttTableEvent} = mochijson:decode(Input),
		{_, "ResourceCreationNotification"}
				= lists:keyfind("eventType", 1, GttTableEvent),
		{_, {struct, GttTableList}} = lists:keyfind("event", 1, GttTableEvent),
		{_, Table} = lists:keyfind("name", 1, GttTableList)
	end,		
	Prefix = "1519240",
	Description = "Bell Mobility",
	Amount = 10000,
	{ok, #gtt{}} = ocs_gtt:insert(Table, Prefix, {Description, Amount}),
	ResId = Table ++ "-" ++ Prefix,
	receive
		Receive1 ->
			{struct, GttEvent1} = mochijson:decode(Receive1),
			{_, "ResourceCreationNotification"}
					= lists:keyfind("eventType", 1, GttEvent1),
			{_, {struct, GttList1}} = lists:keyfind("event", 1, GttEvent1),
			{_, ResId} = lists:keyfind("id", 1, GttList1)
	end,
	ok = ocs_gtt:delete(Table, Prefix),
	receive
		Receive2 ->
			{struct, GttEvent2} = mochijson:decode(Receive2),
			{_, "ResourceRemoveNotification"}
					= lists:keyfind("eventType", 1, GttEvent2),
			{_, {struct, GttList2}} = lists:keyfind("event", 1, GttEvent2),
			{_, ResId} = lists:keyfind("id", 1, GttList2)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

query_gtt_notification() ->
	[{userdata, [{doc, "Query gtt notifications"}]}].

query_gtt_notification(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/querygttnotification",
	Prefix = "1519240",
	Query = "eventType=ResourceRemoveNotification&id=" ++ Prefix,
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathResourceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Table = "tariff_table8",
	ok = ocs_gtt:new(Table, []),
	Description = "Bell Mobility",
	Amount = 10000,
	{ok, #gtt{}} = ocs_gtt:insert(Table, Prefix, {Description, Amount}),
	ok = ocs_gtt:delete(Table, Prefix),
	receive
		Receive ->
			{struct, GttEvent} = mochijson:decode(Receive),
			{_, "ResourceRemoveNotification"}
					= lists:keyfind("eventType", 1, GttEvent),
			{_, {struct, GttList}} = lists:keyfind("event", 1, GttEvent),
			ResId = Table ++ "-" ++ Prefix,
			{_, ResId} = lists:keyfind("id", 1, GttList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_add_resource() ->
	[{userdata, [{doc, "Receive resource creation notification."}]}].

notify_add_resource(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifyaddresource",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathResourceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	PolicyResource = #resource{name = "ct-example-1",
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + rand:uniform(10000000000),
			state = "created", specification = #specification_ref{id = "3",
			href = "/resourceCatalogManagement/v3/resourceSpecification/3",
			name = "PolicyTable", version = "1.0"}},
	{ok, #resource{}} = ocs:add_resource(PolicyResource),
	receive
		Receive ->
			{struct, ResEvent} = mochijson:decode(Receive),
			{_, "ResourceCreationNotification"}
					= lists:keyfind("eventType", 1, ResEvent),
			{_, {struct, ResList}} = lists:keyfind("event", 1, ResEvent),
			{_, Id} = lists:keyfind("id", 1, ResList),
			true = is_list(Id)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

notify_delete_resource() ->
	[{userdata, [{doc, "Receive resource deletion notification"}]}].

notify_delete_resource(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydeleteresource",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathResourceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	PolicyResource = #resource{name = "ct-example-2",
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + rand:uniform(10000000000),
			state = "created", specification = #specification_ref{id = "3",
			href = "/resourceCatalogManagement/v3/resourceSpecification/3",
			name = "PolicyTable", version = "1.0"}},
	{ok, #resource{id = Id}} = ocs:add_resource(PolicyResource),
	receive
		Receive1 ->
			{struct, ResEvent1} = mochijson:decode(Receive1),
			{_, "ResourceCreationNotification"}
					= lists:keyfind("eventType", 1, ResEvent1)
	end,
	ok = ocs:delete_resource(Id),
	receive
		Receive2 ->
			{struct, ResEvent2} = mochijson:decode(Receive2),
			{_, "ResourceRemoveNotification"}
					= lists:keyfind("eventType", 1, ResEvent2),
			{_, {struct, ResList}} = lists:keyfind("event", 1, ResEvent2),
			{_, Id} = lists:keyfind("id", 1, ResList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

query_resource_notification() ->
	[{userdata, [{doc, "Query resource notifications"}]}].

query_resource_notification(Config) ->
	PolicyResource = #resource{name = "Example",
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + rand:uniform(10000000000),
			state = "created", specification = #specification_ref{id = "3",
			href = "/resourceCatalogManagement/v3/resourceSpecification/3",
			name = "PolicyTable", version = "1.0"}},
	{ok, #resource{id = Id}} = ocs:add_resource(PolicyResource),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathResourceHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/queryresourcenotification",
	Query = "eventType=ResourceRemoveNotification&id=" ++ Id,
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "\t\"query\": \"" ++ Query ++ "\"\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathResourceHub ++ SubId} = lists:keyfind("location", 1, Headers),
	ok = ocs:delete_resource(Id),
	receive
		Receive ->
			{struct, ResEvent} = mochijson:decode(Receive),
			{_, "ResourceRemoveNotification"}
					= lists:keyfind("eventType", 1, ResEvent),
			{_, {struct, ResList}} = lists:keyfind("event", 1, ResEvent),
			{_, Id} = lists:keyfind("id", 1, ResList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

post_hub_usage() ->
	[{userdata, [{doc, "Register hub listener for usage"}]}].

post_hub_usage(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathUsageHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

get_usage_hubs() ->
	[{userdata, [{doc, "Get usage hub listeners"}]}].

get_usage_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathUsageHub,
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, "/usageManagement/v1/hub/" ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_usage_hub() ->
	[{userdata, [{doc, "Get hub listener"}]}].

get_usage_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathUsageHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

delete_hub_usage() ->
	[{userdata, [{doc, "Unregister hub listener for usage"}]}].

delete_hub_usage(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathUsageHub,
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

notify_diameter_acct_log() ->
	[{userdata, [{doc, "Receive DIAMETER CCR/CCA logging notification"}]}].

notify_diameter_acct_log(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathUsageHub,
	ListenerPort = ?config(listener_port, Config),
	ListenerServer = "http://localhost:" ++ integer_to_list(ListenerPort),
	Callback = ListenerServer ++ "/listener/"
			++ atom_to_list(?MODULE) ++ "/notifydiameteracctlog",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, Headers, _}} = httpc:request(post, Request1, [], []),
	{_, ?PathUsageHub ++ SubId} = lists:keyfind("location", 1, Headers),
	Protocol = diameter,
	ServerAddress = {0, 0, 0, 0},
	ServerPort = 1813,
	Server = {ServerAddress, ServerPort},
	RequestType = start,
	ok = ocs_log:acct_log(Protocol, Server, RequestType,
			#'3gpp_ro_CCR'{'Origin-Host' = <<"diameter-89.sigscale.net">>,
			'Service-Context-Id' = <<"92.32156.3gpp.org">>}, #'3gpp_ro_CCA'{},
			undefined),
	receive
		Receive ->
			{struct, AcctLogEvent} = mochijson:decode(Receive),
			{_, "UsageCreationEvent"}
					= lists:keyfind("eventType", 1, AcctLogEvent),
			{_, {struct, AcctUsageList}}
					= lists:keyfind("event", 1, AcctLogEvent),
			{_, "/usageManagement/v1/usage/acct-" ++ _}
					= lists:keyfind("href", 1, AcctUsageList),
			{_, "AAAAccountingUsage"}
					= lists:keyfind("type", 1, AcctUsageList)
	end,
	Request2 = {CollectionUrl ++ SubId, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request2, [], []).

get_tariff_resource() ->
	[{userdata, [{doc,"Get tariff resource with given resource
			inventory reference"}]}].

get_tariff_resource(Config) ->
	ok = ocs_gtt:new(tariff_table1, []),
	Schema = "/resourceInventoryManagement/v1/schema/"
			"resourceInventoryManagement#/definitions/resource",
	ResourceRelID = ocs:generate_identity(),
	Resource = #resource{class_type = "LogicalResource", base_type = "Resource",
			schema = Schema, description = "tariff row resource",
			category = "tariff", state = "Active",
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2678400000,
			related = [#resource_rel{id = ResourceRelID,
					href = "/resourceInventoryManagement/v1/resource/"
					++ ResourceRelID, type = "contained", name = "tariff_table1"}],
			specification = #specification_ref{id = "2", name = "tariff spec",
					href = "/resourceCatalogManagement/v2/resourceSpecification/2"},
			characteristic  = [#resource_char{name = "prefix", value = "125"},
					#resource_char{name = "description", value = "test"},
					#resource_char{name = "rate", value = 250}]},
	{ok, #resource{id = Id}} = ocs:add_resource(Resource),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ "/resourceInventoryManagement/v1/resource/" ++ Id,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, Object),
	{_, "/resourceInventoryManagement/v1/resource/" ++ Id}
			= lists:keyfind("href", 1, Object),
	{_, "tariff"} = lists:keyfind("category", 1, Object),
	{_, {struct, SpecList}} = lists:keyfind("resourceSpecification", 1, Object),
	{_, "tariff spec"} = lists:keyfind("name", 1, SpecList),
	{_, {array, [{struct, RelList}]}}
			= lists:keyfind("resourceRelationship", 1, Object),
	{_, {struct, ObjList}} = lists:keyfind("resource", 1, RelList),
	{_, "tariff_table1"} = lists:keyfind("name", 1, ObjList),
	{_, {array, CharList}} = lists:keyfind("resourceCharacteristic", 1, Object),
	3 = length(CharList).

get_tariff_resources() ->
	[{userdata, [{doc, "GET Resource collection"}]}].

get_tariff_resources(Config) ->
	ok = ocs_gtt:new(tariff_table2, []),
	{ok, #resource{}}
			= add_resource("1", "tariff table", "tariff", "tariff_table2"),
	{ok, #resource{}}
			= add_resource("2", "tariff row", "tariff", "tariff_table2"),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ "/resourceInventoryManagement/v1/resource/",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, Objects} = mochijson:decode(ResponseBody),
	true = length(Objects) >= 2.

post_tariff_resource() ->
	[{userdata, [{doc,"Add tariff resource in rest interface"}]}].

post_tariff_resource(Config) ->
	Table = "tariff_table4",
	ok = ocs_gtt:new(Table, []),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ "/resourceInventoryManagement/v1/resource/",
	Name = "Tariff",
	Description = "tariff resource",
	Version = random_string(3),
	ClassType = "LogicalResource",
	ClassSchema = "/resourceInventoryManagement/v3/"
			"schema/resourceInventoryManagement",
	BaseType = "Resource",
	Category = "tariff",
	ResSpecId = "2",
	ResSpecName = "TariffTableRow",
	ResSpecHref = "/resourceCatalogManagement/v2/resourceSpecification/"
			++ ResSpecId,
	ResourceRelId = ocs:generate_identity(),
	ResourceRelHref = "/resourceInventoryManagement/v1/resource/"
			++ ResourceRelId,
	CharPrefix = "125",
	CharDes = "test",
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ Name ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"category\": \"" ++ Category ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"@schemaLocation\": \"" ++ ClassSchema ++ "\",\n"
			++ "\t\"@baseType\": \"" ++ BaseType ++ "\",\n"
			++ "\t\"version\": \"" ++ Version ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2021-01-20T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2021-12-31T23:59\"\n"
			++ "\t},\n"
			++ "\t\"lifecycleState\": \"In Test\",\n"
			++ "\t\"resourceSpecification\": {\n"
			++ "\t\t\"id\": \"" ++ ResSpecId ++ "\",\n"
			++ "\t\t\"href\": \"" ++ ResSpecHref ++ "\",\n"
			++ "\t\t\"name\": \"" ++ ResSpecName ++ "\"\n"
			++ "\t\t},\n"
			++ "\t\"resourceRelationship\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"resource\": {\n"
			++ "\t\t\t\t\"id\": \"" ++ ResourceRelId ++ "\",\n"
			++ "\t\t\t\t\"href\": \"" ++ ResourceRelHref ++ "\",\n"
			++ "\t\t\t\t\"name\": \"" ++ Table ++ "\"\n"
			++ "\t\t\t\t},\n"
			++ "\t\t\t\"relationshipType\": \"contained\"\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"resourceCharacteristic\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"name\": \"prefix\",\n"
			++ "\t\t\t\"value\": \"" ++ CharPrefix ++ "\"\n"
			++ "\t\t},\n"
			++ "\t\t{\n"
			++ "\t\t\t\"name\": \"description\",\n"
			++ "\t\t\t\"value\": \"" ++ CharDes ++ "\"\n"
			++ "\t\t},\n"
			++ "\t\t{\n"
			++ "\t\t\t\"name\": \"rate\",\n"
			++ "\t\t\t\"value\": \"250\"\n"
			++ "\t\t}\n"
			++ "\t]\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/resourceInventoryManagement/v1/resource/" ++ Id, _}
			= httpd_util:split_path(URI),
	[Table, CharPrefix] = string:tokens(Id, "-"),
	{CharDes, 250000000, _} = ocs_gtt:lookup_first(Table, CharPrefix).

delete_tariff_resource() ->
	[{userdata, [{doc,"Delete tariff resource inventory"}]}].

delete_tariff_resource(Config) ->
	ok = ocs_gtt:new(tariff_table3, []),
	{ok, #resource{id = Id}}
			= add_resource("1", "tariff table", "tariff", "tariff_table3"),
	URI = "/resourceInventoryManagement/v1/resource/" ++ Id,
	HostUrl = ?config(host_url, Config),
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers, []} = Result,
	{_, "0"} = lists:keyfind("content-length", 1, Headers),
	{error, not_found} = ocs:get_resource(Id).

update_tariff_resource() ->
	[{userdata, [{doc, "Use PATCH for update tariff resource entity"}]}].

update_tariff_resource(Config) ->
	ResourceHref = "/resourceInventoryManagement/v1/resource/",
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	ReqList = resource_inventory(),
	ReqBody = lists:flatten(mochijson:encode({struct, ReqList})),
	Request1 = {HostUrl ++ ResourceHref,
			[Accept, auth_header()], ContentType, ReqBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, "Created"}, Headers1, _ResponseBody1} = Result1,
	{_, Etag} = lists:keyfind("etag", 1, Headers1),
	{_, URI} = lists:keyfind("location", 1, Headers1),
	{"/resourceInventoryManagement/v1/resource/" ++ ResourceId, _}
			= httpd_util:split_path(URI),
	NewDescription = "testing update",
	NewRate1 = 456,
	ResCharOp1 = {struct, [{op, "replace"}, {path, "/resourceCharacteristic/1/value"},
			{value, NewDescription}]},
	ResCharOp2 = {struct, [{op, "replace"}, {path, "/resourceCharacteristic/2/value"},
			{value, NewRate1}]},
	OpArray = {array, [ResCharOp1, ResCharOp2]},
	PatchReqBody = lists:flatten(mochijson:encode(OpArray)),
	PatchContentType = "application/json-patch+json",
	Request2 = {HostUrl ++ ResourceHref ++ ResourceId, [Accept, auth_header(),
			{"if-match", Etag}], PatchContentType, PatchReqBody},
	{ok, Result2} = httpc:request(patch, Request2, [], []),
	{{"HTTP/1.1", 200, "OK"}, _Headers2, _ResponseBody2} = Result2,
	[Table, Prefix] = string:tokens(ResourceId, "-"),
	NewRate2 = ocs_rest:millionths_in(NewRate1),
	{NewDescription, NewRate2, _} = ocs_gtt:lookup_first(Table, Prefix).

post_policy_resource() ->
	[{userdata, [{doc,"Add policy in rest interface"}]}].

post_policy_resource(Config) ->
	ResName = ocs:generate_identity(),
	ServiceId = ocs:generate_identity(),
	Name = {"name", ResName},
	ResourceSpec = {"resourceSpecification",
			{struct, [{"id", "4"}, {"name", "PolicyTableRow"}]}},
	RelId = ocs:generate_identity(),
	RelHref = "/resourceInventoryManagement/v1/resource/" ++ RelId,
	Relationship = {"resourceRelationship",
			{array, [{struct, [{"resource", {struct, [{"id", RelId},
			{"href", RelHref}, {"name", "example"}]}},
			{"relationshipType", "contained"}]}]}},
	Char1 = {struct, [{"name", "name"}, {"value", ResName}]},
	ClassId = rand:uniform(10),
	MaxUL = 1000000000,
	MaxDL = 1000000000,
	Char2 = {struct, [{"name", "qosInformation"}, {"value",
			{struct, [{"qosClassIdentifier", ClassId},
			{"maxRequestedBandwidthUL", MaxUL},
			{"maxRequestedBandwidthDL", MaxDL}]}}]},
	Char3 = {struct, [{"name", "chargingKey"}, {"value", 1}]},
	Char4 = {struct, [{"name", "flowInformation"}, {"value",{array,
			[{struct, [{"flowDescription", "permit in ip from any to 10/8"},
					{"flowDirection", "down"}]},
			{struct, [{"flowDescription", "permit in ip from any to 10/8"},
					{"flowDirection", "up"}]}]}}]},
	Char5 = {struct, [{"name", "precedence"}, {"value", 1}]},
	Char6 = {struct, [{"name", "serviceId"}, {"value", ServiceId}]},
	Characteristics = {"resourceCharacteristic",
			{array, [Char1, Char2, Char3, Char4, Char5, Char6]}},
	JSON = {struct, [Name, ResourceSpec, Relationship, Characteristics]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request = {HostUrl ++ "/resourceInventoryManagement/v1/resource/",
			[Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/resourceInventoryManagement/v1/resource/" ++ ID, _}
			= httpd_util:split_path(URI),
	{ok, #resource{name = ResName, specification = S, related = [R],
			characteristic = ResChar}} = ocs:get_resource(ID),
	#specification_ref{id = "4", name = "PolicyTableRow"} = S,
	#resource_rel{id = RelId, href = RelHref,
			type = "contained", name = "example"} = R,
	#resource_char{value = ResName}
			= lists:keyfind("name", #resource_char.name, ResChar),
	#resource_char{value = #{"qosClassIdentifier" := ClassId,
			"maxRequestedBandwidthUL" := MaxUL,
			"maxRequestedBandwidthDL" := MaxDL}}
			= lists:keyfind("qosInformation", #resource_char.name, ResChar),
	#resource_char{value = 1}
			= lists:keyfind("chargingKey", #resource_char.name, ResChar),
	#resource_char{value = [#{"flowDescription" := Description,
			"flowDirection" := Direction} | _]}
			= lists:keyfind("flowInformation", #resource_char.name, ResChar),
	true = is_list(Description),
	true = is_integer(Direction),
	#resource_char{value = Precedence}
			= lists:keyfind("precedence", #resource_char.name, ResChar),
	true = is_integer(Precedence),
	#resource_char{value = ServiceId}
			= lists:keyfind("serviceId", #resource_char.name, ResChar).

query_policy_resource() ->
	[{userdata, [{doc, "Query policy entry in resource table"}]}].

query_policy_resource(Config) ->
	ok = ocs_gtt:new(tariff_table6, []),
	TariffTable = #resource{name = "tariff_table6", description = "Tariff Table",
			category = "Tariff", class_type = "LogicalResource",
			base_type = "Resource", specification = #specification_ref{id = "1",
					href = "/resourceCatalogManagement/v2/resourceSpecification/1",
					name = "TariffTable"}},
	{ok, #resource{}} = ocs:add_resource(TariffTable),
	PolicyTable = #resource{name = "PolicyTable", description = "Policy Table",
			category = "Policy", class_type = "LogicalResource",
			base_type = "Resource", specification = #specification_ref{id = "3",
					href = "/resourceCatalogManagement/v2/resourceSpecification/3",
					name = "PolicyTable"}},
	{ok, #resource{id = PolicyTableId1}} = ocs:add_resource(PolicyTable),
	PolicyRow1 = #resource{name = "PolicyRow1", description = "Policy Row",
			category = "Policy", class_type = "LogicalResource",
			base_type = "Resource", related = [#resource_rel{id = PolicyTableId1,
					href = "/resourceInventoryManagement/v1/resource/"
					++ PolicyTableId1, name = "PolicyTable1", type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v2/resourceSpecification/4",
					name = "PolicyTableRow"},
			characteristic = [#resource_char{name = "name", value = "example"},
					#resource_char{name = "qosInformation", value =
							#{"maxRequestedBandwidthDL" => 1000000000,
							"maxRequestedBandwidthUL" => 1000000000,
							"qosClassIdentifier" => 4}},
					#resource_char{name = "chargingKey", value = 1},
					#resource_char{name = "flowInformation", value =
							[#{"flowDirection" => 1, "flowDescription" =>
									"permit in ip from any to 10/8"},
							#{"flowDirection" => 2, "flowDescription" =>
									"permit in ip from any to 10/8"}]},
					#resource_char{name = "precedence", value = 1},
					#resource_char{name = "serviceId",
							value = ocs:generate_identity()}]},
	{ok, #resource{id = RowId1}} = ocs:add_resource(PolicyRow1),
	PolicyTableId2 = ocs:generate_identity(),
	PolicyRow2 = #resource{name = "PolicyRow2", description = "Policy Row",
			category = "Policy", class_type = "LogicalResource",
			base_type = "Resource", related = [#resource_rel{id = PolicyTableId2,
					href = "/resourceInventoryManagement/v1/resource/"
					++ PolicyTableId2, name = "PolicyTable2", type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v2/resourceSpecification/4",
					name = "PolicyTableRow"},
			characteristic = [#resource_char{name = "name", value = "example"},
					#resource_char{name = "chargingKey", value = 1},
					#resource_char{name = "flowInformation", value =
							[#{"flowDirection" => 1, "flowDescription" =>
									"permit in ip from any to 172.16/12"},
							#{"flowDirection" => 2, "flowDescription" =>
									"permit in ip from any to 172.16/12"}]},
					#resource_char{name = "precedence", value = 2}]},
	{ok, #resource{id = _RowId2}} = ocs:add_resource(PolicyRow2),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Query = "resourceSpecification.id=4" ++
		"&resourceRelationship.resource.name=PolicyTable1",
	Request = {HostUrl ++ "/resourceInventoryManagement/v1/resource/?" ++ Query,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{array, [{struct, Object}]} = mochijson:decode(ResponseBody),
	{_, RowId1} = lists:keyfind("id", 1, Object),
	{_, "/resourceInventoryManagement/v1/resource/" ++ RowId1}
			= lists:keyfind("href", 1, Object),
	{_, {struct, SpecList}} = lists:keyfind("resourceSpecification", 1, Object),
	{_, "4"} = lists:keyfind("id", 1, SpecList),
	{_, {array, [{struct, RelList}]}}
			= lists:keyfind("resourceRelationship", 1, Object),
	{_, {struct, ObjList}} = lists:keyfind("resource", 1, RelList),
	{_, "PolicyTable1"} = lists:keyfind("name", 1, ObjList).

delete_policy_table() ->
	[{userdata, [{doc,"Delete policy table resource"}]}].

delete_policy_table(Config) ->
	{TableId, TableName} = add_policy_table(),
	PolicyRowId1 = add_policy_row(TableId, TableName, 0),
	PolicyRowId2 = add_policy_row(TableId, TableName, 1),
	URI = "/resourceInventoryManagement/v1/resource/" ++ TableId,
	HostUrl = ?config(host_url, Config),
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers, []} = Result,
	{_, "0"} = lists:keyfind("content-length", 1, Headers),
	{error, not_found} = ocs:get_resource(PolicyRowId1),
	{error, not_found} = ocs:get_resource(PolicyRowId2),
	{error, not_found} = ocs:get_resource(TableId).

oauth_authentication()->
	[{userdata, [{doc, "Authenticate a JWT using oauth"}]}].

oauth_authentication(Config)->
	ID = "cornflakes",
	Locale = "es",
	{ok, _} = ocs:add_user(ID, "", Locale),
	ok = application:set_env(ocs, oauth_issuer, "joe"),
	ok = application:set_env(ocs, oauth_audience, "network-subscriber.sigscale-ocs"),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Header = {struct, [{"alg", "RS256"}, {"typ", "JWT"}]},
	Payload = {struct, [{"iss", "joe"}, {"exp", 1300819380}, {"email", "cornflakes"},
			{"aud", {array, ["network-subscriber.sigscale-ocs", "account"]}},
			{"preferred_username","flakes"}]},
	EncodedHeader = encode_base64url(lists:flatten(mochijson:encode(Header))),
	EncodedPayload = encode_base64url(lists:flatten(mochijson:encode(Payload))),
	Path = ?config(data_dir, Config),
	KeyPath = Path ++ "key.pem",
	{ok, PrivBin} = file:read_file(KeyPath),
	[RSAPrivEntry] = public_key:pem_decode(PrivBin),
	Key = public_key:pem_entry_decode(RSAPrivEntry),
	M = Key#'RSAPrivateKey'.modulus,
	E = Key#'RSAPrivateKey'.publicExponent,
	RSAPublicKey = #'RSAPublicKey'{modulus = M, publicExponent = E},
	PemEntry = public_key:pem_entry_encode('RSAPublicKey', RSAPublicKey),
	PemBin = public_key:pem_encode([PemEntry]),
	file:write_file(Path ++ "pub.pem", PemBin),
	Msg = list_to_binary(EncodedHeader ++ "." ++ EncodedPayload),
	Signature = public_key:sign(Msg, sha256, Key),
	EncodedSignature = encode_base64url(binary_to_list(Signature)),
	AuthKey = "Bearer " ++ EncodedHeader ++ "." ++ EncodedPayload ++ "." ++ EncodedSignature,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl, [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _}, _, _} = Result.

arbitrary_char_service() ->
	[{userdata, [{doc,"Add service with arbitrary characteristics"}]}].

arbitrary_char_service(Config) ->
	OfferId = ?config(product_id, Config),
	{ok, #product{}} = ocs:add_product(OfferId, []),
	ID = ocs:generate_identity(),
	Password = ocs:generate_password(),
	State = {"state", active},
	IsServiceEnabled = {"isServiceEnabled", true},
	Char1= {struct, [{"name", "acctSessionInterval"}, {"value", rand:uniform(500)}]},
	Char2 = {struct, [{"name", "sessionTimeout"}, {"value", rand:uniform(2500)}]},
	Char3 = {struct, [{"name", "serviceIdentity"}, {"value", ID}]},
	Char4 = {struct, [{"name", "servicePassword"}, {"value", Password}]},
	Char5 = {struct, [{"name", "multiSession"}, {"value", true}]},
	Char6 = {struct, [{"name", "foo"}, {"valueType", "number"}, {"value", 42}]},
	Char7 = {struct, [{"name", "radiusReserveOctets"},
			{"value", {struct, [{"unitOfMeasure", "bytes"},
			{"value", rand:uniform(100000)}]}}]},
	SortedChars = lists:sort([Char1, Char2, Char3, Char4, Char5, Char6, Char7]),
	Characteristics = {"serviceCharacteristic", {array, SortedChars}},
	JSON = {struct, [State, IsServiceEnabled, Characteristics]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request = {HostUrl ++ "/serviceInventoryManagement/v2/service",
			[Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _} = lists:keyfind("etag", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/serviceInventoryManagement/v2/service/" ++ ID, _} = httpd_util:split_path(URI),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, {array, Chars}} = lists:keyfind("serviceCharacteristic", 1, Object),
	F = fun({struct, [{"name", "foo"}, {"value", 42}]}) ->
				true;
			({struct, [{"value", 42}, {"name", "foo"}]}) ->
				true;
			(_) ->
				false
	end,
	lists:any(F, Chars).

post_role() ->
	[{userdata, [{doc, "POST to Role collection"}]}].

post_role(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	RequestBody = lists:flatten(mochijson:encode(party_role("Global_Pirates"))),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	PartyRole = mochijson:decode(ResponseBody),
	true = lists:all(fun is_role/1, [PartyRole]).

delete_role() ->
	[{userdata, [{doc,"Delete a role by id"}]}].

delete_role(Config) ->
	RequestBody = lists:flatten(mochijson:encode(party_role("Queen"))),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, _ResponseBody1} = Result1,
	{_, Href} = lists:keyfind("location", 1, Headers1),
	Request2 = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(delete, Request2, [], []),
	{{"HTTP/1.1", 204, _NoContent}, _Headers2, []} = Result2,
	{ok, {{"HTTP/1.1", 404, "Object Not Found"}, _Headers3, _ResponseBody3}}
			= httpc:request(get, Request2, [], []).

get_roles() ->
	[{userdata, [{doc, "Get the role collection."}]}].

get_roles(Config) ->
	RequestBody1 = lists:flatten(mochijson:encode(party_role("USA_Pirates"))),
	RequestBody2 = lists:flatten(mochijson:encode(party_role("CA_Pirates"))),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, Created}, _Headers1, _ResponseBody1} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{"HTTP/1.1", 201, Created}, _Headers2, _ResponseBody2} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers3, ResponseBody3} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers3),
	ContentLength = integer_to_list(length(ResponseBody3)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers3),
	{array, PartyRoles} = mochijson:decode(ResponseBody3),
	false = is_empty(PartyRoles),
	true = lists:all(fun is_role/1, PartyRoles).

get_role() ->
	[{userdata, [{doc, "Get a role."}]}].

get_role(Config) ->
	RequestBody = lists:flatten(mochijson:encode(party_role("SL_Pirates"))),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, _ResponseBody1} = Result1,
	{_, Href} = lists:keyfind("location", 1, Headers1),
	Request2 = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody2} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody2)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	PartyRole = mochijson:decode(ResponseBody2),
	true = lists:all(fun is_role/1, [PartyRole]).

post_hub_role() ->
	[{userdata, [{doc, "Register hub listener for role"}]}].

post_hub_role(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathRole ++ "hub/",
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\n"
			++ "\t\"callback\": \"" ++ Callback ++ "\",\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{struct, HubList} = mochijson:decode(ResponseBody),
	2 = length(HubList),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList).

delete_hub_role() ->
	[{userdata, [{doc, "Unregister hub listener for usage"}]}].

delete_hub_role(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathRole ++ "hub/",
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{struct, HubList} = mochijson:decode(ResponseBody),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

get_role_hubs() ->
	[{userdata, [{doc, "Get role hub listeners"}]}].

get_role_hubs(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "hub/",
	Callback1 = "http://in.listener1.com",
	Callback2 = "http://in.listener2.com",
	RequestBody1 = "{\"callback\":\"" ++ Callback1 ++ "\"}",
	RequestBody2 = "{\"callback\":\"" ++ Callback2 ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept,
			auth_header()], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, _, _} = Result1,
	Request2 = {CollectionUrl, [Accept,
			auth_header()], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{_, 201, _}, _, _} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{array, HubStructs} = mochijson:decode(ResponseBody),
	true = length(HubStructs) >= 2,
	F = fun({struct, HubList}) ->
			case lists:keyfind("href", 1, HubList) of
				{_, ?PathRole ++ "hub/" ++ _} ->
					true;
				_ ->
					false
			end
	end,
	true = lists:all(F, HubStructs).

get_role_hub() ->
	[{userdata, [{doc, "Get a role hub listener by id"}]}].

get_role_hub(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathRole ++ "hub/",
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = "{\"callback\":\"" ++ Callback ++ "\"}",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{_, 201, _}, Headers1, _} = Result1,
	{_, Location} = lists:keyfind("location", 1, Headers1),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	Request2 = {CollectionUrl ++ Id, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{struct, HubList} = mochijson:decode(ResponseBody),
	3 = length(HubList),
	{_, Callback} = lists:keyfind("callback", 1, HubList),
	{_, Id} = lists:keyfind("id", 1, HubList),
	Href = PathHub ++ Id,
	{_, Href} = lists:keyfind("href", 1, HubList).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

-spec notifycreatebucket(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_create_bucket test case.
notifycreatebucket(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_create_bucket ! Input.

-spec notifydeletebucket(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_delete_bucket test case.
notifydeletebucket(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_delete_bucket ! Input.

-spec notifyratingdeletedbucket(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_rating_deleted_bucket test case.
notifyratingdeletedbucket(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_rating_deleted_bucket ! Input.

-spec notifyaccumulatedbalancethreshold(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_accumulated_balance_threshold
%% test case.
notifyaccumulatedbalancethreshold(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_accumulated_balance_threshold ! Input.

-spec queryaccumulatedbalancenotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_accumulated_balance_notification
%% test case.
queryaccumulatedbalancenotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_accumulated_balance_notification ! Input.

-spec querybucketnotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_bucket_notification test case.
querybucketnotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_bucket_notification ! Input.

-spec notifyproductcharge(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_product_charge test case.
notifyproductcharge(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_product_charge ! Input.

-spec notifycreateproduct(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_create_product test case.
notifycreateproduct(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_create_product ! Input.

-spec notifydeleteproduct(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_delete_product test case.
notifydeleteproduct(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_delete_product ! Input.

-spec queryproductnotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_product_notification test case.
queryproductnotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_product_notification ! Input.

-spec notifycreateservice(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_create_service test case.
notifycreateservice(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_create_service ! Input.

-spec notifydeleteservice(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_delete_service test case.
notifydeleteservice(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_delete_service ! Input.

-spec queryservicenotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_service_notification test case.
queryservicenotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_service_notification ! Input.

-spec notifycreateoffer(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_create_offer test case.
notifycreateoffer(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_create_offer ! Input.

-spec notifydeleteoffer(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_delete_offer test case.
notifydeleteoffer(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_delete_offer ! Input.

-spec queryoffernotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_offer_notification test case.
queryoffernotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_offer_notification ! Input.

-spec notifyinsertgtt(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_insert_gtt test case.
notifyinsertgtt(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_insert_gtt ! Input.

-spec notifydeletegtt(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_delete_gtt test case.
notifydeletegtt(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_delete_gtt ! Input.

-spec querygttnotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_gtt_notification test case.
querygttnotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_gtt_notification ! Input.

-spec notifyaddresource(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_add_resource test case.
notifyaddresource(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_add_resource ! Input.

-spec notifydeleteresource(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_delete_resource test case.
notifydeleteresource(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_delete_resource ! Input.

-spec queryresourcenotification(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for query_resource_notification test case.
queryresourcenotification(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	query_resource_notification ! Input.

-spec notifydiameteracctlog(SessionID :: term(), Env :: list(),
		Input :: string()) -> any().
%% @doc Notification callback for notify_diameter_acct_log test case.
notifydiameteracctlog(SessionID, _Env, Input) ->
	mod_esi:deliver(SessionID, "status: 201 Created\r\n\r\n"),
	notify_diameter_acct_log ! Input.

product_offer() ->
	CatalogHref = "/productCatalogManagement/v2",
	ProdName = {"name", ocs:generate_password()},
	ProdDescirption = {"description", ocs:generate_password()},
	IsBundle = {"isBundle", false},
	IsCustomerVisible = {"isCustomerVisible", true},
	Status = {"lifecycleStatus", "Active"},
	StartTime = {"startDateTime", ocs_rest:iso8601(erlang:system_time(millisecond))},
	EndTime = {"endDateTime", ocs_rest:iso8601(erlang:system_time(millisecond)  + 2678400000)},
	ValidFor = {"validFor", {struct, [StartTime, EndTime]}},
	ProdSpecID = {"id", "1"},
	ProdSpecHref = {"href", CatalogHref ++ "/productSpecification/1"},
	ProdSpec = {"productSpecification", {struct, [ProdSpecID, ProdSpecHref]}},
	POPName1 = {"name", ocs:generate_password()},
	POPDescription1 = {"description", ocs:generate_password()},
	POPStartDateTime1 = {"startDateTime", ocs_rest:iso8601(erlang:system_time(millisecond))},
	POPEndDateTime1 = {"endDateTime", ocs_rest:iso8601(erlang:system_time(millisecond)  + 2678400000)},
	POPValidFor1 = {"validFor", {struct, [POPStartDateTime1, POPEndDateTime1]}},
	POPPriceType1 = {"priceType", "recurring"},
	POPPriceTaxInclude1 = {"taxIncludedAmount", integer_to_list(rand:uniform(10000))},
	POPPriceCurrency1 = {"currencyCode", "USD"},
	POPPrice1 = {"price", {struct, [POPPriceTaxInclude1, POPPriceCurrency1]}},
	POPRecChargPeriod1 = {"recurringChargePeriod", "monthly"},
	ProdAlterName = {"name", "allowance"},
	ProdAlterDescription = {"description", ocs:generate_password()},
	ProdAlterValidFor = {"validFor", {struct, [POPStartDateTime1]}},
	ProdAlterPriceType = {"priceType", "usage"},
	ProdAlterUOMeasure = {"unitOfMeasure", "100g"},
	ProdAlterAmount = {"taxIncludedAmount", "0"},
	POPPAlterCurrency = {"currencyCode", "USD"},
	ProdAlterPrice = {"price", {struct, [ProdAlterAmount, POPPAlterCurrency]}},
	POPAlteration = {"productOfferPriceAlteration", {struct, [ProdAlterName, ProdAlterDescription,
		ProdAlterValidFor, ProdAlterPriceType, ProdAlterUOMeasure, ProdAlterPrice]}},
	ProdOfferPrice1 = {struct, [POPName1, POPDescription1, POPValidFor1,
			POPPriceType1, POPPrice1, POPRecChargPeriod1, POPAlteration]},
	POPName2 = {"name", "usage"},
	POPDescription2 = {"description", ocs:generate_password()},
	POPStratDateTime2 = {"startDateTime", ocs_rest:iso8601(erlang:system_time(millisecond))},
	POPEndDateTime2 = {"endDateTime", ocs_rest:iso8601(erlang:system_time(millisecond)  + 2678400000)},
	POPValidFor2 = {"validFor", {struct, [POPStratDateTime2, POPEndDateTime2]}},
	POPPriceType2 = {"priceType", "usage"},
	POPUOMeasure2 = {"unitOfMeasure", "1g"},
	POPPriceTaxInclude2 = {"taxIncludedAmount",
			integer_to_list(rand:uniform(1000)) ++ "." ++ integer_to_list(rand:uniform(999999))},
	POPPriceCurrency2 = {"currencyCode", "USD"},
	POPPrice2 = {"price", {struct, [POPPriceTaxInclude2, POPPriceCurrency2]}},
	ProdOfferPrice2 = {struct, [POPName2, POPDescription2, POPValidFor2, POPPriceType2,
			POPPrice2, POPUOMeasure2]},
	ProdOfferPrice = {"productOfferingPrice", {array, [ProdOfferPrice1, ProdOfferPrice2]}},
	[ProdName, ProdDescirption, IsBundle, IsCustomerVisible, ValidFor, ProdSpec, Status, ProdOfferPrice].

patch_request(SslSock, Port, ContentType, Etag, AuthKey, ProdID, ReqBody) when is_list(ReqBody) ->
	BinBody = list_to_binary(ReqBody),
	patch_request(SslSock, Port, ContentType, Etag, AuthKey, ProdID, BinBody);
patch_request(SslSock, Port, ContentType, Etag, AuthKey, ProdID, ReqBody) ->
	Timeout = 1500,
	Length = size(ReqBody),
	CatalogHref = "/productCatalogManagement/v2",
	PatchURI = CatalogHref ++ "/productOffering/" ++ ProdID,
	Request =
			["PATCH ", PatchURI, " HTTP/1.1",$\r,$\n,
			"Content-Type:"++ ContentType, $\r,$\n,
			"Accept:application/json",$\r,$\n,
			"Authorization:"++ AuthKey,$\r,$\n,
			"Host:localhost:" ++ integer_to_list(Port),$\r,$\n,
			"Content-Length:" ++ integer_to_list(Length),$\r,$\n,
			"If-match:" ++ Etag,$\r,$\n,
			$\r,$\n,
			ReqBody],
	ok = ssl:send(SslSock, Request),
	F = fun(_F, _Sock, {error, timeout}, Acc) ->
					lists:reverse(Acc);
			(F, Sock, {ok, Bin}, Acc) ->
					F(F, Sock, ssl:recv(Sock, 0, Timeout), [Bin | Acc])
	end,
	RecvBuf = F(F, SslSock, ssl:recv(SslSock, 0, Timeout), []),
	PatchResponse = list_to_binary(RecvBuf),
	[Headers, ResponseBody] = binary:split(PatchResponse, <<$\r,$\n,$\r,$\n>>),
	{Headers, ResponseBody}.

ssl_socket_open(IP, Port) ->
	{ok, SslSock} = ssl:connect(IP, Port,
		[binary, {active, false}], infinity),
	SslSock.

ssl_socket_close(SslSock) ->
	ok = ssl:close(SslSock).

product_name(ProdID) ->
	Op = {"op", "replace"},
	Path = {"path", "/name"},
	Value = {"value", ProdID},
	{struct, [Op, Path, Value]}.

product_description() ->
	Description = ocs:generate_password(),
	Op = {"op", "replace"},
	Path = {"path", "/description"},
	Value = {"value", Description},
	{struct, [Op, Path, Value]}.

product_status() ->
	Status = "In Design", 
	Op = {"op", "replace"},
	Path = {"path", "/lifecycleStatus"},
	Value = {"value", Status},
	{struct, [Op, Path, Value]}.

prod_price_name() ->
	Name = ocs:generate_password(),
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/1/name"},
	Value = {"value", Name},
	{struct, [Op, Path, Value]}.

prod_price_description() ->
	Description = ocs:generate_password(),
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/1/description"},
	Value = {"value", Description},
	{struct, [Op, Path, Value]}.

prod_price_rc_period() ->
	Period = "yearly",
	Op = {"op", "add"},
	Path = {"path", "/productOfferingPrice/1/recurringChargePeriod"},
	Value = {"value", Period},
	{struct, [Op, Path, Value]}.

prod_price_ufm() ->
	UFM = "10000b",
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/1/unitOfMeasure"},
	Value = {"value", UFM},
	{struct, [Op, Path, Value]}.

prod_price_type() ->
	PT = "recurring",
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/1/priceType"},
	Value = {"value", PT},
	{struct, [Op, Path, Value]}.

pp_alter_name() ->
	Name = ocs:generate_password(),
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/0/productOfferPriceAlteration/name"},
	Value = {"value", Name},
	{struct, [Op, Path, Value]}.

pp_alter_description() ->
	Description = ocs:generate_password(),
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/0/productOfferPriceAlteration/description"},
	Value = {"value", Description},
	{struct, [Op, Path, Value]}.

pp_alter_type() ->
	PT = "recurring",
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/0/productOfferPriceAlteration/priceType"},
	Value = {"value", PT},
	{struct, [Op, Path, Value]}.

pp_alter_ufm() ->
	UFM = "1000b",
	Op = {"op", "replace"},
	Path = {"path", "/productOfferingPrice/0/productOfferPriceAlteration/unitOfMeasure"},
	Value = {"value", UFM},
	{struct, [Op, Path, Value]}.

%% @hidden
is_etag_valid(Etag) ->
	[X1, X2] = string:tokens(Etag, "-"),
	true = is_integer(list_to_integer(X1)),
	true = is_integer(list_to_integer(X2)).

%% @hidden
basic_auth() ->
	RestUser = ct:get_config({rest, user}),
	RestPass = ct:get_config({rest, password}),
	EncodeKey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	"Basic " ++ EncodeKey.

%% @hidden
auth_header() ->
	{"authorization", basic_auth()}.

%% @hidden
price(Type, undefined, undefined, Amount)
		when ((Type == one_time) or (Type == recurring)),
		is_integer(Amount) ->
	#price{name = ocs:generate_identity(),
			type = Type, amount = Amount};
price(usage, Units, Size, Amount)
		when ((Units == octets) or (Units == seconds) or (Units == messages)),
		is_integer(Size), Size > 0,
		is_integer(Amount), Amount > 0 ->
	#price{name = ocs:generate_identity(),
			type = usage, units = Units, size = Size, amount = Amount};
price(tariff, Units, Size, undefined)
		when ((Units == octets) or (Units == seconds) or (Units == messages)),
		is_integer(Size), Size > 0 ->
	#price{name = ocs:generate_identity(),
			type = tariff, units = Units, size = Size}.

%% @hidden
b(Units, RA) ->
	#bucket{units = Units, remain_amount = RA,
		start_date = erlang:system_time(millisecond),
		end_date = erlang:system_time(millisecond) + 2592000000}.

%% @hidden
offer_add(Prices, Spec) when is_integer(Spec) ->
	offer_add(Prices, integer_to_list(Spec));
offer_add(Prices, Spec) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId.

%% @hidden
product_add(OfferId) ->
	product_add(OfferId, []).
product_add(OfferId, Chars) ->
	{ok, #product{id = ProdRef}} = ocs:add_product(OfferId, [], Chars),
	ProdRef.

%% @hidden
service_add(ProdRef) ->
	ServiceId = ocs:generate_identity(),
	{ok, _Service1} =
			ocs:add_service(ServiceId, ocs:generate_password(),
			ProdRef, []),
	ServiceId.

%% @hidden
bucket_add(ProdRef, Bucket) ->
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.

%% @hidden
binary_to_hex(B) ->
	binary_to_hex(B, []).
%% @hidden
binary_to_hex(<<N:4, Rest/bits>>, Acc) when N >= 10 ->
	binary_to_hex(Rest, [N - 10 + $a | Acc]);

binary_to_hex(<<N:4, Rest/bits>>, Acc) ->
	binary_to_hex(Rest, [N + $0 | Acc]);
binary_to_hex(<<>>, Acc) ->
	lists:reverse(Acc).

-spec encode_base64url(Value) -> EncodedValue 
	when
		Value :: string(),
		EncodedValue :: list().
%% @doc Encode a value using base64url encoding.
encode_base64url(Value)
		when is_list(Value) ->
	EncodedValue = base64:encode_to_string(Value),
	StrippedValue = string:strip(EncodedValue, both, $=),
	sub_chars_en(StrippedValue, []).

%% @hidden 
sub_chars_en([$/ | T], Acc) ->
	sub_chars_en(T, [$_ | Acc]);
sub_chars_en([$+ | T], Acc) ->
	sub_chars_en(T, [$- | Acc]);
sub_chars_en([H | T], Acc) ->
	sub_chars_en(T, [H | Acc]);
sub_chars_en([], Acc) ->
	lists:reverse(Acc).

set_inet_mod() ->
	{ok, EnvObj} = application:get_env(inets, services),
	[{httpd, Services}] = EnvObj,
	NewModTuple = replace_mod(lists:keyfind(modules, 1, Services), []),
	NewServices = lists:keyreplace(modules, 1, Services, NewModTuple),
	ok = application:set_env(inets, services, [{httpd, NewServices}]).
	
replace_mod({modules, Mods}, Acc) ->
	replace_mod1(Mods, Acc).
%% @hidden
replace_mod1([mod_auth | T], Acc) ->
	replace_mod1(T, [mod_oauth | Acc]);
replace_mod1([mod_oauth | T], Acc) ->
	replace_mod1(T, [mod_auth | Acc]);
replace_mod1([H | T], Acc) ->
	replace_mod1(T, [H | Acc]);
replace_mod1([], Acc) ->
	{modules, lists:reverse(Acc)}.

%% @hidden
add_offer(Prices, Spec) when is_integer(Spec) ->
	add_offer(Prices, integer_to_list(Spec));
add_offer(Prices, Spec) ->
	Offer = #offer{name = ocs:generate_identity(),
	price = Prices, specification = Spec},
	{ok, #offer{name = OfferId}} = ocs:add_offer(Offer),
	OfferId.

%% @hidden
add_bucket(ProdRef, Units, RA) ->
	Bucket = #bucket{units = Units, remain_amount = RA,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2592000000},
	{ok, _, #bucket{id = BId}} = ocs:add_bucket(ProdRef, Bucket),
	BId.

%% @hidden
add_resource("1", Description, Category, TableName) ->
	Schema = "/resourceInventoryManagement/v1/schema/"
			"resourceInventoryManagement#/definitions/resource",
	Resource = #resource{name = TableName, class_type = "LogicalResource",
			base_type = "Resource", description = Description, category = Category,
			state = "Active", schema = Schema,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2678400000,
			specification = #specification_ref{id = "1", name = "TariffTable",
					href = "/resourceCatalogManagement/v2/resourceSpecification/1"}},
	ocs:add_resource(Resource);
add_resource("2", Description, Category, TableName) ->
	Schema = "/resourceInventoryManagement/v1/schema/"
			"resourceInventoryManagement#/definitions/resource",
	ResourceRelID = ocs:generate_identity(),
	Resource = #resource{class_type = "LogicalResource", base_type = "Resource",
			schema = Schema, description = Description, category = Category,
			start_date = erlang:system_time(millisecond),
			end_date = erlang:system_time(millisecond) + 2678400000,
			related = [#resource_rel{id = ResourceRelID,
					href = "/resourceInventoryManagement/v1/resource/"
					++ ResourceRelID, referred_type = "contained", name = TableName}],
			specification = #specification_ref{id = "2", name = "TariffTableRow",
					href = "/resourceCatalogManagement/v2/resourceSpecification/2"},
			characteristic  = [#resource_char{name = "prefix", value = "125"},
					#resource_char{name = "description", value = "test"},
					#resource_char{name = "rate", value = 250}]},
	ocs:add_resource(Resource).

%% @hidden
fill_resource_char(N) ->
	fill_resource_char(N, []).
fill_resource_char(0, Acc) ->
	Acc;
fill_resource_char(N, Acc) ->
	Characteristic = #resource_char{name = random_string(10),
			class_type = random_string(5),
			schema = random_string(25), value = random_string(15)},
	fill_resource_char(N - 1, [Characteristic | Acc]).

%% @hidden
fill_related(N) ->
	fill_related(N, []).
fill_related(0, Acc) ->
	Acc;
fill_related(N, Acc) ->
	Id = random_string(10),
	Href = "/resourceInventoryManagement/v1/resourceRelationship/" ++ Id,
	Related = #resource_rel{id = Id, href = Href,
			type = "contained", name = "example-" ++ Id},
	fill_related(N - 1, [Related | Acc]).

%% @hidden
random_string(Length) ->
	Charset = lists:seq($a, $z),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	random_string(Random, Charset, NumChars,[]).
random_string(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	random_string(Rest, Charset, NumChars, NewAcc);
random_string(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

%% @hidden
resource_inventory() ->
	ok = ocs_gtt:new(tariff_table5, []),
	Name = {"name", "Tariff"},
	Description = {"description", "tariff resource"},
	Category = {"category", "tariff"},
	ClassType = {"@type", "LogicalResource"},
	Schema = {"@schemaLocation", "/resourceInventoryManagement/v1/schema/"
			"resourceInventoryManagement#/definitions/resource"},
	BaseType = {"@baseType", "Resource"},
	Version = {"version", random_string(3)},
	Status = {"lifecycleStatus", "Active"},
	StartTime = {"startDateTime",
			ocs_rest:iso8601(erlang:system_time(millisecond))},
	EndTime = {"endDateTime",
			ocs_rest:iso8601(erlang:system_time(millisecond) + 2678400000)},
	ValidFor = {"validFor", {struct, [StartTime, EndTime]}},
	ResSpecID = {"id", "2"},
	ResSpecName = {"name", "TariffTableRow"},
	ResSpecHref = {"href",
			"/resourceCatalogManagement/v2/resourceSpecification/2"},
	ResSpec = {"resourceSpecification",
			{struct, [ResSpecID, ResSpecName, ResSpecHref]}},
	ResRelId = {"id", RelId = random_string(5)},
	ResRelName = {"name", "tariff_table5"},
	ResRelType = {"relationshipType", "contained"},
	ResRelHref = {"href", "/resourceInventoryManagement/v1/resource/" ++ RelId},
	ResRel = {struct, [{resource, {struct, [ResRelId, ResRelName, ResRelHref]}},
			ResRelType]},
	ResourceRelationship = {"resourceRelationship", {array, [ResRel]}},
	ResChar1 = {struct, [{"name", "prefix"}, {"value", "125"}]},
	ResChar2 = {struct, [{"name", "description"}, {"value", "testing"}]},
	ResChar3 = {struct, [{"name", "rate"}, {"value", 250}]},
	ResourceCharacteristics = {"resourceCharacteristic",
			{array, [ResChar1, ResChar2, ResChar3]}},
	[Name, Description, Category, ClassType, Schema, BaseType, Version, Status,
			ValidFor, ResSpec, ResourceRelationship, ResourceCharacteristics].

-spec add_policy_table() -> {ResourceId, ResourceName}
	when
		ResourceId :: string(),
		ResourceName :: string().
add_policy_table() ->
	Name = "examplePolicyTable",
	PolicyTable = #resource{name = Name, description = "Policy Table",
			category = "Policy", class_type = "LogicalResource",
			base_type = "Resource", specification = #specification_ref{id = "3",
					href = "/resourceCatalogManagement/v2/resourceSpecification/3",
					name = "PolicyTable"}},
	{ok, #resource{id = PolicyTableId}} = ocs:add_resource(PolicyTable),
	{PolicyTableId, Name}.

-spec add_policy_row(TableId, TableName, Cardinality) -> ResourceId
	when
		TableId :: string(),
		TableName :: string(),
		Cardinality :: string(),
		ResourceId :: string().
%% @doc Encode a value using base64url encoding.
add_policy_row(TableId, TableName, 0) ->
	PolicyRow = #resource{name = "PolicyRow1", description = "Policy Row",
			category = "Policy", class_type = "LogicalResource",
			base_type = "Resource", related = [#resource_rel{id = TableId,
					href = "/resourceInventoryManagement/v1/resource/"
					++ TableId, name = TableName, type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v2/resourceSpecification/4",
					name = "PolicyTableRow"},
			characteristic = [#resource_char{name = "name", value = "example"},
					#resource_char{name = "qosInformation", value =
							#{"maxRequestedBandwidthDL" => 1000000000,
							"maxRequestedBandwidthUL" => 1000000000,
							"qosClassIdentifier" => 4}},
					#resource_char{name = "chargingKey", value = 1},
					#resource_char{name = "flowInformation", value =
							[#{"flowDirection" => 1, "flowDescription" =>
									"permit in ip from any to 10/8"},
							#{"flowDirection" => 2, "flowDescription" =>
									"permit in ip from any to 10/8"}]},
					#resource_char{name = "precedence", value = 1},
					#resource_char{name = "serviceId",
							value = ocs:generate_identity()}]},
	{ok, #resource{id = RowId}} = ocs:add_resource(PolicyRow),
	RowId;
add_policy_row(TableId, TableName, 1) ->
	PolicyRow = #resource{name = "PolicyRow2", description = "Policy Row",
			category = "Policy", class_type = "LogicalResource",
			base_type = "Resource", related = [#resource_rel{id = TableId,
					href = "/resourceInventoryManagement/v1/resource/" ++ TableId,
					name = TableName, type = "contained"}],
			specification = #specification_ref{id = "4",
					href = "/resourceCatalogManagement/v2/resourceSpecification/4",
					name = "PolicyTableRow"},
			characteristic = [#resource_char{name = "name", value = "example"},
					#resource_char{name = "chargingKey", value = 1},
					#resource_char{name = "flowInformation", value =
							[#{"flowDirection" => 1, "flowDescription" =>
									"permit in ip from any to 172.16/12"},
							#{"flowDirection" => 2, "flowDescription" =>
									"permit in ip from any to 172.16/12"}]},
					#resource_char{name = "precedence", value = 2}]},
	{ok, #resource{id = RowId}} = ocs:add_resource(PolicyRow),
	RowId.

%% @hidden
party_role(RoleName) ->
	RoleType = "PartyRole",
	{struct, [{"@type", RoleType},
		{"name", RoleName}]}.

%% @hidden
is_role({struct, RoleObj}) when length(RoleObj) == 4 ->
	F = fun({"id", Name}) when is_list(Name) ->
				true;
			({"href", Href}) when is_list(Href) ->
				true;
			({"name", Name}) when is_list(Name) ->
				true;
			({"@type", RoleType}) when is_list(RoleType) ->
				true;
			(_) ->
				false
	end,
	lists:all(F, RoleObj);
is_role(_) ->
	false.

%% @hidden
is_empty([]) ->
	true;
is_empty(_) ->
	false.
