%%% ocs_rest_api_SUITE.erl
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
%%%  @doc Test suite for REST API
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_api_SUITE).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

%% support deprecated_time_unit()
-define(SECOND, seconds).
%-define(SECOND, second).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for REST API in OCS"}]},
	{timetrap, {minutes, 1}},
	{require, rest_user}, {default_config, rest_user, "bss"},
	{require, rest_pass}, {default_config, rest_pass, "nfc9xgp32xha"},
	{require, rest_group}, {default_config, rest_group, "all"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ocs_test_lib:initialize_db(),
	ok = ocs_test_lib:start(),
	{ok, Services} = application:get_env(inets, services),
	Fport = fun(F, [{httpd, L} | T]) ->
				case lists:keyfind(server_name, 1, L) of
					{_, "rest"} ->
						H1 = lists:keyfind(bind_address, 1, L),
						P1 = lists:keyfind(port, 1, L),
						{H1, P1};
					_ ->
						F(F, T)
				end;
			(F, [_ | T]) ->
				F(F, T)
	end,
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	RestGroup = ct:get_config(rest_group),
	{Host, Port} = case Fport(Fport, Services) of
		{{_, H2}, {_, P2}} when H2 == "localhost"; H2 == {127,0,0,1} ->
			true = mod_auth:add_user(RestUser, RestPass, [], {127,0,0,1}, P2, "/"),
			true = mod_auth:add_group_member(RestGroup, RestUser, {127,0,0,1}, P2, "/"),
			{"localhost", P2};
		{{_, H2}, {_, P2}} ->
			true = mod_auth:add_user(RestUser, RestPass, [], H2, P2, "/"),
			true = mod_auth:add_group_member(RestGroup, RestUser, H2, P2, "/"),
			case H2 of
				H2 when is_tuple(H2) ->
					{inet:ntoa(H2), P2};
				H2 when is_list(H2) ->
					{H2, P2}
			end;
		{false, {_, P2}} ->
			true = mod_auth:add_user(RestUser, RestPass, [], P2, "/"),
			true = mod_auth:add_group_member(RestGroup, RestUser, P2, "/"),
			{"localhost", P2}
	end,
	Config1 = [{port, Port} | Config],
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
init_per_testcase(_TestCase, Config) ->
	{ok, [{auth, AuthInstance}, {acct, _AcctInstance}]} = application:get_env(ocs, radius),
	[{IP, _Port, _}] = AuthInstance,
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, IP}, binary]),
	[{socket, Socket} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	ok =  gen_udp:close(Socket).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[authenticate_user_request, unauthenticate_user_request,
	authenticate_subscriber_request, unauthenticate_subscriber_request,
	authenticate_client_request, unauthenticate_client_request,
	add_subscriber, add_subscriber_without_password, get_subscriber,
	get_subscriber_not_found, retrieve_all_subscriber, delete_subscriber,
	add_client, add_client_without_password, get_client, get_client_id,
	get_client_bogus, get_client_notfound, get_all_clients, delete_client,
	get_usagespecs, get_usagespecs_query, get_usagespec, get_auth_usage,
	get_auth_usage_id, get_auth_usage_filter, get_acct_usage,
	get_acct_usage_id, get_acct_usage_filter, get_ipdr_usage,
	top_up_subscriber_balance, get_subscriber_balance, add_user,
	get_user, delete_user, simultaneous_updates_on_user_faliure,
	simultaneous_updates_on_subscriber_faliure,
	simultaneous_updates_on_client_faliure].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------
authenticate_user_request() ->
	[{userdata, [{doc, "Authorized user request to the server"}]}].

authenticate_user_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/usageManagement/v1/usage", [Accept, Authentication]},
	{ok, _Result} = httpc:request(get, Request, [], []).

unauthenticate_user_request() ->
	[{userdata, [{doc, "Authorized user request to the server"}]}].

unauthenticate_user_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = "Polymer",
	RestPass = "Interest",
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/usageManagement/v1/usage", [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 401, _}, _, _} = Result.

authenticate_subscriber_request() ->
	[{userdata, [{doc, "Authorized subscriber request to the server"}]}].

authenticate_subscriber_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication]},
	{ok, _Result} = httpc:request(get, Request, [], []).

unauthenticate_subscriber_request() ->
	[{userdata, [{doc, "Unauthorized subscriber request to the server"}]}].

unauthenticate_subscriber_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = "Love",
	RestPass = "Like",
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 401, _}, _, _} = Result.

authenticate_client_request() ->
	[{userdata, [{doc, "Authorized client request to the server"}]}].

authenticate_client_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/client", [Accept, Authentication]},
	{ok, _Result} = httpc:request(get, Request, [], []).

unauthenticate_client_request() ->
	[{userdata, [{doc, "Unauthorized subscriber request to the server"}]}].

unauthenticate_client_request(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = "Love",
	RestPass = "Like",
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/client", [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 401, _}, _, _} = Result.

add_subscriber() ->
	[{userdata, [{doc,"Add subscriber in rest interface"}]}].

add_subscriber(Config) ->
	ContentType = "application/json",
	ID = "eacfd73ae10a",
	Password = "ksc8c244npqc",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"value", 1000000}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"value", 64000}]},
	SessionTimeout = {struct, [{"name", "sessionTimeout"}, {"value", 3600}]},
	Interval = {struct, [{"name", "acctInterimInterval"}, {"value", 300}]},
	Class = {struct, [{"name", "class"}, {"value", "skiorgs"}]},
	SortedAttributes = lists:sort([AsendDataRate, AsendXmitRate, SessionTimeout, Interval, Class]),
	AttributeArray = {array, SortedAttributes},
	Balance = 100,
	Enable = true,
	JSON1 = {struct, [{"id", ID}, {"password", Password},
	{"attributes", AttributeArray}, {"balance", Balance}, {"enabled", Enable}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/ocs/v1/subscriber/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{"password", Password} = lists:keyfind("password", 1, Object),
	{_, {array, Attributes}} = lists:keyfind("attributes", 1, Object),
	ExtraAttributes = Attributes -- SortedAttributes,
	SortedAttributes = lists:sort(Attributes -- ExtraAttributes),
	{"balance", Balance} = lists:keyfind("balance", 1, Object),
	{"enabled", Enable} = lists:keyfind("enabled", 1, Object).

add_subscriber_without_password() ->
	[{userdata, [{doc,"Add subscriber with generated password"}]}].

add_subscriber_without_password(Config) ->
	ContentType = "application/json",
	JSON1 = {struct, [{"id", "beebdeedfeef"}, {"balance", 100000}, {"enabled", true}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, _Headers, ResponseBody} = Result,
	{struct, Object} = mochijson:decode(ResponseBody),
	{"password", Password} = lists:keyfind("password", 1, Object),
	12 = length(Password).

get_subscriber() ->
	[{userdata, [{doc,"get subscriber in rest interface"}]}].

get_subscriber(Config) ->
	ContentType = "application/json",
	AcceptValue = "application/json",
	ID = "eacfd73ae10a",
	Password = "ksc8c244npqc",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"value", 1000000}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"value", 64000}]},
	SessionTimeout = {struct, [{"name", "sessionTimeout"}, {"value", 10864}]},
	Interval = {struct, [{"name", "acctInterimInterval"}, {"value", 300}]},
	Class = {struct, [{"name", "class"}, {"value", "skiorgs"}]},
	SortedAttributes = lists:sort([AsendDataRate, AsendXmitRate, SessionTimeout, Interval, Class]),
	AttributeArray = {array, SortedAttributes},
	Balance = 100,
	Enable = true,
	JSON1 = {struct, [{"id", ID}, {"password", Password},
	{"attributes", AttributeArray}, {"balance", Balance}, {"enabled", Enable}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	{URI2, _} = httpd_util:split_path(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication ]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers1),
	ContentLength = integer_to_list(length(Body1)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI2} = lists:keyfind("href", 1, Object),
	{"password", Password} = lists:keyfind("password", 1, Object),
	{_, {array, Attributes}} = lists:keyfind("attributes", 1, Object),
	ExtraAttributes = Attributes -- SortedAttributes,
	SortedAttributes = lists:sort(Attributes -- ExtraAttributes),
	{"balance", Balance} = lists:keyfind("balance", 1, Object),
	{"enabled", Enable} = lists:keyfind("enabled", 1, Object).

get_subscriber_not_found() ->
	[{userdata, [{doc, "get subscriber notfound in rest interface"}]}].

get_subscriber_not_found(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Username = ct:get_config(rest_user),
	Password = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(Username ++ ":", Password)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	ID = "beefbeefcafe",
	Request = {HostUrl ++ "/ocs/v1/subscriber/" ++ ID, [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 404, _NotFound}, _Headers, _Body} = Result.

retrieve_all_subscriber() ->
	[{userdata, [{doc,"get subscriber in rest interface"}]}].

retrieve_all_subscriber(Config) ->
	ContentType = "application/json",
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	ID = "5557615036fd",
	Password = "2h7csggw35aa",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"value", 1000000}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"value", 64000}]},
	SessionTimeout = {struct, [{"name", "sessionTimeout"}, {"value", 10864}]},
	Interval = {struct, [{"name", "acctInterimInterval"}, {"value", 300}]},
	Class = {struct, [{"name", "class"}, {"value", "skiorgs"}]},
	SortedAttributes = lists:sort([AsendDataRate, AsendXmitRate, SessionTimeout, Interval, Class]),
	AttributeArray = {array, SortedAttributes},
	Balance = 100,
	Enable = true,
	JSON1 = {struct, [{"id", ID}, {"password", Password},
	{"attributes", AttributeArray}, {"balance", Balance}, {"enabled", Enable}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	Request2 = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers1),
	ContentLength = integer_to_list(length(Body1)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers1),
	{array, Subscribers} = mochijson:decode(Body1),
	Pred = fun({struct, Params}) ->
		case lists:keyfind("id", 1, Params) of
			{_, ID} ->
				true;
			{_, _ID} ->
				false
		end
	end,
	[{struct, Subscriber}] = lists:filter(Pred, Subscribers),
	{_, URI1} = lists:keyfind("href", 1, Subscriber),
	{"password", Password} = lists:keyfind("password", 1, Subscriber),
	{_, {array, Attributes}} = lists:keyfind("attributes", 1, Subscriber),
	ExtraAttributes = Attributes -- SortedAttributes,
	SortedAttributes = lists:sort(Attributes -- ExtraAttributes),
	{"balance", Balance} = lists:keyfind("balance", 1, Subscriber),
	{"enabled", Enable} = lists:keyfind("enabled", 1, Subscriber).

delete_subscriber() ->
	[{userdata, [{doc,"Delete subscriber in rest interface"}]}].

delete_subscriber(Config) ->
	ContentType = "application/json",
	ID = "eacfd73ae11d",
	Password = "ksc8c333npqc",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"value", 1000000}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"value", 64000}]},
	SessionTimeout = {struct, [{"name", "sessionTimeout"}, {"value", 10864}]},
	Interval = {struct, [{"name", "acctInterimInterval"}, {"value", 300}]},
	Class = {struct, [{"name", "class"}, {"value", "skiorgs"}]},
	SortedAttributes = lists:sort([AsendDataRate, AsendXmitRate, SessionTimeout, Interval, Class]),
	AttributeArray = {array, SortedAttributes},
	Balance = 100,
	Enable = true,
	JSON1 = {struct, [{"id", ID}, {"password", Password},
	{"attributes", AttributeArray}, {"balance", Balance}, {"enabled", Enable}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	{URI2, _} = httpd_util:split_path(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication], ContentType, []},
	{ok, Result1} = httpc:request(delete, Request2, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers1, []} = Result1,
	{_, "0"} = lists:keyfind("content-length", 1, Headers1).

add_user() ->
	[{userdata, [{doc,"Add user in rest interface"}]}].

add_user(Config) ->
	ContentType = "application/json",
	ID = "King",
	Username = "King",
	Password = "King",
	Locale = "en",
	Port = ?config(port, Config),
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, _AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	PasswordAttr = {struct, [{"name", "password"}, {"value", Password}]},
	LocaleAttr = {struct, [{"name", "locale"}, {"value", Locale}]},
	UsernameAttr = {struct, [{"name", "username"}, {"value", Username}]},
	CharArray = {array, [UsernameAttr, PasswordAttr, LocaleAttr]},
	JSON = {struct, [{"id", ID}, {"characteristic", CharArray}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/partyManagement/v1/individual", [Accept, Authentication], ContentType, RequestBody},
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
	{ok, #httpd_user{}} = mod_auth:get_user(ID, Address, Port, Directory).

get_user() ->
	[{userdata, [{doc,"get user in rest interface"}]}].

get_user(Config) ->
	ID = "King1",
	Password = "King1",
	Locale = "en",
	Port = ?config(port, Config),
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, _AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	true = mod_auth:add_user(ID, Password, [{locale, Locale}], Address,  Port, Directory),
	HostUrl = ?config(host_url, Config),
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Accept = {"accept", "application/json"},
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request2 = {HostUrl ++ "/partyManagement/v1/individual/" ++ ID, [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, _URI2} = lists:keyfind("href", 1, Object),
	{_, {array, _Characteristic}} = lists:keyfind("characteristic", 1, Object).

delete_user() ->
	[{userdata, [{doc,"Delete user in rest interface"}]}].

delete_user(Config) ->
	ID = "Queen",
	Password = "Queen",
	Locale = "en",
	Port = ?config(port, Config),
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, _AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	true = mod_auth:add_user(ID, Password, [{locale, Locale}], Address,  Port, Directory),
	HostUrl = ?config(host_url, Config),
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/partyManagement/v1/individual/" ++ ID, [Authentication]},
	{ok, Result1} = httpc:request(delete, Request1, [], []),
	{{"HTTP/1.1", 204, _NoContent}, _Headers1, []} = Result1,
	{error, no_such_user} = mod_auth:get_user(ID, Address, Port, Directory).

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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, Authentication], ContentType, RequestBody},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, Authentication], ContentType, RequestBody},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	{URI2, _} = httpd_util:split_path(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication]},
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
	ok = ocs:add_client(ID, Secret),
	{ok, Address} = inet:parse_address(ID),
	Fun = fun() ->
				[C1] = mnesia:read(client, Address, write),
				C2 = C1#client{identifier = list_to_binary(Identifier)},
				mnesia:write(C2)
	end,
	{atomic, ok} = mnesia:transaction(Fun),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/client/" ++ ID, [Accept, Authentication]},
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
	Username = ct:get_config(rest_user),
	Password = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(Username ++ ":", Password)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	ID = "beefbeefcafe",
	Request = {HostUrl ++ "/ocs/v1/client/" ++ ID, [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 400, _BadRequest}, _Headers, _Body} = Result.

get_client_notfound() ->
	[{userdata, [{doc, "get client notfound in rest interface"}]}].

get_client_notfound(Config) ->
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Username = ct:get_config(rest_user),
	Password = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(Username ++ ":", Password)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	ID = "10.2.53.20",
	Request = {HostUrl ++ "/ocs/v1/client/" ++ ID, [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/client", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	Request2 = {HostUrl ++ "/ocs/v1/client", [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/client", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, _} = Result,
	{_, URI1} = lists:keyfind("location", 1, Headers),
	{URI2, _} = httpd_util:split_path(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication], ContentType, []},
	{ok, Result1} = httpc:request(delete, Request2, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers1, []} = Result1,
	{_, "0"} = lists:keyfind("content-length", 1, Headers1).

get_usagespecs() ->
	[{userdata, [{doc,"Get usageSpecification collection"}]}].

get_usagespecs(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/usageManagement/v1/usageSpecification", [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Path = HostUrl ++ "/usageManagement/v1/usageSpecification",
	Request1 = {Path, [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, UsageSpecs} = mochijson:decode(Body1),
	F1 = fun({struct, UsageSpec1}) ->
				{_, Type1} = lists:keyfind("name", 1, UsageSpec1),
				Type1
	end,
	Types = lists:map(F1, UsageSpecs),
	F2 = fun(Type2) ->
				Request2 = {Path ++ "?name=" ++ Type2, [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/usageManagement/v1/usageSpecification", [Accept, Authentication]},
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
				Request2 = {HostUrl ++ Uri, [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage&sort=-date",
	Request = {RequestUri, [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri1 = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage",
	Request1 = {RequestUri1, [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, Usages} = mochijson:decode(Body1),
	{struct, Usage} = lists:last(Usages),
	{_, Id} = lists:keyfind("id", 1, Usage),
	{_, Href} = lists:keyfind("href", 1, Usage),
	RequestUri2 = HostUrl ++ Href,
	Request2 = {RequestUri2, [Accept, Authentication]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccessUsage&sort=-date&filter=date,status,usageCharacteristic",
	Request = {RequestUri, [Accept, Authentication]},
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
			{?AcctDelayTime, 5}, {?EventTimestamp, erlang:system_time(?SECOND)},
			{?AcctMultiSessionId, "8250731f"}, {?AcctLinkCount, 2},
			{?AcctAuthentic, 1}, {?AcctSessionTime, 3021},
			{?AcctInputOctets, 1702487}, {?AcctOutputOctets, 301629083},
			{?AcctInputGigawords, 1}, {?AcctOutputGigawords, 2},
			{?AcctInputPackets, 3021}, {?AcctOutputPackets, 125026},
			{?AcctTerminateCause, 5}],
	ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1813}, stop, Attrs),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage&sort=-date",
	Request = {RequestUri, [Accept, Authentication]},
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
	ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1812}, stop, Attrs),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri1 = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage",
	Request1 = {RequestUri1, [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request1, [], []),
	{{"HTTP/1.1", 200, _OK}, _Headers1, Body1} = Result1,
	{array, Usages} = mochijson:decode(Body1),
	{struct, Usage} = lists:last(Usages),
	{_, Href} = lists:keyfind("href", 1, Usage),
	RequestUri2 = HostUrl ++ Href,
	Request2 = {RequestUri2, [Accept, Authentication]},
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
	ok = ocs_log:acct_log(radius, {{0,0,0,0}, 1812}, stop, Attrs),
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=AAAAccountingUsage&sort=-date&filter=date,status,usageCharacteristic",
	Request = {RequestUri, [Accept, Authentication]},
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

get_ipdr_usage() ->
	[{userdata, [{doc,"Get a TMF635 IPDR usage"}]}].

get_ipdr_usage(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	RequestUri = HostUrl ++ "/usageManagement/v1/usage?type=PublicWLANAccessUsage",
	Request = {RequestUri, [Accept, Authentication]},
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

top_up_subscriber_balance() ->
	[{userdata, [{doc,"TMF654 Prepay Balance Management API :
			Top-up balance for subscriber"}]}].

top_up_subscriber_balance(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	ContentType = "application/json",
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Identity = binary_to_list(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(Identity, Password, []),
	RequestURI = HostUrl ++ "/balanceManagement/v1/" ++ Identity ++ "/balanceTopups",
	BucketType = {"type", "buckettype"}, 
	Channel = {"channel", {struct, [{"name", "POS"}]}},
	Amount = {"amount", {struct, [{"units", "octect"}, {"amount", 10000000}]}},
	JSON = {struct, [BucketType, Channel, Amount]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	Request = {RequestURI, [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, _, _} = Result.

get_subscriber_balance() ->
	[{userdata, [{doc,"TMF654 Prepay Balance Management API :
			Query balance for subscriber"}]}].

get_subscriber_balance(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	ContentType = "application/json",
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Identity = binary_to_list(ocs:generate_identity()),
	Password = ocs:generate_password(),
	{ok, _} = ocs:add_subscriber(Identity, Password, []),
	POSTURI = HostUrl ++ "/balanceManagement/v1/" ++ Identity ++ "/balanceTopups",
	BucketType = {"type", "buckettype"},
	Channel = {"channel", {struct, [{"name", "POS"}]}},
	Balance = 10000000,
	Amount = {"amount", {struct, [{"units", "octect"}, {"amount", Balance}]}},
	JSON = {struct, [BucketType, Channel, Amount]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	PostRequest = {POSTURI, [Accept, Authentication], ContentType, RequestBody},
	{ok, POSTResult} = httpc:request(post, PostRequest, [], []),
	{{"HTTP/1.1", 201, _Created}, _, _} = POSTResult,
	GETURI = HostUrl ++ "/balanceManagement/v1/" ++ Identity ++ "/buckets",
	GETRequest = {GETURI, [Accept, Authentication]},
	{ok, GETResult} = httpc:request(get, GETRequest, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = GETResult,
	ContentLength = integer_to_list(length(Body)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{struct, PrePayBalance} = mochijson:decode(Body),
	{_, Identity} = lists:keyfind("id", 1, PrePayBalance),
	{_, "/balanceManagement/v1/buckets/" ++ Identity} =
			lists:keyfind("href", 1, PrePayBalance),
	{_, "octects"} = lists:keyfind("bucketType", 1, PrePayBalance),
	{_, {struct, RemAmount}} = lists:keyfind("remainedAmount", 1, PrePayBalance),
	{_, Balance} = lists:keyfind("amount", 1, RemAmount),
	{_, "octect"} = lists:keyfind("units", 1, RemAmount),
	{_, "active"} = lists:keyfind("status", 1, PrePayBalance).

simultaneous_updates_on_user_faliure() ->
	[{userdata, [{doc,"Simulataneous updates on user resource using HTTP PUT.
		PUT operations on a already modified user resource should result in HTTP code
		412 (Preconditions failed)"}]}].

simultaneous_updates_on_user_faliure(Config) ->
	ID = "tylerjoseph",
	Password = ocs:generate_password(),
	Locale = "en",
	PasswordObj = {struct, [{name, password}, {value, Password}]},
	LocaleObj = {struct, [{name, locale}, {value, Locale}]},
	Characteristic = {array, [PasswordObj, LocaleObj]},
	JSON = {struct, [{id, ID}, {characteristic, Characteristic}]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	ContentType = "application/json",
	Request1 = {HostUrl ++ "/partyManagement/v1/individual/", [Accept, Authentication],
			ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result1,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _Etag} = lists:keyfind("etag", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	URI = "/partyManagement/v1/individual/" ++ ID,
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, Characteristic1} = lists:keyfind("characteristic", 1, Object),
	{array, [{struct, [{"name", "password"}, {"value", Password}]},
			{struct, [{"name", "locale"}, {"value", Locale}]}]} = Characteristic1,
	TS = integer_to_list(erlang:system_time(milli_seconds)),
	N = integer_to_list(erlang:unique_integer([positive])),
	NewEtag = TS ++ "-" ++ N,
	IfMatch = {"if-match", NewEtag},
	Request2 = {HostUrl ++ "/partyManagement/v1/individual/" ++ ID, [Accept, Authentication,
			IfMatch], ContentType, RequestBody},
	{ok, Result2} = httpc:request(put, Request2, [], []),
	{{"HTTP/1.1", 412, _Failed}, _Headers, _} = Result2.

simultaneous_updates_on_subscriber_faliure() ->
	[{userdata, [{doc,"Simulataneous HTTP PATCH updates on subscriber resource must fail
			if the resource is already being updated by someone else"}]}].

simultaneous_updates_on_subscriber_faliure(Config) ->
	ContentType = "application/json",
	ID = "eacfd73ae10a",
	Password = "ksc8c244npqc",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"value", 1000000}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"value", 64000}]},
	SessionTimeout = {struct, [{"name", "sessionTimeout"}, {"value", 3600}]},
	Interval = {struct, [{"name", "acctInterimInterval"}, {"value", 300}]},
	Class = {struct, [{"name", "class"}, {"value", "skiorgs"}]},
	SortedAttributes = lists:sort([AsendDataRate, AsendXmitRate, SessionTimeout, Interval, Class]),
	AttributeArray = {array, SortedAttributes},
	Balance = 100,
	Enable = true,
	JSON1 = {struct, [{"id", ID}, {"password", Password},
	{"attributes", AttributeArray}, {"balance", Balance}, {"enabled", Enable}]},
	RequestBody = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{_, _Etag} = lists:keyfind("etag", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{"/ocs/v1/subscriber/" ++ ID, _} = httpd_util:split_path(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{"password", Password} = lists:keyfind("password", 1, Object),
	{_, {array, Attributes}} = lists:keyfind("attributes", 1, Object),
	ExtraAttributes = Attributes -- SortedAttributes,
	SortedAttributes = lists:sort(Attributes -- ExtraAttributes),
	{"balance", Balance} = lists:keyfind("balance", 1, Object),
	{"enabled", Enable} = lists:keyfind("enabled", 1, Object),
	Port = ?config(port, Config),
	{ok, SslSock} = ssl:connect({127,0,0,1}, Port,  [binary, {active, false}], infinity),
	ok = ssl:ssl_accept(SslSock),
	PatchBody = "{ \"update\":\"password\", \"newpassword\":\"7fy8qhs7hh7n\" }",
	PatchBodyLen = size(list_to_binary(PatchBody)),
	PatchUri = "/ocs/v1/subscriber/" ++ ID,
	TS = integer_to_list(erlang:system_time(milli_seconds)),
	N = integer_to_list(erlang:unique_integer([positive])),
	NewEtag = TS ++ "-" ++ N,
	PatchReq = ["PATCH ", PatchUri, " HTTP/1.1",$\r,$\n,
			"Content-Type:application/json", $\r,$\n, "Accept:application/json",$\r,$\n,
			"If-match:" ++ NewEtag,$\r,$\n,"Authorization:"++ AuthKey,$\r,$\n,
			"Host:localhost:" ++ integer_to_list(Port),$\r,$\n,
			"Content-Length:" ++ integer_to_list(PatchBodyLen),$\r,$\n,
			$\r, $\n,
			PatchBody],
	ok = ssl:send(SslSock, list_to_binary(PatchReq)),
	Timeout = 3000,
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

simultaneous_updates_on_client_faliure() ->
	[{userdata, [{doc,"Simulataneous HTTP PATCH requests on client resource must fail
			if the resource is already being updated by someone else"}]}].

simultaneous_updates_on_client_faliure(Config) ->
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request1 = {HostUrl ++ "/ocs/v1/client/", [Accept, Authentication], ContentType, RequestBody},
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
	ok = ssl:ssl_accept(SslSock),
	NewSecret = ocs:generate_password(),
	PatchBody =  "{\"secret\" : \""  ++ NewSecret ++ "\"}",
	PatchBodyLen = size(list_to_binary(PatchBody)),
	PatchUri = "/ocs/v1/client/" ++ ID,
	TS = integer_to_list(erlang:system_time(milli_seconds)),
	N = integer_to_list(erlang:unique_integer([positive])),
	NewEtag = TS ++ "-" ++ N,
	PatchReq = ["PATCH ", PatchUri, " HTTP/1.1",$\r,$\n,
			"Content-Type:application/json", $\r,$\n, "Accept:application/json",$\r,$\n,
			"If-match:" ++ NewEtag,$\r,$\n,"Authorization:"++ AuthKey,$\r,$\n,
			"Host:localhost:" ++ integer_to_list(RestPort),$\r,$\n,
			"Content-Length:" ++ integer_to_list(PatchBodyLen),$\r,$\n,
			$\r,$\n,
			PatchBody],
	ok = ssl:send(SslSock, list_to_binary(PatchReq)),
	Timeout = 3000,
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

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

