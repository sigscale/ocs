%%% ocs_rest_api_SUITE.erl
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
%%%  @doc Test suite for REST API
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_api_SUITE).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

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
	{ok, IP} = application:get_env(ocs, radius_auth_addr),
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
	[authenticate_user_request, unauthenticate_user_request, authenticate_subscriber_request,
	unauthenticate_subscriber_request, authenticate_client_request,
	unauthenticate_client_request, add_subscriber, get_subscriber,
	get_subscriber_not_found, retrieve_all_subscriber, delete_subscriber,
	add_client, get_client, get_client_bogus, get_client_notfound, get_all_clients,
	delete_client, get_usage].

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
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 197}, {"value", 1024}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 255}, {"value", 512}]},
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
	Request = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{_, _, "/ocs/v1/subscriber/" ++ ID, _, _} = mochiweb_util:urlsplit(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{"id", ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{"password", Password} = lists:keyfind("password", 1, Object),
	{_, {array, Attributes}} = lists:keyfind("attributes", 1, Object),
	ExtraAttributes = Attributes -- SortedAttributes,
	SortedAttributes = lists:sort(Attributes -- ExtraAttributes),
	{"balance", Balance} = lists:keyfind("balance", 1, Object),
	{"enabled", Enable} = lists:keyfind("enabled", 1, Object).


get_subscriber() ->
	[{userdata, [{doc,"get subscriber in rest interface"}]}].

get_subscriber(Config) ->
	ContentType = "application/json",
	AcceptValue = "application/json",
	ID = "eacfd73ae10a",
	Password = "ksc8c244npqc",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 197}, {"value", 1024}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 255}, {"value", 512}]},
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
	{_, _, URI2, _, _} = mochiweb_util:urlsplit(URI1),
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
	ID = "5557615036fd",
	Password = "2h7csggw35aa",
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 197}, {"value", 1024}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 255}, {"value", 512}]},
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
	Request2 = {HostUrl ++ "/ocs/v1/subscriber", [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, Accept} = lists:keyfind("content-type", 1, Headers1),
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
	AsendDataRate = {struct, [{"name", "ascendDataRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 197}, {"value", 1024}]},
	AsendXmitRate = {struct, [{"name", "ascendXmitRate"}, {"type", 26},
		{"vendorId", 529}, {"vendorType", 255}, {"value", 512}]},
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
	{_, _, URI2, _, _} = mochiweb_util:urlsplit(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication], ContentType, []},
	{ok, Result1} = httpc:request(delete, Request2, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers1, []} = Result1,
	{_, "0"} = lists:keyfind("content-length", 1, Headers1).

add_client() ->
	[{userdata, [{doc,"Add client in rest interface"}]}].

add_client(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.9",
	Disconnect = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON = {struct, [{"id", ID}, {"disconnectPort", Disconnect}, {"protocol", Protocol},
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
	{_, _, "/ocs/v1/client/" ++ ID, _, _} = mochiweb_util:urlsplit(URI),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI} = lists:keyfind("href", 1, Object),
	{_, Disconnect} = lists:keyfind("disconnectPort", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object).

get_client() ->
	[{userdata, [{doc,"get client in rest interface"}]}].

get_client(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.9",
	Disconnect = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON = {struct, [{"id", ID}, {"disconnectPort", Disconnect}, {"protocol", Protocol},
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
	{_, _, URI2, _, _} = mochiweb_util:urlsplit(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers1),
	ContentLength = integer_to_list(length(Body1)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers1),
	{struct, Object} = mochijson:decode(Body1),
	{_, ID} = lists:keyfind("id", 1, Object),
	{_, URI2} = lists:keyfind("href", 1, Object),
	{_, Disconnect} = lists:keyfind("disconnectPort", 1, Object),
	{_, Protocol} = lists:keyfind("protocol", 1, Object),
	{_, Secret} = lists:keyfind("secret", 1, Object).

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
	Disconnect = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c344npqc",
	JSON = {struct, [{"id", ID}, {"disconnectPort", Disconnect}, {"protocol", Protocol},
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
	{_, Disconnect} = lists:keyfind("disconnectPort", 1, ClientVar),
	{_, Protocol} = lists:keyfind("protocol", 1, ClientVar),
	{_, Secret} = lists:keyfind("secret", 1, ClientVar).

delete_client() ->
	[{userdata, [{doc,"Delete client in rest interface"}]}].

delete_client(Config) ->
	ContentType = "application/json",
	ID = "10.2.53.9",
	Disconnect = 1899,
	Protocol = "RADIUS",
	Secret = "ksc8c244npqc",
	JSON1 = {struct, [{"id", ID}, {"disconnectPort", Disconnect}, {"protocol", Protocol},
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
	{_, _, URI2, _, _} = mochiweb_util:urlsplit(URI1),
	Request2 = {HostUrl ++ URI2, [Accept, Authentication], ContentType, []},
	{ok, Result1} = httpc:request(delete, Request2, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers1, []} = Result1,
	{_, "0"} = lists:keyfind("content-length", 1, Headers1).

get_usage() ->
	[{userdata, [{doc,"Get usage in rest interface"}]}].

get_usage(Config) ->
	HostUrl = ?config(host_url, Config),
	AcceptValue = "application/json",
	Accept = {"accept", AcceptValue},
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	Encodekey = base64:encode_to_string(string:concat(RestUser ++ ":", RestPass)),
	AuthKey = "Basic " ++ Encodekey,
	Authentication = {"authorization", AuthKey},
	Request2 = {HostUrl ++ "/usageManagement/v1/usage", [Accept, Authentication ]},
	{ok, Result1} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers1, Body1} = Result1,
	{_, AcceptValue} = lists:keyfind("content-type", 1, Headers1),
	ContentLength = integer_to_list(length(Body1)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers1),
	{array, [{struct, Usage} | _] = _UsageList} = mochijson:decode(Body1),
	{_, ID} = lists:keyfind("id", 1, Usage),
	{_, HREF} = lists:keyfind("href", 1, Usage),
	{_, Date} = lists:keyfind("date", 1, Usage),
	{_, "PublicWLANAccessUsage"} = lists:keyfind("type", 1, Usage),
	{_, Description} = lists:keyfind("description", 1, Usage),
	{_, "recieved"} = lists:keyfind("status", 1, Usage),
	{struct, UsageSpecification} = lists:keyfind("usageSpecification", 1, Usage),
	{_, ID1} = lists:keyfind("id", 1, UsageSpecification),
	{_, HREF} = lists:keyfind("href", 1, UsageSpecification),
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
	true = lists:any(F, UsageCharacteristic).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

