%%% ocs_re_interface_SUITE.erl
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
%%%  @doc Test suite for the Re interface (OCF - RF)
%%% 	of the {@link //ocs. ocs} application.
%%%
-module(ocs_re_interface_SUITE).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(PathNrfRating, "/nrf-rating/v1/").

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
	[{userdata, [{doc, "Test suite for Re interface in OCS"}]},
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
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	_RestGroup = ct:get_config(rest_group),
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
	HostUrl = "https://" ++ Host ++ ":" ++ integer_to_list(Port),
	Config1 = [{port, Port}, {host_url, HostUrl} | Config].

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
	[].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

send_iec() ->
	[{userdata, [{doc, "Send Immediate Event Charging (IEC) from OCF"}]}].

send_iec(Config) ->
	MSISDN = msisdn(),
	IMSI = imsi(),
	NodeFunctionality = [{"nodeFunctionality", "OCF"}],
	NfConsumerIdentification = {struct, NodeFunctionality},
	InvocationTimeStamp1 = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber = {"invocationSequenceNumber", 1},
	ActualTime = {"actualTime",
			timestamp(erlang:system_time(?MILLISECOND) - rand:uniform(100))},
	SubscriptionId = {"subscriptionId",
			["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]},
	OneTimeEvent = {"oneTimeEvent", true},
	OneTimeEventType = {"oneTimeEventType", "IEC"},
	ServiceContextId = {"serviceContextId", "mnc001.mcc001.15.32274@3gpp.org"},
	ServiceId = {"serviceId", rand:uniform(64)},
	DestinationType = {"destinationIdType", "DN"},
	DestinationData = {"destinationIdData", msisdn()},
	DestinationId = {"destinationId",
			{struct, [DestinationType, DestinationData]}},
	RequestSubType = {"requestSubType", "DEBIT"}, 
	Service1 = [ServiceContextId, ServiceId, DestinationId, RequestSubType],
	ServiceRating1 = {"serviceRating", [{struct, Service1}]},
	JSON = {struct, [NfConsumerIdentification, InvocationTimeStamp1,
			InvocationSequenceNumber, ActualTime, SubscriptionId,
			OneTimeEvent, OneTimeEventType, ServiceRating1]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request1 = {HostUrl ++ ?PathNrfRating, [Accept], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, ContentType} = lists:keyfind("content-type", 1, Headers),
	{_, _URI} = lists:keyfind("location", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, TS} = lists:keyfind("invocationTimeStamp", 1, Object),
	true = is_integer(ocs_log:iso8601(TS)),
	InvocationSequenceNumber = lists:keyfind("invocationSequenceNumber", 1, Object),
	{_, [{struct, ServiceRating2}]} = lists:keyfind("serviceRating", 1, Object),
	ServiceContextId = lists:keyfind("serviceContextId", 1, ServiceRating2),
	ServiceId = lists:keyfind("serviceId", 1, ServiceRating2),
	{_, {struct, ConsumedUnit}} = lists:keyfind("consumedUnit", 1, ServiceRating2),
	{_, 1} = lists:keyfind("serviceSpecificUnit", 1, ConsumedUnit),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating2).

send_ecur() ->
	[{userdata, [{doc, "Send Event Charging with Unit Reservation (ECUR) from OCF"}]}].

send_ecur(Config) ->
	MSISDN = msisdn(),
	IMSI = imsi(),
	NodeFunctionality = [{"nodeFunctionality", "OCF"}],
	NfConsumerIdentification = {struct, NodeFunctionality},
	InvocationTimeStamp1 = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber1 = {"invocationSequenceNumber", 1},
	ActualTime = {"actualTime",
			timestamp(erlang:system_time(?MILLISECOND) - rand:uniform(100))},
	SubscriptionId = {"subscriptionId",
			["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]},
	OneTimeEvent = {"oneTimeEvent", true},
	OneTimeEventType = {"oneTimeEventType", "PEC"},
	ServiceContextId = {"serviceContextId", "mnc001.mcc001.15.32274@3gpp.org"},
	ServiceId = {"serviceId", rand:uniform(64)},
	DestinationType = {"destinationIdType", "DN"},
	DestinationData = {"destinationIdData", msisdn()},
	DestinationId = {"destinationId",
			{struct, [DestinationType, DestinationData]}},
	RequestSubType = {"requestSubType", "RESERVE"}, 
	ServiceSpecificUnit = {"serviceSpecificUnit", 1},
	RequestedUnit = {"requestedUnit", {struct, [ServiceSpecificUnit]}},
	Service1 = [ServiceContextId, ServiceId,
			DestinationId, RequestSubType, RequestedUnit],
	ServiceRating1 = {"serviceRating", [{struct, Service1}]},
	JSON = {struct, [NfConsumerIdentification, InvocationTimeStamp1,
			InvocationSequenceNumber1, ActualTime, SubscriptionId,
			OneTimeEvent, OneTimeEventType, ServiceRating1]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request1 = {HostUrl ++ ?PathNrfRating, [Accept], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, ContentType} = lists:keyfind("content-type", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, TS} = lists:keyfind("invocationTimeStamp", 1, Object),
	true = is_integer(ocs_log:iso8601(TS)),
	InvocationSequenceNumber = lists:keyfind("invocationSequenceNumber", 1, Object),
	{_, [{struct, ServiceRating2}]} = lists:keyfind("serviceRating", 1, Object),
	ServiceContextId = lists:keyfind("serviceContextId", 1, ServiceRating2),
	ServiceId = lists:keyfind("serviceId", 1, ServiceRating2),
	{_, {struct, ConsumedUnit}} = lists:keyfind("grantedUnit", 1, ServiceRating2),
	ServiceSpecificUnit = lists:keyfind("serviceSpecificUnit", 1, ConsumedUnit),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating2).

send_ecur_final() ->
	[{userdata, [{doc, "Send final ECUR from OCF"}]}].

send_ecur_final(Config) ->
	MSISDN = msisdn(),
	IMSI = imsi(),
	NodeFunctionality = [{"nodeFunctionality", "OCF"}],
	NfConsumerIdentification = {struct, NodeFunctionality},
	InvocationTimeStamp1 = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber1 = {"invocationSequenceNumber", 1},
	ActualTime1 = {"actualTime",
			timestamp(erlang:system_time(?MILLISECOND) - rand:uniform(100))},
	SubscriptionId = {"subscriptionId",
			["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]},
	OneTimeEvent = {"oneTimeEvent", true},
	OneTimeEventType = {"oneTimeEventType", "PEC"},
	ServiceContextId = {"serviceContextId", "mnc001.mcc001.15.32274@3gpp.org"},
	ServiceId = {"serviceId", rand:uniform(64)},
	DestinationType = {"destinationIdType", "DN"},
	DestinationData = {"destinationIdData", msisdn()},
	DestinationId = {"destinationId",
			{struct, [DestinationType, DestinationData]}},
	RequestSubType1 = {"requestSubType", "RESERVE"}, 
	ServiceSpecificUnit = {"serviceSpecificUnit", 1},
	RequestedUnit = {"requestedUnit", {struct, [ServiceSpecificUnit]}},
	Service1 = [ServiceContextId, ServiceId,
			DestinationId, RequestSubType1, RequestedUnit],
	ServiceRating1 = {"serviceRating", [{struct, Service1}]},
	JSON1 = {struct, [NfConsumerIdentification, InvocationTimeStamp1,
			InvocationSequenceNumber1, ActualTime1, SubscriptionId,
			OneTimeEvent, OneTimeEventType, ServiceRating1]},
	RequestBody1 = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request1 = {HostUrl ++ ?PathNrfRating, [Accept], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, ResponseBody1} = Result1,
	{_, URI} = lists:keyfind("location", 1, Headers1),
	{struct, Object1} = mochijson:decode(ResponseBody1),
	{_, TS1} = lists:keyfind("invocationTimeStamp", 1, Object1),
	true = is_integer(ocs_log:iso8601(TS1)),
	InvocationSequenceNumber1 = lists:keyfind("invocationSequenceNumber", 1, Object1),
	{_, [{struct, ServiceRating2}]} = lists:keyfind("serviceRating", 1, Object1),
	ServiceContextId = lists:keyfind("serviceContextId", 1, ServiceRating2),
	ServiceId = lists:keyfind("serviceId", 1, ServiceRating2),
	{_, {struct, GrantedUnit}} = lists:keyfind("grantedUnit", 1, ServiceRating2),
	{_, SSU} = lists:keyfind("serviceSpecificUnit", 1, GrantedUnit),
	true = is_integer(SSU),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating2),
	ActualTime2 = {"actualTime", timestamp()},
	InvocationTimeStamp2 = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber2 = {"invocationSequenceNumber", 2},
	ConsumedUnit1 = {"consumedUnit", {struct, [ServiceSpecificUnit]}},
	RequestSubType2 = {"requestSubType", "Debit"}, 
	Service3 = [ServiceContextId, ServiceId,
			DestinationId, RequestSubType2, ConsumedUnit1],
	ServiceRating3 = {"serviceRating", [{struct, Service3}]},
	JSON2 = {struct, [NfConsumerIdentification, InvocationTimeStamp2,
			InvocationSequenceNumber2, ActualTime2, SubscriptionId,
			OneTimeEvent, OneTimeEventType, ServiceRating3]},
	RequestBody2 = lists:flatten(mochijson:encode(JSON2)),
	Request2 = {HostUrl ++ URI ++ "/release",
			[Accept], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody2} = Result2,
	{_, ContentType} = lists:keyfind("content-type", 1, Headers2),
	{struct, Object2} = mochijson:decode(ResponseBody2),
	{_, TS2} = lists:keyfind("invocationTimeStamp", 1, Object2),
	true = is_integer(ocs_log:iso8601(TS2)),
	InvocationSequenceNumber2 = lists:keyfind("invocationSequenceNumber", 1, Object2),
	{_, [{struct, ServiceRating4}]} = lists:keyfind("serviceRating", 1, Object2),
	ServiceContextId = lists:keyfind("serviceContextId", 1, ServiceRating4),
	ServiceId = lists:keyfind("serviceId", 1, ServiceRating4),
	{_, {struct, ConsumedUnit2}} = lists:keyfind("consumedUnit", 1, ServiceRating4),
	ServiceSpecificUnit = lists:keyfind("serviceSpecificUnit", 1, ConsumedUnit2),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, ServiceRating4).

send_scur() ->
	[{userdata, [{doc, "Send Session Charging with Unit Reservation (SCUR) from OCF"}]}].

send_scur(Config) ->
	MSISDN = msisdn(),
	IMSI = imsi(),
	NodeFunctionality = [{"nodeFunctionality", "OCF"}],
	NfConsumerIdentification = {struct, NodeFunctionality},
	InvocationTimeStamp = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber = {"invocationSequenceNumber", 1},
	ActualTime = {"actualTime",
			timestamp(erlang:system_time(?MILLISECOND) - rand:uniform(100))},
	SubscriptionId = {"subscriptionId",
			["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]},
	ServiceContextId = {"serviceContextId", "mnc001.mcc001.15.32251@3gpp.org"},
	ServiceId = {"serviceId", rand:uniform(64)},
	RatingGroup1 = {"ratingGroup", rand:uniform(64)},
	RatingGroup2 = {"ratingGroup", rand:uniform(64)},
	RequestSubType = {"requestSubType", "RESERVE"}, 
	RequestedUnit = {"requestedUnit", {struct, []}},
	Service1 = [ServiceContextId, ServiceId,
			RatingGroup1, RequestSubType, RequestedUnit],
	Service2 = [ServiceContextId, ServiceId,
			RatingGroup2, RequestSubType, RequestedUnit],
	ServiceRating1 = {"serviceRating",
			[{struct, Service1}, {struct, Service2}]},
	JSON = {struct, [NfConsumerIdentification, InvocationTimeStamp,
			InvocationSequenceNumber, ActualTime, SubscriptionId,
			ServiceRating1]},
	RequestBody = lists:flatten(mochijson:encode(JSON)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request1 = {HostUrl ++ ?PathNrfRating, [Accept], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, ContentType} = lists:keyfind("content-type", 1, Headers),
	{_, _URI} = lists:keyfind("location", 1, Headers),
	{struct, Object} = mochijson:decode(ResponseBody),
	{_, TS} = lists:keyfind("invocationTimeStamp", 1, Object),
	true = is_integer(ocs_log:iso8601(TS)),
	InvocationSequenceNumber = lists:keyfind("invocationSequenceNumber", 1, Object),
	{_, ServiceRating2} = lists:keyfind("serviceRating", 1, Object),
	[{struct, Service3}, {struct, Service4}] = ServiceRating2,
	ServiceContextId = lists:keyfind("serviceContextId", 1, Service3),
	ServiceContextId = lists:keyfind("serviceContextId", 1, Service4),
	ServiceId = lists:keyfind("serviceId", 1, Service3),
	ServiceId = lists:keyfind("serviceId", 1, Service4),
	RatingGroup1 = lists:keyfind("ratingGroup", 1, Service3),
	RatingGroup2 = lists:keyfind("ratingGroup", 1, Service4),
	{_, {struct, GrantedUnit1}} = lists:keyfind("grantedUnit", 1, Service3),
	{_, {struct, GrantedUnit2}} = lists:keyfind("grantedUnit", 1, Service4),
	F = fun({"time", N}) when is_integer(N), N > 0 ->
				true;
			({"totalVolume", N}) when is_integer(N), N > 0 ->
				true;
			({"uplinkVolume", N}) when is_integer(N), N > 0 ->
				true;
			({"downlinkVolume", N}) when is_integer(N), N > 0 ->
				true;
			(_)  ->
				false
	end,
	true = lists:all(F, GrantedUnit1),
	true = lists:all(F, GrantedUnit2),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, Service3),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, Service4).

send_scur_update() ->
	[{userdata, [{doc, "Send SCUR update from OCF"}]}].

send_scur_update(Config) ->
	MSISDN = msisdn(),
	IMSI = imsi(),
	NodeFunctionality = [{"nodeFunctionality", "OCF"}],
	NfConsumerIdentification = {struct, NodeFunctionality},
	InvocationTimeStamp1 = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber1 = {"invocationSequenceNumber", 1},
	ActualTime1 = {"actualTime",
			timestamp(erlang:system_time(?MILLISECOND) - rand:uniform(100))},
	SubscriptionId = {"subscriptionId",
			["msisdn-" ++ MSISDN, "imsi-" ++ IMSI]},
	ServiceContextId = {"serviceContextId", "mnc001.mcc001.15.32251@3gpp.org"},
	ServiceId = {"serviceId", rand:uniform(64)},
	RatingGroup1 = {"ratingGroup", rand:uniform(64)},
	RatingGroup2 = {"ratingGroup", rand:uniform(64)},
	RequestSubType = {"requestSubType", "RESERVE"}, 
	RequestedUnit = {"requestedUnit", {struct, []}},
	Service1 = [ServiceContextId, ServiceId,
			RatingGroup1, RequestSubType, RequestedUnit],
	Service2 = [ServiceContextId, ServiceId,
			RatingGroup2, RequestSubType, RequestedUnit],
	ServiceRating1 = {"serviceRating",
			[{struct, Service1}, {struct, Service2}]},
	JSON1 = {struct, [NfConsumerIdentification, InvocationTimeStamp1,
			InvocationSequenceNumber1, ActualTime1, SubscriptionId,
			ServiceRating1]},
	RequestBody1 = lists:flatten(mochijson:encode(JSON1)),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	ContentType = "application/json",
	Request1 = {HostUrl ++ ?PathNrfRating, [Accept], ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, ResponseBody1} = Result1,
	{_, ContentType} = lists:keyfind("content-type", 1, Headers1),
	{_, URI} = lists:keyfind("location", 1, Headers1),
	{struct, Object1} = mochijson:decode(ResponseBody1),
	{_, TS1} = lists:keyfind("invocationTimeStamp", 1, Object1),
	true = is_integer(ocs_log:iso8601(TS1)),
	InvocationSequenceNumber1 = lists:keyfind("invocationSequenceNumber", 1, Object1),
	{_, ServiceRatings1} = lists:keyfind("serviceRating", 1, Object1),
	[{struct, Service3}, {struct, Service4}] = ServiceRatings1,
	ServiceContextId = lists:keyfind("serviceContextId", 1, Service3),
	ServiceContextId = lists:keyfind("serviceContextId", 1, Service4),
	ServiceId = lists:keyfind("serviceId", 1, Service3),
	ServiceId = lists:keyfind("serviceId", 1, Service4),
	RatingGroup1 = lists:keyfind("ratingGroup", 1, Service3),
	RatingGroup2 = lists:keyfind("ratingGroup", 1, Service4),
	{_, {struct, GrantedUnit1}} = lists:keyfind("grantedUnit", 1, Service3),
	{_, {struct, GrantedUnit2}} = lists:keyfind("grantedUnit", 1, Service4),
	F = fun({"time", N}) when is_integer(N), N > 0 ->
				true;
			({"totalVolume", N}) when is_integer(N), N > 0 ->
				true;
			({"uplinkVolume", N}) when is_integer(N), N > 0 ->
				true;
			({"downlinkVolume", N}) when is_integer(N), N > 0 ->
				true;
			(_)  ->
				false
	end,
	true = lists:all(F, GrantedUnit1),
	true = lists:all(F, GrantedUnit2),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, Service3),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, Service4),
	TotalVolume1 = {"totalVolume", rand:uniform(100000000)},
	TotalVolume2 = {"totalVolume", rand:uniform(100000000)},
	UsedUnit1 = {"usedUnits", {struct, [TotalVolume1]}},
	UsedUnit2 = {"usedUnits", {struct, [TotalVolume2]}},
	InvocationTimeStamp2 = {"invocationTimeStamp", timestamp()},
	InvocationSequenceNumber2 = {"invocationSequenceNumber", 2},
	ActualTime2 = {"actualTime",
			timestamp(erlang:system_time(?MILLISECOND) - rand:uniform(100))},
	Service3 = [ServiceContextId, ServiceId,
			RatingGroup1, RequestSubType, RequestedUnit, UsedUnit1],
	Service4 = [ServiceContextId, ServiceId,
			RatingGroup2, RequestSubType, RequestedUnit, UsedUnit2],
	ServiceRating3 = {"serviceRating",
			[{struct, Service3}, {struct, Service4}]},
	JSON2 = {struct, [NfConsumerIdentification, InvocationTimeStamp2,
			InvocationSequenceNumber2, ActualTime2, SubscriptionId,
			ServiceRating3]},
	RequestBody2 = lists:flatten(mochijson:encode(JSON2)),
	Request2 = {HostUrl ++ URI, [Accept], ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody2} = Result2,
	{struct, Object2} = mochijson:decode(ResponseBody2),
	{_, TS2} = lists:keyfind("invocationTimeStamp", 1, Object2),
	true = is_integer(ocs_log:iso8601(TS2)),
	InvocationSequenceNumber2 = lists:keyfind("invocationSequenceNumber", 1, Object2),
	{_, ServiceRatings2} = lists:keyfind("serviceRating", 1, Object2),
	[{struct, Service5}, {struct, Service6}] = ServiceRatings2,
	ServiceContextId = lists:keyfind("serviceContextId", 1, Service5),
	ServiceContextId = lists:keyfind("serviceContextId", 1, Service6),
	ServiceId = lists:keyfind("serviceId", 1, Service5),
	ServiceId = lists:keyfind("serviceId", 1, Service6),
	RatingGroup1 = lists:keyfind("ratingGroup", 1, Service5),
	RatingGroup2 = lists:keyfind("ratingGroup", 1, Service6),
	{_, {struct, GrantedUnit1}} = lists:keyfind("grantedUnit", 1, Service5),
	{_, {struct, GrantedUnit2}} = lists:keyfind("grantedUnit", 1, Service6),
	F = fun({"time", N}) when is_integer(N), N > 0 ->
				true;
			({"totalVolume", N}) when is_integer(N), N > 0 ->
				true;
			({"uplinkVolume", N}) when is_integer(N), N > 0 ->
				true;
			({"downlinkVolume", N}) when is_integer(N), N > 0 ->
				true;
			(_)  ->
				false
	end,
	true = lists:all(F, GrantedUnit1),
	true = lists:all(F, GrantedUnit2),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, Service3),
	{_, "SUCCESS"} = lists:keyfind("resultCode", 1, Service4).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

timestamp() ->
	timestamp(erlang:system_time(?MILLISECOND)).

timestamp(TS) ->
	ocs_rest:iso8601(TS).

msisdn() ->
	digits(rand:uniform(9) + 6, []).

imsi() ->
	digits(15, []).

digits(0, Acc) ->
	Acc;
digits(N, Acc) ->
	Digit = rand:uniform(10) + $0 - 1,
	digits(N - 1, [Digit | Acc]).

