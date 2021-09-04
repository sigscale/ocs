%%% ocs_rest_res_health.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
%%% This module reports on the health of the system.
%%%
%%% @reference <a href="https://tools.ietf.org/id/draft-inadarei-api-health-check-05.html">
%%% 	Health Check Response Format for HTTP APIs</a>
%%%
-module(ocs_rest_res_health).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_health/2, get_applications/2]).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provide list of resource representations accepted.
content_types_accepted() ->
	[].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/health+json"].

-spec get_health(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /health'
%% requests.
get_health([] = _Query, _Headers) ->
	try
		Check1 = scheduler(),
		Check2 = application([ocs, inets, diameter, radius, snmp]),
		Check3 = table_size([offer, product, service, resource, bucket]),
		{"checks", {struct, [Check1, Check2, Check3]}}
	of
		Checks ->
			Status = {"status", "pass"},
			ServiceId = {"serviceId", atom_to_list(node())},
			Description = {"description", "Health of SigScale OCS"},
			Health = {struct, [Status, ServiceId, Description, Checks]},
			Body = mochijson:encode(Health),
			Headers1 = [{content_type, "application/health+json"}],
			{ok, Headers1, Body}
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec get_applications(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /health/application'
%% requests.
get_applications([] = _Query, _Headers) ->
	try
		Check = application([ocs, inets, diameter, radius, snmp]),
		{"checks", {struct, [Check]}}
	of
		Checks ->
			Status = {"status", "pass"},
			ServiceId = {"serviceId", atom_to_list(node())},
			Description = {"description", "OTP applications"},
			Application = {struct, [Status, ServiceId, Description, Checks]},
			Body = mochijson:encode(Application),
			Headers1 = [{content_type, "application/health+json"}],
			{ok, Headers1, Body}
	catch
		_:_Reason ->
			{error, 500}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec scheduler() -> Check
	when
		Check :: tuple().
%% @doc Check scheduler component.
%% @hidden
scheduler() ->
	scheduler(ocs:statistics(scheduler_utilization)).
scheduler({ok, {_Etag, _Interval, Report}}) ->
	F = fun({SchedulerId, Utilization}) ->
				Component1 = {"componentId",
						integer_to_list(SchedulerId)},
				Value1 = {"observeredValue", Utilization},
				Unit1 = {"observedUnit", "percent"},
				Type1 = {"componentType", "system"},
				{struct, [Component1, Value1, Unit1, Type1]}
	end,
	{"scheduler:utilization", {array, lists:map(F, Report)}};
scheduler({error, Reason}) ->
	exit(Reason).

-spec application(Names) -> Check
	when
		Names :: [atom()],
		Check :: tuple().
%% @doc Check application component.
%% @hidden
application(Names) ->
	application(Names, application:which_applications(), []).
%% @hidden
application([Name | T], Running, Acc) ->
	Status = case lists:keymember(Name, 1, Running) of
		true ->
			"up";
		false ->
			"down"
	end,
	NewAcc = [{struct, [{"componentId", Name},
			{"componentType", "component"},
			{"status", Status}]} | Acc],
	application(T, Running, NewAcc);
application([], _Running, Acc) ->
	{"application", {array, Acc}}.

-spec table_size(Names) -> Check
	when
		Names :: [atom()],
		Check :: tuple().
%% @doc Check table component size.
%% @hidden
table_size(Names) ->
	table_size(Names, []).
%% @hidden
table_size([Name | T], Acc) ->
	Size = mnesia:table_info(Name, size),
	NewAcc = [{struct, [{"componentId", Name},
			{"componentType", "component"},
			{"observedUnit", "rows"},
			{"observeredValue", Size}]} | Acc],
	table_size(T, NewAcc);
table_size([], Acc) ->
	{"table:size", {array, Acc}}.

