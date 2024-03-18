%%% ocs_rest_res_health.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022 - 2023 SigScale Global Inc.
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
-copyright('Copyright (c) 2022 - 2023 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_health/2, get_applications/2, get_application/2]).

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
	["application/health+json", "application/problem+json"].

-spec get_health(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for `GET /health'
%% requests.
get_health([] = _Query, _RequestHeaders) ->
	try
		Check1 = application([ocs, inets, diameter, radius, snmp]),
		Check2 = table_size([offer, product, service, resource, bucket]),
		Check3 = up(),
		DiameterChecks = get_diameter_statistics(),
		case scheduler() of
			{ok, HeadOptions, Check4} ->
				{HeadOptions, {"checks", {struct,
						[Check1, Check2, Check3, Check4 |  DiameterChecks]}}};
			{error, _Reason1} ->
				{[], {"checks", {struct,
						[Check1, Check2, Check3 |  DiameterChecks]}}}
		end
	of
		{CacheControl, {_, {_, [{"application", {_, [{_, [{_, ocs}, _,
				{_, "up"}]} | _]}} | _]}} = Checks} ->
			Status = {"status", "pass"},
			ServiceId = {"serviceId", atom_to_list(node())},
			Description = {"description", "Health of SigScale OCS"},
			Health = {struct, [Status, ServiceId, Description, Checks]},
			ResponseBody = mochijson:encode(Health),
			ResponseHeaders = [{content_type, "application/health+json"}
					| CacheControl],
			{ok, ResponseHeaders, ResponseBody};
		{_CacheControl, Checks} ->
			Status = {"status", "fail"},
			ServiceId = {"serviceId", atom_to_list(node())},
			Description = {"description", "Health of SigScale OCS"},
			Health = {struct, [Status, ServiceId, Description, Checks]},
			ResponseBody = mochijson:encode(Health),
			ResponseHeaders = [{content_type, "application/health+json"}],
			{error, 503, ResponseHeaders, ResponseBody}
	catch
		_:_Reason2 ->
			{error, 500}
	end.

-spec get_applications(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for `GET /health/application'
%% requests.
get_applications([] = _Query, _RequestHeaders) ->
	try
		Check = application([ocs, inets, diameter, radius, snmp]),
		{"checks", {struct, [Check]}}
	of
		{_, {_, [{"application", {_, Applications}}]}} = Checks ->
			F = fun({_, [_, _, {"status", "up"}]}) ->
						false;
					({_, [_, _, {"status", "down"}]}) ->
						true
			end,
			case lists:any(F, Applications) of
				false ->
					Status = {"status", "pass"},
					ServiceId = {"serviceId", atom_to_list(node())},
					Description = {"description", "OTP applications"},
					Application = {struct, [Status, ServiceId, Description, Checks]},
					ResponseBody = mochijson:encode(Application),
					ResponseHeaders = [{content_type, "application/health+json"}],
					{ok, ResponseHeaders, ResponseBody};
				true ->
					Status = {"status", "fail"},
					ServiceId = {"serviceId", atom_to_list(node())},
					Description = {"description", "OTP applications"},
					Application = {struct, [Status, ServiceId, Description, Checks]},
					ResponseBody = mochijson:encode(Application),
					ResponseHeaders = [{content_type, "application/health+json"}],
					{error, 503, ResponseHeaders, ResponseBody}
			end
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec get_application(Id, RequestHeaders) -> Result
	when
		Id :: string(),
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for `GET /health/application/{Id}'
%% requests.
get_application(Id, _RequestHeaders) ->
	try
		Running = application:which_applications(),
		case lists:keymember(list_to_existing_atom(Id), 1, Running) of
			true ->
				Status = {"status", "up"},
				ServiceId = {"serviceId", Id},
				Application = {struct, [Status, ServiceId]},
				ResponseHeaders = [{content_type, "application/health+json"}],
				ResponseBody = mochijson:encode(Application),
				{ok, ResponseHeaders, ResponseBody};
			false ->
				Status = {"status", "down"},
				ServiceId = {"serviceId", Id},
				Application = {struct, [Status, ServiceId]},
				ResponseHeaders = [{content_type, "application/health+json"}],
				ResponseBody = mochijson:encode(Application),
				{error, 503, ResponseHeaders, ResponseBody}
		end
	catch
		_:badarg ->
			{error, 404};
		_:_Reason ->
			{error, 500}
	end.                                                                                                                                                                                                                                 

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec scheduler() -> Result
	when
		Result :: {ok, HeadOptions, Check} | {error, Reason},
		HeadOptions :: [{Option, Value}],
		Option :: etag | cache_control,
		Value :: string(),
		Check :: tuple(),
		Reason :: term().
%% @doc Check scheduler component.
%% @hidden
scheduler() ->
	scheduler(ocs:statistics(scheduler_utilization)).
scheduler({ok, {Etag, Interval, Report}}) ->
	[TS, _] = string:tokens(Etag, [$-]),
	Next = case (list_to_integer(TS) + Interval)
			- erlang:system_time(millisecond) of
		N when N =< 0 ->
			0;
		N when (N rem 1000) >= 500 ->
			(N div 1000) + 1;
		N ->
			N div 1000
	end,
	MaxAge = "max-age=" ++ integer_to_list(Next),
	HeadOptions = [{etag, Etag}, {cache_control, MaxAge}],
	F = fun({SchedulerId, Utilization}) ->
				Component1 = {"componentId",
						integer_to_list(SchedulerId)},
				Value1 = {"observedValue", Utilization},
				Unit1 = {"observedUnit", "percent"},
				Type1 = {"componentType", "system"},
				{struct, [Component1, Value1, Unit1, Type1]}
	end,
	Check = {"scheduler:utilization", {array, lists:map(F, Report)}},
	{ok, HeadOptions, Check};
scheduler({error, Reason}) ->
	{error, Reason}.

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
	{"application", {array, lists:reverse(Acc)}}.

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
			{"observedValue", Size}]} | Acc],
	table_size(T, NewAcc);
table_size([], Acc) ->
	{"table:size", {array, Acc}}.

-spec up() -> Time
	when
		Time :: tuple().
%% @doc Check uptime in seconds.
%% @hidden
up() ->
	CurrentTime = erlang:system_time(second),
	StartTime = erlang:convert_time_unit(erlang:system_info(start_time) +  erlang:time_offset(), native, second),
	Uptime = CurrentTime - StartTime,
	Time = [{struct, [{"componentType", "system"},
			{"observedUnit", "s"}, {"observedValue", Uptime}]}],
	{"uptime", {array, Time}}.

-spec get_diameter_statistics() -> DiameterChecks
	when
		DiameterChecks :: [tuple()].
%% @doc Get Diameter statistics checks.
get_diameter_statistics() ->
	Services = diameter:services(),
	Dictionaries = diameter_all_dictionaries(Services, []),
	get_diameter_checks(Dictionaries, Services, []).

%% @hidden
get_diameter_checks([Dictionary | T], Services, Acc) ->
	case get_diameter_counters(Dictionary, Services, #{}) of
		{_, {array, []}} = _Check ->
			get_diameter_checks(T, Services, Acc);
		Check ->
			get_diameter_checks(T, Services, [Check | Acc])
	end;
get_diameter_checks([], _Services, Acc) ->
	Acc.

%% @doc Get all dictionaries.
%% @hidden
diameter_all_dictionaries([Service | T], Acc) ->
	Applications = diameter:service_info(Service, applications),
	Dictionaries = diameter_dictionaries(Applications, []),
	diameter_all_dictionaries(T, [Dictionaries | Acc]);
diameter_all_dictionaries([], Acc) ->
	lists:sort(lists:flatten(Acc)).

%% @hidden
diameter_dictionaries([Application | T], Acc) ->
	{dictionary, Dictionary} = lists:keyfind(dictionary, 1, Application),
	diameter_dictionaries(T, [Dictionary | Acc]);
diameter_dictionaries([], Acc) ->
	Acc.

-spec get_diameter_counters(Dictionary, Services, Counters) -> DiameterCheck
	when
		Dictionary :: atom(),
		Services :: [diameter:service_name()],
		Counters :: map(),
		DiameterCheck :: tuple().
%% @doc Get Diameter count.
%% @hidden
get_diameter_counters(diameter_gen_base_rfc6733, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(0, Statistics, Counters), 
	get_diameter_counters(diameter_gen_base_rfc6733, T, NewCounters);
get_diameter_counters(diameter_gen_base_rfc6733, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-base:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_ro_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(4, Statistics, Counters), 
	get_diameter_counters(diameter_gen_3gpp_ro_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_ro_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-ro:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_gx_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(16777238, Statistics, Counters),
	get_diameter_counters(diameter_gen_3gpp_gx_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_gx_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-gx:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_s6a_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(16777251, Statistics, Counters),
	get_diameter_counters(diameter_gen_3gpp_s6a_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_s6a_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-s6a:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_s6b_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(16777272, Statistics, Counters),
	get_diameter_counters(diameter_gen_3gpp_s6b_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_s6b_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-s6b:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_sta_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(16777250, Statistics, Counters),
	get_diameter_counters(diameter_gen_3gpp_sta_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_sta_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-sta:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_swm_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(16777264, Statistics, Counters),
	get_diameter_counters(diameter_gen_3gpp_swm_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_swm_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-swm:counters", {array, Components}};
get_diameter_counters(diameter_gen_3gpp_swx_application, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(16777265, Statistics, Counters),
	get_diameter_counters(diameter_gen_3gpp_swx_application, T, NewCounters);
get_diameter_counters(diameter_gen_3gpp_swx_application, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-swx:counters", {array, Components}};
get_diameter_counters(diameter_gen_eap_application_rfc4072, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(5, Statistics, Counters),
	get_diameter_counters(diameter_gen_eap_application_rfc4072, T, NewCounters);
get_diameter_counters(diameter_gen_eap_application_rfc4072, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-eap:counters", {array, Components}};
get_diameter_counters(diameter_gen_nas_application_rfc7155, [Service | T], Counters) ->
	Statistics = diameter:service_info(Service, statistics),
	NewCounters = service_counters(1, Statistics, Counters),
	get_diameter_counters(diameter_gen_nas_application_rfc7155, T, NewCounters);
get_diameter_counters(diameter_gen_nas_application_rfc7155, [], Counters) ->
	Components = get_components(Counters),
	{"diameter-nas:counters", {array, Components}}.

%% @hidden
get_components(Counters) ->
	F = fun({CommandCode, ResultCode}, Count, Acc) ->
				[dia_count(CommandCode, ResultCode, Count) | Acc]
	end,
	maps:fold(F, [], Counters).

-spec service_counters(Application, Statistics, Counters) -> Counters
	when
		Application :: integer(),
		Statistics :: [tuple()],
		Counters :: #{{CommandCode, ResultCode} := Count},
		CommandCode :: pos_integer(),
		ResultCode :: pos_integer(),
		Count :: non_neg_integer().
%% @doc Parse service name statistics.
%% @hidden
service_counters(Application, [{_, PeerStat} | T] = _Statistics, Counters) ->
	NewCounters = peer_stat(Application, PeerStat, Counters),
	service_counters(Application, T, NewCounters);
service_counters(_Application, [], Counters) ->
	Counters.

%% @doc Parse peer statistics.
%% @hidden
peer_stat(Application, PeerStats, Counters) ->
	NewCounters = peer_stat1(Application, PeerStats, Counters).
%% @hidden
peer_stat1(Application, [{{{Application, CommandCode, 0}, send,
		{'Result-Code', ResultCode}}, Count} | T], Acc) ->
	NewAcc = case maps:find({CommandCode, ResultCode}, Acc) of
		{ok, Value} ->
			Acc#{{CommandCode, ResultCode} => Value + Count};
		error->
			Acc#{{CommandCode, ResultCode} => Count}
	end,
	peer_stat1(Application, T, NewAcc);
peer_stat1(Application, [_ | T], Acc) ->
	peer_stat1(Application, T, Acc);
peer_stat1(_Application, [], Acc) ->
	Acc.

-spec dia_count(CommandCode, ResultCode, Count) -> Component 
	when
		CommandCode :: non_neg_integer(),
		ResultCode :: non_neg_integer(),
		Count :: non_neg_integer(),
		Component :: tuple().
%% @doc Returns JSON object for a diameter count.
dia_count(257, ResultCode, Count) ->
		Component = "CEA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(280, ResultCode, Count) ->
		Component = "DWA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(271, ResultCode, Count) ->
		Component = "ACA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(282, ResultCode, Count) ->
		Component = "DPA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(258, ResultCode, Count) ->
		Component = "RAA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(274, ResultCode, Count) ->
		Component = "ASA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(275, ResultCode, Count) ->
		Component = "STA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(272, ResultCode, Count) ->
		Component = "CCA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(265, ResultCode, Count) ->
		Component = "AAA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(268, ResultCode, Count) ->
		Component = "DEA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(301, ResultCode, Count) ->
		Component = "SAA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(303, ResultCode, Count) ->
		Component = "MAA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedUnit", "Count"},
			{"observedValue", Count}]};
dia_count(304, ResultCode, Count) ->
		Component = "RTA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(316, ResultCode, Count) ->
		Component = "ULA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(318, ResultCode, Count) ->
		Component = "AIA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]};
dia_count(321, ResultCode, Count) ->
		Component = "PUA Result-Code: " ++ integer_to_list(ResultCode),
		{struct, [{"componentId", Component},
			{"componentType", "Protocol"},
			{"observedValue", Count}]}.

