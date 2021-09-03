%%% ocs_rest_res_prometheus.erl
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
%%% This module exports metrics for Prometheus server to "scrape".
%%%
%%% @reference <a href="https://github.com/prometheus/prometheus">Prometheus.io</a>.
%%%
-module(ocs_rest_res_prometheus).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_metrics/2]).

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
	["text/plain"].

-spec get_metrics(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /metrics'
%% requests.
get_metrics([] = _Query, _Headers) ->
	Body = [diameter()],
	{ok, [], Body}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
diameter() ->
	diameter_services(diameter:services(), [], []).

%% @hidden
diameter_services([H | T], In, Out) ->
	ServiceInfo = diameter:service_info(H, [applications, transport]),
	{_, Applications} = lists:keyfind(applications, 1, ServiceInfo),
	{_, Transports} = lists:keyfind(transport, 1, ServiceInfo),
	Dictionaries = application_dicts(Applications),
	diameter_transport(T, Transports, Dictionaries, In, Out);
diameter_services([], In, Out) ->
	In1 = case In of
		In when length(In) > 0 ->
			["# HELP diameter_message_in_total A counter of DIAMETER"
					" messages received on a transport.\n"
					"# TYPE diameter_message_in_total counter\n"
					| lists:reverse(In)];
		[] ->
			[]
	end,
	Out1 = case Out of
		Out when length(Out) > 0 ->
			["# HELP diameter_message_out_total A counter of DIAMETER"
					" messages sent on a transport.\n"
					"# TYPE diameter_message_out_total counter\n"
					| lists:reverse(Out)];
		[] ->
			[]
	end,
	[In1, Out1].

%% @hidden
diameter_transport(Services, [H | T], Dictionaries, In, Out) ->
	{_, Options} = lists:keyfind(options, 1, H),
	{_, Config} = lists:keyfind(transport_config, 1, Options),
	{_, IP} = lists:keyfind(ip, 1, Config),
	{_, Port} = lists:keyfind(port, 1, Config),
	S = case lists:keyfind(transport_module, 1, Options) of
		{_, diameter_tcp} ->
			"{transport=\"tcp\"";
		{_, diameter_sctp} ->
			"{transport=\"sctp\""
	end,
	Acc = case lists:keyfind(type, 1, H) of
		{_, listen} ->
			["\",role=\"server\"", integer_to_list(Port), "\",port=\"",
					address(IP), ",address=\"", S];
		{_, connect} ->
			{_, Raddr} = lists:keyfind(raddr, 1, Config),
			{_, Rport} = lists:keyfind(rport, 1, Config),
			["\"", integer_to_list(Rport), "\",rport=\"",
					address(Raddr), "\",role=\"client\",raddr=\"",
					integer_to_list(Port), "\",port=\"", address(IP),
					",address=\"", S]
	end,
	{_, Statistics} = lists:keyfind(statistics, 1, H),
	diameter_transport(Services, T, Dictionaries, Statistics, In, Out, Acc);
diameter_transport(Services, [], _Dictionaries, In, Out) ->
	diameter_services(Services, In, Out).
%% @hidden
diameter_transport(Services, Transports, Dictionaries,
		[{{{AppId, Code, Rbit}, Direction}, Count} | T],
		In, Out, Acc) ->
	{_, Module} = lists:keyfind(AppId, 1, Dictionaries),
	Message = case Rbit of
		0 ->
			atom_to_list(Module:msg_name(Code, false));
		1 ->
			atom_to_list(Module:msg_name(Code, true))
	end,
	case Direction of
		recv ->
			L1 = ["\n", integer_to_list(Count), "\"} ",
					Message, ",message=\"" | Acc],
			L2 = lists:reverse(L1),
			L3 =  lists:flatten(["diameter_message_in_total" | L2]),
			diameter_transport(Services, Transports, Dictionaries,
					T, [L3 | In], Out, Acc);
		send ->
			L1 = ["\n", integer_to_list(Count), "\"} ",
					Message, ",message=\"" | Acc],
			L2 = lists:reverse(L1),
			L3 =  lists:flatten(["diameter_message_out_total" | L2]),
			diameter_transport(Services, Transports, Dictionaries,
					T, In, [L3 | Out], Acc)
	end;
diameter_transport(Services, Transports, Dictionaries,
		[{{{_AppId, _Code, _Rbit}, _Direction,
		{'Result-Code', _}}, _Count} | T], In, Out, Acc) ->
	% @todo result code metrics.
	diameter_transport(Services, Transports, Dictionaries,
			T, In, Out, Acc);
diameter_transport(Services, Transports,
		Dictionaries, [], In, Out, _Acc) ->
	diameter_transport(Services, Transports, Dictionaries, In, Out).

%% @hidden
application_dicts(Applications) ->
	application_dicts(Applications, []).
%% @hidden
application_dicts([H | T], Acc) ->
	{_, Id} = lists:keyfind(id, 1, H),
	{_, Dictionary} = lists:keyfind(dictionary, 1, H),
	application_dicts(T, [{Id, Dictionary} | Acc]);
application_dicts([], Acc) ->
	lists:reverse(Acc).

%% @hidden
address(Address) when is_tuple(Address) ->
	inet:ntoa(Address);
address(Address) when is_list(Address) ->
	Address.

