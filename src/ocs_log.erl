%%% ocs_log.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016-2017 SigScale Global Inc.
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
%%% @doc This library module implements functions used in handling logging
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% 	@reference <a href="http://www.tmforum.org/ipdr/">IPDR Specifications</a>.
%%%
-module(ocs_log).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc.').

%% export the ocs_log public API
-export([acct_open/0, acct_log/4, acct_close/0,
		acct_query/5, acct_query/6,
		auth_open/0, auth_log/5, auth_log/6, auth_close/0,
		auth_query/6, auth_query/7,
		ipdr_log/3, ipdr_file/2, get_range/3, last/2,
		dump_file/2, http_file/2, httpd_logname/1,
		date/1, iso8601/1]).

%% export the ocs_log event types
-export_type([auth_event/0, acct_event/0]).

-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include("../include/diameter_gen_cc_application_rfc4006.hrl").
-include("../include/diameter_gen_nas_application_rfc7155.hrl").
-include("../include/diameter_gen_eap_application_rfc4072.hrl").

-define(ACCTLOG, ocs_acct).
-define(AUTHLOG, ocs_auth).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The ocs_log public API
%%----------------------------------------------------------------------

-spec acct_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Open an accounting event disk log.
acct_open() ->
	{ok, Directory} = application:get_env(ocs, acct_log_dir),
	case file:make_dir(Directory) of
		ok ->
			acct_open1(Directory);
		{error, eexist} ->
			acct_open1(Directory);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
acct_open1(Directory) ->
	{ok, LogSize} = application:get_env(ocs, acct_log_size),
	{ok, LogFiles} = application:get_env(ocs, acct_log_files),
	Log = ?ACCTLOG,
	FileName = Directory ++ "/" ++ atom_to_list(Log),
	case disk_log:open([{name, Log}, {file, FileName},
					{type, wrap}, {size, {LogSize, LogFiles}}]) of
		{ok, Log} ->
			ok;
		{repaired, Log, _Recovered, _Bad} ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, Log}, {error, Reason}]),
			{error, Reason}
	end.

-type acct_event() :: {
		TS :: pos_integer(), % posix time in milliseconds
		N :: pos_integer(), % unique number
		Protocol :: radius | diameter,
		Node :: atom(),
		Server :: {inet:ip_address(), inet:port_number()},
		Attributes :: radius_attributes:attributes()}.

-spec acct_log(Protocol, Server, Type, Attributes) -> Result
	when
		Protocol :: radius | diameter,
		Server :: {Address, Port},
		Address :: inet:ip_address(),
		Port :: integer(),
		Type :: on | off | start | stop | interim | event,
		Attributes :: radius_attributes:attributes() | #diameter_cc_app_CCR{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write an event to accounting log.
acct_log(Protocol, Server, Type, Attributes)
		when ((Protocol == radius) or (Protocol == diameter)) ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	Event = {TS, N, Protocol, node(), Server, Type, Attributes},
	disk_log:log(?ACCTLOG, Event).

-spec acct_close() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Close accounting disk log.
acct_close() ->
	case disk_log:close(?ACCTLOG) of
		ok ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, ?ACCTLOG}, {error, Reason}]),
			{error, Reason}
	end.

-spec acct_query(Continuation, Start, End, Types, AttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Types :: [Type] | '_',
		Type :: on | off | start | stop | interim | event,
		AttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: term() | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()],
		Reason :: term().
%% @doc Query accounting log events with filters.
%% @equiv acct_query(Continuation, Start, End, radius, Types, AttrsMatch)
acct_query(Continuation, Start, End, Types, AttrsMatch) ->
	acct_query(Continuation, Start, End, radius, Types, AttrsMatch).

-spec acct_query(Continuation, Start, End, Protocol, Types, AttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Protocol :: radius | diameter | '_',
		Types :: [Type] | '_',
		Type :: on | off | start | stop | interim | event,
		AttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: term() | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()],
		Reason :: term().
%% @doc Query accounting log events with filters.
%%
%% 	The first time called `Continuation' should have the value `start'.
%%
%% 	Events before `Start' or after `Stop', or which do not match
%% 	the `Protocol' or one of the `Types', are ignored.
%%
%% 	Events which do not include `Attribute' in attributes are ignored.
%% 	If `Match' is '_' any attribute value will match, otherwise events
%% 	with attributes having a value not equal to `Match' will be ignored.
%% 	All attribute filters must match or the event will be ignored.
%%
%% 	`Protocol', `Types', or `AttrsMatch' may be '_' which matches any value.
%%
%% 	Returns a new `Continuation' and a list of matching accounting events.
%% 	Successive calls use the new `Continuation' to read more events.
%%
acct_query(Continuation, {{_, _, _}, {_, _, _}} = Start,
		End, Protocol, Types, AttrsMatch) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	acct_query(Continuation, Seconds * 1000, End, Protocol, Types, AttrsMatch);
acct_query(Continuation, Start, {{_, _, _}, {_, _, _}} = End,
		Protocol, Types, AttrsMatch) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	acct_query(Continuation, Start, Seconds * 1000 + 999,
			Protocol, Types, AttrsMatch);
acct_query(start, Start, End, Protocol, Types, AttrsMatch)
		when is_integer(Start), is_integer(End) ->
	acct_query(Start, End, Protocol, Types, AttrsMatch,
			[], disk_log:bchunk(?ACCTLOG, start));
acct_query(Continuation, Start, End, Protocol, Types, AttrsMatch)
		when is_integer(Start), is_integer(End) ->
	acct_query1(Start, End, Protocol, Types, AttrsMatch,
			disk_log:chunk(?ACCTLOG, Continuation), []).

%% @hidden
acct_query(Start, End, Protocol, Types, AttrsMatch,
		PrevChunk, eof) ->
	Chunk = [binary_to_term(E) || E <- PrevChunk],
	acct_query1(Start, End, Protocol, Types, AttrsMatch, {eof, Chunk}, []);
acct_query(_Start, _End, _Protocol, _Types, _AttrsMatch,
		_PrevChunk, {error, Reason}) ->
	{error, Reason};
acct_query(Start, End, Protocol, Types, AttrsMatch,
		PrevChunk, {Cont, [H | T] = Chunk}) ->
	case binary_to_term(H) of
		Event when element(1, Event) > End ->
			{eof, []};
		Event when element(1, Event) >= Start ->
			NewChunk = [binary_to_term(E) || E <- PrevChunk ++ Chunk],
			acct_query1(Start, End, Protocol, Types, AttrsMatch,
					{Cont, NewChunk}, []);
		_Event ->
			acct_query(Start, End, Protocol, Types, AttrsMatch,
					T, disk_log:bchunk(?ACCTLOG, Cont))
	end.
%% @hidden
acct_query1(_Start, _End, _Protocol, _Types, _AttrsMatch, eof, Acc) ->
	{eof, lists:reverse(Acc)};
acct_query1(_Start, _End, _Protocol, _Types, _AttrsMatch, {error, Reason}, _Acc) ->
	{error, Reason};
acct_query1(_, End, _, _, _, {_, [{TS, _, _, _, _, _, _, _} | _]}, Acc)
		when TS > End ->
	{eof, lists:reverse(Acc)};
acct_query1(Start, End, '_', '_', AttrsMatch,
		{Cont, [{TS, _, _, _, _, _, _} | _] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	acct_query2(Start, End, '_', '_', AttrsMatch,
			{Cont, Chunk}, Acc, AttrsMatch);
acct_query1(Start, End, Protocol, '_', AttrsMatch,
		{Cont, [{TS, _, Protocol, _, _, _, _} | _] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	acct_query2(Start, End, Protocol, '_', AttrsMatch,
			{Cont, Chunk}, Acc, AttrsMatch);
acct_query1(Start, End, Protocol, Types, AttrsMatch,
		{Cont, [{TS, _, Protocol, _, _, Type, _} | T] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	case lists:member(Type, Types) of
		true ->
			acct_query2(Start, End, Protocol, Types,
					AttrsMatch, {Cont, Chunk}, Acc, AttrsMatch);
		false ->
			acct_query1(Start, End, Protocol, Types,
					AttrsMatch, {Cont, T}, Acc)
	end;
acct_query1(Start, End, Protocol, Types, AttrsMatch, {Cont, [_ | T]}, Acc) ->
	acct_query1(Start, End, Protocol, Types, AttrsMatch, {Cont, T}, Acc);
acct_query1(_, _, _, _, _, {eof, []}, Acc) ->
	{eof, lists:reverse(Acc)};
acct_query1(_, _, _, _, _, {Cont, []}, Acc) ->
	{Cont, lists:reverse(Acc)}.
%% @hidden
acct_query2(Start, End, Protocol, Types, '_', {Cont, [H | T]}, Acc, '_') ->
	acct_query1(Start, End, Protocol, Types, '_', {Cont, T}, [H | Acc]);
acct_query2(Start, End, Protocol, Types, AttrsMatch,
		{Cont, [{_, _, _, _, _, _, Attrs} | T] = Chunk},
		Acc, [{Attribute, Match} | T1]) ->
	case lists:keyfind(Attribute, 1, Attrs) of
		{Attribute, Match} ->
			acct_query2(Start, End, Protocol, Types, AttrsMatch,
					{Cont, Chunk}, Acc, T1);
		{Attribute, _} when Match == '_' ->
			acct_query2(Start, End, Protocol, Types, AttrsMatch,
					{Cont, Chunk}, Acc, T1);
		_ ->
			acct_query1(Start, End, Protocol, Types, AttrsMatch, {Cont, T}, Acc)
	end;
acct_query2(Start, End, Protocol, Types, AttrsMatch, {Cont, [H | T]}, Acc, []) ->
	acct_query1(Start, End, Protocol, Types, AttrsMatch, {Cont, T}, [H | Acc]).

-spec auth_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Open authorization event disk log.
auth_open() ->
	{ok, Directory} = application:get_env(ocs, auth_log_dir),
	case file:make_dir(Directory) of
		ok ->
			auth_open1(Directory);
		{error, eexist} ->
			auth_open1(Directory);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
auth_open1(Directory) ->
	{ok, LogSize} = application:get_env(ocs, auth_log_size),
	{ok, LogFiles} = application:get_env(ocs, auth_log_files),
	Log = ?AUTHLOG,
	FileName = Directory ++ "/" ++ atom_to_list(Log),
	case disk_log:open([{name, Log}, {file, FileName},
					{type, wrap}, {size, {LogSize, LogFiles}}]) of
		{ok, Log} ->
			ok;
		{repaired, Log, _Recovered, _Bad} ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, Log}, {error, Reason}]),
			{error, Reason}
	end.

-type auth_event() :: {
		TS :: pos_integer(), % posix time in milliseconds
		N :: pos_integer(), % unique number
		Protocol :: radius | diameter,
		Node :: atom(),
		Server :: {inet:ip_address(), inet:port_number()},
		Client :: {inet:ip_address(), inet:port_number()},
		RequestAttributes :: radius_attributes:attributes(),
		ResponseAttributes :: radius_attributes:attributes()}.

-spec auth_log(Protocol, Server, Client, Type, RequestAttributes,
		ResponseAttributes) -> Result
	when
		Protocol :: radius,
		Server :: {Address, Port},
		Client :: {Address, Port},
		Address :: inet:ip_address(),
		Port :: integer(),
		Type :: accept | reject | change,
		RequestAttributes :: radius_attributes:attributes(),
		ResponseAttributes :: radius_attributes:attributes(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write a RADIUS event to authorization log.
auth_log(Protocol, Server, Client, Type, RequestAttributes, ResponseAttributes) ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	Event = {TS, N, Protocol, node(), Server, Client, Type,
			RequestAttributes, ResponseAttributes},
	disk_log:log(?AUTHLOG, Event).

-spec auth_log(Protocol, Server, Client, Request, Response) -> Result
	when
		Protocol :: diameter,
		Server :: {Address, Port},
		Client :: {Address, Port},
		Address :: inet:ip_address(),
		Port :: integer(),
		Request :: #diameter_nas_app_AAR{} | #diameter_eap_app_DER{},
		Response :: #diameter_nas_app_AAA{} | #diameter_eap_app_DEA{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write a DIAMETER event to authorization log.
auth_log(Protocol, Server, Client, Request, Response) ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	Event = {TS, N, Protocol, node(), Server, Client, Request, Response},
	disk_log:log(?AUTHLOG, Event).

-spec auth_query(Continuation, Start, End, Types,
		ReqAttrsMatch, RespAttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Types :: [Type] | '_',
		Type :: accept | reject | change,
		ReqAttrsMatch :: [{Attribute, Match}] | '_',
		RespAttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: term() | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [auth_event()],
		Reason :: term().
%% @doc Query access log events with filters.
%% @equiv auth_query(Continuation, Start, End, radius, Types, ReqAttrsMatch, RespAttrsMatch)
auth_query(Continuation, Start, End, Types, ReqAttrsMatch, RespAttrsMatch) ->
	auth_query(Continuation, Start, End, radius, Types,
			ReqAttrsMatch, RespAttrsMatch).

-spec auth_query(Continuation, Start, End, Protocol, Types,
		ReqAttrsMatch, RespAttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Protocol :: radius | diameter | '_',
		Types :: [Type] | '_',
		Type :: accept | reject | change,
		ReqAttrsMatch :: [{Attribute, Match}] | '_',
		RespAttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: term() | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [auth_event()],
		Reason :: term().
%% @doc Query access log events with filters.
%%
%% 	The first time called `Continuation' should have the value `start'.
%%
%% 	Events before `Start' or after `Stop' or which do not match
%% 	the protocol or one of the `Types' are ignored.
%%
%% 	Events which do not include `Attribute' in request or response
%% 	attributes are ignored. If `Match' is '_' any attribute value
%% 	will match, otherwise events with attributes having a value not
%% 	equal to `Match' will be ignored. All attribute filters must
%% 	match or the event will be ignored.
%%
%% 	`Protocol', `Types', `ReqAttrsMatch' `ResAttrsMatch'  may be
%% 	'_' which matches any value.
%%
%% 	Returns a new `Continuation' and a list of matching access events.
%% 	Successive calls use the new `Continuation' to read more events.
%%
%%
auth_query(Continuation, {{_, _, _}, {_, _, _}} = Start, End, Protocol,
		Types, ReqAttrsMatch, RespAttrsMatch) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	auth_query(Continuation, Seconds * 1000, End, Protocol, Types,
			ReqAttrsMatch, RespAttrsMatch);
auth_query(Continuation, Start, {{_, _, _}, {_, _, _}} = End, Protocol,
		Types, ReqAttrsMatch, RespAttrsMatch) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	auth_query(Continuation, Start, Seconds * 1000 + 999,
			Protocol, Types, ReqAttrsMatch, RespAttrsMatch);
auth_query(start, Start, End, Protocol, Types,
		ReqAttrsMatch, RespAttrsMatch)
		when is_integer(Start), is_integer(End) ->
	auth_query(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
			[], disk_log:bchunk(?AUTHLOG, start));
auth_query(Continuation, Start, End, Protocol, Types,
		ReqAttrsMatch, RespAttrsMatch)
		when is_integer(Start), is_integer(End) ->
	auth_query1(Start, End, Protocol, Types, ReqAttrsMatch,
			RespAttrsMatch, disk_log:chunk(?AUTHLOG, Continuation), []).

%% @hidden
auth_query(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
		PrevChunk, eof) ->
	Chunk = [binary_to_term(E) || E <- PrevChunk],
	auth_query1(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
			{eof, Chunk}, []);
auth_query(_Start, _End, _Protocol, _Types, _ReqAttrsMatch, _RespAttrsMatch,
		_PrevChunk, {error, Reason}) ->
	{error, Reason};
auth_query(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
		PrevChunk, {Cont, [H | T] = Chunk}) ->
	case binary_to_term(H) of
		Event when element(1, Event) > End ->
			{eof, []};
		Event when element(1, Event) >= Start ->
			NewChunk = [binary_to_term(E) || E <- PrevChunk ++ Chunk],
			auth_query1(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
					{Cont, NewChunk}, []);
		_Event ->
			auth_query(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
					T, disk_log:bchunk(?ACCTLOG, Cont))
	end.
%% @hidden
auth_query1(_Start, _End, _Protocol, _Types, _ReqAttrsMatch, _RespAttrsMatch,
		eof, Acc) ->
	{eof, lists:reverse(Acc)};
auth_query1(_Start, _End, _Protocol, _Types, _ReqAttrsMatch, _RespAttrsMatch,
		{error, Reason}, _Acc) ->
	{error, Reason};
%% @hidden
auth_query1(_, End, _, _, _, _, {_, [{TS, _, _, _, _, _, _, _, _} | _]}, Acc)
		when TS > End ->
	{eof, lists:reverse(Acc)};
auth_query1(Start, End, '_', '_', ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{TS, _, _, _, _, _, _, _, _} | _] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	auth_query2(Start, End, '_', '_', ReqAttrsMatch,
			RespAttrsMatch, {Cont, Chunk}, Acc, ReqAttrsMatch);
auth_query1(Start, End, Protocol, '_', ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{TS, _, Protocol, _, _, _, _, _, _} | _] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	auth_query2(Start, End, Protocol, '_', ReqAttrsMatch,
			RespAttrsMatch, {Cont, Chunk}, Acc, ReqAttrsMatch);
auth_query1(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{TS, _, Protocol, _, _, _, Type, _, _} | T] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	case lists:member(Type, Types) of
		true ->
			auth_query2(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, ReqAttrsMatch);
		false ->
			auth_query1(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, T}, Acc)
	end;
auth_query1(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [_ | T]}, Acc) ->
	auth_query1(Start, End, Protocol, Types, ReqAttrsMatch,
			RespAttrsMatch, {Cont, T}, Acc);
auth_query1(_, _, _, _, _, _, {eof, []}, Acc) ->
	{eof, lists:reverse(Acc)};
auth_query1(_, _, _, _, _, _, {Cont, []}, Acc) ->
	{Cont, lists:reverse(Acc)}.
%% @hidden
auth_query2(Start, End, Protocol, Types, '_', RespAttrsMatch,
		{Cont, Chunk}, Acc, '_') ->
	auth_query3(Start, End, Protocol, Types, '_',
			RespAttrsMatch, {Cont, Chunk}, Acc, RespAttrsMatch);
auth_query2(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{_, _, _, _, _, _, _, ReqAttrs, _} | T] = Chunk},
		Acc, [{Attribute, Match} | T1]) ->
	case lists:keyfind(Attribute, 1, ReqAttrs) of
		{Attribute, Match} ->
			auth_query2(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, T1);
		{Attribute, _} when Match == '_' ->
			auth_query2(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, T1);
		_ ->
			auth_query1(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, T}, Acc)
	end;
auth_query2(Start, End, Protocol, Types, ReqAttrsMatch,
		RespAttrsMatch, {Cont, Chunk}, Acc, []) ->
	auth_query3(Start, End, Protocol, Types, ReqAttrsMatch,
			RespAttrsMatch, {Cont, Chunk}, Acc, RespAttrsMatch).
%% @hidden
auth_query3(Start, End, Protocol, Types, ReqAttrsMatch, '_',
		{Cont, [H | T]}, Acc, '_') ->
	auth_query1(Start, End, Protocol, Types, ReqAttrsMatch, '_',
			{Cont, T}, [H | Acc]);
auth_query3(Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{_, _, _, _, _, _, _, _, RespAttrs} | T] = Chunk},
		Acc, [{Attribute, Match} | T1]) ->
	case lists:keyfind(Attribute, 1, RespAttrs) of
		{Attribute, Match} ->
			auth_query3(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, T1);
		{Attribute, _} when Match == '_' ->
			auth_query3(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, T1);
		_ ->
			auth_query1(Start, End, Protocol, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, T}, Acc)
	end;
auth_query3(Start, End, Protocol, Types, ReqAttrsMatch,
		RespAttrsMatch, {Cont, [H | T]}, Acc, []) ->
	auth_query1(Start, End, Protocol, Types, ReqAttrsMatch,
			RespAttrsMatch, {Cont, T}, [H | Acc]).

-spec auth_close() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Close auth disk log.
auth_close() ->
	case disk_log:close(?AUTHLOG) of
		ok ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, ?AUTHLOG}, {error, Reason}]),
			{error, Reason}
	end.

-spec ipdr_log(File, Start, End) -> Result
	when
		File :: file:filename(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Log accounting records within range to new IPDR disk log.
%%
%% 	Creates a new {@link //kernel/disk_log:log(). disk_log:log()},
%% 	or overwrites an existing, with filename `File'. The log starts
%% 	with a `#ipdrDoc{}' header, is followed by `#ipdr{}' records,
%% 	and ends with a `#ipdrDocEnd{}' trailer.
%%
%% 	The `ocs_acct' log is searched for events created between `Start'
%% 	and `End' which may be given as
%% 	`{{Year, Month, Day}, {Hour, Minute, Second}}' or the native
%% 	{@link //erts/erlang:system_time(). erlang:system_time(millisecond)}.
%%
ipdr_log(File, {{_, _, _}, {_, _, _}} = Start, End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	ipdr_log(File, Seconds * 1000, End);
ipdr_log(File, Start, {{_, _, _}, {_, _, _}} = End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	ipdr_log(File, Start, Seconds * 1000 + 999);
ipdr_log(File, Start, End) when is_list(File),
		is_integer(Start), is_integer(End) ->
	case disk_log:open([{name, File}, {file, File}, {repair, truncate}]) of
		{ok, IpdrLog} ->
			IpdrDoc = #ipdrDoc{docId = uuid(), version = "3.1",
					creationTime = iso8601(erlang:system_time(?MILLISECOND)),
					ipdrRecorderInfo = atom_to_list(node())},
			case disk_log:log(IpdrLog, IpdrDoc) of
				ok ->
					ipdr_log1(IpdrLog, Start, End,
							start_binary_tree(?ACCTLOG, Start, End));
				{error, Reason} ->
					error_logger:error_report([disk_log:format_error(Reason),
							{module, ?MODULE}, {log, IpdrLog}, {error, Reason}]),
					disk_log:close(IpdrLog),
					{error, Reason}
			end;
		{error, Reason} ->
			error_logger:error_report([disk_log:format_error(Reason),
					{module, ?MODULE}, {file, File}, {error, Reason}]),
			{error, Reason}
	end.
%% @hidden
ipdr_log1(IpdrLog, _Start, _End, {error, Reason}) ->
	error_logger:error_report([disk_log:format_error(Reason),
			{module, ?MODULE}, {log, ?ACCTLOG}, {error, Reason}]),
	ipdr_log4(IpdrLog, 0);
ipdr_log1(IpdrLog, _Start, _End, eof) ->
	ipdr_log4(IpdrLog, 0);
ipdr_log1(IpdrLog, Start, End, Cont) ->
	ipdr_log2(IpdrLog, Start, End, [], disk_log:chunk(?ACCTLOG, Cont)).
%% @hidden
ipdr_log2(IpdrLog, _Start, _End, _PrevChunk, {error, Reason}) ->
	error_logger:error_report([disk_log:format_error(Reason),
			{module, ?MODULE}, {log, ?ACCTLOG}, {error, Reason}]),
	ipdr_log4(IpdrLog, 0);
ipdr_log2(IpdrLog, _Start, _End, [], eof) ->
	ipdr_log4(IpdrLog, 0);
ipdr_log2(IpdrLog, Start, End, PrevChunk, eof) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	ipdr_log3(IpdrLog, Start, End, 0,
			{eof, lists:dropwhile(Fstart, PrevChunk)});
ipdr_log2(IpdrLog, Start, End, _PrevChunk, {Cont, [H | T]})
		when element(1, H) < Start ->
	ipdr_log2(IpdrLog, Start, End, T, disk_log:chunk(?ACCTLOG, Cont));
ipdr_log2(IpdrLog, Start, End, PrevChunk, {Cont, Chunk}) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	ipdr_log3(IpdrLog, Start, End, 0,
			{Cont, lists:dropwhile(Fstart, PrevChunk ++ Chunk)}).
%% @hidden
ipdr_log3(IpdrLog, _Start, _End, SeqNum, eof) ->
	ipdr_log4(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, _Start, _End, SeqNum, {error, _Reason}) ->
	ipdr_log4(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, _Start, _End, SeqNum, {eof, []}) ->
	ipdr_log4(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, []}) ->
	ipdr_log3(IpdrLog, Start, End, SeqNum, disk_log:chunk(?ACCTLOG, Cont));
ipdr_log3(IpdrLog, _Start, End, SeqNum, {_Cont, [H | _]})
		when element(1, H) > End ->
	ipdr_log4(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, _Start, End, SeqNum, {Cont, [H | T]})
		when element(5, H) == stop ->
	IPDR = ipdr_codec(H),
	NewSeqNum = SeqNum + 1,
	case disk_log:log(IpdrLog, IPDR#ipdr{seqNum = NewSeqNum}) of
		ok ->
			ipdr_log3(IpdrLog, _Start, End, NewSeqNum, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([disk_log:format_error(Reason),
					{module, ?MODULE}, {log, IpdrLog}, {error, Reason}]),
			disk_log:close(IpdrLog),
			{error, Reason}
	end;
ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, [_ | T]}) ->
	ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, T}).
%% @hidden
ipdr_log4(IpdrLog, SeqNum) ->
	EndTime = iso8601(erlang:system_time(?MILLISECOND)),
	IpdrDocEnd = #ipdrDocEnd{count = SeqNum, endTime = EndTime},
	case disk_log:log(IpdrLog, IpdrDocEnd) of
		ok ->
			case disk_log:close(IpdrLog) of
				ok ->
					ok;
				{error, Reason} ->
					error_logger:error_report([disk_log:format_error(Reason),
							{module, ?MODULE}, {log, IpdrLog}, {error, Reason}]),
					{error, Reason}
			end;
		{error, Reason} ->
			error_logger:error_report([disk_log:format_error(Reason),
					{module, ?MODULE}, {log, IpdrLog}, {error, Reason}]),
			disk_log:close(IpdrLog),
			{error, Reason}
	end.

-spec ipdr_file(LogFile, Format) -> Result
	when
		LogFile :: file:filename(),
		Format :: xml | xdr | csv,
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Export internal IPDR disk log.
%%
%% 	Creates a file named `LogFile'.`Format' with the details from the
%% 	{@link //kernel/disk_log:log(). disk_log:log()} file `LogFile'
%% 	created previously with {@link ipdr_log/3}.
%%
ipdr_file(LogFile, Format) when is_list(LogFile),
		((Format == xml) or (Format == xdr) or (Format == csv)) ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	FileName = Directory ++ "/" ++ LogFile,
	case disk_log:open([{name, make_ref()}, {file, FileName}, {repair, true}]) of
		{ok, Log} ->
			ipdr_file1(LogFile, Log, Format);
		{repaired, Log, _Recovered, _Bad} ->
			ipdr_file1(LogFile, Log, Format);
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{file, FileName}, {error, Reason}]),
			{error, Reason}
	end.
%% @hidden
ipdr_file1(FileName, Log, Format) ->
	{ok, Directory} = application:get_env(ocs, export_dir),
	case file:make_dir(Directory) of
		ok ->
			ipdr_file2(FileName, Log, Format, Directory);
		{error, eexist} ->
			ipdr_file2(FileName, Log, Format, Directory);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{directory, Directory}, {error, Reason}]),
			disk_log:close(Log),
			{error, Reason}
	end.
%% @hidden
ipdr_file2(FileName, Log, Format, ExportDir) ->
	CsvFile = ExportDir ++ "/" ++ FileName ++ "." ++ atom_to_list(Format),
	case file:open(CsvFile, [raw, write, delayed_write]) of
		{ok, IoDevice} ->
			ipdr_file3(Log, IoDevice, Format, disk_log:chunk(Log, start));
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log},
					{filename, CsvFile}, {error, Reason}]),
			disk_log:close(Log),
			{error, Reason}
	end.
%% @hidden
ipdr_file3(Log, IoDevice, _Format, eof) ->
	case disk_log:close(Log) of
		ok ->
			file:close(IoDevice);
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, Log}, {error, Reason}]),
			file:close(IoDevice),
			{error, Reason}
	end;
ipdr_file3(Log, IoDevice, _Format, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, Log}, {error, Reason}]),
	disk_log:close(Log),
	file:close(IoDevice),
	{error, Reason};
ipdr_file3(Log, IoDevice, Format, {Cont, []}) ->
	ipdr_file3(Log, IoDevice, Format, disk_log:chunk(Log, Cont));
ipdr_file3(Log, IoDevice, xml, {Cont, Events}) ->
	ipdr_xml(Log, IoDevice, {Cont, Events});
ipdr_file3(Log, IoDevice, xdr, {Cont, Events}) ->
	ipdr_xdr(Log, IoDevice, {Cont, Events});
ipdr_file3(Log, IoDevice, csv, {Cont, Events}) ->
	ipdr_csv(Log, IoDevice, {Cont, Events}).

-spec get_range(Log, Start, End) -> Result
	when
		Log :: disk_log:log(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Result :: [term()].
%% @doc Get all events in a log within a date/time range.
%%
%% @private
get_range(Log, {{_, _, _}, {_, _, _}} = Start, End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	get_range(Log, Seconds * 1000, End);
get_range(Log, Start, {{_, _, _}, {_, _, _}} = End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	get_range(Log, Start, Seconds * 1000 + 999);
get_range(Log, Start, End) when is_integer(Start), is_integer(End) ->
	get_range(Log, Start, End, start).

-spec dump_file(Log, FileName) -> Result
	when
		Log :: disk_log:log(),
		FileName :: file:filename(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write all logged events to a file.
%%
dump_file(Log, FileName) when is_list(FileName) ->
	case file:open(FileName, [write]) of
		{ok, IoDevice} ->
			file_chunk(Log, IoDevice, tuple, start);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			{error, Reason}
	end.

-spec http_file(Log, FileName) -> Result
	when
		Log :: transfer | error | security,
		FileName :: file:filename(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write events logged by `httpd' to a file.
%%
http_file(LogType, FileName) when is_atom(LogType), is_list(FileName) ->
	Log = httpd_logname(LogType),
	case file:open(FileName, [raw, write]) of
		{ok, IoDevice} ->
			file_chunk(Log, IoDevice, binary, start);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			{error, Reason}
	end.

-spec httpd_logname(LogType) -> disk_log:log()
	when
		LogType :: transfer | error | security.
%% @doc Find local name of {@link //inets/httpd. httpd} disk_log.
%%
httpd_logname(Log) ->
	{ok, Services} = application:get_env(inets, services),
	{_, HttpdConfig} = lists:keyfind(httpd, 1, Services),
	{_, ServerRoot} = lists:keyfind(server_root, 1, HttpdConfig),
	httpd_logname(Log, ServerRoot, HttpdConfig).
%% @hidden
httpd_logname(transfer, ServerRoot, HttpdConfig) ->
	{_, LogName} = lists:keyfind(transfer_disk_log, 1, HttpdConfig),
	filename:join(ServerRoot, string:strip(LogName));
httpd_logname(error, ServerRoot, HttpdConfig) ->
	{_, LogName} = lists:keyfind(error_disk_log, 1, HttpdConfig),
	filename:join(ServerRoot, string:strip(LogName));
httpd_logname(security, ServerRoot, HttpdConfig) ->
	{_, LogName} = lists:keyfind(security_disk_log, 1, HttpdConfig),
	filename:join(ServerRoot, string:strip(LogName)).

-spec last(Log, MaxItems) -> Result
	when
		Log :: disk_log:log(),
		MaxItems :: pos_integer(),
		Result :: {NumItems, Items} | {error, Reason},
		NumItems :: non_neg_integer(),
		Items :: [term()],
		Reason :: term().
%% @doc Get the last `MaxItems' events in most recent item first order.
last(Log, MaxItems) ->
	case disk_log:chunk_step(Log, start, 0) of
		{error, end_of_log} ->
			{0, []};
		{error, Reason} ->
			{error, Reason};
		{ok, Cont1} ->
			last(Log, MaxItems, Cont1, [Cont1])
	end.
%% @hidden
last(Log, MaxItems, Cont1, [H | _] = Acc) ->
	case disk_log:chunk_step(Log, H, 1) of
		{error, end_of_log} ->
			last1(Log, MaxItems, Acc, {0, []});
		{ok, Cont1} ->
			last1(Log, MaxItems, Acc, {0, []});
		{ok, ContN} ->
			last(Log, MaxItems, Cont1, [ContN | Acc])
	end.
%% @hidden
last1(Log, MaxItems, [Cont | T], _Acc) ->
	case last2(Log, MaxItems, Cont, []) of
		{error, Reason} ->
			{error, Reason};
		{N, Items} when N < MaxItems ->
			last1(Log, MaxItems, T, {N, Items});
		{MaxItems, Items} ->
			{MaxItems, lists:flatten(Items)}
	end;
last1(_Log, _MaxItems, [], {NumItems, Items}) ->
	{NumItems, lists:flatten(Items)}.
%% @hidden
last2(Log, MaxItems, Cont, Acc) ->
	case disk_log:bchunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		eof ->
			last3(Log, MaxItems, Acc, 0, []);
		{Cont1, _Chunk} ->
			last2(Log, MaxItems, Cont1, [Cont | Acc])
	end.
%% @hidden
last3(Log, MaxItems, [Cont | T], NumItems, Acc) ->
	case disk_log:chunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		{_, Items} ->
			RevItems = lists:reverse(Items),
			NumNewItems = length(RevItems),
			case NumItems + NumNewItems of
				MaxItems ->
					NewAcc = [RevItems | Acc],
					{MaxItems, lists:reverse(NewAcc)};
				N when N > MaxItems ->
					NumHead = MaxItems - NumItems,
					{NewItems, _} = lists:split(NumHead, RevItems),
					NewAcc = [NewItems | Acc],
					{MaxItems, lists:reverse(NewAcc)};
				N ->
					NewAcc = [RevItems | Acc],
					last3(Log, MaxItems, T, N, NewAcc)
			end
	end;
last3(_Log, _MaxItems, [], NumItems, Acc) ->
	{NumItems, lists:reverse(Acc)}.

-spec date(MilliSeconds) -> Result
	when
		MilliSeconds :: pos_integer(),
		Result :: calendar:datetime().
%% @doc Convert timestamp to date and time.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds).

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds :: pos_integer(),
		Result :: string().
%% @doc Convert timestamp to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars).

uuid() ->
	<<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
	Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
	Values = [A, B, (C bsr 4) bor 16#4000, (D bsr 2) bor 16#8000, E],
	Chars = io_lib:fwrite(Format, Values),
	lists:flatten(Chars).


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
file_chunk(Log, IoDevice, Type, Cont) ->
	case disk_log:chunk(Log, Cont) of
		eof ->
			file:close(IoDevice);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			{error, Reason};
		{NextCont, Terms} ->
			file_chunk1(Log, IoDevice, Type, NextCont, Terms)
	end.
%% @hidden
file_chunk1(Log, IoDevice, tuple, Cont, [Event | T]) ->
	io:fwrite(IoDevice, "~999p~n", [Event]),
	file_chunk1(Log, IoDevice, tuple, Cont, T);
file_chunk1(Log, IoDevice, binary, Cont, [Event | T]) ->
	case file:write(IoDevice, Event) of
		ok ->
			file_chunk1(Log, IoDevice, binary, Cont, T);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			{error, Reason}
	end;
file_chunk1(Log, IoDevice, Type, Cont, []) ->
	file_chunk(Log, IoDevice, Type, Cont).

-spec start_binary_tree(Log, Start, End) -> Result
	when
		Log :: disk_log:log(),
		Start :: pos_integer(),
		End :: pos_integer(),
		Result :: eof | disk_log:continuation() | {error, Reason},
		Reason :: term().
%% @doc Binary tree search of multi file wrap disk_log.
%% @private
%% @hidden
start_binary_tree(Log, Start, _End) ->
	InfoList = disk_log:info(Log),
	{size, {_MaxBytes, MaxFiles}} = lists:keyfind(size, 1, InfoList),
	StartStep = MaxFiles div 2,
	start_binary_tree(Log, Start, MaxFiles, start, 0, StartStep, StartStep).
%% @hidden
start_binary_tree(_Log, _Start, NumFiles,
		LastCont, _LastStep, _StepSize, NumFiles) ->
	LastCont;
start_binary_tree(_Log, _Start, _NumFiles,
		_LastCont, _LastStep, _StepSize, -1) ->
	eof;
start_binary_tree(Log, Start, NumFiles, LastCont, LastStep, StepSize, Step) ->
	case disk_log:chunk_step(Log, start, Step) of
		{ok, NewCont} ->
			start_binary_tree(Log, Start, NumFiles, LastCont, LastStep,
					StepSize, Step, NewCont, disk_log:chunk(Log, NewCont, 1));
		{error, end_of_log} ->
			LastCont;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start_binary_tree(_Log, Start, _NumFiles, _LastCont, LastStep, 1,
		Step, NewCont, {_, [R]}) when element(1, R) < Start,
		LastStep == (Step + 1) ->
	NewCont;
start_binary_tree(Log, Start, NumFiles, _LastCont, _LastStep, 1,
		Step, NewCont, {_, [R]}) when element(1, R) < Start ->
	start_binary_tree(Log, Start, NumFiles, NewCont, Step, 1, Step + 1);
start_binary_tree(Log, Start, NumFiles, _LastCont, _LastStep, StepSize,
		Step, NewCont, {_, [R]}) when element(1, R) < Start ->
	NewStepSize = StepSize div 2,
	start_binary_tree(Log, Start, NumFiles, NewCont, Step,
			NewStepSize, Step + NewStepSize);
start_binary_tree(_Log, Start, _NumFiles, LastCont, LastStep, 1,
		Step, _NewCont, {_, [R]}) when element(1, R) >= Start,
		LastStep == (Step - 1) ->
	LastCont;
start_binary_tree(Log, Start, NumFiles, _LastCont, _LastStep, 1,
		Step, NewCont, {_, [R]}) when element(1, R) >= Start ->
	start_binary_tree(Log, Start, NumFiles, NewCont, Step, 1, Step - 1);
start_binary_tree(Log, Start, NumFiles, _LastCont, _LastStep, StepSize,
		Step, NewCont, {_, [R]}) when element(1, R) >= Start ->
	NewStepSize = StepSize div 2,
	start_binary_tree(Log, Start, NumFiles, NewCont, Step,
			NewStepSize, Step - NewStepSize);
start_binary_tree(_, _, _, _, _, _, _, _, {error, Reason}) ->
	{error, Reason}.

-spec get_range(Log, Start, End, Cont) -> Result
	when
		Log :: disk_log:log(),
		Start :: pos_integer(),
		End :: pos_integer(),
		Cont :: start | disk_log:continuation(),
		Result :: [term()].
%% @doc Sequentially read 64KB chunks.
%%
%% 	Filters out records before `Start' and after `End'.
%% 	Returns filtered records.
%% @private
%% @hidden
get_range(Log, Start, End, Cont) ->
	get_range(Log, Start, End, [], disk_log:bchunk(Log, Cont)).
%% @hidden
get_range(_Log, _Start, _End, _PrevChunk, {error, Reason}) ->
	{error, Reason};
get_range(Log, Start, End, PrevChunk, eof) ->
	Chunk = [binary_to_term(E) || E <- PrevChunk],
	get_range1(Log, Start, End, {eof, Chunk}, []);
get_range(Log, Start, End, PrevChunk, {Cont, [H | T] = Chunk}) ->
	case binary_to_term(H) of
		Event when element(1, Event) < Start ->
			get_range(Log, Start, End, T, disk_log:bchunk(Log, Cont));
		_Event ->
			NewChunk = [binary_to_term(E) || E <- PrevChunk ++ Chunk],
			get_range1(Log, Start, End, {Cont, NewChunk}, [])
	end.
%% @hidden
get_range1(Log, Start, End, {Cont, Chunk}, Acc) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	NewChunk = lists:dropwhile(Fstart, Chunk),
	get_range2(Log, End, {Cont, NewChunk}, Acc).
get_range2(Log, End, {Cont, Chunk}, Acc) ->
	Fend = fun(R) when element(1, R) =< End ->
				true;
			(_) ->
				false
	end,
	case {Cont, lists:last(Chunk)} of
		{eof, R} when element(1, R) =< End ->
			lists:flatten(lists:reverse([Chunk | Acc]));
		{Cont, R} when element(1, R) =< End ->
			get_range2(Log, End, disk_log:chunk(Log, Cont), [Chunk | Acc]);
		{_, _} ->
			lists:flatten(lists:reverse([lists:takewhile(Fend, Chunk) | Acc]))
	end.

-spec ipdr_codec({TimeStamp, Protocol, Node, Server, RequestType, Attributes}) -> IPDR
	when
		TimeStamp :: pos_integer(),
		Protocol :: radius | diameter,
		Node :: node(),
		Server :: {Address, Port},
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		RequestType :: stop,
		Attributes :: radius_attributes:attributes() | #diameter_cc_app_CCR{},
		IPDR :: #ipdr{}.
%% @doc Convert `ocs_acct' log entry to IPDR log entry.
%% @private
ipdr_codec({TimeStamp, Protocol, _Node, _Server, RequestType, Attributes})
		when ((Protocol == radius) or (Protocol == diameter)),
		((RequestType == stop) or (RequestType == event)) ->
	IPDR = #ipdr{ipdrCreationTime = iso8601(TimeStamp)},
	ipdr_codec1(TimeStamp, Protocol, RequestType, Attributes, IPDR).
%% @hidden
ipdr_codec1(TimeStamp, radius, stop, Attributes, Acc) ->
	case radius_attributes:find(?AcctDelayTime, Attributes) of
		{ok, DelayTime} ->
			EndTime = TimeStamp - (DelayTime * 1000),
			ipdr_codec2(EndTime, radius, stop, Attributes,
					Acc#ipdr{gmtSessionEndDateTime = iso8601(EndTime)});
		{error, not_found} ->
			ipdr_codec2(TimeStamp, radius, stop, Attributes,
					Acc#ipdr{gmtSessionEndDateTime = iso8601(TimeStamp)})
	end.
%% @hidden
ipdr_codec2(EndTime, radius, stop, Attributes, Acc) ->
	case radius_attributes:find(?AcctSessionTime, Attributes) of
		{ok, Duration} ->
			StartTime = EndTime - (Duration * 1000),
			ipdr_codec3(Attributes, radius, stop,
					Acc#ipdr{gmtSessionStartDateTime = iso8601(StartTime)});
		{error, not_found} ->
			ipdr_codec3(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec3(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?UserName, Attributes) of
		{ok, UserName} ->
			ipdr_codec4(Attributes, radius, stop, Acc#ipdr{username = UserName});
		{error, not_found} ->
			ipdr_codec4(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec4(Attributes, radius, stop, Acc) ->
	SessionID = radius_attributes:fetch(?AcctSessionId, Attributes),
	ipdr_codec5(Attributes, radius, stop, Acc#ipdr{acctSessionId = SessionID}).
%% @hidden
ipdr_codec5(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?FramedIpAddress, Attributes) of
		{ok, Address} ->
			ipdr_codec6(Attributes, radius, stop, Acc#ipdr{userIpAddress = inet:ntoa(Address)});
		{error, not_found} ->
			ipdr_codec6(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec6(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?CallingStationId, Attributes) of
		{ok, StationID} ->
			ipdr_codec7(Attributes, radius, stop, Acc#ipdr{callingStationId = StationID});
		{error, not_found} ->
			ipdr_codec7(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec7(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?CalledStationId, Attributes) of
		{ok, StationID} ->
			ipdr_codec8(Attributes, radius, stop, Acc#ipdr{calledStationId = StationID});
		{error, not_found} ->
			ipdr_codec8(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec8(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?NasIpAddress, Attributes) of
		{ok, Address} ->
			ipdr_codec9(Attributes, radius, stop, Acc#ipdr{nasIpAddress = inet:ntoa(Address)});
		{error, not_found} ->
			ipdr_codec9(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec9(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?NasIdentifier, Attributes) of
		{ok, Identifier} ->
			ipdr_codec10(Attributes, radius, stop, Acc#ipdr{nasId = Identifier});
		{error, not_found} ->
			ipdr_codec10(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec10(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?AcctSessionTime, Attributes) of
		{ok, SessionTime} ->
			ipdr_codec11(Attributes, radius, stop, Acc#ipdr{sessionDuration = SessionTime});
		{error, not_found} ->
			ipdr_codec11(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec11(Attributes, radius, stop, Acc) ->
	Octets = radius_attributes:fetch(?AcctInputOctets, Attributes),
	case radius_attributes:find(?AcctInputGigawords, Attributes) of
		{ok, GigaWords} ->
			GigaOctets = (GigaWords * (16#ffffffff + 1)) + Octets,
			ipdr_codec12(Attributes, radius, stop, Acc#ipdr{inputOctets = GigaOctets});
		{error, not_found} ->
			ipdr_codec12(Attributes, radius, stop, Acc#ipdr{inputOctets = Octets})
	end.
%% @hidden
ipdr_codec12(Attributes, radius, stop, Acc) ->
	Octets = radius_attributes:fetch(?AcctOutputOctets, Attributes),
	case radius_attributes:find(?AcctOutputGigawords, Attributes) of
		{ok, GigaWords} ->
			GigaOctets = (GigaWords * (16#ffffffff + 1)) + Octets,
			ipdr_codec13(Attributes, radius, stop, Acc#ipdr{outputOctets = GigaOctets});
		{error, not_found} ->
			ipdr_codec13(Attributes, radius, stop, Acc#ipdr{outputOctets = Octets})
	end.
%% @hidden
ipdr_codec13(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?Class, Attributes) of
		{ok, Class} ->
			ipdr_codec14(Attributes, radius, stop, Acc#ipdr{class = Class});
		{error, not_found} ->
			ipdr_codec14(Attributes, radius, stop, Acc)
	end.
%% @hidden
ipdr_codec14(Attributes, radius, stop, Acc) ->
	case radius_attributes:find(?AcctTerminateCause, Attributes) of
		{ok, Cause} ->
			Acc#ipdr{sessionTerminateCause = Cause};
		{error, not_found} ->
			Acc
	end.

%% @hidden
ipdr_xml(Log, IoDevice, {Cont, [#ipdrDoc{} = _I | T]}) ->
	Header = [],
	case file:write(IoDevice, Header) of
		ok ->
			ipdr_xml(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xml(Log, IoDevice, {Cont, [#ipdr{} = _I | T]}) ->
	IPDR = <<>>,
	case file:write(IoDevice, IPDR) of
		ok ->
			ipdr_xml(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xml(Log, IoDevice, {Cont, [#ipdrDocEnd{}]}) ->
	Trailer = <<>>,
	case file:write(IoDevice, Trailer) of
		ok ->
			ipdr_file3(Log, IoDevice, xml, {Cont, []});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xml(Log, IoDevice, {Cont, []}) ->
	ipdr_file3(Log, IoDevice, xml, {Cont, []}).

%% @hidden
ipdr_xdr(Log, IoDevice, {Cont, [#ipdrDoc{} = _I | T]}) ->
	Header = [<<>>],
	case file:write(IoDevice, Header) of
		ok ->
			ipdr_xdr(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xdr(Log, IoDevice, {Cont, [#ipdr{} = _I | T]}) ->
	IPDR = <<>>,
	case file:write(IoDevice, IPDR) of
		ok ->
			ipdr_xdr(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xdr(Log, IoDevice, {Cont, [#ipdrDocEnd{}]}) ->
	Trailer = <<>>,
	case file:write(IoDevice, Trailer) of
		ok ->
			ipdr_file3(Log, IoDevice, xdr, {Cont, []});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xdr(Log, IoDevice, {Cont, [_ | T]}) ->
	ipdr_xdr(Log, IoDevice, {Cont, T});
ipdr_xdr(Log, IoDevice, {Cont, []}) ->
	ipdr_file3(Log, IoDevice, xdr, {Cont, []}).

%% @hidden
ipdr_csv(Log, IoDevice, {Cont, [#ipdrDoc{} | T]}) ->
	Header = [<<"Creation Time;Sequence Number;Username;">>,
			<<"Accounting Session ID;User IP Address;Calling Station ID;">>,
			<<"Called Station ID;NAS IP Address;NAS Identifier;">>,
			<<"Session Duration;Input Octets;Output Octets;">>,
			<<"Class;Session Terminate Cause">>, $\r, $\n],
	case file:write(IoDevice, Header) of
		ok ->
			ipdr_csv(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, {Cont, [#ipdr{} = I | T]}) ->
	Time = list_to_binary(I#ipdr.ipdrCreationTime),
	Seq = integer_to_binary(I#ipdr.seqNum),
	User = case I#ipdr.username of
		undefined ->
			<<>>;
		US ->
			list_to_binary(US)
	end,
	Sess = case I#ipdr.acctSessionId of
		undefined ->
			<<>>;
		SI ->
			list_to_binary(SI)
	end,
	IP = case I#ipdr.userIpAddress of
		undefined ->
			<<>>;
		IpAddress ->
			list_to_binary(IpAddress)
	end,
	Calling = case I#ipdr.callingStationId of
		undefined ->
			<<>>;
		CgID ->
			list_to_binary(CgID)
	end,
	Called = case I#ipdr.calledStationId of
		undefined ->
			<<>>;
		CdID ->
			list_to_binary(CdID)
	end,
	NasIP = case I#ipdr.nasIpAddress of
		undefined ->
			<<>>;
		NIP ->
			list_to_binary(NIP)
	end,
	NasID = case I#ipdr.nasId of
		undefined ->
			<<>>;
		NID ->
			list_to_binary(NID)
	end,
	Duration = case I#ipdr.sessionDuration of
		undefined ->
			<<>>;
		DU ->
			integer_to_binary(DU)
	end,
	Input = case I#ipdr.inputOctets of
		undefined ->
			<<>>;
		IN ->
			integer_to_binary(IN)
	end,
	Output = case I#ipdr.outputOctets of
		undefined ->
			<<>>;
		OUT ->
			integer_to_binary(OUT)
	end,
	Class = case I#ipdr.class of
		undefined ->
			<<>>;
		CLS ->
			list_to_binary(CLS)
	end,
	Cause = case I#ipdr.sessionTerminateCause of
		undefined ->
			<<>>;
		CS ->
			integer_to_binary(CS)
	end,
	IPDR = [Time, $;, Seq, $;, User, $;, Sess, $;, IP, $;, Calling, $;,
			Called, $;, NasIP, $;, NasID, $;, Duration, $;,
			Input, $;, Output, $;, Class, $;, Cause, $\r, $\n],
	case file:write(IoDevice, IPDR) of
		ok ->
			ipdr_csv(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, {Cont, [#ipdrDocEnd{}]}) ->
	ipdr_file3(Log, IoDevice, csv, {Cont, []});
ipdr_csv(Log, IoDevice, {Cont, []}) ->
	ipdr_file3(Log, IoDevice, csv, {Cont, []}).

