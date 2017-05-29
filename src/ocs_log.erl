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
-export([acct_open/0, acct_log/4, acct_close/0]).
-export([auth_open/0, auth_log/6, auth_log/7, auth_close/0, auth_query/5]).
-export([ipdr_log/3, ipdr_file/2, get_range/3, last/2]).
-export([dump_file/2, http_file/2, httpd_logname/1]).
-export([date/1, iso8601/1]).

%% export the ocs_log private API
-export([]).

-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include("../include/diameter_gen_cc_application_rfc4006.hrl").

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
	Event = {TS, Protocol, node(), Server, Type, Attributes},
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
	Event = {TS, Protocol, node(), Server, Client, Type,
			RequestAttributes, ResponseAttributes},
	disk_log:log(?AUTHLOG, Event).

-spec auth_log(Protocol, Server, Subscriber, OriginHost, OriginRealm, AuthType,
		ResultCode) -> Result
	when
		Protocol :: diameter,
		Server :: {Address, Port},
		Subscriber :: string() | binary(),
		Address :: inet:ip_address(),
		Port :: integer(),
		OriginHost :: term(),
		OriginRealm :: term(),
		AuthType :: integer(),
		ResultCode:: integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write a DIAMETER AAR event to authorization log.
auth_log(Protocol, Server, Subscriber, OriginHost, OriginRealm, AuthType,
		ResultCode) ->
	TS = erlang:system_time(?MILLISECOND),
	Event = {TS, Protocol, node(), Server, Subscriber, OriginHost, OriginRealm,
			AuthType, ResultCode},
	disk_log:log(?AUTHLOG, Event).

-spec auth_query(Start, End, Types, ReqAttrsMatch, RespAttrsMatch) -> Result
	when
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Types :: [Type],
		Type :: accept | reject | change,
		ReqAttrsMatch :: [{Attribute, Match}],
		RespAttrsMatch :: [{Attribute, Match}],
		Attribute :: byte(),
		Match :: term() | '_',
		Result :: [term()].
%% @doc Query RADIUS access request events with filters.
%%
%% 	Events before `Start' or after `Stop' or which do not match one of
%% 	the `Types' are ignored.
%%
%% 	Events which do not include `Attribute' in request or response
%% 	attributes are ignored. If `Match' is `'_'' any attribute value
%% 	will match, otherwise events with attributes having a value not
%% 	equal to `Match' will be ignored. All attribute filters must
%% 	match or the event will be ignored.
%%
%% 	Returns a list of matching authentication events.
%%
auth_query({{_, _, _}, {_, _, _}} = Start, End, Types,
		ReqAttrsMatch, RespAttrsMatch) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	auth_query(Seconds * 1000, End, Types, ReqAttrsMatch, RespAttrsMatch);
auth_query(Start, {{_, _, _}, {_, _, _}} = End, Types,
		ReqAttrsMatch, RespAttrsMatch) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	auth_query(Start, Seconds * 1000, Types, ReqAttrsMatch, RespAttrsMatch);
auth_query(Start, End, Types, ReqAttrsMatch, RespAttrsMatch)
		when is_integer(Start), is_integer(End) ->
	auth_query(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
			disk_log:chunk(?AUTHLOG, start), []).

%% @hidden
auth_query(_Start, _End, _Types, _ReqAttrsMatch,
		_RespAttrsMatch, eof, Acc) ->
	lists:reverse(Acc);
auth_query(_Start, _End, _Types, _ReqAttrsMatch,
		_RespAttrsMatch, {error, Reason}, _Acc) ->
	{error, Reason};
auth_query(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{TS,_,_,_,Type,_,_} | T] = Chunk}, Acc)
		when TS >= Start, TS =< End ->
	case lists:member(Type, Types) of
		true ->
			auth_query1(Start, End, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, ReqAttrsMatch);
		false ->
			auth_query(Start, End, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, T}, Acc)
	end;
auth_query(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [_ | T]}, Acc) ->
	auth_query(Start, End, Types, ReqAttrsMatch,
			RespAttrsMatch, {Cont, T}, Acc);
auth_query(Start, End, Types, ReqAttrsMatch,
		RespAttrsMatch, {Cont, []}, Acc) ->
	auth_query(Start, End, Types, ReqAttrsMatch,
			RespAttrsMatch, disk_log:chunk(?AUTHLOG, Cont), Acc).

%% @hidden
auth_query1(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
		{Cont, [{_,_,_,_,_,ReqAttrs,_} | T] = Chunk},
		Acc, [{Attribute, Match} | T1]) ->
	case lists:keyfind(Attribute, 1, ReqAttrs) of
		{Attribute, Match} ->
			auth_query1(Start, End, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, T1);
		{Attribute, _} when Match == '_' ->
			auth_query1(Start, End, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, Chunk}, Acc, T1);
		_ ->
			auth_query(Start, End, Types, ReqAttrsMatch,
					RespAttrsMatch, {Cont, T}, Acc)
	end;
auth_query1(Start, End, Types, ReqAttrsMatch,
		RespAttrsMatch, {Cont, Chunk}, Acc, []) ->
	auth_query2(Start, End, Types, ReqAttrsMatch,
			RespAttrsMatch, {Cont, Chunk}, Acc, RespAttrsMatch).

%% @hidden
auth_query2(Start, End, Types, ReqAttrsMatch, RespAttrsMatch, {Cont,
[{_,_,_,_,_,_,RespAttrs} | T] = Chunk}, Acc, [{Attribute, Match} | T1]) ->
	case lists:keyfind(Attribute, 1, RespAttrs) of
		{Attribute, Match} ->
			auth_query2(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
{Cont, Chunk}, Acc, T1);
		{Attribute, _} when Match == '_' ->
			auth_query2(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
{Cont, Chunk}, Acc, T1);
		false ->
			auth_query(Start, End, Types, ReqAttrsMatch, RespAttrsMatch,
{Cont, T}, Acc)
	end;
auth_query2(Start, End, Types, ReqAttrsMatch,
		RespAttrsMatch, {Cont, [H | T]}, Acc, []) ->
	auth_query(Start, End, Types, ReqAttrsMatch,
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
	ipdr_log(File, Start, Seconds * 1000);
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
	get_range(Log, Start, Seconds * 1000);
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
		Result :: {NumItems, Events} | {error, Reason},
		NumItems :: non_neg_integer(),
		Events :: [term()],
		Reason :: term().
%% @doc Get the last `MaxItems' events in most recent item first order.
last(Log, MaxItems) ->
	last(Log, MaxItems, start, []).
%% @hidden
last(Log, MaxItems, Cont, Acc) ->
	case disk_log:chunk_step(Log, Cont, 1) of
		{error, end_of_log} when Acc == [] ->
			last1(Log, MaxItems, 0, [start], []);
		{error, end_of_log} ->
			last1(Log, MaxItems, 0, Acc, []);
		{ok, Cont1} ->
			last(Log, MaxItems, Cont1, [Cont | Acc])
	end.
%% @hidden
last1(Log, MaxItems, NumItems, [Cont | T], Acc) ->
	case last2(Log, MaxItems, NumItems, Cont, []) of
		{error, Reason} ->
			{error, Reason};
		{MaxItems, Events} ->
			NewAcc = [Events | Acc],
			{MaxItems, lists:flatten(lists:reverse(NewAcc))};
		{NewNumItems, Events} ->
			NewAcc = [Events | Acc],
			last1(Log, MaxItems, NewNumItems, T, NewAcc)
	end;
last1(_Log, _MaxItems, NumItems, [], Acc) ->
	{NumItems, lists:flatten(lists:reverse(Acc))}.
%% @hidden
last2(Log, MaxItems, NumItems, Cont, Acc) ->
	case disk_log:chunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		eof ->
			last3(Log, MaxItems, NumItems, Acc, []);
		{Cont1, _Events} ->
			last2(Log, MaxItems, NumItems, Cont1, [Cont | Acc])
	end.
%% @hidden
last3(Log, MaxItems, NumItems, [Cont | T], Acc) ->
	case disk_log:chunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		{_, Events} ->
			RevEvents = lists:reverse(Events),
			NumEvents = length(RevEvents),
			case NumItems + NumEvents of
				MaxItems ->
					NewAcc = [RevEvents | Acc],
					{MaxItems, lists:reverse(NewAcc)};
				N when N > MaxItems ->
					NumHead = MaxItems - NumItems,
					{NewEvents, _} = lists:split(NumHead, RevEvents),
					NewAcc = [NewEvents | Acc],
					{MaxItems, lists:reverse(NewAcc)};
				N ->
					NewAcc = [RevEvents | Acc],
					last3(Log, MaxItems, N, T, NewAcc)
			end
	end;
last3(_Log, _MaxItems, NumItems, [], Acc) ->
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
	get_range(Log, Start, End, [], disk_log:chunk(Log, Cont)).
%% @hidden
get_range(_Log, _Start, _End, _PrevChunk, {error, Reason}) ->
	{error, Reason};
get_range(Log, Start, End, PrevChunk, eof) ->
	get_range1(Log, Start, End, {eof, PrevChunk}, []);
get_range(Log, Start, End, _PrevChunk, {Cont, [H | T]})
		when element(1, H) < Start ->
	get_range(Log, Start, End, T, disk_log:chunk(Log, Cont));
get_range(Log, Start, End, PrevChunk, {Cont, Chunk}) ->
	get_range1(Log, Start, End, {Cont, PrevChunk ++ Chunk}, []).
%% @hidden
get_range1(Log, Start, End, {Cont, Chunk}, Acc) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	NewChunk = lists:dropwhile(Fstart, Chunk),
	get_range2(Log, End, {Cont, NewChunk}, Acc).
%% @hidden
get_range2(_Log, _End, eof, Acc) ->
	lists:flatten(lists:reverse(Acc));
get_range2(_Log, _End, {error, Reason}, _Acc) ->
	{error, Reason};
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
ipdr_xml(Log, IoDevice, {Cont, [#ipdrDoc{} = I | T]}) ->
	Header = [],
	case file:write_file(IoDevice, Header) of
		ok ->
			ipdr_xml(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xml(Log, IoDevice, {Cont, [#ipdr{} = I | T]}) ->
	IPDR = <<>>,
	case file:write_file(IoDevice, IPDR) of
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
ipdr_xdr(Log, IoDevice, {Cont, [#ipdrDoc{} = I | T]}) ->
	Header = [],
	case file:write_file(IoDevice, Header) of
		ok ->
			ipdr_xdr(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_xdr(Log, IoDevice, {Cont, [#ipdr{} = I | T]}) ->
	IPDR = <<>>,
	case file:write_file(IoDevice, IPDR) of
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
	case file:write_file(IoDevice, Trailer) of
		ok ->
			ipdr_file3(Log, IoDevice, xdr, {Cont, []});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
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

