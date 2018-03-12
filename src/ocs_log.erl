%%% ocs_log.erl
%%% vim: ts=3
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
-export([acct_open/0, acct_log/6, acct_close/0,
		acct_query/5, acct_query/6]).
-export([auth_open/0, auth_log/5, auth_log/6, auth_close/0,
			auth_query/6, auth_query/7]).
-export([ipdr_log/3, ipdr_file/2, ipdr_query/5]).
-export([abmf_open/0, abmf_log/15,
			abmf_query/8]).
-export([get_range/3, last/2, dump_file/2, httpd_logname/1,
			http_file/2, date/1, iso8601/1]).
-export([http_query/8]).

%% exported the private function
-export([acct_query/4, ipdr_query/2, auth_query/5, abmf_query/6]).

%% export the ocs_log event types
-export_type([auth_event/0, acct_event/0, http_event/0]).

-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").

-define(ACCTLOG, ocs_acct).
-define(AUTHLOG, ocs_auth).
-define(BALANCELOG, ocs_abmf).

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
	{ok, LogSize} = application:get_env(ocs, acct_log_size),
	{ok, LogFiles} = application:get_env(ocs, acct_log_files),
	open_log(Directory, ?ACCTLOG, LogSize, LogFiles).

-spec acct_log(Protocol, Server, Type, Request, Response, Rated) -> Result
	when
		Protocol :: diameter | radius,
		Server :: {Address, Port},
		Address :: inet:ip_address(),
		Port :: integer(),
		Type :: on | off | start | stop | update | interim | final,
		Request :: #'3gpp_ro_CCR'{} | #'3gpp_ro_RAR'{}
				| radius_attributes:attributes(),
		Response :: #'3gpp_ro_CCA'{} | #'3gpp_ro_RAA'{}
				| radius_attributes:attributes() | undefined,
		Rated :: [#rated{}] | undefined,
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write an event to accounting log.
acct_log(Protocol, Server, Type, Request, Response, Rated) ->
	Event = [Protocol, node(), Server, Type, Request, Response, Rated],
	write_log(?ACCTLOG, Event).

-spec acct_close() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Close accounting disk log.
acct_close() ->
	close_log(?ACCTLOG).

-spec acct_query(Continuation, Start, End, Types, AttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Types :: [Type] | '_',
		Type :: on | off | start | stop | interim | event,
		AttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]} | '_',
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
		Match :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]} | '_',
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
%% 	If `Match' is '_' any attribute value will match or
%% 	`{Operator, MatchValue}' may be used for more complex queries.
%%
%% 	All attribute filters must match or the event will be ignored.
%%
%% 	`Protocol', `Types', or `AttrsMatch' may be '_' which matches any value.
%%
%% 	Returns a new `Continuation' and a list of matching accounting events.
%% 	Successive calls use the new `Continuation' to read more events.
%%
acct_query(Continuation, Start, End, Protocol, Types, AttrsMatch) ->
	MFA = {?MODULE, acct_query, [Protocol, Types, AttrsMatch]},
	query_log(Continuation, Start, End, ?ACCTLOG, MFA).

-spec auth_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Open authorization event disk log.
auth_open() ->
	{ok, Directory} = application:get_env(ocs, auth_log_dir),
	{ok, LogSize} = application:get_env(ocs, auth_log_size),
	{ok, LogFiles} = application:get_env(ocs, auth_log_files),
	open_log(Directory, ?AUTHLOG, LogSize, LogFiles).

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
	Event = [Protocol, node(), Server, Client, Type,
			RequestAttributes, ResponseAttributes],
	write_log(?AUTHLOG, Event).

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
	Event = [Protocol, node(), Server, Client, Request, Response],
	write_log(?AUTHLOG, Event).

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
		Match :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]} | '_',
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
		Match :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]} | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [auth_event()],
		Reason :: term().
%% @doc Query access log events with filters.
%%
%% 	The first time called `Continuation' should have the value `start'.
%%
%% 	Events before `Start' or after `Stop' or which do not match
%% 	`Protocol' or one of the `Types' are ignored.
%%
%% 	Events which do not include `Attribute' in request or response
%% 	attributes are ignored. If `Match' is '_' any attribute value
%% 	will match or `{Operator, MatchValue}' may be used for more
%% 	complex queries.
%%
%% 	All attribute filters must match or the event will be ignored.
%%
%% 	`Protocol', `Types', `ReqAttrsMatch' `ResAttrsMatch'  may be
%% 	'_' which matches any value.
%%
%% 	Returns a new `Continuation' and a list of matching access events.
%% 	Successive calls use the new `Continuation' to read more events.
%%
%%
auth_query(Continuation, Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch) ->
	MFA = {?MODULE, auth_query, [Protocol, Types, ReqAttrsMatch, RespAttrsMatch]},
	query_log(Continuation, Start, End, ?AUTHLOG, MFA).

-spec auth_close() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Close auth disk log.
auth_close() ->
	close_log(?AUTHLOG).

-record(event,
		{host :: string(),
		user :: string() | undefined,
		date :: string() | undefined,
		method :: string() | undefined,
		uri :: string() | undefined,
		httpStatus :: integer() | undefined}).

-spec http_query(Continuation, LogType, DateTime, Host, User, Method, URI, HTTPStatus) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		LogType ::  transfer | error | security,
		DateTime :: '_' | string(),
		Host :: '_' | string(),
		User :: '_' | string(),
		Method :: '_' | string(),
		URI :: '_' | string(),
		HTTPStatus :: '_' | string() | integer(),
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [http_event()],
		Reason :: term().
%% @doc Query http log events with fileters
http_query(start, LogType, DateTime, Host, User, Method, URI, HTTPStatus) ->
	Log = ocs_log:httpd_logname(LogType),
	http_query1(disk_log:chunk(Log, start), Log,
		DateTime, Host, User, Method, URI, HTTPStatus, []);
http_query(Cont, LogType, DateTime, Host, User, Method, URI, HTTPStatus) ->
	Log = ocs_log:httpd_logname(LogType),
	http_query1(disk_log:chunk(Log, Cont), Log,
		DateTime, Host, User, Method, URI, HTTPStatus, []).
%% @hidden
http_query1({error, Reason}, _, _, _, _, _, _, _, _) ->
	{error, Reason};
http_query1(eof, _Log, DateTime, Host, User, Method, URI, HTTPStatus, PrevChunk) ->
	http_query2(lists:flatten(PrevChunk), DateTime, Host, User, Method, URI, HTTPStatus);
http_query1({Cont, Chunk}, Log, DateTime, Host, User, Method, URI, HTTPStatus, PrevChunk) ->
	ParseChunk = lists:map(fun http_parse/1, Chunk),
	CurrentChunk = [ParseChunk | PrevChunk],
	http_query1(disk_log:chunk(Log, Cont), Log, DateTime,
			Host, User, Method, URI, HTTPStatus, CurrentChunk).
%% @hidden
http_query2(Chunks, DateTime, Host, User, Method, URI, '_') ->
	http_query3(Chunks, DateTime, Host, User, Method, URI);
http_query2(Chunks, DateTime, Host, User, Method, URI, HTTPStatus) when is_list(HTTPStatus) ->
	http_query2(Chunks, DateTime, Host, User, Method, URI, list_to_integer(HTTPStatus));
http_query2(Chunks, DateTime, Host, User, Method, URI, HTTPStatus) ->
	F = fun(#event{httpStatus = HS}) when HS =:= HTTPStatus -> true; (_) -> false end,
	http_query3(lists:filtermap(F, Chunks), DateTime, Host, User, Method, URI).
%% @hidden
http_query3(Chunks, DateTime, Host, User, Method, '_') ->
	http_query4(Chunks, DateTime, Host, User, Method);
http_query3(Chunks, DateTime, Host, User, Method, URI) ->
	F = fun(#event{uri = U}) -> lists:prefix(URI, U) end,
	http_query4(lists:filtermap(F, Chunks), DateTime, Host, User, Method).
%% @hidden
http_query4(Chunks, DateTime, Host, User, '_') ->
	http_query5(Chunks, DateTime, Host, User);
http_query4(Chunks, DateTime, Host, User, Method) ->
	F = fun(#event{method = M}) -> lists:prefix(Method, M) end,
	http_query5(lists:filtermap(F, Chunks), DateTime, Host, User).
%% @hidden
http_query5(Chunks, DateTime, Host, '_') ->
	http_query6(Chunks, DateTime, Host);
http_query5(Chunks, DateTime, Host, User) ->
	F = fun(#event{user = U}) -> lists:prefix(User, U) end,
	http_query6(lists:filtermap(F, Chunks), DateTime, Host).
%% @hidden
http_query6(Chunks, DateTime, '_') ->
	http_query7(Chunks, DateTime);
http_query6(Chunks, DateTime, Host) ->
	F = fun(#event{host = H}) -> lists:prefix(Host, H) end,
	http_query7(lists:filtermap(F, Chunks), DateTime).
%% @hidden
http_query7(Chunks, '_') ->
	http_query8(Chunks);
http_query7(Chunks, DateTime) ->
	F = fun(#event{date = D}) -> lists:prefix(DateTime, D) end,
	http_query8(lists:filtermap(F, Chunks)).
%% @hidden
http_query8(Chunks) ->
	F = fun(#event{host = H, user = U, date = D, method = M, uri = URI, httpStatus = S}, Acc) ->
			[{H, U, D, M, URI, S} | Acc]
	end,
	{eof, lists:reverse(lists:foldl(F, [], Chunks))}.

-spec ipdr_query(Continuation, Log, Start, End, AttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Log :: atom(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		AttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]} | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()],
		Reason :: term().
%% @doc Ipdr log query.
ipdr_query(Continuation, Log, Start, End, AttrsMatch) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			{eof, []};
		{Continuation1, Events} ->
			{Continuation1, Events}
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
		when element(6, H) == stop ->
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

-spec iso8601(DateTime) -> DateTime
	when
		DateTime :: pos_integer() | string().
%% @doc Convert between ISO 8601 and Unix epoch milliseconds.
%% 	Parsing is not strict to allow prefix matching.
iso8601(DateTime) when is_integer(DateTime) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(DateTime),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, DateTime rem 1000]),
	lists:flatten(Chars);
iso8601([Y1, Y2, Y3, Y4 | T])
		when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
		Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	iso8601month(list_to_integer([Y1, Y2, Y3, Y4]), T).
%% @hidden
iso8601month(Year, []) ->
	DateTime = {{Year, 1, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601month(Year, [$-]) ->
	iso8601month(Year, []);
iso8601month(Year, [$-, $0]) ->
	iso8601month(Year, [$-, $0, $1]);
iso8601month(Year, [$-, $1]) ->
	iso8601month(Year, [$-, $1, $0]);
iso8601month(Year, [$-, M1, M2 | T])
		when M1 >= $0, M1 =< $1, M2 >= $0, M2 =< $9 ->
	iso8601day(Year, list_to_integer([M1, M2]), T).
%% @hidden
iso8601day(Year, Month, []) ->
	DateTime = {{Year, Month, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601day(Year, Month, [$-]) ->
	iso8601day(Year, Month, []);
iso8601day(Year, Month, [$-, $0]) ->
	iso8601day(Year, Month, [$-, $1, $0]);
iso8601day(Year, Month, [$-, D1])
		when D1 >= $1, D1 =< $3 ->
	iso8601day(Year, Month, [$-, D1, $0]);
iso8601day(Year, Month, [$-, D1, D2 | T])
		when D1 >= $0, D1 =< $3, D2 >= $0, D2 =< $9 ->
	Day = list_to_integer([D1, D2]),
	iso8601hour({Year, Month, Day}, T).
%% @hidden
iso8601hour(Date, []) ->
	DateTime = {Date, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601hour(Date, [$T]) ->
	iso8601hour(Date, []);
iso8601hour(Date, [$T, H1])
		when H1 >= $0, H1 =< $2 ->
	iso8601hour(Date, [$T, H1, $0]);
iso8601hour(Date, [$T, H1, H2 | T])
		when H1 >= $0, H1 =< $2, H2 >= $0, H2 =< $9 ->
	Hour = list_to_integer([H1, H2]),
	iso8601minute(Date, Hour, T).
%% @hidden
iso8601minute(Date, Hour, []) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601minute(Date, Hour, [$:]) ->
	iso8601minute(Date, Hour, []);
iso8601minute(Date, Hour, [$:, M1])
		when M1 >= $0, M1 =< $5 ->
	iso8601minute(Date, Hour, [$:, M1, $0]);
iso8601minute(Date, Hour, [$:, M1, M2 | T])
		when M1 >= $0, M1 =< $5, M2 >= $0, M2 =< $9 ->
	Minute = list_to_integer([M1, M2]),
	iso8601second(Date, Hour, Minute, T);
iso8601minute(Date, Hour, _) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601second(Date, Hour, Minute, []) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601second(Date, Hour, Minute, [$:]) ->
	iso8601second(Date, Hour, Minute, []);
iso8601second(Date, Hour, Minute, [$:, S1])
		when S1 >= $0, S1 =< $5 ->
	iso8601second(Date, Hour, Minute, [$:, S1, $0]);
iso8601second(Date, Hour, Minute, [$:, S1, S2 | T])
		when S1 >= $0, S1 =< $5, S2 >= $0, S2 =< $9 ->
	Second = list_to_integer([S1, S2]),
	DateTime = {Date, {Hour, Minute, Second}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	EpocMilliseconds = (GS - ?EPOCH) * 1000,
	iso8601millisecond(EpocMilliseconds, T);
iso8601second(Date, Hour, Minute, _) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601millisecond(EpocMilliseconds, []) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$.]) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$., N1, N2, N3 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9,
		N3 >= $0, N3 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2, N3]);
iso8601millisecond(EpocMilliseconds, [$., N1, N2 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2]) * 10;
iso8601millisecond(EpocMilliseconds, [$., N | _])
		when N >= $0, N =< $9 ->
	EpocMilliseconds + list_to_integer([N]) * 100;
iso8601millisecond(EpocMilliseconds, _) ->
	EpocMilliseconds.

uuid() ->
	<<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
	Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
	Values = [A, B, (C bsr 4) bor 16#4000, (D bsr 2) bor 16#8000, E],
	Chars = io_lib:fwrite(Format, Values),
	lists:flatten(Chars).

-spec abmf_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Open balance activity event disk log.
abmf_open() ->
	{ok, Directory} = application:get_env(ocs, abmf_log_dir),
	{ok, LogSize} = application:get_env(ocs, abmf_log_size),
	{ok, LogFiles} = application:get_env(ocs, abmf_log_files),
	open_log(Directory, ?BALANCELOG, LogSize, LogFiles).

-spec abmf_log(Type, Subscriber, Bucket, Units, Product, Amount,
		AmountBefore, AmountAfter, Validity, Channel, Requestor,
		RelatedParty, PaymentMeans, Action, Status) -> Result
	when
		Type :: deduct | reserve | unreserve | transfer | topup | adjustment,
		Subscriber :: binary(),
		Bucket :: undefined | string(),
		Units :: cents | seconds | octets,
		Product :: string(),
		Amount :: integer(),
		AmountBefore :: integer(),
		AmountAfter :: integer(),
		Validity :: undefined | pos_integer(),
		Channel :: undefined | string(),
		Requestor :: undefined | [{Id, Role, Name}],
		RelatedParty :: undefined | [{Id, Role, Name}],
		PaymentMeans :: undefined | string(),
		Action :: undefined | string(),
		Status :: undefined | term(),
		Id :: string(),
		Role :: string(),
		Name :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write a balance activity log
abmf_log(Type, Subscriber, Bucket, Units, Product, Amount,
		AmountBefore, AmountAfter, Validity, Channel, Requestor,
		RelatedParty, PaymentMeans, Action, Status)
		when ((Type == transfer) orelse (Type == topup) orelse
		(Type == adjustment) orelse (Type == deduct) orelse (Type == reserve)
		orelse (Type == unreserve)), is_binary(Subscriber), is_list(Bucket),
		((Units == cents) orelse (Units == seconds) orelse (Units == octets)),
		is_integer(AmountBefore), is_integer(AmountAfter), is_list(Product),
		is_integer(Amount)->
	Event = [node(), Type, Subscriber, Bucket, Units, Product, Amount,
			AmountBefore, AmountAfter, Validity, Channel, Requestor,
			RelatedParty, PaymentMeans, Action, Status],
	write_log(?BALANCELOG, Event).

-spec abmf_query(Continuation, Start, End, Type, Subscriber,
		Bucket, Units, Product) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		Type :: deduct | reserve | unreserve | transfer | topup | adjustment,
		Subscriber :: binary() | '_',
		Bucket :: string() | '_',
		Units :: cents | seconds | octets | '_',
		Product :: string() | '_',
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [abmf_event()],
		Reason :: term().
%% @doc Query balance activity log events with filters.
abmf_query(Continuation, Start, End, Type, Subscriber,
		Bucket, Units, Product) ->
	MFA = {?MODULE, abmf_query, [Type, Subscriber, Bucket, Units, Product]},
	query_log(Continuation, Start, End, ?BALANCELOG, MFA).

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

-spec ipdr_codec({TimeStamp, N, Protocol, Node, Server, RequestType, Attributes}) -> IPDR
	when
		TimeStamp :: pos_integer(),
		N :: pos_integer(),
		Protocol :: radius | diameter,
		Node :: node(),
		Server :: {Address, Port},
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		RequestType :: stop,
		Attributes :: radius_attributes:attributes() | #'3gpp_ro_CCR'{},
		IPDR :: #ipdr{}.
%% @doc Convert `ocs_acct' log entry to IPDR log entry.
%% @private
ipdr_codec(Event) when size(Event) > 6,
		((element(3, Event) == radius
		andalso element(6, Event) == stop)
		orelse (element(3, Event) == diameter
		andalso element(6, Event) == final)) ->
	TimeStamp = element(1, Event),
	Protocol = element(3, Event),
	RequestType = element(6, Event),
	ReqAttrs = element(7, Event),
	RespAttrs = case size(Event) > 7 of
		true ->
			element(8, Event);
		false ->
			undefined
	end,
	Rated = case size(Event) > 8 of
		true ->
			element(9, Event);
		false ->
			undefined
	end,
	IPDR = #ipdr{ipdrCreationTime = iso8601(TimeStamp)},
	ipdr_codec1(TimeStamp, Protocol,
			RequestType, ReqAttrs, RespAttrs, Rated, IPDR).
%% @hidden
ipdr_codec1(TimeStamp, radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?AcctDelayTime, ReqAttrs) of
		{ok, DelayTime} ->
			EndTime = TimeStamp - (DelayTime * 1000),
			ipdr_codec2(EndTime, radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{gmtSessionEndDateTime = iso8601(EndTime)});
		{error, not_found} ->
			ipdr_codec2(TimeStamp, radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{gmtSessionEndDateTime = iso8601(TimeStamp)})
	end.
%% @hidden
ipdr_codec2(EndTime, radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?AcctSessionTime, ReqAttrs) of
		{ok, Duration} ->
			StartTime = EndTime - (Duration * 1000),
			ipdr_codec3(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{gmtSessionStartDateTime = iso8601(StartTime)});
		{error, not_found} ->
			ipdr_codec3(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec3(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?UserName, ReqAttrs) of
		{ok, UserName} ->
			ipdr_codec4(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{username = UserName});
		{error, not_found} ->
			ipdr_codec4(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec4(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?AcctSessionId, ReqAttrs) of
		{ok, SessionID} ->
			ipdr_codec5(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{acctSessionId = SessionID});
		{error, not_found} ->
			ipdr_codec5(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec5(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?FramedIpAddress, ReqAttrs) of
		{ok, Address} ->
			ipdr_codec6(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{userIpAddress = inet:ntoa(Address)});
		{error, not_found} ->
			ipdr_codec6(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec6(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?CallingStationId, ReqAttrs) of
		{ok, StationID} ->
			ipdr_codec7(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{callingStationId = StationID});
		{error, not_found} ->
			ipdr_codec7(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec7(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?CalledStationId, ReqAttrs) of
		{ok, StationID} ->
			ipdr_codec8(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{calledStationId = StationID});
		{error, not_found} ->
			ipdr_codec8(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec8(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?NasIpAddress, ReqAttrs) of
		{ok, Address} ->
			ipdr_codec9(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{nasIpAddress = inet:ntoa(Address)});
		{error, not_found} ->
			ipdr_codec9(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec9(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?NasIdentifier, ReqAttrs) of
		{ok, Identifier} ->
			ipdr_codec10(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{nasId = Identifier});
		{error, not_found} ->
			ipdr_codec10(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec10(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?AcctSessionTime, ReqAttrs) of
		{ok, SessionTime} ->
			ipdr_codec11(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{sessionDuration = SessionTime});
		{error, not_found} ->
			ipdr_codec11(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec11(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	Octets = case radius_attributes:find(?AcctInputOctets, ReqAttrs) of
		{ok, N} ->
			N;
		{error, not_found} ->
			0
	end,
	case radius_attributes:find(?AcctInputGigawords, ReqAttrs) of
		{ok, GigaWords} ->
			GigaOctets = (GigaWords * (16#ffffffff + 1)) + Octets,
			ipdr_codec12(radius, stop,ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{inputOctets = GigaOctets});
		{error, not_found} ->
			ipdr_codec12(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{inputOctets = Octets})
	end.
%% @hidden
ipdr_codec12(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	Octets = case radius_attributes:find(?AcctOutputOctets, ReqAttrs) of
		{ok, N} ->
			N;
		{error, not_found} ->
			0
	end,
	case radius_attributes:find(?AcctOutputGigawords, ReqAttrs) of
		{ok, GigaWords} ->
			GigaOctets = (GigaWords * (16#ffffffff + 1)) + Octets,
			ipdr_codec13(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{outputOctets = GigaOctets});
		{error, not_found} ->
			ipdr_codec13(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{outputOctets = Octets})
	end.
%% @hidden
ipdr_codec13(radius, stop, ReqAttrs, RespAttrs, Rated, Acc) ->
	case radius_attributes:find(?Class, ReqAttrs) of
		{ok, Class} ->
			ipdr_codec14(radius, stop, ReqAttrs, RespAttrs, Rated,
					Acc#ipdr{class = Class});
		{error, not_found} ->
			ipdr_codec14(radius, stop, ReqAttrs, RespAttrs, Rated, Acc)
	end.
%% @hidden
ipdr_codec14(radius, stop, ReqAttrs, _RespAttrs, _Rated, Acc) ->
	case radius_attributes:find(?AcctTerminateCause, ReqAttrs) of
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

% @private
http_parse(Event) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Host:Offset/binary, 32, $-, 32, Rest/binary>> = Event,
	http_parse1(Rest, #event{host = binary_to_list(Host)}).
% @hidden
http_parse1(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<User:Offset/binary, 32, $[, Rest/binary>> = Event,
	http_parse2(Rest, Acc#event{user = binary_to_list(User)}).
% @hidden
http_parse2(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<$]>>),
	<<Date:Offset/binary, $], 32, $", Rest/binary>> = Event,
	http_parse3(Rest, Acc#event{date = binary_to_list(Date)}).
% @hidden
http_parse3(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Method:Offset/binary, 32, Rest/binary>> = Event,
	http_parse4(Rest, Acc#event{method = binary_to_list(Method)}).
% @hidden
http_parse4(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<URI:Offset/binary, 32, Rest/binary>> = Event,
	http_parse5(Rest, Acc#event{uri = binary_to_list(URI)}).
% @hidden
http_parse5(Event, Acc) ->
	{Offset, 2} = binary:match(Event, <<$", 32>>),
	<<_Http:Offset/binary, $", 32, Rest/binary>> = Event,
	http_parse6(Rest, Acc).
% @hidden
http_parse6(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Status:Offset/binary, 32, _Rest/binary>> = Event,
	Acc#event{httpStatus = binary_to_integer(Status)}.

-spec open_log(Directory, Log, LogSize, LogFiles) -> Result
	when
		Directory  :: string(),
		Log :: atom(),
		LogSize :: integer(),
		LogFiles :: integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc open disk log file
open_log(Directory, Log, LogSize, LogFiles) ->
	case file:make_dir(Directory) of
		ok ->
			open_log1(Directory, Log, LogSize, LogFiles);
		{error, eexist} ->
			open_log1(Directory, Log, LogSize, LogFiles);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
open_log1(Directory, Log, LogSize, LogFiles) ->
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

-spec write_log(Log, Event) -> Result
	when
		Log :: atom(),
		Event :: list(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc write event into given log file
write_log(Log, Event) ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	LogEvent = list_to_tuple([TS, N | Event]),
	disk_log:log(Log, LogEvent).

-spec close_log(Log) -> Result
	when
		Log :: atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc close log files
close_log(Log) ->
	case disk_log:close(Log) of
		ok ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, Log}, {error, Reason}]),
			{error, Reason}
	end.

-spec query_log(Continuation, Start, End, Log, MFA) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | pos_integer(),
		End :: calendar:datetime() | pos_integer(),
		MFA :: {Module, Function, Args},
		Log :: atom(),
		Module :: atom(),
		Function :: atom(),
		Args :: [Arg],
		Arg :: term(),
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [term()],
		Reason :: term().
%% @doc
query_log(Continuation, {{_, _, _}, {_, _, _}} = Start, End, Log, MFA) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	query_log(Continuation, Seconds * 1000, End, Log, MFA);
query_log(Continuation, Start, {{_, _, _}, {_, _, _}} = End, Log, MFA) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	query_log(Continuation, Start, Seconds * 1000 + 999, Log, MFA);
query_log(start, Start, End, Log, MFA) when is_integer(Start), is_integer(End) ->
	query_log1(Start, End, Log, MFA, [], disk_log:bchunk(Log, start));
query_log(Continuation, Start, End, Log, MFA) when is_integer(Start), is_integer(End) ->
	query_log2(Start, End, MFA, disk_log:chunk(Log, Continuation), []).
%% @hidden
query_log1(Start, End, _Log, MFA, PrevChunk, eof) ->
	Chunk = [binary_to_term(E) || E <- PrevChunk],
	query_log2(Start, End, MFA, {eof, Chunk}, []);
query_log1(_Start, _End, _Log, _MFA, _PrevChunk, {error, Reason}) ->
	{error, Reason};
query_log1(Start, End, Log, MFA, PrevChunk, {Cont, [H | T] = Chunk}) ->
	case binary_to_term(H) of
		Event when element(1, Event) > End ->
			{eof, []};
		Event when element(1, Event) >= Start ->
			NewChunk = [binary_to_term(E) || E <- PrevChunk ++ Chunk],
			query_log2(Start, End, MFA, {Cont, NewChunk}, []);
		_Event ->
			query_log1(Start, End, Log, MFA, T, disk_log:bchunk(Log, Cont))
	end.
%% @hidden
query_log2(_Start, _End, {M, F, A}, eof, Acc) ->
	apply(M, F, [{eof, lists:reverse(Acc)} | A]);
query_log2(_Start, _End, _MFA, {error, Reason}, _Acc)->
	{error, Reason};
query_log2(_Start, End, {M, F, A}, {_, [Event | _]}, Acc) when element(1, Event) > End ->
	apply(M, F, [{eof, lists:reverse(Acc)} | A]);
query_log2(Start, End, MFA, {Cont, [Event | T]}, Acc)
		when element(1, Event) >= Start, element(1, Event) =< End ->
	query_log2(Start, End, MFA, {Cont, T}, [Event | Acc]);
query_log2(Start, End, MFA, {Cont, [_ | T]}, Acc) ->
	query_log2(Start, End, MFA, {Cont, T}, Acc);
query_log2(_Start, _End, {M, F, A}, {Cont, []}, Acc) ->
	apply(M, F, [{Cont, lists:reverse(Acc)} | A]).

-spec acct_query(Continuation, Protocol, Types, MatchSpec) -> Result
	when
		Continuation :: {Continuation2, Events},
		Protocol :: atom() | '_',
		Types :: [Type] | '_',
		Type :: atom(),
		MatchSpec :: [tuple()] | '_',
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()].
%% @private
%% @doc Query accounting log events with filters.
%%
acct_query({Cont, Events}, Protocol, Types, AttrsMatch) ->
	{Cont, acct_query1(Events,  Protocol, Types, AttrsMatch, [])}.
%% @hidden
acct_query1(Events, Protocol, '_',  AttrsMatch, _Acc) ->
	acct_query2(Events, Protocol, AttrsMatch, []);
acct_query1([H | T], Protocol, Types,  AttrsMatch, Acc) ->
	case lists:member(element(6, H), Types) of
		true ->
			acct_query1(T, Protocol, Types, AttrsMatch, [H | Acc]);
		false ->
			acct_query1(T, Protocol, Types, AttrsMatch, Acc)
	end;
acct_query1([], Protocol, _Types,  AttrsMatch, Acc) ->
	acct_query2(lists:reverse(Acc), Protocol, AttrsMatch, []).
%% @hidden
acct_query2(Events, '_', AttrsMatch, _Acc) ->
	acct_query3(Events, AttrsMatch, []);
acct_query2([H | T], Protocol, AttrsMatch, Acc)
		when element(3, H) == Protocol ->
	acct_query2(T, Protocol, AttrsMatch, [H |Acc]);
acct_query2([_ | T], Protocol, AttrsMatch, Acc) ->
	acct_query2(T, Protocol, AttrsMatch, Acc);
acct_query2([], _Protocol, AttrsMatch, Acc) ->
	acct_query3(lists:reverse(Acc), AttrsMatch, []).
%% @hidden
acct_query3(Events, '_', _Acc) ->
	Events;
acct_query3([H | T], AttrsMatch, Acc) ->
	case acct_query4(element(7, H), AttrsMatch) of
		true ->
			acct_query3(T, AttrsMatch, [H | Acc]);
		false ->
			acct_query3(T, AttrsMatch, Acc)
	end;
acct_query3([], _AttrsMatch, Acc) ->
	lists:reverse(Acc).
%% @hidden
acct_query4(Attributes, [{Attribute, {exact, Match}} | T]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Match1} when
				(Match1 == Match) or (Match == '_') ->
			acct_query4(Attributes, T);
		_ ->
			false
	end;
acct_query4(Attributes, [{Attribute, {like, [H | T1]}} | T2]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Value} ->
			case lists:prefix(H, Value) of
				true ->
					acct_query4(Attributes, [{Attribute, {like, T1}} | T2]);
				false ->
					false
			end;
		_ ->
			false
	end;
acct_query4(Attributes, [{_, {like, []}} | T]) ->
	acct_query4(Attributes, T);
acct_query4(Attributes, [_ | T]) ->
	acct_query4(Attributes, T);
acct_query4(_Attributes, []) ->
	true.

-spec ipdr_query(Continuation, MatchSpec) -> Result
	when
		Continuation :: {Continuation2, Events},
		MatchSpec :: [tuple()] | '_',
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [#ipdr{}].
%% @private
%% @doc Query accounting log events with filters.
%%
ipdr_query({Cont, Events}, AttrsMatch) ->
	{Cont, ipdr_query3(Events, AttrsMatch, [])}.
%% @hidden
ipdr_query3(Events, '_', _Acc) ->
	Events;
ipdr_query3([H | T], AttrsMatch, Acc) ->
	case ipdr_query4(element(7, H), AttrsMatch) of
		true ->
			ipdr_query3(T, AttrsMatch, [H | Acc]);
		false ->
			ipdr_query3(T, AttrsMatch, Acc)
	end;
ipdr_query3([], _AttrsMatch, Acc) ->
	lists:reverse(Acc).
%% @hidden
ipdr_query4(Attributes, [{Attribute, {exact, Match}} | T]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Match1} when
				(Match1 == Match) or (Match == '_') ->
			ipdr_query4(Attributes, T);
		_ ->
			false
	end;
ipdr_query4(Attributes, [{Attribute, {like, [H | T1]}} | T2]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Value} ->
			case lists:prefix(H, Value) of
				true ->
					ipdr_query4(Attributes, [{Attribute, {like, T1}} | T2]);
				false ->
					false
			end;
		_ ->
			false
	end;
ipdr_query4(Attributes, [{_, {like, []}} | T]) ->
	ipdr_query4(Attributes, T);
ipdr_query4(Attributes, [_ | T]) ->
	ipdr_query4(Attributes, T);
ipdr_query4(_Attributes, []) ->
	true.

-spec auth_query(Continuation, Protocol, Types, ReqAttrsMatch, RespAttrsMatch) -> Result
	when
		Continuation :: {Continuation2, Events},
		Protocol :: atom() | '_',
		Types :: [Type] | '_',
		Type :: atom(),
		ReqAttrsMatch :: [tuple()] | '_',
		RespAttrsMatch :: [tuple()] | '_',
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),

		Events :: [acct_event()].
%% @private
%% @doc Query authentication log events with filters.
%%
auth_query({Cont, Events}, Protocol, Types, ReqAttrsMatch, RespAttrsMatch) ->
	{Cont, auth_query1(Events, Protocol, Types, ReqAttrsMatch, RespAttrsMatch)}.
%% @hidden
auth_query1(Events, Protocol, Types, ReqAttrsMatch, RespAttrsMatch) ->
	auth_query1(Events, Protocol, Types, ReqAttrsMatch, RespAttrsMatch, []).
%% @hidden
auth_query1(Events, Protocol, '_', ReqAttrsMatch, RespAttrsMatch, []) ->
	auth_query2(Events, Protocol, ReqAttrsMatch, RespAttrsMatch, []);
auth_query1([{_, _, _, _, _, _, Type, _, _} = H | T],
		Protocol, Types, ReqAttrsMatch, RespAttrsMatch, Acc) ->
	case lists:member(Type, Types) of
		true ->
			auth_query1(T, Protocol, Types,
				ReqAttrsMatch, RespAttrsMatch, [H | Acc]);
		false ->
			auth_query1(T, Protocol, Types,
				ReqAttrsMatch, RespAttrsMatch, Acc)
	end;
auth_query1([], Protocol, _Types, ReqAttrsMatch, RespAttrsMatch, Acc) ->
	auth_query2(lists:reverse(Acc), Protocol, ReqAttrsMatch, RespAttrsMatch, []).
%% @hidden
auth_query2(Events, '_', ReqAttrsMatch, RespAttrsMatch, []) ->
	auth_query3(Events, ReqAttrsMatch, RespAttrsMatch, []);
auth_query2([{_, _, Protocol, _, _, _, _, _, _} = H | T],
		Protocol, ReqAttrsMatch, RespAttrsMatch, Acc) ->
	auth_query2(T, Protocol, ReqAttrsMatch, RespAttrsMatch, [H | Acc]);
auth_query2([_ | T], Protocol, ReqAttrsMatch, RespAttrsMatch, Acc) ->
	auth_query2(T, Protocol, ReqAttrsMatch, RespAttrsMatch, Acc);
auth_query2([], _Protocol, ReqAttrsMatch, RespAttrsMatch, Acc) ->
	auth_query3(lists:reverse(Acc), ReqAttrsMatch, RespAttrsMatch, []).
%% @hidden
auth_query3(Events, '_', RespAttrsMatch, []) ->
	auth_query4(Events, RespAttrsMatch, []);
auth_query3([{_, _, _, _, _, _, _, ReqAttr, _} = H | T],
		ReqAttrsMatch, RespAttrsMatch, Acc) ->
	case auth_query5(ReqAttr, ReqAttrsMatch) of
		true ->
			auth_query3(T, ReqAttrsMatch, RespAttrsMatch, [H | Acc]);
		false ->
			auth_query3(T, ReqAttrsMatch, RespAttrsMatch, Acc)
	end;
auth_query3([], _ReqAttrsMatch, RespAttrsMatch, Acc) ->
	auth_query4(lists:reverse(Acc), RespAttrsMatch, []).
%% @hidden
auth_query4(Events, '_', []) ->
	lists:reverse(Events);
auth_query4([{_, _, _, _, _, _, _, _, RespAttr} = H | T],
		RespAttrsMatch, Acc) ->
	case auth_query5(RespAttr, RespAttrsMatch) of
		true ->
			auth_query4(T, RespAttrsMatch, [H | Acc]);
		false ->
			auth_query4(T, RespAttrsMatch, Acc)
	end;
auth_query4([], _RespAttrsMatch, Acc) ->
	lists:reverse(Acc).
%% @hidden
auth_query5(Attributes, [{Attribute, {exact, Match}} | T]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Match1} when
				(Match == Match1) or (Match == '_') ->
			auth_query5(Attributes, T);
		_ ->
			false
	end;
auth_query5(Attributes, [{Attribute, {like, [H | T1]}} | T2]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Value} ->
			case lists:prefix(H, Value) of
				true ->
					auth_query5(Attributes, [{Attribute, {like, T1}} | T2]);
				false ->
					false
			end;
		_ ->
			false
	end;
auth_query5(Attributes, [{_, {like, []}} | T]) ->
	auth_query5(Attributes, T);
auth_query5(Attributes, [_H | T]) ->
	auth_query5(Attributes, T);
auth_query5(_Attributes, []) ->
	true.

-spec abmf_query(Continuation, Type, Subscriber, Bucket,
		Units, Product) -> Result
	when
		Continuation :: {Continuation2, Events},
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),
		Type :: transfer | topup | adjustment | '_',
		Subscriber :: binary() | '_',
		Bucket :: string() | '_',
		Units :: cents | seconds | octets | '_',
		Product :: string() | '_',
		Events :: [acct_event()].
%% @private
%% @doc Query balance activity log events with filters.
%%
abmf_query({Cont, Events}, Type, Subscriber, Bucket,
		 Units, Product) ->
	{Cont, abmf_query1(Events, Type, Subscriber, Bucket, Units, Product)}.
%% @hidden
abmf_query1(Events, '_', Subscriber, Bucket, Units, Product) ->
	abmf_query2(Events, Subscriber, Bucket, Units, Product);
abmf_query1(Events, Type, Subscriber, Bucket, Units, Product) ->
	F = fun(Event) when element(4, Event) == Type -> true; (_) -> false end,
	abmf_query2(lists:filter(F, Events), Subscriber, Bucket, Units, Product).
%% @hidden
abmf_query2(Events, '_', Bucket, Units, Product) ->
	abmf_query3(Events, Bucket, Units, Product);
abmf_query2(Events, Subscriber, Bucket, Units, Product) ->
	F = fun(Event) when element(5, Event) == Subscriber -> true; (_) -> false end,
	abmf_query3(lists:filter(F, Events), Bucket, Units, Product).
%% @hidden
abmf_query3(Events, '_', Units, Product) ->
	abmf_query4(Events, Units, Product);
abmf_query3(Events, Bucket, Units, Product) ->
	F = fun(Event) when element(6, Event) == Bucket -> true; (_) -> false end,
	abmf_query4(lists:filter(F, Events), Units, Product).
%% @hidden
abmf_query4(Events, '_', Product) ->
	abmf_query5(Events, Product);
abmf_query4(Events, Units, Product) ->
	F = fun(Event) when element(7, Event) == Units -> true; (_) -> false end,
	abmf_query5(lists:filter(F, Events), Product).
%% @hidden
abmf_query5(Events, '_') ->
	lists:reverse(Events);
abmf_query5(Events, Product) ->
	F = fun(Event) when element(8, Event) == Product -> true; (_) -> false end,
	lists:reverse(lists:filter(F, Events)).

