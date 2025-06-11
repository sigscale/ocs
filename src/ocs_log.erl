%%% ocs_log.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
%%% 	Event logging in {@link //ocs. ocs} uses
%%% 	{@link //kernel/disk_log. disk_log} wrap logs configured for file
%%% 	size, and number of files, using application environment variables
%%% 	(e.g. `acct_log_size', `acct_log_files'). As the log items are
%%% 	chronologically ordered finding an event by start time is relatively
%%% 	efficient. The {@link btree_search/2. btree_search/2} function is
%%% 	used to perform a binary tree search across the files and a chunk
%%% 	head comparison to quickly find the file and chunk containing items
%%% 	with a given start time.
%%%
%%% 	All log items have the common form:
%%% 	<ul>
%%% 		<li>tuple with artity > 2</li>
%%% 		<li>first element (TS) is {@link timestamp(). timestamp()}</li>
%%% 		<li>second element(N) is {@link unique(). unique()}</li>
%%% 		<li>`{TS, N}' is unique within a log</li>
%%% 	</ul>
%%%
%%% 	Functions reading log files SHALL assume the above format and
%%% 	SHOULD ignore items with unexpected tuple arity or element values.
%%%
%%% @reference <a href="http://www.tmforum.org/ipdr/">IPDR Specifications</a>.
%%%
-module(ocs_log).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

%% export the ocs_log public API
-export([acct_open/0, acct_log/6, acct_close/0,
		acct_query/5, acct_query/6]).
-export([auth_open/0, auth_log/5, auth_log/6, auth_close/0,
			auth_query/6, auth_query/7]).
-export([ipdr_log/4, ipdr_file/3, ipdr_query/5]).
-export([abmf_open/0, abmf_log/15,
			abmf_query/8]).
-export([get_range/3, last/2, dump_file/2, httpd_logname/1,
			http_file/2, date/1, iso8601/1]).
-export([http_query/8]).
-export([log_name/1]).
-export([btree_search/2]).
-export([auth_to_ecs/1, acct_to_ecs/1]).

%% exported the private function
-export([acct_query/4, ipdr_query/2, auth_query/5, abmf_query/6]).

%% export the ocs_log event types
-export_type([auth_event/0, acct_event/0, abmf_event/0,
		http_event/0]).

-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_s6b_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include("diameter_gen_3gpp_s6a_application.hrl").
-include("diameter_gen_3gpp_gx_application.hrl").

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).
-define(usageSpecPath, "/usageManagement/v1/usageSpecification/").
-define(usagePath, "/usageManagement/v1/usage/").

%% export the common log field types
-type timestamp() :: pos_integer().
-type unique() :: pos_integer().
-type protocol() :: radius | diameter | nrf.
-type server() :: {Address :: inet:ip_address(),
		Port :: non_neg_integer()} | undefined.
-export_type([timestamp/0, unique/0, protocol/0, server/0]).

%% export the ocs_acct field types
-export_type([acct_type/0, acct_request/0, acct_response/0, acct_rated/0]).

%% export the ocs_auth field types
-export_type([auth_type/0, auth_request/0, auth_response/0]).

%%----------------------------------------------------------------------
%%  The ocs_log public API
%%----------------------------------------------------------------------

-spec log_name(Var) -> Name
	when
		Var :: atom(),
		Name :: term().
%% @doc Return log name from `Var' application environment variable.
log_name(Var) ->
	{ok, Name} = application:get_env(ocs, Var),
	Name.

-spec acct_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Open an accounting event disk log.
acct_open() ->
	{ok, Directory} = application:get_env(ocs, acct_log_dir),
	{ok, LogSize} = application:get_env(ocs, acct_log_size),
	{ok, LogFiles} = application:get_env(ocs, acct_log_files),
	open_log(Directory, log_name(acct_log_name), LogSize, LogFiles).

-type nrf_request() :: map().
-type nrf_response() :: map().
-type acct_type() :: on | off | start | stop | update | interim | final | 'event'.
-type acct_request() :: #'3gpp_ro_CCR'{} | #'3gpp_ro_RAR'{}
		| #'3gpp_gx_CCR'{} | #'3gpp_gx_RAR'{}
		| radius_attributes:attributes() | nrf_request().
-type acct_response() :: #'3gpp_ro_CCA'{} | #'3gpp_ro_RAA'{}
		| #'3gpp_gx_CCA'{} | #'3gpp_gx_RAA'{}
		| radius_attributes:attributes() | nrf_response().
-type acct_rated() :: [#rated{}].

-spec acct_log(Protocol, Server, Type, Request, Response, Rated) -> Result
	when
		Protocol :: protocol(),
		Server :: server(),
		Type :: acct_type(),
		Request :: acct_request(),
		Response :: acct_response() | undefined,
		Rated :: acct_rated() | undefined,
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write an event to accounting log.
acct_log(Protocol, Server, Type, Request, Response, Rated) ->
	Event = [Protocol, node(), Server, Type, Request, Response, Rated],
	write_log(log_name(acct_log_name), Event).

-spec acct_close() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Close accounting disk log.
acct_close() ->
	close_log(log_name(acct_log_name)).

-spec acct_query(Continuation, Start, End, Types, Matches) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Types :: [Type] | '_',
		Type :: acct_type(),
		Matches :: [Match] | '_',
		Match :: RadiusMatch | DiameterMatchSpec | NrfMatchSpec | RatedMatchSpec,
		RadiusMatch :: {Attribute, AttributeMatch},
		Attribute :: byte(),
		AttributeMatch :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]},
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{}
				| #'3gpp_ro_RAR'{} | #'3gpp_ro_RAA'{}
				| #'3gpp_gx_CCR'{} | #'3gpp_gx_CCA'{}
				| #'3gpp_gx_RAR'{} | #'3gpp_gx_RAA'{},
		NrfMatchSpec :: {NrfMatchHead, MatchConditions},
		NrfMatchHead :: map(),
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()],
		Reason :: term().
%% @doc Query accounting log events with filters.
%% @equiv acct_query(Continuation, Start, End, '_', Types, AttrsMatch)
acct_query(Continuation, Start, End, Types, Matches) ->
	acct_query(Continuation, Start, End, '_', Types, Matches).

-spec acct_query(Continuation, Start, End, Protocol, Types, Matches) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Protocol :: protocol() | [protocol()] | '_',
		Types :: [Type] | '_',
		Type :: acct_type(),
		Matches :: [Match] | '_',
		Match :: RadiusMatch | DiameterMatchSpec | NrfMatchSpec | RatedMatchSpec,
		RadiusMatch :: {Attribute, AttributeMatch},
		Attribute :: byte(),
		AttributeMatch :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]},
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{}
				| #'3gpp_ro_RAR'{} | #'3gpp_ro_RAA'{}
				| #'3gpp_gx_CCR'{} | #'3gpp_gx_CCA'{}
				| #'3gpp_gx_RAR'{} | #'3gpp_gx_RAA'{},
		NrfMatchSpec :: {NrfMatchHead, MatchConditions},
		NrfMatchHead :: map(),
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
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
%% 	`Protocol', `Types', or `MatchSpec' may be '_' which matches any value.
%%
%% 	Returns a new `Continuation' and a list of matching accounting events.
%% 	Successive calls use the new `Continuation' to read more events.
%%
acct_query(Continuation, Start, End, Protocol, Types, Matches)
		when (is_integer(Start) orelse (tuple_size(Start) == 2)),
		(is_integer(End) orelse (tuple_size(End) == 2)),
		(is_atom(Protocol) orelse is_list(Protocol)
				orelse (Protocol == '_')),
		(is_list(Types) orelse (Types == '_')),
		(is_list(Matches) orelse (Matches == '_')) ->
	MFA = {?MODULE, acct_query, [Protocol, Types, Matches]},
	query_log(Continuation, Start, End, log_name(acct_log_name), MFA).

-spec auth_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Open authorization event disk log.
auth_open() ->
	{ok, Directory} = application:get_env(ocs, auth_log_dir),
	{ok, LogSize} = application:get_env(ocs, auth_log_size),
	{ok, LogFiles} = application:get_env(ocs, auth_log_files),
	open_log(Directory, log_name(auth_log_name), LogSize, LogFiles).

-type auth_type() :: accept | reject | change.
-type auth_request_rad() :: radius_attributes:attributes().
-type auth_request_dia() :: #diameter_nas_app_AAR{}
		| #diameter_eap_app_DER{} | #'3gpp_sta_DER'{} | #'3gpp_swm_DER'{}
		| #'3gpp_sta_STR'{} | #'3gpp_swm_STR'{} | #'3gpp_s6b_STR'{}
		| #'3gpp_swx_RTR'{} | #'3gpp_s6b_AAR'{} | #'3gpp_s6a_AIR'{}
		| #'3gpp_s6a_ULR'{} | #'3gpp_s6a_PUR'{}.
-type auth_request() :: auth_request_rad() | auth_request_dia().
-type auth_response_rad() :: radius_attributes:attributes().
-type auth_response_dia() :: #diameter_nas_app_AAA{}
		| #diameter_eap_app_DEA{} | #'3gpp_sta_DEA'{} | #'3gpp_swm_DEA'{}
		| #'3gpp_sta_STA'{} | #'3gpp_swm_STA'{} | #'3gpp_s6b_STA'{}
		| #'3gpp_swx_RTA'{} | #'3gpp_s6b_AAA'{} | #'3gpp_s6a_AIA'{}
		| #'3gpp_s6a_ULA'{} | #'3gpp_s6a_PUA'{}.
-type auth_response() :: auth_response_rad() | auth_response_dia().

-spec auth_log(Protocol, Server, Client, Type, RequestAttributes,
		ResponseAttributes) -> Result
	when
		Protocol :: radius,
		Server :: server(),
		Client :: server(),
		Type :: auth_type(),
		RequestAttributes :: auth_request_rad(),
		ResponseAttributes :: auth_response_rad(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write a RADIUS event to authorization log.
auth_log(Protocol, Server, Client, Type, RequestAttributes, ResponseAttributes) ->
	Event = [Protocol, node(), Server, Client, Type,
			RequestAttributes, ResponseAttributes],
	write_log(log_name(auth_log_name), Event).

-spec auth_log(Protocol, Server, Client, Request, Response) -> Result
	when
		Protocol :: diameter,
		Server :: server(),
		Client :: server(),
		Request :: auth_request_dia(),
		Response :: auth_response_dia(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Write a DIAMETER event to authorization log.
auth_log(Protocol, Server, Client, Request, Response) ->
	Event = [Protocol, node(), Server, Client, Request, Response],
	write_log(log_name(auth_log_name), Event).

-spec auth_query(Continuation, Start, End, Types,
		ReqAttrsMatch, RespAttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Types :: [Type] | '_',
		Type :: auth_type(),
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
%% @equiv auth_query(Continuation, Start, End, '_', Types, ReqAttrsMatch, RespAttrsMatch)
auth_query(Continuation, Start, End, Types, ReqAttrsMatch, RespAttrsMatch) ->
	auth_query(Continuation, Start, End, '_', Types,
			ReqAttrsMatch, RespAttrsMatch).

-spec auth_query(Continuation, Start, End, Protocol, Types,
		ReqAttrsMatch, RespAttrsMatch) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Protocol :: protocol() | '_',
		Types :: [Type] | '_',
		Type :: auth_type(),
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
auth_query(Continuation, Start, End, Protocol, Types, ReqAttrsMatch, RespAttrsMatch)
		when (is_integer(Start) orelse (tuple_size(Start) == 2)),
		(is_integer(End) orelse (tuple_size(End) == 2)),
		(is_atom(Protocol) orelse is_list(Protocol)
				orelse (Protocol == '_')),
		(is_list(Types) orelse (Types == '_')),
		(is_list(ReqAttrsMatch) orelse (ReqAttrsMatch== '_')),
		(is_list(RespAttrsMatch) orelse (RespAttrsMatch== '_')) ->
	MFA = {?MODULE, auth_query, [Protocol, Types, ReqAttrsMatch, RespAttrsMatch]},
	query_log(Continuation, Start, End, log_name(auth_log_name), MFA).

-spec auth_close() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Close auth disk log.
auth_close() ->
	close_log(log_name(auth_log_name)).

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
%% @doc Query http log events with filters
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
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		AttrsMatch :: [{Attribute, Match}] | '_',
		Attribute :: byte(),
		Match :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]} | '_',
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()].
%% @doc Ipdr log query.
ipdr_query(Continuation, Log, _Start, _End, _AttrsMatch) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			{eof, []};
		{Continuation1, Events} ->
			{Continuation1, Events}
	end.

-spec ipdr_log(Type, File, Start, End) -> Result
	when
		Type :: wlan | voip,
		File :: file:filename(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Log accounting records within range to new IPDR disk log.
%%
%% 	Creates a new {@link //kernel/disk_log:log(). disk_log:log()},
%% 	or overwrites an existing, with filename `File'. The log starts
%% 	with a `#ipdrDocWLAN{}' header, is followed by `#ipdr_wlan{}' records,
%% 	and ends with a `#ipdrDocEnd{}' trailer.
%%
%% 	The `ocs_acct' log is searched for events created between `Start'
%% 	and `End' which may be given as
%% 	`{{Year, Month, Day}, {Hour, Minute, Second}}' or the native
%% 	{@link //erts/erlang:system_time(). erlang:system_time(millisecond)}.
%%
ipdr_log(Type, File, {{_, _, _}, {_, _, _}} = Start, End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	ipdr_log(Type, File, Seconds * 1000, End);
ipdr_log(Type, File, Start, {{_, _, _}, {_, _, _}} = End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	ipdr_log(Type, File, Start, Seconds * 1000 + 999);
ipdr_log(Type, File, Start, End) when is_list(File),
		is_integer(Start), is_integer(End) ->
		{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
		FileName = Directory ++ "/" ++ atom_to_list(Type) ++ "/" ++ File,
	case disk_log:open([{name, File}, {file, FileName}, {repair, truncate}]) of
		{ok, IpdrLog} ->
			IpdrDoc = case Type of
				wlan ->
					#ipdrDocWLAN{docId = uuid(), version = "3.1",
							creationTime = iso8601(erlang:system_time(millisecond)),
							ipdrRecorderInfo = atom_to_list(node())};
				voip ->
					#ipdrDocVoIP{docId = uuid(), version = "3.1",
							creationTime = iso8601(erlang:system_time(millisecond)),
							ipdrRecorderInfo = atom_to_list(node())}
			end,
			case disk_log:log(IpdrLog, IpdrDoc) of
				ok ->
					ipdr_log1(IpdrLog, Start, End,
%							btree_search(log_name(acct_log_name), Start, End));
							btree_search(log_name(acct_log_name), Start));
				{error, Reason} ->
					Descr = lists:flatten(disk_log:format_error(Reason)),
					Trunc = lists:sublist(Descr, length(Descr) - 1),
					error_logger:error_report([Trunc, {module, ?MODULE},
							{log, IpdrLog}, {error, Reason}]),
					disk_log:close(IpdrLog),
					{error, Reason}
			end;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{file, File}, {error, Reason}]),
			{error, Reason}
	end.
%% @hidden
ipdr_log1(IpdrLog, _Start, _End, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, log_name(acct_log_name)}, {error, Reason}]),
	ipdr_log4(IpdrLog, 0);
%ipdr_log1(IpdrLog, _Start, _End, eof) ->
%	ipdr_log4(IpdrLog, 0);
ipdr_log1(IpdrLog, Start, End, Cont) ->
	ipdr_log2(IpdrLog, Start, End, [], disk_log:chunk(log_name(acct_log_name), Cont)).
%% @hidden
ipdr_log2(IpdrLog, _Start, _End, _PrevChunk, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, log_name(acct_log_name)}, {error, Reason}]),
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
	ipdr_log2(IpdrLog, Start, End, T, disk_log:chunk(log_name(acct_log_name), Cont));
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
	ipdr_log3(IpdrLog, Start, End, SeqNum, disk_log:chunk(log_name(acct_log_name), Cont));
ipdr_log3(IpdrLog, _Start, End, SeqNum, {_Cont, [H | _]})
		when element(1, H) > End ->
	ipdr_log4(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, [H | T]})
		when element(6, H) == stop ->
	case ipdr_codec(H) of
		[#ipdr_wlan{} | _] = IPDR ->
			NewSeqNum = SeqNum + 1,
			F1 = fun F1([H1 | T2]) ->
					case disk_log:log(IpdrLog, H1#ipdr_wlan{seqNum = NewSeqNum}) of
						ok ->
							F1(T2);
						{error, Reason} ->
							Descr = lists:flatten(disk_log:format_error(Reason)),
							Trunc = lists:sublist(Descr, length(Descr) - 1),
							error_logger:error_report([Trunc, {module, ?MODULE},
									{log, IpdrLog}, {error, Reason}]),
							disk_log:close(IpdrLog),
							{error, Reason}
					end;
				F1([]) ->
					ipdr_log3(IpdrLog, Start, End, NewSeqNum, {Cont, T})
			end,
			F1(IPDR);
		[#ipdr_voip{} | _] = IPDR ->
			NewSeqNum = SeqNum + 1,
			F2 = fun F2([H2 | T2]) ->
					case disk_log:log(IpdrLog, H2#ipdr_voip{seqNum = NewSeqNum}) of
						ok ->
							F2(T2);
						{error, Reason} ->
							Descr = lists:flatten(disk_log:format_error(Reason)),
							Trunc = lists:sublist(Descr, length(Descr) - 1),
							error_logger:error_report([Trunc, {module, ?MODULE},
									{log, IpdrLog}, {error, Reason}]),
							disk_log:close(IpdrLog),
							{error, Reason}
					end;
				F2([]) ->
					ipdr_log3(IpdrLog, Start, End, NewSeqNum, {Cont, T})
			end,
			F2(IPDR);
		_ ->
			ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, T})
	end;
ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, [_ | T]}) ->
	ipdr_log3(IpdrLog, Start, End, SeqNum, {Cont, T}).
%% @hidden
ipdr_log4(IpdrLog, SeqNum) ->
	EndTime = iso8601(erlang:system_time(millisecond)),
	IpdrDocEnd = #ipdrDocEnd{count = SeqNum, endTime = EndTime},
	case disk_log:log(IpdrLog, IpdrDocEnd) of
		ok ->
			case disk_log:close(IpdrLog) of
				ok ->
					ok;
				{error, Reason} ->
					Descr = lists:flatten(disk_log:format_error(Reason)),
					Trunc = lists:sublist(Descr, length(Descr) - 1),
					error_logger:error_report([Trunc, {module, ?MODULE},
							{log, IpdrLog}, {error, Reason}]),
					{error, Reason}
			end;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, IpdrLog}, {error, Reason}]),
			disk_log:close(IpdrLog),
			{error, Reason}
	end.

-spec ipdr_file(Type, LogFile, Format) -> Result
	when
		Type :: wlan | voip,
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
ipdr_file(Type, LogFile, Format) when is_list(LogFile),
		((Format == xml) or (Format == xdr) or (Format == csv)) ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	FileName = Directory ++ "/" ++ atom_to_list(Type) ++ "/" ++ LogFile,
	case disk_log:open([{name, make_ref()}, {file, FileName}, {repair, true}]) of
		{ok, Log} ->
			ipdr_file1(LogFile, Log, Format);
		{repaired, Log, Recovered, BadBytes} ->
			error_logger:warning_report(["Log Repaired",
					{module, ?MODULE}, {log, Log}, Recovered, BadBytes]),
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
					{module, ?MODULE}, {log, Log},
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
ipdr_file3(_Log, _IoDevice, xml, {_Cont, _Events}) ->
	 {error, unimplemented};
ipdr_file3(_Log, _IoDevice, xdr, {_Cont,_Events}) ->
	 {error, unimplemented};
ipdr_file3(Log, IoDevice, csv, {Cont, Events}) ->
	ipdr_csv(Log, IoDevice, {Cont, Events}).

-spec get_range(Log, Start, End) -> Result
	when
		Log :: disk_log:log(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Result :: [term()].
%% @doc Get all events in a log within a date/time range.
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
					{module, ?MODULE}, {log, Log},
					{filename, FileName}, {error, Reason}]),
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
					{module, ?MODULE}, {log, Log},
					{filename, FileName}, {error, Reason}]),
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
			last1(Log, MaxItems, [start], {0, []});
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
		MilliSeconds :: timestamp(),
		Result :: calendar:datetime().
%% @doc Convert Unix epoch timestamp to OTP date and time.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds).

-spec iso8601(DateTime) -> DateTime
	when
		DateTime :: timestamp() | string().
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
	open_log(Directory, log_name(abmf_log_name), LogSize, LogFiles).

-spec abmf_log(Type, ServiceId, Bucket, Units, Product, Amount,
		AmountBefore, AmountAfter, Validity, Channel, Requestor,
		RelatedParty, PaymentMeans, Action, Status) -> Result
	when
		Type :: topup | adjustment | delete | deduct | reserve | unreserve | transfer,
		ServiceId :: undefined | binary(),
		Bucket :: string(),
		Units :: cents | seconds | octets | messages,
		Product :: string(),
		Amount :: integer(),
		AmountBefore :: integer() | undefined,
		AmountAfter :: integer() | undefined,
		Validity :: undefined | timestamp(),
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
%% @doc Write a balance activity log event.
abmf_log(Type, ServiceId, Bucket, Units, Product, Amount,
		AmountBefore, AmountAfter, Validity, Channel, Requestor,
		RelatedParty, PaymentMeans, Action, Status)
		when ((Type == topup) orelse (Type == adjustment) orelse (Type == delete)
				orelse (Type == transfer) orelse (Type == deduct)
				orelse (Type == reserve) orelse (Type == unreserve)),
		((is_binary(ServiceId)) orelse (ServiceId == undefined)),
		is_list(Bucket), ((Units == cents) orelse (Units == seconds)
				orelse (Units == octets) orelse (Units == messages)),
		(is_integer(AmountBefore) orelse (AmountBefore == undefined)),
		(is_integer(AmountAfter) orelse (AmountAfter == undefined)),
		is_list(Product), is_integer(Amount)->
	Event = [node(), Type, ServiceId, Bucket, Units, Product, Amount,
			AmountBefore, AmountAfter, Validity, Channel, Requestor,
			RelatedParty, PaymentMeans, Action, Status],
	write_log(log_name(abmf_log_name), Event).

-spec abmf_query(Continuation, Start, End, Type, Subscriber,
		Bucket, Units, Product) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Type :: [{type, {MatchType, TypeValue}}] | '_',
		TypeValue :: deduct | reserve | unreserve | transfer | topup | adjustment | '_',
		Subscriber :: [{subscriber, {MatchType, SubscriberValue}}] | '_',
		SubscriberValue :: binary() | '_',
		Bucket :: [{bucket, {MatchType, BucketValue}}] | '_',
		BucketValue :: string() | '_',
		Units :: [{units, {MatchType, UnitsValue}}] | '_',
		UnitsValue :: cents | seconds | octets | messages | '_',
		Product :: [{product, {MatchType, ProductValue}}] | '_',
		ProductValue :: string() | '_',
		MatchType :: exact | like,
		Result :: {Continuation2, Events} | {error, Reason},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [abmf_event()],
		Reason :: term().
%% @doc Query balance activity log events with filters.
abmf_query(Continuation, Start, End, Type, Subscriber, Bucket, Units, Product)
		when (is_integer(Start) orelse (tuple_size(Start) == 2)),
		(is_integer(End) orelse (tuple_size(End) == 2)),
		(is_list(Type) orelse (Type == '_')),
		(is_list(Subscriber) orelse (Subscriber == '_')),
		(is_list(Bucket) orelse (Bucket == '_')),
		(is_list(Units ) orelse (Units == '_')),
		(is_list(Product) orelse (Product == '_')) ->
	MFA = {?MODULE, abmf_query, [Type, Subscriber, Bucket, Units, Product]},
	query_log(Continuation, Start, End, log_name(abmf_log_name), MFA).

-spec auth_to_ecs(Event) -> ECS
	when
		Event :: auth_event(),
		ECS :: {struct, list()}.
%% @doc Convert auth_event to ECS.
auth_to_ecs({TS, N, radius = P, Node, Server,
		Client, EventType, ReqAttributes, ResAttributes}) ->
	auth_to_ecs({TS, N, P, Node, Server, Client, EventType, ReqAttributes,
			ResAttributes}, lists:keyfind(?NasIdentifier, 1, ReqAttributes));
auth_to_ecs({TS, N, diameter = P, Node, Server, Client, Request, Response}) ->
	auth_to_ecs({TS, N, P, Node, Server, Client, Request, Response},
		dia_req_and_res(Request, Response, Client)).
%% @hidden
auth_to_ecs({TS, N, radius = P, Node, Server, {ClientIp, _} = Client,
		EventType, ReqAttributes, ResAttributes}, {_, OriginHost}) ->
	ClientFields = case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			[{"ip", OriginHost1}];
		OriginHost2 when is_list(OriginHost2) ->
			[{"ip", parse_ip(ClientIp)}, {"domain", OriginHost2}]
	end,
	ClientObj = {struct, [{"address", OriginHost}] ++ ClientFields},
	auth_to_ecs({TS, N, radius = P, Node, Server, Client,
			EventType, ReqAttributes, ResAttributes}, ClientObj,
			lists:keyfind(?UserName, 1, ReqAttributes));
auth_to_ecs({TS, N, diameter = Protocol, Node,
		{ServerIP, ServerPort}, _Client, _Request, _Response},
		{EventType, Outcome, App, ClientAddress, ClientIp,
		ClientDomain, OriginDomain, DestinationDom, Subscriber, UserName}) ->
	Now = erlang:system_time(millisecond),
	EventId = integer_to_list(TS) ++ "-" ++ integer_to_list(N),
	ExampleSocket = "http://host.example.net:8080",
	EventObj = {struct, [{"created", ocs_log:iso8601(Now)},
			{"id", EventId},
			{"url", ExampleSocket ++ ?usagePath ++ EventId},
			{"reference", ExampleSocket ++ ?usageSpecPath ++ "AAAAccessUsageSpec"},
			{"dataset", "auth"},
			{"kind", "event"},
			{"category", "authentication"},
			{"type", EventType},
			{"outcome", Outcome}]},
	ServiceObj = {struct, [{"ip", parse_ip(ServerIP)},
			{"type", "sigscale-ocs"},
			{"name", "aaa"},
			{"node", {struct, [{"name", Node}]}}]},
	ClientObj = {struct, [{"ip", ClientIp},
			{"address", ClientAddress},
			{"domain", ClientDomain},
			{"subdomain", OriginDomain}]},
	SourceObj = case UserName of
		[] ->
			case Subscriber of
				undefined ->
					[];
				Sub when is_binary(Sub) ->
					[{"source", {struct, [{"user", {struct, [{"id", Sub}]}}]}}]
			end;
		[U] ->
			case Subscriber of
				undefined ->
					[{"source", {struct, [{"user", {struct, [{"name", U}]}}]}}];
				Sub when is_binary(Sub) ->
					[{"source", {struct, [{"user", {struct,
							[{"id", Sub}, {"name", U}]}}]}}]
			end
	end,
	{struct, [{"@timestamp", ocs_log:iso8601(TS)},
			{"event", EventObj},
			{"service", ServiceObj},
			{"server", {struct, [{"port", ServerPort}]}},
			{"agent", {struct, [{"type", "sigscale-ocs"}]}},
			{"network", {struct, [{"application", App}, {"protocol", Protocol}]}},
			{"client", ClientObj},
			{"destination", {struct, [{"subdomain", DestinationDom}]}}]
			++ SourceObj};
auth_to_ecs(_Event, false) ->
	throw(not_found).
%% @hidden
auth_to_ecs({TS, N, radius = Protocol, Node, {ServerIP, ServerPort}, _Client,
		Type, _ReqAttributes, _ResAttributes}, ClientObj, {_, UserName}) ->
	Now = erlang:system_time(millisecond),
	EventId = integer_to_list(TS) ++ "-" ++ integer_to_list(N),
	ExampleSocket = "http://host.example.net:8080",
	{EventType, Outcome} = case Type of
		accept ->
			{"start", "success"};
		reject ->
			{"end", "failure"};
		change ->
			{"info", "unknown"}
	end,
	EventObj = {struct, [{"created", ocs_log:iso8601(Now)},
			{"id", EventId},
			{"url", ExampleSocket ++ ?usagePath ++ EventId},
			{"reference", ExampleSocket ++ ?usageSpecPath ++ "AAAAccessUsageSpec"},
			{"dataset", "auth"},
			{"kind", "event"},
			{"category", "authentication"},
			{"type", EventType},
			{"outcome", Outcome}]},
	ServiceObj = {struct, [{"ip", parse_ip(ServerIP)},
			{"type", "sigscale-ocs"},
			{"name", "aaa"},
			{"node", {struct, [{"name", Node}]}}]},
	SourceObj = {struct, [{"user", {struct,
			[{"id", UserName}, {"name", UserName}]}}]},
	{struct, [{"@timestamp", ocs_log:iso8601(TS)},
			{"event", EventObj},
			{"service", ServiceObj},
			{"server", {struct, [{"port", ServerPort}]}},
			{"agent", {struct, [{"type", "sigscale-ocs"}]}},
			{"network", {struct, [{"protocol", Protocol}]}},
			{"client", ClientObj},
			{"source", SourceObj}]};
auth_to_ecs(_Event, _ClientObj, false) ->
	throw(not_found).

-spec acct_to_ecs(Event) -> ECS
	when
		Event :: acct_event(),
		ECS :: {struct, list()}.
%% @doc Convert auth_event to ECS.
acct_to_ecs({TS, N, radius = P, Node, Server,
		Type, ReqAttributes, ResAttributes, Rated}) ->
	acct_to_ecs({TS, N, P, Node, Server, Type, ReqAttributes, ResAttributes,
			Rated}, lists:keyfind(?NasIdentifier, 1, ReqAttributes));
acct_to_ecs({TS, N, diameter = P, Node,
		Server, Type, Request, Response, Rated}) ->
	acct_to_ecs({TS, N, P, Node, Server, Type, Request, Response, Rated},
		dia_req_and_res(Request, Response)).
%% @hidden
acct_to_ecs({_TS, _N, radius, _Node, _Server, _Type, ReqAttributes,
		_ResAttributes, _Rated} = Event, {_, OriginHost}) ->
	ClientFields = case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			[{"ip", OriginHost1}];
		OriginHost2 when is_list(OriginHost2) ->
			[{"domain", OriginHost2}]
	end,
	ClientObj = {struct, [{"address", OriginHost}] ++ ClientFields},
	acct_to_ecs(Event, ClientObj, lists:keyfind(?UserName, 1, ReqAttributes));
acct_to_ecs({TS, N, diameter = Protocol, Node,
		{ServerIP, ServerPort}, Type, _Request, _Response, _Rated},
		{Outcome, App, ClientAddress, ClientIp, ClientDomain,
		OriginDomain, DestinationDom, Subscriber, UserName, Reference}) ->
	Now = erlang:system_time(millisecond),
	EventId = integer_to_list(TS) ++ "-" ++ integer_to_list(N),
	ExampleSocket = "http://host.example.net:8080",
	EventObj = {struct, [{"created", ocs_log:iso8601(Now)},
			{"id", EventId},
			{"url", ExampleSocket ++ ?usagePath ++ EventId},
			{"reference", ExampleSocket ++ ?usageSpecPath ++ Reference},
			{"dataset", "acct"},
			{"kind", "event"},
			{"category", "session"},
			{"type", event_type(Type)},
			{"outcome", Outcome}]},
	ServiceObj = {struct, [{"ip", parse_ip(ServerIP)},
			{"type", "sigscale-ocs"},
			{"name", "aaa"},
			{"node", {struct, [{"name", Node}]}}]},
	ClientObj = case {ClientIp, ClientDomain} of
		{undefined, CD} when is_list(CD) ->
			{struct, [{"address", ClientAddress},
					{"domain", CD}, {"subdomain", OriginDomain}]};
		{CIP, undefined} when is_binary(CIP) ->
			{struct, [{"ip", CIP}, {"address", ClientAddress},
					{"subdomain", OriginDomain}]}
	end,
	SourceObj = case {Subscriber, UserName} of
		{undefined, []} ->
			[];
		{undefined, [UN]} when is_binary(UN) ->
			[{"source", {struct, [{"user", {struct,
					[{"name", binary_to_list(UN)}]}}]}}];
		{Sub, []} when is_binary(Sub) ->
			[{"source", {struct, [{"user",
					{struct, [{"id", binary_to_list(Sub)}]}}]}}];
		{Sub, [UN]} when is_binary(Sub), is_binary(UN) ->
			[{"source", {struct, [{"user", {struct,
					[{"id", binary_to_list(Sub)}, {"name", binary_to_list(UN)}]}}]}}]
	end,
	{struct, [{"@timestamp", ocs_log:iso8601(TS)},
			{"event", EventObj},
			{"service", ServiceObj},
			{"server", {struct, [{"port", ServerPort}]}},
			{"agent", {struct, [{"type", "sigscale-ocs"}]}},
			{"network", {struct, [{"application", App}, {"protocol", Protocol}]}},
			{"client", ClientObj},
			{"destination", {struct,
					[{"subdomain", DestinationDom}]}}] ++ SourceObj};
acct_to_ecs(_Event, false) ->
	throw(not_found).
%% @hidden
acct_to_ecs({TS, N, radius = Protocol, Node, {ServerIP, ServerPort},
		Type, _Req, _Res, _Rated}, ClientObj, {_, UserName}) ->
	Now = erlang:system_time(millisecond),
	EventId = integer_to_list(TS) ++ "-" ++ integer_to_list(N),
	ExampleSocket = "http://host.example.net:8080",
	EventObj = {struct, [{"created", ocs_log:iso8601(Now)},
			{"id", EventId},
			{"url", ExampleSocket ++ ?usagePath ++ EventId},
			{"reference", ExampleSocket
					++ ?usageSpecPath ++ "AAAAccountingUsageSpec"},
			{"dataset", "auth"},
			{"kind", "event"},
			{"category", "authentication"},
			{"type", event_type(Type)},
			{"outcome", "success"}]},
	ServiceObj = {struct, [{"ip", parse_ip(ServerIP)},
			{"type", "sigscale-ocs"},
			{"name", "aaa"},
			{"node", {struct, [{"name", Node}]}}]},
	SourceObj = {struct, [{"user", {struct,
			[{"id", UserName}, {"name", UserName}]}}]},
	{struct, [{"@timestamp", ocs_log:iso8601(TS)},
			{"event", EventObj},
			{"service", ServiceObj},
			{"server", {struct, [{"port", ServerPort}]}},
			{"agent", {struct, [{"type", "sigscale-ocs"}]}},
			{"network", {struct, [{"protocol", Protocol}]}},
			{"client", ClientObj},
			{"source", SourceObj}]};
acct_to_ecs(_Event, _ClientObj, false) ->
	throw(not_found).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
event_type(on) ->
	"start";
event_type(off) ->
	"end";
event_type(start) ->
	"start";
event_type(stop) ->
	"end";
event_type(update) ->
	"info";
event_type(interim) ->
	"info";
event_type(final) ->
	"end";
event_type('event') ->
	"start".

%% @hidden
parse_ip({I1, I2, I3, I4}) when is_integer(I1),
		is_integer(I2), is_integer(I3), is_integer(I4) ->
	integer_to_list(I1) ++ "," ++ integer_to_list(I2) ++ ","
			++ integer_to_list(I3) ++ "," ++ integer_to_list(I4).

%% @hidden
dia_req_and_res(#'3gpp_ro_CCR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName, 'Subscription-Id' = SubscriptionId},
		#'3gpp_ro_CCA'{'Result-Code' = undefined}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"unknown", "ro", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, Subscriber, UserName, "AAAAccountingUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"unknown", "ro", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, Subscriber, UserName, "AAAAccountingUsageSpec"}
	end;
dia_req_and_res(#'3gpp_ro_CCR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName, 'Subscription-Id' = SubscriptionId},
		#'3gpp_ro_CCA'{'Result-Code' = ResultCode})
		when ResultCode >= 2000, ResultCode < 3000 ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"success", "ro", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, Subscriber, UserName, "AAAAccountingUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"success", "ro", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, Subscriber, UserName, "AAAAccountingUsageSpec"}
	end;
dia_req_and_res(#'3gpp_ro_CCR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName, 'Subscription-Id' = SubscriptionId},
		#'3gpp_ro_CCA'{'Result-Code' = _ResultCode}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"failure", "ro", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, Subscriber, UserName, "AAAAccountingUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"failure", "ro", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, Subscriber, UserName, "AAAAccountingUsageSpec"}
	end;
dia_req_and_res(#'3gpp_ro_RAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName}, #'3gpp_ro_RAA'{'Result-Code' = undefined}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"unknown", "ro", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, undefined, UserName, "AAAAccountingUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"unknown", "ro", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, undefined, UserName, "AAAAccountingUsageSpec"}
	end;
dia_req_and_res(#'3gpp_ro_RAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName}, #'3gpp_ro_RAA'{'Result-Code' = ResultCode})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"success", "ro", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, undefined, UserName, "AAAAccountingUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"success", "ro", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, undefined, UserName, "AAAAccountingUsageSpec"}
	end;
dia_req_and_res(#'3gpp_ro_RAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName}, #'3gpp_ro_RAA'{'Result-Code' = _ResultCode}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"failure", "ro", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, undefined, UserName, "AAAAccountingUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"failure", "ro", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, undefined, UserName, "AAAAccountingUsageSpec"}
	end;
dia_req_and_res(#'3gpp_gx_CCR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'Subscription-Id' = SubscriptionId},
		#'3gpp_gx_CCA'{'Result-Code' = undefined}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_gx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"unknown", "gx", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, Subscriber, undefined, "AAAPolicyUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"unknown", "gx", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, Subscriber, undefined, "AAAPolicyUsageSpec"}
	end;
dia_req_and_res(#'3gpp_gx_CCR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'Subscription-Id' = SubscriptionId},
		#'3gpp_gx_CCA'{'Result-Code' = ResultCode})
		when ResultCode >= 2000, ResultCode < 3000 ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_gx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"success", "gx", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, Subscriber, undefined, "AAAPolicyUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"success", "gx", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, Subscriber, undefined, "AAAPolicyUsageSpec"}
	end;
dia_req_and_res(#'3gpp_gx_CCR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'Subscription-Id' = SubscriptionId},
		#'3gpp_gx_CCA'{'Result-Code' = _ResultCode}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_gx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"failure", "gx", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, Subscriber, undefined, "AAAPolicyUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"failure", "gx", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, Subscriber, undefined, "AAAPolicyUsageSpec"}
	end;
dia_req_and_res(#'3gpp_gx_RAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm},
		#'3gpp_gx_RAA'{'Result-Code' = undefined}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"unknown", "gx", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, undefined, undefined, "AAAPolicyUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"unknown", "gx", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, undefined, undefined, "AAAPolicyUsageSpec"}
	end;
dia_req_and_res(#'3gpp_gx_RAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm},
		#'3gpp_gx_RAA'{'Result-Code' = ResultCode})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"success", "gx", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, undefined, undefined, "AAAPolicyUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"success", "gx", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, undefined, undefined, "AAAPolicyUsageSpec"}
	end;
dia_req_and_res(#'3gpp_gx_RAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm},
		#'3gpp_gx_RAA'{'Result-Code' = _ResultCode}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"failure", "gx", OriginHost1, OriginHost1, undefined, OriginRealm,
					DesRealm, undefined, undefined, "AAAPolicyUsageSpec"};
		OriginHost2 when is_list(OriginHost2) ->
			{"failure", "gx", OriginHost2, undefined, OriginHost2, OriginRealm,
					DesRealm, undefined, undefined, "AAAPolicyUsageSpec"}
	end.
%% @hidden
dia_req_and_res(#diameter_nas_app_AAR{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#diameter_nas_app_AAA{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "unknown", "nas", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "unknown", "nas", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#diameter_nas_app_AAR{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#diameter_nas_app_AAA{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "success", "nas", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "success", "nas", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#diameter_nas_app_AAR{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#diameter_nas_app_AAA{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "failure", "nas", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "failure", "nas", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#diameter_eap_app_DER{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#diameter_eap_app_DEA{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "unknown", "eap", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "unknown", "eap", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#diameter_eap_app_DER{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#diameter_eap_app_DEA{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "success", "eap", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "success", "eap", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#diameter_eap_app_DER{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#diameter_eap_app_DEA{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "failure", "eap", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "failure", "eap", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_sta_DER'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_sta_DEA'{'Result-Code' = undefined,
		'Subscription-Id' = SubscriptionId}, {Ip, _Port}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_swx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub;
		[#'3gpp_sta_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "sta", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, Subscriber, UserName};
		OriginHost2 ->
			{"end", "unknown", "sta", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, Subscriber, UserName}
	end;
dia_req_and_res(#'3gpp_sta_DER'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_sta_DEA'{'Result-Code' = ResultCode,
		'Subscription-Id' = SubscriptionId}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_swx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub;
		[#'3gpp_sta_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "sta", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, Subscriber, UserName};
		OriginHost2 ->
			{"end", "unknown", "sta", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, Subscriber, UserName}
	end;
dia_req_and_res(#'3gpp_sta_DER'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_sta_DEA'{'Result-Code' = _ResultCode,
		'Subscription-Id' = SubscriptionId}, {Ip, _Port}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_swx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub;
		[#'3gpp_sta_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "failure", "sta", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, Subscriber, UserName};
		OriginHost2 ->
			{"end", "failure", "sta", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, Subscriber, UserName}
	end;
dia_req_and_res(#'3gpp_swm_DER'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swm_DEA'{'Result-Code' = undefined,
		'Subscription-Id' = SubscriptionId}, {Ip, _Port}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_swx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub;
		[#'3gpp_swm_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "unknown", "swm", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, Subscriber, UserName};
		OriginHost2 ->
			{"start", "unknown", "swm", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, Subscriber, UserName}
	end;
dia_req_and_res(#'3gpp_swm_DER'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swm_DEA'{'Result-Code' = ResultCode,
		'Subscription-Id' = SubscriptionId}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_swx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub;
		[#'3gpp_swm_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "success", "swm", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, Subscriber, UserName};
		OriginHost2 ->
			{"start", "success", "swm", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, Subscriber, UserName}
	end;
dia_req_and_res(#'3gpp_swm_DER'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swm_DEA'{'Result-Code' = _ResultCode,
		'Subscription-Id' = SubscriptionId}, {Ip, _Port}) ->
	Subscriber = case SubscriptionId of
		[] ->
			undefined;
		[#'3gpp_swx_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub;
		[#'3gpp_swm_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
			Sub
	end,
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "failure", "swm", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, Subscriber, UserName};
		OriginHost2 ->
			{"start", "failure", "swm", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, Subscriber, UserName}
	end;
dia_req_and_res(#'3gpp_sta_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_sta_STA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "sta", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "unknown", "sta", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_sta_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_sta_STA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "success", "sta", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "success", "sta", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_sta_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_sta_STA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "failure", "sta", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "failure", "sta", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_swm_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swm_STA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "swm", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "unknown", "swm", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_swm_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swm_STA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "success", "swm", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "success", "swm", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_swm_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swm_STA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "failure", "swm", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "failure", "swm", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6b_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6b_STA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "s6b", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "unknown", "s6b", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6b_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6b_STA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "success", "s6b", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "success", "s6b", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6b_STR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6b_STA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "failure", "s6b", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "failure", "s6b", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_swx_RTR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swx_RTA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "swx", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "unknown", "swx", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_swx_RTR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swx_RTA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "success", "swx", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "success", "swx", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_swx_RTR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_swx_RTA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "failure", "swx", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "failure", "swx", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6b_AAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6b_AAA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "unknown", "s6b", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "unknown", "s6b", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6b_AAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6b_AAA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "success", "s6b", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "success", "s6b", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6b_AAR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6b_AAA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "failure", "s6b", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "failure", "s6b", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_AIR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_AIA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "unknown", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "unknown", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_AIR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_AIA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "success", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "success", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_AIR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_AIA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"start", "failure", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"start", "failure", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_ULR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_ULA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"info", "unknown", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"info", "unknown", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_ULR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_ULA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"info", "success", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"info", "success", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_ULR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_ULA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"info", "failure", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"info", "failure", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_PUR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_PUA'{'Result-Code' = undefined}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "unknown", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "unknown", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_PUR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_PUA'{'Result-Code' = ResultCode}, {Ip, _Port})
		when ResultCode >= 2000, ResultCode < 3000 ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "success", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "success", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end;
dia_req_and_res(#'3gpp_s6a_PUR'{'Origin-Realm' = OriginRealm,
		'Origin-Host' = OriginHost, 'Destination-Realm' = DesRealm,
		'User-Name' = UserName},
		#'3gpp_s6a_PUA'{'Result-Code' = _ResultCode}, {Ip, _Port}) ->
	case OriginHost of
		OriginHost1 when is_binary(OriginHost1) ->
			{"end", "failure", "s6a", OriginHost1, OriginHost1,
					undefined, OriginRealm, DesRealm, undefined, UserName};
		OriginHost2 ->
			{"end", "failure", "s6a", OriginHost2, parse_ip(Ip),
					OriginHost2, OriginRealm, DesRealm, undefined, UserName}
	end.

%% @hidden
file_chunk(Log, IoDevice, Type, Cont) when Type == binary; Type == tuple ->
	case disk_log:chunk(Log, Cont) of
		eof ->
			file:close(IoDevice);
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, Log}, {error, Reason}]),
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

-spec btree_search(Log, Start) -> Result
	when
		Log :: disk_log:log(),
		Start :: timestamp(),
		Result :: disk_log:continuation() | {error, Reason},
		Reason :: term().
%% @doc Binary tree search of multi file wrap disk_log.
%% @private
btree_search(Log, Start) ->
	btree_search(Log, Start, disk_log:chunk(Log, start, 1)).
%% @hidden
btree_search(Log, Start, {Cont, Terms, BadBytes}) ->
	error_logger:error_report(["Error reading log",
			{log, Log},{badbytes, BadBytes}]),
	btree_search(Log, Start, {Cont, Terms});
btree_search(_Log, _Start, eof) ->
	start;
btree_search(_Log, _Start, {error, Reason}) ->
	{error, Reason};
btree_search(_Log, Start, {_Cont, [R]}) when element(1, R) >= Start ->
	start;
btree_search(Log, Start, {Cont, [R]}) when element(1, R) < Start ->
	case disk_log:info(Log) of
		{error,no_such_log} ->
			{error,no_such_log};
		InfoList ->
			Step = case lists:keyfind(size, 1, InfoList) of
				{size, {_MaxBytes, MaxFiles}} when (MaxFiles rem 2) == 0, MaxFiles > 2 ->
					(MaxFiles div 2) - 1;
				{size, {_MaxBytes, MaxFiles}} ->
					MaxFiles div 2
			end,
			btree_search(Log, Start, Step, start, element(1, R), disk_log:chunk_step(Log, Cont, Step))
	end.
%% @hidden
btree_search(Log, Start, Step, PrevCont, PrevChunkStart, {ok, Cont}) ->
	btree_search(Log, Start, Step, PrevCont, PrevChunkStart, Cont,
			disk_log:chunk(Log, Cont, 1));
btree_search(_Log, _Start, Step, PrevCont, _PrevChunkStart, {error, end_of_log})
		when Step == 1; Step == -1 ->
	PrevCont;
btree_search(Log, Start, _Step, PrevCont, PrevChunkStart, {error, end_of_log}) ->
	case disk_log:info(Log) of
		{error,no_such_log} ->
			{error,no_such_log};
		LogInfo ->
			case lists:keyfind(current_file, 1, LogInfo) of
				{current_file, CurrentFile} when (CurrentFile rem 2) == 0, CurrentFile > 2 ->
					Step1 = (CurrentFile div 2) - 1,
					btree_search(Log, Start, Step1, PrevCont, PrevChunkStart,
							disk_log:chunk_step(Log, PrevCont, Step1));
				{current_file, 1} ->
					start;
				{current_file, CurrentFile} ->
					Step1 = CurrentFile div 2,
					btree_search(Log, Start, Step1, PrevCont, PrevChunkStart,
							disk_log:chunk_step(Log, PrevCont, Step1))
			end
	end;
btree_search(_Log, _Start, _Step, _PrevCont, _PrevChunkStart, {error, Reason}) ->
	{error, Reason}.
%% @hidden
btree_search(_Log, Start, 1, PrevCont, _PrevChunkStart, _Cont, {_NextCont, [R]})
		when element(1, R) >= Start ->
	PrevCont;
btree_search(_Log, _Start, Step, _PrevCont, PrevChunkStart, Cont, {_NextCont, [R]})
		when Step < 0, element(1, R) > PrevChunkStart ->
	Cont;
btree_search(_Log, _Start, Step, PrevCont, PrevChunkStart, _Cont, {_NextCont, [R]})
		when Step > 0, element(1, R) < PrevChunkStart ->
	PrevCont;
btree_search(_Log, Start, -1, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when element(1, R) < Start ->
	Cont;
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step == 1; Step == -1 ->
	btree_search(Log, Start, Step, Cont, element(1, R), disk_log:chunk_step(Log, Cont, Step));
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step > 2, element(1, R) < Start, (Step rem 2) == 0 ->
	NextStep = (Step div 2) - 1,
	btree_search(Log, Start, NextStep, Cont, element(1, R), disk_log:chunk_step(Log, Cont, NextStep));
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step > 0, element(1, R) < Start ->
	NextStep = Step div 2,
	btree_search(Log, Start, NextStep, Cont, element(1, R), disk_log:chunk_step(Log, Cont, NextStep));
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step > 0, element(1, R) >= Start ->
	NextStep = -(Step div 2),
	btree_search(Log, Start, NextStep, Cont, element(1, R), disk_log:chunk_step(Log, Cont, NextStep));
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step < -2, element(1, R) >= Start, (Step rem 2) == 0 ->
	NextStep = (Step div 2) - 1,
	btree_search(Log, Start, NextStep, Cont, element(1, R), disk_log:chunk_step(Log, Cont, NextStep));
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step < 0, element(1, R) >= Start ->
	NextStep = Step div 2,
	btree_search(Log, Start, NextStep, Cont, element(1, R), disk_log:chunk_step(Log, Cont, NextStep));
btree_search(Log, Start, Step, _PrevCont, _PrevChunkStart, Cont, {_NextCont, [R]})
		when Step < 0, element(1, R) < Start ->
	NextStep = -(Step div 2),
	btree_search(Log, Start, NextStep, Cont, element(1, R), disk_log:chunk_step(Log, Cont, NextStep));
btree_search(_Log, _Start, _Step, _PrevCont, _PrevChunkStart, Cont, eof) ->
	Cont;
btree_search(_Log, _Start, _Step, _PrevCont, _PrevChunkStart, _Cont, {error, Reason}) ->
	{error, Reason}.

-spec get_range(Log, Start, End, Cont) -> Result
	when
		Log :: disk_log:log(),
		Start :: timestamp(),
		End :: timestamp(),
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
%% @hidden
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

-spec ipdr_codec(Event) -> IPDRs
	when
		Event :: tuple(),
		IPDRs :: [IPDR],
		IPDR :: #ipdr_wlan{} | #ipdr_voip{} | undefined.
%% @doc Convert `ocs_acct' log entry to IPDR log entry.
%% @private
ipdr_codec(Event) when size(Event) > 6,
		((element(3, Event) == radius)
		orelse (element(3, Event) == diameter)
		orelse (element(3, Event) == nrf)),
		element(6, Event) == stop ->
	TimeStamp = element(1, Event),
	Protocol = element(3, Event),
	ReqType = element(6, Event),
	Req = element(7, Event),
	Res = case size(Event) > 7 of
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
	ipdr_codec1(Protocol, TimeStamp, ReqType, Req, Res, Rated, []).
%% @hidden
ipdr_codec1(Protocol, TimeStamp, ReqType, Req, Res, [H | T], Acc)
		when Protocol == diameter; Protocol == nrf ->
	IPDR = ipdr_codec2(Protocol, TimeStamp, ReqType, Req, Res, H),
	ipdr_codec1(Protocol, TimeStamp, ReqType, Req, Res, T, [IPDR | Acc]);
ipdr_codec1(Protocol, TimeStamp, ReqType, Req, Res, undefined, Acc)
		when Protocol == diameter; Protocol == nrf ->
	[ipdr_codec2(Protocol, TimeStamp, ReqType, Req, Res, undefined)];
ipdr_codec1(radius, TimeStamp, ReqType, Req, Res, Rated, Acc) ->
	[ipdr_codec2(radius, TimeStamp, ReqType, Req, Res, Rated)];
ipdr_codec1(_Protocol, _TimeStamp, _ReqType, _Req, _Res, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
ipdr_codec2(diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Id,
		'Service-Information' = []} = Req, Res, Rated) ->
	case binary:part(Id, size(Id), -8) of
		<<"3gpp.org">> ->
			ServiceType = binary:part(Id, byte_size(Id) - 14, 5),
			ipdr_codec3([], ServiceType, Protocol,
					TimeStamp, ReqType, Req, Res, Rated);
		_ ->
			exit(service_type_not_found)
	end;
ipdr_codec2(diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Id,
		'Service-Information' = [ServiceInfo]} = Req, Res, Rated) ->
	case binary:part(Id, size(Id), -8) of
		<<"3gpp.org">> ->
			ServiceType = binary:part(Id, byte_size(Id) - 14, 5),
			ipdr_codec3(ServiceInfo, ServiceType, Protocol,
					TimeStamp, ReqType, Req, Res, Rated);
		_ ->
			exit(service_type_not_found)
	end;
ipdr_codec2(nrf = Protocol, TimeStamp, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Id,
		"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Res, Rated) ->
	case lists:sublist(Id, length(Id) - 7, 8) of
		"3gpp.org" ->
			ServiceType = lists:sublist(Id, length(Id) - 13, 5),
			ipdr_codec3(ServiceInfo, list_to_binary(ServiceType),
					Protocol, TimeStamp, ReqType, Req, Res, Rated);
		_ ->
			exit(service_type_not_found)
	end;
ipdr_codec2(nrf = Protocol, TimeStamp, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Id} | _]} = Req,
		Res, Rated) ->
	case lists:sublist(Id, length(Id) - 7, 8) of
		"3gpp.org" ->
			ServiceType = lists:sublist(Id, length(Id) - 13, 5),
			ipdr_codec3([], list_to_binary(ServiceType),
					Protocol, TimeStamp, ReqType, Req, Res, Rated);
		_ ->
			exit(service_type_not_found)
	end;
ipdr_codec2(radius, TimeStamp, ReqType, Req, Res, Rated)
		when is_list(Req) ->
	ipdr_wlan(radius, TimeStamp, ReqType, Req, Res, Rated);
ipdr_codec2(_, _, _, _, _, _) ->
	undefined.
%% @hidden
ipdr_codec3(#'3gpp_ro_Service-Information'{'PS-Information' = [_PSInfo]},
		_ServiceType, Protocol, TimeStamp, ReqType, Req, Res, Rated) ->
	ipdr_wlan(Protocol, TimeStamp, ReqType, Req, Res, Rated);
ipdr_codec3(#'3gpp_ro_Service-Information'{'IMS-Information' = [_IMSInfo]},
		ServiceType, Protocol, TimeStamp, ReqType, Req, Res, Rated) ->
	ipdr_ims(ServiceType, Protocol, TimeStamp, ReqType, Req, Res, Rated);
ipdr_codec3(ServiceInfo, ServiceType, nrf, TimeStamp, ReqType, Req, Res, Rated) ->
	case lists:keymember("nodeFunctionality", 1, ServiceInfo) of
		true ->
			ipdr_ims(ServiceType, nrf, TimeStamp, ReqType, Req, Res, Rated);
		false ->
			ipdr_wlan(nrf, TimeStamp, ReqType, Req, Res, Rated)
	end;
ipdr_codec3(_, _ServiceType, Protocol, TimeStamp, ReqType, Req, Res, Rated) ->
	ipdr_wlan(Protocol, TimeStamp, ReqType, Req, Res, Rated).

-spec ipdr_ims(ServiceType, Protocol, TimeStamp, ReqType, Req, Res, Rated) -> IPDR
	when
		ServiceType :: binary(),
		Protocol :: diameter | nrf,
		TimeStamp :: timestamp(),
		ReqType :: stop,
		Req :: #'3gpp_ro_CCR'{},
		Res :: #'3gpp_ro_CCA'{},
		Rated :: [#rated{}] | undefined,
		IPDR :: #ipdr_voip{}.
%% @doc CODEC for IMS VOIP
%% @todo ipdr_data
ipdr_ims(<<"32251">> = _Data, _Protocol, _TimeStamp, _ReqType, _Req, _Res, _Rated) ->
	exit(not_found);
ipdr_ims(<<"32260">> = _Voice, Protocol, TimeStamp, ReqType, Req, Res, Rated) ->
	ipdr_ims_voip(Protocol, TimeStamp, ReqType, Req, Res, Rated).

-spec ipdr_ims_voip(Protocol, TimeStamp, ReqType, Req, Res, Rated) -> IPDR
	when
		Protocol :: diameter | nrf,
		TimeStamp :: timestamp(),
		ReqType :: stop,
		Req :: #'3gpp_ro_CCR'{} | map(),
		Res :: #'3gpp_ro_CCA'{} | map(),
		Rated :: [#rated{}] | undefined,
		IPDR :: #ipdr_voip{}.
ipdr_ims_voip(Protocol, TimeStamp, ReqType, Req, Res, Rated) ->
	ipdr_ims_voip1(record_info(fields, ipdr_voip), Protocol,
			TimeStamp, ReqType, Req, Res, Rated, #ipdr_voip{}).
%% @hidden
ipdr_ims_voip1([ipdrCreationTime | T], Protocol, TimeStamp,
		ReqType, Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{ipdrCreationTime = iso8601(TimeStamp)},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([endTime | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Event-Timestamp' = [EventTimestamp]} = Req,
		Res, Rated, IPDR) ->
	GS = calendar:datetime_to_gregorian_seconds(EventTimestamp),
	EventTimeMilliSeconds = (GS - ?EPOCH) * 1000,
	NewIPDR = IPDR#ipdr_voip{endTime = iso8601(EventTimeMilliSeconds)},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([endTime | T], nrf = Protocol, TimeStamp, ReqType,
		#{"beginTimeStamp" := BeginTimeStamp} = Req,
		Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{endTime = BeginTimeStamp},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([endTime | T], nrf = Protocol, TimeStamp, ReqType,
		#{"invocationTimeStamp" := InvocationTimeStamp} = Req,
		Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{endTime = InvocationTimeStamp},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([callCompletionCode | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [ServiceCreditControl]} = Req,
		Res, Rated, IPDR) ->
	case ServiceCreditControl of
		#'3gpp_ro_Multiple-Services-Credit-Control'{'Reporting-Reason' = [ReportReason]} ->
			NewIPDR = IPDR#ipdr_voip{callCompletionCode = ReportReason},
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
		_ ->
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
	end;
ipdr_ims_voip1([originalDestinationId | T], Protocol, TimeStamp,
		ReqType, Req, Res, Rated, IPDR) ->
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR);
ipdr_ims_voip1([hostName | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Origin-Host' = OriginHost} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{hostName = binary_to_list(OriginHost)},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([hostName | T], nrf = Protocol, TimeStamp, ReqType,
		#{"nfConsumerIdentification" := #{"nFFqdn" := FQDN}} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{hostName = FQDN},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([hostName | T], nrf = Protocol, TimeStamp, ReqType,
		#{"nfConsumerIdentification" := #{"nFName" := NfName}} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{hostName = NfName},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([subscriberId | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Subscription-Id' = [SubscriptionID | _]} = Req, Res, Rated, IPDR) ->
	case SubscriptionID of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Subscriber} ->
			NewIPDR = IPDR#ipdr_voip{subscriberId = binary_to_list(Subscriber)},
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
		_ ->
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
	end;
ipdr_ims_voip1([subscriberId | T], nrf = Protocol, TimeStamp, ReqType,
		#{"subscriptionId" := [SubscriptionID | _]} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{subscriberId = SubscriptionID},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([uniqueCallID | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Service-Information' = [ServiceInfo]} = Req, Res, Rated, IPDR) ->
	case ServiceInfo of
		#'3gpp_ro_Service-Information'{'IMS-Information' = [IMSINFO]} ->
			case IMSINFO of
				#'3gpp_ro_IMS-Information'{'IMS-Charging-Identifier' = [UniqueID]} ->
					NewIPDR = IPDR#ipdr_voip{uniqueCallID = binary_to_list(UniqueID)},
					ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
				_ ->
					ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
			end;
		_ ->
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
	end;
ipdr_ims_voip1([uniqueCallID | T], nrf = Protocol, TimeStamp, ReqType,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Res, Rated, IPDR) ->
	case lists:keyfind("callReferenceNumber", 1, ServiceInfo) of
		{_, CallReferenceNumber} ->
			NewIPDR = IPDR#ipdr_voip{uniqueCallID = CallReferenceNumber},
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
		false ->
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
	end;
ipdr_ims_voip1([disconnectReason | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Service-Information' = [ServiceInfo]} = Req, Res, Rated, IPDR) ->
	case ServiceInfo of
		#'3gpp_ro_Service-Information'{'IMS-Information' = [IMSINFO]} ->
			case IMSINFO of
				#'3gpp_ro_IMS-Information'{'Cause-Code' = [CauseCode]} ->
					NewIPDR = IPDR#ipdr_voip{disconnectReason = CauseCode},
					ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
				_ ->
					ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
			end;
		_ ->
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
	end;
ipdr_ims_voip1([disconnectReason | T], nrf = Protocol, TimeStamp, ReqType,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Res, Rated, IPDR) ->
	NewIPDR = case lists:keyfind("isupCause", 1, ServiceInfo) of
		{_, {struct, IsupCause}} ->
			case lists:keyfind("causeValue", 1, IsupCause) of
				{_, CauseValue} ->
					IPDR#ipdr_voip{disconnectReason = CauseValue};
				_ ->
					IPDR
			end;
		_ ->
			IPDR
	end,
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([destinationID | T], diameter = Protocol, TimeStamp, ReqType,
		#'3gpp_ro_CCR'{'Service-Information' = [ServiceInfo]} = Req, Res, Rated, IPDR) ->
	case ServiceInfo of
		#'3gpp_ro_Service-Information'{'IMS-Information' = [IMSINFO]} ->
			case IMSINFO of
				#'3gpp_ro_IMS-Information'{'Called-Party-Address' = [CalledParty]} ->
					Prefix = "tel:",
					CParty1 = binary_to_list(CalledParty),
					case lists:prefix(Prefix, CParty1) of
						true ->
							{_, CParty2} = lists:split(4, CParty1),
							NewIPDR = IPDR#ipdr_voip{destinationID = CParty2},
							ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
						false ->
							NewIPDR = IPDR#ipdr_voip{destinationID = CParty1},
							ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR)
					end;
				_ ->
					ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
			end;
		_ ->
			ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR)
	end;
ipdr_ims_voip1([destinationID | T], nrf = Protocol, TimeStamp, ReqType,
		#{"serviceRating" := [#{"destinationId" := {array,
				[{struct, DestinationId} | _]}} | _]} = Req,
		Res, Rated, IPDR) ->
	NewIPDR = case lists:keyfind("destinationIdData", 1, DestinationId) of
		{_, DestinationIdData} ->
			case lists:keyfind("destinationIdData", 1, DestinationIdData) of
				{_, CauseValue} ->
					IPDR#ipdr_voip{destinationID = CauseValue};
				_ ->
					IPDR
			end;
		_ ->
			IPDR
	end,
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([chargeAmount | T], Protocol, TimeStamp, ReqType, Req, Res, undefined, IPDR) ->
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, undefined, IPDR);
ipdr_ims_voip1([chargeAmount | T], Protocol, TimeStamp, ReqType,
		Req, Res, [#rated{bucket_value = BValue, bucket_type = BType,
		tariff_type = TType, tax_excluded_amount = TxExAmount,
		price_type = PType, usage_rating_tag = URating, product = Product} | _], IPDR) ->
	NewIPDR = IPDR#ipdr_voip{chargeAmount = TxExAmount, bucketValue = BValue,
			bucketType = BType, tariffType = TType, priceType = PType,
			usageRating = URating, product = Product},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, undefined, NewIPDR);
ipdr_ims_voip1([_ | T], Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR) ->
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, IPDR);
ipdr_ims_voip1([], _Protocol, _TimeStamp, _ReqType, _Req, _Res, _Rated, IPDR) ->
	IPDR.

-spec ipdr_wlan(Protocol, TimeStamp, ReqType, Req, Res, Rated) -> IPDRWlan
	when
		Protocol :: radius | diameter | nrf,
		TimeStamp :: timestamp(),
		ReqType :: stop,
		Req :: [tuple()] | #'3gpp_ro_CCR'{} | map() | undefined,
		Res :: [tuple()] | #'3gpp_ro_CCA'{} | map() | undefined,
		Rated :: #rated{} | undefined,
		IPDRWlan :: #ipdr_wlan{}.
%% @doc CODEC for IPDR Wlan
ipdr_wlan(Protocol, TimeStamp, ReqType, Req, Res, Rated) ->
	ipdr_wlan1(record_info(fields, ipdr_wlan), Protocol, TimeStamp,
			ReqType, Req, Res, Rated, #ipdr_wlan{}).
%% @hidden
ipdr_wlan1([ipdrCreationTime | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{ipdrCreationTime = iso8601(TimeStamp)},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([username | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Username = proplists:get_value(?UserName, Req),
	NewIPDR = IPDR#ipdr_wlan{username = Username},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([username | T], diameter = Protocol, TimeStamp, stop,
		#'3gpp_ro_CCR'{'Subscription-Id' = [SubscriptionID, _]} = Req, Resp,
		Rated, IPDR) ->
	case SubscriptionID of
		#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Subscriber} ->
			NewIPDR = IPDR#ipdr_wlan{username = Subscriber},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
		_ ->
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR)
	end;
ipdr_wlan1([scId | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([acctSessionId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	SessionId = proplists:get_value(?AcctSessionId, Req),
	NewIPDR = IPDR#ipdr_wlan{acctSessionId = SessionId},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([acctSessionId | T], diameter = Protocol, TimeStamp, stop,
		#'3gpp_ro_CCR'{'Session-Id' = SessionId} = Req, Resp, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{acctSessionId = SessionId},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([acctSessionId | T], nrf = Protocol, TimeStamp, stop,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Resp, Rated, IPDR) ->
	NewIPDR = case lists:keyfind("pduSessionInformation", 1, ServiceInfo) of
		{_, {struct, PduSessionInformation}} ->
			case lists:keyfind("pduSessionID", 1, PduSessionInformation) of
				{_, PduSessionId} ->
					IPDR#ipdr_wlan{acctSessionId = PduSessionId};
				_ ->
					IPDR
			end;
		_ ->
			IPDR
	end,
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([callingStationId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	CallingParty = proplists:get_value(?CallingStationId, Req),
	NewIPDR = IPDR#ipdr_wlan{callingStationId = CallingParty},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([calledStationId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	CalledParty = proplists:get_value(?CalledStationId, Req),
	NewIPDR = IPDR#ipdr_wlan{calledStationId = CalledParty},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([callingStationId | T], diameter = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([calledStationId | T], diameter = Protocol, TimeStamp, stop,
		#'3gpp_ro_CCR'{'Service-Information' = [ServiceInfo]} = Req, Resp, Rated, IPDR) ->
	case ServiceInfo of
		#'3gpp_ro_Service-Information'{'PS-Information' = [PSInfo]} ->
			case PSInfo of
				#'3gpp_ro_PS-Information'{'Called-Station-Id' = CalledStationId} ->
					NewIPDR = IPDR#ipdr_wlan{calledStationId = CalledStationId},
					ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
				_ ->
					ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR)
			end;
		_ ->
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR)
	end;
ipdr_wlan1([calledStationId | T], nrf = Protocol, TimeStamp, stop,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Resp, Rated, IPDR) ->
	NewIPDR = case lists:keyfind("apn", 1, ServiceInfo) of
		{_, APN} ->
			IPDR#ipdr_wlan{calledStationId = APN};
		_ ->
			case lists:keyfind("pduSessionInformation", 1, ServiceInfo) of
				{_, {struct, PduSessionInformation}} ->
					case lists:keyfind("dnnId", 1, PduSessionInformation) of
						{_, DnnId} ->
							IPDR#ipdr_wlan{calledStationId = DnnId};
						_ ->
							IPDR
					end;
				_ ->
					IPDR
			end
	end,
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([sessionTerminateCause | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Cause = proplists:get_value(?AcctTerminateCause, Req),
	NewIPDR = IPDR#ipdr_wlan{sessionTerminateCause = Cause},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([nasIpAddress | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	case radius_attributes:find(?NasIpAddress, Req) of
		{ok, Address} ->
			NewIPDR = IPDR#ipdr_wlan{nasIpAddress = inet:ntoa(Address)},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
		{error, not_found} ->
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR)
	end;
ipdr_wlan1([nasIpAddress | T], diameter = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([nasId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Identifier = proplists:get_value(?NasIdentifier, Req),
	NewIPDR = IPDR#ipdr_wlan{nasId = Identifier},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([nasId | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([userIpAddress | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	case radius_attributes:find(?FramedIpAddress, Req) of
		{ok, Address} ->
			NewIPDR = IPDR#ipdr_wlan{userIpAddress = inet:ntoa(Address)},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
		{error, not_found} ->
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR)
	end;
ipdr_wlan1([userIpAddress | T], diameter = Protocol, TimeStamp, stop,
		#'3gpp_ro_CCR'{'Service-Information' = [ServiceInfo]} = Req, Resp, Rated, IPDR) ->
	NewIPDR = case ServiceInfo of
		#'3gpp_ro_Service-Information'{'PS-Information' = [PSInfo]} ->
			case PSInfo of
				#'3gpp_ro_PS-Information'{'PDP-Address' = [PdpAddress | _]} ->
					IPDR#ipdr_wlan{userIpAddress = inet:ntoa(PdpAddress)};
				_ ->
					IPDR
			end;
		_ ->
			IPDR
	end,
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([userIpAddress | T], nrf = Protocol, TimeStamp, stop,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Resp, Rated, IPDR) ->
	NewIPDR = case lists:keyfind("pdpAddress", 1, ServiceInfo) of
		{_, PdpAddress} ->
			IPDR#ipdr_wlan{userIpAddress = PdpAddress};
		_ ->
			case lists:keyfind("pduSessionInformation", 1, ServiceInfo) of
				{_, {struct, PduSessionInformation}} ->
					case lists:keyfind("pduAddress", 1, PduSessionInformation) of
						{_, {struct, PduAddress}} ->
							case lists:keyfind("pduIPv4Address", 1, PduAddress) of
								{_, PduIpv4Address} ->
									IPDR#ipdr_wlan{userIpAddress = PduIpv4Address};
								_ ->
									IPDR
							end;
						_ ->
							IPDR
					end;
				_ ->
					IPDR
			end
	end,
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([sessionDuration | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
		SessionTime = proplists:get_value(?AcctSessionTime, Req),
		NewIPDR = IPDR#ipdr_wlan{sessionDuration = SessionTime},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([sessionDuration | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([inputOctets | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Octets = proplists:get_value(?AcctInputOctets, Req, 0),
	case radius_attributes:find(?AcctInputGigawords, Req) of
		{ok, GigaWords} ->
			GigaOctets = (GigaWords * (16#ffffffff + 1)) + Octets,
			NewIPDR = IPDR#ipdr_wlan{inputOctets = GigaOctets},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
		{error, not_found} ->
			NewIPDR = IPDR#ipdr_wlan{inputOctets = Octets},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR)
	end;
ipdr_wlan1([inputOctets | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([outputOctets | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Octets = proplists:get_value(?AcctOutputOctets, Req, 0),
	case radius_attributes:find(?AcctOutputGigawords, Req) of
		{ok, GigaWords} ->
			GigaOctets = (GigaWords * (16#ffffffff + 1)) + Octets,
			NewIPDR = IPDR#ipdr_wlan{outputOctets = GigaOctets},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
		{error, not_found} ->
			NewIPDR = IPDR#ipdr_wlan{outputOctets = Octets},
			ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR)
	end;
ipdr_wlan1([outputOctets | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([gmtSessionStartDateTime | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([gmtSessionEndDateTime | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	EndTime = case radius_attributes:find(?AcctDelayTime, Req) of
		{ok, DelayTime} ->
			TimeStamp - (DelayTime * 1000);
		{error, not_found} ->
			TimeStamp
	end,
	StartTime = case radius_attributes:find(?AcctSessionTime, Req) of
		{ok, Duration} ->
			iso8601(EndTime - (Duration * 1000));
		{error, not_found} ->
			undefined
	end,
	NewIPDR = IPDR#ipdr_wlan{gmtSessionStartDateTime = StartTime,
			gmtSessionEndDateTime = iso8601(EndTime)},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([gmtSessionEndDateTime | T], diameter = Protocol, TimeStamp, stop,
		#'3gpp_ro_CCR'{'Event-Timestamp' = [EventTimestamp]} = Req, Resp, Rated, IPDR) ->
	GS = calendar:datetime_to_gregorian_seconds(EventTimestamp),
	EventTimeMilliSeconds = (GS - ?EPOCH) * 1000,
	NewIPDR = IPDR#ipdr_wlan{gmtSessionEndDateTime = iso8601(EventTimeMilliSeconds)},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([gmtSessionEndDateTime | T], nrf = Protocol, TimeStamp, stop,
		#{"beginTimeStamp" := BeginTimeStamp} = Req, Resp, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{gmtSessionEndDateTime = BeginTimeStamp},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([unitOfMeasure | T], Protocol, TimeStamp, stop, Req, Resp,
		#rated{product = Product, bucket_type = BType, bucket_value = BValue,
		price_type = PType, tariff_type = TType, currency = Currency,
		tax_excluded_amount = Amount, usage_rating_tag = non_included} = Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{unitOfMeasure = BType, chargeAmount = Amount,
			chargeCurrencyType = Currency, bucketValue = BValue,
			bucketType = BType, tariffType = TType, priceType = PType,
			usageRating = non_included, product = Product},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([unitOfMeasure | T], Protocol, TimeStamp, stop, Req, Resp,
		#rated{product = Product, price_type = PType, tariff_type = TType,
				bucket_type = BType, bucket_value = BValue,
				usage_rating_tag = included} = Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{unitOfMeasure = BType, bucketType = BType,
			tariffType = TType, priceType = PType, usageRating = included,
			product = Product, bucketValue = BValue},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([class | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Class = proplists:get_value(?Class, Req),
	NewIPDR = IPDR#ipdr_wlan{class = Class},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([class | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([chargeableUnit| T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([chargeAmount | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([chargeCurrencyType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([otherParty | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([taxPercentage | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([taxAmount | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([taxType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([seqNum | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([homeServiceProviderType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([homeServiceProvider| T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([accessProviderType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([accessServiceProvider | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationName | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationCountryCode | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationStateProvince | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationCity | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationGeocode | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([locationGeocodeType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([nasPortType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([paymentType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([networkConnectionType | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([billingClassOfService | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([intermediaryName| T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([_ | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([], _Protocol, _TimeStamp, _Flag, _Req, _Resp, _Rated, IPDR) ->
	IPDR.

%% @hidden
ipdr_csv(Log, IoDevice, {Cont, [#ipdrDocWLAN{} | T]}) ->
	Header = [<<"Creation Time;Sequence Number;Username;">>,
			<<"Accounting Session ID;User IP Address;Calling Station ID;">>,
			<<"Called Station ID;NAS IP Address;NAS Identifier;">>,
			<<"Class;Session Terminate Cause;Session Duration;">>,
			<<"Input Octets;Output Octets;Chargeable Quantity;">>,
			<<"Bucket Type;Bucket Value;Tariff Type;">>,
			<<"Product;Price Type;Usage Rating;Charge Amount;">>, $\r, $\n],
	case file:write(IoDevice, Header) of
		ok ->
			ipdr_csv(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, {Cont, [#ipdrDocVoIP{} | T]}) ->
	Header = [<<"Creation Time;Sequence Number;Subscriber ID;">>,
			<<"Unique Call ID;Destination ID;Call Completion Code;">>,
			<<"Disconnect Reason;Host Name;">>,
			<<"Bucket Type;Bucket Value;Tariff Type;">>,
			<<"Product;Price Type;Usage Rating;Charge Amount;">>, $\r, $\n],
	case file:write(IoDevice, Header) of
		ok ->
			ipdr_csv(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, {Cont, [#ipdr_voip{} = I | T]}) ->
	Time = list_to_binary(I#ipdr_voip.ipdrCreationTime),
	Seq = integer_to_binary(I#ipdr_voip.seqNum),
	SubId = case I#ipdr_voip.subscriberId of
		undefined ->
			<<>>;
		SID ->
			list_to_binary(SID)
	end,
	UniqueId = case I#ipdr_voip.uniqueCallID of
		undefined ->
			<<>>;
		UID ->
			list_to_binary(UID)
	end,
	BType = case I#ipdr_voip.bucketType of
		undefined ->
			<<>>;
		BT ->
			atom_to_binary(BT, latin1)
	end,
	BValue = case I#ipdr_voip.bucketValue of
		undefined ->
			<<>>;
		BV ->
			integer_to_binary(BV)
	end,
	TType = case I#ipdr_voip.tariffType of
		undefined ->
			<<>>;
		TP ->
			atom_to_binary(TP, latin1)
	end,
	Prod = case I#ipdr_voip.product of
		undefined ->
			<<>>;
		P ->
		 term_to_binary(P)
	end,
	PType = case I#ipdr_voip.priceType of
		undefined ->
			<<>>;
		PT ->
			atom_to_binary(PT, latin1)
	end,
	URating = case I#ipdr_voip.usageRating of
		undefined ->
			<<>>;
		UR ->
			atom_to_binary(UR, latin1)
	end,
	ChargeA = case I#ipdr_voip.chargeAmount of
		undefined ->
			<<>>;
		CA ->
		 list_to_binary(ocs_rest:millionths_out(CA))
	end,
	Dest = case I#ipdr_voip.destinationID of
		undefined ->
			<<>>;
		DestID ->
			list_to_binary(DestID)
	end,
	CCCode = case I#ipdr_voip.callCompletionCode of
		undefined ->
			<<>>;
		ComCode when is_integer(ComCode) ->
			integer_to_list(ComCode);
		ComCode when is_list(ComCode) ->
			list_to_binary(ComCode)
	end,
	DiscReason = case I#ipdr_voip.disconnectReason of
		undefined ->
			<<>>;
		Disc when is_integer(Disc) ->
			integer_to_binary(Disc);
		Disc when is_list(Disc) ->
			list_to_binary(Disc)
	end,
	HostName = case I#ipdr_voip.hostName of
		undefined ->
			<<>>;
		HN ->
			list_to_binary(HN)
	end,
	IPDR = [Time, $;, Seq, $;, SubId, $;, UniqueId, $;, Dest, $;,
			CCCode, $;, DiscReason, $;, HostName, $;, BType, $;,
			BValue, $;, TType, $;, Prod, $;, PType, $;, URating, $;,
			ChargeA, $\r, $\n],
	case file:write(IoDevice, IPDR) of
		ok ->
			ipdr_csv(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, {Cont, [I | T]})
		when ((size(I) =:= 48) and (element(1, I) == ipdr_wlan)) ->
	ipdr_csv(Log, IoDevice, {Cont, [idpr_convert(I) | T]});
ipdr_csv(Log, IoDevice, {Cont, [#ipdr_wlan{} = I | T]}) ->
	Time = list_to_binary(I#ipdr_wlan.ipdrCreationTime),
	Seq = integer_to_binary(I#ipdr_wlan.seqNum),
	User = case I#ipdr_wlan.username of
		undefined ->
			<<>>;
		US ->
			US
	end,
	Sess = case I#ipdr_wlan.acctSessionId of
		undefined ->
			<<>>;
		SI ->
			SI
	end,
	IP = case I#ipdr_wlan.userIpAddress of
		undefined ->
			<<>>;
		IpAddress ->
			IpAddress
	end,
	Calling = case I#ipdr_wlan.callingStationId of
		undefined ->
			<<>>;
		CgID ->
			CgID
	end,
	Called = case I#ipdr_wlan.calledStationId of
		undefined ->
			<<>>;
		CdID ->
			CdID
	end,
	NasIP = case I#ipdr_wlan.nasIpAddress of
		undefined ->
			<<>>;
		NIP ->
			NIP
	end,
	NasID = case I#ipdr_wlan.nasId of
		undefined ->
			<<>>;
		NID ->
			NID
	end,
	Duration = case I#ipdr_wlan.sessionDuration of
		undefined ->
			<<>>;
		DU ->
			integer_to_binary(DU)
	end,
	Input = case I#ipdr_wlan.inputOctets of
		undefined ->
			<<>>;
		IN ->
			integer_to_binary(IN)
	end,
	Output = case I#ipdr_wlan.outputOctets of
		undefined ->
			<<>>;
		OUT ->
			integer_to_binary(OUT)
	end,
	Class = case I#ipdr_wlan.class of
		undefined ->
			<<>>;
		CLS ->
			CLS
	end,
	Cause = case I#ipdr_wlan.sessionTerminateCause of
		undefined ->
			<<>>;
		CS ->
			integer_to_binary(CS)
	end,
	ChargeQ = case I#ipdr_wlan.chargeableQuantity of
		undefined ->
			<<>>;
		CQ ->
			integer_to_binary(CQ)
	end,
	BType = case I#ipdr_wlan.bucketType of
		undefined ->
			<<>>;
		BT ->
			atom_to_binary(BT, latin1)
	end,
	BValue = case I#ipdr_wlan.bucketValue of
		undefined ->
			<<>>;
		BV ->
			integer_to_binary(BV)
	end,
	TType = case I#ipdr_wlan.tariffType of
		undefined ->
			<<>>;
		TP ->
			atom_to_binary(TP, latin1)
	end,
	Prod = case I#ipdr_wlan.product of
		undefined ->
			<<>>;
		P ->
		 list_to_binary(P)
	end,
	PType = case I#ipdr_wlan.priceType of
		undefined ->
			<<>>;
		PT ->
			atom_to_binary(PT, latin1)
	end,
	URating = case I#ipdr_wlan.usageRating of
		undefined ->
			<<>>;
		UR ->
			atom_to_binary(UR, latin1)
	end,
	ChargeA = case I#ipdr_wlan.chargeAmount of
		undefined ->
			<<>>;
		CA ->
		 list_to_binary(ocs_rest:millionths_out(CA))
	end,
	IPDR = [Time, $;, Seq, $;, User, $;, $", Sess, $", $;, IP, $;, Calling, $;,
			Called, $;, NasIP, $;, NasID, $;, Class, $;, Cause, $;, Duration, $;,
			Input, $;, Output, $;, ChargeQ, $;, BType, $;, BValue, $;, TType, $;,
			Prod, $;, PType, $;, URating, $;, ChargeA, $\r, $\n],
	case file:write(IoDevice, IPDR) of
		ok ->
			ipdr_csv(Log, IoDevice, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
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
		Log :: term(),
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
		{ok, _} ->
			ok;
		{error, {name_already_open, Log}} ->
			ok;
		{error, {size_mismatch, _CurrentSize, {LogSize, LogFiles}}} ->
			open_log2(Log, FileName, LogSize, LogFiles);
		{repaired, Log, Recovered, BadBytes} ->
			error_logger:warning_report(["Log Repaired",
					{module, ?MODULE}, {log, Log}, Recovered, BadBytes]),
			ok;
		{error, Reason} ->
			open_log3(Log, Reason)
	end.
%% @hidden
open_log2(Log, FileName, LogSize, LogFiles) ->
	case disk_log:open([{name, Log}, {file, FileName}, {type, wrap}]) of
		{ok, _} ->
			case disk_log:change_size(Log, {LogSize, LogFiles}) of
				ok ->
					ok;
				{error, Reason} ->
					open_log3(Log, Reason)
			end;
		{repaired, Log, Recovered, BadBytes} ->
			error_logger:warning_report(["Log Repaired",
					{module, ?MODULE}, {log, Log}, Recovered, BadBytes]),
			case disk_log:change_size(Log, {LogSize, LogFiles}) of
				ok ->
					ok;
				{error, Reason} ->
					open_log3(Log, Reason)
			end;
		{error, Reason} ->
			open_log3(Log, Reason)
	end.
%% @hidden
open_log3(Log, Reason) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc,
			{module, ?MODULE}, {log, Log}, {error, Reason}]),
	{error, Reason}.

-spec write_log(Log, Event) -> Result
	when
		Log :: atom(),
		Event :: list(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc write event into given log file
write_log(Log, Event) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LogEvent = list_to_tuple([TS, N | Event]),
	write_log(Log, LogEvent, disk_log:log(Log, LogEvent)).
%% @hidden
write_log(ocs_auth, LogEvent, Result) ->
	ok = ocs_event_log:notify(ocs_auth, LogEvent),
	Result;
write_log(ocs_acct, LogEvent, Result) ->
	ok = ocs_event:notify(log_acct, LogEvent, usage),
	ok = ocs_event_log:notify(ocs_acct, LogEvent),
	Result;
write_log(ocs_abmf, _LogEvent, Result) ->
	Result.

-spec close_log(Log) -> Result
	when
		Log :: atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc close log files
close_log(Log) ->
	close_log1(Log,  disk_log:sync(Log)).
%% @hidden
close_log1(Log, ok) ->
	close_log2(Log, disk_log:close(Log));
close_log1(Log, {error, Reason}) ->
	close_log2(Log, {error, Reason}).
%% @hidden
close_log2(Log, ok) ->
	ok;
close_log2(Log, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, Log}, {error, Reason}]),
	{error, Reason}.

-spec query_log(Continuation, Start, End, Log, MFA) -> Result
	when
		Continuation :: start | disk_log:continuation(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
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
%% @doc Filter events by `Start' and `End'.
query_log(Continuation, {{_, _, _}, {_, _, _}} = Start, End, Log, MFA) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	query_log(Continuation, Seconds * 1000, End, Log, MFA);
query_log(Continuation, Start, {{_, _, _}, {_, _, _}} = End, Log, MFA) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	query_log(Continuation, Start, Seconds * 1000 + 999, Log, MFA);
query_log(start, Start, End, Log, MFA) when is_integer(Start), is_integer(End) ->
	case btree_search(Log, Start) of
		{error, Reason} ->
			{error, Reason};
		Continuation ->
			query_log1(Start, End, MFA, disk_log:chunk(Log, Continuation), [])
	end;
query_log(Continuation, Start, End, Log, MFA) when is_integer(Start), is_integer(End) ->
	query_log1(Start, End, MFA, disk_log:chunk(Log, Continuation), []).
%% @hidden
query_log1(_Start, _End, {M, F, A}, eof, Acc) ->
	apply(M, F, [{eof, lists:reverse(Acc)} | A]);
query_log1(_Start, _End, _MFA, {error, Reason}, _Acc) ->
	{error, Reason};
query_log1(Start, End, MFA, {Cont, Chunk, 0}, Acc) ->
	query_log1(Start, End, MFA, {Cont, Chunk}, Acc);
query_log1(_Start, End, {M, F, A}, {_, [Event | _]}, Acc) when element(1, Event) > End ->
	apply(M, F, [{eof, lists:reverse(Acc)} | A]);
query_log1(Start, End, MFA, {Cont, [Event | T]}, Acc)
		when element(1, Event) >= Start, element(1, Event) =< End ->
	query_log1(Start, End, MFA, {Cont, T}, [Event | Acc]);
query_log1(Start, End, MFA, {Cont, [_ | T]}, Acc) ->
	query_log1(Start, End, MFA, {Cont, T}, Acc);
query_log1(_Start, _End, {M, F, A}, {Cont, []}, Acc) ->
	apply(M, F, [{Cont, lists:reverse(Acc)} | A]).

-spec acct_query(Continuation, Protocol, Types, Matches) -> Result
	when
		Continuation :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [acct_event()],
		Protocol :: protocol() | [protocol()] | '_',
		Types :: [Type] | '_',
		Type :: start | interim | stop | event | on | off,
		Matches :: [Match] | '_',
		Match :: RadiusMatch | DiameterMatchSpec | NrfMatchSpec | RatedMatchSpec,
		RadiusMatch :: {Attribute, AttributeMatch},
		Attribute :: byte(),
		AttributeMatch :: {exact, term()} | {notexact, term()}
				| {lt, term()} | {lte, term()}
				| {gt, term()} | {gte, term()}
				| {regex, term()} | {like, [term()]} | {notlike, [term()]}
				| {in, [term()]} | {notin, [term()]} | {contains, [term()]}
				| {notcontain, [term()]} | {containsall, [term()]},
		DiameterMatchSpec :: {DiameterMatchHead, MatchConditions},
		DiameterMatchHead :: #'3gpp_ro_CCR'{} | #'3gpp_ro_CCA'{}
				| #'3gpp_ro_RAR'{} | #'3gpp_ro_RAA'{}
				| #'3gpp_gx_CCR'{} | #'3gpp_gx_CCA'{}
				| #'3gpp_gx_RAR'{} | #'3gpp_gx_RAA'{},
		NrfMatchSpec :: {NrfMatchHead, MatchConditions},
		NrfMatchHead :: map(),
		RatedMatchSpec :: {RatedMatchHead, MatchConditions},
		RatedMatchHead :: #rated{},
		MatchConditions :: [tuple()],
		Result :: {Continuation2, Events}.
%% @doc Continue query of accounting log events.
%% @private
acct_query({Cont, Events} = _Continuation, Protocol, Types, Matches)
		when (is_tuple(Cont) or (Cont == eof)) ->
	{Cont, acct_query1(Events,  Protocol, Types, Matches, [])}.
%% @hidden
acct_query1(Events, Protocol, '_', Matches, _Acc) ->
	acct_query2(Events, Protocol, Matches, []);
acct_query1([H | T], Protocol, Types, Matches, Acc) ->
	case lists:member(element(6, H), Types) of
		true ->
			acct_query1(T, Protocol, Types, Matches, [H | Acc]);
		false ->
			acct_query1(T, Protocol, Types, Matches, Acc)
	end;
acct_query1([], Protocol, _Types,  Matches, Acc) ->
	acct_query2(lists:reverse(Acc), Protocol, Matches, []).
%% @hidden
acct_query2(Events, '_', Matches, _Acc) ->
	acct_query3(Events, Matches);
acct_query2([H | T], Protocol, Matches, Acc)
		when element(3, H) == Protocol ->
	acct_query2(T, Protocol, Matches, [H |Acc]);
acct_query2([H | T], [Protocol | _] = Protocols, Matches, Acc)
		when element(3, H) == Protocol ->
	acct_query2(T, Protocols, Matches, [H |Acc]);
acct_query2([H | T], [_, Protocol | _] = Protocols, Matches, Acc)
		when element(3, H) == Protocol ->
	acct_query2(T, Protocols, Matches, [H |Acc]);
acct_query2([H | T], [_, _, Protocol | _] = Protocols, Matches, Acc)
		when element(3, H) == Protocol ->
	acct_query2(T, Protocols, Matches, [H |Acc]);
acct_query2([_ | T], Protocol, Matches, Acc) ->
	acct_query2(T, Protocol, Matches, Acc);
acct_query2([], _Protocol, Matches, Acc) ->
	acct_query3(lists:reverse(Acc), Matches).
%% @hidden
acct_query3(Events, Matches) when is_list(Matches) ->
	Fradius = fun({Attribute, _Match}) when is_integer(Attribute) ->
				true;
			(_) ->
				false
	end,
	Fdiameter = fun({#'3gpp_ro_CCR'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			({#'3gpp_ro_RAR'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			({#'3gpp_gx_CCR'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			({#'3gpp_gx_RAR'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			(_) ->
				false
	end,
	Fnrf = fun({MatchHead, MatchConds}) when is_map(MatchHead) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			(_) ->
				false
	end,
	RadiusMatchSpec = lists:filtermap(Fradius, Matches),
	DiameterMatchSpec = lists:filtermap(Fdiameter, Matches),
	NrfMatchSpec = lists:filtermap(Fnrf, Matches),
	acct_query4(Events, Matches, RadiusMatchSpec,
			DiameterMatchSpec, NrfMatchSpec, []);
acct_query3(Events, '_') ->
	Events.
%% @hidden
acct_query4([H | T] = _Events, Matches,
		RadiusMatchSpec, DiameterMatchSpec, NrfMatchSpec, Acc)
		when element(3, H) == radius, length(RadiusMatchSpec) > 0 ->
	case acct_query5(element(7, H), RadiusMatchSpec) of
		true ->
			acct_query4(T, Matches, RadiusMatchSpec,
					DiameterMatchSpec, NrfMatchSpec, [H | Acc]);
		false ->
			acct_query4(T, Matches, RadiusMatchSpec,
					DiameterMatchSpec, NrfMatchSpec, Acc)
	end;
acct_query4([H | T] = _Events, Matches,
		RadiusMatchSpec, DiameterMatchSpec, NrfMatchSpec, Acc)
		when element(3, H) == diameter, length(DiameterMatchSpec) > 0 ->
	case erlang:match_spec_test(element(7, H), DiameterMatchSpec, table) of
		{ok, Event, [], []} when is_tuple(Event) ->
			acct_query4(T,  Matches,RadiusMatchSpec,
					DiameterMatchSpec, NrfMatchSpec, [H | Acc]);
		{ok, false , [], []}->
			acct_query4(T,  Matches,RadiusMatchSpec,
					DiameterMatchSpec, NrfMatchSpec, Acc);
		{error, Reason} ->
			{error, Reason}
	end;
acct_query4([H | T] = _Events, Matches,
		RadiusMatchSpec, DiameterMatchSpec, NrfMatchSpec, Acc)
		when element(3, H) == nrf, length(NrfMatchSpec) > 0 ->
	case erlang:match_spec_test(element(7, H), NrfMatchSpec, table) of
		{ok, #{}, [], []} ->
			acct_query4(T, Matches, RadiusMatchSpec,
					DiameterMatchSpec, NrfMatchSpec, [H | Acc]);
		{ok, false , [], []}->
			acct_query4(T, Matches, RadiusMatchSpec,
					DiameterMatchSpec, NrfMatchSpec, Acc);
		{error, Reason} ->
			{error, Reason}
	end;
acct_query4([H | T], Matches, RadiusMatchSpec,
		DiameterMatchSpec, NrfMatchSpec, Acc) ->
	acct_query4(T, Matches, RadiusMatchSpec,
			DiameterMatchSpec, NrfMatchSpec, [H | Acc]);
acct_query4([], Matches, _, _, _, Acc) ->
	acct_query6(lists:reverse(Acc), Matches).
%% @hidden
acct_query5(Attributes, [{Attribute, {exact, Match}} | T]) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Match1} when
				(Match1 == Match) or (Match == '_') ->
			acct_query5(Attributes, T);
		_ ->
			false
	end;
acct_query5(Attributes, [{Attribute, {like, [H | T1]}} | T2])
		when is_list(H) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		{Attribute, Value} ->
			case lists:prefix(H, Value) of
				true ->
					acct_query5(Attributes, [{Attribute, {like, T1}} | T2]);
				false ->
					false
			end;
		_ ->
			false
	end;
acct_query5(Attributes, [{_, {like, []}} | T]) ->
	acct_query5(Attributes, T);
acct_query5(Attributes, [_ | T]) ->
	acct_query5(Attributes, T);
acct_query5(_Attributes, []) ->
	true.
%% @hidden
acct_query6(Events, Matches) when is_list(Matches) ->
	Fdiameter = fun({#'3gpp_ro_CCA'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			({#'3gpp_ro_RAA'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			({#'3gpp_gx_CCA'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			({#'3gpp_gx_RAA'{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			(_) ->
				false
	end,
	DiameterMatchSpec = lists:filtermap(Fdiameter, Matches),
	acct_query7(Events, Matches, DiameterMatchSpec, []).
%% @hidden
acct_query7([H | T] = _Events, Matches, [] = DiameterMatchSpec, Acc)
		when element(3, H) == diameter, length(DiameterMatchSpec) > 0 ->
	case erlang:match_spec_test(element(8, H), DiameterMatchSpec, table) of
		{ok, Event, [], []} when is_tuple(Event) ->
			acct_query7(T, Matches, DiameterMatchSpec, [H | Acc]);
		{ok, false , [], []}->
			acct_query7(T, Matches, DiameterMatchSpec, Acc);
		{error, Reason} ->
			{error, Reason}
	end;
acct_query7([H | T], Matches, DiameterMatchSpec, Acc) ->
	acct_query7(T, Matches, DiameterMatchSpec, [H | Acc]);
acct_query7([], Matches, _, Acc) ->
	acct_query8(lists:reverse(Acc), Matches).
%% @hidden
acct_query8(Events, Matches) ->
	Frated = fun({#rated{} = MatchHead, MatchConds}) ->
				{true, {MatchHead, MatchConds, ['$_']}};
			(_) ->
				false
	end,
	RatedMatchSpec = lists:filtermap(Frated, Matches),
	acct_query9(Events, Matches, RatedMatchSpec, []).
%% @hidden
acct_query9([H | T] = _Events, Matches, RatedMatchSpec, Acc)
		when length(RatedMatchSpec) > 0 ->
	F = fun(#rated{} = Rated) ->
				case erlang:match_spec_test(Rated, RatedMatchSpec, table) of
					{ok, #rated{}, [], []} ->
						true;
					{ok, false , [], []}->
						false;
					{error, _Reason} ->
						false
				end
	end,
	case lists:any(F, element(9, H)) of
		true ->
			acct_query9(T, Matches, RatedMatchSpec, [H | Acc]);
		false ->
			acct_query9(T, Matches, RatedMatchSpec, Acc)
	end;
acct_query9([H | T], Matches, RatedMatchSpec, Acc) ->
	acct_query9(T, Matches, RatedMatchSpec, [H | Acc]);
acct_query9([], _, _, Acc) ->
	lists:reverse(Acc).

-spec ipdr_query(Continuation, MatchSpec) -> Result
	when
		Continuation :: {Continuation2, Events},
		MatchSpec :: [tuple()] | '_',
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [#ipdr_wlan{}].
%% @doc Continue query of IPDR log events.
%% @private
ipdr_query({Cont, Events}, AttrsMatch)
		when (is_tuple(Cont) or (Cont == eof)) ->
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
		Protocol :: radius | diameter | '_',
		Types :: [Type] | '_',
		Type :: atom(),
		ReqAttrsMatch :: [tuple()] | '_',
		RespAttrsMatch :: [tuple()] | '_',
		Result :: {Continuation2, Events},
		Continuation2 :: eof | disk_log:continuation(),

		Events :: [acct_event()].
%% @doc Continue query of authentication log events.
%% @private
auth_query({Cont, Events}, Protocol, Types, ReqAttrsMatch, RespAttrsMatch)
		when (is_tuple(Cont) or (Cont == eof)) ->
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
	Events;
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
		Continuation2 :: eof | disk_log:continuation(),
		Events :: [abmf_event()],
		Type :: [{type, {MatchType, TypeValue}}] | '_',
		TypeValue :: deduct | reserve | unreserve | transfer | topup | adjustment | '_',
		Subscriber :: [{subscriber, {MatchType, SubscriberValue}}] | '_',
		SubscriberValue :: binary() | '_',
		Bucket :: [{bucket, {MatchType, BucketValue}}] | '_',
		BucketValue :: string() | '_',
		Units :: [{units, {MatchType, UnitsValue}}] | '_',
		UnitsValue :: cents | seconds | octets | messages | '_',
		Product :: [{product, {MatchType, ProductValue}}] | '_',
		ProductValue :: string() | '_',
		MatchType :: exact | like,
		Result :: {Continuation2, Events}.
%% @doc Continue query of balance activity log events.
%% @private
abmf_query({Cont, Events}, Type, Subscriber, Bucket, Units, Product)
		when (is_tuple(Cont) or (Cont == eof)) ->
	{Cont, abmf_query1(Events, Type, Subscriber, Bucket, Units, Product, [])}.
%% @hidden
abmf_query1(Events, '_', Subscriber, Bucket, Units, Product, []) ->
	abmf_query2(Events, Subscriber, Bucket, Units, Product, []);
abmf_query1([H | T] = _Events, Type, Subscriber, Bucket, Units, Product, Acc) ->
	case abmf_query6(H, Type) of
		true ->
			abmf_query1(T, Type, Subscriber, Bucket, Units, Product, [H | Acc]);
		false ->
			abmf_query1(T, Type, Subscriber, Bucket, Units, Product, Acc)
	end;
abmf_query1([], _Type, Subscriber, Bucket, Units, Product, Acc) ->
	abmf_query2(lists:reverse(Acc), Subscriber, Bucket, Units, Product, []).
%% @hidden
abmf_query2(Events, '_', Bucket, Units, Product, []) ->
	abmf_query3(Events, Bucket, Units, Product, []);
abmf_query2([H | T], Subscriber, Bucket, Units, Product, Acc) ->
	case abmf_query6(H, Subscriber) of
		true ->
			abmf_query2(T, Subscriber, Bucket, Units, Product, [H | Acc]);
		false ->
			abmf_query2(T, Subscriber, Bucket, Units, Product, Acc)
	end;
abmf_query2([], _Subscriber, Bucket, Units, Product, Acc) ->
	abmf_query3(lists:reverse(Acc), Bucket, Units, Product, []).
%% @hidden
abmf_query3(Events, '_', Units, Product, []) ->
	abmf_query4(Events, Units, Product, []);
abmf_query3([H | T], Bucket, Units, Product, Acc) ->
	case abmf_query6(H, Bucket) of
		true ->
			abmf_query3(T, Bucket, Units, Product, [H | Acc]);
		false ->
			abmf_query3(T, Bucket, Units, Product, Acc)
	end;
abmf_query3([], _Bucket, Units, Product, Acc) ->
	abmf_query4(lists:reverse(Acc), Units, Product, []).
%% @hidden
abmf_query4(Events, '_', Product, []) ->
	abmf_query5(Events, Product, []);
abmf_query4([H | T], Units, Product, Acc) ->
	case abmf_query6(H, Units) of
		true ->
			abmf_query4(T, Units, Product, [H | Acc]);
		false ->
			abmf_query4(T, Units, Product, Acc)
	end;
abmf_query4([], _Units, Product, Acc) ->
	abmf_query5(lists:reverse(Acc), Product, []).
%% @hidden
abmf_query5(Events, '_', []) ->
	Events;
abmf_query5([H | T], Product, Acc) ->
	case abmf_query6(H, Product) of
		true ->
			abmf_query5(T, Product, [H | Acc]);
		false ->
			abmf_query5(T, Product, Acc)
	end;
abmf_query5([], _Product, Acc) ->
	lists:reverse(Acc).
%% @hidden
abmf_query6(Attributes, [{_Attribute, {exact, Match}} | _])
		when Match == element(4, Attributes) ->
	true;
abmf_query6(Attributes, [{_Attribute, {exact, Match}} | _])
		when Match == element(5, Attributes) ->
	true;
abmf_query6(Attributes, [{_Attribute, {exact, Match}} | _])
		when Match == element(6, Attributes) ->
	true;
abmf_query6(Attributes, [{_Attribute, {exact, Match}} | _])
		when Match == element(7, Attributes) ->
	true;
abmf_query6(Attributes, [{_Attribute, {exact, Match}} | _])
		when Match == element(8, Attributes) ->
	true;
abmf_query6(Attributes, [{type, {like, [H | _T1]}} | _])
		when is_atom(element(4, Attributes)) ->
		case lists:prefix(H, atom_to_list(element(4, Attributes))) of
			true ->
				true;
			false ->
				false
		end;
abmf_query6(Attributes, [{subscriber, {like, [H | _T1]}} | _])
		when is_list(element(5, Attributes))->
		case lists:prefix(H, element(5, Attributes)) of
			true ->
				true;
			false ->
				false
		end;
abmf_query6(Attributes, [{bucket, {like, [H | _T1]}} | _])
		when is_list(element(6, Attributes))->
		case lists:prefix(H, element(6, Attributes)) of
			true ->
				true;
			false ->
				false
		end;
abmf_query6(Attributes, [{units, {like, [H | _T1]}} | _])
		when is_atom(element(7, Attributes))->
		case lists:prefix(H, atom_to_list(element(7, Attributes))) of
			true ->
				true;
			false ->
				false
		end;
abmf_query6(Attributes, [{product, {like, [H | _T1]}} | _])
		when is_list(element(8, Attributes))->
		case lists:prefix(H, element(8, Attributes)) of
			true ->
				true;
			false ->
				false
		end;
abmf_query6(Attributes, [_H | T]) ->
	false;
abmf_query6(_Attributes, []) ->
	false.

%% @hidden
idpr_convert({ipdr_wlan, IpdrCreationTime, SeqNum,
		Username, ScIdType, ScId, HomeServiceProviderType,
		HomeServiceProvider, AcctSessionId, UserIpAddress,
		CallingStationId, CalledStationId, NasIpAddress, NasId, AccessProviderType,
		AccessServiceProvider, LocationName, LocationId, LocationType,
		LocationCountryCode, LocationStateProvince, LocationCity,
		LocationGeocode, LocationGeocodeType, NasPortType, PaymentType,
		NetworkConnectionType, SessionDuration, InputOctets,
		OutputOctets, Class, GmtSessionStartDateTime, GmtSessionEndDateTime, SessionTerminateCause,
		BillingClassOfService, UnitOfMeasure, ChargeableUnit, ChargeableQuantity,
		ChargeAmount, ChargeCurrencyType, OtherParty,
		TaxPercentage, TaxAmount, TaxType, IntermediaryName,
		ServiceName, RelatedIpdrIdList, TempUserId}) ->
	#ipdr_wlan{ipdrCreationTime = IpdrCreationTime, seqNum = SeqNum,
	username = Username, scIdType = ScIdType, scId = ScId,
	homeServiceProviderType = HomeServiceProviderType,
	homeServiceProvider = HomeServiceProvider, acctSessionId = AcctSessionId,
	userIpAddress = UserIpAddress, callingStationId = CallingStationId,
	calledStationId = CalledStationId, nasIpAddress = NasIpAddress,
	nasId = NasId, accessProviderType = AccessProviderType,
	accessServiceProvider = AccessServiceProvider, locationName = LocationName,
	locationId = LocationId, locationType = LocationType,
	locationCountryCode = LocationCountryCode,
	locationStateProvince = LocationStateProvince, locationCity = LocationCity,
	locationGeocode = LocationGeocode, locationGeocodeType = LocationGeocodeType,
	nasPortType = NasPortType, paymentType = PaymentType,
	networkConnectionType = NetworkConnectionType,
	sessionDuration = SessionDuration, inputOctets = InputOctets,
	outputOctets = OutputOctets, class = Class,
	gmtSessionStartDateTime = GmtSessionStartDateTime,
	gmtSessionEndDateTime = GmtSessionEndDateTime,
	sessionTerminateCause = SessionTerminateCause,
	billingClassOfService = BillingClassOfService, unitOfMeasure = UnitOfMeasure,
	chargeableUnit = ChargeableUnit, chargeableQuantity = ChargeableQuantity,
	chargeAmount = ChargeAmount, chargeCurrencyType = ChargeCurrencyType,
	bucketType = undefined, bucketValue = undefined,
	tariffType = undefined, product = undefined,
	priceType = undefined, usageRating = undefined,
	otherParty = OtherParty, taxPercentage = TaxPercentage,
	taxAmount = TaxAmount, taxType = TaxType,
	intermediaryName = IntermediaryName, serviceName = ServiceName,
	relatedIpdrIdList = RelatedIpdrIdList, tempUserId = TempUserId}.

