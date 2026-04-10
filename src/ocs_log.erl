%%% ocs_log.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2026 SigScale Global Inc.
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
%%% @doc This library module implements functions used in handling of
%%% 	logging in the {@link //ocs. ocs} application.
%%%
%%% 	Event logging in {@link //ocs. ocs} uses
%%% 	{@link //kernel/disk_log. disk_log} wrap logs configured for file
%%% 	size, and number of files, with application environment variables
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
-module(ocs_log).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

%% export the ocs_log public API
-export([acct_open/0, acct_log/6, acct_close/0,
		acct_query/5, acct_query/6]).
-export([auth_open/0, auth_log/5, auth_log/6, auth_close/0,
			auth_query/6, auth_query/7]).
-export([cdr_log/4, cdr_file/3]).
-export([abmf_open/0, abmf_log/15,
			abmf_query/8]).
-export([get_range/3, last/2, dump_file/2, httpd_logname/1,
			http_file/2, date/1, iso8601/0, iso8601/1]).
-export([http_query/8]).
-export([log_name/1]).
-export([auth_to_ecs/1, acct_to_ecs/1]).
%% export the deprecated public API
-export([ipdr_log/4, ipdr_file/3]).

%% export the private API
-export([acct_query/4, auth_query/5, abmf_query/6]).
-export([btree_search/2]).

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

-type timestamp() :: pos_integer().
-type unique() :: pos_integer().
-type protocol() :: radius | diameter | nrf.
-type server() :: {Address :: inet:ip_address(),
		Port :: non_neg_integer()} | undefined.
-export_type([timestamp/0, unique/0, protocol/0, server/0]).

-type diameter_auth_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Client :: ocs_log:server(),
		RequestAttributes :: ocs_log:auth_request(),
		ResponseAttributes :: ocs_log:auth_response()}.
%% DIAMETER event in the `auth' log.

-type radius_auth_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Client :: ocs_log:server(),
		Type :: ocs_log:auth_type(),
		RequestAttributes :: ocs_log:auth_request(),
		ResponseAttributes :: ocs_log:auth_response()}.
%% RADIUS event in the `auth' log.

-type auth_event() :: diameter_auth_event() | radius_auth_event().
%% Event in the `auth' log.

-type acct_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Type :: ocs_log:acct_type(),
		RequestAttributes :: ocs_log:acct_request(),
		ResponseAttributes :: ocs_log:acct_response(),
		Rated :: [#rated{}] | undefined}.
%% Event in the `acct' log.

-type http_event() :: {
		Host :: string(),
		User :: string(),
		DateTime :: string(),
		Method :: string(),
		URI :: string(),
		HttpStatus :: string()}.
%% Event in the `http' log.

-type abmf_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Node :: atom(),
		Type :: topup | adjustment | delete | deduct | reserve | unreserve | transfer,
		Subscriber :: binary(),
		Bucket :: undefined | string(),
		Units :: cents | seconds | octets | messages,
		Product :: string(),
		Amount :: integer(),
		AmountBefore :: integer() | undefined,
		AmountAfter :: integer() | undefined,
		Validity :: undefined | pos_integer(),
		Channel :: undefined | string(),
		Requestor :: undefined | [{Id :: string(),
				Role :: string(), Name :: string()}],
		RelatedParty :: undefined | [{Id :: string(),
				Role :: string(), Name :: string()}],
		PaymentMeans :: undefined | string(),
		Action :: undefined | string(),
		Status :: undefined | term()}.
%% Event in the `abmf' log.

-export_type([auth_event/0, acct_event/0, abmf_event/0,
		http_event/0]).

-export_type([acct_type/0, acct_request/0, acct_response/0, acct_rated/0]).

-export_type([auth_type/0, auth_request/0, auth_response/0]).

-export_type([cdr/0]).

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
%% A request on the Nrf interface.

-type nrf_response() :: map().
%% A response on the Nrf interface.

-type acct_type() :: on | off | start | stop | update | interim | final | event.
%% Type of an event in the `acct' log.

-type acct_request() :: #'3gpp_ro_CCR'{} | #'3gpp_ro_RAR'{}
		| #'3gpp_gx_CCR'{} | #'3gpp_gx_RAR'{}
		| radius_attributes:attributes() | nrf_request().
%% A request in an event of the `acct' log.

-type acct_response() :: #'3gpp_ro_CCA'{} | #'3gpp_ro_RAA'{}
		| #'3gpp_gx_CCA'{} | #'3gpp_gx_RAA'{}
		| radius_attributes:attributes() | nrf_response().
%% A response in an event of the `acct' log.

-type acct_rated() :: [#rated{}].
%% Rated records in an event of the `acct' log.

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
%% Type of an event in the `auth' log.

-type auth_request_rad() :: radius_attributes:attributes().
%% A request on a RADIUS interface.

-type auth_request_dia() :: #diameter_nas_app_AAR{}
		| #diameter_eap_app_DER{} | #'3gpp_sta_DER'{} | #'3gpp_swm_DER'{}
		| #'3gpp_sta_STR'{} | #'3gpp_swm_STR'{} | #'3gpp_s6b_STR'{}
		| #'3gpp_swx_RTR'{} | #'3gpp_s6b_AAR'{} | #'3gpp_s6a_AIR'{}
		| #'3gpp_s6a_ULR'{} | #'3gpp_s6a_PUR'{}.
%% A request on a DIAMETER interface.

-type auth_request() :: auth_request_rad() | auth_request_dia().
%% A request in an event of the `auth' log.

-type auth_response_rad() :: radius_attributes:attributes().
%% A response on a RADIUS interface.

-type auth_response_dia() :: #diameter_nas_app_AAA{}
		| #diameter_eap_app_DEA{} | #'3gpp_sta_DEA'{} | #'3gpp_swm_DEA'{}
		| #'3gpp_sta_STA'{} | #'3gpp_swm_STA'{} | #'3gpp_s6b_STA'{}
		| #'3gpp_swx_RTA'{} | #'3gpp_s6b_AAA'{} | #'3gpp_s6a_AIA'{}
		| #'3gpp_s6a_ULA'{} | #'3gpp_s6a_PUA'{}.
%% A response on a DIAMETER interface.

-type auth_response() :: auth_response_rad() | auth_response_dia().
%% A response in an event of the `auth' log.

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

-spec cdr_log(Type, File, Start, End) -> Result
	when
		Type :: chf,
		File :: file:filename(),
		Start :: calendar:datetime() | timestamp(),
		End :: calendar:datetime() | timestamp(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Log accounting records within range to new CDR disk log.
%%
%% 	Creates a new {@link //kernel/disk_log:log(). disk_log:log()},
%% 	or overwrites an existing, with filename `File'.
%%
%% 	The `ocs_acct' log is searched for events created between
%% 	`Start' and `End'.
%%
cdr_log(Type, File, {{_, _, _}, {_, _, _}} = Start, End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	cdr_log(Type, File, Seconds * 1000, End);
cdr_log(Type, File, Start, {{_, _, _}, {_, _, _}} = End) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	cdr_log(Type, File, Start, Seconds * 1000 + 999);
cdr_log(chf = Type, File, Start, End) when is_list(File),
		is_integer(Start), is_integer(End) ->
		{ok, Directory} = application:get_env(ocs, cdr_log_dir),
		FileName = filename:join([Directory, Type, File]),
	case disk_log:open([{name, File},
			{file, FileName}, {repair, truncate}]) of
		{ok, CdrLog} ->
			{ok, AcctLog} = application:get_env(ocs, acct_log_name),
			cdr_log1(CdrLog, Start, End,
					AcctLog, btree_search(AcctLog, Start));
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc,
					{module, ?MODULE}, {function, ?FUNCTION_NAME},
					{file, File}, {error, Reason}]),
			{error, Reason}
	end.
%% @hidden
cdr_log1(CdrLog, _Start, _End, AcctLog, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc,
			{module, ?MODULE}, {function, ?FUNCTION_NAME},
			{log, AcctLog}, {error, Reason}]),
	cdr_log4(CdrLog);
cdr_log1(CdrLog, Start, End, AcctLog, Cont) ->
	cdr_log2(CdrLog, Start, End, AcctLog, [],
			disk_log:chunk(AcctLog, Cont)).
%% @hidden
cdr_log2(CdrLog, _Start, _End, AcctLog,
		_PrevChunk, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc,
			{module, ?MODULE}, {function, ?FUNCTION_NAME},
			{log, AcctLog}, {error, Reason}]),
	cdr_log4(CdrLog);
cdr_log2(CdrLog, _Start, _End, _AcctLog, [], eof) ->
	cdr_log4(CdrLog);
cdr_log2(CdrLog, Start, End, AcctLog, PrevChunk, eof) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	cdr_log3(CdrLog, Start, End, AcctLog,
			{eof, lists:dropwhile(Fstart, PrevChunk)});
cdr_log2(CdrLog, Start, End, AcctLog,
		_PrevChunk, {Cont, [H | T]})
		when element(1, H) < Start ->
	cdr_log2(CdrLog, Start, End,
			AcctLog, T, disk_log:chunk(AcctLog, Cont));
cdr_log2(CdrLog, Start, End, AcctLog, PrevChunk, {Cont, Chunk}) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	cdr_log3(CdrLog, Start, End, AcctLog,
			{Cont, lists:dropwhile(Fstart, PrevChunk ++ Chunk)}).
%% @hidden
cdr_log3(CdrLog, _Start, _End, _AcctLog, eof) ->
	cdr_log4(CdrLog);
cdr_log3(CdrLog, _Start, _End, _AcctLog, {error, _Reason}) ->
	cdr_log4(CdrLog);
cdr_log3(CdrLog, _Start, _End, _AcctLog, {eof, []}) ->
	cdr_log4(CdrLog);
cdr_log3(CdrLog, _Start, End, _AcctLog, {_Cont, [H | _]})
		when element(1, H) > End ->
	cdr_log4(CdrLog);
cdr_log3(CdrLog, Start, End, AcctLog, {Cont, [H | T]}) ->
	case disk_log:log(CdrLog, cdr_chf_codec(H)) of
		ok ->
			cdr_log3(CdrLog, Start, End, AcctLog, {Cont, T});
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc,
					{module, ?MODULE}, {function, ?FUNCTION_NAME},
					{log, CdrLog}, {error, Reason}]),
			disk_log:close(CdrLog),
			{error, Reason}
	end;
cdr_log3(CdrLog, Start, End, AcctLog, {Cont, []}) ->
	cdr_log3(CdrLog, Start, End,
			AcctLog, disk_log:chunk(AcctLog, Cont)).
%% @hidden
cdr_log4(CdrLog) ->
	case disk_log:close(CdrLog) of
		ok ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc,
					{module, ?MODULE}, {function, ?FUNCTION_NAME},
					{log, CdrLog}, {error, Reason}]),
			{error, Reason}
	end.

-spec cdr_file(Type, LogFile, Format) -> Result
	when
		Type :: chf,
		LogFile :: file:filename(),
		Format :: xml | json | csv,
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Export internal CDR disk log.
%%
%% 	Creates a file named `LogFile.Format' with the details from the
%% 	{@link //kernel/disk_log:log(). disk_log:log()} file `LogFile'
%% 	created previously with {@link cdr_log/4}.
%%
cdr_file(Type, LogFile, Format) when is_list(LogFile),
		((Format == xml) or (Format == json) or (Format == csv)) ->
	{ok, CdrLogDir} = application:get_env(ocs, cdr_log_dir),
	FileName = filename:join([CdrLogDir, Type, LogFile]),
	case disk_log:open([{name, make_ref()},
			{file, FileName}, {repair, true}]) of
		{ok, Log} ->
			cdr_file1(LogFile, Log, Format);
		{repaired, Log, Recovered, BadBytes} ->
			error_logger:warning_report(["Log Repaired",
					{module, ?MODULE}, {log, Log}, Recovered, BadBytes]),
			cdr_file1(LogFile, Log, Format);
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc,
					{module, ?MODULE}, {function, ?FUNCTION_NAME},
					{file, FileName}, {error, Reason}]),
			{error, Reason}
	end.
%% @hidden
cdr_file1(FileName, Log, Format) ->
	{ok, Directory} = application:get_env(ocs, export_dir),
	case file:make_dir(Directory) of
		ok ->
			cdr_file2(FileName, Log, Format, Directory);
		{error, eexist} ->
			cdr_file2(FileName, Log, Format, Directory);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log},
					{directory, Directory}, {error, Reason}]),
			disk_log:close(Log),
			{error, Reason}
	end.
%% @hidden
cdr_file2(FileName, Log, Format, ExportDir) ->
	ExportFile = FileName ++ "." ++ atom_to_list(Format),
	ExportPath = filename:join([ExportDir, ExportFile]),
	case file:open(ExportPath, [raw, write, delayed_write]) of
		{ok, IoDevice} when Format == csv ->
			chf_csv_header(Log, IoDevice, $,);
		{ok, IoDevice} ->
			cdr_file3(Log, IoDevice, Format, disk_log:chunk(Log, start));
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log},
					{filename, ExportFile}, {error, Reason}]),
			disk_log:close(Log),
			{error, Reason}
	end.
%% @hidden
cdr_file3(Log, IoDevice, _Format, eof) ->
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
cdr_file3(Log, IoDevice, _Format, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, Log}, {error, Reason}]),
	disk_log:close(Log),
	file:close(IoDevice),
	{error, Reason};
cdr_file3(Log, IoDevice, Format, {Cont, []}) ->
	cdr_file3(Log, IoDevice, Format, disk_log:chunk(Log, Cont));
cdr_file3(_Log, _IoDevice, xml, {_Cont, _Events}) ->
	 {error, unimplemented};
cdr_file3(_Log, _IoDevice, json, {_Cont,_Events}) ->
	 {error, unimplemented};
cdr_file3(Log, IoDevice, csv, {Cont, Events}) ->
	chf_csv(Log, IoDevice, $,, {Cont, Events}).

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
%% 	with a `#ipdrDocWLAN{}' | `#ipdrDocVoIP{}' header, is followed
%% 	by `#ipdr_wlan{}' | `#ipdr_voip{}'  records, and ends with a
%% 	`#ipdrDocEnd{}' trailer.
%%
%% 	The `ocs_acct' log is searched for events created between `Start'
%% 	and `End' which may be given as
%% 	`{{Year, Month, Day}, {Hour, Minute, Second}}' or the native
%% 	{@link //erts/erlang:system_time(). erlang:system_time(millisecond)}.
%%
%% @deprecated The IPDR format has been deprecated.
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
		FileName = filename:join([Directory, Type, File]),
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
					{ok, AcctLog} = application:get_env(ocs, acct_log_name),
					ipdr_log1(IpdrLog, Start, End,
							AcctLog, btree_search(AcctLog, Start));
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
ipdr_log1(IpdrLog, _Start, _End, AcctLog, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, AcctLog}, {error, Reason}]),
	ipdr_log5(IpdrLog, 0);
%ipdr_log1(IpdrLog, _Start, _End, _AcctLog, eof) ->
%	ipdr_log5(IpdrLog, 0);
ipdr_log1(IpdrLog, Start, End, AcctLog, Cont) ->
	ipdr_log2(IpdrLog, Start, End, AcctLog,
			[], disk_log:chunk(AcctLog, Cont)).
%% @hidden
ipdr_log2(IpdrLog, _Start, _End,
		AcctLog, _PrevChunk, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
			{log, AcctLog}, {error, Reason}]),
	ipdr_log5(IpdrLog, 0);
ipdr_log2(IpdrLog, _Start, _End, _AcctLog, [], eof) ->
	ipdr_log5(IpdrLog, 0);
ipdr_log2(IpdrLog, Start, End, AcctLog, PrevChunk, eof) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	ipdr_log3(IpdrLog, Start, End, AcctLog, 0,
			{eof, lists:dropwhile(Fstart, PrevChunk)});
ipdr_log2(IpdrLog, Start, End, AcctLog, _PrevChunk, {Cont, [H | T]})
		when element(1, H) < Start ->
	ipdr_log2(IpdrLog, Start, End,
			AcctLog, T, disk_log:chunk(log_name(acct_log_name), Cont));
ipdr_log2(IpdrLog, Start, End, AcctLog, PrevChunk, {Cont, Chunk}) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	ipdr_log3(IpdrLog, Start, End, AcctLog, 0,
			{Cont, lists:dropwhile(Fstart, PrevChunk ++ Chunk)}).
%% @hidden
ipdr_log3(IpdrLog, _Start, _End, _AcctLog, SeqNum, eof) ->
	ipdr_log5(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, _Start, _End, _AcctLog, SeqNum, {error, _Reason}) ->
	ipdr_log5(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, _Start, _End, _AcctLog, SeqNum, {eof, []}) ->
	ipdr_log5(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, Start, End, AcctLog, SeqNum, {Cont, []}) ->
	ipdr_log3(IpdrLog, Start,
			End, AcctLog, SeqNum, disk_log:chunk(AcctLog, Cont));
ipdr_log3(IpdrLog, _Start, End, _AcctLog, SeqNum, {_Cont, [H | _]})
		when element(1, H) > End ->
	ipdr_log5(IpdrLog, SeqNum);
ipdr_log3(IpdrLog, Start, End, AcctLog, SeqNum, {Cont, [H | T]})
		when element(6, H) == stop ->
	ipdr_log4(IpdrLog, Start, End,
			AcctLog, SeqNum, {Cont, T}, ipdr_codec(H));
ipdr_log3(IpdrLog, Start, End, AcctLog, SeqNum, {Cont, [_ | T]}) ->
	ipdr_log3(IpdrLog, Start, End, AcctLog, SeqNum, {Cont, T}).
%% @hidden
ipdr_log4(IpdrLog, Start, End,
		AcctLog, SeqNum, Cont, [#ipdr_wlan{} = IPDR | T]) ->
	NewSeqNum = SeqNum + 1,
	case disk_log:log(IpdrLog, IPDR#ipdr_wlan{seqNum = NewSeqNum}) of
		ok ->
			ipdr_log4(IpdrLog, Start, End, AcctLog, NewSeqNum, Cont, T);
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, IpdrLog}, {error, Reason}]),
			disk_log:close(IpdrLog),
			{error, Reason}
	end;
ipdr_log4(IpdrLog, Start, End,
		AcctLog, SeqNum, Cont, [#ipdr_voip{} = IPDR | T]) ->
	NewSeqNum = SeqNum + 1,
	case disk_log:log(IpdrLog, IPDR#ipdr_voip{seqNum = NewSeqNum}) of
		ok ->
			ipdr_log4(IpdrLog, Start, End, AcctLog, NewSeqNum, Cont, T);
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, IpdrLog}, {error, Reason}]),
			disk_log:close(IpdrLog),
			{error, Reason}
	end;
ipdr_log4(IpdrLog, Start, End, AcctLog, SeqNum, Cont, []) ->
	ipdr_log3(IpdrLog, Start, End, AcctLog, SeqNum, Cont).
%% @hidden
ipdr_log5(IpdrLog, SeqNum) ->
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
%% @deprecated The IPDR format has been deprecated.
%%
ipdr_file(Type, LogFile, Format) when is_list(LogFile),
		((Format == xml) or (Format == xdr) or (Format == csv)) ->
	{ok, Directory} = application:get_env(ocs, ipdr_log_dir),
	FileName = filename:join([Directory, Type, LogFile]),
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
	ExportFile = FileName ++ "." ++ atom_to_list(Format),
	ExportPath = filename:join([ExportDir, ExportFile]),
	case file:open(ExportPath, [raw, write, delayed_write]) of
		{ok, IoDevice} ->
			ipdr_file3(Log, IoDevice, Format, disk_log:chunk(Log, start));
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log},
					{filename, ExportFile}, {error, Reason}]),
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
	ipdr_csv(Log, IoDevice, $,, {Cont, Events}).

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

-spec date(DateTime) -> Result
	when
		DateTime :: timestamp() | calendar:datetime1970(),
		Result :: calendar:datetime() | timestamp().
%% @doc Convert between Unix epoch timestamp and OTP date and time.
date(DateTime) when is_integer(DateTime) ->
	Seconds = ?EPOCH + (DateTime div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH,
	Seconds * 1000.

-spec iso8601() -> DateTime
	when
		DateTime :: string().
%% @doc Convert system time to ISO 8601.
iso8601() ->
	iso8601(erlang:system_time(millisecond)).

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
%%  The ocs_log private API
%%----------------------------------------------------------------------

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
		when element(3, H) == radius,
		length(RadiusMatchSpec) > 0 ->
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
		when element(3, H) == radius,
		((length(DiameterMatchSpec) > 0)
				orelse (length(NrfMatchSpec) > 0)) ->
	acct_query4(T, Matches, RadiusMatchSpec,
			DiameterMatchSpec, NrfMatchSpec, Acc);
acct_query4([H | T] = _Events, Matches,
		RadiusMatchSpec, DiameterMatchSpec, NrfMatchSpec, Acc)
		when element(3, H) == diameter,
		length(DiameterMatchSpec) > 0 ->
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
		when element(3, H) == diameter,
		((length(RadiusMatchSpec) > 0)
				orelse (length(NrfMatchSpec) > 0)) ->
	acct_query4(T, Matches, RadiusMatchSpec,
			DiameterMatchSpec, NrfMatchSpec, Acc);
acct_query4([H | T] = _Events, Matches,
		RadiusMatchSpec, DiameterMatchSpec, NrfMatchSpec, Acc)
		when element(3, H) == nrf,
		length(NrfMatchSpec) > 0 ->
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
acct_query4([H | T] = _Events, Matches,
		RadiusMatchSpec, DiameterMatchSpec, NrfMatchSpec, Acc)
		when element(3, H) == nrf,
		((length(RadiusMatchSpec) > 0)
				orelse (length(DiameterMatchSpec) > 0)) ->
	acct_query4(T, Matches, RadiusMatchSpec,
			DiameterMatchSpec, NrfMatchSpec, Acc);
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
acct_query7([H | T] = _Events, Matches, DiameterMatchSpec, Acc)
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
		when is_list(element(9, H)), length(RatedMatchSpec) > 0 ->
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
acct_query9([H | T] = _Events, Matches, RatedMatchSpec, Acc)
		when length(RatedMatchSpec) > 0 ->
	acct_query9(T, Matches, RatedMatchSpec, Acc);
acct_query9([H | T], Matches, RatedMatchSpec, Acc) ->
	acct_query9(T, Matches, RatedMatchSpec, [H | Acc]);
acct_query9([], _, _, Acc) ->
	lists:reverse(Acc).

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

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
uuid() ->
	<<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
	Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
	Values = [A, B, (C bsr 4) bor 16#4000, (D bsr 2) bor 16#8000, E],
	Chars = io_lib:fwrite(Format, Values),
	lists:flatten(Chars).

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
		Start :: timestamp() | calendar:datetime1970(),
		Result :: disk_log:continuation() | {error, Reason},
		Reason :: term().
%% @doc Binary tree search of multi file wrap disk_log.
%% @private
btree_search(Log, {{_, _, _}, {_, _, _}} = Start) ->
	btree_search(Log, date(Start));
btree_search(Log, Start) when is_integer(Start) ->
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
			btree_search(Log, Start, Step, start, element(1, R),
					disk_log:chunk_step(Log, Cont, Step))
	end.
%% @hidden
btree_search(Log, Start, Step, PrevCont, PrevChunkStart, {ok, Cont}) ->
	btree_search(Log, Start, Step, PrevCont, PrevChunkStart, Cont,
			disk_log:chunk(Log, Cont, 1));
btree_search(_Log, _Start, 1, PrevCont, _PrevChunkStart, {error, end_of_log}) ->
	PrevCont;
btree_search(Log, Start, Step, PrevCont, PrevChunkStart, {error, end_of_log}) ->
	Step1 = Step div 2,
	btree_search(Log, Start, Step1, PrevCont, PrevChunkStart,
			disk_log:chunk_step(Log, PrevCont, Step1));
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

-spec cdr_chf_codec(Event) -> CDR
	when
		Event :: acct_event(),
		CDR :: cdr().
%% @doc Convert `ocs_acct' log event to rated CDR log event.
%% @private
cdr_chf_codec(Event) when size(Event) > 6,
		((element(3, Event) == radius)
		orelse (element(3, Event) == diameter)
		orelse (element(3, Event) == nrf)) ->
	TimeStamp = element(1, Event),
	Unique = element(2, Event),
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
	cdr_chf_codec1(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated).
%% @hidden
cdr_chf_codec1(TimeStamp, Unique, Protocol, ReqType, Req, Res, undefined) ->
	cdr_chf_codec1(TimeStamp, Unique, Protocol, ReqType, Req, Res, []);
cdr_chf_codec1(TimeStamp, Unique, diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Id} = Req, Res, Rated) ->
	case binary:part(Id, size(Id), -8) of
		<<"3gpp.org">> ->
			ServiceType = binary:part(Id, byte_size(Id) - 14, 5),
			cdr_chf_codec2(TimeStamp, Unique, Protocol,
					ReqType, Req, Res, Rated, ServiceType);
		_ ->
			exit(missing_service_context_id)
	end;
cdr_chf_codec1(TimeStamp, Unique, nrf = Protocol, ReqType,
		#{"serviceContextId" := Id} = Req, Res, Rated) ->
	case lists:sublist(Id, length(Id) - 7, 8) of
		"3gpp.org" ->
			ServiceType = lists:sublist(Id, length(Id) - 13, 5),
			cdr_chf_codec2(TimeStamp, Unique, Protocol,
					ReqType, Req, Res, Rated, ServiceType);
		_ ->
			exit(missing_service_context_id)
	end;
cdr_chf_codec1(TimeStamp, Unique, nrf = Protocol, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Id} | _]} = Req,
		Res, Rated) ->
	% deprecated in Nrf_Rating v1.2.0
	case lists:sublist(Id, length(Id) - 7, 8) of
		"3gpp.org" ->
			ServiceType = lists:sublist(Id, length(Id) - 13, 5),
			cdr_chf_codec2(TimeStamp, Unique, Protocol,
					ReqType, Req, Res, Rated, ServiceType);
		_ ->
			exit(missing_service_context_id)
	end;
cdr_chf_codec1(TimeStamp, Unique, radius, ReqType, Req, Res, Rated)
		when is_list(Req) ->
	chf_ps(TimeStamp, Unique, radius, ReqType, Req, Res, Rated).
%% @hidden
cdr_chf_codec2(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated, ServiceType)
		when ServiceType == <<"32251">>; ServiceType == "32251";
		ServiceType == <<"32255">>; ServiceType == "32255" ->
	chf_ps(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated);
cdr_chf_codec2(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated,  ServiceType)
		when ServiceType == <<"32260">>; ServiceType == "32260" ->
	chf_ims(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated);
cdr_chf_codec2(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated, ServiceType)
		when ServiceType == <<"32276">>; ServiceType == "32276" ->
	chf_vcs(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated);
cdr_chf_codec2(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated, ServiceType)
		when ServiceType == <<"32274">>; ServiceType == "32274" ->
	chf_sms(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated).

-spec ipdr_codec(Event) -> IPDRs
	when
		Event :: tuple(),
		IPDRs :: [IPDR],
		IPDR :: #ipdr_wlan{} | #ipdr_voip{} | undefined.
%% @doc Convert `ocs_acct' log entry to IPDR log entry.
%% @deprecated The IPDR format has been deprecated.
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
%% @deprecated The IPDR format has been deprecated.
%% @private
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
%% @doc CODEC for IMS VOIP
%% @hidden
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
		#{"subscriptionId" := ["imsi-" ++ IMSI | _]} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{subscriberId = IMSI},
	ipdr_ims_voip1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_ims_voip1([subscriberId | T], nrf = Protocol, TimeStamp, ReqType,
		#{"subscriptionId" := ["msisdn-" ++ MSISDN | _]} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_voip{subscriberId = MSISDN},
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
%% @deprecated The IPDR format has been deprecated.
%% @private
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
ipdr_wlan1([username | T], nrf = Protocol, TimeStamp, ReqType,
		#{"subscriptionId" := ["imsi-" ++ IMSI | _]} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{username = IMSI},
	ipdr_wlan1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_wlan1([username | T], nrf = Protocol, TimeStamp, ReqType,
		#{"subscriptionId" := ["msisdn-" ++ MSISDN | _]} = Req, Res, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{username = MSISDN},
	ipdr_wlan1(T, Protocol, TimeStamp, ReqType, Req, Res, Rated, NewIPDR);
ipdr_wlan1([scId | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([acctSessionId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	SessionId = proplists:get_value(?AcctSessionId, Req),
	NewIPDR = IPDR#ipdr_wlan{acctSessionId = SessionId},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([acctSessionId | T], diameter = Protocol, TimeStamp, stop,
		#'3gpp_ro_CCR'{'Session-Id' = SessionId} = Req, Resp, Rated, IPDR) ->
	NewIPDR = IPDR#ipdr_wlan{acctSessionId = binary_to_list(SessionId)},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([acctSessionId | T], nrf = Protocol, TimeStamp, stop,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Resp, Rated, IPDR) ->
	NewIPDR = case lists:keyfind("chargingId", 1, ServiceInfo) of
		{_, ChargingId} ->
			IPDR#ipdr_wlan{acctSessionId = integer_to_list(ChargingId)};
		_ ->
			IPDR
	end,
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([callingStationId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	CallingParty = proplists:get_value(?CallingStationId, Req),
	NewIPDR = IPDR#ipdr_wlan{callingStationId = CallingParty},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([callingStationId | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([calledStationId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	CalledParty = proplists:get_value(?CalledStationId, Req),
	NewIPDR = IPDR#ipdr_wlan{calledStationId = CalledParty},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
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
ipdr_wlan1([nasIpAddress | T], nrf = Protocol, TimeStamp, stop,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Resp, Rated, IPDR) ->
	NewIPDR = case nrf_nf_address(ServiceInfo) of
		NasIpAddress when is_list(NasIpAddress) ->
			IPDR#ipdr_wlan{nasIpAddress = NasIpAddress};
		undefined ->
			IPDR
	end,
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([nasIpAddress | T], Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR);
ipdr_wlan1([nasId | T], radius = Protocol, TimeStamp, stop, Req, Resp, Rated, IPDR) ->
	Identifier = proplists:get_value(?NasIdentifier, Req),
	NewIPDR = IPDR#ipdr_wlan{nasId = Identifier},
	ipdr_wlan1(T, Protocol, TimeStamp, stop, Req, Resp, Rated, NewIPDR);
ipdr_wlan1([nasId | T], nrf = Protocol, TimeStamp, stop,
		#{"serviceRating" := [#{"serviceInformation" := {struct, ServiceInfo}} | _]} = Req,
		Resp, Rated, IPDR) ->
	NewIPDR = case nf_name(ServiceInfo) of
		NasId when is_list(NasId) ->
			IPDR#ipdr_wlan{nasId = NasId};
		undefined ->
			IPDR
	end,
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
ipdr_csv(Log, IoDevice, Seperator, {Cont, [#ipdrDocWLAN{} | T]}) ->
	Columns = [<<"Creation Time">>, <<"Sequence Number">>,
			<<"Username">>, <<"Accounting Session ID">>,
			<<"User IP Address">>, <<"Calling Station ID">>,
			<<"Called Station ID">>, <<"NAS IP Address">>,
			<<"NAS Identifier">>, <<"Class">>,
			<<"Session Terminate Cause">>, <<"Session Duration">>,
			<<"Input Octets">>, <<"Output Octets">>,
			<<"Chargeable Quantity">>, <<"Bucket Type">>,
			<<"Bucket Value">>, <<"Tariff Type">>, <<"Product">>,
			<<"Price Type">>, <<"Usage Rating">>, <<"Charge Amount">>],
	Header = [hd(Columns) | [[Seperator, C] || C <- tl(Columns)]],
	case file:write(IoDevice, [Header, $\r, $\n]) of
		ok ->
			ipdr_csv(Log, IoDevice, Seperator, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, Seperator, {Cont, [#ipdrDocVoIP{} | T]}) ->
	Columns = [<<"Creation Time">>, <<"Sequence Number">>,
			<<"Subscriber ID">>, <<"Unique Call ID">>,
			<<"Destination ID">>, <<"Call Completion Code">>,
			<<"Disconnect Reason">>, <<"Host Name">>,
			<<"Bucket Type">>, <<"Bucket Value">>,
			<<"Tariff Type">>, <<"Product">>, <<"Price Type">>,
			<<"Usage Rating">>, <<"Charge Amount">>],
	Header = [hd(Columns) | [[Seperator, C] || C <- tl(Columns)]],
	case file:write(IoDevice, [Header, $\r, $\n]) of
		ok ->
			ipdr_csv(Log, IoDevice, Seperator, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, Seperator, {Cont, [#ipdr_voip{} = I | T]}) ->
	Time = list_to_binary(I#ipdr_voip.ipdrCreationTime),
	Seq = integer_to_binary(I#ipdr_voip.seqNum),
	SubId = case I#ipdr_voip.subscriberId of
		undefined ->
			<<>>;
		SID ->
			[$", SID, $"]
	end,
	UniqueId = case I#ipdr_voip.uniqueCallID of
		undefined ->
			<<>>;
		UID ->
			[$", UID, $"]
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
		 ocs_rest:millionths_out(CA)
	end,
	Dest = case I#ipdr_voip.destinationID of
		undefined ->
			<<>>;
		DestID ->
			[$", DestID, $"]
	end,
	CCCode = case I#ipdr_voip.callCompletionCode of
		undefined ->
			<<>>;
		ComCode when is_integer(ComCode) ->
			integer_to_list(ComCode);
		ComCode when is_list(ComCode) ->
			ComCode
	end,
	DiscReason = case I#ipdr_voip.disconnectReason of
		undefined ->
			<<>>;
		Disc when is_integer(Disc) ->
			integer_to_binary(Disc);
		Disc when is_list(Disc) ->
			Disc
	end,
	HostName = case I#ipdr_voip.hostName of
		undefined ->
			<<>>;
		HN ->
			HN
	end,
	Columns = [Time, Seq, SubId, UniqueId, Dest, CCCode,
			DiscReason, HostName, BType, BValue, TType,
			Prod, PType, URating, ChargeA],
	Row = [hd(Columns) | [[Seperator, C] || C <- tl(Columns)]],
	case file:write(IoDevice, [Row, $\r, $\n]) of
		ok ->
			ipdr_csv(Log, IoDevice, Seperator, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, Seperator, {Cont, [#ipdr_wlan{} = I | T]}) ->
	Time = list_to_binary(I#ipdr_wlan.ipdrCreationTime),
	Seq = integer_to_binary(I#ipdr_wlan.seqNum),
	User = case I#ipdr_wlan.username of
		undefined ->
			<<>>;
		US ->
			[$", US, $"]
	end,
	Sess = case I#ipdr_wlan.acctSessionId of
		undefined ->
			<<>>;
		SI ->
			[$", SI, $"]
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
			[$", CgID, $"]
	end,
	Called = case I#ipdr_wlan.calledStationId of
		undefined ->
			<<>>;
		CdID ->
			[$", CdID, $"]
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
			[$", NID, $"]
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
		 ocs_rest:millionths_out(CA)
	end,
	Columns = [Time, Seq, User, Sess, IP, Calling,
			Called, NasIP, NasID, Class, Cause, Duration,
			Input, Output, ChargeQ, BType, BValue, TType,
			Prod, PType, URating, ChargeA],
	Row = [hd(Columns) | [[Seperator, C] || C <- tl(Columns)]],
	case file:write(IoDevice, [Row, $\r, $\n]) of
		ok ->
			ipdr_csv(Log, IoDevice, Seperator, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
ipdr_csv(Log, IoDevice, _Seperator, {Cont, [#ipdrDocEnd{}]}) ->
	ipdr_file3(Log, IoDevice, csv, {Cont, []});
ipdr_csv(Log, IoDevice, _Seperator, {Cont, []}) ->
	ipdr_file3(Log, IoDevice, csv, {Cont, []}).

%% @hidden
http_parse(Event) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Host:Offset/binary, 32, $-, 32, Rest/binary>> = Event,
	http_parse1(Rest, #event{host = binary_to_list(Host)}).
%% @hidden
http_parse1(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<User:Offset/binary, 32, $[, Rest/binary>> = Event,
	http_parse2(Rest, Acc#event{user = binary_to_list(User)}).
%% @hidden
http_parse2(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<$]>>),
	<<Date:Offset/binary, $], 32, $", Rest/binary>> = Event,
	http_parse3(Rest, Acc#event{date = binary_to_list(Date)}).
%% @hidden
http_parse3(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Method:Offset/binary, 32, Rest/binary>> = Event,
	http_parse4(Rest, Acc#event{method = binary_to_list(Method)}).
%% @hidden
http_parse4(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<URI:Offset/binary, 32, Rest/binary>> = Event,
	http_parse5(Rest, Acc#event{uri = binary_to_list(URI)}).
%% @hidden
http_parse5(Event, Acc) ->
	{Offset, 2} = binary:match(Event, <<$", 32>>),
	<<_Http:Offset/binary, $", 32, Rest/binary>> = Event,
	http_parse6(Rest, Acc).
%% @hidden
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
%% @doc Open log.
%% @hidden
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
	FileName = filename:join([Directory, Log]),
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
%% @doc Write event to log.
%% @hidden
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
%% @doc Close log.
%% @hidden
close_log(Log) ->
	close_log1(Log,  disk_log:sync(Log)).
%% @hidden
close_log1(Log, ok) ->
	close_log2(Log, disk_log:close(Log));
close_log1(Log, {error, Reason}) ->
	close_log2(Log, {error, Reason}).
%% @hidden
close_log2(_Log, ok) ->
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
%% @private
query_log(Continuation, {{_, _, _}, {_, _, _}} = Start, End, Log, MFA) ->
	Seconds = calendar:datetime_to_gregorian_seconds(Start) - ?EPOCH,
	query_log(Continuation, Seconds * 1000, End, Log, MFA);
query_log(Continuation, Start, {{_, _, _}, {_, _, _}} = End, Log, MFA) ->
	Seconds = calendar:datetime_to_gregorian_seconds(End) - ?EPOCH,
	query_log(Continuation, Start, Seconds * 1000 + 999, Log, MFA);
query_log(start, Start, End, Log, MFA)
		when is_integer(Start), is_integer(End), Start =< End ->
	case btree_search(Log, Start) of
		{error, Reason} ->
			{error, Reason};
		Continuation ->
			query_log1(Start, End, MFA, disk_log:chunk(Log, Continuation), [])
	end;
query_log(Continuation, Start, End, Log, MFA)
		when is_integer(Start), is_integer(End), Start =< End ->
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

%% @hidden
nrf_nf_address(PDUSessionChargingInformation)
		when is_list(PDUSessionChargingInformation) ->
	nrf_nf_address1(lists:keyfind("pduSessionInformation",
			1, PDUSessionChargingInformation));
nrf_nf_address(_) ->
	undefined.
%% @hidden
nrf_nf_address1({_, {struct, PduSessionInformation}}) ->
	nrf_nf_address2(lists:keyfind("servingNetworkFunctionID",
			1, PduSessionInformation));
nrf_nf_address1(_) ->
	undefined.
%% @hidden
nrf_nf_address2({_, {struct, ServingNetworkFunctionID}}) ->
	nrf_nf_address3(lists:keyfind("servingNetworkFunctionInformation",
			1, ServingNetworkFunctionID));
nrf_nf_address2(_) ->
	undefined.
%% @hidden
nrf_nf_address3({_, {struct, NFIdentification}}) ->
	nrf_nf_address4(NFIdentification,
			lists:keyfind("nFIPv4Address", 1, NFIdentification));
nrf_nf_address3(_) ->
	undefined.
%% @hidden
nrf_nf_address4(_NFIdentification, {_, Ipv4Addr}) ->
	Ipv4Addr;
nrf_nf_address4(NFIdentification, _) ->
	nrf_nf_address5(lists:keyfind("nFIPv6Address", 1, NFIdentification)).
%% @hidden
nrf_nf_address5({_, Ipv6Addr}) ->
	Ipv6Addr;
nrf_nf_address5(_) ->
	undefined.

%% @hidden
nf_name(PDUSessionChargingInformation)
		when is_list(PDUSessionChargingInformation) ->
	nf_name1(lists:keyfind("pduSessionInformation",
			1, PDUSessionChargingInformation));
nf_name(_) ->
	undefined.
%% @hidden
nf_name1({_, {struct, PduSessionInformation}}) ->
	nf_name2(lists:keyfind("servingNetworkFunctionID",
			1, PduSessionInformation));
nf_name1(_) ->
	undefined.
%% @hidden
nf_name2({_, {struct, ServingNetworkFunctionID}}) ->
	nf_name3(lists:keyfind("servingNetworkFunctionInformation",
			1, ServingNetworkFunctionID));
nf_name2(_) ->
	undefined.
%% @hidden
nf_name3({_, {struct, NFIdentification}}) ->
	nf_name4(lists:keyfind("nFName", 1, NFIdentification));
nf_name3(_) ->
	undefined.
%% @hidden
nf_name4({_, NFInstanceId}) ->
	NFInstanceId;
nf_name4(_) ->
	undefined.

-type involved_party() :: {'sIP-URI', binary()}
		| {'tEL-URI', binary()}
		| {uRN, binary()}
		| {'iSDN-E164', binary()}
		| {externalId , binary()}.
%% Involved party.

-type subscription_id() :: #{
		subscriptionIDType := 'eND-USER-E164' | 'eND-USER-IMSI'
				| 'eND-USER-SIP-URI' | 'eND-USER-NAI' | 'eND-USER-PRIVATE',
		subscriptionIDData := binary()}.
%% Subscription identifier.
%%
%% See 3GPP TS 23.003.

-type subscriber_equipment_number() :: #{
		subscriberEquipmentNumberType := iMEISV | mAC | eUI64 | modifiedEUI64,
		subscriberEquipmentNumberData := binary()}.
%% Subscriber equipment number.
%%
%% See 3GPP TS 32.298 5.1.1.7 Subscriber Equipment Number.

-type cell_global_id() :: #{
		plmnId := binary(),
		lac := binary(),
		cellId := binary()}.
%% Cell Global ID (CGI)

-type service_area_id() :: #{
		plmnId := binary(),
		lac := binary(),
		sac := binary()}.
%% Service Area ID (SAI)

-type location_area_id() :: #{
		plmnId := binary(),
		lac := binary()}.
%% Location Area ID

-type routing_area_id() :: #{
		plmnId := binary(),
		lac := binary(),
		rac := binary()}.
%% Routing Area ID (RAI)

-type tracking_area_id() :: #{
		plmnId := binary(),
		tac := binary()}.
%% Tracking Area ID (TAI)

-type ecgi() :: #{
		plmnId := binary(),
		eutraCellId := binary(),
		nid => binary()}.
%% EUTRAN Cell Global ID (ECGI)

-type ncgi() :: #{
		plmnId := binary(),
		nrCellId := binary(),
		nid => binary()}.
%% 5G NR Cell Global ID (NCGI)

-type gnb_id() :: #{
		bitLength => pos_integer(),
		gNbValue => binary()}.
%% 5G NR gNodeB ID

-type global_ran_node_id() :: #{
		pLMNId => binary(),
		n3IwfId => binary(),
		gNbId => gnb_id(),
		ngeNbId => binary(),
		wagfId => binary(),
		tngfId => binary(),
		nid => binary(),
		eNbId => binary()}.
%% Global RAN node ID

-type nr_location() :: #{
		tai => tracking_area_id(),
		ncgi => ncgi(),
		ageOfLocationInformation => non_neg_integer(),
		ueLocationTimestamp => timestamp(),
		geographicalInformation => binary(),
		geodeticInformation => binary(),
		globalNgenbId => global_ran_node_id(),
		globalGnbId => global_ran_node_id()}.
%% 5G NR UE Location

-type eutra_location() :: #{
		tai => tracking_area_id(),
		ecgi => ecgi(),
		ageOfLocationInformation => non_neg_integer(),
		ueLocationTimestamp => timestamp(),
		geographicalInformation => binary(),
		geodeticInformation => binary(),
		globalNgenbId => global_ran_node_id(),
		globalENbId => global_ran_node_id()}.
%% EUTRAN UE Location

-type utra_location() :: #{
		cgi => cell_global_id(),
		sai => service_area_id(),
		lai => location_area_id(),
		rai => routing_area_id(),
		ageOfLocationInformation => non_neg_integer(),
		ueLocationTimestamp => timestamp(),
		geographicalInformation => binary(),
		geodeticInformation => binary()}.
%% UTRAN UE Location

-type gera_location() :: #{
		locationNumber => binary(),
		cgi => cell_global_id(),
		sai => service_area_id(),
		lai => location_area_id(),
		rai => routing_area_id(),
		vlrNumber => binary(),
		mscNumber => binary(),
		ageOfLocationInformation => non_neg_integer(),
		ueLocationTimestamp => timestamp(),
		geographicalInformation => binary(),
		geodeticInformation => binary()}.
%% GERAN UE Location

-type n3ga_location() :: #{
		n3gppTai => tracking_area_id(),
		n3IwfId => binary(),
		ueIpv4Addr => inet:ip4_address(),
		ueIpv6Addr => inet:ip6_address(),
		portNumber => 0..65535,
		tnapId => binary(),
		twapId => binary(),
		hfcNodeId => binary(),
		w5gbanLineType => dSL | pON,
		gli => binary(),
		gci => binary()}.
%% non-3GPP UE Location

-type structured_location_info() :: #{
		eutraLocation => eutra_location(),
		nrLocation => nr_location(),
		n3gaLocation => n3ga_location(),
		utraLocation => utra_location(),
		geraLocation => gera_location()}.
%% UE location information.
%%
%% See 3GPP TS 32.298 5.1.2.2.75 User Location Information.

-type single_nssai() :: #{
		sST := 0..255,
		sD := binary()}.
%% S-NSSAI (Single Network Slice Selection Assistance Information).

-type serving_nf_id() :: #{
		servingNetworkFunctionInformation := nf_info(),
		aMFIdentifier := binary()}.
%% Serving network function (NF) identifier.

-type pdu_address() :: #{
		pDUIPv4Address => inet:ip4_address(),
		pDUIPv6AddresswithPrefix => inet:ip6_address(),
		iPV4dynamicAddressFlag => boolean(),
		iPV6dynamicPrefixFlag => boolean(),
		additionalPDUIPv6Prefixes => [inet:ip6_address()]}.
%% PDU address.

-type allocation_retension_priority() :: #{
		priorityLevel := 1..15,
		preemptionCapability := 'nOT-PREEMPT' | 'mAY-PREEMPT',
		preemptionVulnerability := 'nOT-PREEMPTABLE' | 'pREEMPTABLE'}.
%% Allocation retension priority.

-type authorized_qos_info() :: #{
		fiveQi => 0..255,
		aRP => allocation_retension_priority(),
		priorityLevel => 1..15,
		averWindow => 1..4095,
		maxDataBurstVol => 1..4095}.
%% Authorized QoS Information.
%%
%% See 3GPP TS 32.298 5.1.2.2.46 QoS Requested/QoS Negotiated.

-type diagnostics() :: {gsm0408Cause, 0..127}
		| {gsm0902MapErrorValue, 0..1
				| 3 | 5..22 | 25..39 | 42..54 | 58..62 | 71..72}
		| {'itu-tQ767Cause', 1..5 | 16..19 | 21..22 | 27..29 | 31 | 34
				| 38 | 41..42 | 44 | 47 | 50 | 55 | 57..58 | 63 | 65
				| 69..70 | 79 | 87..88 | 91 | 95 | 97 | 99 | 102..103
				| 111 | 127}
		| {diameterResultCodeAndExperimentalResult, 2000..5999}.
%% Diagnostics.
%%
%% See 3GPP TS 32.298 5.1.2.2.11 Diagnostics.

-type subscribed_qos_info() :: #{
		fiveQi => 0..255,
		aRP => allocation_retension_priority(),
		priorityLevel => 1..15}.
%% Subscribed QoS Information.
%%
%% See 3GPP TS 32.298 5.1.2.2.46 QoS Requested/QoS Negotiated.

-type session_ambr() :: #{
		ambrUL := binary(),
		ambrDL := binary()}.
%% EPC QoS information.
%%
%% See 3GPP TS 32.298 5.1.2.2.13B EPC QoS Information.

-type rat_type() :: byte().
%% RAT Type.
%%
%% 	1   UTRAN
%% 	2   GERAN
%% 	3   WLAN
%% 	4   GAN (UMA)
%% 	5   HSPA Evolution
%% 	6   EUTRAN
%% 	7   Virtual
%% 	8   EUTRAN NB-IoT
%% 	9   LTE-M
%% 	10  NR
%% 	51  NR
%% 	52  NR Unlicenced
%% 	53  EUTRAN Unlicened
%% 	54  LTE-M
%% 	55  Wireline
%% 	56  Wireline Cable
%% 	57  Wireline BBF
%% 	58  NR REDCAP
%% 	59  NR LEO
%% 	60  NR MEO
%% 	61  NR GEO
%% 	62  NR Other Satellite
%% 	65  N3GA Trusted
%% 	66  WLAN Trusted
%% 	101 IEEE 802.16e
%% 	102 3GPP2 eHRPD
%% 	103 3GPP2 HRPD
%% 	104 3GPP2 1xRTT
%% 	105 3GPP2 UMB
%%
%% See 3GPP TS 32.298 5.1.2.2.47 RAT Type.

-type pdu_session_charging_info() :: #{
		pDUSessionChargingID := 0..4294967295,
		userIdentifier => involved_party(),
		userEquipmentInfo =>  subscriber_equipment_number(),
		userLocationInformation => structured_location_info() | binary(),
		userRoamerInOut => roamerInBound | roamerOutBound,
		pDUSessionId := 0..255,
		networkSliceInstanceID => single_nssai(),
		pDUType =>  iPv4v6 | iPv4 | iPv6 | unstructured | ethernet,
		sSCMode => 1..3,
		sUPIPLMNIdentifier => binary(),
		servingNetworkFunctionID => [serving_nf_id()],
		rATType => rat_type(),
		dataNetworkNameIdentifier => binary(),
		pDUAddress => pdu_address(),
		authorizedQoSInformation => authorized_qos_info(),
		uETimeZone => binary(),
		pDUSessionstartTime => timestamp(),
		pDUSessionstopTime => timestamp(),
		diagnostics => diagnostics(),
		chargingCharacteristics => binary(),
		chChSelectionMode => servingNodeSupplied | subscriptionSpecific
				| aPNSpecific | homeDefault | roamingDefault
				| visitingDefault | fixedDefault,
		threeGPPPSDataOffStatus => active | inactive,
%		rANSecondaryRATUsageReport => [NGRANSecondaryRATUsageReport],
		subscribedQoSInformation => subscribed_qos_info(),
		authorizedSessionAMBR => session_ambr(),
		subscribedSessionAMBR => session_ambr(),
		servingCNPLMNID => binary(),
		sUPIunauthenticatedFlag => boolean(),
		dnnSelectionMode => uEorNetworkProvidedSubscriptionVerified
				| uEProvidedSubscriptionNotVerified
				| networkProvidedSubscriptionNotVerified,
		homeProvidedChargingID => 0..4294967295,
		mAPDUNonThreeGPPUserLocationInfo => structured_location_info() | binary(),
		mAPDUNonThreeGPPRATType => rat_type(),
%		mAPDUSessionInformation => MAPDUSessionInformation,
%		enhancedDiagnostics => EnhancedDiagnostics5G,
		userLocationInformationASN1 => structured_location_info(),
		mAPDUNonThreeGPPUserLocationInfoASN1 => structured_location_info(),
		mAPDUNonThreeGPPUserLocationTime => timestamp(),
%		listOfPresenceReportingAreaInformation => [PresenceReportingAreaInfo],
		redundantTransmissionType => nonTransmission
				| endToEndUserPlanePaths | n3N9 | transportLayer,
		pDUSessionPairID => 0..4294967295,
%		fiveGLANTypeService => FiveGLANTypeService,
		cpCIoTOptimisationIndicator => timestamp(),
%		fiveGSControlPlaneOnlyIndicator => QosMonitoringReport,
		smfChargingID => binary(),
		smfHomeProvidedChargingID => binary()}.
%% PS charging information.
%%
%% See 3GPP TS 32.298 5.1.2.2 PS domain CDR parameters.

-type sip_event_type() :: #{
		sIPMethod => binary(),
		eventHeader => integer(),
      expiresHeader => binary()}.
%% SIP event type.
%%
%% See 3GPP TS 32.298 5.1.3.1.15 Event.

-type isup_cause() :: #{
		iSUPCauseLocation => 0..10,
      iSUPCauseValue => 1..127,
      iSUPCauseDiagnostics => binary()}.
%% ISUP cause.
%%
%% See 3GPP TS 32.298 5.1.3.1.21C ISUP Cause.

-type called_id_change() :: #{
		calledIdentity => involved_party(),
		changeTime => timestamp()}.
%% Terminating identity address change.
%%
%% See 3GPP TS 32.298 5.1.3.1.23A List of Called Identity Changes.

-type inter_operator_identifier() :: #{
		originatingIOI => binary(),
		terminatingIOI => binary()}.
%% Inter Operator Identifier (IOI).
%%
%% See 3GPP TS 32.298 5.1.3.1.26 List of Inter Operator Identifiers.

-type app_server_info() :: #{
		applicationServersInvolved => inet:ip_address() | binary(),
		applicationProvidedCalledParties => [involved_party()],
		sTatus => fourxx | fivexx | 'time-out'}.
%% Application Server (AS) information.
%%
%% See 3GPP TS 32.298 5.1.3.1.5 Application Servers Information.

-type sdp_media_component() :: #{
		'sDP-Media-Name' => binary(),
		'sDP-Media-Descriptions' => [binary()],
		accessCorrelationID => {'gPRS-Charging-Id', 0..4294967295}
				| {accessNetworkChargingIdentifier, binary()},
		localGWInsertedIndication => boolean(),
		iPRealmDefaultIndication => boolean(),
		transcoderInsertedIndication => boolean()}.
%% Early SDP media component.
%%
%% See 3GPP TS 32.298 5.1.3.1.25 List of Early SDP Media Components.

-type early_media_components() :: #{
		'sDP-Offer-Timestamp' => timestamp(),
		'sDP-Answer-Timestamp' => timestamp(),
		'sDP-Media-Components' => [sdp_media_component()],
		mediaInitiatorFlag => boolean(),
		'sDP-Session-Description' => [binary()],
		'sDP-Type' => 'sDP-offer' | 'sDP-answer'}.
%% Early SDP media components.
%%
%% See 3GPP TS 32.298 5.1.3.1.25 List of Early SDP Media Components.

-type scscf_info() :: #{
		mandatoryCapabilities => [binary()],
		optionalCapabilities => [binary()],
		serverName => binary()}.
%% Serving-CSCF information.
%%
%% See 3GPP TS 32.298 5.1.3.1.66 S-CSCF Information

-type transmission_medium() :: #{
		tMR => binary(),
		tMU => binary()}.
%% Bearer service.
%%
%% See 3GPP TS 32.298 5.1.3.1.8 Bearer Service.

-type message_body() :: #{
		'content-Type' := binary(),
		'content-Disposition' => binary(),
		'content-Length' := pos_integer(),
		originator => involved_party()}.
%% Message body.
%%
%% See 3GPP TS 32.298 5.1.3.1.27 List of Message Bodies.

-type access_transfer_info() :: #{
		accessTransferType => pSToCS | cSToPS | pSToPS | cSToCS,
		accessNetworkInformation => binary(),
		additionalAccessNetworkInformation => binary(),
		'inter-UE-Transfer' => boolean(),
		relatedICID => binary(),
		relatedICIDGenerationNode => inet:ip_address() | binary(),
		accessTransferTime => timestamp(),
		subscriberEquipmentNumber => subscriber_equipment_number(),
		instanceId => binary(),
		cellularNetworkInformation => binary()}.
%% Access transfer information.
%%
%% See 3GPP TS 32.298 5.1.3.1.21D List of Access Transfer Information.

-type access_network_info_change() :: #{
		accessNetworkInformation => binary(),
		additionalAccessNetworkInformation => binary(),
		accessChangeTime => timestamp(),
		cellularNetworkInformation => binary()}.
%% Access network info change.
%%
%% See 3GPP TS 32.298 5.1.3.1.21Ca List of Access Network Info Change.

-type nni_info() :: #{
		sessionDirection => inbound | outbound,
		nNIType => 'non-roaming' | 'roaming-without-loopback'
				| 'roaming-with-loopback',
		relationshipMode => trusted | 'non-trusted',
		neighbourNodeAddress => inet:ip_address() | binary()}.
%% NNI Information.
%%
%% See 3GPP TS 32.298 5.1.3.1.27A List of NNI Information.

-type ims_charging_info() :: #{
		eventType => sip_event_type(),
		iMSNodeFunctionality => 'iMS-GWF' | aS | mRFC,
		roleOfNode => originating | terminating,
		userIdentifier => involved_party(),
		userEquipmentInfo => subscriber_equipment_number(),
		userLocationInformation => structured_location_info() | binary(),
		ueTimeZone => binary(),
		threeGPPPSDataOffStatus => active | inactive,
		iSUPCause => isup_cause(),
		controlPlaneAddress => inet:ip_address() | binary(),
		vlrNumber => binary(),
		mscAddress => binary(),
		userSessionID => binary(),
		outgoingSessionID => binary(),
		sessionPriority => 0..4,
		callingPartyAddresses => [involved_party()],
		calledPartyAddress => involved_party(),
		numberPortabilityRouting => binary(),
		carrierSelectRoutingInformation => binary(),
		alternateChargedPartyAddress => binary(),
		requestedPartyAddresses => [involved_party()],
		calledAssertedIdentities => [involved_party()],
		calledIdentityChanges => [called_id_change()],
		associatedURIs => [involved_party()],
		timeStamps => timestamp(),
		applicationServerInformation => [app_server_info()],
		interOperatorIdentifiers => [inter_operator_identifier()],
		imsChargingIdentifier => binary(),
		relatedICID => binary(),
		relatedICIDGenerationNode => inet:ip_address() | binary(),
		transitIOIList => [binary()],
		earlyMediaDescription => [early_media_components()],
		sdpSessionDescription => [binary()],
		sdpMediaComponent => [sdp_media_component()],
		servedPartyIPAddress => inet:ip_address() | binary(),
		serverCapabilities => scscf_info(),
		trunkGroupID => {incoming, binary()} | {outgoing, binary()},
		bearerService => transmission_medium(),
		imsServiceId => binary(),
		messageBodies => [message_body()],
		accessNetworkInformation => [binary()],
		additionalAccessNetworkInformation => binary(),
		cellularNetworkInformation => binary(),
		accessTransferInformation => [access_transfer_info()],
		accessNetworkInfoChange => [access_network_info_change()],
		imsCommunicationServiceID => binary(),
		imsApplicationReferenceID => binary(),
		causeCode => 1..127,
		reasonHeaders => [binary()],
		initialIMSChargingIdentifier => binary(),
		nniInformation => [nni_info()],
		fromAddress => binary(),
		imsEmergencyIndicator => boolean(),
		imsVisitedNetworkIdentifier => binary(),
		sipRouteHeaderReceived => binary(),
		sipRouteHeaderTransmitted => binary(),
		tadIdentifier => cS | pS,
		feIdentifierList => [binary()]}.
%% IMS charging information.
%%
%% See 3GPP TS 32.298 5.1.3.1 IMS CDR parameters.

-type sms_address_domain() :: #{
		sMDomainName => binary(),
		'threeGPPIMSI-MCC-MNC' => binary()}.
%% SM address domain.
%%
%% See 3GPP TS 32.298 5.1.4.6.8 Originator Info.

-type sms_address_info() :: #{
		sMAddressType => emailAddress | mSISDN
				| iPv4Address | iPv6Address
				| numericShortCode | alphanumericShortCode
				| other | iMSI | nAI | externalId,
		sMAddressData => binary(),
		sMAddressDomain  => sms_address_domain()}.
%% SM address info.
%%
%% See 3GPP TS 32.298 5.1.4.6.8 Originator Info.

-type sm_interface() :: #{
		interfaceId => binary(),
		interfaceText => binary(),
		interfacePort => binary(),
		interfaceType => unkown
				| mobileOriginating | mobileTerminating
				| applicationOriginating | applicationTerminating
				| deviceTrigger}.
%% SM originator interface.
%%
%% See 3GPP TS 32.298 5.1.4.6.8 Originator Info.

-type originator_info() :: #{
		originatorIMSI => binary(),
		originatorMSISDN => binary(),
		originatorOtherAddress => sms_address_info(),
		originatorSCCPAddress => binary(),
		originatorReceivedAddress => sms_address_info(),
		sMOriginatorInterface => sm_interface(),
		sMOriginatorProtocolID => binary(),
		originatorOtherAddresses => [sms_address_info()]}.
%% Set of information on the originator of the Short Message (SM).
%%
%% See 3GPP TS 32.298 5.1.4.6.8 Originator Info.

-type recipient_info() :: #{
		recipientIMSI => binary(),
		recipientMSISDN => binary(),
		recipientOtherAddress => sms_address_info(),
		recipientSCCPAddress => binary(),
		recipientReceivedAddress => sms_address_info(),
		sMDestinationInterface => sm_interface(),
		sMRecipientProtocolID => binary(),
		recipientOtherAddresses => [sms_address_info()]}.
%% Set of information on a recipient of the Short Message (SM).
%%
%% See 3GPP TS 32.298 5.1.4.6.15 Recipient Info.

-type sms_charging_info() :: #{
		originatorInfo => originator_info(),
		recipientInfos => [recipient_info()],
		userEquipmentInfo => subscriber_equipment_number(),
		userLocationInformation => structured_location_info() | binary(),
		uETimeZone => binary(),
		rATType => rat_type(),
		sMSCAddress => binary(),
		eventtimestamp := timestamp(),
		sMDataCodingScheme => byte(),
		sMMessageType => submission | deliveryReport | sMServiceRequest
				| delivery | t4DeviceTrigger | sMDeviceTrigger ,
		sMReplyPathRequested => noReplyPathSet | replyPathSet,
		sMUserDataHeader => binary(),
		sMSStatus => binary(),
		sMDischargeTime => timestamp(),
		sMTotalNumber => 0..255,
		sMServiceType => 0..199,
		sMSequenceNumber => 0..255,
		sMSResult => diagnostics(),
		submissionTime => timestamp(),
		sMPriority => low | normal | high,
		messageReference => binary(),
		messageSize => non_neg_integer(),
		messageClass => personal | advertisement
				| 'information-service' | auto,
		sMdeliveryReportRequested => yes | no,
		messageClassTokenText => binary(),
		userRoamerInOut => roamerInBound | roamerOutBound,
		userLocationInformationASN1 => structured_location_info()}.
%% SMS charging information.
%%
%% See 3GPP TS 32.298 5.1.4.6 SMS CDR parameters.

-type nf_info() :: #{
		networkFunctionality => cHF | sMF | aMF | sMSF
				| sGW | iSMF | ePDG | cEF | nEF | pGWCSMF
				| 'mnS-Producer' | sGSN | fiveGDDNMF | vSMF
				| 'iMS-Node' | eES | 'mMS-Node' | pCF | uDM
				| uPF | 'tSN-AF' | tSNTSF | 'mB-SMF ',
		networkFunctionName => binary(),
		networkFunctionIPv4Address => inet:ip4_address(),
		networkFunctionPLMNIdentifier => binary(),
		networkFunctionIPv6Address => inet:ip6_address(),
		networkFunctionFQDN => binary()}.
%% Information about the network function (NF) that used the charging service.
%%
%% See 3GPP TS 32.298 5.1.5.1.6 NF Consumer Information.

-type chargingFunctionRecord() :: #{
		recordType := chargingFunctionRecord,
		recordingNetworkFunctionID := binary(),
		subscriberIdentifier => [subscription_id()],
		nFunctionConsumerInformation => nf_info(),
%		triggers => [Trigger],
%		listOfMultipleUnitUsage => [MultipleUnitUsage],
		recordOpeningTime := timestamp(),
		duration => non_neg_integer(),
		recordSequenceNumber => non_neg_integer(),
		causeForRecClosing => normalRelease
				| abnormalRelease | volumeLimit | timeLimit,
		diagnostics => diagnostics(),
		localRecordSequenceNumber => 0..4294967295,
%		recordExtensions => ManagementExtensions,
		pDUSessionChargingInformation => pdu_session_charging_info(),
%		roamingQBCInformation => RoamingQBCInformation,
		sMSChargingInformation => sms_charging_info(),
		chargingSessionIdentifier => binary(),
		serviceSpecificationInformation => binary(),
%		exposureFunctionAPIInformation => ExposureFunctionAPIInformation,
%		registrationChargingInformation => RegistrationChargingInformation,
%		n2ConnectionChargingInformation => N2ConnectionChargingInformation,
%		locationReportingChargingInformation => LocationReportingChargingInformation,
%		incompleteCDRIndication => IncompleteCDRIndication,
		tenantIdentifier => binary(),
		mnSConsumerIdentifier => binary(),
%		nSMChargingInformation => NSMChargingInformation,
%		nSPAChargingInformation => NSPAChargingInformation,
		chargingID => 0..4294967295,
		iMSChargingInformation => ims_charging_info(),
%		mMTelChargingInformation => MMTelChargingInformation,
%		edgeInfrastructureUsageChargingInformation => EdgeInfrastructureUsageChargingInformation,
%		eASDeploymentChargingInformation => EASDeploymentChargingInformation,
%		directEdgeEnablingServiceChargingInformation => ExposureFunctionAPIInformation,
%		exposedEdgeEnablingServiceChargingInformation => ExposureFunctionAPIInformation,
%		proseChargingInformation => ProseChargingInformation,
		eASID => binary(),
		eDNID => binary(),
		eASProviderIdentifier => binary(),
		aMFIdentifier => binary()}.
%% CHF record (CHF-CDR).
%%
%% See 3GPP TS 32.298 5.1.5.0.

-type cdr() :: {TimeStamp :: timestamp(),
		Unique :: unique(),
		Protocol :: radius | diameter | nrf,
		ChargingRecord :: chargingFunctionRecord(),
		Rated :: acct_rated()}.
%% A charging detail record in a CDR archive log.

-spec chf_ps(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) -> CDR
	when
		TimeStamp :: timestamp(),
		Unique :: unique(),
		Protocol :: radius | diameter | nrf,
		ReqType :: acct_type(),
		Req :: [tuple()] | #'3gpp_ro_CCR'{} | map() | undefined,
		Res :: [tuple()] | #'3gpp_ro_CCA'{} | map() | undefined,
		Rated :: acct_rated(),
		CDR :: cdr().
%% @doc CODEC for PS CHF-CDR.
%% @private
chf_ps(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) ->
	CFR = #{recordType => chargingFunctionRecord,
			recordingNetworkFunctionID => atom_to_binary(node()),
			recordOpeningTime => TimeStamp},
	CFR1 = chf_ps1(Protocol, ReqType, Req, Res, CFR),
	{TimeStamp, Unique, Protocol, CFR1, Rated}.
%% @hidden
chf_ps1(nrf = Protocol, ReqType,
		#{"ratingSessionId" := SessionId} = Req, Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => list_to_binary(SessionId)},
	chf_ps2(Protocol, ReqType, Req, Res, CFR1);
chf_ps1(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Session-Id' = SessionId} = Req,
		Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => SessionId},
	chf_ps2(Protocol, ReqType, Req, Res, CFR1);
chf_ps1(radius = Protocol, ReqType, Req, Res, CFR)
		when is_list(Req) ->
	CFR1 = case radius_attributes:find(?AcctSessionId, Req) of
		{ok, SessionId} ->
			CFR#{chargingSessionIdentifier => list_to_binary(SessionId)};
		{error, not_found} ->
			CFR
	end,
	chf_ps2(Protocol, ReqType, Req, Res, CFR1);
chf_ps1(Protocol, ReqType, Req, Res, CFR) ->
	chf_ps2(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ps2(nrf = Protocol, ReqType,
		#{"nfConsumerIdentification" := NfInfo} = Req,
		Res, CFR) ->
	NFI = nrf_nf_info(NfInfo),
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_ps3(Protocol, ReqType, Req, Res, CFR1);
chf_ps2(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Origin-Host' = OriginHost} = Req,
		Res, CFR) ->
	NFI = #{networkFunctionFQDN => OriginHost},
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_ps3(Protocol, ReqType, Req, Res, CFR1);
chf_ps2(radius = Protocol, ReqType, Req, Res, CFR)
		when is_list(Req) ->
	NFI = case radius_attributes:find(?NasIdentifier, Req) of
		{ok, Name} ->
			#{networkFunctionName => list_to_binary(Name)};
		{error, not_found} ->
			#{}
	end,
	NFI1 = case radius_attributes:find(?NasIpAddress, Req) of
		{ok, Address} ->
			NFI#{networkFunctionIPv4Address => Address};
		{error, not_found} ->
			NFI
	end,
	CFR1 = CFR#{nFunctionConsumerInformation => NFI1},
	chf_ps3(Protocol, ReqType, Req, Res, CFR1);
chf_ps2(Protocol, ReqType, Req, Res, CFR) ->
	chf_ps3(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ps3(nrf = Protocol, ReqType,
		#{"serviceContextId" := Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_ps4(Protocol, ReqType, Req, Res, CFR1);
chf_ps3(nrf = Protocol, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Context} | _]} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_ps4(Protocol, ReqType, Req, Res, CFR1);
chf_ps3(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => Context},
	chf_ps4(Protocol, ReqType, Req, Res, CFR1);
chf_ps3(Protocol, ReqType, Req, Res, CFR) ->
	chf_ps4(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ps4(nrf = Protocol, ReqType,
		#{"subscriptionId" := SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = nrf_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_ps5(Protocol, ReqType, Req, Res, CFR1);
chf_ps4(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Subscription-Id' = SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = diameter_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_ps5(Protocol, ReqType, Req, Res, CFR1);
chf_ps4(radius = Protocol, ReqType, Req, Res, CFR) ->
	UserName = radius_attributes:fetch(?UserName, Req),
	NAI = list_to_binary(["nai-", UserName]),
	SubscriptionId1 = [#{subscriptionIDType => 'eND-USER-NAI',
			subscriptionIDData => NAI}],
	CFR1 = case radius_attributes:find(?'3GPP', ?'3GPP-IMSI', Req) of
		{ok, IMSI} ->
			SubscriptionId2 = [#{subscriptionIDType => 'eND-USER-IMSI',
					subscriptionIDData => IMSI}],
			CFR#{subscriberIdentifier => SubscriptionId2 ++ SubscriptionId1};
		{error, not_found} ->
			CFR#{subscriberIdentifier => SubscriptionId1}
	end,
	chf_ps5(Protocol, ReqType, Req, Res, CFR1);
chf_ps4(Protocol, ReqType, Req, Res, CFR) ->
	chf_ps5(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ps5(nrf = Protocol, ReqType,
		#{"serviceRating" := ServiceRating} = Req,
		Res, CFR) ->
	CFR1 = case nrf_pdu_session_charging_info(ServiceRating) of
		undefined ->
			CFR;
		PduInfo ->
			CFR#{pDUSessionChargingInformation => PduInfo}
	end,
	chf_ps6(Protocol, ReqType, Req, Res, CFR1);
chf_ps5(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Service-Information' = [ServiceInfo]} = Req,
		Res, CFR) ->
	CFR1 = case diameter_pdu_session_charging_info(ServiceInfo) of
		undefined ->
			CFR;
		PduInfo ->
			CFR#{pDUSessionChargingInformation => PduInfo}
	end,
	chf_ps6(Protocol, ReqType, Req, Res, CFR1);
chf_ps5(radius = Protocol, ReqType, Req, Res, CFR) ->
	CFR1 = case radius_pdu_session_charging_info(Req) of
		undefined ->
			CFR;
		PduInfo ->
			CFR#{pDUSessionChargingInformation => PduInfo}
	end,
	chf_ps6(Protocol, ReqType, Req, Res, CFR1);
chf_ps5(Protocol, ReqType, Req, Res, CFR) ->
	chf_ps6(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ps6(_Protocol, _ReqType, _Req, _Res, CFR) ->
	CFR.

-spec chf_ims(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) -> CDR
	when
		TimeStamp :: timestamp(),
		Unique :: unique(),
		Protocol :: diameter | nrf | radius,
		ReqType :: acct_type(),
		Req :: #'3gpp_ro_CCR'{} | map() | undefined,
		Res :: #'3gpp_ro_CCA'{} | map() | undefined,
		Rated :: acct_rated(),
		CDR :: cdr().
%% @doc CODEC for IMS CHF-CDR.
%% @private
chf_ims(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) ->
	CFR = #{recordType => chargingFunctionRecord,
			recordingNetworkFunctionID => atom_to_binary(node()),
			recordOpeningTime => TimeStamp},
	CFR1 = chf_ims1(Protocol, ReqType, Req, Res, CFR),
	{TimeStamp, Unique, Protocol, CFR1, Rated}.
%% @hidden
chf_ims1(nrf = Protocol, ReqType,
		#{"ratingSessionId" := SessionId} = Req, Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => list_to_binary(SessionId)},
	chf_ims2(Protocol, ReqType, Req, Res, CFR1);
chf_ims1(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Session-Id' = SessionId} = Req,
		Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => SessionId},
	chf_ims2(Protocol, ReqType, Req, Res, CFR1);
chf_ims1(Protocol, ReqType, Req, Res, CFR) ->
	chf_ims2(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ims2(nrf = Protocol, ReqType,
		#{"nfConsumerIdentification" := NfInfo} = Req,
		Res, CFR) ->
	NFI = nrf_nf_info(NfInfo),
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_ims3(Protocol, ReqType, Req, Res, CFR1);
chf_ims2(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Origin-Host' = OriginHost} = Req,
		Res, CFR) ->
	NFI = #{networkFunctionFQDN => OriginHost},
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_ims3(Protocol, ReqType, Req, Res, CFR1);
chf_ims2(Protocol, ReqType, Req, Res, CFR) ->
	chf_ims3(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ims3(nrf = Protocol, ReqType,
		#{"serviceContextId" := Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_ims4(Protocol, ReqType, Req, Res, CFR1);
chf_ims3(nrf = Protocol, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Context} | _]} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_ims4(Protocol, ReqType, Req, Res, CFR1);
chf_ims3(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => Context},
	chf_ims4(Protocol, ReqType, Req, Res, CFR1);
chf_ims3(Protocol, ReqType, Req, Res, CFR) ->
	chf_ims4(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ims4(nrf = Protocol, ReqType,
		#{"subscriptionId" := SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = nrf_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_ims5(Protocol, ReqType, Req, Res, CFR1);
chf_ims4(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Subscription-Id' = SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = diameter_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_ims5(Protocol, ReqType, Req, Res, CFR1);
chf_ims4(Protocol, ReqType, Req, Res, CFR) ->
	chf_ims2(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_ims5(_Protocol, _ReqType, _Req, _Res, CFR) ->
	CFR.

-spec chf_sms(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) -> CDR
	when
		TimeStamp :: timestamp(),
		Unique :: unique(),
		Protocol :: diameter | nrf | radius,
		ReqType :: acct_type(),
		Req :: #'3gpp_ro_CCR'{} | map() | undefined,
		Res :: #'3gpp_ro_CCA'{} | map() | undefined,
		Rated :: acct_rated(),
		CDR :: cdr().
%% @doc CODEC for SMS CHF-CDR.
%% @private
chf_sms(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) ->
	CFR = #{recordType => chargingFunctionRecord,
			recordingNetworkFunctionID => atom_to_binary(node()),
			recordOpeningTime => TimeStamp},
	CFR1 = chf_sms1(Protocol, ReqType, Req, Res, CFR),
	{TimeStamp, Unique, Protocol, CFR1, Rated}.
%% @hidden
chf_sms1(nrf = Protocol, ReqType,
		#{"ratingSessionId" := SessionId} = Req, Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => list_to_binary(SessionId)},
	chf_sms2(Protocol, ReqType, Req, Res, CFR1);
chf_sms1(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Session-Id' = SessionId} = Req,
		Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => SessionId},
	chf_sms2(Protocol, ReqType, Req, Res, CFR1);
chf_sms1(Protocol, ReqType, Req, Res, CFR) ->
	chf_sms2(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_sms2(nrf = Protocol, ReqType,
		#{"nfConsumerIdentification" := NfInfo} = Req,
		Res, CFR) ->
	NFI = nrf_nf_info(NfInfo),
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_sms3(Protocol, ReqType, Req, Res, CFR1);
chf_sms2(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Origin-Host' = OriginHost} = Req,
		Res, CFR) ->
	NFI = #{networkFunctionFQDN => OriginHost},
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_sms3(Protocol, ReqType, Req, Res, CFR1);
chf_sms2(Protocol, ReqType, Req, Res, CFR) ->
	chf_sms3(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_sms3(nrf = Protocol, ReqType,
		#{"serviceContextId" := Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_sms4(Protocol, ReqType, Req, Res, CFR1);
chf_sms3(nrf = Protocol, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Context} | _]} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_sms4(Protocol, ReqType, Req, Res, CFR1);
chf_sms3(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => Context},
	chf_sms4(Protocol, ReqType, Req, Res, CFR1);
chf_sms3(Protocol, ReqType, Req, Res, CFR) ->
	chf_sms4(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_sms4(nrf = Protocol, ReqType,
		#{"subscriptionId" := SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = nrf_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_sms5(Protocol, ReqType, Req, Res, CFR1);
chf_sms4(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Subscription-Id' = SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = diameter_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_sms5(Protocol, ReqType, Req, Res, CFR1);
chf_sms4(Protocol, ReqType, Req, Res, CFR) ->
	chf_sms5(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_sms5(_Protocol, _ReqType, _Req, _Res, CFR) ->
	CFR.

-spec chf_vcs(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) -> CDR
	when
		TimeStamp :: timestamp(),
		Unique :: unique(),
		Protocol :: diameter | nrf | radius,
		ReqType :: acct_type(),
		Req :: #'3gpp_ro_CCR'{} | map() | undefined,
		Res :: #'3gpp_ro_CCA'{} | map() | undefined,
		Rated :: acct_rated(),
		CDR :: cdr().
%% @doc CODEC for VCS CHF-CDR.
%% @private
chf_vcs(TimeStamp, Unique, Protocol, ReqType, Req, Res, Rated) ->
	CFR = #{recordType => chargingFunctionRecord,
			recordingNetworkFunctionID => atom_to_binary(node()),
			recordOpeningTime => TimeStamp},
	CFR1 = chf_vcs1(Protocol, ReqType, Req, Res, CFR),
	{TimeStamp, Unique, Protocol, CFR1, Rated}.
%% @hidden
chf_vcs1(nrf = Protocol, ReqType,
		#{"ratingSessionId" := SessionId} = Req, Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => list_to_binary(SessionId)},
	chf_vcs2(Protocol, ReqType, Req, Res, CFR1);
chf_vcs1(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Session-Id' = SessionId} = Req, Res, CFR) ->
	CFR1 = CFR#{chargingSessionIdentifier => SessionId},
	chf_vcs2(Protocol, ReqType, Req, Res, CFR1);
chf_vcs1(Protocol, ReqType, Req, Res, CFR) ->
	chf_vcs2(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_vcs2(nrf = Protocol, ReqType,
		#{"nfConsumerIdentification" := NfInfo} = Req,
		Res, CFR) ->
	NFI = nrf_nf_info(NfInfo),
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_vcs3(Protocol, ReqType, Req, Res, CFR1);
chf_vcs2(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Origin-Host' = OriginHost} = Req,
		Res, CFR) ->
	NFI = #{networkFunctionFQDN => OriginHost},
	CFR1 = CFR#{nFunctionConsumerInformation => NFI},
	chf_vcs3(Protocol, ReqType, Req, Res, CFR1);
chf_vcs2(Protocol, ReqType, Req, Res, CFR) ->
	chf_vcs3(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_vcs3(nrf = Protocol, ReqType,
		#{"serviceContextId" := Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_vcs4(Protocol, ReqType, Req, Res, CFR1);
chf_vcs3(nrf = Protocol, ReqType,
		#{"serviceRating" := [#{"serviceContextId" := Context} | _]} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => list_to_binary(Context)},
	chf_vcs4(Protocol, ReqType, Req, Res, CFR1);
chf_vcs3(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Service-Context-Id' = Context} = Req,
		Res, CFR) ->
	CFR1 = CFR#{serviceSpecificationInformation => Context},
	chf_vcs4(Protocol, ReqType, Req, Res, CFR1);
chf_vcs3(Protocol, ReqType, Req, Res, CFR) ->
	chf_vcs4(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_vcs4(nrf = Protocol, ReqType,
		#{"subscriptionId" := SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = nrf_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_vcs5(Protocol, ReqType, Req, Res, CFR1);
chf_vcs4(diameter = Protocol, ReqType,
		#'3gpp_ro_CCR'{'Subscription-Id' = SubscriptionId} = Req,
		Res, CFR) ->
	SubscriptionId1 = diameter_subscription_id(SubscriptionId),
	CFR1 = CFR#{subscriberIdentifier => SubscriptionId1},
	chf_vcs5(Protocol, ReqType, Req, Res, CFR1);
chf_vcs4(Protocol, ReqType, Req, Res, CFR) ->
	chf_vcs5(Protocol, ReqType, Req, Res, CFR).
%% @hidden
chf_vcs5(_Protocol, _ReqType, _Req, _Res, CFR) ->
	CFR.

%% @hidden
nrf_nf_info(NfInfo) ->
	nrf_nf_info1(NfInfo, #{}).
%% @hidden
nrf_nf_info1(#{"nFIPv4Address" := Address} = NfInfo, Acc)
		when is_list(Address) ->
	case inet:parse_ipv4_address(Address) of
		{ok, IPv4Address} ->
			Acc1 = Acc#{networkFunctionIPv4Address => IPv4Address},
			nrf_nf_info2(NfInfo, Acc1);
		{error, einval} ->
			nrf_nf_info2(NfInfo, Acc)
	end;
nrf_nf_info1(NfInfo, Acc) ->
	nrf_nf_info2(NfInfo, Acc).
%% @hidden
nrf_nf_info2(#{"nFIPv6Address" := Address} = NfInfo, Acc)
		when is_list(Address) ->
	case inet:parse_ipv6_address(Address) of
		{ok, IPv4Address} ->
			Acc1 = Acc#{networkFunctionIPv6Address => IPv4Address},
			nrf_nf_info3(NfInfo, Acc1);
		{error, einval} ->
			nrf_nf_info3(NfInfo, Acc)
	end;
nrf_nf_info2(NfInfo, Acc) ->
	nrf_nf_info3(NfInfo, Acc).
%% @hidden
nrf_nf_info3(#{"nFPLMNID" := #{"mcc" := MCC, "mnc" := MNC}} = NfInfo, Acc)
		when is_list(MCC), is_list(MNC) ->
	PLMN = list_to_binary([MCC, MNC]),
	Acc1 = Acc#{networkFunctionPLMNIdentifier => PLMN},
	nrf_nf_info4(NfInfo, Acc1);
nrf_nf_info3(NfInfo, Acc) ->
	nrf_nf_info4(NfInfo, Acc).
%% @hidden
nrf_nf_info4(#{"nodeFunctionality" := "AMF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => aMF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" :=  "SMF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => sMF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "SMSF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => sMSF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "PGW_C_SMF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => pGWCSMF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "SGW"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => sGW},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "I_SMF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => iSMF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "ePDG"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => ePDG},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "CEF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => cEF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "NEF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => nEF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" :=  "MnS_Producer"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => 'mnS-Producer'},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "SGSN"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => sGSN},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "V_SMF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => vSMF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "5G_DDNMF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => fiveGDDNMF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" :=  "IMS_Node"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => 'iMS-Node'},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "EES"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => eES},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "PCF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => pCF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "UDM"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => uDM},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "UPF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => uPF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "CHF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => cHF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(#{"nodeFunctionality" := "OCF"} = NfInfo, Acc) ->
	Acc1 = Acc#{networkFunctionality => cHF},
	nrf_nf_info5(NfInfo, Acc1);
nrf_nf_info4(NfInfo, Acc) ->
	nrf_nf_info5(NfInfo, Acc).
%% @hidden
nrf_nf_info5(#{"nFName" := NfName} = NfInfo, Acc)
		when is_list(NfName) ->
	Acc1 = Acc#{networkFunctionName => list_to_binary(NfName)},
	nrf_nf_info6(NfInfo, Acc1);
nrf_nf_info5(NfInfo, Acc) ->
	nrf_nf_info6(NfInfo, Acc).
%% @hidden
nrf_nf_info6(#{"nFFqdn" := FQDN}, Acc)
		when is_list(FQDN) ->
	Acc#{networkFunctionFQDN => list_to_binary(FQDN)};
nrf_nf_info6(_NfInfo, Acc) ->
	Acc.

%% @hidden
nrf_subscription_id(SubscriptionId) ->
	nrf_subscription_id(SubscriptionId, []).
%% @hidden
nrf_subscription_id(["msisdn-" ++ MSISDN | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-E164',
			subscriptionIDData => list_to_binary(MSISDN)},
	nrf_subscription_id(T, [SubscriptionId | Acc]);
nrf_subscription_id(["imsi-" ++ IMSI | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-IMSI',
			subscriptionIDData =>  list_to_binary(IMSI)},
	nrf_subscription_id(T, [SubscriptionId | Acc]);
nrf_subscription_id(["nai-" ++ NAI | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-NAI',
			subscriptionIDData =>  list_to_binary(NAI)},
	nrf_subscription_id(T, [SubscriptionId | Acc]);
nrf_subscription_id(["iccid-" ++ ICCID | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-PRIVATE',
			subscriptionIDData => list_to_binary(ICCID)},
	nrf_subscription_id(T, [SubscriptionId | Acc]);
nrf_subscription_id([], Acc) ->
	lists:reverse(Acc).

%% @hidden
nrf_pdu_session_charging_info([#{"serviceInformation" := SI} | _])
	when is_map_key("pduSessionInformation", SI) ->
	nrf_pdu_session_charging_info1(SI, #{});
nrf_pdu_session_charging_info([#{"serviceInformation" := SI} | _])
	when is_map_key("sgsnMccMnc", SI) ->
	nrf_pdu_session_charging_info1(SI, #{});
nrf_pdu_session_charging_info([_SR | T]) ->
	nrf_pdu_session_charging_info(T);
nrf_pdu_session_charging_info([]) ->
	undefined.
%% @hidden
nrf_pdu_session_charging_info1(#{"chargingId" := CID} = SI, Acc) ->
	Acc1 = Acc#{pDUSessionChargingID => CID},
	nrf_pdu_session_charging_info2(SI, Acc1);
nrf_pdu_session_charging_info1(SI, Acc) ->
	nrf_pdu_session_charging_info2(SI, Acc).
%% @hidden
nrf_pdu_session_charging_info2(#{"sgsnMccMnc" := #{"mcc" := MCC, "mnc" := MNC}} = SI, Acc) ->
	SNFI = #{networkFunctionPLMNIdentifier => list_to_binary([MCC, MNC])}, 
	Acc1 = Acc#{servingNetworkFunctionInformation => SNFI},
	nrf_pdu_session_charging_info3(SI, Acc1);
nrf_pdu_session_charging_info2(SI, Acc) ->
	nrf_pdu_session_charging_info3(SI, Acc).
%% @hidden
nrf_pdu_session_charging_info3(#{"pdpAddress" := Address} = SI, Acc) ->
	Acc1 = case inet:parse_ipv4_address(Address) of
		{ok, Address1} ->
			Acc#{pDUAddress => #{pDUIPv4Address => Address1}};
		{error, einval} ->
			Acc
	end,
	nrf_pdu_session_charging_info4(SI, Acc1);
nrf_pdu_session_charging_info3(SI, Acc) ->
	nrf_pdu_session_charging_info4(SI, Acc).
%% @hidden
nrf_pdu_session_charging_info4(#{"chargingCharacteristics" := Chars} = SI, Acc) ->
	Acc1 = Acc#{chargingCharacteristics => list_to_binary(Chars)},
	nrf_pdu_session_charging_info5(SI, Acc1);
nrf_pdu_session_charging_info4(SI, Acc) ->
	nrf_pdu_session_charging_info5(SI, Acc).
%% @hidden
nrf_pdu_session_charging_info5(#{"ratType" := "NR"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 51},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(#{"ratType" := "EUTRA"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 6},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(#{"ratType" := "WLAN"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 3},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(#{"ratType" := "VIRTUAL"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 7},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(#{"ratType" := "NBIOT"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 8},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(#{"ratType" := "UTRA"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 1},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(#{"ratType" := "GERA"} = SI, Acc) ->
	Acc1 = Acc#{rATType => 2},
	nrf_pdu_session_charging_info6(SI, Acc1);
nrf_pdu_session_charging_info5(SI, Acc) ->
	nrf_pdu_session_charging_info6(SI, Acc).
%% @hidden
%nrf_pdu_session_charging_info6(#{"userInformation" := UI} = SI, Acc) ->
nrf_pdu_session_charging_info6(SI, Acc) ->
	nrf_pdu_session_charging_info7(SI, Acc).
%% @hidden
nrf_pdu_session_charging_info7(#{"userLocationinfo" := ULI} = SI, Acc) ->
	Acc1 = Acc#{userLocationInformation => nrf_user_location_info(ULI)},
	nrf_pdu_session_charging_info8(SI, Acc1);
nrf_pdu_session_charging_info7(SI, Acc) ->
	nrf_pdu_session_charging_info8(SI, Acc).
%% @hidden
nrf_pdu_session_charging_info8(#{"pduSessionInformation" := PSI}, Acc) ->
	nrf_pdu_session_info(PSI, Acc);
nrf_pdu_session_charging_info8(_SI, Acc) ->
	Acc.

%% @hidden
nrf_user_location_info(ULI) ->
	nrf_user_location_info1(ULI, #{}).
%% @hidden
nrf_user_location_info1(#{"nrLocation" := Location} = ULI, Acc) ->
	Acc1 = Acc#{nrLocation => nrf_ran_location(Location)},
	nrf_user_location_info2(ULI, Acc1);
nrf_user_location_info1(ULI, Acc) ->
	nrf_user_location_info2(ULI, Acc).
%% @hidden
nrf_user_location_info2(#{"eutraLocation" := Location} = ULI, Acc) ->
	Acc1 = Acc#{eutraLocation => nrf_ran_location(Location)},
	nrf_user_location_info3(ULI, Acc1);
nrf_user_location_info2(ULI, Acc) ->
	nrf_user_location_info3(ULI, Acc).
%% @hidden
nrf_user_location_info3(#{"utraLocation" := Location} = ULI, Acc) ->
	Acc1 = Acc#{utraLocation => nrf_ran_location(Location)},
	nrf_user_location_info4(ULI, Acc1);
nrf_user_location_info3(ULI, Acc) ->
	nrf_user_location_info4(ULI, Acc).
%% @hidden
nrf_user_location_info4(#{"n3gaLocation" := Location} = ULI, Acc) ->
	Acc1 = Acc#{n3gaLocation => nrf_ran_location(Location)},
	nrf_user_location_info5(ULI, Acc1);
nrf_user_location_info4(ULI, Acc) ->
	nrf_user_location_info5(ULI, Acc).
%% @hidden
nrf_user_location_info5(#{"geraLocation" := Location}, Acc) ->
	Acc#{geraLocation => nrf_ran_location(Location)};
nrf_user_location_info5(_ULI, Acc) ->
	Acc.

%% @hidden
nrf_ran_location(Location) ->
	nrf_ran_location1(Location, #{}).
%% @hidden
nrf_ran_location1(#{"tai" := TAI} = Location, Acc) ->
	Acc1 = Acc#{tai => nrf_tai(TAI)},
	nrf_ran_location2(Location, Acc1);
nrf_ran_location1(Location, Acc) ->
	nrf_ran_location2(Location, Acc).
%% @hidden
nrf_ran_location2(#{"sai" := SAI} = Location, Acc) ->
	Acc1 = Acc#{sai => nrf_sai(SAI)},
	nrf_ran_location3(Location, Acc1);
nrf_ran_location2(Location, Acc) ->
	nrf_ran_location3(Location, Acc).
%% @hidden
nrf_ran_location3(#{"rai" := RAI} = Location, Acc) ->
	Acc1 = Acc#{rai => nrf_rai(RAI)},
	nrf_ran_location4(Location, Acc1);
nrf_ran_location3(Location, Acc) ->
	nrf_ran_location4(Location, Acc).
%% @hidden
nrf_ran_location4(#{"ncgi" := NCGI} = Location, Acc) ->
	Acc1 = Acc#{ncgi => nrf_ncgi(NCGI)},
	nrf_ran_location5(Location, Acc1);
nrf_ran_location4(Location, Acc) ->
	nrf_ran_location5(Location, Acc).
%% @hidden
nrf_ran_location5(#{"ecgi" := ECGI} = Location, Acc) ->
	Acc1 = Acc#{ecgi => nrf_ecgi(ECGI)},
	nrf_ran_location6(Location, Acc1);
nrf_ran_location5(Location, Acc) ->
	nrf_ran_location6(Location, Acc).
%% @hidden
nrf_ran_location6(#{"cgi" := CGI} = Location, Acc) ->
	Acc1 = Acc#{cgi => nrf_cgi(CGI)},
	nrf_ran_location7(Location, Acc1);
nrf_ran_location6(Location, Acc) ->
	nrf_ran_location7(Location, Acc).
%% @hidden
nrf_ran_location7(#{"n3gppTai" := TAI}, Acc) ->
	Acc#{n3gppTai => nrf_tai(TAI)};
nrf_ran_location7(_Location, Acc) ->
	Acc.

%% @hidden
nrf_plmnid(#{"mcc" := MCC, "mnc" := MNC}) ->
	list_to_binary([MCC, MNC]).

%% @hidden
nrf_tai(TAI) ->
	nrf_tai1(TAI, #{}).
%% @hidden
nrf_tai1(#{"plmnId" := PLMN} = TAI, Acc) ->
	Acc1 = Acc#{plmnId => nrf_plmnid(PLMN)},
	nrf_tai2(TAI, Acc1).
%% @hidden
nrf_tai2(#{"tac" := TAC} = TAI, Acc) ->
	Acc1 = Acc#{tac => list_to_binary(TAC)},
	nrf_tai3(TAI, Acc1).
%% @hidden
nrf_tai3(#{"nid" := NID}, Acc) ->
	Acc#{nid => list_to_binary(NID)};
nrf_tai3(_TAI, Acc) ->
	Acc.

%% @hidden
nrf_sai(SAI) ->
	nrf_sai1(SAI, #{}).
%% @hidden
nrf_sai1(#{"plmnId" := PLMN} = SAI, Acc) ->
	Acc1 = Acc#{plmnId => nrf_plmnid(PLMN)},
	nrf_sai2(SAI, Acc1);
nrf_sai1(SAI, Acc) ->
	nrf_sai2(SAI, Acc).
%% @hidden
nrf_sai2(#{"lac" := LAC} = SAI, Acc) ->
	Acc1 = Acc#{lac => list_to_binary(LAC)},
	nrf_sai3(SAI, Acc1);
nrf_sai2(SAI, Acc) ->
	nrf_sai3(SAI, Acc).
%% @hidden
nrf_sai3(#{"sac" := SAC}, Acc) ->
	Acc#{sac => list_to_binary(SAC)};
nrf_sai3(_SAI, Acc) ->
	Acc.

%% @hidden
nrf_rai(RAI) ->
	nrf_rai1(RAI, #{}).
%% @hidden
nrf_rai1(#{"plmnId" := PLMN} = RAI, Acc) ->
	Acc1 = Acc#{plmnId => nrf_plmnid(PLMN)},
	nrf_rai2(RAI, Acc1);
nrf_rai1(RAI, Acc) ->
	nrf_rai2(RAI, Acc).
%% @hidden
nrf_rai2(#{"lac" := LAC} = RAI, Acc) ->
	Acc1 = Acc#{lac => list_to_binary(LAC)},
	nrf_rai3(RAI, Acc1);
nrf_rai2(RAI, Acc) ->
	nrf_rai3(RAI, Acc).
%% @hidden
nrf_rai3(#{"rac" := RAC}, Acc) ->
	Acc#{rac => list_to_binary(RAC)};
nrf_rai3(_RAI, Acc) ->
	Acc.

%% @hidden
nrf_ncgi(NCGI) ->
	nrf_ncgi1(NCGI, #{}).
%% @hidden
nrf_ncgi1(#{"plmnId" := PLMN} = NCGI, Acc) ->
	Acc1 = Acc#{plmnId => nrf_plmnid(PLMN)},
	nrf_ncgi2(NCGI, Acc1);
nrf_ncgi1(NCGI, Acc) ->
	nrf_ncgi2(NCGI, Acc).
%% @hidden
nrf_ncgi2(#{"nrCellId" := CID} = NCGI, Acc) ->
	Acc1 = Acc#{nrCellId => CID},
	nrf_ncgi3(NCGI, Acc1);
nrf_ncgi2(NCGI, Acc) ->
	nrf_ncgi3(NCGI, Acc).
%% @hidden
nrf_ncgi3(#{"nid" := NID}, Acc) ->
	Acc#{nid => list_to_binary(NID)};
nrf_ncgi3(_NCGI, Acc) ->
	Acc.

%% @hidden
nrf_ecgi(ECGI) ->
	nrf_ecgi1(ECGI, #{}).
%% @hidden
nrf_ecgi1(#{"plmnId" := PLMN} = ECGI, Acc) ->
	Acc1 = Acc#{plmnId => nrf_plmnid(PLMN)},
	nrf_ecgi2(ECGI, Acc1);
nrf_ecgi1(ECGI, Acc) ->
	nrf_ecgi2(ECGI, Acc).
%% @hidden
nrf_ecgi2(#{"eutraCellId" := CID} = ECGI, Acc) ->
	Acc1 = Acc#{eutraCellId => CID},
	nrf_ecgi3(ECGI, Acc1);
nrf_ecgi2(ECGI, Acc) ->
	nrf_ecgi3(ECGI, Acc).
%% @hidden
nrf_ecgi3(#{"nid" := NID}, Acc) ->
	Acc#{nid => list_to_binary(NID)};
nrf_ecgi3(_ECGI, Acc) ->
	Acc.

%% @hidden
nrf_cgi(CGI) ->
	nrf_cgi1(CGI, #{}).
%% @hidden
nrf_cgi1(#{"plmnId" := PLMN} = CGI, Acc) ->
	Acc1 = Acc#{plmnId => nrf_plmnid(PLMN)},
	nrf_cgi2(CGI, Acc1);
nrf_cgi1(CGI, Acc) ->
	nrf_cgi2(CGI, Acc).
%% @hidden
nrf_cgi2(#{"utraCellId" := CID} = CGI, Acc) ->
	Acc1 = Acc#{utraCellId => CID},
	nrf_cgi3(CGI, Acc1);
nrf_cgi2(CGI, Acc) ->
	nrf_cgi3(CGI, Acc).
%% @hidden
nrf_cgi3(#{"lac" := LAC}, Acc) ->
	Acc#{lac => list_to_binary(LAC)};
nrf_cgi3(_CGI, Acc) ->
	Acc.

%% @hidden
nrf_pdu_session_info(#{"pduSessionID" := SessionId} = PSI, Acc)
		when is_integer(SessionId) ->
	Acc1 = Acc#{pDUSessionId => SessionId},
	nrf_pdu_session_info1(PSI, Acc1).
%% @hidden
nrf_pdu_session_info1(#{"dnnId" := DNN} = PSI, Acc) ->
	Acc1 = Acc#{dataNetworkNameIdentifier => list_to_binary(DNN)},
	nrf_pdu_session_info2(PSI, Acc1).
%% @hidden
nrf_pdu_session_info2(#{"pduType" := "IPV4"} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => iPv4},
	nrf_pdu_session_info3(PSI, Acc1);
nrf_pdu_session_info2(#{"pduType" := "IPV6"} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => iPv6},
	nrf_pdu_session_info3(PSI, Acc1);
nrf_pdu_session_info2(#{"pduType" := "IPV4V6"} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => iPv4v6},
	nrf_pdu_session_info3(PSI, Acc1);
nrf_pdu_session_info2(#{"pduType" := "UNSTRUCTURED"} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => unstructured},
	nrf_pdu_session_info3(PSI, Acc1);
nrf_pdu_session_info2(#{"pduType" := "ETHERNET"} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => ethernet},
	nrf_pdu_session_info3(PSI, Acc1);
nrf_pdu_session_info2(PSI, Acc) ->
	nrf_pdu_session_info3(PSI, Acc).
%% @hidden
nrf_pdu_session_info3(#{"networkSlicingInfo" := NSI} = PSI, Acc) ->
	Acc1 = Acc#{networkSliceInstanceID => nrf_network_slice_info(NSI)},
	nrf_pdu_session_info4(PSI, Acc1);
nrf_pdu_session_info3(PSI, Acc) ->
	nrf_pdu_session_info4(PSI, Acc).
%% @hidden
nrf_pdu_session_info4(#{"servingNetworkFunctionID" := SNFI} = PSI, Acc) ->
	Acc1 = Acc#{servingNetworkFunctionID => [nrf_serving_nf_id(SNFI)]},
	nrf_pdu_session_info5(PSI, Acc1);
nrf_pdu_session_info4(PSI, Acc) ->
	nrf_pdu_session_info5(PSI, Acc).
%% @hidden
nrf_pdu_session_info5(#{"ratType" := "NR"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 51},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(#{"ratType" := "EUTRA"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 6},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(#{"ratType" := "WLAN"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 3},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(#{"ratType" := "VIRTUAL"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 7},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(#{"ratType" := "NBIOT"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 8},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(#{"ratType" := "UTRA"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 1},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(#{"ratType" := "GERA"} = PSI, Acc) ->
	Acc1 = Acc#{rATType => 2},
	nrf_pdu_session_info6(PSI, Acc1);
nrf_pdu_session_info5(PSI, Acc) ->
	nrf_pdu_session_info6(PSI, Acc).
%% @hidden
nrf_pdu_session_info6(#{"chargingCharacteristics" := Chars} = PSI, Acc) ->
	Acc1 = Acc#{chargingCharacteristics => list_to_binary(Chars)},
	nrf_pdu_session_info7(PSI, Acc1);
nrf_pdu_session_info6(PSI, Acc) ->
	nrf_pdu_session_info7(PSI, Acc).
%% @hidden
nrf_pdu_session_info7(#{"startTime" := StartTime} = PSI, Acc)
		when is_list(StartTime) ->
	Acc1 = Acc#{pDUSessionstartTime => iso8601(StartTime)},
	nrf_pdu_session_info8(PSI, Acc1);
nrf_pdu_session_info7(PSI, Acc) ->
	nrf_pdu_session_info8(PSI, Acc).
%% @hidden
nrf_pdu_session_info8(#{"stopTime" := StopTime} = PSI, Acc)
		when is_list(StopTime) ->
	Acc1 = Acc#{pDUSessionstopTime => iso8601(StopTime)},
	nrf_pdu_session_info9(PSI, Acc1);
nrf_pdu_session_info8(PSI, Acc) ->
	nrf_pdu_session_info9(PSI, Acc).
%% @hidden
nrf_pdu_session_info9(#{"pduAddress" := PduAddress} = PSI, Acc) ->
	Acc1 = Acc#{pDUAddress => nrf_pdu_address(PduAddress)},
	nrf_pdu_session_info10(PSI, Acc1);
nrf_pdu_session_info9(PSI, Acc) ->
	nrf_pdu_session_info10(PSI, Acc).
%% @hidden
%nrf_pdu_session_info10(#{"subscribedQoSInformation" := QoS} = PSI, Acc) ->
nrf_pdu_session_info10(PSI, Acc) ->
	nrf_pdu_session_info11(PSI, Acc).
%% @hidden
%nrf_pdu_session_info11(#{"authorizedSessionAMBR" := AMBR} = PSI, Acc) ->
nrf_pdu_session_info11(PSI, Acc) ->
	nrf_pdu_session_info12(PSI, Acc).
%% @hidden
%nrf_pdu_session_info12(#{"subscribedSessionAMBR" := AMBR} = PSI, Acc) ->
nrf_pdu_session_info12(PSI, Acc) ->
	nrf_pdu_session_info13(PSI, Acc).
%% @hidden
%nrf_pdu_session_info13(#{"servingCNPlmnId" := PLMN} = PSI, Acc) ->
nrf_pdu_session_info13(_PSI, Acc) ->
	Acc.

%% @hidden
nrf_network_slice_info(#{"sNSSAI" := SNSSAI}) ->
	nrf_network_slice_info1(SNSSAI, #{}).
%% @hidden
nrf_network_slice_info1(#{"sst" := SST} = SNSSAI, Acc)
		when is_integer(SST) ->
	Acc1 = Acc#{sST => SST},
	nrf_network_slice_info2(SNSSAI, Acc1).
%% @hidden
nrf_network_slice_info2(#{"sd" := SD}, Acc)
		when is_list(SD) ->
	Acc#{sD => list_to_binary(SD)};
nrf_network_slice_info2(_SNSSAI, Acc) ->
	Acc.

%% @hidden
nrf_serving_nf_id(#{"servingNetworkFunctionInformation" := SNFI} = SNFID) ->
	Acc = #{servingNetworkFunctionInformation => nrf_nf_info(SNFI)},
	nrf_serving_nf_id1(SNFID, Acc).
%% @hidden
nrf_serving_nf_id1(#{"aMFId" := AMFID} = _SNFID, Acc)
		when is_list(AMFID) ->
	Acc#{aMFIdentifier => list_to_binary(AMFID)};
nrf_serving_nf_id1(_SNFID, Acc) ->
	Acc.

%% @hidden
nrf_pdu_address(PDUA) ->
	nrf_pdu_address1(PDUA, #{}).
%% @hidden
nrf_pdu_address1(#{"pduIPv4Address" := Address} = PDUA, Acc) ->
	Acc1 = case inet:parse_ipv4_address(Address) of
		{ok, Address1} ->
			Acc#{pDUIPv4Address => Address1};
		{error, einval} ->
			Acc
	end,
	nrf_pdu_address2(PDUA, Acc1);
nrf_pdu_address1(PDUA, Acc) ->
	nrf_pdu_address2(PDUA, Acc).
%% @hidden
nrf_pdu_address2(#{"pduIPv6AddresswithPrefix" := Address} = PDUA, Acc) ->
	Acc1 = case inet:parse_ipv6_address(Address) of
		{ok, Address1} ->
			Acc#{pDUIPv6Address => Address1};
		{error, einval} ->
			Acc
	end,
	nrf_pdu_address3(PDUA, Acc1);
nrf_pdu_address2(PDUA, Acc) ->
	nrf_pdu_address3(PDUA, Acc).
%% @hidden
nrf_pdu_address3(#{"iPv4dynamicAddressFlag" := Flag} = PDUA, Acc) ->
	Acc1 = Acc#{iPV4dynamicAddressFlag => Flag},
	nrf_pdu_address4(PDUA, Acc1);
nrf_pdu_address3(PDUA, Acc) ->
	nrf_pdu_address4(PDUA, Acc).
%% @hidden
nrf_pdu_address4(#{"iPv6dynamicPrefixFlag" := Flag} = PDUA, Acc) ->
	Acc1 = Acc#{iPV6dynamicPrefixFlag => Flag},
	nrf_pdu_address5(PDUA, Acc1);
nrf_pdu_address4(PDUA, Acc) ->
	nrf_pdu_address5(PDUA, Acc).
%% @hidden
nrf_pdu_address5(#{"addIpv6AddrPrefixes" := Address} = PDUA, Acc) ->
	Acc1 = case inet:parse_ipv6_address(Address) of
		{ok, Address1} ->
			Acc#{additionalPDUIPv6Prefixes => [Address1]};
		{error, einval} ->
			Acc
	end,
	nrf_pdu_address6(PDUA, Acc1);
nrf_pdu_address5(PDUA, Acc) ->
	nrf_pdu_address6(PDUA, Acc).
%% @hidden
nrf_pdu_address6(#{"addIpv6AddrPrefixList" := Addresses}, Acc) ->
	F = fun({ok, Address}, Acc1) ->
				[Address | Acc1];
			({error, einval}, Acc1) ->
				Acc1
	end,
	Addresses1 = lists:foldr(F, [], Addresses),
	Acc#{additionalPDUIPv6Prefixes => Addresses1};
nrf_pdu_address6(_PDUA, Acc) ->
	Acc.

%% @hidden
diameter_subscription_id(SubscriptionId) ->
	diameter_subscription_id(SubscriptionId, []).
%% @hidden
diameter_subscription_id([#'3gpp_ro_Subscription-Id'{
		'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
		'Subscription-Id-Data' = MSISDN} | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-E164',
			subscriptionIDData => MSISDN},
	diameter_subscription_id(T, [SubscriptionId | Acc]);
diameter_subscription_id([#'3gpp_ro_Subscription-Id'{
		'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
		'Subscription-Id-Data' = IMSI} | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-IMSI',
			subscriptionIDData => IMSI},
	diameter_subscription_id(T, [SubscriptionId | Acc]);
diameter_subscription_id([#'3gpp_ro_Subscription-Id'{
		'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_SIP_URI',
		'Subscription-Id-Data' = SIPURI} | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-SIP-URI',
			subscriptionIDData => SIPURI},
	diameter_subscription_id(T, [SubscriptionId | Acc]);
diameter_subscription_id([#'3gpp_ro_Subscription-Id'{
		'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_NAI',
		'Subscription-Id-Data' = NAI}  | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-NAI',
			subscriptionIDData => NAI},
	diameter_subscription_id(T, [SubscriptionId | Acc]);
diameter_subscription_id([#'3gpp_ro_Subscription-Id'{
		'Subscription-Id-Type' = ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_PRIVATE',
		'Subscription-Id-Data' = PRIVATE}  | T], Acc) ->
	SubscriptionId = #{subscriptionIDType => 'eND-USER-PRIVATE',
			subscriptionIDData => PRIVATE},
	diameter_subscription_id(T, [SubscriptionId | Acc]);
diameter_subscription_id([], Acc) ->
	lists:reverse(Acc).

%% @hidden
diameter_pdu_session_charging_info(#'3gpp_ro_Service-Information'{
		'PS-Information' = [PSI]}) ->
	diameter_pdu_session_charging_info1(PSI, #{});
diameter_pdu_session_charging_info(_) ->
	undefined.
%% @hidden
diameter_pdu_session_charging_info1(#'3gpp_ro_PS-Information'{
		'PDN-Connection-Charging-ID' = [ChargingId]} = PSI, Acc) ->
	Acc1 = Acc#{pDUSessionChargingID => ChargingId},
	diameter_pdu_session_charging_info2(PSI, Acc1);
diameter_pdu_session_charging_info1(#'3gpp_ro_PS-Information'{
		'3GPP-Charging-Id' = [<<ChargingId:32>>]} = PSI, Acc) ->
	Acc1 = Acc#{pDUSessionChargingID => ChargingId},
	diameter_pdu_session_charging_info2(PSI, Acc1);
diameter_pdu_session_charging_info1(PSI, Acc) ->
	diameter_pdu_session_charging_info2(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info2(#'3gpp_ro_PS-Information'{
		'3GPP-SGSN-MCC-MNC' = [MccMnc]} = PSI, Acc) ->
	SNFI = #{networkFunctionality => sGSN,
			networkFunctionPLMNIdentifier => MccMnc}, 
	Acc1 = Acc#{servingNetworkFunctionInformation => SNFI},
	diameter_pdu_session_charging_info3(PSI, Acc1);
diameter_pdu_session_charging_info2(PSI, Acc) ->
	diameter_pdu_session_charging_info3(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info3(#'3gpp_ro_PS-Information'{
		'3GPP-User-Location-Info' = [ULI]} = PSI, Acc) ->
	Acc1 = Acc#{userLocationInformation => diameter_user_location_info(ULI)},
	diameter_pdu_session_charging_info4(PSI, Acc1);
diameter_pdu_session_charging_info3(PSI, Acc) ->
	diameter_pdu_session_charging_info4(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info4(#'3gpp_ro_PS-Information'{
		'3GPP-PDP-Type' = [0]} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => iPv4},
	diameter_pdu_session_charging_info5(PSI, Acc1);
diameter_pdu_session_charging_info4(#'3gpp_ro_PS-Information'{
		'3GPP-PDP-Type' = [2]} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => iPv6},
	diameter_pdu_session_charging_info5(PSI, Acc1);
diameter_pdu_session_charging_info4(#'3gpp_ro_PS-Information'{
		'3GPP-PDP-Type' = [3]} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => iPv4v6},
	diameter_pdu_session_charging_info5(PSI, Acc1);
diameter_pdu_session_charging_info4(#'3gpp_ro_PS-Information'{
		'3GPP-PDP-Type' = [5]} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => unstructured},
	diameter_pdu_session_charging_info5(PSI, Acc1);
diameter_pdu_session_charging_info4(#'3gpp_ro_PS-Information'{
		'3GPP-PDP-Type' = [6]} = PSI, Acc) ->
	Acc1 = Acc#{pDUType => ethernet},
	diameter_pdu_session_charging_info5(PSI, Acc1);
diameter_pdu_session_charging_info4(PSI, Acc) ->
	diameter_pdu_session_charging_info5(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info5(#'3gpp_ro_PS-Information'{
		'3GPP-RAT-Type' = [RAT]} = PSI, Acc) ->
	Acc1 = Acc#{rATType => RAT},
	diameter_pdu_session_charging_info6(PSI, Acc1);
diameter_pdu_session_charging_info5(PSI, Acc) ->
	diameter_pdu_session_charging_info6(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info6(#'3gpp_ro_PS-Information'{
		'3GPP-Charging-Characteristics' = [Chars]} = PSI, Acc) ->
	Acc1 = Acc#{chargingCharacteristics => Chars},
	diameter_pdu_session_charging_info7(PSI, Acc1);
diameter_pdu_session_charging_info6(PSI, Acc) ->
	diameter_pdu_session_charging_info7(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info7(#'3gpp_ro_PS-Information'{
		'Start-Time' = [StartTime]} = PSI, Acc)
		when is_tuple(StartTime) ->
	Acc1 = Acc#{pDUSessionstartTime => date(StartTime)},
	diameter_pdu_session_charging_info8(PSI, Acc1);
diameter_pdu_session_charging_info7(PSI, Acc) ->
	diameter_pdu_session_charging_info8(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info8(#'3gpp_ro_PS-Information'{
		'Stop-Time' = [StopTime]} = PSI, Acc)
		when is_tuple(StopTime) ->
	Acc1 = Acc#{pDUSessionstopTime => date(StopTime)},
	diameter_pdu_session_charging_info9(PSI, Acc1);
diameter_pdu_session_charging_info8(PSI, Acc) ->
	diameter_pdu_session_charging_info9(PSI, Acc).
%% @hidden
diameter_pdu_session_charging_info9(#'3gpp_ro_PS-Information'{
		'PDP-Address' = [Address]} = _PSI, Acc) ->
	Acc#{pDUAddress => #{pDUIPv4Address => Address}};
diameter_pdu_session_charging_info9(_PSI, Acc) ->
	Acc.

%% @hidden
% CGI (3GPP TS 29.274 8.21.1)
diameter_user_location_info(<<0, MCCMNC:3/binary, LAC:16, CI:16>>) ->
	CGI = #{plmnId => tbcd(MCCMNC),
			lac => list_to_binary(io_lib:fwrite("~4.16.0b", [LAC])),
			utraCellId => list_to_binary(io_lib:fwrite("~4.16.0b", [CI]))},
	#{utraLocation => #{cgi => CGI}};
% SAI (3GPP TS 29.274 8.21.2)
diameter_user_location_info(<<1, MCCMNC:3/binary, LAC:16, SAC:16>>) ->
	SAI = #{plmnId => tbcd(MCCMNC),
			lac => list_to_binary(io_lib:fwrite("~4.16.0b", [LAC])),
			sac => list_to_binary(io_lib:fwrite("~4.16.0b", [SAC]))},
	#{utraLocation => #{sai => SAI}};
% RAI (3GPP TS 29.274 8.21.3)
diameter_user_location_info(<<2, MCCMNC:3/binary, LAC:16, RAC:16>>) ->
	RAI = #{plmnId => tbcd(MCCMNC),
			lac => list_to_binary(io_lib:fwrite("~4.16.0b", [LAC])),
			rac => list_to_binary(io_lib:fwrite("~4.16.0b", [RAC]))},
	#{utraLocation => #{rai => RAI}};
% TAI (3GPP TS 29.274 8.21.4)
diameter_user_location_info(<<128, MCCMNC:3/binary, TAC:16>>) ->
	TAI = #{plmnId => tbcd(MCCMNC),
			tac => list_to_binary(io_lib:fwrite("~4.16.0b", [TAC]))},
	#{eutraLocation => #{tai => TAI}};
% ECGI (3GPP TS 29.274 8.21.5)
diameter_user_location_info(<<129, MCCMNC:3/binary, _:4, ECI:28>>) ->
	ECGI = #{plmnId => tbcd(MCCMNC),
			eutraCellId => list_to_binary(io_lib:fwrite("~7.16.0b", [ECI]))},
	#{eutraLocation => #{ecgi => ECGI}};
% TAI and ECGI
diameter_user_location_info(<<130, MCCMNC1:3/binary, TAC:16, MCCMNC2:3/binary,
		_:4, ECI:28>>) ->
	TAI = #{plmnId => tbcd(MCCMNC1),
			tac => list_to_binary(io_lib:fwrite("~4.16.0b", [TAC]))},
	ECGI = #{plmnId => tbcd(MCCMNC2),
			eutraCellId => list_to_binary(io_lib:fwrite("~7.16.0b", [ECI]))},
	#{eutraLocation => #{tai => TAI, ecgi => ECGI}};
% Macro eNodeB ID (3GPP TS 29.274 8.21.7)
diameter_user_location_info(<<131, MCCMNC:3/binary, _:4, ENBID:20>>) ->
	ECGI = #{plmnId => tbcd(MCCMNC),
			globalENbId => list_to_binary(io_lib:fwrite("~5.16.0b", [ENBID]))},
	#{eutraLocation => #{ecgi => ECGI}};
% TAI and eNodeB ID
diameter_user_location_info(<<132, MCCMNC1:3/binary, TAC:16, MCCMNC2:3/binary,
		_:4, ENBID:20>>) ->
	TAI = #{plmnId => tbcd(MCCMNC1),
			tac => list_to_binary(io_lib:fwrite("~4.16.0b", [TAC]))},
	ECGI = #{plmnId => tbcd(MCCMNC2),
			globalENbId => list_to_binary(io_lib:fwrite("~5.16.0b", [ENBID]))},
	#{eutraLocation => #{tai => TAI, ecgi => ECGI}};
% Extended long macro eNodeB ID (3GPP TS 29.274 8.21.8)
diameter_user_location_info(<<133, MCCMNC:3/binary, 0:1, _:2, ENBID:21>>) ->
	ECGI = #{plmnId => tbcd(MCCMNC),
			globalENbId => list_to_binary(io_lib:fwrite("~6.16.0b", [ENBID]))},
	#{eutraLocation => #{ecgi => ECGI}};
% Extended short macro eNodeB ID (3GPP TS 29.274 8.21.8)
diameter_user_location_info(<<133, MCCMNC:3/binary, 1:1, _:5, ENBID:18>>) ->
	ECGI = #{plmnId => tbcd(MCCMNC),
			globalENbId => list_to_binary(io_lib:fwrite("~5.16.0b", [ENBID]))},
	#{eutraLocation => #{ecgi => ECGI}};
% TAI and extended long macro eNodeB ID
diameter_user_location_info(<<134, MCCMNC1:3/binary, TAC:16, MCCMNC2:3/binary,
		0:1, _:2, ENBID:21>>) ->
	TAI = #{plmnId => tbcd(MCCMNC1),
			tac => list_to_binary(io_lib:fwrite("~4.16.0b", [TAC]))},
	ECGI = #{plmnId => tbcd(MCCMNC2),
			globalENbId => list_to_binary(io_lib:fwrite("~6.16.0b", [ENBID]))},
	#{eutraLocation => #{tai => TAI, ecgi => ECGI}};
% TAI and extended short macro eNodeB ID
diameter_user_location_info(<<134, MCCMNC1:3/binary, TAC:16, MCCMNC2:3/binary,
		1:1, _:5, ENBID:18>>) ->
	TAI = #{plmnId => tbcd(MCCMNC1),
			tac => list_to_binary(io_lib:fwrite("~4.16.0b", [TAC]))},
	ECGI = #{plmnId => tbcd(MCCMNC2),
			globalENbId => list_to_binary(io_lib:fwrite("~5.16.0b", [ENBID]))},
	#{eutraLocation => #{tai => TAI, ecgi => ECGI}}.

%% @hidden
tbcd(<<MCC2:4, MCC1:4, 15:4, MCC3:4, MNC2:4, MNC1:4>>) ->
	MCC = tbcd(<<MCC2:4, MCC1:4, 15:4, MCC3:4>>, []),
	MNC = tbcd(<<MNC2:4, MNC1:4>>, []),
	list_to_binary([MCC, MNC]);
tbcd(<<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>) ->
	MCC = tbcd(<<MCC2:4, MCC1:4, 15:4, MCC3:4>>, []),
	MNC = tbcd(<<MNC2:4, MNC1:4, 15:4, MNC3:4>>, []),
	list_to_binary([MCC, MNC]).
%% @hidden
tbcd(<<15:4, A:4>>, Acc) ->
	lists:reverse([A + 48 | Acc]);
tbcd(<<A2:4, A1:4, Rest/binary>>, Acc)
		when A1 >= 0, A1 < 10, A2 >= 0, A2 < 10 ->
	tbcd(Rest, [A2 + 48, A1 + 48 | Acc]);
tbcd(<<>>, Acc) ->
	lists:reverse(Acc).

%% @hidden
radius_pdu_session_charging_info(Attr) ->
	case radius_attributes:find(?ServiceType, Attr) of
		{ok, 2} ->
			radius_pdu_session_charging_info1(Attr, #{});
		{error, not_found} ->
			undefined
	end.
%% @hidden
radius_pdu_session_charging_info1(Attr, Acc) ->
	case radius_attributes:find(?'3GPP', ?'3GPP-Charging-ID', Attr) of
		{ok, ChargingId} ->
			Acc1 = Acc#{pDUSessionChargingID => ChargingId},
			radius_pdu_session_charging_info2(Attr, Acc1);
		{error, not_found} ->
			radius_pdu_session_charging_info2(Attr, Acc)
	end.
%% @hidden
radius_pdu_session_charging_info2(Attr, Acc) ->
	case radius_attributes:find(?'3GPP', ?'3GPP-PDP-Type', Attr) of
		{ok, 0} ->
			Acc1 = Acc#{pDUType => iPv4},
			radius_pdu_session_charging_info3(Attr, Acc1);
		{ok, 2} ->
			Acc1 = Acc#{pDUType => iPv6},
			radius_pdu_session_charging_info3(Attr, Acc1);
		{ok, 3} ->
			Acc1 = Acc#{pDUType => iPv4v6},
			radius_pdu_session_charging_info3(Attr, Acc1);
		{ok, 5} ->
			Acc1 = Acc#{pDUType => unstructured},
			radius_pdu_session_charging_info3(Attr, Acc1);
		{ok, 6} ->
			Acc1 = Acc#{pDUType => ethernet},
			radius_pdu_session_charging_info3(Attr, Acc1);
		{error, not_found} ->
			radius_pdu_session_charging_info3(Attr, Acc)
	end.
%% @hidden
radius_pdu_session_charging_info3(Attr, Acc) ->
	case radius_attributes:find(?'3GPP', ?'3GPP-RAT-Type', Attr) of
		{ok, RAT} ->
			Acc1 = Acc#{rATType => RAT},
			radius_pdu_session_charging_info4(Attr, Acc1);
		{error, not_found} ->
			radius_pdu_session_charging_info4(Attr, Acc)
	end.
%% @hidden
radius_pdu_session_charging_info4(Attr, Acc) ->
	case radius_attributes:find(?FramedIpAddress, Attr) of
		{ok, Address} ->
			Acc#{pDUAddress => #{pDUIPv4Address => Address}};
		{error, not_found} ->
			Acc
	end.

%% @hidden
chf_csv_header(Log, IoDevice, Seperator) ->
	Columns = [<<"Invocation Timestamp">>,
			<<"NF Name">>, <<"NF Address">>,
			<<"Charging Session Identifier">>,
			<<"Service Context Identifier">>,
			<<"IMSI">>, <<"MSISDN">>, <<"NAI">>,
			<<"Serving PLMN">>, <<"Cell Identifier">>,
			<<"RAT">>, <<"PDU Address">>,
			<<"Origin">>, <<"Destination">>, <<"Cause">>,
			<<"Rated Units">>, <<"Rated Amount">>,
			<<"Rated Cost">>, <<"Included">>,
			<<"Offer Name">>, <<"Price Name">>,
			<<"Price Type">>], 
	Header = [hd(Columns) | [[Seperator, C] || C <- tl(Columns)]],
	case file:write(IoDevice, [Header, $\r, $\n]) of
		ok ->
			cdr_file3(Log, IoDevice, csv, disk_log:chunk(Log, start));
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end.

%% @hidden
chf_csv(Log, IoDevice, Seperator,
		{Cont, [{TS, _, _, CFR, Rated} | T]}) ->
	Timestamp = ocs_rest:iso8601(TS),
	Columns1 = chf_cfr_csv(CFR, [Timestamp]),
	Columns2 = chf_rated_csv(Rated),
	Columns = Columns1 ++ Columns2,
	Row = [hd(Columns) | [[Seperator, C] || C <- tl(Columns)]],
	case file:write(IoDevice, [Row, $\r, $\n]) of
		ok ->
			chf_csv(Log, IoDevice, Seperator, {Cont, T});
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			disk_log:close(Log),
			{error, Reason}
	end;
chf_csv(Log, IoDevice, _Seperator, {Cont, []}) ->
	cdr_file3(Log, IoDevice, csv, {Cont, []}).

%% @hidden
chf_cfr_csv(#{nFunctionConsumerInformation := NfInfo} = CFR, Acc) ->
	NfName = nf_info_name(NfInfo),
	NfAddress = nf_info_address(NfInfo),
	chf_cfr_csv1(CFR, [NfAddress, NfName | Acc]);
chf_cfr_csv(CFR, Acc) ->
	chf_cfr_csv1(CFR, [<<>>, <<>> | Acc]).
%% @hidden
chf_cfr_csv1(#{chargingSessionIdentifier := SessionId} = CFR, Acc) ->
	chf_cfr_csv2(CFR, [SessionId | Acc]);
chf_cfr_csv1(CFR, Acc) ->
	chf_cfr_csv2(CFR, [<<>> | Acc]).
%% @hidden
chf_cfr_csv2(#{serviceSpecificationInformation := Context} = CFR, Acc) ->
	chf_cfr_csv3(CFR, [Context | Acc]);
chf_cfr_csv2(CFR, Acc) ->
	chf_cfr_csv3(CFR, [<<>> | Acc]).
%% @hidden
chf_cfr_csv3(#{subscriberIdentifier := SubscriberId} = CFR, Acc) ->
	{IMSI, MSISDN, NAI} = csv_subscriber(SubscriberId),
	chf_cfr_csv4(CFR, [NAI, MSISDN, IMSI| Acc]).
%% @hidden
chf_cfr_csv4(#{iMSChargingInformation := IMS} = CFR, Acc)
		when is_map_key(accessNetworkInformation, IMS) ->
	{PLMN, Cell} = csv_access_network_info(IMS),
	chf_cfr_csv5(CFR, [Cell, PLMN | Acc]);
chf_cfr_csv4(#{sMSChargingInformation := SMS} = CFR, Acc) ->
	{PLMN, Cell} = csv_user_location(SMS),
	chf_cfr_csv5(CFR, [Cell, PLMN | Acc]);
chf_cfr_csv4(#{pDUSessionChargingInformation := PS} = CFR, Acc) ->
	{PLMN, Cell} = csv_user_location(PS),
	chf_cfr_csv5(CFR, [Cell, PLMN | Acc]);
chf_cfr_csv4(#{iMSChargingInformation := IMS} = CFR, Acc) ->
	{PLMN, Cell} = csv_user_location(IMS),
	chf_cfr_csv5(CFR, [Cell, PLMN | Acc]);
chf_cfr_csv4(CFR, Acc) ->
	chf_cfr_csv5(CFR, [<<>>, <<>> | Acc]).
%% @hidden
chf_cfr_csv5(#{pDUSessionChargingInformation := #{rATType := RAT}} = CFR, Acc) ->
	chf_cfr_csv6(CFR, [integer_to_binary(RAT) | Acc]);
chf_cfr_csv5(#{sMSChargingInformation := #{rATType := RAT}} = CFR, Acc) ->
	chf_cfr_csv6(CFR, [integer_to_binary(RAT) | Acc]);
chf_cfr_csv5(CFR, Acc) ->
	chf_cfr_csv6(CFR, [<<>> | Acc]).
%% @hidden
chf_cfr_csv6(#{pDUSessionChargingInformation := #{pDUAddress
		:= #{pDUIPv4Address := Address}}} = CFR, Acc) ->
	chf_cfr_csv7(CFR, [list_to_binary(inet:ntoa(Address)) | Acc]);
chf_cfr_csv6(#{pDUSessionChargingInformation := #{pDUAddress
		:= #{pDUIPv6AddresswithPrefix := Address}}} = CFR, Acc) ->
	chf_cfr_csv7(CFR, [list_to_binary(inet:ntoa(Address)) | Acc]);
chf_cfr_csv6(CFR, Acc) ->
	chf_cfr_csv7(CFR, [<<>> | Acc]).
%% @hidden
chf_cfr_csv7(#{sMSChargingInformation := SMS} = CFR, Acc) ->
	Origin = csv_sms_origin(SMS),
	Destination = csv_sms_recipient(SMS),
	chf_cfr_csv8(CFR, [Destination, Origin | Acc]);
chf_cfr_csv7(#{iMSChargingInformation := IMS} = CFR, Acc) ->
	Origin = csv_ims_calling(IMS),
	Destination = csv_ims_called(IMS),
	chf_cfr_csv8(CFR, [Destination, Origin | Acc]);
chf_cfr_csv7(CFR, Acc) ->
	chf_cfr_csv8(CFR, [<<>>, <<>> | Acc]).
%% @hidden
chf_cfr_csv8(#{cause := Cause} = CFR, Acc) ->
	chf_cfr_csv9(CFR, [atom_to_binary(Cause) | Acc]);
chf_cfr_csv8(CFR, Acc) ->
	chf_cfr_csv9(CFR, [<<>> | Acc]).
%% @hidden
chf_cfr_csv9(_CFR, Acc) ->
	lists:reverse(Acc).

%% @hidden
chf_rated_csv(Rated) ->
	chf_rated_csv(Rated, []).
%% @hidden
chf_rated_csv([H | T], Acc) ->
	chf_rated_csv(T, chf_rated_csv1(H, Acc));
chf_rated_csv([], Acc) ->
	lists:reverse(Acc).
%% @hidden
chf_rated_csv1(#rated{bucket_type = Type} = Rated, Acc)
		when Type /= undefined ->
	Units = atom_to_binary(Type),
	chf_rated_csv2(Rated, [Units | Acc]);
chf_rated_csv1(Rated, Acc) ->
	chf_rated_csv2(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv2(#rated{bucket_type = cents, bucket_value = N} = Rated,
		Acc) when is_integer(N) ->
	chf_rated_csv3(Rated, [<<>> | Acc]);
chf_rated_csv2(#rated{bucket_value = N} = Rated, Acc)
		when is_integer(N) ->
	Amount = integer_to_binary(N),
	chf_rated_csv3(Rated, [Amount | Acc]);
chf_rated_csv2(Rated, Acc) ->
	chf_rated_csv3(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv3(#rated{tax_excluded_amount = N} = Rated,
		Acc) when is_integer(N) ->
	Cost = case {N div 1000000, N rem 1000000} of
		{Cents, 0} ->
			integer_to_binary(Cents);
		{Cents, Decimal} ->
			S1 = io_lib:fwrite("~b.~6.10.0b", [Cents, Decimal]),
			S2 = string:trim(S1, trailing, [$0]),
			list_to_binary(S2)
	end,
	chf_rated_csv4(Rated, [Cost | Acc]);
chf_rated_csv3(Rated, Acc) ->
	chf_rated_csv4(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv4(#rated{usage_rating_tag = included} = Rated, Acc) ->
	chf_rated_csv5(Rated, [<<"true">> | Acc]);
chf_rated_csv4(#rated{usage_rating_tag = non_included} = Rated, Acc) ->
	chf_rated_csv5(Rated, [<<"false">> | Acc]);
chf_rated_csv4(Rated, Acc) ->
	chf_rated_csv5(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv5(#rated{product = Offer} = Rated, Acc)
		when is_list(Offer) ->
	chf_rated_csv6(Rated, [list_to_binary(Offer) | Acc]);
chf_rated_csv5(Rated, Acc) ->
	chf_rated_csv6(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv6(#rated{price_name = Price} = Rated, Acc)
		when is_list(Price) ->
	chf_rated_csv7(Rated, [list_to_binary(Price) | Acc]);
chf_rated_csv6(Rated, Acc) ->
	chf_rated_csv7(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv7(#rated{price_type = usage} = Rated, Acc) ->
	chf_rated_csv8(Rated, [<<"usage">> | Acc]);
chf_rated_csv7(#rated{price_type = tariff} = Rated, Acc) ->
	chf_rated_csv8(Rated, [<<"tariff">> | Acc]);
chf_rated_csv7(#rated{price_type = event} = Rated, Acc) ->
	chf_rated_csv8(Rated, [<<"event">> | Acc]);
chf_rated_csv7(Rated, Acc) ->
	chf_rated_csv8(Rated, [<<>> | Acc]).
%% @hidden
chf_rated_csv8(_Rated, Acc) ->
	Acc.

%% @hidden
nf_info_name(#{networkFunctionName := Name}) ->
	Name;
nf_info_name(#{networkFunctionFQDN := FQDN}) ->
	FQDN;
nf_info_name(_) ->
	<<>>.

%% @hidden
nf_info_address(#{networkFunctionIPv4Address := Address}) ->
	list_to_binary(inet:ntoa(Address));
nf_info_address(#{networkFunctionIPv6Address := Address}) ->
	list_to_binary(inet:ntoa(Address));
nf_info_address(_) ->
	<<>>.

%% @hidden
csv_subscriber(L) ->
	csv_subscriber(L, <<>>, <<>>, <<>>).
%% @hidden
csv_subscriber([#{subscriptionIDType := 'eND-USER-IMSI',
		subscriptionIDData := IMSI} | T], <<>>, MSISDN, NAI) ->
	csv_subscriber(T, IMSI, MSISDN, NAI);
csv_subscriber([#{subscriptionIDType := 'eND-USER-E164',
		subscriptionIDData := MSISDN} | T], IMSI, <<>>, NAI) ->
	csv_subscriber(T, IMSI, MSISDN, NAI);
csv_subscriber([#{subscriptionIDType := 'eND-USER-NAI',
		subscriptionIDData := NAI} | T], IMSI, MSISDN, <<>>) ->
	csv_subscriber(T, IMSI, MSISDN, NAI);
csv_subscriber([_ | T], IMSI, MSISDN, NAI) ->
	csv_subscriber(T, IMSI, MSISDN, NAI);
csv_subscriber([], IMSI, MSISDN, NAI) ->
	{IMSI, MSISDN, NAI}.

%% @todo Implement csv_access_network_info/1
%% @hidden
csv_access_network_info(#{accessNetworkInformation := [H | _]}) ->
	{<<>>, <<>>}.

%% @hidden
csv_user_location(#{userLocationInformation := LocationInfo})
		when is_map(LocationInfo) ->
	csv_location_info(LocationInfo);
csv_user_location(#{userLocationInformation := LocationInfo})
		when is_binary(LocationInfo) ->
	csv_location_info(location_info(LocationInfo));
csv_user_location(_ULI) ->
	{<<>>, <<>>}.

%% @hidden
csv_location_info(#{eutraLocation := #{ecgi := ECGI}}) ->
	csv_ecgi(ECGI);
csv_location_info(#{eutraLocation := #{tai := TAI}}) ->
	{csv_plmnid(TAI), <<>>};
csv_location_info(#{nrLocation := #{ncgi := NCGI}}) ->
	csv_ncgi(NCGI);
csv_location_info(#{nrLocation := #{tai := TAI}}) ->
	{csv_plmnid(TAI), <<>>};
csv_location_info(#{n3gaLocation := #{n3gppTai := TAI, tnapId := AP}}) ->
	{csv_plmnid(TAI), AP};
csv_location_info(#{n3gaLocation := #{n3gppTai := TAI, twapId := AP}}) ->
	{csv_plmnid(TAI), AP};
csv_location_info(#{n3gaLocation := #{n3gppTai := TAI}}) ->
	{csv_plmnid(TAI), <<>>};
csv_location_info(#{utraLocation := #{cgi := CGI}}) ->
	csv_cgi(CGI);
csv_location_info(#{utraLocation := #{sai := SAI}}) ->
	{csv_plmnid(SAI), <<>>};
csv_location_info(#{utraLocation := #{lai := LAI}}) ->
	{csv_plmnid(LAI), <<>>};
csv_location_info(#{utraLocation := #{rai := RAI}}) ->
	{csv_plmnid(RAI), <<>>};
csv_location_info(#{geraLocation := #{cgi := CGI}}) ->
	csv_cgi(CGI);
csv_location_info(#{geraLocation := #{sai := SAI}}) ->
	{csv_plmnid(SAI), <<>>};
csv_location_info(#{geraLocation := #{lai := LAI}}) ->
	{csv_plmnid(LAI), <<>>};
csv_location_info(#{geraLocation := #{rai := RAI}}) ->
	{csv_plmnid(RAI), <<>>};
csv_location_info(#{geraLocation := #{vlrNumber := VLR}}) ->
	{VLR, <<>>};
csv_location_info(#{geraLocation := #{mscNumber := MSC}}) ->
	{MSC, <<>>};
csv_location_info(_) ->
	{<<>>, <<>>}.

%% @hidden
csv_plmnid(#{plmnId := PLMN}) ->
	PLMN.

%% @hidden
csv_ecgi(#{plmnId :=  PLMN, eutraCellId := Cell}) ->
	{PLMN, Cell}.

%% @hidden
csv_ncgi(#{plmnId := PLMN, nrCellId := Cell}) ->
	{PLMN, Cell}.

%% @hidden
csv_cgi(#{plmnId := PLMN, cellId := Cell}) ->
	{PLMN, Cell}.

-dialyzer([{nowarn_function, [location_info/1]}, no_underspecs]).
-spec location_info(LocationInfo) -> LocationInfo
	when
		LocationInfo :: structured_location_info() | binary().
%% @doc CODEC for user location information.
%% @todo Implement location_info/1
%% @hidden
location_info(LocationInfo)
		when is_map(LocationInfo) ->
	<<>>;
location_info(LocationInfo)
		when is_binary(LocationInfo) ->
	#{}.

%% @hidden
csv_ims_calling(#{callingPartyAddresses := [H | _]}) ->
	csv_involved_party(H).

%% @hidden
csv_ims_called(#{calledPartyAddress := CalledParty}) ->
	csv_involved_party(CalledParty).

%% @hidden
csv_involved_party({'iSDN-E164', E164}) ->
	E164;
csv_involved_party({'tEL-URI', TELURI}) ->
	#{path := Path} = uri_string:parse(TELURI),
	[DN | _Params] = string:lexemes(Path, [$;]),
	DN;
csv_involved_party({'sIP-URI', SIPURI}) ->
	#{path := Path} = uri_string:parse(SIPURI),
	[Party | _Params] = string:lexemes(Path, [$;]),
	Party;
csv_involved_party({uRN, URN}) ->
	URN;
csv_involved_party({externalId, ExternalId}) ->
	ExternalId.

%% @hidden
csv_sms_origin(#{originatorInfo := OriginInfo}) ->
	csv_originator_info(OriginInfo).

%% @hidden
csv_sms_recipient(#{recipientInfos := [H | _]}) ->
	csv_recipient_info(H).

%% @hidden
csv_originator_info(#{originatorMSISDN := MSISDN}) ->
	MSISDN;
csv_originator_info(#{originatorIMSI := IMSI}) ->
	IMSI;
csv_originator_info(#{originatorOtherAddress := Other}) ->
	Other;
csv_originator_info(_) ->
	<<>>.

%% @hidden
csv_recipient_info(#{recipientMSISDN := MSISDN}) ->
	MSISDN;
csv_recipient_info(#{recipientIMSI := IMSI}) ->
	IMSI;
csv_recipient_info(#{recipientOtherAddress := Other}) ->
	Other;
csv_recipient_info(_) ->
	<<>>.

