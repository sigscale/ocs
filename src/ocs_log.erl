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
%%% @doc This library module implements function used in handling logging
%%% 	in the {@link //ocs. ocs} application.
%%%
-module(ocs_log).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc.').

%% export the ocs_log public API
-export([radius_acct_open/0, radius_acct_log/4, radius_acct_file/1,
		radius_acct_close/0]).

%% export the ocs_log private API
-export([]).

-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").

-define(RADACCT, radius_acct).

%%----------------------------------------------------------------------
%%  The ocs_log public API
%%----------------------------------------------------------------------

-spec radius_acct_open() -> Result
	when
		Result :: ok | {error, Reason :: term()}.
%% @doc Open the accounting log for logging events.
radius_acct_open() ->
	{ok, Directory} = application:get_env(ocs, acct_log_dir),
	case file:make_dir(Directory) of
		ok ->
			radius_acct_open1(Directory);
		{error, eexist} ->
			radius_acct_open1(Directory);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
radius_acct_open1(Directory) ->
	{ok, LogSize} = application:get_env(ocs, acct_log_size),
	{ok, LogFiles} = application:get_env(ocs, acct_log_files),
	Log = ?RADACCT,
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

-spec radius_acct_log(Server, Client, Type, Attributes) -> Result
	when
		Server :: {Address :: inet:ip_address(),
				Port :: integer()},
		Client :: {Address :: inet:ip_address(),
				Port :: integer()},
		Type :: on | off | start | stop | interim,
		Attributes :: radius_attributes:attributes(),
		Result :: ok | {error, Reason :: term()}.
%% @doc Write an accounting event to disk log.
radius_acct_log(Server, Client, Type, Attributes) ->
	TS = erlang:system_time(millisecond),
	Event = {TS, node(), Server, Client, Type, Attributes},
	disk_log:log(?RADACCT, Event).

-spec radius_acct_close() -> Result
	when
		Result :: ok | {error, Reason :: term()}.
%% @doc Close accounting disk log.
radius_acct_close() ->
	case disk_log:close(?RADACCT) of
		ok ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, ?RADACCT}, {error, Reason}]),
			{error, Reason}
	end.

-spec radius_acct_file(FileName) -> Result
	when
		FileName :: string(),
		Result :: ok | {error, Reason :: term()}.
%% @doc Write all logged accounting records to a file.
%%
radius_acct_file(FileName) when is_list(FileName) ->
   case file:open(FileName, [write]) of
   	{ok, IODevice} ->
   		file_chunk(?RADACCT, IODevice, start);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, ?RADACCT}, {error, Reason}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
file_chunk(Log, IODevice, Continuation) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			file:close(IODevice);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IODevice),
			{error, Reason};
		{Continuation2, Terms} ->
			Fun =  fun(Event) ->
						io:fwrite(IODevice, "~999p~n", [Event])
			end,
			lists:foreach(Fun, Terms),
			file_chunk(Log, IODevice, Continuation2)
	end.

