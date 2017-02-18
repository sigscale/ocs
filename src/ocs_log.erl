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
-export([radius_acct_open/0, radius_acct_log/4, radius_acct_close/0]).
-export([radius_auth_open/0, radius_auth_log/5, radius_auth_close/0]).
-export([ipdr_log/3, get_range/4, dump_file/2]).

%% export the ocs_log private API
-export([]).

-include("ocs_log.hrl").
-include_lib("radius/include/radius.hrl").

-define(RADACCT, radius_acct).
-define(RADAUTH, radius_auth).

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

-spec radius_auth_open() -> Result
	when
		Result :: ok | {error, Reason :: term()}.
%% @doc Open the authorization log for logging events.
radius_auth_open() ->
	{ok, Directory} = application:get_env(ocs, auth_log_dir),
	case file:make_dir(Directory) of
		ok ->
			radius_auth_open1(Directory);
		{error, eexist} ->
			radius_auth_open1(Directory);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
radius_auth_open1(Directory) ->
	{ok, LogSize} = application:get_env(ocs, auth_log_size),
	{ok, LogFiles} = application:get_env(ocs, auth_log_files),
	Log = ?RADAUTH,
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

-spec radius_auth_log(Server, Client, Type, RequestAttributes,
		ResponseAttributes) -> Result
	when
		Server :: {Address :: inet:ip_address(),
				Port :: integer()},
		Client :: {Address :: inet:ip_address(),
				Port :: integer()},
		Type :: accept | reject | change,
		RequestAttributes :: radius_attributes:attributes(),
		ResponseAttributes :: radius_attributes:attributes(),
		Result :: ok | {error, Reason :: term()}.
%% @doc Write an authorization event to disk log.
radius_auth_log(Server, Client, Type, RequestAttributes, ResponseAttributes) ->
	TS = erlang:system_time(millisecond),
	Event = {TS, node(), Server, Client, Type,
			RequestAttributes, ResponseAttributes},
	disk_log:log(?RADAUTH, Event).

-spec radius_auth_close() -> Result
	when
		Result :: ok | {error, Reason :: term()}.
%% @doc Close authorization disk log.
radius_auth_close() ->
	case disk_log:close(?RADAUTH) of
		ok ->
			ok;
		{error, Reason} ->
			Descr = lists:flatten(disk_log:format_error(Reason)),
			Trunc = lists:sublist(Descr, length(Descr) - 1),
			error_logger:error_report([Trunc, {module, ?MODULE},
					{log, ?RADAUTH}, {error, Reason}]),
			{error, Reason}
	end.

-spec ipdr_log(File, Start, End) -> Result
	when
		File :: file:filename(),
		Start :: pos_integer(),
		End :: pos_integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Log accounting records within range to new IPDR disk log.
ipdr_log(File, Start, End) when is_list(File),
		is_integer(Start), is_integer(End) ->
	case disk_log:open([{name, File}, {file, File}]) of
		{ok, IpdrLog} ->
			ipdr_log1(IpdrLog, Start, End,
					start_binary_tree(?RADACCT, Start, End));
		{error, Reason} ->
			error_logger:error_report([disk_log:format_error(Reason),
					{module, ?MODULE}, {file, File}, {error, Reason}]),
			{error, Reason}
	end.
%% @hidden
ipdr_log1(IpdrLog, _Start, _End, eof) ->
	ipdr_log5(IpdrLog, disk_log:close(IpdrLog));
ipdr_log1(IpdrLog, Start, End, Cont) ->
	ipdr_log2(IpdrLog, Start, End, disk_log:chunk(?RADACCT, Cont)).
%% @hidden
ipdr_log2(IpdrLog, _Start, _End, eof) ->
	ipdr_log5(IpdrLog, disk_log:close(IpdrLog));
ipdr_log2(IpdrLog, _Start, _End, {error, Reason}) ->
	error_logger:error_report([disk_log:format_error(Reason),
			{module, ?MODULE}, {log, ?RADACCT}, {error, Reason}]),
	ipdr_log5(IpdrLog, disk_log:close(IpdrLog));
ipdr_log2(IpdrLog, Start, End, {Cont, Accts}) ->
	Fstart = fun(R) when element(1, R) < Start ->
				true;
			(_) ->
				false
	end,
	case lists:dropwhile(Fstart, Accts) of
		[] ->
			ipdr_log2(IpdrLog, Start, End, disk_log:chunk(?RADACCT, Cont));
		Accts1 ->
			ipdr_log3(IpdrLog, Start, End, {Cont, Accts1})
	end.
%% @hidden
ipdr_log3(IpdrLog, _Start, _End, eof) ->
	ipdr_log5(IpdrLog, disk_log:close(IpdrLog));
ipdr_log3(IpdrLog, _Start, _End, {error, Reason}) ->
	error_logger:error_report([disk_log:format_error(Reason),
			{module, ?MODULE}, {log, ?RADACCT}, {error, Reason}]),
	ipdr_log5(IpdrLog, disk_log:close(IpdrLog));
ipdr_log3(IpdrLog, Start, End, {Cont, Accts}) ->
	Fend = fun(R) when element(1, R) < End ->
				true;
			(_) ->
				false
	end,
	ipdr_log4(IpdrLog, Start, End, {Cont, Accts}, lists:takewhile(Fend, Accts)).
%% @hidden
ipdr_log4(IpdrLog, _Start, _End, {_Cont, _Accts}, []) ->
	ipdr_log5(IpdrLog, disk_log:close(IpdrLog));
ipdr_log4(IpdrLog, Start, End, {Cont, Accts}, Filtered) ->
	case disk_log:log_terms(IpdrLog, Filtered) of
		ok ->
			case Filtered of
				Accts ->
					ipdr_log3(IpdrLog, Start, End, disk_log:chunk(?RADACCT, Cont));
				_ ->
					ipdr_log5(IpdrLog, disk_log:close(IpdrLog))
			end;
		{error, Reason} ->
			error_logger:error_report([disk_log:format_error(Reason),
					{module, ?MODULE}, {log, IpdrLog}, {error, Reason}]),
			ipdr_log5(IpdrLog, disk_log:close(IpdrLog))
	end.
%% @hidden
ipdr_log5(_IpdrLog, ok) ->
	ok;
ipdr_log5(IpdrLog, {error, Reason}) ->
	error_logger:error_report([disk_log:format_error(Reason),
			{module, ?MODULE}, {log, IpdrLog}, {error, Reason}]),
	{error, Reason}.

-spec dump_file(Log, FileName) -> Result
	when
		Log :: disk_log:log(),
		FileName :: file:filename(),
		Result :: ok | {error, Reason :: term()}.
%% @doc Write all logged records to a file.
%%
dump_file(Log, FileName) when is_list(FileName) ->
	case file:open(FileName, [write]) of
		{ok, IoDevice} ->
			file_chunk(Log, IoDevice, start);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
file_chunk(Log, IoDevice, Cont) ->
	case disk_log:chunk(Log, Cont) of
		eof ->
			file:close(IoDevice);
		{error, Reason} ->
			error_logger:error_report([file:format_error(Reason),
					{module, ?MODULE}, {log, Log}, {error, Reason}]),
			file:close(IoDevice),
			{error, Reason};
		{NextCont, Terms} ->
			Fun =  fun(Event) ->
						io:fwrite(IoDevice, "~999p~n", [Event])
			end,
			lists:foreach(Fun, Terms),
			file_chunk(Log, IoDevice, NextCont)
	end.

-spec start_binary_tree(Log, Start, End) -> Result
	when
		Log :: disk_log:log(),
		Start :: pos_integer(),
		End :: pos_integer(),
		Result :: eof | disk_log:continuation().
%% @doc Binary tree search of multi file wrap disk_log.
%% @private
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
		Cont :: disk_log:continuation(),
		Result :: [term()].
%% @doc Sequentially read 64KB chunks.
%%
%% 	Filters out records before `Start' and after `End'.
%% 	Returns filtered records.
%% @private
get_range(Log, Start, End, Cont) ->
	get_range(Log, Start, End, Cont, []).
%% @hidden
get_range(Log, Start, End, Cont, Acc) ->
	case disk_log:chunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		{NextCont, Records} ->
			Fstart = fun(R) when element(1, R) < Start ->
						true;
					(_) ->
						false
			end,
			Records1 = lists:dropwhile(Fstart, Records),
			Fend = fun(R) when element(1, R) < End ->
						true;
					(_) ->
						false
			end,
			case lists:takewhile(Fend, Records1) of
				Records1 ->
					get_range(Log, Start, End, NextCont, [Records1 | Acc]);
				Records2 ->
					lists:flatten(lists:reverse([Records2 | Acc]))
			end;
		eof ->
			lists:flatten(lists:reverse(Acc))
	end.

