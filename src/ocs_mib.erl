%%% ocs_mib.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2022 SigScale Global Inc.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the SNMP MIB for the
%%%     {@link //ocs. ocs} application.
%%%
-module(ocs_mib).
-copyright('Copyright (c) 2018 - 2022 SigScale Global Inc.').

%% export the ocs_mib public API
-export([load/0, load/1, unload/0, unload/1]).

%% export the ocs_mib snmp agent callbacks
-export([client_table/3, radius_auth_server/2, radius_acct_server/2,
		dbp_local_config/2, dbp_local_stats/2,
		dcca_peer_info/3, dcca_peer_stats/3]).

-include("ocs.hrl").

-record(peer_stats,
		{ccr_in = 0 :: non_neg_integer(),
		ccr_out = 0 :: non_neg_integer(),
		ccr_dropped = 0 :: non_neg_integer(),
		cca_in = 0 :: non_neg_integer(),
		cca_out = 0 :: non_neg_integer(),
		cca_dropped = 0 :: non_neg_integer(),
		rar_in = 0 :: non_neg_integer(),
		rar_dropped = 0 :: non_neg_integer(),
		rar_out = 0 :: non_neg_integer(),
		raa_out = 0 :: non_neg_integer(),
		str_in = 0 :: non_neg_integer(),
		str_out = 0 :: non_neg_integer(),
		str_dropped = 0 :: non_neg_integer(),
		sta_in = 0 :: non_neg_integer(),
		sta_out = 0 :: non_neg_integer(),
		sta_dropped = 0 :: non_neg_integer(),
		aar_in = 0 :: non_neg_integer(),
		aar_out = 0 :: non_neg_integer(),
		aar_dropped = 0 :: non_neg_integer(),
		aaa_in = 0 :: non_neg_integer(),
		aaa_out = 0 :: non_neg_integer(),
		aaa_dropped = 0 :: non_neg_integer(),
		asr_in = 0 :: non_neg_integer(),
		asr_dropped = 0 :: non_neg_integer(),
		asr_out = 0 :: non_neg_integer(),
		asa_dropped = 0 :: non_neg_integer(),
		asa_out = 0 :: non_neg_integer(),
		raa_in = 0 :: non_neg_integer(),
		asa_in = 0 :: non_neg_integer()}).

%%----------------------------------------------------------------------
%%  The ocs_mib public API
%%----------------------------------------------------------------------

-spec load() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Loads the SigScale OCS MIB.
load() ->
	case code:priv_dir(ocs) of
		PrivDir when is_list(PrivDir) ->
			MibDir = PrivDir ++ "/mibs/",
			Mibs = [MibDir ++ MIB || MIB <- mibs()],
			snmpa:load_mibs(Mibs);
		{error, Reason} ->
			{error, Reason}
	end.

-spec load(Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Loads the SigScale OCS MIB.
load(Agent) ->
	case code:priv_dir(ocs) of
		PrivDir when is_list(PrivDir) ->
			MibDir = PrivDir ++ "/mibs/",
			Mibs = [MibDir ++ MIB || MIB <- mibs()],
			snmpa:load_mibs(Agent, Mibs);
		{error, Reason} ->
			{error, Reason}
	end.

-spec unload() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Unloads the SigScale OCS MIB.
unload() ->
	snmpa:unload_mibs(mibs()).

-spec unload(Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Unloads the SigScale OCS MIB.
unload(Agent) ->
	snmpa:unload_mibs(Agent, mibs()).

%%----------------------------------------------------------------------
%% The ocs_mib snmp agent callbacks
%----------------------------------------------------------------------

-spec client_table(Operation, RowIndex, Columns) -> Result
	when
		Operation :: get | get_next,
		RowIndex :: ObjectId,
		ObjectId :: [integer()],
		Columns :: [Column],
		Column :: integer(),
		Result :: [Element] | {genErr, Column},
		Element :: {value, Value} | {ObjectId, Value},
		Value :: atom() | integer() | string() | [integer()].
%% @doc Handle SNMP requests for the client table.
%% @private
client_table(get, [1, 4] ++ Key = _RowIndex, Columns)
		when length(Key) == 4 ->
	case catch ocs:find_client(list_to_tuple(Key)) of
		{ok, #client{port = Port, identifier = Id, protocol = Proto}} ->
			F2 = fun(1, Acc) ->
						[{value, ipv4} | Acc];
					(2, Acc) ->
						[{value, Key} | Acc];
					(3, Acc) when Port == undefined ->
						[{value, 0} | Acc];
					(3, Acc) ->
						[{value, Port} | Acc];
					(4, Acc) ->
						[{value, binary_to_list(Id)} | Acc];
					(5, Acc) ->
						[{value, Proto} | Acc];
					(_, Acc) ->
						[{noValue, noSuchInstance} | Acc]
			end,
			lists:reverse(lists:foldl(F2, [], Columns));
		{error, not_found} ->
			{noValue, noSuchInstance};
		{'EXIT', _Reason} ->
			{genErr, 0}
	end;
client_table(get, _RowIndex, _Columns) ->
	{noValue, noSuchInstance};
client_table(get_next, [], Columns) ->
	F = fun() ->
			 mnesia:first(client)
	end,
	client_get_next(F, Columns, true);
client_table(get_next, [1, 4] ++ Key, Columns)
		when length(Key) == 4 ->
	F = fun() ->
			 mnesia:next(client, list_to_tuple(Key))
	end,
	client_get_next(F, Columns, true).
%% @hidden
client_get_next(F1, Columns, First) ->
	case mnesia:ets(F1) of
		IP when is_tuple(IP) ->
			case ocs:find_client(IP) of
				{ok, #client{port = Port, identifier = Id, protocol = Proto}} ->
					Key = tuple_to_list(IP),
					F2 = fun(0, Acc) ->
								[{[1, 1, 4 | Key], ipv4} | Acc];
							(1, Acc) ->
								[{[1, 1, 4 | Key], ipv4} | Acc];
							(2, Acc) ->
								[{[2, 1, 4 | Key], Key} | Acc];
							(3, Acc) when Port == undefined ->
								[{[3, 1, 4 | Key], 0} | Acc];
							(3, Acc) ->
								[{[3, 1, 4 | Key], Port} | Acc];
							(4, Acc) ->
								[{[4, 1, 4 | Key], binary_to_list(Id)} | Acc];
							(5, Acc) ->
								[{[5, 1, 4 | Key], Proto} | Acc];
							(_, Acc) ->
								[endOfTable | Acc]
					end,
					lists:reverse(lists:foldl(F2, [], Columns));
				{error, _Reason} ->
					{genErr, 0}
			end;
		'$end_of_table' when First == true ->
			F3 = fun(N) ->
					N + 1
			end,
			NextColumns = lists:map(F3, Columns),
			F4 = fun() ->
					mnesia:first(client)
			end,
			client_get_next(F4, NextColumns, false);
		'$end_of_table' ->
			[endOfTable || _ <- Columns]
	end.

-spec radius_auth_server(Operation, Item) -> Result
	when
		Operation :: get,
		Item :: ident,
		Result :: {value, Value} | genErr,
		Value :: atom() | integer() | string() | [integer()].
% @doc Get RADIUS authentication server identity"
% @private
radius_auth_server(get, ident = _Item) ->
	{value, "SigScale OCS"}.

-spec radius_acct_server(Operation, Item) -> Result
	when
		Operation :: get,
		Item :: ident,
		Result :: {value, Value} | genErr,
		Value :: atom() | integer() | string() | [integer()].
% @doc Get RADIUS accounting server identity"
% @private
radius_acct_server(get, ident = _Item) ->
	{value, "SigScale OCS"}.

-spec dbp_local_config(Operation, Item) -> Result
	when
		Operation :: get,
		Item :: 'Origin-Host' | 'Origin-Realm' | 'Product-Name',
		Result :: {value, Value} | genErr,
		Value :: atom() | integer() | string() | [integer()].
% @doc Get local DIAMETER configuration.
% @private
dbp_local_config(get, Item) ->
	case lists:keyfind(ocs_diameter_acct_service, 1, diameter:services()) of
		Service when is_tuple(Service) ->
			case diameter:service_info(Service, Item) of
				Info when is_binary(Info) ->
					{value, binary_to_list(Info)};
				Info when is_list(Info) ->
					{value, Info};
				_ ->
					genErr
			end;
		false ->
			{noValue, noSuchInstance}
	end.

-spec dbp_local_stats(Operation, Item) -> Result
	when
		Operation :: get,
		Item :: uptime,
		Result :: {value, Value} | {noValue, noSuchInstance} | genErr,
		Value :: atom() | integer() | string() | [integer()].
%% @doc Handle SNMP requests for `DIAMETER-BASE-PROTOCOL-MIB::dbpLocalStats'.
%% @private
dbp_local_stats(get, uptime) ->
	case catch diameter_stats:uptime() of
		{'EXIT', _Reason} ->
			genErr;
		{Hours, Mins, Secs, MicroSecs} ->
			{value, (Hours * 360000) + (Mins * 6000)
					+ (Secs * 100) + (MicroSecs div 10)}
	end;
dbp_local_stats(get, Item) ->
	case lists:keyfind(ocs_diameter_acct_service, 1, diameter:services()) of
		Service when is_tuple(Service) ->
			case catch diameter:service_info(Service, transport) of
				Info when is_list(Info) ->
					case total_packets(Info) of
						{ok, {PacketsIn, _}} when Item == in ->
							{value, PacketsIn};
						{ok, {_, PacketsOut}} when Item == out ->
							{value, PacketsOut};
						{error, not_found} ->
							{noValue, noSuchInstance}
					end;
				_ ->
					genErr
			end;
		false ->
			genErr
	end.

-spec dcca_peer_info(Operation, RowIndex, Columns) -> Result
	when
		Operation :: get | get_next,
		RowIndex :: ObjectId,
		ObjectId :: [integer()],
		Columns :: [Column],
		Column :: integer(),
		Result :: [Element] | {genErr, Column},
		Element :: {value, Value} | {ObjectId, Value},
		Value :: atom() | integer() | string() | [integer()].
%% @doc Handle SNMP requests for the peer info table.
dcca_peer_info(get_next = _Operation, [] = _RowIndex, Columns) ->
	dcca_peer_info_get_next(1, Columns, true);
dcca_peer_info(get_next, [N], Columns) ->
	dcca_peer_info_get_next(N + 1, Columns, true);
dcca_peer_info(get, [N], Columns) ->
	dcca_peer_info_get(N, Columns).
%% @hidden
dcca_peer_info_get_next(Index, Columns, First) ->
	case lists:keyfind(ocs_diameter_acct_service, 1, diameter:services()) of
		Service when is_tuple(Service) ->
			case catch diameter:service_info(Service, connections) of
				Info when is_list(Info) ->
					case peer_info(Index, Info) of
						{ok, {PeerId, Rev}} ->
							F1 = fun(0, Acc) ->
										[{[1, Index], Index} | Acc];
									(1, Acc) ->
										[{[1, Index], Index} | Acc];
									(2, Acc) ->
										[{[2, Index], PeerId} | Acc];
									(3, Acc) when Rev == undefined ->
										case dcca_peer_info_get_next(Index + 1, [3], true) of
											[NextResult] ->
												[NextResult | Acc];
											{genError, N} ->
												 throw({genError, N})
										end;
									(3, Acc) ->
										[{[3, Index], Rev} | Acc];
									(4, Acc) ->
										[{[4, Index], volatile} | Acc];
									(5, Acc) ->
										[{[5, Index], active} | Acc];
									(_, Acc) ->
										[endOfTable | Acc]
							end,
							try
								 lists:reverse(lists:foldl(F1, [], Columns))
							catch	
								{genError, N} ->
									{genError, N}
							end;
						{error, not_found} when First == true ->
							F2 = fun(N) ->
									N + 1
							end,
							NextColumns = lists:map(F2, Columns),
							dcca_peer_info_get_next(1, NextColumns, false);
						{error, not_found} ->
							[endOfTable || _ <- Columns]
					end;
				_Info ->
					[endOfTable || _ <- Columns]
			end;
		false ->
			{genErr, 0}
	end.
%% @hidden
dcca_peer_info_get(Index, Columns) ->
	case lists:keyfind(ocs_diameter_acct_service, 1, diameter:services()) of
		Service when is_tuple(Service) ->
			case catch diameter:service_info(Service, connections) of
				Info when is_list(Info) ->
					case peer_info(Index, Info) of
						{ok, {PeerId, Rev}} ->
							F1 = fun(0, Acc) ->
										[{value, Index} | Acc];
									(1, Acc) ->
										[{value, Index} | Acc];
									(2, Acc) ->
										[{value, PeerId} | Acc];
									(3, _Acc) when Rev == undefined ->
										[{noValue, noSuchInstance}];
									(3, Acc) ->
										[{value, Rev} | Acc];
									(4, Acc) ->
										[{value, volatile} | Acc];
									(5, Acc) ->
										[{value, active} | Acc];
									(_, _Acc) ->
										{noValue, noSuchInstance}
							end,
							lists:reverse(lists:foldl(F1, [], Columns));
						{error, not_found} ->
							{noValue, noSuchInstance}
					end;
				_Info ->
						{noValue, noSuchInstance}
			end;
		false ->
			{genErr, 0}
	end.

-spec dcca_peer_stats(Operation, RowIndex, Columns) -> Result
	when
		Operation :: get | get_next,
		RowIndex :: ObjectId,
		ObjectId :: [integer()],
		Columns :: [Column],
		Column :: integer(),
		Result :: [Element] | {genErr, Column},
		Element :: {value, Value} | {ObjectId, Value},
		Value :: atom() | integer() | string() | [integer()].
%% @doc Handle SNMP requests for the peer stats table.
dcca_peer_stats(get_next = _Operation, [] = _RowIndex, Columns) ->
	dcca_peer_stats_get_next(1, Columns, true);
dcca_peer_stats(get_next, [N], Columns) ->
	dcca_peer_stats_get_next(N + 1, Columns, true);
dcca_peer_stats(get, [N], Columns) ->
	dcca_peer_stats_get(N, Columns).
	
%% @hidden
dcca_peer_stats_get_next(Index, Columns, First) ->
	case lists:keyfind(ocs_diameter_acct_service, 1, diameter:services()) of
		Service when is_tuple(Service) ->
			case catch diameter:service_info(Service, connections) of
				Info when is_list(Info) ->
					case peer_stats(Index, Info) of
						{ok, Stats} ->
							F1 = fun(0, Acc) ->
										[{[2, Index], Stats#peer_stats.ccr_in} | Acc];
									(1, Acc) ->
										[{genErr, 0} | Acc];
									(2, Acc) ->
										[{[2, Index], Stats#peer_stats.ccr_in} | Acc];
									(3, Acc) ->
										[{[3, Index], Stats#peer_stats.ccr_out} | Acc];
									(4, Acc) ->
										[{[4, Index], Stats#peer_stats.ccr_dropped} | Acc];
									(5, Acc) ->
										[{[5, Index], Stats#peer_stats.cca_in} | Acc];
									(6, Acc) ->
										[{[6, Index], Stats#peer_stats.cca_out} | Acc];
									(7, Acc) ->
										[{[7, Index], Stats#peer_stats.cca_dropped} | Acc];
									(8, Acc) ->
										[{[8, Index], Stats#peer_stats.rar_in} | Acc];
									(9, Acc) ->
										[{[9, Index], Stats#peer_stats.rar_dropped} | Acc];
									(10, Acc) ->
										[{[10, Index], Stats#peer_stats.raa_out} | Acc];
									(11, Acc) ->
										[{[11, Index], Stats#peer_stats.rar_dropped} | Acc];
									(12, Acc) ->
										[{[12, Index], Stats#peer_stats.str_out} | Acc];
									(13, Acc) ->
										[{[13, Index], Stats#peer_stats.str_dropped} | Acc];
									(14, Acc) ->
										[{[14, Index], Stats#peer_stats.sta_in} | Acc];
									(15, Acc) ->
										[{[15, Index], Stats#peer_stats.sta_dropped} | Acc];
									(16, Acc) ->
										[{[16, Index], Stats#peer_stats.aar_out} | Acc];
									(17, Acc) ->
										[{[17, Index], Stats#peer_stats.aar_dropped} | Acc];
									(18, Acc) ->
										[{[18, Index], Stats#peer_stats.aaa_in} | Acc];
									(19, Acc) ->
										[{[19, Index], Stats#peer_stats.aaa_dropped} | Acc];
									(20, Acc) ->
										[{[20, Index], Stats#peer_stats.asr_in} | Acc];
									(21, Acc) ->
										[{[21, Index], Stats#peer_stats.asr_dropped} | Acc];
									(22, Acc) ->
										[{[22, Index], Stats#peer_stats.asa_out} | Acc];
									(23, Acc) ->
										[{[23, Index], Stats#peer_stats.asa_dropped} | Acc];
									(_, Acc) ->
										[endOfTable | Acc]
							end,
							lists:reverse(lists:foldl(F1, [], Columns));
						{error, not_found} when First == true ->
							F2 = fun(N) ->
									N + 1
							end,
							NextColumns = lists:map(F2, Columns),
							dcca_peer_stats_get_next(1, NextColumns, false);
						{error, not_found} ->
							[endOfTable || _ <- Columns]
					end;
				_Info ->
					[endOfTable || _ <- Columns]
			end;
		false ->
			{genErr, 0}
	end.
%% @hidden
dcca_peer_stats_get(Index, Columns) ->
	case lists:keyfind(ocs_diameter_acct_service, 1, diameter:services()) of
		Service when is_tuple(Service) ->
			case catch diameter:service_info(Service, connections) of
				Info when is_list(Info) ->
					case peer_stats(Index, Info) of
						{ok, Stats} ->
							F1 = fun(0, Acc) ->
										[{genErr, 0} | Acc];
									(1, Acc) ->
										[{noValue, noSuchInstance} | Acc];
									(2, Acc) ->
										[{value, Stats#peer_stats.ccr_in} | Acc];
									(3, Acc) ->
										[{value, Stats#peer_stats.ccr_out} | Acc];
									(4, Acc) ->
										[{value, Stats#peer_stats.ccr_dropped} | Acc];
									(5, Acc) ->
										[{value, Stats#peer_stats.cca_in} | Acc];
									(6, Acc) ->
										[{value, Stats#peer_stats.cca_out} | Acc];
									(7, Acc) ->
										[{value, Stats#peer_stats.cca_dropped} | Acc];
									(8, Acc) ->
										[{value, Stats#peer_stats.rar_in} | Acc];
									(9, Acc) ->
										[{value, Stats#peer_stats.rar_dropped} | Acc];
									(10, Acc) ->
										[{value, Stats#peer_stats.raa_out} | Acc];
									(11, Acc) ->
										[{value, Stats#peer_stats.rar_dropped} | Acc];
									(12, Acc) ->
										[{value, Stats#peer_stats.str_out} | Acc];
									(13, Acc) ->
										[{value, Stats#peer_stats.str_dropped} | Acc];
									(14, Acc) ->
										[{value, Stats#peer_stats.sta_in} | Acc];
									(15, Acc) ->
										[{value, Stats#peer_stats.sta_dropped} | Acc];
									(16, Acc) ->
										[{value, Stats#peer_stats.aar_out} | Acc];
									(17, Acc) ->
										[{value, Stats#peer_stats.aar_dropped} | Acc];
									(18, Acc) ->
										[{value, Stats#peer_stats.aaa_in} | Acc];
									(19, Acc) ->
										[{value, Stats#peer_stats.aaa_dropped} | Acc];
									(20, Acc) ->
										[{value, Stats#peer_stats.asr_in} | Acc];
									(21, Acc) ->
										[{value, Stats#peer_stats.asr_dropped} | Acc];
									(22, Acc) ->
										[{value, Stats#peer_stats.asa_out} | Acc];
									(23, Acc) ->
										[{value, Stats#peer_stats.asa_dropped} | Acc];
									(_, Acc) ->
										[{noValue, noSuchInstance} | Acc]
							end,
							lists:reverse(lists:foldl(F1, [], Columns));
						{error, not_found} ->
							{noValue, noSuchInstance}
					end;
				_Info ->
						{noValue, noSuchInstance}
			end;
		false ->
			{genErr, 0}
	end.
%%----------------------------------------------------------------------
%% internal functions
%----------------------------------------------------------------------

%% @hidden
mibs() ->
	["SIGSCALE-OCS-MIB",
			"SIGSCALE-DIAMETER-BASE-PROTOCOL-MIB",
			"SIGSCALE-DIAMETER-CC-APPLICATION-MIB",
			"RADIUS-AUTH-SERVER-MIB",
			"RADIUS-ACC-SERVER-MIB"].

-spec total_packets(Info) -> Result
	when
		Info :: [tuple()],
		Result :: {ok, {PacketsIn, PacketsOut}} | {error, Reason},
		PacketsIn :: integer(),
		PacketsOut :: integer(),
		Reason :: term().
%% @doc Get packet counts from service info.

total_packets(Info) ->
	total_packets(Info, {0, 0}).
%% @hidden
total_packets([H | T], Acc) ->
	case lists:keyfind(accept, 1, H) of
		{_, L} ->
			case total_packets1(L, Acc) of
				{ok, Acc1} ->
					total_packets(T, Acc1);
				{error, _Reason} ->
					total_packets(T, Acc)
			end;
		false ->
			{error, not_found}
	end;
total_packets([], Acc) ->
	{ok, Acc}.
%% @hidden
total_packets1([H | T], Acc) ->
	case lists:keyfind(port, 1, H) of
		{_, L} ->
			case total_packets2(L, Acc) of
				{ok, Acc1} ->
					total_packets1(T, Acc1);
				{error, Reason} ->
					{error, Reason}
			end;
		false ->
			total_packets1(T, Acc)
	end;
total_packets1([], Acc) ->
	{ok, Acc}.
%% @hidden
total_packets2(L1, Acc) ->
	case lists:keyfind(statistics, 1, L1) of
		{_, L2} ->
			total_packets3(L2, Acc);
		false ->
			{error, not_found}
	end.
%% @hidden
total_packets3(L, {PacketsIn, PacketsOut}) ->
	case lists:keyfind(recv_cnt, 1, L) of
		{_, N} ->
			total_packets4(L, {PacketsIn + N, PacketsOut});
		false ->
			{error, not_found}
	end.
%% @hidden
total_packets4(L, {PacketsIn, PacketsOut}) ->
	case lists:keyfind(send_cnt, 1, L) of
		{_, N} ->
			{ok, {PacketsIn, PacketsOut + N}};
		false ->
			{error, not_found}
	end.

-spec peer_info(Index, Info) -> Result
   when
      Index :: integer(),
      Info :: [tuple()],
      Result :: {ok, {PeerId, Rev}} | {error, Reason},
      PeerId :: string(),
      Rev :: integer() | undefined,
      Reason :: term().
%% @doc Get peer entry table.
%% @hidden
peer_info(Index, Info) ->
	case catch lists:nth(Index, Info) of
		Connection when is_list(Connection) ->
			peer_info(Connection);
		_ ->
			{error, not_found}
	end.
%% @hidden
peer_info(Info) ->
	case lists:keyfind(caps, 1, Info) of
		{_, Caps} ->
			peer_info1(Caps);
		false ->
         {error, not_found}
	end.
%% @hidden
peer_info1(Caps) ->
	case lists:keyfind(origin_host, 1, Caps) of
		{_, {_,PeerId}} ->
			peer_info2(Caps, binary_to_list(PeerId));
		false ->
         {error, not_found}
	end.
%% @hidden
peer_info2(Caps, PeerId) ->
	case lists:keyfind(firmware_revision, 1, Caps) of
		{_, {_, []}} ->
			peer_info3(PeerId, undefined);
		{_, {_, [Rev]}} ->
			peer_info3(PeerId, Rev);
		false ->
         {error, not_found}
	end.
%% @hidden
peer_info3(PeerId, Rev) ->
	{ok, {PeerId, Rev}}.

-spec peer_stats(Index, Info) -> Result
   when
		Index :: integer(),
      Info :: [tuple()],
      Result :: {ok, #peer_stats{}} | {error, Reason},
      Reason :: term().
%% @doc Get peer stats table entry.
%% @hidden
peer_stats(Index, Info) ->
	case catch lists:nth(Index, Info) of
		Connection when is_list(Connection) ->
			peer_stats(Connection);
		_ ->
			{error, not_found}
	end.
%% @hidden
peer_stats(Connection) ->
	case lists:keyfind(statistics, 1, Connection) of
		{_, Statistics} ->
				peer_stats1(Statistics, #peer_stats{});
		false ->
			{error, not_found}
	end.
%% @hidden
peer_stats1([{{{_, 272, 0}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{ccr_in = N});
peer_stats1([{{{_, 272, 0}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{ccr_out = N});
peer_stats1([{{{_, 272, 1}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{cca_in = N});
peer_stats1([{{{_, 272, 1}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{cca_out = N});
peer_stats1([{{{_, 258, 0}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{rar_in = N});
peer_stats1([{{{_, 258, 0}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{rar_out = N});
peer_stats1([{{{_, 258, 1}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{raa_in = N});
peer_stats1([{{{_, 258, 1}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{raa_out = N});
peer_stats1([{{{_, 275, 0}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{str_in = N});
peer_stats1([{{{_, 275, 0}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{str_out = N});
peer_stats1([{{{_, 275, 1}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{sta_in = N});
peer_stats1([{{{_, 275, 1}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{sta_out = N});
peer_stats1([{{{_, 265, 0}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{aar_in = N});
peer_stats1([{{{_, 265, 0}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{aar_out = N});
peer_stats1([{{{_, 265, 1}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{aaa_in = N});
peer_stats1([{{{_, 265, 1}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{aaa_out = N});
peer_stats1([{{{_, 274, 0}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{asr_in = N});
peer_stats1([{{{_, 274, 0}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{asr_out = N});
peer_stats1([{{{_, 274, 1}, recv}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{asa_in = N});
peer_stats1([{{{_, 274, 1}, send}, N} | T], Acc) ->
	peer_stats1(T, Acc#peer_stats{asa_out = N});
peer_stats1([_ | T], Acc) ->
	peer_stats1(T, Acc);
peer_stats1([], Acc) ->
	{ok, Acc}.

