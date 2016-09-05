%%% ocs.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
%%% @doc This library module implements the public API for the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export the ocs public API
-export([add_client/2, find_client/1]).
-export([add_subscriber/3, find_subscriber/1]).
-export([log_file/1]).
-export([generate_password/0]).
-export([start/3]).

-include("ocs.hrl").
-define(LOGNAME, radius_acct).

%%----------------------------------------------------------------------
%%  The ocs public API
%%----------------------------------------------------------------------

-spec add_client(Address :: inet:ip_address(), Secret :: string() | binary()) ->
	Result :: ok | {error, Reason :: term()}.
%% @doc Store the shared secret for a RADIUS client.
%%
add_client(Address, Secret) when is_list(Secret) ->
	add_client(Address, list_to_binary(Secret));
add_client(Address, Secret) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, Secret);
add_client(Address, Secret) when is_tuple(Address), is_binary(Secret) ->
	F = fun() ->
				R = #radius_client{address = Address, secret = Secret},
				mnesia:write(R)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_client(Address :: inet:ip_address()) ->
	Result :: {ok, Secret :: binary()} | error.
%% @doc Look up the shared secret for a RADIUS client.
%%
find_client(Address) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	find_client(AddressTuple);
find_client(Address) when is_tuple(Address) ->
	F = fun() ->
				mnesia:read(radius_client, Address, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#radius_client{secret = Secret}]} ->
			{ok, Secret};
		{atomic, []} ->
			error;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec add_subscriber(Subscriber :: string(), Password :: string() | binary(),
		Attributes :: binary() | [byte()]) -> ok | {error, Reason :: term()}.
%% @doc Store the password and static attributes for a subscriber.
%%
add_subscriber(Subscriber, Password, Attributes) when is_list(Password) ->
	F = fun(C)->
		lists:member(C, "abcdefghijkmnpqrstuvwyz23456789")
	end,
	case lists:all(F, Password) of
		true ->
			add_subscriber(Subscriber, list_to_binary(Password), Attributes);
		false ->
			{error, badarg}
	end;
add_subscriber(Subscriber, Password, Attributes) when is_list(Subscriber),
		is_binary(Password), (is_list(Attributes) orelse is_binary(Attributes)) -> 
	F = fun() ->
				R = #subscriber{name = Subscriber, password = Password,
						attributes = Attributes},
				mnesia:write(R)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_subscriber(Subscriber :: string()) ->
	Result :: {ok, Password :: string(),
		Attributes :: binary() | [byte()]} | error.
%% @doc Look up a subscriber and return the password and attributes assigned.
%%
find_subscriber(Subscriber) when is_list(Subscriber) ->
	F = fun() ->
				mnesia:read(subscriber, Subscriber, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#subscriber{password = Password, attributes = Attributes}]} ->
			{ok, Password, Attributes};
		{atomic, []} ->
			error;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec log_file(FileName :: string()) -> ok.
%% @doc Write all logged accounting records to a file.
%% 
log_file(FileName) when is_list(FileName) ->
   {ok, IODevice} = file:open(FileName, [write]),
   file_chunk(?LOGNAME, IODevice, start).

-type password() :: [50..57 | 97..107 | 109..110 | 112..122].
-spec generate_password() -> password().
%% @equiv generate_password(12)
generate_password() ->
	generate_password(12).

-spec start(Type :: auth | acct, Address :: inet:ip_address(),
		Port :: pos_integer()) ->
	{ok, Pid :: pid()} | {error, Reason :: term()}.
%% @doc Start a RADIUS request handler.
start(Type, Address, Port) when is_tuple(Address), is_integer(Port) ->
	gen_server:call(ocs, {start, Type, Address, Port}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec generate_password(Length :: pos_integer()) -> password().
%% @doc Generate a random uniform password.
%% @private
generate_password(Length) when Length > 0 ->
	Charset = charset(),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	generate_password(Random, Charset, NumChars,[]).
%% @hidden
generate_password(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	generate_password(Rest, Charset, NumChars, NewAcc);
generate_password(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

-spec charset() -> Charset :: password().
%% @doc Returns the table of valid characters for passwords.
%% @private
charset() ->
	C1 = lists:seq($2, $9),
	C2 = lists:seq($a, $k),
	C3 = lists:seq($m, $n),
	C4 = lists:seq($p, $z),
	lists:append([C1, C2, C3, C4]).	

%% @hidden
file_chunk(Log, IODevice, Continuation) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			file:close(IODevice);
		{error, Reason} ->
			file:close(IODevice),
			exit(Reason);
		{Continuation2, Terms} ->
			Fun =  fun(Event) ->
						io:fwrite(IODevice, "~999p~n", [Event])
			end,
			lists:foreach(Fun, Terms),
			file_chunk(Log, IODevice, Continuation2)
	end.

