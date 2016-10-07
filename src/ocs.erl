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
-export([add_subscriber/3, add_subscriber/4, find_subscriber/1, delete_subscriber/2,
				update_subscriber_password/3, update_subscriber_attributes/3,
				decrement_subscriber_balance/2]).
-export([log_file/1]).
-export([generate_password/0]).
-export([start/3]).

-include("ocs.hrl").
-define(LOGNAME, radius_acct).

%%----------------------------------------------------------------------
%%  The ocs public API
%%----------------------------------------------------------------------

-spec add_client(Address :: inet:ip_address(), Secret :: string() | binary()) ->
	Result :: ok.
%% @doc Create an entry in the RADIUS client table.
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
			exit(Reason)
	end.

-spec find_client(Address :: inet:ip_address()) ->
	Result :: {ok, Secret :: binary()} | {error, Reason :: not_found | term()}.
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
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec add_subscriber(Subscriber :: string(), Password :: string() | binary(),
		Attributes :: binary() | [byte()]) -> ok | {error, Reason :: term()}.
%% @equiv add_subscriber(Subscriber, Password, Attributes, 0)
add_subscriber(Subscriber, Password, Attributes) ->
	add_subscriber(Subscriber, Password, Attributes, 0).

-spec add_subscriber(Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius:attributes() | binary(),
		Balance :: non_neg_integer()) ->
		ok | {error, Reason :: term()}.
%% @doc Create an entry in the subscriber table.
%%
%% 	Authentication will be done using `Password'. An optional list of
%% 	RADIUS `Attributes', to be returned in an `AccessRequest' response,
%% 	may be provided.  These attributes will overide any default values.
%%
%% 	An initial account `Balance' value may be provided.
%%
add_subscriber(Subscriber, Password, Attributes, Balance)
		when is_list(Subscriber) ->
	add_subscriber(list_to_binary(Subscriber), Password, Attributes, Balance);
add_subscriber(Subscriber, Password, Attributes, Balance)
		when is_list(Password) ->
	add_subscriber(Subscriber, list_to_binary(Password), Attributes, Balance);
add_subscriber(Subscriber, Password, Attributes, Balance)
		when is_list(Attributes) ->
	Bin = radius_attributes:codec(Attributes),
	add_subscriber(Subscriber, Password, Bin, Balance);
add_subscriber(Subscriber, Password, Attributes, Balance)
		when is_binary(Subscriber), is_binary(Password),
		is_binary(Attributes), is_integer(Balance) ->
	F1 = fun(F, <<C, Rest/binary>>)
					when (((C >= $a) and (C =< $z)) or ((C >= $2) and (C =< $9))),
					C /= $i, C /= $l, C /= $o, C /= $u, C /= $v, C /= $0, C /= $1 ->
				F(F, Rest);
			(_, <<_, _/binary>>) ->
				false;
			(_, <<>>) ->
				true
	end,
	case F1(F1, Password) of
		true ->
			F2 = fun() ->
						R = #subscriber{name = Subscriber, password = Password,
								attributes = Attributes, balance = Balance},
						mnesia:write(R)
			end,
			case mnesia:transaction(F2) of
				{atomic, ok} ->
					ok;
				{aborted, Reason} ->
					{error, Reason}
			end;
		false ->
			{error, badarg}
	end.

-spec find_subscriber(Subscriber :: string() | binary()) ->
	Result :: {ok, Password :: binary(), Attributes :: binary(),
	Balance :: integer()} | {error, Reason :: not_found | term()}.
%% @doc Look up an entry in the subscriber tabe.
%%
find_subscriber(Subscriber) when is_list(Subscriber) ->
	find_subscriber(list_to_binary(Subscriber));
find_subscriber(Subscriber) when is_binary(Subscriber) ->
	F = fun() ->
				mnesia:read(subscriber, Subscriber, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#subscriber{password = Password, attributes = Attributes,
				balance = Balance}]} ->
			{ok, Password, Attributes, Balance};
		{atomic, []} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec delete_subscriber(Subscriber :: string() | binary(),
		Password :: binary() | string()) -> ok.
%% @doc Delete a subscriber from the database.
%%
delete_subscriber(Subscriber, Password) when is_list(Subscriber) ->
	delete_subscriber(list_to_binary(Subscriber), Password);
delete_subscriber(Subscriber, Password) when is_list(Password) ->
	delete_subscriber(Subscriber, list_to_binary(Password));
delete_subscriber(Subscriber, Password) when is_binary(Subscriber),
		is_binary(Password) ->
	F = fun() ->
		mnesia:delete(subscriber, Subscriber, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec update_subscriber_password(Subscriber :: string(), OldPassword :: string() | binary(),
	NewPassword :: string() | binary())-> ok.
%% @doc Update a new subscriber password
%% @see ocs:generate_password/0
update_subscriber_password(Subscriber, OldPassword, NewPassword) when is_list(OldPassword) ->
	update_subscriber_password(Subscriber, list_to_binary(OldPassword), NewPassword);
update_subscriber_password(Subscriber, OldPassword, NewPassword) ->
	case find_subscriber(Subscriber) of
		{ok, OldPassword, Attribtes, Balance} ->
			F = fun() ->
				mnesia:delete(subscriber, Subscriber, write)
			end,
			case mnesia:transaction(F) of
				{atomic, []} ->
					exit(not_found);
				{atomic, _} ->
					case add_subscriber(Subscriber, NewPassword, Attribtes, Balance) of
						ok ->
							ok;
						{error, Reason} ->
							exit(Reason)
					end;
				{aborted, Reason} ->
					exit(Reason)
			end;
		{ok, _, _, _} ->
			exit(current_password_not_matched);
		{error, Reason} ->
			exit(Reason)
	end.

-spec decrement_subscriber_balance(Subscriber :: string(), Usage :: non_neg_integer()) ->
	{ok, NewBalance :: integer()} .
%% @doc Decrements subscriber's current balance
decrement_subscriber_balance(Subscriber, Usage) when is_list(Subscriber),
		is_number(Usage) ->
	case find_subscriber(Subscriber) of
		{ok, Password, Attributes, Balance} ->
			F = fun() ->
				mnesia:delete(subscriber, Subscriber, write)
			end,
			case mnesia:transaction(F) of
				{atomic, []} ->
					exit(not_found);
				{atomic, _} ->
					NewBalance = Balance - Usage,
					case add_subscriber(Subscriber, Password, Attributes, NewBalance) of
						ok ->
							{ok, NewBalance};
						{error, Reason} ->
							exit(Reason)
					end;
				{aborted, Reason} ->
					exit(Reason)
			end;
		{error, Reason} ->
			exit(Reason)
	end.


-spec update_subscriber_attributes(Subscriber :: string(), Password :: string() | binary(),
		Attributes :: binary() | [byte()]) -> ok.
%% @doc Update subscriber attributes.
%%
update_subscriber_attributes(Subscriber, Password, Attributes) when is_list(Password) ->
	update_subscriber_attributes(Subscriber, list_to_binary(Password), Attributes);
update_subscriber_attributes(Subscriber, Password, Attributes) when is_list(Subscriber),
		is_binary(Password), (is_list(Attributes) orelse is_binary(Attributes)) ->
	case find_subscriber(Subscriber) of
		{ok, Password, _, Balance} ->
			F = fun() ->
				mnesia:delete(subscriber, Subscriber, write)
			end,
			case mnesia:transaction(F) of
				{atomic, []} ->
					exit(not_found);
				{atomic, _} ->
					case add_subscriber(Subscriber, Password, Attributes, Balance) of
						ok ->
							ok;
						{error, Reason} ->
							exit(Reason)
					end;
				{aborted, Reason} ->
					exit(Reason)
			end;
		{ok, _, _, _} ->
			exit(current_password_not_matched);
		{error, Reason} ->
			exit(Reason)
	end.

-spec log_file(FileName :: string()) -> ok.
%% @doc Write all logged accounting records to a file.
%%
log_file(FileName) when is_list(FileName) ->
   {ok, IODevice} = file:open(FileName, [write]),
   file_chunk(?LOGNAME, IODevice, start).

-type password() :: [50..57 | 97..104 | 106..107 | 109..110 | 112..116 | 119..122].
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
	C2 = lists:seq($a, $h),
	C3 = lists:seq($j, $k),
	C4 = lists:seq($m, $n),
	C5 = lists:seq($p, $t),
	C6 = lists:seq($w, $z),
	lists:append([C1, C2, C3, C4, C5, C6]).

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

