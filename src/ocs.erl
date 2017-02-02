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
-export([add_client/2, find_client/1, update_client/2, get_clients/0,
				 delete_client/1]).
-export([add_subscriber/3, add_subscriber/4, find_subscriber/1,
				delete_subscriber/1, update_password/2, update_attributes/4,
				get_subscribers/0]).
-export([log_file/1]).
-export([generate_password/0]).
-export([start/3]).
%% export the ocs private API
-export([authorize/2]).

-export_type([eap_method/0]).

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

-spec update_client(Address :: string() | inet:ip_address(),
		Password :: string() | binary())->
	ok | {error, Reason :: not_found | term()}.
%% @doc Update client password
update_client(Address, Password) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	update_client(AddressTuple, Password);
update_client(Address, Password) when is_list(Password) ->
	update_client(Address, list_to_binary(Password));
update_client(Address, Password) ->
	F = fun() ->
				case mnesia:read(radius_client, Address, write) of
					[Entry] ->
						NewEntry = Entry#radius_client{secret = Password},
						mnesia:write(radius_client, NewEntry, write);
					[] ->
						throw(not_found)
				end
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec get_clients() -> Result :: [#radius_client{}] |
	{error, Reason :: term()}.
%% @doc Get all RADIUS clients
get_clients()->
	F1 = fun(Sub, Acc)->  [Sub | Acc] end,
	F2 = fun()-> mnesia:foldl(F1, [], radius_client) end,
	case mnesia:transaction(F2) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Clients} ->
			Clients
	end.

-spec delete_client(Client :: string() | inet:ip_address()) -> ok.
%% @doc Delete an entry from the  client table.
delete_client(Client) when is_list(Client) ->
	{ok, ClientT} = inet:parse_address(Client),
	delete_client(ClientT);
delete_client(Client) when is_tuple(Client) ->
	F = fun() ->
		mnesia:delete(radius_client, Client, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec add_subscriber(Subscriber :: string() | binary(), Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary()) ->
	ok | {error, Reason :: term()}.
%% @equiv add_subscriber(Subscriber, Password, Attributes, 0)
add_subscriber(Subscriber, Password, Attributes) ->
	add_subscriber(Subscriber, Password, Attributes, 0).

-spec add_subscriber(Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary(),
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
		when is_binary(Subscriber), is_binary(Password),
		is_list(Attributes), is_integer(Balance) ->
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
	Result :: {ok, Password :: binary(),
			Attributes :: radius_attributes:attributes(),
			Balance :: integer(),
			Enabled :: boolean()}
			| {error, Reason :: not_found | term()}.
%% @doc Look up an entry in the subscriber table.
find_subscriber(Subscriber) when is_list(Subscriber) ->
	find_subscriber(list_to_binary(Subscriber));
find_subscriber(Subscriber) when is_binary(Subscriber) ->
	F = fun() ->
				mnesia:read(subscriber, Subscriber, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#subscriber{password = Password, attributes = Attributes,
				balance = Balance, enabled = Enabled}]} ->
			{ok, Password, Attributes, Balance, Enabled};
		{atomic, []} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec get_subscribers() -> Result :: [#subscriber{}] |
	{error, Reason :: term()}.
%% @doc Get all entries in the subscriber table.
get_subscribers()->
	F1 = fun(Sub, Acc)->  [Sub | Acc] end,
	F2 = fun()-> mnesia:foldl(F1, [], subscriber) end,
	case mnesia:transaction(F2) of
		{aborted, Reason} -> 
			{error, Reason};
		{atomic, Subscribers} ->
			Subscribers
	end.

-spec delete_subscriber(Subscriber :: string() | binary()) -> ok.
%% @doc Delete an entry in the subscriber table.
delete_subscriber(Subscriber) when is_list(Subscriber) ->
	delete_subscriber(list_to_binary(Subscriber));
delete_subscriber(Subscriber) when is_binary(Subscriber) ->
	F = fun() ->
		mnesia:delete(subscriber, Subscriber, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec update_password(Subscriber :: string() | binary(),
		Password :: string() | binary())->
	ok | {error, Reason :: not_found | term()}.
%% @doc Update a new subscriber password
%% @see ocs:generate_password/0
update_password(Subscriber, Password)
		when is_list(Subscriber) ->
	update_password(list_to_binary(Subscriber), Password);
update_password(Subscriber, Password)
		when is_list(Password) ->
	update_password(Subscriber, list_to_binary(Password));
update_password(Subscriber, Password) ->
	F = fun() ->
				case mnesia:read(subscriber, Subscriber, write) of
					[Entry] ->
						NewEntry = Entry#subscriber{password = Password},
						mnesia:write(subscriber, NewEntry, write);
					[] ->
						throw(not_found)
				end
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec update_attributes(Subscriber :: string() | binary(),
		Balance :: pos_integer(), Attributes :: radius_attributes:attributes(),
		EnabledStatus :: boolean()) ->
	ok | {error, Reason :: not_found | term()}.
%% @doc Update subscriber attributes.
%%
update_attributes(Subscriber, Balance, Attributes, EnabledStatus)
		when is_list(Subscriber), is_number(Balance), is_boolean(EnabledStatus) ->
	update_attributes(list_to_binary(Subscriber), Balance, Attributes,
		EnabledStatus);
update_attributes(Subscriber, Balance, Attributes, EnabledStatus)
		when is_binary(Subscriber), is_list(Attributes) ->
	F = fun() ->
				case mnesia:read(subscriber, Subscriber, write) of
					[Entry] ->
						NewEntry = Entry#subscriber{attributes = Attributes,
							balance = Balance, enabled = EnabledStatus},
						mnesia:write(subscriber, NewEntry, write);
					[] ->
						throw(not_found)
				end
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
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
%% @equiv start(Type, Address, Port, [])
start(Type, Address, Port) when is_tuple(Address), is_integer(Port) ->
	start(Type, Address, Port, []).

-type eap_method() :: pwd | ttls.
-spec start(Type :: auth | acct, Address :: inet:ip_address(),
		Port :: pos_integer(),
		Options :: [{eap_method_prefer, EapType :: eap_method()} |
		{eap_method_order, EapTypes :: [eap_method()]}]) ->
	{ok, Pid :: pid()} | {error, Reason :: term()}.
%% @doc Start a RADIUS request handler.
start(Type, Address, Port, Options) when is_tuple(Address),
		is_integer(Port), is_list(Options) ->
	gen_server:call(ocs, {start, Type, Address, Port, Options}).

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

-spec authorize(Subscriber :: string() | binary(),
		Password :: string() | binary()) ->
	{ok, Password :: binary(), Attributes :: radius_attributes:attributes()} |
	{error, Reason :: out_of_credit | disabled | bad_password |
					not_found | term()}.
%% @doc Authorize a subscriber based on `enabled' and `balance' fields.
%%
%% 	If the subscriber `enabled' field true and have sufficient `balance'
%%		set disconnect field to false and return `password' and `attributes'
%%		or return the error reason.
%% @private
authorize(Subscriber, Password) when is_list(Subscriber) ->
	authorize(list_to_binary(Subscriber), Password);
authorize(Subscriber, Password) when is_list(Password) ->
	authorize(Subscriber, list_to_binary(Password));
authorize(Subscriber, Password) when is_binary(Subscriber),
		is_binary(Password) ->
	F= fun() ->
				case mnesia:read(subscriber, Subscriber, write) of
					[#subscriber{password = Password, attributes = Attributes,
							enabled = true, disconnect = false} =
							Entry ] when Entry#subscriber.balance > 0 ->
						{Password, Attributes};
					[#subscriber{password = Password, attributes = Attributes,
							enabled = true, disconnect = true} =
							Entry] when Entry#subscriber.balance > 0 ->
						NewEntry = Entry#subscriber{disconnect = false},
						mnesia:write(subscriber, NewEntry, write),
						{Password, Attributes};
					[#subscriber{password = MTPassword, attributes = Attributes,
							enabled = true, disconnect = false} = Entry ] when
								Entry#subscriber.balance > 0,
								Password == <<>>,
								MTPassword =/= Password ->
						{MTPassword, Attributes};
					[#subscriber{password = MTPassword, attributes = Attributes,
							enabled = true, disconnect = true} = Entry] when
								Entry#subscriber.balance > 0,
								Password == <<>>,
								MTPassword =/= Password ->
						NewEntry = Entry#subscriber{disconnect = false},
						mnesia:write(subscriber, NewEntry, write),
						{MTPassword, Attributes};
					[#subscriber{password = Password, enabled = false}] ->
						throw(disabled);
					[#subscriber{password = Password} = Entry] when
							Entry#subscriber.balance =< 0 ->
						throw(out_of_credit);
					[#subscriber{}] ->
						throw(bad_password);
					[] ->
						throw(not_found)
				end
	end,
	case mnesia:transaction(F) of
		{atomic, {SubPassword, Attributes}} ->
			{ok, SubPassword, Attributes};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.

