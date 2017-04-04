%%% ocs.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%html%%
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
-export([add_client/2, add_client/4, find_client/1, update_client/2,
		update_client/3, get_clients/0, delete_client/1]).
-export([add_subscriber/3, add_subscriber/4, add_subscriber/5,
		find_subscriber/1, delete_subscriber/1, update_password/2,
		update_attributes/2, update_attributes/4, get_subscribers/0]).
-export([generate_password/0]).
-export([start/4]).
%% export the ocs private API
-export([authorize/2, normalize/1]).

-export_type([eap_method/0]).

-include("ocs.hrl").
-define(LOGNAME, radius_acct).
-define(CHUNKSIZE, 100).

%%----------------------------------------------------------------------
%%  The ocs public API
%%----------------------------------------------------------------------

-spec add_client(Address, Secret) -> ok
	when
		Address :: inet:ip_address(),
		Secret :: string() | binary().
%% @doc Create an entry in the RADIUS client table.
%%
add_client(Address, Secret) ->
	add_client(Address, 3799, radius, Secret).

-spec add_client(Address, DisconnectPort, Protocol, Secret) -> Result
	when
		Address :: inet:ip_address(),
		DisconnectPort :: inet:port_number(),
		Protocol :: atom(),
		Secret :: string() | binary(),
		Result :: ok.
%% @doc Create an entry in the RADIUS client table.
%%
add_client(Address, DiscPort, Protocol, Secret) when is_list(Secret), is_integer(DiscPort), is_atom(Protocol) ->
	add_client(Address, DiscPort, Protocol, list_to_binary(Secret));
add_client(Address, DiscPort, Protocol, Secret) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, DiscPort, Protocol, Secret);
add_client(Address, DiscPort, Protocol, Secret) when is_tuple(Address), is_binary(Secret) ->
	F = fun() ->
				R = #client{address = Address, disconnect_port = DiscPort,
						protocol = Protocol, secret = Secret},
				mnesia:write(R)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec find_client(Address) -> Result
	when
		Address :: inet:ip_address(),
		Result :: {ok, DisconnectPort, Protocol, Secret} | {error, Reason}, 
		DisconnectPort :: inet:port_number(),
		Protocol :: atom(),
		Secret :: binary(),
		Reason :: notfound | term().
%% @doc Look up the shared secret for a RADIUS client.
%%
find_client(Address) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	find_client(AddressTuple);
find_client(Address) when is_tuple(Address) ->
	F = fun() ->
				mnesia:read(client, Address, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#client{disconnect_port = DiscPort,
				protocol = Protocol, secret = Secret}]} ->
			{ok, DiscPort, Protocol, Secret};
		{atomic, []} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec update_client(Address, Password)-> Result
	when
		Address :: string() | inet:ip_address(),
		Password :: string() | binary(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update client password
update_client(Address, Password) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	update_client(AddressTuple, Password);
update_client(Address, Password) when is_list(Password) ->
	update_client(Address, list_to_binary(Password));
update_client(Address, Password) ->
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						NewEntry = Entry#client{secret = Password},
						mnesia:write(client, NewEntry, write);
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

-spec update_client(Address, DisconnectPort, Protocol)-> Result
	when
		Address :: string() | inet:ip_address(),
		DisconnectPort :: inet:port_number(),
		Protocol :: atom(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update client attributes
update_client(Address, DiscPort, Protocol) when is_list(Address),
			is_integer(DiscPort), is_atom(Protocol)  ->
	{ok, AddressTuple} = inet_parse:address(Address),
	update_client(AddressTuple, DiscPort, Protocol);
update_client(Address, DiscPort, Protocol) when is_tuple(Address) ->
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						NewEntry = Entry#client{disconnect_port = DiscPort, protocol = Protocol},
						mnesia:write(client, NewEntry, write);
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

-spec get_clients() -> Result
	when
		Result :: [#client{}] | {error, Reason},
		Reason :: term().
%% @doc Get all RADIUS clients.
get_clients()->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(client, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Clients, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Clients | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			Result
	end.

-spec delete_client(Client) -> ok
	when
		Client :: string() | inet:ip_address().
%% @doc Delete an entry from the  client table.
delete_client(Client) when is_list(Client) ->
	{ok, ClientT} = inet:parse_address(Client),
	delete_client(ClientT);
delete_client(Client) when is_tuple(Client) ->
	F = fun() ->
		mnesia:delete(client, Client, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec add_subscriber(Subscriber, Password, Attributes) -> Result
	when
		Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Subscriber, Password, Attributes, 0, true)
add_subscriber(Subscriber, Password, Attributes) ->
	add_subscriber(Subscriber, Password, Attributes, 0, true).

-spec add_subscriber(Subscriber, Password, Attributes, Balance) -> Result
	when 
		Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary(),
		Balance :: non_neg_integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Subscriber, Password, Attributes, Balance, true)
add_subscriber(Subscriber, Password, Attributes, Balance) ->
	add_subscriber(Subscriber, Password, Attributes, Balance, true).

-spec add_subscriber(Subscriber, Password, Attributes, Balance, EnabledStatus) -> Result
	when 
		Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary(),
		Balance :: non_neg_integer(),
		EnabledStatus :: boolean(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Create an entry in the subscriber table.
%%
%% 	Authentication will be done using `Password'. An optional list of
%% 	RADIUS `Attributes', to be returned in an `AccessRequest' response,
%% 	may be provided.  These attributes will overide any default values.
%%
%% 	An initial account `Balance' value and `Enabled' status may be provided.
%%
add_subscriber(Subscriber, Password, Attributes, Balance, EnabledStatus)
		when is_list(Subscriber), is_boolean(EnabledStatus) ->
	add_subscriber(list_to_binary(Subscriber), Password, Attributes, Balance,
			EnabledStatus);
add_subscriber(Subscriber, Password, Attributes, Balance, EnabledStatus)
		when is_list(Password) ->
	add_subscriber(Subscriber, list_to_binary(Password), Attributes, Balance,
			EnabledStatus);
add_subscriber(Subscriber, Password, Attributes, Balance, EnabledStatus)
		when is_binary(Subscriber), is_binary(Password),
		is_list(Attributes), is_integer(Balance), Balance >= 0 ->
	F1 = fun() ->
				R = #subscriber{name = Subscriber, password = Password,
						attributes = Attributes, balance = Balance,
						enabled = EnabledStatus},
				mnesia:write(R)
	end,
	case mnesia:transaction(F1) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
			end.

-spec find_subscriber(Subscriber) -> Result  
	when
		Result :: {ok, Password, Attributes, Balance, Enabled} | {error, Reason},
		Password :: binary(),
		Subscriber :: string() | binary(),
		Attributes :: radius_attributes:attributes(),
		Balance :: integer(),
		Enabled :: boolean(),
		Reason :: not_found | term().
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

-spec get_subscribers() -> Result
	when
		Result :: [#subscriber{}] | {error, Reason},
		Reason :: term().
%% @doc Get all entries in the subscriber table.
get_subscribers()->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(subscriber, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Subscribers, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Subscribers | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			Result
	end.

-spec delete_subscriber(Subscriber) -> ok
	when
		Subscriber :: string() | binary().
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

-spec update_password(Subscriber, Password)-> Result
	when
		Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
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

-spec update_attributes(Subscriber, Attributes) -> Result
	when
		Subscriber :: string() | binary(),
		Attributes :: radius_attributes:attributes(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update subscriber attributes.
%%
update_attributes(Subscriber, Attributes) when is_list(Subscriber) ->
	update_attributes(list_to_binary(Subscriber), Attributes);
update_attributes(Subscriber, Attributes)
		when is_binary(Subscriber), is_list(Attributes) ->
	F = fun() ->
				case mnesia:read(subscriber, Subscriber, write) of
					[Entry] ->
						NewEntry = Entry#subscriber{attributes = Attributes},
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

-spec update_attributes(Subscriber, Balance, Attributes, EnabledStatus) -> Result
	when
		Subscriber :: string() | binary(),
		Balance :: pos_integer(),
		Attributes :: radius_attributes:attributes(),
		EnabledStatus :: boolean(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
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

-type password() :: [50..57 | 97..104 | 106..107 | 109..110 | 112..116 | 119..122].
-spec generate_password() -> password().
%% @equiv generate_password(12)
generate_password() ->
	generate_password(12).

-spec start(Type, Address, Port, LogRotateTime) -> Result
	when
		Type :: auth | acct,
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		LogRotateTime :: non_neg_integer(),
		Result :: {ok, Pid} | {error, Reason},
		Pid :: pid(),
		Reason :: term().
%% @equiv start(Type, Address, Port, [])
start(Type, Address, Port, LogRotateTime) when is_tuple(Address), is_integer(Port),
		is_integer(LogRotateTime)->
	start(Type, Address, Port, LogRotateTime, []).

-type eap_method() :: pwd | ttls.
-spec start(Type, Address, Port, LogRotateTime, Options) -> Result
	when
		Type :: auth | acct,
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		LogRotateTime :: non_neg_integer(),
		Options :: [{eap_method_prefer, EapType} | {eap_method_order, EapTypes}],
		EapType :: eap_method(),
		EapTypes :: [eap_method()],
		Result :: {ok, Pid} | {error, Reason},
		Pid :: pid(),
		Reason :: term().
%% @doc Start a RADIUS request handler.
start(Type, Address, Port, LogRotateTime, Options) when is_tuple(Address),
		is_integer(Port), is_integer(LogRotateTime), is_list(Options) ->
		gen_server:call(ocs, {start, Type, Address, Port, LogRotateTime, Options}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec generate_password(Length) -> password()
	when 
		Length :: pos_integer().
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

-spec charset() -> Charset
	when
		Charset :: password().
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

-spec authorize(Subscriber, Password) -> Result
	when
		Subscriber :: string() | binary(),
		Password :: string() | binary(),
		Result :: {ok, Password, Attributes} | {error, Reason},
		Attributes :: radius_attributes:attributes(),
		Reason :: out_of_credit | disabled | bad_password | not_found | term().
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

-spec normalize(String) -> string()
	when
		String :: string().
%% @doc Strip non hex digits and convert to lower case.
%% @private
normalize(String) ->
	normalize(String, []).
%% @hidden
normalize([Char | T], Acc) when Char >= 48, Char =< 57 ->
	normalize(T, [Char | Acc]);
normalize([Char | T], Acc) when Char >= 97, Char =< 102 ->
	normalize(T, [Char | Acc]);
normalize([$A | T], Acc) ->
	normalize(T, [$a | Acc]);
normalize([$B | T], Acc) ->
	normalize(T, [$b | Acc]);
normalize([$C | T], Acc) ->
	normalize(T, [$c | Acc]);
normalize([$D | T], Acc) ->
	normalize(T, [$d | Acc]);
normalize([$E | T], Acc) ->
	normalize(T, [$e | Acc]);
normalize([$F | T], Acc) ->
	normalize(T, [$f | Acc]);
normalize([_ | T], Acc) ->
	normalize(T, Acc);
normalize([], Acc) ->
	lists:reverse(Acc).

