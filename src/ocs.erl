%%% ocs.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% export the ocs public API
-export([add_client/2, add_client/4, find_client/1, update_client/2,
		update_client/3, get_clients/0, delete_client/1]).
-export([add_subscriber/3, add_subscriber/4, add_subscriber/6,
		find_subscriber/1, delete_subscriber/1, update_password/2,
		update_attributes/2, update_attributes/4, get_subscribers/0]).
-export([add_user/3, add_user/5, add_user/6, list_users/0,
		get_user/1, get_user/3, get_user/4, delete_user/1]).
-export([generate_password/0, generate_identity/0]).
-export([start/4, start/5]).
%% export the ocs private API
-export([authorize/2, normalize/1]).

-export_type([eap_method/0]).

-include("ocs.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(LOGNAME, radius_acct).
-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The ocs public API
%%----------------------------------------------------------------------

-spec add_client(Address, Secret) -> ok
	when
		Address :: inet:ip_address(),
		Secret :: string() | binary().
%% @doc Create an entry in the client table.
%%
add_client(Address, Secret) ->
	add_client(Address, 3799, radius, Secret).

-spec add_client(Address, Port, Protocol, Secret) -> Result
	when
		Address :: inet:ip_address(),
		Port :: inet:port_number(),
		Protocol :: atom(),
		Secret :: string() | binary(),
		Result :: ok.
%% @doc Create an entry in the client table.
%%
add_client(Address, Port, Protocol, Secret) when is_list(Secret),
		is_integer(Port), is_atom(Protocol), Port >= 0 ->
	add_client(Address, Port, Protocol, list_to_binary(Secret));
add_client(Address, Port, Protocol, Secret) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, Port, Protocol, Secret);
add_client(Address, Port, Protocol, Secret) when is_tuple(Address),
		is_binary(Secret) ->
	F = fun() ->
				R = #client{address = Address, port = Port,
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
		Result :: {ok, #client{}} | {error, Reason}, 
		Reason :: notfound | term().
%% @doc Find a client by IP address.
%%
find_client(Address) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	find_client(AddressTuple);
find_client(Address) when is_tuple(Address) ->
	F = fun() ->
				mnesia:read(client, Address, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#client{} = Client]} ->
			{ok, Client};
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
%% @doc Update a client password.
update_client(Address, Password) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	update_client(AddressTuple, Password);
update_client(Address, Password) when is_list(Password) ->
	update_client(Address, list_to_binary(Password));
update_client(Address, Password) ->
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						TS = erlang:system_time(milli_seconds),
						N = erlang:unique_integer([positive]),
						NewEntry = Entry#client{secret = Password, last_modified = {TS,N}},
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

-spec update_client(Address, Port, Protocol)-> Result
	when
		Address :: string() | inet:ip_address(),
		Port :: inet:port_number(),
		Protocol :: radius | diameter,
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update client port and protocol.
update_client(Address, Port, Protocol) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	update_client(AddressTuple, Port, Protocol);
update_client(Address, Port, Protocol)
		when is_tuple(Address), is_integer(Port),
		((Protocol == radius) or (Protocol == diameter)) ->
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						TS = erlang:system_time(milli_seconds),
						N = erlang:unique_integer([positive]),
						NewEntry = Entry#client{port = Port, protocol = Protocol,
								last_modified = {TS, N}},
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
%% @doc Get all clients.
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

-spec add_subscriber(Identity, Password, Attributes) -> Result
	when
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary(),
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Attributes, 0, true, false)
add_subscriber(Identity, Password, Attributes) ->
	add_subscriber(Identity, Password, Attributes, 0, true, false).

-spec add_subscriber(Identity, Password, Attributes, Balance) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Attributes :: radius_attributes:attributes() | binary(),
		Balance :: non_neg_integer(),
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Attributes, Balance, true, false)
add_subscriber(Identity, Password, Attributes, Balance) ->
	add_subscriber(Identity, Password, Attributes, Balance, true, false).

-spec add_subscriber(Identity, Password, Attributes, Balance, EnabledStatus,
		MultiSessions) -> Result
	when 
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | undefined,
		Attributes :: radius_attributes:attributes() | binary(),
		Balance :: non_neg_integer() | undefined,
		EnabledStatus :: boolean() | undefined,
		MultiSessions :: boolean(),
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @doc Create an entry in the subscriber table.
%%
%% 	Authentication will be done using `Password'. An optional list of
%% 	RADIUS `Attributes', to be returned in an `AccessRequest' response,
%% 	may be provided.  These attributes will overide any default values.
%%
%% 	An initial account `Balance' value, `Enabled' status and `MultiSessions'
%% 	status may be provided.
%%
add_subscriber(Identity, Password, Attributes, Balance, undefined, MSessions) ->
	add_subscriber(Identity, Password, Attributes, Balance, true, MSessions);
add_subscriber(Identity, Password, Attributes, undefined, EnabledStatus, MSessions) ->
	add_subscriber(Identity, Password, Attributes, 0, EnabledStatus, MSessions);
add_subscriber(Identity, Password, undefined, Balance, EnabledStatus, MSessions) ->
	add_subscriber(Identity, Password, [], Balance, EnabledStatus, MSessions);
add_subscriber(Identity, undefined, Attributes, Balance, EnabledStatus, MSessions) ->
	add_subscriber(Identity, ocs:generate_password(),
			Attributes, Balance, EnabledStatus, MSessions);
add_subscriber(Identity, Password, Attributes, Balance, EnabledStatus, MSessions)
		when is_list(Identity) ->
	add_subscriber(list_to_binary(Identity), Password, Attributes, Balance,
			EnabledStatus, MSessions);
add_subscriber(Identity, Password, Attributes, Balance, EnabledStatus, MSessions)
		when is_list(Password) ->
	add_subscriber(Identity, list_to_binary(Password), Attributes, Balance,
			EnabledStatus, MSessions);
add_subscriber(undefined, Password, Attributes, Balance, EnabledStatus, MSessions) ->
	F2 = fun() ->
				F1 = fun(_, _, 0) ->
							mnesia:abort(retries);
						(F, Identity, N) ->
							case mnesia:read(subscriber, Identity, read) of
								[] ->
									S = #subscriber{name = Identity,
											password = Password, attributes = Attributes,
											balance = Balance, enabled = EnabledStatus,
											multi_sessions_allowed = MSessions},
									ok = mnesia:write(S),
									S;
								[_] ->
									F(F, list_to_binary(generate_identity()), N - 1)
							end
				end,
				F1(F1, list_to_binary(generate_identity()), 5)
	end,
	case mnesia:transaction(F2) of
		{atomic, Subscriber} ->
			{ok, Subscriber};
		{aborted, Reason} ->
			{error, Reason}
	end;
add_subscriber(Identity, Password, Attributes, Balance, EnabledStatus, MSessions)
		when is_binary(Identity), is_binary(Password), is_list(Attributes),
		is_integer(Balance), Balance >= 0, is_boolean(EnabledStatus) ->
	F1 = fun() ->
				S = #subscriber{name = Identity, password = Password,
						attributes = Attributes, balance = Balance,
						enabled = EnabledStatus, multi_sessions_allowed = MSessions},
				ok = mnesia:write(S),
				S
	end,
	case mnesia:transaction(F1) of
		{atomic, Subscriber} ->
			{ok, Subscriber};
		{aborted, Reason} ->
			{error, Reason}
			end.

-spec find_subscriber(Identity) -> Result  
	when
		Identity :: string() | binary(),
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: not_found | term().
%% @doc Look up an entry in the subscriber table.
find_subscriber(Identity) when is_list(Identity) ->
	find_subscriber(list_to_binary(Identity));
find_subscriber(Identity) when is_binary(Identity) ->
	F = fun() ->
				mnesia:read(subscriber, Identity, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#subscriber{} = Subscriber]} ->
			{ok, Subscriber};
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

-spec delete_subscriber(Identity) -> ok
	when
		Identity :: string() | binary().
%% @doc Delete an entry in the subscriber table.
delete_subscriber(Identity) when is_list(Identity) ->
	delete_subscriber(list_to_binary(Identity));
delete_subscriber(Identity) when is_binary(Identity) ->
	F = fun() ->
		mnesia:delete(subscriber, Identity, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec update_password(Identity, Password)-> Result
	when
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update a new subscriber password
%% @see ocs:generate_password/0
update_password(Identity, Password)
		when is_list(Identity) ->
	update_password(list_to_binary(Identity), Password);
update_password(Identity, Password)
		when is_list(Password) ->
	update_password(Identity, list_to_binary(Password));
update_password(Identity, Password) ->
	F = fun() ->
				case mnesia:read(subscriber, Identity, write) of
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

-spec update_attributes(Identity, Attributes) -> Result
	when
		Identity :: string() | binary(),
		Attributes :: radius_attributes:attributes(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update subscriber attributes.
%%
update_attributes(Identity, Attributes) when is_list(Identity) ->
	update_attributes(list_to_binary(Identity), Attributes);
update_attributes(Identity, Attributes)
		when is_binary(Identity), is_list(Attributes) ->
	F = fun() ->
				case mnesia:read(subscriber, Identity, write) of
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

-spec update_attributes(Identity, Balance, Attributes, EnabledStatus,
		MultiSessions) -> Result
	when
		Identity :: string() | binary(),
		Balance :: pos_integer(),
		Attributes :: radius_attributes:attributes(),
		EnabledStatus :: boolean(),
		MultiSessions :: boolean(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update subscriber attributes.
%%
update_attributes(Identity, Balance, Attributes, EnabledStatus, MSessions)
		when is_list(Identity), is_number(Balance), is_boolean(EnabledStatus),
		is_boolean(MSessions) ->
	update_attributes(list_to_binary(Identity), Balance, Attributes,
		EnabledStatus, MSessions);
update_attributes(Identity, Balance, Attributes, EnabledStatus, MSessions)
		when is_binary(Identity), is_list(Attributes) ->
	F = fun() ->
				case mnesia:read(subscriber, Identity, write) of
					[Entry] ->
						NewEntry = Entry#subscriber{attributes = Attributes,
							balance = Balance, enabled = EnabledStatus,
							multi_sessions_allowed = MSessions},
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

-spec generate_identity() -> string().
%% @equiv generate_identity(7)
generate_identity() ->
	generate_identity(7).

-spec start(Protocol, Type, Address, Port) -> Result
	when
		Protocol :: radius | diameter,
		Type :: auth | acct,
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		Result :: {ok, Pid} | {error, Reason},
		Pid :: pid(),
		Reason :: term().
%% @equiv start(Type, Address, Port, [])
start(Protocol, Type, Address, Port) when is_tuple(Address), is_integer(Port) ->
	start(Protocol, Type, Address, Port, []).

-type eap_method() :: pwd | ttls.
-spec start(Protocol, Type, Address, Port, Options) -> Result
	when
		Protocol :: radius | diameter,
		Type :: auth | acct,
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		Options :: [{eap_method_prefer, EapType} | {eap_method_order, EapTypes}],
		EapType :: eap_method(),
		EapTypes :: [eap_method()],
		Result :: {ok, Pid} | {error, Reason},
		Pid :: pid(),
		Reason :: term().
%% @doc Start a RADIUS/DIAMETER request handler.
start(Protocol, Type, Address, Port, Options) when is_tuple(Address),
		is_integer(Port), is_list(Options) ->
		gen_server:call(ocs, {start, Protocol, Type, Address, Port, Options}).

-spec add_user(Username, Password, UserData) -> Result
	when
		Username :: string(),
		Password :: string(),
		UserData :: list(),
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: term().
%% @equiv add_user(Username, Password, UserData, Address, Port, Dir)
add_user(Username, Password, UserData)
			when is_list(Username), is_list(Password) ->
	{Port, Address, Dir, _} = get_params(),
	add_user(Username, Password, UserData, Address, Port, Dir).

-spec add_user(Username, Password, UserData, Port, Dir) -> Result
	when
		Username :: string(),
		Password :: string(),
		UserData :: list(),
		Port :: inet:port_number(),
		Dir :: string(),
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: term().
%% @equiv add_user(Username, Password, UserData, Address, Port, Dir)
add_user(Username, Password, UserData, Port, Dir)
			when is_list(Username), is_list(Password),
			is_list(UserData), is_integer(Port),
			is_list(Dir) ->
	{_, Address, _, _} = get_params(),
	add_user(Username, Password, UserData, Address, Port, Dir).

-spec add_user(Username, Password, UserData, Address, Port, Dir) -> Result
	when
		Username :: string(),
		Password :: string(),
		UserData :: list(),
		Address :: inet:ip_address() | string() | undefined,
		Port :: inet:port_number(),
		Dir :: string(),
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: term().
%% @equiv mod_auth:add_user(Username, Password, UserData, Address,
%% Port, Dir)
add_user(Username, Password, UserData, Address, Port, Dir)
			when is_list(Username), is_list(Password),
         is_list(UserData), is_integer(Port), is_list(Dir) ->
	LastModified = {erlang:system_time(?MILLISECOND),
			erlang:unique_integer([positive])},
	NewUserData = [{last_modified, LastModified} | UserData],
	case mod_auth:add_user(Username, Password,
			NewUserData, Address, Port, Dir) of
		true ->
			{_, _, _, Group} = get_params(),
			case mod_auth:add_group_member(Group, Username,
					Address, Port, Dir) of
				true ->
					{ok, LastModified};
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

-spec list_users() -> Result
	when
		Result :: {ok, Users} | {error, Reason},
		Users :: list(),
		Reason :: term().
%% @equiv  mod_auth:list_users(Address, Port, Dir)
list_users() ->
	{Port, Address, Dir, _} = get_params(),
	mod_auth:list_users(Address, Port, Dir).

-spec get_user(Username) -> Result
	when
		Username :: string(),
		Result :: {ok, User} | {error, Reason},
		User :: #httpd_user{},
		Reason :: term().
%% @doc Get a user record.
%% @equiv get_user(Username, Address, Port, Dir)
get_user(Username) ->
	{Port, Address, Dir, _GroupName} = get_params(),
	get_user(Username, Address, Port, Dir).

-spec get_user(Username, Port, Dir) -> Result
	when
		Username :: string(),
		Port :: inet:port_number(),
		Dir :: string(),
		Result :: {ok, User} | {error, Reason},
		User :: #httpd_user{},
		Reason :: term().
%% @doc Get a user record.
%% @equiv mod_auth:get_user(Username, Port, Dir)
get_user(Username, Port, Dir) ->
	mod_auth:get_user(Username, Port, Dir).

-spec get_user(Username, Address, Port, Dir) -> Result
	when
		Username :: string(),
		Address :: inet:ip_address() | string() | undefined,
		Port :: inet:port_number(),
		Dir :: string(),
		Result :: {ok, User} | {error, Reason},
		User :: #httpd_user{},
		Reason :: term().
%% @doc Get a user record.
%% @equiv mod_auth:get_user(Username, Address, Port, Dir)
get_user(Username, Address, Port, Dir) ->
	mod_auth:get_user(Username, Address, Port, Dir).

-spec delete_user(Username) -> Result
	when
		Username :: string(),
		Result :: true | {error, Reason},
		Reason :: term().
%% @doc Delete an existing user.
delete_user(Username) ->
	{Port, Address, Dir, GroupName} = get_params(),
	case mod_auth:delete_user(Username, Address, Port, Dir) of
		true ->
			mod_auth:delete_group_member(GroupName, Username, Address, Port, Dir);
		{error, Reason} ->
			{error, Reason}
	end.

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

-spec generate_identity(Length) -> string()
	when
		Length :: pos_integer().
%% @doc Generate a random uniform numeric identity.
%% @private
generate_identity(Length) when Length > 0 ->
	Charset = lists:seq($0, $9),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	generate_identity(Random, Charset, NumChars,[]).
%% @hidden
generate_identity(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	generate_identity(Rest, Charset, NumChars, NewAcc);
generate_identity(<<>>, _Charset, _NumChars, Acc) ->
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

-spec authorize(Identity, Password) -> Result
	when
		Identity :: string() | binary(),
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
authorize(Identity, Password) when is_list(Identity) ->
	authorize(list_to_binary(Identity), Password);
authorize(Identity, Password) when is_list(Password) ->
	authorize(Identity, list_to_binary(Password));
authorize(Identity, Password) when is_binary(Identity),
		is_binary(Password) ->
	F= fun() ->
				case mnesia:read(subscriber, Identity, write) of
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

-spec get_params() -> {Port :: integer(), Address :: string(),
		Directory :: string(), Group :: string()}.
%% @doc Returns configurations details for currently running
%% {@link //inets. httpd} service.
%% @hidden
get_params() ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	case lists:keyfind(require_group, 1, AuthObj) of
		{require_group, [Group | _T]} ->
			{Port, Address, Directory, Group};
		false ->
			exit(not_found)
	end.

