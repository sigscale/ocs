%%% ocs.erl
%%% vim: ts=3
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
		update_client/3, get_clients/0, delete_client/1, query_clients/6]).
-export([add_subscriber/3, add_subscriber/4, add_subscriber/5,
		add_subscriber/6, add_subscriber/8,
		find_subscriber/1, delete_subscriber/1, update_password/2,
		update_attributes/2, update_attributes/5, get_subscribers/0,
		query_subscriber/1]).
-export([add_user/3, list_users/0, get_user/1, delete_user/1,
		query_users/3, update_user/3]).
-export([add_product/1, find_product/1, get_products/0, delete_product/1,
		query_product/7]).
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

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The ocs public API
%%----------------------------------------------------------------------

-spec add_client(Address, Secret) -> Result
	when
		Address :: inet:ip_address(),
		Secret :: string() | binary(),
		Result :: {ok, #client{}}.
%% @doc Create an entry in the client table.
%%
add_client(Address, Secret) ->
	add_client(Address, 3799, radius, Secret).

-spec add_client(Address, Port, Protocol, Secret) -> Result
	when
		Address :: inet:ip_address(),
		Port :: inet:port_number() | undefined,
		Protocol :: atom() | undefined,
		Secret :: string() | binary() | undefined,
		Result :: {ok, #client{}}.
%% @doc Create an entry in the client table.
%%
add_client(Address, Port, Protocol, Secret) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, Port, Protocol, Secret);
add_client(Address, undefined, diameter, undefined)
		when is_tuple(Address) ->
	F = fun() ->
				TS = erlang:system_time(?MILLISECOND),
				N = erlang:unique_integer([positive]),
				R = #client{address = Address,
						protocol = diameter, last_modified = {TS, N}},
				mnesia:write(R),
				R
	end,
	case mnesia:transaction(F) of
		{atomic, Client} ->
			{ok, Client};
		{aborted, Reason} ->
			exit(Reason)
	end;
add_client(Address, Port, Protocol, undefined) ->
	add_client(Address, Port, Protocol, generate_password());
add_client(Address, Port, undefined, Secret) ->
	add_client(Address, Port, radius, Secret);
add_client(Address, undefined, Protocol, Secret) ->
	add_client(Address, 3799, Protocol, Secret);
add_client(Address, Port, Protocol, Secret) when is_list(Secret) ->
	add_client(Address, Port, Protocol, list_to_binary(Secret));
add_client(Address, Port, radius, Secret) when is_tuple(Address),
		is_binary(Secret) ->
	F = fun() ->
				TS = erlang:system_time(?MILLISECOND),
				N = erlang:unique_integer([positive]),
				LM = {TS, N},
				R = #client{address = Address, port = Port,
						protocol = radius, secret = Secret,
						last_modified = LM},
				ok = mnesia:write(R),
				R
	end,
	case mnesia:transaction(F) of
		{atomic, Client} ->
			{ok, Client};
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
						TS = erlang:system_time(?MILLISECOND),
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
		Port :: inet:port_number() | undefined,
		Protocol :: radius | diameter,
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update client port and protocol.
update_client(Address, Port, Protocol) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	update_client(AddressTuple, Port, Protocol);
update_client(Address, Port, Protocol) when is_tuple(Address),
		(((Protocol == radius) and is_integer(Port))
		or ((Protocol == diameter) and (Port == undefined))) ->
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						TS = erlang:system_time(?MILLISECOND),
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

-spec query_clients(Cont, Address, Identifier, Port, Protocol, Secret) -> Result
	when
		Cont :: start | eof | any(),
		Address :: undefined | string(),
		Identifier :: undefined | string(),
		Port :: undefined | string(),
		Protocol :: undefined | string(),
		Secret :: undefined | string(),
		Result :: {Cont, [#client{}]} | {error, Reason},
		Reason :: term().
query_clients(start, Address, Identifier, Port, Protocol, Secret) ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun() ->
		mnesia:select(client, MatchSpec, read)
	end,
	case mnesia:transaction(F) of
		{atomic, Clients} ->
			query_clients1(Clients, Address, Identifier, Port, Protocol, Secret);
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
query_clients1(Clients, Address, Identifier, Port, Protocol, undefined) ->
	query_clients2(Clients, Address, Identifier, Port, Protocol);
query_clients1(Clients, Address, Identifier, Port, Protocol, Secret) ->
	SecretBin = list_to_binary(Secret),
	SecretLen = size(SecretBin),
	Fun = fun(#client{secret = S}) ->
			case S of
				<<SecretBin:SecretLen/binary, _/binary>> ->
					true;
				_ ->
					false
			end
	end,
	FilteredClients = lists:filter(Fun, Clients),
	query_clients2(FilteredClients, Address, Identifier, Port, Protocol).
%% @hidden
query_clients2(Clients, Address, Identifier, Port, undefined) ->
	query_clients3(Clients, Address, Identifier, Port);
query_clients2(Clients, Address, Identifier, Port, Protocol) ->
	P1 = string:to_upper(Protocol),
	Fun = fun(#client{protocol = P}) ->
				P2 = string:to_upper(atom_to_list(P)),
				lists:prefix(P1, P2)
	end,
	FilteredClients = lists:filter(Fun, Clients),
	query_clients3(FilteredClients, Address, Identifier, Port).
%% @hidden
query_clients3(Clients, Address, Identifier, undefined) ->
	query_clients4(Clients, Address, Identifier);
query_clients3(Clients, Address, Identifier, Port) ->
	Fun = fun(#client{port = P}) -> lists:prefix(Port, integer_to_list(P)) end,
	FilteredClients = lists:filter(Fun, Clients),
	query_clients4(FilteredClients, Address, Identifier).
%% @hidden
query_clients4(Clients, Address, undefined) ->
	query_clients4(Clients, Address);
query_clients4(Clients, Address, Identifier) ->
	IdBin = list_to_binary(Identifier),
	IdLen = size(IdBin),
	Fun = fun(#client{identifier = I}) ->
			case I of
				<<IdBin:IdLen/binary, _/binary>> ->
					true;
				_ ->
					false
			end
	end,
	FilteredClients = lists:filter(Fun, Clients),
	query_clients4(FilteredClients, Address).
%% @hidden
query_clients4(Clients, undefined) ->
	{eof, Clients};
query_clients4(Clients, Address) ->
	Fun = fun(#client{address = A}) -> lists:prefix(Address, inet:ntoa(A)) end,
	{eof, lists:filter(Fun, Clients)}.

-spec add_subscriber(Identity, Password, Product) -> Result
	when
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Product :: string() | undefined,
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Product, [], [], [], true, false)
add_subscriber(Identity, Password, Product) ->
	add_subscriber(Identity, Password, Product, [], [], [], true, false).

-spec add_subscriber(Identity, Password, Product, Characteristics) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Product :: string() | undefined,
		Characteristics :: [tuple()],
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Product, Characteristics, Buckets, [], true, false)
add_subscriber(Identity, Password, Product, Characteristics) ->
	add_subscriber(Identity, Password, Product, Characteristics, [], [], true, false).

-spec add_subscriber(Identity, Password, Product, Characteristics, Buckets) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Product :: string() | undefined,
		Characteristics :: [tuple()],
		Buckets :: [#bucket{}],
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Product, Characteristics, Buckets, [], true, false)
add_subscriber(Identity, Password, Product, Characteristics, Buckets) ->
	add_subscriber(Identity, Password, Product, Characteristics, Buckets, [], true, false).

-spec add_subscriber(Identity, Password, Product,
		Characteristics, Buckets, Attributes) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Product :: string() | undefined,
		Characteristics :: [tuple()],
		Buckets :: [#bucket{}],
		Attributes :: radius_attributes:attributes() | binary(),
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Product,
%%		Characteristics, Buckets, Attributes, true, false)
add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes) ->
	add_subscriber(Identity, Password, Product,
			Characteristics, Buckets, Attributes, true, false).

-spec add_subscriber(Identity, Password, Product,
		Characteristics, Buckets, Attributes, EnabledStatus, MultiSessions) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | undefined,
		Product :: string() | undefined,
		Characteristics :: [tuple()] | undefined,
		Buckets :: [#bucket{}] | undefined,
		Attributes :: radius_attributes:attributes() | binary(),
		EnabledStatus :: boolean() | undefined,
		MultiSessions :: boolean() | undefined,
		Result :: {ok, #subscriber{}} | {error, Reason},
		Reason :: term().
%% @doc Create an entry in the subscriber table.
%%
%% 	Authentication will be done using `Password'. An optional list of
%% 	RADIUS `Attributes', to be returned in an `AccessRequest' response,
%% 	may be provided.  These attributes will overide any default values.
%%
%% 	An initial account `Bucket', `Product' key for product reference
%%		`Enabled' status and `MultiSessions' status may be provided.
%%
add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, EnabledStatus, undefined) ->
	add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, EnabledStatus, false);
add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, undefined, MultiSession) ->
	add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, true, MultiSession);
add_subscriber(Identity, Password, Product, Characteristics, Buckets, undefined, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, Product, Characteristics, Buckets, [], EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Product, Characteristics, undefined, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, Product, Characteristics, [], Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Product, undefined, Buckets, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, Product, [], Buckets, Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, undefined, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, [], Characteristics, Buckets, Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, undefined, Product, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, ocs:generate_password(),
			Product, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_list(Identity) ->
	add_subscriber(list_to_binary(Identity), Password, Product, Characteristics, Buckets,
			Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_list(Password) ->
	add_subscriber(Identity, list_to_binary(Password), Product, Characteristics, Buckets,
			Attributes, EnabledStatus, MultiSession);
add_subscriber(undefined, Password, Product, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_binary(Password), is_list(Product), is_list(Buckets), is_list(Attributes),
		is_boolean(EnabledStatus), is_boolean(MultiSession) ->
	Now = erlang:system_time(?MILLISECOND),
	F1 = fun() ->
				ProductId = case Product of
					[] ->
						case mnesia:first(product) of
							'$end_of_table' ->
								throw(product_not_found);
							ProdId ->
								ProdId
						end;
					Product ->
						Product
				end,
				case mnesia:read(product, ProductId, read) of
					[#product{} = P] ->
						N = erlang:unique_integer([positive]),
						S1 = #subscriber{password = Password,
						attributes = Attributes, buckets = Buckets,
						enabled = EnabledStatus, multisession = MultiSession,
						last_modified = {Now, N}},
						S2 = subscription(S1, P, Characteristics),
						F3 = fun(_, _, 0) ->
									mnesia:abort(retries);
								(F, Identity, I) ->
									case mnesia:read(subscriber, Identity, read) of
										[] ->
											S3 = S2#subscriber{name = Identity},
											ok = mnesia:write(S3),
											S3;
										[_] ->
											F(F, list_to_binary(generate_identity()), I - 1)
									end
						end,
						F3(F3, list_to_binary(generate_identity()), 5);
					[] ->
						throw(product_not_found)
				end
	end,
	case mnesia:transaction(F1) of
		{atomic, Subscriber} ->
			{ok, Subscriber};
		{aborted, Reason} ->
			{error, Reason}
	end;
add_subscriber(Identity, Password, Product, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_binary(Identity), is_binary(Password), is_list(Product),
		is_list(Buckets), is_list(Attributes), is_boolean(EnabledStatus), is_boolean(MultiSession) ->
	Now = erlang:system_time(?MILLISECOND),
	F1 = fun() ->
				ProductId = case Product of
					[] ->
						case mnesia:first(product) of
							'$end_of_table' ->
								throw(product_not_found);
							ProdId ->
								ProdId
						end;
					Product ->
						Product
				end,
				case mnesia:read(product, ProductId, read) of
					[#product{} = P] ->
						N = erlang:unique_integer([positive]),
						S1 = #subscriber{name = Identity, password = Password,
						attributes = Attributes, buckets = Buckets,
						enabled = EnabledStatus, multisession = MultiSession,
						last_modified = {Now, N}},
						S2 = subscription(S1, P, Characteristics),
						ok = mnesia:write(S2),
						S2;
					[] ->
						throw(product_not_found)
				end
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

-spec query_subscriber(Cont) -> Result
	when
		Cont :: start | eof | any(),
		Result :: {Cont, [#product{}]} | {error, Reason},
		Reason :: term().
%% @doc Query product inventories
query_subscriber(start) ->
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
			{eof, Result}
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

-spec update_attributes(Identity, Buckets, Attributes, EnabledStatus,
		MultiSessions) -> Result
	when
		Identity :: string() | binary(),
		Buckets :: [#bucket{}],
		Attributes :: radius_attributes:attributes(),
		EnabledStatus :: boolean(),
		MultiSessions :: boolean(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Update subscriber attributes.
%%
update_attributes(Identity, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_list(Identity), is_list(Buckets), is_boolean(EnabledStatus),
		is_boolean(MultiSession) ->
	update_attributes(list_to_binary(Identity), Buckets, Attributes,
		EnabledStatus, MultiSession);
update_attributes(Identity, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_binary(Identity), is_list(Attributes) ->
	F = fun() ->
				case mnesia:read(subscriber, Identity, write) of
					[Entry] ->
						TS = erlang:system_time(?MILLISECOND),
						N = erlang:unique_integer([positive]),
						NewBuckets = [B#bucket{last_modified = {TS, N}} || B <- Buckets],
						NewEntry = Entry#subscriber{attributes = Attributes,
							buckets = NewBuckets, enabled = EnabledStatus,
							multisession = MultiSession, last_modified = {TS, N}},
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

-spec add_product(Product) -> Result
	when
		Product :: #product{},
		Result :: {ok, #product{}} | {error, Reason},
		Reason :: validation_failed | term().
%% @doc Add a new entry in product table.
add_product(#product{price = Prices} = Product) when length(Prices) > 0 ->
	Fvala = fun(undefined) ->
				true;
			(#alteration{name = Name, type = one_time, period = undefined,
					amount = Amount}) when length(Name) > 0, is_integer(Amount) ->
				true;
			(#alteration{name = Name, type = recurring, period = Period,
					amount = Amount}) when length(Name) > 0, ((Period == hourly)
					or (Period == daily) or (Period == weekly)
					or (Period == monthly) or (Period == yearly)),
					is_integer(Amount) ->
				true;
			(#alteration{name = Name, type = usage, period = undefined,
					units = Units, size = Size, amount = Amount})
					when length(Name) > 0, ((Units == octets)
					or (Units == seconds)), is_integer(Size), Size > 0,
					is_integer(Amount) ->
				true;
			(#alteration{}) ->
				false
	end,
	Fvalp = fun(#price{name = Name, type = one_time, period = undefined,
					amount = Amount, alteration = Alteration})
					when length(Name) > 0, is_integer(Amount), Amount > 0 ->
				Fvala(Alteration);
			(#price{name = Name, type = recurring, period = Period,
					amount = Amount, alteration = Alteration})
					when length(Name) > 0, ((Period == hourly)
					or (Period == daily) or (Period == weekly)
					or (Period == monthly) or (Period == yearly)),
					is_integer(Amount), Amount > 0 ->
				Fvala(Alteration);
			(#price{name = Name, type = usage, period = undefined,
					units = Units, size = Size,
					amount = Amount, alteration = Alteration})
					when length(Name) > 0, ((Units == octets)
					or (Units == seconds)), is_integer(Size), Size > 0,
					is_integer(Amount), Amount > 0 ->
				Fvala(Alteration);
			(#price{}) ->
				false
	end,
	case lists:all(Fvalp, Prices) of
		true ->
			add_product1(Product);
		false ->
			{error, validation_failed}
	end.
%% @hidden
add_product1(Product) ->
	Fadd = fun() ->
		TS = erlang:system_time(?MILLISECOND),
		N = erlang:unique_integer([positive]),
		Product1 = Product#product{last_modified = {TS, N}},
		ok = mnesia:write(product, Product1, write),
		Product1
	end,
	case mnesia:transaction(Fadd) of
		{atomic, Product2} ->
			{ok, Product2};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_product(ProductID) -> Result
	when
		ProductID :: string(),
		Result :: {ok, Product} | {error, Reason},
		Product :: #product{},
		Reason :: term().
%% @doc Find product by product id
find_product(ProductID) ->
	F = fun() ->
		case mnesia:read(product, ProductID) of
			[Entry] ->
				Entry;
			[] ->
				throw(not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, Product} ->
			{ok, Product};
		{aborted, {throw, not_found}} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec get_products() -> Result
	when
		Result :: [#product{}] | {error, Reason},
		Reason :: term().
%% @doc Get all entries in the product table.
get_products() ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(product, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Product, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Product | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			Result
	end.

-spec delete_product(ProductID) -> Result
	when
		ProductID :: string(),
		Result :: ok.
%% @doc Delete an entry from the product table.
delete_product(ProductID) ->
	F = fun() ->
		mnesia:delete(product, ProductID, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec query_product(Cont, Name, Description, Status, SDT, EDT, Price) -> Result
	when
		Cont :: start | eof | any(),
		Name :: undefined | '_' | string(),
		Description :: undefined | '_' | string(),
		Status :: undefined | '_' | atom(),
		SDT :: undefined | '_' | string() | integer(),
		EDT :: undefined | '_' | string() | integer(),
		Price :: undefined | '_' | string(),
		Result :: {Cont, [#product{}]} | {error, Reason},
		Reason :: term().
%% @doc Query product entires
query_product(Con, Name, Description, Status, STD, EDT, undefined) ->
	query_product(Con, Name, Description, Status, STD, EDT, '_');
query_product(Con, Name, Description, Status, STD, undefined, Price) ->
	query_product(Con, Name, Description, Status, STD, '_', Price);
query_product(Con, Name, Description, Status, undefined, EDT, Price) ->
	query_product(Con, Name, Description, Status, '_', EDT, Price);
query_product(Con, Name, Description, undefined, SDT, EDT, Price) ->
	query_product(Con, Name, Description, '_', SDT, EDT, Price);
query_product(Con, Name, undefined, Status, SDT, EDT, Price) ->
	query_product(Con, Name, '_', Status, SDT, EDT, Price);
query_product(Con, undefined, Description, Status, SDT, EDT, Price) ->
	query_product(Con, '_', Description, Status, SDT, EDT, Price);
query_product(Con, Name, Description, Status, SDT, EDT, Price) when is_list(EDT) ->
	ISOEDT = ocs_rest:iso8601(EDT),
	query_product(Con, Name, Description, Status, SDT, ISOEDT, Price);
query_product(Con, Name, Description, Status, SDT, EDT, Price) when is_list(SDT) ->
	ISOSDT = ocs_rest:iso8601(SDT),
	query_product(Con, Name, Description, Status, ISOSDT, EDT, Price);
query_product(start, Name, Description, Status, SDT, EDT, Price) ->
	MatchHead = #product{name = Name, description = Description,
			start_date = SDT, end_date = EDT, is_bundle = '_',
			status = Status, specification = '_', price = '_', last_modified = '_'},
	MatchSpec = MatchSpec = [{MatchHead, [], ['$_']}],
	F = fun() ->
		mnesia:select(product, MatchSpec, read)
	end,
	case mnesia:transaction(F) of
		{atomic, Products} ->
			query_product1(Products, Price, []);
		{aborted, Reason} ->
			{error, Reason}
	end;
query_product(eof, _Name, _Description, _Status, _STD, _EDT, _Price) ->
	{eof, []}.
%% @hidden
query_product1([], _, Acc) ->
	{eof, lists:reverse(Acc)};
query_product1(Products, '_', _) ->
	{eof, Products};
query_product1([#product{price = Prices} = Product | T], PriceName, Acc) ->
	case lists:keyfind(PriceName, #price.name, Prices) of
		false ->
			query_product1(T, PriceName, Acc);
		_ ->
			query_product1(T, PriceName, [Product | Acc])
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

-spec add_user(Username, Password, Locale) -> Result
	when
		Username :: string(),
		Password :: string(),
		Locale :: string(),
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: user_exists | term().
%% @doc Add an HTTP user.
%% 	HTTP Basic authentication (RFC7617) is required with
%% 	`Username' and  `Password' used to construct the
%% 	`Authorization' header in requests.
%%
%% 	`Locale' is used to set the language for text in the web UI.
%% 	For English use `"en"', for Spanish use `"es'"..
%%
add_user(Username, Password, Language) when is_list(Username),
		is_list(Password), is_list(Language) ->
	{Port, Address, Dir, _} = get_params(),
	case ocs:get_user(Username) of
		{error, no_such_user} ->
			TS = erlang:system_time(?MILLISECOND),
			N = erlang:unique_integer([positive]),
			NewUserData = [{last_modified, {TS, N}}, {locale, Language}],
			case mod_auth:add_user(Username, Password,
					NewUserData, Address, Port, Dir) of
				true ->
					{_, _, _, Group} = get_params(),
					case mod_auth:add_group_member(Group, Username,
							Address, Port, Dir) of
						true ->
							{ok, {TS, N}};
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason};
		{ok, _} ->
			{error, user_exists}
	end.

-spec list_users() -> Result
	when
		Result :: {ok, Users} | {error, Reason},
		Users :: [Username],
		Username :: string(),
		Reason :: term().
%% @doc List HTTP users.
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
%% @doc Get an HTTP user record.
%% @equiv mod_auth:get_user(Username, Address, Port, Dir)
get_user(Username) ->
	{Port, Address, Dir, _} = get_params(),
	mod_auth:get_user(Username, Address, Port, Dir).

-spec delete_user(Username) -> Result
	when
		Username :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete an existing HTTP user.
delete_user(Username) ->
	{Port, Address, Dir, GroupName} = get_params(),
	case mod_auth:delete_user(Username, Address, Port, Dir) of
		true ->
			delete_user1(GroupName, Username, Address, Port, Dir);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
delete_user1(GroupName, Username, Address, Port, Dir) ->
	case mod_auth:delete_group_member(GroupName,
			Username, Address, Port, Dir) of
		true ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

-spec update_user(Username, Password, Language) -> Result
	when
		Username :: string(),
		Password :: string(),
		Language :: string(),
		Result :: {ok, LM} | {error, Reason},
		LM :: {integer(), integer()},
		Reason :: term().
%% @hidden Update user password and language
update_user(Username, Password, Language) ->
	case get_user(Username) of
		{error, Reason} ->
			{error, Reason};
		{ok, #httpd_user{}} ->
			case delete_user(Username) of
				ok ->
					case add_user(Username, Password, Language) of
						{ok, LM} ->
							{ok, LM};
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end
	end.

-spec query_users(Cont, Id, Locale) -> Result
	when
		Cont :: start | eof | any(),
		Id :: undefined | string(),
		Locale :: undefined | string(),
		Result :: {Cont, [#httpd_user{}]} | {error, Reason},
		Reason :: term().
query_users(start, Id, Locale) ->
	MatchSpec = MatchSpec = [{'_', [], ['$_']}],
	F = fun() ->
		mnesia:select(httpd_user, MatchSpec, read)
	end,
	case mnesia:transaction(F) of
		{atomic, Users} ->
			query_users1(Users, Id, Locale);
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
query_users1([], _, _) ->
	{eof, []};
query_users1(Users, undefined, Locale) ->
	query_users2(Users, Locale);
query_users1(Users, Id, Locale) ->
	F = fun(#httpd_user{username = Username}) when element(1, Username) =:= Id ->
				true;
			(_) ->
				false
	end,
	case lists:filter(F, Users) of
		[] ->
			{error, not_found};
		FilteredUsers ->
			query_users2(FilteredUsers, Locale)
	end.
%% @hidden
query_users2(Users, undefined) ->
	{eof, Users};
query_users2(Users, Locale) ->
	F2 = fun(#httpd_user{user_data = C}) ->
				case lists:keyfind(locale, 1, C) of
					{_, Locale} -> true;
					_ -> false
				end;
			(_) ->
					false
	end,
	{eof, lists:filter(F2, Users)}.

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
		Result :: {ok, #subscriber{}} | {disabled, SessionsList} | {error, Reason},
		SessionsList :: [{TimeStamp, SessionAttributes}],
		TimeStamp :: integer(),
		SessionAttributes :: [tuple()],
		Reason :: out_of_credit | bad_password | not_found | term().
%% @doc Authorize a subscriber.
%%
%% 	If the subscriber `enabled' field is `true' and have sufficient balance
%%		set `disconnect' field to `false' and return {ok, #subscriber{password = `PSK'}}
%% 	where `PSK' is used for `Mikrotik-Wireless-Psk' and `Attributes' are
%% 	additional attributes to be returned in an `Access-Accept' response.
%% @private
authorize(Identity, Password) when is_list(Identity) ->
	authorize(list_to_binary(Identity), Password);
authorize(Identity, Password) when is_list(Password) ->
	authorize(Identity, list_to_binary(Password));
authorize(Identity, Password) when is_binary(Identity),
		is_binary(Password) ->
	F= fun() ->
				case mnesia:read(subscriber, Identity, write) of
					[#subscriber{buckets = Buckets, enabled = Enabled,
							disconnect = Disconnect} = Entry] ->
						Now = erlang:system_time(?MILLISECOND),
						F2 = fun(#bucket{remain_amount = Amount,
											termination_date = TD}) when
													TD =/= undefined;
													TD > Now,
													Amount > 0 ->
										true;
									(_) ->
										false
						end,
						case lists:any(F2, Buckets) of
							true ->
								case {Enabled, Disconnect, Entry#subscriber.password} of
									{true, false, Password} ->
										Entry;
									{true, true, Password} ->
										NewEntry = Entry#subscriber{disconnect = false},
										ok = mnesia:write(subscriber, NewEntry, write),
										NewEntry;
									{true, false, MTPassword} when
											Password == <<>>,
											MTPassword =/= Password ->
										Entry;
									{true, true, MTPassword} when
											Password == <<>>,
											MTPassword =/= Password ->
										NewEntry = Entry#subscriber{disconnect = false},
										ok = mnesia:write(subscriber, NewEntry, write),
										NewEntry;
									{false, _, Password} ->
										SessionsList = Entry#subscriber.session_attributes,
										NewEntry = Entry#subscriber{session_attributes = []},
										ok = mnesia:write(subscriber, NewEntry, write),
										{disabled, SessionsList};
									{_, _, _} ->
										throw(bad_password)
								end;
							false ->
								throw(out_of_credit)
						end;
					[] ->
						throw(not_found)
				end
	end,
	case mnesia:transaction(F) of
		{atomic, #subscriber{} = S} ->
			{ok, S};
		{atomic, SessionAttributes} ->
			{disabled, SessionAttributes};
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

-spec charge(Amount, Buckets) -> Buckets
	when
		Amount :: non_neg_integer(),
		Buckets :: [#bucket{}].
%% @doc Charge `Amount' to `Buckets'.
%% @private
charge(Amount, Buckets) ->
	charge(Amount, Buckets, []).
%% @hidden
charge(0, T, Acc) ->
	lists:reverse(Acc) ++ T;
charge(Amount, [#bucket{units = cents,
		remain_amount = Remain} = B | T], Acc) when Amount < Remain ->
	lists:reverse(Acc) ++ [B#bucket{remain_amount = Remain - Amount} | T];
charge(Amount, [#bucket{units = cents,
		remain_amount = Remain} = B], Acc) ->
	lists:reverse([B#bucket{remain_amount = Remain - Amount} | Acc]);
charge(Amount, [#bucket{units = cents,
		remain_amount = Remain} | T], Acc) ->
	charge(Amount - Remain, T, Acc);
charge(Amount, [H | T], Acc) ->
	charge(Amount, T, [H | Acc]);
charge(Amount, [], Acc) ->
	lists:reverse([#bucket{units = cents, remain_amount = - Amount} | Acc]).

-spec date(MilliSeconds) -> DateTime
	when
		MilliSeconds :: pos_integer(),
		DateTime :: calendar:datetime().
%% @doc Convert timestamp to date and time.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds).

-spec end_period(StartTime, Period) -> EndTime
	when
		StartTime :: non_neg_integer(),
		Period :: hourly | daily | weekly | monthly | yearly,
		EndTime :: non_neg_integer().
%% @doc Calculate end of period.
end_period(StartTime, Period) when is_integer(StartTime) ->
	end_period1(date(StartTime), Period).
%% @hidden
end_period1({Date, {23, Minute, Second}}, hourly) ->
	NextDay = calendar:date_to_gregorian_days(Date) + 1,
	EndDate = calendar:gregorian_days_to_date(NextDay),
	EndTime = {0, Minute, Second},
	calendar:datetime_to_gregorian_seconds({EndDate, EndTime}) * 1000 - 1;
end_period1({Date, {Hour, Minute, Second}}, hourly) ->
	EndTime = {Hour + 1, Minute, Second},
	calendar:datetime_to_gregorian_seconds({Date, EndTime}) * 1000 - 1;
end_period1({Date, Time}, daily) ->
	NextDay = calendar:date_to_gregorian_days(Date) + 1,
	EndDate = calendar:gregorian_days_to_date(NextDay),
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({Date, Time}, weekly) ->
	NextDay = calendar:date_to_gregorian_days(Date) + 7,
	EndDate = calendar:gregorian_days_to_date(NextDay),
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({{Year, 1, 31}, Time}, monthly) ->
	NextDay = calendar:last_day_of_the_month(Year, 2),
	EndDate = {Year, 2, NextDay},
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({{Year, 2, Day}, Time}, monthly) when Day < 28 ->
	EndDate = {Year, 3, Day},
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({{Year, 2, Day}, Time}, monthly) ->
	EndDate = case calendar:last_day_of_the_month(Year, 2) of
		Day ->
			{Year, 3, 31};
		_ ->
			{Year, 3, Day}
	end,
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({{Year, 3, 31}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 4, 30}, Time}) * 1000 - 1;
end_period1({{Year, 4, 30}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 5, 31}, Time}) * 1000 - 1;
end_period1({{Year, 5, 31}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 6, 30}, Time}) * 1000 - 1;
end_period1({{Year, 6, 31}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 7, 31}, Time}) * 1000 - 1;
end_period1({{Year, 8, 31}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 9, 30}, Time}) * 1000 - 1;
end_period1({{Year, 9, 30}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 10, 31}, Time}) * 1000 - 1;
end_period1({{Year, 10, 30}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 11, 31}, Time}) * 1000 - 1;
end_period1({{Year, 11, 30}, Time}, monthly) ->
	calendar:datetime_to_gregorian_seconds({{Year, 12, 31}, Time}) * 1000 - 1;
end_period1({{Year, 12, Day}, Time}, monthly) ->
	EndDate = {Year + 1, 1, Day},
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({{Year, Month, Day}, Time}, monthly) ->
	EndDate = {Year, Month + 1, Day},
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1;
end_period1({{Year, Month, Day}, Time}, yearly) ->
	EndDate = {Year + 1, Month, Day},
	calendar:datetime_to_gregorian_seconds({EndDate, Time}) * 1000 - 1.

-spec subscription(Subscriber, Product, Characteristics) ->
		Subscriber
	when
		Subscriber :: #subscriber{},
		Product :: #product{},
		Characteristics :: [tuple()] | undefined.
%% @doc Prepare buckets and add allowances.
%% @private
subscription(#subscriber{last_modified = {Now, _}} = Subscriber,
		#product{name = ProductName, price = Prices} = _Product,
		Characteristics) ->
	subscription(Subscriber, ProductName, Characteristics, Now, Prices).
%% @hidden
subscription(#subscriber{buckets = Buckets} = Subscriber,
		ProductName, Characteristics, Now,
		[#price{type = recurring,
		period = Period, amount = SubscriptionAmount,
		alteration = #alteration{units = Units, size = Size,
		amount = AllowanceAmount}} | T]) when Period /= undefined,
		Units == octets; Units == seconds ->
	NewBuckets = charge(SubscriptionAmount + AllowanceAmount,
			[#bucket{units = Units, remain_amount = Size,
			termination_date = end_period(Now, Period)} | Buckets]),
	subscription(Subscriber#subscriber{buckets = NewBuckets},
			ProductName, Characteristics, Now, T);
subscription(#subscriber{buckets = Buckets} = Subscriber,
		ProductName, Characteristics, Now, [#price{type = usage,
		alteration = #alteration{type = recurring,
		period = Period, units = Units, size = Size,
		amount = Amount}} | T]) when Period /= undefined,
		Units == octets; Units == seconds ->
	NewBuckets = charge(Amount, [#bucket{units = Units,
			remain_amount = Size,
			termination_date = end_period(Now, Period)} | Buckets]),
	subscription(Subscriber#subscriber{buckets = NewBuckets},
			ProductName, Characteristics, Now, T);
subscription(Subscriber, ProductName, Characteristics, Now, [_ | T]) ->
	subscription(Subscriber, ProductName, Characteristics, Now, T);
subscription(#subscriber{buckets = Buckets} = Subscriber,
		ProductName, Characteristics, Now, []) ->
	NewBuckets = [B#bucket{last_modified = {Now,
			erlang:unique_integer([positive])}} || B <- Buckets],
	P = #product_instance{start_date = Now,
			product = ProductName, characteristics = Characteristics,
			last_modified = {Now, erlang:unique_integer([positive])}},
	Subscriber#subscriber{buckets = NewBuckets, product = P}.

