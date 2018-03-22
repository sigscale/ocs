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
-export([add_client/2, add_client/3, add_client/5, find_client/1,
		update_client/2, update_client/3, get_clients/0, delete_client/1,
		query_clients/6]).
-export([add_service/3, add_service/4, add_service/6,
		add_product/2, add_product/4]).
-export([add_subscriber/3, add_subscriber/4, add_subscriber/5,
		add_subscriber/6, add_subscriber/8, find_service/1,
		delete_service/1, get_services/0, query_service/1]).
-export([add_bucket/2, find_bucket/1, delete_bucket/1]).
-export([add_user/3, list_users/0, get_user/1, delete_user/1,
		query_users/3, update_user/3]).
-export([add_offer/1, find_offer/1, get_offers/0, delete_offer/1,
		query_offer/7]).
-export([add_pla/1, add_pla/2, find_pla/1, get_plas/0, delete_pla/1, query_table/6]).
-export([generate_password/0, generate_identity/0]).
-export([start/4, start/5]).
%% export the ocs private API
-export([authorize/3, normalize/1, subscription/4]).

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
%% @equiv add_client(Address, 3799, radius, Secret, true)
%% @doc Create an entry in the client table.
%%
add_client(Address, Secret) ->
	add_client(Address, 3799, radius, Secret, true).

-spec add_client(Address, Secret, PasswordRequired) -> Result
	when
		Address :: inet:ip_address(),
		Secret :: string() | binary(),
		PasswordRequired :: boolean(),
		Result :: {ok, #client{}}.
%% @equiv add_client(Address, 3799, radius, Secret, PasswordRequired)
add_client(Address, Secret, PasswordRequired) ->
	add_client(Address, 3799, radius, Secret, PasswordRequired).

-spec add_client(Address, Port, Protocol, Secret, PasswordRequired) -> Result
	when
		Address :: inet:ip_address(),
		Port :: inet:port_number() | undefined,
		Protocol :: atom() | undefined,
		Secret :: string() | binary() | undefined,
		PasswordRequired :: boolean(),
		Result :: {ok, # client{}}.
%% @doc Create an entry in the client table.
%%
add_client(Address, Port, Protocol, Secret, PasswordRequired) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, Port, Protocol, Secret, PasswordRequired);
add_client(Address, Port, Protocol, Secret, undefined) ->
	add_client(Address, Port, Protocol, Secret, true);
add_client(Address, undefined, diameter, undefined, PasswordRequired)
		when is_tuple(Address), is_boolean(PasswordRequired) ->
	F = fun() ->
				TS = erlang:system_time(?MILLISECOND),
				N = erlang:unique_integer([positive]),
				R = #client{
						address = Address,
						protocol = diameter, last_modified = {TS, N},
						password_required = PasswordRequired},
				mnesia:write(R),
				R
	end,
	case mnesia:transaction(F) of
		{atomic, Client} ->
			{ok, Client};
		{aborted, Reason} ->
			exit(Reason)
	end;
add_client(Address, Port, Protocol, undefined, PasswordRequired) ->
	add_client(Address, Port, Protocol, generate_password(), PasswordRequired);
add_client(Address, Port, undefined, Secret, PasswordRequired) ->
	add_client(Address, Port, radius, Secret, PasswordRequired);
add_client(Address, undefined, Protocol, Secret, PasswordRequired) ->
	add_client(Address, 3799, Protocol, Secret, PasswordRequired);
add_client(Address, Port, Protocol, Secret, PasswordRequired) when is_list(Secret) ->
	add_client(Address, Port, Protocol, list_to_binary(Secret), PasswordRequired);
add_client(Address, Port, radius, Secret, PasswordRequired) when
		is_tuple(Address), is_binary(Secret), is_boolean(PasswordRequired) ->
	F = fun() ->
				TS = erlang:system_time(?MILLISECOND),
				N = erlang:unique_integer([positive]),
				LM = {TS, N},
				R = #client{address = Address, port = Port,
						protocol = radius, secret = Secret,
						password_required = PasswordRequired,
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

-spec add_product(Offer, Characteristics) -> Result
	when
		Offer :: string(),
		Characteristics :: [tuple()],
		Result :: {ok, #product{}} | {error, Reason},
		Reason :: term().
%% @equiv add_product(Offer, undefined, undefined, Characteristics)
add_product(Offer, Characteristics) ->
	add_product(Offer, undefined, undefined, Characteristics).

-spec add_product(Offer, StartDate, EndDate, Characteristics) -> Result
	when
		Offer :: string(),
		StartDate :: undefined | pos_integer(),
		EndDate :: undefined | pos_integer(),
		Characteristics :: [tuple()],
		Result :: {ok, #product{}} | {error, Reason},
		Reason :: term().
%% @doc Add a product invenotry subscription instance.
add_product(Offer, StartDate, EndDate, Characteristics)
		when (is_integer(StartDate) orelse (StartDate == undefined)),
		(is_integer(EndDate) orelse (EndDate == undefined)),
		is_list(Characteristics), is_list(Offer) ->
	F = fun() ->
			case mnesia:read(offer, Offer, read) of
				[#offer{char_value_use = CharValueUse}] ->
					TS = erlang:system_time(?MILLISECOND),
					N = erlang:unique_integer([positive]),
					LM = {TS, N},
					Id = ocs_rest:etag(LM),
					NewChars = default_chars(CharValueUse, Characteristics),
					Product = #product{id = Id, product = Offer, start_date = StartDate,
							termination_date = EndDate, characteristics = NewChars,
							last_modified = LM},
					ok = mnesia:write(Product),
					Product;
				[] ->
					throw(offer_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, Product} ->
			{ok, Product};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec add_service(Identity, Password, ProductRef) -> Result
	when
		Identity :: string() | binary(),
		Password :: string() | binary(),
		ProductRef :: string() | undefined,
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_service(Identity, Password, ProductRef, [], true, false)
add_service(Identity, Password, ProductRef) ->
	add_service(Identity, Password, ProductRef, [], true, false).

-spec add_service(Identity, Password, ProductRef, Attributes) -> Result
	when
		Identity :: string() | binary(),
		Password :: string() | binary(),
		ProductRef :: string() | undefined,
		Attributes :: radius_attributes:attributes() | binary(),
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_service(Identity, Password, ProductRef, Attributes, true, false)
add_service(Identity, Password, ProductRef, Attributes) ->
	add_service(Identity, Password, ProductRef, Attributes, true, false).

-spec add_service(Identity, Password, ProductRef,
		Attributes, EnabledStatus, MultiSessions) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | undefined,
		ProductRef :: string() | undefined,
		Attributes :: radius_attributes:attributes() | binary(),
		EnabledStatus :: boolean() | undefined,
		MultiSessions :: boolean() | undefined,
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @doc Create an entry in the service table.
%%
%% 	Authentication will be done using `Password'. An optional list of
%% 	RADIUS `Attributes', to be returned in an `AccessRequest' response,
%% 	may be provided.  These attributes will overide any default values.
%%
%% 	`ProductRef' key for product invenotry reference,
%%		`Enabled' status and `MultiSessions' status may be provided.
%%
add_service(Identity, Password, ProductRef, Attributes, EnabledStatus, undefined) ->
	add_service(Identity, Password, ProductRef, Attributes, EnabledStatus, false);
add_service(Identity, Password, ProductRef, Attributes, undefined, MultiSession) ->
	add_service(Identity, Password, ProductRef, Attributes, true, MultiSession);
add_service(Identity, Password, ProductRef, undefined, EnabledStatus, MultiSession) ->
	add_service(Identity, Password, ProductRef, [], EnabledStatus, MultiSession);
add_service(Identity, Password, undefined, Attributes, EnabledStatus, MultiSession) ->
	add_service(Identity, Password, [], Attributes, EnabledStatus, MultiSession);
add_service(Identity, undefined, ProductRef, Attributes, EnabledStatus, MultiSession) ->
	add_service(Identity, ocs:generate_password(), ProductRef, Attributes, EnabledStatus, MultiSession);
add_service(Identity, Password, ProductRef, Attributes, EnabledStatus, MultiSession) when is_list(Identity) ->
	add_service(list_to_binary(Identity), Password, ProductRef, Attributes, EnabledStatus, MultiSession);
add_service(Identity, Password, ProductRef, Attributes, EnabledStatus, MultiSession) when is_list(Password) ->
	add_service(Identity, list_to_binary(Password), ProductRef, Attributes, EnabledStatus, MultiSession);
add_service(undefined, Password, ProductRef, Attributes, EnabledStatus, MultiSession) when is_binary(Password),
		is_list(ProductRef), is_list(Attributes), is_boolean(EnabledStatus), is_boolean(MultiSession) ->
	F1 = fun() ->
			case mnesia:read(product, ProductRef, write) of
				[#product{service = ServiceRefs} = P1] ->
					Now = erlang:system_time(?MILLISECOND),
					N = erlang:unique_integer([positive]),
					S1 = #service{password = Password,
						product = ProductRef,
						attributes = Attributes,
						enabled = EnabledStatus,
						multisession = MultiSession,
						last_modified = {Now, N}},
					F3 = fun(_, _, 0) ->
								mnesia:abort(retries);
							(F, Identity, I) ->
								case mnesia:read(service, Identity, read) of
									[] ->
										S2 = S1#service{name = Identity},
										ok = mnesia:write(S2),
										P2 = P1#product{service = [Identity | ServiceRefs]},
										ok = mnesia:write(P2),
										S2;
									[_] ->
										F(F, list_to_binary(generate_identity()), I - 1)
								end
					end,
					F3(F3, list_to_binary(generate_identity()), 5);
				[] ->
					throw(product_inventory_not_found)
			end
	end,
	case mnesia:transaction(F1) of
		{atomic, Service} ->
			{ok, Service};
		{aborted, Reason} ->
			{error, Reason}
	end;
add_service(Identity, Password, ProductRef, Attributes, EnabledStatus, MultiSession)
		when is_binary(Identity), size(Identity) > 0, is_binary(Password), is_list(ProductRef),
		is_list(Attributes), is_boolean(EnabledStatus), is_boolean(MultiSession) ->
	F1 = fun() ->
				case mnesia:read(product, ProductRef, read) of
					[#product{service = ServiceRefs} = P1] ->
						Now = erlang:system_time(?MILLISECOND),
						N = erlang:unique_integer([positive]),
						P2 = P1#product{service = [Identity | ServiceRefs]},
						ok = mnesia:write(P2),
						S1 = #service{name = Identity,
								password = Password,
								product = ProductRef,
								attributes = Attributes,
								enabled = EnabledStatus,
								multisession = MultiSession,
								last_modified = {Now, N}},
						ok = mnesia:write(S1),
						S1;
					[] ->
						throw(offer_not_found)
				end
	end,
	case mnesia:transaction(F1) of
		{atomic, Service} ->
			{ok, Service};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec add_bucket(ProductRef, Bucket) -> Result
	when
		ProductRef :: string(),
		Bucket :: #bucket{},
		Result :: {ok, BucketBefore, BucketAfter} | {error, Reason},
		BucketBefore :: #bucket{},
		BucketAfter :: #bucket{},
		Reason :: term().
%% @doc Add a new bucket to bucket table or update exsiting bucket
add_bucket(ProductRef, #bucket{id = undefined} = Bucket) when is_list(ProductRef) ->
	F = fun() ->
		case mnesia:read(product, ProductRef, write) of
			[#product{balance = B} = P] ->
				BId = generate_bucket_id(),
				Bucket1  = Bucket#bucket{id = BId},
				ok = mnesia:write(bucket, Bucket1, write),
				Product = P#product{balance = lists:reverse([BId | B])},
				ok = mnesia:write(product, Product, write),
				{ok, undefined, Bucket1};
			[] ->
				throw(product_not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, {ok, OldBucket, NewBucket}} ->
			{ok, OldBucket, NewBucket};
		{aborted, Reason} ->
			{error, Reason}
	end;
add_bucket(ProductRef, #bucket{id = BId, product = ProdRef1,
		remain_amount = RAmount1, termination_date = TD} = _Bucket)
		when is_list(ProductRef) ->
	F = fun() ->
		case mnesia:read(product, ProductRef, write) of
			[#product{balance = B} = P] ->
				case mnesia:read(bucket, BId, write) of
					[#bucket{product = ProdRef2,
							remain_amount = RAmount2} = Bucket2] ->
						ProdRef3 = ProdRef2 ++ [ProdRef1 -- ProdRef2],
						Bucket3  = Bucket2#bucket{id = BId, product = ProdRef3,
							remain_amount = RAmount2 + RAmount1, termination_date = TD},
						ok = mnesia:write(bucket, Bucket3, write),
						case lists:any(fun(Id) when Id == BId -> true; (_) -> false end, B) of
							true ->
								Product = P#product{balance = lists:reverse([BId | B])},
								ok = mnesia:write(product, Product, write),
								{ok, Bucket2, Bucket3};
							false ->
								{ok, Bucket2, Bucket3}
						end;
					[] ->
						throw(bucket_not_found)
				end;
			[] ->
				throw(product_not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, {ok, OldBucket, NewBucket}} ->
			{ok, OldBucket, NewBucket};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_bucket(BucketId) -> Result
	when
		BucketId :: term(),
		Result :: {ok, Bucket} | {error, Reason},
		Bucket :: #bucket{},
		Reason :: not_found | term().
%% @doc Look up an entry in the bucket table.
find_bucket(BucketId) ->
	F = fun() -> mnesia:read(bucket, BucketId, read) end,
	case mnesia:transaction(F) of
		{atomic, [#bucket{} = B]} ->
			{ok, B};
		{atomic, []} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec delete_bucket(BucketId) -> ok
	when
		BucketId :: term().
%% @doc Delete entry in the bucket table.
delete_bucket(BucketId) ->
	F = fun() -> mnesia:delete(bucket, BucketId, write) end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.


-spec add_subscriber(Identity, Password, Offer) -> Result
	when
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Offer :: string() | undefined,
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Offer, [], [], [], true, false)
add_subscriber(Identity, Password, Offer) ->
	add_subscriber(Identity, Password, Offer, [], [], [], true, false).

-spec add_subscriber(Identity, Password, Offer, Characteristics) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Offer :: string() | undefined,
		Characteristics :: [tuple()],
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Offer, Characteristics, Buckets, [], true, false)
add_subscriber(Identity, Password, Offer, Characteristics) ->
	add_subscriber(Identity, Password, Offer, Characteristics, [], [], true, false).

-spec add_subscriber(Identity, Password, Offer, Characteristics, Buckets) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Offer :: string() | undefined,
		Characteristics :: [tuple()],
		Buckets :: [#bucket{}],
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Offer, Characteristics, Buckets, [], true, false)
add_subscriber(Identity, Password, Offer, Characteristics, Buckets) ->
	add_subscriber(Identity, Password, Offer, Characteristics, Buckets, [], true, false).

-spec add_subscriber(Identity, Password, Offer,
		Characteristics, Buckets, Attributes) -> Result
	when 
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Offer :: string() | undefined,
		Characteristics :: [tuple()],
		Buckets :: [#bucket{}],
		Attributes :: radius_attributes:attributes() | binary(),
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_subscriber(Identity, Password, Offer,
%%		Characteristics, Buckets, Attributes, true, false)
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes) ->
	add_subscriber(Identity, Password, Offer,
			Characteristics, Buckets, Attributes, true, false).

-spec add_subscriber(Identity, Password, Offer,
		Characteristics, Buckets, Attributes, EnabledStatus, MultiSessions) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | undefined,
		Offer :: string() | undefined,
		Characteristics :: [tuple()] | undefined,
		Buckets :: [#bucket{}] | undefined,
		Attributes :: radius_attributes:attributes() | binary(),
		EnabledStatus :: boolean() | undefined,
		MultiSessions :: boolean() | undefined,
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @doc Create an entry in the service table.
%%
%% 	Authentication will be done using `Password'. An optional list of
%% 	RADIUS `Attributes', to be returned in an `AccessRequest' response,
%% 	may be provided.  These attributes will overide any default values.
%%
%% 	An initial account `Bucket', `Offer' key for offer reference
%%		`Enabled' status and `MultiSessions' status may be provided.
%%
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, EnabledStatus, undefined) ->
	add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, EnabledStatus, false);
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, undefined, MultiSession) ->
	add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, true, MultiSession);
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, undefined, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, Offer, Characteristics, Buckets, [], EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Offer, Characteristics, undefined, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, Offer, Characteristics, [], Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Offer, undefined, Buckets, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, Offer, [], Buckets, Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, undefined, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, Password, [], Characteristics, Buckets, Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, undefined, Offer, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession) ->
	add_subscriber(Identity, ocs:generate_password(),
			Offer, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_list(Identity) ->
	add_subscriber(list_to_binary(Identity), Password, Offer, Characteristics, Buckets,
			Attributes, EnabledStatus, MultiSession);
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_list(Password) ->
	add_subscriber(Identity, list_to_binary(Password), Offer, Characteristics, Buckets,
			Attributes, EnabledStatus, MultiSession);
add_subscriber(undefined, Password, Offer, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_binary(Password), is_list(Offer), is_list(Buckets), is_list(Attributes),
		is_boolean(EnabledStatus), is_boolean(MultiSession) ->
	Now = erlang:system_time(?MILLISECOND),
	F1 = fun() ->
				OfferId = case Offer of
					[] ->
						case mnesia:first(offer) of
							'$end_of_table' ->
								throw(offer_not_found);
							ProdId ->
								ProdId
						end;
					Offer ->
						Offer
				end,
				case mnesia:read(offer, OfferId, read) of
					[#offer{char_value_use = CharValueUse} = P] ->
						N = erlang:unique_integer([positive]),
						S1 = #service{password = Password,
								attributes = Attributes,
								buckets = Buckets,
								enabled = EnabledStatus,
								multisession = MultiSession,
								last_modified = {Now, N}},
						NewChars = default_chars(CharValueUse, Characteristics),
						S2 = subscription(S1, P, NewChars, true),
						F3 = fun(_, _, 0) ->
									mnesia:abort(retries);
								(F, Identity, I) ->
									case mnesia:read(service, Identity, read) of
										[] ->
											S3 = S2#service{name = Identity},
											ok = mnesia:write(S3),
											S3;
										[_] ->
											F(F, list_to_binary(generate_identity()), I - 1)
									end
						end,
						F3(F3, list_to_binary(generate_identity()), 5);
					[] ->
						throw(offer_not_found)
				end
	end,
	case mnesia:transaction(F1) of
		{atomic, Service} ->
			{ok, Service};
		{aborted, Reason} ->
			{error, Reason}
	end;
add_subscriber(Identity, Password, Offer, Characteristics, Buckets, Attributes, EnabledStatus, MultiSession)
		when is_binary(Identity), size(Identity) > 0, is_binary(Password), is_list(Offer),
		is_list(Buckets), is_list(Attributes), is_boolean(EnabledStatus), is_boolean(MultiSession) ->
	Now = erlang:system_time(?MILLISECOND),
	F1 = fun() ->
				OfferId = case Offer of
					[] ->
						case mnesia:first(offer) of
							'$end_of_table' ->
								throw(offer_not_found);
							ProdId ->
								ProdId
						end;
					Offer ->
						Offer
				end,
				case mnesia:read(offer, OfferId, read) of
					[#offer{char_value_use = CharValueUse} = P] ->
						N = erlang:unique_integer([positive]),
						S1 = #service{name = Identity,
								password = Password,
								attributes = Attributes,
								buckets = Buckets,
								enabled = EnabledStatus,
								multisession = MultiSession,
								last_modified = {Now, N}},
						NewChars = default_chars(CharValueUse, Characteristics),
						S2 = subscription(S1, P, NewChars, true),
						ok = mnesia:write(S2),
						S2;
					[] ->
						throw(offer_not_found)
				end
	end,
	case mnesia:transaction(F1) of
		{atomic, Service} ->
			{ok, Service};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_service(Identity) -> Result  
	when
		Identity :: string() | binary(),
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: not_found | term().
%% @doc Look up an entry in the service table.
find_service(Identity) when is_list(Identity) ->
	find_service(list_to_binary(Identity));
find_service(Identity) when is_binary(Identity) ->
	F = fun() ->
				mnesia:read(service, Identity, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#service{} = Service]} ->
			{ok, Service};
		{atomic, []} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec get_services() -> Result
	when
		Result :: [#service{}] | {error, Reason},
		Reason :: term().
%% @doc Get all entries in the service table.
get_services()->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(service, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Services, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Services | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			Result
	end.

-spec query_service(Cont) -> Result
	when
		Cont :: start | eof | any(),
		Result :: {Cont, [#offer{}]} | {error, Reason},
		Reason :: term().
%% @doc Query offer inventories
query_service(start) ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(service, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Services, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Services | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			{eof, Result}
	end.

-spec delete_service(Identity) -> ok
	when
		Identity :: string() | binary().
%% @doc Delete an entry in the service table.
delete_service(Identity) when is_list(Identity) ->
	delete_service(list_to_binary(Identity));
delete_service(Identity) when is_binary(Identity) ->
	F = fun() ->
		mnesia:delete(service, Identity, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec add_offer(Offer) -> Result
	when
		Offer :: #offer{},
		Result :: {ok, #offer{}} | {error, Reason},
		Reason :: validation_failed | term().
%% @doc Add a new entry in offer table.
add_offer(#offer{price = Prices} = Offer) when length(Prices) > 0 ->
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
					or (Units == seconds) or (Units == messages)),
					is_integer(Size), Size > 0, is_integer(Amount) ->
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
			(#price{name = Name, type = usage, units = Units,
					size = Size, amount = Amount, alteration = undefined})
					when length(Name) > 0, Units == messages, is_integer(Size),
					Size > 0, is_integer(Amount), Amount > 0 ->
				true;
			(#price{name = Name, type = usage, period = undefined,
					units = Units, size = Size,
					amount = Amount, alteration = Alteration})
					when length(Name) > 0, ((Units == octets)
					or (Units == seconds) or (Units == message)),
					is_integer(Size), Size > 0, is_integer(Amount),
					Amount > 0 ->
				Fvala(Alteration);
			(#price{type = tariff, alteration = undefined,
					size = Size, units = Units, amount = Amount})
					when is_integer(Size), Size > 0, ((Units == octets)
					or (Units == seconds)), ((Amount == undefined) or
					(Amount == 0)) ->
				true;
			(#price{}) ->
				false
	end,
	case lists:all(Fvalp, Prices) of
		true ->
			add_offer1(Offer);
		false ->
			{error, validation_failed}
	end;
add_offer(#offer{specification = undefined, bundle = L} = Offer)
		when length(L) > 0 ->
	add_offer1(Offer);
add_offer(#offer{specification = L, bundle = []} = Offer)
		when length(L) > 0 ->
	add_offer1(Offer).
%% @hidden
add_offer1(Offer) ->
	Fadd = fun() ->
		TS = erlang:system_time(?MILLISECOND),
		N = erlang:unique_integer([positive]),
		Offer1 = Offer#offer{last_modified = {TS, N}},
		ok = mnesia:write(offer, Offer1, write),
		Offer1
	end,
	case mnesia:transaction(Fadd) of
		{atomic, Offer2} ->
			{ok, Offer2};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec add_pla(Pla) -> Result
	when
		Pla :: #pla{},
		Result :: {ok, #pla{}} | {error, Reason},
		Reason :: validation_failed | term().
%% @doc Add a new entry in pricing logic algorithm table.
add_pla(#pla{} = Pla) ->
	F = fun() ->
		TS = erlang:system_time(?MILLISECOND),
		N = erlang:unique_integer([positive]),
		R = Pla#pla{last_modified = {TS, N}},
		ok = mnesia:write(pla, R, write),
		R 
	end,
	case mnesia:transaction(F) of
		{atomic, #pla{name = Name} = Pla1} ->
			case catch list_to_existing_atom(Name) of
				{'EXIT', _Reason} ->
					ok = ocs_gtt:new(list_to_atom(Name), []),
					{ok, Pla1};
				_ ->
					{ok, Pla1}
			end;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec add_pla(Pla, File) -> Result
	when
		Pla :: #pla{},
		File :: file:filename(),
		Result :: {ok, #pla{}} | {error, Reason},
		Reason :: validation_failed | term().
%% @doc Add a new entry in pricing logic algorithm table.
%% 	Import table rows from CSV file.
add_pla(#pla{} = Pla, File) when is_list(File) ->
	case catch ocs_gtt:import(File) of
		ok ->
			Basename = filename:basename(File),
			Name = string:sub_string(Basename, 1, string:rchr(Basename, $.) - 1),
			add_pla(Pla#pla{name = Name});
		{'EXIT', Reason} ->
			{error, Reason}
	end.

-spec find_offer(OfferID) -> Result
	when
		OfferID :: string(),
		Result :: {ok, Offer} | {error, Reason},
		Offer :: #offer{},
		Reason :: term().
%% @doc Find offer by product id
find_offer(OfferID) ->
	F = fun() ->
		case mnesia:read(offer, OfferID) of
			[Entry] ->
				Entry;
			[] ->
				throw(not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, Offer} ->
			{ok, Offer};
		{aborted, {throw, not_found}} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec get_offers() -> Result
	when
		Result :: [#offer{}] | {error, Reason},
		Reason :: term().
%% @doc Get all entries in the offer table.
get_offers() ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(offer, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Offer, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Offer | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			Result
	end.

-spec delete_offer(OfferID) -> Result
	when
		OfferID :: string(),
		Result :: ok.
%% @doc Delete an entry from the offer table.
delete_offer(OfferID) ->
	F = fun() ->
		mnesia:delete(offer, OfferID, write)
	end,
	case mnesia:transaction(F) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec query_offer(Cont, Name, Description, Status, SDT, EDT, Price) -> Result
	when
		Cont :: start | eof | any(),
		Name :: undefined | '_' | string(),
		Description :: undefined | '_' | string(),
		Status :: undefined | '_' | atom(),
		SDT :: undefined | '_' | string() | integer(),
		EDT :: undefined | '_' | string() | integer(),
		Price :: undefined | '_' | string(),
		Result :: {Cont, [#offer{}]} | {error, Reason},
		Reason :: term().
%% @doc Query offer entires
query_offer(Con, Name, Description, Status, STD, EDT, undefined) ->
	query_offer(Con, Name, Description, Status, STD, EDT, '_');
query_offer(Con, Name, Description, Status, STD, undefined, Price) ->
	query_offer(Con, Name, Description, Status, STD, '_', Price);
query_offer(Con, Name, Description, Status, undefined, EDT, Price) ->
	query_offer(Con, Name, Description, Status, '_', EDT, Price);
query_offer(Con, Name, Description, undefined, SDT, EDT, Price) ->
	query_offer(Con, Name, Description, '_', SDT, EDT, Price);
query_offer(Con, Name, undefined, Status, SDT, EDT, Price) ->
	query_offer(Con, Name, '_', Status, SDT, EDT, Price);
query_offer(Con, undefined, Description, Status, SDT, EDT, Price) ->
	query_offer(Con, '_', Description, Status, SDT, EDT, Price);
query_offer(Con, Name, Description, Status, SDT, EDT, Price) when is_list(EDT) ->
	ISOEDT = ocs_rest:iso8601(EDT),
	query_offer(Con, Name, Description, Status, SDT, ISOEDT, Price);
query_offer(Con, Name, Description, Status, SDT, EDT, Price) when is_list(SDT) ->
	ISOSDT = ocs_rest:iso8601(SDT),
	query_offer(Con, Name, Description, Status, ISOSDT, EDT, Price);
query_offer(start, Name, Description, Status, SDT, EDT, Price) ->
	MatchHead = #offer{name = Name, description = Description,
			start_date = SDT, end_date = EDT, bundle = '_',
			status = Status, specification = '_',
			char_value_use = '_', price = '_', last_modified = '_'},
	MatchSpec = MatchSpec = [{MatchHead, [], ['$_']}],
	F = fun() ->
		mnesia:select(offer, MatchSpec, read)
	end,
	case mnesia:transaction(F) of
		{atomic, Offers} ->
			query_offer1(Offers, Price, []);
		{aborted, Reason} ->
			{error, Reason}
	end;
query_offer(eof, _Name, _Description, _Status, _STD, _EDT, _Price) ->
	{eof, []}.
%% @hidden
query_offer1([], _, Acc) ->
	{eof, lists:reverse(Acc)};
query_offer1(Offers, '_', _) ->
	{eof, Offers};
query_offer1([#offer{price = Prices} = Offer | T], PriceName, Acc) ->
	case lists:keyfind(PriceName, #price.name, Prices) of
		false ->
			query_offer1(T, PriceName, Acc);
		_ ->
			query_offer1(T, PriceName, [Offer | Acc])
	end.

-spec query_table(Cont, Name, Prefix, Description, Rate, LM) -> Result
	when
		Cont :: start | eof | any(),
		Name :: undefined | '_' | atom(),
		Prefix :: undefined | '_' | string(),
		Description :: undefined | '_' | string(),
		Rate :: undefined | '_' | string(),
		LM :: undefined | '_' | tuple(),
		Result :: {Cont, [#gtt{}]} | {error, Reason},
		Reason :: term().
%% @doc Query pricing logic algorithm entires
query_table(Cont, Name, Prefix, Description, Rate, undefined) ->
	query_table(Cont, Name, Prefix, Description, Rate, '_');
query_table(Cont, Name, Prefix, Description, undefined, LM) ->
	query_table(Cont, Name, Prefix, Description, '_', LM);
query_table(Cont, Name, Prefix, undefined, Rate, LM) ->
	query_table(Cont, Name, Prefix, '_', Rate, LM);
query_table(Cont, Name, undefined, Description, Rate, LM) ->
	query_table(Cont, Name, '_', Description, Rate, LM);
query_table(start, Name, Prefix, Description, Rate, LM) ->
	MatchHead = #gtt{num = Prefix, value = {Description, Rate, LM}},
	MatchSpec = MatchSpec = [{MatchHead, [], ['$_']}],
	F = fun() ->
		mnesia:select(Name, MatchSpec, read)
	end,
	case mnesia:transaction(F) of
		{atomic, Pla} ->
			query_table1(Pla, []);
		{aborted, Reason} ->
			{error, Reason}
	end;
query_table(eof, _Name, _Prefix, _Description, _Rate, _LM) ->
	{eof, []}.
%% @hidden
query_table1([], Acc) ->
	{eof, lists:reverse(Acc)};
query_table1(Pla, _Acc) ->
	{eof, Pla}.

-spec get_plas() -> Result
	when
		Result :: [#pla{}] | {error, Reason},
		Reason :: term().
%% @doc Get all entries in the pla table.
get_plas() ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun(F, start, Acc) ->
				F(F, mnesia:select(pla, MatchSpec,
						?CHUNKSIZE, read), Acc);
			(_F, '$end_of_table', Acc) ->
				lists:flatten(lists:reverse(Acc));
			(_F, {error, Reason}, _Acc) ->
				{error, Reason};
			(F,{Pla, Cont}, Acc) ->
				F(F, mnesia:select(Cont), [Pla | Acc])
	end,
	case mnesia:transaction(F, [F, start, []]) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, Result} ->
			Result
	end.

-spec find_pla(ID) -> Result
	when
		ID :: string(),
		Result :: {ok, Pla} | {error, Reason},
		Pla :: #pla{},
		Reason :: term().
%% @doc Find pricing logic algorithm by id.
find_pla(ID) ->
	F = fun() ->
		case mnesia:read(pla, ID) of
			[Entry] ->
				Entry;
			[] ->
				throw(not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, Pla} ->
			{ok, Pla};
		{aborted, {throw, not_found}} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec delete_pla(ID) -> Result
	when
		ID :: string(),
		Result :: ok.
%% @doc Delete an entry from the pla table.
delete_pla(ID) ->
	F = fun() ->
		mnesia:delete(pla, ID, write)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{atomic, ok} = mnesia:delete_table(list_to_existing_atom(ID)),
			ok;
		{aborted, Reason} ->
			exit(Reason)
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

%% service types
-define(DATA, 2).
-define(VOICE, 12).

-spec authorize(ServiceType, Identity, Password) -> Result
	when
		ServiceType :: undefined | integer(),
		Identity :: string() | binary(),
		Password :: string() | binary(),
		Result :: {ok, #service{}} | {disabled, SessionsList} | {error, Reason},
		SessionsList :: [{TimeStamp, SessionAttributes}],
		TimeStamp :: integer(),
		SessionAttributes :: [tuple()],
		Reason :: out_of_credit | bad_password | not_found | term().
%% @doc Authorize a service.
%%
%% 	If the service `enabled' field is `true' and have sufficient balance
%%		set `disconnect' field to `false' and return {ok, #service{password = `PSK'}}
%% 	where `PSK' is used for `Mikrotik-Wireless-Psk' and `Attributes' are
%% 	additional attributes to be returned in an `Access-Accept' response.
%% @private
authorize(ServiceType, Identity, Password) when is_list(Identity) ->
	authorize(ServiceType, list_to_binary(Identity), Password);
authorize(ServiceType, Identity, Password) when is_list(Password) ->
	authorize(ServiceType, Identity, list_to_binary(Password));
authorize(ServiceType, Identity, Password) when is_binary(Identity),
		is_binary(Password) ->
	F= fun() ->
				case mnesia:read(service, Identity, write) of
					[#service{buckets = Buckets, enabled = Enabled,
							disconnect = Disconnect} = Entry] ->
						Now = erlang:system_time(?MILLISECOND),
						F2 = fun(#bucket{remain_amount = Amount,
											termination_date = TD, units = Units}) when
											((TD =/= undefined) orelse (TD > Now))
											and
											((ServiceType == undefined) orelse
											((ServiceType == ?DATA)
												and ((Units == octets) orelse (Units == cents))) orelse
											((ServiceType == ?VOICE)
												and ((Units == seconds) orelse (Units == cents))))
											and
											(Amount > 0) ->
										true;
									(_) ->
										false
						end,
						case lists:any(F2, Buckets) of
							true ->
								case {Enabled, Disconnect, Entry#service.password} of
									{true, false, Password} ->
										Entry;
									{true, true, Password} ->
										NewEntry = Entry#service{disconnect = false},
										ok = mnesia:write(service, NewEntry, write),
										NewEntry;
									{true, false, MTPassword} when
											Password == <<>>,
											MTPassword =/= Password ->
										Entry;
									{true, true, MTPassword} when
											Password == <<>>,
											MTPassword =/= Password ->
										NewEntry = Entry#service{disconnect = false},
										ok = mnesia:write(service, NewEntry, write),
										NewEntry;
									{false, _, Password} ->
										SessionsList = Entry#service.session_attributes,
										NewEntry = Entry#service{session_attributes = []},
										ok = mnesia:write(service, NewEntry, write),
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
		{atomic, #service{} = S} ->
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
	gregorian_datetime_to_system_time({EndDate, EndTime}) - 1;
end_period1({Date, {Hour, Minute, Second}}, hourly) ->
	EndTime = {Hour + 1, Minute, Second},
	gregorian_datetime_to_system_time({Date, EndTime}) - 1;
end_period1({Date, Time}, daily) ->
	NextDay = calendar:date_to_gregorian_days(Date) + 1,
	EndDate = calendar:gregorian_days_to_date(NextDay),
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({Date, Time}, weekly) ->
	NextDay = calendar:date_to_gregorian_days(Date) + 7,
	EndDate = calendar:gregorian_days_to_date(NextDay),
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({{Year, 1, Day}, Time}, monthly)
		when Day > 28 ->
	NextDay = calendar:last_day_of_the_month(Year, 2),
	EndDate = {Year, 2, NextDay},
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({{Year, 2, Day}, Time}, monthly) when Day < 28 ->
	EndDate = {Year, 3, Day},
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({{Year, 2, Day}, Time}, monthly) ->
	EndDate = case calendar:last_day_of_the_month(Year, 2) of
		Day ->
			{Year, 3, 31};
		_ ->
			{Year, 3, Day}
	end,
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({{Year, 3, 31}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 4, 30}, Time}) - 1;
end_period1({{Year, 4, 30}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 5, 31}, Time}) - 1;
end_period1({{Year, 5, 31}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 6, 30}, Time}) - 1;
end_period1({{Year, 6, 31}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 7, 31}, Time}) - 1;
end_period1({{Year, 8, 31}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 9, 30}, Time}) - 1;
end_period1({{Year, 9, 30}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 10, 31}, Time}) - 1;
end_period1({{Year, 10, 30}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 11, 30}, Time}) - 1;
end_period1({{Year, 11, 30}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 12, 31}, Time}) - 1;
end_period1({{Year, 12, Day}, Time}, monthly) ->
	EndDate = {Year + 1, 1, Day},
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({{Year, Month, Day}, Time}, monthly) ->
	EndDate = {Year, Month + 1, Day},
	gregorian_datetime_to_system_time({EndDate, Time}) - 1;
end_period1({{Year, Month, Day}, Time}, yearly) ->
	EndDate = {Year + 1, Month, Day},
	gregorian_datetime_to_system_time({EndDate, Time}) - 1.

-spec default_chars(CharValueUse, ReqChars) -> NewChars
	when
		CharValueUse:: [#char_value_use{}],
		ReqChars :: [tuple()],
		NewChars :: [tuple()].
%% @doc Add default characteristic values.
%% @hidden
default_chars([#char_value_use{name = Name, values = Values} | T], Acc) ->
	case lists:keymember(Name, 1, Acc) of
		true ->
			default_chars(T, Acc);
		false ->
			case default_chars1(Values) of
				undefined ->
					default_chars(T, Acc);
				Value ->
					default_chars(T, [{Name, Value} | Acc])
			end
	end;
default_chars([], Acc) ->
	lists:reverse(Acc).
%% @hidden
default_chars1([#char_value{default = true, value = Value} | _]) ->
	Value;
default_chars1([_ | T]) ->
	default_chars1(T);
default_chars1([]) ->
	undefined.

-spec subscription(Service, Offer, Characteristics, InitialFlag) ->
		Service
	when
		Service :: #service{},
		Offer :: #offer{},
		Characteristics :: [tuple()],
		InitialFlag :: boolean().
%% @doc Apply product offering charges.
%% 	If `InitialFlag' is `true' initial bucket preparation
%% 	is done and one time prices are charged.
%% @throws offer_not_found
subscription(#service{last_modified = {Now, _}} = Service,
		#offer{name = OfferName, bundle = [], price = Prices} = _Offer,
		Characteristics, InitialFlag) ->
	Service1 = subscription(Service,
			Characteristics, Now, InitialFlag, Prices),
	ProductInstance = #product_instance{start_date = Now,
			product = OfferName, characteristics = Characteristics,
			last_modified = {Now, erlang:unique_integer([positive])}},
	Service1#service{product = ProductInstance};
subscription(#service{last_modified = {Now, _}} = Service,
		#offer{name = BundleName, bundle = Bundled, price = Prices},
		Characteristics, InitialFlag) when length(Bundled) > 0 ->
	F = fun(#bundled_po{name = P}, S) ->
				case mnesia:read(offer, P, read) of
					[Offer] ->
						subscription(S, Offer, Characteristics, true);
					[] ->
						throw(offer_not_found)
				end
	end,
	Service1 = lists:foldl(F, Service, Bundled),
	Service2 = subscription(Service1,
			Characteristics, Now, InitialFlag, Prices),
	ProductInstance = #product_instance{start_date = Now,
			product = BundleName, characteristics = Characteristics,
			last_modified = {Now, erlang:unique_integer([positive])}},
	Service2#service{product = ProductInstance}.
%% @hidden
subscription(#service{buckets = Buckets} = Service,
		Characteristics, Now, true, [#price{type = one_time,
		amount = Amount, alteration = undefined} | T]) ->
	NewBuckets = charge(Amount, Buckets),
	subscription(Service#service{buckets = NewBuckets},
			Characteristics, Now, true, T);
subscription(#service{buckets = Buckets} = Service,
		Characteristics, Now, true, [#price{type = one_time,
		amount = PriceAmount, name = Name,
		alteration = #alteration{units = Units, size = Size,
		amount = AlterationAmount}} | T]) ->
	NewBuckets = charge(PriceAmount + AlterationAmount,
		[#bucket{units = Units, remain_amount = Size, prices = [Name]} | Buckets]),
	subscription(Service#service{buckets = NewBuckets},
			Characteristics, Now, true, T);
subscription(#service{buckets = Buckets} = Service,
		Characteristics, Now, true, [#price{type = usage,
		name = Name, alteration = #alteration{type = one_time,
		units = Units, size = Size, amount = AlterationAmount}} | T]) ->
	NewBuckets = charge(AlterationAmount, [#bucket{units = Units,
		remain_amount = Size, prices = [Name]} | Buckets]),
	subscription(Service#service{buckets = NewBuckets},
			Characteristics, Now, true, T);
subscription(#service{buckets = Buckets} = Service,
		Characteristics, Now, InitialFlag, [#price{type = recurring,
		period = Period, amount = SubscriptionAmount,
		alteration = undefined} | T]) when Period /= undefined ->
	NewBuckets = charge(SubscriptionAmount, Buckets),
	subscription(Service#service{buckets = NewBuckets},
			Characteristics, Now, InitialFlag, T);
subscription(#service{buckets = Buckets} = Service,
		Characteristics, Now, InitialFlag,
		[#price{type = recurring, name = Name,
		period = Period, amount = SubscriptionAmount,
		alteration = #alteration{units = Units, size = Size,
		amount = AllowanceAmount}} | T]) when Period /= undefined,
		Units == octets; Period /= undefined, Units == seconds ->
	NewBuckets = charge(SubscriptionAmount + AllowanceAmount,
			[#bucket{units = Units, remain_amount = Size, name = Name,
			termination_date = end_period(Now, Period)} | Buckets]),
	subscription(Service#service{buckets = NewBuckets},
			Characteristics, Now, InitialFlag, T);
subscription(#service{buckets = Buckets} = Service,
		Characteristics, Now, InitialFlag, [#price{type = usage,
		name = Name, alteration = #alteration{type = recurring,
		period = Period, units = Units, size = Size,
		amount = Amount}} | T]) when Period /= undefined,
		Units == octets; Units == seconds ->
	NewBuckets = charge(Amount, [#bucket{units = Units,
			remain_amount = Size, prices = [Name],
			termination_date = end_period(Now, Period)} | Buckets]),
	subscription(Service#service{buckets = NewBuckets},
			Characteristics, Now, InitialFlag, T);
subscription(Service, Characteristics, Now, InitialFlag, [_H | T]) ->
	subscription(Service, Characteristics, Now, InitialFlag, T);
subscription(#service{buckets = Buckets} = Service, _, Now, _, []) ->
	NewBuckets = [B#bucket{last_modified = {Now,
			erlang:unique_integer([positive])},
			id = generate_bucket_id()} || B <- Buckets],
	Service#service{buckets = NewBuckets}.

%% @hidden
generate_bucket_id() ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

-spec gregorian_datetime_to_system_time(DateTime) -> MilliSeconds
	when
		DateTime :: tuple(),
		MilliSeconds :: pos_integer().
%% @doc Convert gregorian datetime to system time in milliseconds.
%% @hidden
gregorian_datetime_to_system_time(DateTime) ->
	(calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH) * 1000.

