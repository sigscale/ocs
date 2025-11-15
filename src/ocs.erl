%% ocs.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

%% export the ocs public API
-export([add_client/2, add_client/3, add_client/5, add_client/6,
		find_client/1, update_client/2, update_client/3,
		get_clients/0, delete_client/1, query_clients/6]).
-export([add_service/2, add_service/3, add_service/4, add_service/5,
		add_service/8, add_product/2, add_product/3, add_product/5,
		update_product/1, update_service/1,
		delete_product/1, get_products/0, query_product/4]).
-export([find_service/1, delete_service/1, get_services/0, query_service/3,
		find_product/1]).
-export([add_bucket/2, find_bucket/1, get_buckets/0, get_buckets/1,
		delete_bucket/1, query_bucket/3, adjustment/1]).
-export([add_user/3, list_users/0, get_user/1, delete_user/1,
		query_users/3, update_user/3]).
-export([add_offer/1, find_offer/1, get_offers/0, delete_offer/1,
		query_offer/7]).
-export([add_resource/1, update_resource/1, get_resources/0,
		get_resource/1, delete_resource/1, query_resource/5]).
-export([clean_services/1, clean_buckets/0, clean_buckets/1,
		clean_reservations/1, clean_reservations/2]).
-export([generate_password/0, generate_identity/0]).
-export([statistics/1]).
-export([start/4, start/5, stop/3, get_acct/1, get_auth/1]).
%% export the ocs private API
-export([normalize/1, subscription/4, end_period/2]).
-export([parse_bucket/1]).

-export_type([eap_method/0, match/0]).

-include("ocs.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(LOGNAME, radius_acct).
-define(CHUNKSIZE, 100).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

-define(PathResInv, "/resourceInventoryManagement/v1/").

-define(TARIFF_TABLE_SPEC,  "1").
-define(TARIFF_ROW_SPEC,    "2").
-define(POLICY_TABLE_SPEC,  "3").
-define(POLICY_ROW_SPEC,    "4").
-define(PERIOD_TABLE_SPEC,  "5").
-define(PERIOD_ROW_SPEC,    "6").
-define(ROAMING_TABLE_SPEC, "7").
-define(ROAMING_ROW_SPEC,   "8").

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
%% @equiv add_client(Address, Port, Protocol, Secret, PasswordRequired, true)
add_client(Address, Port, Protocol, Secret, PasswordRequired) ->
	add_client(Address, Port, Protocol, Secret, PasswordRequired, true).

-spec add_client(Address, Port, Protocol, Secret, PasswordRequired, Trusted) -> Result
	when
		Address :: inet:ip_address(),
		Port :: inet:port_number() | undefined,
		Protocol :: atom() | undefined,
		Secret :: string() | binary() | undefined,
		PasswordRequired :: boolean(),
		Trusted :: boolean(),
		Result :: {ok, # client{}}.
%% @doc Create an entry in the client table.
add_client(Address, Port, Protocol, Secret, PasswordRequired, Trusted)
		when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, Port, Protocol, Secret, PasswordRequired, Trusted);
add_client(Address, Port, Protocol, Secret, undefined, Trusted) ->
	add_client(Address, Port, Protocol, Secret, true, Trusted);
add_client({A, B, C, D} = Address,
		undefined, diameter, undefined, PasswordRequired, Trusted)
		when A >= 1, A =< 255, B >= 0, C =< 255, C >= 0, D =< 255, D >= 1, A < 255,
		is_boolean(PasswordRequired), is_boolean(Trusted) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {TS, N},
	R = #client{address = Address, protocol = diameter, last_modified = LM,
			password_required = PasswordRequired, trusted = Trusted},
	F = fun() ->
				mnesia:write(R),
				R
	end,
	case mnesia:transaction(F) of
		{atomic, Client} ->
			{ok, Client};
		{aborted, Reason} ->
			exit(Reason)
	end;
add_client(Address, Port, Protocol, undefined, PasswordRequired, Trusted) ->
	add_client(Address, Port, Protocol,
		generate_password(), PasswordRequired, Trusted);
add_client(Address, Port, undefined, Secret, PasswordRequired, Trusted) ->
	add_client(Address, Port, radius, Secret, PasswordRequired, Trusted);
add_client(Address, undefined, Protocol, Secret, PasswordRequired, Trusted) ->
	add_client(Address, 3799, Protocol, Secret, PasswordRequired, Trusted);
add_client(Address, Port, Protocol,
		Secret, PasswordRequired, Trusted) when is_list(Secret) ->
	add_client(Address, Port, Protocol,
		list_to_binary(Secret), PasswordRequired, Trusted);
add_client({A, B, C, D} = Address,
		Port, radius, Secret, PasswordRequired, Trusted)
		when A >= 1, A =< 255, B >= 0, C =< 255,
		C >= 0, D =< 255, D >= 1, A < 255,
		is_binary(Secret), is_boolean(PasswordRequired) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {TS, N},
	R = #client{address = Address, port = Port, protocol = radius,
			secret = Secret, password_required = PasswordRequired,
			trusted = Trusted, last_modified = LM},
	F = fun() ->
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
		Reason :: not_found | term().
%% @doc Find a client by IP address.
%%
find_client(Address) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	find_client(AddressTuple);
find_client(Address) when is_tuple(Address) ->
	case mnesia:dirty_read(client, Address) of
		[#client{} = Client] ->
			{ok, Client};
		[] ->
			{error, not_found}
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
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {TS, N},
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						NewEntry = Entry#client{secret = Password, last_modified = LM},
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
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {TS, N},
	F = fun() ->
				case mnesia:read(client, Address, write) of
					[Entry] ->
						NewEntry = Entry#client{port = Port,
								protocol = Protocol, last_modified = LM},
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
	F = fun F(start, Acc) ->
				F(mnesia:select(client, MatchSpec,
						?CHUNKSIZE, read), Acc);
			F('$end_of_table', Acc) ->
				{ok, Acc};
			F({error, Reason}, _Acc) ->
				{error, Reason};
			F({L, Cont}, Acc) ->
				F(mnesia:select(Cont), [L | Acc])
	end,
	case mnesia:ets(F, [start, []]) of
		{error, Reason} ->
			{error, Reason};
		{ok, Acc} when is_list(Acc) ->
			lists:flatten(lists:reverse(Acc))
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
		Cont :: start | any(),
		Address :: Match,
		Identifier :: Match,
		Port :: Match,
		Protocol :: Match,
		Secret :: Match,
		Match :: {exact, string()} | {like, string()} | '_',
		Result :: {Cont1, [#client{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @hidden
query_clients(start, {Op, String}, Identifier, Port, Protocol, Secret)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	{MatchHead, MatchConditions}  = case lists:last(String) of
		$% when Op == like ->
			{AddressMatch, Conditions} = match_address(lists:droplast(String)),
			{#client{address = AddressMatch, _ = '_'}, Conditions};
		_ ->
			{ok, Address} = inet:parse_address(String),
			{#client{address = Address, _  = '_'}, []}
	end,
	query_clients1(start, MatchHead, MatchConditions,
			Identifier, Port, Protocol, Secret);
query_clients(start, '_', Identifier, Port, Protocol, Secret) ->
	MatchHead = #client{_ = '_'},
	query_clients1(start, MatchHead, [], Identifier, Port, Protocol, Secret);
query_clients(Cont, _Address, Identifier, Port, _Protocol, Secret) ->
	query_clients2(Cont, [], [], Identifier, Port, Secret).
%% @hidden
query_clients1(start, MatchHead, MatchConditions,
		Identifier, Port, {Op, String}, Secret)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	try
		case lists:last(String) of
			$% when Op == like ->
				match_protocol(lists:droplast(String));
			_ ->
				case String of
					"diameter" ->
						diameter;
					"DIAMETER" ->
						diameter;
					"radius" ->
						radius;
					"RADIUS" ->
						radius;
					_ ->
						throw(badarg)
				end
		end
	of
		Protocol ->
			query_clients2(start, MatchHead#client{protocol = Protocol},
					MatchConditions, Identifier, Port, Secret)
	catch
		throw:badarg ->
			{eof, []}
	end;
query_clients1(start, MatchHead, MatchConditions, Identifier, Port, '_', Secret) ->
	query_clients2(start, MatchHead, MatchConditions, Identifier, Port, Secret).
%% @hidden
query_clients2(start, MatchHead, MatchConditions, Identifier, Port, Secret) ->
	MatchSpec = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			mnesia:select(client, MatchSpec, ?CHUNKSIZE, read)
	end,
	query_clients3(mnesia:ets(F), Identifier, Port, Secret);
query_clients2(Cont, _MatchHead, _MatchConditions, Identifier, Port, Secret) ->
	F = fun() ->
			mnesia:select(Cont)
	end,
	query_clients3(mnesia:ets(F), Identifier, Port, Secret).
%% @hidden
query_clients3({Clients, Cont}, '_', Port, Secret) ->
	query_clients4({Clients, Cont}, Port, Secret);
query_clients3({Clients, Cont}, {Op, String}, Port, Secret)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	F = case lists:last(String) of
		$% when Op == like ->
			Prefix = list_to_binary(lists:droplast(String)),
			Size = size(Prefix),
			fun(#client{identifier = Identifier}) ->
				case binary:part(Identifier, 0, Size) of
					Prefix ->
						true;
					_ ->
						false
				end
			end;
		_ ->
			ExactMatch = list_to_binary(String),
			fun(#client{identifier = Identifier}) when Identifier == ExactMatch ->
					true;
				(_) ->
					false
			end
	end,
	query_clients4({lists:filter(F, Clients), Cont}, Port, Secret);
query_clients3('$end_of_table', _Identifier, _Port, _Secret) ->
      {eof, []}.
%% @hidden
query_clients4({Clients, Cont}, '_', Secret) ->
	query_clients5({Clients, Cont}, Secret);
query_clients4({Clients, Cont}, {Op, String}, Secret)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	F = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			fun(#client{port = Port}) ->
					lists:prefix(Prefix, integer_to_list(Port))
			end;
		_ ->
			fun(#client{port = Port}) ->
					String == integer_to_list(Port)
			end
	end,
	query_clients5({lists:filter(F, Clients), Cont}, Secret).
%% @hidden
query_clients5({Clients, Cont}, '_') ->
	{Cont, Clients};
query_clients5({Clients, Cont}, {Op, String})
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	F = case lists:last(String) of
		$% when Op == like ->
			Prefix = list_to_binary(lists:droplast(String)),
			Size = size(Prefix),
			fun(#client{secret = Secret}) ->
				case binary:part(Secret, 0, Size) of
					Prefix ->
						true;
					_ ->
						false
				end
			end;
		_ ->
			ExactMatch = list_to_binary(String),
			fun(#client{secret = Secret}) when Secret == ExactMatch ->
					true;
				(_) ->
					false
			end
	end,
	{Cont, lists:filter(F, Clients)}.

get_products()->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun F(start, Acc) ->
				F(mnesia:select(product, MatchSpec,
						?CHUNKSIZE, read), Acc);
			F('$end_of_table', Acc) ->
				{ok, Acc};
			F({error, Reason}, _Acc) ->
				{error, Reason};
			F({L, Cont}, Acc) ->
				F(mnesia:select(Cont), [L | Acc])
	end,
	case mnesia:ets(F, [start, []]) of
		{error, Reason} ->
			{error, Reason};
		{ok, Acc} when is_list(Acc) ->
			lists:flatten(lists:reverse(Acc))
	end.

-spec query_product(Cont, MatchId, MatchOffer, MatchService) -> Result
	when
		Cont :: start | any(),
		MatchId ::  Match,
		MatchOffer ::  Match,
		MatchService ::  Match,
		Match :: {exact, string()} | {like, string()} | '_',
		Result :: {Cont1, [#product{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query product
query_product(Cont, '_' = _MatchId, MatchOffer, MatchService) ->
	 MatchHead = #product{_ = '_'},
	query_product1(Cont, MatchHead, MatchOffer, MatchService);
query_product(Cont, {Op, String}, MatchOffer, MatchService)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			#product{id = Prefix ++ '_', _ = '_'};
		_ ->
			#product{id = String, _ = '_'}
	end,
	query_product1(Cont, MatchHead, MatchOffer, MatchService).
%% @hidden
query_product1(Cont, MatchHead, '_', MatchService) ->
	MatchSpec = [{MatchHead, [], ['$_']}],
	query_product2(Cont, MatchSpec, MatchService);
query_product1(Cont, MatchHead, {Op, String} = _MatchOffer, MatchService)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead1 = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			MatchHead#product{product = Prefix ++ '_'};
		_ ->
			MatchHead#product{product = String}
	end,
	MatchSpec = [{MatchHead1, [], ['$_']}],
	query_product2(Cont, MatchSpec, MatchService).
%% @hidden
query_product2(start, MatchSpec, MatchService) ->
	F = fun() ->
		mnesia:select(product, MatchSpec, ?CHUNKSIZE, read)
	end,
	query_product3(mnesia:ets(F), MatchService);
query_product2(Cont, _MatchSpec, MatchService) ->
	F = fun() ->
		mnesia:select(Cont)
	end,
	query_product3(mnesia:ets(F), MatchService).
%% @hidden
query_product3({Products, Cont}, '_') ->
	{Cont, Products};
query_product3({Products, Cont}, {Op, String})
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	F1 = case lists:last(String) of
		$% when Op == like ->
			Prefix = list_to_binary(lists:droplast(String)),
			Size = size(Prefix),
			F2 = fun(<<P:Size/binary, _/binary>>) when P == Prefix ->
						true;
					(_) ->
						false
			end,
			fun(#product{service = Services}) ->
						lists:any(F2, Services)
			end;
		_ ->
			Service = list_to_binary(String),
			fun(#product{service = Services}) ->
						lists:member(Service, Services)
			end
	end,
	{Cont, lists:filter(F1, Products)};
query_product3('$end_of_table', _MatchService) ->
	{eof, []}.

-spec add_product(Offer, ServiceRefs) -> Result
	when
		Offer :: string(),
		ServiceRefs :: [ServiceRef],
		Result :: {ok, #product{}} | {error, Reason},
		ServiceRef :: binary(),
		Reason :: service_not_found | service_has_product | offer_not_found | term().
%% @equiv add_product(Offer, ServiceRefs, undefined, undefined, [])
add_product(Offer, ServiceRefs) ->
	add_product(Offer, ServiceRefs, undefined, undefined, []).

-spec add_product(Offer, ServiceRefs, Characteristics) -> Result
	when
		Offer :: string(),
		ServiceRefs :: [ServiceRef],
		Characteristics :: [tuple()],
		ServiceRef :: binary(),
		Result :: {ok, #product{}} | {error, Reason},
		Reason :: service_not_found | service_has_product | offer_not_found | term().
%% @equiv add_product(Offer, ServiceRefs, undefined, undefined, Characteristics)
add_product(Offer, ServiceRefs, Characteristics) ->
	add_product(Offer, ServiceRefs, undefined, undefined, Characteristics).

-spec add_product(OfferId, ServiceRefs, StartDate, EndDate, Characteristics) -> Result
	when
		OfferId :: string(),
		ServiceRefs :: [ServiceRef],
		StartDate :: undefined | pos_integer(),
		EndDate :: undefined | pos_integer(),
		Characteristics :: [tuple()],
		ServiceRef :: binary(),
		Result :: {ok, #product{}} | {error, Reason},
		Reason :: service_not_found | service_has_product | offer_not_found | term().
%% @doc Add a product inventory subscription instance.
add_product(OfferId, ServiceRefs, StartDate, EndDate, Characteristics)
		when (is_integer(StartDate) orelse (StartDate == undefined)),
		(is_integer(EndDate) orelse (EndDate == undefined)),
		is_list(Characteristics), is_list(OfferId), is_list(ServiceRefs) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {TS, N},
	Id = ocs_rest:etag(LM),
	F = fun() ->
			case mnesia:read(offer, OfferId, read) of
				[#offer{char_value_use = CharValueUse} = Offer] ->
					F2 = fun(ServiceRef) ->
								case mnesia:read(service, ServiceRef, write) of
									[#service{product = undefined} = Service] ->
										ok = mnesia:write(Service#service{product = Id,
												last_modified = LM});
									[_Service] ->
										mnesia:abort(service_has_product);
									_ ->
										mnesia:abort(service_not_found)
								end
					end,
					ok = lists:foreach(F2, ServiceRefs),
					NewChars = default_chars(CharValueUse, Characteristics),
					Product1 = #product{id = Id, product = OfferId, start_date = StartDate,
							end_date = EndDate, characteristics = NewChars,
							service = ServiceRefs, last_modified = LM},
					{Product2, Buckets} = subscription(Product1, Offer, [], true),
					F3 = fun(#bucket{} = B) -> ok = mnesia:write(bucket, B, write) end,
					ok = lists:foreach(F3, Buckets),
					ok = mnesia:write(Product2),
					Product2;
				[] ->
					mnesia:abort(offer_not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, Product} ->
			ok = ocs_event:notify(create_product, Product, product),
			{ok, Product};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec update_product(Product) -> Result
	when
		Result :: {ok, Product} | {error, Reason},
		Reason :: not_found | stale | term().
%% @doc Update an existing Product.
update_product(#product{id = Id, last_modified = LM} = Product)
		when is_list(Id), is_tuple(LM) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	NewLM = {TS, N},
	Ftrans = fun() ->
				case mnesia:read(product, Id, write) of
					[#product{last_modified = LM}] ->
						Product1 = Product#product{last_modified = NewLM},
						ok = mnesia:write(Product1),
						Product1;
					[#product{}] ->
						mnesia:abort(stale);
					[] ->
						mnesia:abort(not_found)
				end
	end,
   update_product1(mnesia:transaction(Ftrans)).
%% @hidden
%% @todo update_product event
update_product1({atomic, #product{} = Product}) ->
	ok = ocs_event:notify(create_product, Product, resource),
	{ok, Product};
update_product1({aborted, Reason}) ->
	{error, Reason}.

-spec find_product(ProductRef) -> Result
	when
		ProductRef :: string(),
		Result :: {ok, Product} | {error, Reason},
		Product :: #product{},
		Reason :: not_found | term().
%% @doc Look up entry in product table
find_product(ProductRef) when is_list(ProductRef) ->
	case mnesia:dirty_read(product, ProductRef) of
		[] ->
			{error, not_found};
		[#product{} = Product] ->
			{ok, Product}
	end.

-spec delete_product(ProductRef) -> Result
	when
		ProductRef :: string(),
		Result :: ok.
%% @doc Delete an entry from product table
delete_product(ProductRef) when is_list(ProductRef) ->
	F1 = fun() ->
			case mnesia:read(product, ProductRef, write) of
				[#product{service = Services,
						balance = Buckets} = Product] when length(Services) > 0 ->
					case service_exist(Services) of
						true ->
							mnesia:abort(service_exists);
						false ->
							delete_product(Product, Buckets)
					end;
				[#product{balance = Buckets} = Product] ->
					delete_product(Product, Buckets);
				[] ->
					mnesia:abort(not_found)
			end
	end,
	case mnesia:transaction(F1) of
		{atomic, {ok, Product}} ->
			ok = ocs_event:notify(delete_product, Product, product),
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.
%% @hidden
delete_product(#product{id = ProductRef} = Product, Buckets) ->
	F = fun(B) ->
			mnesia:delete(bucket, B, write)
	end,
	mnesia:delete(product, ProductRef, write),
	{lists:foreach(F, Buckets), Product}.

-spec add_service(Identity, Password) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | aka_cred() | undefined,
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_service(Identity, Password, undefined, [], true, false)
add_service(Identity, Password) ->
	add_service(Identity, Password, active, undefined, [], [], true, false).

-spec add_service(Identity, Password, ProductRef) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | aka_cred() | undefined,
		ProductRef :: string() | undefined,
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_service(Identity, Password, ProductRef, [], true, false)
add_service(Identity, Password, ProductRef) ->
	add_service(Identity, Password, active, ProductRef, [], [], true, false).

-spec add_service(Identity, Password, ProductRef, Chars) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | aka_cred() | undefined,
		ProductRef :: string() | undefined,
		Chars :: [tuple()],
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_service(Identity, Password, ProductRef, Chars, [], true, false)
add_service(Identity, Password, ProductRef, Chars) ->
	add_service(Identity, Password, active, ProductRef, Chars, [], true, false).

-spec add_service(Identity, Password, ProductRef, Chars, Attributes) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | aka_cred() | undefined,
		ProductRef :: string() | undefined,
		Chars :: [tuple()],
		Attributes :: radius_attributes:attributes() | binary(),
		Result :: {ok, #service{}} | {error, Reason},
		Reason :: term().
%% @equiv add_service(Identity, Password, ProductRef, Chars, Attributes, true, false)
add_service(Identity, Password, ProductRef, Chars, Attributes) ->
	add_service(Identity, Password, active, ProductRef, Chars, Attributes, true, false).

-spec add_service(Identity, Password, State, ProductRef, Chars,
		Attributes, EnabledStatus, MultiSessions) -> Result
	when
		Identity :: string() | binary() | undefined,
		Password :: string() | binary() | aka_cred() | undefined,
		State :: atom() | string() | undefined,
		ProductRef :: string() | undefined,
		Chars :: [tuple()] | undefined,
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
%% 	`ProductRef' key for product inventory reference,
%%		`Enabled' status and `MultiSessions' status may be provided.
%%
add_service(Identity, Password, State, ProductRef,
		Chars, Attributes, EnabledStatus, undefined) ->
	add_service(Identity, Password, State, ProductRef,
			Chars, Attributes, EnabledStatus, false);
add_service(Identity, Password, State, ProductRef,
		Chars, Attributes, undefined, MultiSession) ->
	add_service(Identity, Password, State, ProductRef,
			Chars, Attributes, true, MultiSession);
add_service(Identity, Password, State, ProductRef,
		Chars, undefined, EnabledStatus, MultiSession) ->
	add_service(Identity, Password, State, ProductRef,
			Chars, [], EnabledStatus, MultiSession);
add_service(Identity, Password, State, ProductRef,
		undefined, Attributes, EnabledStatus, MultiSession) ->
	add_service(Identity, Password, State, ProductRef,
			[], Attributes, EnabledStatus, MultiSession);
add_service(Identity, Password, undefined, ProductRef,
		Chars, Attributes, EnabledStatus, MultiSession) ->
	add_service(Identity, Password, active, ProductRef,
			Chars, Attributes, EnabledStatus, MultiSession);
add_service(Identity, undefined, State, ProductRef,
		Chars, Attributes, EnabledStatus, MultiSession) ->
	add_service(Identity, ocs:generate_password(), State,
			ProductRef, Chars, Attributes, EnabledStatus, MultiSession);
add_service(Identity, Password, State, ProductRef, Chars,
		Attributes, EnabledStatus, MultiSession) when is_list(Identity) ->
	add_service(list_to_binary(Identity), Password, State,
			ProductRef, Chars, Attributes, EnabledStatus, MultiSession);
add_service(Identity, Password, State, ProductRef, Chars,
		Attributes, EnabledStatus, MultiSession) when is_list(Password) ->
	add_service(Identity, list_to_binary(Password), State,
			ProductRef, Chars, Attributes, EnabledStatus, MultiSession);
add_service(undefined, Password, State, ProductRef, Chars,
		Attributes, EnabledStatus, MultiSession)
		when (is_binary(Password) or is_record(Password, aka_cred)),
		is_list(Attributes), is_boolean(EnabledStatus),
		is_boolean(MultiSession) ->
	Now = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {Now, N},
	F1 = fun() ->
			F2 = fun F2(_, 0) ->
							mnesia:abort(retries);
						F2(Identity1, I) ->
							case mnesia:read(service, Identity1, write) of
								[] ->
									Identity1;
								[_] ->
									F2(list_to_binary(generate_identity()), I - 1)
							end
			end,
			Identity = F2(list_to_binary(generate_identity()), 5),
			add_service1(Identity, Password, State, ProductRef,
					Chars, Attributes, EnabledStatus, MultiSession, LM)
	end,
	case mnesia:transaction(F1) of
		{atomic, Service} ->
			ok = ocs_event:notify(create_service, Service, service),
			{ok, Service};
		{aborted, Reason} ->
			{error, Reason}
	end;
add_service(Identity, Password, State, ProductRef, Chars, Attributes,
		EnabledStatus, MultiSession) when is_binary(Identity), size(Identity) > 0,
		(is_binary(Password) or is_record(Password, aka_cred)),
		is_list(Attributes), is_boolean(EnabledStatus),
		is_boolean(MultiSession) ->
	Now = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {Now, N},
	F1 =  fun() ->
			add_service1(Identity, Password, State, ProductRef,
					Chars, Attributes, EnabledStatus, MultiSession, LM)
	end,
	case mnesia:transaction(F1) of
		{atomic, Service} ->
			ok = ocs_event:notify(create_service, Service, service),
			{ok, Service};
		{aborted, {throw, Reason}} ->
			{error, Reason};
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_service1(Identity, Password, State, undefined,
		Chars, Attributes, EnabledStatus, MultiSession, LM) ->
	S1 = #service{name = Identity,
					password = Password,
					state = State,
					attributes = Attributes,
					enabled = EnabledStatus,
					multisession = MultiSession,
					characteristics = Chars,
					last_modified = LM},
	ok = mnesia:write(service, S1, write),
	S1;
add_service1(Identity, Password, State, ProductRef,
		Chars, Attributes, EnabledStatus, MultiSession, LM) ->
	case mnesia:read(product, ProductRef, read) of
		[#product{service = ServiceRefs} = P1] ->
			case lists:member(Identity, ServiceRefs) of
				false ->
					P2 = P1#product{service = [Identity | ServiceRefs],
							last_modified = LM},
					ok = mnesia:write(P2),
					S1 = #service{name = Identity,
									password = Password,
									state = State,
									product = ProductRef,
									attributes = Attributes,
									enabled = EnabledStatus,
									multisession = MultiSession,
									characteristics = Chars,
									last_modified = LM},
					ok = mnesia:write(service, S1, write),
					S1;
				true ->
					throw(service_exists)
			end;
		[] ->
			throw(product_not_found)
	end.

-spec update_service(Service) -> Result
	when
		Result :: {ok, Service} | {error, Reason},
		Reason :: not_found | stale | term().
%% @doc Update an existing Service.
update_service(#service{name = Name, last_modified = LM} = Service)
		when is_binary(Name), is_tuple(LM) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	NewLM = {TS, N},
	Ftrans = fun() ->
				case mnesia:read(service, Name, write) of
					[#service{last_modified = LM}] ->
						Service1 = Service#service{last_modified = NewLM},
						ok = mnesia:write(Service1),
						Service1;
					[#service{}] ->
						mnesia:abort(stale);
					[] ->
						mnesia:abort(not_found)
				end
	end,
   update_service1(mnesia:transaction(Ftrans)).
%% @hidden
%% @todo update_service event
update_service1({atomic, #service{} = Service}) ->
	ok = ocs_event:notify(create_service, Service, resource),
	{ok, Service};
update_service1({aborted, Reason}) ->
	{error, Reason}.

-spec add_bucket(ProductRef, Bucket) -> Result
	when
		ProductRef :: string(),
		Bucket :: #bucket{},
		Result :: {ok, BucketBefore, BucketAfter} | {error, Reason},
		BucketBefore :: #bucket{},
		BucketAfter :: #bucket{},
		Reason :: term().
%% @doc Add a new bucket to bucket table or update exsiting bucket
add_bucket(ProductRef, #bucket{id = undefined,
		attributes = #{bucket_type := Type}} = Bucket)
		when is_list(ProductRef), ((Type == normal) or (Type == session)) ->
	BId = generate_bucket_id(),
	F = fun() ->
		case mnesia:read(product, ProductRef, write) of
			[#product{balance = B} = P] ->
				Bucket1  = Bucket#bucket{id = BId, product = [ProductRef]},
				ok = mnesia:write(bucket, Bucket1, write),
				Product = P#product{balance = lists:reverse([BId | B])},
				ok = mnesia:write(product, Product, write),
				{ok, Bucket, Bucket1};
			[] ->
				throw(product_not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, {ok, #bucket{remain_amount = Amount} = OldBucket,
				#bucket{id = Bid1, name = Channel, product = [ProdRef | _],
						units = Units, remain_amount = After,
						end_date = Validity, status = Status} = NewBucket}} ->
			ocs_log:abmf_log(topup, undefined, Bid1, Units, ProdRef,
					Amount, 0, After, Validity, Channel, undefined,
					undefined, undefined, undefined, Status),
			ok = ocs_event:notify(create_bucket, NewBucket, balance),
			{ok, OldBucket, NewBucket};
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
	case mnesia:dirty_read(bucket, BucketId)  of
		[#bucket{} = B] ->
			{ok, B};
		[] ->
			{error, not_found}
	end.

-spec get_buckets() -> Result
	when
		Result :: Buckets | {error, Reason},
		Buckets :: [#bucket{}],
		Reason :: term().
%% @doc Get all buckets.
get_buckets() ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun F(start, Acc) ->
				F(mnesia:select(bucket, MatchSpec,
					?CHUNKSIZE, read), Acc);
			F('$end_of_table', Acc) ->
				{ok, Acc};
			F({error, Reason}, _Acc) ->
				{error, Reason};
			F({L, Cont}, Acc) ->
				F(mnesia:select(Cont), [L| Acc])
	end,
	case mnesia:ets(F, [start, []]) of
		{error, Reason} ->
			{error, Reason};
		{ok, Acc} when is_list(Acc) ->
			lists:flatten(lists:reverse(Acc))
	end.

-spec get_buckets(ProdRef) -> Result
	when
		ProdRef :: string(),
		Result :: [#bucket{}] | {error, Reason},
		Reason :: product_not_found.
%% @doc Get all buckets for given product reference.
get_buckets(ProdRef) when is_list(ProdRef) ->
	F = fun() ->
		case mnesia:read(product, ProdRef) of
			[#product{balance = []}] ->
				{ok, []};
			[#product{balance = BucketRefs}] ->
				MatchHead = #bucket{id = '$1', _ = '_'},
				MatchIds = [{'==', Id, '$1'} || Id <- BucketRefs],
				MatchConditions = [list_to_tuple(['or' | MatchIds])],
				{ok, mnesia:select(bucket, [{MatchHead, MatchConditions, ['$_']}])};
			[] ->
				throw(product_not_found)
		end
	end,
	case mnesia:ets(F) of
		{ok, Buckets} ->
			Buckets;
		{throw, Reason} ->
			{error, Reason}
	end.

-spec query_bucket(Cont, MatchId, MatchProduct) -> Result
	when
		Cont :: start | any(),
		MatchId :: Match,
		MatchProduct :: Match,
		Match :: {exact, string()} | {like, string()} | '_',
		Result :: {Cont1, [#bucket{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query bucket
query_bucket(Cont, '_' = _MatchId, MatchProduct) ->
	MatchSpec = [{'_', [], ['$_']}],
	query_bucket1(Cont, MatchSpec, MatchProduct);
query_bucket(Cont, {Op, Id}, MatchProduct)
		when is_list(Id), ((Op == exact) orelse (Op == like)) ->
	MatchSpec = case lists:last(Id) of
		$% when Op == like ->
			Prefix = lists:droplast(Id),
			MatchHead = #bucket{id = Prefix ++ '_', _ = '_'},
			[{MatchHead, [], ['$_']}];
		_ ->
			MatchHead = #bucket{id = Id, _ = '_'},
			[{MatchHead, [], ['$_']}]
	end,
	query_bucket1(Cont, MatchSpec, MatchProduct).
%% @hidden
query_bucket1(start, MatchSpec, MatchProduct) ->
	F = fun() ->
		mnesia:select(bucket, MatchSpec, ?CHUNKSIZE, read)
	end,
	query_bucket2(mnesia:ets(F), MatchProduct);
query_bucket1(Cont, _MatchSpec, MatchProduct) ->
	F = fun() ->
		mnesia:select(Cont)
	end,
	query_bucket2(mnesia:ets(F), MatchProduct).
%% @hidden
query_bucket2({Buckets, Cont}, '_') ->
	{Cont, Buckets};
query_bucket2({Buckets, Cont}, {Op, String})
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	F1 = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			fun(#bucket{product = Products}) ->
					F2 = fun(P) ->
							lists:prefix(Prefix, P)
					end,
					lists:any(F2, Products)
			end;
		_ ->
			fun(#bucket{product = Products}) ->
					lists:member(String, Products)
			end
	end,
	{Cont, lists:filter(F1, Buckets)};
query_bucket2('$end_of_table', _MatchProduct) ->
      {eof, []}.

-spec delete_bucket(BucketId) -> ok
	when
		BucketId :: term().
%% @doc Delete entry in the bucket table.
delete_bucket(BucketId) ->
	F1 = fun(ProdRef) ->
			case mnesia:read(product, ProdRef, write) of
				[#product{balance = Buckets1} = Product1] ->
					Buckets2 = lists:delete(BucketId, Buckets1),
					Product2 = Product1#product{balance = Buckets2},
					mnesia:write(Product2);
				[] ->
					ok
			end
	end,
	F2 = fun() ->
		case mnesia:read(bucket, BucketId, write) of
			[#bucket{product = ProdRefs} = Bucket] ->
				lists:foreach(F1, ProdRefs),
				{mnesia:delete(bucket, BucketId, write), Bucket};
			[] ->
				mnesia:abort(not_found)
		end
	end,
	case mnesia:transaction(F2) of
		{atomic, {ok, #bucket{name = Channel,
				units = Units, product = [ProdRef | _],
				remain_amount = Before, end_date = Validity,
				status = Status} = Bucket}} ->
			ocs_log:abmf_log(delete, undefined, BucketId,
					Units, ProdRef, 0, Before, 0, Validity,
					Channel, undefined, undefined, undefined,
					undefined, Status),
			ok = ocs_event:notify(delete_bucket, Bucket, balance),
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec adjustment(Adjustment) -> Result
	when
		Adjustment :: #adjustment{},
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Applying balance adjustment.
adjustment(#adjustment{amount = Amount, product = ProductRef, units = Units,
		start_date = StartDate, end_date = EndDate})
		when is_list(ProductRef), is_integer(Amount), Amount >= 0 ->
	F1 = fun() ->
		case mnesia:read(product, ProductRef, write) of
			[#product{balance = BucketRefs1} = P1] ->
				Buckets1 = lists:flatten([mnesia:read(bucket, B1, write)
						|| B1 <- BucketRefs1]),
				F2 = fun(B2) -> mnesia:delete(bucket, B2, write) end,
				F3 = fun(B3) -> mnesia:write(bucket, B3, write) end,
				case credit(Units, Amount, Buckets1) of
					{0, DeleteRefs, Buckets2} ->
						lists:foreach(F2, DeleteRefs),
						lists:foreach(F3, Buckets2),
						BucketRefs2 = [B5 || #bucket{id = B5} <- Buckets2],
						P2 = P1#product{balance = BucketRefs2},
						mnesia:write(product, P2, write),
						{0, BucketRefs2, DeleteRefs};
					{RemainAmount, DeleteRefs, Buckets2} ->
						B4 = #bucket{id = generate_bucket_id(),
								start_date = StartDate, end_date = EndDate,
								remain_amount = RemainAmount, units = Units,
								attributes = #{bucket_type => normal},
								product = [ProductRef],
								last_modified = make_lm()},
						mnesia:write(B4),
						lists:foreach(F2, DeleteRefs),
						lists:foreach(F3, Buckets2),
						BucketRefs2 = [B5 || #bucket{id = B5} <- Buckets2],
						P2 = P1#product{balance = [B4#bucket.id | BucketRefs2]},
						mnesia:write(product, P2, write),
						{RemainAmount, DeleteRefs, [B4#bucket.id | BucketRefs2]}
				end;
			[] ->
				mnesia:abort(not_found)
		end
	end,
	case mnesia:transaction(F1) of
		{atomic, {AmountAfter, DeleteBucketRefs2, BucketRefs}} ->
			log_adjustment(AmountAfter, DeleteBucketRefs2, BucketRefs,
					Units, ProductRef, Amount, AmountAfter - Amount);
		{aborted, Reason} ->
			{error, Reason}
	end;
adjustment(#adjustment{amount = Amount1, product = ProductRef, units = Units,
		start_date = StartDate, end_date = EndDate})
		when is_list(ProductRef), is_integer(Amount1), Amount1 < 0 ->
	F1 = fun() ->
		case mnesia:read(product, ProductRef, write) of
			[#product{balance = []} = P] ->
				BId = generate_bucket_id(),
				NewBucket = #bucket{id = BId, start_date = StartDate, end_date = EndDate,
						remain_amount = Amount1, units = Units, product = [ProductRef],
						attributes = #{bucket_type => normal},
						last_modified = make_lm()},
				mnesia:write(bucket, NewBucket, write),
				NewProduct = P#product{balance = [BId]},
				ok = mnesia:write(product, NewProduct, write),
				{[BId], [BId], []};
			[#product{balance = BucketRefs} = P] ->
				F2 = fun F([H | T], Amount2, Acc, WrittenRefs, DeletedRefs) ->
						case mnesia:read(bucket, H, write) of
							[#bucket{id = Id, remain_amount = Remain, units = Units} = B]
									when Remain > Amount2 ->
								NewBucket = B#bucket{remain_amount = Remain - Amount2},
								ok = mnesia:write(bucket, NewBucket, write),
								{[Id | Acc] ++ T, [NewBucket#bucket.id | WrittenRefs], DeletedRefs};
							[#bucket{id = Id, remain_amount = Amount2, units = Units}] ->
								ok = mnesia:delete(bucket, Id, write),
								{Acc ++ T, WrittenRefs, [Id | DeletedRefs]};
							[#bucket{id = Id, remain_amount = Remain, units = Units}] ->
								ok = mnesia:delete(bucket, Id, write),
								F(T, Amount2 - Remain, Acc, WrittenRefs, [Id | DeletedRefs]);
							[#bucket{}] ->
								F(T, Amount2, [H | Acc], WrittenRefs, DeletedRefs)
						end;
					F([], Amount2, Acc, WrittenRefs, DeletedRefs) ->
						BId = generate_bucket_id(),
						NewBucket = #bucket{id = BId, start_date = StartDate, end_date = EndDate,
								remain_amount = -Amount2, units = Units, product = [ProductRef],
								attributes = #{bucket_type => normal},
								last_modified = make_lm()},
						ok = mnesia:write(bucket, NewBucket, write),
						{[BId |Acc], [BId | WrittenRefs], DeletedRefs}
				end,
				{NewBucketRefs, WrittenRefs, DeletedRefs} = F2(BucketRefs, abs(Amount1), [], [], []),
				NewProduct = P#product{balance = NewBucketRefs},
				ok = mnesia:write(product, NewProduct, write),
				{NewBucketRefs, WrittenRefs, DeletedRefs};
			[] ->
				mnesia:abort(badarg)
		end
	end,
	case mnesia:transaction(F1) of
		{atomic, {_BucketRefs, WrittenRefs, DeletedRefs}} ->
			log_adjustment(undefined, DeletedRefs ,WrittenRefs,
					Units, ProductRef, Amount1, undefined);
		{aborted, Reason} ->
			{error, Reason}
	end;
adjustment(#adjustment{amount = Amount, service = ServiceRef, product = undefined,
		units = Units, start_date = StartDate, end_date = EndDate})
		when is_list(ServiceRef), is_integer(Amount), Amount >= 0 ->
	F1 = fun() ->
		case mnesia:read(service, list_to_binary(ServiceRef), read) of
			[#service{product = ProductRef}] ->
				case mnesia:read(product, ProductRef, write) of
					[#product{balance = BucketRefs1} = P1] ->
						Buckets1 = lists:flatten([mnesia:read(bucket, B1, write)
								|| B1 <- BucketRefs1]),
						F2 = fun(B2) -> mnesia:delete(bucket, B2, write) end,
						F3 = fun(B3) -> mnesia:write(bucket, B3, write) end,
						case credit(Units, Amount, Buckets1) of
							{0, DeleteRefs, Buckets2} ->
								lists:foreach(F2, DeleteRefs),
								lists:foreach(F3, Buckets2),
								BucketRefs2 = [B5 || #bucket{id = B5} <- Buckets2],
								P2 = P1#product{balance = BucketRefs2},
								mnesia:write(product, P2, write),
								{0, BucketRefs2, DeleteRefs, ProductRef};
							{RemainAmount, DeleteRefs, Buckets2} ->
								B4 = #bucket{id = generate_bucket_id(),
										start_date = StartDate, end_date = EndDate,
										remain_amount = RemainAmount, units = Units,
										attributes = #{bucket_type => normal},
										product = [ProductRef],
										last_modified = make_lm()},
								mnesia:write(B4),
								lists:foreach(F2, DeleteRefs),
								lists:foreach(F3, Buckets2),
								BucketRefs2 = [B5 || #bucket{id = B5} <- Buckets2],
								P2 = P1#product{balance = [B4#bucket.id | BucketRefs2]},
								mnesia:write(product, P2, write),
								{RemainAmount, DeleteRefs, [B4#bucket.id | BucketRefs2], ProductRef}
						end;
					[] ->
						mnesia:abort(not_found)
				end;
			[] ->
				mnesia:abort(not_found)
		end
	end,
	case mnesia:transaction(F1) of
		{atomic, {AmountAfter, DeleteBucketRefs2, BucketRefs, ProductRef}} ->
			log_adjustment(AmountAfter, DeleteBucketRefs2, BucketRefs,
					Units, ProductRef, Amount, AmountAfter - Amount);
		{aborted, Reason} ->
			{error, Reason}
	end;
adjustment(#adjustment{amount = Amount1, service = ServiceRef, product = undefined,
		units = Units, start_date = StartDate, end_date = EndDate})
		when is_list(ServiceRef), is_integer(Amount1), Amount1 < 0 ->
	F1 = fun() ->
		case mnesia:read(service, ServiceRef, read) of
			[#service{product = ProductRef}] ->
				case mnesia:read(product, ProductRef, write) of
					[#product{balance = []} = P] ->
						BId = generate_bucket_id(),
						NewBucket = #bucket{id = BId, start_date = StartDate,
								end_date = EndDate, remain_amount = Amount1,
								attributes = #{bucket_type => normal},
								units = Units, product = [ProductRef],
								last_modified = make_lm()},
						mnesia:write(bucket, NewBucket, write),
						NewProduct = P#product{balance = [BId]},
						ok = mnesia:write(product, NewProduct, write),
						{[BId], [BId], [], ProductRef};
					[#product{balance = BucketRefs} = P] ->
						F2 = fun F([H | T], Amount2, Acc, WrittenRefs, DeletedRefs) ->
							case mnesia:read(bucket, H, write) of
								[#bucket{id = Id, remain_amount = Remain, units = Units} = B]
										when Remain > Amount2 ->
									NewBucket = B#bucket{remain_amount = Remain - Amount2},
									ok = mnesia:write(bucket, NewBucket, write),
									{[Id | Acc] ++ T, [NewBucket#bucket.id | WrittenRefs], DeletedRefs};
								[#bucket{id = Id, remain_amount = Amount2, units = Units}] ->
									ok = mnesia:delete(bucket, Id, write),
									{Acc ++ T, WrittenRefs, [Id | DeletedRefs]};
								[#bucket{id = Id, remain_amount = Remain, units = Units}] ->
									ok = mnesia:delete(bucket, Id, write),
									F(T, Amount2 - Remain, Acc, WrittenRefs, [Id | DeletedRefs]);
								[#bucket{}] ->
									F(T, Amount2, [H | Acc], WrittenRefs, DeletedRefs)
							end;
						F([], Amount2, Acc, WrittenRefs, DeletedRefs) ->
							BId = generate_bucket_id(),
							NewBucket = #bucket{id = BId, start_date = StartDate, end_date = EndDate,
									remain_amount = -Amount2, units = Units, product = [ProductRef],
									attributes = #{bucket_type => normal},
									last_modified = make_lm()},
							ok = mnesia:write(bucket, NewBucket, write),
							{[BId |Acc], [BId | WrittenRefs], DeletedRefs}
						end,
						{NewBucketRefs, WrittenRefs, DeletedRefs} = F2(BucketRefs, abs(Amount1), [], [], []),
						NewProduct = P#product{balance = NewBucketRefs},
						ok = mnesia:write(product, NewProduct, write),
						{NewBucketRefs, WrittenRefs, DeletedRefs, ProductRef};
					[] ->
						mnesia:abort(badarg)
				end;
			[] ->
				mnesia:abort(not_found)
		end
	end,
	case mnesia:transaction(F1) of
		{atomic, {_BucketRefs, WrittenRefs, DeletedRefs, ProductRef}} ->
			log_adjustment(undefined, DeletedRefs ,WrittenRefs,
					Units, ProductRef, Amount1, undefined);
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec log_adjustment(AmountAfter, DeleteRefs, BucketRefs, Units,
		ProductRef, Amount, AmountBefore) -> Result
	when
		AmountAfter :: integer() | undefined,
		DeleteRefs :: list(),
		BucketRefs :: list(),
		Units ::	cents | seconds | octets | messages,
		ProductRef :: [string()],
		Amount :: integer(),
		AmountBefore :: integer() | undefined,
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Log an adjustment in the ABMF log.
log_adjustment(AmountAfter, [], [H | T], Units,
		ProductRef, Amount, AmountBefore) ->
	case ocs_log:abmf_log(adjustment, undefined, H, Units,
			ProductRef, Amount, AmountBefore, AmountAfter,
			undefined, undefined, undefined, undefined, undefined,
			undefined, undefined) of
		ok ->
			log_adjustment(AmountAfter, [], T, Units, ProductRef,
				Amount, AmountBefore);
		{error, Reason} ->
			{error, Reason}
	end;
log_adjustment(AmountAfter, [H | T], [], Units,
		ProductRef, Amount, AmountBefore) ->
	case ocs_log:abmf_log(adjustment, undefined, H, Units,
			ProductRef, Amount, AmountBefore, AmountAfter,
			undefined, undefined, undefined, undefined, undefined,
			undefined, undefined) of
		ok ->
			log_adjustment(AmountAfter, [], T, Units, ProductRef,
					Amount, AmountBefore);
		{error, Reason} ->
			{error, Reason}
	end;
log_adjustment(AmountAfter, [H1 | T1], [H2 | T2], Units,
		ProductRef, Amount, AmountBefore) ->
	case ocs_log:abmf_log(adjustment, undefined, H1, Units,
			ProductRef, Amount, AmountBefore, AmountAfter,
			undefined, undefined, undefined, undefined, undefined,
			undefined, undefined) of
		ok ->
			case ocs_log:abmf_log(adjustment, undefined, H2, Units,
					ProductRef, Amount, AmountBefore, AmountAfter,
					undefined, undefined, undefined, undefined, undefined,
					undefined, undefined) of
				ok ->
					log_adjustment(AmountAfter, T1, T2, Units, ProductRef,
							Amount, AmountBefore);
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end;
log_adjustment(_AmountAfter, [], [], _Units,
		_ProductRef, _Amount, _AmountBefore) ->
	ok.

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
	F = fun F(start, Acc) ->
				F(mnesia:select(service, MatchSpec,
						?CHUNKSIZE, read), Acc);
			F('$end_of_table', Acc) ->
				{ok, Acc};
			F({error, Reason}, _Acc) ->
				{error, Reason};
			F({L, Cont}, Acc) ->
				F(mnesia:select(Cont), [L | Acc])
	end,
	case mnesia:ets(F, [start, []]) of
		{error, Reason} ->
			{error, Reason};
		{ok, Acc} when is_list(Acc) ->
			lists:flatten(lists:reverse(Acc))
	end.

-spec query_service(Cont, MatchId, MatchProduct) -> Result
	when
		Cont :: start | any(),
		MatchId :: Match,
		MatchProduct :: Match,
		Match :: {exact, string()} | {like, string()} | '_',
		Result :: {Cont1, [#service{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query services
query_service(Cont, MatchId, '_') ->
	MatchSpec = [{'_', [], ['$_']}],
	query_service1(Cont, MatchSpec, MatchId);
query_service(Cont, MatchId, {Op, String} = _MatchProduct)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	Product = case lists:last(String) of
		$% when Op == like ->
			lists:droplast(String) ++ '_';
		_ ->
         String
	end,
	MatchHead = #service{product = Product, _ = '_'},
	MatchSpec = [{MatchHead, [], ['$_']}],
   query_service1(Cont, MatchSpec, MatchId).
%% @hidden
query_service1(start, MatchSpec, MatchId) ->
	F = fun() ->
			mnesia:select(service, MatchSpec, ?CHUNKSIZE, read)
	end,
	query_service2(mnesia:ets(F), MatchId);
query_service1(Cont, _MatchSpec, MatchId) ->
	F = fun() ->
         mnesia:select(Cont)
   end,
	query_service2(mnesia:ets(F), MatchId).
%% @hidden
query_service2({Services, Cont}, '_') ->
	{Cont, Services};
query_service2({Services, Cont}, {Op, String})
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	F = case lists:last(String) of
		$% when Op == like ->
			Prefix = list_to_binary(lists:droplast(String)),
			Size = size(Prefix),
			fun
				(#service{name = Name}) when size(Name) >= Size ->
					case binary:part(Name, 0, Size) of
						Prefix ->
							true;
						_ ->
							false
					end;
				(_) ->
					false
			end;
		_ ->
			ExactMatch = list_to_binary(String),
			fun(#service{name = Name}) when Name == ExactMatch ->
					true;
				(_) ->
					false
			end
	end,
	{Cont, lists:filter(F, Services)};
query_service2('$end_of_table', _MatchId) ->
		{eof, []}.

-spec delete_service(Identity) -> ok
	when
		Identity :: string() | binary().
%% @doc Delete an entry in the service table.
delete_service(Identity) when is_list(Identity) ->
	delete_service(list_to_binary(Identity));
delete_service(Identity) when is_binary(Identity) ->
	F = fun() ->
			case mnesia:read(service, Identity, write) of
				[#service{product = undefined} = Service] ->
					{mnesia:delete(service, Identity, write), Service};
				[#service{product = ProdRef} = Service] ->
					case mnesia:read(product, ProdRef, write) of
						[#product{service = ServiceRefs} = P] ->
							P1 = P#product{service = ServiceRefs -- [Identity]},
							ok = mnesia:write(P1),
							{mnesia:delete(service, Identity, write), Service};
						[] ->
							{mnesia:delete(service, Identity, write), Service}
					end;
				[] ->
					mnesia:abort(not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {ok, Service}} ->
			ok = ocs_event:notify(delete_service, Service, service),
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
	Fvala = fun(#alteration{name = Name, type = one_time, period = undefined,
					units = Units, size = Size, amount = Amount})
					when length(Name) > 0, ((Units == octets)
					or (Units == seconds) or (Units == messages)),
					is_integer(Size), Size > 0, is_integer(Amount) ->
				true;
			(#alteration{name = Name, type = recurring, period = Period,
					units = Units, size = Size, amount = Amount})
					when length(Name) > 0, ((Period == hourly)
					or (Period == daily) or (Period == weekly)
					or (Period == monthly) or (Period == yearly)),
					((Units == octets) or (Units == seconds) or (Units == messages)),
					is_integer(Size), Size > 0, is_integer(Amount) ->
				true;
			(#alteration{name = Name, type = usage, period = undefined,
					units = Units, size = Size, amount = Amount})
					when length(Name) > 0,
					((Units == octets) or (Units == seconds) or (Units == messages)),
					is_integer(Size), Size > 0, is_integer(Amount) ->
				true;
			(#alteration{}) ->
				false
	end,
	Fvalp = fun(#price{name = Name, type = one_time, period = undefined,
					units = undefined, size = undefined, amount = Amount,
					alteration = undefined})
					when length(Name) > 0, is_integer(Amount), Amount > 0 ->
				true;
			(#price{name = Name, type = one_time, period = undefined,
					units = undefined, size = undefined, amount = Amount,
					alteration = #alteration{type = Type} = Alteration})
					when length(Name) > 0, is_integer(Amount) ->
				Fvala(Alteration);
			(#price{name = Name, type = recurring, period = Period,
					units = undefined, size = undefined, amount = Amount,
					alteration = undefined})
					when length(Name) > 0, ((Period == hourly)
					or (Period == daily) or (Period == weekly)
					or (Period == monthly) or (Period == yearly)),
					is_integer(Amount), Amount > 0 ->
				true;
			(#price{name = Name, type = recurring, period = Period,
					units = undefined, size = undefined, amount = Amount,
					alteration = #alteration{} = Alteration})
					when length(Name) > 0, ((Period == hourly)
					or (Period == daily) or (Period == weekly)
					or (Period == monthly) or (Period == yearly)),
					is_integer(Amount)  ->
				Fvala(Alteration);
			(#price{name = Name, type = usage, period = undefined,
					units = Units, size = Size, amount = Amount,
					alteration = undefined})
					when length(Name) > 0, ((Units == octets)
					or (Units == seconds) or (Units == messages)),
					is_integer(Size), Size > 0, is_integer(Amount),
					Amount >= 0 ->
				true;
			(#price{name = Name, type = usage, period = undefined,
					units = Units, size = Size, amount = Amount,
					alteration = #alteration{type = AltType} = Alteration})
					when length(Name) > 0, ((Units == octets)
					or (Units == seconds) or (Units == messages)),
					is_integer(Size), Size > 0, is_integer(Amount),
					AltType /= usage ->
				Fvala(Alteration);
			(#price{type = #pla_ref{href = "http" ++ _}, period = undefined,
					units = undefined, size = undefined, amount = undefined,
					alteration = undefined}) ->
				true;
			(#price{type = #pla_ref{href = "http" ++ _}, period = undefined,
					units = undefined, size = undefined, amount = undefined,
					alteration = #alteration{type = AltType} = Alteration})
					when AltType /= usage  ->
				Fvala(Alteration);
			(#price{type = tariff, period = undefined,
					units = Units, size = Size, amount = Amount,
					alteration = undefined})
					when is_integer(Size), Size > 0, ((Units == octets)
					or (Units == seconds) or (Units == messages)),
					((Amount == undefined) or (Amount == 0)) ->
				true;
			(#price{type = tariff, period = undefined,
					units = Units, size = Size, amount = Amount,
					alteration = #alteration{type = AltType} = Alteration})
					when is_integer(Size), Size > 0, ((Units == octets)
					or (Units == seconds) or (Units == messages)),
					((Amount == undefined) or (Amount == 0)),
					AltType /= usage  ->
				Fvala(Alteration);
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
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	LM = {TS, N},
	Fadd = fun() ->
		Offer1 = Offer#offer{last_modified = LM},
		{mnesia:write(offer, Offer1, write), Offer1}
	end,
	case mnesia:transaction(Fadd) of
		{atomic, {ok, Offer2}} ->
			ok = ocs_event:notify(create_offer, Offer2, product),
			{ok, Offer2};
		{aborted, Reason} ->
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
	F = fun() -> mnesia:read(offer, OfferID) end,
	case mnesia:transaction(F) of
		{atomic, [#offer{} = Offer]} ->
			{ok, Offer};
		{atomic, []} ->
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
	F = fun F(start, Acc) ->
				F(mnesia:select(offer, MatchSpec,
						?CHUNKSIZE, read), Acc);
			F('$end_of_table', Acc) ->
				{ok, Acc};
			F({error, Reason}, _Acc) ->
				{error, Reason};
			F({L, Cont}, Acc) ->
				F(mnesia:select(Cont), [L | Acc])
	end,
	case mnesia:ets(F, [start, []]) of
		{error, Reason} ->
			{error, Reason};
		{ok, Acc} when is_list(Acc) ->
			lists:flatten(lists:reverse(Acc))
	end.

-spec delete_offer(OfferID) -> Result
	when
		OfferID :: string(),
		Result :: ok.
%% @doc Delete an entry from the offer table.
delete_offer(OfferID) ->
	Ftrans = fun() ->
			case mnesia:read(offer, OfferID, write) of
				[#offer{} = Offer] ->
					Fselect = fun F(start) ->
								MatchSpec = [{#product{product = OfferID,
										_ = '_'}, [], ['$_']}],
								F(mnesia:select(product, MatchSpec, 1, read));
							F({[], Cont}) ->
								F(mnesia:select(Cont));
							F({_, _Cont}) ->
								mnesia:abort(unable_to_delete);
							F('$end_of_table') ->
								{mnesia:delete(offer, OfferID, write), Offer}
					end,
					Fselect(start);
				[] ->
					mnesia:abort(not_found)
			end
	end,
	case mnesia:transaction(Ftrans) of
		{atomic, {ok, Offer}} ->
			ok = ocs_event:notify(delete_offer, Offer, product),
			ok;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec query_offer(Cont, Name, Description, Status, SDT, EDT, Price) -> Result
	when
		Cont :: start | any(),
		Name :: Match,
		Description :: Match,
		Status :: Match,
		SDT :: Match,
		EDT:: Match,
		Price :: Match,
		Match :: {exact, string()} | {notexact, string()} | {like, string()} | '_',
		Result :: {Cont1, [#offer{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query offer entires
query_offer(Cont, '_' = _Name, Description, Status, STD, EDT, Price) ->
	MatchSpec = [{'_', [], ['$_']}],
	query_offer1(Cont, MatchSpec, Description, Status, STD, EDT, Price);
query_offer(Cont, {Op, String}, Description, Status, STD, EDT, Price)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	 MatchSpec = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			MatchHead = #offer{name = Prefix ++ '_', _ = '_'},
			[{MatchHead, [], ['$_']}];
		_ ->
			MatchHead = #offer{name = String, _ = '_'},
			[{MatchHead, [], ['$_']}]
	end,
	query_offer1(Cont, MatchSpec, Description, Status, STD, EDT, Price).
%% @hidden
query_offer1(start, MatchSpec, Description, Status, STD, EDT, Price) ->
	F = fun() ->
		mnesia:select(offer, MatchSpec, ?CHUNKSIZE, read)
	end,
	query_offer2(mnesia:ets(F), Description, Status, STD, EDT, Price);
query_offer1(Cont, _MatchSpec, Description, Status, STD, EDT, Price) ->
	F = fun() ->
		mnesia:select(Cont)
	end,
	query_offer2(mnesia:ets(F), Description, Status, STD, EDT, Price).
%% @hidden
query_offer2({Offers, Cont}, '_', '_', '_', '_', '_') ->
	{Cont, Offers};
query_offer2('$end_of_table', _Description, _Status, _STD, _EDT, _Price) ->
	{eof, []}.

-spec add_resource(Resource) -> Result
	when
		Result :: {ok, Resource} | {error, Reason},
		Reason :: table_not_found | table_exists | missing_char | term().
%% @doc Create a new Resource.
add_resource(#resource{id = undefined,
		specification = #specification_ref{id = SpecId},
		last_modified = undefined} = Resource)
		when SpecId == ?POLICY_TABLE_SPEC;
		SpecId == ?POLICY_ROW_SPEC ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	Id = integer_to_list(TS) ++ integer_to_list(N),
	LM = {TS, N},
	Href = ?PathResInv ++ "resource/" ++ Id,
	NewResource = Resource#resource{id = Id,
			href = Href, last_modified = LM},
	F = fun() ->
				ok = mnesia:write(NewResource),
				NewResource
	end,
   add_resource1(mnesia:transaction(F));
add_resource(#resource{name = TableName,
		id = undefined, href = undefined,
		specification = #specification_ref{id = SpecId},
		last_modified = undefined} = Resource)
		when is_list(TableName),
		((SpecId == ?TARIFF_TABLE_SPEC)
				orelse (SpecId == ?PERIOD_TABLE_SPEC)
				orelse (SpecId == ?ROAMING_TABLE_SPEC)) ->
	case mnesia:table_info(list_to_existing_atom(TableName),
			attributes) of
		[num, value] ->
			Find = fun F(eof, Acc) ->
						lists:flatten(Acc);
					F(Cont1, Acc) ->
						{Cont2, L} = ocs:query_resource(Cont1,
								'_', {exact, TableName}, {exact, SpecId}, '_'),
						F(Cont2, [L | Acc])
			end,
			case Find(start, []) of
				[] ->
					TS = erlang:system_time(millisecond),
					N = erlang:unique_integer([positive]),
					Id = integer_to_list(TS) ++ integer_to_list(N),
					LM = {TS, N},
					Href = ?PathResInv ++ "resource/" ++ Id,
					NewResource = Resource#resource{id = Id,
							href = Href, last_modified = LM},
					Ftrans = fun() ->
								ok = mnesia:write(NewResource),
								NewResource
					end,
					add_resource1(mnesia:transaction(Ftrans));
				[#resource{} | _] ->
					{error, table_exists}
			end;
		_ ->
			{error, table_not_found}
	end;
add_resource(#resource{id = undefined, href = undefined,
		specification = #specification_ref{id = ?TARIFF_ROW_SPEC},
		related = [#resource_rel{name = TableName} | _],
		characteristic = Chars} = Resource)
		when is_list(TableName), is_list(Chars) ->
	try
		Prefix = case lists:keyfind("prefix",
				#resource_char.name, Chars) of
			#resource_char{value = Char1} when is_list(Char1) ->
				Char1;
			_ ->
				throw(missing_char)
		end,
		Description = case lists:keyfind("description",
				#resource_char.name, Chars) of
			#resource_char{value = Char2} when is_list(Char2) ->
				Char2;
			_ ->
				undefined
		end,
		Rate = case lists:keyfind("rate",
				#resource_char.name, Chars) of
			#resource_char{value = Char3}
					when is_list(Char3); is_integer(Char3); is_float(Char3)  ->
				ocs_rest:millionths_in(Char3);
			_ ->
				throw(missing_char)
		end,
		ocs_gtt:insert(TableName, Prefix, {Description, Rate})
	of
		{ok, #gtt{value = {_, _, {TS, N} = LM}}} ->
			Id = integer_to_list(TS) ++ integer_to_list(N),
			Href = ?PathResInv ++ "resource/" ++ Id,
			NewResource = Resource#resource{id = Id,
					href = Href, last_modified = LM},
			Ftrans = fun() ->
						ok = mnesia:write(NewResource),
						NewResource
			end,
			add_resource1(mnesia:transaction(Ftrans))
	catch
		_:Reason ->
			{error, Reason}
	end;
add_resource(#resource{id = undefined, href = undefined,
		specification = #specification_ref{id = ?PERIOD_ROW_SPEC},
		related = [#resource_rel{name = TableName} | _],
		characteristic = Chars} = Resource)
		when is_list(TableName), is_list(Chars) ->
	try
		Prefix = case lists:keyfind("prefix",
				#resource_char.name, Chars) of
			#resource_char{value = Char1} when is_list(Char1) ->
				Char1;
			_ ->
				throw(missing_char)
		end,
		Description = case lists:keyfind("description",
				#resource_char.name, Chars) of
			#resource_char{value = Char2} when is_list(Char2) ->
				Char2;
			_ ->
				undefined
		end,
		PeriodInitial = case lists:keyfind("periodInitial",
				#resource_char.name, Chars) of
			#resource_char{value = Char3} when is_integer(Char3) ->
				Char3;
			_ ->
				throw(missing_char)
		end,
		RateInitial = case lists:keyfind("rateInitial",
				#resource_char.name, Chars) of
			#resource_char{value = Char4}
					when is_list(Char4); is_integer(Char4); is_float(Char4)  ->
				ocs_rest:millionths_in(Char4);
			_ ->
				throw(missing_char)
		end,
		PeriodAdditional = case lists:keyfind("periodAdditional",
				#resource_char.name, Chars) of
			#resource_char{value = Char5} when is_integer(Char5) ->
				Char5;
			_ ->
				throw(missing_char)
		end,
		RateAdditional = case lists:keyfind("rateAdditional",
				#resource_char.name, Chars) of
			#resource_char{value = Char6}
					when is_list(Char6); is_integer(Char6); is_float(Char6)  ->
				ocs_rest:millionths_in(Char6);
			_ ->
				throw(missing_char)
		end,
		ocs_gtt:insert(TableName, Prefix,
				{Description, PeriodInitial, RateInitial,
				PeriodAdditional, RateAdditional})
	of
		{ok, #gtt{value = {_, _, _, _, _, {TS, N} = LM}}} ->
			Id = integer_to_list(TS) ++ integer_to_list(N),
			Href = ?PathResInv ++ "resource/" ++ Id,
			NewResource = Resource#resource{id = Id,
					href = Href, last_modified = LM},
			Ftrans = fun() ->
						ok = mnesia:write(NewResource),
						NewResource
			end,
			add_resource1(mnesia:transaction(Ftrans))
	catch
		_:Reason ->
			{error, Reason}
	end;
add_resource(#resource{id = undefined, href = undefined,
		specification = #specification_ref{id = ?ROAMING_ROW_SPEC},
		related = [#resource_rel{name = TableName} | _],
		characteristic = Chars} = Resource)
		when is_list(TableName), is_list(Chars) ->
	try
		Prefix = case lists:keyfind("prefix",
				#resource_char.name, Chars) of
			#resource_char{value = Char1} when is_list(Char1) ->
				Char1;
			_ ->
				throw(missing_char)
		end,
		Description = case lists:keyfind("description",
				#resource_char.name, Chars) of
			#resource_char{value = Char2} when is_list(Char2) ->
				Char2;
			_ ->
				undefined
		end,
		Tariff = case lists:keyfind("tariff",
				#resource_char.name, Chars) of
			#resource_char{value = Char3} when is_list(Char3) ->
				Char3;
			_ ->
				throw(missing_char)
		end,
		ocs_gtt:insert(TableName, Prefix, {Description, Tariff})
	of
		{ok, #gtt{value = {_, _, {TS, N} = LM}}} ->
			Id = integer_to_list(TS) ++ integer_to_list(N),
			Href = ?PathResInv ++ "resource/" ++ Id,
			NewResource = Resource#resource{id = Id,
					href = Href, last_modified = LM},
			Ftrans = fun() ->
						ok = mnesia:write(NewResource),
						NewResource
			end,
			add_resource1(mnesia:transaction(Ftrans))
	catch
		_:Reason ->
			{error, Reason}
	end.
%% @hidden
add_resource1({atomic, #resource{} = Resource}) ->
	ok = ocs_event:notify(create_resource, Resource, resource),
	{ok, Resource};
add_resource1({aborted, Reason}) ->
	{error, Reason}.

-spec update_resource(Resource) -> Result
	when
		Result :: {ok, Resource} | {error, Reason},
		Reason :: not_found | not_allowed | stale | missing_char | term().
%% @doc Update an existing Resource.
update_resource(#resource{id = Id, href = Href,
		specification = #specification_ref{id = SpecId},
		last_modified = LM} = Resource)
		when is_list(Id), is_list(Href), is_tuple(LM),
		((SpecId == ?POLICY_TABLE_SPEC)
				orelse (SpecId == ?POLICY_ROW_SPEC)) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	NewLM = {TS, N},
	Ftrans = fun() ->
				case mnesia:read(resource, Id, write) of
					[#resource{specification = #specification_ref{id = SpecId},
							last_modified = LM}] ->
						Resource1 = Resource#resource{last_modified = NewLM},
						ok = mnesia:write(Resource1),
						Resource1;
					[#resource{specification = #specification_ref{id = SpecId}}] ->
						mnesia:abort(stale);
					[#resource{}] ->
						mnesia:abort(not_allowed);
					[] ->
						mnesia:abort(not_found)
				end
	end,
   update_resource1(mnesia:transaction(Ftrans));
update_resource(#resource{id = Id, href = Href,
		specification = #specification_ref{id = SpecId},
		name = TableName, last_modified = LM} = Resource)
		when is_list(Id), is_list(Href), is_list(TableName),
		is_tuple(LM), ((SpecId == ?TARIFF_TABLE_SPEC)
				orelse (SpecId == ?PERIOD_TABLE_SPEC)
				orelse (SpecId == ?ROAMING_TABLE_SPEC)) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	NewLM = {TS, N},
	Ftrans = fun() ->
				case mnesia:read(resource, Id, write) of
					[#resource{name = TableName,
							specification = #specification_ref{id = SpecId},
							last_modified = LM}] ->
						Resource1 = Resource#resource{last_modified = NewLM},
						ok = mnesia:write(Resource1),
						Resource1;
					[#resource{name = TableName,
							specification = #specification_ref{id = SpecId}}] ->
						mnesia:abort(stale);
					[#resource{}] ->
						mnesia:abort(not_allowed);
					[] ->
						mnesia:abort(not_found)
				end
	end,
   update_resource1(mnesia:transaction(Ftrans));
update_resource(#resource{id = Id, href = Href,
		specification = #specification_ref{id = SpecId},
		related = [#resource_rel{name = TableName} | _],
		last_modified = LM, characteristic = Chars} = Resource)
		when is_list(Id), is_list(Href), is_list(TableName),
		is_list(Chars), is_tuple(LM), SpecId == ?TARIFF_ROW_SPEC ->
	try
		Prefix = case lists:keyfind("prefix",
				#resource_char.name, Chars) of
			#resource_char{value = Char1} when is_list(Char1) ->
				Char1;
			_ ->
				throw(missing_char)
		end,
		Description = case lists:keyfind("description",
				#resource_char.name, Chars) of
			#resource_char{value = Char2} when is_list(Char2) ->
				Char2;
			_ ->
				undefined
		end,
		Rate = case lists:keyfind("rate",
				#resource_char.name, Chars) of
			#resource_char{value = Char3}
					when is_list(Char3); is_integer(Char3); is_float(Char3)  ->
				ocs_rest:millionths_in(Char3);
			_ ->
				throw(missing_char)
		end,
		{Resource, Prefix, Description, Rate}
	of
		{Resource1, Prefix1, Description1, Rate1} ->
			Ftrans = fun() ->
					case mnesia:read(resource, Id, write) of
						[#resource{specification = #specification_ref{id = SpecId},
								related = [#resource_rel{name = TableName} | _],
								characteristic = Chars1, last_modified = LM}] ->
							case lists:keyfind("prefix",
									#resource_char.name, Chars1) of
								#resource_char{value = Prefix1} ->
									{ok, #gtt{value = {_, _, NewLM}}}
											= ocs_gtt:insert(TableName,
											Prefix1, {Description1, Rate1}),
									Resource2 = Resource1#resource{last_modified = NewLM},
									ok = mnesia:write(Resource2),
									Resource2;
								#resource_char{} ->
									mnesia:abort(not_allowed)
							end;
						[#resource{specification = #specification_ref{id = SpecId},
								related = [#resource_rel{name = TableName} | _]}] ->
							mnesia:abort(stale);
						[#resource{}] ->
							mnesia:abort(not_allowed);
						[] ->
							mnesia:abort(not_found)
					end
			end,
			update_resource1(mnesia:transaction(Ftrans))
	catch
		_:Reason ->
			{error, Reason}
	end;
update_resource(#resource{id = Id, href = Href,
		specification = #specification_ref{id = SpecId},
		related = [#resource_rel{name = TableName} | _],
		characteristic = Chars, last_modified = LM} = Resource)
		when is_list(Id), is_list(Href), is_list(TableName),
		is_list(Chars), is_tuple(LM), SpecId == ?PERIOD_ROW_SPEC->
	try
		Prefix = case lists:keyfind("prefix",
				#resource_char.name, Chars) of
			#resource_char{value = Char1} when is_list(Char1) ->
				Char1;
			_ ->
				throw(missing_char)
		end,
		Description = case lists:keyfind("description",
				#resource_char.name, Chars) of
			#resource_char{value = Char2} when is_list(Char2) ->
				Char2;
			_ ->
				undefined
		end,
		PeriodInitial = case lists:keyfind("periodInitial",
				#resource_char.name, Chars) of
			#resource_char{value = Char3} when is_integer(Char3) ->
				Char3;
			_ ->
				throw(missing_char)
		end,
		RateInitial = case lists:keyfind("rateInitial",
				#resource_char.name, Chars) of
			#resource_char{value = Char4}
					when is_list(Char4); is_integer(Char4); is_float(Char4)  ->
				ocs_rest:millionths_in(Char4);
			_ ->
				throw(missing_char)
		end,
		PeriodAdditional = case lists:keyfind("periodAdditional",
				#resource_char.name, Chars) of
			#resource_char{value = Char5} when is_integer(Char5) ->
				Char5;
			_ ->
				throw(missing_char)
		end,
		RateAdditional = case lists:keyfind("rateAdditional",
				#resource_char.name, Chars) of
			#resource_char{value = Char6}
					when is_list(Char6); is_integer(Char6); is_float(Char6)  ->
				ocs_rest:millionths_in(Char6);
			_ ->
				throw(missing_char)
		end,
		{Resource, Prefix, Description, PeriodInitial, RateInitial, PeriodAdditional, RateAdditional}
	of
		{Resource1, Prefix1, Description1,
				PeriodInitial1, RateInitial1, PeriodAdditional1, RateAdditional1} ->
			Ftrans = fun() ->
					case mnesia:read(resource, Id, write) of
						[#resource{specification = #specification_ref{id = SpecId},
								related = [#resource_rel{name = TableName} | _],
								characteristic = Chars1, last_modified = LM}] ->
							case lists:keyfind("prefix",
									#resource_char.name, Chars1) of
								#resource_char{value = Prefix1} ->
									{ok, #gtt{value = {_, _, _, _, _, NewLM}}}
											= ocs_gtt:insert(TableName, Prefix1,
											{Description1, PeriodInitial1, RateInitial1,
											PeriodAdditional1, RateAdditional1}),
									Resource2 = Resource1#resource{last_modified = NewLM},
									ok = mnesia:write(Resource2),
									Resource2;
								#resource_char{} ->
									mnesia:abort(not_allowed)
							end;
						[#resource{specification = #specification_ref{id = SpecId},
								related = [#resource_rel{name = TableName} | _]}] ->
							mnesia:abort(stale);
						[#resource{}] ->
							mnesia:abort(not_allowed);
						[] ->
							mnesia:abort(not_found)
					end
			end,
			update_resource1(mnesia:transaction(Ftrans))
	catch
		_:Reason ->
			{error, Reason}
	end;
update_resource(#resource{id = Id, href = Href,
		specification = #specification_ref{id = SpecId},
		related = [#resource_rel{name = TableName} | _],
		characteristic = Chars, last_modified = LM} = Resource)
		when is_list(Id), is_list(Href), is_list(TableName),
		is_list(Chars), is_tuple(LM), SpecId == ?ROAMING_ROW_SPEC->
	try
		Prefix = case lists:keyfind("prefix",
				#resource_char.name, Chars) of
			#resource_char{value = Char1} when is_list(Char1) ->
				Char1;
			_ ->
				throw(missing_char)
		end,
		Description = case lists:keyfind("description",
				#resource_char.name, Chars) of
			#resource_char{value = Char2} when is_list(Char2) ->
				Char2;
			_ ->
				undefined
		end,
		Tariff = case lists:keyfind("tariff",
				#resource_char.name, Chars) of
			#resource_char{value = Char3} when is_list(Char3) ->
				Char3;
			_ ->
				throw(missing_char)
		end,
		{Resource, Prefix, Description, Tariff}
	of
		{Resource1, Prefix1, Description1, Tariff1} ->
			Ftrans = fun() ->
					case mnesia:read(resource, Id, write) of
						[#resource{specification = #specification_ref{id = SpecId},
								related = [#resource_rel{name = TableName} | _],
								characteristic = Chars1, last_modified = LM}] ->
							case lists:keyfind("prefix",
									#resource_char.name, Chars1) of
								#resource_char{value = Prefix1} ->
									{ok, #gtt{value = {_, _, NewLM}}}
											= ocs_gtt:insert(TableName,
													Prefix1, {Description1, Tariff1}),
									Resource2 = Resource1#resource{last_modified = NewLM},
									ok = mnesia:write(Resource2),
									Resource2;
								#resource_char{} ->
									mnesia:abort(not_allowed)
							end;
						[#resource{specification = #specification_ref{id = SpecId},
								related = [#resource_rel{name = TableName} | _]}] ->
							mnesia:abort(stale);
						[#resource{}] ->
							mnesia:abort(not_allowed);
						[] ->
							mnesia:abort(not_found)
					end
			end,
			update_resource1(mnesia:transaction(Ftrans))
	catch
		_:Reason ->
			{error, Reason}
	end.
%% @hidden
%% @todo update_resource event
update_resource1({atomic, #resource{} = Resource}) ->
	ok = ocs_event:notify(create_resource, Resource, resource),
	{ok, Resource};
update_resource1({aborted, Reason}) ->
	{error, Reason}.

-spec get_resources() -> Result
	when
		Result :: [#resource{}] | {error, Reason},
		Reason :: term().
%% @doc List all entries in the resource table.
get_resources() ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun F(start, Acc) ->
				F(mnesia:select(resource, MatchSpec,
						?CHUNKSIZE, read), Acc);
			F('$end_of_table', Acc) ->
				{ok, Acc};
			F({error, Reason}, _Acc) ->
				{error, Reason};
			F({L, Cont}, Acc) ->
				F(mnesia:select(Cont), [L | Acc])
	end,
	case mnesia:ets(F, [start, []]) of
		{error, Reason} ->
			{error, Reason};
		{ok, Acc} when is_list(Acc) ->
			lists:flatten(lists:reverse(Acc))
	end.

-spec get_resource(ResourceID) -> Result
	when
		ResourceID :: string(),
		Result :: {ok, Resource} | {error, Reason},
		Resource :: resource(),
		Reason :: not_found | term().
%% @doc Get a Resource by identifier.
get_resource(ResourceID) when is_list(ResourceID) ->
	F = fun() ->
			mnesia:read(resource, ResourceID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Resource]} ->
			{ok, Resource};
		{atomic, []} ->
			{error, not_found}
	end.

-spec delete_resource(ResourceID) -> Result
	when
		ResourceID :: string(),
		Result :: ok | {error, Reason},
		Reason :: not_found | missing_char | term().
%% @doc Delete a Resource.
delete_resource(ResourceID) when is_list(ResourceID) ->
	F = fun() ->
			case mnesia:read(resource, ResourceID, write) of
				[#resource{specification = #specification_ref{id = SpecId},
						related = [#resource_rel{name = TableName} | _],
						characteristic = Chars} = Resource]
						when is_list(TableName),
						((SpecId == ?TARIFF_ROW_SPEC)
								orelse (SpecId == ?PERIOD_ROW_SPEC)
								orelse (SpecId == ?ROAMING_ROW_SPEC)) ->
					case lists:keyfind("prefix", #resource_char.name, Chars) of
						#resource_char{value = Prefix} when is_list(Prefix) ->
							{mnesia:delete(resource, ResourceID, write),
									Resource, TableName, Prefix};
						false ->
							{mnesia:delete(resource, ResourceID, write), Resource}
					end;
				[#resource{} = Resource] ->
					{mnesia:delete(resource, ResourceID, write), Resource};
				[] ->
					mnesia:abort(not_found)
			end
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, {ok, Resource, TableName, Prefix}} ->
			ok = ocs_gtt:delete(TableName, Prefix),
			ocs_event:notify(delete_resource, Resource, resource);
		{atomic, {ok, #resource{} = Resource}} ->
			ocs_event:notify(delete_resource, Resource, resource)
	end.

-spec query_resource(Cont, MatchId, MatchName,
		MatchResSpecId, MatchRelName) -> Result
	when
		Cont :: start | any(),
		MatchId :: match() | [match()] | '_',
		MatchName :: match() | [match()] | '_',
		MatchResSpecId :: match() | [match()] | '_',
		MatchRelName :: match() | [match()] | '_',
		Result :: {Cont1, [#resource{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query resources
query_resource(Cont, '_',
		MatchName, MatchResSpecId, MatchRelName) ->
	MatchHead = #resource{_ = '_'},
	MatchCond = [],
	query_resource2(Cont, MatchHead, MatchCond,
			MatchName, MatchResSpecId, MatchRelName);
query_resource(Cont, {Op, String},
		MatchName, MatchResSpecId, MatchRelName)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead = case lists:last(String) of
		$% when Op == like ->
			#resource{id = lists:droplast(String) ++ '_', _ = '_'};
		_ ->
         #resource{id = String, _ = '_'}
	end,
	query_resource2(Cont, MatchHead, [],
			MatchName, MatchResSpecId, MatchRelName);
query_resource(Cont, MatchId,
		MatchName, MatchResSpecId, MatchRelName)
		when is_tuple(MatchId) ->
	query_resource(Cont, [MatchId],
			MatchName, MatchResSpecId, MatchRelName);
query_resource(Cont, MatchId,
		MatchName, MatchResSpecId, MatchRelName)
		when is_list(MatchId) ->
	MatchHead = #resource{id = '$1', _ = '_'},
	query_resource1(Cont, MatchHead, [], MatchId,
			MatchName, MatchResSpecId, MatchRelName).
%% @hidden
query_resource1(Cont, MatchHead, MatchIdCond, [H | T],
		MatchName, MatchResSpecId, MatchRelName) ->
	MatchIdCond1 = [match_condition('$1', H) | MatchIdCond],
	query_resource1(Cont, MatchHead, MatchIdCond1, T,
			MatchName, MatchResSpecId, MatchRelName);
query_resource1(Cont, MatchHead, MatchIdCond, [],
		MatchName, MatchResSpecId, MatchRelName) ->
	MatchCond = case MatchIdCond of
		[] ->
			[];
		[Cond] ->
			[Cond];
		_ ->
			[list_to_tuple(['or' | lists:reverse(MatchIdCond)])]
	end,
	query_resource2(Cont, MatchHead, MatchCond,
			MatchName, MatchResSpecId, MatchRelName).
%% @hidden
query_resource2(Cont, MatchHead, MatchCond, '_',
		MatchResSpecId, MatchRelName) ->
	query_resource4(Cont, MatchHead, MatchCond,
			MatchResSpecId, MatchRelName);
query_resource2(Cont, MatchHead, MatchCond, {Op, String},
		MatchResSpecId, MatchRelName)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead1 = case lists:last(String) of
		$% when Op == like ->
			MatchHead#resource{name = lists:droplast(String) ++ '_'};
		_ ->
         MatchHead#resource{name = String}
	end,
   query_resource4(Cont, MatchHead1, MatchCond,
			MatchResSpecId, MatchRelName);
query_resource2(Cont, MatchHead, MatchCond,
		MatchName, MatchResSpecId, MatchRelName)
		when is_tuple(MatchName) ->
   query_resource2(Cont, MatchHead, MatchCond,
			[MatchName], MatchResSpecId, MatchRelName);
query_resource2(Cont, MatchHead, MatchCond,
		MatchName, MatchResSpecId, MatchRelName)
		when is_list(MatchName) ->
	MatchHead1 = MatchHead#resource{name = '$2'},
   query_resource3(Cont, MatchHead1, MatchCond, [],
			MatchName, MatchResSpecId, MatchRelName).
%% @hidden
query_resource3(Cont, MatchHead, MatchCond, MatchNameCond,
		[H | T], MatchResSpecId, MatchRelName) ->
	MatchNameCond1 = [match_condition('$2', H) | MatchNameCond],
	query_resource3(Cont, MatchHead, MatchCond, MatchNameCond1,
		T, MatchResSpecId, MatchRelName);
query_resource3(Cont, MatchHead, MatchCond, MatchNameCond,
		[], MatchResSpecId, MatchRelName) ->
	MatchCond1 = case MatchNameCond of
		[] ->
			MatchCond;
		[Cond] ->
			[Cond | MatchCond];
		_ ->
			[list_to_tuple(['or' | lists:reverse(MatchNameCond)])
					| MatchCond]
	end,
	query_resource4(Cont, MatchHead, MatchCond1,
			MatchResSpecId, MatchRelName).
%% @hidden
query_resource4(Cont, MatchHead, MatchCond, '_',
		MatchRelName) ->
	query_resource6(Cont, MatchHead, MatchCond,
			MatchRelName);
query_resource4(Cont, MatchHead, MatchCond, {Op, String},
		MatchRelName)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead1 = case lists:last(String) of
		$% when Op == like ->
			MatchHead#resource{specification = #specification_ref{id
					= lists:droplast(String) ++ '_', _ = '_'}};
		_ ->
         MatchHead#resource{specification
					= #specification_ref{id = String, _ = '_'}}
	end,
   query_resource6(Cont, MatchHead1, MatchCond, MatchRelName);
query_resource4(Cont, MatchHead, MatchCond, MatchResSpecId,
		MatchRelName)
		when is_tuple(MatchResSpecId) ->
	query_resource4(Cont, MatchHead, MatchCond,
			[MatchResSpecId], MatchRelName);
query_resource4(Cont, MatchHead, MatchCond, MatchResSpecId,
		MatchRelName)
		when is_list(MatchResSpecId) ->
	MatchHead1 = MatchHead#resource{specification
			= #specification_ref{id = '$3', _ = '_'}},
	query_resource5(Cont, MatchHead1, MatchCond, [],
			MatchResSpecId, MatchRelName).
%% @hidden
query_resource5(Cont, MatchHead, MatchCond,
		MatchResSpecIdCond, [H | T], MatchRelName) ->
	MatchResSpecIdCond1 = [match_condition('$3', H) | MatchResSpecIdCond],
	query_resource5(Cont, MatchHead, MatchCond,
			MatchResSpecIdCond1, T, MatchRelName);
query_resource5(Cont, MatchHead, MatchCond,
		MatchResSpecIdCond, [], MatchRelName) ->
	MatchCond1 = case MatchResSpecIdCond of
		[] ->
			MatchCond;
		[Cond] ->
			[Cond | MatchCond];
		_ ->
			[list_to_tuple(['or' | lists:reverse(MatchResSpecIdCond)])
					| MatchCond]
	end,
	query_resource6(Cont, MatchHead, MatchCond1, MatchRelName).
%% @hidden
query_resource6(Cont, MatchHead, MatchCond, '_')
		when length(MatchCond) > 1 ->
	MatchCond1 = [list_to_tuple(['and'
			| lists:reverse(MatchCond)])],
	MatchSpec = [{MatchHead, MatchCond1, ['$_']}],
	query_resource8(Cont, MatchSpec);
query_resource6(Cont, MatchHead, MatchCond, '_') ->
	MatchSpec = [{MatchHead, MatchCond, ['$_']}],
	query_resource8(Cont, MatchSpec);
query_resource6(Cont, MatchHead, MatchCond, {Op, String})
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchHead1 = case lists:last(String) of
		$% when Op == like ->
			MatchHead#resource{related = [#resource_rel{name
					= lists:droplast(String) ++ '_', _ = '_'}]};
		_ ->
         MatchHead#resource{related
					= [#resource_rel{name = String, _ = '_'}]}
	end,
	MatchCond1 = case length(MatchCond) of
		N when N > 1 ->
			[list_to_tuple(['and' | lists:reverse(MatchCond)])];
		_N ->
			MatchCond
	end,
	MatchSpec = [{MatchHead1, MatchCond1, ['$_']}],
   query_resource8(Cont, MatchSpec);
query_resource6(Cont, MatchHead, MatchCond, MatchResSpecId)
		when is_tuple(MatchResSpecId) ->
	query_resource6(Cont, MatchHead, MatchCond, [MatchResSpecId]);
query_resource6(Cont, MatchHead, MatchCond, MatchResSpecId)
		when is_list(MatchResSpecId) ->
	MatchHead1 = MatchHead#resource{related
			= [#resource_rel{name = '$4', _ = '_'}]},
	query_resource7(Cont, MatchHead1, MatchCond, [], MatchResSpecId).
%% @hidden
query_resource7(Cont, MatchHead, MatchCond, MatchRelNameCond, [H | T]) ->
	MatchRelNameCond1 = [match_condition('$4', H) | MatchRelNameCond],
	query_resource7(Cont, MatchHead, MatchCond, MatchRelNameCond1, T);
query_resource7(Cont, MatchHead, MatchCond, MatchRelNameCond, []) ->
	MatchCond1 = case MatchRelNameCond of
		[] ->
			MatchCond;
		[Cond] ->
			[Cond | MatchCond];
		_ ->
			[list_to_tuple(['or' | lists:reverse(MatchRelNameCond)])
					| MatchCond]
	end,
	MatchCond2 = case length(MatchCond1) of
		N when N > 1 ->
			[list_to_tuple(['and' | lists:reverse(MatchCond1)])];
		_N ->
			MatchCond
	end,
	MatchSpec = [{MatchHead, MatchCond2, ['$_']}],
	query_resource8(Cont, MatchSpec).
%% @hidden
query_resource8(start, MatchSpec) ->
	F = fun() ->
			mnesia:select(resource, MatchSpec, ?CHUNKSIZE, read)
	end,
	query_resource9(mnesia:ets(F));
query_resource8(Cont, _MatchSpec) ->
	F = fun() ->
         mnesia:select(Cont)
   end,
	query_resource9(mnesia:ets(F)).
%% @hidden
query_resource9({Resources, Cont}) ->
	{Cont, Resources};
query_resource9('$end_of_table') ->
		{eof, []}.

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

-type eap_method() :: pwd | ttls | aka | akap.
-spec start(Protocol, Type, Address, Port, Options) -> Result
	when
		Protocol :: radius | diameter,
		Type :: auth | acct,
		Address :: inet:ip_address(),
		Port :: pos_integer(),
		Options :: [Option],
		Option :: EapOption | SubOption | NrfOption | diameter:service_opt(),
		EapOption :: {eap_method_prefer, EapType} | {eap_method_order, EapTypes},
		EapType :: eap_method(),
		EapTypes :: [eap_method()],
		SubOption :: {sub_id_type, [SubIdType]},
		SubIdType :: imsi | msisdn | nai | sip | private,
		NrfOption :: {nrf_uri, NrfUri} | {rf_class, RfClass},
		NrfUri :: uri_string:uri_string(),
		RfClass :: b | a,
		Result :: {ok, Pid} | {error, Reason},
		Pid :: pid(),
		Reason :: term().
%% @doc Start a RADIUS/DIAMETER request handler.
start(Protocol, Type, Address, Port, Options) when is_tuple(Address),
		is_integer(Port), is_list(Options) ->
	gen_server:call(ocs, {start, Protocol, Type, Address, Port, Options}).

-spec stop(Protocol, Type, Pid) -> Result
	when
		Protocol :: radius | diameter,
		Type :: auth | acct,
		Pid :: pid(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Stop a RADIUS/DIAMETER request handler.
stop(Protocol, Type, Pid) when is_pid(Pid) ->
	gen_server:call(ocs, {stop, Protocol, Type, Pid}).

-spec get_acct(Protocol) -> Result
	when
		Protocol :: radius | diameter,
		Result :: [pid()] | {error, Reason},
		Reason :: term().
%% @doc Get RADIUS/DIAMETER acct request handlers.
get_acct(Protocol)
		when Protocol == radius; Protocol == diameter ->
	gen_server:call(ocs, {get, Protocol, acct}).

-spec get_auth(Protocol) -> Result
	when
		Protocol :: radius | diameter,
		Result :: [pid()] | {error, Reason},
		Reason :: term().
%% @doc Get RADIUS/DIAMETER auth request handlers.
get_auth(Protocol)
		when Protocol == radius; Protocol == diameter ->
	gen_server:call(ocs, {get, Protocol, auth}).

-spec add_user(Username, Password, UserData) -> Result
	when
		Username :: string(),
		Password :: string(),
		UserData :: [{Name, Value}],
		Name :: atom(),
		Value :: term(),
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: user_exists | term().
%% @doc Add an HTTP user.
%% 	HTTP Basic authentication (RFC7617) is required with
%% 	`Username' and `Password' used to construct the
%% 	`Authorization' header in requests.
%%
%%		`UserData' contains addtional properties specific to each user.
%%
add_user(Username, Password, UserData) when is_list(Username),
		is_list(Password), is_list(UserData) ->
	add_user1(Username, Password, UserData, get_params()).
%% @hidden
add_user1(Username, Password, UserData, {Port, Address, Dir, Group}) ->
	add_user2(Username, Password, UserData,
			Address, Port, Dir, Group, ocs:get_user(Username));
add_user1(_, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user2(Username, Password, UserData,
		Address, Port, Dir, Group, {error, no_such_user}) ->
	LM = {erlang:system_time(millisecond), erlang:unique_integer([positive])},
	NewUserData = case lists:keyfind(locale, 1, UserData) of
		{locale, Locale} when is_list(Locale) ->
			[{last_modified, LM} | UserData];
		false ->
			[{last_modified, LM}, {locale, "en"} | UserData]
	end,
	add_user3(Username, Address, Port, Dir, Group, LM,
			mod_auth:add_user(Username, Password, NewUserData, Address, Port, Dir));
add_user2(_, _, _, _, _, _, _, {error, Reason}) ->
	{error, Reason};
add_user2(_, _, _, _, _, _, _, {ok, _}) ->
	{error, user_exists}.
%% @hidden
add_user3(Username, Address, Port, Dir, Group, LM, true) ->
	add_user4(LM, mod_auth:add_group_member(Group, Username, Address, Port, Dir));
add_user3(_, _, _, _, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user4(LM, true) ->
	{ok, LM};
add_user4(_, {error, Reason}) ->
	{error, Reason}.

-spec list_users() -> Result
	when
		Result :: {ok, Users} | {error, Reason},
		Users :: [Username],
		Username :: string(),
		Reason :: term().
%% @doc List HTTP users.
%% @equiv  mod_auth:list_users(Address, Port, Dir)
list_users() ->
	list_users1(get_params()).
%% @hidden
list_users1({Port, Address, Dir, _}) ->
	mod_auth:list_users(Address, Port, Dir);
list_users1({error, Reason}) ->
	{error, Reason}.

-spec get_user(Username) -> Result
	when
		Username :: string(),
		Result :: {ok, User} | {error, Reason},
		User :: #httpd_user{},
		Reason :: term().
%% @doc Get an HTTP user record.
%% @equiv mod_auth:get_user(Username, Address, Port, Dir)
get_user(Username) ->
	get_user(Username, get_params()).
%% @hidden
get_user(Username, {Port, Address, Dir, _}) ->
	mod_auth:get_user(Username, Address, Port, Dir);
get_user(_, {error, Reason}) ->
	{error, Reason}.

-spec delete_user(Username) -> Result
	when
		Username :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete an existing HTTP user.
delete_user(Username) ->
	delete_user1(Username, get_params()).
%% @hidden
delete_user1(Username, {Port, Address, Dir, GroupName}) ->
	delete_user2(GroupName, Username, Address, Port, Dir,
			mod_auth:delete_user(Username, Address, Port, Dir));
delete_user1(_, {error, Reason}) ->
	{error, Reason}.
%% @hidden
delete_user2(GroupName, Username, Address, Port, Dir, true) ->
	delete_user3(mod_auth:delete_group_member(GroupName,
			Username, Address, Port, Dir));
delete_user2(_, _, _, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
delete_user3(true) ->
	ok;
delete_user3({error, Reason}) ->
	{error, Reason}.

-spec update_user(Username, Password, UserData) -> Result
	when
		Username :: string(),
		Password :: string(),
		UserData :: [{Name, Value}],
		Name :: atom(),
		Value :: term(),
		Result :: {ok, LM} | {error, Reason},
		LM :: {integer(), integer()},
		Reason :: term().
%% @hidden Update user password and data.
update_user(Username, Password, UserData) ->
	case get_user(Username) of
		{error, Reason} ->
			{error, Reason};
		{ok, #httpd_user{}} ->
			case delete_user(Username) of
				ok ->
					case add_user(Username, Password, UserData) of
						{ok, LM} ->
							{ok, LM};
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end
	end.

-spec query_users(Cont, MatchId, MatchLocale) -> Result
	when
		Cont :: start | any(),
		MatchId :: Match,
		MatchLocale ::Match,
		Match :: {exact, string()} | {notexact, string()} | {like, string()},
		Result :: {Cont1, [#httpd_user{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query the user table.
query_users(start, '_', MatchLocale) ->
	MatchSpec = [{'_', [], ['$_']}],
	query_users1(MatchSpec, MatchLocale);
query_users(start, {Op, String} = _MatchId, MatchLocale)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchSpec = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			Username = {Prefix ++ '_', '_', '_', '_'},
			MatchHead = #httpd_user{username = Username, _ = '_'},
			[{MatchHead, [], ['$_']}];
		_ ->
			Username = {String, '_', '_', '_'},
			MatchHead = #httpd_user{username = Username, _ = '_'},
			[{MatchHead, [], ['$_']}]
	end,
	query_users1(MatchSpec, MatchLocale);
query_users(start, {notexact, String} = _MatchId, MatchLocale)
		when is_list(String) ->
	Username = {'$1', '_', '_', '_'},
	MatchHead = #httpd_user{username = Username, _ = '_'},
	MatchSpec = [{MatchHead, [{'/=', '$1', String}], ['$_']}],
	query_users1(MatchSpec, MatchLocale);
query_users(Cont, _MatchId, MatchLocale) when is_tuple(Cont) ->
	F = fun() ->
			mnesia:select(Cont)
	end,
	case mnesia:ets(F) of
		{Users, Cont1} ->
			query_users2(MatchLocale, Cont1, Users);
		'$end_of_table' ->
			{eof, []}
	end;
query_users(start, MatchId, MatchLocale) when is_tuple(MatchId) ->
	MatchCondition = [match_condition('$1', MatchId)],
	Username = {'$1', '_', '_', '_'},
	MatchHead = #httpd_user{username = Username, _ = '_'},
	MatchSpec = [{MatchHead, MatchCondition, ['$_']}],
	query_users1(MatchSpec, MatchLocale).
%% @hidden
query_users1(MatchSpec, MatchLocale) ->
	F = fun() ->
			mnesia:select(httpd_user, MatchSpec, ?CHUNKSIZE, read)
	end,
	case mnesia:ets(F) of
		{Users, Cont} ->
			query_users2(MatchLocale, Cont, Users);
		'$end_of_table' ->
			{eof, []}
	end.
%% @hidden
query_users2('_' = _MatchLocale, Cont, Users) ->
	{Cont, Users};
query_users2({exact, String} = _MatchLocale, Cont, Users)
		when is_list(String) ->
	F = fun(#httpd_user{user_data = UD}) ->
			case lists:keyfind(locale, 1, UD) of
				{_, String} ->
					true;
				_ ->
					false
			end
	end,
	{Cont, lists:filter(F, Users)};
query_users2({notexact, String} = _MatchLocale, Cont, Users)
		when is_list(String) ->
	F = fun(#httpd_user{user_data = UD}) ->
			case lists:keyfind(locale, 1, UD) of
				{_, String} ->
					false;
				_ ->
					true
			end
	end,
	{Cont, lists:filter(F, Users)};
query_users2({like, String} = _MatchLocale, Cont, Users)
		when is_list(String) ->
	F = case lists:last(String) of
		$% ->
			Prefix = lists:droplast(String),
			fun(#httpd_user{user_data = UD}) ->
					case lists:keyfind(locale, 1, UD) of
						{_, Locale} ->
							lists:prefix(Prefix, Locale);
						_ ->
							false
					end
			end;
		_ ->
			fun(#httpd_user{user_data = UD}) ->
					case lists:keyfind(locale, 1, UD) of
						{_, String} ->
							true;
						_ ->
							false
					end
			end
	end,
	{Cont, lists:filter(F, Users)}.

-spec statistics(Item) -> Result
	when
		Item :: scheduler_utilization,
		Result :: {ok, {Etag, Interval, Report}} | {error, Reason},
		Etag :: string(),
		Interval :: pos_integer(),
		Report :: [ItemResult],
		ItemResult :: {SchedulerId, Utilization},
		SchedulerId :: pos_integer(),
		Utilization :: non_neg_integer(),
		Reason :: term().
%% @doc Get system statistics.
statistics(Item) ->
	case catch gen_server:call(ocs_statistics, Item) of
		{Etag, Interval, Report} ->
			{ok, {Etag, Interval, Report}};
		{error, Reason} ->
			{error, Reason};
		{'EXIT', {noproc,_}} ->
			case catch supervisor:start_child(ocs_statistics_sup, []) of
				{ok, Child} ->
					case catch gen_server:call(Child, Item) of
						{Etag, Interval, Report} ->
							{ok, {Etag, Interval, Report}};
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason};
				{'EXIT', {noproc,_}} ->
					{error, ocs_down}
			end
	end.

-spec clean_services(Before) -> Result
	when
		Before :: ocs_rest:timestamp(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Clean the `service' table.
%%
%% 	Traverse the `service' table, removing expired sessions.
%%
%% 	The `service' table entries include a `session_attributes'
%% 	field used to track active sessions. It is a list of
%% 	`{TS, Attributes}' where `TS' is a timestamp and `Attributes'
%% 	is a list of RADIUS or DIAMETER AVPs uniquely identifying a
%% 	session. Instability of the RADIUS/DIAMETER connections may
%% 	result in sessions not being removed.
%%
%% 	This function lazily traverses the `service' table, removing
%% 	any session timestamped earlier than `Before'.
%%
clean_services(Before) when is_tuple(Before) ->
	clean_services(ocs_rest:date(Before));
clean_services(Before) when is_list(Before) ->
	clean_services(ocs_rest:iso8601(Before));
clean_services(Before)
		when is_integer(Before) ->
	clean_services(Before, mnesia:dirty_first(service)).
%% @hidden
clean_services(Before, Key) when is_binary(Key) ->
	Next = mnesia:dirty_next(service, Key),
	Fclean = fun({TS, _Attributes}) when TS > Before ->
				true;
			({TS, _Attributes}) when TS =< Before ->
				false
	end,
	Ftrans = fun() ->
			[#service{session_attributes = SA1} = S1]
					= mnesia:read(service, Key, read),
			case lists:filter(Fclean, SA1) of
				SA2 when length(SA2) < length(SA1) ->
					S2 = S1#service{session_attributes = SA2},
					mnesia:write(service, S2, write);
				_SA2 ->
					ok
			end
	end,
	case mnesia:transaction(Ftrans) of
		{atomic, ok} ->
			clean_services(Before, Next);
		{aborted, Reason} ->
			{error, Reason}
	end;
clean_services(_Before, '$end_of_table') ->
	ok.

-spec clean_buckets() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Clean the `bucket' table.
%%
%% 	Traverse the `buckets' table, removing expired buckets.
%%
%% 	The `bucket' table entries optionally include an
%% 	expiration date and time in the `end_date' field.
%%
%% 	This function lazily traverses the `bucket' table,
%% 	removing expired buckets.
%%
clean_buckets() ->
	clean_buckets(1).

-spec clean_buckets(Before) -> Result
	when
		Before :: ocs_rest:timestamp(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Clean the `bucket' table.
%%
%% 	Traverse the `buckets' table, removing old buckets.
%%
%% 	<div class="alert">
%% 		WARNING: This will permanently delete subscriber balances!
%%
%% 		There should be no reason to use this function
%% 		in normal operation of the OCS. Expired buckets
%% 		are removed as they are encountered during normal
%% 		rating operations.
%% 	</div>
%% 	
%% 	The `bucket' table entries include a `last_modified'
%% 	field with a timestamp of the last write. The `end_date'
%% 	field may (optionally) contain an expiration date.
%%
%% 	This function lazily traverses the `bucket' table,
%% 	removing expired buckets and stale buckets which
%% 	have not been modified after `Before'.
%%
clean_buckets(Before) when is_tuple(Before) ->
	clean_buckets(ocs_rest:date(Before));
clean_buckets(Before) when is_list(Before) ->
	clean_buckets(ocs_rest:iso8601(Before));
clean_buckets(Before) when is_integer(Before) ->
	clean_buckets(Before, mnesia:dirty_first(bucket)).
%% @hidden
clean_buckets(Before, Key) when is_list(Key) ->
	Next = mnesia:dirty_next(bucket, Key),
	Now = erlang:system_time(millisecond),
	Ftrans = fun() ->
			case mnesia:read(bucket, Key, read) of
				[#bucket{product = [ProdRef], end_date = Expiry,
						last_modified = LM}]
						when (is_tuple(LM) andalso (element(1, LM) < Before));
						((Expiry /= undefined) and (Expiry < Now)) ->
					case mnesia:read(product, ProdRef, write) of
						[#product{balance = Buckets1} = Product1] ->
							Buckets2 = lists:delete(Key, Buckets1),
							Product2 = Product1#product{balance = Buckets2},
							mnesia:write(Product2),
							mnesia:delete(bucket, Key, write);
						[] ->
							mnesia:delete(bucket, Key, write)
					end;
				[#bucket{}] ->
					ok
			end
	end,
	case mnesia:transaction(Ftrans) of
		{atomic, ok} ->
			clean_buckets(Before, Next);
		{aborted, Reason} ->
			{error, Reason}
	end;
clean_buckets(_Before, '$end_of_table') ->
	ok.

-spec clean_reservations(Before) -> Result
	when
		Before :: ocs_rest:timestamp(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Clean reservations held in balance `bucket's.
%% @equiv clean_reservations(Before, true)
clean_reservations(Before) ->
	clean_reservations(Before, true).

-spec clean_reservations(Before, Refund) -> Result
	when
		Before :: ocs_rest:timestamp(),
		Result :: ok | {error, Reason},
		Refund :: boolean(),
		Reason :: term().
%% @doc Clean reservations held in balance `bucket's.
%%
%% 	Traverse the `buckets' table, removing old reservations.
%%
%% 	The `bucket' table entries include a `reservations'
%% 	attribute describing credit debits and reserves and
%% 	a timestamp of the last transaction.
%%
%% 	This function lazily traverses the `bucket' table,
%% 	removing reservations which haven't been updated
%% 	since `Before'.
%%
%% 	If `Refund' is `true' the unused portion of
%% 	reservations to be removed will be returned to
%% 	the remaining amount of the bucket.
%%
clean_reservations(Before, Refund) when is_tuple(Before) ->
	clean_reservations(ocs_rest:date(Before), Refund);
clean_reservations(Before, Refund) when is_list(Before) ->
	clean_reservations(ocs_rest:iso8601(Before), Refund);
clean_reservations(Before, Refund)
		when is_integer(Before), is_boolean(Refund) ->
	clean_reservations1(Before, Refund, mnesia:dirty_first(bucket)).
%% @hidden
clean_reservations1(Before, Refund, Key)
		when is_list(Key) ->
	Next = mnesia:dirty_next(bucket, Key),
	Fold = fun(_, #{ts := TS, reserve := N}, {Acc1, Acc2})
					when TS < Before ->
				{Acc1, Acc2 + N};
			(SessionId, Reservation, {Acc1, Acc2}) ->
				{Acc1#{SessionId => Reservation}, Acc2}
	end,
	Ftrans = fun() ->
			case mnesia:read(bucket, Key, read) of
				[#bucket{remain_amount = Remain,
						attributes = #{reservations
								:= Reserves} = Attributes} = Bucket] ->
					case maps:fold(Fold, {#{}, 0}, Reserves) of
						{Reserves, _N} ->
							ok;
						{Reserves1, N} when Refund == true ->
							Remain1 = Remain + N,
							Attributes1 = Attributes#{reservations => Reserves1},
							Bucket1 = Bucket#bucket{remain_amount = Remain1,
									attributes = Attributes1},
							mnesia:write(Bucket1);
						{Reserves1, _N} when Refund == false ->
							Attributes1 = Attributes#{reservations => Reserves1},
							Bucket1 = Bucket#bucket{attributes = Attributes1},
							mnesia:write(Bucket1)
					end;
				[#bucket{}] ->
					ok
			end
	end,
	case mnesia:transaction(Ftrans) of
		{atomic, ok} ->
			clean_reservations1(Before, Refund, Next);
		{aborted, Reason} ->
			{error, Reason}
	end;
clean_reservations1(_Before, _Refund, '$end_of_table') ->
	ok.

%%----------------------------------------------------------------------
%%  The ocs private API
%%----------------------------------------------------------------------

-spec parse_bucket(Bucket) -> Bucket
	when
		Bucket :: #bucket{}.
%% @doc Replace reservations field of bucket with attributes.
%% @private
parse_bucket(Bucket) ->
	case element(8, Bucket) of
		Attributes when is_map(Attributes) ->
			Bucket;
		Reservations when is_list(Reservations) ->
			F1 = fun(R, MapAcc) ->
				Size = size(R),
				F2 = fun F2(6, Map) ->
							SessionId = element(6, R),
							MapAcc#{SessionId => Map};
						F2(N, Map) when N =< Size ->
							case element(N, R)  of
								undefined ->
									F2(N + 1, Map);
								Value ->
									Key = reservation_key(N),
									F2(N + 1, Map#{Key => Value})
							end
				end,
				F2(1, #{})
			end,
			parse_bucket(Bucket, lists:foldl(F1, #{}, Reservations))
	end.
%% @hidden
parse_bucket(#bucket{start_date = Milliseconds,
		end_date = Milliseconds} = Bucket, ReservationMap)
		when is_integer(Milliseconds) ->
	parse_bucket(Bucket, ReservationMap, session);
parse_bucket(#bucket{} = Bucket, ReservationMap) ->
	parse_bucket(Bucket, ReservationMap, normal).
%% @hidden
parse_bucket(Bucket, Reservations, BucketType) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	Attributes = case maps:size(Reservations) of
		0 ->
			#{bucket_type => BucketType};
		_Num ->
			#{bucket_type => BucketType, reservations => Reservations}
	end,
	Bucket#bucket{attributes = Attributes, last_modified = {TS, N}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec generate_password(Length) -> password()
	when
		Length :: pos_integer().
%% @doc Generate a random uniform password.
%% @hidden
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
%% @hidden
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
%% @hidden
charset() ->
	C1 = lists:seq($2, $9),
	C2 = lists:seq($a, $h),
	C3 = lists:seq($j, $k),
	C4 = lists:seq($m, $n),
	C5 = lists:seq($p, $t),
	C6 = lists:seq($w, $z),
	lists:append([C1, C2, C3, C4, C5, C6]).

-spec normalize(String) -> string()
	when
		String :: string().
%% @doc Strip non hex digits and convert to lower case.
%% @hidden
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

-spec subscription(Product, Offer, Buckets, InitialFlag) -> Result
	when
		Product :: #product{},
		Offer :: #offer{},
		Buckets :: [#bucket{}],
		InitialFlag :: boolean(),
		Result :: {Product, Buckets}.
%% @doc Apply one time and recurring charges.
%% @private
subscription(Product, #offer{bundle = [], price = Prices} =
		_Offer, Buckets, InitialFlag) ->
	Now = erlang:system_time(millisecond),
	subscription(Product, Now, InitialFlag, Buckets, Prices);
subscription(#product{product = OfferId} = Product,
		#offer{name = OfferId, bundle = Bundled, price = Prices} = _Offer,
		Buckets, InitialFlag) when length(Bundled) > 0 ->
	Now = erlang:system_time(millisecond),
	F = fun(#bundled_po{name = P}, {Prod, B}) ->
				case mnesia:read(offer, P, read) of
					[Offer] ->
						subscription(Prod, Offer, B, InitialFlag);
					[] ->
						throw(offer_not_found)
				end
	end,
	{Product1, Buckets1} = lists:foldl(F, {Product, Buckets}, Bundled),
	subscription(Product1, Now, InitialFlag, Buckets1, Prices).
%% @hidden
subscription(#product{id = ProdRef} = Product, Now, true, Buckets,
		[#price{type = one_time, amount = Amount,
			alteration = undefined} | T]) ->
	NewBuckets = charge(ProdRef, Amount, Buckets),
	subscription(Product, Now, true, NewBuckets, T);
subscription(#product{id = ProdRef} = Product, Now, true,
		Buckets, [#price{type = one_time, amount = PriceAmount,
			name = Name, char_value_use = CharValueUse,
			alteration = #alteration{units = Units, size = Size,
			amount = AlterAmount}} | T]) ->
	N = erlang:unique_integer([positive]),
	NewBuckets = charge(ProdRef, PriceAmount + AlterAmount,
			[#bucket{id = generate_bucket_id(), product = [ProdRef],
					units = Units, remain_amount = Size,
					price = bucket_price(Name, CharValueUse),
					attributes = #{bucket_type => normal},
					last_modified = {Now, N}} | Buckets]),
	subscription(Product, Now, true, NewBuckets, T);
subscription(Product, Now, false, Buckets, [#price{type = one_time} | T]) ->
	subscription(Product, Now, false, Buckets, T);
subscription(#product{id = ProdRef} = Product, Now, true, Buckets,
		[#price{type = Type, name = Name, char_value_use = CharValueUse,
			alteration = #alteration{type = one_time,
			units = Units, size = Size, amount = AlterationAmount}} | T])
		when ((Type == usage) or (Type == tariff)) ->
	N = erlang:unique_integer([positive]),
	NewBuckets = charge(ProdRef, AlterationAmount,
			[#bucket{id = generate_bucket_id(), units = Units,
					price = bucket_price(Name, CharValueUse),
					attributes = #{bucket_type => normal},
					remain_amount = Size, product = [ProdRef],
					last_modified = {Now, N}}
			| Buckets]),
	subscription(Product, Now, true, NewBuckets, T);
subscription(Product, Now, false, Buckets,
		[#price{type = Type, alteration = #alteration{type = one_time}} | T])
		when ((Type == usage) or (Type == tariff)) ->
	subscription(Product, Now, false, Buckets, T);
subscription(#product{id = ProdRef, payment = Payments} = Product,
		Now, true, Buckets, [#price{type = recurring, period = Period,
		amount = Amount, name = Name, alteration = undefined} | T]) when
		Period /= undefined ->
	NewBuckets = charge(ProdRef, Amount, Buckets),
	NewPayments = [{Name, end_period(Now, Period)} | Payments],
	Product1 = Product#product{payment = NewPayments},
	subscription(Product1, Now, true, NewBuckets, T);
subscription(#product{id = ProdRef, payment = Payments} = Product,
		Now, false, Buckets, [#price{type = recurring, period = Period,
		amount = Amount, name = Name, alteration = undefined} | T])
		when Period /= undefined ->
	{NewPayments, NewBuckets} = dues(Payments, Now,
			Buckets, Name, Period, Amount, ProdRef),
	Product1 = Product#product{payment = NewPayments},
	subscription(Product1, Now, false, NewBuckets, T);
subscription(#product{id = ProdRef, payment = Payments} = Product,
		Now, true, Buckets, [#price{type = recurring, period = Period,
			amount = Amount, alteration = #alteration{units = Units,
			size = Size, amount = AllowanceAmount}, name = Name,
			char_value_use = CharValueUse} | T]) when
			(Period /= undefined) and ((Units == octets) orelse
			(Units == seconds) orelse  (Units == messages)) ->
	N = erlang:unique_integer([positive]),
	NewBuckets = charge(ProdRef, Amount + AllowanceAmount,
			[#bucket{id = generate_bucket_id(),
					price = bucket_price(Name, CharValueUse),
					attributes = #{bucket_type => normal},
					units = Units, remain_amount = Size,
					product = [ProdRef],
					end_date = end_period(Now, Period),
					last_modified = {Now, N}} | Buckets]),
	NewPayments = [{Name, end_period(Now, Period)} | Payments],
	Product1 = Product#product{payment = NewPayments},
	subscription(Product1, Now, true, NewBuckets, T);
subscription(#product{id = ProdRef, payment = Payments} = Product,
		Now, false, Buckets, [#price{type = recurring, period = Period,
			amount = Amount, alteration = #alteration{units = Units,
			size = Size, amount = AllowanceAmount}, name = Name,
			char_value_use = CharValueUse} | T])
			when (Period /= undefined) and ((Units == octets)
			orelse (Units == seconds) orelse (Units == messages)) ->
	{NewPayments, NewBuckets1} = dues(Payments, Now,
			Buckets, Name, Period, Amount, ProdRef),
	N = erlang:unique_integer([positive]),
	NewBuckets2 = charge(ProdRef, AllowanceAmount,
			[#bucket{id = generate_bucket_id(),
					price = bucket_price(Name, CharValueUse),
					attributes = #{bucket_type => normal},
					units = Units, remain_amount = Size, product = [ProdRef],
					end_date = end_period(Now, Period), last_modified = {Now, N}}
			| NewBuckets1]),
	Product1 = Product#product{payment = NewPayments},
	subscription(Product1, Now, false, NewBuckets2, T);
subscription(#product{id = ProdRef, payment = Payments} = Product, Now, true,
		Buckets, [#price{type = Type,
		alteration = #alteration{type = recurring, period = Period,
		units = Units, size = Size, amount = Amount}, name = Name,
		char_value_use = CharValueUse} | T])
		when Period /= undefined, Units == octets; Units == seconds;
		Units == messages, ((Type == usage) or (Type == tariff)) ->
	N = erlang:unique_integer([positive]),
	NewBuckets = charge(ProdRef, Amount,
			[#bucket{id = generate_bucket_id(), units = Units,
					price = bucket_price(Name, CharValueUse),
					attributes = #{bucket_type => normal},
					remain_amount = Size, product = [ProdRef],
					end_date = end_period(Now, Period), last_modified = {Now, N}}
			| Buckets]),
	NewPayments = [{Name, end_period(Now, Period)} | Payments],
	Product1 = Product#product{payment = NewPayments},
	subscription(Product1, Now, true, NewBuckets, T);
subscription(#product{id = ProdRef, payment = Payments} = Product,
		Now, false, Buckets, [#price{type = Type, name = Name,
		char_value_use = CharValueUse,
		alteration = #alteration{type = recurring, period = Period, units = Units,
		size = Size, amount = Amount}} | T]) when Period /= undefined, Units == octets;
		Units == seconds; Units == messages, ((Type == usage) or (Type == tariff)) ->
	{NewPayments, NewBuckets1} = dues(Payments, Now, Buckets, Name, Period, Amount, ProdRef),
	N = erlang:unique_integer([positive]),
	NewBuckets2 = charge(ProdRef, Amount,
			[#bucket{id = generate_bucket_id(), units = Units,
					price = bucket_price(Name, CharValueUse),
					attributes = #{bucket_type => normal},
					remain_amount = Size, product = [ProdRef],
					end_date = end_period(Now, Period), last_modified = {Now, N}}
			| NewBuckets1]),
	Product1 = Product#product{payment = NewPayments},
	subscription(Product1, Now, false, NewBuckets2, T);
subscription(Product, Now, InitialFlag, Buckets, [_H | T]) ->
	subscription(Product, Now, InitialFlag, Buckets, T);
subscription(Product, _Now, _, Buckets, []) ->
	NewBIds = [Id || #bucket{id = Id} <- Buckets],
	{Product#product{balance = NewBIds}, Buckets}.

%% @hidden
bucket_price(PriceName,
		[#char_value_use{name = "fixedPriceBucket",
		values = [#char_value{value = true}]} | _]) ->
	PriceName;
bucket_price(PriceName, [_ | T]) ->
	bucket_price(PriceName, T);
bucket_price(PriceName, []) ->
	[].

%% @hidden
dues(Payments, Now, Buckets, PName, Period, Amount, ProdRef) ->
	dues(Payments, Now, Buckets, PName, Period, Amount, ProdRef, []).
%% @hidden
dues([{_, DueDate} = P | T], Now, Buckets, PName, Period, Amount, ProdRef, Acc) when DueDate > Now ->
	dues(T, Now, Buckets, PName, Period, Amount, ProdRef, [P | Acc]);
dues([{PName, DueDate} | T], Now, Buckets, PName, Period, Amount, ProdRef, Acc) ->
	NewBuckets = charge(ProdRef, Amount, Buckets),
	case end_period(DueDate, Period) of
		NextDueDate when NextDueDate < Now ->
			dues([{PName, NextDueDate} | T], Now,
					NewBuckets, PName, Period, Amount, ProdRef, Acc);
		NextDueDate ->
			dues(T, Now, NewBuckets, PName, Period,
					Amount, ProdRef, [{PName, NextDueDate} | Acc])
	end;
dues([P | T], Now, Buckets, PName, Period, Amount, ProdRef, Acc) ->
	dues(T, Now, Buckets, PName, Period, Amount, ProdRef, [P | Acc]);
dues([], _Now, Buckets, _PName, _Period, _Amount, _ProdRef, Acc) ->
	{lists:reverse(Acc), Buckets}.

-spec get_params() -> Result
	when
		Result :: {Port, Address, Directory, Group} | {error, Reason},
		Port :: integer(),
		Address :: inet:ip_address() | inet:hostname() | any,
		Directory :: string(),
		Group :: string(),
		Reason :: term().
%% @doc Returns configurations details for currently running
%% {@link //inets. httpd} service.
%% @hidden
get_params() ->
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			get_params1(lists:keyfind(httpd, 1, ServicesInfo));
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
get_params1({httpd, _, HttpdInfo}) ->
	{_, Address} = lists:keyfind(bind_address, 1, HttpdInfo),
	{_, Port} = lists:keyfind(port, 1, HttpdInfo),
	get_params2(Address, Port, application:get_env(inets, services));
get_params1(false) ->
	{error, httpd_not_started}.
%% @hidden
get_params2(Address, Port, {ok, Services}) ->
	get_params3(Address, Port, lists:keyfind(httpd, 1, Services));
get_params2(_, _, undefined) ->
	{error, inet_services_undefined}.
%% @hidden
get_params3(Address, Port, {httpd, Httpd}) ->
	F = fun({directory, _}) ->
				true;
			(_) ->
				false
	end,
	get_params4(Address, Port, lists:filter(F, Httpd));
get_params3(_, _, false) ->
	{error, httpd_service_undefined}.
%% @hidden
get_params4(Address, Port, [{directory, {_Dir, []}} | T]) ->
	get_params4(Address, Port, T);
get_params4(Address, Port, [{directory, {Directory, Auth}} | _T]) ->
	get_params5(Address, Port, Directory,
			lists:keyfind(require_group, 1, Auth));
get_params4(_, _, []) ->
	{error, httpd_directory_undefined}.
%% @hidden
get_params5(Address, Port, Directory, {require_group, [Group | _]}) ->
	{Port, Address, Directory, Group};
get_params5(_, _, _, _) ->
	{error, httpd_group_undefined}.

-spec charge(ProdRef, Amount, Buckets) -> Buckets
	when
		ProdRef :: string(),
		Amount :: non_neg_integer(),
		Buckets :: [#bucket{}].
%% @doc Charge `Amount' to `Buckets'.
%% @private
charge(ProdRef, Amount, Buckets) ->
	charge(ProdRef, Amount, sort(Buckets), []).
%% @hidden
charge(_ProdRef, 0, T, Acc) ->
	lists:reverse(Acc) ++ T;
charge(_ProdRef, Amount, [#bucket{units = cents,
		remain_amount = Remain} = B | T], Acc)
		when ((Amount < Remain) or  (Remain < 0)) ->
	lists:reverse(Acc) ++ [B#bucket{remain_amount = Remain - Amount} | T];
charge(ProdRef, Amount, [#bucket{units = cents,
		remain_amount = Remain} = B | T], Acc) when Remain > 0 ->
	charge(ProdRef, Amount - Remain, T, [B#bucket{remain_amount = 0} | Acc]);
charge(ProdRef, Amount, [H | T], Acc) ->
	charge(ProdRef, Amount, T, [H | Acc]);
charge(ProdRef, Amount, [], Acc) ->
	[#bucket{id = generate_bucket_id(), units = cents,
			attributes = #{bucket_type => normal},
			remain_amount = - Amount, product = [ProdRef]}
			| lists:reverse(Acc)].

-spec credit(Units, Amount, Buckets) -> Result
	when
		Units :: cents | octets | seconds | messages,
		Amount :: pos_integer(),
		Buckets :: [#bucket{}],
		Result :: {RemainAmount, DeleteRefs, Buckets},
		RemainAmount :: non_neg_integer(),
		DeleteRefs :: [string()].
%% @doc Credit `Amount' on `Buckets'.
%% @private
credit(Units, Amount, Buckets) ->
	credit(Units, Amount, Buckets, [], []).
%% @hidden
credit(_Units, 0, Buckets, DeleteRefs, Acc) ->
	{0, DeleteRefs, lists:reverse(Acc) ++ Buckets};
credit(Units, Amount,
		[#bucket{units = Units, remain_amount = Remain} = B | T],
		DeleteRefs, Acc) when Remain < 0, (Remain + Amount) < 0 ->
	{0, DeleteRefs, lists:reverse(Acc)
			++ [B#bucket{remain_amount = Remain + Amount} | T]};
credit(Units, Amount,
		[#bucket{id = Ref, units = Units, remain_amount = Remain} | T],
		DeleteRefs, Acc) when Remain < 0, (Remain + Amount) >= 0  ->
	credit(Units, Remain + Amount, T, [Ref | DeleteRefs],  Acc);
credit(Units, Amount, [H | T], DeleteRefs, Acc) ->
	credit(Units, Amount, T, DeleteRefs, [H | Acc]);
credit(_Units, Amount, [], DeleteRefs, Acc) ->
	{Amount, DeleteRefs, lists:reverse(Acc)}.

-spec sort(Buckets) -> Buckets
	when
		Buckets :: [#bucket{}].
%% @doc Sort `Buckets' oldest first.
%% @hidden
sort(Buckets) ->
	F = fun(#bucket{end_date = T1},
				#bucket{end_date = T2}) when T1 =< T2 ->
			true;
		(_, _)->
			false
	end,
	lists:sort(F, Buckets).

%% @hidden
generate_bucket_id() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

-spec date(MilliSeconds) -> DateTime
	when
		MilliSeconds :: pos_integer(),
		DateTime :: calendar:datetime().
%% @doc Convert timestamp to date and time.
%% @hidden
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds).

-spec end_period(StartTime, Period) -> EndTime
	when
		StartTime :: non_neg_integer(),
		Period :: hourly | daily | weekly | monthly | yearly,
		EndTime :: non_neg_integer().
%% @doc Calculate end of period.
%% @private
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
end_period1({{Year, 6, 30}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 7, 31}, Time}) - 1;
end_period1({{Year, 7, 31}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 8, 31}, Time}) - 1;
end_period1({{Year, 8, 31}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 9, 30}, Time}) - 1;
end_period1({{Year, 9, 30}, Time}, monthly) ->
	gregorian_datetime_to_system_time({{Year, 10, 31}, Time}) - 1;
end_period1({{Year, 10, 31}, Time}, monthly) ->
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

-spec gregorian_datetime_to_system_time(DateTime) -> MilliSeconds
	when
		DateTime :: tuple(),
		MilliSeconds :: pos_integer().
%% @doc Convert gregorian datetime to system time in milliseconds.
%% @hidden
gregorian_datetime_to_system_time(DateTime) ->
	(calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH) * 1000.

-type match() :: {exact, term()} | {notexact, term()} | {lt, term()}
		| {lte, term()} | {gt, term()} | {gte, term()} | {like, [term()]}.

-spec match_condition(MatchVariable, Match) -> MatchCondition
	when
		MatchVariable :: atom(), % '$<number>'
		Match :: {exact, term()} | {notexact, term()} | {lt, term()}
				| {lte, term()} | {gt, term()} | {gte, term()},
		MatchCondition :: {GuardFunction, MatchVariable, Term},
		Term :: any(),
		GuardFunction :: '=:=' | '=/=' | '<' | '=<' | '>' | '>='.
%% @doc Convert REST query patterns to Erlang match specification conditions.
%% @hidden
match_condition(Var, {exact, Term}) ->
	{'=:=', Var, Term};
match_condition(Var, {notexact, Term}) ->
	{'=/=', Var, Term};
match_condition(Var, {lt, Term}) ->
	{'<', Var, Term};
match_condition(Var, {lte, Term}) ->
	{'=<', Var, Term};
match_condition(Var, {gt, Term}) ->
	{'>', Var, Term};
match_condition(Var, {gte, Term}) ->
	{'>=', Var, Term}.

-spec match_address(String) -> Result
	when
		String :: string(),
		Result ::{MatchAddress, MatchConditions},
		MatchAddress :: tuple(),
		MatchConditions :: [tuple()].
%% @doc Construct match specification for IP address.
%% @hidden
match_address(String) ->
	Ns = [list_to_integer(N) || N <- string:tokens(String, [$.])],
	match_address1(lists:reverse(Ns)).
%% @hidden
match_address1([N | T]) when N >= 100 ->
	match_address2(T, [N], []);
match_address1([N | T]) when N >= 10 ->
	match_address2(T, ['$1'], [{'or', {'==', '$1', N},
			{'and', {'>=', '$1', N * 10}, {'<', '$1', (N + 1) * 10}}}]);
match_address1([N | T]) ->
	match_address2(T, ['$1'], [{'or', {'==', '$1', N},
			{'and', {'>=', '$1', N * 10}, {'<', '$1', (N + 1) * 10}},
			{'and', {'>=', '$1', N * 100}, {'<', '$1', (N + 1) * 100}}}]).
%% @hidden
match_address2(T, Head, Conditions) ->
	Head1 = lists:reverse(T) ++ Head,
	Head2 = Head1 ++ lists:duplicate(4 - length(Head1), '_'),
	{list_to_tuple(Head2), Conditions}.

%% @hidden
match_protocol(Prefix) ->
	case lists:prefix(Prefix, "diameter") of
		true ->
			diameter;
		false ->
			match_protocol1(Prefix)
	end.
%% @hidden
match_protocol1(Prefix) ->
	case lists:prefix(Prefix, "DIAMETER") of
		true ->
			diameter;
		false ->
			match_protocol2(Prefix)
	end.
%% @hidden
match_protocol2(Prefix) ->
	case lists:prefix(Prefix, "radius") of
		true ->
			radius;
		false ->
			match_protocol3(Prefix)
	end.
%% @hidden
match_protocol3(Prefix) ->
	case lists:prefix(Prefix, "RADIUS") of
		true ->
			radius;
		false ->
			throw(badmatch)
	end.

%% @hidden
make_lm() ->
	{erlang:system_time(millisecond), erlang:unique_integer([positive])}.

-spec service_exist(Services) -> Result
	when
		Services :: [#service{}],
		Result :: boolean().
%% @doc Check at lease one service actually exists.
%% @hidden
service_exist(Services) ->
	F = fun(Service) ->
			case mnesia:read(service, Service) of
				[] ->
					false;
				[#service{}] ->
					true
			end
	end,
	lists:any(F, Services).

%% @hidden
reservation_key(1) ->
	ts;
reservation_key(2) ->
	debit;
reservation_key(3) ->
	reserve;
reservation_key(4) ->
	service_id;
reservation_key(5) ->
	charging_key.

