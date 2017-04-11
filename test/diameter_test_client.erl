%%% diameter_test_client.erl.erl
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
%%% @doc This module implements a {@link //diameter. diameter} client for
%%% testing purposes.
%%%
-module(diameter_test_client).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-export([start/3, connect/4, call/1, stop/1]).

%%----------------------------------------------------------------------
%%  diameter_test_client public api
%%----------------------------------------------------------------------

-spec start(SvcName, Address, Port) -> Result
	when
		SvcName :: atom(),
		Address :: inet:ip_address(),
		Port :: non_neg_integer(),
		Result :: term().
%% @doc Start a diameter client with specified arguments.
%%
start(SvcName, Address, Port) ->
	ok = diameter:start_service(SvcName, service_opts(SvcName)),
	{ok, _Ref} = connect(SvcName, Address, Port, diameter_tcp).

-spec connect(SvcName, Address, Port, Transport) -> Result
	when
		SvcName :: atom(),
		Address :: inet:ip_address(),
		Port :: non_neg_integer(),
		Transport :: diameter_tcp | diameter_sctp,
		Result :: {ok, Ref} | {error, Reason},
		Ref :: reference(),
		Reason :: term().
%% @doc Add a transport capability to diameter service.
connect(SvcName, Address, Port, Transport) when is_atom(Transport) ->
	connect1(SvcName, [{connect_timer, 5000} | transport_opts(Address, Port, Transport)]).

-spec call(SvcName) -> Result
	when
		SvcName :: atom(),
		Result :: term().
%% @doc Send a DIAMETER request to diameter server.
%%
call(SvcName) ->
	SId = diameter:session_id(atom_to_list(SvcName)),
	RAR = #diameter_base_RAR{'Session-Id' = SId, 'Auth-Application-Id' = 0,
		'Re-Auth-Request-Type' = 0},
	diameter:call(SvcName, common, RAR, []).

-spec stop(SvcName) -> Result
	when
		SvcName :: atom(),
		Result :: ok.
%% @doc Stops diameter client.
stop(SvcName) ->
	ok = diameter:stop_service(SvcName).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
service_opts(SvcName) ->
	[{'Origin-Host', atom_to_list(SvcName) ++ ".example.com"},
		{'Origin-Realm', "example.com"},
		{'Vendor-Id', 0},
		{'Product-Name', "Test Client"},
		{'Auth-Application-Id', [0]},
		{string_decode, false},
		{application, [{alias, common},
		{dictionary, diameter_gen_base_rfc6733},
		{module, diameter_test_client_cb}]}].

%% @hidden
connect1(SvcName, Opts)->
	diameter:add_transport(SvcName, {connect, Opts}).

%% @hidden
transport_opts(Address, Port, Trans) when is_atom(Trans) ->
	transport_opts1({Trans, Address, Address, Port}).

%% @hidden
transport_opts1({Trans, LocalAddr, RemAddr, RemPort}) ->
	[{transport_module, Trans},
		{transport_config, [{raddr, RemAddr},
		{rport, RemPort},
		{reuseaddr, true}
		| [{ip, LocalAddr}]]}].

