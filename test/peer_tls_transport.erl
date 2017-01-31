%%% peer_tls_transport.erl
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
%%% @doc This callback module provides a {@link //kernel/gen_tcp. gen_tcp}
%%% 	compatible transport layer interface to EAP sessions for the
%%% 	{@link //ssl. ssl} application.
%%% 
%%% 	Use the {@link //ssl/ssl:transportoption(). ssl:transportoption()}
%%%	`{cb_info, {peer_tls_transport, eap_tls, eap_tls_closed, eap_tls_error}}'
%%% 	with {@link //ssl/ssl:listen/2. ssl:listen/2}.
%%%
-module(peer_tls_transport).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export public API
-export([ssl_connect/3, deliver/3]).

%% export inet compatible API
-export([setopts/2]).

%% export gen_tcp compatible API
-export([connect/4, send/2, controlling_process/2, close/1, shutdown/2]).

-define(cb_info,
		{cb_info, {?MODULE, eap_tls, eap_tls_closed, eap_tls_error}}).

%%----------------------------------------------------------------------
%%  peer_tls_transport public api
%%----------------------------------------------------------------------

-dialyzer({nowarn_function, ssl_connect/3}).
-spec ssl_connect(Address, ClientPid, Options) ->
		{ok, SslSocket} | {error, Reason} when
	Address :: inet:ip_address(),
	ClientPid :: pid(),
	Options :: [term()],
	SslSocket :: pid(),
	Reason :: term().
%% @doc Open an SSL connection to Host, Port.
ssl_connect(Address, ClientPid, Options) ->
	{ok , SslSocket} = ssl:connect(Address, ClientPid, [?cb_info | Options]),
	ClientPid ! {ssl_socket, SslSocket},
	keep_alive().

-spec deliver(SslPid, ClientPid, Data) ->
	ok when
		SslPid :: pid(),
		ClientPid :: pid(),
		Data :: iodata().
%% @doc Deliver received EAP-TLS payload to SSL.
deliver(SslPid, ClientPid, Data) when is_pid(SslPid), is_pid(ClientPid) ->
	SslPid ! {eap_tls, ClientPid, iolist_to_binary(Data)},
	ok.

%%----------------------------------------------------------------------
%%  peer_tls_transport callbacks
%%----------------------------------------------------------------------

-spec setopts(ClientPid, Options) ->
	ok | {error, Reason} when
	ClientPid :: pid(),
	Options :: list(),
	Reason :: term().
%% @doc Sets one or more options for an EAP session.
setopts(_ClientPid, _Options) when is_pid(_ClientPid) -> 
	ok.

-spec connect(Address, ClientPid, SocketOpts, Timeout) ->
		{ok, ClientPid} when
	Address :: inet:socket_address() | inet:hostname(),
	ClientPid :: pid(),
	SocketOpts :: [term()],
	Timeout :: timeout().
%% @doc Connects to the EAP session
connect(_Address, ClientPid, _SocketOpts, _Timeout) ->
	{ok, ClientPid}.

-spec send(ClientPid, Data) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		Data :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(ClientPid, Data) when is_pid(ClientPid) ->
	ClientPid ! {eap_tls, self(), iolist_to_binary(Data)},
	ok.

-spec controlling_process(ClientPid, Pid) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(ClientPid, Pid) when is_pid(ClientPid) ->
	ok.

-spec close(ClientPid) ->
	ok when
		ClientPid :: pid().
%% @doc Close an EAP session.
close(ClientPid) when is_pid(ClientPid) ->
	ok.

-spec shutdown(ClientPid, How) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		How :: read | write | read_write,
		Reason :: term().
%% @doc Close an EAP session in one or two directions.
shutdown(ClientPid, _How) when is_pid(ClientPid) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @doc Keep the spawned process alive
keep_alive() ->
	receive
		_Msg -> keep_alive()
	end.

