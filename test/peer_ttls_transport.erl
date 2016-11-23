%%% peer_ttls_transport.erl
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
%%%	`{cb_info, {ocs_eap_ttls_transport, eap_ttls, eap_ttls_closed, eap_ttls_error}}'
%%% 	with {@link //ssl/ssl:listen/2. ssl:listen/2}.
%%%
-module(peer_ttls_transport).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export inet compatible API
-export([setopts/2, getopts/2]).
%% export gen_tcp compatible API
-export([send/2, controlling_process/2,
		shutdown/2, close/1, connect/4]).
%% export public API
-export([ssl_connect/3, deliver/3]).

-export_type([listen_option/0, eap_option/0]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

-type listen_option() :: any().
-type eap_option() :: any().

-define(cb_info,
		{cb_info, {?MODULE, eap_ttls, eap_ttls_closed, eap_ttls_error}}).

%%Macro definitions for TLS record Content Type
-define(ChangeCipherSpec,	20).
-define(Alert,					21).
-define(Handshake,			22).
-define(Application,			23).
-define(Heartbeat,			24).

%%Macro definitions for TLS handshake protocal message type
-define(HelloRequest,			0).
-define(ClientHello,				1).
-define(ServerHello,				2).
-define(NewSessionTicket,		4).
-define(Certificate,				11).
-define(ServerKeyExchange,		12).
-define(CertificateRequest,	13).
-define(ServerHelloDone,		14).
-define(CertificateVerify,		15).
-define(ClientKeyExchange,		16).
-define(Finished,					20).

%%----------------------------------------------------------------------
%%  ocs_eap_ttls_transport public api
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
	ClientPid ! {ssl_socket, SslSocket}.

-spec deliver(SslPid, ClientPid, Data) ->
	ok when
		SslPid :: pid(),
		ClientPid :: pid(),
		Data :: iodata().
%% @doc Deliver received EAP-TTLS payload to SSL.
deliver(SslPid, ClientPid, Data) when is_pid(SslPid), is_pid(ClientPid) ->
	SslPid ! {eap_ttls, ClientPid, iolist_to_binary(Data)},
	ok.

%%----------------------------------------------------------------------
%%  peer_ttls_transport callbacks
%%----------------------------------------------------------------------
-spec connect(Address, ClientPid, SocketOpts, Timeout) ->
		{ok, ClientPid} when
	Address :: inet:socket_address() | inet:hostname(),
	ClientPid :: pid(),
	SocketOpts :: [term()],
	Timeout :: timeout().
%% @doc Connects to the EAP session
connect(Address, ClientPid, SocketOpts, Timeout) ->
	{ok, ClientPid}.

-spec setopts(ClientPid, Options) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
      Options :: [eap_option()],
		Reason :: term().
%% @doc Sets one or more options for an EAP session.
setopts(ClientPid, Options) when is_pid(ClientPid) -> 
	case proplists:get_value(active, Options) of
		undefined ->
			ok;
		Active ->
			% ClientPid ! {ssl_setopts, Options},
			ok
	end.

-spec getopts(ClientPid, Options) ->
	{ok, OptionValues} | {error, Reason} when
		ClientPid :: pid(),
      Options :: [eap_option()],
      OptionValues :: [eap_option()],
		Reason :: term().
%% @doc Gets one or more options for an EAP session.
getopts(ClientPid, Options) when is_pid(ClientPid) ->
	{ok, []}.

-spec shutdown(ClientPid, How) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		How :: read | write | read_write,
		Reason :: term().
%% @doc Close an EAP session in one or two directions.
shutdown(ClientPid, How) when is_pid(ClientPid) ->
	ok.

-spec close(ClientPid) ->
	ok when
		ClientPid :: pid().
%% @doc Close an EAP session.
close(ClientPid) when is_pid(ClientPid) ->
	ok.

-spec send(ClientPid, Data) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		Data :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(ClientPid, Data) when is_pid(ClientPid) ->
	ClientPid ! {eap_ttls, self(), Data},
	ok.

-spec controlling_process(ClientPid, Pid) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(ClientPid, Pid) when is_pid(ClientPid) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------
