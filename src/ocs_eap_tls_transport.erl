%%% ocs_eap_tls_transport.erl
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
%%%	`{cb_info, {ocs_eap_tls_transport, eap_tls, eap_tls_closed, eap_tls_error}}'
%%% 	with {@link //ssl/ssl:listen/2. ssl:listen/2}.
%%%
-module(ocs_eap_tls_transport).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export inet compatible API
-export([setopts/2, getopts/2, peername/1, sockname/1, port/1]).
%% export gen_tcp compatible API
-export([listen/2, accept/2, send/2, controlling_process/2,
		shutdown/2, close/1]).
%% export public API
-export([ssl_listen/2, deliver/3]).

-export_type([listen_option/0, eap_option/0]).

-type listen_option() :: any().
-type eap_option() :: any().

-define(cb_info,
		{cb_info, {?MODULE, eap_tls, eap_tls_closed, eap_tls_error}}).

%%----------------------------------------------------------------------
%%  ocs_eap_tls_transport public api
%%----------------------------------------------------------------------

-dialyzer({nowarn_function, ssl_listen/2}).
%% The type spec for ssl:listen/2 declares Port as inet:portnumber()
%% however the implementation of that function has no such guard.
-spec ssl_listen(TlsFsm, Options) ->
		{ok, TlsRecordLayerSocket} | {error, Reason} when
	TlsFsm :: pid(),
	Options :: ssl:options(),
	TlsRecordLayerSocket :: ssl:sslsocket(),
	Reason :: term().
%% @doc Creates an {@link //ssl/ssl. ssl} listen socket.
ssl_listen(TlsFsm, Options) when is_pid(TlsFsm), is_list(Options) ->
	ssl:listen(self(), [?cb_info | Options]).

-spec deliver(SslPid, TlsFsm, Data) ->
	ok when
		SslPid :: pid(),
		TlsFsm :: pid(),
		Data :: iodata().
%% @doc Deliver received EAP-TLS payload to SSL.
deliver(SslPid, TlsFsm, Data) when is_pid(SslPid), is_pid(TlsFsm) ->
	SslPid ! {eap_tls, TlsFsm, iolist_to_binary(Data)},
	ok.

%%----------------------------------------------------------------------
%%  ocs_eap_tls_transport callbacks
%%----------------------------------------------------------------------

-spec peername(TlsFsm) ->
	{ok, {Address, Port}} | {error, Reason} when
		TlsFsm :: pid(),
      Address :: inet:ip_address(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the address and port for the other end of a connection.
peername(TlsFsm) when is_pid(TlsFsm) -> 
	{ok, {{127,0,0,1}, 0}}.

-spec sockname(TlsFsm) ->
	{ok, {Address, Port}} | {error, Reason} when
		TlsFsm :: pid(),
      Address :: inet:ip_address(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the local address and port number for an EAP session.
sockname(TlsFsm) when is_pid(TlsFsm) -> 
	{ok, {{127,0,0,1}, 0}}.

-spec port(TlsFsm) ->
	{ok, Port} | {error, Reason} when
		TlsFsm :: pid(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the local port number for an EAP session.
port(TlsFsm) when is_pid(TlsFsm) ->
	{ok, 0}.

-spec setopts(TlsFsm, Options) ->
	ok | {error, Reason} when
		TlsFsm :: pid(),
      Options :: [eap_option()],
		Reason :: term().
%% @doc Sets one or more options for an EAP session.
setopts(TlsFsm, Options) when is_pid(TlsFsm) -> 
	case proplists:get_value(active, Options) of
		undefined ->
			ok;
		_Active ->
			% TlsFsm ! {ssl_setopts, Options},
			ok
	end.

-spec getopts(TlsFsm, Options) ->
	{ok, OptionValues} | {error, Reason} when
		TlsFsm :: pid(),
      Options :: [eap_option()],
      OptionValues :: [eap_option()],
		Reason :: term().
%% @doc Gets one or more options for an EAP session.
getopts(TlsFsm, _Options) when is_pid(TlsFsm) ->
	{ok, []}.

-spec listen(TlsFsm, Options) ->
	{ok, TlsFsm} | {error, Reason} when
		TlsFsm :: pid(),
		Options :: [listen_option()],
		Reason :: term().
%% @doc Listen on an EAP session.
listen(TlsFsm, _Options) when is_pid(TlsFsm) ->
	{ok, TlsFsm}.

-spec accept(TlsFsm, Timeout) ->
	{ok, TlsFsm} | {error, Reason} when
		TlsFsm :: pid(),
		Timeout :: timeout(),
		Reason :: term().
%% @doc Accepts an incoming connection request on a listen socket. 
accept(TlsFsm, _Timeout) when is_pid(TlsFsm) ->
	{ok, TlsFsm}.

-spec shutdown(TlsFsm, How) ->
	ok | {error, Reason} when
		TlsFsm :: pid(),
		How :: read | write | read_write,
		Reason :: term().
%% @doc Close an EAP session in one or two directions.
shutdown(TlsFsm, _How) when is_pid(TlsFsm) ->
	ok.

-spec close(TlsFsm) ->
	ok when
		TlsFsm :: pid().
%% @doc Close an EAP session.
close(TlsFsm) when is_pid(TlsFsm) ->
	ok.

-spec send(TlsFsm, Data) ->
	ok | {error, Reason} when
		TlsFsm :: pid(),
		Data :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(TlsFsm, Data) when is_pid(TlsFsm) ->
	gen_fsm:send_event(TlsFsm, {eap_tls, self(), Data}).

-spec controlling_process(TlsFsm, Pid) ->
	ok | {error, Reason} when
		TlsFsm :: pid(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(TlsFsm, Pid) when is_pid(TlsFsm) ->
	gen_fsm:send_event(TlsFsm, {ssl_pid, Pid}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

