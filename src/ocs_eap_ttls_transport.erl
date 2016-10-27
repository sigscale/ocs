%%% ocs_eap_ttls_transport.erl
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
-module(ocs_eap_ttls_transport).
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
		{cb_info, {?MODULE, eap_ttls, eap_ttls_closed, eap_ttls_error}}).

%%----------------------------------------------------------------------
%%  ocs_eap_ttls_transport public api
%%----------------------------------------------------------------------

-dialyzer({nowarn_function, ssl_listen/2}).
%% The type spec for ssl:listen/2 decalres Port as inet:portnumber()
%% however the implementation of that function has no such guard.
-spec ssl_listen(TtlsFsm, Options) ->
		{ok, Socket} | {error, Reason} when
	TtlsFsm :: pid(),
	Options :: ssl:options(),
	Socket :: ssl:sslsocket(),
	Reason :: term().
%% @doc Start an {@link //ssl/ssl. ssl} listener process.
ssl_listen(TtlsFsm, Options) when is_pid(TtlsFsm), is_list(Options) ->
	ssl:listen(self(), [?cb_info | Options]).

-spec deliver(SslPid, TtlsFsm, Data) ->
	ok when
		SslPid :: pid(),
		TtlsFsm :: pid(),
		Data :: binary().
%% @doc Deliver received EAP-TTLS payload to SSL.
deliver(SslPid, TtlsFsm, Data) when is_pid(SslPid),
		is_pid(TtlsFsm), is_binary(Data) ->
	SslPid ! {eap_ttls, TtlsFsm, Data},
	ok.

%%----------------------------------------------------------------------
%%  ocs_eap_ttls_transport callbacks
%%----------------------------------------------------------------------

-spec peername(TtlsFsm) ->
	{ok, {Address, Port}} | {error, Reason} when
		TtlsFsm :: pid(),
      Address :: inet:ip_address(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the address and port for the other end of a connection.
peername(TtlsFsm) when is_pid(TtlsFsm) -> 
	{ok, {{127,0,0,1}, 0}}.

-spec sockname(TtlsFsm) ->
	{ok, {Address, Port}} | {error, Reason} when
		TtlsFsm :: pid(),
      Address :: inet:ip_address(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the local address and port number for an EAP session.
sockname(TtlsFsm) when is_pid(TtlsFsm) -> 
	{ok, {{127,0,0,1}, 0}}.

-spec port(TtlsFsm) ->
	{ok, Port} | {error, Reason} when
		TtlsFsm :: pid(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the local port number for an EAP session.
port(TtlsFsm) when is_pid(TtlsFsm) ->
	{ok, 0}.

-spec setopts(TtlsFsm, Options) ->
	ok | {error, Reason} when
		TtlsFsm :: pid(),
      Options :: [eap_option()],
		Reason :: term().
%% @doc Sets one or more options for an EAP session.
setopts(TtlsFsm, Options) when is_pid(TtlsFsm) -> 
	ok.

-spec getopts(TtlsFsm, Options) ->
	{ok, OptionValues} | {error, Reason} when
		TtlsFsm :: pid(),
      Options :: [eap_option()],
      OptionValues :: [eap_option()],
		Reason :: term().
%% @doc Gets one or more options for an EAP session.
getopts(TtlsFsm, Options) when is_pid(TtlsFsm) ->
	{ok, []}.

-spec listen(TtlsFsm, Options) ->
	{ok, TtlsFsm} | {error, Reason} when
		TtlsFsm :: pid(),
		Options :: [listen_option()],
		Reason :: term().
%% @doc Listen on an EAP session.
listen(TtlsFsm, Options) when is_pid(TtlsFsm) ->
	{ok, TtlsFsm}.

-spec accept(TtlsFsm, Timeout) ->
	{ok, TtlsFsm} | {error, Reason} when
		TtlsFsm :: pid(),
		Timeout :: timeout(),
		Reason :: term().
%% @doc Accepts an incoming connection request on a listen socket. 
accept(TtlsFsm, Timeout) when is_pid(TtlsFsm) ->
	{ok, TtlsFsm}.

-spec shutdown(TtlsFsm, How) ->
	ok | {error, Reason} when
		TtlsFsm :: pid(),
		How :: read | write | read_write,
		Reason :: term().
%% @doc Close an EAP session in one or two directions.
shutdown(TtlsFsm, How) when is_pid(TtlsFsm) ->
	ok.

-spec close(TtlsFsm) ->
	ok when
		TtlsFsm :: pid().
%% @doc Close an EAP session.
close(TtlsFsm) when is_pid(TtlsFsm) ->
	ok.

-spec send(TtlsFsm, Data) ->
	ok | {error, Reason} when
		TtlsFsm :: pid(),
		Data :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(TtlsFsm, Data) when is_pid(TtlsFsm) ->
	gen_fsm:send_event(TtlsFsm, {eap_ttls, self(), Data}).

-spec controlling_process(TtlsFsm, Pid) ->
	ok | {error, Reason} when
		TtlsFsm :: pid(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(TtlsFsm, Pid) when is_pid(TtlsFsm) ->
	gen_fsm:send_event(TtlsFsm, {ssl_pid, Pid}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

