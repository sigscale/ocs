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
%%% @todo Implement all the functions!
%%% 
-module(ocs_eap_ttls_transport).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export inet compatible API
-export([setopts/2, getopts/2, peername/1, sockname/1, port/1]).
%% export gen_tcp compatible API
-export([listen/2, accept/2, send/2, controlling_process/2,
		shutdown/2, close/1]).

-export_type([listen_option/0, eap_session/0]).

-record(eap_session, {}).
-record(sslsocket, {fd = nil, pid = nil}).

-type listen_option() :: any().
-type eap_session() :: #eap_session{}.
-type eap_option() :: any().

%%----------------------------------------------------------------------
%%  ocs_eap_ttls_transport callbacks
%%----------------------------------------------------------------------

-spec peername(EapSession) ->
	{ok, {Address, Port}} | {error, Reason} when
		EapSession :: eap_session(),
      Address :: inet:ip_address(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the address and port for the other end of a connection.
peername(EapSession) -> 
	{ok, {{127,0,0,1}, 0}}.

-spec sockname(EapSession) ->
	{ok, {Address, Port}} | {error, Reason} when
		EapSession :: eap_session(),
      Address :: inet:ip_address(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the local address and port number for an EAP session.
sockname(EapSession) -> 
	{ok, {{127,0,0,1}, 0}}.

-spec port(EapSession) ->
	{ok, Port} | {error, Reason} when
		EapSession :: eap_session(),
      Port :: inet:port_number(),
		Reason :: term().
%% @doc Returns the local port number for an EAP session.
port(EapSession) ->
	{ok, 0}.

-spec setopts(EapSession, Options) ->
	ok | {error, Reason} when
		EapSession :: eap_session(),
      Options :: [eap_option()],
		Reason :: term().
%% @doc Sets one or more options for an EAP session.
setopts(EapSession, Options) -> 
	ok.

-spec getopts(EapSession, Options) ->
	{ok, OptionValues} | {error, Reason} when
		EapSession :: eap_session(),
      Options :: [eap_option()],
      OptionValues :: [eap_option()],
		Reason :: term().
%% @doc Gets one or more options for an EAP session.
getopts(EapSession, Options) ->
	{ok, []}.

-spec listen(Port, Options) ->
	{ok, ListenSocket} | {error, Reason} when
		Port :: term(),
		Options :: [listen_option()],
		ListenSocket :: ssl:sslsocket(),
		Reason :: term().
%% @doc Listen on an EAP session.
%% @todo What value shall Port take?
listen(Port, Options) ->
	{ok, #sslsocket{}}.

-spec accept(ListenSocket, Timeout) ->
	{ok, EapSession} | {error, Reason} when
		ListenSocket :: ssl:sslsocket(),
		Timeout :: timeout(),
		EapSession :: eap_session(),
		Reason :: term().
%% @doc Accepts an incoming connection request on a listen socket. 
accept(ListenSocket, Timeout) ->
	{ok, #eap_session{}}.

-spec shutdown(EapSession, How) ->
	ok | {error, Reason} when
		EapSession :: eap_session(),
		How :: read | write | read_write,
		Reason :: term().
%% @doc Close an eap_session in one or two directions.
shutdown(EapSession, How) ->
	ok.

-spec close(EapSession) ->
	ok when
		EapSession :: eap_session().
%% @doc Close an EAP session.
close(EapSession) ->
	ok.

-spec send(EapSession, Packet) ->
	ok | {error, Reason} when
		EapSession :: eap_session(),
		Packet :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(EapSession, Packet) ->
	ok.

-spec controlling_process(EapSession, Pid) ->
	ok | {error, Reason} when
		EapSession :: eap_session(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(EapSession, Pid) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

