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

-export_type([listen_option/0, eap_option/0]).

-type listen_option() :: any().
-type eap_option() :: any().

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
	{ok, TtlFsm} | {error, Reason} when
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

-spec send(TtlsFsm, Packet) ->
	ok | {error, Reason} when
		TtlsFsm :: pid(),
		Packet :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(TtlsFsm, Packet) when is_pid(TtlsFsm) ->
	gen_fsm:send_event(TtlFsm, Packet).

-spec controlling_process(TtlsFsm, Pid) ->
	ok | {error, Reason} when
		TtlsFsm :: pid(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(TtlsFsm, Pid) when is_pid(TtlsFsm) ->
	gen_fsm:send_event(TtlFsm, {ssl_pid, Pid}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

