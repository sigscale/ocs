%%% ocs_eap_ttls_aaah_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2026 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module implements the functions associated with a AAA server in
%%% 	the user's home domain (AAA/H) within EAP Tunneled Transport
%%% 	Layer Security (EAP-TTLS)
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://www.rfc-editor.org/info/rfc5281/">
%%% 	RFC5281 - EAP Tunneled Transport Layer Security (EAP-TTLS)</a>
%%%
-module(ocs_eap_ttls_aaah_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([idle/3, request/3]).

-include("ocs.hrl").
-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").

-record(statedata,
		{ttls_fsm :: undefined | pid(),
		ssl_socket :: undefined | ssl:sslsocket()}).
-type statedata() :: #statedata{}.
-type state() :: idle | request.

-define(TIMEOUT, 30000).

-ifdef(OTP_RELEASE).
	-define(SSL_ACCEPT(Socket, Timeout), ssl:handshake(Socket, Timeout)).
-else.
	-define(SSL_ACCEPT(Socket, Timeout), ssl:ssl_accept(Socket, Timeout)).
-endif.

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_aaah_fsm gen_statem call backs
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions].

-spec init(Args) -> Result
	when
		Args :: [term()],
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:init_result(State, Data).
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_statem:init/1
%% @private
%%
init(_Args) ->
	process_flag(trap_exit, true),
	Action = {timeout, ?TIMEOUT, timeout},
	{ok, idle, #statedata{}, Action}.

-dialyzer({no_match, idle/3}).
-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
idle(cast = _EventType,
		{ttls_socket, TtlsFsm, TlsRecordLayerSocket} = _EventContent,
		Data) ->
	case ssl:transport_accept(TlsRecordLayerSocket) of
		{ok, SslSocket} ->
			case ?SSL_ACCEPT(SslSocket, ?TIMEOUT) of
				ok ->
					NewData = Data#statedata{ssl_socket = SslSocket,
							ttls_fsm = TtlsFsm},
					{next_state, request, NewData};
				{ok, NewSslSocket} ->
					NewData = Data#statedata{ssl_socket = NewSslSocket,
							ttls_fsm = TtlsFsm},
					{next_state, request, NewData};
				{error, Reason} ->
					{stop, Reason, Data}
			end;
		{error, Reason} ->
			{stop, Reason, Data}
	end;
idle(timeout = _EventType, timeout = _EventContent,
		#statedata{} = _Data) ->
	{stop, shutdown}.

-spec request(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>request</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
request(info = _EventType,
		{ssl, SslSocket, AVPs} = _EventContent,
		#statedata{ssl_socket = SslSocket, ttls_fsm = TtlsFsm} = Data) ->
	try
		AvpList = diameter_codec:collect_avps(AVPs),
		#diameter_avp{data = Password} = lists:keyfind(?UserPassword, #diameter_avp.code, AvpList),
		#diameter_avp{data = Identity} = lists:keyfind(?UserName, #diameter_avp.code, AvpList),
		request1(Identity, iolist_to_binary(Password))
	of
		{ok, Subscriber} ->
			gen_statem:cast(TtlsFsm, {accept, Subscriber, SslSocket}),
			{next_state, request, Data};
		{error, Reason} ->
			gen_statem:cast(TtlsFsm, reject),
			{stop, Reason, Data}
	catch
		_:Reason ->
			gen_statem:cast(TtlsFsm, reject),
			{stop, Reason, Data}
			
	end;
request(info = _EventType,
		{ssl_closed, SslSocket} = _EventCont,
		#statedata{ssl_socket = SslSocket, ttls_fsm = _TtlsFsm} = Data) ->
	% gen_statem:cast(RadiusFsm, {reject, SslSocket, socket_closed}),
	{stop, shutdown, Data};
request(info = _EventType,
		{ssl_error, SslSocket, Reason} = _EventContent,
		#statedata{ssl_socket = SslSocket, ttls_fsm = _TtlsFsm} = Data) ->
	% gen_statem:cast(RadiusFsm, {reject, SslSocket, Reason}),
	{stop, Reason, Data}.
%% @hidden
request1(Identity, Password) ->
	try
		case ocs:find_service(Identity) of
			{ok, #service{password = UserPassWord} = Subscriber} ->
				Size = size(UserPassWord),
				<<UserPassWord:Size/binary, _/binary>> = Password,
				{ok, Subscriber};
			{error, not_found} ->
				{error, not_found}
		end
	catch
		_:_ ->
			{error, bad_password}
	end.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, _Data) ->
	ok.

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

