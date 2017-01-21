%%% ocs_eap_ttls_aaah_fsm.erl
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback module
%%% 	implements the functions associated with a AAA server in the user's
%%% 	home domain (AAA/H) within EAP Tunneled Transport Layer Security
%%% 	(EAP-TTLS) in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/rfc/rfc5281.txt">
%%% 	RFC5281 - EAP Tunneled Transport Layer Security (EAP-TTLS)</a>
%%%
-module(ocs_eap_ttls_aaah_fsm).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_ttls_aaah_fsm API
-export([]).

%% export the ocs_eap_ttls_aaah_fsm state callbacks
-export([idle/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").

-record(statedata,
		{ttls_fsm :: undefined | pid(),
		ssl_socket :: undefined | ssl:sslsocket()}).
-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_aaah_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_aaah_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	Result :: {ok, StateName :: atom(), StateData :: statedata()}
		| {ok, StateName :: atom(), StateData :: statedata(),
			Timeout :: non_neg_integer() | infinity}
		| {ok, StateName :: atom(), StateData :: statedata(), hibernate}
		| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init(_Args) ->
	process_flag(trap_exit, true),
	{ok, idle, #statedata{}, ?TIMEOUT}.

-spec idle(Event :: timeout | term(), StateData :: statedata()) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>idle</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
idle(timeout, #statedata{} = StateData) ->
	{stop, shutdown, StateData};
idle({ttls_socket, TtlsFsm, TlsRecordLayerSocket}, StateData) ->
	case ssl:transport_accept(TlsRecordLayerSocket) of
		{ok, SslSocket} ->
			case ssl:ssl_accept(SslSocket, ?TIMEOUT) of
				ok ->
					NewStateData = StateData#statedata{ssl_socket = SslSocket,
							ttls_fsm = TtlsFsm},
					{next_state, request, NewStateData};
				{error, Reason} ->
					{stop, Reason, StateData}
			end;
		{error, Reason} ->
			{stop, Reason, StateData}
	end.

-spec handle_event(Event :: term(), StateName :: atom(),
		StateData :: statedata()) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData, ?TIMEOUT}.

-spec handle_sync_event(Event :: term(), From :: {Pid :: pid(), Tag :: term()},
		StateName :: atom(), StateData :: statedata()) ->
	Result :: {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: statedata()}
		| {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), Reply :: term(), NewStateData :: statedata()}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

-spec handle_info(Info :: term(), StateName :: atom(), StateData :: statedata()) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info({ssl, SslSocket, AVPs}, request, 
		#statedata{ssl_socket = SslSocket,
		ttls_fsm = TtlsFsm} = StateData) ->
	try
		AvpList = diameter_codec:collect_avps(AVPs),
		#diameter_avp{data = Password} = lists:keyfind(?UserPassword, #diameter_avp.code, AvpList),
		#diameter_avp{data = Identity1} = lists:keyfind(?UserName, #diameter_avp.code, AvpList),
		handle_info1(Identity1, iolist_to_binary(Password))
	of
		{ok, Identity2} ->
			gen_fsm:send_event(TtlsFsm, {accept, Identity2, SslSocket}),
			{next_state, request, StateData};
		{error, Reason} ->
			gen_fsm:send_event(TtlsFsm, reject),
			{stop, Reason, StateData}
	catch
		_:Reason ->
			gen_fsm:send_event(TtlsFsm, reject),
			{stop, Reason, StateData}
			
	end;
handle_info({ssl_closed, SslSocket}, request, #statedata{ssl_socket = SslSocket,
		ttls_fsm = _TtlsFsm} = StateData) ->
	%gen_fsm:send_event(RadiusFsm, {reject, SslSocket, socket_closed}),
	{stop, shutdown, StateData};
handle_info({ssl_error, SslSocket, Reason}, request, #statedata{ssl_socket = SslSocket,
		ttls_fsm = _TtlsFsm} = StateData) ->
	%gen_fsm:send_event(RadiusFsm, {reject, SslSocket, Reason}),
	{stop, Reason, StateData}.
%% @hidden
handle_info1(Subscriber, Password) ->
	try
		case ocs:find_subscriber(Subscriber) of
			{ok, UserPassWord, _, _, _} ->
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

-spec terminate(Reason :: normal | shutdown | term(), StateName :: atom(),
		StateData :: statedata()) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason, _StateName, _StateData) ->
	ok.

-spec code_change(OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(), StateData :: statedata(), Extra :: term()) ->
	Result :: {ok, NextStateName :: atom(), NewStateData :: statedata()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------
