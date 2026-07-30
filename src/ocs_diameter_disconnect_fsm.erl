%%% ocs_diameter_disconnect_fsm.erl
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
%%% 	module implements sending DIAMETER Abort-Session-Request to DIAMETER
%%% 	credit-control clients (Network Access Servers)
%%% 	the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://tools.ietf.org/html/rfc6733#section-8.5">
%%% 	RFC6733 - Diameter Base Protocol, Sec 8.5, Aborting a Session</a>
%%%
-module(ocs_diameter_disconnect_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([send_request/3, receive_response/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-record(statedata,
		{diameter_service :: term(),
		app_alias :: term(),
		session_id :: string(),
		origin_host :: string() | binary(),
		destination_host :: string() | binary(),
		origin_realm :: string() | binary(),
		destination_realm :: string() | binary(),
		retry_time = 500 :: integer(),
		retry_count = 0 :: integer(),
		auth_app_id :: integer()}).
-type statedata() :: #statedata{}.
-type state() :: send_request | receive_response.

%%----------------------------------------------------------------------
%%  The ocs_diameter_disconnect_fsm gen_statem call backs
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
init([Svc, AppAlias, SessionId, OHost, DHost, ORealm, DRealm,
		AuthAppId] = _Args) ->
	process_flag(trap_exit, true),
	Data = #statedata{diameter_service = Svc, app_alias = AppAlias,
			session_id = SessionId, origin_host = OHost,
			destination_host = DHost, origin_realm = ORealm,
			destination_realm = DRealm, auth_app_id = AuthAppId},
	{ok, send_request, Data, {timeout, 0, initial}}.

-spec send_request(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>send_request</em> state.
%% @private
send_request(timeout = _EventType, EventContent,
		#statedata{diameter_service = Svc, app_alias = AppAlias,
				session_id = SId, origin_host = OH, destination_host = DH,
				origin_realm = OR, destination_realm = DR,
				auth_app_id = AuthAppId, retry_time = Retry,
				retry_count = Count} = Data)
		when EventContent == initial; EventContent == retry ->
	ASR = #diameter_base_ASR{'Session-Id' = SId,
			'Origin-Host' = OH, 'Origin-Realm' = OR,
			'Destination-Realm' = DR, 'Destination-Host' = DH,
			'Auth-Application-Id' = AuthAppId},
	case diameter:call(Svc, AppAlias, ASR, []) of
		ok ->
			{stop, {shutdown, SId}, Data};
		{error, _Reason} ->
			NewRetry = Retry * 2,
			NewCount = Count + 1,
			NewData = Data#statedata{retry_count = NewCount,
					retry_time = NewRetry},
			{keep_state, NewData, {timeout, retry, NewRetry}};
		{ok, _ASA} ->
			{stop, {shutdown, SId}, Data}
	end.

-spec receive_response(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>receive_response</em> state.
%% @private
receive_response(_EventType, _EventContent, _Data) ->
	keep_state_and_data.

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

