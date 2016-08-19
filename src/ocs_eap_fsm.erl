%%% ocs_eap_fsm.erl
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
%%% @reference <a href="http://tools.ietf.org/rfc/rfc3579.txt">
%%% 	RFC3579 - RADIUS Support For EAP</a>
%%%
-module(ocs_eap_fsm).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the radius_fsm API
-export([]).

%% export the radius_fsm state callbacks
-export([idle/2, wait_for_id/2, wait_for_commit/2, wait_for_confirm/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-record(statedata,
		{address :: inet:ip_address(),
		port :: pos_integer(),
		identifier :: non_neg_integer(),
		authenticator :: binary(),
		radius_fsm :: pid(),
		token :: binary(),
		prep :: none | rfc2759 | saslprep,
		peer_id :: string(),
		pwe :: binary(),
		s_rand :: integer(),
		scalar_s :: binary(),
		element_s :: binary(),
		scalar_p :: binary(),
		element_p :: binary(),
		ks :: binary(),
		confirm_s :: binary()}).

-define(TIMEOUT, 30000).

%%----------------------------------------------------------------------
%%  The radius_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The radius_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	Result :: {ok, StateName :: atom(), StateData :: #statedata{}}
		| {ok, StateName :: atom(), StateData :: #statedata{},
			Timeout :: non_neg_integer() | infinity}
		| {ok, StateName :: atom(), StateData :: #statedata{}, hibernate}
		| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([RadiusFsm, Authenticator, Address, Port, Identifier] = _Args) ->
	process_flag(trap_exit, true),
	StateData = #statedata{radius_fsm = RadiusFsm, address = Address,
			port = Port, identifier = Identifier, authenticator = Authenticator},
	{ok, idle, StateData, 0}.

-spec idle(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>idle</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
idle(timeout, #statedata{identifier = Identifier, radius_fsm = RadiusFsm,
		authenticator = Authenticator} = StateData) ->
	Token = binary_to_list(crypto:rand_bytes(4)),
	{ok, HostName} = inet:gethostname(),
	GrpDesc = 19,
	RandFunc = 16#1,
	Prf = 16#1,
	PwdPrep = none,
	PwdExch = id,
	Body = #eap_pwd_id{group_desc = GrpDesc, random_fun = RandFunc, prf = Prf, token = Token,
		pwd_prep = PwdPrep, identity = HostName},
	BodyData = ocs_eap_codec:eap_pwd_id(Body),
	Header = #eap_pwd{type = ?PWD, length = false, more = false, pwd_exch = PwdExch,
		data = BodyData},
	EAPData = ocs_eap_codec:eap_pwd(Header),
	Packet = #eap_packet{code = ?Request, identifier = Identifier, data = EAPData},
	EAPPacketData = ocs_eap_codec:eap_packet(Packet),
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?EAPMessage, EAPPacketData, AttributeList0),
	Response = #radius{code = ?AccessChallenge, id = Identifier, authenticator = Authenticator, attributes = AttributeList1},
	ResponsePacket = radius:codec(Response),
	radius:response(RadiusFsm, ResponsePacket),
	NewStateData = StateData#statedata{token = Token, prep = PwdPrep},
	{next_state, wait_for_id, NewStateData}.

-spec wait_for_id(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>wait_for_id</b> state. This state is responsible
%%		for sending EAP-PWD-ID request to peer.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
wait_for_id(timeout, #statedata{identifier = Identifier} = StateData)->
	{stop, {shutdown, Identifier}, StateData};
wait_for_id({request, _Address, _Port, _Packet} , StateData)->
	{next_state, wait_for_commit, StateData, ?TIMEOUT}.

-spec wait_for_commit(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>wait_for_commit</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
wait_for_commit(timeout, #statedata{identifier = Identifier} = StateData)->
	{stop, {shutdown, Identifier}, StateData};
wait_for_commit(_Event, StateData)->
	{next_state, wait_for_confirm, StateData, ?TIMEOUT}.

-spec wait_for_confirm(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>wait_for_confirm</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
wait_for_confirm(timeout, #statedata{identifier = Identifier} = StateData)->
	{stop, {shutdown, Identifier}, StateData};
wait_for_confirm(_Event, StateData)->
	{next_state, idle, StateData, ?TIMEOUT}.

-spec handle_event(Event :: term(), StateName :: atom(),
		StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec handle_sync_event(Event :: term(), From :: {Pid :: pid(), Tag :: term()},
		StateName :: atom(), StateData :: #statedata{}) ->
	Result :: {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: #statedata{}}
		| {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), Reply :: term(), NewStateData :: #statedata{}}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

-spec handle_info(Info :: term(), StateName :: atom(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec terminate(Reason :: normal | shutdown | term(), StateName :: atom(),
		StateData :: #statedata{}) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason, _StateName, _StateData) ->
	ok.

-spec code_change(OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(), StateData :: #statedata{}, Extra :: term()) ->
	Result :: {ok, NextStateName :: atom(), NewStateData :: #statedata{}}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

