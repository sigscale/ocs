%%% ocs_simple_auth_fsm.erl
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
-module(ocs_simple_auth_fsm).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_simple_auth_fsm API
-export([]).

%% export the ocs_simple_auth_fsm state callbacks
-export([request/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-record(statedata,
		{address :: inet:ip_address(),
		port :: pos_integer(),
		radius_fsm :: pid(),
		secret :: binary(),
		session_id:: {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		radius_id :: byte(),
		req_auth :: binary(),
		req_attr :: radius_attributes:attributes(),
		subscriber :: binary()}).

-define(TIMEOUT, 30000).

%%----------------------------------------------------------------------
%%  The ocs_simple_auth_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_simple_auth_fsm gen_fsm call backs
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
init([Address, Port, RadiusFsm, Secret, SessionID,
		#radius{code = ?AccessRequest, id = ID,
		authenticator = Authenticator, attributes = Attributes}] = _Args) ->
	StateData = #statedata{address = Address, port = Port,
		radius_fsm = RadiusFsm, secret = Secret, session_id = SessionID,
		radius_id = ID, req_auth = Authenticator, req_attr = Attributes},
	process_flag(trap_exit, true),
	{ok, request, StateData, 0}.

-spec request(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>request</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
request(timeout, #statedata{req_attr = Attributes,
		session_id = SessionID} = StateData) ->
	case radius_attributes:find(?UserName, Attributes) of
		{ok, Subscriber} ->
			request1(StateData#statedata{subscriber = Subscriber});
		{error, not_found} ->
			response(?AccessReject, [], StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
request1(#statedata{req_attr = Attributes, req_auth = Authenticator,
		secret = Secret, session_id = SessionID} = StateData) ->
	case radius_attributes:find(?UserPassword, Attributes) of
		{ok, Hidden} ->
			Password = radius_attributes:unhide(Secret, Authenticator, Hidden),
			request2(list_to_binary(Password), StateData);
		{error, not_found} ->
			response(?AccessReject, [], StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
request2(Password, #statedata{subscriber = Subscriber,
		session_id = SessionID} = StateData) ->
	case ocs:find_subscriber(Subscriber) of
		{ok, Password, ResponseAttributes, Balance, _Enabled} when Balance > 0 ->
			case ocs:subscriber_status(Subscriber, false) of
				ok ->
					response(?AccessAccept, ResponseAttributes, StateData),
					{stop, {shutdown, SessionID}, StateData};
				{error, Reason} ->
					error_logger:warning_report(["Faild to set subscriber status",
						{session_id, SessionID}, {peer, Subscriber}, {error, Reason}]),
					{stop, {shutdown, SessionID}, StateData}
			end;
		{ok, Password, _, _, _} ->
			RejectAttributes = [{?ReplyMessage, "Out of Credit"}],
			response(?AccessReject, RejectAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		{ok, _, _, _, _} ->
			RejectAttributes = [{?ReplyMessage, "Bad Password"}],
			response(?AccessReject, RejectAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		{error, not_found} ->
			RejectAttributes = [{?ReplyMessage, "Unknown Username"}],
			response(?AccessReject, RejectAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.

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

-spec response(RadiusCode :: byte(),
		ResponseAttributes :: radius_attributes:attributes(),
		StateData :: #statedata{}) -> ok.
%% @doc Send a RADIUS Access-Reject or Access-Accept reply
%% @hidden
response(RadiusCode, ResponseAttributes,
		#statedata{radius_id = RadiusID, req_auth = RequestAuthenticator,
		secret = Secret, radius_fsm = RadiusFsm} = _StateData) ->
	AttributeList1 = radius_attributes:store(?MessageAuthenticator,
		<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, ResponseAttributes),
	Attributes1 = radius_attributes:codec(AttributeList1),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = crypto:hmac(md5, Secret, [<<RadiusCode, RadiusID,
			Length:16>>, RequestAuthenticator, Attributes1]),
	AttrbuteList2 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttributeList1),
	Attributes2 = radius_attributes:codec(AttrbuteList2),
	ResponseAuthenticator = crypto:hash(md5, [<<RadiusCode, RadiusID,
			Length:16>>, RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	ResponsePacket = radius:codec(Response),
	radius:response(RadiusFsm, {response, ResponsePacket}).

