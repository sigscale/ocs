%%% ocs_diameter_auth_service_callback.erl 
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module receives {@link //diameter. diameter} messages on a port assigned
%%% 	for authentication in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://tools.ietf.org/pdf/rfc6733.pdf">
%%% 	RFC6733 - DIAMETER base protocol</a>
%%%
-module(ocs_diameter_auth_service_callback).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
			prepare_retransmit/3, handle_answer/4, handle_error/4,
			handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-record(state, {}).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  diameter_codec:packet().
-type message() ::  diameter_codec:message().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

%%----------------------------------------------------------------------
%%  The Diameter application callbacks
%%----------------------------------------------------------------------

-spec peer_up(SvcName, Peer, State) -> NewState
	when
		SvcName :: diameter:service_name(),
		Peer ::  peer(),
		State :: state(),
		NewState :: state().
%% @doc Invoked when the peer connection is available
peer_up(_SvcName, _Peer, State) ->
    State.

-spec peer_down(SvcName, Peer, State) -> NewState
	when
		SvcName :: diameter:service_name(),
		Peer :: peer(),
		State :: state(),
		NewState :: state().
%% @doc Invoked when the peer connection is not available
peer_down(_SvcName, _Peer, State) ->
    State.

-spec pick_peer(LocalCandidates, RemoteCandidates, SvcName, State) -> Result
	when
		LocalCandidates :: [peer()],
		RemoteCandidates :: [peer()],
		SvcName :: diameter:service_name(),
		State :: state(),
		NewState :: state(),
		Selection :: {ok, Peer} | {Peer, NewState},
		Peer :: peer() | false,
		Result :: Selection | false.
%% @doc Invoked as a consequence of a call to diameter:call/4 to select
%% a destination peer for an outgoing request. 
pick_peer(_, _, _SvcName, _State) ->
    false.

-spec prepare_request(Packet, SvcName, Peer) -> Action
	when
		Packet :: packet(),
		SvcName :: diameter:service_name(),
		Peer :: peer(),
		Action :: Send | Discard | {eval_packet, Action, PostF},
		Send :: {send, packet() | message()},
		Discard :: {discard, Reason} | discard,
		Reason :: term(),
		PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and transport 
prepare_request(_, _SvcName, _Peer) ->
    discard.

-spec prepare_retransmit(Packet, SvcName, Peer) -> Action
	when
		Packet :: packet(),
		SvcName :: diameter:service_name(),
		Peer :: peer(),
		Action :: Send | Discard | {eval_packet, Action, PostF},
		Send :: {send, packet() | message()},
		Discard :: {discard, Reason} | discard,
		Reason :: term(),
		PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and retransmission.
%% In case of peer connection is lost alternate peer is selected.
prepare_retransmit(_Packet, _SvcName, _Peer) ->
    discard.

-spec handle_answer(Packet, Request, SvcName, Peer) -> Result
	when
		Packet :: packet(),
		Request :: message(),
		SvcName :: diameter:service_name(),
		Peer :: peer(),
		Result :: term().
%% @doc Invoked when an answer message is received from a peer.
handle_answer(_Packet, _Request, _SvcName, _Peer) ->
    not_implemented.

-spec handle_error(Reason, Request, SvcName, Peer) -> Result
	when
		Reason :: timeout | failover | term(),
		Request :: message(),
		SvcName :: diameter:service_name(),
		Peer :: peer(),
		Result :: term().
%% @doc Invoked when an error occurs before an answer message is received
%% in response to an outgoing request.
handle_error(_Reason, _Request, _SvcName, _Peer) ->
	not_implemented.

-spec handle_request(Packet, SvcName, Peer) -> Action
	when
		Packet :: packet(),
		SvcName :: term(),
		Peer :: peer(),
		Action :: Reply | {relay, [Opt]} | discard
			| {eval|eval_packet, Action, PostF},
		Reply :: {reply, packet() | message()}
			| {answer_message, 3000..3999|5000..5999}
			| {protocol_error, 3000..3999},
		Opt :: diameter:call_opt(),
		PostF :: diameter:evaluable().
%% @doc Invoked when a request messge is received from the peer. 
handle_request(#diameter_packet{msg = Req, errors = []}, _SvcName, {_, Caps})
		when is_record(Req, diameter_base_RAR) ->
	#diameter_caps{origin_host = {OH,_}, origin_realm = {OR,_}} = Caps,
	#diameter_base_RAR{'Session-Id' = Id, 'Re-Auth-Request-Type' = Type} = Req,
	Action = {reply, #diameter_base_RAA{'Result-Code' = rc(Type),
			'Origin-Host' = OH, 'Origin-Realm' = OR, 'Session-Id' = Id}},
	Action;
handle_request(#diameter_packet{msg = Req}, _SvcName, {_, Caps})
		when is_record(Req, diameter_base_RAR) ->
	#diameter_caps{origin_host = {OH,_}, origin_realm = {OR,_}} = Caps,
	#diameter_base_RAR{'Session-Id' = Id} = Req,
	Action = {reply, #diameter_base_RAA{'Origin-Host' = OH,
		'Origin-Realm' = OR, 'Session-Id' = Id}},
	Action;
handle_request(#diameter_packet{}, _SvcName, _) ->
	{answer_message, 3001}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @doc Generate result codes
%% @hidden
rc(0) ->
    2001;
rc(_) ->
    5012.
