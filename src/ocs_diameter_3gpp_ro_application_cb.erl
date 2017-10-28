%%% ocs_diameter_3gpp_ro_application_cb.erl 
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
%%% 	for the 3GPP DIAMETER Ro in the {@link //ocs. ocs} application.
%%%
%%% @reference 3GPP TS 29.299 Diameter charging applications
%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%% 	RFC4006 - DIAMETER Credit-Control Application </a>
%%%
-module(ocs_diameter_3gpp_ro_application_cb).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
			prepare_retransmit/3, handle_answer/4, handle_error/4,
			handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_3gpp.hrl").
-include("../include/diameter_gen_3gpp_ro_application.hrl").
-include("ocs.hrl").

-record(state, {}).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  #diameter_packet{}.
-type message() ::  tuple() | list().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

%%----------------------------------------------------------------------
%%  The DIAMETER application callbacks
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
pick_peer([Peer | _], _, _SvcName, _State) ->
	{ok, Peer}.

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
prepare_request(#diameter_packet{msg = ['RAR' = T | Avps]}, _, {_, Caps}) ->
	#diameter_caps{origin_host = {OH, DH}, origin_realm = {OR, DR}} = Caps,
	{send, [T, {'Origin-Host', OH}, {'Origin-Realm', OR},
			{'Destination-Host', DH}, {'Destination-Realm', DR} | Avps]};
prepare_request(#diameter_packet{msg = Record}, _, {_, Caps}) ->
	#diameter_caps{origin_host = {OH, DH}, origin_realm = {OR, DR}} = Caps,
	ASR = Record#diameter_base_ASR{'Origin-Host' = OH, 'Origin-Realm' = OR,
	'Destination-Host' = DH, 'Destination-Realm' = DR},
	{send, ASR}.

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
prepare_retransmit(Packet, SvcName, Peer) ->
	prepare_request(Packet, SvcName, Peer).

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
handle_request(#diameter_packet{msg = Req, errors = []},
		SvcName, {_Peer, Caps}) ->
	request(SvcName, Caps, Req);
handle_request(#diameter_packet{errors = [{ResultCode, _} | _]}, _, _) ->
	{answer_message, ResultCode};
handle_request(#diameter_packet{errors = [ResultCode | _]}, _, _) ->
	{answer_message, ResultCode}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec request(Svc, Caps, Request) -> Action
	when
		Svc :: atom(),
		Caps :: capabilities(),
		Request :: message(),
		Action :: Reply | {relay, [Opt]} | discard
			| {eval|eval_packet, Action, PostF},
		Reply :: {reply, packet() | message()}
			| {answer_message, 3000..3999|5000..5999}
			| {protocol_error, 3000..3999},
		Opt :: diameter:call_opt(),
		PostF :: diameter:evaluable().
%% @doc Handle received request.
%% 	Authorize client then forward capabilities and request
%% 	to the accounting port server matching the service the
%% 	request was received on.
%% @hidden
request(SvcName, Capabilities, Request) ->
	#diameter_caps{host_ip_address = {_, HostIpAddresses}} = Capabilities,
	request(SvcName, Capabilities, Request, HostIpAddresses).
%% @hidden
request({_, Address, Port} = SvcName, Capabilities, Request, [H | T]) ->
	case ocs:find_client(H) of
		{ok, #client{protocol = diameter}} ->
			PortServer = global:whereis_name({ocs_diameter_acct, Address, Port}),
			{reply, gen_server:call(PortServer,
					{diameter_request, Capabilities, Request})};
		{error, not_found} ->
			request(SvcName, Capabilities, Request, T)
	end;
request(_, _, _, []) ->
	{answer, 3010}.

