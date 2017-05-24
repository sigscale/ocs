%%% ocs_diameter_cc_application_cb.erl 
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
%%% 	for authentication in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://tools.ietf.org/pdf/rfc7155.pdf">
%%% 	RFC6733 - DIAMETER Network Access Server Application</a>
%%%
-module(ocs_diameter_cc_application_cb).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
			prepare_retransmit/3, handle_answer/4, handle_error/4,
			handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_nas_application_rfc7155.hrl").
-include("../include/diameter_gen_cc_application_rfc4006.hrl").

-record(state, {}).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  diameter_codec:packet().
-type message() ::  diameter_codec:message().
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
handle_request(#diameter_packet{msg = Req, errors = []},
		SvcName, {_Peer, Caps}) ->
	is_client_authorized(SvcName, Caps, Req).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec send_to_port_server(Svc, Caps, Request) -> Action
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
%% @doc Locate ocs_diameter_acct_port_server process and sent it
%% peer's capabilities and diameter request.
send_to_port_server(Svc, Caps, Request) ->
	[Info] = diameter:service_info(Svc, transport),
	case lists:keyfind(options, 1, Info) of
		{options, Options} ->
			case lists:keyfind(transport_config, 1, Options) of
				{transport_config, [_, {ip, IP}, {port, Port}]} ->
					case global:whereis_name({ocs_diameter_acct, IP, Port}) of
						undefined ->
							discard;
						PortServer ->
							Answer = gen_server:call(PortServer,
									{diameter_request, Caps, Request}),
							{reply, Answer}
					end;
				false ->
					discard
			end;
		false ->
			discard
	end.

-spec is_client_authorized(Svc, Caps, Request) -> Action
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
%% @doc Checks DIAMETER client's identity present in Host-IP-Address AVP in
%% CER message against identities in client table.
%% @hidden
is_client_authorized(SvcName, Caps, Req) ->
	try
		HostIPAddresses = Caps#diameter_caps.host_ip_address,
		{ClientIPs, _} = HostIPAddresses,
		[HostIpAddress | _] = ClientIPs,
		{ok, _, diameter, _} = ocs:find_client(HostIpAddress),
		true
	of
		true ->
			send_to_port_server(SvcName, Caps, Req)
	catch
		_ : _ ->
			send_error(Caps, Req, ?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_PEER')
	end.

-spec send_error(Caps, Request, ErrorCode) -> Answer
	when
		Caps :: capabilities(),
		Request :: message(),
		ErrorCode :: non_neg_integer(),
		Answer :: message().
%% @doc When protocol/application error occurs, send DIAMETER answer with appropriate
%% error indicated in Result-Code AVP.
%% @hidden
send_error(Caps, Request, ErrorCode) ->
	#diameter_caps{origin_host = {OHost,_},
			origin_realm = {ORealm, DRealm}} = Caps,
send_error(OHost, ORealm, DRealm, Request, ErrorCode).

%% @hidden
send_error(OHost, ORealm, DRealm, Request, ErrorCode)
		when is_record(Request, diameter_nas_app_AAR)->
	#diameter_nas_app_AAR{'Session-Id' = SessId,
			'Auth-Request-Type' = Type}= Request,
	#diameter_nas_app_AAA{'Result-Code' = ErrorCode,
			'Error-Reporting-Host' = DRealm, 'Auth-Request-Type' = Type,
			'Auth-Application-Id' = 1, 'Origin-Host' = OHost,
			'Origin-Realm' = ORealm, 'Session-Id' = SessId}.

