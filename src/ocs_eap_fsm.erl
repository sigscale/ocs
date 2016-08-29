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

%% export the ocs_eap_fsm API
-export([]).

%% export the ocs_eap_fsm state callbacks
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
		session_id:: {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		radius_id :: byte(),
		eap_id :: byte(),
		grp_dec  = 19 :: byte(),
		rand_func = 1 :: byte(),
		prf = 1 :: byte(),
		authenticator :: binary(),
		secret :: binary(),
		password :: binary(),
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
%%  The ocs_eap_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_fsm gen_fsm call backs
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
init([RadiusFsm, Address, Port, Identifier,
		Authenticator, Secret, SessionID] = _Args) ->
	process_flag(trap_exit, true),
	StateData = #statedata{radius_fsm = RadiusFsm, address = Address,
			port = Port, authenticator = Authenticator, secret = Secret,
			radius_id = Identifier, session_id = SessionID},
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
idle(timeout, #statedata{radius_fsm = RadiusFsm, radius_id = RadiusID,
		authenticator = RequestAuthenticator, secret = Secret} = StateData) ->
	EapID = 1,
	Token = crypto:rand_bytes(4),
	{ok, HostName} = inet:gethostname(),
	Body = #eap_pwd_id{group_desc = 19, random_fun = 1, prf = 1, token = Token,
			pwd_prep = none, identity = HostName},
	BodyData = ocs_eap_codec:eap_pwd_id(Body),
	Header = #eap_pwd{type = ?PWD, length = false,
			more = false, pwd_exch = id, data = BodyData},
	EAPData = ocs_eap_codec:eap_pwd(Header),
	Packet = #eap_packet{code = ?Request,
			identifier = EapID, data = EAPData},
	EAPPacketData = ocs_eap_codec:eap_packet(Packet),
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?EAPMessage,
			EAPPacketData, AttributeList0),
	AttributeList2 = radius_attributes:store(?MessageAuthenticator,
			<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, AttributeList1),
	AttrList2bin = radius_attributes:codec(AttributeList2),
	Response1 = #radius{code = ?AccessChallenge, id = RadiusID,
			authenticator = RequestAuthenticator, attributes = AttrList2bin},
	ResponsePacket1 = radius:codec(Response1),
	MessageAuthenticator = crypto:hmac(md5, Secret, ResponsePacket1),
	AttributeList3 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttributeList1),
	AttributeData = radius_attributes:codec(AttributeList3),
	Length = size(AttributeData) + 20,
	ResponseAuthenticator = crypto:hash(md5,[<<?AccessChallenge, RadiusID,
			Length:16>>, RequestAuthenticator, AttributeData, Secret]),
	Response = #radius{code = ?AccessChallenge, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = AttributeData},
	ResponsePacket = radius:codec(Response),
	radius:response(RadiusFsm, {response, ResponsePacket}),
	NewStateData = StateData#statedata{eap_id = EapID, grp_dec = 19,
			rand_func = 1, prf = 1, token = Token, prep = none},
	{next_state, wait_for_id, NewStateData, ?TIMEOUT}.

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
wait_for_id(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
wait_for_id({eap_response, RadiusFsm, EAPPacket} , #statedata{token = Token,
		eap_id = EapID, radius_id = RadiusID, secret = Secret,
		authenticator = RequestAuthenticator, grp_dec = GroupDesc, rand_func = RandFun,
		prf = PRF} = StateData)->
	{ok, HostName_s} = inet:gethostname(),
	HostName = list_to_binary(HostName_s),
	S_rand = crypto:rand_uniform(1, ?R),
	try
		#eap_packet{code = ?Response, identifier = EapID, data = Data} =
				ocs_eap_codec:eap_packet(EAPPacket),
		EAPHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{type = ?PWD, pwd_exch = id, data = BodyData} = EAPHeader,
		Body = ocs_eap_codec:eap_pwd_id(BodyData),
		#eap_pwd_id{group_desc = GroupDesc, random_fun = RandFun, prf = PRF,
				 token = Token, pwd_prep = none, identity = PeerID_s} = Body,
		PeerID = list_to_binary(PeerID_s),
		PWE = ocs_eap_pwd:compute_pwe(Token, PeerID, HostName, Secret),
		{ScalarS, ElementS} = ocs_eap_pwd:compute_scalar(<<S_rand:256>>, PWE),
		CommitReq = #eap_pwd_commit{scalar = ScalarS, element = ElementS},
		CommitReqBody = ocs_eap_codec:eap_pwd_commit(CommitReq),
		CommitReqHeader = #eap_pwd{type = ?PWD, length = false,
				more = false, pwd_exch = commit, data = CommitReqBody},
		CommitEAPData = ocs_eap_codec:eap_pwd(CommitReqHeader),
		NewEAPID = EapID + 1,
		send_radius_response(?Request, NewEAPID, CommitEAPData, ?AccessChallenge, RadiusID,
				RequestAuthenticator, Secret, RadiusFsm),
		NewStateData = StateData#statedata{pwe = PWE, s_rand = S_rand, peer_id = PeerID,
			eap_id = NewEAPID, scalar_s = ScalarS, element_s = ElementS, radius_fsm = RadiusFsm},
		{next_state, wait_for_commit, NewStateData, ?TIMEOUT}
	catch
		_:_ ->
			radius:response(RadiusFsm, {error, ignore}),
			{next_state, wait_for_id, StateData, 0}
	end.

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
wait_for_commit(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
wait_for_commit({eap_response, RadiusFsm, EAPPacket}, #statedata{radius_id = RadiusID, pwe = PWE,
		element_s = ElementS, scalar_s = ScalarS, authenticator = RequestAuthenticator, secret = Secret,
		grp_dec = GrpDec, rand_func = RandomFunc, prf = PRF, s_rand = S_rand} = StateData)->
	try 
		#eap_packet{code = ?Response, identifier = EapID, data = Data} = ocs_eap_codec:eap_packet(EAPPacket),
		EAPHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{type = ?PWD, pwd_exch = commit, data = BodyData} = EAPHeader,
		Body = ocs_eap_codec:eap_pwd_commit(BodyData),
		#eap_pwd_commit{element = ElementP, scalar = ScalarP} = Body,
		NewStateData = StateData#statedata{scalar_p = ScalarP, element_p = ElementP},
		case 	wait_for_commit1(BodyData, NewStateData) of
			ok ->
				Ks = ocs_eap_pwd:compute_ks(<<S_rand:256>>, PWE, ScalarP, ElementP),
				Input = [<<Ks/binary, ElementS/binary, ScalarS/binary, ElementP/binary,
						ScalarP/binary, GrpDec:16, RandomFunc, PRF >>],
				ConfirmS = ocs_eap_pwd:h(Input),
				ConfirmBody = #eap_pwd_confirm{confirm = ConfirmS},
				ConfirmBodyData = ocs_eap_codec:eap_pwd_confirm(ConfirmBody),
				ConfirmHeader = #eap_pwd{type = ?PWD, length = false,
						more = false, pwd_exch = confirm, data = ConfirmBodyData},
				ConfirmEAPData = ocs_eap_codec:eap_pwd(ConfirmHeader),
				NewEAPID = EapID + 1,
				send_radius_response(?Request, NewEAPID, ConfirmEAPData, ?AccessChallenge,
						RadiusID, RequestAuthenticator, Secret, RadiusFsm),
				NewStateData1 = NewStateData#statedata{eap_id = NewEAPID, ks = Ks,
						confirm_s = ConfirmS, radius_fsm = RadiusFsm},
				{next_state, wait_for_confirm, NewStateData1, ?TIMEOUT};
			{error,exit} ->
				{next_state, wait_for_commit, NewStateData,0}
		end
	catch
		_:_ ->
			radius:response(RadiusFsm, {error, ignore}),
			{next_state, wait_for_commit, StateData,0}
	end.
%% @hidden
wait_for_commit1(BodyData, #statedata{scalar_s = ScalarS, element_s = ElementS,
		radius_fsm = RadiusFsm, radius_id = RadiusID, eap_id = NewEAPID,
		secret = Secret, authenticator = RequestAuthenticator} = StateData) ->
		ExpectedSize = size(<<ElementS/binary, ScalarS/binary>>),
		case size(BodyData) of 
			ExpectedSize ->
				wait_for_commit2(BodyData, StateData);
			_ ->
				send_radius_response(?Failure, NewEAPID, BodyData, ?AccessReject,
						RadiusID, RequestAuthenticator, Secret, RadiusFsm),
				{error, exit}
		end.
%% @hidden
wait_for_commit2(BodyData, #statedata{element_p = ElementP, scalar_p = ScalarP, scalar_s = ScalarS,
		element_s = ElementS,  radius_fsm = RadiusFsm, radius_id = RadiusID,
		secret = Secret, authenticator = RequestAuthenticator, eap_id = NewEAPID} = StateData) ->
	case {ElementP, ScalarP} of
		{ElementS, ScalarS} ->
			send_radius_response(?Failure, NewEAPID, BodyData, ?AccessReject,
					RadiusID, RequestAuthenticator, Secret, RadiusFsm),
			{error, exit};
		_ ->
			wait_for_commit3(BodyData, StateData)
	end.
%% @hidden
wait_for_commit3(BodyData, #statedata{scalar_p = ScalarP, radius_fsm = RadiusFsm, radius_id = RadiusID,
		secret = Secret, authenticator = RequestAuthenticator, eap_id = NewEAPID} = _StateData)->
	case ScalarP of
		_ScalarP_Valid when  1 =< ScalarP, ScalarP >= $R ->
			ok;
		_ScalarP_Out_of_Range ->
			send_radius_response(?Failure, NewEAPID, BodyData, ?AccessReject, RadiusID,
					RequestAuthenticator, Secret, RadiusFsm),
			{error, exit}
	end.

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
wait_for_confirm(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
wait_for_confirm({eap_response, RadiusFsm, EAPPacket}, #statedata{radius_id = RadiusID,
		authenticator = RequestAuthenticator, secret = Secret, peer_id = PeerID, token = Token,
		confirm_s = ConfirmS, element_s = ElementS, scalar_s = ScalarS, ks = Ks,
		eap_id = EapID} = StateData)->
	try
		EAPData = ocs_eap_codec:eap_packet(EAPPacket),
		#eap_packet{code = ?Response, identifier = EapID, data = Data} = EAPData,
		EAPHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{type = ?PWD, pwd_exch = confirm, data = BodyData} = EAPHeader,
		Body = ocs_eap_codec:eap_pwd_confirm(BodyData),
		#eap_pwd_confirm{confirm = ConfirmP} = Body,
		case 	wait_for_confirm1(BodyData, StateData) of
			ok ->
				{ok, HostName_s} = inet:gethostname(),
				HostName = list_to_binary(HostName_s),
				P_rand = crypto:rand_uniform(1, ?R),
				P_rand_bin = <<P_rand:256>>,
				PeerID_bin = list_to_binary(PeerID),
				P_pwe = ocs_eap_pwd:compute_pwe(Token, PeerID_bin, HostName, Secret),
				Kp = ocs_eap_pwd:compute_ks(P_rand_bin, P_pwe, ScalarS, ElementS),  
				MK_S = ocs_eap_pwd:h([Ks, ConfirmP, ConfirmS]),
				MK_P = ocs_eap_pwd:h([Kp, ConfirmP, ConfirmS]),
					case MK_S of
						MK_P ->
							NewEapID = EapID + 1,
							send_radius_response(?Success, NewEapID, <<>>, ?AccessAccept, RadiusID,
									RequestAuthenticator, Secret, RadiusFsm),
							{next_state, wait_for_confirm, StateData, ?TIMEOUT};
						_ ->
							NewEapID = EapID + 1,
							send_radius_response(?Failure, NewEapID, <<>>, ?AccessReject, RadiusID,
									RequestAuthenticator, Secret, RadiusFsm),
							{next_state, wait_for_confirm, StateData, 0}
				end;
			{error,exit} ->
				{next_state, wait_for_confirm, StateData,0}
		end
	catch
		_:_ ->
			radius:response(RadiusFsm, {error, ignore}),
			{next_state, wait_for_confirm, StateData,0}
	end.
wait_for_confirm1(BodyData, #statedata{radius_fsm = RadiusFsm, radius_id = RadiusID,
		secret = Secret, authenticator = RequestAuthenticator, confirm_s = ConfirmS,
		eap_id = NewEAPID} = _StateData) ->
		ExpectedSize = size(<<ConfirmS/binary>>),
		case size(BodyData) of 
			ExpectedSize ->
				ok;
			_ ->
				send_radius_response(?Failure, NewEAPID, BodyData, ?AccessReject, RadiusID,
						RequestAuthenticator, Secret, RadiusFsm),
				{error, exit}
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

-spec send_radius_response(EAPCode :: integer(), NewEAPID :: byte(), EAPData :: binary(),
		RadiusCode :: integer(), RadiusID :: byte(), RequestAuthenticator :: binary(),
		Secret :: binary(), RadiusFsm :: pid()) -> ok.
%% @doc Sends an RADIUS-Access/Challenge or Reject or Accept  packet to peer
%% @hidden
send_radius_response(EAPCode, NewEAPID, EAPData, RadiusCode, RadiusID, RequestAuthenticator, Secret, RadiusFsm) ->
	Packet = #eap_packet{code = EAPCode, identifier = NewEAPID, data = EAPData},
	EAPPacketData = ocs_eap_codec:eap_packet(Packet),
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?EAPMessage,
			EAPPacketData, AttributeList0),
	AttributeList2 = radius_attributes:store(?MessageAuthenticator,
		<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, AttributeList1),
	AttrList2bin = radius_attributes:codec(AttributeList2),
	Response1 = #radius{code = RadiusCode, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = AttrList2bin},
	ResponsePacket1 = radius:codec(Response1),
	MessageAuthenticator = crypto:hmac(md5, Secret, ResponsePacket1),
	AttributeList3 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttributeList1),
	AttributeData = radius_attributes:codec(AttributeList3),
	Length = size(AttributeData) + 20,
	ResponseAuthenticator = crypto:hash(md5,[<<?AccessChallenge, RadiusID,
			Length:16>>, RequestAuthenticator, AttributeData, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = AttributeData},
	ResponsePacket = radius:codec(Response),
	radius:response(RadiusFsm, {response, ResponsePacket}),
	ok.
