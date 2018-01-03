%%% ocs_eap_pwd_fsm.erl
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
%%% @reference <a href="http://tools.ietf.org/rfc/rfc3579.txt">
%%% 	RFC3579 - RADIUS Support For EAP</a>
%%%
-module(ocs_eap_pwd_fsm).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_pwd_fsm API
-export([]).

%% export the ocs_eap_pwd_fsm state callbacks
-export([eap_start/2, id/2, commit/2, confirm/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include("../include/diameter_gen_eap_application_rfc4072.hrl").
-include("../include/diameter_gen_nas_application_rfc7155.hrl").

-define(CC_APPLICATION_ID, 4).

-record(statedata,
		{server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		radius_fsm :: undefined | pid(),
		session_id:: string() | {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		start :: undefined | #diameter_eap_app_DER{} | #radius{},
		eap_id = 0 :: byte(),
		group_desc  = 19 :: byte(),
		rand_func = 1 :: byte(),
		prf = 1 :: byte(),
		secret :: undefined | binary(),
		token :: undefined | binary(),
		password :: undefined | binary(),
		prep :: undefined | none | rfc2759 | saslprep,
		server_id  ::  binary(),
		peer_id :: undefined | binary(),
		pwe :: undefined | binary(),
		s_rand :: undefined | integer(),
		scalar_s :: undefined | binary(),
		element_s :: undefined | binary(),
		scalar_p :: undefined | binary(),
		element_p :: undefined | binary(),
		ks :: undefined | binary(),
		confirm_s :: undefined | binary(),
		confirm_p :: undefined | binary(),
		mk :: undefined | binary(),
		msk :: undefined | binary(),
		auth_app_id :: undefined | integer(),
		auth_req_type :: undefined | integer(),
		origin_host :: undefined | binary(),
		origin_realm :: undefined | binary(),
		diameter_port_server :: undefined | pid(),
		password_required :: boolean(),
		service_type :: undefined | integer()}).

-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).

%%----------------------------------------------------------------------
%%  The ocs_eap_pwd_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_pwd_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData} | {ok, StateName, StateData, Timeout}
		| {ok, StateName, StateData, hibernate} | {stop, Reason} | ignore,
		StateName ::atom(),
		StateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, SessionID, AccessRequest] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
	StateData = #statedata{server_address = ServerAddress,
			server_port = ServerPort, client_address = ClientAddress,
			client_port = ClientPort, radius_fsm = RadiusFsm,
			secret = Secret, session_id = SessionID,
			server_id = list_to_binary(Hostname), start = AccessRequest,
			password_required = PasswordReq},
	process_flag(trap_exit, true),
	{ok, eap_start, StateData, 0};
init([diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, SessionId, ApplicationId, AuthType, OHost, ORealm,
		_DHost, _DRealm, Request, _Options] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
	case global:whereis_name({ocs_diameter_auth, ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			ServiceType = case Request of
				#diameter_nas_app_AAR{'Service-Type' = [ST]} ->
					ST;
				_ ->
					undefined
			end,
			StateData = #statedata{server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionId,
					server_id = list_to_binary(Hostname), auth_app_id = ApplicationId,
					auth_req_type = AuthType, origin_host = OHost,
					origin_realm = ORealm, diameter_port_server = PortServer,
					start = Request, password_required = PasswordReq,
					service_type = ServiceType},
			process_flag(trap_exit, true),
			{ok, eap_start, StateData, 0}
		end.

-spec eap_start(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
				| {next_state, NextStateName, NewStateData, Timeout}
				| {next_state, NextStateName, NewStateData, hibernate}
				| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>eap_start</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
eap_start(timeout, #statedata{start = Request} = StateData) ->
	try crypto:strong_rand_bytes(4) of
		Token ->
			case Request of
				#diameter_eap_app_DER{} ->
					eap_start1(Token, StateData);
				_ ->
					eap_start2(Token, StateData)
			end
	catch
		Reason ->
			{stop, Reason, StateData}
	end.
%% @hidden
eap_start1(Token, #statedata{eap_id = EapID, session_id = SessionId,
		server_id = ServerID, group_desc = GroupDesc, rand_func = RandFunc,
		prf = PRF, auth_req_type = AuthType, start = Request,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer} =
		StateData) ->
	EapPwdId = #eap_pwd_id{group_desc = GroupDesc, random_fun = RandFunc,
			prf = PRF, token = Token, pwd_prep = none, identity = ServerID},
	EapPwdData = ocs_eap_codec:eap_pwd_id(EapPwdId),
	EapPwd = #eap_pwd{pwd_exch = id, data = EapPwdData},
	EapData = ocs_eap_codec:eap_pwd(EapPwd),
	case Request#diameter_eap_app_DER.'EAP-Payload' of
		[] ->
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = EapID, data = EapData},
			send_diameter_response(SessionId, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
					EapPacket, PortServer, Request, StateData),
			{next_state, id, StateData, ?TIMEOUT};
		EapMessage ->
			case catch ocs_eap_codec:eap_packet(EapMessage) of
				#eap_packet{code = response, type = ?Identity,
						identifier = NewEapID} ->
					NextEapID = (NewEapID rem 255) + 1,
					EapPacket = #eap_packet{code = request, type = ?PWD,
							identifier = NextEapID, data = EapData},
					NewStateData = StateData#statedata{eap_id = NextEapID},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
							EapPacket, PortServer, Request, StateData),
					{next_state, id, NewStateData, ?TIMEOUT};
				#eap_packet{code = request, identifier = NewEapID} ->
					EapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
							EapPacket, PortServer, Request, StateData),
					{stop, {shutdown, SessionId}, StateData};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = Data} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionId}, {code, Code},
							{type, EapType}, {identifier, NewEapID}, {data, Data}]),
					EapPacket = #eap_packet{code = failure, identifier = NewEapID},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
							EapPacket, PortServer, Request, StateData),
					{stop, {shutdown, SessionId}, StateData};
				{'EXIT', _Reason} ->
					EapPacket = #eap_packet{code = failure, identifier = EapID},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
							EapPacket, PortServer, Request, StateData),
					{stop, {shutdown, SessionId}, StateData}
			end
	end.
%% @hidden
eap_start2(Token, #statedata{eap_id = EapID,
		start = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = RequestAttributes},
		session_id = SessionID, server_id = ServerID, group_desc = GroupDesc,
		rand_func = RandFunc, prf = PRF} = StateData) ->
	EapPwdId = #eap_pwd_id{group_desc = GroupDesc,
			random_fun = RandFunc, prf = PRF, token = Token,
			pwd_prep = none, identity = ServerID},
	EapPwdData = ocs_eap_codec:eap_pwd_id(EapPwdId),
	EapPwd = #eap_pwd{pwd_exch = id, data = EapPwdData},
	EapData = ocs_eap_codec:eap_pwd(EapPwd),
	NewStateData = StateData#statedata{token = Token, start = undefined},
	case radius_attributes:find(?EAPMessage, RequestAttributes) of
		{ok, <<>>} ->
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{next_state, id, NewStateData, ?TIMEOUT};
		{ok, EAPMessage} ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response, type = ?Identity,
						identifier = NewEapID} ->
					NextEapID = (NewEapID rem 255) + 1,
					EapPacket = #eap_packet{code = request,
							type = ?PWD, identifier = NextEapID, data = EapData},
					send_response(EapPacket, ?AccessChallenge, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					NextStateData = NewStateData#statedata{eap_id = NextEapID},
					{next_state, id, NextStateData, ?TIMEOUT};
				#eap_packet{code = request, identifier = NewEapID} ->
					EapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_response(EapPacket, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionID}, StateData};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = Data} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID}, {code, Code},
							{type, EapType}, {identifier, NewEapID}, {data, Data}]),
					EapPacket = #eap_packet{code = failure, identifier = NewEapID},
					send_response(EapPacket, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionID}, StateData};
				{'EXIT', _Reason} ->
					EapPacket = #eap_packet{code = failure, identifier = EapID},
					send_response(EapPacket, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionID}, StateData}
			end;
		{error, not_found} ->
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{next_state, id, NewStateData, ?TIMEOUT}
	end.

-spec id(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
				| {next_state, NextStateName, NewStateData, Timeout}
				| {next_state, NextStateName, NewStateData, hibernate} 
				| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>id</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
id(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
id({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest, RadiusFsm},
		#statedata{eap_id = EapID, group_desc = GroupDesc, rand_func = RandFunc,
		prf = PRF, session_id = SessionID} = StateData) ->
	NewStateData = StateData#statedata{radius_fsm = RadiusFsm},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?PWD, identifier = EapID,
					data = Data} -> 
				#eap_pwd{pwd_exch = id,
						data = EapPwdId} = ocs_eap_codec:eap_pwd(Data),
				#eap_pwd_id{group_desc = GroupDesc, random_fun = RandFunc,
						prf = PRF, token = Token, pwd_prep = none,
						identity = PeerID} = ocs_eap_codec:eap_pwd_id(EapPwdId),
				id1(AccessRequest, PeerID, Token, NewStateData);
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionID}, NewStateData}
		end
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
id(#diameter_eap_app_DER{} = Request, #statedata{eap_id = EapID,
		group_desc = GroupDesc, rand_func = RandFunc, prf = PRF,
		session_id = SessionID, auth_req_type = RequestType,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer} = StateData) ->
	try
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?PWD, identifier = EapID,
					data = Data} -> 
				#eap_pwd{pwd_exch = id,
						data = EapPwdId} = ocs_eap_codec:eap_pwd(Data),
				#eap_pwd_id{group_desc = GroupDesc, random_fun = RandFunc,
						prf = PRF, token = Token, pwd_prep = none,
						identity = PeerID} = ocs_eap_codec:eap_pwd_id(EapPwdId),
				id2(Request, PeerID, Token, StateData);
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				send_diameter_response(SessionID, RequestType,
					?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS', OHost, ORealm,
					none, PortServer, Request, StateData),
				{stop, {shutdown, SessionID}, StateData}
		end
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, RequestType,
					?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS', OHost, ORealm,
					EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
id1(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes}, PeerID, Token,
		#statedata{eap_id = EapID, server_id = ServerID,
		session_id = SessionID, password_required = PwdReq} = StateData) ->
	try
		S_rand = crypto:rand_uniform(1, ?R),
		NewEapID = (EapID rem 255) + 1,
		case catch ocs:find_subscriber(PeerID) of
			{ok, #subscriber{password = Pwd}} ->
				Password = case PwdReq of
					false ->
						<<>>;
					_ ->
						Pwd
				end,
				PWE = ocs_eap_pwd:compute_pwe(Token, PeerID, ServerID, Password),
				{ScalarS, ElementS} = ocs_eap_pwd:compute_scalar(<<S_rand:256>>,
						PWE),
				CommitReq = #eap_pwd_commit{scalar = ScalarS, element = ElementS},
				CommitReqBody = ocs_eap_codec:eap_pwd_commit(CommitReq),
				CommitReqHeader = #eap_pwd{length = false,
					more = false, pwd_exch = commit, data = CommitReqBody},
				CommitEapData = ocs_eap_codec:eap_pwd(CommitReqHeader),
				EapPacket = #eap_packet{code = request,
						type = ?PWD, identifier = NewEapID, data = CommitEapData},
				NewStateData = StateData#statedata{pwe = PWE, s_rand = S_rand,
					peer_id = PeerID, eap_id = NewEapID, scalar_s = ScalarS,
					element_s = ElementS, password = Pwd},
				send_response(EapPacket, ?AccessChallenge, [], RadiusID,
						RequestAuthenticator, RequestAttributes, NewStateData),
				{next_state, commit, NewStateData, ?TIMEOUT};
			{error, _Reason} ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				send_response(EapPacket1, ?AccessReject, [], RadiusID,
						RequestAuthenticator, RequestAttributes, StateData),
				{stop, {shutdown, SessionID}, StateData}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket2, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
id2(#diameter_eap_app_DER{} = Request, PeerID, Token,
		#statedata{eap_id = EapID, server_id = ServerID, session_id = SessionID,
		auth_req_type = AuthType, origin_host = OH, origin_realm = OR,
		diameter_port_server = PortServer, password_required = PwdReq} = StateData) ->
	try
		S_rand = crypto:rand_uniform(1, ?R),
		NewEapID = (EapID rem 255) + 1,
		case catch ocs:find_subscriber(PeerID) of
			{ok, #subscriber{password = Pwd}} ->
				Password = case PwdReq of
					false ->
						<<>>;
					_ ->
						Pwd
				end,
				PWE = ocs_eap_pwd:compute_pwe(Token, PeerID, ServerID, Password),
				{ScalarS, ElementS} = ocs_eap_pwd:compute_scalar(<<S_rand:256>>,
						PWE),
				CommitReq = #eap_pwd_commit{scalar = ScalarS, element = ElementS},
				CommitReqBody = ocs_eap_codec:eap_pwd_commit(CommitReq),
				CommitReqHeader = #eap_pwd{length = false,
					more = false, pwd_exch = commit, data = CommitReqBody},
				CommitEapData = ocs_eap_codec:eap_pwd(CommitReqHeader),
				EapPacket = #eap_packet{code = request,
						type = ?PWD, identifier = NewEapID, data = CommitEapData},
				NewStateData = StateData#statedata{pwe = PWE, s_rand = S_rand,
					peer_id = PeerID, eap_id = NewEapID, scalar_s = ScalarS,
					element_s = ElementS, password = Pwd},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
						EapPacket, PortServer, Request, StateData),
				{next_state, commit, NewStateData, ?TIMEOUT};
			{error, _Reason} ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
						EapPacket1, PortServer, Request, StateData),
				{stop, {shutdown, SessionID}, StateData}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					EapPacket2, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.


-spec commit(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>commit</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
commit(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
commit({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionID} = StateData) ->
	NewStateData = StateData#statedata{radius_fsm = RadiusFsm},
	try 
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		EapPacket1 = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = Data} = EapPacket1,
		EapHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{pwd_exch = commit, data = BodyData} = EapHeader,
		Body = ocs_eap_codec:eap_pwd_commit(BodyData),
		#eap_pwd_commit{element = ElementP, scalar = ScalarP} = Body,
		NextStateData = NewStateData#statedata{scalar_p = ScalarP,
				element_p = ElementP},
		commit1(AccessRequest, BodyData, NextStateData)
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket2, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
commit(#diameter_eap_app_DER{} = Request, #statedata{eap_id = EapID,
		session_id = SessionID, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR, diameter_port_server = PortServer} = StateData) ->
	try 
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		EapPacket1 = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = Data} = EapPacket1,
		EapHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{pwd_exch = commit, data = BodyData} = EapHeader,
		Body = ocs_eap_codec:eap_pwd_commit(BodyData),
		#eap_pwd_commit{element = ElementP, scalar = ScalarP} = Body,
		NextStateData = StateData#statedata{scalar_p = ScalarP,
				element_p = ElementP},
		commit5(Request, BodyData, NextStateData)
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket2, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
commit1(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest, BodyData,
		#statedata{eap_id = EapID, scalar_s = ScalarS, element_s = ElementS,
		session_id = SessionID} = StateData) ->
	ExpectedSize = size(<<ElementS/binary, ScalarS/binary>>),
	case size(BodyData) of 
		ExpectedSize ->
			commit2(AccessRequest, StateData);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
commit2(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest,
		#statedata{element_p = ElementP, scalar_p = ScalarP,
		scalar_s = ScalarS, element_s = ElementS,
		eap_id = EapID, session_id = SessionID} = StateData) ->
	case {ElementP, ScalarP} of
		{ElementS, ScalarS} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		_ ->
			commit3(AccessRequest, StateData)
	end.
%% @hidden
commit3(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest,
		#statedata{scalar_p = ScalarP, eap_id = EapID,
		session_id = SessionID} = StateData)->
	case ScalarP of
		<<ScalarP_Valid:256>> when 1 < ScalarP_Valid, ScalarP_Valid < ?R ->
			commit4(AccessRequest, StateData);
		_ScalarP_Out_of_Range ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
commit4(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = _AccessRequest,
		#statedata{eap_id = EapID, session_id = SessionID, scalar_p = ScalarP,
		element_p = ElementP, scalar_s = ScalarS, element_s = ElementS,
		s_rand = Srand, pwe = PWE, group_desc = GroupDesc,
		rand_func = RandFunc, prf = PRF} = StateData)->
	case catch ocs_eap_pwd:compute_ks(<<Srand:256>>,
			PWE, ScalarP, ElementP) of
		{'EXIT', _Reason} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		Ks ->
			Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
			Input = [Ks, ElementS, ScalarS, ElementP, ScalarP, Ciphersuite],
			ConfirmS = ocs_eap_pwd:h(Input),
			ConfirmHeader = #eap_pwd{length = false,
					more = false, pwd_exch = confirm, data = ConfirmS},
			ConfirmEapData = ocs_eap_codec:eap_pwd(ConfirmHeader),
			NewEapID = (EapID rem 255) + 1,
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = NewEapID, data = ConfirmEapData},
			send_response(EapPacket, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			NewStateData = StateData#statedata{eap_id = NewEapID, ks = Ks,
					confirm_s = ConfirmS},
			{next_state, confirm, NewStateData, ?TIMEOUT}
	end.
%% @hidden
commit5(Request, BodyData, #statedata{eap_id = EapID, scalar_s = ScalarS,
		element_s = ElementS, session_id = SessionID, auth_req_type = AuthType,
		origin_host = OH, origin_realm = OR,
		diameter_port_server = PortServer} = StateData) ->
	ExpectedSize = size(<<ElementS/binary, ScalarS/binary>>),
	case size(BodyData) of 
		ExpectedSize ->
			commit6(Request, StateData);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
commit6(Request, #statedata{element_p = ElementP, scalar_p = ScalarP,
		scalar_s = ScalarS, element_s = ElementS, eap_id = EapID,
		session_id = SessionID, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR, diameter_port_server = PortServer} = StateData) ->
	case {ElementP, ScalarP} of
		{ElementS, ScalarS} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData};
		_ ->
			commit7(Request, StateData)
	end.
%% @hidden
commit7(Request, #statedata{scalar_p = ScalarP, eap_id = EapID,
		session_id = SessionID, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR,
	diameter_port_server = PortServer} = StateData)->
	case ScalarP of
		<<ScalarP_Valid:256>> when 1 < ScalarP_Valid, ScalarP_Valid < ?R ->
			commit8(Request, StateData);
		_ScalarP_Out_of_Range ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
commit8(Request, #statedata{eap_id = EapID, session_id = SessionID,
		scalar_p = ScalarP, element_p = ElementP, scalar_s = ScalarS,
		element_s = ElementS, s_rand = Srand, pwe = PWE, group_desc = GroupDesc,
		rand_func = RandFunc, prf = PRF, auth_req_type = AuthType,
		origin_host = OH, origin_realm = OR,
		diameter_port_server = PortServer} = StateData)->
	case catch ocs_eap_pwd:compute_ks(<<Srand:256>>,
			PWE, ScalarP, ElementP) of
		{'EXIT', _Reason} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData};
		Ks ->
			Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
			Input = [Ks, ElementS, ScalarS, ElementP, ScalarP, Ciphersuite],
			ConfirmS = ocs_eap_pwd:h(Input),
			ConfirmHeader = #eap_pwd{length = false,
					more = false, pwd_exch = confirm, data = ConfirmS},
			ConfirmEapData = ocs_eap_codec:eap_pwd(ConfirmHeader),
			NewEapID = (EapID rem 255) + 1,
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = NewEapID, data = ConfirmEapData},
			NewStateData = StateData#statedata{eap_id = NewEapID, ks = Ks,
					confirm_s = ConfirmS},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{next_state, confirm, NewStateData, ?TIMEOUT}
	end.

-spec confirm(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>confirm</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
confirm(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
confirm({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest, RadiusFsm},
		#statedata{session_id = SessionID, eap_id = EapID} = StateData)->
	NewStateData = StateData#statedata{radius_fsm = RadiusFsm},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		EapData = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = Data} = EapData,
		EapHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{pwd_exch = confirm, data = ConfirmP} = EapHeader,
		NextStateData = NewStateData#statedata{confirm_p = ConfirmP},
		confirm1(AccessRequest, NextStateData)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
confirm(#diameter_eap_app_DER{} = Request, #statedata{eap_id = EapID,
		session_id = SessionID, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR, diameter_port_server = PortServer} = StateData) ->
	try
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		EapData = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = Data} = EapData,
		EapHeader = ocs_eap_codec:eap_pwd(Data),
		#eap_pwd{pwd_exch = confirm, data = ConfirmP} = EapHeader,
		NewStateData = StateData#statedata{confirm_p = ConfirmP},
		confirm4(Request, NewStateData)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
confirm1(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest,
		#statedata{confirm_s = ConfirmS, eap_id = EapID,
		confirm_p = ConfirmP, session_id = SessionID} = StateData) ->
	ExpectedSize = size(ConfirmS),
	case size(ConfirmP) of 
		ExpectedSize ->
			confirm2(AccessRequest, StateData);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
confirm2(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest,
		#statedata{eap_id = EapID, ks = Ks, confirm_p = ConfirmP,
		scalar_s = ScalarS, element_s = ElementS, scalar_p = ScalarP,
		element_p = ElementP, group_desc = GroupDesc, rand_func = RandFunc,
		prf = PRF, session_id = SessionID} = StateData) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	case ocs_eap_pwd:h([Ks, ElementP, ScalarP,
			ElementS, ScalarS, Ciphersuite]) of
		ConfirmP ->
			confirm3(AccessRequest, StateData);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
confirm3(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = _AccessRequest,
		#statedata{secret = Secret, eap_id = EapID, ks = Ks,
		confirm_p = ConfirmP, confirm_s = ConfirmS, scalar_s = ScalarS,
		scalar_p = ScalarP, group_desc = GroupDesc, rand_func = RandFunc,
		prf = PRF, session_id = SessionID, peer_id = PeerID,
		password = Password} = StateData) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	MK = ocs_eap_pwd:h([Ks, ConfirmP, ConfirmS]),
	MethodID = ocs_eap_pwd:h([Ciphersuite, ScalarP, ScalarS]),
	<<MSK:64/binary, _EMSK:64/binary>> = ocs_eap_pwd:kdf(MK,
			<<?PWD, MethodID/binary>>, 128),
	Salt = crypto:rand_uniform(16#8000, 16#ffff),
	MsMppeKey = encrypt_key(Secret, RequestAuthenticator, Salt, MSK),
	UserName = binary_to_list(PeerID),
	ServiceType = case radius_attributes:find(?ServiceType, RequestAttributes) of
		{error, not_found} ->
			undefined;
		{_, ST} ->
			ST
	end,
	Destination = proplists:get_value(?CalledStationId, RequestAttributes, ""),
	SessionAttributes = extract_session_attributes(RequestAttributes),
	case ocs_rating:authorize(radius, ServiceType,
			PeerID, Password, Destination, SessionAttributes) of
		{authorized, _Subscriber, Attributes, _ExistingSessionAttributes} ->
			Attr1 = radius_attributes:store(?UserName, UserName, Attributes),
			Attr2 = radius_attributes:store(?Microsoft,
					?MsMppeSendKey, {Salt, MsMppeKey}, Attr1),
			Attr3 = radius_attributes:store(?Microsoft,
					?MsMppeRecvKey, {Salt, MsMppeKey}, Attr2),
			EapPacket = #eap_packet{code = success, identifier = EapID},
			send_response(EapPacket, ?AccessAccept, Attr3, RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData#statedata{mk = MK, msk = MSK}};
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(radius, ExistingSessionAttributes, StateData),
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		{unauthorized, _Reason, _ExistingSessionAttributes} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
confirm4(Request, #statedata{confirm_s = ConfirmS, eap_id = EapID,
		confirm_p = ConfirmP, session_id = SessionID, auth_req_type = AuthType,
		origin_host = OH, origin_realm = OR, diameter_port_server = PortServer} =
		 StateData) ->
	ExpectedSize = size(ConfirmS),
	case size(ConfirmP) of 
		ExpectedSize ->
			confirm5(Request, StateData);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
confirm5(Request, #statedata{eap_id = EapID, ks = Ks, confirm_p = ConfirmP,
		scalar_s = ScalarS, element_s = ElementS, scalar_p = ScalarP,
		element_p = ElementP, group_desc = GroupDesc, rand_func = RandFunc,
		prf = PRF, session_id = SessionID, auth_req_type = AuthType,
		origin_host = OH, origin_realm = OR, diameter_port_server = PortServer}=
		 StateData) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	case ocs_eap_pwd:h([Ks, ElementP, ScalarP,
			ElementS, ScalarS, Ciphersuite]) of
		ConfirmP ->
			confirm6(Request, StateData);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
confirm6(Request, #statedata{eap_id = EapID, ks = Ks, confirm_p = ConfirmP,
		confirm_s = ConfirmS, scalar_s = ScalarS, scalar_p = ScalarP,
		group_desc = GroupDesc, rand_func = RandFunc, prf = PRF,
		session_id = SessionID, peer_id = PeerID, password = Password,
		auth_req_type = AuthType, origin_host = OH, origin_realm = OR,
		diameter_port_server = PortServer, service_type = ServiceType} = StateData) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	MK = ocs_eap_pwd:h([Ks, ConfirmP, ConfirmS]),
	MethodID = ocs_eap_pwd:h([Ciphersuite, ScalarP, ScalarS]),
	<<MSK:64/binary, _EMSK:64/binary>> = ocs_eap_pwd:kdf(MK,
			<<?PWD, MethodID/binary>>, 128),
	SessionAttributes = [{'Origin-Host', OH}, {'Origin-Realm', OR},
			{'Session-Id', SessionID}],
	case ocs_rating:authorize(diameter, ServiceType,
			PeerID, Password, undefined, SessionAttributes) of
		{authorized, _Subscriber, _Attributes, _ExistingSessionAttributes} ->
			EapPacket = #eap_packet{code = success, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OH, OR, EapPacket,
					 PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData#statedata{mk = MK, msk = MSK}};
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(diameter, ExistingSessionAttributes, StateData),
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData};
		{unauthorized, _Reason, _ExistingSessionAttributes} ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					 ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					 EapPacket1, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.			

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid, Tag},
		Pid :: pid(),
		Tag :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {reply, Reply, NextStateName, NewStateData}
		| {reply, Reply, NextStateName, NewStateData, Timeout}
		| {reply, Reply, NextStateName, NewStateData, hibernate}
		| {next_state, NextStateName, NewStateData }
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, Reply, NewStateData} | {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec terminate(Reason, StateName, StateData) -> any()
	when
		Reason :: normal | shutdown | term(),
		StateName :: atom(),
		StateData :: statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason, _StateName, _StateData) ->
	ok.

-spec code_change(OldVsn, StateName, StateData, Extra) -> Result
	when
		OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(),
		StateData :: statedata(),
		Extra :: term(),
		Result :: {ok, NextStateName, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec send_response(EapPacket, RadiusCode, ResponseAttributes, RadiusID,
		RequestAuthenticator, RequestAttributes, StateData) -> ok
	when
		EapPacket :: #eap_packet{},
		RadiusCode :: integer(), 
		ResponseAttributes :: radius_attributes:attributes(),
		RadiusID :: integer(),
		RequestAuthenticator :: [byte()],
		RequestAttributes :: radius_attributes:attributes(),
		StateData :: #statedata{}.
%% @doc Sends an RADIUS-Access/Challenge or Reject or Accept packet to peer.
%% @hidden
send_response(EapPacket, RadiusCode, ResponseAttributes,
		RadiusID, RequestAuthenticator, RequestAttributes,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		secret = Secret, radius_fsm = RadiusFsm} = _StateData) ->
	EapPacketData = ocs_eap_codec:eap_packet(EapPacket),
	AttrList1 = radius_attributes:add(?EAPMessage,
			EapPacketData, ResponseAttributes),
	AttrList2 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, AttrList1),
	Attributes1 = radius_attributes:codec(AttrList2),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = crypto:hmac(md5, Secret, [<<RadiusCode, RadiusID,
			Length:16>>, RequestAuthenticator, Attributes1]),
	AttrList3 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttrList2),
	Attributes2 = radius_attributes:codec(AttrList3),
	ResponseAuthenticator = crypto:hash(md5, [<<RadiusCode, RadiusID,
			Length:16>>, RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	ResponsePacket = radius:codec(Response),
	case RadiusCode of
		?AccessAccept ->
			ok = ocs_log:auth_log(radius, {ServerAddress, ServerPort},
					{ClientAddress, ClientPort}, accept,
					RequestAttributes, AttrList3);
		?AccessReject ->
			ok = ocs_log:auth_log(radius, {ServerAddress, ServerPort},
					{ClientAddress, ClientPort}, reject,
					RequestAttributes, AttrList3);
		?AccessChallenge ->
			ok
	end,
	radius:response(RadiusFsm, {response, ResponsePacket}).

-spec encrypt_key(Secret, RequestAuthenticator, Salt, Key) -> Ciphertext
	when
		Secret :: binary(),
		RequestAuthenticator :: [byte()],
		Salt :: integer(),
		Key :: binary(),
		Ciphertext :: binary().
%% @doc Encrypt the Pairwise Master Key (PMK) according to RFC2548
%% 	section 2.4.2 for use as String in a MS-MPPE-Send-Key attribute.
%% @private
encrypt_key(Secret, RequestAuthenticator, Salt, Key)
		when (Salt bsr 15) == 1 ->
	KeyLength = size(Key),
	Plaintext = case (KeyLength + 1) rem 16 of
		0 ->
			<<KeyLength, Key/binary>>;
		N ->
			PadLength = (16 - N) * 8,
			<<KeyLength, Key/binary, 0:PadLength>>
	end,
	F = fun(P, [H | _] = Acc) ->
				B = crypto:hash(md5, [Secret, H]),
				C = crypto:exor(P, B),
				[C | Acc]
	end,
	AccIn = [[RequestAuthenticator, <<Salt:16>>]],
	AccOut = lists:foldl(F, AccIn, [P || <<P:16/binary>> <= Plaintext]),
	iolist_to_binary(tl(lists:reverse(AccOut))).

-spec send_diameter_response(SId, AuthType, ResultCode, OH, OR,
		EapPacket, PortServer, Request, StateData) -> ok
	when
		SId :: string(), 
		AuthType :: integer(),
		ResultCode :: integer(),
		OH :: binary(),
		OR :: binary(),
		EapPacket :: none | #eap_packet{},
		PortServer :: pid(),
		Request :: #diameter_eap_app_DER{},
		StateData :: #statedata{}.
%% @doc Log DIAMETER event and send appropriate DIAMETER answer to
%% ocs_diameter_auth_port_server.
%% @hidden
send_diameter_response(SId, AuthType, ResultCode, OH, OR, none,
		PortServer, Request, #statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _StateData) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
			'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
			'Result-Code' = ResultCode, 'Origin-Host' = OH, 'Origin-Realm' = OR},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SId, AuthType, ResultCode, OH, OR, EapPacket,
		PortServer, Request, #statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _StateData) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	try
		EapData = ocs_eap_codec:eap_packet(EapPacket),
		Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
				'Result-Code' = ResultCode, 'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapData]},
		ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
		gen_server:cast(PortServer, {self(), Answer})
	catch
		_:_ ->
		Answer1 = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				'Origin-Host' = OH, 'Origin-Realm' = OR},
		ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer1),
		gen_server:cast(PortServer, {self(), Answer1})
	end.

%% @hidden
start_disconnect(radius, SessionList, #statedata{peer_id = SubscriberId,
		session_id = SessionID, server_address = Address} = StateData) ->
	case pg2:get_closest_pid(ocs_radius_acct_port_sup) of
		{error, Reason} ->
			error_logger:error_report(["Failed to initiate session disconnect function",
					{module, ?MODULE}, {subscriber, SubscriberId}, {address, Address},
					{session, SessionID}, {error, Reason}]);
		DiscSup ->
			start_disconnect1(radius, DiscSup, SessionList, StateData)
	end;
start_disconnect(diameter, SessionList, #statedata{peer_id = SubscriberId,
		session_id = SessionID, origin_host = OHost, origin_realm = ORealm} = State) ->
	case pg2:get_closest_pid(ocs_diamter_acct_port_sup) of
		{error, Reason} ->
			error_logger:error_report(["Failed to initiate session disconnect function",
					{module, ?MODULE}, {subscriber, SubscriberId}, {origin_host, OHost},
					{origin_realm, ORealm}, {session, SessionID}, {error, Reason}]);
		DiscSup ->
			start_disconnect1(diameter, DiscSup, SessionList, State)
	end.
%% @hidden
start_disconnect1(_Protocol, _DiscSup, [], _State) ->
	ok;
start_disconnect1(Protocol, DiscSup, [H | Tail], State) ->
	start_disconnect2(Protocol, DiscSup, H, State),
	start_disconnect1(Protocol, DiscSup, Tail, State).
%% @hidden
start_disconnect2(radius, DiscSup, SessionAttributes, #statedata{peer_id = Subscriber}) ->
	DiscArgs = [Subscriber, SessionAttributes],
	StartArgs = [DiscArgs, []],
	supervisor:start_child(DiscSup, StartArgs);
start_disconnect2(diameter, DiscSup, {_, SessionAttributes}, #statedata{session_id = SessionID}) ->
	Svc = ocs_diameter_acct_service,
	Alias = ocs_diameter_base_application,
	AppId = ?CC_APPLICATION_ID,
	SessionID  = proplists:get_value('Session-Id',SessionAttributes),
	OHost  = proplists:get_value('Origin-Host', SessionAttributes),
	ORealm  = proplists:get_value('Origin-Realm', SessionAttributes),
	DHost  = proplists:get_value('Destination-Host', SessionAttributes),
	DRealm  = proplists:get_value('Destination-Realm', SessionAttributes),
	DiscArgs = [Svc, Alias, SessionID, OHost, DHost, ORealm, DRealm, AppId],
	StartArgs = [DiscArgs, []],
	supervisor:start_child(DiscSup, StartArgs).

-spec extract_session_attributes(Attributes) -> SessionAttributes
	when
		Attributes :: radius_attributes:attributes(),
		SessionAttributes :: radius_attributes:attributes().
%% @doc Extract and return RADIUS session related attributes from
%% `Attributes'.
%% @hidden
extract_session_attributes(Attributes) ->
	F = fun({K, _}) when K == ?NasIdentifier; K == ?NasIpAddress;
				K == ?UserName; K == ?FramedIpAddress; K == ?NasPort;
				K == ?NasPortType; K == ?CalledStationId; K == ?CallingStationId;
				K == ?AcctSessionId; K == ?AcctMultiSessionId; K == ?NasPortId;
				K == ?OriginatingLineInfo; K == ?FramedInterfaceId;
				K == ?FramedIPv6Prefix ->
			true;
		(_) ->
			false
	end,
	lists:filter(F, Attributes).

