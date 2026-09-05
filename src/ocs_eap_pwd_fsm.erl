%%% ocs_eap_pwd_fsm.erl
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
%%% 	module implements the functions associated with an EAP server using
%%% 	only a password (EAP-PWD)
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://www.rfc-editor.org/info/rfc3579/">
%%% 	RFC3579 - RADIUS Support For EAP</a>
%%% @reference <a href="https://www.rfc-editor.org/info/rfc5931/">
%%% 	RFC5931 - EAP Authentication Using Only a Password</a>
%%%
-module(ocs_eap_pwd_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([eap_start/3, id/3, commit/3, confirm/3]).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").

-define(CC_APPLICATION_ID, 4).
-define(EAP_APPLICATION_ID, 5).

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
		trusted :: boolean(),
		service_type :: undefined | integer()}).
-type statedata() :: #statedata{}.
-type state() :: eap_start | id | commit | confirm.

-define(TIMEOUT, 30000).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(HMAC(Key, Data), crypto:mac(hmac, md5, Key, Data)).
		-define(PG_CLOSEST(Name),
				case pg:get_local_members(pg_scope_ocs, Name) of
					[] ->
						case pg:get_members(pg_scope_ocs, Name) of
							[] ->
								{error, {no_such_group, Name}};
							[Pid | _] ->
								Pid
						end;
					[Pid | _] ->
						Pid
				end).
	-else.
		-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
		-define(PG_CLOSEST(Name), pg2:get_closest_pid(Name)).
	-endif.
-else.
	-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
	-define(PG_CLOSEST(Name), pg2:get_closest_pid(Name)).
-endif.

%%----------------------------------------------------------------------
%%  The ocs_eap_pwd_fsm gen_statem call backs
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
init([radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, Trusted, SessionID,
		AccessRequest] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
	Data = #statedata{server_address = ServerAddress,
			server_port = ServerPort, client_address = ClientAddress,
			client_port = ClientPort, radius_fsm = RadiusFsm,
			secret = Secret, session_id = SessionID,
			server_id = list_to_binary(Hostname), start = AccessRequest,
			password_required = PasswordReq, trusted = Trusted},
	process_flag(trap_exit, true),
	Action = {next_event, internal, start},
	{ok, eap_start, Data, Action};
init([diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, Trusted, SessionId, ApplicationId, AuthType,
		OHost, ORealm, _DHost, _DRealm, Request, _Options] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
	case global:whereis_name({ocs_diameter_auth,
			node(), ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			ServiceType = case Request of
				#diameter_nas_app_AAR{'Service-Type' = [ST]} ->
					ST;
				_ ->
					undefined
			end,
			Data = #statedata{server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionId,
					server_id = list_to_binary(Hostname), auth_app_id = ApplicationId,
					auth_req_type = AuthType, origin_host = OHost,
					origin_realm = ORealm, diameter_port_server = PortServer,
					start = Request, password_required = PasswordReq,
					trusted = Trusted, service_type = ServiceType},
			process_flag(trap_exit, true),
			Action = {next_event, internal, start},
			{ok, eap_start, Data, Action}
	end.

-spec eap_start(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>eap_start</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
eap_start(internal = _EventType, start = _EventContent,
		#statedata{start = Request} = Data) ->
	try crypto:strong_rand_bytes(4) of
		Token ->
			case Request of
				#diameter_eap_app_DER{} ->
					eap_start1(Token, Data);
				#radius{} ->
					eap_start2(Token, Data)
			end
	catch
		Reason ->
			{stop, Reason}
	end.
%% @hidden
eap_start1(Token,
		#statedata{eap_id = EapID, session_id = SessionId,
				server_id = ServerID, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF,
				auth_req_type = AuthType, start = Request,
				origin_host = OHost, origin_realm = ORealm,
				diameter_port_server = PortServer} = Data) ->
	PwdId = #eap_pwd_id{group_desc = GroupDesc, random_fun = RandFunc,
			prf = PRF, token = Token, pwd_prep = none, identity = ServerID},
	PwdData = ocs_eap_codec:eap_pwd_id(PwdId),
	EapPwd = #eap_pwd{pwd_exch = id, data = PwdData},
	EapData = ocs_eap_codec:eap_pwd(EapPwd),
	Action = {timeout, ?TIMEOUT, timeout},
	case Request#diameter_eap_app_DER.'EAP-Payload' of
		[] ->
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = EapID, data = EapData},
			send_diameter_response(SessionId, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OHost, ORealm, EapPacket, PortServer, Request,
					Data),
			{next_state, id, Data, Action};
		EapMessage ->
			case catch ocs_eap_codec:eap_packet(EapMessage) of
				#eap_packet{code = response, type = ?Identity,
						identifier = NewEapID} ->
					NextEapID = (NewEapID rem 255) + 1,
					EapPacket = #eap_packet{code = request, type = ?PWD,
							identifier = NextEapID, data = EapData},
					NewData = Data#statedata{eap_id = NextEapID},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
							OHost, ORealm, EapPacket, PortServer, Request,
							Data),
					{next_state, id, NewData, Action};
				#eap_packet{code = request, identifier = NewEapID} ->
					EapPacket = #eap_packet{code = response,
							type = ?LegacyNak, identifier = NewEapID,
							data = <<0>>},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OHost, ORealm, EapPacket, PortServer, Request,
							Data),
					{stop, {shutdown, SessionId}};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = EapData1} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionId},
							{code, Code}, {type, EapType},
							{identifier, NewEapID}, {data, EapData1}]),
					EapPacket = #eap_packet{code = failure,
							identifier = NewEapID},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OHost, ORealm, EapPacket, PortServer, Request,
							Data),
					{stop, {shutdown, SessionId}};
				{'EXIT', _Reason} ->
					EapPacket = #eap_packet{code = failure,
							identifier = EapID},
					send_diameter_response(SessionId, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OHost, ORealm, EapPacket, PortServer, Request,
							Data),
					{stop, {shutdown, SessionId}}
			end
	end.
%% @hidden
eap_start2(Token,
		#statedata{eap_id = EapID,
				start = #radius{code = ?AccessRequest, id = RadiusID,
				authenticator = RequestAuthenticator,
				attributes = RequestAttributes}, session_id = SessionID,
				server_id = ServerID, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF} = Data) ->
	PwdId = #eap_pwd_id{group_desc = GroupDesc,
			random_fun = RandFunc, prf = PRF, token = Token,
			pwd_prep = none, identity = ServerID},
	PwdData = ocs_eap_codec:eap_pwd_id(PwdId),
	EapPwd = #eap_pwd{pwd_exch = id, data = PwdData},
	EapData = ocs_eap_codec:eap_pwd(EapPwd),
	NewData = Data#statedata{token = Token, start = undefined},
	Action = {timeout, ?TIMEOUT, timeout},
	case radius_attributes:find(?EAPMessage, RequestAttributes) of
		{ok, <<>>} ->
			EapPacket = #eap_packet{code = request, type = ?PWD,
					identifier = EapID, data = EapData},
			send_radius_response(EapPacket, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, NewData),
			{next_state, id, NewData, Action};
		{ok, EAPMessage} ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response, type = ?Identity,
						identifier = NewEapID} ->
					NextEapID = (NewEapID rem 255) + 1,
					EapPacket = #eap_packet{code = request, type = ?PWD,
							identifier = NextEapID, data = EapData},
					send_radius_response(EapPacket, ?AccessChallenge,
							[], RadiusID, RequestAuthenticator,
							RequestAttributes, NewData),
					NextData = NewData#statedata{eap_id = NextEapID},
					{next_state, id, NextData, Action};
				#eap_packet{code = request, identifier = NewEapID} ->
					EapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_radius_response(EapPacket, ?AccessReject,
							[], RadiusID, RequestAuthenticator,
							RequestAttributes, NewData),
					{stop, {shutdown, SessionID}};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = EapData1} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID},
							{code, Code}, {type, EapType},
							{identifier, NewEapID}, {data, EapData1}]),
					EapPacket = #eap_packet{code = failure,
							identifier = NewEapID},
					send_radius_response(EapPacket, ?AccessReject,
							[], RadiusID, RequestAuthenticator,
							RequestAttributes, NewData),
					{stop, {shutdown, SessionID}};
				{'EXIT', _Reason} ->
					EapPacket = #eap_packet{code = failure,
							identifier = EapID},
					send_radius_response(EapPacket, ?AccessReject,
							[], RadiusID, RequestAuthenticator,
							RequestAttributes, NewData),
					{stop, {shutdown, SessionID}}
			end;
		{error, not_found} ->
			EapPacket = #eap_packet{code = request, type = ?PWD,
					identifier = EapID, data = EapData},
			send_radius_response(EapPacket, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, NewData),
			{next_state, id, NewData, Action}
	end.

-spec id(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>id</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
id(cast = _EventType,
		{#radius{id = RadiusID, authenticator = RequestAuthenticator,
				attributes = RequestAttributes} = AccessRequest, RadiusFsm},
		#statedata{eap_id = EapID, group_desc = GroupDesc, rand_func = RandFunc,
				prf = PRF, session_id = SessionID} = Data) ->
	NewData = Data#statedata{radius_fsm = RadiusFsm},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?PWD, identifier = EapID,
					data = EapData} -> 
				#eap_pwd{pwd_exch = id,
						data = PwdId} = ocs_eap_codec:eap_pwd(EapData),
				#eap_pwd_id{group_desc = GroupDesc, random_fun = RandFunc,
						prf = PRF, token = Token, pwd_prep = none,
						identity = PeerID} = ocs_eap_codec:eap_pwd_id(PwdId),
				id1(AccessRequest, PeerID, Token, NewData);
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionID}, NewData}
		end
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
id(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF,
				session_id = SessionID, auth_req_type = RequestType,
				origin_host = OHost, origin_realm = ORealm,
				diameter_port_server = PortServer} = Data) ->
	try
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?PWD, identifier = EapID,
					data = EapData} -> 
				#eap_pwd{pwd_exch = id,
						data = PwdId} = ocs_eap_codec:eap_pwd(EapData),
				#eap_pwd_id{group_desc = GroupDesc, random_fun = RandFunc,
						prf = PRF, token = Token, pwd_prep = none,
						identity = PeerID} = ocs_eap_codec:eap_pwd_id(PwdId),
				id2(Request, PeerID, Token, Data);
			#eap_packet{code = response, type = ?LegacyNak,
					identifier = EapID} ->
				send_diameter_response(SessionID, RequestType,
						?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS',
						OHost, ORealm, none, PortServer, Request, Data),
				{stop, {shutdown, SessionID}}
		end
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, RequestType,
					?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS',
					OHost, ORealm, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end;
id(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.
%% @hidden
id1(#radius{id = RadiusID, authenticator = RequestAuthenticator,
			attributes = RequestAttributes},
		PeerID, Token,
		#statedata{eap_id = EapID,
				server_id = ServerID, session_id = SessionID,
				password_required = PwdReq} = Data) ->
	try
		S_rand = rand:uniform(?R),
		NewEapID = (EapID rem 255) + 1,
		case catch ocs:find_service(PeerID) of
			{ok, #service{password = Pwd}} ->
				Password = case PwdReq of
					false ->
						<<>>;
					_ ->
						Pwd
				end,
				PWE = ocs_eap_pwd:compute_pwe(Token, PeerID, ServerID, Password),
				{ScalarS, ElementS} = ocs_eap_pwd:compute_scalar(<<S_rand:256>>,
						PWE),
				Commit = #eap_pwd_commit{scalar = ScalarS, element = ElementS},
				PwdData = ocs_eap_codec:eap_pwd_commit(Commit),
				EapPwd = #eap_pwd{length = false,
						more = false, pwd_exch = commit, data = PwdData},
				EapData = ocs_eap_codec:eap_pwd(EapPwd),
				EapPacket = #eap_packet{code = request,
						type = ?PWD, identifier = NewEapID, data = EapData},
				NewData = Data#statedata{pwe = PWE, s_rand = S_rand,
					peer_id = PeerID, eap_id = NewEapID, scalar_s = ScalarS,
					element_s = ElementS, password = Pwd},
				send_radius_response(EapPacket, ?AccessChallenge,
						[], RadiusID, RequestAuthenticator,
						RequestAttributes, NewData),
				Action = {timeout, ?TIMEOUT, timeout},
				{next_state, commit, NewData, Action};
			{error, _Reason} ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				send_radius_response(EapPacket1, ?AccessReject, [], RadiusID,
						RequestAuthenticator, RequestAttributes, Data),
				{stop, {shutdown, SessionID}}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket2, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
id2(#diameter_eap_app_DER{} = Request, PeerID, Token,
		#statedata{eap_id = EapID, server_id = ServerID,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer,
				password_required = PwdReq} = Data) ->
	try
		S_rand = rand:uniform(?R),
		NewEapID = (EapID rem 255) + 1,
		case catch ocs:find_service(PeerID) of
			{ok, #service{password = Pwd}} ->
				Password = case PwdReq of
					false ->
						<<>>;
					_ ->
						Pwd
				end,
				PWE = ocs_eap_pwd:compute_pwe(Token, PeerID, ServerID, Password),
				{ScalarS, ElementS} = ocs_eap_pwd:compute_scalar(<<S_rand:256>>,
						PWE),
				Commit = #eap_pwd_commit{scalar = ScalarS, element = ElementS},
				PwdData = ocs_eap_codec:eap_pwd_commit(Commit),
				EapPwd = #eap_pwd{length = false,
					more = false, pwd_exch = commit, data = PwdData},
				EapData = ocs_eap_codec:eap_pwd(EapPwd),
				EapPacket = #eap_packet{code = request,
						type = ?PWD, identifier = NewEapID, data = EapData},
				NewData = Data#statedata{pwe = PWE, s_rand = S_rand,
					peer_id = PeerID, eap_id = NewEapID, scalar_s = ScalarS,
					element_s = ElementS, password = Pwd},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket, PortServer, Request, Data),
				Action = {timeout, ?TIMEOUT, timeout},
				{next_state, commit, NewData, Action};
			{error, _Reason} ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
						OH, OR, EapPacket1, PortServer, Request, Data),
				{stop, {shutdown, SessionID}}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket2, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end.

-spec commit(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>commit</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
commit(cast = _EventType,
		{#radius{id = RadiusID, authenticator = RequestAuthenticator,
				attributes = RequestAttributes} = AccessRequest, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionID} = Data) ->
	NewData = Data#statedata{radius_fsm = RadiusFsm},
	try 
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		EapPacket1 = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = EapData} = EapPacket1,
		EapPwd = ocs_eap_codec:eap_pwd(EapData),
		#eap_pwd{pwd_exch = commit, data = PwdData} = EapPwd,
		Commit = ocs_eap_codec:eap_pwd_commit(PwdData),
		#eap_pwd_commit{element = ElementP, scalar = ScalarP} = Commit,
		NextData = NewData#statedata{scalar_p = ScalarP,
				element_p = ElementP},
		commit1(AccessRequest, PwdData, NextData)
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket2, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
commit(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	try 
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		EapPacket1 = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = EapData} = EapPacket1,
		EapPwd = ocs_eap_codec:eap_pwd(EapData),
		#eap_pwd{pwd_exch = commit, data = PwdData} = EapPwd,
		Commit = ocs_eap_codec:eap_pwd_commit(PwdData),
		#eap_pwd_commit{element = ElementP, scalar = ScalarP} = Commit,
		NextData = Data#statedata{scalar_p = ScalarP,
				element_p = ElementP},
		commit5(Request, PwdData, NextData)
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket2, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end;
commit(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.
%% @hidden
commit1(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest, PwdData,
		#statedata{eap_id = EapID, scalar_s = ScalarS,
				element_s = ElementS, session_id = SessionID} = Data) ->
	ExpectedSize = size(<<ElementS/binary, ScalarS/binary>>),
	case size(PwdData) of 
		ExpectedSize ->
			commit2(AccessRequest, Data);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
commit2(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest,
		#statedata{element_p = ElementP, scalar_p = ScalarP,
				scalar_s = ScalarS, element_s = ElementS,
				eap_id = EapID, session_id = SessionID} = Data) ->
	case {ElementP, ScalarP} of
		{ElementS, ScalarS} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}};
		_ ->
			commit3(AccessRequest, Data)
	end.
%% @hidden
commit3(#radius{id = RadiusID, authenticator = RequestAuthenticator,
				attributes = RequestAttributes} = AccessRequest,
		#statedata{scalar_p = ScalarP, eap_id = EapID,
		session_id = SessionID} = Data) ->
	case ScalarP of
		<<ScalarP_Valid:256>>
				when 1 < ScalarP_Valid, ScalarP_Valid < ?R ->
			commit4(AccessRequest, Data);
		_ScalarP_Out_of_Range ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
commit4(#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = _AccessRequest,
		#statedata{eap_id = EapID, session_id = SessionID,
				scalar_p = ScalarP, element_p = ElementP,
				scalar_s = ScalarS, element_s = ElementS,
				s_rand = Srand, pwe = PWE, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF} = Data) ->
	case catch ocs_eap_pwd:compute_ks(<<Srand:256>>,
			PWE, ScalarP, ElementP) of
		{'EXIT', _Reason} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}};
		Ks ->
			Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
			Input = [Ks, ElementS, ScalarS,
					ElementP, ScalarP, Ciphersuite],
			ConfirmS = ocs_eap_pwd:h(Input),
			EapPwd = #eap_pwd{length = false,
					more = false, pwd_exch = confirm, data = ConfirmS},
			EapData = ocs_eap_codec:eap_pwd(EapPwd),
			NewEapID = (EapID rem 255) + 1,
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = NewEapID, data = EapData},
			send_radius_response(EapPacket, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			NewData = Data#statedata{eap_id = NewEapID,
					ks = Ks, confirm_s = ConfirmS},
			Action = {timeout, ?TIMEOUT, timeout},
			{next_state, confirm, NewData, Action}
	end.
%% @hidden
commit5(Request, BodyData,
		#statedata{eap_id = EapID, scalar_s = ScalarS,
				element_s = ElementS, session_id = SessionID,
				auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	ExpectedSize = size(<<ElementS/binary, ScalarS/binary>>),
	case size(BodyData) of 
		ExpectedSize ->
			commit6(Request, Data);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
commit6(Request,
		#statedata{element_p = ElementP, scalar_p = ScalarP,
				scalar_s = ScalarS, element_s = ElementS,
				eap_id = EapID, session_id = SessionID,
				auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	case {ElementP, ScalarP} of
		{ElementS, ScalarS} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}};
		_ ->
			commit7(Request, Data)
	end.
%% @hidden
commit7(Request,
		#statedata{scalar_p = ScalarP, eap_id = EapID,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	case ScalarP of
		<<ScalarP_Valid:256>>
				when 1 < ScalarP_Valid, ScalarP_Valid < ?R ->
			commit8(Request, Data);
		_ScalarP_Out_of_Range ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
commit8(Request,
		#statedata{eap_id = EapID, session_id = SessionID,
				scalar_p = ScalarP, element_p = ElementP,
				scalar_s = ScalarS, element_s = ElementS,
				s_rand = Srand, pwe = PWE, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF,
				auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	case catch ocs_eap_pwd:compute_ks(<<Srand:256>>,
			PWE, ScalarP, ElementP) of
		{'EXIT', _Reason} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}};
		Ks ->
			Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
			Input = [Ks, ElementS, ScalarS, ElementP, ScalarP, Ciphersuite],
			ConfirmS = ocs_eap_pwd:h(Input),
			EapPwd = #eap_pwd{length = false,
					more = false, pwd_exch = confirm, data = ConfirmS},
			EapData = ocs_eap_codec:eap_pwd(EapPwd),
			NewEapID = (EapID rem 255) + 1,
			EapPacket = #eap_packet{code = request,
					type = ?PWD, identifier = NewEapID, data = EapData},
			NewData = Data#statedata{eap_id = NewEapID, ks = Ks,
					confirm_s = ConfirmS},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OH, OR, EapPacket, PortServer, Request, Data),
			Action = {timeout, ?TIMEOUT, timeout},
			{next_state, confirm, NewData, Action}
	end.

-spec confirm(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>confirm</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
confirm(cast = _EventType,
		{#radius{id = RadiusID,
				authenticator = RequestAuthenticator,
				attributes = RequestAttributes} = AccessRequest,
				RadiusFsm},
		#statedata{session_id = SessionID, eap_id = EapID} = Data) ->
	NewData = Data#statedata{radius_fsm = RadiusFsm},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		EapPacket = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = EapData} = EapPacket,
		EapPwd = ocs_eap_codec:eap_pwd(EapData),
		#eap_pwd{pwd_exch = confirm, data = ConfirmP} = EapPwd,
		NextData = NewData#statedata{confirm_p = ConfirmP},
		confirm1(AccessRequest, NextData)
	catch
		_:_ ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket1, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
confirm(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	try
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		EapPacket = ocs_eap_codec:eap_packet(EapMessage),
		#eap_packet{code = response, type = ?PWD,
				identifier = EapID, data = EapData} = EapPacket,
		EapPwd = ocs_eap_codec:eap_pwd(EapData),
		#eap_pwd{pwd_exch = confirm, data = ConfirmP} = EapPwd,
		NewData = Data#statedata{confirm_p = ConfirmP},
		confirm4(Request, NewData)
	catch
		_:_ ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket1, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end;
confirm(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.
%% @hidden
confirm1(#radius{id = RadiusID,
			authenticator = RequestAuthenticator,
			attributes = RequestAttributes} = AccessRequest,
		#statedata{confirm_s = ConfirmS, eap_id = EapID,
				confirm_p = ConfirmP, session_id = SessionID} = Data) ->
	ExpectedSize = size(ConfirmS),
	case size(ConfirmP) of 
		ExpectedSize ->
			confirm2(AccessRequest, Data);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
confirm2(#radius{id = RadiusID, authenticator = RequestAuthenticator,
				attributes = RequestAttributes} = AccessRequest,
		#statedata{eap_id = EapID, ks = Ks,
				confirm_p = ConfirmP, scalar_s = ScalarS,
				element_s = ElementS, scalar_p = ScalarP,
				element_p = ElementP, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF,
				session_id = SessionID} = Data) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	case ocs_eap_pwd:h([Ks, ElementP, ScalarP,
			ElementS, ScalarS, Ciphersuite]) of
		ConfirmP ->
			confirm3(AccessRequest, Data);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
confirm3(#radius{id = RadiusID, authenticator = RequestAuthenticator,
			attributes = RequestAttributes} = _AccessRequest,
		#statedata{secret = Secret, eap_id = EapID, ks = Ks,
				confirm_p = ConfirmP, confirm_s = ConfirmS,
				scalar_s = ScalarS, scalar_p = ScalarP,
				group_desc = GroupDesc, rand_func = RandFunc,
				prf = PRF, session_id = SessionID,
				peer_id = PeerID, password = Password} = Data) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	MK = ocs_eap_pwd:h([Ks, ConfirmP, ConfirmS]),
	MethodID = ocs_eap_pwd:h([Ciphersuite, ScalarP, ScalarS]),
	<<MSK:64/binary, _EMSK:64/binary>> = ocs_eap_pwd:kdf(MK,
			<<?PWD, MethodID/binary>>, 128),
	Salt = rand:uniform(16#7fff) + 16#7fff,
	<<MSK1:32/binary, MSK2:32/binary>> = MSK,
	MsMppeRecvKey = encrypt_key(Secret, RequestAuthenticator, Salt, MSK1),
	MsMppeSendKey = encrypt_key(Secret, RequestAuthenticator, Salt, MSK2),
	UserName = binary_to_list(PeerID),
	{ServiceType, Direction, CallAddress} = get_service_type(RequestAttributes),
	Timestamp = calendar:local_time(),
	SessionAttributes = ocs_rating:session_attributes(RequestAttributes),
	case ocs_rating:authorize(radius, ServiceType, [PeerID], Password,
			Timestamp, CallAddress, Direction, SessionAttributes) of
		{authorized, _Subscriber, Attributes, _ExistingSessionAttributes} ->
			Attr1 = radius_attributes:store(?UserName, UserName, Attributes),
			Attr2 = radius_attributes:store(?Microsoft,
					?MsMppeRecvKey, {Salt, MsMppeRecvKey}, Attr1),
			Attr3 = radius_attributes:store(?Microsoft,
					?MsMppeSendKey, {Salt, MsMppeSendKey}, Attr2),
			EapPacket = #eap_packet{code = success, identifier = EapID},
			send_radius_response(EapPacket, ?AccessAccept, Attr3, RadiusID,
					RequestAuthenticator, RequestAttributes, Data),
			{stop, {shutdown, SessionID}, Data#statedata{mk = MK, msk = MSK}};
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(radius, ExistingSessionAttributes, Data),
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, Data),
			{stop, {shutdown, SessionID}};
		{unauthorized, _Reason, _ExistingSessionAttributes} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
confirm4(Request,
		#statedata{confirm_s = ConfirmS, eap_id = EapID,
				confirm_p = ConfirmP, session_id = SessionID,
				auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	ExpectedSize = size(ConfirmS),
	case size(ConfirmP) of 
		ExpectedSize ->
			confirm5(Request, Data);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
confirm5(Request,
		#statedata{eap_id = EapID, ks = Ks, confirm_p = ConfirmP,
				scalar_s = ScalarS, element_s = ElementS,
				scalar_p = ScalarP, element_p = ElementP,
				group_desc = GroupDesc, rand_func = RandFunc,
				prf = PRF, session_id = SessionID,
				auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer} = Data) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	case ocs_eap_pwd:h([Ks, ElementP, ScalarP,
			ElementS, ScalarS, Ciphersuite]) of
		ConfirmP ->
			confirm6(Request, Data);
		_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
confirm6(Request,
		#statedata{eap_id = EapID, ks = Ks, confirm_p = ConfirmP,
				confirm_s = ConfirmS, scalar_s = ScalarS,
				scalar_p = ScalarP, group_desc = GroupDesc,
				rand_func = RandFunc, prf = PRF,
				session_id = SessionID, peer_id = PeerID,
				password = Password, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				diameter_port_server = PortServer,
				service_type = ServiceType} = Data) ->
	Ciphersuite = <<GroupDesc:16, RandFunc, PRF>>,
	MK = ocs_eap_pwd:h([Ks, ConfirmP, ConfirmS]),
	MethodID = ocs_eap_pwd:h([Ciphersuite, ScalarP, ScalarS]),
	<<MSK:64/binary, _EMSK:64/binary>> = ocs_eap_pwd:kdf(MK,
			<<?PWD, MethodID/binary>>, 128),
	Timestamp = calendar:local_time(),
	SessionAttributes = [{'Origin-Host', OH},
			{'Origin-Realm', OR}, {'Session-Id', SessionID}],
	NewData = Data#statedata{mk = MK, msk = MSK},
	case ocs_rating:authorize(diameter, ServiceType,
			[PeerID], Password, Timestamp,
			undefined, undefined, SessionAttributes) of
		{authorized, _Subscriber, _Attributes, _ExistingSessionAttributes} ->
			EapPacket = #eap_packet{code = success, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
					OH, OR, EapPacket, PortServer, Request, NewData),
			{stop, {shutdown, SessionID}, NewData};
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(diameter, ExistingSessionAttributes, NewData),
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, NewData),
			{stop, {shutdown, SessionID}, NewData};
		{unauthorized, _Reason, _ExistingSessionAttributes} ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket1, PortServer, Request, NewData),
			{stop, {shutdown, SessionID}, NewData}
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

-spec send_radius_response(EapPacket, RadiusCode, ResponseAttributes, RadiusID,
		RequestAuthenticator, RequestAttributes, Data) -> ok
	when
		EapPacket :: #eap_packet{},
		RadiusCode :: integer(), 
		ResponseAttributes :: radius_attributes:attributes(),
		RadiusID :: integer(),
		RequestAuthenticator :: [byte()],
		RequestAttributes :: radius_attributes:attributes(),
		Data :: #statedata{}.
%% @doc Sends an RADIUS-Access/Challenge or Reject or Accept packet to peer.
%% @hidden
send_radius_response(EapPacket, RadiusCode, ResponseAttributes,
		RadiusID, RequestAuthenticator, RequestAttributes,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		secret = Secret, radius_fsm = RadiusFsm} = _Data) ->
	EapPacketData = ocs_eap_codec:eap_packet(EapPacket),
	AttrList1 = radius_attributes:add(?EAPMessage,
			EapPacketData, ResponseAttributes),
	AttrList2 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, AttrList1),
	Attributes1 = radius_attributes:codec(AttrList2),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = ?HMAC(Secret, [<<RadiusCode, RadiusID,
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
%% 	section 2.4.2 for use as String in a MS-MPPE-Recv-Key
%% 	or MS-MPPE-Send-Key attribute.
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
		EapPacket, PortServer, Request, Data) -> ok
	when
		SId :: string(), 
		AuthType :: integer(),
		ResultCode :: integer(),
		OH :: binary(),
		OR :: binary(),
		EapPacket :: none | #eap_packet{},
		PortServer :: pid(),
		Request :: #diameter_eap_app_DER{},
		Data :: #statedata{}.
%% @doc Log DIAMETER event and send appropriate DIAMETER answer to
%% ocs_diameter_auth_port_server.
%% @hidden
send_diameter_response(SId, AuthType, ResultCode, OH, OR, none,
		PortServer, Request, #statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _Data) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ResultCode, 'Origin-Host' = OH, 'Origin-Realm' = OR},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SId, AuthType, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
		OH, OR, EapPacket, PortServer, Request,
		#statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort, msk = MSK} = _Data) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	try
		EapData = ocs_eap_codec:eap_packet(EapPacket),
		Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = ?EAP_APPLICATION_ID,
				'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapData], 'EAP-Master-Session-Key' = [MSK]},
		ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
		gen_server:cast(PortServer, {self(), Answer})
	catch
		_:_ ->
		Answer1 = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = ?EAP_APPLICATION_ID,
				'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				'Origin-Host' = OH, 'Origin-Realm' = OR},
		ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer1),
		gen_server:cast(PortServer, {self(), Answer1})
	end;
send_diameter_response(SId, AuthType, ResultCode, OH, OR, EapPacket,
		PortServer, Request, #statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _Data) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	try
		EapData = ocs_eap_codec:eap_packet(EapPacket),
		Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = ?EAP_APPLICATION_ID,
				'Auth-Request-Type' = AuthType,
				'Result-Code' = ResultCode, 'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapData]},
		ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
		gen_server:cast(PortServer, {self(), Answer})
	catch
		_:_ ->
		Answer1 = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = ?EAP_APPLICATION_ID,
				'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				'Origin-Host' = OH, 'Origin-Realm' = OR},
		ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer1),
		gen_server:cast(PortServer, {self(), Answer1})
	end.

%% @hidden
start_disconnect(radius, SessionList, #statedata{peer_id = SubscriberId,
		session_id = SessionID, server_address = Address} = Data) ->
	case ?PG_CLOSEST(ocs_radius_acct_port_sup) of
		{error, Reason} ->
			error_logger:error_report(["Failed to initiate session disconnect function",
					{module, ?MODULE}, {subscriber, SubscriberId}, {address, Address},
					{session, SessionID}, {error, Reason}]);
		DiscSup ->
			start_disconnect1(radius, DiscSup, SessionList, Data)
	end;
start_disconnect(diameter, SessionList, #statedata{peer_id = SubscriberId,
		session_id = SessionID, origin_host = OHost, origin_realm = ORealm} = State) ->
	case ?PG_CLOSEST(ocs_diameter_acct_port_sup) of
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

get_service_type(Attr) ->
	case radius_attributes:find(?ServiceType, Attr) of
		{ok, 12} ->
			case radius_attributes:find(?Cisco, ?H323CallOrigin, Attr) of
				{ok, answer} ->
					case radius_attributes:find(?CallingStationId, Attr) of
						{ok, Address} ->
							{12, answer, Address};
						{error, not_found} ->
							{12, answer, undefined}
					end;
				_Other ->
					case radius_attributes:find(?CalledStationId, Attr) of
						{ok, Address} ->
							{12, originate, Address};
						{error, not_found} ->
							{12, originate, undefined}
					end
			end;
		{ok, ServiceType} ->
			{ServiceType, undefined, undefined};
		{error, not_found} ->
			{undefined, undefined, undefined}
	end.

