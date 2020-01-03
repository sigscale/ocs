%%% ocs_eap_akap_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2018 SigScale Global Inc.
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
%%% 	implements the functions associated with an EAP server within EAP
%%% 	3rd Generation Authentication and Key Agreement (EAP-AKA/AKA')
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc4187">
%%% 	RFC4187 - Extensible Authentication Protocol Method for 3rd Generation
%%% 		Authentication and Key Agreement (EAP-AKA)</a>
%%% @reference <a href="http://tools.ietf.org/html/rfc5448">
%%% 	RFC5448 - Improved Extensible Authentication Protocol Method for
%%% 		3rd Generation Authentication and Key Agreement (EAP-AKA')</a>
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=33&amp;GSMSpecPart2=402&amp;Search=search">
%%% 	3GPP TS 33.402 - Security aspects of non-3GPP accesses</a>
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=23&amp;GSMSpecPart2=003&amp;Search=search">
%%% 	3GPP TS 23.003 - Numbering, addressing and identification</a>
%%%
-module(ocs_eap_akap_fsm).
-copyright('Copyright (c) 2016 - 2018 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_akap_fsm API
-export([]).

%% export the ocs_eap_akap_fsm state callbacks
-export([eap_start/2, identity/2, vector/2, challenge/2, failure/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_3gpp_sta_application.hrl").
-include("diameter_3gpp.hrl").
-include("ocs_eap_codec.hrl").

-record(statedata,
		{sup :: pid(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		radius_fsm :: undefined | pid(),
		auc_fsm :: undefined | pid(),
		session_id:: binary() | {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		request :: undefined | #diameter_eap_app_DER{} | #'3gpp_sta_DER'{}
				| #radius{},
		secret :: undefined | binary(),
		eap_id = 0 :: byte(),
		server_id  ::  binary(),
		auth_app_id :: undefined | integer(),
		auth_req_type :: undefined | integer(),
		origin_host :: undefined | binary(),
		origin_realm :: undefined | binary(),
		diameter_port_server :: undefined | pid(),
		password_required :: boolean(),
		trusted :: boolean(),
		service_type :: undefined | integer(),
		keys :: [{pos_integer(), binary()}],
		id_req :: any | full | permanent | undefined,
		identity :: binary() | undefined,
		res :: binary() | undefined,
		ck :: binary() | undefined,
		ik :: binary() | undefined,
		msk :: binary() | undefined,
		emsk :: binary() | undefined,
		kaut :: binary() | undefined,
		kencr :: binary() | undefined,
		kre :: binary() | undefined,
		failure :: integer() | undefined}).
-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).

-define(EAP_APPLICATION_ID, 5).
-define(STa_APPLICATION_ID, 16777250).

%% 3GPP TS 23.003 19.3.2 Root NAI
-define(PERM_AKAp, $6).
%% 3GPP TS 23.003 19.3.4 Fast Re-auth
-define(FAST_AKAp, $8).
%% 3GPP TS 23.003 19.3.5 Pseudonym
-define(TEMP_AKAp, $7).

%%----------------------------------------------------------------------
%%  The ocs_eap_akap_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_akap_fsm gen_fsm call backs
%%----------------------------------------------------------------------

init([Sup, diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, Trusted, SessionId, ApplicationId, AuthReqType, OHost, ORealm,
		_DHost, _DRealm, Request, _Options] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
	{ok, Keys} = application:get_env(aka_kpseu),
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
			StateData = #statedata{sup = Sup,
					server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionId,
					server_id = list_to_binary(Hostname), auth_app_id = ApplicationId,
					auth_req_type = AuthReqType, origin_host = OHost,
					origin_realm = ORealm, diameter_port_server = PortServer,
					request = Request, password_required = PasswordReq,
					trusted = Trusted, keys = Keys, service_type = ServiceType},
			process_flag(trap_exit, true),
			{ok, eap_start, StateData, 0}
		end;
init([Sup, radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, Trusted, SessionId,
		#radius{attributes = Attributes} = AccessRequest] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
	{ok, Keys} = application:get_env(aka_kpseu),
	ServiceType = case radius_attributes:find(?ServiceType, Attributes) of
		{error, not_found} ->
			undefined;
		{_, ST} ->
			ST
	end,
	StateData = #statedata{sup = Sup,
			server_address = ServerAddress,
			server_port = ServerPort, client_address = ClientAddress,
			client_port = ClientPort, radius_fsm = RadiusFsm,
			secret = Secret, session_id = SessionId,
			server_id = list_to_binary(Hostname), request = AccessRequest,
			password_required = PasswordReq, trusted = Trusted,
			keys = Keys, service_type = ServiceType},
	process_flag(trap_exit, true),
	{ok, eap_start, StateData, 0}.

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
eap_start(timeout, #statedata{sup = Sup, eap_id = EapID,
		request = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator,
		attributes = RequestAttributes}, session_id = SessionId,
		trusted = Trusted, keys = Keys} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	NewStateData = StateData#statedata{auc_fsm = AucFsm},
	case radius_attributes:find(?EAPMessage, RequestAttributes) of
		{ok, <<>>} ->
			EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
			NextStateData = NewStateData#statedata{request = undefined, id_req = full},
			EapPacket = #eap_packet{code = request,
					type = ?AKAprime, identifier = EapID, data = EapData},
			EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
			send_radius_response(EapMessage1, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{next_state, identity, NextStateData, ?TIMEOUT};
		{ok, EapMessage1} ->
			try
				case ocs_eap_codec:eap_packet(EapMessage1) of
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID,
							data = <<?PERM_AKAp, PermanentID/binary>> = Identity}
							when Trusted == true ->
						NextStateData = NewStateData#statedata{eap_id = StartEapID,
								identity = Identity},
						[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
						% @todo handle RADIUS attribute for ANID
						gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
						{next_state, vector, NextStateData, ?TIMEOUT};
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID,
							data = <<?TEMP_AKAp:6, _/bits>> = Identity}
							when Trusted == true ->
						NextStateData = NewStateData#statedata{eap_id = StartEapID,
								identity = Identity},
						[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
						CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
						IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
						% @todo handle RADIUS attribute for ANID
						gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
						{next_state, vector, NextStateData, ?TIMEOUT};
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID,
							data = <<?FAST_AKAp:6, _/bits>> = Identity}
							when Trusted == true ->
						% @todo handle fast re-authentication
						NextEapID = (StartEapID rem 255) + 1,
						EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
						NextStateData = NewStateData#statedata{request = undefined,
								eap_id = NextEapID, identity = Identity, id_req = full},
						EapPacket = #eap_packet{code = request,
								type = ?AKAprime, identifier = NextEapID, data = EapData},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessChallenge, [], RadiusID,
								RequestAuthenticator, RequestAttributes, NextStateData),
						{next_state, identity, NextStateData, ?TIMEOUT};
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID, data = _Identity} ->
						NextEapID = (StartEapID rem 255) + 1,
						EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
						NextStateData = NewStateData#statedata{request = undefined,
								eap_id = NextEapID, id_req = full},
						EapPacket = #eap_packet{code = request,
								type = ?AKAprime, identifier = NextEapID, data = EapData},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessChallenge, [], RadiusID,
								RequestAuthenticator, RequestAttributes, NextStateData),
						{next_state, identity, NextStateData, ?TIMEOUT};
					#eap_packet{code = request, identifier = StartEapID} ->
						EapPacket = #eap_packet{code = response, type = ?LegacyNak,
								identifier = StartEapID, data = <<0>>},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessReject, [], RadiusID,
								RequestAuthenticator, RequestAttributes, NewStateData),
						{stop, {shutdown, SessionId}, StateData};
					#eap_packet{code = Code, type = EapType,
							identifier = StartEapID, data = Data} ->
						error_logger:warning_report(["Unknown EAP received",
								{pid, self()}, {session_id, SessionId}, {code, Code},
								{type, EapType}, {identifier, StartEapID}, {data, Data}]),
						EapPacket = #eap_packet{code = failure, identifier = StartEapID},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessReject, [], RadiusID,
								RequestAuthenticator, RequestAttributes, NewStateData),
						{stop, {shutdown, SessionId}, StateData}
				end
			catch
				{'EXIT', Reason} ->
					error_logger:warning_report(["EAP-AKA' failure",
							{pid, self()}, {session_id, SessionId},
							{radius_id, RadiusID}, {eap_msg, EapMessage1},
							{error, Reason}]),
					EapPacket1 = #eap_packet{code = failure, identifier = EapID},
					EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
					send_radius_response(EapMessage3, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionId}, StateData}
			end;
		{error, not_found} ->
			EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
			NextStateData = NewStateData#statedata{request = undefined, id_req = full},
			EapPacket = #eap_packet{code = request,
					type = ?AKAprime, identifier = EapID, data = EapData},
			EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
			send_radius_response(EapMessage1, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{next_state, identity, NextStateData, ?TIMEOUT}
	end;
eap_start(timeout, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		request = #diameter_eap_app_DER{'EAP-Payload' = []} = Request,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer, sup = Sup} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
	NewStateData = StateData#statedata{request = undefined,
			auc_fsm = AucFsm, id_req = full},
	EapPacket = #eap_packet{code = request,
			type = ?AKAprime, identifier = EapID, data = EapData},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
			EapMessage, PortServer, Request, StateData),
	{next_state, identity, NewStateData, ?TIMEOUT};
eap_start(timeout, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		request = #'3gpp_sta_DER'{'EAP-Payload' = []} = Request,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer, sup = Sup} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
	NewStateData = StateData#statedata{request = undefined,
			auc_fsm = AucFsm, id_req = full},
	EapPacket = #eap_packet{code = request,
			type = ?AKAprime, identifier = EapID, data = EapData},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
			EapMessage, PortServer, Request, StateData),
	{next_state, identity, NewStateData, ?TIMEOUT};
eap_start(timeout, #statedata{request =
		#diameter_eap_app_DER{'EAP-Payload' = EapMessage}} = StateData) ->
	eap_start1(EapMessage, StateData);
eap_start(timeout, #statedata{request =
		#'3gpp_sta_DER'{'EAP-Payload' = EapMessage}} = StateData) ->
	eap_start1(EapMessage, StateData).
%% @hidden
eap_start1(EapMessage, #statedata{sup = Sup, eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		request = Request, origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer, trusted = Trusted,
		keys = Keys} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	NewStateData = StateData#statedata{auc_fsm = AucFsm},
	try
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID,
					data = <<?PERM_AKAp, PermanentID/binary>> = Identity}
					when Trusted == true ->
				NextStateData = NewStateData#statedata{eap_id = StartEapID,
						identity = Identity},
				[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
				% @todo handle DIAMETER ANID AVP
				gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
				{next_state, vector, NextStateData, ?TIMEOUT};
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID,
					data = <<?TEMP_AKAp:6, _/bits>> = Identity}
					when Trusted == true ->
				NextStateData = NewStateData#statedata{eap_id = StartEapID,
						identity = Identity},
				[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
				CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
				IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
				% @todo handle DIAMETER ANID AVP
				gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
				{next_state, vector, NextStateData, ?TIMEOUT};
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID,
					data = <<?FAST_AKAp:6, _/bits>> = Identity}
					when Trusted == true ->
				% @todo handle fast re-authentication
				NextEapID = (StartEapID rem 255) + 1,
				EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
				NextStateData = NewStateData#statedata{request = undefined,
						eap_id = NextEapID, id_req = full, identity = Identity},
				EapPacket = #eap_packet{code = request,
						type = ?AKAprime, identifier = NextEapID, data = EapData},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
						EapMessage1, PortServer, Request, NextStateData),
				{next_state, identity, NextStateData, ?TIMEOUT};
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID, data = _Identity} ->
				NextEapID = (StartEapID rem 255) + 1,
				EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
				NextStateData = NewStateData#statedata{request = undefined,
						eap_id = NextEapID, id_req = full},
				EapPacket = #eap_packet{code = request,
						type = ?AKAprime, identifier = NextEapID, data = EapData},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
						EapMessage1, PortServer, Request, NextStateData),
				{next_state, identity, NextStateData, ?TIMEOUT};
			#eap_packet{code = request, identifier = NewEapID} ->
				EapPacket = #eap_packet{code = response, type = ?LegacyNak,
						identifier = NewEapID, data = <<0>>},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
						EapMessage1, PortServer, Request, NewStateData),
				{stop, {shutdown, SessionId}, NewStateData};
			#eap_packet{code = Code, type = EapType,
					identifier = NewEapID, data = Data} ->
				error_logger:warning_report(["Unknown EAP received",
						{pid, self()}, {session_id, SessionId}, {code, Code},
						{type, EapType}, {identifier, NewEapID}, {data, Data}]),
				EapPacket = #eap_packet{code = failure, identifier = NewEapID},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
						EapMessage1, PortServer, Request, NewStateData),
				{stop, {shutdown, SessionId}, NewStateData}
		end
	catch
		{'EXIT', Reason} ->
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_real, ORealm},
					{eap_msg, EapMessage}, {error, Reason}]),
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			EapMessage2 = ocs_eap_codec:eap_packet(EapPacket1),
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
					EapMessage2, PortServer, Request, NewStateData),
			{stop, {shutdown, SessionId}, NewStateData}
	end.

-spec identity(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>identity</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
identity(timeout, #statedata{session_id = SessionId} = StateData) ->
	{stop, {shutdown, SessionId}, StateData};
identity({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = Request, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionId,
		auc_fsm = AucFsm, keys = Keys, id_req = IdReq} = StateData) ->
	NewStateData = StateData#statedata{request = Request,
			radius_fsm = RadiusFsm},
	try
		EapMessage1 = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		case ocs_eap_codec:eap_packet(EapMessage1) of
			#eap_packet{code = response, type = ?AKAprime, identifier = EapID, data = Data} ->
				case ocs_eap_codec:eap_aka(Data) of
					#eap_aka_identity{identity = <<?PERM_AKAp,
							PermanentID/binary>> = Identity} when IdReq == full ->
						[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
						% @todo handle RADIUS attribute for ANID
						gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
						NextStateData = NewStateData#statedata{identity = Identity},
						{next_state, vector, NextStateData, ?TIMEOUT};
					#eap_aka_identity{identity = <<?TEMP_AKAp:6, _/bits>> = Identity}
							when IdReq == full ->
						[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
						CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
						IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
						% @todo handle RADIUS attribute for ANID
						gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
						NextStateData = NewStateData#statedata{identity = Identity},
						{next_state, vector, NextStateData, ?TIMEOUT}
				end;
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionId}, NewStateData}
		end
	catch
		_:Reason ->
			EapMessage2 = get_radius_eap(RequestAttributes),
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{radius_id, RadiusID}, {eap_msg, EapMessage2},
					{error, Reason}]),
			Notification = #eap_aka_notification{notification = 16384},
			Data1 = ocs_eap_codec:eap_aka(Notification),
			EapPacket = #eap_packet{code = request,
					type = ?AKAprime, identifier = EapID, data = Data1},
			EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
			NextStateData1 = NewStateData#statedata{failure = ?AccessReject},
			send_radius_response(EapMessage2, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NextStateData1),
			{next_state, failure, NextStateData1, ?TIMEOUT}
	end;
identity(#diameter_eap_app_DER{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	identity1(EapMessage, Request, StateData);
identity(#'3gpp_sta_DER'{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	identity1(EapMessage, Request, StateData).
%% @hidden
identity1(EapMessage, Request,
		#statedata{eap_id = EapID, session_id = SessionId,
		auth_req_type = AuthReqType, origin_host = OHost,
		origin_realm = ORealm, auc_fsm = AucFsm, id_req = IdReq,
		diameter_port_server = PortServer, keys = Keys} = StateData) ->
	try
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?AKAprime, identifier = EapID, data = Data} ->
				case ocs_eap_codec:eap_aka(Data) of
					#eap_aka_identity{identity = <<?PERM_AKAp,
							PermanentID/binary>> = Identity} when IdReq == full ->
						[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
						% @todo handle DIAMETER ANID AVP
						gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
						NewStateData = StateData#statedata{request = Request,
								identity = Identity},
						{next_state, vector, NewStateData, ?TIMEOUT};
					#eap_aka_identity{identity = <<?TEMP_AKAp:6, _/bits>> = Identity}
							when IdReq == full ->
						[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
						CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
						IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
						% @todo handle DIAMETER ANID AVP
						gen_fsm:send_event(AucFsm, {self(), IMSI, "WLAN"}),
						NewStateData = StateData#statedata{request = Request,
								identity = Identity},
						{next_state, vector, NewStateData, ?TIMEOUT}
				end;
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionId}, StateData}
		end
	catch
		_:Reason ->
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_real, ORealm},
					{eap_msg, EapMessage}, {error, Reason}]),
			Notification = #eap_aka_notification{notification = 16384},
			Data1 = ocs_eap_codec:eap_aka(Notification),
			EapPacket = #eap_packet{code = request,
					type = ?AKAprime, identifier = EapID, data = Data1},
			EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
			NewStateData1 = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OHost, ORealm, EapMessage1, PortServer, Request, NewStateData1),
			{next_state, failure, NewStateData1, ?TIMEOUT}
	end.

-spec vector(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>vector</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
vector(timeout, #statedata{session_id = SessionId} = StateData)->
	{stop, {shutdown, SessionId}, StateData};
vector({RAND, AUTN, CKprime, IKprime, XRES}, #statedata{eap_id = EapID,
		request = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = RequestAttributes},
		identity = Identity} = StateData) ->
	NextEapID = (EapID rem 255) + 1,
	<<Kencr:16/binary, Kaut:32/binary, Kre:32/binary, MSK:64/binary,
			EMSK:64/binary, _/binary>> = prf(<<IKprime/binary,
			CKprime/binary>>, <<"EAP-AKA'", Identity/binary>>, 7),
	AkaChallenge = #eap_aka_challenge{mac = <<0:128>>,
			kdf = [1], network = <<"WLAN">>, rand = RAND, autn = AUTN},
	EapData = ocs_eap_codec:eap_aka(AkaChallenge),
	EapPacket = #eap_packet{code = request, type = ?AKAprime,
			identifier = NextEapID, data = EapData},
	EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
	Mac = crypto:hmac(sha256, Kaut, EapMessage1, 16),
	EapMessage2 = ocs_eap_codec:aka_set_mac(Mac, EapMessage1),
	NewStateData = StateData#statedata{request = undefined,
			eap_id = NextEapID, res = XRES, ck = CKprime, ik = IKprime,
			msk = MSK, emsk = EMSK, kaut = Kaut, kencr = Kencr, kre = Kre},
	send_radius_response(EapMessage2, ?AccessChallenge, [], RadiusID,
			RequestAuthenticator, RequestAttributes, NewStateData),
	{next_state, challenge, NewStateData};
vector({RAND, AUTN, CKprime, IKprime, XRES}, #statedata{eap_id = EapID,
		request = Request, auth_req_type = AuthReqType,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer,
		session_id = SessionId, identity = Identity} = StateData) ->
	NextEapID = (EapID rem 255) + 1,
	<<Kencr:16/binary, Kaut:32/binary, Kre:32/binary, MSK:64/binary,
			EMSK:64/binary, _/binary>> = prf(<<IKprime/binary,
			CKprime/binary>>, <<"EAP-AKA'", Identity/binary>>, 7),
	AkaChallenge = #eap_aka_challenge{mac = <<0:128>>,
			kdf = [1], network = <<"WLAN">>, rand = RAND, autn = AUTN},
	EapData = ocs_eap_codec:eap_aka(AkaChallenge),
	EapPacket = #eap_packet{code = request, type = ?AKAprime,
			identifier = NextEapID, data = EapData},
	EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
	Mac = crypto:hmac(sha256, Kaut, EapMessage1, 16),
	EapMessage2 = ocs_eap_codec:aka_set_mac(Mac, EapMessage1),
	NewStateData = StateData#statedata{request = undefined,
			eap_id = NextEapID, res = XRES, ck = CKprime, ik = IKprime,
			msk = MSK, emsk = EMSK, kaut = Kaut, kencr = Kencr, kre = Kre},
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
			EapMessage2, PortServer, Request, NewStateData),
	{next_state, challenge, NewStateData};
vector({error, _Reason}, #statedata{eap_id = EapID,
		request = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator,
		attributes = RequestAttributes}} = StateData) ->
	Notification = #eap_aka_notification{notification = 16384},
	Data1 = ocs_eap_codec:eap_aka(Notification),
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = Data1},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	NewStateData = StateData#statedata{failure = ?AccessReject},
	send_radius_response(EapMessage, ?AccessChallenge, [], RadiusID,
			RequestAuthenticator, RequestAttributes, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT};
vector({error, user_unknown}, #statedata{request = Request,
		eap_id = EapID, auth_req_type = AuthReqType,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer,
		session_id = SessionId} = StateData) ->
	Notification = #eap_aka_notification{notification = 16384},
	Data1 = ocs_eap_codec:eap_aka(Notification),
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = Data1},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	NewStateData = StateData#statedata{failure = ?'DIAMETER_ERROR_USER_UNKNOWN'},
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			OHost, ORealm, EapMessage, PortServer, Request, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT};
vector({error, _Reason}, #statedata{request = Request,
		eap_id = EapID, auth_req_type = AuthReqType,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer,
		session_id = SessionId} = StateData) ->
	Notification = #eap_aka_notification{notification = 16384},
	Data1 = ocs_eap_codec:eap_aka(Notification),
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = Data1},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	NewStateData = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			OHost, ORealm, EapMessage, PortServer, Request, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT}.

-spec challenge(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>challenge</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
challenge(timeout, #statedata{session_id = SessionId} = StateData)->
	{stop, {shutdown, SessionId}, StateData};
challenge({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = Request, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionId, secret = Secret,
		identity = Identity, msk = MSK, res = RES, kaut = Kaut} = StateData) ->
	NewStateData = StateData#statedata{request = Request,
			radius_fsm = RadiusFsm},
	try
		EapMessage1 = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		#eap_packet{code = response, type = ?AKAprime, identifier = EapID,
				data = Data1} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data1) of
			#eap_aka_challenge{res = RES, checkcode = CheckCode, mac = MAC}
					when ((CheckCode == undefined) or (CheckCode == <<>>)) ->
				EapMessage2 = ocs_eap_codec:aka_clear_mac(EapMessage1),
				case crypto:hmac(sha256, Kaut, EapMessage2, 16) of
					MAC ->
						Salt = crypto:rand_uniform(16#8000, 16#ffff),
						<<MSK1:32/binary, MSK2:32/binary>> = MSK,
						MsMppeRecvKey = ocs_eap_aka:encrypt_key(Secret,
								RequestAuthenticator, Salt, MSK1),
						MsMppeSendKey = ocs_eap_aka:encrypt_key(Secret,
								RequestAuthenticator, Salt, MSK2),
						UserName = binary_to_list(Identity),
						Attributes = radius_attributes:new(),
						Attr1 = radius_attributes:store(?UserName, UserName, Attributes),
						Attr2 = radius_attributes:store(?Microsoft,
								?MsMppeRecvKey, {Salt, MsMppeRecvKey}, Attr1),
						Attr3 = radius_attributes:store(?Microsoft,
								?MsMppeSendKey, {Salt, MsMppeSendKey}, Attr2),
						EapPacket1 = #eap_packet{code = success, identifier = EapID},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						send_radius_response(EapMessage3, ?AccessAccept, Attr3, RadiusID,
								RequestAuthenticator, RequestAttributes, NewStateData),
						{stop, {shutdown, SessionId}, NewStateData};
					_MAC ->
						Notification = #eap_aka_notification{notification = 16384},
						Data2 = ocs_eap_codec:eap_aka(Notification),
						EapPacket1 = #eap_packet{code = request,
								type = ?AKAprime, identifier = EapID, data = Data2},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						NextStateData = NewStateData#statedata{failure = ?AccessReject},
						send_radius_response(EapMessage3, ?AccessChallenge, [], RadiusID,
								RequestAuthenticator, RequestAttributes, NextStateData),
						{next_state, failure, NextStateData, ?TIMEOUT}
				end;
			#eap_aka_challenge{checkcode = CheckCode}
					when ((CheckCode == undefined) or (CheckCode == <<>>)) ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKAprime, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?AccessReject},
				send_radius_response(EapMessage3, ?AccessChallenge, [], RadiusID,
						RequestAuthenticator, RequestAttributes, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT};
			% #eap_aka_synchronization_failure{auts = _AUTS} = _EAP ->
			% @todo handle resynchronization
			#eap_aka_authentication_reject{} = _EAP ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKAprime, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?AccessReject},
				send_radius_response(EapMessage3, ?AccessChallenge, [], RadiusID,
						RequestAuthenticator, RequestAttributes, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT};
			#eap_aka_client_error{client_error_code = _Code} = _EAP->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKAprime, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?AccessReject},
				send_radius_response(EapMessage3, ?AccessChallenge, [], RadiusID,
						RequestAuthenticator, RequestAttributes, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT}
		end
	catch
		_:Reason ->
			EapMessage4 = get_radius_eap(RequestAttributes),
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{radius_id, RadiusID}, {eap_msg, EapMessage4},
					{error, Reason}]),
			Notification1 = #eap_aka_notification{notification = 16384},
			Data3 = ocs_eap_codec:eap_aka(Notification1),
			EapPacket2 = #eap_packet{code = request,
					type = ?AKAprime, identifier = EapID, data = Data3},
			EapMessage5 = ocs_eap_codec:eap_packet(EapPacket2),
			NextStateData1 = NewStateData#statedata{failure = ?AccessReject},
			send_radius_response(EapMessage5, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NextStateData1),
			{next_state, failure, NextStateData1, ?TIMEOUT}
	end;
challenge(#diameter_eap_app_DER{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	challenge1(EapMessage, Request, StateData);
challenge(#'3gpp_sta_DER'{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	challenge1(EapMessage, Request, StateData).
%% @hidden
challenge1(EapMessage1, Request,
		#statedata{eap_id = EapID, session_id = SessionId,
		auth_req_type = AuthReqType, origin_host = OHost,
		origin_realm = ORealm, diameter_port_server = PortServer,
		res = RES, kaut = Kaut} = StateData) ->
	try
		#eap_packet{code = response, type = ?AKAprime, identifier = EapID,
				data = Data1} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data1) of
			#eap_aka_challenge{res = RES, checkcode = CheckCode, mac = MAC}
					when ((CheckCode == undefined) or (CheckCode == <<>>)) ->
				EapMessage2 = ocs_eap_codec:aka_clear_mac(EapMessage1),
				case crypto:hmac(sha256, Kaut, EapMessage2, 16) of
					MAC ->
						EapPacket1 = #eap_packet{code = success, identifier = EapID},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						send_diameter_response(SessionId, AuthReqType,
								?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
								OHost, ORealm, EapMessage3, PortServer, Request, StateData),
						{stop, {shutdown, SessionId}, StateData};
					_MAC ->
						Notification = #eap_aka_notification{notification = 16384},
						Data2 = ocs_eap_codec:eap_aka(Notification),
						EapPacket1 = #eap_packet{code = request,
								type = ?AKAprime, identifier = EapID, data = Data2},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						NewStateData = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
						send_diameter_response(SessionId, AuthReqType,
								?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
								OHost, ORealm, EapMessage3, PortServer, Request, NewStateData),
						{next_state, failure, NewStateData, ?TIMEOUT}
				end;
			#eap_aka_challenge{checkcode = CheckCode}
					when ((CheckCode == undefined) or (CheckCode == <<>>)) ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKAprime, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NewStateData = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OHost, ORealm, EapMessage3, PortServer, Request, NewStateData),
				{next_state, failure, NewStateData, ?TIMEOUT};
			% #eap_aka_synchronization_failure{auts = _AUTS} = _EAP ->
			% @todo handle resynchronization
			#eap_aka_authentication_reject{} = _EAP ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKAprime, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NewStateData = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OHost, ORealm, EapMessage3, PortServer, Request, NewStateData),
				{next_state, failure, NewStateData, ?TIMEOUT};
			#eap_aka_client_error{client_error_code = _Code} ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKAprime, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NewStateData = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OHost, ORealm, EapMessage3, PortServer, Request, NewStateData),
				{next_state, failure, NewStateData, ?TIMEOUT}
		end
	catch
		_:Reason ->
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_real, ORealm},
					{eap_msg, EapMessage1}, {error, Reason}]),
			Notification1 = #eap_aka_notification{notification = 16384},
			Data3 = ocs_eap_codec:eap_aka(Notification1),
			EapPacket2 = #eap_packet{code = request,
					type = ?AKAprime, identifier = EapID, data = Data3},
			EapMessage4 = ocs_eap_codec:eap_packet(EapPacket2),
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OHost, ORealm, EapMessage4, PortServer, Request, StateData),
			{next_state, failure, StateData, ?TIMEOUT}
	end.

-spec failure(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>failure</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
failure(timeout, #statedata{session_id = SessionId} = StateData)->
	{stop, {shutdown, SessionId}, StateData};
failure({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = Request, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionId,
		failure = RadiusCode} = StateData) ->
	NewStateData = StateData#statedata{request = Request,
			radius_fsm = RadiusFsm},
	try
		EapMessage1 = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		#eap_packet{code = response, type = ?AKAprime, identifier = EapID,
				data = Data} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data) of
			#eap_aka_notification{mac = undefined} = _EAP ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				EapMessage2 = ocs_eap_codec:eap_packet(EapPacket1),
				send_radius_response(EapMessage2, RadiusCode, [], RadiusID,
						RequestAuthenticator, RequestAttributes, NewStateData),
				{stop, {shutdown, SessionId}, NewStateData}
			% #eap_aka_notification{mac = MAC} = _EAP ->
			% @todo Handle notification when P=0
		end
	catch
		_:Reason ->
			EapMessage3 = get_radius_eap(RequestAttributes),
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{radius_id, RadiusID}, {eap_msg, EapMessage3},
					{error, Reason}]),
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			EapMessage4 = ocs_eap_codec:eap_packet(EapPacket2),
			send_radius_response(EapMessage4, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{stop, {shutdown, SessionId}, NewStateData}
	end;
failure(#diameter_eap_app_DER{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	failure(EapMessage, Request, StateData);
failure(#'3gpp_sta_DER'{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	failure(EapMessage, Request, StateData).
%% @hidden
failure(EapMessage1, Request, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer,
		failure = ResultCode} = StateData) ->
	try
		#eap_packet{code = response, type = ?AKAprime, identifier = EapID,
				data = Data} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data) of
			#eap_aka_notification{mac = undefined} = _EAP ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				EapMessage2 = ocs_eap_codec:eap_packet(EapPacket1),
				send_diameter_response(SessionId, AuthReqType, ResultCode,
						OHost, ORealm, EapMessage2, PortServer, Request, StateData),
				{stop, {shutdown, SessionId}, StateData}
			% #eap_aka_notification{mac = MAC} = _EAP ->
			% @todo Handle notification when P=0
		end
	catch
		_:Reason ->
			error_logger:warning_report(["EAP-AKA' failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_real, ORealm},
					{eap_msg, EapMessage1}, {error, Reason}]),
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			EapMessage3 = ocs_eap_codec:eap_packet(EapPacket2),
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, EapMessage3, PortServer, Request, StateData),
			{stop, {shutdown, SessionId}, StateData}
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
	{next_state, StateName, StateData, ?TIMEOUT}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid :: pid(), Tag :: term()},
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {reply, Reply, NextStateName, NewStateData}
		| {reply, Reply, NextStateName, NewStateData, Timeout}
		| {reply, Reply, NextStateName, NewStateData, hibernate}
		| {next_state, NextStateName, NewStateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, Reply, NewStateData}
		| {stop, Reason, NewStateData},
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
	{next_state, StateName, StateData, ?TIMEOUT}.

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

-spec send_radius_response(EapMessage, RadiusCode, ResponseAttributes, RadiusID,
		RequestAuthenticator, RequestAttributes, StateData) -> ok
	when
		EapMessage :: binary(),
		RadiusCode :: integer(),
		ResponseAttributes :: radius_attributes:attributes(),
		RadiusID :: integer(),
		RequestAuthenticator :: [byte()],
		RequestAttributes :: radius_attributes:attributes(),
		StateData :: #statedata{}.
%% @doc Sends a RADIUS Access/Challenge, Reject or Accept packet to peer.
%% @hidden
send_radius_response(EapMessage, RadiusCode, ResponseAttributes,
		RadiusID, RequestAuthenticator, RequestAttributes,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		secret = Secret, radius_fsm = RadiusFsm} = _StateData) ->
	AttrList1 = radius_attributes:add(?EAPMessage,
			EapMessage, ResponseAttributes),
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
		_ ->
			ok
	end,
	radius:response(RadiusFsm, {response, ResponsePacket}).

-spec send_diameter_response(SessionId, AuthType, ResultCode,
		OriginHost, OriginRealm, EapMessage, PortServer,
		Request, StateData) -> ok
	when
		SessionId :: binary(),
		AuthType :: integer(),
		ResultCode :: integer(),
		OriginHost :: binary(),
		OriginRealm :: binary(),
		EapMessage :: binary(),
		PortServer :: pid(),
		Request :: #diameter_eap_app_DER{} | #'3gpp_sta_DER'{},
		StateData :: #statedata{}.
%% @doc Log DIAMETER event and send appropriate DIAMETER answer to
%% 	ocs_diameter_auth_port_server.
%% @hidden
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
		OriginHost, OriginRealm, EapMessage,
		PortServer, #diameter_eap_app_DER{} = Request,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort} = _StateData)
		when is_binary(SessionId), is_integer(AuthType),
		is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
		OriginHost, OriginRealm, EapMessage,
		PortServer, #'3gpp_sta_DER'{} = Request,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort} = _StateData)
		when is_binary(SessionId), is_integer(AuthType),
		is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_sta_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
		OriginHost, OriginRealm, EapMessage,
		PortServer, #diameter_eap_app_DER{} = Request,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		msk = MSK} = _StateData) when is_binary(SessionId),
		is_integer(AuthType), is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer), is_binary(MSK) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'EAP-Master-Session-Key' = [MSK]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
		OriginHost, OriginRealm, EapMessage,
		PortServer, #'3gpp_sta_DER'{} = Request,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		msk = MSK} = _StateData) when is_binary(SessionId),
		is_integer(AuthType), is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer), is_binary(MSK) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_sta_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'EAP-Master-Session-Key' = [MSK]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_ERROR_USER_UNKNOWN',
		OriginHost, OriginRealm, EapMessage,
		PortServer, #'3gpp_sta_DER'{} = Request,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		msk = MSK} = _StateData) when is_binary(SessionId),
		is_integer(AuthType), is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer), is_binary(MSK) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_sta_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = #'diameter_base_Experimental-Result'{
					'Vendor-Id' = 10415,
					'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'},
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'EAP-Master-Session-Key' = [MSK]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType, ResultCode,
		OriginHost, OriginRealm, EapMessage,
		PortServer, #diameter_eap_app_DER{} = Request,
		#statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _StateData)
		when is_binary(SessionId),
		is_integer(AuthType), is_integer(ResultCode),
		is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType, ResultCode,
		OriginHost, OriginRealm, EapMessage,
		PortServer, #'3gpp_sta_DER'{} = Request,
		#statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _StateData)
		when is_binary(SessionId),
		is_integer(AuthType), is_integer(ResultCode),
		is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_sta_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}).

-spec prf(K, S, N) -> MK
	when
		K :: binary(),
		S :: binary(),
		N :: pos_integer(),
		MK :: binary().
%% @doc Pseudo-Random Number Function (PRF).
%%
%% 	See RFC5448 3.4.
%% @private
prf(K, S, N) when is_binary(K), is_binary(S), is_integer(N), N > 1 ->
	prf(K, S, N, 1, <<>>, []).
%% @hidden
prf(_, _, N, P, _, Acc) when P > N ->
	iolist_to_binary(lists:reverse(Acc));
prf(K, S, N, P, T1, Acc) ->
	T2 = crypto:hmac(sha256, K, <<T1/binary, S/binary, P>>),
	prf(K, S, N, P + 1, T2, [T2 | Acc]).

%% @hidden
get_radius_eap(RequestAttributes) ->
	case radius_attributes:find(?EAPMessage, RequestAttributes) of
		{ok, Value} ->
			Value;
		{error, not_found} ->
			undefined
	end.

