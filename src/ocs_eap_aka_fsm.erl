%%% ocs_eap_aka_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=33&amp;GSMSpecPart2=402&amp;Search=search">
%%% 	3GPP TS 33.402 - Security aspects of non-3GPP accesses</a>
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=29&amp;GSMSpecPart2=273">
%%% 	3GPP TS 29.273 - 3GPP EPS AAA interfaces</a>
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=23&amp;GSMSpecPart2=003&amp;Search=search">
%%% 	3GPP TS 23.003 - Numbering, addressing and identification</a>
%%%
-module(ocs_eap_aka_fsm).
-copyright('Copyright (c) 2016 - 2023 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_aka_fsm API
-export([]).

%% export the ocs_eap_aka_fsm state callbacks
-export([eap_start/2, identity/2, vector/2, challenge/2,
			register/2, failure/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").
-include("diameter_gen_3gpp_swx_application.hrl").
-include("diameter_3gpp.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").

-record(statedata,
		{sup :: pid(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		radius_fsm :: undefined | pid(),
		auc_fsm :: undefined | pid(),
		session_id:: diameter:'OctetString'()
				| {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		request :: undefined | #diameter_eap_app_DER{} | #'3gpp_swm_DER'{}
				| #radius{},
		response :: undefined | {EapMessage :: binary(),
				radius_attributes:attributes()},
		secret :: undefined | binary(),
		eap_id = 0 :: byte(),
		auth_app_id :: undefined | integer(),
		auth_req_type :: undefined | integer(),
		origin_host :: undefined | binary(),
		origin_realm :: undefined | binary(),
		nas_host :: undefined | binary(),
		nas_realm :: undefined | binary(),
		diameter_port_server :: undefined | pid(),
		password_required :: boolean(),
		trusted :: boolean(),
		service_type :: undefined | integer(),
		keys :: [{pos_integer(), binary()}],
		id_req :: any | full | permanent | undefined,
		identity = [] :: binary() | [],
		imsi :: binary() | undefined,
		res :: binary() | undefined,
		ck :: binary() | undefined,
		ik :: binary() | undefined,
		msk :: binary() | undefined,
		emsk :: binary() | undefined,
		kaut :: binary() | undefined,
		kencr :: binary() | undefined,
		failure :: integer() | undefined}).
-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).

-define(EAP_APPLICATION_ID, 5).
-define(SWm_APPLICATION_ID, 16777264).

%% 3GPP TS 23.003 19.3.2 Root NAI
-define(PERM_AKA,  $0).
%% 3GPP TS 23.003 19.3.4 Fast Re-auth
-define(FAST_AKA,  $4).
%% 3GPP TS 23.003 19.3.5 Pseudonym
-define(TEMP_AKA,  $2).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(HMAC_SHA(Key, Data), crypto:macN(hmac, sha, Key, Data, 16)).
	-else.
		-define(HMAC_SHA(Key, Data), crypto:hmac(sha, Key, Data, 16)).
	-endif.
-else.
		-define(HMAC_SHA(Key, Data), crypto:hmac(sha, Key, Data, 16)).
-endif.
-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(HMAC_MD5(Key, Data), crypto:mac(hmac, md5, Key, Data)).
	-else.
		-define(HMAC_MD5(Key, Data), crypto:hmac(md5, Key, Data)).
	-endif.
-else.
	-define(HMAC_MD5(Key, Data), crypto:hmac(md5, Key, Data)).
-endif.

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_fsm gen_fsm call backs
%%----------------------------------------------------------------------

init([Sup, diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, Trusted, SessionId, ApplicationId, AuthReqType, OHost, ORealm,
		DHost, DRealm, Request, _Options] = _Args) ->
	{ok, Keys} = application:get_env(aka_kpseu),
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
			StateData = #statedata{sup = Sup,
					server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionId,
					auth_app_id = ApplicationId, auth_req_type = AuthReqType,
					origin_host = OHost, origin_realm = ORealm,
					nas_host = DHost, nas_realm = DRealm,
					diameter_port_server = PortServer, request = Request,
					password_required = PasswordReq, trusted = Trusted,
					keys = Keys, service_type = ServiceType},
			process_flag(trap_exit, true),
			{ok, eap_start, StateData, 0}
		end;
init([Sup, radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, Trusted, SessionId,
		#radius{attributes = Attributes} = AccessRequest] = _Args) ->
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
			request = AccessRequest, password_required = PasswordReq,
			trusted = Trusted, keys = Keys, service_type = ServiceType},
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
					type = ?AKA, identifier = EapID, data = EapData},
			EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
			send_radius_response(EapMessage1, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, undefined, NextStateData),
			{next_state, identity, NextStateData, ?TIMEOUT};
		{ok, EapMessage1} ->
			try
				case ocs_eap_codec:eap_packet(EapMessage1) of
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID,
							data = <<?PERM_AKA, PermanentID/binary>> = Identity}
							when Trusted == true ->
						[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
						NextStateData = NewStateData#statedata{eap_id = StartEapID,
								imsi = IMSI, identity = Identity},
						gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined,
								get_radius_rat(RequestAttributes)}}),
						{next_state, vector, NextStateData, ?TIMEOUT};
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID,
							data = <<?TEMP_AKA:6, _/bits>> = Identity}
							when Trusted == true ->
						[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
						CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
						IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
						NextStateData = NewStateData#statedata{eap_id = StartEapID,
								imsi = IMSI, identity = Identity},
						gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined,
								get_radius_rat(RequestAttributes)}}),
						{next_state, vector, NextStateData, ?TIMEOUT};
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID,
							data = <<?FAST_AKA:6, _/bits>> = Identity}
							when Trusted == true ->
						% @todo handle fast re-authentication
						NextEapID = (StartEapID rem 255) + 1,
						EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
						NextStateData = NewStateData#statedata{request = undefined,
								eap_id = NextEapID, identity = Identity, id_req = full},
						EapPacket = #eap_packet{code = request,
								type = ?AKA, identifier = NextEapID, data = EapData},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessChallenge,
								[], RadiusID, RequestAuthenticator,
								RequestAttributes, undefined, NextStateData),
						{next_state, identity, NextStateData, ?TIMEOUT};
					#eap_packet{code = response, type = ?Identity,
							identifier = StartEapID, data = _Identity} ->
						NextEapID = (StartEapID rem 255) + 1,
						EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
						NextStateData = NewStateData#statedata{request = undefined,
								eap_id = NextEapID, id_req = full},
						EapPacket = #eap_packet{code = request,
								type = ?AKA, identifier = NextEapID, data = EapData},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessChallenge,
								[], RadiusID, RequestAuthenticator,
								RequestAttributes, undefined, NextStateData),
						{next_state, identity, NextStateData, ?TIMEOUT};
					#eap_packet{code = request, identifier = StartEapID} ->
						EapPacket = #eap_packet{code = response, type = ?LegacyNak,
								identifier = StartEapID, data = <<0>>},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessReject,
								[], RadiusID, RequestAuthenticator,
								RequestAttributes, undefined, NewStateData),
						{stop, {shutdown, SessionId}, StateData};
					#eap_packet{code = Code, type = EapType,
							identifier = StartEapID, data = Data} = _EAP ->
						error_logger:warning_report(["Unknown EAP received",
								{pid, self()}, {session_id, SessionId}, {code, Code},
								{type, EapType}, {identifier, StartEapID}, {data, Data}]),
						EapPacket = #eap_packet{code = failure, identifier = StartEapID},
						EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
						send_radius_response(EapMessage2, ?AccessReject,
								[], RadiusID, RequestAuthenticator,
								RequestAttributes, undefined, NewStateData),
						{stop, {shutdown, SessionId}, StateData}
				end
			catch
				{'EXIT', Reason} ->
					error_logger:warning_report(["EAP-AKA failure",
							{pid, self()}, {session_id, SessionId},
							{radius_id, RadiusID}, {eap_msg, EapMessage1},
							{error, Reason}]),
					EapPacket1 = #eap_packet{code = failure, identifier = EapID},
					EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
					send_radius_response(EapMessage3, ?AccessReject,
							[], RadiusID, RequestAuthenticator,
							RequestAttributes, undefined, NewStateData),
					{stop, {shutdown, SessionId}, StateData}
			end;
		{error, not_found} ->
			EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
			NextStateData = NewStateData#statedata{request = undefined, id_req = full},
			EapPacket = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = EapData},
			EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
			send_radius_response(EapMessage1, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, undefined, NewStateData),
			{next_state, identity, NextStateData, ?TIMEOUT}
	end;
eap_start(timeout, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		request = #diameter_eap_app_DER{'EAP-Payload' = []} = Request,
		diameter_port_server = PortServer, sup = Sup} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
	NewStateData = StateData#statedata{request = undefined,
			auc_fsm = AucFsm, id_req = full},
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = EapData},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			EapMessage, PortServer, Request, undefined, NewStateData),
	{next_state, identity, NewStateData, ?TIMEOUT};
eap_start(timeout, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		request = #'3gpp_swm_DER'{'EAP-Payload' = []} = Request,
		diameter_port_server = PortServer, sup = Sup} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
	NewStateData = StateData#statedata{request = undefined,
			auc_fsm = AucFsm, id_req = full},
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = EapData},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			EapMessage, PortServer, Request, undefined, NewStateData),
	{next_state, identity, NewStateData, ?TIMEOUT};
eap_start(timeout, #statedata{request =
		#diameter_eap_app_DER{'EAP-Payload' = EapMessage,
		'NAS-Port-Type' = NasPortType}} = StateData) ->
	eap_start1(EapMessage, nas_port_type(NasPortType), StateData);
eap_start(timeout, #statedata{request =
		#'3gpp_swm_DER'{'EAP-Payload' = EapMessage,
		'RAT-Type' = [RAT]}} = StateData) ->
	eap_start1(EapMessage, RAT, StateData).
%% @hidden
eap_start1(EapMessage, RAT, #statedata{sup = Sup, eap_id = EapID,
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
					data = <<?PERM_AKA, PermanentID/binary>> = Identity}
					when Trusted == true ->
				[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
				NextStateData = NewStateData#statedata{eap_id = StartEapID,
						imsi = IMSI, identity = Identity},
				gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined, RAT}}),
				{next_state, vector, NextStateData, ?TIMEOUT};
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID,
					data = <<?TEMP_AKA:6, _/bits>> = Identity}
					when Trusted == true ->
				[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
				CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
				IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
				gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined, RAT}}),
				NextStateData = NewStateData#statedata{eap_id = StartEapID,
						imsi = IMSI, identity = Identity},
				{next_state, vector, NextStateData, ?TIMEOUT};
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID,
					data = <<?FAST_AKA:6, _/bits>> = Identity}
					when Trusted == true ->
				% @todo handle fast re-authentication
				NextEapID = (StartEapID rem 255) + 1,
				EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
				NextStateData = NewStateData#statedata{request = undefined,
						eap_id = NextEapID, id_req = full, identity = Identity},
				EapPacket = #eap_packet{code = request,
						type = ?AKA, identifier = NextEapID, data = EapData},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						EapMessage1, PortServer, Request, undefined, NextStateData),
				{next_state, identity, NextStateData, ?TIMEOUT};
			#eap_packet{code = response, type = ?Identity,
					identifier = StartEapID, data = _Identity} ->
				NextEapID = (StartEapID rem 255) + 1,
				EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{fullauth_id_req = true}),
				NextStateData = NewStateData#statedata{request = undefined,
						eap_id = NextEapID, id_req = full},
				EapPacket = #eap_packet{code = request,
						type = ?AKA, identifier = NextEapID, data = EapData},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						EapMessage1, PortServer, Request, undefined, NextStateData),
				{next_state, identity, NextStateData, ?TIMEOUT};
			#eap_packet{code = request, identifier = NewEapID} ->
				EapPacket = #eap_packet{code = response, type = ?LegacyNak,
						identifier = NewEapID, data = <<0>>},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
						EapMessage1, PortServer, Request, undefined, NewStateData),
				{stop, {shutdown, SessionId}, NewStateData};
			#eap_packet{code = Code, type = EapType,
					identifier = NewEapID, data = Data} ->
				error_logger:warning_report(["Unknown EAP received",
						{pid, self()}, {session_id, SessionId}, {code, Code},
						{type, EapType}, {identifier, NewEapID}, {data, Data}]),
				EapPacket = #eap_packet{code = failure, identifier = NewEapID},
				EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
						EapMessage1, PortServer, Request, undefined, NewStateData),
				{stop, {shutdown, SessionId}, NewStateData}
		end
	catch
		{'EXIT', Reason} ->
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_realm, ORealm},
					{eap_msg, EapMessage}, {error, Reason}]),
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			EapMessage2 = ocs_eap_codec:eap_packet(EapPacket1),
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					EapMessage2, PortServer, Request, undefined, NewStateData),
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
			#eap_packet{code = response, type = ?AKA, identifier = EapID, data = Data} ->
				case ocs_eap_codec:eap_aka(Data) of
					#eap_aka_identity{identity = <<?PERM_AKA,
							PermanentID/binary>> = Identity} when IdReq == full ->
						[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
						gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined,
								get_radius_rat(RequestAttributes)}}),
						NextStateData = NewStateData#statedata{imsi = IMSI,
								identity = Identity},
						{next_state, vector, NextStateData, ?TIMEOUT};
					#eap_aka_identity{identity = <<?TEMP_AKA:6, _/bits>> = Identity}
							when IdReq == full ->
						[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
						CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
						IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
						gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined,
								get_radius_rat(RequestAttributes)}}),
						NextStateData = NewStateData#statedata{imsi = IMSI,
								identity = Identity},
						{next_state, vector, NextStateData, ?TIMEOUT}
				end;
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionId}, NewStateData}
		end
	catch
		_:Reason ->
			EapMessage2 = get_radius_eap(RequestAttributes),
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{radius_id, RadiusID}, {eap_msg, EapMessage2},
					{error, Reason}]),
			Notification = #eap_aka_notification{notification = 16384},
			Data1 = ocs_eap_codec:eap_aka(Notification),
			EapPacket = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = Data1},
			EapMessage2 = ocs_eap_codec:eap_packet(EapPacket),
			NextStateData1 = NewStateData#statedata{failure = ?AccessReject},
			send_radius_response(EapMessage2, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, undefined, NextStateData1),
			{next_state, failure, NextStateData1, ?TIMEOUT}
	end;
identity(#diameter_eap_app_DER{'EAP-Payload' = EapMessage,
		'NAS-Port-Type' = NasPortType} = Request, StateData) ->
	identity1(EapMessage, nas_port_type(NasPortType),
			StateData#statedata{request = Request});
identity(#'3gpp_swm_DER'{'EAP-Payload' = EapMessage,
		'RAT-Type' = [RAT]} = Request, StateData) ->
	identity1(EapMessage, RAT, StateData#statedata{request = Request}).
%% @hidden
identity1(EapMessage, RAT,
		#statedata{eap_id = EapID, session_id = SessionId,
		request = Request, auth_req_type = AuthReqType,
		origin_host = OHost, origin_realm = ORealm,
		auc_fsm = AucFsm, id_req = IdReq,
		diameter_port_server = PortServer, keys = Keys} = StateData) ->
	try
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?AKA, identifier = EapID, data = Data} ->
				case ocs_eap_codec:eap_aka(Data) of
					#eap_aka_identity{identity = <<?PERM_AKA,
							PermanentID/binary>> = Identity} when IdReq == full ->
						[IMSI | _] = binary:split(PermanentID, <<$@>>, []),
						gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined, RAT}}),
						NewStateData = StateData#statedata{imsi = IMSI,
								identity = Identity},
						{next_state, vector, NewStateData, ?TIMEOUT};
					#eap_aka_identity{identity = <<?TEMP_AKA:6, _/bits>> = Identity}
							when IdReq == full ->
						[Pseudonym | _] = binary:split(Identity, <<$@>>, []),
						CompressedIMSI = ocs_eap_aka:decrypt_imsi(Pseudonym, Keys),
						IMSI = ocs_eap_aka:compressed_imsi(CompressedIMSI),
						gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, undefined, RAT}}),
						NewStateData = StateData#statedata{imsi = IMSI,
								identity = Identity},
						{next_state, vector, NewStateData, ?TIMEOUT}
				end;
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionId}, StateData}
		end
	catch
		_:Reason ->
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_realm, ORealm},
					{eap_msg, EapMessage}, {error, Reason}]),
			Notification = #eap_aka_notification{notification = 16384},
			Data1 = ocs_eap_codec:eap_aka(Notification),
			EapPacket = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = Data1},
			EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
			NewStateData1 = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					EapMessage1, PortServer, Request, undefined, NewStateData1),
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
vector(timeout, #statedata{session_id = SessionId} = StateData) ->
	{stop, {shutdown, SessionId}, StateData};
vector({ok, {RAND, AUTN, CK, IK, XRES}}, #statedata{eap_id = EapID,
		request = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = RequestAttributes},
		identity = Identity} = StateData) ->
	NextEapID = (EapID rem 255) + 1,
	MK = crypto:hash(sha, [Identity, IK, CK]),
	<<Kencr:16/binary, Kaut:16/binary, MSK:64/binary,
			EMSK:64/binary>> = ocs_eap_aka:prf(MK),
	AkaChallenge = #eap_aka_challenge{mac = <<0:128>>, rand = RAND, autn = AUTN},
	EapData = ocs_eap_codec:eap_aka(AkaChallenge),
	EapPacket = #eap_packet{code = request, type = ?AKA,
			identifier = NextEapID, data = EapData},
	EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
	Mac = ?HMAC_SHA(Kaut, EapMessage1),
	EapMessage2 = ocs_eap_codec:aka_set_mac(Mac, EapMessage1),
	NewStateData = StateData#statedata{request = undefined,
			eap_id = NextEapID, res = XRES, ck = CK, ik = IK,
			msk = MSK, emsk = EMSK, kaut = Kaut, kencr = Kencr},
	send_radius_response(EapMessage2, ?AccessChallenge,
			[], RadiusID, RequestAuthenticator,
			RequestAttributes, undefined, NewStateData),
	{next_state, challenge, NewStateData};
vector({ok, {RAND, AUTN, CK, IK, XRES}}, #statedata{eap_id = EapID,
		request = Request, auth_req_type = AuthReqType,
		diameter_port_server = PortServer,
		session_id = SessionId, identity = Identity} = StateData) ->
	NextEapID = (EapID rem 255) + 1,
	MK = crypto:hash(sha, [Identity, IK, CK]),
	<<Kencr:16/binary, Kaut:16/binary, MSK:64/binary,
			EMSK:64/binary>> = ocs_eap_aka:prf(MK),
	AkaChallenge = #eap_aka_challenge{mac = <<0:128>>, rand = RAND, autn = AUTN},
	EapData = ocs_eap_codec:eap_aka(AkaChallenge),
	EapPacket = #eap_packet{code = request, type = ?AKA,
			identifier = NextEapID, data = EapData},
	EapMessage1 = ocs_eap_codec:eap_packet(EapPacket),
	Mac = ?HMAC_SHA(Kaut, EapMessage1),
	EapMessage2 = ocs_eap_codec:aka_set_mac(Mac, EapMessage1),
	NewStateData = StateData#statedata{request = undefined,
			eap_id = NextEapID, res = XRES, ck = CK, ik = IK,
			msk = MSK, emsk = EMSK, kaut = Kaut, kencr = Kencr},
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			EapMessage2, PortServer, Request, undefined, NewStateData),
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
	send_radius_response(EapMessage, ?AccessChallenge,
			[], RadiusID, RequestAuthenticator,
			RequestAttributes, undefined, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT};
vector({error, user_unknown}, #statedata{request = Request,
		eap_id = EapID, auth_req_type = AuthReqType,
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
			EapMessage, PortServer, Request, undefined, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT};
vector({error, _Reason}, #statedata{request = Request,
		eap_id = EapID, auth_req_type = AuthReqType,
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
			EapMessage, PortServer, Request, undefined, NewStateData),
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
		#statedata{eap_id = EapID, session_id = SessionId,
		secret = Secret, auc_fsm = AucFsm,
		identity = <<?PERM_AKA, _PermanentID/binary>> = Identity,
		imsi = IMSI, msk = MSK, res = RES, kaut = Kaut} = StateData) ->
	NewStateData = StateData#statedata{request = Request,
			radius_fsm = RadiusFsm},
	try
		EapMessage1 = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		#eap_packet{code = response, type = ?AKA, identifier = EapID,
				data = Data1} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data1) of
			#eap_aka_challenge{res = RES, mac = MAC} ->
				EapMessage2 = ocs_eap_codec:aka_clear_mac(EapMessage1),
				case ?HMAC_SHA(Kaut, EapMessage2) of
					MAC ->
						Salt = rand:uniform(16#7fff) + 16#7fff,
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
						Response = {EapMessage3, Attr3},
						NextStateData = NewStateData#statedata{response = Response},
						gen_fsm:send_event(AucFsm, {register, {self(), IMSI}}),
						{next_state, register, NextStateData, ?TIMEOUT};
					_MAC ->
						Notification = #eap_aka_notification{notification = 16384},
						Data2 = ocs_eap_codec:eap_aka(Notification),
						EapPacket1 = #eap_packet{code = request,
								type = ?AKA, identifier = EapID, data = Data2},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						NextStateData = NewStateData#statedata{failure = ?AccessReject},
						send_radius_response(EapMessage3, ?AccessChallenge,
								[], RadiusID, RequestAuthenticator,
								RequestAttributes, undefined, NextStateData),
						{next_state, failure, NextStateData, ?TIMEOUT}
				end;
			#eap_aka_challenge{} ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKA, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?AccessReject},
				send_radius_response(EapMessage3, ?AccessChallenge,
						[], RadiusID, RequestAuthenticator,
						RequestAttributes, undefined, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT};
			#eap_aka_synchronization_failure{auts = AUTS} = _EAP ->
				gen_fsm:send_event(AucFsm, {vector, {self(), IMSI,
						AUTS, get_radius_rat(RequestAttributes)}}),
				{next_state, vector, NewStateData, ?TIMEOUT};
			#eap_aka_authentication_reject{} = _EAP ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKA, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?AccessReject},
				send_radius_response(EapMessage3, ?AccessChallenge,
						[], RadiusID, RequestAuthenticator,
						RequestAttributes, undefined, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT};
			#eap_aka_client_error{client_error_code = _Code} = _EAP->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKA, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?AccessReject},
				send_radius_response(EapMessage3, ?AccessChallenge,
						[], RadiusID, RequestAuthenticator,
						RequestAttributes, undefined, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT}
		end
	catch
		_:Reason ->
			EapMessage4 = get_radius_eap(RequestAttributes),
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{radius_id, RadiusID}, {eap_msg, EapMessage4},
					{error, Reason}]),
			Notification1 = #eap_aka_notification{notification = 16384},
			Data3 = ocs_eap_codec:eap_aka(Notification1),
			EapPacket2 = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = Data3},
			EapMessage5 = ocs_eap_codec:eap_packet(EapPacket2),
			NextStateData1 = NewStateData#statedata{failure = ?AccessReject},
			send_radius_response(EapMessage5, ?AccessChallenge,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, undefined, NextStateData1),
			{next_state, failure, NextStateData1, ?TIMEOUT}
	end;
challenge(#diameter_eap_app_DER{'EAP-Payload' = EapMessage,
		'NAS-Port-Type' = NasPortType} = Request, StateData) ->
	challenge1(EapMessage, Request, nas_port_type(NasPortType), StateData);
challenge(#'3gpp_swm_DER'{'EAP-Payload' = EapMessage,
		'RAT-Type' = [RATType]} = Request, StateData) ->
	challenge1(EapMessage, Request, RATType,StateData);
challenge(#'3gpp_swm_DER'{'EAP-Payload' = EapMessage,
		'RAT-Type' = []} = Request, StateData) ->
	challenge1(EapMessage, Request, 1, StateData).
%% @hidden
challenge1(EapMessage1, Request, RAT,
		#statedata{eap_id = EapID, session_id = SessionId,
		auth_req_type = AuthReqType, origin_host = OHost,
		origin_realm = ORealm, diameter_port_server = PortServer,
		identity = <<?PERM_AKA, _PermanentID/binary>> = _Identity,
		imsi = IMSI, auc_fsm = AucFsm, res = RES, kaut = Kaut} = StateData) ->
	NewStateData = StateData#statedata{request = Request},
	try
		#eap_packet{code = response, type = ?AKA, identifier = EapID,
				data = Data1} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data1) of
			#eap_aka_challenge{res = RES, mac = MAC} ->
				EapMessage2 = ocs_eap_codec:aka_clear_mac(EapMessage1),
				case ?HMAC_SHA(Kaut, EapMessage2) of
					MAC ->
						EapPacket1 = #eap_packet{code = success, identifier = EapID},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						Response = {EapMessage3, []},
						NextStateData = NewStateData#statedata{response = Response},
						gen_fsm:send_event(AucFsm, {register, {self(), IMSI}}),
						{next_state, register, NextStateData};
					_MAC ->
						Notification = #eap_aka_notification{notification = 16384},
						Data2 = ocs_eap_codec:eap_aka(Notification),
						EapPacket1 = #eap_packet{code = request,
								type = ?AKA, identifier = EapID, data = Data2},
						EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
						NextStateData = NewStateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
						send_diameter_response(SessionId, AuthReqType,
								?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
								EapMessage3, PortServer, Request, undefined, NextStateData),
						{next_state, failure, NextStateData, ?TIMEOUT}
				end;
			#eap_aka_challenge{} ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKA, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						EapMessage3, PortServer, Request, undefined, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT};
			#eap_aka_synchronization_failure{auts = AUTS} = _EAP ->
				gen_fsm:send_event(AucFsm, {vector, {self(), IMSI, AUTS, RAT}}),
				{next_state, vector, NewStateData, ?TIMEOUT};
			#eap_aka_authentication_reject{} = _EAP ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKA, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						EapMessage3, PortServer, Request, undefined, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT};
			#eap_aka_client_error{client_error_code = _Code} ->
				Notification = #eap_aka_notification{notification = 16384},
				Data2 = ocs_eap_codec:eap_aka(Notification),
				EapPacket1 = #eap_packet{code = request,
						type = ?AKA, identifier = EapID, data = Data2},
				EapMessage3 = ocs_eap_codec:eap_packet(EapPacket1),
				NextStateData = NewStateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
				send_diameter_response(SessionId, AuthReqType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						EapMessage3, PortServer, Request, undefined, NextStateData),
				{next_state, failure, NextStateData, ?TIMEOUT}
		end
	catch
		_:Reason ->
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_realm, ORealm},
					{eap_msg, EapMessage1}, {error, Reason}]),
			Notification1 = #eap_aka_notification{notification = 16384},
			Data3 = ocs_eap_codec:eap_aka(Notification1),
			EapPacket2 = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = Data3},
			EapMessage4 = ocs_eap_codec:eap_packet(EapPacket2),
			NextStateData1 = NewStateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					EapMessage4, PortServer, Request, undefined, NextStateData1),
			{next_state, failure, NextStateData1, ?TIMEOUT}
	end.

-spec register(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>register</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
register(timeout, #statedata{session_id = SessionId} = StateData)->
	{stop, {shutdown, SessionId}, StateData};
register({ok, #'3gpp_swx_Non-3GPP-User-Data'{} = UserProfile,
		HssRealm, HssHost}, #statedata{session_id = SessionId,
		request = #radius{id = RadiusID,
		authenticator = RequestAuthenticator, attributes = RequestAttributes},
		client_address = ClientAddress, imsi = IMSI, identity = Identity,
		response = {EapMessage, Attributes}} = StateData) ->
	LM = {erlang:system_time(millisecond), erlang:unique_integer([positive])},
	Session = #session{id = SessionId, imsi = IMSI, identity = Identity,
		hss_realm = HssRealm, hss_host = HssHost,
		nas_address = ClientAddress, user_profile = UserProfile,
		last_modified = LM},
	F = fun() -> mnesia:write(session, Session, write) end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			send_radius_response(EapMessage, ?AccessAccept,
					Attributes, RadiusID, RequestAuthenticator,
					RequestAttributes, UserProfile, StateData),
			{stop, {shutdown, SessionId}, StateData};
		{aborted, Reason} ->
			{stop, Reason, StateData}
	end;
register({ok, #'3gpp_swx_Non-3GPP-User-Data'{} = UserProfile,
		HssRealm, HssHost}, #statedata{session_id = SessionId,
		auth_req_type = AuthReqType, diameter_port_server = PortServer,
		nas_host = NasHost, nas_realm = NasRealm,
		request = Request, response = {EapMessage, []},
		imsi = IMSI, identity = Identity} = StateData) ->
	Application = case Request of
		#diameter_eap_app_DER{} ->
			?EAP_APPLICATION_ID;
		#'3gpp_swm_DER'{} ->
			?SWm_APPLICATION_ID
	end,
	LM = {erlang:system_time(millisecond), erlang:unique_integer([positive])},
	Session = #session{id = SessionId, imsi = IMSI, identity = Identity,
		hss_realm = HssRealm, hss_host = HssHost, application = Application,
		nas_host = NasHost, nas_realm = NasRealm,
		user_profile = UserProfile, last_modified = LM},
	F = fun() -> mnesia:write(session, Session, write) end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
					EapMessage, PortServer, Request, UserProfile, StateData),
			{stop, {shutdown, SessionId}, StateData};
		{aborted, Reason} ->
			{stop, Reason, StateData}
	end;
register({error, _Reason}, #statedata{eap_id = EapID,
		request = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator,
		attributes = RequestAttributes}} = StateData) ->
	Notification = #eap_aka_notification{notification = 16384},
	Data = ocs_eap_codec:eap_aka(Notification),
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = Data},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	NewStateData = StateData#statedata{failure = ?AccessReject},
	send_radius_response(EapMessage, ?AccessChallenge,
			[], RadiusID, RequestAuthenticator,
			RequestAttributes, undefined, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT};
register({error, _Reason}, #statedata{request = Request,
		eap_id = EapID, auth_req_type = AuthReqType,
		diameter_port_server = PortServer,
		session_id = SessionId} = StateData) ->
	Notification = #eap_aka_notification{notification = 16384},
	Data = ocs_eap_codec:eap_aka(Notification),
	EapPacket = #eap_packet{code = request,
			type = ?AKA, identifier = EapID, data = Data},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	NewStateData = StateData#statedata{failure = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'},
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			EapMessage, PortServer, Request, undefined, NewStateData),
	{next_state, failure, NewStateData, ?TIMEOUT}.

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
		#eap_packet{code = response, type = ?AKA, identifier = EapID,
				data = Data} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data) of
			#eap_aka_notification{mac = undefined} = _EAP ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				EapMessage2 = ocs_eap_codec:eap_packet(EapPacket1),
				send_radius_response(EapMessage2, RadiusCode,
						[], RadiusID, RequestAuthenticator,
						RequestAttributes, undefined, NewStateData),
				{stop, {shutdown, SessionId}, NewStateData}
			% #eap_aka_notification{mac = MAC} = _EAP ->
			% @todo Handle notification when P=0
		end
	catch
		_:Reason ->
			EapMessage3 = get_radius_eap(RequestAttributes),
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{radius_id, RadiusID}, {eap_msg, EapMessage3},
					{error, Reason}]),
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			EapMessage4 = ocs_eap_codec:eap_packet(EapPacket2),
			send_radius_response(EapMessage4, ?AccessReject,
					[], RadiusID, RequestAuthenticator,
					RequestAttributes, undefined, NewStateData),
			{stop, {shutdown, SessionId}, NewStateData}
	end;
failure(#diameter_eap_app_DER{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	failure(EapMessage, Request, StateData);
failure(#'3gpp_swm_DER'{'EAP-Payload' = EapMessage} = Request,
		StateData) ->
	failure(EapMessage, Request, StateData).
%% @hidden
failure(EapMessage1, Request, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer,
		failure = ResultCode} = StateData) ->
	try
		#eap_packet{code = response, type = ?AKA, identifier = EapID,
				data = Data} = ocs_eap_codec:eap_packet(EapMessage1),
		case ocs_eap_codec:eap_aka(Data) of
			#eap_aka_notification{mac = undefined} = _EAP ->
				EapPacket1 = #eap_packet{code = failure, identifier = EapID},
				EapMessage2 = ocs_eap_codec:eap_packet(EapPacket1),
				send_diameter_response(SessionId, AuthReqType, ResultCode,
						EapMessage2, PortServer, Request, undefined, StateData),
				{stop, {shutdown, SessionId}, StateData}
			% #eap_aka_notification{mac = MAC} = _EAP ->
			% @todo Handle notification when P=0
		end
	catch
		_:Reason ->
			error_logger:warning_report(["EAP-AKA failure",
					{pid, self()}, {session_id, SessionId},
					{origin_host, OHost}, {origin_realm, ORealm},
					{eap_msg, EapMessage1}, {error, Reason}]),
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			EapMessage3 = ocs_eap_codec:eap_packet(EapPacket2),
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					EapMessage3, PortServer, Request, undefined, StateData),
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

-spec send_radius_response(EapMessage, RadiusCode, ResponseAttributes,
		RadiusID, RequestAuthenticator, RequestAttributes,
		UserProfile, StateData) -> ok
	when
		EapMessage :: binary(),
		RadiusCode :: ?AccessChallenge | ?AccessAccept | ?AccessReject,
		ResponseAttributes :: radius_attributes:attributes(),
		RadiusID :: integer(),
		RequestAuthenticator :: [byte()],
		RequestAttributes :: radius_attributes:attributes(),
		UserProfile :: #'3gpp_swx_Non-3GPP-User-Data'{} | undefined,
		StateData :: #statedata{}.
%% @doc Sends a RADIUS Access Challenge, Reject or Accept packet to peer.
%% @hidden
send_radius_response(EapMessage, ?AccessChallenge = RadiusCode,
		ResponseAttributes, RadiusID, RequestAuthenticator,
		_RequestAttributes, undefined, #statedata{secret = Secret,
		radius_fsm = RadiusFsm} = _StateData) ->
	AttrList1 = radius_attributes:add(?EAPMessage,
			EapMessage, ResponseAttributes),
	AttrList2 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, AttrList1),
	Attributes1 = radius_attributes:codec(AttrList2),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = ?HMAC_MD5(Secret,
			[<<RadiusCode, RadiusID, Length:16>>,
			RequestAuthenticator, Attributes1]),
	AttrList3 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttrList2),
	Attributes2 = radius_attributes:codec(AttrList3),
	ResponseAuthenticator = crypto:hash(md5,
			[<<RadiusCode, RadiusID, Length:16>>,
			RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	ResponsePacket = radius:codec(Response),
	radius:response(RadiusFsm, {response, ResponsePacket});
send_radius_response(EapMessage, ?AccessAccept = RadiusCode,
		ResponseAttributes, RadiusID, RequestAuthenticator,
		RequestAttributes, #'3gpp_swx_Non-3GPP-User-Data'{
				'Session-Timeout' = SessionTimeout} = _UserProfile,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		secret = Secret, radius_fsm = RadiusFsm} = _StateData) ->
	AttrList1 = radius_attributes:add(?EAPMessage,
			EapMessage, ResponseAttributes),
	AttrList2 = case SessionTimeout of
		[V1] ->
			radius_attributes:add(?SessionTimeout, V1, AttrList1);
		[] ->
			AttrList1
	end,
	AttrList3 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, AttrList2),
	Attributes1 = radius_attributes:codec(AttrList3),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = ?HMAC_MD5(Secret,
			[<<RadiusCode, RadiusID, Length:16>>,
			RequestAuthenticator, Attributes1]),
	AttrList4 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttrList3),
	Attributes2 = radius_attributes:codec(AttrList4),
	ResponseAuthenticator = crypto:hash(md5,
			[<<RadiusCode, RadiusID, Length:16>>,
			RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	ResponsePacket = radius:codec(Response),
	ok = ocs_log:auth_log(radius, {ServerAddress, ServerPort},
			{ClientAddress, ClientPort}, accept, RequestAttributes, AttrList4),
	radius:response(RadiusFsm, {response, ResponsePacket});
send_radius_response(EapMessage, ?AccessReject = RadiusCode,
		ResponseAttributes, RadiusID, RequestAuthenticator,
		RequestAttributes, undefined,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		secret = Secret, radius_fsm = RadiusFsm} = _StateData) ->
	AttrList1 = radius_attributes:add(?EAPMessage,
			EapMessage, ResponseAttributes),
	AttrList2 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, AttrList1),
	Attributes1 = radius_attributes:codec(AttrList2),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = ?HMAC_MD5(Secret,
			[<<RadiusCode, RadiusID, Length:16>>,
			RequestAuthenticator, Attributes1]),
	AttrList3 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttrList2),
	Attributes2 = radius_attributes:codec(AttrList3),
	ResponseAuthenticator = crypto:hash(md5,
			[<<RadiusCode, RadiusID, Length:16>>,
			RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	ResponsePacket = radius:codec(Response),
	ok = ocs_log:auth_log(radius, {ServerAddress, ServerPort},
			{ClientAddress, ClientPort}, reject, RequestAttributes, AttrList3),
	radius:response(RadiusFsm, {response, ResponsePacket}).

-spec send_diameter_response(SessionId, AuthType, ResultCode,
		EapMessage, PortServer, Request, UserProfile, StateData) -> ok
	when
		SessionId :: diameter:'OctetString'(),
		AuthType :: integer(),
		ResultCode :: integer(),
		EapMessage :: binary(),
		PortServer :: pid(),
		Request :: #diameter_eap_app_DER{} | #'3gpp_swm_DER'{},
		UserProfile :: #'3gpp_swx_Non-3GPP-User-Data'{} | undefined,
		StateData :: #statedata{}.
%% @doc Log DIAMETER event and send appropriate DIAMETER answer to
%% 	ocs_diameter_auth_port_server.
%% @hidden
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', EapMessage,
		PortServer, #diameter_eap_app_DER{} = Request, undefined,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		identity = Identity} = _StateData)
		when is_integer(AuthType), is_binary(OriginHost),
		is_binary(OriginRealm), is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'User-Name' = [Identity]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', EapMessage,
		PortServer, #'3gpp_swm_DER'{} = Request, undefined,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		identity = Identity} = _StateData)
		when is_integer(AuthType), is_binary(OriginHost),
		is_binary(OriginRealm), is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_swm_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'User-Name' = [Identity]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_SUCCESS', EapMessage,
		PortServer, #diameter_eap_app_DER{} = Request, _UserProfile,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		msk = MSK, identity = Identity} = _StateData) when
		is_integer(AuthType), is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer), is_binary(MSK) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'User-Name' = [Identity],
			'EAP-Master-Session-Key' = [MSK]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_BASE_RESULT-CODE_SUCCESS', EapMessage,
		PortServer, #'3gpp_swm_DER'{} = Request,
		#'3gpp_swx_Non-3GPP-User-Data'{'APN-OI-Replacement' = APNOIReplacement,
				'Context-Identifier' = [ContextIdentifier],
				'APN-Configuration' = APNConfiguration,
				'MIP6-Feature-Vector' = MIP6FeatureVector,
				'Trace-Info' = TraceInfo, 'Subscription-Id' = SubscriptionId,
				'Session-Timeout' = SessionTimeout,
				'3GPP-Charging-Characteristics' = ChargingCharacteristics,
				'UE-Usage-Type' = UEUsageType, 'Emergency-Info' = EmergencyInfo,
				'Core-Network-Restrictions' = CoreNetworkRestrictions} = _UserProfile,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		msk = MSK, identity = Identity} = _StateData) when
		is_integer(AuthType), is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer), is_binary(MSK) ->
	Server = {ServerAddress, ServerPort},
	Client = {ClientAddress, ClientPort},
	Answer = #'3gpp_swm_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'User-Name' = [Identity],
			'EAP-Master-Session-Key' = [MSK],
			'APN-OI-Replacement' =  APNOIReplacement,
			'APN-Configuration' = APNConfiguration,
			'MIP6-Feature-Vector' = MIP6FeatureVector,
			'Trace-Info' = TraceInfo, 'Subscription-Id' = SubscriptionId,
			'Session-Timeout' = SessionTimeout,
			'3GPP-Charging-Characteristics' = ChargingCharacteristics,
			'UE-Usage-Type' = UEUsageType, 'Emergency-Info' = EmergencyInfo,
			'Core-Network-Restrictions' = CoreNetworkRestrictions},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType,
		?'DIAMETER_ERROR_USER_UNKNOWN', EapMessage,
		PortServer, #'3gpp_swm_DER'{} = Request, undefined,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		msk = MSK, identity = Identity} = _StateData) when
		is_integer(AuthType), is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer), is_binary(MSK) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_swm_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = #'diameter_base_Experimental-Result'{
					'Vendor-Id' = 10415,
					'Experimental-Result-Code' = ?'DIAMETER_ERROR_USER_UNKNOWN'},
			'Origin-Host' = OriginHost, 'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage], 'User-Name' = [Identity],
			'EAP-Master-Session-Key' = [MSK]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType, ResultCode, EapMessage,
		PortServer, #diameter_eap_app_DER{} = Request, undefined,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		identity = Identity} = _StateData)
		when is_integer(AuthType), is_integer(ResultCode),
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
			'EAP-Payload' = [EapMessage],
			'User-Name' = [Identity]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer});
send_diameter_response(SessionId, AuthType, ResultCode, EapMessage,
		PortServer, #'3gpp_swm_DER'{} = Request, undefined,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		origin_host = OriginHost, origin_realm = OriginRealm,
		identity = Identity} = _StateData)
		when is_integer(AuthType), is_integer(ResultCode),
		is_binary(OriginHost), is_binary(OriginRealm),
		is_binary(EapMessage), is_pid(PortServer) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #'3gpp_swm_DEA'{'Session-Id' = SessionId,
			'Auth-Application-Id' = ?SWm_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
			'Result-Code' = ResultCode,
			'Origin-Host' = OriginHost,
			'Origin-Realm' = OriginRealm,
			'EAP-Payload' = [EapMessage],
			'User-Name' = [Identity]},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}).

%% @hidden
get_radius_eap(RequestAttributes) ->
	case radius_attributes:find(?EAPMessage, RequestAttributes) of
		{ok, Value} ->
			Value;
		{error, not_found} ->
			undefined
	end.

-spec get_radius_rat(Attributes) -> RAT
	when
		Attributes :: radius_attributes:attributes(),
		RAT :: WLAN | Virtual | UTRAN | GERAN | GAN | HSPA | EUTRAN
				| NBIoT | NGRAN | CDMA2000 | HRPD | UMB | EHRPD,
		WLAN :: 0,
		Virtual :: 1,
		UTRAN :: 1000,
		GERAN :: 100,
		GAN :: 1002,
		HSPA :: 1003,
		EUTRAN :: 1004,
		NBIoT :: 1005,
		NGRAN :: 1006,
		CDMA2000 :: 2000,
		HRPD :: 2001,
		UMB :: 2002,
		EHRPD :: 2003.
%% @hidden
get_radius_rat(Attributes) ->
	case radius_attributes:find(?'3GPP-RAT-Type', Attributes) of
		{error, not_found} ->
			case radius_attributes:find(?NasPortType, Attributes) of
				{error, not_found} ->
					1;
				{_, RAT} ->
					nas_port_type(RAT)
			end;
		{_, 1} ->
			1000;
		{_, 2} ->
			100;
		{_, 3} ->
			0;
		{_, 4} ->
			1002;
		{_, 5} ->
			1003;
		{_, 6} ->
			1004;
		{_, 7} ->
			1;
		{_, 8} ->
			1005;
		{_, 9} ->
			1005;
		{_, 51} ->
			10006;
		{_, 102} ->
			2003;
		{_, 103} ->
			2001;
		{_, 104} ->
			2000;
		{_, 105} ->
			2002;
		{_, _} ->
			1
	end.

-spec nas_port_type(NasPortType) -> RAT
	when
		NasPortType :: integer(),
		RAT :: WLAN | Virtual | UTRAN | GERAN | GAN | HSPA | EUTRAN
				| NBIoT | NGRAN | CDMA2000 | HRPD | UMB | EHRPD,
		WLAN :: 0,
		Virtual :: 1,
		UTRAN :: 1000,
		GERAN :: 100,
		GAN :: 1002,
		HSPA :: 1003,
		EUTRAN :: 1004,
		NBIoT :: 1005,
		NGRAN :: 1006,
		CDMA2000 :: 2000,
		HRPD :: 2001,
		UMB :: 2002,
		EHRPD :: 2003.
%% @hidden
nas_port_type(5) ->
	1;
nas_port_type(19) ->
	0;
nas_port_type(22) ->
	2000;
nas_port_type(23) ->
	1000;
nas_port_type(24) ->
	2001;
nas_port_type(_) ->
	1.

