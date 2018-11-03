%%% ocs_eap_aka_fsm.erl
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
%%%
-module(ocs_eap_aka_fsm).
-copyright('Copyright (c) 2016 - 2018 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_aka_fsm API
-export([]).

%% export the ocs_eap_aka_fsm state callbacks
-export([eap_start/2, identity/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("radius/include/radius.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").
-include("ocs_eap_codec.hrl").

-record(statedata,
		{sup :: pid(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		radius_fsm :: undefined | pid(),
		auc_fsm :: undefined | pid(),
		session_id:: string() | {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		start :: undefined | #diameter_eap_app_DER{} | #radius{},
		secret :: undefined | binary(),
		eap_id = 0 :: byte(),
		server_id  ::  binary(),
		auth_app_id :: undefined | integer(),
		auth_req_type :: undefined | integer(),
		origin_host :: undefined | binary(),
		origin_realm :: undefined | binary(),
		diameter_port_server :: undefined | pid(),
		password_required :: boolean(),
		service_type :: undefined | integer()}).
-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).

-define(EAP_APPLICATION_ID, 5).

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_aka_fsm gen_fsm call backs
%%----------------------------------------------------------------------

init([Sup, radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, SessionID,
		#radius{attributes = Attributes} = AccessRequest] = _Args) ->
	{ok, Hostname} = inet:gethostname(),
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
			secret = Secret, session_id = SessionID,
			server_id = list_to_binary(Hostname), start = AccessRequest,
			password_required = PasswordReq,
			service_type = ServiceType},
	process_flag(trap_exit, true),
	{ok, eap_start, StateData, 0};
init([Sup, diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, SessionId, ApplicationId, AuthReqType, OHost, ORealm,
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
			StateData = #statedata{sup = Sup,
					server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionId,
					server_id = list_to_binary(Hostname), auth_app_id = ApplicationId,
					auth_req_type = AuthReqType, origin_host = OHost,
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
eap_start(timeout, #statedata{eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		start = #diameter_eap_app_DER{'EAP-Payload' = []} = Request,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer} = StateData) ->
	EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{any_id_req = true}),
	EapPacket = #eap_packet{code = request, type = ?AKA,
			identifier = EapID, data = EapData},
	send_diameter_response(SessionId, AuthReqType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
			EapPacket, PortServer, Request, StateData),
	{next_state, identity, StateData, ?TIMEOUT};
eap_start(timeout, #statedata{sup = Sup, eap_id = EapID,
		session_id = SessionId, auth_req_type = AuthReqType,
		start = #diameter_eap_app_DER{'EAP-Payload' = EapMessage} = Request,
		origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	NewStateData = StateData#statedata{start = undeined, auc_fsm = AucFsm},
	EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{any_id_req = true}),
	case catch ocs_eap_codec:eap_packet(EapMessage) of
		#eap_packet{code = response, type = ?Identity, identifier = NewEapID} ->
			NextEapID = (NewEapID rem 255) + 1,
			EapPacket = #eap_packet{code = request, type = ?AKA,
					identifier = NextEapID, data = EapData},
			NextStateData = NewStateData#statedata{eap_id = NextEapID},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OHost, ORealm,
					EapPacket, PortServer, Request, NextStateData),
			{next_state, identity, NextStateData, ?TIMEOUT};
		#eap_packet{code = request, identifier = NewEapID} ->
			EapPacket = #eap_packet{code = response, type = ?LegacyNak,
					identifier = NewEapID, data = <<0>>},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
					EapPacket, PortServer, Request, NewStateData),
			{stop, {shutdown, SessionId}, NewStateData};
		#eap_packet{code = Code, type = EapType,
				identifier = NewEapID, data = Data} ->
			error_logger:warning_report(["Unknown EAP received",
					{pid, self()}, {session_id, SessionId}, {code, Code},
					{type, EapType}, {identifier, NewEapID}, {data, Data}]),
			EapPacket = #eap_packet{code = failure, identifier = NewEapID},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
					EapPacket, PortServer, Request, NewStateData),
			{stop, {shutdown, SessionId}, NewStateData};
		{'EXIT', _Reason} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionId, AuthReqType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost, ORealm,
					EapPacket, PortServer, Request, NewStateData),
			{stop, {shutdown, SessionId}, NewStateData}
	end;
%% @hidden
eap_start(timeout, #statedata{sup = Sup, eap_id = EapID,
		start = #radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = RequestAttributes},
		session_id = SessionID} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AucFsm, _, _} = lists:keyfind(ocs_eap_aka_auc_fsm, 1, Children),
	NewStateData = StateData#statedata{start = undefined, auc_fsm = AucFsm},
	case radius_attributes:find(?EAPMessage, RequestAttributes) of
		{ok, <<>>} ->
			EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{any_id_req = true}),
			EapPacket = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = EapData},
			send_radius_response(EapPacket, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{next_state, identity, NewStateData, ?TIMEOUT};
		{ok, EAPMessage} ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response, type = ?Identity,
						identifier = NewEapID} ->
					NextEapID = (NewEapID rem 255) + 1,
					EapData = ocs_eap_codec:eap_aka(#eap_aka_challenge{any_id_req = true}),
					% EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{any_id_req = true}),
					EapPacket = #eap_packet{code = request,
							type = ?AKA, identifier = NextEapID, data = EapData},
					send_radius_response(EapPacket, ?AccessChallenge, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					NextStateData = NewStateData#statedata{eap_id = NextEapID},
					{next_state, identity, NextStateData, ?TIMEOUT};
				#eap_packet{code = request, identifier = NewEapID} ->
					EapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_radius_response(EapPacket, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionID}, StateData};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = Data} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID}, {code, Code},
							{type, EapType}, {identifier, NewEapID}, {data, Data}]),
					EapPacket = #eap_packet{code = failure, identifier = NewEapID},
					send_radius_response(EapPacket, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionID}, StateData};
				{'EXIT', _Reason} ->
					EapPacket = #eap_packet{code = failure, identifier = EapID},
					send_radius_response(EapPacket, ?AccessReject, [], RadiusID,
							RequestAuthenticator, RequestAttributes, NewStateData),
					{stop, {shutdown, SessionID}, StateData}
			end;
		{error, not_found} ->
					EapData = ocs_eap_codec:eap_aka(#eap_aka_identity{any_id_req = true}),
			EapPacket = #eap_packet{code = request,
					type = ?AKA, identifier = EapID, data = EapData},
			send_radius_response(EapPacket, ?AccessChallenge, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{next_state, identity, NewStateData, ?TIMEOUT}
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
identity(timeout, #statedata{session_id = SessionID} = StateData)->
	{stop, {shutdown, SessionID}, StateData};
identity({#radius{id = RadiusID, authenticator = RequestAuthenticator,
		attributes = RequestAttributes} = AccessRequest, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionID} = StateData) ->
	NewStateData = StateData#statedata{radius_fsm = RadiusFsm},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, RequestAttributes),
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?AKA,
					identifier = EapID, data = Data} ->
				case ocs_eap_codec:eap_aka(Data) of
					#eap_aka_identity{identity = Identity} ->
{stop, {shutdown, SessionID}, NewStateData}
				end;
			#eap_packet{code = response,
					type = ?LegacyNak, identifier = EapID} ->
				{stop, {shutdown, SessionID}, NewStateData}
		end
	catch
		_:_Reason ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_radius_response(EapPacket, ?AccessReject, [], RadiusID,
					RequestAuthenticator, RequestAttributes, NewStateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
identity(#diameter_eap_app_DER{'EAP-Payload' = EapMessage} = Request,
		#statedata{eap_id = EapID, session_id = SessionID,
		auth_req_type = RequestType, origin_host = OHost,
		origin_realm = ORealm, diameter_port_server = PortServer} = StateData) ->
	try
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, type = ?AKA,
					identifier = EapID, data = Data} ->
{stop, {shutdown, SessionID}, StateData};
			#eap_packet{code = response, type = ?LegacyNak, identifier = EapID} ->
				send_diameter_response(SessionID, RequestType,
					?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS', OHost, ORealm,
					none, PortServer, Request, StateData),
				{stop, {shutdown, SessionID}, StateData}
		end
	catch
		_:_Reason ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, RequestType,
					?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS', OHost, ORealm,
					EapPacket, PortServer, Request, StateData),
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

-spec send_radius_response(EapPacket, RadiusCode, ResponseAttributes, RadiusID,
		RequestAuthenticator, RequestAttributes, StateData) -> ok
	when
		EapPacket :: #eap_packet{},
		RadiusCode :: integer(),
		ResponseAttributes :: radius_attributes:attributes(),
		RadiusID :: integer(),
		RequestAuthenticator :: [byte()],
		RequestAttributes :: radius_attributes:attributes(),
		StateData :: #statedata{}.
%% @doc Sends a RADIUS Access/Challenge, Reject or Accept packet to peer.
%% @hidden
send_radius_response(EapPacket, RadiusCode, ResponseAttributes,
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
			'Auth-Application-Id' = ?EAP_APPLICATION_ID,
			'Auth-Request-Type' = AuthType,
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

