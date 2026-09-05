%%% ocs_eap_ttls_fsm.erl
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
%%% 	module implements the functions associated with a TTLS server within
%%% 	EAP Tunneled Transport Layer Security (EAP-TTLS)
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://www.rfc-editor.org/info/rfc5281/">
%%% 	RFC5281 - EAP Tunneled Transport Layer Security (EAP-TTLS)</a>
%%%
-module(ocs_eap_ttls_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([ssl_start/3, eap_start/3, client_hello/3, server_hello/3,
			client_cipher/3, server_cipher/3, finish/3, client_passthrough/3,
			server_passthrough/3]).

-dialyzer({[nowarn_function, no_contracts, no_return], prf/5}).

%%Macro definitions for TLS record Content Type
-define(ChangeCipherSpec,	20).
-define(Alert,					21).
-define(Handshake,			22).
-define(Application,			23).
-define(Heartbeat,			24).

%%Macro definitions for TLS handshake protocal message type
-define(HelloRequest,			0).
-define(ClientHello,				1).
-define(ServerHello,				2).
-define(NewSessionTicket,		4).
-define(Certificate,				11).
-define(ServerKeyExchange,		12).
-define(CertificateRequest,	13).
-define(ServerHelloDone,		14).
-define(CertificateVerify,		15).
-define(ClientKeyExchange,		16).
-define(Finished,					20).


-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_eap_application_rfc4072.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").

-record(statedata,
		{sup :: pid(),
		aaah_fsm :: undefined | pid(),
		server_address :: inet:ip_address(),
		server_port :: pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		session_id :: binary() | {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		secret :: undefined | secret | binary(),
		eap_id = 0 :: byte(),
		start :: #radius{} | #diameter_eap_app_DER{},
		radius_fsm :: undefined | pid(),
		radius_id :: undefined | byte(),
		req_auth :: undefined | [byte()],
		ssl_socket :: undefined | ssl:sslsocket(),
		socket_options :: undefined | [ssl:ssl_option()],
		max_size :: undefined | pos_integer(),
		rx_length :: undefined | pos_integer(),
		rx_buf = <<>> :: binary(),
		tx_buf = <<>> :: binary(),
		ssl_pid :: undefined | pid(),
		client_rand :: undefined | binary(),
		server_rand :: undefined | binary(),
		tls_key :: string(),
		tls_cert :: string(),
		tls_cacert :: string(),
		app_id :: undefined | integer(),
		auth_req_type :: undefined | integer(),
		origin_host :: undefined | binary(),
		origin_realm :: undefined | binary(),
		port_server :: undefined | pid(),
		password_required :: boolean(),
		trusted :: boolean(),
		service_type :: undefined | integer()}).
-type statedata() :: #statedata{}.
-type state() :: idle.

-define(TIMEOUT, 30000).
-define(BufTIMEOUT, 100).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 23).
		-define(HMAC(Key, Data), crypto:mac(hmac, md5, Key, Data)).
	-else.
		-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
	-endif.
-else.
	-define(HMAC(Key, Data), crypto:hmac(md5, Key, Data)).
-endif.

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_fsm gen_statem call backs
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
init([Sup, radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, Trusted, SessionID,
		#radius{attributes = Attributes} = AccessRequest] = _Args) ->
	{ok, TLSkey} = application:get_env(ocs, tls_key),
	{ok, TLScert} = application:get_env(ocs, tls_cert),
	{ok, TLScacert} = application:get_env(ocs, tls_cacert),
	ServiceType = case radius_attributes:find(?ServiceType, Attributes) of
		{error, not_found} ->
			undefined;
		{_, ST} ->
			ST
	end,
	Data = #statedata{sup = Sup, server_address = ServerAddress,
			server_port = ServerPort, client_address = ClientAddress,
			client_port = ClientPort, radius_fsm = RadiusFsm, secret = Secret,
			session_id = SessionID, start = AccessRequest, tls_key = TLSkey,
			tls_cert = TLScert, tls_cacert = TLScacert,
			password_required = PasswordReq, trusted = Trusted,
			service_type = ServiceType},
	process_flag(trap_exit, true),
	Action = {next_event, internal, start},
	{ok, ssl_start, Data, Action};
init([Sup, diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, Trusted, SessionID, AppId, ReqType, OHost, ORealm,
		_DHost, _DRealm, DiameterRequest, _Options] = _Args) ->
	{ok, TLSkey} = application:get_env(ocs, tls_key),
	{ok, TLScert} = application:get_env(ocs, tls_cert),
	{ok, TLScacert} = application:get_env(ocs, tls_cacert),
	case global:whereis_name({ocs_diameter_auth,
			node(), ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			ServiceType = case DiameterRequest of
				#diameter_nas_app_AAR{'Service-Type' = [ST]} ->
					ST;
				_ ->
					undefined
			end,
			Data = #statedata{sup = Sup, server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionID,
					start = DiameterRequest, tls_key = TLSkey, tls_cert = TLScert,
					tls_cacert = TLScacert, app_id = AppId,
					auth_req_type = ReqType, origin_host = OHost,
					origin_realm = ORealm, port_server = PortServer,
					password_required = PasswordReq, trusted = Trusted,
					service_type = ServiceType},
			process_flag(trap_exit, true),
			Action = {next_event, internal, start},
			{ok, ssl_start, Data, Action}
	end.

-spec ssl_start(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>ssl_start</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
ssl_start(internal = _EventType, start = _EventContent,
		#statedata{start = #radius{code = ?AccessRequest},
				ssl_socket = undefined, sup = Sup,
				tls_key = TLSkey, tls_cert = TLScert,
				tls_cacert = TLScacert} = Data) ->
	Children = supervisor:which_children(Sup),
	{_, AaahFsm, _, _} = lists:keyfind(ocs_eap_ttls_aaah_fsm,
			1, Children),
	Options = [{mode, binary}, {certfile, TLScert},
			{keyfile, TLSkey}, {cacertfile, TLScacert}],
	{ok, SslSocket} = ocs_eap_tls_transport:ssl_listen(self(),
			Options),
	gen_statem:cast(AaahFsm,
			{ttls_socket, self(), SslSocket}),
	NewData = Data#statedata{aaah_fsm = AaahFsm,
			ssl_socket = SslSocket},
	Action = {timeout, ?TIMEOUT, timeout},
	{keep_state, NewData, Action};
ssl_start(internal = _EventType, start = _EventContent,
		#statedata{start = #diameter_eap_app_DER{},
				ssl_socket = undefined, sup = Sup,
				tls_key = TLSkey, tls_cert = TLScert,
				tls_cacert = TLScacert} = Data) ->
	Children = supervisor:which_children(Sup),
	{_, AaahFsm, _, _} = lists:keyfind(ocs_eap_ttls_aaah_fsm,
			1, Children),
	Options = [{mode, binary}, {certfile, TLScert},
			{keyfile, TLSkey}, {cacertfile, TLScacert}],
	{ok, SslSocket} = ocs_eap_tls_transport:ssl_listen(self(),
			Options),
	gen_statem:cast(AaahFsm,
			{ttls_socket, self(), SslSocket}),
	NewData = Data#statedata{aaah_fsm = AaahFsm,
			ssl_socket = SslSocket},
	Action = {timeout, ?TIMEOUT, timeout},
	{keep_state, NewData, Action};
ssl_start(cast = _EventType, {ssl_pid, SslPid}, Data) ->
	NewData = Data#statedata{ssl_pid = SslPid},
	Action = {next_event, internal, start},
	{next_state, eap_start, NewData, Action};
ssl_start(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.

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
		#statedata{start = #radius{code = ?AccessRequest,
				id = RadiusID, authenticator = RequestAuthenticator,
				attributes = Attributes}, radius_fsm = RadiusFsm,
				eap_id = EapID, session_id = SessionID,
				secret = Secret} = Data) ->
	EapTtls = #eap_ttls{start = true},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	NewData = Data#statedata{req_auth = RequestAuthenticator},
	Action = {timeout, ?TIMEOUT, timeout},
	case radius_attributes:find(?EAPMessage, Attributes) of
		{ok, <<>>} ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes,
					Secret, RadiusFsm, NewData),
			{next_state, client_hello, NewData, Action};
		{ok, EAPMessage} ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response,
						type = ?Identity, identifier = StartEapID} ->
					NewEapID = (StartEapID rem 255) + 1,
					NewEapPacket = #eap_packet{code = request,
							type = ?TTLS, identifier = NewEapID,
							data = EapData},
					send_response(NewEapPacket, ?AccessChallenge,
							RadiusID, [], RequestAuthenticator,
							Attributes, Secret, RadiusFsm, NewData),
					NextData = NewData#statedata{eap_id = NewEapID},
					{next_state, client_hello, NextData, Action};
				#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response,
							type = ?LegacyNak, identifier = NewEapID,
							data = <<0>>},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator,
							Attributes, Secret, RadiusFsm, NewData),
					{stop, {shutdown, SessionID}, NewData};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = EapData1} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID},
							{eap_id, NewEapID}, {code, Code},
							{type, EapType}, {data, EapData1}]),
					NewEapPacket = #eap_packet{code = failure,
							identifier = NewEapID},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator,
							Attributes, Secret, RadiusFsm, NewData),
					{stop, {shutdown, SessionID}, NewData};
				{'EXIT', _Reason} ->
					NewEapPacket = #eap_packet{code = failure,
							identifier = EapID},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator,
							Attributes, Secret, RadiusFsm, NewData),
					{stop, {shutdown, SessionID}, NewData}
			end;
		{error, not_found} ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator,
					Attributes, Secret, RadiusFsm, NewData),
			{next_state, client_hello, NewData, Action}
	end;
eap_start(internal = _EventType, start = _EventContent,
		#statedata{start = DiameterRequest, eap_id = EapID,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer} = Data) ->
	EapTtls = #eap_ttls{start = true},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	Action = {timeout, ?TIMEOUT, timeout},
	case DiameterRequest#diameter_eap_app_DER.'EAP-Payload' of
		<<>> ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OH, OR, EapPacket, PortServer, DiameterRequest,
					Data),
			{next_state, client_hello, Data, Action};
		EAPMessage ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response,
						type = ?Identity, identifier = StartEapID} ->
					NewEapID = (StartEapID rem 255) + 1,
					NewEapPacket = #eap_packet{code = request,
							type = ?TTLS, identifier = NewEapID,
							data = EapData},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
							OH, OR, NewEapPacket, PortServer,
							DiameterRequest, Data),
					NextData = Data#statedata{eap_id = NewEapID},
					{next_state, client_hello, NextData, Action};
				#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response,
							type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OH, OR, NewEapPacket, PortServer,
							DiameterRequest, Data),
					{stop, {shutdown, SessionID}};
				#eap_packet{code = Code, type = EapType,
						identifier = NewEapID, data = EapData1} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID},
							{eap_id, NewEapID}, {code, Code},
							{type, EapType}, {data, EapData1}]),
					NewEapPacket = #eap_packet{code = failure,
							identifier = NewEapID},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OH, OR, NewEapPacket, PortServer,
							DiameterRequest, Data),
					{stop, {shutdown, SessionID}};
				{'EXIT', _Reason} ->
					NewEapPacket = #eap_packet{code = failure,
							identifier = EapID},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OH, OR, NewEapPacket, PortServer,
							DiameterRequest, Data),
					{stop, {shutdown, SessionID}}
			end
	end.

-spec client_hello(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>client_hello</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
client_hello(cast = _EventType,
		{ssl_setopts, Options}, Data) ->
	NewData = Data#statedata{socket_options = Options},
	Action = {timeout, ?TIMEOUT, timeout},
	{keep_state, NewData, Action};
client_hello(cast = _EventType,
		{#radius{code = ?AccessRequest, id = RadiusID,
				authenticator = RequestAuthenticator,
				attributes = Attributes}, RadiusFsm},
		#statedata{eap_id = EapID,
				session_id = SessionID, secret = Secret,
				rx_length = RxLength, rx_buf = RxBuf,
				ssl_pid = SslPid} = Data) ->
	EapMessages = radius_attributes:get_all(?EAPMessage, Attributes),
	EapMessage = iolist_to_binary(EapMessages),
	NewData = case {radius_attributes:find(?FramedMtu, Attributes),
			radius_attributes:find(?NasPortType, Attributes)} of
		{{ok, MTU}, {ok, 19}} when MTU > 1496 -> % 802.11
			Data#statedata{max_size = MTU - 4,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{{ok, MTU}, {ok, 19}} when MTU < 1496 -> % 802.11
			Data#statedata{max_size = 1496,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{{ok, MTU}, {ok, 15}} -> % Ethernet
			Data#statedata{max_size = MTU - 4,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{{ok, MTU}, _} -> % Ethernet
			Data#statedata{max_size = MTU,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{_, _} ->
			Data#statedata{max_size = 16#ffff,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator}
	end,
	Action = {timeout, ?TIMEOUT, timeout},
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = EapData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(EapData) of
			#eap_ttls{more = false, start = false, data = TtlsData}
					when RxLength == undefined ->
				CHMsg = <<RxBuf/binary, TtlsData/binary>>,
				NextData = client_hello1(CHMsg, NewData),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextNewData = NextData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_hello, NextNewData, Action};
			#eap_ttls{more = false, start = false, data = TtlsData} ->
				CHMsg = <<RxBuf/binary, TtlsData/binary>>,
				RxLength = size(CHMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextData = NewData#statedata{rx_buf = <<>>,
								rx_length = undefined},
				{next_state, server_hello, NextData, Action};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = TtlsData} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response,
						type = ?TTLS, identifier = NewEapID,
						data = EapData1},
				send_response(EapPacket1, ?AccessChallenge,
						RadiusID, [], RequestAuthenticator,
						Attributes, Secret, RadiusFsm, NewData),
				NextRxBuf = <<RxBuf/binary, TtlsData/binary>>,
				NextData = NewData#statedata{rx_buf = NextRxBuf},
				{keep_state, NextData, Action};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = TtlsData} ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response,
						type = ?TTLS, identifier = NewEapID,
						data = EapData1},
				send_response(EapPacket1, ?AccessChallenge,
						RadiusID, [], RequestAuthenticator,
						Attributes, Secret, RadiusFsm, NewData),
				NextData = NewData#statedata{rx_buf = TtlsData,
						rx_length = MessageLength},
				{keep_state, NextData, Action}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure,
					identifier = EapID},
			send_response(EapPacket2, ?AccessReject, RadiusID,
					[], RequestAuthenticator, Attributes,
					Secret, RadiusFsm, NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
client_hello(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID, session_id = SessionID,
				rx_length = RxLength, rx_buf = RxBuf,
				ssl_pid = SslPid,
				origin_host = OH, origin_realm = OR,
				auth_req_type = AuthType,
				port_server = PortServer} = Data) ->
	{EapMessage, FramedMTU, NasPortType} = get_diameter_attributes(Request),
	NewData = case {FramedMTU, NasPortType} of
		{undefined, undefined} ->
			Data#statedata{max_size = 16#ffff};
		{MTU, 19} when MTU > 1496 -> % 802.11
			Data#statedata{max_size = MTU - 4};
		{MTU, 19} when MTU < 1496 -> % 802.11
			Data#statedata{max_size = 1496};
		{MTU, 15} -> % Ethernet
			Data#statedata{max_size = MTU - 4};
		{MTU, _} -> % Ethernet
			Data#statedata{max_size = MTU}
	end,
	Action = {timeout, ?TIMEOUT, timeout},
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = EapData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(EapData) of
			#eap_ttls{more = false, start = false, data = TtlsData}
					when RxLength == undefined ->
				CHMsg = <<RxBuf/binary, TtlsData/binary>>,
				NextData = client_hello1(CHMsg, NewData),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextNewData = NextData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_hello, NextNewData, Action};
			#eap_ttls{more = false, start = false, data = TtlsData} ->
				CHMsg = <<RxBuf/binary, TtlsData/binary>>,
				RxLength = size(CHMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextData = NewData#statedata{rx_buf = <<>>,
								rx_length = undefined},
				{next_state, server_hello, NextData, Action};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = TtlsData} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = EapData1},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket1, PortServer, Request, NewData),
				NextRxBuf = <<RxBuf/binary, TtlsData/binary>>,
				NextData = NewData#statedata{rx_buf = NextRxBuf},
				{keep_state, NextData, Action};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = TtlsData} ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = EapData1},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket1, PortServer, Request, NewData),
				NextData = NewData#statedata{rx_buf = TtlsData,
						rx_length = MessageLength},
				{keep_state, NextData, Action}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure,
					identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket2, PortServer, Request,
					NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
client_hello(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.
%% @hidden
% TLS Record - <<ContentType, Version:16, Length:16, ProtocolMessage>>
% ProtocolMessage - <<MessageType, Length:24, ClientHelloMessage>>
% RFC 5246 Section 7.4.1.2
% ClientHelloMessage -
% <<ProtocolVersion:16, Gmt_unix_time:32, RandomBytes:28/binary,
%	SessionID, CipherSuite, CompressionMethod, ..>>
client_hello1(<<?Handshake, _Version:16, _L1:16, ?ClientHello, _L2:24,
		_ClientVersion:16, ClientRand:32/binary, _/binary>>, Data) ->
	Data#statedata{client_rand = ClientRand}.

-spec server_hello(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>server_hello</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
% TLS Record - <<ContentType, Version:16, Length:16, ProtocolMessage>>
% ProtocolMessage - <<MessageType, Length:24, ServerHelloMessage>>
% RFC 5246 Section 7.4.1.3
% ServerHelloMessage -
% <<ProtocolVersion:16, Gmt_unix_time:32, RandomBytes:28/binary,
% SessionID, CipherSuite, CompressionMethod, ..>>
server_hello(cast = _EventType,
		{eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
				?ServerHello, _L2:24, _ServerVersion:16,
				ServerRand:32/binary, _/binary>> = TlsData}, Data) ->
	NewData = Data#statedata{server_rand = ServerRand},
	server_hello1(TlsData, NewData);
server_hello(cast = _EventType,
		{eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
				?Certificate, _/binary>> = TlsData}, Data) ->
	server_hello1(TlsData, Data);
server_hello(cast = _EventType,
		{eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
				?ServerKeyExchange, _/binary>> = TlsData}, Data) ->
	server_hello1(TlsData, Data);
server_hello(cast = _EventType,
		{eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
				?ServerHelloDone, _/binary>> = TlsData},
		#statedata{tx_buf = TxBuf} = Data) ->
	NextTxBuf = <<TxBuf/binary, TlsData/binary>>,
	NewData = Data#statedata{tx_buf = NextTxBuf},
	server_hello2([], NewData);
server_hello(cast = _EventType,
		{#radius{code = ?AccessRequest, id = RadiusID,
				authenticator = RequestAuthenticator,
				attributes = Attributes}, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionID,
				secret = Secret} = Data) ->
	NewData = Data#statedata{radius_fsm = RadiusFsm,
      	radius_id = RadiusID, req_auth = RequestAuthenticator},
	EapMessages = radius_attributes:get_all(?EAPMessage, Attributes),
	EapMessage = iolist_to_binary(EapMessages),
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = EapData} = ocs_eap_codec:eap_packet(EapMessage),
		#eap_ttls{more = false, start = false,
				data = <<>>} = ocs_eap_codec:eap_ttls(EapData),
		server_hello2(Attributes, NewData)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, RadiusID, [],
					RequestAuthenticator, Attributes, Secret, RadiusFsm,
					NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
server_hello(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID, session_id = SessionID,
				auth_req_type = AuthType, origin_host = OH,
				origin_realm = OR, port_server = PortServer} = Data) ->
	{EapMessages, _, _} = get_diameter_attributes(Request),
	EapMessage = iolist_to_binary(EapMessages),
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = EapData} = ocs_eap_codec:eap_packet(EapMessage),
		#eap_ttls{more = false, start = false,
				data = <<>>} = ocs_eap_codec:eap_ttls(EapData),
		server_hello2([], Data)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end;
server_hello(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID, tx_buf = <<>>} = _Data) ->
	{stop, {shutdown, SessionID}};
server_hello(timeout = _EventType, timeout = _EventContent, Data) ->
	server_hello2([], Data).
%% @hidden
server_hello1(<<_:24, Length:16, _/binary>> = TlsData,
		#statedata{ssl_pid = SslPid, tx_buf = TxBuf} = Data) ->
	Action = {timeout, ?TIMEOUT, timeout},
	case TlsData of
		<<_:40, _:Length/binary>> = TRLayer ->
			NextTxBuf = <<TxBuf/binary, TRLayer/binary>>,
			NewData = Data#statedata{tx_buf = NextTxBuf},
			{keep_state, NewData, Action};
		<<_:40, _:Length/binary, Rest/binary>> = TRLayer ->
			Size = Length + 5,
			<<Msg:Size/binary, _/binary>>  = TRLayer,
			NextTxBuf = <<TxBuf/binary, Msg/binary>>,
			NewData = Data#statedata{tx_buf = NextTxBuf},
			{keep_state, NewData, Action}
	end.
%% @hidden
server_hello2(RequestAttributes,
		#statedata{start = #radius{}, tx_buf = TxBuf,
				radius_fsm = RadiusFsm, radius_id = RadiusID,
				req_auth = RequestAuthenticator, secret = Secret,
				eap_id = EapID, max_size = MaxSize} = Data) ->
	MaxData = MaxSize - 10,
	NewEapID = (EapID rem 255) + 1,
	Action = {timeout, ?TIMEOUT, timeout},
	case size(TxBuf) of
		Size when Size > MaxData ->
			<<Chunk:MaxData/binary, Rest/binary>> = TxBuf,
			EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge, RadiusID, [],
					RequestAuthenticator, RequestAttributes, Secret,
					RadiusFsm, Data),
			NewData = Data#statedata{eap_id = NewEapID, tx_buf = Rest},
			{keep_state, NewData, Action};
		_Size ->
			EapTtls = #eap_ttls{data = TxBuf},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge, RadiusID,
					[], RequestAuthenticator, RequestAttributes,
					Secret, RadiusFsm, Data),
			NewData = Data#statedata{eap_id = NewEapID, tx_buf = <<>>},
			{next_state, client_cipher, NewData, Action}
	end;
server_hello2(_,
		#statedata{start = #diameter_eap_app_DER{},
				tx_buf = TxBuf, eap_id = EapID, max_size = MaxSize,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer} = Data) ->
	MaxData = MaxSize - 10,
	NewEapID = (EapID rem 255) + 1,
	Action = {timeout, ?TIMEOUT, timeout},
	case size(TxBuf) of
		Size when Size > MaxData ->
			<<Chunk:MaxData/binary, Rest/binary>> = TxBuf,
			EapTtls = #eap_ttls{more = true,
					message_len = Size, data = Chunk},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OH, OR, EapPacket, PortServer,
					#diameter_eap_app_DER{}, Data),
			NewData = Data#statedata{eap_id = NewEapID, tx_buf = Rest},
			{keep_state, NewData, Action};
		_Size ->
			EapTtls = #eap_ttls{data = TxBuf},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OH, OR, EapPacket, PortServer,
					#diameter_eap_app_DER{}, Data),
			NewData = Data#statedata{eap_id = NewEapID, tx_buf = <<>>},
			{next_state, client_cipher, NewData, Action}
	end.

-spec client_cipher(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>client_cipher</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
client_cipher(cast = _EventType,
		{#radius{code = ?AccessRequest, id = RadiusID,
				authenticator = RequestAuthenticator,
				attributes = Attributes}, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionID,
				secret = Secret, rx_length = RxLength, rx_buf = RxBuf,
				ssl_pid = SslPid} = Data) ->
	EapMessages = radius_attributes:get_all(?EAPMessage, Attributes),
	EapMessage = iolist_to_binary(EapMessages),
	NewData = Data#statedata{radius_fsm = RadiusFsm,
		radius_id = RadiusID, req_auth = RequestAuthenticator},
	Action = {timeout, ?TIMEOUT, timeout},
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = EapData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(EapData) of
			#eap_ttls{more = false, start = false, data = TtlsData}
					when RxLength == undefined ->
				CCMsg = <<RxBuf/binary, TtlsData/binary>>,
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextData = NewData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextData};
			#eap_ttls{more = false, start = false, data = TtlsData} ->
				CCMsg = <<RxBuf/binary, TtlsData/binary>>,
				RxLength = size(CCMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextData = NewData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextData};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = TtlsData} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = EapData1},
				CCMsg = <<RxBuf/binary, TtlsData/binary>>,
				send_response(EapPacket1, ?AccessChallenge, RadiusID,
						[], RequestAuthenticator, Attributes, Secret,
						RadiusFsm, NewData),
				NextData = NewData#statedata{rx_buf = CCMsg},
				{keep_state, NextData, Action};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = TtlsData} ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = EapData1},
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes,
					Secret, RadiusFsm, NewData),
				NextData = NewData#statedata{rx_buf = TtlsData,
						rx_length = MessageLength},
				{keep_state, NextData, Action}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket2, ?AccessReject, RadiusID,
					[], RequestAuthenticator, Attributes, Secret,
					RadiusFsm, NewData),
			{stop, {shutdown, SessionID}, NewData}
	end;
client_cipher(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID, session_id = SessionID,
				rx_length = RxLength, rx_buf = RxBuf,
				ssl_pid = SslPid, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer} = Data) ->
	EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
	Action = {timeout, ?TIMEOUT, timeout},
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = EapData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(EapData) of
			#eap_ttls{more = false, start = false, data = TtlsData}
					when RxLength == undefined ->
				CCMsg = <<RxBuf/binary, TtlsData/binary>>,
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextData = Data#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextData};
			#eap_ttls{more = false, start = false, data = TtlsData} ->
				CCMsg = <<RxBuf/binary, TtlsData/binary>>,
				RxLength = size(CCMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextData = Data#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextData};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = TtlsData} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = EapData1},
				CCMsg = <<RxBuf/binary, TtlsData/binary>>,
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket1, PortServer, Request, Data),
				NextData = Data#statedata{rx_buf = CCMsg},
				{keep_state, NextData, Action};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = TtlsData} ->
				NewEapID = (EapID rem 255) + 1,
				EapData1 = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = EapData1},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket1, PortServer, Request, Data),
				NextData = Data#statedata{rx_buf = TtlsData,
						rx_length = MessageLength},
				{keep_state, NextData, Action}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket2, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end;
client_cipher(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.

-spec server_cipher(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>server_cipher</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
server_cipher(cast = _EventType,
		{eap_tls, SslPid, <<?ChangeCipherSpec, _/binary>> = TlsData},
		Data) ->
	NewData = Data#statedata{ssl_pid = SslPid},
	server_cipher1(TlsData, NewData);
server_cipher(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.
%% @hidden
server_cipher1(<<_:24, Length:16, _/binary>> = TlsData,
		#statedata{ssl_pid = SslPid, tx_buf = Buf} = Data) ->
	case TlsData of
		<<_:40, _:Length/binary>> = SC ->
			TxBuf = <<Buf/binary, SC/binary>>,
			NewData = Data#statedata{tx_buf = TxBuf},
			{next_state, finish, NewData};
		<<_:40, _:Length/binary, Rest/binary>> = SC ->
			Size = Length + 5,
			<<Msg:Size/binary, _/binary>> = SC,
			TxBuf = <<Buf/binary, Msg/binary>>,
			NewData = Data#statedata{tx_buf = TxBuf},
			Action = {next_event, cast, {eap_tls, SslPid, Rest}},
			{next_state, finish, NewData, Action}
	end.

-spec finish(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>finish</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
finish(cast = _EventType,
		{eap_tls, _SslPid, <<?Handshake, _/binary>> = TlsData},
		#statedata{tx_buf = TxBuf, start = #radius{},
				radius_id = RadiusID, radius_fsm = RadiusFsm,
				req_auth = RequestAuthenticator, secret = Secret,
				eap_id = EapID} = Data) ->
	NewEapID = (EapID rem 255) + 1,
	BinData = <<TxBuf/binary, TlsData/binary>>,
	EapTtls = #eap_ttls{data = BinData},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = request, type = ?TTLS,
			identifier = NewEapID, data = EapData},
	send_response(EapPacket, ?AccessChallenge,
			RadiusID, [], RequestAuthenticator,
			[], Secret, RadiusFsm, Data),
	NewData = Data#statedata{eap_id = NewEapID, tx_buf = <<>>},
	{next_state, client_passthrough, NewData};
finish(cast = _EventType,
		{eap_tls, _SslPid, <<?Handshake, _/binary>> = TlsData},
		#statedata{tx_buf = TxBuf,
				start = #diameter_eap_app_DER{}, eap_id = EapID,
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer} = Data) ->
	NewEapID = (EapID rem 255) + 1,
	BinData = <<TxBuf/binary, TlsData/binary>>,
	EapTtls = #eap_ttls{data = BinData},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = request, type = ?TTLS,
			identifier = NewEapID, data = EapData},
	send_diameter_response(SessionID, AuthType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
			OH, OR, EapPacket, PortServer,
			#diameter_eap_app_DER{}, Data),
	NewData = Data#statedata{eap_id = NewEapID, tx_buf = <<>>},
	{next_state, client_passthrough, NewData};
finish(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.

-spec client_passthrough(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>client_passthrough</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
client_passthrough(cast = _EventType,
		{#radius{code = ?AccessRequest, id = RadiusID,
				authenticator = RequestAuthenticator,
				attributes = Attributes}, RadiusFsm},
		#statedata{eap_id = EapID, session_id = SessionID,
				secret = Secret, ssl_pid = SslPid} = Data) ->
	NewData = Data#statedata{req_auth = RequestAuthenticator,
			radius_fsm = RadiusFsm, radius_id = RadiusID},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, Attributes),
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response,
					identifier = EapID, data = EapData} ->
				#eap_ttls{data = TtlsData} = ocs_eap_codec:eap_ttls(EapData),
				ocs_eap_tls_transport:deliver(SslPid, self(), TtlsData),
				{next_state, server_passthrough, NewData};
			#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response,
							type = ?LegacyNak, identifier = NewEapID,
							data = <<0>>},
					send_response(NewEapPacket, ?AccessReject, RadiusID,
							[], RequestAuthenticator, Attributes, Secret,
							RadiusFsm, NewData),
					{stop, {shutdown, SessionID}, NewData};
			#eap_packet{code = Code, type = EapType,
					identifier = NewEapID, data = EapData} ->
				error_logger:warning_report(["Unknown EAP received",
						{pid, self()}, {session_id, SessionID},
						{eap_id, NewEapID}, {code, Code},
						{type, EapType}, {data, EapData}]),
				NewEapPacket = #eap_packet{code = failure,
						identifier = NewEapID},
				send_response(NewEapPacket, ?AccessReject, RadiusID,
						[], RequestAuthenticator, Attributes, Secret,
						RadiusFsm, NewData),
				{stop, {shutdown, SessionID}, NewData}
		end
	catch
		_:_ ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket1, ?AccessReject, RadiusID,
					[], RequestAuthenticator, Attributes, Secret,
					RadiusFsm, Data),
				{stop, {shutdown, SessionID}}
	end;
client_passthrough(cast = _EventType,
		#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID, session_id = SessionID,
				ssl_pid = SslPid, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer} = Data) ->
	try
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response,
					identifier = EapID, data = EapData} ->
				#eap_ttls{data = TtlsData} = ocs_eap_codec:eap_ttls(EapData),
				ocs_eap_tls_transport:deliver(SslPid, self(), TtlsData),
				{next_state, server_passthrough, Data};
			#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response,
							type = ?LegacyNak, identifier = NewEapID,
							data = <<0>>},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
							OH, OR, NewEapPacket, PortServer, Request, Data),
					{stop, {shutdown, SessionID}};
			#eap_packet{code = Code, type = EapType,
					identifier = NewEapID, data = EapData} ->
				error_logger:warning_report(["Unknown EAP received",
						{pid, self()}, {session_id, SessionID},
						{eap_id, NewEapID}, {code, Code},
						{type, EapType}, {data, EapData}]),
				NewEapPacket = #eap_packet{code = failure,
						identifier = NewEapID},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
						OH, OR, NewEapPacket, PortServer, Request, Data),
				{stop, {shutdown, SessionID}}
		end
	catch
		_:_ ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket1, PortServer, Request, Data),
			{stop, {shutdown, SessionID}}
	end;
client_passthrough(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.

-spec server_passthrough(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>server_passthrough</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
server_passthrough(cast = _EventType,
		{accept, #service{name = Identity, password = Password}, SslSocket},
		#statedata{eap_id = EapID,
				start = #radius{attributes = RequestAttributes},
				session_id = SessionID, secret = Secret,
				req_auth = RequestAuthenticator, radius_fsm = RadiusFsm,
				radius_id = RadiusID, %ssl_socket = SslSocket,
				client_rand = ClientRandom, server_rand = ServerRandom,
				service_type = ServiceType} = Data) ->
	Timestamp = calendar:local_time(),
	CallAddress = proplists:get_value(?CalledStationId,
			RequestAttributes, ""),
	SessionAttributes = ocs_rating:session_attributes(RequestAttributes),
	case ocs_rating:authorize(radius,
			ServiceType, [Identity], Password, Timestamp,
			CallAddress, undefined, SessionAttributes) of
		{authorized, _Subscriber, Attributes, _ExistingSessionAttributes} ->
			Seed = [<<ClientRandom/binary, ServerRandom/binary>>],
			{MSK, _} = prf(SslSocket, master_secret ,
					<<"ttls keying material">>, Seed, 128),
			UserName = binary_to_list(Identity),
			Salt = rand:uniform(16#7fff) + 16#7fff,
			<<MSK1:32/binary, MSK2:32/binary>> = MSK,
			MsMppeRecvKey = encrypt_key(Secret, RequestAuthenticator, Salt, MSK1),
			MsMppeSendKey = encrypt_key(Secret, RequestAuthenticator, Salt, MSK2),
			Attr1 = radius_attributes:store(?UserName, UserName, Attributes),
			Attr2 = radius_attributes:store(?Microsoft,
					?MsMppeRecvKey, {Salt, MsMppeRecvKey}, Attr1),
			Attr3 = radius_attributes:store(?Microsoft,
					?MsMppeSendKey, {Salt, MsMppeSendKey}, Attr2),
			EapPacket = #eap_packet{code = success, identifier = EapID},
			send_response(EapPacket, ?AccessAccept, RadiusID, Attr3,
					RequestAuthenticator, [], Secret, RadiusFsm, Data),
			{stop, {shutdown, SessionID}};
		{unauthorized, disabled, _ExistingSessionAttributes} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, RadiusID, [],
					RequestAuthenticator, [], Secret, RadiusFsm, Data),
			{stop, {shutdown, SessionID}};
		{unauthorized, _Reason, _ExistingSessionAttributes} ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject, RadiusID, [],
					RequestAuthenticator, [], Secret, RadiusFsm, Data),
			{stop, {shutdown, SessionID}}
	end;
server_passthrough(cast = _EventType, reject,
		#statedata{eap_id = EapID, session_id = SessionID,
				secret = Secret, start = #radius{},
				req_auth = RequestAuthenticator, radius_fsm = RadiusFsm,
				radius_id = RadiusID} = Data) ->
	EapPacket = #eap_packet{code = failure, identifier = EapID},
	send_response(EapPacket, ?AccessReject, RadiusID, [],
			RequestAuthenticator, [], Secret, RadiusFsm, Data),
	{stop, {shutdown, SessionID}};
server_passthrough(cast = _EventType,
		{accept, _UserName, SslSocket},
		#statedata{eap_id = EapID, start = #diameter_eap_app_DER{},
				session_id = SessionID, auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer, client_rand = ClientRandom,
				server_rand = ServerRandom} = _Data) ->
	Seed = [<<ClientRandom/binary, ServerRandom/binary>>],
	{MSK, _} = prf(SslSocket, master_secret,
			<<"ttls keying material">>, Seed, 128),
	EapPacket = #eap_packet{code = success, identifier = EapID},
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionID,
				'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapMessage],
				'EAP-Master-Session-Key' = [MSK]},
	gen_server:cast(PortServer, {self(), Answer}),
	{stop, {shutdown, SessionID}};
server_passthrough(cast = _EventType, reject,
		#statedata{eap_id = EapID, session_id = SessionID,
				start = #diameter_eap_app_DER{},
	 			auth_req_type = AuthType,
				origin_host = OH, origin_realm = OR,
				port_server = PortServer} = Data) ->
	EapPacket = #eap_packet{code = failure, identifier = EapID},
	send_diameter_response(SessionID, AuthType,
			?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			OH, OR, EapPacket, PortServer, #diameter_eap_app_DER{},
			Data),
	{stop, {shutdown, SessionID}};
server_passthrough(timeout = _EventType, timeout = _EventContent,
		#statedata{session_id = SessionID} = _Data) ->
	{stop, {shutdown, SessionID}}.

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

-spec send_response(EapPacket, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm,
		Data) -> ok
	when
		EapPacket :: #eap_packet{},
		RadiusCode :: integer(), 
		RadiusID :: byte(),
		ResponseAttributes :: radius_attributes:attributes(),
		RequestAuthenticator :: binary() | [byte()], 
		RequestAttributes :: radius_attributes:attributes(),
		Secret :: binary(),
		RadiusFsm :: pid(),
		Data :: statedata().
%% @doc Sends a RADIUS-Access/Challenge, Reject or Accept packet to peer.
%% @hidden
send_response(#eap_packet{} = EapPacket, RadiusCode, RadiusID,
		ResponseAttributes, RequestAuthenticator, RequestAttributes,
		Secret, RadiusFsm, Data) ->
	EapMessage = ocs_eap_codec:eap_packet(EapPacket),
	send_response1(EapMessage, RadiusCode, RadiusID,
		ResponseAttributes, RequestAuthenticator, RequestAttributes,
		Secret, RadiusFsm, Data).
%% @hidden
send_response1(<<Chunk:253/binary, Rest/binary>>, RadiusCode,
		RadiusID, ResponseAttributes, RequestAuthenticator,
		RequestAttributes, Secret, RadiusFsm, Data) ->
	AttrList1 = radius_attributes:add(?EAPMessage,
			Chunk, ResponseAttributes),
	send_response1(Rest, RadiusCode, RadiusID, AttrList1,
		RequestAuthenticator,  RequestAttributes,
		Secret, RadiusFsm, Data);
send_response1(<<>>, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret,
		RadiusFsm, Data) ->
	send_response2(RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret,
		RadiusFsm, Data);
send_response1(Chunk, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret,
		RadiusFsm, Data) when is_binary(Chunk) ->
	AttrList1 = radius_attributes:add(?EAPMessage,
			Chunk, ResponseAttributes),
	send_response2(RadiusCode, RadiusID, AttrList1,
			RequestAuthenticator, RequestAttributes, Secret,
			RadiusFsm, Data).
%% @hidden
send_response2(RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm,
		#statedata{server_address = ServerAddress,
				server_port = ServerPort,
				client_address = ClientAddress,
				client_port = ClientPort} = _Data) ->
	AttrList2 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, ResponseAttributes),
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

%% ssl:prf/5 includes an incorrect type specification for Seed!
-spec prf(SslSocket, Secret, Label, Seed, WantedLength) ->
	{ok, MSK, EMSK} | {error, Reason} when
		SslSocket :: ssl:sslsocket(),
		Secret :: binary() | master_secret,
		Label :: binary(),
		Seed :: [binary() | ssl:prf_random()],
		WantedLength :: non_neg_integer(),
		MSK :: binary(),
		EMSK :: binary(),
		Reason :: term().
%% @doc Use the Pseudo-Random Function (PRF) of a TLS session
%%	to generate extra key material.
prf(SslSocket, Secret, Label, Seed, WantedLength) when is_list(Seed) ->
	case catch ssl:prf(SslSocket, Secret, Label, Seed, WantedLength) of
		{ok, <<MSK:64/binary, EMSK:64/binary>>} ->
			{MSK, EMSK};
		{'EXIT', _Reason} -> % fake dialyzer out
			{<<0:512>>, <<0:512>>}
	end.

-spec get_diameter_attributes(Packet) -> Result
	when
		Packet :: #diameter_eap_app_DER{},
		Result :: {EapPacket, FramedMTU, NasPortType},
		EapPacket :: binary(),
		FramedMTU :: undefined | integer(),
		NasPortType :: undefined | integer().
get_diameter_attributes(Packet) ->
	EapPacket = Packet#diameter_eap_app_DER.'EAP-Payload',
	FramedMTU = try
		[MTU] = Packet#diameter_eap_app_DER.'Framed-MTU',
		MTU
	catch
		_:_ ->
			undefined
	end,
	NasPortType = try
		[NPT] = Packet#diameter_eap_app_DER.'NAS-Port-Type',
		NPT
	catch
		_:_ ->
			undefined
	end,
	{EapPacket, FramedMTU, NasPortType}.

-spec send_diameter_response(SessionID, AuthType, ResultCode, OH, OR,
		EapPacket, PortServer, Request, Data) -> ok
	when
		SessionID :: string() | binary(),
		AuthType :: integer(),
		ResultCode :: integer(),
		OH :: binary(),
		OR :: binary(),
		EapPacket :: #eap_packet{},
		PortServer :: pid(),
		Request :: #diameter_eap_app_DER{},
		Data :: statedata().
%% @doc Send appropriate DIAMETER answer to ocs_diameter_auth_port_server.
%% @hidden
send_diameter_response(SId, AuthType, ResultCode, OH, OR, EapPacket,
		PortServer, Request, #statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _Data) ->
	try
		EapMessage = ocs_eap_codec:eap_packet(EapPacket),
		Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = 5,
				'Auth-Request-Type' = AuthType,
				'Result-Code' = ResultCode,
				'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapMessage]},
		ok = ocs_log:auth_log(diameter, {ServerAddress, ServerPort},
				{ClientAddress, ClientPort}, Request, Answer),
		gen_server:cast(PortServer, {self(), Answer})
	catch
		_:_ ->
		Answer1 = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = 5,
				'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				'Origin-Host' = OH, 'Origin-Realm' = OR},
		ok = ocs_log:auth_log(diameter, {ServerAddress, ServerPort},
				{ClientAddress, ClientPort}, Request, Answer1),
		gen_server:cast(PortServer, {self(), Answer1})
	end.

