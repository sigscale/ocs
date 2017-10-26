%%% ocs_eap_ttls_fsm.erl
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback module
%%% 	implements the functions associated with a TTLS server within EAP
%%% 	Tunneled Transport Layer Security (EAP-TTLS)
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/rfc/rfc5281.txt">
%%% 	RFC5281 - EAP Tunneled Transport Layer Security (EAP-TTLS)</a>
%%%
-module(ocs_eap_ttls_fsm).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_ttls_fsm API
-export([]).

%% export the ocs_eap_ttls_fsm state callbacks
-export([ssl_start/2, eap_start/2, client_hello/2, server_hello/2,
			client_cipher/2, server_cipher/2, finish/2, client_passthrough/2,
			server_passthrough/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

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
-include("../include/diameter_gen_eap_application_rfc4072.hrl").

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
		server_id :: binary(),
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
		port_server :: undefined | pid()}).

-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).
-define(BufTIMEOUT, 100).

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData}
		| {ok, StateName, StateData, Timeout}
		| {ok, StateName, StateData, hibernate}
		| {stop, Reason} | ignore,
		StateName :: atom(),
		StateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([Sup, radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, SessionID, AccessRequest] = _Args) ->
	{ok, TLSkey} = application:get_env(ocs, tls_key),
	{ok, TLScert} = application:get_env(ocs, tls_cert),
	{ok, TLScacert} = application:get_env(ocs, tls_cacert),
	{ok, Hostname} = inet:gethostname(),
	StateData = #statedata{sup = Sup, server_address = ServerAddress,
			server_port = ServerPort, client_address = ClientAddress,
			client_port = ClientPort, radius_fsm = RadiusFsm, secret = Secret,
			session_id = SessionID, server_id = list_to_binary(Hostname),
			start = AccessRequest, tls_key = TLSkey, tls_cert = TLScert,
			tls_cacert = TLScacert},
	process_flag(trap_exit, true),
	{ok, ssl_start, StateData, 0};
init([Sup, diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessionID, AppId, ReqType, OHost, ORealm, _DHost, _DRealm, DiameterRequest,
		_Options] = _Args) ->
	{ok, TLSkey} = application:get_env(ocs, tls_key),
	{ok, TLScert} = application:get_env(ocs, tls_cert),
	{ok, TLScacert} = application:get_env(ocs, tls_cacert),
	{ok, Hostname} = inet:gethostname(),
	case global:whereis_name({ocs_diameter_auth, ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			StateData = #statedata{sup = Sup, server_address = ServerAddress,
					server_port = ServerPort, client_address = ClientAddress,
					client_port = ClientPort, session_id = SessionID,
					server_id = list_to_binary(Hostname), start = DiameterRequest,
					tls_key = TLSkey, tls_cert = TLScert, tls_cacert = TLScacert,
					app_id = AppId, auth_req_type = ReqType, origin_host = OHost,
					origin_realm = ORealm, port_server = PortServer},
			process_flag(trap_exit, true),
			{ok, ssl_start, StateData, 0}
		end.

-spec ssl_start(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>ssl_start</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
ssl_start(timeout, #statedata{start = #radius{code = ?AccessRequest},
		ssl_socket = undefined, sup = Sup,
		tls_key = TLSkey, tls_cert = TLScert, tls_cacert = TLScacert} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AaahFsm, _, _} = lists:keyfind(ocs_eap_ttls_aaah_fsm, 1, Children),
	Options = [{mode, binary}, {certfile, TLScert}, {keyfile, TLSkey},
			{cacertfile, TLScacert}],
	{ok, SslSocket} = ocs_eap_tls_transport:ssl_listen(self(), Options),
	gen_fsm:send_event(AaahFsm, {ttls_socket, self(), SslSocket}),
	NewStateData = StateData#statedata{aaah_fsm = AaahFsm,
			ssl_socket = SslSocket},
	{next_state, ssl_start, NewStateData, ?TIMEOUT};
ssl_start(timeout, #statedata{start = #diameter_eap_app_DER{},
		ssl_socket = undefined, sup = Sup, tls_key = TLSkey,
		tls_cert = TLScert, tls_cacert = TLScacert} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AaahFsm, _, _} = lists:keyfind(ocs_eap_ttls_aaah_fsm, 1, Children),
	Options = [{mode, binary}, {certfile, TLScert}, {keyfile, TLSkey},
			{cacertfile, TLScacert}],
	{ok, SslSocket} = ocs_eap_tls_transport:ssl_listen(self(), Options),
	gen_fsm:send_event(AaahFsm, {ttls_socket, self(), SslSocket}),
	NewStateData = StateData#statedata{aaah_fsm = AaahFsm,
			ssl_socket = SslSocket},
	{next_state, ssl_start, NewStateData, ?TIMEOUT};
ssl_start(timeout, #statedata{session_id = SessionID} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
ssl_start({ssl_pid, SslPid}, StateData) ->
	NewStateData = StateData#statedata{ssl_pid = SslPid},
	{next_state, eap_start, NewStateData, 0}.

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
%%
eap_start(timeout, #statedata{start = #radius{code = ?AccessRequest,
		id = RadiusID, authenticator = RequestAuthenticator,
		attributes = Attributes}, radius_fsm = RadiusFsm, eap_id = EapID,
		session_id = SessionID, secret = Secret} = StateData) ->
	EapTtls = #eap_ttls{start = true},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	NewStateData = StateData#statedata{req_auth = RequestAuthenticator},
	case radius_attributes:find(?EAPMessage, Attributes) of
		{ok, <<>>} ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
			{next_state, client_hello, NewStateData, ?TIMEOUT};
		{ok, EAPMessage} ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response,
						type = ?Identity, identifier = StartEapID} ->
					NewEapID = (StartEapID rem 255) + 1,
					NewEapPacket = #eap_packet{code = request, type = ?TTLS,
							identifier = NewEapID, data = EapData},
					send_response(NewEapPacket, ?AccessChallenge,
							RadiusID, [], RequestAuthenticator, Attributes, Secret,
							RadiusFsm, StateData),
					NextStateData = NewStateData#statedata{eap_id = NewEapID},
					{next_state, client_hello, NextStateData, ?TIMEOUT};
				#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Attributes, Secret,
							RadiusFsm, StateData),
					{stop, {shutdown, SessionID}, NewStateData};
				#eap_packet{code = Code,
							type = EapType, identifier = NewEapID, data = Data} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID},
							{eap_id, NewEapID}, {code, Code},
							{type, EapType}, {data, Data}]),
					NewEapPacket = #eap_packet{code = failure, identifier = NewEapID},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Attributes, Secret,
							RadiusFsm, StateData),
					{stop, {shutdown, SessionID}, StateData};
				{'EXIT', _Reason} ->
					NewEapPacket = #eap_packet{code = failure, identifier = EapID},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Attributes, Secret,
							RadiusFsm, StateData),
					{stop, {shutdown, SessionID}, StateData}
			end;
		{error, not_found} ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes, Secret,
					RadiusFsm, StateData),
			{next_state, client_hello, NewStateData, ?TIMEOUT}
	end;
eap_start(timeout, #statedata{start = DiameterRequest,
		eap_id = EapID, session_id = SessionID, auth_req_type = AuthType,
		origin_host = OH, origin_realm = OR, port_server = PortServer}
		= StateData) ->
	EapTtls = #eap_ttls{start = true},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	case DiameterRequest#diameter_eap_app_DER.'EAP-Payload' of
		<<>> ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
					OH, OR, EapPacket, PortServer, DiameterRequest, StateData),
			{next_state, client_hello, StateData, ?TIMEOUT};
		EAPMessage ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response,
						type = ?Identity, identifier = StartEapID} ->
					NewEapID = (StartEapID rem 255) + 1,
					NewEapPacket = #eap_packet{code = request, type = ?TTLS,
							identifier = NewEapID, data = EapData},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
							OH, OR, NewEapPacket, PortServer, DiameterRequest, StateData),
					NextStateData = StateData#statedata{eap_id = NewEapID},
					{next_state, client_hello, NextStateData, ?TIMEOUT};
				#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OH, OR, NewEapPacket, PortServer, DiameterRequest, StateData),
					{stop, {shutdown, SessionID}, StateData};
				#eap_packet{code = Code,
							type = EapType, identifier = NewEapID, data = Data} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID},
							{eap_id, NewEapID}, {code, Code},
							{type, EapType}, {data, Data}]),
					NewEapPacket = #eap_packet{code = failure, identifier = NewEapID},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OH, OR, NewEapPacket, PortServer, DiameterRequest, StateData),
					{stop, {shutdown, SessionID}, StateData};
				{'EXIT', _Reason} ->
					NewEapPacket = #eap_packet{code = failure, identifier = EapID},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
							OH, OR, NewEapPacket, PortServer, DiameterRequest, StateData),
					{stop, {shutdown, SessionID}, StateData}
			end
	end.

-spec client_hello(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>client_hello</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
client_hello(timeout, #statedata{session_id = SessionID} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
client_hello({ssl_setopts, Options}, StateData) ->
	NewStateData = StateData#statedata{socket_options = Options},
	{next_state, client_hello, NewStateData, ?TIMEOUT};
client_hello({#radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = Attributes},
		RadiusFsm}, #statedata{eap_id = EapID,
		session_id = SessionID, secret = Secret,
		rx_length = RxLength, rx_buf = RxBuf, ssl_pid = SslPid} = StateData) ->
	EapMessages = radius_attributes:get_all(?EAPMessage, Attributes),
	EapMessage = iolist_to_binary(EapMessages),
	NewStateData = case {radius_attributes:find(?FramedMtu, Attributes),
			radius_attributes:find(?NasPortType, Attributes)} of
		{{ok, MTU}, {ok, 19}} when MTU > 1496 -> % 802.11
			StateData#statedata{max_size = MTU - 4,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{{ok, MTU}, {ok, 19}} when MTU < 1496 -> % 802.11
			StateData#statedata{max_size = 1496,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{{ok, MTU}, {ok, 15}} -> % Ethernet
			StateData#statedata{max_size = MTU - 4,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{{ok, MTU}, _} -> % Ethernet
			StateData#statedata{max_size = MTU,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator};
		{_, _} ->
			StateData#statedata{max_size = 16#ffff,
					radius_fsm = RadiusFsm, radius_id = RadiusID,
					req_auth = RequestAuthenticator}
	end,
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = TtlsData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(TtlsData) of
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} when RxLength == undefined ->
				CHMsg = <<RxBuf/binary, Data/binary>>,
				NextStateData = client_hello1(CHMsg, NewStateData),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextNewStateData = NextStateData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_hello, NextNewStateData, ?TIMEOUT};
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} ->
				CHMsg = <<RxBuf/binary, Data/binary>>,
				RxLength = size(CHMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextStateData = NewStateData#statedata{rx_buf = <<>>,
								rx_length = undefined},
				{next_state, server_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = Data} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
				NextRxBuf = <<RxBuf/binary, Data/binary>>,
				NextStateData = NewStateData#statedata{rx_buf = NextRxBuf},
				{next_state, client_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = Data} ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
				NextStateData = NewStateData#statedata{rx_buf = Data,
						rx_length = MessageLength},
				{next_state, client_hello, NextStateData, ?TIMEOUT}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket2, ?AccessReject,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
client_hello(#diameter_eap_app_DER{} = Request, #statedata{eap_id = EapID,
		session_id = SessionID, rx_length = RxLength, rx_buf = RxBuf,
		ssl_pid = SslPid, origin_host = OH, origin_realm = OR,
		auth_req_type = AuthType, port_server = PortServer} = StateData) ->
	{EapMessage, FramedMTU, NasPortType} = get_diameter_attributes(Request),
	NewStateData = case {FramedMTU, NasPortType} of
		{undefined, undefined} ->
			StateData#statedata{max_size = 16#ffff};
		{MTU, 19} when MTU > 1496 -> % 802.11
			StateData#statedata{max_size = MTU - 4};
		{MTU, 19} when MTU < 1496 -> % 802.11
			StateData#statedata{max_size = 1496};
		{MTU, 15} -> % Ethernet
			StateData#statedata{max_size = MTU - 4};
		{MTU, _} -> % Ethernet
			StateData#statedata{max_size = MTU}
	end,
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = TtlsData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(TtlsData) of
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} when RxLength == undefined ->
				CHMsg = <<RxBuf/binary, Data/binary>>,
				NextStateData = client_hello1(CHMsg, NewStateData),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextNewStateData = NextStateData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_hello, NextNewStateData, ?TIMEOUT};
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} ->
				CHMsg = <<RxBuf/binary, Data/binary>>,
				RxLength = size(CHMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CHMsg),
				NextStateData = NewStateData#statedata{rx_buf = <<>>,
								rx_length = undefined},
				{next_state, server_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = Data} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket1, PortServer, Request, StateData),
				NextRxBuf = <<RxBuf/binary, Data/binary>>,
				NextStateData = NewStateData#statedata{rx_buf = NextRxBuf},
				{next_state, client_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = Data} ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH',
						OH, OR, EapPacket1, PortServer, Request, StateData),
				NextStateData = NewStateData#statedata{rx_buf = Data,
						rx_length = MessageLength},
				{next_state, client_hello, NextStateData, ?TIMEOUT}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OH, OR, EapPacket2, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end.
%% @hidden
%% TLS Record - <<ContentType, Version:16, Length:16, ProtocolMessage>>
%% ProtocolMessage - <<MessageType, Length:24, ClientHelloMessage>>
%% RFC 5246 Section 7.4.1.2
%% ClientHelloMessage -
%% <<ProtocolVersion:16, Gmt_unix_time:32, ClientRand:28/binary,
%%	SessionID, CipherSuite, CompressionMethod, ..>>
client_hello1(<<?Handshake, _Version:16, _L1:16, ?ClientHello, _L2:24,
		_ClientVersion:16, ClientRand:32/binary, _/binary>>, StateData) ->
	StateData#statedata{client_rand = ClientRand}.


-spec server_hello(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>server_hello</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
server_hello(timeout, #statedata{session_id = SessionID,
		tx_buf = <<>>} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
server_hello(timeout, StateData) ->
	server_hello2([], StateData);
%% TLS Record - <<ContentType, Version:16, Length:16, ProtocolMessage>>
%% ProtocolMessage - <<MessageType, Length:24, ServeHelloMessage>>
%% RFC 5246 Section 7.4.1.3
%% ServerHelloMessage -
%% <<ProtocolVersion:16, Gmt_unix_time:32, ServerRand:28/binary,
%% SessionID, CipherSuite, CompressionMethod, ..>>
server_hello({eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
		?ServerHello, _ServerVersion:16, ServerRand:32/binary,
		_/binary>> = Data}, StateData) ->
	NewStateData = StateData#statedata{server_rand = ServerRand},
	server_hello1(Data, NewStateData);
server_hello({eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
		?Certificate, _/binary>> = Data}, StateData) ->
	server_hello1(Data, StateData);
server_hello({eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
		?ServerKeyExchange, _/binary>> = Data}, StateData) ->
	server_hello1(Data, StateData);
server_hello({eap_tls, _SslPid, <<?Handshake, _Version:16, _L1:16,
		?ServerHelloDone, _/binary>> = Data},
		#statedata{tx_buf = TxBuf} = StateData) ->
	NextTxBuf = <<TxBuf/binary, Data/binary>>,
	NewStateData = StateData#statedata{tx_buf = NextTxBuf},
	server_hello2([], NewStateData);
server_hello({#radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = Attributes},
		RadiusFsm}, #statedata{eap_id = EapID, session_id = SessionID,
		secret = Secret} = StateData) ->
	NewStateData = StateData#statedata{radius_fsm = RadiusFsm,
      radius_id = RadiusID, req_auth = RequestAuthenticator},
	EapMessages = radius_attributes:get_all(?EAPMessage, Attributes),
	EapMessage = iolist_to_binary(EapMessages),
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = TtlsData} = ocs_eap_codec:eap_packet(EapMessage),
		#eap_ttls{more = false, message_len = undefined,
				start = false, data = <<>>} = ocs_eap_codec:eap_ttls(TtlsData),
		server_hello2(Attributes, NewStateData)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
server_hello(#diameter_eap_app_DER{} = Request, #statedata{eap_id = EapID,
		session_id = SessionID, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR, port_server = PortServer} = StateData) ->
	{EapMessages, _, _} = get_diameter_attributes(Request),
	EapMessage = iolist_to_binary(EapMessages),
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = TtlsData} = ocs_eap_codec:eap_packet(EapMessage),
		#eap_ttls{more = false, message_len = undefined,
				start = false, data = <<>>} = ocs_eap_codec:eap_ttls(TtlsData),
		server_hello2([], StateData)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					EapPacket, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
server_hello1(<<_:24, Length:16, _/binary>> = Data,
		#statedata{ssl_pid = SslPid, tx_buf = TxBuf} = StateData) ->
	case Data of
		<<_:40, _:Length/binary>>  = TRLayer->
			NextTxBuf = <<TxBuf/binary, TRLayer/binary>>,
			NewStateData = StateData#statedata{tx_buf = NextTxBuf},
			{next_state, server_hello, NewStateData, ?TIMEOUT};
		<<_:40,  _:Length/binary, Rest/binary>> = TRLayer ->
			Size = Length + 5,
			<<Msg:Size/binary, _/binary>>  = TRLayer,
			NextTxBuf = <<TxBuf/binary, Msg/binary>>,
			NewStateData = StateData#statedata{tx_buf = NextTxBuf},
			server_hello({eap_tls, SslPid, Rest}, NewStateData)
	end.
%% @hidden
server_hello2(RequestAttributes, #statedata{start = #radius{},
		tx_buf = TxBuf, radius_fsm = RadiusFsm, radius_id = RadiusID,
		req_auth = RequestAuthenticator, secret = Secret, eap_id = EapID,
		max_size = MaxSize} = StateData) ->
	MaxData = MaxSize - 10,
	NewEapID = (EapID rem 255) + 1,
	case size(TxBuf) of
		Size when Size > MaxData ->
			<<Chunk:MaxData/binary, Rest/binary>> = TxBuf,
			EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, RequestAttributes, Secret,
					RadiusFsm, StateData),
			NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = Rest},
			{next_state, server_hello, NewStateData, ?TIMEOUT};
		_Size ->
			EapTtls = #eap_ttls{data = TxBuf},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, RequestAttributes, Secret,
					RadiusFsm, StateData),
			NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = <<>>},
			{next_state, client_cipher, NewStateData, ?TIMEOUT}
	end;
server_hello2(_, #statedata{start = #diameter_eap_app_DER{}, tx_buf = TxBuf,
	 eap_id = EapID, max_size = MaxSize, session_id = SessionID,
	 auth_req_type = AuthType, origin_host = OH, origin_realm = OR,
	 port_server = PortServer} = StateData) ->
	MaxData = MaxSize - 10,
	NewEapID = (EapID rem 255) + 1,
	case size(TxBuf) of
		Size when Size > MaxData ->
			<<Chunk:MaxData/binary, Rest/binary>> = TxBuf,
			EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
					EapPacket, PortServer, #diameter_eap_app_DER{}, StateData),
			NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = Rest},
			{next_state, server_hello, NewStateData, ?TIMEOUT};
		_Size ->
			EapTtls = #eap_ttls{data = TxBuf},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
					EapPacket, PortServer, #diameter_eap_app_DER{}, StateData),
			NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = <<>>},
			{next_state, client_cipher, NewStateData, ?TIMEOUT}
	end.

-spec client_cipher(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>client_cipher</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
client_cipher(timeout,
		#statedata{session_id = SessionID} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
client_cipher({#radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = Attributes},
		RadiusFsm}, #statedata{eap_id = EapID, session_id = SessionID,
		secret = Secret, rx_length = RxLength, rx_buf = RxBuf,
		ssl_pid = SslPid} = StateData) ->
	EapMessages = radius_attributes:get_all(?EAPMessage, Attributes),
	EapMessage = iolist_to_binary(EapMessages),
	NewStateData = StateData#statedata{radius_fsm = RadiusFsm,
		radius_id = RadiusID, req_auth = RequestAuthenticator},
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = TtlsData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(TtlsData) of
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} when RxLength == undefined ->
				CCMsg = <<RxBuf/binary, Data/binary>>,
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextStateData = NewStateData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextStateData};
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} ->
				CCMsg = <<RxBuf/binary, Data/binary>>,
				RxLength = size(CCMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextStateData = NewStateData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextStateData};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = Data} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				CCMsg = <<RxBuf/binary, Data/binary>>,
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
				NextStateData = NewStateData#statedata{rx_buf = CCMsg},
				{next_state, client_cipher, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = Data} ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
				NextStateData = NewStateData#statedata{rx_buf = Data,
						rx_length = MessageLength},
				{next_state, client_cipher, NextStateData, ?TIMEOUT}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket2, ?AccessReject,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
			{stop, {shutdown, SessionID}, NewStateData}
	end;
client_cipher(#diameter_eap_app_DER{} = Request, #statedata{eap_id = EapID,
		session_id = SessionID, rx_length = RxLength, rx_buf = RxBuf,
		ssl_pid = SslPid, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR, port_server = PortServer} = StateData) ->
	EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
	try
		#eap_packet{code = response, type = ?TTLS, identifier = EapID,
				data = TtlsData} = ocs_eap_codec:eap_packet(EapMessage),
		case ocs_eap_codec:eap_ttls(TtlsData) of
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} when RxLength == undefined ->
				CCMsg = <<RxBuf/binary, Data/binary>>,
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextStateData = StateData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextStateData};
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} ->
				CCMsg = <<RxBuf/binary, Data/binary>>,
				RxLength = size(CCMsg),
				ocs_eap_tls_transport:deliver(SslPid, self(), CCMsg),
				NextStateData = StateData#statedata{rx_buf = <<>>,
						rx_length = undefined},
				{next_state, server_cipher, NextStateData};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = Data} when RxBuf /= <<>> ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				CCMsg = <<RxBuf/binary, Data/binary>>,
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
						EapPacket1, PortServer, Request, StateData),
				NextStateData = StateData#statedata{rx_buf = CCMsg},
				{next_state, client_cipher, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = Data} ->
				NewEapID = (EapID rem 255) + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
						EapPacket1, PortServer, Request, StateData),
				NextStateData = StateData#statedata{rx_buf = Data,
						rx_length = MessageLength},
				{next_state, client_cipher, NextStateData, ?TIMEOUT}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					EapPacket2, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.

-spec server_cipher(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>server_cipher</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
server_cipher(timeout,
		#statedata{session_id = SessionID} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
server_cipher({eap_tls, SslPid, <<?ChangeCipherSpec, _/binary>> = Data},
	StateData) ->
	NewStateData = StateData#statedata{ssl_pid = SslPid},
	server_cipher1(Data, NewStateData).

%% @hidden
server_cipher1(<<_:24, Length:16, _/binary>> = Data,
		#statedata{ssl_pid = SslPid, tx_buf = Buf} = StateData) ->
	case Data of
		<<_:40, _:Length/binary>> = SC ->
			TxBuf = <<Buf/binary, SC/binary>>,
			NewStateData = StateData#statedata{tx_buf = TxBuf},
			{next_state, finish, NewStateData};
		<<_:40, _:Length/binary, Rest/binary>> = SC ->
			Size = Length + 5,
			<<Msg:Size/binary, _/binary>>  = SC,
			TxBuf = <<Buf/binary, Msg/binary>>,
			NewStateData = StateData#statedata{tx_buf = TxBuf},
			finish({eap_tls, SslPid, Rest}, NewStateData)
	end.

-spec finish(Event, StateData) -> Result
	when
		Event :: timeout | term(), 
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData , hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(), 
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>finish</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
finish(timeout,
		#statedata{session_id = SessionID} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
finish({eap_tls, _SslPid, <<?Handshake, _/binary>> = Data},
		#statedata{tx_buf = TxBuf, start = #radius{}, radius_id = RadiusID,
		radius_fsm = RadiusFsm, req_auth = RequestAuthenticator, secret = Secret,
		eap_id = EapID} = StateData) ->
	NewEapID = (EapID rem 255) + 1,
	BinData = <<TxBuf/binary, Data/binary>>,
	EapTtls = #eap_ttls{data = BinData},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = request, type = ?TTLS,
			identifier = NewEapID, data = EapData},
	send_response(EapPacket, ?AccessChallenge,
			RadiusID, [], RequestAuthenticator, [], Secret, RadiusFsm, StateData),
	NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = <<>>},
	{next_state, client_passthrough, NewStateData};
finish({eap_tls, _SslPid, <<?Handshake, _/binary>> = Data},
		#statedata{tx_buf = TxBuf, start = #diameter_eap_app_DER{},
		eap_id = EapID, session_id = SessionID, auth_req_type = AuthType,
		origin_host = OH, origin_realm = OR, port_server = PortServer} =
		StateData) ->
	NewEapID = (EapID rem 255) + 1,
	BinData = <<TxBuf/binary, Data/binary>>,
	EapTtls = #eap_ttls{data = BinData},
	EapData = ocs_eap_codec:eap_ttls(EapTtls),
	EapPacket = #eap_packet{code = request, type = ?TTLS,
			identifier = NewEapID, data = EapData},
	send_diameter_response(SessionID, AuthType,
			?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR, EapPacket,
			PortServer, #diameter_eap_app_DER{}, StateData),
	NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = <<>>},
	{next_state, client_passthrough, NewStateData}.

-spec client_passthrough(Event, StateData) -> Result
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
%%		gen_fsm:send_event/2} in the <b>client_passthrough</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
client_passthrough(timeout, #statedata{session_id = SessionID} =
		StateData) ->
	{stop, {shutdown, SessionID}, StateData};
client_passthrough({#radius{code = ?AccessRequest, id = RadiusID,
		authenticator = RequestAuthenticator, attributes = Attributes},
		RadiusFsm}, #statedata{eap_id = EapID,session_id = SessionID,
		secret = Secret, ssl_pid = SslPid} = StateData) ->
	NewStateData = StateData#statedata{req_auth = RequestAuthenticator,
			radius_fsm = RadiusFsm, radius_id = RadiusID},
	try
		EapMessage = radius_attributes:fetch(?EAPMessage, Attributes),
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, identifier = EapID, data = TtlsPacket} ->
				#eap_ttls{data = TtlsData} = ocs_eap_codec:eap_ttls(TtlsPacket),
				ocs_eap_tls_transport:deliver(SslPid, self(), TtlsData),
				{next_state, server_passthrough, NewStateData};
			#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Attributes, Secret,
							RadiusFsm, StateData),
					{stop, {shutdown, SessionID}, NewStateData};
			#eap_packet{code = Code,
					type = EapType, identifier = NewEapID, data = Data} ->
				error_logger:warning_report(["Unknown EAP received",
						{pid, self()}, {session_id, SessionID},
						{eap_id, NewEapID}, {code, Code},
						{type, EapType}, {data, Data}]),
				NewEapPacket = #eap_packet{code = failure, identifier = NewEapID},
				send_response(NewEapPacket, ?AccessReject,
						RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
						StateData),
				{stop, {shutdown, SessionID}, StateData}
		end
	catch
		_:_ ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket1, ?AccessReject,
					RadiusID, [], RequestAuthenticator, Attributes, Secret, RadiusFsm,
					StateData),
				{stop, {shutdown, SessionID}, StateData}
	end;
client_passthrough(#diameter_eap_app_DER{} = Request,
		#statedata{eap_id = EapID, session_id = SessionID, ssl_pid = SslPid,
		auth_req_type = AuthType, origin_host = OH, origin_realm = OR,
		port_server = PortServer} = StateData) ->
	try
		EapMessage = Request#diameter_eap_app_DER.'EAP-Payload',
		case ocs_eap_codec:eap_packet(EapMessage) of
			#eap_packet{code = response, identifier = EapID, data = TtlsPacket} ->
				#eap_ttls{data = TtlsData} = ocs_eap_codec:eap_ttls(TtlsPacket),
				ocs_eap_tls_transport:deliver(SslPid, self(), TtlsData),
				{next_state, server_passthrough, StateData};
			#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_diameter_response(SessionID, AuthType,
							?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH', OH, OR,
							NewEapPacket, PortServer, Request, StateData),
					{stop, {shutdown, SessionID}, StateData};
			#eap_packet{code = Code,
					type = EapType, identifier = NewEapID, data = Data} ->
				error_logger:warning_report(["Unknown EAP received",
						{pid, self()}, {session_id, SessionID},
						{eap_id, NewEapID}, {code, Code},
						{type, EapType}, {data, Data}]),
				NewEapPacket = #eap_packet{code = failure, identifier = NewEapID},
				send_diameter_response(SessionID, AuthType,
						?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
						NewEapPacket, PortServer, Request, StateData),
				{stop, {shutdown, SessionID}, StateData}
		end
	catch
		_:_ ->
			EapPacket1 = #eap_packet{code = failure, identifier = EapID},
			send_diameter_response(SessionID, AuthType,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OH, OR,
					EapPacket1, PortServer, Request, StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.

-spec server_passthrough(Event, StateData) -> Result
	when
		Event :: timeout | term(), 
		StateData :: statedata(),
		Result :: {next_state, NextStateName, StateData}
		| {next_state, NextStateName, NewStateData, Timeout}
		| {next_state, NextStateName, NewStateData, hibernate}
		| {stop, Reason, NewStateData},
		NextStateName :: atom(), 
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>server_passthrough</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
server_passthrough(timeout, #statedata{session_id = SessionID} =
		StateData) ->
	{stop, {shutdown, SessionID}, StateData};
server_passthrough({accept, #subscriber{name = Identity,
		attributes = Attributes}, SslSocket}, #statedata{eap_id = EapID,
		start = #radius{}, session_id = SessionID, secret = Secret,
		req_auth = RequestAuthenticator, radius_fsm = RadiusFsm,
		radius_id = RadiusID, %ssl_socket = SslSocket,
		client_rand = ClientRandom, server_rand = ServerRandom}
		= StateData) ->
	Seed = [<<ClientRandom/binary, ServerRandom/binary>>],
	{MSK, _} = prf(SslSocket, master_secret ,
			<<"ttls keying material">>, Seed, 128),
	UserName = binary_to_list(Identity),
	Salt = crypto:rand_uniform(16#8000, 16#ffff),
	MsMppeKey = encrypt_key(Secret, RequestAuthenticator, Salt, MSK),
	Attr1 = radius_attributes:store(?UserName, UserName, Attributes),
	VendorSpecific1 = {?Microsoft, {?MsMppeSendKey, {Salt, MsMppeKey}}},
	Attr2 = radius_attributes:add(?VendorSpecific, VendorSpecific1, Attr1),
	VendorSpecific2 = {?Microsoft, {?MsMppeRecvKey, {Salt, MsMppeKey}}},
	Attr3 = radius_attributes:add(?VendorSpecific, VendorSpecific2, Attr2),
	EapPacket = #eap_packet{code = success, identifier = EapID},
	send_response(EapPacket, ?AccessAccept,
		RadiusID, Attr3, RequestAuthenticator, [], Secret, RadiusFsm, StateData),
	{stop, {shutdown, SessionID}, StateData};
server_passthrough(reject, #statedata{eap_id = EapID, 
		session_id = SessionID, secret = Secret, start = #radius{},
		req_auth = RequestAuthenticator, radius_fsm = RadiusFsm,
		radius_id = RadiusID} = StateData) ->
	EapPacket = #eap_packet{code = failure, identifier = EapID},
	send_response(EapPacket, ?AccessReject, RadiusID,
			[], RequestAuthenticator, [], Secret, RadiusFsm, StateData),
	{stop, {shutdown, SessionID}, StateData};
server_passthrough({accept, _UserName, SslSocket},
		#statedata{eap_id = EapID, start = #diameter_eap_app_DER{},
		session_id = SessionID, auth_req_type = AuthType, origin_host = OH,
		origin_realm = OR, port_server = PortServer, client_rand = ClientRandom,
		server_rand = ServerRandom} = StateData) ->
	Seed = [<<ClientRandom/binary, ServerRandom/binary>>],
	{MSK, _} = prf(SslSocket, master_secret , <<"ttls keying material">>,
			Seed, 128),
	EapPacket = #eap_packet{code = success, identifier = EapID},
	EapData = ocs_eap_codec:eap_packet(EapPacket),
	Answer = #diameter_eap_app_DEA{'Session-Id' = SessionID,
				'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
				'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapData], 'EAP-Master-Session-Key' = [MSK]},
	gen_server:cast(PortServer, {self(), Answer}),
	{stop, {shutdown, SessionID}, StateData};
server_passthrough(reject, #statedata{eap_id = EapID,
		session_id = SessionID, start = #diameter_eap_app_DER{},
	 	auth_req_type = AuthType, origin_host = OH, origin_realm = OR,
		port_server = PortServer} = StateData) ->
	EapPacket = #eap_packet{code = failure, identifier = EapID},
	send_diameter_response(SessionID, AuthType,
			?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED', OH, OR,
			EapPacket, PortServer, #diameter_eap_app_DER{}, StateData),
	{stop, {shutdown, SessionID}, StateData}.

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

-spec send_response(EapPacket, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm,
		StateData) -> ok
	when
		EapPacket :: #eap_packet{},
		RadiusCode :: integer(), 
		RadiusID :: byte(),
		ResponseAttributes :: radius_attributes:attributes(),
		RequestAuthenticator :: binary() | [byte()], 
		RequestAttributes :: radius_attributes:attributes(),
		Secret :: binary(),
		RadiusFsm :: pid(),
		StateData :: statedata().
%% @doc Sends an RADIUS-Access/Challenge or Reject or Accept  packet to peer
%% @hidden
send_response(#eap_packet{} = EapPacket, RadiusCode, RadiusID,
		ResponseAttributes, RequestAuthenticator, RequestAttributes, Secret,
		RadiusFsm, StateData) ->
	BinEapPacket = ocs_eap_codec:eap_packet(EapPacket),
	send_response1(BinEapPacket, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm, StateData).
%% @hidden
send_response1(<<Chunk:253/binary, Rest/binary>>, RadiusCode, RadiusID,
		ResponseAttributes, RequestAuthenticator, RequestAttributes, Secret,
		RadiusFsm, StateData) ->
	AttrList1 = radius_attributes:add(?EAPMessage, Chunk,
			ResponseAttributes),
	send_response1(Rest, RadiusCode, RadiusID, AttrList1,
		RequestAuthenticator,  RequestAttributes, Secret, RadiusFsm, StateData);
send_response1(<<>>, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm, StateData) ->
	send_response2(RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm, StateData);
send_response1(Chunk, RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm,
		StateData) when is_binary(Chunk) ->
	AttrList1 = radius_attributes:add(?EAPMessage, Chunk,
			ResponseAttributes),
	send_response2(RadiusCode, RadiusID, AttrList1, RequestAuthenticator,
			RequestAttributes, Secret, RadiusFsm, StateData).
%% @hidden
send_response2(RadiusCode, RadiusID, ResponseAttributes,
		RequestAuthenticator, RequestAttributes, Secret, RadiusFsm,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort} = _StateData) ->
	AttrList2 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, ResponseAttributes),
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
		EapPacket, PortServer, Request, StateData) -> ok
	when
		SessionID :: string() | binary(),
		AuthType :: integer(),
		ResultCode :: integer(),
		OH :: binary(),
		OR :: binary(),
		EapPacket :: #eap_packet{},
		PortServer :: pid(),
		Request :: #diameter_eap_app_DER{},
		StateData :: statedata().
%% @doc Send appropriate DIAMETER answer to ocs_diameter_auth_port_server.
%% @hidden
send_diameter_response(SId, AuthType, ResultCode, OH, OR, EapPacket,
		PortServer, Request, #statedata{server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort} = _StateData) ->
	try
		EapData = ocs_eap_codec:eap_packet(EapPacket),
		Answer = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
				'Result-Code' = ResultCode, 'Origin-Host' = OH, 'Origin-Realm' = OR,
				'EAP-Payload' = [EapData]},
		ok = ocs_log:auth_log(diameter, {ServerAddress, ServerPort},
				{ClientAddress, ClientPort}, Request, Answer),
		gen_server:cast(PortServer, {self(), Answer})
	catch
		_:_ ->
		Answer1 = #diameter_eap_app_DEA{'Session-Id' = SId,
				'Auth-Application-Id' = 5, 'Auth-Request-Type' = AuthType,
				'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				'Origin-Host' = OH, 'Origin-Realm' = OR},
		ok = ocs_log:auth_log(diameter, {ServerAddress, ServerPort},
				{ClientAddress, ClientPort}, Request, Answer1),
		gen_server:cast(PortServer, {self(), Answer1})
	end.

