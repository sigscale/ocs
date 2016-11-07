%%% ocs_eap_ttls_fsm.erl
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
%%% @reference <a href="http://tools.ietf.org/rfc/rfc5281.txt">
%%% 	RFC5281 - EAP Tunneled Transport Layer Security (EAP-TTLS)</a>
%%%
-module(ocs_eap_ttls_fsm).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_eap_ttls_fsm API
-export([]).

%% export the ocs_eap_ttls_fsm state callbacks
-export([ssl_start/2, eap_start/2, client_hello/2, server_hello/2,
			client_key_exchange/2, passthrough/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

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

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").

-record(statedata,
		{sup :: pid(),
		aaah_fsm :: pid(),
		address :: inet:ip_address(),
		port :: pos_integer(),
		session_id :: {NAS :: inet:ip_address() | string(),
			Port :: string(), Peer :: string()},
		secret :: binary(),
		eap_id = 0 :: byte(),
		start :: #radius{},
		server_id :: binary(),
		radius_fsm :: pid(),
		radius_id :: byte(),
		req_auth :: [byte()],
		ssl_socket :: ssl:sslsocket(),
		socket_options :: ssl:options(),
		max_size :: pos_integer(),
		rx_length :: pos_integer(),
		rx_buf = [] :: iolist(),
		tx_buf = [] :: iolist(),
		ssl_pid :: pid(),
		tls_key :: string(),
		tls_crt :: string()}).

-define(TIMEOUT, 30000).
-define(BufTIMEOUT, 100).

% suppress warning from ssl:listen/2
-dialyzer({no_return, eap_start/2}).

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_eap_ttls_fsm gen_fsm call backs
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
init([Sup, Address, Port, RadiusFsm, Secret, SessionID, AccessRequest] = _Args) ->
	{ok, TLSkey} = application:get_env(ocs, tls_key),
	{ok, TLScert} = application:get_env(ocs, tls_crt),
	{ok, Hostname} = inet:gethostname(),
	StateData = #statedata{sup = Sup, address = Address, port = Port,
			radius_fsm = RadiusFsm, secret = Secret, session_id = SessionID,
			server_id = list_to_binary(Hostname), start = AccessRequest,
			tls_key = TLSkey, tls_crt = TLScert},
	process_flag(trap_exit, true),
	{ok, ssl_start, StateData, 0}.

-spec ssl_start(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>ssl_start</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
ssl_start(timeout, #statedata{start = #radius{code = ?AccessRequest},
		ssl_socket = undefined, sup = Sup,
		tls_key = TLSkey, tls_crt = TLScert} = StateData) ->
	Children = supervisor:which_children(Sup),
	{_, AaahFsm, _, _} = lists:keyfind(ocs_eap_ttls_aaah_fsm, 1, Children),
	Options = [{mode, binary}, {certfile, TLScert}, {keyfile, TLSkey},
			{cacertfile,"/home/prahveen/ocs/ocs.build/priv/CAcert.crt"}],
	{ok, SslSocket} = ocs_eap_ttls_transport:ssl_listen(self(), Options),
	gen_fsm:send_event(AaahFsm, {ttls_socket, self(), SslSocket}),
	NewStateData = StateData#statedata{aaah_fsm = AaahFsm,
			ssl_socket = SslSocket},
	{next_state, ssl_start, NewStateData, ?TIMEOUT};
ssl_start(timeout, #statedata{session_id = SessionID} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
ssl_start({ssl_pid, SslPid}, StateData) ->
	NewStateData = StateData#statedata{ssl_pid = SslPid},
	{next_state, eap_start, NewStateData, 0}.

-spec eap_start(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
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
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
			{next_state, client_hello, NewStateData, ?TIMEOUT};
		{ok, EAPMessage} ->
			case catch ocs_eap_codec:eap_packet(EAPMessage) of
				#eap_packet{code = response,
						type = ?Identity, identifier = StartEapID} ->
					NewEapID = StartEapID + 1,
					NewEapPacket = #eap_packet{code = request, type = ?TTLS,
							identifier = NewEapID, data = EapData},
					send_response(NewEapPacket, ?AccessChallenge,
							RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
					NextStateData = NewStateData#statedata{eap_id = NewEapID},
					{next_state, client_hello, NextStateData, ?TIMEOUT};
				#eap_packet{code = request, identifier = NewEapID} ->
					NewEapPacket = #eap_packet{code = response, type = ?LegacyNak,
							identifier = NewEapID, data = <<0>>},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
					{stop, {shutdown, SessionID}, NewStateData};
				#eap_packet{code = Code,
							type = EapType, identifier = NewEapID, data = Data} ->
					error_logger:warning_report(["Unknown EAP received",
							{pid, self()}, {session_id, SessionID},
							{eap_id, NewEapID}, {code, Code},
							{type, EapType}, {data, Data}]),
					NewEapPacket = #eap_packet{code = failure, identifier = NewEapID},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
					{stop, {shutdown, SessionID}, StateData};
				{'EXIT', _Reason} ->
					NewEapPacket = #eap_packet{code = failure, identifier = EapID},
					send_response(NewEapPacket, ?AccessReject,
							RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
					{stop, {shutdown, SessionID}, StateData}
			end;
		{error, not_found} ->
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = EapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
			{next_state, client_hello, NewStateData, ?TIMEOUT}
	end.

-spec client_hello(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
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
		rx_length = RxLength, rx_buf = RxBuf} = StateData) ->
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
				TtlsRecord = iolist_to_binary([RxBuf, Data]),
				client_hello1(TtlsRecord, StateData),
				NextStateData = NewStateData#statedata{rx_buf = [],
						rx_length = undefined},
				{next_state, server_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = false, message_len = undefined,
					start = false, data = Data} ->
				RxLength = iolist_size([RxBuf, Data]),
				TtlsRecord = iolist_to_binary([RxBuf, Data]),
				client_hello1(TtlsRecord, StateData),
				NextStateData = NewStateData#statedata{rx_buf = [],
								rx_length = undefined},
				{next_state, server_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = undefined,
					start = false, data = Data} when RxBuf /= [] ->
				NewEapID = EapID + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
				NextStateData = NewStateData#statedata{rx_buf = [RxBuf, Data]},
				{next_state, client_hello, NextStateData, ?TIMEOUT};
			#eap_ttls{more = true, message_len = MessageLength,
					start = false, data = Data} ->
				NewEapID = EapID + 1,
				TtlsData = ocs_eap_codec:eap_ttls(#eap_ttls{}),
				EapPacket1 = #eap_packet{code = response, type = ?TTLS,
						identifier = NewEapID, data = TtlsData},
				send_response(EapPacket1, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
				NextStateData = NewStateData#statedata{rx_buf = [Data],
						rx_length = MessageLength},
				{next_state, client_hello, NextStateData, ?TIMEOUT}
		end
	catch
		_:_ ->
			EapPacket2 = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket2, ?AccessReject,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
			{stop, {shutdown, SessionID}, NewStateData}
	end.
%% @hidden
client_hello1(<<?ClientHello, _/binary>> = TtlsRecord,
		#statedata{ssl_pid = SslPid} = _StateData) ->
	ocs_eap_ttls_transport:deliver(SslPid, self(), TtlsRecord).

-spec server_hello(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>server_hello</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
server_hello(timeout, #statedata{session_id = SessionID,
		tx_buf = []} = StateData) ->
	{stop, {shutdown, SessionID}, StateData};
server_hello(timeout, StateData) ->
	server_hello1(StateData);
server_hello({eap_ttls, _SslPid, <<?Handshake, _:32, ?ServerHello, _/binary >>},
		#statedata{tx_buf = TxBuf} = StateData) ->
	NewStateData = StateData#statedata{tx_buf = [TxBuf, Data]},
	{next_state, server_hello, NewStateData};
server_hello({eap_ttls, _SslPid, <<?Handshake, _:32, ?Certificate, _/binary >>},
		#statedata{tx_buf = TxBuf} = StateData) ->
	NewStateData = StateData#statedata{tx_buf = [TxBuf, Data]},
	{next_state, server_hello, NewStateData};
server_hello({eap_ttls, _SslPid, <<?Handshake, _:32, ?ServerKeyExchange, _/binary >>},
		#statedata{tx_buf = TxBuf} = StateData) ->
	NewStateData = StateData#statedata{tx_buf = [TxBuf, Data]},
	{next_state, server_hello, NewStateData};
server_hello({eap_ttls, _SslPid, <<?Handshake, _:32, ?ServerHelloDone, _/binary >>},
		#statedata{tx_buf = TxBuf} = StateData) ->
	NewStateData = StateData#statedata{tx_buf = [TxBuf, Data]},
	{next_state, server_hello, NewStateData};
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
		server_hello1(NewStateData)
	catch
		_:_ ->
			EapPacket = #eap_packet{code = failure, identifier = EapID},
			send_response(EapPacket, ?AccessReject,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
			{stop, {shutdown, SessionID}, NewStateData}
	end.
%% @hidden
server_hello1(#statedata{tx_buf = TxBuf, radius_fsm = RadiusFsm,
		radius_id = RadiusID, req_auth = RequestAuthenticator,
		secret = Secret, eap_id = EapID, max_size = MaxSize} = StateData) ->
	MaxData = MaxSize - 10,
	NewEapID = EapID + 1,
	case iolist_size(TxBuf) of
		Size when Size > MaxData ->
			<<Chunk:MaxData/binary, Rest/binary>> = iolist_to_binary(TxBuf),
			EapTtls = #eap_ttls{more = true, message_len = Size, data = Chunk},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
			NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = [Rest]},
			{next_state, server_hello, NewStateData, ?TIMEOUT};
		_Size ->
			EapTtls = #eap_ttls{data = iolist_to_binary(TxBuf)},
			EapData = ocs_eap_codec:eap_ttls(EapTtls),
			EapPacket = #eap_packet{code = request, type = ?TTLS,
					identifier = NewEapID, data = EapData},
			send_response(EapPacket, ?AccessChallenge,
					RadiusID, [], RequestAuthenticator, Secret, RadiusFsm),
			NewStateData = StateData#statedata{eap_id = NewEapID, tx_buf = []},
			{next_state, client_hello, NewStateData, ?TIMEOUT}
	end.

-spec client_key_exchange(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>client_key_exchange</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
client_key_exchange(timeout, #statedata{session_id = SessionID} =
		StateData) ->
	{stop, {shutdown, SessionID}, StateData}.

-spec passthrough(Event :: timeout | term(), StateData :: #statedata{}) ->
	Result :: {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}, hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>passthrough</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
passthrough(timeout, #statedata{session_id = SessionID} =
		StateData) ->
	{stop, {shutdown, SessionID}, StateData}.

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
	{next_state, StateName, StateData, ?TIMEOUT}.

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
	{next_state, StateName, StateData, ?TIMEOUT}.

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

-spec send_response(EapPacket :: #eap_packet{},
		RadiusCode :: integer(), RadiusID :: byte(),
		RadiusAttributes :: radius_attributes:attributes(),
		RequestAuthenticator :: binary() | [byte()], Secret :: binary(),
		RadiusFsm :: pid()) -> ok.
%% @doc Sends an RADIUS-Access/Challenge or Reject or Accept  packet to peer
%% @hidden
send_response(#eap_packet{} = EapPacket, RadiusCode, RadiusID, RadiusAttributes,
		RequestAuthenticator, Secret, RadiusFsm) ->
	BinEapPacket = ocs_eap_codec:eap_packet(EapPacket),
	send_response1(BinEapPacket, RadiusCode, RadiusID, RadiusAttributes,
			RequestAuthenticator, Secret, RadiusFsm).
%% @hidden
send_response1(<<Chunk:247/binary, Rest/binary>>, RadiusCode, RadiusID,
		RadiusAttributes, RequestAuthenticator, Secret, RadiusFsm) ->
	AttrList1 = radius_attributes:add(?EAPMessage, Chunk,
			RadiusAttributes),
	send_response1(Rest, RadiusCode, RadiusID, AttrList1,
		RequestAuthenticator, Secret, RadiusFsm);
send_response1(<<>>, RadiusCode, RadiusID, RadiusAttributes,
		RequestAuthenticator, Secret, RadiusFsm) ->
	send_response2(RadiusCode, RadiusID, RadiusAttributes,
		RequestAuthenticator, Secret, RadiusFsm);
send_response1(Chunk, RadiusCode, RadiusID, RadiusAttributes,
		RequestAuthenticator, Secret, RadiusFsm) when is_binary(Chunk) ->
	AttrList1 = radius_attributes:add(?EAPMessage, Chunk,
			RadiusAttributes),
	send_response2(RadiusCode, RadiusID, AttrList1, RequestAuthenticator,
			Secret, RadiusFsm).
%% @hidden
send_response2(RadiusCode, RadiusID, RadiusAttributes,
		RequestAuthenticator, Secret, RadiusFsm) ->
	AttrList2 = radius_attributes:store(?MessageAuthenticator,
		<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, RadiusAttributes),
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
	radius:response(RadiusFsm, {response, ResponsePacket}).

