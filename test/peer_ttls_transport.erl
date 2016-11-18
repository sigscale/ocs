%%% peer_ttls_transport.erl
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
%%% @doc This callback module provides a {@link //kernel/gen_tcp. gen_tcp}
%%% 	compatible transport layer interface to EAP sessions for the
%%% 	{@link //ssl. ssl} application.
%%% 
%%% 	Use the {@link //ssl/ssl:transportoption(). ssl:transportoption()}
%%%	`{cb_info, {ocs_eap_ttls_transport, eap_ttls, eap_ttls_closed, eap_ttls_error}}'
%%% 	with {@link //ssl/ssl:listen/2. ssl:listen/2}.
%%%
-module(peer_ttls_transport).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export inet compatible API
-export([setopts/2, getopts/2]).
%% export gen_tcp compatible API
-export([send/2, controlling_process/2,
		shutdown/2, close/1, connect/4]).
%% export public API
-export([ssl_connect/3, deliver/1]).

-export_type([listen_option/0, eap_option/0]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("common_test/include/ct.hrl").

-type listen_option() :: any().
-type eap_option() :: any().

-define(cb_info,
		{cb_info, {?MODULE, eap_ttls, eap_ttls_closed, eap_ttls_error}}).

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

%%----------------------------------------------------------------------
%%  ocs_eap_ttls_transport public api
%%----------------------------------------------------------------------

-dialyzer({nowarn_function, ssl_connect/3}).
-spec ssl_connect(Address, Port, Options) ->
		{ok, SslSocket} | {error, Reason} when
	Address :: inet:ip_address(),
	Port :: inet:port_number(),
	Options :: [term()],
	SslSocket :: pid(),
	Reason :: term().
%% @doc Open an SSL connection to Host, Port.
ssl_connect(Address, Port, Options) ->
	ssl:connect(Address, Port, [?cb_info | Options]).

-spec deliver(Data) ->
	ok when
		Data :: binary().
%% @doc Deliver received EAP-TTLS payload to SSL.
deliver(Data) ->
	self() ! {eap_ttls, self(), Data},
	ok.

%%----------------------------------------------------------------------
%%  ocs_eap_ttls_transport callbacks
%%----------------------------------------------------------------------
connect(Address, Port, SocketOpts, Timeout) ->
	{ok, self()}.

-spec setopts(ClientPid, Options) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
      Options :: [eap_option()],
		Reason :: term().
%% @doc Sets one or more options for an EAP session.
setopts(ClientPid, Options) when is_pid(ClientPid) -> 
	case proplists:get_value(active, Options) of
		undefined ->
			ok;
		Active ->
			% ClientPid ! {ssl_setopts, Options},
			ok
	end.

-spec getopts(ClientPid, Options) ->
	{ok, OptionValues} | {error, Reason} when
		ClientPid :: pid(),
      Options :: [eap_option()],
      OptionValues :: [eap_option()],
		Reason :: term().
%% @doc Gets one or more options for an EAP session.
getopts(ClientPid, Options) when is_pid(ClientPid) ->
	{ok, []}.

-spec shutdown(ClientPid, How) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		How :: read | write | read_write,
		Reason :: term().
%% @doc Close an EAP session in one or two directions.
shutdown(ClientPid, How) when is_pid(ClientPid) ->
	ok.

-spec close(ClientPid) ->
	ok when
		ClientPid :: pid().
%% @doc Close an EAP session.
close(ClientPid) when is_pid(ClientPid) ->
	ok.

-spec send(ClientPid, Data) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		Data :: iodata(),
		Reason :: closed | term().
%% @doc Sends a packet on an EAP session.
send(ClientPid, Data) when is_pid(ClientPid) ->
	{ok, Address} = application:get_env(ocs, radius_auth_addr),
	{ok, Port} = application:get_env(ocs, radius_auth_port),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, {ip, Address}, binary]),
	Secret = "xyzzy5461",
	NasId = atom_to_list(node()),
	AnonymousName = "DonaldTrump",
	MAC = "dd:ee:ff:aa:bb:cc",
	send1(ClientPid, Socket, Address, Port, Secret, NasId, MAC, AnonymousName, Data).
%% @hidden
send1(ClientPid, Socket, Address, Port, Secret, NasId, MAC, UserName,
			[<<?Handshake, _:32>>, [[?ClientHello | _] | _]] = Data) ->
	RadId = 10, EapId = 3,
	Auth = radius:authenticator(),
	client_hello(Socket, Address, Port, Auth,
			RadId, EapId, Secret, NasId, MAC, UserName, Data),
	server_hello(ClientPid, Socket, Address, Port, Auth, RadId, Secret);
send1(_, _, _, _, _, _, _, _, [<<?Handshake, _:32>>,
		[[?ClientKeyExchange | _] | _]] = Data) ->
	ets:new(client_cipher, [named_table]),
	ets:insert(client_cipher, {key_ex, Data});
send1(_, _, _, _, _, _, _, _, [<<?ChangeCipherSpec, _:32>> | _] = Data) ->
	ets:insert(client_cipher, {change_cipher, Data});
send1(ClientPid, Socket, Address, Port, Secret, NasId, MAC, UserName,
			[<<?Handshake, _:32>> | _] = Data) ->
	RadId = 11, EapId = 4,
	Auth = radius:authenticator(),
	[{_, KeyEx}] = ets:lookup(client_cipher, key_ex), 
	[{_, ChangeCipher}] = ets:lookup(client_cipher, change_cipher), 
	ClientCipher = [KeyEx, ChangeCipher, Data],
	client_cipher(Socket, Address, Port, Auth,
			RadId, EapId, Secret, NasId, MAC, UserName, ClientCipher),
	server_cipher(ClientPid, Socket, Address, Port, Auth, RadId, Secret).

-spec controlling_process(ClientPid, Pid) ->
	ok | {error, Reason} when
		ClientPid :: pid(),
		Pid :: pid(),
		Reason :: closed | not_owner | term().
%% @doc Assigns a new controlling process Pid to EAP session.
controlling_process(ClientPid, Pid) when is_pid(ClientPid) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------
client_hello(Socket, Address, Port, ReqAuth, RadId, EapId, Secret, NasId,
			MAC, AnonymousName, Data) ->
	TtlsData = iolist_to_binary(Data),
	Ttls = ocs_eap_codec:eap_ttls(#eap_ttls{version = 1,data = TtlsData}),
	EapRecord = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = Ttls},
	EapMsg = ocs_eap_codec:eap_packet(EapRecord),
	EapMessages = eap_fragment(EapMsg, []),
	access_request(Socket, Address, Port, NasId,
			AnonymousName, Secret, MAC, ReqAuth,RadId, EapMessages).

server_hello(ClientPid, Socket, Address, Port, ReqAuth, RadId, Secret) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?TTLS,
			identifier = 4, data = TtlsMsg} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_ttls{data = TtlsData} = ocs_eap_codec:eap_ttls(TtlsMsg),
	deliver(TtlsData).

client_cipher(Socket, Address, Port, ReqAuth,
		RadId, EapId, Secret, NasId, MAC, UserName, Data) ->
	TtlsData = iolist_to_binary(Data),
	Ttls = ocs_eap_codec:eap_ttls(#eap_ttls{version = 1,data = TtlsData}),
	EapRecord = #eap_packet{code = response, type = ?TTLS,
			identifier = EapId, data = Ttls},
	EapMsg = ocs_eap_codec:eap_packet(EapRecord),
	EapMessages = eap_fragment(EapMsg, []),
	access_request(Socket, Address, Port, NasId,
			UserName, Secret, MAC, ReqAuth,RadId, EapMessages).

server_cipher(ClientPid, Socket, Address, Port, ReqAuth, RadId, Secret) ->
	EapMsg = access_challenge(Socket, Address, Port,
			Secret, RadId, ReqAuth),
	#eap_packet{code = request, type = ?TTLS,
			identifier = 5, data = TtlsMsg} = ocs_eap_codec:eap_packet(EapMsg),
	#eap_ttls{data = TtlsData} = ocs_eap_codec:eap_ttls(TtlsMsg),
	deliver(TtlsData).

eap_fragment(<<Chunk:253/binary, Rest/binary>>, RadiusAttributes) ->
	AttributList = radius_attributes:add(?EAPMessage, Chunk,
			RadiusAttributes),
	eap_fragment(Rest, AttributList);
eap_fragment(<<>>, RadiusAttributes) ->
	RadiusAttributes;
eap_fragment(Chunk, RadiusAttributes) ->
	radius_attributes:add(?EAPMessage, Chunk,
			RadiusAttributes).

access_request(Socket, Address, Port, NasId,
		UserName, Secret, MAC, ReqAuth,  RadId, RadiusAttributes) ->
	A1 = radius_attributes:add(?FramedMtu, 65536, RadiusAttributes),
	A2 = radius_attributes:add(?UserName, UserName, A1),
	A3 = radius_attributes:add(?NasPort, 0, A2),
	A4 = radius_attributes:add(?NasIdentifier, NasId, A3),
	A5 = radius_attributes:add(?CallingStationId, MAC, A4),
	A6 = radius_attributes:add(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16,0)), A5),
	Request1 = #radius{code = ?AccessRequest, id = RadId,
		authenticator = ReqAuth, attributes = A6},
	ReqPacket1 = radius:codec(Request1),
	MsgAuth1 = crypto:hmac(md5, Secret, ReqPacket1),
	A7 = radius_attributes:store(?MessageAuthenticator, MsgAuth1, A6),
	Request2 = Request1#radius{attributes = A7},
	ReqPacket2 = radius:codec(Request2),
	ok = gen_udp:send(Socket, Address, Port, ReqPacket2).

access_challenge(Socket, Address, Port, Secret, RadId, ReqAuth) ->
	receive_radius(?AccessChallenge, Socket, Address, Port, Secret, RadId, ReqAuth).

receive_radius(Code, Socket, Address, Port, Secret, RadId, ReqAuth) ->
	{ok, {Address, Port, RespPacket1}} = gen_udp:recv(Socket, 0),
	Resp1 = radius:codec(RespPacket1),
	#radius{code = Code, id = RadId, authenticator = RespAuth,
		attributes = BinRespAttr1} = Resp1,
	Resp2 = Resp1#radius{authenticator = ReqAuth},
	RespPacket2 = radius:codec(Resp2),
	RespAuth = binary_to_list(crypto:hash(md5, [RespPacket2, Secret])),
	RespAttr1 = radius_attributes:codec(BinRespAttr1),
	{ok, MsgAuth} = radius_attributes:find(?MessageAuthenticator, RespAttr1),
	RespAttr2 = radius_attributes:store(?MessageAuthenticator,
			list_to_binary(lists:duplicate(16, 0)), RespAttr1),
	Resp3 = Resp2#radius{attributes = RespAttr2},
	RespPacket3 = radius:codec(Resp3),
	MsgAuth = crypto:hmac(md5, Secret, RespPacket3),
	EapMsg = radius_attributes:get_all(?EAPMessage, RespAttr1),
	iolist_to_binary(EapMsg).

