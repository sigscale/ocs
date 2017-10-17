%%% ocs_simple_auth_fsm.erl
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
%%% @doc This library module implements functions for simple authentication
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc2865">
%%% 	RFC2865 - Remote Authentication Dial In User Service (RADIUS)</a>
%%%
-module(ocs_simple_auth_fsm).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the ocs_simple_auth_fsm API
-export([]).

%% export the ocs_simple_auth_fsm state callbacks
-export([request/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_nas_application_rfc7155.hrl").

-record(statedata,
		{protocol :: radius | diameter,
		server_address :: undefined | inet:ip_address(),
		server_port :: undefined | pos_integer(),
		client_address :: undefined | inet:ip_address(),
		client_port :: undefined | pos_integer(),
		radius_fsm :: undefined | pid(),
		shared_secret :: undefined | binary(),
		session_id :: string() | {NAS :: inet:ip_address() | string(),
				Port :: string(), Peer :: string()},
		radius_id :: undefined | byte(),
		req_auth :: undefined | binary(),
		req_attr :: undefined | radius_attributes:attributes(),
		subscriber :: undefined | string(),
		multisession :: undefined | boolean(),
		app_id :: undefined | non_neg_integer(),
		auth_request_type :: undefined | 1..3,
		origin_host :: undefined | string(),
		origin_realm :: undefined | string(),
		password :: undefined | string(),
		diameter_port_server :: undefined | pid(),
		request :: undefined | #diameter_nas_app_AAR{}}).

-type statedata() :: #statedata{}.

-define(TIMEOUT, 30000).

%%----------------------------------------------------------------------
%%  The ocs_simple_auth_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_simple_auth_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName :: atom(), StateData :: statedata()}
		| {ok, StateName :: atom(), StateData :: statedata(),
			Timeout :: non_neg_integer() | infinity}
		| {ok, StateName :: atom(), StateData :: statedata(), hibernate}
		| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		SessId, AppId, AuthType, OHost, ORealm, Request, Options] = _Args) ->
	[Subscriber, Password] = Options,
	case global:whereis_name({ocs_diameter_auth, ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			process_flag(trap_exit, true),
			StateData = #statedata{protocol = diameter, session_id = SessId,
					app_id = AppId, auth_request_type = AuthType, origin_host = OHost,
					origin_realm = ORealm, subscriber = Subscriber, password = Password,
					server_address = ServerAddress, server_port = ServerPort,
					client_address = ClientAddress, client_port = ClientPort,
					diameter_port_server = PortServer, request = Request},
			{ok, request, StateData, 0}
	end;
init([radius, ServerAddress, ServerPort, ClientAddress, ClientPort, RadiusFsm,
		Secret, SessionID, #radius{code = ?AccessRequest, id = ID,
		authenticator = Authenticator, attributes = Attributes}] = _Args) ->
	StateData = #statedata{protocol = radius, server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort, radius_fsm = RadiusFsm, shared_secret = Secret,
		session_id = SessionID, radius_id = ID, req_auth = Authenticator,
		req_attr = Attributes},
	process_flag(trap_exit, true),
	{ok, request, StateData, 0}.

-spec request(Event, StateData) -> Result
	when
		Event :: timeout | term(), 
		StateData :: statedata(),
		Result :: {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>request</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
request(timeout, #statedata{protocol = radius, req_attr = Attributes,
		session_id = SessionID} = StateData) ->
	case radius_attributes:find(?UserName, Attributes) of
		{ok, Subscriber} ->
			request1(StateData#statedata{subscriber = Subscriber});
		{error, not_found} ->
			response(?AccessReject, [], StateData),
			{stop, {shutdown, SessionID}, StateData}
	end;
request(timeout, #statedata{protocol = diameter, session_id = SessionID,
		server_address = ServerAddress, server_port = ServerPort,
		subscriber = Subscriber, password = Password, app_id = AppId,
		auth_request_type = Type, origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer, client_address = ClientAddress,
		client_port = ClientPort, request = Request} = StateData) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	case ocs:authorize(Subscriber, Password) of
		{ok, _, _Attr} ->
			Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
					'Auth-Application-Id' = AppId, 'Auth-Request-Type' = Type,
					'Origin-Host' = OHost, 'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
					'Origin-Realm' = ORealm },
			ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
			gen_server:cast(PortServer, {self(), Answer}),
			{stop, {shutdown, SessionID}, StateData};
		{error, _Reason} ->
			Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
					'Auth-Application-Id' = AppId, 'Auth-Request-Type' = Type,
					'Origin-Host' = OHost,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
					'Origin-Realm' = ORealm },
			ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
			gen_server:cast(PortServer, {self(), Answer}),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
request1(#statedata{req_attr = Attributes, req_auth = Authenticator,
		shared_secret = Secret, session_id = SessionID} = StateData) ->
	case radius_attributes:find(?UserPassword, Attributes) of
		{ok, Hidden} ->
			Password = radius_attributes:unhide(Secret, Authenticator, Hidden),
			NewStateData = StateData#statedata{password = Password},
			request2(NewStateData);
		{error, not_found} ->
			response(?AccessReject, [], StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
request2(#statedata{subscriber = Subscriber, password = Password} = StateData) ->
	case existing_sessions(Subscriber) of
		false ->
			request3(list_to_binary(Password), StateData);
		{true, false, [ExistingSessionAtt]} ->
			NewStateData = StateData#statedata{multisession = false},
			case pg2:get_closest_pid(ocs_radius_acct_port_sup) of
				{error, Reason} ->
					request5(Reason, NewStateData);
				DiscFsmSup ->
					start_disconnect(DiscFsmSup, ExistingSessionAtt, NewStateData)
			end;
		{true, true, _ExistingSessionAtt} ->
			NewStateData = StateData#statedata{multisession = true},
			request3(list_to_binary(Password), NewStateData);
		{error, _Reason} ->
			request5(not_found, StateData)
	end.
%% @hidden
request3(<<>>, #statedata{subscriber = Subscriber} = StateData) ->
	case ocs:authorize(ocs:normalize(Subscriber), []) of
		{ok, <<>>, Attributes} ->
			request4(?AccessAccept, Attributes, StateData);
		{ok, PSK, Attributes} when is_binary(PSK) ->
			VendorSpecific = {?Mikrotik, ?MikrotikWirelessPsk, binary_to_list(PSK)},
			ResponseAttributes = radius_attributes:store(?VendorSpecific,
					VendorSpecific, Attributes),
			request4(?AccessAccept, ResponseAttributes, StateData);
		{error, Reason} ->
			request5(Reason, StateData)
	end;
request3(Password, #statedata{subscriber = Subscriber} = StateData) ->
	case ocs:authorize(Subscriber, Password) of
		{ok, _, ResponseAttributes} ->
			request4(?AccessAccept, ResponseAttributes, StateData);
		{error, Reason} ->
			request5(Reason, StateData)
	end.
%% @hidden
request4(RadiusPacketType, ResponseAttributes, #statedata{session_id = SessionID,
		req_attr = Attributes, subscriber = Subscriber,
		multisession = MultiSessions} = StateData) ->
	case add_session_attributes(Subscriber, Attributes, MultiSessions) of
		ok ->
			response(RadiusPacketType, ResponseAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		{error, Reason} ->
			request5(Reason, StateData)
	end.
%% @hidden
request5(out_of_credit, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Out of Credit"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
request5(disabled, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Subscriber Disabled"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
request5(bad_password, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Bad Password"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
request5(not_found, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Unknown Username"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
request5(_, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Unable to comply"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData}.

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(), 
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
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
		From :: {Pid :: pid(), Tag :: term()},
		StateName :: atom(), 
		StateData :: statedata(),
		Result :: {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: statedata()}
		| {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), Reply :: term(), NewStateData :: statedata()}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(), 
		StateName :: atom(), 
		StateData :: statedata(),
		Result :: {next_state, NextStateName :: atom(), NewStateData :: statedata()}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity}
		| {next_state, NextStateName :: atom(), NewStateData :: statedata(), hibernate}
		| {stop, Reason :: normal | term(), NewStateData :: statedata()}.
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info({'EXIT', _Fsm, {shutdown, _SessionID}}, _StateName,
		#statedata{password = Password} = StateData) ->
	request3(list_to_binary(Password), StateData).

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
		Result :: {ok, NextStateName :: atom(), NewStateData :: statedata()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec response(RadiusCode, ResponseAttributes, StateData) -> ok
	when
		RadiusCode :: byte(),
		ResponseAttributes :: radius_attributes:attributes(),
		StateData :: statedata().
%% @doc Send a RADIUS Access-Reject or Access-Accept reply
%% @hidden
response(RadiusCode, ResponseAttributes,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		radius_id = RadiusID, req_auth = RequestAuthenticator,
		shared_secret = Secret, radius_fsm = RadiusFsm,
		req_attr = RequestAttributes} = _StateData) ->
	AttributeList1 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, ResponseAttributes),
	Attributes1 = radius_attributes:codec(AttributeList1),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = crypto:hmac(md5, Secret, [<<RadiusCode, RadiusID,
			Length:16>>, RequestAuthenticator, Attributes1]),
	AttributeList2 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttributeList1),
	Attributes2 = radius_attributes:codec(AttributeList2),
	ResponseAuthenticator = crypto:hash(md5, [<<RadiusCode, RadiusID,
			Length:16>>, RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = RadiusCode, id = RadiusID,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	ResponsePacket = radius:codec(Response),
	Type = case RadiusCode of
		?AccessAccept ->
			accept;
		?AccessReject ->
			reject
	end,
	ok = ocs_log:auth_log(radius, {ServerAddress, ServerPort},
			{ClientAddress, ClientPort}, Type, RequestAttributes, AttributeList2),
	radius:response(RadiusFsm, {response, ResponsePacket}).

-spec existing_sessions(Subscriber) -> Result
	when
		Subscriber :: string() | binary(),
		Result :: boolean() | {boolean(), MultiSessionStatus, SessionAttributes}
				| {error, Reason},
		MultiSessionStatus :: boolean(),
		SessionAttributes :: [radius_attributes:attributes()],
		Reason :: term().
%% @doc Checks for `Subscriber's existing sessions on OCS. Only if the
%% `Subscriber' is authenticatable, returns
%%  {true, `MultiSessionStatus', `SessionAttributes'}.
%% @hidden
existing_sessions(Subscriber) ->
	case ocs:find_subscriber(Subscriber) of
		{ok, #subscriber{session_attributes = []}} ->
			false;
		{ok, #subscriber{session_attributes = SessionAttr, enabled = true,
				balance = Balance, multisession = MultiSessionStatus}}
				when Balance > 0 ->
			{true, MultiSessionStatus, SessionAttr};
		{ok, #subscriber{balance = Balance}} when Balance == 0; Balance < 0 ->
			{error, out_of_credit};
		{ok, #subscriber{enabled = false}} ->
			{error, disabled};
		{error, _Reason} ->
			{error, not_found}
	end.

-spec add_session_attributes(Subscriber, Attributes, MultiSessionStatus) -> Result
	when
		Subscriber :: string() | binary(),
		Attributes :: radius_attributes:attributes(),
		MultiSessionStatus :: undefined | boolean(),
		Result :: ok | {error, term()}.
%% @doc Extract session related attributes from `Attributes' and append
%% to `Subscriber's existing session attribute list.
%% @hidden
add_session_attributes(Subscriber, Attributes, MultiSessionStatus) ->
	NewSessionAttrs = extract_session_attributes(Attributes),
	{ok, #subscriber{session_attributes = CurrentSessionList} = S}
			= ocs:find_subscriber(Subscriber),
	NewRecord = case MultiSessionStatus of
		false ->
			S#subscriber{session_attributes = [NewSessionAttrs]};
		_ ->
			NewSessionList = [NewSessionAttrs | CurrentSessionList],
			S#subscriber{session_attributes = NewSessionList}
	end,
	F = fun() ->
		mnesia:write(NewRecord)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, _} ->
			ok
	end.

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

-spec start_disconnect(DiscFsmSup, ExistingSessionAtt, StateData) -> Result
	when
		DiscFsmSup :: pid(),
		ExistingSessionAtt :: radius_attributes:attributes(),
		StateData :: statedata(),
		Result :: term().
%% @doc Start a disconnect_fsm worker.
start_disconnect(DiscFsmSup, ExistingSessionAtt, #statedata{client_address =
		Address} = StateData) ->
	try
			F = fun(_F, [{K, V} | _T], K) ->
						V;
					(F, [_ | T], K) ->
						F(F, T, K);
					(_, [], _) ->
						undefined
			end,
			NasIp= F(F, ExistingSessionAtt, ?NasIpAddress),
			NasId = F(F, ExistingSessionAtt, ?NasIdentifier),
			Nas = case {NasIp, NasId} of
				{_, undefined} ->
					NasIp;
				_ ->
					NasId
			end,
			{ok, #client{port = ListenPort, protocol = radius, secret = Secret}}
					= ocs:find_client(Address),
			Subscriber = F(F, ExistingSessionAtt, ?UserName),
			AcctSessionId = F(F, ExistingSessionAtt, ?AcctSessionId),
			DiscArgs = [Address, Nas, Subscriber, AcctSessionId, Secret,
					ListenPort, ExistingSessionAtt, 1],
			StartArgs = [DiscArgs, []],
			case supervisor:start_child(DiscFsmSup, StartArgs) of
				{ok, DiscFsm} ->
					link(DiscFsm),
					{next_state, request, StateData};
				{error, Reason} ->
					error_logger:error_report(["Failed to initiate session disconnect function",
							{module, ?MODULE}, {subscriber, Subscriber}, {nas, NasId},
							{address, Address}, {session, AcctSessionId}, {error, Reason}]),
					{next_state, request, StateData}
			end
	catch
		_: Reason1 ->
			request5(Reason1, StateData)
	end.

