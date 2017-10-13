%%% ocs_simple_auth_fsm.erl
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
		res_attr :: undefined | radius_attributes:attributes(),
		subscriber :: undefined | string(),
		multisession :: undefined | boolean(),
		app_id :: undefined | non_neg_integer(),
		auth_request_type :: undefined | 1..3,
		origin_host :: undefined | string(),
		origin_realm :: undefined | string(),
		dest_host :: undefined | string(),
		dest_realm :: undefined | string(),
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
		SessId, AppId, AuthType, OHost, ORealm, Request, DHost, DRealm, Options] = _Args) ->
	[Subscriber, Password] = Options,
	case global:whereis_name({ocs_diameter_auth, ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			process_flag(trap_exit, true),
			StateData = #statedata{protocol = diameter, session_id = SessId,
					app_id = AppId, auth_request_type = AuthType, origin_host = OHost,
					origin_realm = ORealm, dest_host = DHost, dest_realm = DRealm,
					subscriber = Subscriber, password = Password,
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
request(timeout, #statedata{protocol = radius} = StateData) ->
	handle_radius(StateData);
request(timeout, #statedata{protocol = diameter, session_id = SessionID,
		server_address = ServerAddress, server_port = ServerPort,
		subscriber = Subscriber, password = Password, app_id = AppId,
		auth_request_type = Type, origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer, client_address = ClientAddress,
		client_port = ClientPort, request = Request} = StateData) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	case ocs:authorize(Subscriber, Password) of
		{ok, _} ->
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
	end;
request(disconnected, #statedata{protocol = radius} = StateData) ->
	handle_radius3(StateData).

%% @hidden
handle_radius(#statedata{req_attr = Attributes, req_auth = Authenticator,
		session_id = SessionID, shared_secret = Secret} = StateData) ->
	try
		Subscriber = radius_attributes:fetch(?UserName, Attributes),
		Hidden = radius_attributes:fetch(?UserPassword, Attributes),
		Password = radius_attributes:unhide(Secret, Authenticator, Hidden),
		NewStateData = StateData#statedata{subscriber = Subscriber,
				password = list_to_binary(Password)},
		handle_radius1(NewStateData)
	catch
		_:_ ->
			response(?AccessReject, [], StateData),
			{stop, {shutdown, SessionID}, StateData}
	end.
%% @hidden
handle_radius1(#statedata{subscriber = SubscriberId, password = <<>>} = StateData) ->
	case ocs:authorize(ocs:normalize(SubscriberId), []) of
		{ok, #subscriber{password = <<>>,
				attributes = Attributes} = Subscriber} ->
			NewStateData = StateData#statedata{res_attr = Attributes},
			handle_radius2(Subscriber, NewStateData);
		{ok, PSK, #subscriber{attributes = Attributes}
				= Subscriber} when is_binary(PSK) ->
			VendorSpecific = {?Mikrotik, ?MikrotikWirelessPsk, binary_to_list(PSK)},
			ResponseAttributes = radius_attributes:store(?VendorSpecific,
					VendorSpecific, Attributes),
			NewStateData = StateData#statedata{res_attr = ResponseAttributes},
			handle_radius2(Subscriber, NewStateData);
		{error, Reason} ->
			reject_radius(Reason, StateData)
	end;
handle_radius1(#statedata{subscriber = SubscriberId, password = Password} = StateData) ->
	case ocs:authorize(ocs:normalize(SubscriberId), Password) of
		{ok, #subscriber{attributes = Attributes} = Subscriber} ->
			NewStateData = StateData#statedata{res_attr = Attributes},
			handle_radius2(Subscriber, NewStateData);
		{error, Reason} ->
			reject_radius(Reason, StateData)
	end.
%% @hidden
handle_radius2(#subscriber{multisession = true}, StateData) ->
	NewStateData = StateData#statedata{multisession = true},
	handle_radius3(NewStateData);
handle_radius2(#subscriber{session_attributes = [], multisession = MultiSession}, StateData) ->
	NewStateData = StateData#statedata{multisession = MultiSession},
	handle_radius3(NewStateData);
handle_radius2(#subscriber{multisession = false, session_attributes = SessionAttributes},
		#statedata{subscriber = SubscriberId, client_address = Address, client_port = ListenPort,
		shared_secret = Secret, session_id = SessionID} = StateData) ->
	case pg2:get_closest_pid(ocs_radius_acct_port_sup) of
		{error, Reason} ->
			reject_radius(Reason, StateData);
		DiscSup ->
			try
				NAS = case {radius_attributes:find(?NasIpAddress, SessionAttributes),
							radius_attributes:find(?NasIdentifier, SessionAttributes)} of
					{{_, IP}, {error, _}} ->
						IP;
					{_, {ok, ID}} ->
						ID
				end,
				DiscArgs = [Address, NAS, SubscriberId, SessionID, Secret,
						ListenPort, SessionAttributes, 1],
				StartArgs = [DiscArgs, []],
				NewStateData = StateData#statedata{multisession = false},
				case supervisor:start_child(DiscSup, StartArgs) of
					{ok, DiscFsm} ->
						link(DiscFsm),
						{next_state, request, NewStateData};
					{error, Reason} ->
						error_logger:error_report(["Failed to initiate session disconnect function",
								{module, ?MODULE}, {subscriber, SubscriberId}, {nas, NAS},
								{address, Address}, {session, SessionID}, {error, Reason}]),
						{next_state, request, NewStateData}
				end
			catch
				_:R ->
					reject_radius(R, StateData)
			end
	end.
%% @hidden
handle_radius3(#statedata{subscriber = SubscriberId, multisession = MultiSession,
		req_attr = RequestAttributes, res_attr = ResponseAttributes,
		session_id = SessionID} = StateData) ->
	F = fun() ->
		case mnesia:read(subscriber, list_to_binary(SubscriberId), write) of
			[#subscriber{session_attributes = CurrentAttributes} = Subscriber] ->
				SessionAttributes = extract_session_attributes(RequestAttributes),
				Entry = case MultiSession of
					true ->
						NewSessionAttributes = [SessionAttributes | CurrentAttributes],
						Subscriber#subscriber{session_attributes = NewSessionAttributes};
					false ->
						Subscriber#subscriber{session_attributes = SessionAttributes}
				end,
				mnesia:write(Entry);
			[] ->
				throw(not_found)
		end
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			response(?AccessAccept, ResponseAttributes, StateData),
			{stop, {shutdown, SessionID}, StateData};
		{aborted, {throw, not_found}} ->
			reject_radius(not_found, StateData);
		{aborted, Reason} ->
			reject_radius(Reason, StateData)
	end.

%% @hidden
reject_radius(out_of_credit, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Out of Credit"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
reject_radius(disabled, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Subscriber Disabled"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
reject_radius(bad_password, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Bad Password"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
reject_radius(not_found, #statedata{session_id = SessionID} = StateData) ->
	RejectAttributes = [{?ReplyMessage, "Unknown Username"}],
	response(?AccessReject, RejectAttributes, StateData),
	{stop, {shutdown, SessionID}, StateData};
reject_radius(_, #statedata{session_id = SessionID} = StateData) ->
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
handle_info({'EXIT', _Fsm, {shutdown, _SessionID}}, StateName, StateData) ->
	StateName(disconnected, StateData).

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

