%%% ocs_simple_auth_fsm.erl
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
%%% 	module implements functions for simple authentication
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://www.rfc-editor.org/info/rfc2865/">
%%% 	RFC2865 - Remote Authentication Dial In User Service (RADIUS)</a>
%%%
-module(ocs_simple_auth_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([request/3]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").
-include("ocs_eap_codec.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_nas_application_rfc7155.hrl").

%% service types
-define(DATA, 2).
-define(VOICE, 12).
-define(CC_APPLICATION_ID, 4).

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
		req_auth :: undefined | [byte()] | binary(),
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
		password :: undefined | binary()
				| {ChapId :: 0..255, ChapPassword :: binary(),
						Challenge :: binary()},
		diameter_port_server :: undefined | pid(),
		request :: undefined | #diameter_nas_app_AAR{},
		password_required :: boolean(),
		trusted :: boolean(),
		service_type :: undefined | integer()}).
-type statedata() :: #statedata{}.
-type state() :: request.

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
%%  The ocs_simple_auth_fsm gen_statem call backs
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
%%
init([diameter, ServerAddress, ServerPort, ClientAddress, ClientPort,
		PasswordReq, Trusted, SessId, AppId, AuthType, OHost, ORealm,
		DHost, DRealm, Request, Options] = _Args) ->
	[Subscriber, Password] = Options,
	case global:whereis_name({ocs_diameter_auth,
			node(), ServerAddress, ServerPort}) of
		undefined ->
			{stop, ocs_diameter_auth_port_server_not_found};
		PortServer ->
			process_flag(trap_exit, true),
			ServiceType = case Request of
				#diameter_nas_app_AAR{'Service-Type' = [ST]} ->
					ST;
				_ ->
					undefined
			end,
			Data = #statedata{protocol = diameter, session_id = SessId,
					app_id = AppId, auth_request_type = AuthType, origin_host = OHost,
					origin_realm = ORealm, dest_host = DHost, dest_realm = DRealm,
					subscriber = Subscriber, password = Password,
					server_address = ServerAddress, server_port = ServerPort,
					client_address = ClientAddress, client_port = ClientPort,
					diameter_port_server = PortServer, request = Request,
					password_required = PasswordReq, trusted = Trusted,
					service_type = ServiceType},
			Action = {next_event, internal, start},
			{ok, request, Data, Action}
	end;
init([radius, ServerAddress, ServerPort, ClientAddress, ClientPort,
		RadiusFsm, Secret, PasswordReq, Trusted, SessionID,
		#radius{code = ?AccessRequest, id = ID,
		authenticator = Authenticator, attributes = Attributes}] = _Args) ->
	ServiceType = case radius_attributes:find(?ServiceType, Attributes) of
		{error, not_found} ->
			undefined;
		{_, ST} ->
			ST
	end,
	Data = #statedata{protocol = radius, server_address = ServerAddress,
		server_port = ServerPort, client_address = ClientAddress,
		client_port = ClientPort, radius_fsm = RadiusFsm, shared_secret = Secret,
		session_id = SessionID, radius_id = ID, req_auth = Authenticator,
		req_attr = Attributes, password_required = PasswordReq,
		trusted = Trusted, service_type = ServiceType},
	process_flag(trap_exit, true),
	Action = {next_event, internal, start},
	{ok, request, Data, Action}.

-spec request(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>request</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%% @todo handle muliti session for diameter
%%
request(internal = _EventType, start = _EventContent,
		#statedata{protocol = radius} = Data) ->
	handle_radius(Data);
request(internal = _EventType, start = _EventContent,
		#statedata{protocol = diameter} = Data) ->
	handle_diameter(Data).

%% @hidden
handle_radius(#statedata{req_attr = Attributes, session_id = SessionID,
		password_required = false} = Data) ->
	try
		Subscriber = radius_attributes:fetch(?UserName, Attributes),
		NewData = Data#statedata{subscriber = Subscriber,
				password = <<>>},
		handle_radius1(NewData)
	catch
		_:_ ->
			response(?AccessReject, [], Data),
			{stop, {shutdown, SessionID}}
	end;
handle_radius(#statedata{req_attr = Attributes, req_auth = Authenticator,
		session_id = SessionID, shared_secret = Secret,
		password_required = true} = Data) ->
	try
		Subscriber = radius_attributes:fetch(?UserName, Attributes),
		Password = case radius_attributes:find(?UserPassword, Attributes) of
			{ok, Hidden} ->
				list_to_binary(radius_attributes:unhide(Secret,
						Authenticator, Hidden));
			{error, not_found} ->
				{ChapId, ChapPassword} = case radius_attributes:fetch(?ChapPassword,
						Attributes) of
					{N, B} when is_binary(B) ->
						{N, B};
					{N, L} when is_list(L) ->
						{N, list_to_binary(L)}
				end,
				Challenge = case radius_attributes:find(?ChapChallenge,
						Attributes) of
					{ok, ChapChallenge} when is_binary(ChapChallenge) ->
						ChapChallenge;
					{ok, ChapChallenge} when is_list(ChapChallenge) ->
						list_to_binary(ChapChallenge);
					{error, not_found} when is_binary(Authenticator) ->
						Authenticator;
					{error, not_found} when is_list(Authenticator) ->
						list_to_binary(Authenticator)
				end,
				{ChapId, ChapPassword, Challenge}
		end,
		NewData = Data#statedata{subscriber = Subscriber,
				password = Password},
		handle_radius1(NewData)
	catch
		_:_ ->
			response(?AccessReject, [], Data),
			{stop, {shutdown, SessionID}}
	end.
%% @hidden
handle_radius1(#statedata{subscriber = SubscriberId, password = <<>>,
		password_required = PasswordReq, req_attr = ReqAttr} = Data) ->
	Timestamp = calendar:local_time(),
	{ServiceType, Direction, CallAddress} = get_service_type(ReqAttr),
	SessionAttributes = ocs_rating:session_attributes(ReqAttr),
	case ocs_rating:authorize(radius, ServiceType, [SubscriberId], <<>>,
			Timestamp, CallAddress, Direction, SessionAttributes) of
		{authorized, #service{password = <<>>} =
				Subscriber, Attributes, ExistingSessionAttributes} ->
			NewData = Data#statedata{res_attr = Attributes},
			handle_radius2(Subscriber, ExistingSessionAttributes, NewData);
		{authorized, Subscriber, Attributes, ExistingSessionAttributes}
				when PasswordReq == false ->
			NewData = Data#statedata{res_attr = Attributes},
			handle_radius2(Subscriber, ExistingSessionAttributes, NewData);
		{authorized, #service{password = PSK} =
				Subscriber, Attributes, ExistingSessionAttributes}
				when is_binary(PSK) ->
			ResponseAttributes = radius_attributes:store(?Mikrotik,
					?MikrotikWirelessPsk, binary_to_list(PSK), Attributes),
			NewData = Data#statedata{res_attr = ResponseAttributes},
			handle_radius2(Subscriber, ExistingSessionAttributes, NewData);
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(ExistingSessionAttributes, Data),
			reject_radius(disabled, Data);
		{unauthorized, Reason, _ExistingSessionAttributes} ->
			reject_radius(Reason, Data)
	end;
handle_radius1(#statedata{subscriber = SubscriberId, password = Password,
		req_attr = ReqAttr} = Data) ->
	Timestamp = calendar:local_time(),
	{ServiceType, Direction, CallAddress} = get_service_type(ReqAttr),
	SessionAttributes = ocs_rating:session_attributes(ReqAttr),
	case ocs_rating:authorize(radius, ServiceType, [SubscriberId], Password,
			Timestamp, CallAddress, Direction, SessionAttributes) of
		{authorized, Subscriber, Attributes, ExistingSessionAttributes} ->
			NewData = Data#statedata{res_attr = Attributes},
			handle_radius2(Subscriber, ExistingSessionAttributes, NewData);
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(ExistingSessionAttributes, Data),
			reject_radius(disabled, Data);
		{unauthorized, Reason, _ExistingSessionAttributes} ->
			reject_radius(Reason, Data)
	end.
%% @hidden
handle_radius2(#service{multisession = true},
		_ExistingSessions, Data) ->
	handle_radius3(Data);
handle_radius2(#service{session_attributes = []},
		_ExistingSessions, Data) ->
	handle_radius3(Data);
handle_radius2(#service{multisession = false},
		ExistingSessions,  Data) ->
	NewData = Data#statedata{multisession = false},
	start_disconnect(ExistingSessions, NewData),
	handle_radius3(NewData).
%% @hidden
handle_radius3(#statedata{res_attr = ResponseAttributes,
		session_id = SessionID} = Data) ->
	response(?AccessAccept, ResponseAttributes, Data),
	{stop, {shutdown, SessionID}}.

%% @hidden
reject_radius(out_of_credit, #statedata{session_id = SessionID} = Data) ->
	RejectAttributes = [{?ReplyMessage, "Out of Credit"}],
	response(?AccessReject, RejectAttributes, Data),
	{stop, {shutdown, SessionID}};
reject_radius(disabled, #statedata{session_id = SessionID} = Data) ->
	RejectAttributes = [{?ReplyMessage, "Subscriber Disabled"}],
	response(?AccessReject, RejectAttributes, Data),
	{stop, {shutdown, SessionID}};
reject_radius(bad_password, #statedata{session_id = SessionID} = Data) ->
	RejectAttributes = [{?ReplyMessage, "Bad Password"}],
	response(?AccessReject, RejectAttributes, Data),
	{stop, {shutdown, SessionID}};
reject_radius(service_not_found, #statedata{session_id = SessionID} = Data) ->
	RejectAttributes = [{?ReplyMessage, "Unknown Username"}],
	response(?AccessReject, RejectAttributes, Data),
	{stop, {shutdown, SessionID}};
reject_radius(_, #statedata{session_id = SessionID} = Data) ->
	RejectAttributes = [{?ReplyMessage, "Unable to comply"}],
	response(?AccessReject, RejectAttributes, Data),
	{stop, {shutdown, SessionID}}.

%% @hidden
handle_diameter(#statedata{protocol = diameter, session_id = SessionID,
		origin_host = OHost, origin_realm = ORealm, dest_host = DHost,
		dest_realm = DRealm, subscriber = SubscriberId, password = Password,
		auth_request_type = AuthRequestType,
		service_type = ServiceType} = Data) when
		AuthRequestType == ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY';
		AuthRequestType == ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE' ->
	Timestamp = calendar:local_time(),
	SessionAttributes = [{'Origin-Host', OHost}, {'Origin-Realm', ORealm},
			{'Destination-Host', DHost}, {'Destination-Realm', DRealm},
			{'Session-Id', SessionID}],
	case ocs_rating:authorize(diameter, ServiceType, [SubscriberId], Password,
			Timestamp, undefined, undefined, SessionAttributes) of
		{authorized, Subscriber, _Attributes, ExistingSessionAttributes} ->
			handle_diameter1(Subscriber, ExistingSessionAttributes, Data);
		{unauthorized, disabled, ExistingSessionAttributes} ->
			start_disconnect(ExistingSessionAttributes, Data),
			reject_diameter(disabled, Data);
		{unauthorized, Reason, _ExistingSessionAttributes} when
				Reason =/= bad_password, Reason =/= service_not_found,
				AuthRequestType == ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY' ->
			handle_diameter2(Data);
		{unauthorized, Reason, _ExistingSessionAttributes} ->
			reject_diameter(Reason, Data)
	end;
handle_diameter(#statedata{protocol = diameter} = Data) ->
	reject_diameter(unable_to_comply, Data).
%% @hidden
handle_diameter1(#service{multisession = true},
		_ExistingSessions, Data) ->
	NewData = Data#statedata{multisession = true},
	handle_diameter2(NewData);
handle_diameter1(#service{session_attributes = [],
		multisession = MultiSession}, _ExistingSessions, Data) ->
	NewData = Data#statedata{multisession = MultiSession},
	handle_diameter2(NewData);
handle_diameter1(#service{multisession = false},
		ExistingSessions, Data) ->
	NewData = Data#statedata{multisession = false},
	start_disconnect(ExistingSessions, NewData),
	handle_diameter2(NewData).
%% @hidden
handle_diameter2(#statedata{protocol = diameter, session_id = SessionID,
		server_address = ServerAddress, server_port = ServerPort, app_id = AppId,
		auth_request_type = Type, origin_host = OHost, origin_realm = ORealm,
		diameter_port_server = PortServer, client_address = ClientAddress,
		client_port = ClientPort, request = Request} = _Data) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
			'Auth-Application-Id' = AppId, 'Auth-Request-Type' = Type,
			'Origin-Host' = OHost, 'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
			'Origin-Realm' = ORealm},
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}),
	{stop, {shutdown, SessionID}}.

%% @hidden
reject_diameter(Reason, #statedata{session_id = SessionID, app_id = AppId,
		auth_request_type = AuthRequestType, origin_host = OHost,
		origin_realm = ORealm, server_address = ServerAddress,
		server_port = ServerPort, diameter_port_server = PortServer,
		client_address = ClientAddress,
		client_port = ClientPort, request = Request} = _Data) when
		Reason =/= unable_to_comply,
		AuthRequestType == ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY' ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
			'Auth-Application-Id' = AppId,
			'Auth-Request-Type' = AuthRequestType,
			'Origin-Host' = OHost,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Origin-Realm' = ORealm },
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}),
	{stop, {shutdown, SessionID}};
reject_diameter(Reason, #statedata{session_id = SessionID, app_id = AppId,
		auth_request_type = AuthRequestType, origin_host = OHost,
		origin_realm = ORealm, server_address = ServerAddress,
		server_port = ServerPort, diameter_port_server = PortServer,
		client_address = ClientAddress, client_port = ClientPort,
		request = Request} = _Data) when
		AuthRequestType == ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
		((Reason == bad_password) orelse (Reason == service_not_found)) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
			'Auth-Application-Id' = AppId,
			'Auth-Request-Type' = AuthRequestType,
			'Origin-Host' = OHost,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHENTICATION_REJECTED',
			'Origin-Realm' = ORealm },
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}),
	{stop, {shutdown, SessionID}};
reject_diameter(Reason, #statedata{session_id = SessionID, app_id = AppId,
		auth_request_type = AuthRequestType, origin_host = OHost,
		origin_realm = ORealm, server_address = ServerAddress,
		server_port = ServerPort, diameter_port_server = PortServer,
		client_address = ClientAddress, client_port = ClientPort,
		request = Request} = _Data) when
		Reason =/= unable_to_comply,
		AuthRequestType == ?'DIAMETER_BASE_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE' ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
			'Auth-Application-Id' = AppId,
			'Auth-Request-Type' = AuthRequestType,
			'Origin-Host' = OHost,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED',
			'Origin-Realm' = ORealm },
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}),
	{stop, {shutdown, SessionID}};
reject_diameter(_Reason, #statedata{session_id = SessionID, app_id = AppId,
		auth_request_type = AuthRequestType, origin_host = OHost,
		origin_realm = ORealm, server_address = ServerAddress,
		server_port = ServerPort, diameter_port_server = PortServer,
		client_address = ClientAddress, client_port = ClientPort,
		request = Request} = _Data) ->
	Server = {ServerAddress, ServerPort},
	Client= {ClientAddress, ClientPort},
	Answer = #diameter_nas_app_AAA{'Session-Id' = SessionID,
			'Auth-Application-Id' = AppId,
			'Auth-Request-Type' = AuthRequestType,
			'Origin-Host' = OHost,
			'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
			'Origin-Realm' = ORealm },
	ok = ocs_log:auth_log(diameter, Server, Client, Request, Answer),
	gen_server:cast(PortServer, {self(), Answer}),
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

-spec response(RadiusCode, ResponseAttributes, Data) -> ok
	when
		RadiusCode :: byte(),
		ResponseAttributes :: radius_attributes:attributes(),
		Data :: statedata().
%% @doc Send a RADIUS Access-Reject or Access-Accept reply
%% @hidden
response(RadiusCode, ResponseAttributes,
		#statedata{server_address = ServerAddress, server_port = ServerPort,
		client_address = ClientAddress, client_port = ClientPort,
		radius_id = RadiusID, req_auth = RequestAuthenticator,
		shared_secret = Secret, radius_fsm = RadiusFsm,
		req_attr = RequestAttributes} = _Data) ->
	AttributeList1 = radius_attributes:add(?MessageAuthenticator,
			<<0:128>>, ResponseAttributes),
	Attributes1 = radius_attributes:codec(AttributeList1),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = ?HMAC(Secret, [<<RadiusCode, RadiusID,
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

%% @hidden
start_disconnect(SessionList, #statedata{protocol = radius,
		client_address = Address, subscriber = SubscriberId, session_id = SessionID}
		= State) ->
	case ?PG_CLOSEST(ocs_radius_acct_port_sup) of
		{error, Reason} ->
			error_logger:error_report(["Failed to initiate session disconnect function",
					{module, ?MODULE}, {subscriber, SubscriberId}, {address, Address},
					{session, SessionID}, {error, Reason}]);
		DiscSup ->
			start_disconnect1(DiscSup, SessionList, State)
	end;
start_disconnect(SessionList, #statedata{protocol = diameter, session_id = SessionID,
		origin_host = OHost, origin_realm = ORealm, subscriber = SubscriberId} = State) ->
	case ?PG_CLOSEST(ocs_diameter_acct_port_sup) of
		{error, Reason} ->
			error_logger:error_report(["Failed to initiate session disconnect function",
					{module, ?MODULE}, {subscriber, SubscriberId}, {origin_host, OHost},
					{origin_realm, ORealm}, {session, SessionID}, {error, Reason}]);
		DiscSup ->
			start_disconnect1(DiscSup, SessionList, State)
	end.
%% @hidden
start_disconnect1(_DiscSup, [], _State) ->
	ok;
start_disconnect1(DiscSup, [H | Tail], #statedata{protocol = radius} = State) ->
	start_disconnect2(DiscSup, H, State),
	start_disconnect1(DiscSup, Tail, State);
start_disconnect1(DiscSup, SessionList, State) ->
	start_disconnect3(DiscSup, State, SessionList).
%% @hidden
start_disconnect2(DiscSup, SessionAttributes, #statedata{protocol = radius,
		subscriber = Subscriber}) ->
	DiscArgs = [Subscriber, SessionAttributes],
	StartArgs = [DiscArgs, []],
	supervisor:start_child(DiscSup, StartArgs).
%% @hidden
start_disconnect3(_DiscSup, #statedata{protocol = diameter}, []) ->
	ok;
start_disconnect3(DiscSup, #statedata{protocol = diameter, session_id = SessionID,
		origin_host = OHost, origin_realm = ORealm, dest_host = DHost, dest_realm = DRealm}
		= State, [_ | T]) ->
	Svc = ocs_diameter_acct_service,
	Alias = ocs_diameter_base_application,
	AppId = ?CC_APPLICATION_ID,
	DiscArgs = [Svc, Alias, SessionID, OHost, DHost, ORealm, DRealm, AppId],
	StartArgs = [DiscArgs, []],
	supervisor:start_child(DiscSup, StartArgs),
	start_disconnect3(DiscSup, State, T).

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

