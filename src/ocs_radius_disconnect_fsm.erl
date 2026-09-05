%%% ocs_radius_disconnect_fsm.erl
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
%%% 	module implements sending Disconnect Messages (DM) to
%%% 	Network Access Servers (NAS)
%%% 	in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://www.rfc-editor.org/info/rfc3576/">
%%% 	RFC3576 - Dynamic Authorization Extensions for RADIUS</a>
%%%
-module(ocs_radius_disconnect_fsm).
-copyright('Copyright (c) 2016 - 2026 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% export the callbacks for gen_statem states
-export([send_request/3, receive_response/3]).

-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").
-include("ocs.hrl").
-record(statedata,
		{id :: integer(),
		 nas_ip :: inet:ip_address(),
		 nas_id :: undefined | string(),
		 port :: non_neg_integer(),
		 subscriber :: string(),
		 acct_session_id :: string(),
		 secret :: binary(),
		 socket :: undefined | inet:socket(),
		 retry_time = 500 :: integer(),
		 retry_count = 0 :: integer(),
		 request :: undefined | binary(),
		 attributes :: radius_attributes:attributes()}).
-type statedata() :: #statedata{}.
-type state() :: idle.

-define(TIMEOUT, 30000).
-define(ERRORLOG, radius_disconnect_error).

%%----------------------------------------------------------------------
%%  The ocs_radius_disconnect_fsm gen_statem call backs
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
init([Subscriber, {_, SessionAttributes}] = _Args) ->
	process_flag(trap_exit, true),
	NasIp = proplists:get_value(?NasIpAddress, SessionAttributes),
	NasId = proplists:get_value(?NasIdentifier, SessionAttributes),
	AcctSessionId = proplists:get_value(?AcctSessionId, SessionAttributes),
	Id = 1,
	case lookup_client(NasIp, NasId) of
		{ok, #client{port = undefined}} ->
			ignore;
		{ok, #client{port = 0}} ->
			ignore;
		{ok, #client{address = Address, identifier = NasID,
				secret = Secret, port = Port}} ->
			Data = #statedata{nas_ip = Address, 
					nas_id = binary_to_list(NasID),
					subscriber = Subscriber, acct_session_id = AcctSessionId,
					secret = Secret, attributes = SessionAttributes, id = Id,
					port = Port},
			Action = {next_event, internal, start},
			{ok, send_request, Data, Action};
		{error, not_found} ->
			{stop, {shutdown, client_not_found}};
		{error, _Reason} ->
			{stop, shutdown}
	end.

-spec send_request(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>send_request</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
send_request(internal = _EventType, start = _EventContent,
		#statedata{nas_ip = Address, port = Port, id = Id,
				secret = SharedSecret, attributes = Attributes,
				retry_time = Retry} = Data) ->
	DiscAttrList  = extract_attributes(Attributes),
	DiscAttr = radius_attributes:codec(DiscAttrList),
	Length = size(DiscAttr) + 20,
	RequestAuthenticator = crypto:hash(md5,
			[<<?DisconnectRequest, Id, Length:16>>,
			<<0:128>>, DiscAttr, SharedSecret]),
	DisconRec = #radius{code = ?DisconnectRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = DiscAttr},
	DisconnectRequest = radius:codec(DisconRec),
	TimeoutAction = {timeout, Retry, initial},
	case gen_udp:open(0, [{active, once}, binary]) of
		{ok, Socket} ->
			case gen_udp:send(Socket, Address, Port, DisconnectRequest) of
				ok ->
					NewData = Data#statedata{id = Id, socket = Socket,
							request = DisconnectRequest},
					{next_state, receive_response, NewData, TimeoutAction};
				{error, _Reason} ->
					{next_state, receive_response, Data, TimeoutAction}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.

-spec receive_response(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>receive_response</em> state.
%% @@see //stdlib/gen_statem:StateName/3
%% @private
%%
receive_response(timeout = _EventType, retry = _EventContent,
		#statedata{retry_count = Count,
				nas_id = NasId, subscriber = Subscriber,
				acct_session_id = AcctSessionId} = _Data)
				when Count > 5 ->
	{stop, {shutdown, {NasId, Subscriber, AcctSessionId}}};
receive_response(timeout = _EventType, _EventContent,
		#statedata{socket = Socket,
				nas_ip = NasIp, port = Port,
				request =  DisconnectRequest,
				retry_count = Count,
				retry_time = Retry} = Data) ->
	NewRetry = Retry * 2,
	NewCount = Count + 1,
	NewData = Data#statedata{retry_count = NewCount,
			retry_time = NewRetry},
	TimeoutAction = {timeout, NewRetry, retry},
	case gen_udp:send(Socket, NasIp, Port, DisconnectRequest) of
		ok ->
			{keep_state, NewData, TimeoutAction};
		{error, _Reason} ->
			{keep_state, NewData, TimeoutAction}
	end;
receive_response(info = _EventType,
		{udp, _, NasIp, NasPort, Packet} = _EventContent,
		#statedata{id = Id,
				nas_id = NasId, subscriber = Subscriber,
				acct_session_id = AcctSessionId} = _Data) ->
	case radius:codec(Packet) of
		#radius{code = ?DisconnectAck, id = Id} ->
			F = fun() ->
				case mnesia:read(service, Subscriber, write) of
					[#service{disconnect = false} = Entry] ->
						NewEntry = Entry#service{disconnect = true},
						mnesia:write(service, NewEntry, write);
					[#service{disconnect = true}] ->
						ok
				end
			end,
			mnesia:transaction(F);
		#radius{code = ?DisconnectNak, id = Id, attributes = Attrbin} ->
			Attr = radius_attributes:codec(Attrbin),
			case radius_attributes:find(?ErrorCause, Attr) of
				{ok, ErrorCause} ->
					error_logger:error_report(["Failed to disconnect subscriber session",
							{server, NasIp}, {port, NasPort},
							{error, radius_attributes:error_cause(ErrorCause)}]);
				{error, not_found} ->
					error_logger:error_report(["Failed to disconnect subscriber session",
							{server, NasIp}, {port, NasPort}])
			end
	end,
	{stop, {shutdown, {NasId, Subscriber, AcctSessionId}}}.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, #statedata{socket = undefined} = _Data) ->
	ok;
terminate(_Reason, _State, #statedata{socket = Socket} = _Data) ->
	gen_udp:close(Socket).

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

-spec extract_attributes(Attributes) -> NewAttrList
	when
		Attributes :: radius_attributes:attributes(),
		NewAttrList :: radius_attributes:attributes().
%% @doc extract radius attributes needed for a Disconnect/Request
%% @private
%%
extract_attributes(Attributes) ->
	F = fun({K, _}) when K == ?NasIdentifier; K == ?NasIpAddress;
				K == ?UserName; K == ?FramedIpAddress; K == ?NasPort;
				K == ?CalledStationId; K == ?CallingStationId;
				K == ?AcctSessionId; K == ?AcctMultiSessionId; K == ?NasPortId ->
			true;
		(_) ->
			false
	end,
	lists:filter(F, Attributes).

%% @hidden
lookup_client(NasIp, NasId) when is_list(NasIp)->
	{ok, Address} = inet_parse:address(NasIp),
	lookup_client(Address, NasId);
lookup_client(NasIp, NasId) when is_list(NasId)->
	lookup_client(NasIp, list_to_binary(NasId));
lookup_client(undefined, NasId) when is_binary(NasId) ->
	F = fun() ->
			case mnesia:index_read(client, NasId, #client.identifier) of
				[Client] ->
					Client;
				[] ->
					throw(not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, Client} ->
			{ok, Client};
		{aborted, {throw, not_found}} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end;
lookup_client(NasIp, _NasId) ->
	F = fun() ->
			case mnesia:read(client, NasIp, read) of
				[Client] ->
					Client;
				[] ->
					throw(not_found)
			end
	end,
	case mnesia:transaction(F) of
		{atomic, Client} ->
			{ok, Client};
		{aborted, {throw, not_found}} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

