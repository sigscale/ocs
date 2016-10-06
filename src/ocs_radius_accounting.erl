%%% ocs_radius_accounting.erl
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
%%% @doc This {@link //radius/radius. radius} behaviour callback
%%% 	module performs authentication procedures in the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs_radius_accounting).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(radius).

%% export the radius behaviour callbacks
-export([init/2, request/4, terminate/2]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").

-define(LOGNAME, radius_acct).

-record(state,
		{dir :: string(),
		log :: disk_log:log()}).

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

-spec init(Address :: inet:ip_address(), Port :: pos_integer()) ->
	Result :: {ok, State :: #state{}} | {error, Reason :: term()}.
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(_Address, _Port) ->
	{ok, Directory} = application:get_env(ocs, accounting_dir),
	Log = ?LOGNAME,
	FileName = Directory ++ "/" ++ atom_to_list(Log),
	State = #state{dir = Directory},
	try case file:list_dir(Directory) of
		{ok, _} ->
			ok;
		{error, enoent} ->
			case file:make_dir(Directory) of
				ok ->
					ok;
				{error, Reason} ->
					throw(Reason)
			end;
		{error, Reason} ->
			throw(Reason)
	end of
		ok ->
			case disk_log:open([{name, Log}, {file, FileName},
					{type, wrap}, {size, {1048575, 20}}]) of
				{ok, Log} ->
					{ok, State#state{log = Log}};
				{repaired, Log, {recovered, Rec}, {badbytes, Bad}} ->
					error_logger:warning_report(["Disk log repaired",
							{log, Log}, {path, FileName}, {recovered, Rec},
							{badbytes, Bad}]),
					{ok, State#state{log = Log}};
				{error, Reason1} ->
					{error, Reason1}
			end
	catch
		Reason2 ->
			{error, Reason2}
	end.

-spec request(Address :: inet:ip_address(), Port :: pos_integer(),
		Packet :: binary(), State :: #state{}) ->
	{ok, Response :: binary()} | {error, Reason :: term()}.
%% @doc This callback function is called when a request is received
%% 	on the port.
%%
%% @todo implement disconnect radius accouting if balnce in 0
%% @todo Implement decrement subscriber_balance
request(Address, Port, Packet, #state{} = State)
		when is_tuple(Address), is_integer(Port), is_binary(Packet) ->
	case ocs:find_client(Address) of
		{ok, Secret} ->
			request(Packet, Secret, State);
		error ->
			{error, ignore}
	end.
%% @hidden
request(<<_Code, Id, _Length:16, _/binary>> = Packet, Secret,
		#state{log = Log} = _State) ->
	try
		#radius{code = ?AccountingRequest, id = Id,
				authenticator = Authenticator,
				attributes = BinaryAttributes} = radius:codec(Packet),
		Attributes = radius_attributes:codec(BinaryAttributes),
		NasIpAddressV = radius_attributes:find(?NasIpAddress, Attributes),
		NasIdentifierV = radius_attributes:find(?NasIdentifier, Attributes),
		InOctets = radius_attributes:find(?AcctInputOctets, Attributes),
		OutOctets = radius_attributes:find(?AcctOutputOctets, Attributes),
		{ok, Subscriber} = radius_attributes:find(?UserName, Attributes),
		case {NasIpAddressV, NasIdentifierV} of
			{{error, not_found}, {error, not_found}} ->
				throw(reject);
			{_, _} ->
				ok
		end,
		case {InOctets, OutOctets} of
			{{error, not_found}, {error, not_found}} ->
				Usage = 0;
			{{ok,In}, {ok,Out}} ->
				Usage = In + Out
		end,
		{error, not_found} = radius_attributes:find(?UserPassword, Attributes),
		{error, not_found} = radius_attributes:find(?ChapPassword, Attributes),
		{error, not_found} = radius_attributes:find(?ReplyMessage, Attributes),
		{error, not_found} = radius_attributes:find(?State, Attributes),
		{ok, _AcctSessionId} = radius_attributes:find(?AcctSessionId, Attributes),
		case disk_log:log(Log, Attributes) of
			ok ->
				case ocs:decrement_subscriber_balance(Subscriber, Usage) of
					{ok, 0} ->
						{ok, response(Id, Authenticator, Secret, BinaryAttributes)};
						%io:fwrite("Send Disconnect/Request to NAS");
					{ok, Balance} ->
						{ok, response(Id, Authenticator, Secret, BinaryAttributes)}
				end;
			{error, _Reason} ->
				{error, ignore}
		end
	catch
		_:_ ->
			{error, ignore}
	end.

-spec terminate(Reason :: term(), State :: #state{}) -> ok.
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason, #state{log = Log} = _State) ->
	disk_log:close(Log).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec response(Id :: byte(), RequestAuthenticator :: [byte()],
		Secret :: string() | binary(), Attributes :: binary() | [byte()]) ->
	AccessAccept :: binary().
%% @hidden
response(Id, RequestAuthenticator, Secret, Attributes)
		when is_binary(Attributes) ->
	AttributeList = radius_attributes:codec(Attributes),
	response(Id, RequestAuthenticator, Secret, AttributeList);
response(Id, RequestAuthenticator, Secret, AttributeList)
		when is_list(AttributeList) ->
	AttributeList1 = radius_attributes:store(?MessageAuthenticator,
		<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>, AttributeList),
	Attributes1 = radius_attributes:codec(AttributeList1),
	Length = size(Attributes1) + 20,
	MessageAuthenticator = crypto:hmac(md5, Secret, [<<?AccountingRequest, Id,
			Length:16>>, RequestAuthenticator, Attributes1]),
	AttributeList2 = radius_attributes:store(?MessageAuthenticator,
			MessageAuthenticator, AttributeList1),
	Attributes2 = radius_attributes:codec(AttributeList2),
	ResponseAuthenticator = crypto:hash(md5, [<<?AccountingRequest, Id,
			Length:16>>, RequestAuthenticator, Attributes2, Secret]),
	Response = #radius{code = ?AccountingResponse, id = Id,
			authenticator = ResponseAuthenticator, attributes = Attributes2},
	radius:codec(Response).


