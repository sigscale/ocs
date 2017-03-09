%%% ocs_radius_authentication.erl
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
-module(ocs_radius_authentication).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(radius).

%% export the radius behaviour callbacks
-export([init/2, request/4, terminate/2]).

-include_lib("radius/include/radius.hrl").

-record(state,
		{port_server :: atom() | pid()}).

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

-spec init(Address :: inet:ip_address(), Port :: pos_integer()) ->
	{ok, State :: #state{}} | {error, Reason :: term()}.
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
init(Address, Port) when is_tuple(Address), is_integer(Port) ->
	case global:whereis_name({ocs_radius_auth, Address, Port}) of
		PortServer when is_pid(PortServer) ->
			{ok, #state{port_server = PortServer}};
		undefined ->
			{error, radius_auth_port_server_not_found}
	end.

-spec request(Address :: inet:ip_address(), Port :: pos_integer(),
		Packet :: binary(), State :: #state{}) ->
	{ok, Response :: binary()} | {error, Reason :: ignore | term()}.
%% @doc This function is called when a request is received on the port.
%%
request(Address, Port, Packet, #state{port_server = Server} = _State)
		when is_tuple(Address) ->
	try
		{ok, _, _, SharedSecret} = ocs:find_client(Address),
		Radius = radius:codec(Packet),
		#radius{code = ?AccessRequest, attributes = AttributeData} = Radius,
		Attributes = radius_attributes:codec(AttributeData),
		Eap = case radius_attributes:find(?EAPMessage, Attributes) of
			{ok, EapMessage} ->
				MsgAuth = radius_attributes:fetch(?MessageAuthenticator,
						Attributes),
				Attr1 = radius_attributes:store(?MessageAuthenticator,
						<<0:128>>, Attributes),
				AttrBin = radius_attributes:codec(Attr1),
				Packet1 = radius:codec(Radius#radius{attributes = AttrBin}),
				MsgAuth = crypto:hmac(md5, SharedSecret, Packet1),
				{eap, EapMessage};
			{error, not_found} ->
				none
		end,
		{SharedSecret, Radius#radius{attributes = Attributes}, Eap}
	of
		{Secret, AccessRequest, IsEap} ->
			gen_server:call(Server,
					{request, Address, Port, Secret, AccessRequest, IsEap})
	catch
		_:_R ->
			{error, ignore}
	end.

-spec terminate(Reason :: term(), State :: #state{}) -> ok.
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason, _State) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

