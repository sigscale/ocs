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
-export([init/2, request/3, terminate/1]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_eap_codec.hrl").

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

-spec init(Address :: inet:ip_address(), Port :: pos_integer()) ->
	Result :: ok | {error, Reason :: term()}.
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(_Address, _Port) ->
	ok.

-spec request(Address :: inet:ip_address(), Port :: pos_integer(),
		Packet :: binary()) ->
	Result :: binary() | {error, Reason :: ignore | term()}.
%% @doc This function is called when a request is received on the port.
%%
request(Address, _Port, Packet) ->
	case ocs:find_client(Address) of
		{ok, Secret} ->
			request(Packet, Secret);
		error ->
			{error, ignore}
	end.
%% @hidden
request(Packet, Secret) ->
	try 
		#radius{code = ?AccessRequest, id = Id,
				authenticator = Authenticator,
				attributes = BinaryAttributes} = radius:codec(Packet),
		ReqAttrs = radius_attributes:codec(BinaryAttributes),
		NasIdV = radius_attributes:find(?NasIdentifier, ReqAttrs),
		NasIpV = radius_attributes:find(?NasIpAddress, ReqAttrs),
		case {NasIpV, NasIdV} of
			{error, error} ->
				throw(reject);
			_Other ->
				ok
		end,
		NasPortIdV = radius_attributes:find(?NasPort, ReqAttrs),
		SupplicantIdV = radius_attributes:find(?UserName, ReqAttrs),
		CallingId = radius_attributes:find(?CallingStationId, ReqAttrs),
		CalledId = radius_attributes:find(?CalledStationId, ReqAttrs),
		AccSessId = radius_attributes:find(?AcctSessionId, ReqAttrs),
		AccMultiSessId = radius_attributes:find(?AcctMultiSessionId, ReqAttrs),
		StateV = radius_attributes:find(?State, ReqAttrs),
		EAPMsg = radius_attributes:find(?EAPMessage, ReqAttrs),
		case catch ocs_codec_eap:packet(EAPMsg) of
			#eap_packet{} = EAP ->
				% @todo implement EAP procedures
				throw(reject);
			{'EXIT', Reason} ->
				throw(reject)
		end
	catch
		exit:Reason2 ->
			{error, Reason2};
		throw:reject ->
			reject(Packet, Secret)
	end.

-spec terminate(Reason :: term()) -> ok.
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec reject(Request :: binary(), Secret :: string()) ->
	AccessReject :: binary().
%% @hidden
reject(<<_Code, Id, _Len:16, Authenticator:16/binary, _/binary>>, Secret) ->
	Attributes = [],
	Length = length(Attributes) + 20,
	ResponseAuthenticator = erlang:md5([<<?AccessReject, Id, Length:16>>,
			Authenticator, Secret]), 
	Response = #radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	radius:codec(Response).

-spec accept(Id :: byte(), RequestAuthenticator :: [byte()],
		Secret :: string(), Attributes :: binary() | [byte()]) ->
	AccessAccept :: binary().
%% @hidden
accept(Id, RequestAuthenticator, Secret, AttributeList)
		when is_list(AttributeList) -> 
	Attributes = radius_attributes:codec(AttributeList),
	accept(Id, RequestAuthenticator, Secret, Attributes);
accept(Id, RequestAuthenticator, Secret, ResponseAttributes) 
		when is_binary(ResponseAttributes) -> 
	Length = size(ResponseAttributes) + 20,
	ResponseAuthenticator = erlang:md5([<<?AccessAccept, Id, Length:16>>,
			RequestAuthenticator, ResponseAttributes, Secret]), 
	Response = #radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = ResponseAttributes},
	radius:codec(Response).

