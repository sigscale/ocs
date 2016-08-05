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

%% @headerfile "../../include/radius.hrl"
-include_lib("radius/include/radius.hrl").

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
		RequestAttributes = radius_attributes:codec(BinaryAttributes),
		UserNameV = radius_attributes:find(?UserName, RequestAttributes),
		NasIpAddressV = radius_attributes:find(?NasIpAddress, RequestAttributes),
		NasIdentifierV = radius_attributes:find(?NasIdentifier,
				RequestAttributes),
		Name = case {UserNameV, NasIpAddressV, NasIdentifierV} of
			{_, error, error} ->
				throw(reject);
			{error, {ok, NasIpAddress}, error} ->
				NasIpAddress;
			{error, error, {ok, NasIdentifier}} ->
				NasIdentifier;
			{{ok, UserName}, _, _} ->
				UserName
		end,
		UserPasswordV = radius_attributes:find(?UserPassword, RequestAttributes),
		ChapPasswordV = radius_attributes:find(?ChapPassword, RequestAttributes),
		StateV = radius_attributes:find(?State, RequestAttributes),
		case {UserPasswordV, ChapPasswordV, StateV} of
			{error, error, error} ->
				throw(reject);
			{error, error, {ok, _State}} ->
				% @todo Handle State?
				throw(not_implemented);
			{{ok, UserPassword}, error, _State} ->
				Password = radius_attributes:unhide(Secret,
						Authenticator, UserPassword),
				case ocs:find_user(Name) of
					{ok, Password, UserAttributes} ->
						accept(Id, Authenticator, Secret, UserAttributes);
					{ok, _Password, _UserAttributes} ->
						throw(reject);
					error ->
						throw(reject)
				end;
			{error, {ok, {ChapId, ChapResponse}}, _State} ->
				Challenge = case radius_attributes:find(?ChapChallenge,
						RequestAttributes) of
					{ok, ChapChallenge} ->
						ChapChallenge;
					error ->
						Authenticator
				end,
				case ocs:find_user(Name) of
					{ok, Password, UserAttributes} ->
						case binary_to_list(erlang:md5([ChapId, Password,
								Challenge])) of
							ChapResponse ->
								accept(Id, Authenticator, Secret, UserAttributes);
							_ ->
								reject(Packet, Secret)
						end;
					error ->
						throw(reject)
				end;
			{{ok, _UserPassword}, {ok, _ChapPassword}, _State} ->
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

