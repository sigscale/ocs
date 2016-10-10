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
		log :: disk_log:log(),
		acct_server :: atom() | pid()}).

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

-spec init(Address :: inet:ip_address(), Port :: pos_integer()) ->
	Result :: {ok, State :: #state{}} | {error, Reason :: term()}.
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(Address, Port) ->
	case global:whereis_name({ocs_acct, Address, Port}) of
		AcctServer when is_pid(AcctServer) ->
			{ok, #state{acct_server = AcctServer}};
		undefined ->
			{error, acct_server_not_found}
	end.

-spec request(Address :: inet:ip_address(), Port :: pos_integer(),
		Packet :: binary(), State :: #state{}) ->
	{ok, Response :: binary()} | {error, Reason :: ignore | term()}.
%% @doc This function is called when a request is received on the port.
%%
request(Address, Port, Packet, #state{acct_server = Server} = _State)
		when is_tuple(Address) ->
	try
		{ok, SharedSecret} = ocs:find_client(Address),
		Radius = radius:codec(Packet),
		#radius{code = ?AccessRequest, attributes = AttributeData} = Radius,
		Attributes = radius_attributes:codec(AttributeData),
		{SharedSecret, Radius#radius{attributes = Attributes}}
	of
		{Secret, AccessRequest} ->
			gen_server:call(Server,
					{request, Address, Port, Secret, AccessRequest})
	catch
		_:_ ->
			{error, ignore}
	end.

-spec terminate(Reason :: term(), State :: #state{}) -> ok.
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason, #state{log = Log} = _State) ->
	disk_log:close(Log).

