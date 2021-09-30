%%% user_default.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
%%%

-module(user_default).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% export the user_default public API
-export([help/0, di/0, di/1, di/2, dc/0]).

-include_lib("diameter/include/diameter.hrl").

%%----------------------------------------------------------------------
%%  The user_default public API
%%----------------------------------------------------------------------

-spec help() -> true.
%% @doc Get help on shell local commands.
help() ->
	shell_default:help(),
	io:format("** ocs commands ** \n"),
	io:format("di()            -- diameter services info\n"),
	io:format("di(Types)       -- diameter services info of types\n"),
	io:format("di(acct, Types) -- diameter accounting services info\n"),
	io:format("di(auth, Types) -- diameter authentication and authorization services info\n"),
	io:format("dc()            -- diameter capabilities values\n"),
	true.

-spec di() -> Result
	when
		Result :: [ServiceResult],
		ServiceResult :: {Service, [term()]},
		Service :: term().
%% @doc Get information on running diameter services.
di() ->
	diameter_service_info(diameter:services(), []).

-spec di(Info) -> Result
	when
		Info :: [Item],
		Item :: peer | applications | capabilities | transport
				| connections | statistics | Capabilities,
		Capabilities :: 'Origin-Host' | 'Origin-Realm' | 'Vendor-Id'
				| 'Product-Name' | 'Origin-State-Id' | 'Host-IP-Address'
				| 'Supported-Vendor' | 'Auth-Application-Id'
				| 'Inband-Security-Id' | 'Acct-Application-Id'
				| 'Vendor-Specific-Application-Id' | 'Firmware-Revision',
		Result :: [ServiceResult],
		ServiceResult :: {Service, [term()]},
		Service :: term().
%% @doc Get information on running diameter services.
di(Info) ->
	diameter_service_info(diameter:services(), Info).

-spec di(ServiceType, Info) -> Result
	when
		ServiceType :: auth | acct,
		Info :: [Item],
		Item :: peer | applications | capabilities | transport
				| connections | statistics | Capabilities,
		Capabilities :: 'Origin-Host' | 'Origin-Realm' | 'Vendor-Id'
				| 'Product-Name' | 'Origin-State-Id' | 'Host-IP-Address'
				| 'Supported-Vendor' | 'Auth-Application-Id'
				| 'Inband-Security-Id' | 'Acct-Application-Id'
				| 'Vendor-Specific-Application-Id' | 'Firmware-Revision',
		Result :: term() | {error, Reason},
		Reason :: unknown_service.
%% @doc Get information on running diameter services.
di(auth, Info) ->
	F = fun({ocs_diameter_auth_service, _, _}) ->
				true;
			(_) ->
				false
	end,
	AuthServices = lists:filter(F, diameter:services()),
	diameter_service_info(AuthServices, Info);
di(acct, Info) ->
	F = fun({ocs_diameter_acct_service, _, _}) ->
				true;
			(_) ->
				false
	end,
	AcctServices = lists:filter(F, diameter:services()),
	diameter_service_info(AcctServices, Info).

-spec dc() -> Result
	when
		Result :: term().
%% @doc Get diameter capability values.
dc() ->
	Info = ['Origin-Host', 'Origin-Realm', 'Vendor-Id',
			'Product-Name', 'Origin-State-Id', 'Host-IP-Address',
			'Supported-Vendor', 'Auth-Application-Id',
			'Inband-Security-Id', 'Acct-Application-Id',
			'Vendor-Specific-Application-Id',
			'Firmware-Revision'],
	diameter_service_info(diameter:services(), Info).

%%----------------------------------------------------------------------
%%  The user_default private API
%%----------------------------------------------------------------------

-spec diameter_service_info(Services, Info) -> Result
	when
		Services :: [term()],
		Info :: [Item],
		Item :: peer | applications | capabilities
				| transport | connections | statistics,
		Result :: [ServiceResult],
		ServiceResult :: {Service, [term()]},
		Service :: term().
%% @hidden
diameter_service_info(Services, []) ->
	Info = [peer, applications, capabilities,
			transport, connections, statistics],
	diameter_service_info(Services, Info, []);
diameter_service_info(Services, Info) ->
	diameter_service_info(Services, Info, []).
%% @hidden
diameter_service_info([Service | T], Info, Acc) ->
	diameter_service_info(T, Info,
			[{Service, diameter:service_info(Service, Info)} | Acc]);
diameter_service_info([], _Info, Acc) ->
	lists:reverse(Acc).

