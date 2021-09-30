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
-export([help/0, di/0, di/2, get_avp_info/1]).

-include_lib("diameter/include/diameter.hrl").

%%----------------------------------------------------------------------
%%  The user_default public API
%%----------------------------------------------------------------------

-spec help() -> true.
%% @doc Get help on shell local commands.
help() ->
	shell_default:help(),
	io:format("** ocs commands ** \n"),
	io:format("di()       -- diameter services info\n"),
	io:format("di(acct, Info) -- diameter accounting services info\n"),
	io:format("di(auth, Info) -- diameter authentication and authorization services info\n"),
	true.

-spec di() -> Result
	when
		Result :: term().
%% @doc Get information on running diameter services.
di() ->
	Info = [peer, applications, capabilities,
			transport, connections, statistics],
	F = fun F([H | T], Acc) ->
			F(T, [diameter:service_info(H, Info) | Acc]);
		F([], Acc) ->
			lists:reverse(Acc)
	end,
	case diameter:services() of
		Services when length(Services) > 0 ->
			F(Services, []);
		[] ->
			[]
	end.

-spec di(ServiceType, Info) -> Result
	when
		ServiceType :: auth | acct,
		Info :: [Values],
		Values :: [] | peer | applications | capabilities
				| transport | connections | statistics,
		Result :: term() | {error, Reason},
		Reason :: unknown_service.
%% @doc Get the status of running diameter services.
di(auth, Info) ->
	case diameter:services() of
		[{ocs_diameter_auth_service, _, _} = Service, _] ->
			diameter:service_info(Service, get_params(Info));
		[_, {ocs_diameter_auth_service, _, _} = Service] ->
			diameter:service_info(Service, get_params(Info));
		[{ocs_diameter_auth_service, _, _} = Service] ->
			diameter:service_info(Service, get_params(Info));
		_ ->
			{error, unknown_service}
	end;
di(acct, Info) ->
	case diameter:services() of
		[{ocs_diameter_acct_service, _, _} = Service, _] ->
			diameter:service_info(Service, get_params(Info));
		[_, {ocs_diameter_acct_service, _, _} = Service] ->
			diameter:service_info(Service, get_params(Info));
		[{ocs_diameter_acct_service, _, _} = Service] ->
			diameter:service_info(Service, get_params(Info));
		_ ->
			{error, unknown_service}
	end.

-spec get_avp_info(AVP) -> Result
	when
		AVP :: AVPS :: atom() | origin_host | origin_realm |
				vendor_id | product_name |
				origin_state_id | host_ip_address |
				supported_vendor | auth_application_id |
				inband_security_id | acct_application_id |
				vendor_specific_application_id | firmware_revision,
		Result :: term().
%% @doc Get the status of a selected diameter avp.
get_avp_info(AVP) ->
	case diameter:services() of
		Services when length(Services) > 0 ->
			get_avp_info(avp(AVP), Services, []);
		[] ->
			[]
	end.
%% @hidden
get_avp_info(AVP, [H | T], Acc) ->
	get_avp_info(AVP, T, [diameter:service_info(H, AVP) | Acc]);
get_avp_info(_, [], Acc) ->
	lists:reverse(Acc).

%%----------------------------------------------------------------------
%%  The user_default private API
%%----------------------------------------------------------------------

-spec avp(Value) -> AVP
	when
		Value :: origin_host | origin_realm |
				vendor_id | product_name |
				origin_state_id | host_ip_address |
				supported_vendor | auth_application_id |
				inband_security_id | acct_application_id |
				vendor_specific_application_id | firmware_revision,
		AVP :: 'Origin-Host' | 'Origin-Realm' |
				'Vendor-Id' | 'Product-Name' |
				'Origin-State-Id'| 'Host-IP-Address' |
				'Supported-Vendor' | 'Auth-Application-Id' |
				'Inband-Security-Id' | 'Acct-Application-Id' |
				'Vendor-Specific-Application-Id' | 'Firmware-Revision'.
%% @doc Get correct Diameter AVP format
avp(origin_host) ->
	'Origin-Host';
avp(origin_realm) ->
	'Origin-Realm';
avp(vendor_id) ->
	'Vendor-Id';
avp(product_name) ->
	'Product-Name';
avp(origin_state_id) ->
	'Origin-State-Id';
avp(host_ip_address) ->
	'Host-IP-Address';
avp(supported_vendor) ->
	'Supported-Vendor';
avp(auth_application_id) ->
	'Auth-Application-Id';
avp(inband_security_id) ->
	'Inband-Security-Id';
avp(acc_application_id) ->
	'Acct-Application-Id';
avp(vendor_specific_application_id) ->
	'Vendor-Specific-Application-Id';
avp(firmware_revision) ->
	'Firmware-Revision'.

%% @hidden
get_params([]) ->
	[peer, applications, capabilities, transport,
			connections, statistics];
get_params(Info)
		when Info /= undefined ->
	Info.
