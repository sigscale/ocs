%%% ocs_diameter_codec.erl 
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 SigScale Global Inc.
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
%%% @doc This library module performs specialized CODEC functions
%%% 	for DIAMETER in the {@link //ocs. ocs} application.
%%%
-module(ocs_diameter_codec).
-copyright('Copyright (c) 2020 SigScale Global Inc.').

-export(['APN-Configuration'/4, 'EPS-Subscribed-QoS-Profile'/4,
		'AMBR'/4, 'Specific-APN-Info'/4, 'WLAN-offloadability'/4]).

-include("diameter_gen_3gpp_swm_application.hrl").

%%----------------------------------------------------------------------
%%  ocs_diameter_codec public API
%%----------------------------------------------------------------------

-spec 'APN-Configuration'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_APN-Configuration'{}, list()}.
%% @doc Specialized CODEC for 3GPP APN-Configuration.
'APN-Configuration'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data, Opts#{app_dictionary := Mod, module := Mod});
'APN-Configuration'(encode = Operation, 'Grouped' = _Type, Data, #{mode := Mod} = Opts) ->
	Mod:grouped_avp(Operation, 'APN-Configuration', Data, Opts).

-spec 'EPS-Subscribed-QoS-Profile'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_EPS-Subscribed-QoS-Profile'{}, list()}.
%% @doc Specialized CODEC for 3GPP EPS-Subscribed-QoS-Profile.
'EPS-Subscribed-QoS-Profile'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data, Opts#{app_dictionary := Mod, module := Mod});
'EPS-Subscribed-QoS-Profile'(encode = Operation, 'Grouped' = _Type, Data, #{mode := Mod} = Opts) ->
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data, Opts).

-spec 'AMBR'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_AMBR'{}, list()}.
%% @doc Specialized CODEC for 3GPP AMBR.
'AMBR'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'AMBR', Data, Opts#{app_dictionary := Mod, module := Mod});
'AMBR'(encode = Operation, 'Grouped' = _Type, Data, #{mode := Mod} = Opts) ->
	Mod:grouped_avp(Operation, 'AMBR', Data, Opts).

-spec 'Specific-APN-Info'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_Specific-APN-Info'{}, list()}.
%% @doc Specialized CODEC for 3GPP Specific-APN-Info.
'Specific-APN-Info'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data, Opts#{app_dictionary := Mod, module := Mod});
'Specific-APN-Info'(encode = Operation, 'Grouped' = _Type, Data, #{mode := Mod} = Opts) ->
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data, Opts).

-spec 'WLAN-offloadability'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_WLAN-offloadability'{}, list()}.
%% @doc Specialized CODEC for 3GPP WLAN-offloadability.
'WLAN-offloadability'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data, Opts#{app_dictionary := Mod, module := Mod});
'WLAN-offloadability'(encode = Operation, 'Grouped' = _Type, Data, #{mode := Mod} = Opts) ->
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data, Opts).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

