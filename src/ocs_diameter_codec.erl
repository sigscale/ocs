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

-spec 'APN-Configuration'(decode, Type, Data, Opts) -> Result
	when
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_APN-Configuration'{}, list()}.
%% @doc Specialized CODEC for 3GPP APN-Configuration.
'APN-Configuration'(decode, _Type, Data, Opts) ->
erlang:display({?MODULE, ?LINE, 'APN-Configuration', decode, _Type, Data, Opts}),
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(decode, 'APN-Configuration', Data, Opts#{app_dictionary := Mod, module := Mod}).

-spec 'EPS-Subscribed-QoS-Profile'(decode, Type, Data, Opts) -> Result
	when
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_EPS-Subscribed-QoS-Profile'{}, list()}.
%% @doc Specialized CODEC for 3GPP EPS-Subscribed-QoS-Profile.
'EPS-Subscribed-QoS-Profile'(decode, _Type, Data, Opts) ->
erlang:display({?MODULE, ?LINE, 'EPS-Subscribed-QoS-Profile', decode, _Type, Data, Opts}),
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(decode, 'EPS-Subscribed-QoS-Profile', Data, Opts#{app_dictionary := Mod, module := Mod}).

-spec 'AMBR'(decode, Type, Data, Opts) -> Result
	when
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_AMBR'{}, list()}.
%% @doc Specialized CODEC for 3GPP AMBR.
'AMBR'(decode, _Type, Data, Opts) ->
erlang:display({?MODULE, ?LINE, 'AMBR', decode, _Type, Data, Opts}),
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(decode, 'AMBR', Data, Opts#{app_dictionary := Mod, module := Mod}).

-spec 'Specific-APN-Info'(decode, Type, Data, Opts) -> Result
	when
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_Specific-APN-Info'{}, list()}.
%% @doc Specialized CODEC for 3GPP Specific-APN-Info.
'Specific-APN-Info'(decode, _Type, Data, Opts) ->
erlang:display({?MODULE, ?LINE, 'Specific-APN-Info', decode, _Type, Data, Opts}),
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(decode, 'Specific-APN-Info', Data, Opts#{app_dictionary := Mod, module := Mod}).

-spec 'WLAN-offloadability'(decode, Type, Data, Opts) -> Result
	when
		Type :: 'Grouped',
		Data :: binary(),
		Opts :: map(),
		Result :: {#'3gpp_swm_WLAN-offloadability'{}, list()}.
%% @doc Specialized CODEC for 3GPP WLAN-offloadability.
'WLAN-offloadability'(decode, _Type, Data, Opts) ->
erlang:display({?MODULE, ?LINE, 'WLAN-offloadability', decode, _Type, Data, Opts}),
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(decode, 'WLAN-offloadability', Data, Opts#{app_dictionary := Mod, module := Mod}).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

