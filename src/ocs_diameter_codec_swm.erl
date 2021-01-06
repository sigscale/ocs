%%% ocs_diameter_codec_swm.erl 
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 - 2021 SigScale Global Inc.
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
%%% @doc This library module performs specialized DIAMETER CODEC functions
%%% 	for the SWm interface in the {@link //ocs. ocs} application.
%%%
-module(ocs_diameter_codec_swm).
-copyright('Copyright (c) 2020 - 2021 SigScale Global Inc.').

-export(['APN-Configuration'/3, 'APN-Configuration'/4,
		'EPS-Subscribed-QoS-Profile'/3, 'EPS-Subscribed-QoS-Profile'/4,
		'AMBR'/3, 'AMBR'/4, 'Specific-APN-Info'/3, 'Specific-APN-Info'/4,
		'WLAN-offloadability'/3, 'WLAN-offloadability'/4,
		'Trace-Info'/3, 'Trace-Info'/4, 'Emergency-Info'/3, 'Emergency-Info'/4,
		'Subscription-Id'/3, 'Subscription-Id'/4]).

-include("diameter_gen_3gpp_swx_application.hrl").
-include("diameter_gen_3gpp_swm_application.hrl").

%%----------------------------------------------------------------------
%%  ocs_diameter_codec public API
%%----------------------------------------------------------------------

-spec 'APN-Configuration'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_APN-Configuration'{}
				| #'3gpp_swm_APN-Configuration'{},
		Result :: {#'3gpp_swm_APN-Configuration'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP APN-Configurationo (legacy API).
'APN-Configuration'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data);
'APN-Configuration'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_APN-Configuration'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data);
'APN-Configuration'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_APN-Configuration'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data).

-spec 'APN-Configuration'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary(),
		Data :: binary() | #'3gpp_swx_APN-Configuration'{}
				| #'3gpp_swm_APN-Configuration'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_APN-Configuration'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP APN-Configuration.
'APN-Configuration'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data, Opts);
'APN-Configuration'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_APN-Configuration'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data, Opts);
'APN-Configuration'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_APN-Configuration'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'APN-Configuration', Data, Opts).

-spec 'EPS-Subscribed-QoS-Profile'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_EPS-Subscribed-QoS-Profile'{}
				| #'3gpp_swm_EPS-Subscribed-QoS-Profile'{},
		Result :: {#'3gpp_swm_EPS-Subscribed-QoS-Profile'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP EPS-Subscribed-QoS-Profile (legacy API).
'EPS-Subscribed-QoS-Profile'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data);
'EPS-Subscribed-QoS-Profile'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_EPS-Subscribed-QoS-Profile'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data);
'EPS-Subscribed-QoS-Profile'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_EPS-Subscribed-QoS-Profile'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data).

-spec 'EPS-Subscribed-QoS-Profile'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_EPS-Subscribed-QoS-Profile'{}
				| #'3gpp_swm_EPS-Subscribed-QoS-Profile'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_EPS-Subscribed-QoS-Profile'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP EPS-Subscribed-QoS-Profile.
'EPS-Subscribed-QoS-Profile'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data, Opts);
'EPS-Subscribed-QoS-Profile'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_EPS-Subscribed-QoS-Profile'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data, Opts);
'EPS-Subscribed-QoS-Profile'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_EPS-Subscribed-QoS-Profile'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'EPS-Subscribed-QoS-Profile', Data, Opts).

-spec 'AMBR'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_AMBR'{} | #'3gpp_swm_AMBR'{},
		Result :: {#'3gpp_swm_AMBR'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP AMBR (legacy API).
'AMBR'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'AMBR', Data);
'AMBR'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_AMBR'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'AMBR', Data);
'AMBR'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_AMBR'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'AMBR', Data).

-spec 'AMBR'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_AMBR'{} | #'3gpp_swm_AMBR'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_AMBR'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP AMBR.
'AMBR'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'AMBR', Data, Opts);
'AMBR'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_AMBR'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'AMBR', Data, Opts);
'AMBR'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_AMBR'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'AMBR', Data, Opts).

-spec 'Specific-APN-Info'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Specific-APN-Info'{}
				| #'3gpp_swm_Specific-APN-Info'{},
		Result :: {#'3gpp_swm_Specific-APN-Info'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP Specific-APN-Info (legacy API).
'Specific-APN-Info'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data);
'Specific-APN-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Specific-APN-Info'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data);
'Specific-APN-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Specific-APN-Info'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data).

-spec 'Specific-APN-Info'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Specific-APN-Info'{}
				| #'3gpp_swm_Specific-APN-Info'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_Specific-APN-Info'{}, list()} | binary().
%% @doc Specialized CODEC for 3GPP Specific-APN-Info.
'Specific-APN-Info'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data, Opts);
'Specific-APN-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Specific-APN-Info'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data, Opts);
'Specific-APN-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Specific-APN-Info'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Specific-APN-Info', Data, Opts).

-spec 'WLAN-offloadability'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_WLAN-offloadability'{}
				| #'3gpp_swm_WLAN-offloadability'{},
		Result :: {#'3gpp_swm_WLAN-offloadability'{}, list()}.
%% @doc Specialized CODEC for 3GPP WLAN-offloadability (legacy API).
'WLAN-offloadability'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data);
'WLAN-offloadability'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_WLAN-offloadability'{}= Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data);
'WLAN-offloadability'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_WLAN-offloadability'{}= Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data).

-spec 'WLAN-offloadability'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_WLAN-offloadability'{}
				| #'3gpp_swm_WLAN-offloadability'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_WLAN-offloadability'{}, list()}.
%% @doc Specialized CODEC for 3GPP WLAN-offloadability.
'WLAN-offloadability'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data, Opts);
'WLAN-offloadability'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_WLAN-offloadability'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data, Opts);
'WLAN-offloadability'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_WLAN-offloadability'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'WLAN-offloadability', Data, Opts).

-spec 'Trace-Info'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Trace-Info'{} | #'3gpp_swm_Trace-Info'{},
		Result :: {#'3gpp_swm_Trace-Info'{}, list()} | binary().
%% @doc Specialized CODEC for Trace-Info (legacy API).
'Trace-Info'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Trace-Info', Data);
'Trace-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Trace-Info'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Trace-Info', Data);
'Trace-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Trace-Info'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Trace-Info', Data).

-spec 'Trace-Info'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Trace-Info'{} | #'3gpp_swm_Trace-Info'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_Trace-Info'{}, list()} | binary().
%% @doc Specialized CODEC for Trace-Info.
'Trace-Info'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Trace-Info', Data, Opts);
'Trace-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Trace-Info'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Trace-Info', Data, Opts);
'Trace-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Trace-Info'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Trace-Info', Data, Opts).

-spec 'Emergency-Info'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Emergency-Info'{} | #'3gpp_swm_Emergency-Info'{},
		Result :: {#'3gpp_swm_Emergency-Info'{}, list()} | binary().
%% @doc Specialized CODEC for Emergency-Info (legacy API).
'Emergency-Info'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Emergency-Info', Data);
'Emergency-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Emergency-Info'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Emergency-Info', Data);
'Emergency-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Emergency-Info'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Emergency-Info', Data).

-spec 'Emergency-Info'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Emergency-Info'{} | #'3gpp_swm_Emergency-Info'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_Emergency-Info'{}, list()} | binary().
%% @doc Specialized CODEC for Emergency-Info.
'Emergency-Info'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Emergency-Info', Data, Opts);
'Emergency-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Emergency-Info'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Emergency-Info', Data, Opts);
'Emergency-Info'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Emergency-Info'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Emergency-Info', Data, Opts).

-spec 'Subscription-Id'(Operation, Type, Data) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Subscription-Id'{} | #'3gpp_swm_Subscription-Id'{},
		Result :: {#'3gpp_swm_Subscription-Id'{}, list()} | binary().
%% @doc Specialized CODEC for Subscription-Id (legacy API).
'Subscription-Id'(decode = Operation, 'Grouped' = _Type, Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Subscription-Id', Data);
'Subscription-Id'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Subscription-Id'{} = Data) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Subscription-Id', Data);
'Subscription-Id'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Subscription-Id'{} = Data) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Subscription-Id', Data).

-spec 'Subscription-Id'(Operation, Type, Data, Opts) -> Result
	when
		Operation :: decode | encode,
		Type :: 'Grouped',
		Data :: binary() | #'3gpp_swx_Subscription-Id'{} | #'3gpp_swm_Subscription-Id'{},
		Opts :: map(),
		Result :: {#'3gpp_swm_Subscription-Id'{}, list()} | binary().
%% @doc Specialized CODEC for Subscription-Id.
'Subscription-Id'(decode = Operation, 'Grouped' = _Type, Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Subscription-Id', Data, Opts);
'Subscription-Id'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swx_Subscription-Id'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swx_application,
	Mod:grouped_avp(Operation, 'Subscription-Id', Data, Opts);
'Subscription-Id'(encode = Operation, 'Grouped' = _Type,
		#'3gpp_swm_Subscription-Id'{} = Data, Opts) ->
	Mod = diameter_gen_3gpp_swm_application,
	Mod:grouped_avp(Operation, 'Subscription-Id', Data, Opts).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

