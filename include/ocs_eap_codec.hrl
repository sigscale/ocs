%%% ocs_eap_codec.hrl
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
%%% 

%% Macro definitions for EAP Codes
-define(Request,           1).
-define(Response,          2).
-define(Success,           3).
-define(Failure,           4).
-define(Initiate,          5).
-define(Finish,            6).

%% Macro definitions for EAP data types
-define(Identity,          1).
-define(Notification,      2).
-define(LegacyNak,         3).
-define(MD5Challenge,      4).
-define(OneTimePassword,   5).
-define(GenericTokenCard,  6).
-define(TLS,              13).
-define(Cisco,            17).
-define(SIM,              18).
-define(TTLS,             21).
-define(AKA,              23).
-define(PEAP,             25).
-define(MSEAP,            26).
-define(MSCHAPv2,         29).
-define(ProtectedOTP,     32).
-define(HTTPDigest,       38).
-define(FAST,             43).
-define(PSK,              47).
-define(SAKE,             48).
-define(IKEv2,            49).
-define(AKAbis,           50).
-define(GPSK,             51).
-define(PWD,              52).
-define(EKEv1,            53).
-define(PTEAP,            54).
-define(TEAP,             55).
-define(Experimental,    255).

-record(eap_packet,
			{code :: 1..255,
			identifier :: 1..255,
			data :: binary()}).

-record(eap_pwd,
			{type :: integer(),
			length :: boolean(),
			more :: boolean(),
			pwd_exch :: byte(),
			tot_length :: integer(),
			data :: binary()}).

-record(eap_pwd_id,
			{group_desc :: integer(),
			random_fun :: integer(),
			prf :: integer(),
			token :: string(),
			pwd_prep :: integer(),
			identity :: string()}).
