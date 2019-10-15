%%% ocs_eap_codec.hrl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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

%% Macro definitions for EAP packet codes
-define(EapRequest,           1).
-define(EapResponse,          2).
-define(EapSuccess,           3).
-define(EapFailure,           4).
-define(EapInitiate,          5).
-define(EapFinish,            6).

%% Macro definitions for PAP packet codes
-define(PapAuthenticateRequest,			1).
-define(PapAuthenticateAck,				2).
-define(PapAuthenticateNak,				3).

%% Macro definitions for EAP packet types
-define(Identity,          1).
-define(Notification,      2).
-define(LegacyNak,         3).
-define(MD5Challenge,      4).
-define(OneTimePassword,   5).
-define(GenericTokenCard,  6).
-define(TLS,              13).
%-define(Cisco,            17).
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
-define(AKAprime,         50).
-define(GPSK,             51).
-define(PWD,              52).
-define(EKEv1,            53).
-define(PTEAP,            54).
-define(TEAP,             55).
-define(ExpandedNak,     254).
-define(Experimental,    255).

%% Macro definitions for Elliptic Curve Cryptography(ECC) calculations
-define(P,  16#ffffffff00000001000000000000000000000000ffffffffffffffffffffffff).
-define(A,  16#ffffffff00000001000000000000000000000000fffffffffffffffffffffffc).
-define(B,  16#5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b).
-define(Gx, 16#6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296).
-define(Gy, 16#4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5).
-define(R,  16#ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551).

%% Macro definitions for EAP-AKA attributes
-define(AT_RAND,                1).
-define(AT_AUTN,                2).
-define(AT_RES,                 3).
-define(AT_AUTS,                4).
-define(AT_PADDING,             6).
-define(AT_NONCE_MT,            7).
-define(AT_PERMANENT_ID_REQ,   10).
-define(AT_MAC,                11).
-define(AT_NOTIFICATION,       12).
-define(AT_ANY_ID_REQ,         13).
-define(AT_IDENTITY,           14).
-define(AT_VERSION_LIST,       15).
-define(AT_SELECTED_VERSION,   16).
-define(AT_FULLAUTH_ID_REQ,    17).
-define(AT_COUNTER,            19).
-define(AT_COUNTER_TOO_SMALL,  20).
-define(AT_NONCE_S,            21).
-define(AT_CLIENT_ERROR_CODE,  22).
-define(AT_KDF_INPUT,          23).
-define(AT_KDF,                24).
-define(AT_IV,                129).
-define(AT_ENCR_DATA,         130).
-define(AT_NEXT_PSEUDONYM,    132).
-define(AT_NEXT_REAUTH_ID,    133).
-define(AT_CHECKCODE,         134).
-define(AT_RESULT_IND,        135).
-define(AT_BIDDING,           136).

-record(eap_packet,
			{code :: request | response | success | failure | undefined,
			type :: byte() | undefined,
			identifier :: byte() | undefined,
			data :: binary() | undefined}).

-record(eap_pwd,
			{length = false :: boolean() | undefined,
			more = false :: boolean() | undefined,
			pwd_exch :: id | commit | confirm,
			tot_length :: 0..65535 | undefined,
			data :: binary() | undefined}).

-record(eap_pwd_id,
			{group_desc = 19 :: byte() | undefined,
			random_fun = 1 :: byte() | undefined,
			prf = 1 :: byte() | undefined,
			token :: binary() | undefined,
			pwd_prep = none :: none | rfc2759 | saslprep,
			identity :: binary() | undefined}).

-record(eap_pwd_commit,
			{element :: binary() | undefined,
			scalar :: binary() | undefined1}).

-record(eap_ttls,
			{more = false :: boolean() | undefined,
			start = false :: boolean() | undefined,
			version = 0 :: 0..7,
			message_len :: integer() | undefined,
			data = <<>> :: binary() | undefined}).

-record(eap_aka_challenge,
		{rand :: binary() | undefined,
		autn :: binary() | undefined,
		res :: bitstring() | undefined,
		kdf :: [integer()] | undefined,
		network :: binary() | undefined,
		next_pseudonym :: binary() | undefined,
		next_reauth_id :: binary() | undefined,
		iv :: binary() | undefined,
		encr_data :: binary()| undefined,
		checkcode :: binary() | undefined,
		result_ind = false :: boolean() | undefined,
		mac :: binary()  | undefined}).

-record(eap_aka_identity,
		{permanent_id_req = false :: boolean() | undefined,
		any_id_req = false :: boolean()| undefined,
		fullauth_id_req = false :: boolean()| undefined,
		identity :: binary() | undefined}).

-record(eap_aka_notification,
		{iv :: binary() | undefined,
		encr_data :: binary() | undefined,
		mac :: binary() | undefined,
		counter :: 0..65535 | undefined,
		notification :: 0..65535 | undefined}).

-record(eap_aka_reauthentication,
		{next_reauth_id :: binary() | undefined,
		iv :: binary() | undefined,
		encr_data :: binary() | undefined,
		checkcode :: binary() | undefined,
		result_ind = false :: boolean() | undefined,
		mac :: binary() | undefined,
		counter :: 0..65535 | undefined,
		counter_too_small = false :: boolean() | undefined,
		nonce_s :: binary() | undefined}).

-record(eap_aka_authentication_reject,
		{}).

-record(eap_aka_synchronization_failure,
		{auts :: binary() | undefined}).

-record(eap_aka_client_error,
		{client_error_code :: 0..65535 | undefined}).

