%%% diameter_3gpp.hrl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2020 SigScale Global Inc.
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

%% 3GPP TS 29.273 10. Experimental-Result-Code
-define(DIAMETER_ERROR_USER_UNKNOWN,                  5001).
-define(DIAMETER_ERROR_IDENTITY_NOT_REGISTERED,       5003).
-define(DIAMETER_ERROR_ROAMING_NOT_ALLOWED,           5004).
-define(DIAMETER_ERROR_IDENTITY_ALREADY_REGISTERED,   5005).
-define(DIAMETER_ERROR_USER_NO_NON_3GPP_SUBSCRIPTION, 5450).
-define(DIAMETER_ERROR_USER_NO_APN_SUBSCRIPTION,      5451).
-define(DIAMETER_ERROR_RAT_TYPE_NOT_ALLOWED,          5452).
-define(DIAMETER_ERROR_LATE_OVERLAPPING_REQUEST,      5453).
-define(DIAMETER_ERROR_TIMED_OUT_REQUEST,             5454).
-define(DIAMETER_ERROR_ILLEGAL_EQUIPMENT,             5554).

