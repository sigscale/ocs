%%% ocs_eap_ttls_transport.erl
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
-module(ocs_eap_ttls_transport).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-behaviour(radius).

%% export inet behaviour callbacks
-export([]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").

%%----------------------------------------------------------------------
%%  inet callbacks
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

