%%% ocs.hrl
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

%% define client table entries record
-record(radius_client,
		{address :: inet:ip_address(),
		disconnect_port :: inet:port_number(),
		protocol :: radius | diameter,
		secret :: binary()}).

%% define subscriber table entries record
-record(subscriber,
		{name :: binary(),
		password :: binary(),
		attributes :: radius_attributes:attributes(),
		balance :: integer(),
		enabled = true :: boolean(),
		disconnect  = false :: boolean()}).

