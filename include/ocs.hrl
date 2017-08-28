%%% ocs.hrl
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

%% define client table entries record
-record(client,
		{address :: inet:ip_address(),
		identifier = <<>> :: binary(),
		port :: inet:port_number(),
		protocol :: radius | diameter,
		secret :: binary(),
		last_modified  = {erlang:system_time(milli_seconds),
				erlang:unique_integer([positive])} :: tuple()}).

-record(remain_amount,
	{unit :: string(),
	amount :: integer()}).

-record(bucket,
		{id :: string(),
		name :: string(),
		buckt_type :: string(),
		remain_amount :: #remain_amount{}}).

%% define subscriber table entries record
-record(subscriber,
		{name :: binary(),
		password :: binary(),
		attributes :: radius_attributes:attributes(),
		buckets :: [#bucket{}],
		enabled = true :: boolean(),
		disconnect  = false :: boolean(),
		session_attributes = radius_attributes:new() :: [radius_attributes:attributes()],
		multisession = false :: boolean(),
		last_modified  = {erlang:system_time(milli_seconds),
				erlang:unique_integer([positive])} :: tuple()}).

-type product_status() :: created | aborted | cancelled
								| active | pending_active | suspended
								| terminate | pending_terminate.

%% define currency codes
-type currency() :: mx | us | lkr.

-record(product,
		{name :: string(),
		description :: string(),
		price_type :: recurring | one_time | usage,
		is_bundle = false :: boolean(),
		units :: string(),
		status :: product_status(),
		start_date :: pos_integer(), % ISO8601
		termination_date :: pos_integer(), % ISO8601
		price :: integer()}).
