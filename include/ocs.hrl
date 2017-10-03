%%% ocs.hrl
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

-type product_status() :: created | aborted | cancelled
								| active | pending_active | suspended
								| terminate | pending_terminate.

%% define price types
-type price_type() :: recurring | one_time | usage.

%% define unit of measure
-type unit_of_measure() :: octets| cents | seconds | mb | gb.

%% define validity period of a product
-type valid_period() :: daily | weekly | monthly | yearly.

-type valid_for() :: {SDT :: integer() | undefined, EDT :: integer() | undefined}.


%% define client table entries record
-record(client,
		{address :: inet:ip_address(),
		identifier = <<>> :: binary(),
		port :: inet:port_number(),
		protocol :: radius | diameter,
		secret :: binary(),
		last_modified :: tuple()}).

-record(alteration,
		{name :: string(),
		description :: string(),
		valid_for	:: valid_for(),
		type :: price_type(),
		units :: unit_of_measure(),
		size :: integer(),
		amount :: integer()}).

-record(price,
		{name :: string(),
		description :: string(),
		valid_for	:: valid_for(),
		type :: price_type(),
		units :: unit_of_measure(),
		currency :: string(),
		period :: valid_period(),
		size :: integer(),
		amount :: integer(),
		alteration :: #alteration{},
		validity :: integer()}).

-record(product,
		{name :: '_' | string(),
		description :: '_' | string(),
		valid_for	:: '_' | valid_for(),
		is_bundle = false :: '_' | undefined | boolean(),
		status :: '_' | undefined | product_status(),
		start_date :: '_' | pos_integer(), % ISO8601
		termination_date :: '_' | pos_integer(), % ISO8601
		price :: '_' | [#price{}],
		last_modified :: tuple() | '_'}).

-record(remain_amount,
		{unit :: string(),
		amount :: integer()}).

-record(bucket,
		{id :: string(),
		name :: string(),
		bucket_type :: octets | cents | seconds,
		start_date :: pos_integer(),
		termination_date :: pos_integer(),
		remain_amount :: #remain_amount{}}).

-record(product_instance,
		{product :: string(),
		start_date :: pos_integer(),
		termination_date :: pos_integer(),
		status :: atom(),
		product_characteristics :: list()}).

%% define subscriber table entries record
-record(subscriber,
		{name :: binary(),
		password :: binary(),
		attributes :: radius_attributes:attributes(),
		buckets :: [#bucket{}],
		product :: string() | #product_instance{},
		enabled = true :: boolean(),
		disconnect  = false :: boolean(),
		session_attributes = radius_attributes:new() :: [radius_attributes:attributes()],
		multisession = false :: boolean(),
		last_modified :: tuple()}).


