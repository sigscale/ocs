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

-type product_offering_status() :: in_study | in_design | in_test
		| active | rejected | launched | retired | obsolete,
-type product_status() :: created | pending_active | aborted
		| cancelled | active | suspended | pending_terminate | terminated.

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
		{name :: string() | undefined,
		description :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		type :: price_type() | undefined,
		period :: valid_period() | undefined,
		units :: unit_of_measure() | undefined,
		size :: integer() | undefined,
		amount :: integer() | undefined,
		currency :: string() | undefined}).

-record(price,
		{name :: string() | undefined,
		description :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		type :: price_type() | undefined,
		period :: valid_period() | undefined,
		units :: unit_of_measure() | undefined,
		size :: integer() | undefined,
		amount :: integer() | undefined,
		currency :: string() | undefined,
		alteration :: #alteration{} | undefined}).

-record(product,
		{name :: '_' | string() | undefined,
		description :: '_' | string() | undefined,
		start_date :: '_' | pos_integer() | undefined,
		end_date :: '_' | pos_integer() | undefined,
		is_bundle = false :: '_' | boolean() | undefined,
		status :: product_offering_status() | '_' | undefined,
		specification :: string(),
		price :: '_' | [#price{}],
		characteristics = [] :: [tuple()],
		last_modified :: tuple() | '_' | undefined}).

-record(bucket,
		{id :: string() | undefined,
		name :: string() | undefined,
		bucket_type :: octets | cents | seconds,
		start_date :: pos_integer() | undefined,
		termination_date :: pos_integer() | undefined,
		remain_amount = 0 :: integer(),
		units :: octets | cents | seconds | undefined,
		last_modified :: tuple() | '_' | undefined}).

-record(product_instance,
		{product :: string(),
		start_date :: pos_integer(),
		termination_date :: pos_integer(),
		status :: product_offering_status(),
		characteristics = [] :: [tuple()],
		last_modified :: tuple() | '_' | undefined}).

%% define subscriber table entries record
-record(subscriber,
		{name :: binary(),
		password :: binary(),
		attributes :: radius_attributes:attributes(),
		buckets :: [#bucket{}],
		product :: #product_instance{},
		enabled = true :: boolean(),
		disconnect  = false :: boolean(),
		session_attributes = radius_attributes:new() :: [radius_attributes:attributes()],
		multisession = false :: boolean(),
		last_modified :: tuple()}).


