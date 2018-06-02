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

-type offer_status() :: in_study | in_design | in_test
		| active | rejected | launched | retired | obsolete.
-type bucket_status() :: active | expired | suspended.
-type product_status() :: created | pending_active | aborted
		| cancelled | active | suspended | pending_terminate | terminated.
-type service_status() :: feasibilityChecked | designed | reserved
		| active | inactive | terminated.
-type pla_status() :: created | active | cancelled | terminated. 
-type product_price_type() :: recurring | one_time | usage | tariff.
-type unit_of_measure() :: octets | cents | seconds | messages.
-type recur_period() :: hourly | daily | weekly | monthly | yearly.

-record(quantity,
		{amount :: integer(),
		units :: atom() | string()}).
-type quantity() :: #quantity{}.

-record(range,
		{lower :: quantity(),
		upper :: quantity()}).
-type range() :: #range{}.

-record(rate,
		{numerator :: quantity(),
		denominator :: quantity()}).
-type rate() :: #rate{}.

-record(client,
		{address :: inet:ip_address() | undefined,
		identifier = <<>> :: binary(),
		port :: inet:port_number() | undefined,
		protocol :: radius | diameter | undefined,
		secret :: binary() | undefined,
		password_required  = true :: boolean(),
		last_modified :: tuple() | undefined}).

-record(alteration,
		{name :: string() | undefined,
		description :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		type :: product_price_type() | undefined,
		period :: recur_period() | undefined,
		units :: unit_of_measure() | undefined,
		size :: integer() | undefined,
		amount :: integer() | undefined,
		currency :: string() | undefined}).

-record(char_value,
		{default :: boolean() | undefined,
		units :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		value :: quantity() | range() | rate() | term() | undefined,
		from :: term() | undefined,
		to :: term() | undefined,
		type :: string() | undefined, % Number | String | Boolean | DateTime | ...
		interval :: open | closed | closed_bottom | closed_top | undefined,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined}).

-record(char_value_use,
		{name :: string() | undefined,
		description :: string() | undefined,
		type :: string() | undefined, % Number | String | Boolean | DateTime | ...
		min :: non_neg_integer() | undefined,
		max :: pos_integer() | undefined,
		specification :: '_' | string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		values = [] :: [#char_value{}]}).

-record(price,
		{name :: string() | undefined,
		description :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		type :: product_price_type() | undefined,
		period :: recur_period() | undefined,
		units :: unit_of_measure() | undefined,
		size :: integer() | undefined,
		amount :: integer() | undefined,
		currency :: string() | undefined,
		char_value_use = [] :: [#char_value_use{}],
		alteration :: #alteration{} | undefined}).

-record(bundled_po,
		{name :: string() | undefined,
		status :: offer_status() | undefined,
		lower_limit :: non_neg_integer() | undefined,
		upper_limit :: non_neg_integer() | undefined,
		default :: non_neg_integer() | undefined}).

-record(offer,
		{name :: '_' | string() | undefined,
		description :: '_' | string() | undefined,
		start_date :: '_' | pos_integer() | undefined,
		end_date :: '_' | pos_integer() | undefined,
		status :: offer_status() | '_' | undefined,
		specification :: '_' | string() | undefined,
		bundle = [] :: '_' | [#bundled_po{}],
		price = [] :: '_' | [#price{}],
		char_value_use = [] :: '_' | [#char_value_use{}],
		last_modified :: tuple() | '_' | undefined}).

-record(bucket,
		{id :: string() | undefined,
		name :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		status :: bucket_status() | undefined,
		remain_amount = 0 :: integer(),
		reservations = [] :: [{TS :: pos_integer(),
				DebitAmount :: non_neg_integer(),
				ReservedAmount :: non_neg_integer(),
				SessionId :: string() | binary()}],
		units :: octets | cents | seconds | messages | undefined,
		prices = [] :: list(), 
		product :: [ProdRef :: term()],
		last_modified :: tuple() | undefined}).

-record(product,
		{id :: string() | undefined,
		name :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		status :: product_status() | undefined,
		product :: string() | undefined,
		characteristics = [] :: [{Name :: string(), Value :: term()}],
		payment = [] :: [{Price :: string(), DueDate :: pos_integer()}],
		balance = [] :: [BucketRef :: term()],
		service = [] :: [ServiceRef :: binary()],
		last_modified :: tuple() | undefined}).

-record(service,
		{name :: binary() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		state :: service_status() | undefined,
		password :: binary() | undefined,
		attributes :: [tuple()] | undefined,
		product :: ProductRef :: string() | undefined,
		enabled = true :: boolean(),
		disconnect  = false :: boolean(),
		session_attributes = [] :: [{TS :: pos_integer(), Attributes :: [tuple()]}],
		characteristics = [] :: [{Name :: string(), Value :: term()}],
		multisession = false :: boolean(),
		last_modified :: tuple() | undefined}).

-record(pla,
		{name :: string() | undefined,
		description :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		status :: pla_status() | undefined,
		specification :: '_' | string() | undefined,
		characteristics = [] :: [{Name :: string(), Value :: term()}],
		last_modified :: tuple() | undefined}).

-record(gtt,
		{num :: string(),
		value :: {Description :: string(), Rate :: non_neg_integer(), LastModified :: tuple()} | undefined}).

