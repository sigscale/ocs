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
		{address :: inet:ip_address() | undefined | '_',
		identifier = <<>> :: binary() | '_',
		port :: inet:port_number() | undefined | '_',
		protocol :: radius | diameter | undefined | '_',
		secret :: binary() | undefined | '_',
		password_required  = true :: boolean() | '_',
		trusted = true :: boolean(),
		last_modified :: tuple() | undefined | '_'}).

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
		{name :: string() | undefined | '_' | '$1' | '$3',
		description :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		status :: offer_status() | '_' | undefined | '_',
		specification :: string() | undefined | '_',
		bundle = [] :: [#bundled_po{}] | '_',
		price = [] :: [#price{}] | '_',
		char_value_use = [] :: [#char_value_use{}] | '_',
		last_modified :: tuple() | undefined | '_'}).

-record(bucket,
		{id :: string() | undefined | '_' | '$1',
		name :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		status :: bucket_status() | undefined | '_',
		remain_amount = 0 :: integer() | '_',
		reservations = [] :: [{TS :: pos_integer(),
				DebitAmount :: non_neg_integer(),
				ReservedAmount :: non_neg_integer(),
				SessionId :: string() | binary()}] | '_',
		units :: octets | cents | seconds | messages | undefined | '_',
		prices = [] :: list() | '_',
		product  = [] :: [ProdRef :: term()] | '_',
		last_modified :: tuple() | undefined | '_'}).

-record(adjustment,
		{id :: string() | undefined | '_' | '$1',
		type :: string() | undefined | '_',
		description :: string() | undefined | '_',
		reason :: string() | undefined | '_',
		amount :: integer() |undefined | '_',
		units :: octets | cents | seconds | messages | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		product :: ProdRef :: term() | '_',
		bucket :: BucketRef :: term() | '_'}).

-record(product,
		{id :: string() | undefined | '_' | '$1',
		name :: string() | undefined | '_' | '$2',
		start_date :: pos_integer() | undefined | '_' | '$4',
		end_date :: pos_integer() | undefined | '_' | '$5',
		status :: product_status() | undefined | '_',
		product :: string() | undefined | '_' | '$3',
		characteristics = [] :: [{Name :: string(), Value :: term()}] | '_',
		payment = [] :: [{Price :: string(), DueDate :: pos_integer()}] | '_',
		balance = [] :: [BucketRef :: term()] | '_',
		service = [] :: [ServiceRef :: binary()] | '_',
		last_modified :: tuple() | undefined | '_'}).

-record(service,
		{name :: binary() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		state :: service_status() | undefined | '_',
		password :: binary() | undefined | '_',
		attributes :: [tuple()] | undefined | '_',
		product :: ProductRef :: string() | undefined | '_',
		enabled = true :: boolean() | '_',
		disconnect  = false :: boolean() | '_',
		session_attributes = [] :: [{TS :: pos_integer(), Attributes :: [tuple()]}] | '_',
		characteristics = [] :: [{Name :: string(), Value :: term()}] | '_',
		multisession = false :: boolean() | '_',
		last_modified :: tuple() | undefined | '_'}).

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

