%%% ocs.hrl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2022 SigScale Global Inc.
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
-type product_price_type() :: recurring | one_time | usage | tariff | pla_ref().
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

-record(hub,
		{callback :: string() | undefined,
		id :: string() | undefined,
		href :: string() | undefined,
		query :: string() | undefined}).
-type hub() :: #hub{}.

-record(client,
		{address :: inet:ip_address() | undefined | '_',
		identifier = <<>> :: binary() | '_',
		port :: inet:port_number() | undefined | '_',
		protocol :: radius | diameter | undefined | '_',
		secret :: binary() | undefined | '_',
		password_required  = true :: boolean() | '_',
		trusted = true :: boolean() | '_',
		last_modified :: tuple() | undefined | '_'}).

-record(alteration,
		{name :: string() | undefined,
		description :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		type :: product_price_type() | undefined,
		period :: recur_period() | undefined,
		units :: cents | octets | seconds | messages | undefined,
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
		units :: cents | octets | seconds | messages | undefined,
		size :: integer() | undefined,
		amount :: integer() | undefined,
		currency :: string() | undefined,
		char_value_use = [] :: [#char_value_use{}],
		alteration :: #alteration{} | undefined}).

-record(pla_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type pla_ref() :: #pla_ref{}.

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

-type reservations() :: #{SesssionID :: list() => reservation()}.
-type reservation() ::
		#{ts := pos_integer(),
		debit := non_neg_integer(),
		reserve := non_neg_integer(),
		service_id => non_neg_integer(),
		charging_key => non_neg_integer()}.
-type bucket_source() ::
		#{id := string(),
		amount := pos_integer(),
		unit_size := pos_integer(),
		unit_price := pos_integer(),
		expire := pos_integer() | undefined}.
-type bucket_attributes() ::
		#{bucket_type := normal | session,
		from_bucket => [bucket_source()],
		reservations => reservations()}.

-record(bucket,
		{id :: string() | undefined | '_' | '$1',
		name :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		status :: bucket_status() | undefined | '_',
		remain_amount = 0 :: integer() | '_',
		attributes :: bucket_attributes() | '_',
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
		service :: ServiceRef :: term() | '_',
		bucket :: BucketRef :: term() | '_'}).

-record(acc_balance,
		{id :: string() | undefined,
		name :: string() | undefined,
		total_balance :: [quantity()] | undefined,
		bucket = [] :: [string()],
		product = [] :: [string()]}).
-type acc_balance() :: #acc_balance{}.

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

-record(aka_cred,
		{k :: binary(),
		opc :: binary(),
		dif = 0 :: integer()}).
-type aka_cred() :: #aka_cred{}.

-record(service,
		{name :: binary() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		state :: service_status() | undefined | '_',
		password :: binary() | aka_cred() | undefined | '_',
		attributes :: [tuple()] | undefined | '_',
		product :: ProductRef :: string() | undefined | '_',
		enabled = true :: boolean() | '_',
		disconnect = false :: boolean() | '_',
		session_attributes = [] :: [{TS :: pos_integer(), Attributes :: [tuple()]}] | '_',
		characteristics = [] :: [{Name :: string(), Value :: term()}] | '_',
		multisession = false :: boolean() | '_',
		last_modified :: tuple() | undefined | '_'}).

-record(resource,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		category :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		state :: string() | undefined | '_',
		substate :: string() | undefined | '_',
		version :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()}
				| undefined | '_',
		related = [] :: [resource_rel()] | '_',
		specification :: specification_ref() | undefined | '_',
		characteristic = [] :: [resource_char()] | '_'}).
-type resource() :: #resource{}.

-record(resource_rel,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		type :: string() | undefined | '_',
		referred_type :: string() | undefined | '_'}).
-type resource_rel() :: #resource_rel{}.

-record(specification_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		version :: string() | undefined | '_'}).
-type specification_ref() :: #specification_ref{}.

-record(resource_char,
		{name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		value :: term() | undefined | '_'}).
-type resource_char() :: #resource_char{}.

-record(gtt,
		{num :: string() | '_' | '$1',
		value :: {Description :: string(), Rate :: non_neg_integer(), LastModified :: tuple()} | undefined | '_' | '$2'}).

-record(session,
		{id :: diameter:'OctetString'() | radius_attributes:attributes(),
		imsi :: binary(),
		identity :: binary() | undefined,
		application :: pos_integer() | undefined,
		nas_host :: string() | undefined,
		nas_realm :: string() | undefined,
		nas_address :: inet:ip_address() | undefined,
		hss_host :: string() | undefined,
		hss_realm :: string() | undefined,
		user_profile :: tuple() | undefined,
		last_modified :: tuple() | undefined}).

-record(nrf_ref,
		{rating_ref :: string(),
		node_functionality :: string() | undefined,
		subscription_id :: string() | undefined,
		last_modified :: pos_integer() | undefined}).

