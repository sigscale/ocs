%%% ocs_log.hrl
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

%% Authorization event
-type auth_event() :: {
		Timestamp :: pos_integer(),
		N :: pos_integer(),
		Protocol :: radius | diameter,
		Node :: atom(),
		Server :: {inet:ip_address(), inet:port_number()},
		Client :: {inet:ip_address(), inet:port_number()},
		Type :: atom(),
		RequestAttributes :: radius_attributes:attributes(),
		ResponseAttributes :: radius_attributes:attributes()}.

-record(rated,
		{bucket_value :: non_neg_integer() | undefined,
		bucket_type :: cents | octets | seconds | undefined,
		currency :: string() | undefined,
		is_billed = false :: boolean(),
		is_tax_exempt :: boolean() | undefined,
		tariff_type :: atom() | undefined,
		product :: term() | undefined,
		price_type :: tariff | usage | event | undefined,
		description :: string() | undefined,
		tax_excluded_amount :: non_neg_integer() | undefined,
		tax_included_amount :: non_neg_integer() | undefined,
		tax_rate :: integer() | undefined,
		usage_rating_tag :: usage | included | non_included | undefined}).

%% Accounting event
-type acct_event() :: {
		Timestamp :: pos_integer(),
		N :: pos_integer(),
		Protocol :: radius | diameter,
		Node :: atom(),
		Server :: {inet:ip_address(), inet:port_number()},
		Type :: atom(),
		ReqAttrs :: radius_attributes:attributes(),
		RespAttrs :: radius_attributes:attributes(),
		Rated :: #rated{}}.

%% REST API event
-type http_event() :: {
		Host :: string(),
		User :: string(),
		DateTime :: string(),
		Method :: string(),
		URI :: string(),
		HttpStatus :: string()}.

%% Account Balance event
-type abmf_event() :: {
		Timestamp :: pos_integer(),
		N :: pos_integer(),
		Node :: atom(),
		Type :: deduct | reserve | unreserve | transfer | topup | adjustment,
		Subscriber :: binary(),
		Bucket :: undefined | string(),
		Units :: cents | seconds | octets,
		Product :: string(),
		Amount :: integer(),
		AmountBefore :: integer(),
		AmountAfter :: integer(),
		Validity :: undefined | pos_integer(),
		Channel :: undefined | string(),
		Requestor :: undefined | [{Id :: string(),
				Role :: string(), Name :: string()}],
		RelatedParty :: undefined | [{Id :: string(),
				Role :: string(), Name :: string()}],
		PaymentMeans :: undefined | string(),
		Action :: undefined | string(),
		Status :: undefined | term()}.

%% IPDR Document
-record(ipdrDoc,
		{docId :: string(),
		version = "3.1" :: string(),
		creationTime :: string(),
		ipdrRecorderInfo :: string()}).
-record(ipdrDocEnd,
		{count :: non_neg_integer(),
		endTime :: string()}).

%% IPDR Public WLAN Access - WISP Use Case
-record(ipdr,
		{ipdrCreationTime :: string() | undefined,
		seqNum :: non_neg_integer() | undefined,
		username :: string() | undefined,
		scIdType :: 1..3 | undefined,
		scId :: string() | undefined,
		homeServiceProviderType :: 1..4 | undefined,
		homeServiceProvider :: string() | undefined,
		acctSessionId :: string() | undefined,
		userIpAddress :: string() | undefined,
		callingStationId :: string() | undefined,
		calledStationId :: string() | undefined,
		nasIpAddress :: string() | undefined,
		nasId :: string() | undefined,
		accessProviderType :: 1..4 | undefined,
		accessServiceProvider :: string() | undefined,
		locationName :: string() | undefined,
		locationId :: string() | undefined,
		locationType :: string() | undefined,
		locationCountryCode :: string() | undefined,
		locationStateProvince :: string() | undefined,
		locationCity :: string() | undefined,
		locationGeocode :: string() | undefined,
		locationGeocodeType :: string() | undefined,
		nasPortType :: 0..19 | undefined,
		paymentType :: 1..3 | undefined,
		networkConnectionType :: string() | undefined,
		sessionDuration :: pos_integer() | undefined,
		inputOctets :: pos_integer() | undefined,
		outputOctets :: pos_integer() | undefined,
		class :: string() | undefined, 
		gmtSessionStartDateTime :: string() | undefined,
		gmtSessionEndDateTime :: string() | undefined,
		sessionTerminateCause :: 1..7 | undefined,
		billingClassOfService :: string() | undefined,
		unitOfMeasure :: 1..7 | undefined,
		chargeableUnit :: 1..7 | undefined,
		chargeableQuantity :: pos_integer() | undefined,
		chargeAmount :: pos_integer() | undefined,
		chargeCurrencyType :: string() | undefined,
		otherParty :: string() | undefined,
		taxPercentage :: 0..100 | undefined,
		taxAmount :: pos_integer() | undefined,
		taxType :: 1..14 | undefined,
		intermediaryName :: string() | undefined,
		serviceName :: 1..6 | undefined,
		relatedIpdrIdList :: string() | undefined,
		tempUserId :: string() | undefined}).

