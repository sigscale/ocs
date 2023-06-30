%%% ocs_log.hrl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2023 SigScale Global Inc.
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
-type auth_event() :: diameter_auth_event() | radius_auth_event().

-type diameter_auth_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Client :: ocs_log:server(),
		RequestAttributes :: ocs_log:auth_request(),
		ResponseAttributes :: ocs_log:auth_response()}.

-type radius_auth_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Client :: ocs_log:server(),
		Type :: ocs_log:auth_type(),
		RequestAttributes :: ocs_log:auth_request(),
		ResponseAttributes :: ocs_log:auth_response()}.

-record(rated,
		{bucket_value :: non_neg_integer() | undefined | '_',
		bucket_type :: cents | octets | seconds | messages | undefined | '_',
		currency :: string() | undefined | '_',
		is_billed = false :: boolean() | '_',
		is_tax_exempt :: boolean() | undefined | '_',
		tariff_type :: atom() | undefined | '_',
		product :: term() | undefined | '_',
		price_name :: string() | undefined | '_',
		price_type :: tariff | usage | event | undefined | '_',
		description :: string() | undefined | '_',
		tax_excluded_amount :: non_neg_integer() | undefined | '_',
		tax_included_amount :: non_neg_integer() | undefined | '_',
		tax_rate :: integer() | undefined | '_',
		usage_rating_tag :: usage | included | non_included | undefined | '_'}).

%% Accounting event
-type acct_event() :: {
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Type :: ocs_log:acct_type(),
		RequestAttributes :: ocs_log:acct_request(),
		ResponseAttributes :: ocs_log:acct_response(),
		Rated :: [#rated{}]}.

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
		Timestamp :: ocs_log:timestamp(),
		N :: ocs_log:unique(),
		Protocol :: ocs_log:protocol(),
		Node :: atom(),
		Server :: ocs_log:server(),
		Client :: ocs_log:server(),
		Type :: deduct | reserve | unreserve | transfer | topup | adjustment,
		Subscriber :: binary(),
		Bucket :: undefined | string(),
		Units :: cents | seconds | octets | messages,
		Product :: string(),
		Amount :: integer(),
		AmountBefore :: integer() | undefined,
		AmountAfter :: integer() | undefined,
		Validity :: undefined | pos_integer(),
		Channel :: undefined | string(),
		Requestor :: undefined | [{Id :: string(),
				Role :: string(), Name :: string()}],
		RelatedParty :: undefined | [{Id :: string(),
				Role :: string(), Name :: string()}],
		PaymentMeans :: undefined | string(),
		Action :: undefined | string(),
		Status :: undefined | term()}.

%% IPDR WLAN Document
-record(ipdrDocWLAN,
		{docId :: string() | undefined,
		version = "3.1" :: string(),
		creationTime :: string() | undefined,
		ipdrRecorderInfo :: string() | undefined}).
-record(ipdrDocVoIP,
		{docId :: string() | undefined,
		version = "3.1" :: string(),
		creationTime :: string() | undefined,
		ipdrRecorderInfo :: string() | undefined}).
-record(ipdrDocEnd,
		{count :: non_neg_integer() | undefined,
		endTime :: string() | undefined}).

%% IPDR Public WLAN Access - WISP Use Case
-record(ipdr_wlan,
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
		bucketType :: cents | octets | seconds | messages | undefined,
		bucketValue :: non_neg_integer() | undefined,
		tariffType :: atom() | undefined,
		product :: term() | undefined,
		priceType :: tariff | usage | event | undefined,
		usageRating :: usage | included | non_included | undefined,
		otherParty :: string() | undefined,
		taxPercentage :: 0..100 | undefined,
		taxAmount :: pos_integer() | undefined,
		taxType :: 1..14 | undefined,
		intermediaryName :: string() | undefined,
		serviceName :: 1..6 | undefined,
		relatedIpdrIdList :: string() | undefined,
		tempUserId :: string() | undefined}).

-record(ipdr_voip,
		{ipdrCreationTime :: string() | undefined,
		seqNum :: non_neg_integer() | undefined,
		seizeTime :: string() | undefined,
		startTime :: string() | undefined,
		endTime :: string() | undefined,
		timeZoneOffset :: integer() | undefined,
		callCompletionCode :: integer() | string() | undefined,
		originalDestinationId :: string() | undefined,
		hostName :: string() | undefined,
		subscriberId :: string() | undefined,
		uniqueCallID :: string() | undefined,
		ipAddress :: string() | undefined,
		imisIngress :: integer() | undefined,
		esnIngress :: integer() | undefined,
		callProgressState :: 1..8 | undefined,
		disconnectReason :: integer() | string() | undefined,
		destinationID :: string() | undefined,
		thirdPartyID :: string() | undefined,
		ani :: string() | undefined,
		oLIiiDigit :: string() | undefined,
		pin :: string() | undefined,
		serviceConsumerType :: string() | undefined,
		startAccessTime :: string() | undefined,
		endAccessTime :: string() | undefined,
		callSetupDuration :: integer() | undefined,
		callDuration :: integer() | undefined,
		totalDuration :: integer() | undefined,
		tearDownDuration :: integer() | undefined,
		averagePacketLatency :: integer() | undefined,
		type :: string() | undefined,
		paymentType :: string() | undefined,
		bucketType :: cents | octets | seconds | messages | undefined,
		bucketValue :: non_neg_integer() | undefined,
		tariffType :: atom() | undefined,
		product :: term() | undefined,
		priceType :: tariff | usage | event | undefined,
		usageRating :: usage | included | non_included | undefined,
		chargeAmount :: pos_integer() | undefined,
		feature :: string() | undefined,
		incommingCodec :: string() | undefined,
		outgoingCodec :: string() | undefined,
		silenceCompressionMode :: string() | undefined,
		modem :: string() | undefined,
		supplementaryService :: string() | undefined,
		extendedReasonCode :: string() | undefined,
		disconnectLocation :: string() | undefined,
		proprietaryErrorCode :: integer() | undefined,
		unitsConsumed :: integer() | undefined,
		inboundByteCount :: integer() | undefined,
		outboundByteCount :: integer() |  undefined,
		inboundPacketCount :: integer() | undefined,
		outboundPacketCount :: integer() | undefined,
		inboundLostPacketCount :: integer() | undefined,
		outboundLostPacketCount :: integer() | undefined,
		inboundRxmtPacketCount :: integer() | undefined,
		outboundRxmtPacketCount :: integer() | undefined,
		subscribedQoSClasses ::  1..4 | undefined,
		callClarityIndex :: 1..5 | undefined,
		voiceQualityIndex :: 1..5 | undefined,
		transmissionRatingRFactor :: 1..100 | undefined,
		userPercivedRFactor :: 1..100 | undefined,
		packetLossPercentage :: integer() | undefined,
		outOfSequencePackets :: integer() | undefined,
		correctSequencePackets :: integer() | undefined,
		packetDelayVariation :: integer() | undefined,
		ipAddressIngressDevice :: string() | undefined,
		portNumber :: string() | undefined,
		imsiEgress :: string() | undefined,
		esnEgress :: string() | undefined,
		homeLocationIdIngress :: string() | undefined}).

