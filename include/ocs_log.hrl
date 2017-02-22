%%% ocs_log.hrl
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

%% IPDR Document
-record(ipdrDoc,
		{docId :: string(),
		version :: string(),
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

