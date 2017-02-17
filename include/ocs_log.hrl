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
-record(ipdr_doc,
		docId :: string(),
		version :: string(),
		creationTime :: erlang:system_time(),
		}).

%% IPDR Public WLAN Access - WISP Use Case
-record(ipdr_wlan,
		{username :: string(),
		scIdType :: 1..3,
		scId :: string(),
		homeServiceProviderType :: 1..4,
		homeServiceProvider :: string(),
		acctSessionId :: string(),
		userIpAddress :: string(),
		callingStationId :: string(),
		nasIpAddress :: string(),
		calledStationId :: string(),
		nasId :: string(),
		accessProviderType :: 1..4,
		accessServiceProvider :: string(),
		locationName :: string(),
		locationId :: string(),
		locationType :: string(),
		locationCountryCode :: string(),
		locationStateProvince :: string(),
		locationCity :: string(),
		locationGeocode :: string(),
		locationGeocodeType :: string(),
		nasPortType :: 0..19,
		paymentType :: 1..3,
		networkConnectionType :: string(),
		sessionDuration :: pos_integer(),
		inputOctets :: pos_integer(),
		outputOctets :: pos_integer(),
		class :: string(), 
		gmtSessionStartDateTime :: erlang:system_time(),
		gmtSessionEndDateTime :: erlang:system_time(),
		sessionTerminateCause :: 1..7,
		billingClassOfService :: string(),
		unitOfMeasure :: 1..7,
		chargeableUnit :: 1..7,
		chargeableQuantity :: pos_integer(),
		chargeAmount :: pos_integer(),
		chargeCurrencyType :: string(),
		otherParty :: string(),
		taxPercentage :: 0..100,
		taxAmount :: pos_integer(),
		taxType :: 1..14,
		intermediaryName :: string(),
		serviceName :: 1..6,
		relatedIpdrIdList :: string(),
		tempUserId :: string()}).

