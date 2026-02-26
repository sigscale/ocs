%%% ocs_diameter.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
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
%%% @doc This library module implements utilities used by diameter
%%% 	callback modules in the {@link //ocs. ocs} application.
%%%
%%% @reference <a href="https://www.mcc-mnc.com">
%%%   Mobile Country Codes (MCC) and Mobile Network Codes (MNC) </a>
%%%
-module(ocs_diameter).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

%% export the ocs_diameter public API
-export([plmn/1, authenticate_client/2]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("diameter_gen_ietf.hrl").
-include("diameter_gen_3gpp.hrl").
-include("diameter_gen_3gpp_ro_application.hrl").
-include("diameter_gen_cc_application_rfc4006.hrl").
-include("ocs.hrl").

%%----------------------------------------------------------------------
%%  The ocs_diameter public API
%%----------------------------------------------------------------------

-spec plmn(String) -> Result
	when
		String :: binary() | string(),
		Result :: {MCC, MNC, Rest},
		MCC :: [Digit],
		MNC :: [Digit],
		Digit :: 48..57,
		Rest :: string().
%% @doc Extract MCC and MNC 
plmn(String) when is_binary(String) ->
	plmn(binary_to_list(String));
%% COSMOTE
plmn("20201" ++ Rest) ->
	{"202", "01", Rest};
%% vodafone
plmn("20205" ++ Rest) ->
	{"202", "05", Rest};
%% WIND
plmn("20209" ++ Rest) ->
	{"202", "09", Rest};
%% WIND
plmn("20210" ++ Rest) ->
	{"202", "10", Rest};
%% vodafone
plmn("20404" ++ Rest) ->
	{"204", "04", Rest};
%% KPN B.V.
plmn("20408" ++ Rest) ->
	{"204", "08", Rest};
%% Telfort B.V.
plmn("20412" ++ Rest) ->
	{"204", "12", Rest};
%% Odido
plmn("20416" ++ Rest) ->
	{"204", "16", Rest};
%% Proximus
plmn("20601" ++ Rest) ->
	{"206", "01", Rest};
%% BASE
plmn("20605" ++ Rest) ->
	{"206", "05", Rest};
%% Orange Belgium
plmn("20610" ++ Rest) ->
	{"206", "10", Rest};
%% BASE
plmn("20620" ++ Rest) ->
	{"206", "20", Rest};
%% Orange France
plmn("20801" ++ Rest) ->
	{"208", "01", Rest};
%% Orange France
plmn("20802" ++ Rest) ->
	{"208", "02", Rest};
%% SFR
plmn("20809" ++ Rest) ->
	{"208", "09", Rest};
%% SFR
plmn("20810" ++ Rest) ->
	{"208", "10", Rest};
%% SFR (FEMTO)
plmn("20811" ++ Rest) ->
	{"208", "11", Rest};
%% SFR (Contact)
plmn("20813" ++ Rest) ->
	{"208", "13", Rest};
%% Free Mobile
plmn("20815" ++ Rest) ->
	{"208", "15", Rest};
%% Free Mobile
plmn("20816" ++ Rest) ->
	{"208", "16", Rest};
%% Bouygues Telecom
plmn("20820" ++ Rest) ->
	{"208", "20", Rest};
%% Bouygues Telecom (Contact)
plmn("20888" ++ Rest) ->
	{"208", "88", Rest};
%% Monaco Telecom
plmn("21201" ++ Rest) ->
	{"212", "01", Rest};
%% Monaco Telecom
plmn("21210" ++ Rest) ->
	{"212", "10", Rest};
%% ANDORRA TELECOM
plmn("21303" ++ Rest) ->
	{"213", "03", Rest};
%% Vodafone Spain
plmn("21401" ++ Rest) ->
	{"214", "01", Rest};
%% Orange Espagne, S.A.U, sociedad unipersonal
plmn("21403" ++ Rest) ->
	{"214", "03", Rest};
%% YOIGO
plmn("21404" ++ Rest) ->
	{"214", "04", Rest};
%% Movistar
plmn("21407" ++ Rest) ->
	{"214", "07", Rest};
%% Yettel
plmn("21601" ++ Rest) ->
	{"216", "01", Rest};
%% T-Mobile Hungary
plmn("21630" ++ Rest) ->
	{"216", "30", Rest};
%% vodafone
plmn("21670" ++ Rest) ->
	{"216", "70", Rest};
%% ERONET
plmn("21803" ++ Rest) ->
	{"218", "03", Rest};
%% m:tel
plmn("21805" ++ Rest) ->
	{"218", "05", Rest};
%% BH Mobile
plmn("21890" ++ Rest) ->
	{"218", "90", Rest};
%% Croatian Telecom Inc
plmn("21901" ++ Rest) ->
	{"219", "01", Rest};
%% TM HR
plmn("21902" ++ Rest) ->
	{"219", "02", Rest};
%% A1 HR
plmn("21910" ++ Rest) ->
	{"219", "10", Rest};
%% Yettel
plmn("22001" ++ Rest) ->
	{"220", "01", Rest};
%% Telekom Srbija
plmn("22003" ++ Rest) ->
	{"220", "03", Rest};
%% A1 Srbija
plmn("22005" ++ Rest) ->
	{"220", "05", Rest};
%% Vala Kosovo Telecom
plmn("22101" ++ Rest) ->
	{"221", "01", Rest};
%% IPKO
plmn("22102" ++ Rest) ->
	{"221", "02", Rest};
%% TIM
plmn("22201" ++ Rest) ->
	{"222", "01", Rest};
%% vodafone
plmn("22210" ++ Rest) ->
	{"222", "10", Rest};
%% ILIAD ITALIA
plmn("22250" ++ Rest) ->
	{"222", "50", Rest};
%% Wind Tre S.p.A.
plmn("22288" ++ Rest) ->
	{"222", "88", Rest};
%% Vodafone RO
plmn("22601" ++ Rest) ->
	{"226", "01", Rest};
%% TELEKOM.RO
plmn("22603" ++ Rest) ->
	{"226", "03", Rest};
%% Digi.Mobil
plmn("22605" ++ Rest) ->
	{"226", "05", Rest};
%% ORANGE
plmn("22610" ++ Rest) ->
	{"226", "10", Rest};
%% Swisscom
plmn("22801" ++ Rest) ->
	{"228", "01", Rest};
%% Sunrise
plmn("22802" ++ Rest) ->
	{"228", "02", Rest};
%% Salt
plmn("22803" ++ Rest) ->
	{"228", "03", Rest};
%% SolNet
plmn("22872" ++ Rest) ->
	{"228", "72", Rest};
%% netplus.ch SA
plmn("22874" ++ Rest) ->
	{"228", "74", Rest};
%% FL1
plmn("22875" ++ Rest) ->
	{"228", "75", Rest};
%% T-Mobile CZ
plmn("23001" ++ Rest) ->
	{"230", "01", Rest};
%% O2 Czech Republic
plmn("23002" ++ Rest) ->
	{"230", "02", Rest};
%% Vodafone
plmn("23003" ++ Rest) ->
	{"230", "03", Rest};
%% Orange SK
plmn("23101" ++ Rest) ->
	{"231", "01", Rest};
%% Telekom
plmn("23102" ++ Rest) ->
	{"231", "02", Rest};
%% SWAN SK
plmn("23103" ++ Rest) ->
	{"231", "03", Rest};
%% O2 - SK
plmn("23106" ++ Rest) ->
	{"231", "06", Rest};
%% A1
plmn("23201" ++ Rest) ->
	{"232", "01", Rest};
%% Magenta Telekom
plmn("23203" ++ Rest) ->
	{"232", "03", Rest};
%% Drei
plmn("23205" ++ Rest) ->
	{"232", "05", Rest};
%% Tele-ring
plmn("23207" ++ Rest) ->
	{"232", "07", Rest};
%% Drei
plmn("23210" ++ Rest) ->
	{"232", "10", Rest};
%% Magenta Telekom
plmn("23213" ++ Rest) ->
	{"232", "13", Rest};
%% spusu
plmn("23217" ++ Rest) ->
	{"232", "17", Rest};
%% Airtel-Vodafone
plmn("23403" ++ Rest) ->
	{"234", "03", Rest};
%% O2 (UK)
plmn("23410" ++ Rest) ->
	{"234", "10", Rest};
%% vodafone
plmn("23415" ++ Rest) ->
	{"234", "15", Rest};
%% 3
plmn("23420" ++ Rest) ->
	{"234", "20", Rest};
%% T-Mobile
plmn("23430" ++ Rest) ->
	{"234", "30", Rest};
%% Orange
plmn("23433" ++ Rest) ->
	{"234", "33", Rest};
%% JT
plmn("23450" ++ Rest) ->
	{"234", "50", Rest};
%% Sure
plmn("23455" ++ Rest) ->
	{"234", "55", Rest};
%% TDC Denmark
plmn("23801" ++ Rest) ->
	{"238", "01", Rest};
%% Telenor DK
plmn("23802" ++ Rest) ->
	{"238", "02", Rest};
%% 3 DK
plmn("23806" ++ Rest) ->
	{"238", "06", Rest};
%% Telia DK
plmn("23820" ++ Rest) ->
	{"238", "20", Rest};
%% Telia DK
plmn("23866" ++ Rest) ->
	{"238", "66", Rest};
%% TeliaSonera
plmn("24001" ++ Rest) ->
	{"240", "01", Rest};
%% 3
plmn("24002" ++ Rest) ->
	{"240", "02", Rest};
%% Telenor Sverige AB / Hi3G Access AB
plmn("24004" ++ Rest) ->
	{"240", "04", Rest};
%% COMVIQ GSM shared with TeliaSonera MobileNetworks
plmn("24005" ++ Rest) ->
	{"240", "05", Rest};
%% Tele 2 AB
plmn("24007" ++ Rest) ->
	{"240", "07", Rest};
%% Telenor
plmn("24008" ++ Rest) ->
	{"240", "08", Rest};
%% Telenor Sverige AB / Tele2 AB (Shared Network)
plmn("24024" ++ Rest) ->
	{"240", "24", Rest};
%% Telavox
plmn("24050" ++ Rest) ->
	{"240", "50", Rest};
%% TELENOR
plmn("24201" ++ Rest) ->
	{"242", "01", Rest};
%% NetCom
plmn("24202" ++ Rest) ->
	{"242", "02", Rest};
%% ICE Norge AS
plmn("24214" ++ Rest) ->
	{"242", "14", Rest};
%% DNA Ltd
plmn("24403" ++ Rest) ->
	{"244", "03", Rest};
%% Elisa
plmn("24405" ++ Rest) ->
	{"244", "05", Rest};
%% DNA Ltd
plmn("24412" ++ Rest) ->
	{"244", "12", Rest};
%% Alands Telekommunikation Ab
plmn("24414" ++ Rest) ->
	{"244", "14", Rest};
%% Elisa
plmn("24421" ++ Rest) ->
	{"244", "21", Rest};
%% Telia FI - SuomenYV
plmn("24436" ++ Rest) ->
	{"244", "36", Rest};
%% Erillisverkot
plmn("24445" ++ Rest) ->
	{"244", "45", Rest};
%% Erillisverkot
plmn("24446" ++ Rest) ->
	{"244", "46", Rest};
%% Erillisverkot
plmn("24447" ++ Rest) ->
	{"244", "47", Rest};
%% Telia FI
plmn("24491" ++ Rest) ->
	{"244", "91", Rest};
%% Telia LT
plmn("24601" ++ Rest) ->
	{"246", "01", Rest};
%% BITE GSM
plmn("24602" ++ Rest) ->
	{"246", "02", Rest};
%% TELE2
plmn("24603" ++ Rest) ->
	{"246", "03", Rest};
%% LMT
plmn("24701" ++ Rest) ->
	{"247", "01", Rest};
%% TELE2
plmn("24702" ++ Rest) ->
	{"247", "02", Rest};
%% Bite Latvija
plmn("24705" ++ Rest) ->
	{"247", "05", Rest};
%% XOmobile
plmn("24709" ++ Rest) ->
	{"247", "09", Rest};
%% Telia
plmn("24801" ++ Rest) ->
	{"248", "01", Rest};
%% Elisa Eesti
plmn("24802" ++ Rest) ->
	{"248", "02", Rest};
%% Tele2
plmn("24803" ++ Rest) ->
	{"248", "03", Rest};
%% MTS
plmn("25001" ++ Rest) ->
	{"250", "01", Rest};
%% MegaFon
plmn("25002" ++ Rest) ->
	{"250", "02", Rest};
%% MIATEL
plmn("25016" ++ Rest) ->
	{"250", "16", Rest};
%% LETAI
plmn("25027" ++ Rest) ->
	{"250", "27", Rest};
%% beeline
plmn("25099" ++ Rest) ->
	{"250", "99", Rest};
%% VODAFONE
plmn("25501" ++ Rest) ->
	{"255", "01", Rest};
%% Beeline UA
plmn("25502" ++ Rest) ->
	{"255", "02", Rest};
%% KYIVSTAR
plmn("25503" ++ Rest) ->
	{"255", "03", Rest};
%% lifecell
plmn("25506" ++ Rest) ->
	{"255", "06", Rest};
%% TriMob
plmn("25507" ++ Rest) ->
	{"255", "07", Rest};
%% VELCOM
plmn("25701" ++ Rest) ->
	{"257", "01", Rest};
%% MTS BY
plmn("25702" ++ Rest) ->
	{"257", "02", Rest};
%% Belarusian Telecommunications Network CJSC
plmn("25704" ++ Rest) ->
	{"257", "04", Rest};
%% VoXtel
plmn("25901" ++ Rest) ->
	{"259", "01", Rest};
%% Moldcell
plmn("25902" ++ Rest) ->
	{"259", "02", Rest};
%% Moldtelecom
plmn("25905" ++ Rest) ->
	{"259", "05", Rest};
%% Plus
plmn("26001" ++ Rest) ->
	{"260", "01", Rest};
%% T-Mobile.pl
plmn("26002" ++ Rest) ->
	{"260", "02", Rest};
%% Orange
plmn("26003" ++ Rest) ->
	{"260", "03", Rest};
%% P4
plmn("26006" ++ Rest) ->
	{"260", "06", Rest};
%% Telekom Deutschland
plmn("26201" ++ Rest) ->
	{"262", "01", Rest};
%% Vodafone
plmn("26202" ++ Rest) ->
	{"262", "02", Rest};
%% O2 Germany
plmn("26203" ++ Rest) ->
	{"262", "03", Rest};
%% O2 Germany
plmn("26207" ++ Rest) ->
	{"262", "07", Rest};
%% Sipgate Wireless GmbH
plmn("26222" ++ Rest) ->
	{"262", "22", Rest};
%% 1und1 Mobilfunk
plmn("26223" ++ Rest) ->
	{"262", "23", Rest};
%% Simsalasim DE
plmn("26226" ++ Rest) ->
	{"262", "26", Rest};
%% GIBTEL
plmn("26601" ++ Rest) ->
	{"266", "01", Rest};
%% vodafone
plmn("26801" ++ Rest) ->
	{"268", "01", Rest};
%% NOS
plmn("26803" ++ Rest) ->
	{"268", "03", Rest};
%% MEO
plmn("26806" ++ Rest) ->
	{"268", "06", Rest};
%% POST
plmn("27001" ++ Rest) ->
	{"270", "01", Rest};
%% MTX Connect
plmn("27002" ++ Rest) ->
	{"270", "02", Rest};
%% TANGO
plmn("27077" ++ Rest) ->
	{"270", "77", Rest};
%% Orange
plmn("27099" ++ Rest) ->
	{"270", "99", Rest};
%% vodafone
plmn("27201" ++ Rest) ->
	{"272", "01", Rest};
%% 3
plmn("27202" ++ Rest) ->
	{"272", "02", Rest};
%% Meteor
plmn("27203" ++ Rest) ->
	{"272", "03", Rest};
%% 3
plmn("27205" ++ Rest) ->
	{"272", "05", Rest};
%% Siminn
plmn("27401" ++ Rest) ->
	{"274", "01", Rest};
%% Vodafone Iceland
plmn("27402" ++ Rest) ->
	{"274", "02", Rest};
%% NE - On-Waves
plmn("27408" ++ Rest) ->
	{"274", "08", Rest};
%% NOVA
plmn("27411" ++ Rest) ->
	{"274", "11", Rest};
%% One.al
plmn("27601" ++ Rest) ->
	{"276", "01", Rest};
%% Vodafone Albania
plmn("27602" ++ Rest) ->
	{"276", "02", Rest};
%% epic
plmn("27801" ++ Rest) ->
	{"278", "01", Rest};
%% go mobile
plmn("27821" ++ Rest) ->
	{"278", "21", Rest};
%% Melita Mobile
plmn("27877" ++ Rest) ->
	{"278", "77", Rest};
%% Cyta
plmn("28001" ++ Rest) ->
	{"280", "01", Rest};
%% epic
plmn("28010" ++ Rest) ->
	{"280", "10", Rest};
%% PrimeTel PLC
plmn("28020" ++ Rest) ->
	{"280", "20", Rest};
%% Cablenet
plmn("28022" ++ Rest) ->
	{"280", "22", Rest};
%% GEOCELL
plmn("28201" ++ Rest) ->
	{"282", "01", Rest};
%% Magticom LTD
plmn("28202" ++ Rest) ->
	{"282", "02", Rest};
%% Cellfie Mobile
plmn("28204" ++ Rest) ->
	{"282", "04", Rest};
%% Team
plmn("28301" ++ Rest) ->
	{"283", "01", Rest};
%% Viva Armenia
plmn("28305" ++ Rest) ->
	{"283", "05", Rest};
%% Ucom
plmn("28310" ++ Rest) ->
	{"283", "10", Rest};
%% A1 BG
plmn("28401" ++ Rest) ->
	{"284", "01", Rest};
%% Vivacom
plmn("28403" ++ Rest) ->
	{"284", "03", Rest};
%% Yettel
plmn("28405" ++ Rest) ->
	{"284", "05", Rest};
%% Turkcell Iletisim Hizmetleri
plmn("28601" ++ Rest) ->
	{"286", "01", Rest};
%% Vodafone Turkey
plmn("28602" ++ Rest) ->
	{"286", "02", Rest};
%% Turk Telekom
plmn("28603" ++ Rest) ->
	{"286", "03", Rest};
%% Selam Telekom
plmn("28622" ++ Rest) ->
	{"286", "22", Rest};
%% Faroese Telecom
plmn("28801" ++ Rest) ->
	{"288", "01", Rest};
%% VODAFONE FO
plmn("28802" ++ Rest) ->
	{"288", "02", Rest};
%% Tusass
plmn("29001" ++ Rest) ->
	{"290", "01", Rest};
%% A1 SI
plmn("29340" ++ Rest) ->
	{"293", "40", Rest};
%% MOBITEL
plmn("29341" ++ Rest) ->
	{"293", "41", Rest};
%% T-2
plmn("29364" ++ Rest) ->
	{"293", "64", Rest};
%% TELEMACH
plmn("29370" ++ Rest) ->
	{"293", "70", Rest};
%% Makedonski Telekom
plmn("29401" ++ Rest) ->
	{"294", "01", Rest};
%% A1 MK
plmn("29403" ++ Rest) ->
	{"294", "03", Rest};
%% Swisscom
plmn("29501" ++ Rest) ->
	{"295", "01", Rest};
%% Salt.li
plmn("29502" ++ Rest) ->
	{"295", "02", Rest};
%% FL1
plmn("29505" ++ Rest) ->
	{"295", "05", Rest};
%% One MNE
plmn("29701" ++ Rest) ->
	{"297", "01", Rest};
%% Telekom.me
plmn("29702" ++ Rest) ->
	{"297", "02", Rest};
%% MTEL
plmn("29703" ++ Rest) ->
	{"297", "03", Rest};
%% Cogeco
plmn("302150" ++ Rest) ->
	{"302", "150", Rest};
%% TELUS
plmn("302220" ++ Rest) ->
	{"302", "220", Rest};
%% Bragg Communications Inc
plmn("302270" ++ Rest) ->
	{"302", "270", Rest};
%% OPENMOBILE
plmn("302330" ++ Rest) ->
	{"302", "330", Rest};
%% Execulink Telecom
plmn("302340" ++ Rest) ->
	{"302", "340", Rest};
%% FIDO
plmn("302370" ++ Rest) ->
	{"302", "370", Rest};
%% KNET Services
plmn("302380" ++ Rest) ->
	{"302", "380", Rest};
%% SSi
plmn("302480" ++ Rest) ->
	{"302", "480", Rest};
%% Freedom Mobile
plmn("302490" ++ Rest) ->
	{"302", "490", Rest};
%% Videotron
plmn("302500" ++ Rest) ->
	{"302", "500", Rest};
%% Videotron
plmn("302520" ++ Rest) ->
	{"302", "520", Rest};
%% Bell Mobility
plmn("302610" ++ Rest) ->
	{"302", "610", Rest};
%% Ice Wireless Inc
plmn("302620" ++ Rest) ->
	{"302", "620", Rest};
%% Rogers Wireless
plmn("302720" ++ Rest) ->
	{"302", "720", Rest};
%% SaskTel
plmn("302780" ++ Rest) ->
	{"302", "780", Rest};
%% SaskTel shared with Bell & TELUS
plmn("302880" ++ Rest) ->
	{"302", "880", Rest};
%% Wightman Telecom
plmn("302940" ++ Rest) ->
	{"302", "940", Rest};
%% AMERIS
plmn("30801" ++ Rest) ->
	{"308", "01", Rest};
%% SPM Telecom
plmn("30803" ++ Rest) ->
	{"308", "03", Rest};
%% Globaltel
plmn("30804" ++ Rest) ->
	{"308", "04", Rest};
%% Union Telephone Company
plmn("310020" ++ Rest) ->
	{"310", "020", Rest};
%% AT&T Mobility
plmn("310030" ++ Rest) ->
	{"310", "030", Rest};
%% Mobi
plmn("310040" ++ Rest) ->
	{"310", "040", Rest};
%% GCI Communications Corp.
plmn("310050" ++ Rest) ->
	{"310", "050", Rest};
%% Karrierone
plmn("310060" ++ Rest) ->
	{"310", "060", Rest};
%% GTA
plmn("310140" ++ Rest) ->
	{"310", "140", Rest};
%% AT&T Mobility
plmn("310150" ++ Rest) ->
	{"310", "150", Rest};
%% T-Mobile USA
plmn("310160" ++ Rest) ->
	{"310", "160", Rest};
%% AT&T Mobility
plmn("310170" ++ Rest) ->
	{"310", "170", Rest};
%% Alaska Wireless
plmn("310190" ++ Rest) ->
	{"310", "190", Rest};
%% T-Mobile USA
plmn("310200" ++ Rest) ->
	{"310", "200", Rest};
%% T-Mobile USA
plmn("310210" ++ Rest) ->
	{"310", "210", Rest};
%% T-Mobile USA
plmn("310220" ++ Rest) ->
	{"310", "220", Rest};
%% T-Mobile USA
plmn("310230" ++ Rest) ->
	{"310", "230", Rest};
%% T-Mobile USA
plmn("310240" ++ Rest) ->
	{"310", "240", Rest};
%% T-Mobile USA
plmn("310250" ++ Rest) ->
	{"310", "250", Rest};
%% T-Mobile USA
plmn("310260" ++ Rest) ->
	{"310", "260", Rest};
%% T-Mobile USA
plmn("310270" ++ Rest) ->
	{"310", "270", Rest};
%% AT&T Mobility
plmn("310280" ++ Rest) ->
	{"310", "280", Rest};
%% Big Sky Mobile
plmn("310300" ++ Rest) ->
	{"310", "300", Rest};
%% T-Mobile USA
plmn("310310" ++ Rest) ->
	{"310", "310", Rest};
%% CellularOne
plmn("310320" ++ Rest) ->
	{"310", "320", Rest};
%% Limitles Mobile USA
plmn("310340" ++ Rest) ->
	{"310", "340", Rest};
%% DOCOMO PACIFIC INC
plmn("310370" ++ Rest) ->
	{"310", "370", Rest};
%% AT&T Mobility
plmn("310380" ++ Rest) ->
	{"310", "380", Rest};
%% AT&T Mobility
plmn("310410" ++ Rest) ->
	{"310", "410", Rest};
%% World Mobile Netwroks, LLC
plmn("310420" ++ Rest) ->
	{"310", "420", Rest};
%% Viaero Wireless
plmn("310450" ++ Rest) ->
	{"310", "450", Rest};
%% NewCore Wireless, LLC
plmn("310460" ++ Rest) ->
	{"310", "460", Rest};
%% DOCOMO PACIFIC INC
plmn("310470" ++ Rest) ->
	{"310", "470", Rest};
%% T-Mobile USA
plmn("310490" ++ Rest) ->
	{"310", "490", Rest};
%% Inland Cellular LLC
plmn("310580" ++ Rest) ->
	{"310", "580", Rest};
%% CellularOne of Texoma
plmn("310630" ++ Rest) ->
	{"310", "630", Rest};
%% T-Mobile USA
plmn("310660" ++ Rest) ->
	{"310", "660", Rest};
%% Limitles Mobile USA
plmn("310690" ++ Rest) ->
	{"310", "690", Rest};
%% U.S.Cellular
plmn("310730" ++ Rest) ->
	{"310", "730", Rest};
%% T-Mobile USA
plmn("310800" ++ Rest) ->
	{"310", "800", Rest};
%% T-Mobile USA
plmn("310830" ++ Rest) ->
	{"310", "830", Rest};
%% telna Mobile
plmn("310840" ++ Rest) ->
	{"310", "840", Rest};
%% AT&T Mobility
plmn("310950" ++ Rest) ->
	{"310", "950", Rest};
%% Worldcall
plmn("310990" ++ Rest) ->
	{"310", "990", Rest};
%% Commnet
plmn("311040" ++ Rest) ->
	{"311", "040", Rest};
%% Pine Cellular
plmn("311080" ++ Rest) ->
	{"311", "080", Rest};
%% Tampnet
plmn("311170" ++ Rest) ->
	{"311", "170", Rest};
%% AT&T Mobility
plmn("311180" ++ Rest) ->
	{"311", "180", Rest};
%% C Spire
plmn("311230" ++ Rest) ->
	{"311", "230", Rest};
%% Cordova Wireless Communications
plmn("311240" ++ Rest) ->
	{"311", "240", Rest};
%% Verizon Wireless
plmn("311270" ++ Rest) ->
	{"311", "270", Rest};
%% GCI Communications Corp.
plmn("311370" ++ Rest) ->
	{"311", "370", Rest};
%% Verizon Wireless
plmn("311480" ++ Rest) ->
	{"311", "480", Rest};
%% T-Mobile USA
plmn("311490" ++ Rest) ->
	{"311", "490", Rest};
%% Mobi
plmn("311500" ++ Rest) ->
	{"311", "500", Rest};
%% NewCore Wireless LLC
plmn("311530" ++ Rest) ->
	{"311", "530", Rest};
%% OTZ Cellular
plmn("311560" ++ Rest) ->
	{"311", "560", Rest};
%% U.S.Cellular
plmn("311580" ++ Rest) ->
	{"311", "580", Rest};
%% Limitles Mobile USA
plmn("311600" ++ Rest) ->
	{"311", "600", Rest};
%% C Spire
plmn("311630" ++ Rest) ->
	{"311", "630", Rest};
%% United Wireless
plmn("311650" ++ Rest) ->
	{"311", "650", Rest};
%% T-Mobile USA
plmn("311660" ++ Rest) ->
	{"311", "660", Rest};
%% T-Mobile USA
plmn("311882" ++ Rest) ->
	{"311", "882", Rest};
%% VTel Wireless
plmn("311990" ++ Rest) ->
	{"311", "990", Rest};
%% Limitles Mobile USA
plmn("312180" ++ Rest) ->
	{"312", "180", Rest};
%% T-Mobile USA
plmn("312250" ++ Rest) ->
	{"312", "250", Rest};
%% NewCore Wireless, LLC
plmn("312260" ++ Rest) ->
	{"312", "260", Rest};
%% Uintah Basin Electronic Telecommunications, LLC D/B/A Strata Networks
plmn("312290" ++ Rest) ->
	{"312", "290", Rest};
%% Copper Valley Wireless
plmn("312380" ++ Rest) ->
	{"312", "380", Rest};
%% Nex-Tech Wireless
plmn("312420" ++ Rest) ->
	{"312", "420", Rest};
%% Silverstar
plmn("312430" ++ Rest) ->
	{"312", "430", Rest};
%% Nemont
plmn("312480" ++ Rest) ->
	{"312", "480", Rest};
%% NetGenuity, Inc
plmn("312630" ++ Rest) ->
	{"312", "630", Rest};
%% Southern Linc
plmn("312720" ++ Rest) ->
	{"312", "720", Rest};
%% MNSHub
plmn("313070" ++ Rest) ->
	{"313", "070", Rest};
%% ASTCA
plmn("313120" ++ Rest) ->
	{"313", "120", Rest};
%% OptimERA Wireless
plmn("313380" ++ Rest) ->
	{"313", "380", Rest};
%% Spectrum Wireless
plmn("313450" ++ Rest) ->
	{"313", "450", Rest};
%% Mobi
plmn("313460" ++ Rest) ->
	{"313", "460", Rest};
%% Liberty Mobile Puerto Rico
plmn("313790" ++ Rest) ->
	{"313", "790", Rest};
%% TGS
plmn("314440" ++ Rest) ->
	{"314", "440", Rest};
%% Lynk
plmn("314590" ++ Rest) ->
	{"314", "590", Rest};
%% Tribal Ready
plmn("314710" ++ Rest) ->
	{"314", "710", Rest};
%% TextNow
plmn("314730" ++ Rest) ->
	{"314", "730", Rest};
%% Ringer Mobile
plmn("314740" ++ Rest) ->
	{"314", "740", Rest};
%% Claro GSM
plmn("330110" ++ Rest) ->
	{"330", "110", Rest};
%% TELCEL
plmn("334020" ++ Rest) ->
	{"334", "020", Rest};
%% Movistar México
plmn("334030" ++ Rest) ->
	{"334", "030", Rest};
%% AT&T Mexico
plmn("334050" ++ Rest) ->
	{"334", "050", Rest};
%% AT&T Mexico
plmn("334070" ++ Rest) ->
	{"334", "070", Rest};
%% VINOC
plmn("334230" ++ Rest) ->
	{"334", "230", Rest};
%% Movistar México
plmn("334240" ++ Rest) ->
	{"334", "240", Rest};
%% Digicel Jamaica
plmn("338050" ++ Rest) ->
	{"338", "050", Rest};
%% FLOW (LLA)
plmn("338180" ++ Rest) ->
	{"338", "180", Rest};
%% Orange
plmn("34001" ++ Rest) ->
	{"340", "01", Rest};
%% Outremer
plmn("34002" ++ Rest) ->
	{"340", "02", Rest};
%% Free
plmn("34004" ++ Rest) ->
	{"340", "04", Rest};
%% Free
plmn("34009" ++ Rest) ->
	{"340", "09", Rest};
%% Digicel
plmn("34020" ++ Rest) ->
	{"340", "20", Rest};
%% FLOW (LLA)
plmn("342600" ++ Rest) ->
	{"342", "600", Rest};
%% DIGICEL
plmn("342750" ++ Rest) ->
	{"342", "750", Rest};
%% APUA inet
plmn("34403" ++ Rest) ->
	{"344", "03", Rest};
%% FLOW (LLA)
plmn("344920" ++ Rest) ->
	{"344", "920", Rest};
%% FLOW (LLA)
plmn("346140" ++ Rest) ->
	{"346", "140", Rest};
%% FLOW (LLA)
plmn("348170" ++ Rest) ->
	{"348", "170", Rest};
%% CCT
plmn("348570" ++ Rest) ->
	{"348", "570", Rest};
%% CELLONE
plmn("350000" ++ Rest) ->
	{"350", "000", Rest};
%% FLOW (LLA)
plmn("352110" ++ Rest) ->
	{"352", "110", Rest};
%% FLOW (LLA)
plmn("354860" ++ Rest) ->
	{"354", "860", Rest};
%% FLOW (LLA)
plmn("356110" ++ Rest) ->
	{"356", "110", Rest};
%% Digicel (St Lucia)
plmn("358050" ++ Rest) ->
	{"358", "050", Rest};
%% FLOW (LLA)
plmn("358110" ++ Rest) ->
	{"358", "110", Rest};
%% FLOW (LLA)
plmn("360110" ++ Rest) ->
	{"360", "110", Rest};
%% Telcell N.V.
plmn("36251" ++ Rest) ->
	{"362", "51", Rest};
%% Digicel Curacao
plmn("36269" ++ Rest) ->
	{"362", "69", Rest};
%% Kla
plmn("36278" ++ Rest) ->
	{"362", "78", Rest};
%% UTS(SETEL)- NETHERLANDS ANTILLES
plmn("36291" ++ Rest) ->
	{"362", "91", Rest};
%% SETAR GSM
plmn("36301" ++ Rest) ->
	{"363", "01", Rest};
%% Digicel
plmn("36302" ++ Rest) ->
	{"363", "02", Rest};
%% BTC BAHAMAS
plmn("36439" ++ Rest) ->
	{"364", "39", Rest};
%% aliv
plmn("36449" ++ Rest) ->
	{"364", "49", Rest};
%% FLOW (LLA)
plmn("365840" ++ Rest) ->
	{"365", "840", Rest};
%% FLOW (LLA)
plmn("366110" ++ Rest) ->
	{"366", "110", Rest};
%% ALTICE
plmn("37001" ++ Rest) ->
	{"370", "01", Rest};
%% CLARO GSM
plmn("37002" ++ Rest) ->
	{"370", "02", Rest};
%% Viva DO
plmn("37004" ++ Rest) ->
	{"370", "04", Rest};
%% Natcom
plmn("37203" ++ Rest) ->
	{"372", "03", Rest};
%% TSTT
plmn("37412" ++ Rest) ->
	{"374", "12", Rest};
%% Digicel Trinidad and Tobago Ltd
plmn("374130" ++ Rest) ->
	{"374", "130", Rest};
%% FLOW (LLA)
plmn("376350" ++ Rest) ->
	{"376", "350", Rest};
%% Digicel (Turks and Caicos) Limited
plmn("376360" ++ Rest) ->
	{"376", "360", Rest};
%% AZERCELL GSM
plmn("40001" ++ Rest) ->
	{"400", "01", Rest};
%% Bakcell LLC
plmn("40002" ++ Rest) ->
	{"400", "02", Rest};
%% Nar Mobile
plmn("40004" ++ Rest) ->
	{"400", "04", Rest};
%% Beeline
plmn("40101" ++ Rest) ->
	{"401", "01", Rest};
%% Kcell
plmn("40102" ++ Rest) ->
	{"401", "02", Rest};
%% Tele2 Kazakhstan
plmn("40177" ++ Rest) ->
	{"401", "77", Rest};
%% B-Mobile
plmn("40211" ++ Rest) ->
	{"402", "11", Rest};
%% TASHICELL
plmn("40277" ++ Rest) ->
	{"402", "77", Rest};
%% Vodafone Idea Limited
plmn("40401" ++ Rest) ->
	{"404", "01", Rest};
%% airtel
plmn("40402" ++ Rest) ->
	{"404", "02", Rest};
%% airtel
plmn("40403" ++ Rest) ->
	{"404", "03", Rest};
%% Vodafone Idea Limited
plmn("40405" ++ Rest) ->
	{"404", "05", Rest};
%% Vodafone Idea Limited
plmn("40407" ++ Rest) ->
	{"404", "07", Rest};
%% airtel
plmn("40410" ++ Rest) ->
	{"404", "10", Rest};
%% Vodafone Idea Limited
plmn("40411" ++ Rest) ->
	{"404", "11", Rest};
%% Vodafone Idea Limited
plmn("40414" ++ Rest) ->
	{"404", "14", Rest};
%% Vodafone Idea Limited
plmn("40415" ++ Rest) ->
	{"404", "15", Rest};
%% AIRTEL
plmn("40416" ++ Rest) ->
	{"404", "16", Rest};
%% Vodafone Idea Limited
plmn("40419" ++ Rest) ->
	{"404", "19", Rest};
%% Vodafone Idea Limited
plmn("40420" ++ Rest) ->
	{"404", "20", Rest};
%% Vodafone Idea Limited
plmn("40422" ++ Rest) ->
	{"404", "22", Rest};
%% Vodafone Idea Limited
plmn("40427" ++ Rest) ->
	{"404", "27", Rest};
%% Vodafone Idea Limited
plmn("40430" ++ Rest) ->
	{"404", "30", Rest};
%% airtel
plmn("40431" ++ Rest) ->
	{"404", "31", Rest};
%% CellOne Haryana
plmn("40434" ++ Rest) ->
	{"404", "34", Rest};
%% CellOne Assam
plmn("40438" ++ Rest) ->
	{"404", "38", Rest};
%% BSNL MOBILE DELHI
plmn("40439" ++ Rest) ->
	{"404", "39", Rest};
%% airtel
plmn("40440" ++ Rest) ->
	{"404", "40", Rest};
%% Vodafone Idea Limited
plmn("40443" ++ Rest) ->
	{"404", "43", Rest};
%% airtel
plmn("40445" ++ Rest) ->
	{"404", "45", Rest};
%% airtel
plmn("40449" ++ Rest) ->
	{"404", "49", Rest};
%% CellOne Himachal Pradesh
plmn("40451" ++ Rest) ->
	{"404", "51", Rest};
%% CellOne Punjab
plmn("40453" ++ Rest) ->
	{"404", "53", Rest};
%% CellOne Uttar Pradesh (West)
plmn("40454" ++ Rest) ->
	{"404", "54", Rest};
%% CellOne Uttar Pradesh (East)
plmn("40455" ++ Rest) ->
	{"404", "55", Rest};
%% Vodafone Idea Limited
plmn("40456" ++ Rest) ->
	{"404", "56", Rest};
%% CellOne Gujarat
plmn("40457" ++ Rest) ->
	{"404", "57", Rest};
%% CellOne Madhya Pradesh
plmn("40458" ++ Rest) ->
	{"404", "58", Rest};
%% CellOne Rajasthan
plmn("40459" ++ Rest) ->
	{"404", "59", Rest};
%% Vodafone Idea Limited
plmn("40460" ++ Rest) ->
	{"404", "60", Rest};
%% CellOne Jammu & Kashmir
plmn("40462" ++ Rest) ->
	{"404", "62", Rest};
%% CellOne Chennai
plmn("40464" ++ Rest) ->
	{"404", "64", Rest};
%% BSNL MOBILE MUMBAI
plmn("40465" ++ Rest) ->
	{"404", "65", Rest};
%% CellOne Maharashtra
plmn("40466" ++ Rest) ->
	{"404", "66", Rest};
%% AIRTEL
plmn("40470" ++ Rest) ->
	{"404", "70", Rest};
%% CellOne Karnataka
plmn("40471" ++ Rest) ->
	{"404", "71", Rest};
%% CellOne Kerala
plmn("40472" ++ Rest) ->
	{"404", "72", Rest};
%% CellOne Andhra Pradesh
plmn("40473" ++ Rest) ->
	{"404", "73", Rest};
%% CellOne West Bengal
plmn("40474" ++ Rest) ->
	{"404", "74", Rest};
%% CellOne Bihar
plmn("40475" ++ Rest) ->
	{"404", "75", Rest};
%% CellOne Orissa
plmn("40476" ++ Rest) ->
	{"404", "76", Rest};
%% CellOne North East
plmn("40477" ++ Rest) ->
	{"404", "77", Rest};
%% Vodafone Idea Limited
plmn("40478" ++ Rest) ->
	{"404", "78", Rest};
%% CellOne All India except Delhi & Mumbai
plmn("40479" ++ Rest) ->
	{"404", "79", Rest};
%% CellOne Tamil Nadu
plmn("40480" ++ Rest) ->
	{"404", "80", Rest};
%% CellOne Kolkata
plmn("40481" ++ Rest) ->
	{"404", "81", Rest};
%% Vodafone Idea Limited
plmn("40482" ++ Rest) ->
	{"404", "82", Rest};
%% Vodafone Idea Limited
plmn("40484" ++ Rest) ->
	{"404", "84", Rest};
%% Vodafone Idea Limited
plmn("40486" ++ Rest) ->
	{"404", "86", Rest};
%% airtel
plmn("40490" ++ Rest) ->
	{"404", "90", Rest};
%% airtel
plmn("40492" ++ Rest) ->
	{"404", "92", Rest};
%% airtel
plmn("40493" ++ Rest) ->
	{"404", "93", Rest};
%% airtel
plmn("40494" ++ Rest) ->
	{"404", "94", Rest};
%% airtel
plmn("40495" ++ Rest) ->
	{"404", "95", Rest};
%% airtel
plmn("40496" ++ Rest) ->
	{"404", "96", Rest};
%% airtel
plmn("40497" ++ Rest) ->
	{"404", "97", Rest};
%% airtel
plmn("40498" ++ Rest) ->
	{"404", "98", Rest};
%% airtel
plmn("40551" ++ Rest) ->
	{"405", "51", Rest};
%% airtel
plmn("40552" ++ Rest) ->
	{"405", "52", Rest};
%% airtel
plmn("40553" ++ Rest) ->
	{"405", "53", Rest};
%% airtel
plmn("40554" ++ Rest) ->
	{"405", "54", Rest};
%% airtel
plmn("40555" ++ Rest) ->
	{"405", "55", Rest};
%% airtel
plmn("40556" ++ Rest) ->
	{"405", "56", Rest};
%% Vodafone Idea Limited
plmn("40567" ++ Rest) ->
	{"405", "67", Rest};
%% Vodafone Idea Limited
plmn("40570" ++ Rest) ->
	{"405", "70", Rest};
%% Vodafone Idea Limited
plmn("405751" ++ Rest) ->
	{"405", "751", Rest};
%% Vodafone Idea Limited
plmn("405753" ++ Rest) ->
	{"405", "753", Rest};
%% Vodafone Idea Limited
plmn("405755" ++ Rest) ->
	{"405", "755", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405840" ++ Rest) ->
	{"405", "840", Rest};
%% Vodafone Idea Limited
plmn("405846" ++ Rest) ->
	{"405", "846", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405854" ++ Rest) ->
	{"405", "854", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405855" ++ Rest) ->
	{"405", "855", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405856" ++ Rest) ->
	{"405", "856", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405857" ++ Rest) ->
	{"405", "857", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405858" ++ Rest) ->
	{"405", "858", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405859" ++ Rest) ->
	{"405", "859", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405860" ++ Rest) ->
	{"405", "860", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405861" ++ Rest) ->
	{"405", "861", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405862" ++ Rest) ->
	{"405", "862", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405863" ++ Rest) ->
	{"405", "863", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405864" ++ Rest) ->
	{"405", "864", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405865" ++ Rest) ->
	{"405", "865", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405866" ++ Rest) ->
	{"405", "866", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405867" ++ Rest) ->
	{"405", "867", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405868" ++ Rest) ->
	{"405", "868", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405869" ++ Rest) ->
	{"405", "869", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405870" ++ Rest) ->
	{"405", "870", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405871" ++ Rest) ->
	{"405", "871", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405872" ++ Rest) ->
	{"405", "872", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405873" ++ Rest) ->
	{"405", "873", Rest};
%% Reliance Jio Infocomm Ltd India
plmn("405874" ++ Rest) ->
	{"405", "874", Rest};
%% Jazz
plmn("41001" ++ Rest) ->
	{"410", "01", Rest};
%% Ufone
plmn("41003" ++ Rest) ->
	{"410", "03", Rest};
%% Telenor Pakistan (Pvt) Ltd.
plmn("41006" ++ Rest) ->
	{"410", "06", Rest};
%% Jazz
plmn("41007" ++ Rest) ->
	{"410", "07", Rest};
%% AWCC
plmn("41201" ++ Rest) ->
	{"412", "01", Rest};
%% ROSHAN
plmn("41220" ++ Rest) ->
	{"412", "20", Rest};
%% MTN Afghanistan
plmn("41240" ++ Rest) ->
	{"412", "40", Rest};
%% Etisalat Afghanistan
plmn("41250" ++ Rest) ->
	{"412", "50", Rest};
%% Mobitel
plmn("41301" ++ Rest) ->
	{"413", "01", Rest};
%% DIALOG
plmn("41302" ++ Rest) ->
	{"413", "02", Rest};
%% Hutchison Telecommunications Lanka (Pte)
plmn("41303" ++ Rest) ->
	{"413", "03", Rest};
%% DIALOG
plmn("41305" ++ Rest) ->
	{"413", "05", Rest};
%% Hutchison Telecommunications Lanka (Pte)
plmn("41308" ++ Rest) ->
	{"413", "08", Rest};
%% Hutchison Telecommunications Lanka (Pte)
plmn("41309" ++ Rest) ->
	{"413", "09", Rest};
%% MPT GSM Network
plmn("41401" ++ Rest) ->
	{"414", "01", Rest};
%% U9
plmn("41405" ++ Rest) ->
	{"414", "05", Rest};
%% MYTEL
plmn("41409" ++ Rest) ->
	{"414", "09", Rest};
%% Alfa
plmn("41501" ++ Rest) ->
	{"415", "01", Rest};
%% MIC2
plmn("41503" ++ Rest) ->
	{"415", "03", Rest};
%% Zain JO
plmn("41601" ++ Rest) ->
	{"416", "01", Rest};
%% Umniah Mobile Company
plmn("41603" ++ Rest) ->
	{"416", "03", Rest};
%% Orange
plmn("41677" ++ Rest) ->
	{"416", "77", Rest};
%% Asiacell
plmn("41805" ++ Rest) ->
	{"418", "05", Rest};
%% zain IQ
plmn("41820" ++ Rest) ->
	{"418", "20", Rest};
%% Zain Iraq
plmn("41830" ++ Rest) ->
	{"418", "30", Rest};
%% Korek Telecom
plmn("41840" ++ Rest) ->
	{"418", "40", Rest};
%% zain KW
plmn("41902" ++ Rest) ->
	{"419", "02", Rest};
%% Ooredoo
plmn("41903" ++ Rest) ->
	{"419", "03", Rest};
%% stc
plmn("41904" ++ Rest) ->
	{"419", "04", Rest};
%% stc
plmn("42001" ++ Rest) ->
	{"420", "01", Rest};
%% Mobily
plmn("42003" ++ Rest) ->
	{"420", "03", Rest};
%% Zain Saudi Arabia
plmn("42004" ++ Rest) ->
	{"420", "04", Rest};
%% Omantel Oman
plmn("42202" ++ Rest) ->
	{"422", "02", Rest};
%% Ooredoo Oman
plmn("42203" ++ Rest) ->
	{"422", "03", Rest};
%% Vodafone
plmn("42206" ++ Rest) ->
	{"422", "06", Rest};
%% ETISALAT
plmn("42402" ++ Rest) ->
	{"424", "02", Rest};
%% du
plmn("42403" ++ Rest) ->
	{"424", "03", Rest};
%% Partner IL
plmn("42501" ++ Rest) ->
	{"425", "01", Rest};
%% Cellcom Israel
plmn("42502" ++ Rest) ->
	{"425", "02", Rest};
%% Pelephone
plmn("42503" ++ Rest) ->
	{"425", "03", Rest};
%% Palestine Telecommunications Co. P.L.C
plmn("42505" ++ Rest) ->
	{"425", "05", Rest};
%% Ooredoo Palestine
plmn("42506" ++ Rest) ->
	{"425", "06", Rest};
%% Hot Mobile Ltd.
plmn("42507" ++ Rest) ->
	{"425", "07", Rest};
%% Hot Mobile Ltd.
plmn("42528" ++ Rest) ->
	{"425", "28", Rest};
%% BATELCO
plmn("42601" ++ Rest) ->
	{"426", "01", Rest};
%% Zain BH
plmn("42602" ++ Rest) ->
	{"426", "02", Rest};
%% STC Bahrain B.S.C Closed
plmn("42604" ++ Rest) ->
	{"426", "04", Rest};
%% Ooredoo
plmn("42701" ++ Rest) ->
	{"427", "01", Rest};
%% Vodafone Qatar
plmn("42702" ++ Rest) ->
	{"427", "02", Rest};
%% Skytel
plmn("42801" ++ Rest) ->
	{"428", "01", Rest};
%% gmobile
plmn("42806" ++ Rest) ->
	{"428", "06", Rest};
%% Unitel
plmn("42888" ++ Rest) ->
	{"428", "88", Rest};
%% MobiCom
plmn("42899" ++ Rest) ->
	{"428", "99", Rest};
%% Nepal Telecom
plmn("42901" ++ Rest) ->
	{"429", "01", Rest};
%% Beeline
plmn("43404" ++ Rest) ->
	{"434", "04", Rest};
%% Ucell
plmn("43405" ++ Rest) ->
	{"434", "05", Rest};
%% UMS
plmn("43407" ++ Rest) ->
	{"434", "07", Rest};
%% Uzbektelecom
plmn("43408" ++ Rest) ->
	{"434", "08", Rest};
%% Babilon-M
plmn("43604" ++ Rest) ->
	{"436", "04", Rest};
%% ZET-MOBILE
plmn("43605" ++ Rest) ->
	{"436", "05", Rest};
%% Beeline
plmn("43701" ++ Rest) ->
	{"437", "01", Rest};
%% MEGA
plmn("43705" ++ Rest) ->
	{"437", "05", Rest};
%% O!
plmn("43709" ++ Rest) ->
	{"437", "09", Rest};
%% DOCOMO
plmn("44010" ++ Rest) ->
	{"440", "10", Rest};
%% Rakuten Mobile
plmn("44011" ++ Rest) ->
	{"440", "11", Rest};
%% SoftBank
plmn("44020" ++ Rest) ->
	{"440", "20", Rest};
%% KDDI Corporation
plmn("44050" ++ Rest) ->
	{"440", "50", Rest};
%% KDDI Corporation
plmn("44051" ++ Rest) ->
	{"440", "51", Rest};
%% KDDI Corporation
plmn("44052" ++ Rest) ->
	{"440", "52", Rest};
%% KDDI Corporation
plmn("44054" ++ Rest) ->
	{"440", "54", Rest};
%% KDDI Corporation
plmn("44055" ++ Rest) ->
	{"440", "55", Rest};
%% NTTDOCOMO,INC
plmn("44091" ++ Rest) ->
	{"440", "91", Rest};
%% KDDI Corporation
plmn("44092" ++ Rest) ->
	{"440", "92", Rest};
%% SoftBank
plmn("44093" ++ Rest) ->
	{"440", "93", Rest};
%% Rakuten Mobile
plmn("44094" ++ Rest) ->
	{"440", "94", Rest};
%% SK Telecom
plmn("45005" ++ Rest) ->
	{"450", "05", Rest};
%% LG Uplus Corp
plmn("45006" ++ Rest) ->
	{"450", "06", Rest};
%% KT
plmn("45008" ++ Rest) ->
	{"450", "08", Rest};
%% LG Uplus Corp
plmn("45010" ++ Rest) ->
	{"450", "10", Rest};
%% Mobifone
plmn("45201" ++ Rest) ->
	{"452", "01", Rest};
%% VINAPHONE
plmn("45202" ++ Rest) ->
	{"452", "02", Rest};
%% Viettel
plmn("45204" ++ Rest) ->
	{"452", "04", Rest};
%% CSL
plmn("45400" ++ Rest) ->
	{"454", "00", Rest};
%% CSL
plmn("45402" ++ Rest) ->
	{"454", "02", Rest};
%% 3
plmn("45403" ++ Rest) ->
	{"454", "03", Rest};
%% 3
plmn("45404" ++ Rest) ->
	{"454", "04", Rest};
%% SmarTone HK
plmn("45406" ++ Rest) ->
	{"454", "06", Rest};
%% CSL
plmn("45410" ++ Rest) ->
	{"454", "10", Rest};
%% China Mobile HK
plmn("45412" ++ Rest) ->
	{"454", "12", Rest};
%% China Mobile HK
plmn("45413" ++ Rest) ->
	{"454", "13", Rest};
%% SmarTone HK
plmn("45415" ++ Rest) ->
	{"454", "15", Rest};
%% CSL
plmn("45416" ++ Rest) ->
	{"454", "16", Rest};
%% SmarTone HK
plmn("45417" ++ Rest) ->
	{"454", "17", Rest};
%% CSL
plmn("45418" ++ Rest) ->
	{"454", "18", Rest};
%% CSL
plmn("45419" ++ Rest) ->
	{"454", "19", Rest};
%% CSL
plmn("45420" ++ Rest) ->
	{"454", "20", Rest};
%% CTM
plmn("45501" ++ Rest) ->
	{"455", "01", Rest};
%% China Telecom Macau
plmn("45502" ++ Rest) ->
	{"455", "02", Rest};
%% 3 Macau
plmn("45503" ++ Rest) ->
	{"455", "03", Rest};
%% CTM
plmn("45504" ++ Rest) ->
	{"455", "04", Rest};
%% 3 Macau
plmn("45505" ++ Rest) ->
	{"455", "05", Rest};
%% China Telecom Macau
plmn("45507" ++ Rest) ->
	{"455", "07", Rest};
%% Cellcard
plmn("45601" ++ Rest) ->
	{"456", "01", Rest};
%% Smart Axiata
plmn("45602" ++ Rest) ->
	{"456", "02", Rest};
%% Smart Axiata
plmn("45606" ++ Rest) ->
	{"456", "06", Rest};
%% Metfone
plmn("45608" ++ Rest) ->
	{"456", "08", Rest};
%% LAO TELECOMMUNICATIONS
plmn("45701" ++ Rest) ->
	{"457", "01", Rest};
%% MVNO
plmn("45702" ++ Rest) ->
	{"457", "02", Rest};
%% LAT
plmn("45703" ++ Rest) ->
	{"457", "03", Rest};
%% BEST TELECOM
plmn("45707" ++ Rest) ->
	{"457", "07", Rest};
%% CHINA MOBILE
plmn("46000" ++ Rest) ->
	{"460", "00", Rest};
%% CHINA UNICOM GSM
plmn("46001" ++ Rest) ->
	{"460", "01", Rest};
%% CHINA MOBILE
plmn("46002" ++ Rest) ->
	{"460", "02", Rest};
%% China Telecom
plmn("46003" ++ Rest) ->
	{"460", "03", Rest};
%% CHINA MOBILE
plmn("46004" ++ Rest) ->
	{"460", "04", Rest};
%% CHINA MOBILE
plmn("46007" ++ Rest) ->
	{"460", "07", Rest};
%% CHINA MOBILE
plmn("46008" ++ Rest) ->
	{"460", "08", Rest};
%% CHINA UNICOM GSM
plmn("46009" ++ Rest) ->
	{"460", "09", Rest};
%% China Telecom
plmn("46011" ++ Rest) ->
	{"460", "11", Rest};
%% CHINA BROADNET
plmn("46015" ++ Rest) ->
	{"460", "15", Rest};
%% Far EasTone
plmn("46601" ++ Rest) ->
	{"466", "01", Rest};
%% TWN APT
plmn("46605" ++ Rest) ->
	{"466", "05", Rest};
%% Chunghwa Telecom
plmn("46611" ++ Rest) ->
	{"466", "11", Rest};
%% TWN APT
plmn("46612" ++ Rest) ->
	{"466", "12", Rest};
%% Chunghwa Telecom
plmn("46615" ++ Rest) ->
	{"466", "15", Rest};
%% Chunghwa Telecom
plmn("46616" ++ Rest) ->
	{"466", "16", Rest};
%% Chunghwa Telecom
plmn("46617" ++ Rest) ->
	{"466", "17", Rest};
%% Chunghwa Telecom
plmn("46618" ++ Rest) ->
	{"466", "18", Rest};
%% Chunghwa Telecom
plmn("46619" ++ Rest) ->
	{"466", "19", Rest};
%% Far EasTone
plmn("46688" ++ Rest) ->
	{"466", "88", Rest};
%% Taiwan Star
plmn("46689" ++ Rest) ->
	{"466", "89", Rest};
%% Chunghwa Telecom
plmn("46692" ++ Rest) ->
	{"466", "92", Rest};
%% Taiwan Mobile
plmn("46697" ++ Rest) ->
	{"466", "97", Rest};
%% grameenphone
plmn("470001" ++ Rest) ->
	{"470", "001", Rest};
%% robi axiata
plmn("47002" ++ Rest) ->
	{"470", "02", Rest};
%% Banglalink
plmn("47003" ++ Rest) ->
	{"470", "03", Rest};
%% Teletalk Bangladesh Ltd
plmn("47004" ++ Rest) ->
	{"470", "04", Rest};
%% DHIRAAGU
plmn("47201" ++ Rest) ->
	{"472", "01", Rest};
%% Ooredoo Maldives PVT LTD
plmn("47202" ++ Rest) ->
	{"472", "02", Rest};
%% MMS & MB
plmn("50212" ++ Rest) ->
	{"502", "12", Rest};
%% CelcomDigi
plmn("50213" ++ Rest) ->
	{"502", "13", Rest};
%% Tune Talk
plmn("502150" ++ Rest) ->
	{"502", "150", Rest};
%% Yes
plmn("502152" ++ Rest) ->
	{"502", "152", Rest};
%% unifi
plmn("502153" ++ Rest) ->
	{"502", "153", Rest};
%% CelcomDigi
plmn("50216" ++ Rest) ->
	{"502", "16", Rest};
%% U Mobile
plmn("50218" ++ Rest) ->
	{"502", "18", Rest};
%% CelcomDigi
plmn("50219" ++ Rest) ->
	{"502", "19", Rest};
%% Telstra MobileNet
plmn("50501" ++ Rest) ->
	{"505", "01", Rest};
%% YES OPTUS
plmn("50502" ++ Rest) ->
	{"505", "02", Rest};
%% Vodafone
plmn("50503" ++ Rest) ->
	{"505", "03", Rest};
%% Norfolk Telecom
plmn("50510" ++ Rest) ->
	{"505", "10", Rest};
%% Pivotel
plmn("50550" ++ Rest) ->
	{"505", "50", Rest};
%% Telstra MobileNet
plmn("50571" ++ Rest) ->
	{"505", "71", Rest};
%% Telstra MobileNet
plmn("50572" ++ Rest) ->
	{"505", "72", Rest};
%% INDOSATOOREDOO
plmn("51001" ++ Rest) ->
	{"510", "01", Rest};
%% Smartfren
plmn("51009" ++ Rest) ->
	{"510", "09", Rest};
%% TELKOMSEL
plmn("51010" ++ Rest) ->
	{"510", "10", Rest};
%% INDOSATOOREDOO
plmn("51021" ++ Rest) ->
	{"510", "21", Rest};
%% Smartfren
plmn("51028" ++ Rest) ->
	{"510", "28", Rest};
%% Indosat Ooredoo Hutchison
plmn("51089" ++ Rest) ->
	{"510", "89", Rest};
%% Telkomcel
plmn("51401" ++ Rest) ->
	{"514", "01", Rest};
%% Timor Telecom
plmn("51402" ++ Rest) ->
	{"514", "02", Rest};
%% Globe Telecom
plmn("51502" ++ Rest) ->
	{"515", "02", Rest};
%% SMART Gold
plmn("51503" ++ Rest) ->
	{"515", "03", Rest};
%% CAT Telecom PLC
plmn("52000" ++ Rest) ->
	{"520", "00", Rest};
%% AIS
plmn("52001" ++ Rest) ->
	{"520", "01", Rest};
%% AIS
plmn("52003" ++ Rest) ->
	{"520", "03", Rest};
%% True Move H
plmn("52004" ++ Rest) ->
	{"520", "04", Rest};
%% True Move H
plmn("52005" ++ Rest) ->
	{"520", "05", Rest};
%% NT Mobile
plmn("52015" ++ Rest) ->
	{"520", "15", Rest};
%% True Move H
plmn("52019" ++ Rest) ->
	{"520", "19", Rest};
%% NT Mobile
plmn("52047" ++ Rest) ->
	{"520", "47", Rest};
%% True Move H
plmn("52099" ++ Rest) ->
	{"520", "99", Rest};
%% Singtel
plmn("52501" ++ Rest) ->
	{"525", "01", Rest};
%% Singtel
plmn("52502" ++ Rest) ->
	{"525", "02", Rest};
%% M1 Limited
plmn("52503" ++ Rest) ->
	{"525", "03", Rest};
%% StarHub
plmn("52505" ++ Rest) ->
	{"525", "05", Rest};
%% StarHub
plmn("52508" ++ Rest) ->
	{"525", "08", Rest};
%% SIMBA
plmn("52510" ++ Rest) ->
	{"525", "10", Rest};
%% Unified National Networks Sdn Bhd
plmn("52801" ++ Rest) ->
	{"528", "01", Rest};
%% Unified National Networks Sdn Bhd
plmn("52802" ++ Rest) ->
	{"528", "02", Rest};
%% Unified National Networks Sdn Bhd
plmn("52803" ++ Rest) ->
	{"528", "03", Rest};
%% Unified National Networks Sdn Bhd
plmn("52811" ++ Rest) ->
	{"528", "11", Rest};
%% One NZ
plmn("53001" ++ Rest) ->
	{"530", "01", Rest};
%% Spark NZ
plmn("53005" ++ Rest) ->
	{"530", "05", Rest};
%% One NZ SpaceX
plmn("53013" ++ Rest) ->
	{"530", "13", Rest};
%% 2degrees
plmn("53024" ++ Rest) ->
	{"530", "24", Rest};
%% NEOTEL
plmn("53603" ++ Rest) ->
	{"536", "03", Rest};
%% Telikom Limited
plmn("53701" ++ Rest) ->
	{"537", "01", Rest};
%% Telikom Limited
plmn("53702" ++ Rest) ->
	{"537", "02", Rest};
%% Digicel PNG
plmn("53703" ++ Rest) ->
	{"537", "03", Rest};
%% VODAFONE
plmn("53704" ++ Rest) ->
	{"537", "04", Rest};
%% Digicel (Tonga) Limited
plmn("53988" ++ Rest) ->
	{"539", "88", Rest};
%% Our Telekom
plmn("54001" ++ Rest) ->
	{"540", "01", Rest};
%% bmobile
plmn("54002" ++ Rest) ->
	{"540", "02", Rest};
%% SMILE
plmn("54101" ++ Rest) ->
	{"541", "01", Rest};
%% Digicel
plmn("54105" ++ Rest) ->
	{"541", "05", Rest};
%% VODAFONE
plmn("54201" ++ Rest) ->
	{"542", "01", Rest};
%% Digicel (Fiji) Limited
plmn("54202" ++ Rest) ->
	{"542", "02", Rest};
%% Manuia
plmn("54301" ++ Rest) ->
	{"543", "01", Rest};
%% BLUESKY
plmn("544110" ++ Rest) ->
	{"544", "110", Rest};
%% OceanCell
plmn("54502" ++ Rest) ->
	{"545", "02", Rest};
%% Helia
plmn("54601" ++ Rest) ->
	{"546", "01", Rest};
%% VITI
plmn("54705" ++ Rest) ->
	{"547", "05", Rest};
%% VODAFONE
plmn("54715" ++ Rest) ->
	{"547", "15", Rest};
%% VINI
plmn("54720" ++ Rest) ->
	{"547", "20", Rest};
%% Telecom Cook Islands
plmn("54801" ++ Rest) ->
	{"548", "01", Rest};
%% Digicel
plmn("54900" ++ Rest) ->
	{"549", "00", Rest};
%% Vodafone Samoa Limited
plmn("54927" ++ Rest) ->
	{"549", "27", Rest};
%% FSM Telecommunications Corporation
plmn("55001" ++ Rest) ->
	{"550", "01", Rest};
%% Palau National Communications Corporation
plmn("55201" ++ Rest) ->
	{"552", "01", Rest};
%% Palau Mobile Communications Inc.
plmn("55299" ++ Rest) ->
	{"552", "99", Rest};
%% TTC
plmn("55301" ++ Rest) ->
	{"553", "01", Rest};
%% Orange
plmn("60201" ++ Rest) ->
	{"602", "01", Rest};
%% vodafone
plmn("60202" ++ Rest) ->
	{"602", "02", Rest};
%% Etisalat
plmn("60203" ++ Rest) ->
	{"602", "03", Rest};
%% we
plmn("60204" ++ Rest) ->
	{"602", "04", Rest};
%% Mobilis
plmn("60301" ++ Rest) ->
	{"603", "01", Rest};
%% DJEZZY
plmn("60302" ++ Rest) ->
	{"603", "02", Rest};
%% Orange
plmn("60400" ++ Rest) ->
	{"604", "00", Rest};
%% IAM
plmn("60401" ++ Rest) ->
	{"604", "01", Rest};
%% WANA
plmn("60402" ++ Rest) ->
	{"604", "02", Rest};
%% TUNTEL
plmn("60502" ++ Rest) ->
	{"605", "02", Rest};
%% OOREDOO TN
plmn("60503" ++ Rest) ->
	{"605", "03", Rest};
%% Libyana
plmn("60600" ++ Rest) ->
	{"606", "00", Rest};
%% AFRICELL
plmn("60702" ++ Rest) ->
	{"607", "02", Rest};
%% Comium Gambia Co. Ltd
plmn("60703" ++ Rest) ->
	{"607", "03", Rest};
%% Qcell
plmn("60704" ++ Rest) ->
	{"607", "04", Rest};
%% Orange SN
plmn("60801" ++ Rest) ->
	{"608", "01", Rest};
%% Free
plmn("60802" ++ Rest) ->
	{"608", "02", Rest};
%% MATTEL
plmn("60901" ++ Rest) ->
	{"609", "01", Rest};
%% Orange MALI
plmn("61002" ++ Rest) ->
	{"610", "02", Rest};
%% Orange Guinee
plmn("61101" ++ Rest) ->
	{"611", "01", Rest};
%% MOOV CI
plmn("61202" ++ Rest) ->
	{"612", "02", Rest};
%% Orange
plmn("61203" ++ Rest) ->
	{"612", "03", Rest};
%% MTN COTE D'IVOIRE
plmn("61205" ++ Rest) ->
	{"612", "05", Rest};
%% TOGOCOM
plmn("61501" ++ Rest) ->
	{"615", "01", Rest};
%% my.t
plmn("61701" ++ Rest) ->
	{"617", "01", Rest};
%% MTML
plmn("61703" ++ Rest) ->
	{"617", "03", Rest};
%% EMTEL
plmn("61710" ++ Rest) ->
	{"617", "10", Rest};
%% Orange Liberia
plmn("61807" ++ Rest) ->
	{"618", "07", Rest};
%% Africell
plmn("61905" ++ Rest) ->
	{"619", "05", Rest};
%% QCell
plmn("61907" ++ Rest) ->
	{"619", "07", Rest};
%% AirtelTigo
plmn("62003" ++ Rest) ->
	{"620", "03", Rest};
%% AirtelTigo
plmn("62006" ++ Rest) ->
	{"620", "06", Rest};
%% Airtel
plmn("62120" ++ Rest) ->
	{"621", "20", Rest};
%% Glo Mobile
plmn("62150" ++ Rest) ->
	{"621", "50", Rest};
%% Orange
plmn("62402" ++ Rest) ->
	{"624", "02", Rest};
%% Cameroon
plmn("62403" ++ Rest) ->
	{"624", "03", Rest};
%% Cameroon Telecommunications
plmn("62405" ++ Rest) ->
	{"624", "05", Rest};
%% ALOU
plmn("62501" ++ Rest) ->
	{"625", "01", Rest};
%% Unitel T+
plmn("62502" ++ Rest) ->
	{"625", "02", Rest};
%% GETESA
plmn("62701" ++ Rest) ->
	{"627", "01", Rest};
%% LIBERTIS
plmn("62801" ++ Rest) ->
	{"628", "01", Rest};
%% Airtel
plmn("62803" ++ Rest) ->
	{"628", "03", Rest};
%% Airtel
plmn("62901" ++ Rest) ->
	{"629", "01", Rest};
%% Libertis Telecom
plmn("62910" ++ Rest) ->
	{"629", "10", Rest};
%% Orange RDC
plmn("63086" ++ Rest) ->
	{"630", "86", Rest};
%% Africell RDC
plmn("63090" ++ Rest) ->
	{"630", "90", Rest};
%% Orange Bissau
plmn("63203" ++ Rest) ->
	{"632", "03", Rest};
%% CABLE & WIRELESS
plmn("63301" ++ Rest) ->
	{"633", "01", Rest};
%% Airtel
plmn("63310" ++ Rest) ->
	{"633", "10", Rest};
%% Zain Sudan
plmn("63401" ++ Rest) ->
	{"634", "01", Rest};
%% MTN-RWA
plmn("63510" ++ Rest) ->
	{"635", "10", Rest};
%% Airtel Rwanda
plmn("63513" ++ Rest) ->
	{"635", "13", Rest};
%% ethio telecom
plmn("63601" ++ Rest) ->
	{"636", "01", Rest};
%% Telesom
plmn("63701" ++ Rest) ->
	{"637", "01", Rest};
%% Golis Telecom Somalia
plmn("63730" ++ Rest) ->
	{"637", "30", Rest};
%% Hormuud Telecom Inc.
plmn("63750" ++ Rest) ->
	{"637", "50", Rest};
%% AMTEL LTD
plmn("63790" ++ Rest) ->
	{"637", "90", Rest};
%% DJIBOUTI TELECOM
plmn("63801" ++ Rest) ->
	{"638", "01", Rest};
%% Airtel
plmn("63903" ++ Rest) ->
	{"639", "03", Rest};
%% FAIBA
plmn("63910" ++ Rest) ->
	{"639", "10", Rest};
%% Yas
plmn("64002" ++ Rest) ->
	{"640", "02", Rest};
%% Yas
plmn("64003" ++ Rest) ->
	{"640", "03", Rest};
%% Vodacom Tanzania Limited
plmn("64004" ++ Rest) ->
	{"640", "04", Rest};
%% Airtel
plmn("64005" ++ Rest) ->
	{"640", "05", Rest};
%% Lycamobile Uganda
plmn("64104" ++ Rest) ->
	{"641", "04", Rest};
%% MTN UGANDA
plmn("64110" ++ Rest) ->
	{"641", "10", Rest};
%% Uganda Telecom
plmn("64111" ++ Rest) ->
	{"641", "11", Rest};
%% mcel
plmn("64301" ++ Rest) ->
	{"643", "01", Rest};
%% Vodacom
plmn("64304" ++ Rest) ->
	{"643", "04", Rest};
%% Airtel
plmn("64501" ++ Rest) ->
	{"645", "01", Rest};
%% Airtel
plmn("64601" ++ Rest) ->
	{"646", "01", Rest};
%% Telma Mobile
plmn("64604" ++ Rest) ->
	{"646", "04", Rest};
%% Orange Reunion
plmn("64700" ++ Rest) ->
	{"647", "00", Rest};
%% TELCO OI
plmn("64702" ++ Rest) ->
	{"647", "02", Rest};
%% FREE MOBILE RE
plmn("64703" ++ Rest) ->
	{"647", "03", Rest};
%% ZEOP MOBILE
plmn("64704" ++ Rest) ->
	{"647", "04", Rest};
%% SRR
plmn("64710" ++ Rest) ->
	{"647", "10", Rest};
%% ECONET
plmn("64804" ++ Rest) ->
	{"648", "04", Rest};
%% TNM
plmn("65001" ++ Rest) ->
	{"650", "01", Rest};
%% Access
plmn("65002" ++ Rest) ->
	{"650", "02", Rest};
%% Airtel
plmn("65010" ++ Rest) ->
	{"650", "10", Rest};
%% Vodacom Lesotho
plmn("65101" ++ Rest) ->
	{"651", "01", Rest};
%% Econet Telecom Lesotho (Pty) Ltd
plmn("65102" ++ Rest) ->
	{"651", "02", Rest};
%% BTC
plmn("65204" ++ Rest) ->
	{"652", "04", Rest};
%% VodaCom
plmn("65501" ++ Rest) ->
	{"655", "01", Rest};
%% Telkom SA Ltd
plmn("65502" ++ Rest) ->
	{"655", "02", Rest};
%% Telkom SA Ltd
plmn("65505" ++ Rest) ->
	{"655", "05", Rest};
%% Cell C
plmn("65507" ++ Rest) ->
	{"655", "07", Rest};
%% MTN-SA
plmn("65510" ++ Rest) ->
	{"655", "10", Rest};
%% rain
plmn("65538" ++ Rest) ->
	{"655", "38", Rest};
%% EriTel
plmn("65701" ++ Rest) ->
	{"657", "01", Rest};
%% Sure South Atlantic Ltd (Ascension)
plmn("65801" ++ Rest) ->
	{"658", "01", Rest};
%% Belize Telecommunications
plmn("70267" ++ Rest) ->
	{"702", "67", Rest};
%% Speednet Communications Ltd
plmn("70269" ++ Rest) ->
	{"702", "69", Rest};
%% CLARO
plmn("70401" ++ Rest) ->
	{"704", "01", Rest};
%% TIGO GUATEMALA
plmn("70402" ++ Rest) ->
	{"704", "02", Rest};
%% CLARO
plmn("70403" ++ Rest) ->
	{"704", "03", Rest};
%% CLARO SLV
plmn("70601" ++ Rest) ->
	{"706", "01", Rest};
%% TELEMOVIL EL SALVADOR S.A. de C.V.
plmn("70603" ++ Rest) ->
	{"706", "03", Rest};
%% CLARO GSM
plmn("708001" ++ Rest) ->
	{"708", "001", Rest};
%% TIGO
plmn("70802" ++ Rest) ->
	{"708", "02", Rest};
%% Empresa Hondurena de Telecomunicaciones
plmn("708030" ++ Rest) ->
	{"708", "030", Rest};
%% CLARO NIC
plmn("71021" ++ Rest) ->
	{"710", "21", Rest};
%% Tigo
plmn("710300" ++ Rest) ->
	{"710", "300", Rest};
%% I.C.E.
plmn("71201" ++ Rest) ->
	{"712", "01", Rest};
%% I.C.E.
plmn("71202" ++ Rest) ->
	{"712", "02", Rest};
%% Claro CR
plmn("71203" ++ Rest) ->
	{"712", "03", Rest};
%% LIBERTY
plmn("71204" ++ Rest) ->
	{"712", "04", Rest};
%% CABLE AND WIRELESS PANAMA
plmn("71401" ++ Rest) ->
	{"714", "01", Rest};
%% Tigo
plmn("714020" ++ Rest) ->
	{"714", "020", Rest};
%% Movistar Peru
plmn("71606" ++ Rest) ->
	{"716", "06", Rest};
%% CLARO PER
plmn("71610" ++ Rest) ->
	{"716", "10", Rest};
%% Viettel Peru S.A.C.
plmn("71615" ++ Rest) ->
	{"716", "15", Rest};
%% Movistar
plmn("722010" ++ Rest) ->
	{"722", "010", Rest};
%% Movistar
plmn("72207" ++ Rest) ->
	{"722", "07", Rest};
%% Claro Argentina
plmn("722310" ++ Rest) ->
	{"722", "310", Rest};
%% Personal
plmn("72234" ++ Rest) ->
	{"722", "34", Rest};
%% Personal
plmn("72236" ++ Rest) ->
	{"722", "36", Rest};
%% Claro
plmn("72400" ++ Rest) ->
	{"724", "00", Rest};
%% TIM BRASIL
plmn("72402" ++ Rest) ->
	{"724", "02", Rest};
%% TIM BRASIL
plmn("72403" ++ Rest) ->
	{"724", "03", Rest};
%% TIM BRASIL
plmn("72404" ++ Rest) ->
	{"724", "04", Rest};
%% Claro
plmn("72405" ++ Rest) ->
	{"724", "05", Rest};
%% Vivo
plmn("72406" ++ Rest) ->
	{"724", "06", Rest};
%% Vivo
plmn("72410" ++ Rest) ->
	{"724", "10", Rest};
%% Vivo
plmn("72411" ++ Rest) ->
	{"724", "11", Rest};
%% Vivo
plmn("72423" ++ Rest) ->
	{"724", "23", Rest};
%% UNIFIQUE
plmn("72429" ++ Rest) ->
	{"724", "29", Rest};
%% Algar Telecom
plmn("72432" ++ Rest) ->
	{"724", "32", Rest};
%% Algar Telecom
plmn("72433" ++ Rest) ->
	{"724", "33", Rest};
%% Algar Telecom
plmn("72434" ++ Rest) ->
	{"724", "34", Rest};
%% Airnity Brazil
plmn("72472" ++ Rest) ->
	{"724", "72", Rest};
%% ENTEL PCS
plmn("73001" ++ Rest) ->
	{"730", "01", Rest};
%% Telefonica Movil de Chile
plmn("73002" ++ Rest) ->
	{"730", "02", Rest};
%% Claro Chile
plmn("73003" ++ Rest) ->
	{"730", "03", Rest};
%% Telefonica Movil de Chile
plmn("73007" ++ Rest) ->
	{"730", "07", Rest};
%% Claro Chile
plmn("73008" ++ Rest) ->
	{"730", "08", Rest};
%% ENTEL PCS
plmn("73010" ++ Rest) ->
	{"730", "10", Rest};
%% Claro
plmn("732101" ++ Rest) ->
	{"732", "101", Rest};
%% TIGO COLOMBIA
plmn("732103" ++ Rest) ->
	{"732", "103", Rest};
%% TIGO COLOMBIA
plmn("732111" ++ Rest) ->
	{"732", "111", Rest};
%% Movistar
plmn("732123" ++ Rest) ->
	{"732", "123", Rest};
%% DIGITEL
plmn("73402" ++ Rest) ->
	{"734", "02", Rest};
%% Movistar
plmn("73404" ++ Rest) ->
	{"734", "04", Rest};
%% Nuevatel PCS De Bolivia
plmn("73601" ++ Rest) ->
	{"736", "01", Rest};
%% Entel
plmn("73602" ++ Rest) ->
	{"736", "02", Rest};
%% One Communication
plmn("738002" ++ Rest) ->
	{"738", "002", Rest};
%% ENet
plmn("738040" ++ Rest) ->
	{"738", "040", Rest};
%% MOVISTAR
plmn("74000" ++ Rest) ->
	{"740", "00", Rest};
%% Claro Ecuador
plmn("74001" ++ Rest) ->
	{"740", "01", Rest};
%% CNT
plmn("74002" ++ Rest) ->
	{"740", "02", Rest};
%% Claro Paraguay
plmn("74402" ++ Rest) ->
	{"744", "02", Rest};
%% TIGO Paraguay
plmn("74404" ++ Rest) ->
	{"744", "04", Rest};
%% Personal
plmn("74405" ++ Rest) ->
	{"744", "05", Rest};
%% TELESUR
plmn("74602" ++ Rest) ->
	{"746", "02", Rest};
%% Digicel Suriname NV
plmn("74603" ++ Rest) ->
	{"746", "03", Rest};
%% Antel
plmn("74801" ++ Rest) ->
	{"748", "01", Rest};
%% MOVISTAR
plmn("74807" ++ Rest) ->
	{"748", "07", Rest};
%% CLARO URUGUAY
plmn("74810" ++ Rest) ->
	{"748", "10", Rest};
%% Sure South Atlantic Limited
plmn("750001" ++ Rest) ->
	{"750", "001", Rest};
%% Iridium
plmn("90103" ++ Rest) ->
	{"901", "03", Rest};
%% Inmarsat Global
plmn("90111" ++ Rest) ->
	{"901", "11", Rest};
%% Telenor Maritime
plmn("90112" ++ Rest) ->
	{"901", "12", Rest};
%% AeroMobile
plmn("90114" ++ Rest) ->
	{"901", "14", Rest};
%% epic maritime
plmn("90119" ++ Rest) ->
	{"901", "19", Rest};
%% TIM
plmn("90126" ++ Rest) ->
	{"901", "26", Rest};
%% OQ Technology
plmn("90130" ++ Rest) ->
	{"901", "30", Rest};
%% Telekom Deutschland
plmn("90140" ++ Rest) ->
	{"901", "40", Rest};
%% AT&T Mexico
plmn("90144" ++ Rest) ->
	{"901", "44", Rest};
%% AIS
plmn("90145" ++ Rest) ->
	{"901", "45", Rest};
%% Telecom26
plmn("90146" ++ Rest) ->
	{"901", "46", Rest};
%% Zain Group
plmn("90149" ++ Rest) ->
	{"901", "49", Rest};
%% EchoStar Mobile Ltd
plmn("90150" ++ Rest) ->
	{"901", "50", Rest};
%% Tampnet
plmn("90171" ++ Rest) ->
	{"901", "71", Rest};
%% World Mobile Netwroks, LLC
plmn("90191" ++ Rest) ->
	{"901", "91", Rest};
%% Skylo
plmn("90198" ++ Rest) ->
	{"901", "98", Rest}.

-spec authenticate_client(TransportRef, Capabilities) -> Result
	when
		TransportRef :: diameter:transport_ref(),
		Capabilities :: #diameter_caps{},
		Result :: ok | unknown.
%% Authorize a diameter client 
%% @private
authenticate_client(_TransportRef, #diameter_caps{host_ip_address = {_, HostIpAddresses}}) ->
	authenticate_client(HostIpAddresses).
%% @hidden
authenticate_client([H | T]) ->
	case ocs:find_client(H) of
		{ok, #client{protocol = diameter}} ->
			ok;
		{error, not_found} ->
			authenticate_client(T)
	end;
authenticate_client([]) ->
	unknown.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

