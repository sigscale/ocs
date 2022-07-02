%%% ocs_diameter.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

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
%% Test network
plmn("00101" ++ Rest) ->
	{"001", "01", Rest};
%% Test network
plmn("001001" ++ Rest) ->
	{"001", "001", Rest};
%% Abkhazia, A-Mobile
plmn("28968" ++ Rest) ->
	{"289", "68", Rest};
%% Abkhazia, Aquafon
plmn("28967" ++ Rest) ->
	{"289", "67", Rest};
%% Afghanistan, Afghan Telecom Corp. (AT)
plmn("41288" ++ Rest) ->
	{"412", "88", Rest};
%% Afghanistan, Afghan Telecom Corp. (AT)
plmn("41280" ++ Rest) ->
	{"412", "80", Rest};
%% Afghanistan, Afghan Wireless/AWCC
plmn("41201" ++ Rest) ->
	{"412", "01", Rest};
%% Afghanistan, Areeba/MTN
plmn("41240" ++ Rest) ->
	{"412", "40", Rest};
%% Afghanistan, Etisalat
plmn("41230" ++ Rest) ->
	{"412", "30", Rest};
%% Afghanistan, Etisalat
plmn("41250" ++ Rest) ->
	{"412", "50", Rest};
%% Afghanistan, Roshan/TDCA
plmn("41220" ++ Rest) ->
	{"412", "20", Rest};
%% Afghanistan, WaselTelecom (WT)
plmn("41203" ++ Rest) ->
	{"412", "03", Rest};
%% Albania, AMC/Cosmote
plmn("27601" ++ Rest) ->
	{"276", "01", Rest};
%% Albania, Eagle Mobile
plmn("27603" ++ Rest) ->
	{"276", "03", Rest};
%% Albania, PLUS Communication Sh.a
plmn("27604" ++ Rest) ->
	{"276", "04", Rest};
%% Albania, Vodafone
plmn("27602" ++ Rest) ->
	{"276", "02", Rest};
%% Algeria, ATM Mobils
plmn("60301" ++ Rest) ->
	{"603", "01", Rest};
%% Algeria, Orascom / DJEZZY
plmn("60302" ++ Rest) ->
	{"603", "02", Rest};
%% Algeria, Oreedo/Wataniya / Nedjma
plmn("60303" ++ Rest) ->
	{"603", "03", Rest};
%% American Samoa, Blue Sky Communications
plmn("54411" ++ Rest) ->
	{"544", "11", Rest};
%% Andorra, Mobiland
plmn("21303" ++ Rest) ->
	{"213", "03", Rest};
%% Angola, MoviCel
plmn("63104" ++ Rest) ->
	{"631", "04", Rest};
%% Angola, Unitel
plmn("63102" ++ Rest) ->
	{"631", "02", Rest};
%% Anguilla, Cable and Wireless
plmn("365840" ++ Rest) ->
	{"365", "840", Rest};
%% Anguilla, Digicell / Wireless Vent. Ltd
plmn("36510" ++ Rest) ->
	{"365", "10", Rest};
%% Antigua and Barbuda, APUA PCS
plmn("34430" ++ Rest) ->
	{"344", "30", Rest};
%% Antigua and Barbuda, C & W
plmn("344920" ++ Rest) ->
	{"344", "920", Rest};
%% Antigua and Barbuda, DigiCel/Cing. Wireless
plmn("344930" ++ Rest) ->
	{"344", "930", Rest};
%% Argentina Republic, Claro/ CTI/AMX
plmn("722310" ++ Rest) ->
	{"722", "310", Rest};
%% Argentina Republic, Claro/ CTI/AMX
plmn("722330" ++ Rest) ->
	{"722", "330", Rest};
%% Argentina Republic, Claro/ CTI/AMX
plmn("722320" ++ Rest) ->
	{"722", "320", Rest};
%% Argentina Republic, Compania De Radiocomunicaciones Moviles SA
plmn("72210" ++ Rest) ->
	{"722", "10", Rest};
%% Argentina Republic, Movistar/Telefonica
plmn("72207" ++ Rest) ->
	{"722", "07", Rest};
%% Argentina Republic, Movistar/Telefonica
plmn("72270" ++ Rest) ->
	{"722", "70", Rest};
%% Argentina Republic, Nextel
plmn("72220" ++ Rest) ->
	{"722", "20", Rest};
%% Argentina Republic, Telecom Personal S.A.
plmn("722341" ++ Rest) ->
	{"722", "341", Rest};
%% Argentina Republic, Telecom Personal S.A.
plmn("722340" ++ Rest) ->
	{"722", "340", Rest};
%% Armenia, ArmenTel/Beeline
plmn("28301" ++ Rest) ->
	{"283", "01", Rest};
%% Armenia, Karabakh Telecom
plmn("28304" ++ Rest) ->
	{"283", "04", Rest};
%% Armenia, Orange
plmn("28310" ++ Rest) ->
	{"283", "10", Rest};
%% Armenia, Vivacell
plmn("28305" ++ Rest) ->
	{"283", "05", Rest};
%% Aruba, Digicel
plmn("36320" ++ Rest) ->
	{"363", "20", Rest};
%% Aruba, Digicel
plmn("36302" ++ Rest) ->
	{"363", "02", Rest};
%% Aruba, Setar GSM
plmn("36301" ++ Rest) ->
	{"363", "01", Rest};
%% Australia, AAPT Ltd.
plmn("50514" ++ Rest) ->
	{"505", "14", Rest};
%% Australia, Advanced Comm Tech Pty.
plmn("50524" ++ Rest) ->
	{"505", "24", Rest};
%% Australia, Airnet Commercial Australia Ltd..
plmn("50509" ++ Rest) ->
	{"505", "09", Rest};
%% Australia, Department of Defense
plmn("50504" ++ Rest) ->
	{"505", "04", Rest};
%% Australia, Dialogue Communications Pty Ltd
plmn("50526" ++ Rest) ->
	{"505", "26", Rest};
%% Australia, H3G Ltd.
plmn("50512" ++ Rest) ->
	{"505", "12", Rest};
%% Australia, H3G Ltd.
plmn("50506" ++ Rest) ->
	{"505", "06", Rest};
%% Australia, Pivotel Group Ltd
plmn("50588" ++ Rest) ->
	{"505", "88", Rest};
%% Australia, Lycamobile Pty Ltd
plmn("50519" ++ Rest) ->
	{"505", "19", Rest};
%% Australia, Railcorp/Vodafone
plmn("50508" ++ Rest) ->
	{"505", "08", Rest};
%% Australia, Railcorp/Vodafone
plmn("50599" ++ Rest) ->
	{"505", "99", Rest};
%% Australia, Railcorp/Vodafone
plmn("50513" ++ Rest) ->
	{"505", "13", Rest};
%% Australia, Singtel Optus
plmn("50590" ++ Rest) ->
	{"505", "90", Rest};
%% Australia, Singtel Optus
plmn("50502" ++ Rest) ->
	{"505", "02", Rest};
%% Australia, Telstra Corp. Ltd.
plmn("50572" ++ Rest) ->
	{"505", "72", Rest};
%% Australia, Telstra Corp. Ltd.
plmn("50571" ++ Rest) ->
	{"505", "71", Rest};
%% Australia, Telstra Corp. Ltd.
plmn("50501" ++ Rest) ->
	{"505", "01", Rest};
%% Australia, Telstra Corp. Ltd.
plmn("50511" ++ Rest) ->
	{"505", "11", Rest};
%% Australia, The Ozitel Network Pty.
plmn("50505" ++ Rest) ->
	{"505", "05", Rest};
%% Australia, Victorian Rail Track Corp. (VicTrack)
plmn("50516" ++ Rest) ->
	{"505", "16", Rest};
%% Australia, Vodafone
plmn("50507" ++ Rest) ->
	{"505", "07", Rest};
%% Australia, Vodafone
plmn("50503" ++ Rest) ->
	{"505", "03", Rest};
%% Austria, A1 MobilKom
plmn("23202" ++ Rest) ->
	{"232", "02", Rest};
%% Austria, A1 MobilKom
plmn("23211" ++ Rest) ->
	{"232", "11", Rest};
%% Austria, A1 MobilKom
plmn("23209" ++ Rest) ->
	{"232", "09", Rest};
%% Austria, A1 MobilKom
plmn("23201" ++ Rest) ->
	{"232", "01", Rest};
%% Austria, T-Mobile/Telering
plmn("23215" ++ Rest) ->
	{"232", "15", Rest};
%% Austria, Fix Line
plmn("23200" ++ Rest) ->
	{"232", "00", Rest};
%% Austria, H3G
plmn("23210" ++ Rest) ->
	{"232", "10", Rest};
%% Austria, H3G
plmn("23214" ++ Rest) ->
	{"232", "14", Rest};
%% Austria, Mtel
plmn("23220" ++ Rest) ->
	{"232", "20", Rest};
%% Austria, 3/Orange/One Connect
plmn("23206" ++ Rest) ->
	{"232", "06", Rest};
%% Austria, 3/Orange/One Connect
plmn("23205" ++ Rest) ->
	{"232", "05", Rest};
%% Austria, 3/Orange/One Connect
plmn("23212" ++ Rest) ->
	{"232", "12", Rest};
%% Austria, Spusu/Mass Response
plmn("23217" ++ Rest) ->
	{"232", "17", Rest};
%% Austria, T-Mobile/Telering
plmn("23207" ++ Rest) ->
	{"232", "07", Rest};
%% Austria, T-Mobile/Telering
plmn("23204" ++ Rest) ->
	{"232", "04", Rest};
%% Austria, T-Mobile/Telering
plmn("23203" ++ Rest) ->
	{"232", "03", Rest};
%% Austria, T-Mobile/Telering
plmn("23213" ++ Rest) ->
	{"232", "13", Rest};
%% Austria, Tele2
plmn("23219" ++ Rest) ->
	{"232", "19", Rest};
%% Austria, A1 MobilKom
plmn("23208" ++ Rest) ->
	{"232", "08", Rest};
%% Azerbaijan, Azercell Telekom B.M.
plmn("40001" ++ Rest) ->
	{"400", "01", Rest};
%% Azerbaijan, Azerfon.
plmn("40004" ++ Rest) ->
	{"400", "04", Rest};
%% Azerbaijan, CATEL
plmn("40003" ++ Rest) ->
	{"400", "03", Rest};
%% Azerbaijan, J.V. Bakcell GSM 2000
plmn("40002" ++ Rest) ->
	{"400", "02", Rest};
%% Azerbaijan, Naxtel
plmn("40006" ++ Rest) ->
	{"400", "06", Rest};
%% Bahamas, Aliv/Cable Bahamas
plmn("364490" ++ Rest) ->
	{"364", "490", Rest};
%% Bahamas, Bahamas Telco. Comp.
plmn("364390" ++ Rest) ->
	{"364", "390", Rest};
%% Bahamas, Bahamas Telco. Comp.
plmn("36430" ++ Rest) ->
	{"364", "30", Rest};
%% Bahamas, Bahamas Telco. Comp.
plmn("36439" ++ Rest) ->
	{"364", "39", Rest};
%% Bahamas, Smart Communications
plmn("36403" ++ Rest) ->
	{"364", "03", Rest};
%% Bahrain, Batelco
plmn("42601" ++ Rest) ->
	{"426", "01", Rest};
%% Bahrain, ZAIN/Vodafone
plmn("42602" ++ Rest) ->
	{"426", "02", Rest};
%% Bahrain, VIVA
plmn("42604" ++ Rest) ->
	{"426", "04", Rest};
%% Bangladesh, Robi/Aktel
plmn("47002" ++ Rest) ->
	{"470", "02", Rest};
%% Bangladesh, Citycell
plmn("47006" ++ Rest) ->
	{"470", "06", Rest};
%% Bangladesh, Citycell
plmn("47005" ++ Rest) ->
	{"470", "05", Rest};
%% Bangladesh, GrameenPhone
plmn("47001" ++ Rest) ->
	{"470", "01", Rest};
%% Bangladesh, Orascom/Banglalink
plmn("47003" ++ Rest) ->
	{"470", "03", Rest};
%% Bangladesh, TeleTalk
plmn("47004" ++ Rest) ->
	{"470", "04", Rest};
%% Bangladesh, Airtel/Warid
plmn("47007" ++ Rest) ->
	{"470", "07", Rest};
%% Barbados, LIME
plmn("342600" ++ Rest) ->
	{"342", "600", Rest};
%% Barbados, Cingular Wireless
plmn("342810" ++ Rest) ->
	{"342", "810", Rest};
%% Barbados, Digicel
plmn("342750" ++ Rest) ->
	{"342", "750", Rest};
%% Barbados, Digicel
plmn("34250" ++ Rest) ->
	{"342", "50", Rest};
%% Barbados, Sunbeach
plmn("342820" ++ Rest) ->
	{"342", "820", Rest};
%% Belarus, BelCel JV
plmn("25703" ++ Rest) ->
	{"257", "03", Rest};
%% Belarus, BeST
plmn("25704" ++ Rest) ->
	{"257", "04", Rest};
%% Belarus, MDC/Velcom
plmn("25701" ++ Rest) ->
	{"257", "01", Rest};
%% Belarus, MTS
plmn("25702" ++ Rest) ->
	{"257", "02", Rest};
%% Belgium, Base/KPN
plmn("20620" ++ Rest) ->
	{"206", "20", Rest};
%% Belgium, Belgacom/Proximus
plmn("20601" ++ Rest) ->
	{"206", "01", Rest};
%% Belgium, Lycamobile Belgium
plmn("20606" ++ Rest) ->
	{"206", "06", Rest};
%% Belgium, Mobistar/Orange
plmn("20610" ++ Rest) ->
	{"206", "10", Rest};
%% Belgium, SNCT/NMBS
plmn("20602" ++ Rest) ->
	{"206", "02", Rest};
%% Belgium, Telenet NV
plmn("20605" ++ Rest) ->
	{"206", "05", Rest};
%% Belgium, VOO
plmn("20608" ++ Rest) ->
	{"206", "08", Rest};
%% Belize, DigiCell
plmn("70267" ++ Rest) ->
	{"702", "67", Rest};
%% Belize, International Telco (INTELCO)
plmn("70268" ++ Rest) ->
	{"702", "68", Rest};
%% Benin, Bell Benin/BBCOM
plmn("61604" ++ Rest) ->
	{"616", "04", Rest};
%% Benin, Etisalat/MOOV
plmn("61602" ++ Rest) ->
	{"616", "02", Rest};
%% Benin, GloMobile
plmn("61605" ++ Rest) ->
	{"616", "05", Rest};
%% Benin, Libercom
plmn("61601" ++ Rest) ->
	{"616", "01", Rest};
%% Benin, MTN/Spacetel
plmn("61603" ++ Rest) ->
	{"616", "03", Rest};
%% Bermuda, Bermuda Digital Communications Ltd (BDC)
plmn("35000" ++ Rest) ->
	{"350", "00", Rest};
%% Bermuda, CellOne Ltd
plmn("35099" ++ Rest) ->
	{"350", "99", Rest};
%% Bermuda, DigiCel / Cingular
plmn("35010" ++ Rest) ->
	{"350", "10", Rest};
%% Bermuda, M3 Wireless Ltd
plmn("35002" ++ Rest) ->
	{"350", "02", Rest};
%% Bermuda, Telecommunications (Bermuda & West Indies) Ltd (Digicel Bermuda)
plmn("35001" ++ Rest) ->
	{"350", "01", Rest};
%% Bhutan, B-Mobile
plmn("40211" ++ Rest) ->
	{"402", "11", Rest};
%% Bhutan, Bhutan Telecom Ltd (BTL)
plmn("40217" ++ Rest) ->
	{"402", "17", Rest};
%% Bhutan, TashiCell
plmn("40277" ++ Rest) ->
	{"402", "77", Rest};
%% Bolivia, Entel Pcs
plmn("73602" ++ Rest) ->
	{"736", "02", Rest};
%% Bolivia, Viva/Nuevatel
plmn("73601" ++ Rest) ->
	{"736", "01", Rest};
%% Bolivia, Tigo
plmn("73603" ++ Rest) ->
	{"736", "03", Rest};
%% Bosnia & Herzegov., BH Mobile
plmn("21890" ++ Rest) ->
	{"218", "90", Rest};
%% Bosnia & Herzegov., Eronet Mobile
plmn("21803" ++ Rest) ->
	{"218", "03", Rest};
%% Bosnia & Herzegov., M-Tel
plmn("21805" ++ Rest) ->
	{"218", "05", Rest};
%% Botswana, BeMOBILE
plmn("65204" ++ Rest) ->
	{"652", "04", Rest};
%% Botswana, Mascom Wireless (Pty) Ltd.
plmn("65201" ++ Rest) ->
	{"652", "01", Rest};
%% Botswana, Orange
plmn("65202" ++ Rest) ->
	{"652", "02", Rest};
%% Brazil, AmericaNet
plmn("72426" ++ Rest) ->
	{"724", "26", Rest};
%% Brazil, Claro/Albra/America Movil
plmn("72412" ++ Rest) ->
	{"724", "12", Rest};
%% Brazil, Claro/Albra/America Movil
plmn("72438" ++ Rest) ->
	{"724", "38", Rest};
%% Brazil, Claro/Albra/America Movil
plmn("72405" ++ Rest) ->
	{"724", "05", Rest};
%% Brazil, Vivo S.A./Telemig
plmn("72401" ++ Rest) ->
	{"724", "01", Rest};
%% Brazil, CTBC Celular SA (CTBC)
plmn("72433" ++ Rest) ->
	{"724", "33", Rest};
%% Brazil, CTBC Celular SA (CTBC)
plmn("72432" ++ Rest) ->
	{"724", "32", Rest};
%% Brazil, CTBC Celular SA (CTBC)
plmn("72434" ++ Rest) ->
	{"724", "34", Rest};
%% Brazil, TIM
plmn("72408" ++ Rest) ->
	{"724", "08", Rest};
%% Brazil, Nextel (Telet)
plmn("72439" ++ Rest) ->
	{"724", "39", Rest};
%% Brazil, Nextel (Telet)
plmn("72400" ++ Rest) ->
	{"724", "00", Rest};
%% Brazil, Oi (TNL PCS / Oi)
plmn("72430" ++ Rest) ->
	{"724", "30", Rest};
%% Brazil, Oi (TNL PCS / Oi)
plmn("72431" ++ Rest) ->
	{"724", "31", Rest};
%% Brazil, Brazil Telcom
plmn("72416" ++ Rest) ->
	{"724", "16", Rest};
%% Brazil, Amazonia Celular S/A
plmn("72424" ++ Rest) ->
	{"724", "24", Rest};
%% Brazil, PORTO SEGURO TELECOMUNICACOES
plmn("72454" ++ Rest) ->
	{"724", "54", Rest};
%% Brazil, Sercontel Cel
plmn("72415" ++ Rest) ->
	{"724", "15", Rest};
%% Brazil, CTBC/Triangulo
plmn("72407" ++ Rest) ->
	{"724", "07", Rest};
%% Brazil, Vivo S.A./Telemig
plmn("72419" ++ Rest) ->
	{"724", "19", Rest};
%% Brazil, TIM
plmn("72403" ++ Rest) ->
	{"724", "03", Rest};
%% Brazil, TIM
plmn("72402" ++ Rest) ->
	{"724", "02", Rest};
%% Brazil, TIM
plmn("72404" ++ Rest) ->
	{"724", "04", Rest};
%% Brazil, Unicel do Brasil Telecomunicacoes Ltda
plmn("72437" ++ Rest) ->
	{"724", "37", Rest};
%% Brazil, Vivo S.A./Telemig
plmn("72423" ++ Rest) ->
	{"724", "23", Rest};
%% Brazil, Vivo S.A./Telemig
plmn("72411" ++ Rest) ->
	{"724", "11", Rest};
%% Brazil, Vivo S.A./Telemig
plmn("72410" ++ Rest) ->
	{"724", "10", Rest};
%% Brazil, Vivo S.A./Telemig
plmn("72406" ++ Rest) ->
	{"724", "06", Rest};
%% British Virgin Islands, Caribbean Cellular
plmn("348570" ++ Rest) ->
	{"348", "570", Rest};
%% British Virgin Islands, Digicel
plmn("348770" ++ Rest) ->
	{"348", "770", Rest};
%% British Virgin Islands, LIME
plmn("348170" ++ Rest) ->
	{"348", "170", Rest};
%% Brunei Darussalam, b-mobile
plmn("52802" ++ Rest) ->
	{"528", "02", Rest};
%% Brunei Darussalam, Datastream (DTSCom)
plmn("52811" ++ Rest) ->
	{"528", "11", Rest};
%% Brunei Darussalam, Telekom Brunei Bhd (TelBru)
plmn("52801" ++ Rest) ->
	{"528", "01", Rest};
%% Bulgaria, BTC Mobile EOOD (vivatel)
plmn("28406" ++ Rest) ->
	{"284", "06", Rest};
%% Bulgaria, BTC Mobile EOOD (vivatel)
plmn("28403" ++ Rest) ->
	{"284", "03", Rest};
%% Bulgaria, Telenor/Cosmo/Globul
plmn("28405" ++ Rest) ->
	{"284", "05", Rest};
%% Bulgaria, MobilTel AD
plmn("28401" ++ Rest) ->
	{"284", "01", Rest};
%% Burkina Faso, TeleCel
plmn("61303" ++ Rest) ->
	{"613", "03", Rest};
%% Burkina Faso, TeleMob-OnaTel
plmn("61301" ++ Rest) ->
	{"613", "01", Rest};
%% Burkina Faso, Orange/Airtel
plmn("61302" ++ Rest) ->
	{"613", "02", Rest};
%% Burundi, Africel / Safaris
plmn("64202" ++ Rest) ->
	{"642", "02", Rest};
%% Burundi, Lumitel/Viettel
plmn("64208" ++ Rest) ->
	{"642", "08", Rest};
%% Burundi, Onatel / Telecel
plmn("64203" ++ Rest) ->
	{"642", "03", Rest};
%% Burundi, Smart Mobile / LACELL
plmn("64207" ++ Rest) ->
	{"642", "07", Rest};
%% Burundi, Spacetel / Econet / Leo
plmn("64282" ++ Rest) ->
	{"642", "82", Rest};
%% Burundi, Spacetel / Econet / Leo
plmn("64201" ++ Rest) ->
	{"642", "01", Rest};
%% Cambodia, Cambodia Advance Communications Co. Ltd (CADCOMMS)
plmn("45604" ++ Rest) ->
	{"456", "04", Rest};
%% Cambodia, Smart Mobile
plmn("45602" ++ Rest) ->
	{"456", "02", Rest};
%% Cambodia, Viettel/Metfone
plmn("45608" ++ Rest) ->
	{"456", "08", Rest};
%% Cambodia, Mobitel/Cam GSM
plmn("45618" ++ Rest) ->
	{"456", "18", Rest};
%% Cambodia, Mobitel/Cam GSM
plmn("45601" ++ Rest) ->
	{"456", "01", Rest};
%% Cambodia, QB/Cambodia Adv. Comms.
plmn("45603" ++ Rest) ->
	{"456", "03", Rest};
%% Cambodia, SEATEL
plmn("45611" ++ Rest) ->
	{"456", "11", Rest};
%% Cambodia, Smart Mobile
plmn("45605" ++ Rest) ->
	{"456", "05", Rest};
%% Cambodia, Smart Mobile
plmn("45606" ++ Rest) ->
	{"456", "06", Rest};
%% Cambodia, Sotelco/Beeline
plmn("45609" ++ Rest) ->
	{"456", "09", Rest};
%% Cameroon, MTN
plmn("62401" ++ Rest) ->
	{"624", "01", Rest};
%% Cameroon, Nextel
plmn("62404" ++ Rest) ->
	{"624", "04", Rest};
%% Cameroon, Orange
plmn("62402" ++ Rest) ->
	{"624", "02", Rest};
%% Canada, BC Tel Mobility
plmn("302652" ++ Rest) ->
	{"302", "652", Rest};
%% Canada, Bell Aliant
plmn("302630" ++ Rest) ->
	{"302", "630", Rest};
%% Canada, Bell Mobility
plmn("302651" ++ Rest) ->
	{"302", "651", Rest};
%% Canada, Bell Mobility
plmn("302610" ++ Rest) ->
	{"302", "610", Rest};
%% Canada, CityWest Mobility
plmn("302670" ++ Rest) ->
	{"302", "670", Rest};
%% Canada, Clearnet
plmn("302361" ++ Rest) ->
	{"302", "361", Rest};
%% Canada, Clearnet
plmn("302360" ++ Rest) ->
	{"302", "360", Rest};
%% Canada, DMTS Mobility
plmn("302380" ++ Rest) ->
	{"302", "380", Rest};
%% Canada, Globalstar Canada
plmn("302710" ++ Rest) ->
	{"302", "710", Rest};
%% Canada, Latitude Wireless
plmn("302640" ++ Rest) ->
	{"302", "640", Rest};
%% Canada, FIDO (Rogers AT&T/ Microcell)
plmn("302370" ++ Rest) ->
	{"302", "370", Rest};
%% Canada, mobilicity
plmn("302320" ++ Rest) ->
	{"302", "320", Rest};
%% Canada, MT&T Mobility
plmn("302702" ++ Rest) ->
	{"302", "702", Rest};
%% Canada, MTS Mobility
plmn("302655" ++ Rest) ->
	{"302", "655", Rest};
%% Canada, MTS Mobility
plmn("302660" ++ Rest) ->
	{"302", "660", Rest};
%% Canada, NB Tel Mobility
plmn("302701" ++ Rest) ->
	{"302", "701", Rest};
%% Canada, New Tel Mobility
plmn("302703" ++ Rest) ->
	{"302", "703", Rest};
%% Canada, Public Mobile
plmn("302760" ++ Rest) ->
	{"302", "760", Rest};
%% Canada, Quebectel Mobility
plmn("302657" ++ Rest) ->
	{"302", "657", Rest};
%% Canada, Rogers AT&T Wireless
plmn("302720" ++ Rest) ->
	{"302", "720", Rest};
%% Canada, Sask Tel Mobility
plmn("302654" ++ Rest) ->
	{"302", "654", Rest};
%% Canada, Sask Tel Mobility
plmn("302780" ++ Rest) ->
	{"302", "780", Rest};
%% Canada, Sask Tel Mobility
plmn("302680" ++ Rest) ->
	{"302", "680", Rest};
%% Canada, Tbay Mobility
plmn("302656" ++ Rest) ->
	{"302", "656", Rest};
%% Canada, Telus Mobility
plmn("302653" ++ Rest) ->
	{"302", "653", Rest};
%% Canada, Telus Mobility
plmn("302220" ++ Rest) ->
	{"302", "220", Rest};
%% Canada, Videotron
plmn("302500" ++ Rest) ->
	{"302", "500", Rest};
%% Canada, WIND
plmn("302490" ++ Rest) ->
	{"302", "490", Rest};
%% Cape Verde, CV Movel
plmn("62501" ++ Rest) ->
	{"625", "01", Rest};
%% Cape Verde, T+ Telecom
plmn("62502" ++ Rest) ->
	{"625", "02", Rest};
%% Cayman Islands, Digicel Cayman Ltd
plmn("34650" ++ Rest) ->
	{"346", "50", Rest};
%% Cayman Islands, Digicel Ltd.
plmn("34606" ++ Rest) ->
	{"346", "06", Rest};
%% Cayman Islands, LIME / Cable & Wirel.
plmn("346140" ++ Rest) ->
	{"346", "140", Rest};
%% Central African Rep., Centrafr. Telecom+
plmn("62301" ++ Rest) ->
	{"623", "01", Rest};
%% Central African Rep., Nationlink
plmn("62304" ++ Rest) ->
	{"623", "04", Rest};
%% Central African Rep., Orange/Celca
plmn("62303" ++ Rest) ->
	{"623", "03", Rest};
%% Central African Rep., Telecel Centraf.
plmn("62302" ++ Rest) ->
	{"623", "02", Rest};
%% Chad, Salam/Sotel
plmn("62204" ++ Rest) ->
	{"622", "04", Rest};
%% Chad, Tchad Mobile
plmn("62202" ++ Rest) ->
	{"622", "02", Rest};
%% Chad, Tigo/Milicom/Tchad Mobile
plmn("62203" ++ Rest) ->
	{"622", "03", Rest};
%% Chad, Airtel/ZAIN/Celtel
plmn("62201" ++ Rest) ->
	{"622", "01", Rest};
%% Chile, Blue Two Chile SA
plmn("73006" ++ Rest) ->
	{"730", "06", Rest};
%% Chile, Celupago SA
plmn("73011" ++ Rest) ->
	{"730", "11", Rest};
%% Chile, Cibeles Telecom SA
plmn("73015" ++ Rest) ->
	{"730", "15", Rest};
%% Chile, Claro
plmn("73003" ++ Rest) ->
	{"730", "03", Rest};
%% Chile, Entel Telefonia
plmn("73010" ++ Rest) ->
	{"730", "10", Rest};
%% Chile, Entel Telefonia Mov
plmn("73001" ++ Rest) ->
	{"730", "01", Rest};
%% Chile, Netline Telefonica Movil Ltda
plmn("73014" ++ Rest) ->
	{"730", "14", Rest};
%% Chile, Nextel SA
plmn("73005" ++ Rest) ->
	{"730", "05", Rest};
%% Chile, Nextel SA
plmn("73004" ++ Rest) ->
	{"730", "04", Rest};
%% Chile, Nextel SA
plmn("73009" ++ Rest) ->
	{"730", "09", Rest};
%% Chile, Sociedad Falabella Movil SPA
plmn("73019" ++ Rest) ->
	{"730", "19", Rest};
%% Chile, TELEFONICA
plmn("73002" ++ Rest) ->
	{"730", "02", Rest};
%% Chile, TELEFONICA
plmn("73007" ++ Rest) ->
	{"730", "07", Rest};
%% Chile, Telestar Movil SA
plmn("73012" ++ Rest) ->
	{"730", "12", Rest};
%% Chile, TESAM SA
plmn("73000" ++ Rest) ->
	{"730", "00", Rest};
%% Chile, Tribe Mobile SPA
plmn("73013" ++ Rest) ->
	{"730", "13", Rest};
%% Chile, VTR Banda Ancha SA
plmn("73008" ++ Rest) ->
	{"730", "08", Rest};
%% China, China Mobile GSM
plmn("46000" ++ Rest) ->
	{"460", "00", Rest};
%% China, China Mobile GSM
plmn("46002" ++ Rest) ->
	{"460", "02", Rest};
%% China, China Mobile GSM
plmn("46007" ++ Rest) ->
	{"460", "07", Rest};
%% China, China Space Mobile Satellite Telecommunications Co. Ltd (China Spacecom)
plmn("46004" ++ Rest) ->
	{"460", "04", Rest};
%% China, China Telecom
plmn("46003" ++ Rest) ->
	{"460", "03", Rest};
%% China, China Telecom
plmn("46005" ++ Rest) ->
	{"460", "05", Rest};
%% China, China Unicom
plmn("46006" ++ Rest) ->
	{"460", "06", Rest};
%% China, China Unicom
plmn("46001" ++ Rest) ->
	{"460", "01", Rest};
%% Colombia, Avantel SAS
plmn("732130" ++ Rest) ->
	{"732", "130", Rest};
%% Colombia, Movistar
plmn("732102" ++ Rest) ->
	{"732", "102", Rest};
%% Colombia, TIGO/Colombia Movil
plmn("732103" ++ Rest) ->
	{"732", "103", Rest};
%% Colombia, TIGO/Colombia Movil
plmn("73201" ++ Rest) ->
	{"732", "01", Rest};
%% Colombia, Comcel S.A. Occel S.A./Celcaribe
plmn("732101" ++ Rest) ->
	{"732", "101", Rest};
%% Colombia, Edatel S.A.
plmn("73202" ++ Rest) ->
	{"732", "02", Rest};
%% Colombia, eTb
plmn("732187" ++ Rest) ->
	{"732", "187", Rest};
%% Colombia, Movistar
plmn("732123" ++ Rest) ->
	{"732", "123", Rest};
%% Colombia, TIGO/Colombia Movil
plmn("732111" ++ Rest) ->
	{"732", "111", Rest};
%% Colombia, UNE EPM Telecomunicaciones SA ESP
plmn("732142" ++ Rest) ->
	{"732", "142", Rest};
%% Colombia, UNE EPM Telecomunicaciones SA ESP
plmn("73220" ++ Rest) ->
	{"732", "20", Rest};
%% Colombia, Virgin Mobile Colombia SAS
plmn("732154" ++ Rest) ->
	{"732", "154", Rest};
%% Comoros, HURI - SNPT
plmn("65401" ++ Rest) ->
	{"654", "01", Rest};
%% Comoros, TELMA TELCO SA
plmn("65402" ++ Rest) ->
	{"654", "02", Rest};
%% "Congo,  Dem. Rep.",Africell
plmn("63090" ++ Rest) ->
	{"630", "90", Rest};
%% "Congo,  Dem. Rep.",Orange RDC sarl
plmn("63086" ++ Rest) ->
	{"630", "86", Rest};
%% "Congo,  Dem. Rep.",SuperCell
plmn("63005" ++ Rest) ->
	{"630", "05", Rest};
%% "Congo,  Dem. Rep.",TIGO/Oasis
plmn("63089" ++ Rest) ->
	{"630", "89", Rest};
%% "Congo,  Dem. Rep.",Vodacom
plmn("63001" ++ Rest) ->
	{"630", "01", Rest};
%% "Congo,  Dem. Rep.",Yozma Timeturns sprl (YTT)
plmn("63088" ++ Rest) ->
	{"630", "88", Rest};
%% "Congo,  Dem. Rep.",Airtel/ZAIN
plmn("63002" ++ Rest) ->
	{"630", "02", Rest};
%% "Congo,  Republic",Airtel SA
plmn("62901" ++ Rest) ->
	{"629", "01", Rest};
%% "Congo,  Republic",Azur SA (ETC)
plmn("62902" ++ Rest) ->
	{"629", "02", Rest};
%% "Congo,  Republic",MTN/Libertis
plmn("62910" ++ Rest) ->
	{"629", "10", Rest};
%% "Congo,  Republic",Warid
plmn("62907" ++ Rest) ->
	{"629", "07", Rest};
%% Cook Islands, Telecom Cook Islands
plmn("54801" ++ Rest) ->
	{"548", "01", Rest};
%% Costa Rica, Claro
plmn("71203" ++ Rest) ->
	{"712", "03", Rest};
%% Costa Rica, ICE
plmn("71201" ++ Rest) ->
	{"712", "01", Rest};
%% Costa Rica, ICE
plmn("71202" ++ Rest) ->
	{"712", "02", Rest};
%% Costa Rica, Movistar
plmn("71204" ++ Rest) ->
	{"712", "04", Rest};
%% Costa Rica, Virtualis
plmn("71220" ++ Rest) ->
	{"712", "20", Rest};
%% Croatia, T-Mobile/Cronet
plmn("21901" ++ Rest) ->
	{"219", "01", Rest};
%% Croatia, Tele2
plmn("21902" ++ Rest) ->
	{"219", "02", Rest};
%% Croatia, VIPnet d.o.o.
plmn("21910" ++ Rest) ->
	{"219", "10", Rest};
%% Cuba, CubaCel/C-COM
plmn("36801" ++ Rest) ->
	{"368", "01", Rest};
%% Curacao, Polycom N.V./ Digicel
plmn("36269" ++ Rest) ->
	{"362", "69", Rest};
%% Cyprus, MTN/Areeba
plmn("28010" ++ Rest) ->
	{"280", "10", Rest};
%% Cyprus, PrimeTel PLC
plmn("28020" ++ Rest) ->
	{"280", "20", Rest};
%% Cyprus, Vodafone/CyTa
plmn("28001" ++ Rest) ->
	{"280", "01", Rest};
%% Czech Rep., Compatel s.r.o.
plmn("23008" ++ Rest) ->
	{"230", "08", Rest};
%% Czech Rep., O2
plmn("23002" ++ Rest) ->
	{"230", "02", Rest};
%% Czech Rep., T-Mobile / RadioMobil
plmn("23001" ++ Rest) ->
	{"230", "01", Rest};
%% Czech Rep., Travel Telekommunikation s.r.o.
plmn("23005" ++ Rest) ->
	{"230", "05", Rest};
%% Czech Rep., Ufone
plmn("23004" ++ Rest) ->
	{"230", "04", Rest};
%% Czech Rep., Vodafone
plmn("23099" ++ Rest) ->
	{"230", "99", Rest};
%% Czech Rep., Vodafone
plmn("23003" ++ Rest) ->
	{"230", "03", Rest};
%% Denmark, ApS KBUS
plmn("23805" ++ Rest) ->
	{"238", "05", Rest};
%% Denmark, Banedanmark
plmn("23823" ++ Rest) ->
	{"238", "23", Rest};
%% Denmark, CoolTEL ApS
plmn("23828" ++ Rest) ->
	{"238", "28", Rest};
%% Denmark, H3G
plmn("23806" ++ Rest) ->
	{"238", "06", Rest};
%% Denmark, Lycamobile Ltd
plmn("23812" ++ Rest) ->
	{"238", "12", Rest};
%% Denmark, Mach Connectivity ApS
plmn("23803" ++ Rest) ->
	{"238", "03", Rest};
%% Denmark, Mundio Mobile
plmn("23807" ++ Rest) ->
	{"238", "07", Rest};
%% Denmark, NextGen Mobile Ltd (CardBoardFish)
plmn("23804" ++ Rest) ->
	{"238", "04", Rest};
%% Denmark, TDC Denmark
plmn("23801" ++ Rest) ->
	{"238", "01", Rest};
%% Denmark, TDC Denmark
plmn("23810" ++ Rest) ->
	{"238", "10", Rest};
%% Denmark, Telenor/Sonofon
plmn("23877" ++ Rest) ->
	{"238", "77", Rest};
%% Denmark, Telenor/Sonofon
plmn("23802" ++ Rest) ->
	{"238", "02", Rest};
%% Denmark, Telia
plmn("23820" ++ Rest) ->
	{"238", "20", Rest};
%% Denmark, Telia
plmn("23830" ++ Rest) ->
	{"238", "30", Rest};
%% Djibouti, Djibouti Telecom SA (Evatis)
plmn("63801" ++ Rest) ->
	{"638", "01", Rest};
%% Dominica, C & W
plmn("366110" ++ Rest) ->
	{"366", "110", Rest};
%% Dominica, Cingular Wireless/Digicel
plmn("36620" ++ Rest) ->
	{"366", "20", Rest};
%% Dominica, Wireless Ventures (Dominica) Ltd (Digicel Dominica)
plmn("36650" ++ Rest) ->
	{"366", "50", Rest};
%% Dominican Republic, Claro
plmn("37002" ++ Rest) ->
	{"370", "02", Rest};
%% Dominican Republic, Orange
plmn("37001" ++ Rest) ->
	{"370", "01", Rest};
%% Dominican Republic, TRIcom
plmn("37003" ++ Rest) ->
	{"370", "03", Rest};
%% Dominican Republic, Viva
plmn("37004" ++ Rest) ->
	{"370", "04", Rest};
%% Ecuador, Claro/Porta
plmn("74001" ++ Rest) ->
	{"740", "01", Rest};
%% Ecuador, CNT Mobile
plmn("74002" ++ Rest) ->
	{"740", "02", Rest};
%% Ecuador, MOVISTAR/OteCel/Failed Call(s)
plmn("74000" ++ Rest) ->
	{"740", "00", Rest};
%% Ecuador, Tuenti
plmn("74003" ++ Rest) ->
	{"740", "03", Rest};
%% Egypt, Orange/Mobinil
plmn("60201" ++ Rest) ->
	{"602", "01", Rest};
%% Egypt, ETISALAT
plmn("60203" ++ Rest) ->
	{"602", "03", Rest};
%% Egypt, Vodafone/Mirsfone
plmn("60202" ++ Rest) ->
	{"602", "02", Rest};
%% Egypt, WE/Telecom
plmn("60204" ++ Rest) ->
	{"602", "04", Rest};
%% El Salvador, CLARO/CTE
plmn("70601" ++ Rest) ->
	{"706", "01", Rest};
%% El Salvador, Digicel
plmn("70602" ++ Rest) ->
	{"706", "02", Rest};
%% El Salvador, INTELFON SA de CV
plmn("70605" ++ Rest) ->
	{"706", "05", Rest};
%% El Salvador, Telefonica
plmn("70604" ++ Rest) ->
	{"706", "04", Rest};
%% El Salvador, Telemovil
plmn("70603" ++ Rest) ->
	{"706", "03", Rest};
%% Equatorial Guinea, HiTs-GE
plmn("62703" ++ Rest) ->
	{"627", "03", Rest};
%% Equatorial Guinea, ORANGE/GETESA
plmn("62701" ++ Rest) ->
	{"627", "01", Rest};
%% Eritrea, Eritel
plmn("65701" ++ Rest) ->
	{"657", "01", Rest};
%% Estonia, EMT GSM
plmn("24801" ++ Rest) ->
	{"248", "01", Rest};
%% Estonia, Radiolinja Eesti
plmn("24802" ++ Rest) ->
	{"248", "02", Rest};
%% Estonia, Tele2 Eesti AS
plmn("24803" ++ Rest) ->
	{"248", "03", Rest};
%% Estonia, Top Connect OU
plmn("24804" ++ Rest) ->
	{"248", "04", Rest};
%% Ethiopia, ETH/MTN
plmn("63601" ++ Rest) ->
	{"636", "01", Rest};
%% Falkland Islands (Malvinas), Cable and Wireless South Atlantic Ltd (Falkland Islands
plmn("75001" ++ Rest) ->
	{"750", "01", Rest};
%% Faroe Islands, Edge Mobile Sp/F
plmn("28803" ++ Rest) ->
	{"288", "03", Rest};
%% Faroe Islands, Faroese Telecom
plmn("28801" ++ Rest) ->
	{"288", "01", Rest};
%% Faroe Islands, Kall GSM
plmn("28802" ++ Rest) ->
	{"288", "02", Rest};
%% Fiji, DigiCell
plmn("54202" ++ Rest) ->
	{"542", "02", Rest};
%% Fiji, Vodafone
plmn("54201" ++ Rest) ->
	{"542", "01", Rest};
%% Finland, Alands
plmn("24414" ++ Rest) ->
	{"244", "14", Rest};
%% Finland, Compatel Ltd
plmn("24426" ++ Rest) ->
	{"244", "26", Rest};
%% Finland, DNA/Finnet
plmn("24404" ++ Rest) ->
	{"244", "04", Rest};
%% Finland, DNA/Finnet
plmn("24403" ++ Rest) ->
	{"244", "03", Rest};
%% Finland, DNA/Finnet
plmn("24413" ++ Rest) ->
	{"244", "13", Rest};
%% Finland, DNA/Finnet
plmn("24412" ++ Rest) ->
	{"244", "12", Rest};
%% Finland, Elisa/Saunalahti
plmn("24405" ++ Rest) ->
	{"244", "05", Rest};
%% Finland, Elisa/Saunalahti
plmn("24421" ++ Rest) ->
	{"244", "21", Rest};
%% Finland, ID-Mobile
plmn("24482" ++ Rest) ->
	{"244", "82", Rest};
%% Finland, Mundio Mobile (Finland) Ltd
plmn("24411" ++ Rest) ->
	{"244", "11", Rest};
%% Finland, Nokia Oyj
plmn("24409" ++ Rest) ->
	{"244", "09", Rest};
%% Finland, TDC Oy Finland
plmn("24410" ++ Rest) ->
	{"244", "10", Rest};
%% Finland, TeliaSonera
plmn("24491" ++ Rest) ->
	{"244", "91", Rest};
%% France, AFONE SA
plmn("20827" ++ Rest) ->
	{"208", "27", Rest};
%% France, Association Plate-forme Telecom
plmn("20892" ++ Rest) ->
	{"208", "92", Rest};
%% France, Astrium
plmn("20828" ++ Rest) ->
	{"208", "28", Rest};
%% France, Bouygues Telecom
plmn("20821" ++ Rest) ->
	{"208", "21", Rest};
%% France, Bouygues Telecom
plmn("20820" ++ Rest) ->
	{"208", "20", Rest};
%% France, Bouygues Telecom
plmn("20888" ++ Rest) ->
	{"208", "88", Rest};
%% France, Lliad/FREE Mobile
plmn("20814" ++ Rest) ->
	{"208", "14", Rest};
%% France, GlobalStar
plmn("20807" ++ Rest) ->
	{"208", "07", Rest};
%% France, GlobalStar
plmn("20806" ++ Rest) ->
	{"208", "06", Rest};
%% France, GlobalStar
plmn("20805" ++ Rest) ->
	{"208", "05", Rest};
%% France, Orange
plmn("20829" ++ Rest) ->
	{"208", "29", Rest};
%% France, Legos - Local Exchange Global Operation Services SA
plmn("20817" ++ Rest) ->
	{"208", "17", Rest};
%% France, Lliad/FREE Mobile
plmn("20816" ++ Rest) ->
	{"208", "16", Rest};
%% France, Lliad/FREE Mobile
plmn("20815" ++ Rest) ->
	{"208", "15", Rest};
%% France, Lycamobile SARL
plmn("20825" ++ Rest) ->
	{"208", "25", Rest};
%% France, MobiquiThings
plmn("20824" ++ Rest) ->
	{"208", "24", Rest};
%% France, MobiquiThings
plmn("20803" ++ Rest) ->
	{"208", "03", Rest};
%% France, Mundio Mobile (France) Ltd
plmn("20831" ++ Rest) ->
	{"208", "31", Rest};
%% France, NRJ
plmn("20826" ++ Rest) ->
	{"208", "26", Rest};
%% France, Virgin Mobile/Omer
plmn("20889" ++ Rest) ->
	{"208", "89", Rest};
%% France, Virgin Mobile/Omer
plmn("20823" ++ Rest) ->
	{"208", "23", Rest};
%% France, Orange
plmn("20891" ++ Rest) ->
	{"208", "91", Rest};
%% France, Orange
plmn("20802" ++ Rest) ->
	{"208", "02", Rest};
%% France, Orange
plmn("20801" ++ Rest) ->
	{"208", "01", Rest};
%% France, S.F.R.
plmn("20810" ++ Rest) ->
	{"208", "10", Rest};
%% France, S.F.R.
plmn("20813" ++ Rest) ->
	{"208", "13", Rest};
%% France, S.F.R.
plmn("20809" ++ Rest) ->
	{"208", "09", Rest};
%% France, S.F.R.
plmn("20811" ++ Rest) ->
	{"208", "11", Rest};
%% France, SISTEER
plmn("20804" ++ Rest) ->
	{"208", "04", Rest};
%% France, Tel/Tel
plmn("20800" ++ Rest) ->
	{"208", "00", Rest};
%% France, Transatel SA
plmn("20822" ++ Rest) ->
	{"208", "22", Rest};
%% French Guiana, Bouygues/DigiCel
plmn("34020" ++ Rest) ->
	{"340", "20", Rest};
%% French Guiana, Orange Caribe
plmn("34001" ++ Rest) ->
	{"340", "01", Rest};
%% French Guiana, Outremer Telecom
plmn("34002" ++ Rest) ->
	{"340", "02", Rest};
%% French Guiana, TelCell GSM
plmn("34003" ++ Rest) ->
	{"340", "03", Rest};
%% French Guiana, TelCell GSM
plmn("34011" ++ Rest) ->
	{"340", "11", Rest};
%% French Polynesia, Pacific Mobile Telecom (PMT)
plmn("54715" ++ Rest) ->
	{"547", "15", Rest};
%% French Polynesia, Vini/Tikiphone
plmn("54720" ++ Rest) ->
	{"547", "20", Rest};
%% Gabon, Azur/Usan S.A.
plmn("62804" ++ Rest) ->
	{"628", "04", Rest};
%% Gabon, Libertis S.A.
plmn("62801" ++ Rest) ->
	{"628", "01", Rest};
%% Gabon, MOOV/Telecel
plmn("62802" ++ Rest) ->
	{"628", "02", Rest};
%% Gabon, Airtel/ZAIN/Celtel Gabon S.A.
plmn("62803" ++ Rest) ->
	{"628", "03", Rest};
%% Gambia, Africel
plmn("60702" ++ Rest) ->
	{"607", "02", Rest};
%% Gambia, Comium
plmn("60703" ++ Rest) ->
	{"607", "03", Rest};
%% Gambia, Gamcel
plmn("60701" ++ Rest) ->
	{"607", "01", Rest};
%% Gambia, Q-Cell
plmn("60704" ++ Rest) ->
	{"607", "04", Rest};
%% Georgia, Geocell Ltd.
plmn("28201" ++ Rest) ->
	{"282", "01", Rest};
%% Georgia, Iberiatel Ltd.
plmn("28203" ++ Rest) ->
	{"282", "03", Rest};
%% Georgia, Magti GSM Ltd.
plmn("28202" ++ Rest) ->
	{"282", "02", Rest};
%% Georgia, MobiTel/Beeline
plmn("28204" ++ Rest) ->
	{"282", "04", Rest};
%% Georgia, Silknet
plmn("28205" ++ Rest) ->
	{"282", "05", Rest};
%% Germany, E-Plus
plmn("26217" ++ Rest) ->
	{"262", "17", Rest};
%% Germany, DB Netz AG
plmn("26210" ++ Rest) ->
	{"262", "10", Rest};
%% Germany, E-Plus
plmn("26203" ++ Rest) ->
	{"262", "03", Rest};
%% Germany, E-Plus
plmn("26205" ++ Rest) ->
	{"262", "05", Rest};
%% Germany, E-Plus
plmn("26220" ++ Rest) ->
	{"262", "20", Rest};
%% Germany, E-Plus
plmn("26277" ++ Rest) ->
	{"262", "77", Rest};
%% Germany, E-Plus
plmn("26212" ++ Rest) ->
	{"262", "12", Rest};
%% Germany, Group 3G UMTS
plmn("26214" ++ Rest) ->
	{"262", "14", Rest};
%% Germany, Lycamobile
plmn("26243" ++ Rest) ->
	{"262", "43", Rest};
%% Germany, Mobilcom
plmn("26213" ++ Rest) ->
	{"262", "13", Rest};
%% Germany, O2
plmn("26207" ++ Rest) ->
	{"262", "07", Rest};
%% Germany, O2
plmn("26211" ++ Rest) ->
	{"262", "11", Rest};
%% Germany, O2
plmn("26208" ++ Rest) ->
	{"262", "08", Rest};
%% Germany, Sipgate
plmn("26233" ++ Rest) ->
	{"262", "33", Rest};
%% Germany, Sipgate
plmn("26222" ++ Rest) ->
	{"262", "22", Rest};
%% Germany, T-mobile/Telekom
plmn("26201" ++ Rest) ->
	{"262", "01", Rest};
%% Germany, T-mobile/Telekom
plmn("26206" ++ Rest) ->
	{"262", "06", Rest};
%% Germany, Telogic/ViStream
plmn("26216" ++ Rest) ->
	{"262", "16", Rest};
%% Germany, Vodafone D2
plmn("26209" ++ Rest) ->
	{"262", "09", Rest};
%% Germany, Vodafone D2
plmn("26204" ++ Rest) ->
	{"262", "04", Rest};
%% Germany, Vodafone D2
plmn("26202" ++ Rest) ->
	{"262", "02", Rest};
%% Germany, Vodafone D2
plmn("26242" ++ Rest) ->
	{"262", "42", Rest};
%% Ghana, Airtel/Tigo
plmn("62003" ++ Rest) ->
	{"620", "03", Rest};
%% Ghana, Airtel/Tigo
plmn("62006" ++ Rest) ->
	{"620", "06", Rest};
%% Ghana, Expresso Ghana Ltd
plmn("62004" ++ Rest) ->
	{"620", "04", Rest};
%% Ghana, GloMobile
plmn("62007" ++ Rest) ->
	{"620", "07", Rest};
%% Ghana, MTN
plmn("62001" ++ Rest) ->
	{"620", "01", Rest};
%% Ghana, Vodafone
plmn("62002" ++ Rest) ->
	{"620", "02", Rest};
%% Gibraltar, CTS Mobile
plmn("26606" ++ Rest) ->
	{"266", "06", Rest};
%% Gibraltar, eazi telecom
plmn("26609" ++ Rest) ->
	{"266", "09", Rest};
%% Gibraltar, Gibtel GSM
plmn("26601" ++ Rest) ->
	{"266", "01", Rest};
%% Greece, AMD Telecom SA
plmn("20207" ++ Rest) ->
	{"202", "07", Rest};
%% Greece, Cosmote
plmn("20202" ++ Rest) ->
	{"202", "02", Rest};
%% Greece, Cosmote
plmn("20201" ++ Rest) ->
	{"202", "01", Rest};
%% Greece, CyTa Mobile
plmn("20214" ++ Rest) ->
	{"202", "14", Rest};
%% Greece, Organismos Sidirodromon Ellados (OSE)
plmn("20204" ++ Rest) ->
	{"202", "04", Rest};
%% Greece, OTE Hellenic Telecommunications Organization SA
plmn("20203" ++ Rest) ->
	{"202", "03", Rest};
%% Greece, Tim/Wind
plmn("20210" ++ Rest) ->
	{"202", "10", Rest};
%% Greece, Tim/Wind
plmn("20209" ++ Rest) ->
	{"202", "09", Rest};
%% Greece, Vodafone
plmn("20205" ++ Rest) ->
	{"202", "05", Rest};
%% Greenland, Tele Greenland
plmn("29001" ++ Rest) ->
	{"290", "01", Rest};
%% Grenada, Cable & Wireless
plmn("352110" ++ Rest) ->
	{"352", "110", Rest};
%% Grenada, Digicel
plmn("35230" ++ Rest) ->
	{"352", "30", Rest};
%% Grenada, Digicel
plmn("35250" ++ Rest) ->
	{"352", "50", Rest};
%% Guadeloupe, Dauphin Telecom SU (Guadeloupe Telecom)
plmn("34008" ++ Rest) ->
	{"340", "08", Rest};
%% Guadeloupe,
plmn("34010" ++ Rest) ->
	{"340", "10", Rest};
%% Guam, Docomo
plmn("310370" ++ Rest) ->
	{"310", "370", Rest};
%% Guam, Docomo
plmn("310470" ++ Rest) ->
	{"310", "470", Rest};
%% Guam, GTA Wireless
plmn("310140" ++ Rest) ->
	{"310", "140", Rest};
%% Guam, Guam Teleph. Auth.
plmn("31033" ++ Rest) ->
	{"310", "33", Rest};
%% Guam, IT&E OverSeas
%% United States, Smith Bagley Inc.
plmn("31032" ++ Rest) ->
	{"310", "32", Rest};
%% Guam, Wave Runner LLC
plmn("311250" ++ Rest) ->
	{"311", "250", Rest};
%% Guatemala, Claro
plmn("70401" ++ Rest) ->
	{"704", "01", Rest};
%% Guatemala, Telefonica
plmn("70403" ++ Rest) ->
	{"704", "03", Rest};
%% Guatemala, TIGO/COMCEL
plmn("70402" ++ Rest) ->
	{"704", "02", Rest};
%% Guinea, MTN/Areeba
plmn("61104" ++ Rest) ->
	{"611", "04", Rest};
%% Guinea, Celcom
plmn("61105" ++ Rest) ->
	{"611", "05", Rest};
%% Guinea, Intercel
plmn("61103" ++ Rest) ->
	{"611", "03", Rest};
%% Guinea, Orange/Sonatel/Spacetel
plmn("61101" ++ Rest) ->
	{"611", "01", Rest};
%% Guinea, SotelGui
plmn("61102" ++ Rest) ->
	{"611", "02", Rest};
%% Guinea-Bissau, GuineTel
plmn("63201" ++ Rest) ->
	{"632", "01", Rest};
%% Guinea-Bissau, Orange
plmn("63203" ++ Rest) ->
	{"632", "03", Rest};
%% Guinea-Bissau, SpaceTel
plmn("63202" ++ Rest) ->
	{"632", "02", Rest};
%% Guyana, Cellink Plus
plmn("73802" ++ Rest) ->
	{"738", "02", Rest};
%% Guyana, DigiCel
plmn("73801" ++ Rest) ->
	{"738", "01", Rest};
%% Haiti, Comcel
plmn("37201" ++ Rest) ->
	{"372", "01", Rest};
%% Haiti, Digicel
plmn("37202" ++ Rest) ->
	{"372", "02", Rest};
%% Haiti, National Telecom SA (NatCom)
plmn("37203" ++ Rest) ->
	{"372", "03", Rest};
%% Honduras, Digicel
plmn("70840" ++ Rest) ->
	{"708", "40", Rest};
%% Honduras, HonduTel
plmn("70830" ++ Rest) ->
	{"708", "30", Rest};
%% Honduras, SERCOM/CLARO
plmn("70801" ++ Rest) ->
	{"708", "01", Rest};
%% Honduras, Telefonica/CELTEL
plmn("70802" ++ Rest) ->
	{"708", "02", Rest};
%% "Hongkong,  China",China Mobile/Peoples
plmn("45413" ++ Rest) ->
	{"454", "13", Rest};
%% "Hongkong,  China",China Mobile/Peoples
plmn("45412" ++ Rest) ->
	{"454", "12", Rest};
%% "Hongkong,  China",China Mobile/Peoples
plmn("45428" ++ Rest) ->
	{"454", "28", Rest};
%% "Hongkong,  China",China Motion
plmn("45409" ++ Rest) ->
	{"454", "09", Rest};
%% "Hongkong,  China",China Unicom Ltd
plmn("45407" ++ Rest) ->
	{"454", "07", Rest};
%% "Hongkong,  China",China-HongKong Telecom Ltd (CHKTL)
plmn("45411" ++ Rest) ->
	{"454", "11", Rest};
%% "Hongkong,  China",Citic Telecom Ltd.
plmn("45401" ++ Rest) ->
	{"454", "01", Rest};
%% "Hongkong,  China",CSL Ltd.
plmn("45402" ++ Rest) ->
	{"454", "02", Rest};
%% "Hongkong,  China",CSL Ltd.
plmn("45400" ++ Rest) ->
	{"454", "00", Rest};
%% "Hongkong,  China",CSL Ltd.
plmn("45418" ++ Rest) ->
	{"454", "18", Rest};
%% "Hongkong,  China",CSL/New World PCS Ltd.
plmn("45410" ++ Rest) ->
	{"454", "10", Rest};
%% "Hongkong,  China",CTExcel
plmn("45431" ++ Rest) ->
	{"454", "31", Rest};
%% "Hongkong,  China",H3G/Hutchinson
plmn("45414" ++ Rest) ->
	{"454", "14", Rest};
%% "Hongkong,  China",H3G/Hutchinson
plmn("45405" ++ Rest) ->
	{"454", "05", Rest};
%% "Hongkong,  China",H3G/Hutchinson
plmn("45404" ++ Rest) ->
	{"454", "04", Rest};
%% "Hongkong,  China",H3G/Hutchinson
plmn("45403" ++ Rest) ->
	{"454", "03", Rest};
%% "Hongkong,  China",HKT/PCCW
plmn("45429" ++ Rest) ->
	{"454", "29", Rest};
%% "Hongkong,  China",HKT/PCCW
plmn("45416" ++ Rest) ->
	{"454", "16", Rest};
%% "Hongkong,  China",HKT/PCCW
plmn("45419" ++ Rest) ->
	{"454", "19", Rest};
%% "Hongkong,  China",HKT/PCCW
plmn("45420" ++ Rest) ->
	{"454", "20", Rest};
%% "Hongkong,  China",shared by private TETRA systems
plmn("45447" ++ Rest) ->
	{"454", "47", Rest};
%% "Hongkong,  China",Multibyte Info Technology Ltd
plmn("45424" ++ Rest) ->
	{"454", "24", Rest};
%% "Hongkong,  China",shared by private TETRA systems
plmn("45440" ++ Rest) ->
	{"454", "40", Rest};
%% "Hongkong,  China",Truephone
plmn("45408" ++ Rest) ->
	{"454", "08", Rest};
%% "Hongkong,  China",Vodafone/SmarTone
plmn("45417" ++ Rest) ->
	{"454", "17", Rest};
%% "Hongkong,  China",Vodafone/SmarTone
plmn("45415" ++ Rest) ->
	{"454", "15", Rest};
%% "Hongkong,  China",Vodafone/SmarTone
plmn("45406" ++ Rest) ->
	{"454", "06", Rest};
%% Hungary, DIGI
plmn("21603" ++ Rest) ->
	{"216", "03", Rest};
%% Hungary, Pannon/Telenor
plmn("21601" ++ Rest) ->
	{"216", "01", Rest};
%% Hungary, T-mobile/Magyar
plmn("21630" ++ Rest) ->
	{"216", "30", Rest};
%% Hungary, UPC Magyarorszag Kft.
plmn("21671" ++ Rest) ->
	{"216", "71", Rest};
%% Hungary, Vodafone
plmn("21670" ++ Rest) ->
	{"216", "70", Rest};
%% Iceland, Amitelo
plmn("27409" ++ Rest) ->
	{"274", "09", Rest};
%% Iceland, IceCell
plmn("27407" ++ Rest) ->
	{"274", "07", Rest};
%% Iceland, Siminn
plmn("27408" ++ Rest) ->
	{"274", "08", Rest};
%% Iceland, Siminn
plmn("27401" ++ Rest) ->
	{"274", "01", Rest};
%% Iceland, NOVA
plmn("27411" ++ Rest) ->
	{"274", "11", Rest};
%% Iceland, VIKING/IMC
plmn("27404" ++ Rest) ->
	{"274", "04", Rest};
%% Iceland, Vodafone/Tal hf
plmn("27402" ++ Rest) ->
	{"274", "02", Rest};
%% Iceland, Vodafone/Tal hf
plmn("27405" ++ Rest) ->
	{"274", "05", Rest};
%% Iceland, Vodafone/Tal hf
plmn("27403" ++ Rest) ->
	{"274", "03", Rest};
%% India, Aircel
plmn("40428" ++ Rest) ->
	{"404", "28", Rest};
%% India, Aircel
plmn("40425" ++ Rest) ->
	{"404", "25", Rest};
%% India, Aircel
plmn("40417" ++ Rest) ->
	{"404", "17", Rest};
%% India, Aircel
plmn("40442" ++ Rest) ->
	{"404", "42", Rest};
%% India, Aircel
plmn("40433" ++ Rest) ->
	{"404", "33", Rest};
%% India, Aircel
plmn("40429" ++ Rest) ->
	{"404", "29", Rest};
%% India, Aircel Digilink India
plmn("40415" ++ Rest) ->
	{"404", "15", Rest};
%% India, Aircel Digilink India
plmn("40460" ++ Rest) ->
	{"404", "60", Rest};
%% India, Aircel Digilink India
plmn("40401" ++ Rest) ->
	{"404", "01", Rest};
%% India, AirTel
plmn("40553" ++ Rest) ->
	{"405", "53", Rest};
%% India, Barakhamba Sales & Serv.
plmn("40486" ++ Rest) ->
	{"404", "86", Rest};
%% India, Barakhamba Sales & Serv.
plmn("40413" ++ Rest) ->
	{"404", "13", Rest};
%% India, BSNL
plmn("40471" ++ Rest) ->
	{"404", "71", Rest};
%% India, BSNL
plmn("40476" ++ Rest) ->
	{"404", "76", Rest};
%% India, BSNL
plmn("40462" ++ Rest) ->
	{"404", "62", Rest};
%% India, BSNL
plmn("40453" ++ Rest) ->
	{"404", "53", Rest};
%% India, BSNL
plmn("40459" ++ Rest) ->
	{"404", "59", Rest};
%% India, BSNL
plmn("40475" ++ Rest) ->
	{"404", "75", Rest};
%% India, BSNL
plmn("40451" ++ Rest) ->
	{"404", "51", Rest};
%% India, BSNL
plmn("40458" ++ Rest) ->
	{"404", "58", Rest};
%% India, BSNL
plmn("40481" ++ Rest) ->
	{"404", "81", Rest};
%% India, BSNL
plmn("40474" ++ Rest) ->
	{"404", "74", Rest};
%% India, BSNL
plmn("40438" ++ Rest) ->
	{"404", "38", Rest};
%% India, BSNL
plmn("40457" ++ Rest) ->
	{"404", "57", Rest};
%% India, BSNL
plmn("40480" ++ Rest) ->
	{"404", "80", Rest};
%% India, BSNL
plmn("40473" ++ Rest) ->
	{"404", "73", Rest};
%% India, BSNL
plmn("40434" ++ Rest) ->
	{"404", "34", Rest};
%% India, BSNL
plmn("40466" ++ Rest) ->
	{"404", "66", Rest};
%% India, BSNL
plmn("40455" ++ Rest) ->
	{"404", "55", Rest};
%% India, BSNL
plmn("40472" ++ Rest) ->
	{"404", "72", Rest};
%% India, BSNL
plmn("40477" ++ Rest) ->
	{"404", "77", Rest};
%% India, BSNL
plmn("40464" ++ Rest) ->
	{"404", "64", Rest};
%% India, BSNL
plmn("40454" ++ Rest) ->
	{"404", "54", Rest};
%% India, Bharti Airtel Limited (Delhi)
plmn("40410" ++ Rest) ->
	{"404", "10", Rest};
%% India, Bharti Airtel Limited (Karnataka) (India)
plmn("40445" ++ Rest) ->
	{"404", "45", Rest};
%% India, CellOne A&N
plmn("40479" ++ Rest) ->
	{"404", "79", Rest};
%% India, Escorts Telecom Ltd.
plmn("40489" ++ Rest) ->
	{"404", "89", Rest};
%% India, Escorts Telecom Ltd.
plmn("40488" ++ Rest) ->
	{"404", "88", Rest};
%% India, Escorts Telecom Ltd.
plmn("40487" ++ Rest) ->
	{"404", "87", Rest};
%% India, Escorts Telecom Ltd.
plmn("40482" ++ Rest) ->
	{"404", "82", Rest};
%% India, Escotel Mobile Communications
plmn("40412" ++ Rest) ->
	{"404", "12", Rest};
%% India, Escotel Mobile Communications
plmn("40419" ++ Rest) ->
	{"404", "19", Rest};
%% India, Escotel Mobile Communications
plmn("40456" ++ Rest) ->
	{"404", "56", Rest};
%% India, Fascel Limited
plmn("40505" ++ Rest) ->
	{"405", "05", Rest};
%% India, Fascel
plmn("40405" ++ Rest) ->
	{"404", "05", Rest};
%% India, Fix Line
plmn("404998" ++ Rest) ->
	{"404", "998", Rest};
%% India, Hexacom India
plmn("40470" ++ Rest) ->
	{"404", "70", Rest};
%% India, Hexcom India
plmn("40416" ++ Rest) ->
	{"404", "16", Rest};
%% India, Idea Cellular Ltd.
plmn("40422" ++ Rest) ->
	{"404", "22", Rest};
%% India, Idea Cellular Ltd.
plmn("40478" ++ Rest) ->
	{"404", "78", Rest};
%% India, Idea Cellular Ltd.
plmn("40407" ++ Rest) ->
	{"404", "07", Rest};
%% India, Idea Cellular Ltd.
plmn("40404" ++ Rest) ->
	{"404", "04", Rest};
%% India, Idea Cellular Ltd.
plmn("40424" ++ Rest) ->
	{"404", "24", Rest};
%% India, Mahanagar Telephone Nigam
plmn("40468" ++ Rest) ->
	{"404", "68", Rest};
%% India, Mahanagar Telephone Nigam
plmn("40469" ++ Rest) ->
	{"404", "69", Rest};
%% India, Reliable Internet Services
plmn("40483" ++ Rest) ->
	{"404", "83", Rest};
%% India, Reliance Telecom Private
plmn("40452" ++ Rest) ->
	{"404", "52", Rest};
%% India, Reliance Telecom Private
plmn("40450" ++ Rest) ->
	{"404", "50", Rest};
%% India, Reliance Telecom Private
plmn("40467" ++ Rest) ->
	{"404", "67", Rest};
%% India, Reliance Telecom Private
plmn("40418" ++ Rest) ->
	{"404", "18", Rest};
%% India, Reliance Telecom Private
plmn("40485" ++ Rest) ->
	{"404", "85", Rest};
%% India, Reliance Telecom Private
plmn("40409" ++ Rest) ->
	{"404", "09", Rest};
%% India, Reliance Telecom Private
plmn("40587" ++ Rest) ->
	{"405", "87", Rest};
%% India, Reliance Telecom Private
plmn("40436" ++ Rest) ->
	{"404", "36", Rest};
%% India, RPG Cellular
plmn("40441" ++ Rest) ->
	{"404", "41", Rest};
%% India, Spice
plmn("40444" ++ Rest) ->
	{"404", "44", Rest};
%% India, Spice
plmn("40414" ++ Rest) ->
	{"404", "14", Rest};
%% India, Sterling Cellular Ltd.
plmn("40411" ++ Rest) ->
	{"404", "11", Rest};
%% India, TATA / Karnataka
plmn("40534" ++ Rest) ->
	{"405", "34", Rest};
%% India, Usha Martin Telecom
plmn("40430" ++ Rest) ->
	{"404", "30", Rest};
%% India, Various Networks
plmn("404999" ++ Rest) ->
	{"404", "999", Rest};
%% India, Unknown
plmn("40427" ++ Rest) ->
	{"404", "27", Rest};
%% India, Vodafone/Essar/Hutch
plmn("40443" ++ Rest) ->
	{"404", "43", Rest};
%% India, Unknown
plmn("40420" ++ Rest) ->
	{"404", "20", Rest};
%% Indonesia, Axis/Natrindo
plmn("51008" ++ Rest) ->
	{"510", "08", Rest};
%% Indonesia, Esia (PT Bakrie Telecom) (CDMA)
plmn("51099" ++ Rest) ->
	{"510", "99", Rest};
%% Indonesia, Flexi (PT Telkom) (CDMA)/Telkomsel
plmn("51007" ++ Rest) ->
	{"510", "07", Rest};
%% Indonesia, H3G CP
plmn("51089" ++ Rest) ->
	{"510", "89", Rest};
%% Indonesia, Indosat/Satelindo/M3
plmn("51001" ++ Rest) ->
	{"510", "01", Rest};
%% Indonesia, Indosat/Satelindo/M3
plmn("51021" ++ Rest) ->
	{"510", "21", Rest};
%% Indonesia, PT Pasifik Satelit Nusantara (PSN)
plmn("51000" ++ Rest) ->
	{"510", "00", Rest};
%% Indonesia, PT Sampoerna Telekomunikasi Indonesia (STI)
plmn("51027" ++ Rest) ->
	{"510", "27", Rest};
%% Indonesia, PT Smartfren Telecom Tbk
plmn("51028" ++ Rest) ->
	{"510", "28", Rest};
%% Indonesia, PT Smartfren Telecom Tbk
plmn("51009" ++ Rest) ->
	{"510", "09", Rest};
%% Indonesia, PT. Excelcom
plmn("51011" ++ Rest) ->
	{"510", "11", Rest};
%% Indonesia, Telkomsel
plmn("51010" ++ Rest) ->
	{"510", "10", Rest};
%% International Networks, Antarctica
plmn("90113" ++ Rest) ->
	{"901", "13", Rest};
%% Iran, Mobile Telecommunications Company of Esfahan JV-PJS (MTCE)
plmn("43219" ++ Rest) ->
	{"432", "19", Rest};
%% Iran, MTCE
plmn("43270" ++ Rest) ->
	{"432", "70", Rest};
%% Iran, MTN/IranCell
plmn("43235" ++ Rest) ->
	{"432", "35", Rest};
%% Iran, Rightel
plmn("43220" ++ Rest) ->
	{"432", "20", Rest};
%% Iran, Taliya
plmn("43232" ++ Rest) ->
	{"432", "32", Rest};
%% Iran, MCI/TCI
plmn("43211" ++ Rest) ->
	{"432", "11", Rest};
%% Iran, TKC/KFZO
plmn("43214" ++ Rest) ->
	{"432", "14", Rest};
%% Iraq, Asia Cell
plmn("41805" ++ Rest) ->
	{"418", "05", Rest};
%% Iraq, Fastlink
plmn("41866" ++ Rest) ->
	{"418", "66", Rest};
%% Iraq, Itisaluna and Kalemat
plmn("41892" ++ Rest) ->
	{"418", "92", Rest};
%% Iraq, Korek
plmn("41840" ++ Rest) ->
	{"418", "40", Rest};
%% Iraq, Korek
plmn("41882" ++ Rest) ->
	{"418", "82", Rest};
%% Iraq, Mobitel (Iraq-Kurdistan) and Moutiny
plmn("41845" ++ Rest) ->
	{"418", "45", Rest};
%% Iraq, Orascom Telecom
plmn("41830" ++ Rest) ->
	{"418", "30", Rest};
%% Iraq, ZAIN/Atheer/Orascom
plmn("41820" ++ Rest) ->
	{"418", "20", Rest};
%% Iraq, Sanatel
plmn("41808" ++ Rest) ->
	{"418", "08", Rest};
%% Ireland, Access Telecom Ltd.
plmn("27204" ++ Rest) ->
	{"272", "04", Rest};
%% Ireland, Clever Communications Ltd
plmn("27209" ++ Rest) ->
	{"272", "09", Rest};
%% Ireland, eircom Ltd
plmn("27207" ++ Rest) ->
	{"272", "07", Rest};
%% Ireland, Tesco Mobile/Liffey Telecom
plmn("27211" ++ Rest) ->
	{"272", "11", Rest};
%% Ireland, Lycamobile
plmn("27213" ++ Rest) ->
	{"272", "13", Rest};
%% Ireland, Meteor Mobile Ltd.
plmn("27203" ++ Rest) ->
	{"272", "03", Rest};
%% Ireland, Three
plmn("27205" ++ Rest) ->
	{"272", "05", Rest};
%% Ireland, Three
plmn("27217" ++ Rest) ->
	{"272", "17", Rest};
%% Ireland, Three
plmn("27202" ++ Rest) ->
	{"272", "02", Rest};
%% Ireland, Virgin Mobile
plmn("27215" ++ Rest) ->
	{"272", "15", Rest};
%% Ireland, Vodafone Eircell
plmn("27201" ++ Rest) ->
	{"272", "01", Rest};
%% Israel, Alon Cellular Ltd
plmn("42514" ++ Rest) ->
	{"425", "14", Rest};
%% Israel, Cellcom ltd.
plmn("42502" ++ Rest) ->
	{"425", "02", Rest};
%% Israel, Golan Telekom
plmn("42508" ++ Rest) ->
	{"425", "08", Rest};
%% Israel, Home Cellular Ltd
plmn("42515" ++ Rest) ->
	{"425", "15", Rest};
%% Israel, Hot Mobile/Mirs
plmn("42577" ++ Rest) ->
	{"425", "77", Rest};
%% Israel, Hot Mobile/Mirs
plmn("42507" ++ Rest) ->
	{"425", "07", Rest};
%% Israel, We4G/Marathon 018
plmn("42509" ++ Rest) ->
	{"425", "09", Rest};
%% Israel, Orange/Partner Co. Ltd.
plmn("42501" ++ Rest) ->
	{"425", "01", Rest};
%% Israel, Pelephone
plmn("42512" ++ Rest) ->
	{"425", "12", Rest};
%% Israel, Pelephone
plmn("42503" ++ Rest) ->
	{"425", "03", Rest};
%% Israel, Rami Levy Hashikma Marketing Communications Ltd
plmn("42516" ++ Rest) ->
	{"425", "16", Rest};
%% Israel, Telzar/AZI
plmn("42519" ++ Rest) ->
	{"425", "19", Rest};
%% Italy, BT Italia SpA
plmn("22234" ++ Rest) ->
	{"222", "34", Rest};
%% Italy, Digi Mobil
plmn("22236" ++ Rest) ->
	{"222", "36", Rest};
%% Italy, Elsacom
plmn("22202" ++ Rest) ->
	{"222", "02", Rest};
%% Italy, Fastweb SpA
plmn("22208" ++ Rest) ->
	{"222", "08", Rest};
%% Italy, Fix Line/VOIP Line
plmn("22200" ++ Rest) ->
	{"222", "00", Rest};
%% Italy, Hi3G
plmn("22299" ++ Rest) ->
	{"222", "99", Rest};
%% Italy, Iliad
plmn("22250" ++ Rest) ->
	{"222", "50", Rest};
%% Italy, IPSE 2000
plmn("22277" ++ Rest) ->
	{"222", "77", Rest};
%% Italy, Lycamobile Srl
plmn("22235" ++ Rest) ->
	{"222", "35", Rest};
%% Italy, Noverca Italia Srl
plmn("22207" ++ Rest) ->
	{"222", "07", Rest};
%% Italy, PosteMobile SpA
plmn("22233" ++ Rest) ->
	{"222", "33", Rest};
%% Italy, RFI Rete Ferroviaria Italiana SpA
plmn("22230" ++ Rest) ->
	{"222", "30", Rest};
%% Italy, Telecom Italia Mobile SpA
plmn("22248" ++ Rest) ->
	{"222", "48", Rest};
%% Italy, Telecom Italia Mobile SpA
plmn("22243" ++ Rest) ->
	{"222", "43", Rest};
%% Italy, TIM
plmn("22201" ++ Rest) ->
	{"222", "01", Rest};
%% Italy, Vodafone
plmn("22210" ++ Rest) ->
	{"222", "10", Rest};
%% Italy, Vodafone
plmn("22206" ++ Rest) ->
	{"222", "06", Rest};
%% Italy, WIND (Blu) -
plmn("22244" ++ Rest) ->
	{"222", "44", Rest};
%% Italy, WIND (Blu) -
plmn("22288" ++ Rest) ->
	{"222", "88", Rest};
%% Ivory Coast, Aircomm SA
plmn("61207" ++ Rest) ->
	{"612", "07", Rest};
%% Ivory Coast, Atlantik Tel./Moov
plmn("61202" ++ Rest) ->
	{"612", "02", Rest};
%% Ivory Coast, Comium
plmn("61204" ++ Rest) ->
	{"612", "04", Rest};
%% Ivory Coast, Comstar
plmn("61201" ++ Rest) ->
	{"612", "01", Rest};
%% Ivory Coast, MTN
plmn("61205" ++ Rest) ->
	{"612", "05", Rest};
%% Ivory Coast, Orange
plmn("61203" ++ Rest) ->
	{"612", "03", Rest};
%% Ivory Coast, OriCell
plmn("61206" ++ Rest) ->
	{"612", "06", Rest};
%% Jamaica, Cable & Wireless
plmn("338110" ++ Rest) ->
	{"338", "110", Rest};
%% Jamaica, Cable & Wireless
plmn("33820" ++ Rest) ->
	{"338", "20", Rest};
%% Jamaica, Cable & Wireless
plmn("338180" ++ Rest) ->
	{"338", "180", Rest};
%% Jamaica, DIGICEL/Mossel
plmn("33850" ++ Rest) ->
	{"338", "50", Rest};
%% Japan, Y-Mobile
plmn("44000" ++ Rest) ->
	{"440", "00", Rest};
%% Japan, KDDI Corporation
plmn("44074" ++ Rest) ->
	{"440", "74", Rest};
%% Japan, KDDI Corporation
plmn("44070" ++ Rest) ->
	{"440", "70", Rest};
%% Japan, KDDI Corporation
plmn("44089" ++ Rest) ->
	{"440", "89", Rest};
%% Japan, KDDI Corporation
plmn("44051" ++ Rest) ->
	{"440", "51", Rest};
%% Japan, KDDI Corporation
plmn("44075" ++ Rest) ->
	{"440", "75", Rest};
%% Japan, KDDI Corporation
plmn("44056" ++ Rest) ->
	{"440", "56", Rest};
%% Japan, KDDI Corporation
plmn("44170" ++ Rest) ->
	{"441", "70", Rest};
%% Japan, KDDI Corporation
plmn("44052" ++ Rest) ->
	{"440", "52", Rest};
%% Japan, KDDI Corporation
plmn("44076" ++ Rest) ->
	{"440", "76", Rest};
%% Japan, KDDI Corporation
plmn("44071" ++ Rest) ->
	{"440", "71", Rest};
%% Japan, KDDI Corporation
plmn("44053" ++ Rest) ->
	{"440", "53", Rest};
%% Japan, KDDI Corporation
plmn("44077" ++ Rest) ->
	{"440", "77", Rest};
%% Japan, KDDI Corporation
plmn("44008" ++ Rest) ->
	{"440", "08", Rest};
%% Japan, KDDI Corporation
plmn("44072" ++ Rest) ->
	{"440", "72", Rest};
%% Japan, KDDI Corporation
plmn("44054" ++ Rest) ->
	{"440", "54", Rest};
%% Japan, KDDI Corporation
plmn("44079" ++ Rest) ->
	{"440", "79", Rest};
%% Japan, KDDI Corporation
plmn("44007" ++ Rest) ->
	{"440", "07", Rest};
%% Japan, KDDI Corporation
plmn("44073" ++ Rest) ->
	{"440", "73", Rest};
%% Japan, KDDI Corporation
plmn("44055" ++ Rest) ->
	{"440", "55", Rest};
%% Japan, KDDI Corporation
plmn("44088" ++ Rest) ->
	{"440", "88", Rest};
%% Japan, KDDI Corporation
plmn("44050" ++ Rest) ->
	{"440", "50", Rest};
%% Japan, NTT Docomo
plmn("44021" ++ Rest) ->
	{"440", "21", Rest};
%% Japan, NTT Docomo
plmn("44144" ++ Rest) ->
	{"441", "44", Rest};
%% Japan, NTT Docomo
plmn("44013" ++ Rest) ->
	{"440", "13", Rest};
%% Japan, NTT Docomo
plmn("44001" ++ Rest) ->
	{"440", "01", Rest};
%% Japan, NTT Docomo
plmn("44023" ++ Rest) ->
	{"440", "23", Rest};
%% Japan, NTT Docomo
plmn("44016" ++ Rest) ->
	{"440", "16", Rest};
%% Japan, NTT Docomo
plmn("44199" ++ Rest) ->
	{"441", "99", Rest};
%% Japan, NTT Docomo
plmn("44034" ++ Rest) ->
	{"440", "34", Rest};
%% Japan, NTT Docomo
plmn("44069" ++ Rest) ->
	{"440", "69", Rest};
%% Japan, NTT Docomo
plmn("44064" ++ Rest) ->
	{"440", "64", Rest};
%% Japan, NTT Docomo
plmn("44037" ++ Rest) ->
	{"440", "37", Rest};
%% Japan, NTT Docomo
plmn("44025" ++ Rest) ->
	{"440", "25", Rest};
%% Japan, NTT Docomo
plmn("44022" ++ Rest) ->
	{"440", "22", Rest};
%% Japan, NTT Docomo
plmn("44143" ++ Rest) ->
	{"441", "43", Rest};
%% Japan, NTT Docomo
plmn("44027" ++ Rest) ->
	{"440", "27", Rest};
%% Japan, NTT Docomo
plmn("44002" ++ Rest) ->
	{"440", "02", Rest};
%% Japan, NTT Docomo
plmn("44017" ++ Rest) ->
	{"440", "17", Rest};
%% Japan, NTT Docomo
plmn("44031" ++ Rest) ->
	{"440", "31", Rest};
%% Japan, NTT Docomo
plmn("44087" ++ Rest) ->
	{"440", "87", Rest};
%% Japan, NTT Docomo
plmn("44065" ++ Rest) ->
	{"440", "65", Rest};
%% Japan, NTT Docomo
plmn("44036" ++ Rest) ->
	{"440", "36", Rest};
%% Japan, NTT Docomo
plmn("44192" ++ Rest) ->
	{"441", "92", Rest};
%% Japan, NTT Docomo
plmn("44012" ++ Rest) ->
	{"440", "12", Rest};
%% Japan, NTT Docomo
plmn("44058" ++ Rest) ->
	{"440", "58", Rest};
%% Japan, NTT Docomo
plmn("44028" ++ Rest) ->
	{"440", "28", Rest};
%% Japan, NTT Docomo
plmn("44003" ++ Rest) ->
	{"440", "03", Rest};
%% Japan, NTT Docomo
plmn("44018" ++ Rest) ->
	{"440", "18", Rest};
%% Japan, NTT Docomo
plmn("44191" ++ Rest) ->
	{"441", "91", Rest};
%% Japan, NTT Docomo
plmn("44032" ++ Rest) ->
	{"440", "32", Rest};
%% Japan, NTT Docomo
plmn("44061" ++ Rest) ->
	{"440", "61", Rest};
%% Japan, NTT Docomo
plmn("44035" ++ Rest) ->
	{"440", "35", Rest};
%% Japan, NTT Docomo
plmn("44193" ++ Rest) ->
	{"441", "93", Rest};
%% Japan, NTT Docomo
plmn("44140" ++ Rest) ->
	{"441", "40", Rest};
%% Japan, NTT Docomo
plmn("44066" ++ Rest) ->
	{"440", "66", Rest};
%% Japan, NTT Docomo
plmn("44049" ++ Rest) ->
	{"440", "49", Rest};
%% Japan, NTT Docomo
plmn("44029" ++ Rest) ->
	{"440", "29", Rest};
%% Japan, NTT Docomo
plmn("44009" ++ Rest) ->
	{"440", "09", Rest};
%% Japan, NTT Docomo
plmn("44019" ++ Rest) ->
	{"440", "19", Rest};
%% Japan, NTT Docomo
plmn("44190" ++ Rest) ->
	{"441", "90", Rest};
%% Japan, NTT Docomo
plmn("44033" ++ Rest) ->
	{"440", "33", Rest};
%% Japan, NTT Docomo
plmn("44060" ++ Rest) ->
	{"440", "60", Rest};
%% Japan, NTT Docomo
plmn("44014" ++ Rest) ->
	{"440", "14", Rest};
%% Japan, NTT Docomo
plmn("44194" ++ Rest) ->
	{"441", "94", Rest};
%% Japan, NTT Docomo
plmn("44141" ++ Rest) ->
	{"441", "41", Rest};
%% Japan, NTT Docomo
plmn("44067" ++ Rest) ->
	{"440", "67", Rest};
%% Japan, NTT Docomo
plmn("44062" ++ Rest) ->
	{"440", "62", Rest};
%% Japan, NTT Docomo
plmn("44039" ++ Rest) ->
	{"440", "39", Rest};
%% Japan, NTT Docomo
plmn("44030" ++ Rest) ->
	{"440", "30", Rest};
%% Japan, NTT Docomo
plmn("44010" ++ Rest) ->
	{"440", "10", Rest};
%% Japan, NTT Docomo
plmn("44145" ++ Rest) ->
	{"441", "45", Rest};
%% Japan, NTT Docomo
plmn("44024" ++ Rest) ->
	{"440", "24", Rest};
%% Japan, NTT Docomo
plmn("44015" ++ Rest) ->
	{"440", "15", Rest};
%% Japan, NTT Docomo
plmn("44198" ++ Rest) ->
	{"441", "98", Rest};
%% Japan, NTT Docomo
plmn("44142" ++ Rest) ->
	{"441", "42", Rest};
%% Japan, NTT Docomo
plmn("44068" ++ Rest) ->
	{"440", "68", Rest};
%% Japan, NTT Docomo
plmn("44063" ++ Rest) ->
	{"440", "63", Rest};
%% Japan, NTT Docomo
plmn("44038" ++ Rest) ->
	{"440", "38", Rest};
%% Japan, NTT Docomo
plmn("44026" ++ Rest) ->
	{"440", "26", Rest};
%% Japan, NTT Docomo
plmn("44011" ++ Rest) ->
	{"440", "11", Rest};
%% Japan, NTT Docomo
plmn("44099" ++ Rest) ->
	{"440", "99", Rest};
%% Japan, Okinawa Cellular Telephone
plmn("44078" ++ Rest) ->
	{"440", "78", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44047" ++ Rest) ->
	{"440", "47", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44095" ++ Rest) ->
	{"440", "95", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44041" ++ Rest) ->
	{"440", "41", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44164" ++ Rest) ->
	{"441", "64", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44046" ++ Rest) ->
	{"440", "46", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44097" ++ Rest) ->
	{"440", "97", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44042" ++ Rest) ->
	{"440", "42", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44165" ++ Rest) ->
	{"441", "65", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44090" ++ Rest) ->
	{"440", "90", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44092" ++ Rest) ->
	{"440", "92", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44098" ++ Rest) ->
	{"440", "98", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44043" ++ Rest) ->
	{"440", "43", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44093" ++ Rest) ->
	{"440", "93", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44048" ++ Rest) ->
	{"440", "48", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44006" ++ Rest) ->
	{"440", "06", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44161" ++ Rest) ->
	{"441", "61", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44044" ++ Rest) ->
	{"440", "44", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44094" ++ Rest) ->
	{"440", "94", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44004" ++ Rest) ->
	{"440", "04", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44162" ++ Rest) ->
	{"441", "62", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44045" ++ Rest) ->
	{"440", "45", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44020" ++ Rest) ->
	{"440", "20", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44096" ++ Rest) ->
	{"440", "96", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44040" ++ Rest) ->
	{"440", "40", Rest};
%% Japan, SoftBank Mobile Corp
plmn("44163" ++ Rest) ->
	{"441", "63", Rest};
%% Japan, KDDI Corporation
plmn("44083" ++ Rest) ->
	{"440", "83", Rest};
%% Japan, KDDI Corporation
plmn("44085" ++ Rest) ->
	{"440", "85", Rest};
%% Japan, KDDI Corporation
plmn("44081" ++ Rest) ->
	{"440", "81", Rest};
%% Japan, KDDI Corporation
plmn("44080" ++ Rest) ->
	{"440", "80", Rest};
%% Japan, KDDI Corporation
plmn("44086" ++ Rest) ->
	{"440", "86", Rest};
%% Japan, KDDI Corporation
plmn("44084" ++ Rest) ->
	{"440", "84", Rest};
%% Japan, KDDI Corporation
plmn("44082" ++ Rest) ->
	{"440", "82", Rest};
%% Jordan, Orange/Petra
plmn("41677" ++ Rest) ->
	{"416", "77", Rest};
%% Jordan, Umniah Mobile Co.
plmn("41603" ++ Rest) ->
	{"416", "03", Rest};
%% Jordan, Xpress
plmn("41602" ++ Rest) ->
	{"416", "02", Rest};
%% Jordan, ZAIN /J.M.T.S
plmn("41601" ++ Rest) ->
	{"416", "01", Rest};
%% Kazakhstan, Beeline/KaR-Tel LLP
plmn("40101" ++ Rest) ->
	{"401", "01", Rest};
%% Kazakhstan, Dalacom/Altel
plmn("40107" ++ Rest) ->
	{"401", "07", Rest};
%% Kazakhstan, K-Cell
plmn("40102" ++ Rest) ->
	{"401", "02", Rest};
%% Kazakhstan, Tele2/NEO/MTS
plmn("40177" ++ Rest) ->
	{"401", "77", Rest};
%% Kenya, Econet Wireless
plmn("63905" ++ Rest) ->
	{"639", "05", Rest};
%% Kenya, Safaricom Ltd.
plmn("63902" ++ Rest) ->
	{"639", "02", Rest};
%% Kenya, Telkom fka. Orange
plmn("63907" ++ Rest) ->
	{"639", "07", Rest};
%% Kenya, Airtel/Zain/Celtel Ltd.
plmn("63903" ++ Rest) ->
	{"639", "03", Rest};
%% Kiribati, Kiribati Frigate
plmn("54509" ++ Rest) ->
	{"545", "09", Rest};
%% "Korea N.,  Dem. People's Rep.",Sun Net
plmn("467193" ++ Rest) ->
	{"467", "193", Rest};
%% "Korea S,  Republic of",KT Freetel Co. Ltd.
plmn("45004" ++ Rest) ->
	{"450", "04", Rest};
%% "Korea S,  Republic of",KT Freetel Co. Ltd.
plmn("45008" ++ Rest) ->
	{"450", "08", Rest};
%% "Korea S,  Republic of",KT Freetel Co. Ltd.
plmn("45002" ++ Rest) ->
	{"450", "02", Rest};
%% "Korea S,  Republic of",LG Telecom
plmn("45006" ++ Rest) ->
	{"450", "06", Rest};
%% "Korea S,  Republic of",SK Telecom
plmn("45003" ++ Rest) ->
	{"450", "03", Rest};
%% "Korea S,  Republic of",SK Telecom Co. Ltd
plmn("45005" ++ Rest) ->
	{"450", "05", Rest};
%% Kosovo, Dardafon.Net LLC
plmn("22106" ++ Rest) ->
	{"221", "06", Rest};
%% Kosovo, IPKO
plmn("22102" ++ Rest) ->
	{"221", "02", Rest};
%% Kosovo, MTS DOO
plmn("22103" ++ Rest) ->
	{"221", "03", Rest};
%% Kosovo, Vala
plmn("22101" ++ Rest) ->
	{"221", "01", Rest};
%% Kuwait, Zain
plmn("41902" ++ Rest) ->
	{"419", "02", Rest};
%% Kuwait, Viva
plmn("41904" ++ Rest) ->
	{"419", "04", Rest};
%% Kuwait, Ooredoo
plmn("41903" ++ Rest) ->
	{"419", "03", Rest};
%% Kyrgyzstan, AkTel LLC
plmn("43703" ++ Rest) ->
	{"437", "03", Rest};
%% Kyrgyzstan, Beeline/Bitel
plmn("43701" ++ Rest) ->
	{"437", "01", Rest};
%% Kyrgyzstan, MEGACOM
plmn("43705" ++ Rest) ->
	{"437", "05", Rest};
%% Kyrgyzstan, O!/NUR Telecom
plmn("43709" ++ Rest) ->
	{"437", "09", Rest};
%% Laos P.D.R., ETL Mobile
plmn("45702" ++ Rest) ->
	{"457", "02", Rest};
%% Laos P.D.R., Lao Tel
plmn("45701" ++ Rest) ->
	{"457", "01", Rest};
%% Laos P.D.R., Beeline/Tigo/Millicom
plmn("45708" ++ Rest) ->
	{"457", "08", Rest};
%% Laos P.D.R., UNITEL/LAT
plmn("45703" ++ Rest) ->
	{"457", "03", Rest};
%% Latvia, Bite
plmn("24705" ++ Rest) ->
	{"247", "05", Rest};
%% Latvia, Latvian Mobile Phone
plmn("24701" ++ Rest) ->
	{"247", "01", Rest};
%% Latvia, SIA Camel Mobile
plmn("24709" ++ Rest) ->
	{"247", "09", Rest};
%% Latvia, SIA IZZI
plmn("24708" ++ Rest) ->
	{"247", "08", Rest};
%% Latvia, SIA Master Telecom
plmn("24707" ++ Rest) ->
	{"247", "07", Rest};
%% Latvia, SIA Rigatta
plmn("24706" ++ Rest) ->
	{"247", "06", Rest};
%% Latvia, Tele2
plmn("24702" ++ Rest) ->
	{"247", "02", Rest};
%% Latvia, TRIATEL/Telekom Baltija
plmn("24703" ++ Rest) ->
	{"247", "03", Rest};
%% Lebanon, Cellis
plmn("41535" ++ Rest) ->
	{"415", "35", Rest};
%% Lebanon, Cellis
plmn("41533" ++ Rest) ->
	{"415", "33", Rest};
%% Lebanon, Cellis
plmn("41532" ++ Rest) ->
	{"415", "32", Rest};
%% Lebanon, FTML Cellis
plmn("41534" ++ Rest) ->
	{"415", "34", Rest};
%% Lebanon, MIC2/LibanCell/MTC
plmn("41537" ++ Rest) ->
	{"415", "37", Rest};
%% Lebanon, MIC2/LibanCell/MTC
plmn("41539" ++ Rest) ->
	{"415", "39", Rest};
%% Lebanon, MIC2/LibanCell/MTC
plmn("41538" ++ Rest) ->
	{"415", "38", Rest};
%% Lebanon, MIC1 (Alfa)
plmn("41501" ++ Rest) ->
	{"415", "01", Rest};
%% Lebanon, MIC2/LibanCell/MTC
plmn("41503" ++ Rest) ->
	{"415", "03", Rest};
%% Lebanon, MIC2/LibanCell/MTC
plmn("41536" ++ Rest) ->
	{"415", "36", Rest};
%% Lesotho, Econet/Ezi-cel
plmn("65102" ++ Rest) ->
	{"651", "02", Rest};
%% Lesotho, Vodacom Lesotho
plmn("65101" ++ Rest) ->
	{"651", "01", Rest};
%% Liberia, Comium BVI
plmn("61804" ++ Rest) ->
	{"618", "04", Rest};
%% Liberia, Libercell
plmn("61802" ++ Rest) ->
	{"618", "02", Rest};
%% Liberia, LibTelco
plmn("61820" ++ Rest) ->
	{"618", "20", Rest};
%% Liberia, Lonestar
plmn("61801" ++ Rest) ->
	{"618", "01", Rest};
%% Liberia, Orange
plmn("61807" ++ Rest) ->
	{"618", "07", Rest};
%% Libya, Al-Madar
plmn("60602" ++ Rest) ->
	{"606", "02", Rest};
%% Libya, Al-Madar
plmn("60601" ++ Rest) ->
	{"606", "01", Rest};
%% Libya, Hatef
plmn("60606" ++ Rest) ->
	{"606", "06", Rest};
%% Libya, Libyana
plmn("60600" ++ Rest) ->
	{"606", "00", Rest};
%% Libya, Libyana
plmn("60603" ++ Rest) ->
	{"606", "03", Rest};
%% Liechtenstein, CUBIC (Liechtenstein
plmn("29506" ++ Rest) ->
	{"295", "06", Rest};
%% Liechtenstein, First Mobile AG
plmn("29507" ++ Rest) ->
	{"295", "07", Rest};
%% Liechtenstein, Orange
plmn("29502" ++ Rest) ->
	{"295", "02", Rest};
%% Liechtenstein, Swisscom FL AG
plmn("29501" ++ Rest) ->
	{"295", "01", Rest};
%% Liechtenstein, Alpmobile/Tele2
plmn("29577" ++ Rest) ->
	{"295", "77", Rest};
%% Liechtenstein, Telecom FL1 AG
plmn("29505" ++ Rest) ->
	{"295", "05", Rest};
%% Lithuania, Bite
plmn("24602" ++ Rest) ->
	{"246", "02", Rest};
%% Lithuania, Omnitel
plmn("24601" ++ Rest) ->
	{"246", "01", Rest};
%% Lithuania, Tele2
plmn("24603" ++ Rest) ->
	{"246", "03", Rest};
%% Luxembourg, Millicom Tango GSM
plmn("27077" ++ Rest) ->
	{"270", "77", Rest};
%% Luxembourg, P+T/Post LUXGSM
plmn("27001" ++ Rest) ->
	{"270", "01", Rest};
%% Luxembourg, Orange/VOXmobile S.A.
plmn("27099" ++ Rest) ->
	{"270", "99", Rest};
%% "Macao,  China",C.T.M. TELEMOVEL+
plmn("45501" ++ Rest) ->
	{"455", "01", Rest};
%% "Macao,  China",C.T.M. TELEMOVEL+
plmn("45504" ++ Rest) ->
	{"455", "04", Rest};
%% "Macao,  China",China Telecom
plmn("45502" ++ Rest) ->
	{"455", "02", Rest};
%% "Macao,  China",Hutchison Telephone Co. Ltd
plmn("45503" ++ Rest) ->
	{"455", "03", Rest};
%% "Macao,  China",Hutchison Telephone Co. Ltd
plmn("45505" ++ Rest) ->
	{"455", "05", Rest};
%% "Macao,  China",Smartone Mobile
plmn("45506" ++ Rest) ->
	{"455", "06", Rest};
%% "Macao,  China",Smartone Mobile
plmn("45500" ++ Rest) ->
	{"455", "00", Rest};
%% Macedonia, ONE/Cosmofone
plmn("29475" ++ Rest) ->
	{"294", "75", Rest};
%% Macedonia, Lycamobile
plmn("29404" ++ Rest) ->
	{"294", "04", Rest};
%% Macedonia, ONE/Cosmofone
plmn("29402" ++ Rest) ->
	{"294", "02", Rest};
%% Macedonia, T-Mobile/Mobimak
plmn("29401" ++ Rest) ->
	{"294", "01", Rest};
%% Macedonia, VIP Mobile
plmn("29403" ++ Rest) ->
	{"294", "03", Rest};
%% Madagascar, Airtel/MADACOM
plmn("64601" ++ Rest) ->
	{"646", "01", Rest};
%% Madagascar, Orange/Soci
plmn("64602" ++ Rest) ->
	{"646", "02", Rest};
%% Madagascar, Sacel
plmn("64603" ++ Rest) ->
	{"646", "03", Rest};
%% Madagascar, Telma
plmn("64604" ++ Rest) ->
	{"646", "04", Rest};
%% Malawi, TNM/Telekom Network Ltd.
plmn("65001" ++ Rest) ->
	{"650", "01", Rest};
%% Malawi, Airtel/Zain/Celtel ltd.
plmn("65010" ++ Rest) ->
	{"650", "10", Rest};
%% Malaysia, Art900
plmn("50201" ++ Rest) ->
	{"502", "01", Rest};
%% Malaysia, Baraka Telecom Sdn Bhd
plmn("502151" ++ Rest) ->
	{"502", "151", Rest};
%% Malaysia, CelCom
plmn("502198" ++ Rest) ->
	{"502", "198", Rest};
%% Malaysia, CelCom/XOX Com Sdn Bhd
plmn("50219" ++ Rest) ->
	{"502", "19", Rest};
%% Malaysia, CelCom
plmn("50213" ++ Rest) ->
	{"502", "13", Rest};
%% Malaysia, Digi Telecommunications
plmn("50210" ++ Rest) ->
	{"502", "10", Rest};
%% Malaysia, Digi Telecommunications
plmn("50216" ++ Rest) ->
	{"502", "16", Rest};
%% Malaysia, Electcoms Wireless Sdn Bhd
plmn("50220" ++ Rest) ->
	{"502", "20", Rest};
%% Malaysia, Maxis
plmn("50212" ++ Rest) ->
	{"502", "12", Rest};
%% Malaysia, Maxis
plmn("50217" ++ Rest) ->
	{"502", "17", Rest};
%% Malaysia, MTX Utara
plmn("50211" ++ Rest) ->
	{"502", "11", Rest};
%% Malaysia, Webe/Packet One Networks (Malaysia) Sdn Bhd
plmn("502153" ++ Rest) ->
	{"502", "153", Rest};
%% Malaysia, Samata Communications Sdn Bhd
plmn("502155" ++ Rest) ->
	{"502", "155", Rest};
%% Malaysia, Tron/Talk Focus Sdn Bhd
plmn("502154" ++ Rest) ->
	{"502", "154", Rest};
%% Malaysia, TuneTalk
plmn("502150" ++ Rest) ->
	{"502", "150", Rest};
%% Malaysia, U Mobile
plmn("50218" ++ Rest) ->
	{"502", "18", Rest};
%% Malaysia, YES
plmn("502152" ++ Rest) ->
	{"502", "152", Rest};
%% Maldives, Dhiraagu/C&W
plmn("47201" ++ Rest) ->
	{"472", "01", Rest};
%% Maldives, Ooredo/Wataniya
plmn("47202" ++ Rest) ->
	{"472", "02", Rest};
%% Mali, ATEL SA
plmn("61003" ++ Rest) ->
	{"610", "03", Rest};
%% Mali, Malitel
plmn("61001" ++ Rest) ->
	{"610", "01", Rest};
%% Mali, Orange/IKATEL
plmn("61002" ++ Rest) ->
	{"610", "02", Rest};
%% Malta, GO Mobile
plmn("27821" ++ Rest) ->
	{"278", "21", Rest};
%% Malta, Melita
plmn("27877" ++ Rest) ->
	{"278", "77", Rest};
%% Malta, Vodafone
plmn("27801" ++ Rest) ->
	{"278", "01", Rest};
%% Martinique (French Department of), UTS Caraibe
plmn("34012" ++ Rest) ->
	{"340", "12", Rest};
%% Mauritania, Chinguitel SA
plmn("60902" ++ Rest) ->
	{"609", "02", Rest};
%% Mauritania, Mattel
plmn("60901" ++ Rest) ->
	{"609", "01", Rest};
%% Mauritania, Mauritel
plmn("60910" ++ Rest) ->
	{"609", "10", Rest};
%% Mauritius, Emtel Ltd
plmn("61710" ++ Rest) ->
	{"617", "10", Rest};
%% Mauritius, CHILI/MTML
plmn("61703" ++ Rest) ->
	{"617", "03", Rest};
%% Mauritius, CHILI/MTML
plmn("61702" ++ Rest) ->
	{"617", "02", Rest};
%% Mauritius, Orange/Cellplus
plmn("61701" ++ Rest) ->
	{"617", "01", Rest};
%% Mexico, AT&T/IUSACell
plmn("33404" ++ Rest) ->
	{"334", "04", Rest};
%% Mexico, AT&T/IUSACell
plmn("33405" ++ Rest) ->
	{"334", "05", Rest};
%% Mexico, AT&T/IUSACell
plmn("33450" ++ Rest) ->
	{"334", "50", Rest};
%% Mexico, AT&T/IUSACell
plmn("33440" ++ Rest) ->
	{"334", "40", Rest};
%% Mexico, Movistar/Pegaso
plmn("33403" ++ Rest) ->
	{"334", "03", Rest};
%% Mexico, Movistar/Pegaso
plmn("33430" ++ Rest) ->
	{"334", "30", Rest};
%% Mexico, NEXTEL
plmn("33490" ++ Rest) ->
	{"334", "90", Rest};
%% Mexico, NEXTEL
plmn("33410" ++ Rest) ->
	{"334", "10", Rest};
%% Mexico, NEXTEL
plmn("33409" ++ Rest) ->
	{"334", "09", Rest};
%% Mexico, NEXTEL
plmn("33401" ++ Rest) ->
	{"334", "01", Rest};
%% Mexico, Operadora Unefon SA de CV
plmn("33480" ++ Rest) ->
	{"334", "80", Rest};
%% Mexico, Operadora Unefon SA de CV
plmn("33470" ++ Rest) ->
	{"334", "70", Rest};
%% Mexico, SAI PCS
plmn("33460" ++ Rest) ->
	{"334", "60", Rest};
%% Mexico, TelCel/America Movil
plmn("33420" ++ Rest) ->
	{"334", "20", Rest};
%% Mexico, TelCel/America Movil
plmn("33402" ++ Rest) ->
	{"334", "02", Rest};
%% Micronesia, FSM Telecom
plmn("55001" ++ Rest) ->
	{"550", "01", Rest};
%% Moldova, Eventis Mobile
plmn("25904" ++ Rest) ->
	{"259", "04", Rest};
%% Moldova, IDC/Unite
plmn("25905" ++ Rest) ->
	{"259", "05", Rest};
%% Moldova, IDC/Unite
plmn("25903" ++ Rest) ->
	{"259", "03", Rest};
%% Moldova, IDC/Unite
plmn("25999" ++ Rest) ->
	{"259", "99", Rest};
%% Moldova, Moldcell
plmn("25902" ++ Rest) ->
	{"259", "02", Rest};
%% Moldova, Orange/Voxtel
plmn("25901" ++ Rest) ->
	{"259", "01", Rest};
%% Monaco, Monaco Telecom
plmn("21210" ++ Rest) ->
	{"212", "10", Rest};
%% Monaco, Monaco Telecom
plmn("21201" ++ Rest) ->
	{"212", "01", Rest};
%% Mongolia, G-Mobile Corporation Ltd
plmn("42898" ++ Rest) ->
	{"428", "98", Rest};
%% Mongolia, Mobicom
plmn("42899" ++ Rest) ->
	{"428", "99", Rest};
%% Mongolia, Skytel Co. Ltd
plmn("42800" ++ Rest) ->
	{"428", "00", Rest};
%% Mongolia, Skytel Co. Ltd
plmn("42891" ++ Rest) ->
	{"428", "91", Rest};
%% Mongolia, Unitel
plmn("42888" ++ Rest) ->
	{"428", "88", Rest};
%% Mongolia, Ondo / IN Mobile
plmn("42833" ++ Rest) ->
	{"428", "33", Rest};
%% Montenegro, Monet/T-mobile
plmn("29702" ++ Rest) ->
	{"297", "02", Rest};
%% Montenegro, Mtel
plmn("29703" ++ Rest) ->
	{"297", "03", Rest};
%% Montenegro, Telenor/Promonte GSM
plmn("29701" ++ Rest) ->
	{"297", "01", Rest};
%% Montserrat, Cable & Wireless
plmn("354860" ++ Rest) ->
	{"354", "860", Rest};
%% Morocco, Al Houria Telecom
plmn("60404" ++ Rest) ->
	{"604", "04", Rest};
%% Morocco, Al Houria Telecom
plmn("60499" ++ Rest) ->
	{"604", "99", Rest};
%% Morocco, IAM/Itissallat
plmn("60406" ++ Rest) ->
	{"604", "06", Rest};
%% Morocco, IAM/Itissallat
plmn("60401" ++ Rest) ->
	{"604", "01", Rest};
%% Morocco, INWI/WANA
plmn("60405" ++ Rest) ->
	{"604", "05", Rest};
%% Morocco, INWI/WANA
plmn("60402" ++ Rest) ->
	{"604", "02", Rest};
%% Morocco, Orange/Medi Telecom
plmn("60400" ++ Rest) ->
	{"604", "00", Rest};
%% Mozambique, mCel
plmn("64301" ++ Rest) ->
	{"643", "01", Rest};
%% Mozambique, Movitel
plmn("64303" ++ Rest) ->
	{"643", "03", Rest};
%% Mozambique, Vodacom
plmn("64304" ++ Rest) ->
	{"643", "04", Rest};
%% Myanmar (Burma), Myanmar Post & Teleco.
plmn("41401" ++ Rest) ->
	{"414", "01", Rest};
%% Myanmar (Burma), Mytel (Myanmar
plmn("41409" ++ Rest) ->
	{"414", "09", Rest};
%% Myanmar (Burma), Oreedoo
plmn("41405" ++ Rest) ->
	{"414", "05", Rest};
%% Myanmar (Burma), Telenor
plmn("41406" ++ Rest) ->
	{"414", "06", Rest};
%% Namibia, TN Mobile
plmn("64903" ++ Rest) ->
	{"649", "03", Rest};
%% Namibia, MTC
plmn("64901" ++ Rest) ->
	{"649", "01", Rest};
%% Namibia, Switch/Nam. Telec.
plmn("64902" ++ Rest) ->
	{"649", "02", Rest};
%% Nepal, Ncell
plmn("42902" ++ Rest) ->
	{"429", "02", Rest};
%% Nepal, NT Mobile / Namaste
plmn("42901" ++ Rest) ->
	{"429", "01", Rest};
%% Nepal, Smart Cell
plmn("42904" ++ Rest) ->
	{"429", "04", Rest};
%% Netherlands, 6GMOBILE BV
plmn("20414" ++ Rest) ->
	{"204", "14", Rest};
%% Netherlands, Aspider Solutions
plmn("20423" ++ Rest) ->
	{"204", "23", Rest};
%% Netherlands, Elephant Talk Communications Premium Rate Services Netherlands BV
plmn("20405" ++ Rest) ->
	{"204", "05", Rest};
%% Netherlands, Intercity Mobile Communications BV
plmn("20417" ++ Rest) ->
	{"204", "17", Rest};
%% Netherlands, KPN Telecom B.V.
plmn("20410" ++ Rest) ->
	{"204", "10", Rest};
%% Netherlands, KPN Telecom B.V.
plmn("20408" ++ Rest) ->
	{"204", "08", Rest};
%% Netherlands, KPN Telecom B.V.
plmn("20469" ++ Rest) ->
	{"204", "69", Rest};
%% Netherlands, KPN/Telfort
plmn("20412" ++ Rest) ->
	{"204", "12", Rest};
%% Netherlands, Lancelot BV
plmn("20428" ++ Rest) ->
	{"204", "28", Rest};
%% Netherlands, Lycamobile Ltd
plmn("20409" ++ Rest) ->
	{"204", "09", Rest};
%% Netherlands, Mundio/Vectone Mobile
plmn("20406" ++ Rest) ->
	{"204", "06", Rest};
%% Netherlands, NS Railinfrabeheer B.V.
plmn("20421" ++ Rest) ->
	{"204", "21", Rest};
%% Netherlands, Private Mobility Nederland BV
plmn("20424" ++ Rest) ->
	{"204", "24", Rest};
%% Netherlands, T-Mobile B.V.
plmn("20498" ++ Rest) ->
	{"204", "98", Rest};
%% Netherlands, T-Mobile B.V.
plmn("20416" ++ Rest) ->
	{"204", "16", Rest};
%% Netherlands, T-mobile/former Orange
plmn("20420" ++ Rest) ->
	{"204", "20", Rest};
%% Netherlands, Tele2
plmn("20402" ++ Rest) ->
	{"204", "02", Rest};
%% Netherlands, Teleena Holding BV
plmn("20407" ++ Rest) ->
	{"204", "07", Rest};
%% Netherlands, Unify Mobile
plmn("20468" ++ Rest) ->
	{"204", "68", Rest};
%% Netherlands, UPC Nederland BV
plmn("20418" ++ Rest) ->
	{"204", "18", Rest};
%% Netherlands, Vodafone Libertel
plmn("20404" ++ Rest) ->
	{"204", "04", Rest};
%% Netherlands, Voiceworks Mobile BV
plmn("20403" ++ Rest) ->
	{"204", "03", Rest};
%% Netherlands, Ziggo BV
plmn("20415" ++ Rest) ->
	{"204", "15", Rest};
%% Netherlands Antilles, Cingular Wireless
plmn("362630" ++ Rest) ->
	{"362", "630", Rest};
%% Netherlands Antilles, TELCELL GSM
plmn("36251" ++ Rest) ->
	{"362", "51", Rest};
%% Netherlands Antilles, SETEL GSM
plmn("36291" ++ Rest) ->
	{"362", "91", Rest};
%% Netherlands Antilles, UTS Wireless
%% Curacao, EOCG Wireless NV
plmn("362951" ++ Rest) ->
	{"362", "951", Rest};
%% New Caledonia, OPT Mobilis
plmn("54601" ++ Rest) ->
	{"546", "01", Rest};
%% New Zealand, 2degrees
plmn("53028" ++ Rest) ->
	{"530", "28", Rest};
%% New Zealand, Spark/NZ Telecom
plmn("53005" ++ Rest) ->
	{"530", "05", Rest};
%% New Zealand, Spark/NZ Telecom
plmn("53002" ++ Rest) ->
	{"530", "02", Rest};
%% New Zealand, Telstra
plmn("53004" ++ Rest) ->
	{"530", "04", Rest};
%% New Zealand, Two Degrees Mobile Ltd
plmn("53024" ++ Rest) ->
	{"530", "24", Rest};
%% New Zealand, Vodafone
plmn("53001" ++ Rest) ->
	{"530", "01", Rest};
%% New Zealand, Walker Wireless Ltd.
plmn("53003" ++ Rest) ->
	{"530", "03", Rest};
%% Nicaragua, Empresa Nicaraguense de Telecomunicaciones SA (ENITEL)
plmn("71021" ++ Rest) ->
	{"710", "21", Rest};
%% Nicaragua, Movistar
plmn("71030" ++ Rest) ->
	{"710", "30", Rest};
%% Nicaragua, Claro
plmn("71073" ++ Rest) ->
	{"710", "73", Rest};
%% Niger, MOOV/TeleCel
plmn("61403" ++ Rest) ->
	{"614", "03", Rest};
%% Niger, Orange
plmn("61404" ++ Rest) ->
	{"614", "04", Rest};
%% Niger, Sahelcom
plmn("61401" ++ Rest) ->
	{"614", "01", Rest};
%% Niger, Airtel/Zain/CelTel
plmn("61402" ++ Rest) ->
	{"614", "02", Rest};
%% Nigeria, Airtel/ZAIN/Econet
plmn("62120" ++ Rest) ->
	{"621", "20", Rest};
%% Nigeria, ETISALAT
plmn("62160" ++ Rest) ->
	{"621", "60", Rest};
%% Nigeria, Glo Mobile
plmn("62150" ++ Rest) ->
	{"621", "50", Rest};
%% Nigeria, M-Tel/Nigeria Telecom. Ltd.
plmn("62140" ++ Rest) ->
	{"621", "40", Rest};
%% Nigeria, MTN
plmn("62130" ++ Rest) ->
	{"621", "30", Rest};
%% Nigeria, Starcomms
plmn("62199" ++ Rest) ->
	{"621", "99", Rest};
%% Nigeria, Visafone
plmn("62125" ++ Rest) ->
	{"621", "25", Rest};
%% Nigeria, Visafone
plmn("62101" ++ Rest) ->
	{"621", "01", Rest};
%% Niue, Niue Telecom
plmn("55501" ++ Rest) ->
	{"555", "01", Rest};
%% Norway, Com4 AS
plmn("24209" ++ Rest) ->
	{"242", "09", Rest};
%% Norway, ICE Nordisk Mobiltelefon AS
plmn("24214" ++ Rest) ->
	{"242", "14", Rest};
%% Norway, Jernbaneverket (GSM-R)
plmn("24221" ++ Rest) ->
	{"242", "21", Rest};
%% Norway, Jernbaneverket (GSM-R)
plmn("24220" ++ Rest) ->
	{"242", "20", Rest};
%% Norway, Lycamobile Ltd
plmn("24223" ++ Rest) ->
	{"242", "23", Rest};
%% Norway, Telia/Netcom
plmn("24202" ++ Rest) ->
	{"242", "02", Rest};
%% Norway, Telia/Network Norway AS
plmn("24205" ++ Rest) ->
	{"242", "05", Rest};
%% Norway, Telia/Network Norway AS
plmn("24222" ++ Rest) ->
	{"242", "22", Rest};
%% Norway, ICE Nordisk Mobiltelefon AS
plmn("24206" ++ Rest) ->
	{"242", "06", Rest};
%% Norway, TDC Mobil A/S
plmn("24208" ++ Rest) ->
	{"242", "08", Rest};
%% Norway, Tele2
plmn("24204" ++ Rest) ->
	{"242", "04", Rest};
%% Norway, Telenor
plmn("24212" ++ Rest) ->
	{"242", "12", Rest};
%% Norway, Telenor
plmn("24201" ++ Rest) ->
	{"242", "01", Rest};
%% Norway, Teletopia
plmn("24203" ++ Rest) ->
	{"242", "03", Rest};
%% Norway, Ventelo AS
plmn("24217" ++ Rest) ->
	{"242", "17", Rest};
%% Norway, Ventelo AS
plmn("24207" ++ Rest) ->
	{"242", "07", Rest};
%% Oman, Nawras
plmn("42203" ++ Rest) ->
	{"422", "03", Rest};
%% Oman, Oman Mobile/GTO
plmn("42202" ++ Rest) ->
	{"422", "02", Rest};
%% Pakistan, Instaphone
plmn("41008" ++ Rest) ->
	{"410", "08", Rest};
%% Pakistan, Mobilink
plmn("41001" ++ Rest) ->
	{"410", "01", Rest};
%% Pakistan, Telenor
plmn("41006" ++ Rest) ->
	{"410", "06", Rest};
%% Pakistan, UFONE/PAKTel
plmn("41003" ++ Rest) ->
	{"410", "03", Rest};
%% Pakistan, Warid Telecom
plmn("41007" ++ Rest) ->
	{"410", "07", Rest};
%% Pakistan, ZONG/CMPak
plmn("41004" ++ Rest) ->
	{"410", "04", Rest};
%% Palau (Republic of), Palau Mobile Corp. (PMC) (Palau
plmn("55280" ++ Rest) ->
	{"552", "80", Rest};
%% Palau (Republic of), Palau National Communications Corp. (PNCC) (Palau
plmn("55201" ++ Rest) ->
	{"552", "01", Rest};
%% Palau (Republic of), PECI/PalauTel (Palau
plmn("55202" ++ Rest) ->
	{"552", "02", Rest};
%% Palestinian Territory, Jawwal
plmn("42505" ++ Rest) ->
	{"425", "05", Rest};
%% Palestinian Territory, Wataniya Mobile
plmn("42506" ++ Rest) ->
	{"425", "06", Rest};
%% Panama, Cable & W./Mas Movil
plmn("71401" ++ Rest) ->
	{"714", "01", Rest};
%% Panama, Claro
plmn("71403" ++ Rest) ->
	{"714", "03", Rest};
%% Panama, Digicel
plmn("71404" ++ Rest) ->
	{"714", "04", Rest};
%% Panama, Movistar
plmn("71420" ++ Rest) ->
	{"714", "20", Rest};
%% Panama, Movistar
plmn("71402" ++ Rest) ->
	{"714", "02", Rest};
%% Papua New Guinea, Digicel
plmn("53703" ++ Rest) ->
	{"537", "03", Rest};
%% Papua New Guinea, GreenCom PNG Ltd
plmn("53702" ++ Rest) ->
	{"537", "02", Rest};
%% Papua New Guinea, Pacific Mobile
plmn("53701" ++ Rest) ->
	{"537", "01", Rest};
%% Paraguay, Claro/Hutchison
plmn("74402" ++ Rest) ->
	{"744", "02", Rest};
%% Paraguay, Compa
plmn("74403" ++ Rest) ->
	{"744", "03", Rest};
%% Paraguay, Hola/VOX
plmn("74401" ++ Rest) ->
	{"744", "01", Rest};
%% Paraguay, TIM/Nucleo/Personal
plmn("74405" ++ Rest) ->
	{"744", "05", Rest};
%% Paraguay, Tigo/Telecel
plmn("74404" ++ Rest) ->
	{"744", "04", Rest};
%% Peru, Claro /Amer.Mov./TIM
plmn("71620" ++ Rest) ->
	{"716", "20", Rest};
%% Peru, Claro /Amer.Mov./TIM
plmn("71610" ++ Rest) ->
	{"716", "10", Rest};
%% Peru, GlobalStar
plmn("71602" ++ Rest) ->
	{"716", "02", Rest};
%% Peru, GlobalStar
plmn("71601" ++ Rest) ->
	{"716", "01", Rest};
%% Peru, Movistar
plmn("71606" ++ Rest) ->
	{"716", "06", Rest};
%% Peru, Nextel
plmn("71617" ++ Rest) ->
	{"716", "17", Rest};
%% Peru, Nextel
plmn("71607" ++ Rest) ->
	{"716", "07", Rest};
%% Peru, Viettel Mobile
plmn("71615" ++ Rest) ->
	{"716", "15", Rest};
%% Philippines, Fix Line
plmn("51500" ++ Rest) ->
	{"515", "00", Rest};
%% Philippines, Globe Telecom
plmn("51502" ++ Rest) ->
	{"515", "02", Rest};
%% Philippines, Globe Telecom
plmn("51501" ++ Rest) ->
	{"515", "01", Rest};
%% Philippines, Next Mobile
plmn("51588" ++ Rest) ->
	{"515", "88", Rest};
%% Philippines, RED Mobile/Cure
plmn("51518" ++ Rest) ->
	{"515", "18", Rest};
%% Philippines, Smart
plmn("51503" ++ Rest) ->
	{"515", "03", Rest};
%% Philippines, SUN/Digitel
plmn("51505" ++ Rest) ->
	{"515", "05", Rest};
%% Poland, Aero2 SP.
plmn("26017" ++ Rest) ->
	{"260", "17", Rest};
%% Poland, AMD Telecom.
plmn("26018" ++ Rest) ->
	{"260", "18", Rest};
%% Poland, CallFreedom Sp. z o.o.
plmn("26038" ++ Rest) ->
	{"260", "38", Rest};
%% Poland, Cyfrowy POLSAT S.A.
plmn("26012" ++ Rest) ->
	{"260", "12", Rest};
%% Poland, e-Telko
plmn("26008" ++ Rest) ->
	{"260", "08", Rest};
%% Poland, Lycamobile
plmn("26009" ++ Rest) ->
	{"260", "09", Rest};
%% Poland, Mobyland
plmn("26016" ++ Rest) ->
	{"260", "16", Rest};
%% Poland, Mundio Mobile Sp. z o.o.
plmn("26036" ++ Rest) ->
	{"260", "36", Rest};
%% Poland, Play/P4
plmn("26007" ++ Rest) ->
	{"260", "07", Rest};
%% Poland, NORDISK Polska
plmn("26011" ++ Rest) ->
	{"260", "11", Rest};
%% Poland, Orange/IDEA/Centertel
plmn("26005" ++ Rest) ->
	{"260", "05", Rest};
%% Poland, Orange/IDEA/Centertel
plmn("26003" ++ Rest) ->
	{"260", "03", Rest};
%% Poland, PKP Polskie Linie Kolejowe S.A.
plmn("26035" ++ Rest) ->
	{"260", "35", Rest};
%% Poland, Play/P4
plmn("26098" ++ Rest) ->
	{"260", "98", Rest};
%% Poland, Play/P4
plmn("26006" ++ Rest) ->
	{"260", "06", Rest};
%% Poland, Polkomtel/Plus
plmn("26001" ++ Rest) ->
	{"260", "01", Rest};
%% Poland, Sferia
plmn("26013" ++ Rest) ->
	{"260", "13", Rest};
%% Poland, Sferia
plmn("26010" ++ Rest) ->
	{"260", "10", Rest};
%% Poland, Sferia
plmn("26014" ++ Rest) ->
	{"260", "14", Rest};
%% Poland, T-Mobile/ERA
plmn("26034" ++ Rest) ->
	{"260", "34", Rest};
%% Poland, T-Mobile/ERA
plmn("26002" ++ Rest) ->
	{"260", "02", Rest};
%% Poland, Aero2
plmn("26015" ++ Rest) ->
	{"260", "15", Rest};
%% Poland, Aero2
plmn("26004" ++ Rest) ->
	{"260", "04", Rest};
%% Poland, Virgin Mobile
plmn("26045" ++ Rest) ->
	{"260", "45", Rest};
%% Portugal, Lycamobile
plmn("26804" ++ Rest) ->
	{"268", "04", Rest};
%% Portugal, NOS/Optimus
plmn("26803" ++ Rest) ->
	{"268", "03", Rest};
%% Portugal, NOS/Optimus
plmn("26807" ++ Rest) ->
	{"268", "07", Rest};
%% Portugal, MEO/TMN
plmn("26806" ++ Rest) ->
	{"268", "06", Rest};
%% Portugal, Vodafone
plmn("26801" ++ Rest) ->
	{"268", "01", Rest};
%% Puerto Rico, Puerto Rico Telephone Company Inc. (PRTC)
plmn("330110" ++ Rest) ->
	{"330", "110", Rest};
%% Qatar, Ooredoo/Qtel
plmn("42701" ++ Rest) ->
	{"427", "01", Rest};
%% Qatar, Vodafone
plmn("42702" ++ Rest) ->
	{"427", "02", Rest};
%% Reunion, Orange
plmn("64700" ++ Rest) ->
	{"647", "00", Rest};
%% Reunion, Outremer Telecom
plmn("64702" ++ Rest) ->
	{"647", "02", Rest};
%% Reunion, SFR
plmn("64710" ++ Rest) ->
	{"647", "10", Rest};
%% Romania, Telekom Romania
plmn("22603" ++ Rest) ->
	{"226", "03", Rest};
%% Romania, Enigma Systems
plmn("22611" ++ Rest) ->
	{"226", "11", Rest};
%% Romania, Lycamobile
plmn("22616" ++ Rest) ->
	{"226", "16", Rest};
%% Romania, Orange
plmn("22610" ++ Rest) ->
	{"226", "10", Rest};
%% Romania, RCS&RDS Digi Mobile
plmn("22605" ++ Rest) ->
	{"226", "05", Rest};
%% Romania, Romtelecom SA
plmn("22602" ++ Rest) ->
	{"226", "02", Rest};
%% Romania, Telekom Romania
plmn("22606" ++ Rest) ->
	{"226", "06", Rest};
%% Romania, Vodafone
plmn("22601" ++ Rest) ->
	{"226", "01", Rest};
%% Romania, Telekom Romania
plmn("22604" ++ Rest) ->
	{"226", "04", Rest};
%% Russian Federation, Baykal Westcom
plmn("25012" ++ Rest) ->
	{"250", "12", Rest};
%% Russian Federation, BeeLine/VimpelCom
plmn("25099" ++ Rest) ->
	{"250", "99", Rest};
%% Russian Federation, BeeLine/VimpelCom
plmn("25028" ++ Rest) ->
	{"250", "28", Rest};
%% Russian Federation, DTC/Don Telecom
plmn("25010" ++ Rest) ->
	{"250", "10", Rest};
%% Russian Federation, Kuban GSM
plmn("25013" ++ Rest) ->
	{"250", "13", Rest};
%% Russian Federation, MOTIV/LLC Ekaterinburg-2000
plmn("25035" ++ Rest) ->
	{"250", "35", Rest};
%% Russian Federation, Megafon
plmn("25002" ++ Rest) ->
	{"250", "02", Rest};
%% Russian Federation, MTS
plmn("25001" ++ Rest) ->
	{"250", "01", Rest};
%% Russian Federation, NCC
plmn("25003" ++ Rest) ->
	{"250", "03", Rest};
%% Russian Federation, NTC
plmn("25016" ++ Rest) ->
	{"250", "16", Rest};
%% Russian Federation, OJSC Altaysvyaz
plmn("25019" ++ Rest) ->
	{"250", "19", Rest};
%% Russian Federation, Orensot
plmn("25011" ++ Rest) ->
	{"250", "11", Rest};
%% Russian Federation, Printelefone
plmn("25092" ++ Rest) ->
	{"250", "92", Rest};
%% Russian Federation, Sibchallenge
plmn("25004" ++ Rest) ->
	{"250", "04", Rest};
%% Russian Federation, StavTelesot
plmn("25044" ++ Rest) ->
	{"250", "44", Rest};
%% Russian Federation, Tele2/ECC/Volgogr.
plmn("25020" ++ Rest) ->
	{"250", "20", Rest};
%% Russian Federation, Telecom XXL
plmn("25093" ++ Rest) ->
	{"250", "93", Rest};
%% Russian Federation, UralTel
plmn("25039" ++ Rest) ->
	{"250", "39", Rest};
%% Russian Federation, UralTel
plmn("25017" ++ Rest) ->
	{"250", "17", Rest};
%% Russian Federation, Tele2/ECC/Volgogr.
plmn("25005" ++ Rest) ->
	{"250", "05", Rest};
%% Russian Federation, ZAO SMARTS
plmn("25015" ++ Rest) ->
	{"250", "15", Rest};
%% Russian Federation, ZAO SMARTS
plmn("25007" ++ Rest) ->
	{"250", "07", Rest};
%% Rwanda, Airtel
plmn("63514" ++ Rest) ->
	{"635", "14", Rest};
%% Rwanda, MTN/Rwandacell
plmn("63510" ++ Rest) ->
	{"635", "10", Rest};
%% Rwanda, TIGO
plmn("63513" ++ Rest) ->
	{"635", "13", Rest};
%% Saint Kitts and Nevis, Cable & Wireless
plmn("356110" ++ Rest) ->
	{"356", "110", Rest};
%% Saint Kitts and Nevis, Digicel
plmn("35650" ++ Rest) ->
	{"356", "50", Rest};
%% Saint Kitts and Nevis, UTS Cariglobe
plmn("35670" ++ Rest) ->
	{"356", "70", Rest};
%% Saint Lucia, Cable & Wireless
plmn("358110" ++ Rest) ->
	{"358", "110", Rest};
%% Saint Lucia, Cingular Wireless
plmn("35830" ++ Rest) ->
	{"358", "30", Rest};
%% Saint Lucia, Digicel (St Lucia) Limited
plmn("35850" ++ Rest) ->
	{"358", "50", Rest};
%% Samoa, Samoatel Mobile
plmn("54927" ++ Rest) ->
	{"549", "27", Rest};
%% Samoa, Telecom Samoa Cellular Ltd.
plmn("54901" ++ Rest) ->
	{"549", "01", Rest};
%% San Marino, Prima Telecom
plmn("29201" ++ Rest) ->
	{"292", "01", Rest};
%% Sao Tome & Principe, CSTmovel
plmn("62601" ++ Rest) ->
	{"626", "01", Rest};
%% Satellite Networks, AeroMobile
plmn("90114" ++ Rest) ->
	{"901", "14", Rest};
%% Satellite Networks, InMarSAT
plmn("90111" ++ Rest) ->
	{"901", "11", Rest};
%% Satellite Networks, Maritime Communications Partner AS
plmn("90112" ++ Rest) ->
	{"901", "12", Rest};
%% Satellite Networks, Thuraya Satellite
plmn("90105" ++ Rest) ->
	{"901", "05", Rest};
%% Saudi Arabia, Zain
plmn("42007" ++ Rest) ->
	{"420", "07", Rest};
%% Saudi Arabia, Etihad/Etisalat/Mobily
plmn("42003" ++ Rest) ->
	{"420", "03", Rest};
%% Saudi Arabia, Lebara Mobile
plmn("42006" ++ Rest) ->
	{"420", "06", Rest};
%% Saudi Arabia, STC/Al Jawal
plmn("42001" ++ Rest) ->
	{"420", "01", Rest};
%% Saudi Arabia, Virgin Mobile
plmn("42005" ++ Rest) ->
	{"420", "05", Rest};
%% Saudi Arabia, Zain
plmn("42004" ++ Rest) ->
	{"420", "04", Rest};
%% Senegal, Expresso/Sudatel
plmn("60803" ++ Rest) ->
	{"608", "03", Rest};
%% Senegal, Orange/Sonatel
plmn("60801" ++ Rest) ->
	{"608", "01", Rest};
%% Senegal, TIGO/Sentel GSM
plmn("60802" ++ Rest) ->
	{"608", "02", Rest};
%% Serbia, MTS/Telekom Srbija
plmn("22003" ++ Rest) ->
	{"220", "03", Rest};
%% Serbia, Telenor/Mobtel
plmn("22001" ++ Rest) ->
	{"220", "01", Rest};
%% Serbia, Telenor/Mobtel
plmn("22002" ++ Rest) ->
	{"220", "02", Rest};
%% Serbia, VIP Mobile
plmn("22005" ++ Rest) ->
	{"220", "05", Rest};
%% Seychelles, Airtel
plmn("63310" ++ Rest) ->
	{"633", "10", Rest};
%% Seychelles, C&W
plmn("63301" ++ Rest) ->
	{"633", "01", Rest};
%% Seychelles, Smartcom
plmn("63302" ++ Rest) ->
	{"633", "02", Rest};
%% Sierra Leone, Africel
plmn("61903" ++ Rest) ->
	{"619", "03", Rest};
%% Sierra Leone, Orange
plmn("61901" ++ Rest) ->
	{"619", "01", Rest};
%% Sierra Leone, Comium
plmn("61904" ++ Rest) ->
	{"619", "04", Rest};
%% Sierra Leone, Africel
plmn("61905" ++ Rest) ->
	{"619", "05", Rest};
%% Sierra Leone, Tigo/Millicom
plmn("61902" ++ Rest) ->
	{"619", "02", Rest};
%% Sierra Leone, Mobitel
plmn("61925" ++ Rest) ->
	{"619", "25", Rest};
%% Sierra Leone, Qcell
plmn("61907" ++ Rest) ->
	{"619", "07", Rest};
%% Singapore, GRID Communications Pte Ltd
plmn("52512" ++ Rest) ->
	{"525", "12", Rest};
%% Singapore, MobileOne Ltd
plmn("52503" ++ Rest) ->
	{"525", "03", Rest};
%% Singapore, Singtel
plmn("52501" ++ Rest) ->
	{"525", "01", Rest};
%% Singapore, Singtel
plmn("52507" ++ Rest) ->
	{"525", "07", Rest};
%% Singapore, Singtel
plmn("52502" ++ Rest) ->
	{"525", "02", Rest};
%% Singapore, Starhub
plmn("52506" ++ Rest) ->
	{"525", "06", Rest};
%% Singapore, Starhub
plmn("52505" ++ Rest) ->
	{"525", "05", Rest};
%% Slovakia, Swan/4Ka
plmn("23103" ++ Rest) ->
	{"231", "03", Rest};
%% Slovakia, O2
plmn("23106" ++ Rest) ->
	{"231", "06", Rest};
%% Slovakia, Orange
plmn("23101" ++ Rest) ->
	{"231", "01", Rest};
%% Slovakia, Orange
plmn("23105" ++ Rest) ->
	{"231", "05", Rest};
%% Slovakia, Orange
plmn("23115" ++ Rest) ->
	{"231", "15", Rest};
%% Slovakia, T-Mobile
plmn("23102" ++ Rest) ->
	{"231", "02", Rest};
%% Slovakia, T-Mobile
plmn("23104" ++ Rest) ->
	{"231", "04", Rest};
%% Slovakia, Zeleznice Slovenskej republiky (ZSR)
plmn("23199" ++ Rest) ->
	{"231", "99", Rest};
%% Slovenia, Mobitel
plmn("29341" ++ Rest) ->
	{"293", "41", Rest};
%% Slovenia, SI.Mobil
plmn("29340" ++ Rest) ->
	{"293", "40", Rest};
%% Slovenia, Slovenske zeleznice d.o.o.
plmn("29310" ++ Rest) ->
	{"293", "10", Rest};
%% Slovenia, T-2 d.o.o.
plmn("29364" ++ Rest) ->
	{"293", "64", Rest};
%% Slovenia, Telemach/TusMobil/VEGA
plmn("29370" ++ Rest) ->
	{"293", "70", Rest};
%% Solomon Islands, bemobile
plmn("54002" ++ Rest) ->
	{"540", "02", Rest};
%% Solomon Islands, BREEZE
plmn("54010" ++ Rest) ->
	{"540", "10", Rest};
%% Solomon Islands, BREEZE
plmn("54001" ++ Rest) ->
	{"540", "01", Rest};
%% Somalia, Golis
plmn("63730" ++ Rest) ->
	{"637", "30", Rest};
%% Somalia, Hormuud
plmn("63750" ++ Rest) ->
	{"637", "50", Rest};
%% Somalia, HorTel
plmn("63719" ++ Rest) ->
	{"637", "19", Rest};
%% Somalia, Nationlink
plmn("63760" ++ Rest) ->
	{"637", "60", Rest};
%% Somalia, Nationlink
plmn("63710" ++ Rest) ->
	{"637", "10", Rest};
%% Somalia, Somafone
plmn("63704" ++ Rest) ->
	{"637", "04", Rest};
%% Somalia, Somtel
plmn("63782" ++ Rest) ->
	{"637", "82", Rest};
%% Somalia, Somtel
plmn("63771" ++ Rest) ->
	{"637", "71", Rest};
%% Somalia, Telesom
plmn("63701" ++ Rest) ->
	{"637", "01", Rest};
%% South Africa, Telkom/8.ta
plmn("65502" ++ Rest) ->
	{"655", "02", Rest};
%% South Africa, Cape Town Metropolitan
plmn("65521" ++ Rest) ->
	{"655", "21", Rest};
%% South Africa, Cell C
plmn("65507" ++ Rest) ->
	{"655", "07", Rest};
%% South Africa, MTN
plmn("65512" ++ Rest) ->
	{"655", "12", Rest};
%% South Africa, MTN
plmn("65510" ++ Rest) ->
	{"655", "10", Rest};
%% South Africa, Sentech
plmn("65506" ++ Rest) ->
	{"655", "06", Rest};
%% South Africa, Vodacom
plmn("65501" ++ Rest) ->
	{"655", "01", Rest};
%% South Africa, Wireless Business Solutions (Pty) Ltd
plmn("65519" ++ Rest) ->
	{"655", "19", Rest};
%% South Sudan (Republic of), Gemtel Ltd (South Sudan
plmn("65903" ++ Rest) ->
	{"659", "03", Rest};
%% South Sudan (Republic of), MTN South Sudan (South Sudan
plmn("65902" ++ Rest) ->
	{"659", "02", Rest};
%% South Sudan (Republic of), Network of The World Ltd (NOW) (South Sudan
plmn("65904" ++ Rest) ->
	{"659", "04", Rest};
%% South Sudan (Republic of), Zain South Sudan (South Sudan
plmn("65906" ++ Rest) ->
	{"659", "06", Rest};
%% Spain, Lycamobile SL
plmn("21423" ++ Rest) ->
	{"214", "23", Rest};
%% Spain, Digi Spain Telecom SL
plmn("21422" ++ Rest) ->
	{"214", "22", Rest};
%% Spain, BT Espana SAU
plmn("21415" ++ Rest) ->
	{"214", "15", Rest};
%% Spain, Cableuropa SAU (ONO)
plmn("21418" ++ Rest) ->
	{"214", "18", Rest};
%% Spain, Euskaltel SA
plmn("21408" ++ Rest) ->
	{"214", "08", Rest};
%% Spain, fonYou Wireless SL
plmn("21420" ++ Rest) ->
	{"214", "20", Rest};
%% Spain, ION Mobile
plmn("21432" ++ Rest) ->
	{"214", "32", Rest};
%% Spain, Jazz Telecom SAU
plmn("21421" ++ Rest) ->
	{"214", "21", Rest};
%% Spain, Lleida
plmn("21426" ++ Rest) ->
	{"214", "26", Rest};
%% Spain, Lycamobile SL
plmn("21425" ++ Rest) ->
	{"214", "25", Rest};
%% Spain, Movistar
plmn("21407" ++ Rest) ->
	{"214", "07", Rest};
%% Spain, Movistar
plmn("21405" ++ Rest) ->
	{"214", "05", Rest};
%% Spain, Orange
plmn("21403" ++ Rest) ->
	{"214", "03", Rest};
%% Spain, Orange
plmn("21409" ++ Rest) ->
	{"214", "09", Rest};
%% Spain, Orange
plmn("21411" ++ Rest) ->
	{"214", "11", Rest};
%% Spain, R Cable y Telec. Galicia SA
plmn("21417" ++ Rest) ->
	{"214", "17", Rest};
%% Spain, Simyo/KPN
plmn("21419" ++ Rest) ->
	{"214", "19", Rest};
%% Spain, Telecable de Asturias SA
plmn("21416" ++ Rest) ->
	{"214", "16", Rest};
%% Spain, Truphone
plmn("21427" ++ Rest) ->
	{"214", "27", Rest};
%% Spain, Vodafone
plmn("21401" ++ Rest) ->
	{"214", "01", Rest};
%% Spain, Vodafone Enabler Espana SL
plmn("21406" ++ Rest) ->
	{"214", "06", Rest};
%% Spain, Yoigo
plmn("21404" ++ Rest) ->
	{"214", "04", Rest};
%% Sri Lanka, Airtel
plmn("41305" ++ Rest) ->
	{"413", "05", Rest};
%% Sri Lanka, Etisalat/Tigo
plmn("41303" ++ Rest) ->
	{"413", "03", Rest};
%% Sri Lanka, H3G Hutchison
plmn("41308" ++ Rest) ->
	{"413", "08", Rest};
%% Sri Lanka, Mobitel Ltd.
plmn("41301" ++ Rest) ->
	{"413", "01", Rest};
%% Sri Lanka, MTN/Dialog
plmn("41302" ++ Rest) ->
	{"413", "02", Rest};
%% St. Pierre & Miquelon, Ameris
plmn("30801" ++ Rest) ->
	{"308", "01", Rest};
%% St. Vincent & Gren., C & W
plmn("360110" ++ Rest) ->
	{"360", "110", Rest};
%% St. Vincent & Gren., Cingular
plmn("360100" ++ Rest) ->
	{"360", "100", Rest};
%% St. Vincent & Gren., Cingular
plmn("36010" ++ Rest) ->
	{"360", "10", Rest};
%% St. Vincent & Gren., Digicel
plmn("36050" ++ Rest) ->
	{"360", "50", Rest};
%% St. Vincent & Gren., Digicel
plmn("36070" ++ Rest) ->
	{"360", "70", Rest};
%% Sudan, Canar Telecom
plmn("63400" ++ Rest) ->
	{"634", "00", Rest};
%% Sudan, MTN
plmn("63422" ++ Rest) ->
	{"634", "22", Rest};
%% Sudan, MTN
plmn("63402" ++ Rest) ->
	{"634", "02", Rest};
%% Sudan, Sudani One
plmn("63415" ++ Rest) ->
	{"634", "15", Rest};
%% Sudan, Sudani One
plmn("63407" ++ Rest) ->
	{"634", "07", Rest};
%% Sudan, Canar Telecom
plmn("63405" ++ Rest) ->
	{"634", "05", Rest};
%% Sudan, Canar Telecom
plmn("63408" ++ Rest) ->
	{"634", "08", Rest};
%% Sudan, ZAIN/Mobitel
plmn("63406" ++ Rest) ->
	{"634", "06", Rest};
%% Sudan, ZAIN/Mobitel
plmn("63401" ++ Rest) ->
	{"634", "01", Rest};
%% Suriname, Digicel
plmn("74603" ++ Rest) ->
	{"746", "03", Rest};
%% Suriname, Telesur
plmn("74601" ++ Rest) ->
	{"746", "01", Rest};
%% Suriname, Telecommunicatiebedrijf Suriname (TELESUR)
plmn("74602" ++ Rest) ->
	{"746", "02", Rest};
%% Suriname, UNIQA
plmn("74604" ++ Rest) ->
	{"746", "04", Rest};
%% Swaziland, Swazi Mobile
plmn("65302" ++ Rest) ->
	{"653", "02", Rest};
%% Swaziland, Swazi MTN
plmn("65310" ++ Rest) ->
	{"653", "10", Rest};
%% Swaziland, SwaziTelecom
plmn("65301" ++ Rest) ->
	{"653", "01", Rest};
%% Sweden, 42 Telecom AB
plmn("24035" ++ Rest) ->
	{"240", "35", Rest};
%% Sweden, 42 Telecom AB
plmn("24016" ++ Rest) ->
	{"240", "16", Rest};
%% Sweden, Beepsend
plmn("24026" ++ Rest) ->
	{"240", "26", Rest};
%% Sweden, NextGen Mobile Ltd (CardBoardFish)
plmn("24030" ++ Rest) ->
	{"240", "30", Rest};
%% Sweden, CoolTEL Aps
plmn("24028" ++ Rest) ->
	{"240", "28", Rest};
%% Sweden, Digitel Mobile Srl
plmn("24025" ++ Rest) ->
	{"240", "25", Rest};
%% Sweden, Eu Tel AB
plmn("24022" ++ Rest) ->
	{"240", "22", Rest};
%% Sweden, Fogg Mobile AB
plmn("24027" ++ Rest) ->
	{"240", "27", Rest};
%% Sweden, Generic Mobile Systems Sweden AB
plmn("24018" ++ Rest) ->
	{"240", "18", Rest};
%% Sweden, Gotalandsnatet AB
plmn("24017" ++ Rest) ->
	{"240", "17", Rest};
%% Sweden, H3G Access AB
plmn("24004" ++ Rest) ->
	{"240", "04", Rest};
%% Sweden, H3G Access AB
plmn("24002" ++ Rest) ->
	{"240", "02", Rest};
%% Sweden, ID Mobile
plmn("24036" ++ Rest) ->
	{"240", "36", Rest};
%% Sweden, Infobip Ltd.
plmn("24023" ++ Rest) ->
	{"240", "23", Rest};
%% Sweden, Lindholmen Science Park AB
plmn("24011" ++ Rest) ->
	{"240", "11", Rest};
%% Sweden, Lycamobile Ltd
plmn("24012" ++ Rest) ->
	{"240", "12", Rest};
%% Sweden, Mercury International Carrier Services
plmn("24029" ++ Rest) ->
	{"240", "29", Rest};
%% Sweden, Mundio Mobile (Sweden) Ltd
plmn("24019" ++ Rest) ->
	{"240", "19", Rest};
%% Sweden, Netett Sverige AB
plmn("24003" ++ Rest) ->
	{"240", "03", Rest};
%% Sweden, Spring Mobil AB
plmn("24010" ++ Rest) ->
	{"240", "10", Rest};
%% Sweden, Svenska UMTS-N
plmn("24005" ++ Rest) ->
	{"240", "05", Rest};
%% Sweden, TDC Sverige AB
plmn("24014" ++ Rest) ->
	{"240", "14", Rest};
%% Sweden, Tele2 Sverige AB
plmn("24007" ++ Rest) ->
	{"240", "07", Rest};
%% Sweden, Telenor (Vodafone)
plmn("24024" ++ Rest) ->
	{"240", "24", Rest};
%% Sweden, Telenor (Vodafone)
plmn("24008" ++ Rest) ->
	{"240", "08", Rest};
%% Sweden, Telenor (Vodafone)
plmn("24006" ++ Rest) ->
	{"240", "06", Rest};
%% Sweden, Telia Mobile
plmn("24001" ++ Rest) ->
	{"240", "01", Rest};
%% Sweden, Ventelo Sverige AB
plmn("24013" ++ Rest) ->
	{"240", "13", Rest};
%% Sweden, Wireless Maingate AB
plmn("24020" ++ Rest) ->
	{"240", "20", Rest};
%% Sweden, Wireless Maingate Nordic AB
plmn("24015" ++ Rest) ->
	{"240", "15", Rest};
%% Switzerland, BebbiCell AG
plmn("22851" ++ Rest) ->
	{"228", "51", Rest};
%% Switzerland, Beeone
plmn("22858" ++ Rest) ->
	{"228", "58", Rest};
%% Switzerland, Comfone AG
plmn("22809" ++ Rest) ->
	{"228", "09", Rest};
%% Switzerland, Comfone AG
plmn("22805" ++ Rest) ->
	{"228", "05", Rest};
%% Switzerland, TDC Sunrise
plmn("22807" ++ Rest) ->
	{"228", "07", Rest};
%% Switzerland, Lycamobile AG
plmn("22854" ++ Rest) ->
	{"228", "54", Rest};
%% Switzerland, Mundio Mobile AG
plmn("22852" ++ Rest) ->
	{"228", "52", Rest};
%% Switzerland, Salt/Orange
plmn("22803" ++ Rest) ->
	{"228", "03", Rest};
%% Switzerland, Swisscom
plmn("22801" ++ Rest) ->
	{"228", "01", Rest};
%% Switzerland, TDC Sunrise
plmn("22812" ++ Rest) ->
	{"228", "12", Rest};
%% Switzerland, TDC Sunrise
plmn("22802" ++ Rest) ->
	{"228", "02", Rest};
%% Switzerland, TDC Sunrise
plmn("22808" ++ Rest) ->
	{"228", "08", Rest};
%% Switzerland, upc cablecom GmbH
plmn("22853" ++ Rest) ->
	{"228", "53", Rest};
%% Syrian Arab Republic, MTN/Spacetel
plmn("41702" ++ Rest) ->
	{"417", "02", Rest};
%% Syrian Arab Republic, Syriatel Holdings
plmn("41709" ++ Rest) ->
	{"417", "09", Rest};
%% Syrian Arab Republic, Syriatel Holdings
plmn("41701" ++ Rest) ->
	{"417", "01", Rest};
%% Taiwan, ACeS Taiwan - ACeS Taiwan Telecommunications Co Ltd
plmn("46668" ++ Rest) ->
	{"466", "68", Rest};
%% Taiwan, Asia Pacific Telecom Co. Ltd (APT)
plmn("46605" ++ Rest) ->
	{"466", "05", Rest};
%% Taiwan, Chunghwa Telecom LDM
plmn("46692" ++ Rest) ->
	{"466", "92", Rest};
%% Taiwan, Chunghwa Telecom LDM
plmn("46611" ++ Rest) ->
	{"466", "11", Rest};
%% Taiwan, Far EasTone
plmn("46607" ++ Rest) ->
	{"466", "07", Rest};
%% Taiwan, Far EasTone
plmn("46606" ++ Rest) ->
	{"466", "06", Rest};
%% Taiwan, Far EasTone
plmn("46603" ++ Rest) ->
	{"466", "03", Rest};
%% Taiwan, Far EasTone
plmn("46602" ++ Rest) ->
	{"466", "02", Rest};
%% Taiwan, Far EasTone
plmn("46601" ++ Rest) ->
	{"466", "01", Rest};
%% Taiwan, Global Mobile Corp.
plmn("46610" ++ Rest) ->
	{"466", "10", Rest};
%% Taiwan, International Telecom Co. Ltd (FITEL)
plmn("46656" ++ Rest) ->
	{"466", "56", Rest};
%% Taiwan, KG Telecom
plmn("46688" ++ Rest) ->
	{"466", "88", Rest};
%% Taiwan, T-Star/VIBO
plmn("46690" ++ Rest) ->
	{"466", "90", Rest};
%% Taiwan, Taiwan Cellular
plmn("46697" ++ Rest) ->
	{"466", "97", Rest};
%% Taiwan, Mobitai
plmn("46693" ++ Rest) ->
	{"466", "93", Rest};
%% Taiwan, TransAsia
plmn("46699" ++ Rest) ->
	{"466", "99", Rest};
%% Taiwan, T-Star/VIBO
plmn("46689" ++ Rest) ->
	{"466", "89", Rest};
%% Taiwan, VMAX Telecom Co. Ltd
plmn("46609" ++ Rest) ->
	{"466", "09", Rest};
%% Tajikistan, Babilon-M
plmn("43604" ++ Rest) ->
	{"436", "04", Rest};
%% Tajikistan, Bee Line
plmn("43605" ++ Rest) ->
	{"436", "05", Rest};
%% Tajikistan, CJSC Indigo Tajikistan
plmn("43602" ++ Rest) ->
	{"436", "02", Rest};
%% Tajikistan, Tcell/JC Somoncom
plmn("43612" ++ Rest) ->
	{"436", "12", Rest};
%% Tajikistan, Megafon
plmn("43603" ++ Rest) ->
	{"436", "03", Rest};
%% Tajikistan, Tcell/JC Somoncom
plmn("43601" ++ Rest) ->
	{"436", "01", Rest};
%% Tanzania, Benson Informatics Ltd
plmn("64008" ++ Rest) ->
	{"640", "08", Rest};
%% Tanzania, Dovetel (T) Ltd
plmn("64006" ++ Rest) ->
	{"640", "06", Rest};
%% Tanzania, Halotel/Viettel Ltd
plmn("64009" ++ Rest) ->
	{"640", "09", Rest};
%% Tanzania, Smile Communications Tanzania Ltd
plmn("64011" ++ Rest) ->
	{"640", "11", Rest};
%% Tanzania, Tanzania Telecommunications Company Ltd (TTCL)
plmn("64007" ++ Rest) ->
	{"640", "07", Rest};
%% Tanzania, TIGO/MIC
plmn("64002" ++ Rest) ->
	{"640", "02", Rest};
%% Tanzania, Tri Telecomm. Ltd.
plmn("64001" ++ Rest) ->
	{"640", "01", Rest};
%% Tanzania, Vodacom Ltd
plmn("64004" ++ Rest) ->
	{"640", "04", Rest};
%% Tanzania, Airtel/ZAIN/Celtel
plmn("64005" ++ Rest) ->
	{"640", "05", Rest};
%% Tanzania, Zantel/Zanzibar Telecom
plmn("64003" ++ Rest) ->
	{"640", "03", Rest};
%% Thailand, ACeS Thailand - ACeS Regional Services Co Ltd
plmn("52020" ++ Rest) ->
	{"520", "20", Rest};
%% Thailand, ACT Mobile
plmn("52015" ++ Rest) ->
	{"520", "15", Rest};
%% Thailand, AIS/Advanced Info Service
plmn("52003" ++ Rest) ->
	{"520", "03", Rest};
%% Thailand, AIS/Advanced Info Service
plmn("52001" ++ Rest) ->
	{"520", "01", Rest};
%% Thailand, Digital Phone Co.
plmn("52023" ++ Rest) ->
	{"520", "23", Rest};
%% Thailand, Hutch/CAT CDMA
plmn("52000" ++ Rest) ->
	{"520", "00", Rest};
%% Thailand, Total Access (DTAC)
plmn("52005" ++ Rest) ->
	{"520", "05", Rest};
%% Thailand, Total Access (DTAC)
plmn("52018" ++ Rest) ->
	{"520", "18", Rest};
%% Thailand, True Move/Orange
plmn("52099" ++ Rest) ->
	{"520", "99", Rest};
%% Thailand, True Move/Orange
plmn("52004" ++ Rest) ->
	{"520", "04", Rest};
%% Timor-Leste, Telin/ Telkomcel
plmn("51401" ++ Rest) ->
	{"514", "01", Rest};
%% Timor-Leste, Timor Telecom
plmn("51402" ++ Rest) ->
	{"514", "02", Rest};
%% Togo, Telecel/MOOV
plmn("61502" ++ Rest) ->
	{"615", "02", Rest};
%% Togo, Telecel/MOOV
plmn("61503" ++ Rest) ->
	{"615", "03", Rest};
%% Togo, Togo Telecom/TogoCELL
plmn("61501" ++ Rest) ->
	{"615", "01", Rest};
%% Tonga, Digicel
plmn("53988" ++ Rest) ->
	{"539", "88", Rest};
%% Tonga, Shoreline Communication
plmn("53943" ++ Rest) ->
	{"539", "43", Rest};
%% Tonga, Tonga Communications
plmn("53901" ++ Rest) ->
	{"539", "01", Rest};
%% Trinidad and Tobago, Bmobile/TSTT
plmn("37412" ++ Rest) ->
	{"374", "12", Rest};
%% Trinidad and Tobago, Digicel
plmn("374130" ++ Rest) ->
	{"374", "130", Rest};
%% Trinidad and Tobago, LaqTel Ltd.
plmn("374140" ++ Rest) ->
	{"374", "140", Rest};
%% Tunisia, Orange
plmn("60501" ++ Rest) ->
	{"605", "01", Rest};
%% Tunisia, Oreedo/Orascom
plmn("60503" ++ Rest) ->
	{"605", "03", Rest};
%% Tunisia, TuniCell/Tunisia Telecom
plmn("60506" ++ Rest) ->
	{"605", "06", Rest};
%% Tunisia, TuniCell/Tunisia Telecom
plmn("60502" ++ Rest) ->
	{"605", "02", Rest};
%% Turkey, AVEA/Aria
plmn("28603" ++ Rest) ->
	{"286", "03", Rest};
%% Turkey, AVEA/Aria
plmn("28604" ++ Rest) ->
	{"286", "04", Rest};
%% Turkey, Turkcell
plmn("28601" ++ Rest) ->
	{"286", "01", Rest};
%% Turkey, Vodafone-Telsim
plmn("28602" ++ Rest) ->
	{"286", "02", Rest};
%% Turkmenistan, MTS/Barash Communication
plmn("43801" ++ Rest) ->
	{"438", "01", Rest};
%% Turkmenistan, Altyn Asyr/TM-Cell
plmn("43802" ++ Rest) ->
	{"438", "02", Rest};
%% Turks and Caicos Islands, Cable & Wireless (TCI) Ltd
plmn("376350" ++ Rest) ->
	{"376", "350", Rest};
%% "Virgin Islands,  U.S.",Digicel
%% Turks and Caicos Islands, Digicel TCI Ltd
plmn("37650" ++ Rest) ->
	{"376", "50", Rest};
%% Turks and Caicos Islands, IslandCom Communications Ltd.
plmn("376352" ++ Rest) ->
	{"376", "352", Rest};
%% Tuvalu, Tuvalu Telecommunication Corporation (TTC)
plmn("55301" ++ Rest) ->
	{"553", "01", Rest};
%% Uganda, Airtel/Celtel
plmn("64101" ++ Rest) ->
	{"641", "01", Rest};
%% Uganda, i-Tel Ltd
plmn("64166" ++ Rest) ->
	{"641", "66", Rest};
%% Uganda, K2 Telecom Ltd
plmn("64130" ++ Rest) ->
	{"641", "30", Rest};
%% Uganda, MTN Ltd.
plmn("64110" ++ Rest) ->
	{"641", "10", Rest};
%% Uganda, Orange
plmn("64114" ++ Rest) ->
	{"641", "14", Rest};
%% Uganda, Smile Communications Uganda Ltd
plmn("64133" ++ Rest) ->
	{"641", "33", Rest};
%% Uganda, Suretelecom Uganda Ltd
plmn("64118" ++ Rest) ->
	{"641", "18", Rest};
%% Uganda, Uganda Telecom Ltd.
plmn("64111" ++ Rest) ->
	{"641", "11", Rest};
%% Uganda, Airtel/Warid
plmn("64122" ++ Rest) ->
	{"641", "22", Rest};
%% Ukraine, Astelit/LIFE
plmn("25506" ++ Rest) ->
	{"255", "06", Rest};
%% Ukraine, Golden Telecom
plmn("25505" ++ Rest) ->
	{"255", "05", Rest};
%% Ukraine, Golden Telecom
plmn("25539" ++ Rest) ->
	{"255", "39", Rest};
%% Ukraine, Intertelecom Ltd (IT)
plmn("25504" ++ Rest) ->
	{"255", "04", Rest};
%% Ukraine, KyivStar
plmn("25567" ++ Rest) ->
	{"255", "67", Rest};
%% Ukraine, KyivStar
plmn("25503" ++ Rest) ->
	{"255", "03", Rest};
%% Ukraine, Phoenix
plmn("25599" ++ Rest) ->
	{"255", "99", Rest};
%% Ukraine, Telesystems Of Ukraine CJSC (TSU)
plmn("25521" ++ Rest) ->
	{"255", "21", Rest};
%% Ukraine, TriMob LLC
plmn("25507" ++ Rest) ->
	{"255", "07", Rest};
%% Ukraine, Vodafone/MTS
plmn("25550" ++ Rest) ->
	{"255", "50", Rest};
%% Ukraine, Beeline
plmn("25502" ++ Rest) ->
	{"255", "02", Rest};
%% Ukraine, Vodafone/MTS
plmn("25501" ++ Rest) ->
	{"255", "01", Rest};
%% Ukraine, Beeline
plmn("25568" ++ Rest) ->
	{"255", "68", Rest};
%% United Arab Emirates, DU
plmn("42403" ++ Rest) ->
	{"424", "03", Rest};
%% United Arab Emirates, Etisalat
plmn("43002" ++ Rest) ->
	{"430", "02", Rest};
%% United Arab Emirates, Etisalat
plmn("42402" ++ Rest) ->
	{"424", "02", Rest};
%% United Arab Emirates, Etisalat
plmn("43102" ++ Rest) ->
	{"431", "02", Rest};
%% United Kingdom, Airtel/Vodafone
plmn("23403" ++ Rest) ->
	{"234", "03", Rest};
%% United Kingdom, BT Group
plmn("23400" ++ Rest) ->
	{"234", "00", Rest};
%% United Kingdom, BT Group
plmn("23476" ++ Rest) ->
	{"234", "76", Rest};
%% United Kingdom, BT Group
plmn("23477" ++ Rest) ->
	{"234", "77", Rest};
%% United Kingdom, Cable and Wireless
plmn("23492" ++ Rest) ->
	{"234", "92", Rest};
%% United Kingdom, Cable and Wireless
plmn("23407" ++ Rest) ->
	{"234", "07", Rest};
%% United Kingdom, Cable and Wireless Isle of Man
plmn("23436" ++ Rest) ->
	{"234", "36", Rest};
%% United Kingdom, Cloud9/wire9 Tel.
plmn("23418" ++ Rest) ->
	{"234", "18", Rest};
%% United Kingdom, Everyth. Ev.wh.
plmn("23502" ++ Rest) ->
	{"235", "02", Rest};
%% United Kingdom, FIX Line
plmn("234999" ++ Rest) ->
	{"234", "999", Rest};
%% United Kingdom, FlexTel
plmn("23417" ++ Rest) ->
	{"234", "17", Rest};
%% United Kingdom, Guernsey Telecoms
plmn("23455" ++ Rest) ->
	{"234", "55", Rest};
%% United Kingdom, HaySystems
plmn("23414" ++ Rest) ->
	{"234", "14", Rest};
%% United Kingdom, H3G Hutchinson
plmn("23420" ++ Rest) ->
	{"234", "20", Rest};
%% United Kingdom, H3G Hutchinson
plmn("23494" ++ Rest) ->
	{"234", "94", Rest};
%% United Kingdom, Inquam Telecom Ltd
plmn("23475" ++ Rest) ->
	{"234", "75", Rest};
%% United Kingdom, Jersey Telecom
plmn("23450" ++ Rest) ->
	{"234", "50", Rest};
%% United Kingdom, JSC Ingenicum
plmn("23435" ++ Rest) ->
	{"234", "35", Rest};
%% United Kingdom, Lycamobile
plmn("23426" ++ Rest) ->
	{"234", "26", Rest};
%% United Kingdom, Manx Telecom
plmn("23458" ++ Rest) ->
	{"234", "58", Rest};
%% United Kingdom, Mapesbury C. Ltd
plmn("23401" ++ Rest) ->
	{"234", "01", Rest};
%% United Kingdom, Marthon Telecom
plmn("23428" ++ Rest) ->
	{"234", "28", Rest};
%% United Kingdom, O2 Ltd.
plmn("23410" ++ Rest) ->
	{"234", "10", Rest};
%% United Kingdom, O2 Ltd.
plmn("23402" ++ Rest) ->
	{"234", "02", Rest};
%% United Kingdom, O2 Ltd.
plmn("23411" ++ Rest) ->
	{"234", "11", Rest};
%% United Kingdom, OnePhone
plmn("23408" ++ Rest) ->
	{"234", "08", Rest};
%% United Kingdom, Opal Telecom
plmn("23416" ++ Rest) ->
	{"234", "16", Rest};
%% United Kingdom, Everyth. Ev.wh./Orange
plmn("23433" ++ Rest) ->
	{"234", "33", Rest};
%% United Kingdom, Everyth. Ev.wh./Orange
plmn("23434" ++ Rest) ->
	{"234", "34", Rest};
%% United Kingdom, PMN/Teleware
plmn("23419" ++ Rest) ->
	{"234", "19", Rest};
%% United Kingdom, Railtrack Plc
plmn("23412" ++ Rest) ->
	{"234", "12", Rest};
%% United Kingdom, Routotelecom
plmn("23422" ++ Rest) ->
	{"234", "22", Rest};
%% United Kingdom, Sky UK Limited
plmn("23457" ++ Rest) ->
	{"234", "57", Rest};
%% United Kingdom, Stour Marine
plmn("23424" ++ Rest) ->
	{"234", "24", Rest};
%% United Kingdom, Synectiv Ltd.
plmn("23437" ++ Rest) ->
	{"234", "37", Rest};
%% United Kingdom, Everyth. Ev.wh./T-Mobile
plmn("23431" ++ Rest) ->
	{"234", "31", Rest};
%% United Kingdom, Everyth. Ev.wh./T-Mobile
plmn("23430" ++ Rest) ->
	{"234", "30", Rest};
%% United Kingdom, Everyth. Ev.wh./T-Mobile
plmn("23432" ++ Rest) ->
	{"234", "32", Rest};
%% United Kingdom, Vodafone
plmn("23427" ++ Rest) ->
	{"234", "27", Rest};
%% United Kingdom, Tismi
plmn("23409" ++ Rest) ->
	{"234", "09", Rest};
%% United Kingdom, Truphone
plmn("23425" ++ Rest) ->
	{"234", "25", Rest};
%% United Kingdom, Jersey Telecom
plmn("23451" ++ Rest) ->
	{"234", "51", Rest};
%% United Kingdom, Vectofone Mobile Wifi
plmn("23423" ++ Rest) ->
	{"234", "23", Rest};
%% United Kingdom, Virgin Mobile
plmn("23438" ++ Rest) ->
	{"234", "38", Rest};
%% United Kingdom, Vodafone
plmn("23491" ++ Rest) ->
	{"234", "91", Rest};
%% United Kingdom, Vodafone
plmn("23415" ++ Rest) ->
	{"234", "15", Rest};
%% United Kingdom, Vodafone
plmn("23489" ++ Rest) ->
	{"234", "89", Rest};
%% United Kingdom, Wave Telecom Ltd
plmn("23478" ++ Rest) ->
	{"234", "78", Rest};
%% United States,
plmn("310880" ++ Rest) ->
	{"310", "880", Rest};
%% United States, Aeris Comm. Inc.
plmn("310850" ++ Rest) ->
	{"310", "850", Rest};
%% United States,
plmn("310640" ++ Rest) ->
	{"310", "640", Rest};
%% United States, Airtel Wireless LLC
plmn("310510" ++ Rest) ->
	{"310", "510", Rest};
%% United States, Unknown
plmn("310190" ++ Rest) ->
	{"310", "190", Rest};
%% United States, Allied Wireless Communications Corporation
plmn("31290" ++ Rest) ->
	{"312", "90", Rest};
%% United States,
plmn("311130" ++ Rest) ->
	{"311", "130", Rest};
%% United States, Arctic Slope Telephone Association Cooperative Inc.
plmn("310710" ++ Rest) ->
	{"310", "710", Rest};
%% United States, AT&T Wireless Inc.
plmn("310150" ++ Rest) ->
	{"310", "150", Rest};
%% United States, AT&T Wireless Inc.
plmn("310680" ++ Rest) ->
	{"310", "680", Rest};
%% United States, AT&T Wireless Inc.
plmn("310560" ++ Rest) ->
	{"310", "560", Rest};
%% United States, AT&T Wireless Inc.
plmn("310410" ++ Rest) ->
	{"310", "410", Rest};
%% United States, AT&T Wireless Inc.
plmn("310380" ++ Rest) ->
	{"310", "380", Rest};
%% United States, AT&T Wireless Inc.
plmn("310170" ++ Rest) ->
	{"310", "170", Rest};
%% United States, AT&T Wireless Inc.
plmn("310980" ++ Rest) ->
	{"310", "980", Rest};
%% United States, Bluegrass Wireless LLC
plmn("311810" ++ Rest) ->
	{"311", "810", Rest};
%% United States, Bluegrass Wireless LLC
plmn("311800" ++ Rest) ->
	{"311", "800", Rest};
%% United States, Bluegrass Wireless LLC
plmn("311440" ++ Rest) ->
	{"311", "440", Rest};
%% United States, Cable & Communications Corp.
plmn("310900" ++ Rest) ->
	{"310", "900", Rest};
%% United States, California RSA No. 3 Limited Partnership
plmn("311590" ++ Rest) ->
	{"311", "590", Rest};
%% United States, Cambridge Telephone Company Inc.
plmn("311500" ++ Rest) ->
	{"311", "500", Rest};
%% United States, Caprock Cellular Ltd.
plmn("310830" ++ Rest) ->
	{"310", "830", Rest};
%% United States, Verizon Wireless
plmn("311271" ++ Rest) ->
	{"311", "271", Rest};
%% United States, Verizon Wireless
plmn("311287" ++ Rest) ->
	{"311", "287", Rest};
%% United States, Verizon Wireless
plmn("311276" ++ Rest) ->
	{"311", "276", Rest};
%% United States, Verizon Wireless
plmn("311481" ++ Rest) ->
	{"311", "481", Rest};
%% United States, Verizon Wireless
plmn("311281" ++ Rest) ->
	{"311", "281", Rest};
%% United States, Verizon Wireless
plmn("311486" ++ Rest) ->
	{"311", "486", Rest};
%% United States, Verizon Wireless
plmn("311270" ++ Rest) ->
	{"311", "270", Rest};
%% United States, Verizon Wireless
plmn("311286" ++ Rest) ->
	{"311", "286", Rest};
%% United States, Verizon Wireless
plmn("311275" ++ Rest) ->
	{"311", "275", Rest};
%% United States, Verizon Wireless
plmn("311480" ++ Rest) ->
	{"311", "480", Rest};
%% United States, Verizon Wireless
%% United States, Sprint Spectrum
plmn("31012" ++ Rest) ->
	{"310", "12", Rest};
%% United States, Verizon Wireless
plmn("311280" ++ Rest) ->
	{"311", "280", Rest};
%% United States, Verizon Wireless
plmn("311485" ++ Rest) ->
	{"311", "485", Rest};
%% United States, Verizon Wireless
plmn("311110" ++ Rest) ->
	{"311", "110", Rest};
%% United States, Verizon Wireless
plmn("311285" ++ Rest) ->
	{"311", "285", Rest};
%% United States, Verizon Wireless
plmn("311274" ++ Rest) ->
	{"311", "274", Rest};
%% United States, Verizon Wireless
plmn("311390" ++ Rest) ->
	{"311", "390", Rest};
%% United States, Verizon Wireless
%% United States, Plateau Telecommunications Inc.
plmn("31010" ++ Rest) ->
	{"310", "10", Rest};
%% United States, Verizon Wireless
plmn("311279" ++ Rest) ->
	{"311", "279", Rest};
%% United States, Verizon Wireless
plmn("311484" ++ Rest) ->
	{"311", "484", Rest};
%% United States, Verizon Wireless
plmn("310910" ++ Rest) ->
	{"310", "910", Rest};
%% United States, Verizon Wireless
plmn("311284" ++ Rest) ->
	{"311", "284", Rest};
%% United States, Verizon Wireless
plmn("311489" ++ Rest) ->
	{"311", "489", Rest};
%% United States, Verizon Wireless
plmn("311273" ++ Rest) ->
	{"311", "273", Rest};
%% United States, Verizon Wireless
plmn("311289" ++ Rest) ->
	{"311", "289", Rest};
%% United States, Verizon Wireless
plmn("31004" ++ Rest) ->
	{"310", "04", Rest};
%% United States, Verizon Wireless
plmn("311278" ++ Rest) ->
	{"311", "278", Rest};
%% United States, Verizon Wireless
plmn("311483" ++ Rest) ->
	{"311", "483", Rest};
%% United States, Verizon Wireless
plmn("310890" ++ Rest) ->
	{"310", "890", Rest};
%% United States, Verizon Wireless
plmn("311283" ++ Rest) ->
	{"311", "283", Rest};
%% United States, Verizon Wireless
plmn("311488" ++ Rest) ->
	{"311", "488", Rest};
%% United States, Verizon Wireless
plmn("311272" ++ Rest) ->
	{"311", "272", Rest};
%% United States, Verizon Wireless
plmn("311288" ++ Rest) ->
	{"311", "288", Rest};
%% United States, Verizon Wireless
plmn("311277" ++ Rest) ->
	{"311", "277", Rest};
%% United States, Verizon Wireless
plmn("311482" ++ Rest) ->
	{"311", "482", Rest};
%% United States, Verizon Wireless
plmn("310590" ++ Rest) ->
	{"310", "590", Rest};
%% United States, Verizon Wireless
plmn("311282" ++ Rest) ->
	{"311", "282", Rest};
%% United States, Verizon Wireless
plmn("311487" ++ Rest) ->
	{"311", "487", Rest};
%% United States, Cellular Network Partnership LLC
plmn("312280" ++ Rest) ->
	{"312", "280", Rest};
%% United States, Cellular Network Partnership LLC
plmn("312270" ++ Rest) ->
	{"312", "270", Rest};
%% United States, Cellular Network Partnership LLC
plmn("310360" ++ Rest) ->
	{"310", "360", Rest};
%% United States,
plmn("311190" ++ Rest) ->
	{"311", "190", Rest};
%% United States, Choice Phone LLC
plmn("311120" ++ Rest) ->
	{"311", "120", Rest};
%% United States, Choice Phone LLC
plmn("310480" ++ Rest) ->
	{"310", "480", Rest};
%% United States,
plmn("310630" ++ Rest) ->
	{"310", "630", Rest};
%% United States, Cincinnati Bell Wireless LLC
plmn("310420" ++ Rest) ->
	{"310", "420", Rest};
%% United States, Cingular Wireless
plmn("310180" ++ Rest) ->
	{"310", "180", Rest};
%% United States, Coleman County Telco /Trans TX
plmn("310620" ++ Rest) ->
	{"310", "620", Rest};
%% United States,
plmn("31140" ++ Rest) ->
	{"311", "40", Rest};
%% United States, Consolidated Telcom
plmn("31006" ++ Rest) ->
	{"310", "06", Rest};
%% United States,
plmn("312380" ++ Rest) ->
	{"312", "380", Rest};
%% United States,
plmn("310930" ++ Rest) ->
	{"310", "930", Rest};
%% United States,
plmn("311240" ++ Rest) ->
	{"311", "240", Rest};
%% United States, Cross Valliant Cellular Partnership
%% United States, AT&T Wireless Inc.
plmn("310700" ++ Rest) ->
	{"310", "700", Rest};
%% United States, Cross Wireless Telephone Co.
plmn("31230" ++ Rest) ->
	{"312", "30", Rest};
%% United States, Cross Wireless Telephone Co.
plmn("311140" ++ Rest) ->
	{"311", "140", Rest};
%% United States,
plmn("311520" ++ Rest) ->
	{"311", "520", Rest};
%% United States, Custer Telephone Cooperative Inc.
plmn("31240" ++ Rest) ->
	{"312", "40", Rest};
%% United States, Dobson Cellular Systems
plmn("310440" ++ Rest) ->
	{"310", "440", Rest};
%% United States, E.N.M.R. Telephone Coop.
plmn("310990" ++ Rest) ->
	{"310", "990", Rest};
%% United States, East Kentucky Network LLC
plmn("312120" ++ Rest) ->
	{"312", "120", Rest};
%% United States, East Kentucky Network LLC
plmn("310750" ++ Rest) ->
	{"310", "750", Rest};
%% United States, East Kentucky Network LLC
plmn("312130" ++ Rest) ->
	{"312", "130", Rest};
%% United States, Edge Wireless LLC
plmn("31090" ++ Rest) ->
	{"310", "90", Rest};
%% United States, Elkhart TelCo. / Epic Touch Co.
plmn("310610" ++ Rest) ->
	{"310", "610", Rest};
%% United States,
plmn("311210" ++ Rest) ->
	{"311", "210", Rest};
%% United States, Farmers
plmn("311311" ++ Rest) ->
	{"311", "311", Rest};
%% United States, Fisher Wireless Services Inc.
plmn("311460" ++ Rest) ->
	{"311", "460", Rest};
%% United States, GCI Communication Corp.
plmn("311370" ++ Rest) ->
	{"311", "370", Rest};
%% United States, GCI Communication Corp.
plmn("310430" ++ Rest) ->
	{"310", "430", Rest};
%% United States, Get Mobile Inc.
plmn("310920" ++ Rest) ->
	{"310", "920", Rest};
%% United States,
plmn("310970" ++ Rest) ->
	{"310", "970", Rest};
%% United States, Illinois Valley Cellular RSA 2 Partnership
plmn("311340" ++ Rest) ->
	{"311", "340", Rest};
%% United States, Iowa RSA No. 2 Limited Partnership
plmn("311410" ++ Rest) ->
	{"311", "410", Rest};
%% United States, Iowa RSA No. 2 Limited Partnership
plmn("312170" ++ Rest) ->
	{"312", "170", Rest};
%% United States, Iowa Wireless Services LLC
plmn("310770" ++ Rest) ->
	{"310", "770", Rest};
%% United States, Jasper
plmn("310650" ++ Rest) ->
	{"310", "650", Rest};
%% United States, Kaplan Telephone Company Inc.
plmn("310870" ++ Rest) ->
	{"310", "870", Rest};
%% United States, Keystone Wireless LLC
plmn("312180" ++ Rest) ->
	{"312", "180", Rest};
%% United States, Keystone Wireless LLC
plmn("310690" ++ Rest) ->
	{"310", "690", Rest};
%% United States, Lamar County Cellular
plmn("311310" ++ Rest) ->
	{"311", "310", Rest};
%% United States, Leap Wireless International Inc.
plmn("31016" ++ Rest) ->
	{"310", "16", Rest};
%% United States,
plmn("31190" ++ Rest) ->
	{"311", "90", Rest};
%% United States, Message Express Co. / Airlink PCS
plmn("310780" ++ Rest) ->
	{"310", "780", Rest};
%% United States,
plmn("311660" ++ Rest) ->
	{"311", "660", Rest};
%% United States, Michigan Wireless LLC
plmn("311330" ++ Rest) ->
	{"311", "330", Rest};
%% United States,
plmn("31100" ++ Rest) ->
	{"311", "00", Rest};
%% United States, Minnesota South. Wirel. Co. / Hickory
%% United States, Matanuska Tel. Assn. Inc.
plmn("310400" ++ Rest) ->
	{"310", "400", Rest};
%% United States, Missouri RSA No 5 Partnership
plmn("31120" ++ Rest) ->
	{"311", "20", Rest};
%% United States, Missouri RSA No 5 Partnership
plmn("31110" ++ Rest) ->
	{"311", "10", Rest};
%% United States, Missouri RSA No 5 Partnership
plmn("312220" ++ Rest) ->
	{"312", "220", Rest};
%% United States, Missouri RSA No 5 Partnership
plmn("31210" ++ Rest) ->
	{"312", "10", Rest};
%% United States, Missouri RSA No 5 Partnership
plmn("311920" ++ Rest) ->
	{"311", "920", Rest};
%% United States, Mohave Cellular LP
plmn("310350" ++ Rest) ->
	{"310", "350", Rest};
%% United States, MTPCS LLC
plmn("310570" ++ Rest) ->
	{"310", "570", Rest};
%% United States, NEP Cellcorp Inc.
plmn("310290" ++ Rest) ->
	{"310", "290", Rest};
%% United States, Nevada Wireless LLC
%% United States, "Westlink Communications, LLC"
plmn("31034" ++ Rest) ->
	{"310", "34", Rest};
%% United States,
plmn("311380" ++ Rest) ->
	{"311", "380", Rest};
%% United States, New-Cell Inc.
%% United States, Consolidated Telcom
plmn("310600" ++ Rest) ->
	{"310", "600", Rest};
%% United States, Nexus Communications Inc.
plmn("311300" ++ Rest) ->
	{"311", "300", Rest};
%% United States, North Carolina RSA 3 Cellular Tel. Co.
plmn("310130" ++ Rest) ->
	{"310", "130", Rest};
%% United States, North Dakota Network Company
plmn("312230" ++ Rest) ->
	{"312", "230", Rest};
%% United States, North Dakota Network Company
plmn("311610" ++ Rest) ->
	{"311", "610", Rest};
%% United States, Northeast Colorado Cellular Inc.
plmn("310450" ++ Rest) ->
	{"310", "450", Rest};
%% United States, Northeast Wireless Networks LLC
plmn("311710" ++ Rest) ->
	{"311", "710", Rest};
%% United States, Northstar
plmn("310670" ++ Rest) ->
	{"310", "670", Rest};
%% United States, Northstar
plmn("31011" ++ Rest) ->
	{"310", "11", Rest};
%% United States, Northwest Missouri Cellular Limited Partnership
plmn("311420" ++ Rest) ->
	{"311", "420", Rest};
%% United States,
plmn("310540" ++ Rest) ->
	{"310", "540", Rest};
%% United States, Various Networks
plmn("310999" ++ Rest) ->
	{"310", "999", Rest};
%% United States, Panhandle Telephone Cooperative Inc.
plmn("310760" ++ Rest) ->
	{"310", "760", Rest};
%% United States, PCS ONE
plmn("310580" ++ Rest) ->
	{"310", "580", Rest};
%% United States, PetroCom
plmn("311170" ++ Rest) ->
	{"311", "170", Rest};
%% United States, "Pine Belt Cellular, Inc."
plmn("311670" ++ Rest) ->
	{"311", "670", Rest};
%% United States,
plmn("31180" ++ Rest) ->
	{"311", "80", Rest};
%% United States,
plmn("310790" ++ Rest) ->
	{"310", "790", Rest};
%% United States, Poka Lambro Telco Ltd.
plmn("310940" ++ Rest) ->
	{"310", "940", Rest};
%% United States,
plmn("311730" ++ Rest) ->
	{"311", "730", Rest};
%% United States,
plmn("311540" ++ Rest) ->
	{"311", "540", Rest};
%% United States, Public Service Cellular Inc.
plmn("310500" ++ Rest) ->
	{"310", "500", Rest};
%% United States, RSA 1 Limited Partnership
plmn("312160" ++ Rest) ->
	{"312", "160", Rest};
%% United States, RSA 1 Limited Partnership
plmn("311430" ++ Rest) ->
	{"311", "430", Rest};
%% United States, Sagebrush Cellular Inc.
plmn("311350" ++ Rest) ->
	{"311", "350", Rest};
%% United States,
plmn("311910" ++ Rest) ->
	{"311", "910", Rest};
%% United States, SIMMETRY
%% United States, TMP Corporation
plmn("31046" ++ Rest) ->
	{"310", "46", Rest};
%% United States, SLO Cellular Inc / Cellular One of San Luis
plmn("311260" ++ Rest) ->
	{"311", "260", Rest};
%% United States, Unknown
plmn("31015" ++ Rest) ->
	{"310", "15", Rest};
%% United States, Southern Communications Services Inc.
plmn("31611" ++ Rest) ->
	{"316", "11", Rest};
%% United States, Sprint Spectrum
plmn("312530" ++ Rest) ->
	{"312", "530", Rest};
%% United States, Sprint Spectrum
plmn("311870" ++ Rest) ->
	{"311", "870", Rest};
%% United States, Sprint Spectrum
plmn("311490" ++ Rest) ->
	{"311", "490", Rest};
%% United States, Sprint Spectrum
plmn("31610" ++ Rest) ->
	{"316", "10", Rest};
%% United States, Sprint Spectrum
plmn("312190" ++ Rest) ->
	{"312", "190", Rest};
%% United States, Sprint Spectrum
plmn("311880" ++ Rest) ->
	{"311", "880", Rest};
%% United States, T-Mobile
plmn("310260" ++ Rest) ->
	{"310", "260", Rest};
%% United States, T-Mobile
plmn("310200" ++ Rest) ->
	{"310", "200", Rest};
%% United States, T-Mobile
plmn("310250" ++ Rest) ->
	{"310", "250", Rest};
%% United States, T-Mobile
plmn("310240" ++ Rest) ->
	{"310", "240", Rest};
%% United States, T-Mobile
plmn("310660" ++ Rest) ->
	{"310", "660", Rest};
%% United States, T-Mobile
plmn("310230" ++ Rest) ->
	{"310", "230", Rest};
%% United States, T-Mobile
plmn("310220" ++ Rest) ->
	{"310", "220", Rest};
%% United States, T-Mobile
plmn("310270" ++ Rest) ->
	{"310", "270", Rest};
%% United States, T-Mobile
plmn("310210" ++ Rest) ->
	{"310", "210", Rest};
%% United States, T-Mobile
plmn("310300" ++ Rest) ->
	{"310", "300", Rest};
%% United States, T-Mobile
plmn("310280" ++ Rest) ->
	{"310", "280", Rest};
%% United States, T-Mobile
plmn("310800" ++ Rest) ->
	{"310", "800", Rest};
%% United States, T-Mobile
plmn("310310" ++ Rest) ->
	{"310", "310", Rest};
%% United States,
plmn("311740" ++ Rest) ->
	{"311", "740", Rest};
%% United States, Telemetrix Inc.
plmn("310740" ++ Rest) ->
	{"310", "740", Rest};
%% United States, Testing
plmn("31014" ++ Rest) ->
	{"310", "14", Rest};
%% United States, Unknown
plmn("310950" ++ Rest) ->
	{"310", "950", Rest};
%% United States, Texas RSA 15B2 Limited Partnership
plmn("310860" ++ Rest) ->
	{"310", "860", Rest};
%% United States, Thumb Cellular Limited Partnership
plmn("311830" ++ Rest) ->
	{"311", "830", Rest};
%% United States, Thumb Cellular Limited Partnership
plmn("31150" ++ Rest) ->
	{"311", "50", Rest};
%% United States, Triton PCS
plmn("310490" ++ Rest) ->
	{"310", "490", Rest};
%% United States, Uintah Basin Electronics Telecommunications Inc.
plmn("312290" ++ Rest) ->
	{"312", "290", Rest};
%% United States, Uintah Basin Electronics Telecommunications Inc.
plmn("311860" ++ Rest) ->
	{"311", "860", Rest};
%% United States, Uintah Basin Electronics Telecommunications Inc.
plmn("310960" ++ Rest) ->
	{"310", "960", Rest};
%% United States, Union Telephone Co.
plmn("31020" ++ Rest) ->
	{"310", "20", Rest};
%% United States, United States Cellular Corp.
plmn("311220" ++ Rest) ->
	{"311", "220", Rest};
%% United States, United States Cellular Corp.
plmn("310730" ++ Rest) ->
	{"310", "730", Rest};
%% United States, United Wireless Communications Inc.
plmn("311650" ++ Rest) ->
	{"311", "650", Rest};
%% United States, USA 3650 AT&T
plmn("31038" ++ Rest) ->
	{"310", "38", Rest};
%% United States, VeriSign
plmn("310520" ++ Rest) ->
	{"310", "520", Rest};
%% United States, Unknown
plmn("31003" ++ Rest) ->
	{"310", "03", Rest};
%% United States, Unknown
plmn("31023" ++ Rest) ->
	{"310", "23", Rest};
%% United States, Unknown
plmn("31024" ++ Rest) ->
	{"310", "24", Rest};
%% United States, Unknown
plmn("31025" ++ Rest) ->
	{"310", "25", Rest};
%% United States, West Virginia Wireless
plmn("310530" ++ Rest) ->
	{"310", "530", Rest};
%% United States, Unknown
plmn("31026" ++ Rest) ->
	{"310", "26", Rest};
%% United States
plmn("311150" ++ Rest) ->
	{"311", "150", Rest};
%% United States, Wisconsin RSA #7 Limited Partnership
plmn("31170" ++ Rest) ->
	{"311", "70", Rest};
%% United States, Yorkville Telephone Cooperative
plmn("310390" ++ Rest) ->
	{"310", "390", Rest};
%% Uruguay, Ancel/Antel
plmn("74801" ++ Rest) ->
	{"748", "01", Rest};
%% Uruguay, Ancel/Antel
plmn("74803" ++ Rest) ->
	{"748", "03", Rest};
%% Uruguay, Ancel/Antel
plmn("74800" ++ Rest) ->
	{"748", "00", Rest};
%% Uruguay, Claro/AM Wireless
plmn("74810" ++ Rest) ->
	{"748", "10", Rest};
%% Uruguay, MOVISTAR
plmn("74807" ++ Rest) ->
	{"748", "07", Rest};
%% Uzbekistan, Bee Line/Unitel
plmn("43404" ++ Rest) ->
	{"434", "04", Rest};
%% Uzbekistan, Buztel
plmn("43401" ++ Rest) ->
	{"434", "01", Rest};
%% Uzbekistan, MTS/Uzdunrobita
plmn("43407" ++ Rest) ->
	{"434", "07", Rest};
%% Uzbekistan, Ucell/Coscom
plmn("43405" ++ Rest) ->
	{"434", "05", Rest};
%% Uzbekistan, Uzmacom
plmn("43402" ++ Rest) ->
	{"434", "02", Rest};
%% Vanuatu, DigiCel
plmn("54105" ++ Rest) ->
	{"541", "05", Rest};
%% Vanuatu, SMILE
plmn("54101" ++ Rest) ->
	{"541", "01", Rest};
%% Venezuela, DigiTel C.A.
plmn("73403" ++ Rest) ->
	{"734", "03", Rest};
%% Venezuela, DigiTel C.A.
plmn("73402" ++ Rest) ->
	{"734", "02", Rest};
%% Venezuela, DigiTel C.A.
plmn("73401" ++ Rest) ->
	{"734", "01", Rest};
%% Venezuela, Movilnet C.A.
plmn("73406" ++ Rest) ->
	{"734", "06", Rest};
%% Venezuela, Movistar/TelCel
plmn("73404" ++ Rest) ->
	{"734", "04", Rest};
%% Viet Nam, Gmobile
plmn("45207" ++ Rest) ->
	{"452", "07", Rest};
%% Viet Nam, I-Telecom
plmn("45208" ++ Rest) ->
	{"452", "08", Rest};
%% Viet Nam, Mobifone
plmn("45201" ++ Rest) ->
	{"452", "01", Rest};
%% Viet Nam, S-Fone/Telecom
plmn("45203" ++ Rest) ->
	{"452", "03", Rest};
%% Viet Nam, VietnaMobile
plmn("45205" ++ Rest) ->
	{"452", "05", Rest};
%% Viet Nam, Viettel Mobile
plmn("45204" ++ Rest) ->
	{"452", "04", Rest};
%% Viet Nam, Viettel Mobile
plmn("45206" ++ Rest) ->
	{"452", "06", Rest};
%% Viet Nam, Vinaphone
plmn("45202" ++ Rest) ->
	{"452", "02", Rest};
%% Yemen, HITS/Y Unitel
plmn("42104" ++ Rest) ->
	{"421", "04", Rest};
%% Yemen, MTN/Spacetel
plmn("42102" ++ Rest) ->
	{"421", "02", Rest};
%% Yemen, Sabaphone
plmn("42101" ++ Rest) ->
	{"421", "01", Rest};
%% Yemen, Yemen Mob. CDMA
plmn("42103" ++ Rest) ->
	{"421", "03", Rest};
%% Zambia, Zamtel/Cell Z/MTS
plmn("64503" ++ Rest) ->
	{"645", "03", Rest};
%% Zambia, MTN/Telecel
plmn("64502" ++ Rest) ->
	{"645", "02", Rest};
%% Zambia, Airtel/Zain/Celtel
plmn("64501" ++ Rest) ->
	{"645", "01", Rest};
%% Zimbabwe, Econet
plmn("64804" ++ Rest) ->
	{"648", "04", Rest};
%% Zimbabwe, Net One
plmn("64801" ++ Rest) ->
	{"648", "01", Rest};
%% Zimbabwe, Telecel
plmn("64803" ++ Rest) ->
	{"648", "03", Rest}.

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

