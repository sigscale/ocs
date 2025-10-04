#!/usr/bin/env escript
%% vim: syntax=erlang

-include_lib("ocs/include/diameter_gen_3gpp.hrl").
-include_lib("ocs/include/diameter_gen_3gpp_ro_application.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

main(Args) ->
	case options(Args) of
		#{help := true} = _Options ->
			usage();
		Options ->
			data_session(Options)
	end.

data_session(Options) ->
	try
		Name = escript:script_name() ++ "-" ++ ref_to_list(make_ref()),
		ok = diameter:start(),
		Hostname = filename:rootname(filename:basename(Name), ".escript"),
		OriginRealm = case inet_db:res_option(domain) of
			Domain when length(Domain) > 0 ->
				Domain;
			_ ->
				"example.net"
		end,
		Callback = #diameter_callback{},
		ServiceOptions = [{'Origin-Host', Hostname},
				{'Origin-Realm', OriginRealm},
				{'Vendor-Id', ?IANA_PEN_SigScale},
				{'Supported-Vendor-Id', [?IANA_PEN_3GPP]},
				{'Product-Name', "SigScale Test Script"},
				{'Auth-Application-Id', [?RO_APPLICATION_ID]},
				{string_decode, false},
				{restrict_connections, false},
				{application, [{dictionary, diameter_gen_base_rfc6733},
						{module, Callback}]},
				{application, [{alias, ro},
						{dictionary, diameter_gen_3gpp_ro_application},
						{module, Callback}]}],
		ok = diameter:start_service(Name, ServiceOptions),
		true = diameter:subscribe(Name),
		TransportOptions =  [{transport_module, diameter_tcp},
				{transport_config,
						[{raddr, maps:get(raddr, Options, {127,0,0,1})},
						{rport, maps:get(rport, Options, 3868)},
						{ip, maps:get(ip, Options, {127,0,0,1})}]}],
		{ok, _Ref} = diameter:add_transport(Name, {connect, TransportOptions}),
		receive
			#diameter_event{service = Name, info = Info}
					when element(1, Info) == up ->
				ok
		end,
		SId = diameter:session_id(Hostname),
		RequestNum1 = 0,
		IMSI = #'3gpp_ro_Subscription-Id'{
				'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
				'Subscription-Id-Data' = maps:get(imsi, Options, "001001123456789")},
		MSISDN = #'3gpp_ro_Subscription-Id'{
				'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
				'Subscription-Id-Data' = maps:get(msisdn, Options, "14165551234")},
		SubscriptionId = [IMSI, MSISDN],
		SIs = maps:get(service_id, Options, "1,5"),
		ServiceIds = [list_to_integer(SI) || SI <- string:lexemes(SIs, [$,])],
		RGs = maps:get(rating_group, Options, "16,32"),
		RatingGroups = [list_to_integer(RG) || RG <- string:lexemes(RGs, [$,])],
		SiRg = lists:zip(ServiceIds, RatingGroups),
		RSU1 = #'3gpp_ro_Requested-Service-Unit'{},
		MSCC1 = [#'3gpp_ro_Multiple-Services-Credit-Control'{
				'Service-Identifier' = [SI],
				'Rating-Group' = [RG],
				'Requested-Service-Unit' = [RSU1]} || {SI, RG} <- SiRg],
		Location = << <<(list_to_integer([C], 16)):4>>
				|| C <- maps:get(location, Options, "82001100beef0011000deadbee") >>,
		PS = #'3gpp_ro_PS-Information'{
				'Called-Station-Id' = [maps:get(apn, Options, "internet")],
				'3GPP-PDP-Type' = [3],
				'Serving-Node-Type' = [2],
				'SGSN-Address' = [{10,1,2,3}],
				'GGSN-Address' = [{10,4,5,6}],
				'3GPP-IMSI-MCC-MNC' = [maps:get(hplmn, Options, "001001")],
				'3GPP-GGSN-MCC-MNC' = [maps:get(hplmn, Options, "001001")],
				'3GPP-SGSN-MCC-MNC' = [maps:get(vplmn, Options, "001001")],
				'3GPP-User-Location-Info' = [Location]},
		ServiceInformation = #'3gpp_ro_Service-Information'{
				'PS-Information' = [PS]},
		CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
				'Origin-Host' = Hostname,
				'Origin-Realm' = OriginRealm,
				'Destination-Realm' = OriginRealm,
				'Auth-Application-Id' = ?RO_APPLICATION_ID,
				'Service-Context-Id' = maps:get(context, Options, "32251@3gpp.org"),
				'User-Name' = [list_to_binary(Name)],
				'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
				'CC-Request-Number' = RequestNum1,
				'Event-Timestamp' = [calendar:universal_time()],
				'Subscription-Id' = SubscriptionId,
				'Multiple-Services-Credit-Control' = MSCC1,
				'Service-Information' = [ServiceInformation]},
		Fro = fun('3gpp_ro_CCA', _N) ->
					record_info(fields, '3gpp_ro_CCA');
				('3gpp_ro_Multiple-Services-Credit-Control', _N) ->
					record_info(fields, '3gpp_ro_Multiple-Services-Credit-Control');
				('3gpp_ro_Granted-Service-Unit', _N) ->
					record_info(fields, '3gpp_ro_Granted-Service-Unit');
				('3gpp_ro_Final-Unit-Indication', _N) ->
					record_info(fields, '3gpp_ro_Final-Unit-Indication');
				('3gpp_ro_Redirect-Server', _N) ->
					record_info(fields, '3gpp_ro_Redirect-Server')
		end,
		Fbase = fun('diameter_base_answer-message', _N) ->
					record_info(fields, 'diameter_base_answer-message');
				('diameter_base_Failed-AVP', _N) ->
					record_info(fields, 'diameter_base_Failed-AVP');
				('diameter_base_Experimental-Result', _N) ->
					record_info(fields, 'diameter_base_Experimental-Result');
				('diameter_base_Vendor-Specific-Application-Id', _N) ->
					record_info(fields, 'diameter_base_Vendor-Specific-Application-Id');
				('diameter_base_Proxy-Info', _N) ->
					record_info(fields, 'diameter_base_Proxy-Info')
		end,
		case diameter:call(Name, ro, CCR1, []) of
			#'3gpp_ro_CCA'{'Result-Code' = 2001} = Answer1 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer1, Fro)]);
			#'3gpp_ro_CCA'{'Result-Code' = ResultCode} = Answer1 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer1, Fro)]),
				throw(ResultCode);
			#'diameter_base_answer-message'{'Result-Code' = ResultCode} = Answer1 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer1, Fbase)]),
				throw(ResultCode);
			{error, Reason1} ->
				error(Reason1)
		end,
		timer:sleep(maps:get(interval, Options, 1000)),
		Fupdate = fun F(0, ReqNum) ->
					ReqNum;
				F(N, ReqNum) ->
					NewReqNum = ReqNum + 1,
					MSCC2 = [MSCC#'3gpp_ro_Multiple-Services-Credit-Control'{
							'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
							'CC-Total-Octets' = [rand:uniform(1000000)]}]}
							|| MSCC <- MSCC1],
					CCR2 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
							'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
							'CC-Request-Number' = NewReqNum,
							'Multiple-Services-Credit-Control' = MSCC2,
							'Event-Timestamp' = [calendar:universal_time()]},
					case diameter:call(Name, ro, CCR2, []) of
						#'3gpp_ro_CCA'{'Result-Code' = 2001} = Answer2 ->
							io:fwrite("~s~n", [io_lib_pretty:print(Answer2, Fro)]);
						#'3gpp_ro_CCA'{'Result-Code' = ResultCode1} = Answer2 ->
							io:fwrite("~s~n", [io_lib_pretty:print(Answer2, Fro)]),
							throw(ResultCode1);
						#'diameter_base_answer-message'{'Result-Code' = ResultCode1} = Answer2 ->
							io:fwrite("~s~n", [io_lib_pretty:print(Answer2, Fbase)]),
							throw(ResultCode1);
						{error, Reason2} ->
							error(Reason2)
					end,
					timer:sleep(maps:get(interval, Options, 1000)),
					F(N - 1, NewReqNum)
		end,
		RequestNum2 = Fupdate(maps:get(updates, Options, 1), RequestNum1),
		MSCC3 = [MSCC#'3gpp_ro_Multiple-Services-Credit-Control'{
				'Requested-Service-Unit' = [],
				'Used-Service-Unit' = [#'3gpp_ro_Used-Service-Unit'{
				'CC-Total-Octets' = [rand:uniform(1000000)]}]}
				|| MSCC <- MSCC1],
		CCR3 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
				'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
				'CC-Request-Number' = RequestNum2,
				'Multiple-Services-Credit-Control' = MSCC3,
				'Event-Timestamp' = [calendar:universal_time()]},
		case diameter:call(Name, ro, CCR3, []) of
			#'3gpp_ro_CCA'{'Result-Code' = 2001} = Answer3 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer3, Fro)]);
			#'3gpp_ro_CCA'{'Result-Code' = ResultCode2} = Answer3 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer3, Fro)]),
				throw(ResultCode2);
			#'diameter_base_answer-message'{'Result-Code' = ResultCode2} = Answer3 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer3, Fbase)]),
				throw(ResultCode2);
			{error, Reason3} ->
				error(Reason3)
		end
	catch
		throw:_Reason4 ->
			halt(1);
		error:Reason4 ->
			io:fwrite("~w: ~w~n", [error, Reason4]),
			halt(1);
		exit:Reason4 ->
			io:fwrite("~w: ~w~n", [error, Reason4]),
			usage()
	end.

usage() ->
	Option1 = " [--context 32251@3gpp.org]",
	Option2 = " [--service-id 1,5]",
	Option3 = " [--rating-group 16,32]",
	Option4 = " [--apn internet]",
	Option5 = " [--hplmn 001001]",
	Option6 = " [--vplmn 001001]",
	Option7 = " [--location 82001100beef0011000deadbee]",
	Option8 = " [--msisdn 14165551234]",
	Option9 = " [--imsi 001001123456789]",
	Option10 = " [--interval 1000]",
	Option11 = " [--updates 1]",
	Option12 = " [--ip 127.0.0.1]",
	Option13 = " [--raddr 127.0.0.1]",
	Option14 = " [--rport 3868]",
	Options = [Option1, Option2, Option3, Option4, Option5,
			Option6, Option7, Option8, Option9, Option10,
			Option11, Option12, Option13, Option14],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	options(Args, #{}).
options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--context", Context | T], Acc) ->
	options(T, Acc#{context => Context});
options(["--service-id", ServiceId | T], Acc) ->
	options(T, Acc#{service_id => ServiceId});
options(["--rating-group", RatingGroup | T], Acc) ->
	options(T, Acc#{rating_group => RatingGroup});
options(["--apn", APN | T], Acc) ->
	options(T, Acc#{apn => APN});
options(["--hplmn", HPLMN | T], Acc) ->
	options(T, Acc#{hplmn => HPLMN});
options(["--vplmn", VPLMN | T], Acc) ->
	options(T, Acc#{vplmn => VPLMN});
options(["--location", Location | T], Acc) ->
	options(T, Acc#{location  => Location});
options(["--imsi", IMSI | T], Acc) ->
	options(T, Acc#{imsi=> IMSI});
options(["--msisdn", MSISDN | T], Acc) ->
	options(T, Acc#{msisdn => MSISDN});
options(["--interval", MS | T], Acc) ->
	options(T, Acc#{interval => list_to_integer(MS)});
options(["--updates", N | T], Acc) ->
	options(T, Acc#{updates => list_to_integer(N)});
options(["--ip", Address | T], Acc) ->
	{ok, IP} = inet:parse_address(Address),
	options(T, Acc#{ip => IP});
options(["--raddr", Address | T], Acc) ->
	{ok, IP} = inet:parse_address(Address),
	options(T, Acc#{raddr => IP});
options(["--rport", Port | T], Acc) ->
	options(T, Acc#{rport=> list_to_integer(Port)});
options([_H | _T], _Acc) ->
	usage();
options([], Acc) ->
	Acc.

