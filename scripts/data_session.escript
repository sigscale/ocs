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
		Name = escript:script_name(),
		ok = diameter:start(),
		Hostname = erlang:ref_to_list(make_ref()),
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
						[{raddr, {127,0,0,1}},
						{rport, 3868},
						{ip, {127,0,0,1}}]}],
		{ok, _Ref} = diameter:add_transport(Name, {connect, TransportOptions}),
		receive
			#diameter_event{service = Name, info = Info}
					when element(1, Info) == up ->
				ok
		end,
		SId = diameter:session_id(Hostname),
		RequestNum = 0,
		IMSI = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
			'Subscription-Id-Data' = maps:get(imsi, Options, "001001123456789")},
		MSISDN = #'3gpp_ro_Subscription-Id'{
			'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
			'Subscription-Id-Data' = maps:get(msisdn, Options, "14165551234")},
		SubscriptionId = [IMSI, MSISDN],
		MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Requested-Service-Unit' = []},
		ServiceInformation = #'3gpp_ro_Service-Information'{'PS-Information' =
				[#'3gpp_ro_PS-Information'{
					'3GPP-PDP-Type' = [3],
					'Serving-Node-Type' = [2],
					'SGSN-Address' = [{10,1,2,3}],
					'GGSN-Address' = [{10,4,5,6}],
					'3GPP-IMSI-MCC-MNC' = [<<"001001">>],
					'3GPP-GGSN-MCC-MNC' = [<<"001001">>],
					'3GPP-SGSN-MCC-MNC' = [<<"001001">>]}]},
		CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
			'Origin-Host' = Hostname,
			'Origin-Realm' = OriginRealm,
			'Destination-Realm' = OriginRealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = "32251@3gpp.org",
			'User-Name' = [list_to_binary(Name)],
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST',
			'CC-Request-Number' = RequestNum,
			'Event-Timestamp' = [calendar:universal_time()],
			'Subscription-Id' = SubscriptionId,
			'Multiple-Services-Credit-Control' = [MSCC1],
			'Service-Information' = [ServiceInformation]},
		Fro = fun('3gpp_ro_CCA', _N) ->
					record_info(fields, '3gpp_ro_CCA')
		end,
		Fbase = fun('diameter_base_answer-message', _N) ->
					record_info(fields, 'diameter_base_answer-message')
		end,
		case diameter:call(Name, ro, CCR, []) of
			#'3gpp_ro_CCA'{} = Answer ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fro)]);
			#'diameter_base_answer-message'{} = Answer ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fbase)]);
			{error, Reason} ->
						throw(Reason)
		end,
		timer:sleep(maps:get(interval, Options, 1000)),
		Fupdate = fun F(0, ReqNum) ->
					ReqNum;
				F(N, ReqNum) ->
					NewReqNum = ReqNum + 1,
					UsedUnits = rand:uniform(1000000),
					USU = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [UsedUnits]},
					MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
						'Used-Service-Unit' = [USU]},
					CCR1 = CCR#'3gpp_ro_CCR'{'Session-Id' = SId,
						'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
						'CC-Request-Number' = NewReqNum,
						'Multiple-Services-Credit-Control' = [MSCC3],
						'Event-Timestamp' = [calendar:universal_time()]},
					case diameter:call(Name, ro, CCR1, []) of
						#'3gpp_ro_CCA'{} = Answer1 ->
									io:fwrite("~s~n", [io_lib_pretty:print(Answer1, Fro)]);
						#'diameter_base_answer-message'{} = Answer1 ->
									io:fwrite("~s~n", [io_lib_pretty:print(Answer1, Fbase)]);
						{error, Reason1} ->
							throw(Reason1)
					end,
					timer:sleep(maps:get(interval, Options, 1000)),
					F(N - 1, NewReqNum)
		end,
		RequestNum1 = Fupdate(maps:get(updates, Options, 1), RequestNum),
		UsedUnits1 = rand:uniform(1000000),
		USU1 = #'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [UsedUnits1]},
		MSCC4 = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Used-Service-Unit' = [USU1]},
		CCR2 = CCR#'3gpp_ro_CCR'{'Session-Id' = SId,
			'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
			'CC-Request-Number' = RequestNum1,
			'Multiple-Services-Credit-Control' = [MSCC4],
			'Event-Timestamp' = [calendar:universal_time()]},
		case diameter:call(Name, ro, CCR2, []) of
			#'3gpp_ro_CCA'{} = Answer2 ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer2, Fro)]);
			#'diameter_base_answer-message'{} = Answer2 ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer2, Fbase)]);
			{error, Reason2} ->
				throw(Reason2)
		end
	catch
		Error:Reason3 ->
			io:fwrite("~w: ~w~n", [Error, Reason3]),
			usage()
	end.

usage() ->
	Option1 = " [--msisdn MSISDN]",
	Option2 = " [--imsi IMSI]",
	Option3 = " [--interval milliseconds]",
	Option4 = " [--updates N]",
	Options = [Option1, Option2, Option3, Option4],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	options(Args, #{}).
options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--imsi", IMSI | T], Acc) ->
	options(T, Acc#{imsi=> IMSI});
options(["--msisdn", MSISDN | T], Acc) ->
	options(T, Acc#{msisdn => MSISDN});
options(["--interval", MS | T], Acc) ->
	options(T, Acc#{interval => list_to_integer(MS)});
options(["--updates", N | T], Acc) ->
	options(T, Acc#{updates => list_to_integer(N)});
options([_H | _T], _Acc) ->
	usage();
options([], Acc) ->
	Acc.

