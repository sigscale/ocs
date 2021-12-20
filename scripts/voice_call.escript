#!/usr/bin/env escript
%% vim: syntax=erlang

-include_lib("ocs/include/diameter_gen_3gpp.hrl").
-include_lib("ocs/include/diameter_gen_3gpp_ro_application.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-define(RO_APPLICATION_ID, 4).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).

main([CallingParty, CalledParty]) ->
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
						[{raddr, {10,140,15,239}},
						{rport, 4999},
						{ip, {10,140,15,239}}]}],
		{ok, _Ref} = diameter:add_transport(Name, {connect, TransportOptions}),
		receive
			#diameter_event{service = Name, info = Info}
					when element(1, Info) == up ->
				ok
		end,
		SId = diameter:session_id(Hostname),
		SubscriptionId = #'3gpp_ro_Subscription-Id'{
				'Subscription-Id-Type'
						= ?'3GPP_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
				'Subscription-Id-Data' = list_to_binary(CallingParty)},
		MSCC1 = #'3gpp_ro_Multiple-Services-Credit-Control'{
				'Requested-Service-Unit' = []},
		CallingPartyAddress = "tel:+" ++ CallingParty,
		CalledPartyAddress = "tel:+" ++ CalledParty,
		ServiceInformation = #'3gpp_ro_Service-Information'{'IMS-Information' =
				[#'3gpp_ro_IMS-Information'{
						'Node-Functionality' = ?'3GPP_RO_NODE-FUNCTIONALITY_AS',
						'Role-Of-Node' = [?'3GPP_RO_ROLE-OF-NODE_ORIGINATING_ROLE'],
						'Calling-Party-Address' = [CallingPartyAddress],
						'Called-Party-Address' = [CalledPartyAddress]}]},
		RequestNum = 0,
		CCR1 = #'3gpp_ro_CCR'{'Session-Id' = SId,
				'Origin-Host' = Hostname,
				'Origin-Realm' = OriginRealm,
				'Destination-Realm' = OriginRealm,
				'Auth-Application-Id' = ?RO_APPLICATION_ID,
				'Service-Context-Id' = "32260@3gpp.org",
				'User-Name' = [list_to_binary(CallingParty)],
				'CC-Request-Type' = ?'3GPP_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
				'CC-Request-Number' = RequestNum,
				'Event-Timestamp' = [calendar:universal_time()],
				'Subscription-Id' = [SubscriptionId],
				'Multiple-Services-Credit-Control' = [MSCC1],
				'Service-Information' = [ServiceInformation]},
		F = fun('3gpp_ro_CCA', _N) ->
				record_info(fields, '3gpp_ro_CCA')
		end,
		F1 = fun('diameter_base_answer-message', _N) ->
				record_info(fields, 'diameter_base_answer-message')
		end,
		case diameter:call(Name, ro, CCR1, []) of
			#'3gpp_ro_CCA'{} =  Answer ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer, F)]);
			#'diameter_base_answer-message'{} = Answer ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer, F1)]);
			{error, Reason} ->
				throw(Reason)
		end,
		timer:sleep(1000),
		RequestNum1 = RequestNum + 1,
		UsedUnits = rand:uniform(10000),
		USU = #'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedUnits]},
		MSCC3 = #'3gpp_ro_Multiple-Services-Credit-Control'{
				'Used-Service-Unit' = [USU],
				'Requested-Service-Unit' = []},
		CCR2 = CCR1#'3gpp_ro_CCR'{'Session-Id' = SId,
				'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST',
				'CC-Request-Number' = RequestNum1,
				'Multiple-Services-Credit-Control' = [MSCC3],
				'Event-Timestamp' = [calendar:universal_time()]},
		case diameter:call(Name, ro, CCR2, []) of
			#'3gpp_ro_CCA'{} = Answer1 ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer1, F)]);
			#'diameter_base_answer-message'{} = Answer1 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer1, F1)]);
			{error, Reason1} ->
				throw(Reason1)
		end,
		timer:sleep(1000),
		RequestNum2 = RequestNum1 + 1,
		USU1 = #'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedUnits]},
		MSCC4 = #'3gpp_ro_Multiple-Services-Credit-Control'{
				'Used-Service-Unit' = [USU1]},
		CCR3 = CCR2#'3gpp_ro_CCR'{'Session-Id' = SId,
				'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST',
				'CC-Request-Number' = RequestNum2,
				'Multiple-Services-Credit-Control' = [MSCC4],
				'Event-Timestamp' = [calendar:universal_time()]},
		case diameter:call(Name, ro, CCR3, []) of
			#'3gpp_ro_CCA'{} = Answer2 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer2, F)]);
			#'diameter_base_answer-message'{} = Answer2 ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer2, F1)]);
			{error, Reason2} ->
				throw(Reason2)
		end
	catch
		Error:Reason3 ->
			io:fwrite("~w: ~w~n", [Error, Reason3]),
			usage()
	end;
main(_) ->
	usage().

usage() ->
	io:fwrite("usage: ~s Origin Destination~n", [escript:script_name()]),
	halt(1).

