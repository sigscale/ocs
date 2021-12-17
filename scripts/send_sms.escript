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
		SubscriptionId = #'3gpp_ro_Subscription-Id'{
				'Subscription-Id-Type' = ?'3GPP_SUBSCRIPTION-ID-TYPE_END_USER_E164',
				'Subscription-Id-Data' = CallingParty},
		ServiceInformation = #'3gpp_ro_Service-Information'{
				'SMS-Information' = [#'3gpp_ro_SMS-Information'{
				'Recipient-Info' = [#'3gpp_ro_Recipient-Info'{
				'Recipient-Address' = [#'3gpp_ro_Recipient-Address'{
				'Address-Data' = [CalledParty]}]}]}]},
		CCR = #'3gpp_ro_CCR'{'Session-Id' = SId,
				'Origin-Host' = Hostname,
				'Origin-Realm' = OriginRealm,
				'Destination-Realm' = OriginRealm,
				'Auth-Application-Id' = ?RO_APPLICATION_ID,
				'Service-Context-Id' = "32274@3gpp.org",
				'User-Name' = [CallingParty],
				'CC-Request-Type' = ?'3GPP_CC-REQUEST-TYPE_EVENT_REQUEST',
				'CC-Request-Number' = 0,
				'Requested-Action' = [?'3GPP_RO_REQUESTED-ACTION_DIRECT_DEBITING'],
				'Event-Timestamp' = [calendar:universal_time()],
				'Subscription-Id' = [SubscriptionId],
				'Service-Information' = [ServiceInformation]},
		case diameter:call(Name, ro, CCR, []) of
			#'3gpp_ro_CCA'{} = Answer ->
				F = fun('3gpp_ro_CCA', _N) ->
							record_info(fields, '3gpp_ro_CCA')
				end,
				io:fwrite("~s~n", [io_lib_pretty:print(Answer, F)]);
			#'diameter_base_answer-message'{} = Answer ->
				F = fun('diameter_base_answer-message', _N) ->
							record_info(fields, 'diameter_base_answer-message')
				end,
				io:fwrite("~s~n", [io_lib_pretty:print(Answer, F)]);
			{error, Reason} ->
				throw(Reason)
		end
	catch
		Error:Reason1 ->
			io:fwrite("~w: ~w~n", [Error, Reason1]),
			usage()
	end;
main(_) ->
	usage().

usage() ->
	io:fwrite("usage: ~s Origin Destination~n", [escript:script_name()]),
	halt(1).

