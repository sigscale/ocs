#!/usr/bin/env escript
%% vim: syntax=erlang

-include_lib("ocs/include/diameter_gen_3gpp.hrl").
-include_lib("ocs/include/diameter_gen_3gpp_sta_application.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("ocs/include/ocs_eap_codec.hrl").

-define(STa_APPLICATION_ID, 16777250).
-define(IANA_PEN_3GPP, 10415).
-define(IANA_PEN_SigScale, 50386).
%% 3GPP TS 23.003 19.3.2 Root NAI
-define(PERM_AKAp, $6).
%% 3GPP TS 23.003 19.3.4 Fast Re-auth
-define(FAST_AKAp, $8).
%% 3GPP TS 23.003 19.3.5 Pseudonym
-define(TEMP_AKAp, $7).

main(Args) ->
	case options(Args) of
		#{help := true} = _Options ->
			usage();
		Options ->
			auth_session(Options)
	end.

auth_session(Options) ->
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
				{'Auth-Application-Id', [?STa_APPLICATION_ID]},
				{string_decode, false},
				{restrict_connections, false},
				{application, [{dictionary, diameter_gen_base_rfc6733},
						{module, Callback}]},
				{application, [{alias, sta},
						{dictionary, diameter_gen_3gpp_sta_application},
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
		EapId = 0,
		IMSI = maps:get(imsi, Options, "001001123456789"),
		Identity = iolist_to_binary([?PERM_AKAp, IMSI, $@, OriginRealm]),
      EapPacket = #eap_packet{code = response, type = ?Identity,
               identifier = EapId, data = Identity},
		EapMessage = ocs_eap_codec:eap_packet(EapPacket),
		DER = #'3gpp_sta_DER'{'Session-Id' = SId,
			'Auth-Application-Id' = ?STa_APPLICATION_ID,
			'Origin-Host' = Hostname,
			'Origin-Realm' = OriginRealm,
			'Destination-Realm' = OriginRealm,
         'Auth-Request-Type' = ?'3GPP_STA_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE', 
			'EAP-Payload' = EapMessage,
			'User-Name' = [Identity],
			'RAT-Type' = [?'3GPP_STA_RAT-TYPE_WLAN']},
		Fsta = fun('3gpp_sta_DEA', _N) ->
					record_info(fields, '3gpp_sta_DEA')
		end,
		Fbase = fun('diameter_base_answer-message', _N) ->
					record_info(fields, 'diameter_base_answer-message')
		end,
		case diameter:call(Name, sta, DER, []) of
			#'3gpp_sta_DEA'{} = Answer ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fsta)]);
			#'diameter_base_answer-message'{} = Answer ->
						io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fbase)]);
			{error, Reason} ->
						throw(Reason)
		end
	catch
		Error:Reason3 ->
			io:fwrite("~w: ~w~n", [Error, Reason3]),
			usage()
	end.

usage() ->
	Option1 = " [--imsi 001001123456789]",
	Option2 = " [--k 52EB3FD97F3CE03BB9B0877D5EC3AEE3]",
	Option3 = " [--opc 9852CAF72767C963A7D120A45CA07DBA]",
	Option4 = " [--interval 1000]",
	Option5 = " [--updates 1]",
	Option6 = " [--ip 127.0.0.1]",
	Option7 = " [--raddr 127.0.0.1]",
	Option8 = " [--rport 3868]",
	Options = [Option1, Option2, Option3, Option4, Option5, Option6, Option7, Option8],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	options(Args, #{}).
options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--imsi", IMSI | T], Acc) ->
	options(T, Acc#{imsi => IMSI});
options(["--k", K | T], Acc) ->
	options(T, Acc#{k => K});
options(["--opc", OPc | T], Acc) ->
	options(T, Acc#{opc => OPc});
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

