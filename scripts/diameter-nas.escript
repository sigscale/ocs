#!/usr/bin/env escript
%% vim: syntax=erlang

-include_lib("ocs/include/diameter_gen_nas_application_rfc7155.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-define(NAS_APPLICATION_ID, 1).
-define(IANA_PEN_SigScale, 50386).

main(Args) ->
	case options(Args) of
		#{help := true} = _Options ->
			usage();
		Options ->
			nas_session(Options)
	end.

nas_session(Options) ->
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
				{'Product-Name', "SigScale Test Script"},
				{'Auth-Application-Id', [?NAS_APPLICATION_ID]},
				{string_decode, false},
				{restrict_connections, false},
				{application, [{dictionary, diameter_gen_base_rfc6733},
						{module, Callback}]},
				{application, [{alias, nas},
						{dictionary, diameter_gen_nas_application_rfc7155},
						{module, Callback}]}],
		true = diameter:subscribe(Name),
		ok = diameter:start_service(Name, ServiceOptions),
		receive
			#diameter_event{service = Name, info = start} ->
				ok
		end,
		TransportModule =  maps:get(transport, Options, diameter_tcp),
		TransportOptions =  [{transport_module, TransportModule},
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
		AAR = #{diameter_nas_app_AAR{'Session-Id' = SId,
				'Origin-Host' = Hostname,
				'Origin-Realm' = OriginRealm,
				'Destination-Realm' = OriginRealm,
				'Auth-Application-Id' = ?NAS_APPLICATION_ID,
				'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
				'User-Name' = [list_to_binary(Name)],
		},
		Fnas = fun(diameter_nas_app_AAA, _N) ->
					record_info(fields, diameter_nas_app_AAA)
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
		case diameter:call(Name, nas, AAR, []) of
			#diameter_nas_app_AAA{'Session-Id' = SId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'}
					= Answer ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fbase)]),
				nas_session(Options, Answer);
			#diameter_nas_app_AAA{'Session-Id' = SId,
					'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_MULTI_ROUND_AUTH'}
					= Answer ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fbase)]),
				nas_session(Options, Answer);
			#'diameter_base_answer-message'{'Session-Id' = SId,
					'Result-Code' = ResultCode} = Answer ->
				io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fbase)]),
				throw(ResultCode);
			{error, Reason} ->
				error(Reason)
		end
	catch
		throw:_Reason ->
			halt(1);
		error:Reason ->
			io:fwrite("~w: ~w~n", [error, Reason]),
			halt(1);
		exit:Reason ->
			io:fwrite("~w: ~w~n", [error, Reason]),
			usage()
	end.

nas_session(Options, Answer) ->

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
	Option12 = " [--transport tcp]",
	Option13 = " [--ip 127.0.0.1]",
	Option14 = " [--raddr 127.0.0.1]",
	Option15 = " [--rport 3868]",
	Options = [Option1, Option2, Option3, Option4, Option5,
			Option6, Option7, Option8, Option9, Option10,
			Option11, Option12, Option13, Option14, Option15],
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
options(["--transport", "tcp" | T], Acc) ->
	options(T, Acc#{transport => diameter_tcp});
options(["--transport", "sctp" | T], Acc) ->
	options(T, Acc#{transport => diameter_sctp});
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

