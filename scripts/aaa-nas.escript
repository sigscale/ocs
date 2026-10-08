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
						{rport, maps:get(rport, Options, 3869)},
						{ip, maps:get(ip, Options, {127,0,0,1})}]}],
		{ok, _Ref} = diameter:add_transport(Name, {connect, TransportOptions}),
		receive
			#diameter_event{service = Name, info = Info}
					when element(1, Info) == up ->
				ok;
			#diameter_event{service = Name, info = Info} ->
				error(Info)
		end,
		SId = list_to_binary(diameter:session_id(Hostname)),
		Username = maps:get(username, Options, "001001123456789"),
		Password = maps:get(password, Options, "secret"),
		NasIdentifier = maps:get(nas_identifier, Options, "nas-1"),
		IpAddress = maps:get(nas_ip_address, Options, "10.10.10.10"),
		{ok, {N1, N2, N3, N4}} = inet:parse_ipv4_address(IpAddress),
		NasIpAddress = <<N1, N2, N3, N4>>,
		NasPortType = maps:get(nas_port_type, Options,
				?'DIAMETER_NAS_APP_NAS-PORT-TYPE_FTTP'),
		ServiceType = maps:get(service_type, Options,
				?'DIAMETER_NAS_APP_SERVICE-TYPE_FRAMED'),
		AAR = #diameter_nas_app_AAR{'Session-Id' = SId,
			'Auth-Application-Id' = ?NAS_APPLICATION_ID,
			'Origin-Host' = Hostname,
			'Origin-Realm' = OriginRealm,
			'Destination-Realm' = OriginRealm,
			'Auth-Request-Type' = ?'DIAMETER_NAS_APP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
			'User-Name' = [Username],
			'User-Password' = [Password],
         'NAS-Identifier' = [NasIdentifier],
         'NAS-IP-Address' = [NasIpAddress],
			'NAS-Port-Type' = [NasPortType],
			'Service-Type' = [ServiceType]},
		Faaa = fun(diameter_nas_app_AAA, _N) ->
					record_info(fields, diameter_nas_app_AAA)
		end,
		Fbase = fun('diameter_base_answer-message', _N) ->
					record_info(fields, 'diameter_base_answer-message')
		end,
		case diameter:call(Name, nas, AAR, []) of
			#diameter_nas_app_AAA{'Session-Id' = SId,
						'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Answer ->
					io:fwrite("~s~n", [io_lib_pretty:print(Answer, Faaa)]);
			#diameter_nas_app_AAA{'Session-Id' = SId,
						'Result-Code' = ResultCode} = Answer ->
					io:fwrite("~s~n", [io_lib_pretty:print(Answer, Faaa)]),
					throw(ResultCode);
			#'diameter_base_answer-message'{'Session-Id' = SId,
						'Result-Code' = ResultCode} = Answer ->
					io:fwrite("~s~n", [io_lib_pretty:print(Answer, Fbase)]),
					throw(ResultCode);
			{error, Reason} ->
					error(Reason)
		end
	catch
		throw:_Reason3 ->
			halt(1);
		error:Reason3 ->
			io:fwrite("~w: ~p~n", [error, Reason3]),
			halt(1);
		exit:Reason3 ->
			io:fwrite("~w: ~p~n", [error, Reason3]),
			usage()
	end.

usage() ->
	Option1 = " [--username 001001123456789]",
	Option2 = " [--password secret]",
	Option3 = " [--nas-identifier nas-1]",
	Option4 = " [--nas-ip-address 10.10.10.10]",
	Option5 = " [--nas-port-type 25]",
	Option6 = " [--service-type 2]",
	Option7 = " [--transport tcp]",
	Option8 = " [--ip 127.0.0.1]",
	Option9 = " [--raddr 127.0.0.1]",
	Option10 = " [--rport 3869]",
	Options = [Option1, Option2, Option3, Option4, Option5,
			Option6, Option7, Option8, Option9, Option10],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	options(Args, #{}).
options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--username", Username | T], Acc) ->
	options(T, Acc#{username => Username});
options(["--password", Password | T], Acc) ->
	options(T, Acc#{password => Password});
options(["--nas-identifier", NasIdentifier | T], Acc) ->
	options(T, Acc#{nas_identifier => NasIdentifier});
options(["--nas-ip-address", NasIpAddress | T], Acc) ->
	options(T, Acc#{nas_ip_address => NasIpAddress});
options(["--nas-port-type", NasPortType | T], Acc) ->
	options(T, Acc#{nas_port_type => NasPortType});
options(["--service-type", ServiceType | T], Acc) ->
	options(T, Acc#{service_type => ServiceType});
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

