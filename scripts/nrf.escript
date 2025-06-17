#!/usr/bin/env escript
%% vim: syntax=erlang

main(Args) ->
	case options(Args) of
		#{help := true} = _Options ->
			usage();
		Options ->
			data_session(Options)
	end.

data_session(Options) ->
	try
		Verbose = maps:get(verbose, Options, false),
		NfName = escript:script_name(),
		SequenceNumber = 0,
		URL = maps:get(url, Options, "http://localhost:8080"),
		HttpOptions = case lists:prefix("https:", URL) of
			true ->
				ok = application:start(crypto),
				ok = application:start(asn1),
				ok = application:start(public_key),
				ok = application:start(ssl),
				[{ssl, [{verify, verify_none}]}];
			false ->
				[]
		end,
		ok = inets:start(),
		IMSI = "imsi-" ++ maps:get(imsi, Options, "001001123456789"),
		MSISDN = "msisdn-" ++ maps:get(msisdn, Options, "14165551234"),
		SubscriptionId = [$[, $", IMSI, $", $,, $", MSISDN, $", $]],
		ServiceContextId = maps:get(context, Options, "32251@3gpp.org"),
		ServiceInformation = [${, $}],
		RequestSubType = "RESERVE",
		RequestedUnit = [${, $}],
		ServiceRating = [$[, service_rating(ServiceContextId,
				ServiceInformation, RequestSubType, RequestedUnit), $]],
		RatingData = rating_data(NfName, SequenceNumber,
				SubscriptionId, ServiceRating),
		BasePath = maps:get(path, Options, "/nrf-rating/v1"),
		ResourcePath = URL ++ BasePath ++ "/ratingdata", 
		BasicAuth = base64:encode(maps:get(auth, Options, "admin:admin")),
		Authorization = {"authorization", "Basic " ++ BasicAuth},
		Accept = {"accept", "application/json, application/problem+json"},
		RequestHeaders =  [Authorization, Accept],
		ContentType = "application/json",
		Request = {ResourcePath, RequestHeaders, ContentType, RatingData}, 
		RequestOptions = [],
		print_request(Verbose, Request),
		ResourcePath1 = case httpc:request(post,
				Request, HttpOptions, RequestOptions) of
			{ok, {{_, 201, _}, ResponseHeaders, _} = Result} ->
				print_response(Verbose, Result),
				case lists:keyfind("location", 1, ResponseHeaders) of
					{_, Location} ->
						URL ++ Location;
					false ->
						throw(no_location)
				end;
			{ok, {{_, StatusCode, _}, _, _} = Result} ->
				print_response(Verbose, Result),
				throw(StatusCode);
			{error, Reason} ->
				throw(Reason)
		end,
		timer:sleep(maps:get(interval, Options, 1000)),
		Fupdate = fun F(0, SeqNum) ->
					SeqNum;
				F(N, SeqNum) ->
					NewSeqNum = SeqNum + 1,
					UsedUnits = rand:uniform(1000000),
					ConsumedUnit = consumed_units(UsedUnits),
					ServiceRating1 = [$[,
							service_rating(ServiceContextId,
									ServiceInformation, "RESERVE", RequestedUnit), $,,
							service_rating(ServiceContextId,
									ServiceInformation, "DEBIT", ConsumedUnit), $]],
					RatingData1 = rating_data(NfName, NewSeqNum,
							SubscriptionId, ServiceRating1),
					ResourcePath2 = ResourcePath1 ++ "/update", 
					Request1 = {ResourcePath2, RequestHeaders, ContentType, RatingData1}, 
					print_request(Verbose, Request1),
					case httpc:request(post, Request1, HttpOptions, RequestOptions) of
						{ok, {{_, 200, _}, _, _} = Result1} ->
							print_response(Verbose, Result1);
						{ok, {{_, StatusCode1, _}, _, _} = Result1} ->
							print_response(Verbose, Result1),
							throw(StatusCode1);
						{error, Reason1} ->
							throw(Reason1)
					end,
					timer:sleep(maps:get(interval, Options, 1000)),
					F(N - 1, NewSeqNum)
		end,
		SequenceNumber1 = Fupdate(maps:get(updates, Options, 1), SequenceNumber),
		UsedUnits1 = rand:uniform(1000000),
		ConsumedUnit1 = consumed_units(UsedUnits1),
		ServiceRating2 = [$[, service_rating(ServiceContextId,
				ServiceInformation, "DEBIT", ConsumedUnit1), $]],
		RatingData2 = rating_data(NfName, SequenceNumber1,
				SubscriptionId, ServiceRating2),
		ResourcePath3 = ResourcePath1 ++ "/release", 
		Request2 = {ResourcePath3, RequestHeaders, ContentType, RatingData2}, 
		print_request(Verbose, Request2),
		case httpc:request(post, Request2, HttpOptions, RequestOptions) of
			{ok, {{_, 200, _}, _, _} = Result2} ->
				print_response(Verbose, Result2);
			{ok, {{_, StatusCode2, _}, _, _} = Result2} ->
				print_response(Verbose, Result2),
				throw(StatusCode2);
			{error, Reason2} ->
				throw(Reason2)
		end
	catch
		Error:Reason3 ->
			io:fwrite("~w: ~w~n", [Error, Reason3]),
			usage()
	end.

service_rating(ServiceContextId, ServiceInformation,
		"RESERVE" = RequestSubType, RequestedUnit) ->
	[${, $", "requestSubType", $", $:, $", RequestSubType, $", $,,
			$", "serviceContextId", $", $:, $", ServiceContextId, $", $,,
			$", "serviceInformation", $", $:, ServiceInformation, $,,
			$", "requestedUnit", $", $:, RequestedUnit,$}];
service_rating(ServiceContextId, ServiceInformation,
		"DEBIT" = RequestSubType, ConsumedUnit) ->
	[${, $", "requestSubType", $", $:, $", RequestSubType, $", $,,
			$", "serviceContextId", $", $:, $", ServiceContextId, $", $,,
			$", "serviceInformation", $", $:, ServiceInformation, $,,
			$", "consumedUnit", $", $:, ConsumedUnit, $}].

rating_data(NfName, SeqNum, SubscriptionId, ServiceRating) ->
	[${, $", "nfConsumerIdentification", $", $:, ${,
			$", "nFName", $", $:, $", NfName, $", $,,
			$", "nodeFunctionality", $", $:, $", "SMF", $", $}, $,,
			$", "invocationTimeStamp", $", $:,
					$", ocs_rest:iso8601(erlang:system_time(millisecond)),
					$Z, $", $,,
			$", "invocationSequenceNumber", $", $:,
					integer_to_list(SeqNum), $,,
			$", "subscriptionId", $", $:, SubscriptionId, $,,
			$", "serviceRating", $", $:, ServiceRating, $}].

consumed_units(UsedUnits) ->
	[${, $", "totalVolume", $", $:, integer_to_list(UsedUnits), $}].

print_request(true = _Verbose,
		{ResourcePath, RequestHeaders, ContentType, RequestBody}) ->
	io:fwrite("~nPOST ~s~n", [ResourcePath]),
	F = fun({FieldName, FieldValue}) ->
				io:fwrite("~s: ~s~n", [FieldName, FieldValue])
	end,
	lists:foreach(F, RequestHeaders),
	io:fwrite("content-type: ~s~n", [ContentType]),
	io:fwrite("~n~s~n", [RequestBody]);
print_request(false, _) ->
	ok.

print_response(true = _Verbose,
		{{HttpVersion, StatusCode, StatusLine}, ResponseHeaders, ResponseBody}) ->
	io:fwrite("~n~s ~b ~s~n", [HttpVersion, StatusCode, StatusLine]),
	F = fun({FieldName, FieldValue}) ->
				io:fwrite("~s: ~s~n", [FieldName, FieldValue])
	end,
	lists:foreach(F, ResponseHeaders),
	io:fwrite("~n~s~n", [ResponseBody]);
print_response(false, _) ->
	ok.

usage() ->
	Option1 = " [--verbose]",
	Option2 = " [--msisdn 14165551234]",
	Option3 = " [--imsi 001001123456789]",
	Option4 = " [--interval 1000]",
	Option5 = " [--updates 1]",
	Option6 = " [--url http://localhost:8080]",
	Option7 = " [--path /nrf-rating/v1]",
	Option8 = " [--auth admin:admin]",
	Option9 = " [--context 32251@3gpp.org]",
	Options = [Option1, Option2, Option3, Option4, Option5,
			Option6, Option7, Option8, Option9],
	Format = lists:flatten(["usage: ~s", Options, "~n"]),
	io:fwrite(Format, [escript:script_name()]),
	halt(1).

options(Args) ->
	options(Args, #{}).
options(["--help" | T], Acc) ->
	options(T, Acc#{help => true});
options(["--verbose" | T], Acc) ->
	options(T, Acc#{verbose => true});
options(["--imsi", IMSI | T], Acc) ->
	options(T, Acc#{imsi=> IMSI});
options(["--msisdn", MSISDN | T], Acc) ->
	options(T, Acc#{msisdn => MSISDN});
options(["--interval", MS | T], Acc) ->
	options(T, Acc#{interval => list_to_integer(MS)});
options(["--updates", N | T], Acc) ->
	options(T, Acc#{updates => list_to_integer(N)});
options(["--url", URL | T], Acc) ->
	options(T, Acc#{url => URL});
options(["--path", BasePath | T], Acc) ->
	options(T, Acc#{path => BasePath});
options(["--auth", BasicAuth| T], Acc) ->
	options(T, Acc#{auth => BasicAuth});
options(["--context", ServiceContextId | T], Acc) ->
	options(T, Acc#{context => ServiceContextId});
options([_H | _T], _Acc) ->
	usage();
options([], Acc) ->
	Acc.

