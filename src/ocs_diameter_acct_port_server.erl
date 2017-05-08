#diameter_caps{origin_host = {OHost,_}, origin_realm = {ORealm, _}} = Caps,
	#diameter_cc_app_CCR{'Session-Id' = SId,
			'Auth-Application-Id' = ?CC_APPLICATION_ID,
			'Service-Context-Id' = _SvcContextId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum} = Request,
	try
		Subscriber = case Request#diameter_cc_app_CCR.'Subscription-Id' of
			undefined ->
				case Request#diameter_cc_app_CCR.'User-Name' of
					undefined ->
						throw(no_subscriber_identification_information);
					UserName ->
						UserName
				end;
			SubscriptionId ->
				SubscriptionId
		end,
		request1(RequestType, SId, RequestNum, Subscriber, OHost, ORealm, State)
	catch
		_:_ ->
			send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
					OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
					RequestNum, State)
	end.

%% @hidden
request1('DIAMETER_CC_APP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		SId, RequestNum,Subscriber, OHost, ORealm, State) ->
	try
		case ocs:find_subscriber(Subscriber) of
			{ok, Password, _, Balance, true}  ->
				case ocs:authorize(Subscriber, Password) of
					{ok, _, _} ->
						send_answer(SId, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
								OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
								RequestNum, Balance, State);
					{error, _Reason1} ->
						send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
								OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
								RequestNum, State)
				end;
			{error, _Reason} ->
				send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
						OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
						RequestNum, State)
		end
	catch
		_:_ ->
			send_error(SId, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY',
				OHost, ORealm, ?CC_APPLICATION_ID, RequestType,
				RequestNum, State)
	end.

-spec send_answer(SessionId, ResultCode, OriginHost, OriginRealm,
		AuthAppId, RequestType, RequestNum, GrantedUnits, State) -> Result
			when
				SessionId :: string(),
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				AuthAppId :: integer(),
				RequestType :: integer(),
				RequestNum :: integer(),
				GrantedUnits :: integer(),
				Result :: {reply, Reply, State},
				State :: state(),
				Reply :: #diameter_cc_app_CCA{}.
%% @doc Send CCA to Diameter client indicating a successful operation.
%% @hidden
send_answer(SId, ResultCode, OHost, ORealm, AuthAppId, RequestType,
		RequestNum, GrantedUnits, State) ->
	Reply = #diameter_cc_app_CCA{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = AuthAppId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum, 'Granted-Service-Unit' = GrantedUnits},
	{reply, Reply, State}.

-spec send_error(SessionId, ResultCode, OriginHost, OriginRealm,
		AuthAppId, RequestType, RequestNum, State) -> Result
			when
				SessionId :: string(),
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				AuthAppId :: integer(),
				RequestType :: integer(),
				RequestNum :: integer(),
				State :: state(),
				Result :: {reply, Reply, State},
				Reply :: #diameter_cc_app_CCA{}.
%% @doc Send CCA to Diameter client indicating a operation faliure.
%% @hidden
send_error(SId, ResultCode, OHost, ORealm, AuthAppId, RequestType,
		RequestNum, State) ->
	Reply = #diameter_cc_app_CCA{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = AuthAppId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	{reply, Reply, State}.

