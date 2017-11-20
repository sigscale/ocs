%%% ocs_diameter_acct_port_server.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module receives {@link //diameter. diameter} messages on a port assigned
%%% 	for accounting in the {@link //ocs. ocs} application.
%%%% @reference <a href="https://tools.ietf.org/pdf/rfc4006.pdf">
%%%% 	RFC4006 - DIAMETER Credit-Control Application</a>
%%%
-module(ocs_diameter_acct_port_server).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-behaviour(gen_server).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_gen_ietf.hrl").
-include("../include/diameter_gen_3gpp.hrl").
-include("../include/diameter_gen_3gpp_ro_application.hrl").
-include("ocs.hrl").

-record(state,
		{acct_sup :: pid(),
		address :: inet:ip_address(),
		port :: non_neg_integer(),
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				(SessionId :: string()), Value :: (Fsm :: pid()))}).

-define(RO_APPLICATION_ID, 4).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_port_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The ocs_diameter_acct_port_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, State}
			| {ok, State, Timeout}
			| {stop, Reason} | ignore,
		State :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} server.
%% 	Args :: [Sup :: pid(), Module :: atom(), Port :: non_neg_integer(),
%% 	Address :: inet:ip_address()].
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([AcctSup, Address, Port, _Options]) ->
	State = #state{address = Address, port = Port, acct_sup = AcctSup},
	case ocs_log:acct_open() of
		ok ->
			process_flag(trap_exit, true),	
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(), 
		From :: {Pid, Tag},
		Pid :: pid(), 
		Tag :: any(),
		State :: state(),
		Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
		Reply :: term(),
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
handle_call(shutdown, _From, State) ->
	{stop, normal, ok, State};
handle_call({diameter_request, Caps, Request}, From, State) ->
	request(Request, Caps, From, State).

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(), 
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(_Request, State) ->
	{noreply, State}.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(), 
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info({'EXIT', _Pid, {shutdown, SessionId}},
		#state{handlers = Handlers} = State) ->
	NewHandlers = gb_trees:delete(SessionId, Handlers),
	NewState = State#state{handlers = NewHandlers},
	{noreply, NewState};
handle_info({'EXIT', Fsm, _Reason},
		#state{handlers = Handlers} = State) ->
	Fdel = fun(_F, {Key, Pid, _Iter}) when Pid == Fsm ->
				Key;
			(F, {_Key, _Val, Iter}) ->
				F(F, gb_trees:next(Iter));
			(_F, none) ->
				none
	end,
	Iter = gb_trees:iterator(Handlers),
	case Fdel(Fdel, gb_trees:next(Iter)) of
		none ->
			{noreply, State};
		Key ->
			NewHandlers = gb_trees:delete(Key, Handlers),
			NewState = State#state{handlers = NewHandlers},
			{noreply, NewState}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | term(), 
		State :: state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason,  _State) ->
	ocs_log:acct_close().

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: (Vsn | {down, Vsn}),
		Vsn :: term(),
		State :: state(), 
		Extra :: term(),
		Result :: {ok, NewState},
		NewState :: state().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec request(Request, Caps, From, State) -> Result
	when
		Request :: term(),
		Caps :: capabilities(),
		From :: {Pid, Tag}, 
		Pid :: pid(),
		Tag :: term(),
		State :: state(),
		Result :: {reply, Reply, NewState},
		Reply:: term(),
		NewState :: state().
%% @doc Handle a received DIAMETER Accounting packet.
%% @private
request(Request, Caps,  _From, State) ->
	#diameter_caps{origin_host = {OHost, DHost}, origin_realm = {ORealm, DRealm}} = Caps,
	#'3gpp_ro_CCR'{'Session-Id' = SId, 'User-Name' = NAISpecUName,
			'Auth-Application-Id' = ?RO_APPLICATION_ID,
			'Service-Context-Id' = _SvcContextId, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum, 'Subscription-Id' = SubscriptionIds} = Request,
	try
		Subscriber = case SubscriptionIds of
			[#'3gpp_ro_Subscription-Id'{'Subscription-Id-Data' = Sub} | _] ->
				Sub;
			[] ->
				case NAISpecUName of
					[] ->
						throw(no_subscriber_identification_information);
					NAI ->
						[_, Username | _] = string:tokens(NAI, ":@"),%% proto:username@realm
						Username
				end
		end,
		request1(RequestType, Request, SId, RequestNum,
				Subscriber, OHost, DHost, ORealm, DRealm, State)
	catch
		_:_Reason ->
			{Reply1, NewState1} = generate_diameter_error(SId,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, RequestType, RequestNum, State),
			{reply, Reply1, NewState1}
	end.

%% @hidden
request1(?'3GPP_CC-REQUEST-TYPE_INITIAL_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [MSCC | _],
		'Service-Information' = ServiceInformation} = Request,
		SId, RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm, State) ->
	RSU =  case MSCC of
		#'3gpp_ro_Multiple-Services-Credit-Control'{'Requested-Service-Unit' =
				[RequestedServiceUnits | _]} ->
			RequestedServiceUnits;
		_ ->
			throw(multiple_service_credit_control_avp_not_available)
	end,
	{ReqUsageType, ReqUsage} = case RSU of
		#'3gpp_ro_Requested-Service-Unit'{'CC-Time' = [CCTime]} when
				CCTime =/= [] ->
			{seconds, CCTime};
		#'3gpp_ro_Requested-Service-Unit'{'CC-Total-Octets' = [CCTotalOctets]} ->
			{octets, CCTotalOctets};
		#'3gpp_ro_Requested-Service-Unit'{'CC-Output-Octets' = [CCOutputOctets],
				'CC-Input-Octets' = [CCInputOctets]} when is_integer(CCInputOctets),
				is_integer(CCOutputOctets) ->
			{octets, CCOutputOctets + CCOutputOctets};
		_ ->
			throw(unsupported_request_units)
	end,
	Destination = get_destination(ServiceInformation),
	ReserveAmount = [{ReqUsageType, ReqUsage}],
	case ocs_rating:rate(diameter, Subscriber,
			Destination, initial, [], ReserveAmount, [{'Session-Id', SId}]) of
		{ok, _, GrantedAmount} ->
			GrantedUnits = case ReqUsageType of
				seconds ->
					#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedAmount]};
				octets ->
					#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [GrantedAmount]}
			end,
			{Reply, NewState} = generate_diameter_answer(Request, SId,
					GrantedUnits, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OHost, ORealm,
					RequestType, RequestNum, State),
			{reply, Reply, NewState};
		{out_of_credit, _SessionList} ->
			error_logger:warning_report(["out of credit",
					{module, ?MODULE}, {subscriber, Subscriber},
					{origin_host, OHost}]),
			{Reply, NewState} = generate_diameter_answer(Request, SId,
					undefined, ?'IETF_RESULT-CODE_CREDIT_LIMIT_REACHED', OHost,
					ORealm, RequestType, RequestNum, State),
			{reply, Reply, NewState};
		{disabled, _SessionList} ->
			{Reply, NewState} = generate_diameter_answer(Request, SId,
					undefined, ?'IETF_RESULT-CODE_END_USER_SERVICE_DENIED', OHost,
					ORealm, RequestType, RequestNum, State),
			{reply, Reply, NewState};
		{error, subscriber_not_found} ->
			error_logger:warning_report(["diameter accounting subscriber not found",
					{module, ?MODULE}, {subscriber, Subscriber},
					{origin_host, OHost}]),
			{Reply, NewState} = generate_diameter_error(SId, ?'IETF_RESULT-CODE_USER_UNKNOWN',
					OHost, ORealm, RequestType, RequestNum, State),
			{reply, Reply, NewState};
		{error, _Reason} ->
			{Reply, NewState} = generate_diameter_error(SId, ?'IETF_RESULT-CODE_RATING_FAILED',
					OHost, ORealm, RequestType, RequestNum, State),
			{reply, Reply, NewState}
	end;
request1(?'3GPP_CC-REQUEST-TYPE_UPDATE_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [MSCC | _],
		'Service-Information' = ServiceInformation} = Request,
		SId, RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm, State) ->
	try
		RSU =  case MSCC of
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Requested-Service-Unit' =
					[RequestedServiceUnits | _]} ->
				RequestedServiceUnits;
			_ ->
				throw(multiple_service_credit_control_avp_not_available)
		end,
		{ReqUsageType, ReqUsage} = case RSU of
			#'3gpp_ro_Requested-Service-Unit'{'CC-Time' = [CCTime]} when
					CCTime =/= [] ->
				{seconds, CCTime};
			#'3gpp_ro_Requested-Service-Unit'{'CC-Total-Octets' = [CCTotalOctets]} ->
				{octets, CCTotalOctets};
			#'3gpp_ro_Requested-Service-Unit'{'CC-Output-Octets' = [CCOutputOctets],
					'CC-Input-Octets' = [CCInputOctets]} when is_integer(CCInputOctets),
					is_integer(CCOutputOctets) ->
				{octets, CCOutputOctets + CCOutputOctets};
			_ ->
				throw(unsupported_request_units)
		end,
		USU =  case MSCC of
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' =
					[UsedServiceUnit | _]} ->
				UsedServiceUnit;
			_ ->
				throw(multiple_service_credit_control_avp_not_available)
		end,
		{UsedType, UsedUsage} = case USU of
			#'3gpp_ro_Used-Service-Unit'{'CC-Time' = [UsedCCTime]} when UsedCCTime =/= [] ->
				{seconds, UsedCCTime};
			#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [UsedCCTotalOctets]} ->
				{octets, UsedCCTotalOctets};
			#'3gpp_ro_Used-Service-Unit'{'CC-Output-Octets' = [UsedCCOutputOctets],
					'CC-Input-Octets' = [UsedCCInputOctets]} when is_integer(UsedCCInputOctets),
					is_integer(UsedCCOutputOctets) ->
				{octets, UsedCCInputOctets + UsedCCOutputOctets};
			[] ->
				throw(used_amount_not_available)
		end,
		Destination = get_destination(ServiceInformation),
		ReserveAmount = [{ReqUsageType, ReqUsage}],
		DebitAmount = [{UsedType, UsedUsage}],
		case ocs_rating:rate(diameter, Subscriber,
				Destination, interim, DebitAmount, ReserveAmount, [{'Session-Id', SId}]) of
			{ok, _, GrantedAmount} ->
				GrantedUnits = case ReqUsageType of
					seconds ->
						#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedAmount]};
					octets ->
						#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [GrantedAmount]}
				end,
				{Reply, NewState} = generate_diameter_answer(Request, SId,
						GrantedUnits, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OHost, ORealm,
						RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{out_of_credit, _SessionList} ->
				error_logger:warning_report(["out of credit",
						{module, ?MODULE}, {subscriber, Subscriber},
						{origin_host, OHost}]),
				{Reply, NewState} = generate_diameter_answer(Request, SId,
						undefined, ?'IETF_RESULT-CODE_CREDIT_LIMIT_REACHED', OHost,
						ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{disabled, _SessionList} ->
				{Reply, NewState} = generate_diameter_answer(Request, SId,
						undefined, ?'IETF_RESULT-CODE_END_USER_SERVICE_DENIED', OHost,
						ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{error, subscriber_not_found} ->
				error_logger:warning_report(["diameter accounting subscriber not found",
						{module, ?MODULE}, {subscriber, Subscriber},
						{origin_host, OHost}]),
				{Reply, NewState} = generate_diameter_error(SId, ?'IETF_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{error, _Reason} ->
				{Reply, NewState} = generate_diameter_error(SId, ?'IETF_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState}
		end
	catch
		_:_Reason1 ->
			{Reply1, NewState0} = generate_diameter_error(SId,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, RequestType, RequestNum, State),
			{reply, Reply1, NewState0}
	end;
request1(?'3GPP_CC-REQUEST-TYPE_TERMINATION_REQUEST' = RequestType,
		#'3gpp_ro_CCR'{'Multiple-Services-Credit-Control' = [MSCC | _],
		'Service-Information' = ServiceInformation} = Request,
		SId, RequestNum, Subscriber, OHost, _DHost, ORealm, _DRealm, State) ->
	try
		USU =  case MSCC of
			#'3gpp_ro_Multiple-Services-Credit-Control'{'Used-Service-Unit' =
					[UsedServiceUnit | _]} ->
				UsedServiceUnit;
			_ ->
				throw(multiple_service_credit_control_avp_not_available)
		end,
		{UsedType, UsedUsage} = case USU of
			#'3gpp_ro_Used-Service-Unit'{'CC-Time' = [CCTime]} when CCTime =/= [] ->
				{seconds, CCTime};
			#'3gpp_ro_Used-Service-Unit'{'CC-Total-Octets' = [CCTotalOctets]} ->
				{octets, CCTotalOctets};
			#'3gpp_ro_Used-Service-Unit'{'CC-Output-Octets' = [CCOutputOctets],
					'CC-Input-Octets' = [CCInputOctets]} when is_integer(CCInputOctets),
					is_integer(CCOutputOctets) ->
				{octets, CCInputOctets + CCOutputOctets};
			[] ->
				throw(used_amount_not_available)
		end,
		Destination = get_destination(ServiceInformation),
		DebitAmount = [{UsedType, UsedUsage}],
		case ocs_rating:rate(diameter, Subscriber, Destination,
				final, DebitAmount, [], [{'Session-Id', SId}]) of
			{ok, _, GrantedAmount} ->
				GrantedUnits = case UsedType of
					seconds ->
						#'3gpp_ro_Granted-Service-Unit'{'CC-Time' = [GrantedAmount]};
					octets ->
						#'3gpp_ro_Granted-Service-Unit'{'CC-Total-Octets' = [GrantedAmount]}
				end,
				{Reply, NewState} = generate_diameter_answer(Request, SId,
						GrantedUnits, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS', OHost, ORealm,
						RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{out_of_credit, _SessionList} ->
				error_logger:warning_report(["out of credit",
						{module, ?MODULE}, {subscriber, Subscriber},
						{origin_host, OHost}]),
				{Reply, NewState} = generate_diameter_answer(Request, SId,
						undefined, ?'IETF_RESULT-CODE_CREDIT_LIMIT_REACHED', OHost,
						ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{disabled, _SessionList} ->
				{Reply, NewState} = generate_diameter_answer(Request, SId,
						undefined, ?'IETF_RESULT-CODE_END_USER_SERVICE_DENIED', OHost,
						ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{error, subscriber_not_found} ->
				error_logger:warning_report(["diameter accounting subscriber not found",
						{module, ?MODULE}, {subscriber, Subscriber},
						{origin_host, OHost}]),
				{Reply, NewState} = generate_diameter_error(SId, ?'IETF_RESULT-CODE_USER_UNKNOWN',
						OHost, ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState};
			{error, _Reason} ->
				{Reply, NewState} = generate_diameter_error(SId, ?'IETF_RESULT-CODE_RATING_FAILED',
						OHost, ORealm, RequestType, RequestNum, State),
				{reply, Reply, NewState}
		end
	catch
		_:_Reason1 ->
			{Reply1, NewState0} = generate_diameter_error(SId,
					?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY', OHost,
					ORealm, RequestType, RequestNum, State),
			{reply, Reply1, NewState0}
	end.

-spec generate_diameter_answer(Request, SessionId, GrantedUnits,
		ResultCode, OriginHost, OriginRealm, RequestType, RequestNum,
		State) -> Result
			when
				Request :: #'3gpp_ro_CCR'{},
				SessionId :: string(),
				GrantedUnits :: undefined | #'3gpp_ro_Granted-Service-Unit'{},
				ResultCode :: integer(),
				OriginHost :: string(),
				OriginRealm :: string(),
				RequestType :: integer(),
				RequestNum :: integer(),
				Result :: {Reply, State},
				State :: state(),
				Reply :: #'3gpp_ro_CCA'{}.
%% @doc Send CCA to DIAMETER client indicating a successful operation.
%% @hidden
generate_diameter_answer(Request, SId, undefined, ResultCode, OHost,
		ORealm, RequestType, RequestNum, #state{address = Address,
		port = Port} = State) ->
	Reply = #'3gpp_ro_CCA'{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	Server = {Address, Port},
	ok = ocs_log:acct_log(diameter, Server,
			accounting_event_type(RequestType), Request, Reply),
	{Reply, State};
generate_diameter_answer(Request, SId, GrantedUnits, ResultCode, OHost,
		ORealm, RequestType, RequestNum, #state{address = Address,
		port = Port} = State) ->
	MultiServices_CC = #'3gpp_ro_Multiple-Services-Credit-Control'{
			'Granted-Service-Unit' = [GrantedUnits]},
	Reply = #'3gpp_ro_CCA'{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum,
			'Multiple-Services-Credit-Control' = [MultiServices_CC]},
	Server = {Address, Port},
	ok = ocs_log:acct_log(diameter, Server,
			accounting_event_type(RequestType), Request, Reply),
	{Reply, State}.

-spec generate_diameter_error(SessionId, ResultCode, OriginHost,
		OriginRealm, RequestType, RequestNum, State) -> Result
	when
		SessionId :: string(),
		ResultCode :: integer(),
		OriginHost :: string(),
		OriginRealm :: string(),
		RequestType :: integer(),
		RequestNum :: integer(),
		State :: state(),
		Result :: {Reply, State},
		Reply :: #'3gpp_ro_CCA'{}.
%% @doc Send CCA to DIAMETER client indicating an operation failure.
%% @hidden
generate_diameter_error(SId, ResultCode, OHost,
		ORealm, RequestType, RequestNum, State) ->
	Reply = #'3gpp_ro_CCA'{'Session-Id' = SId, 'Result-Code' = ResultCode,
			'Origin-Host' = OHost, 'Origin-Realm' = ORealm,
			'Auth-Application-Id' = ?RO_APPLICATION_ID, 'CC-Request-Type' = RequestType,
			'CC-Request-Number' = RequestNum},
	{Reply, State}.

-spec accounting_event_type(RequestType) -> EventType
	when
	RequestType :: 1..4,
	EventType :: start | interim | stop | event.
%% @doc Converts CC-Request-Type integer value to a readable atom.
accounting_event_type(1) -> start;
accounting_event_type(2) -> interim;
accounting_event_type(3) -> stop.

%% @hidden
get_destination([#'3gpp_ro_Service-Information'{'IMS-Information' = ImsInfo}]) ->
	get_destination(ImsInfo);
get_destination([#'3gpp_ro_IMS-Information'{'Called-Party-Address' = [CalledParty]}]) ->
	destination(CalledParty).

%% @hidden
destination(<<"tel:", Dest/binary>>) ->
	binary_to_list(Dest);
destination(Dest) ->
	binary_to_list(Dest).

