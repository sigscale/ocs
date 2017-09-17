%%% ocs_rest_res_subscriber.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //ocs. ocs} application.
%%%
-module(ocs_rest_res_subscriber).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_subscribers/1, get_subscriber/2, post_subscriber/1,
		patch_subscriber/4, delete_subscriber/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_subscriber(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber/{id}'
%% requests.
get_subscriber(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_subscriber(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_subscriber(Id, Query, [])
	end.
%% @hidden
get_subscriber(Id, [] = _Query, Filters) ->
	get_subscriber1(Id, Filters);
get_subscriber(_Id, _Query, _Filters) ->
	{error, 400}.
%% @hidden
get_subscriber1(Id, Filters) ->
	case ocs:find_subscriber(Id) of
		{ok, #subscriber{password = PWBin, attributes = Attributes,
				balance = Balance, enabled = Enabled, last_modified = LM, 
				multisession = Multi}} ->
			Etag = etag(LM),
			Att = radius_to_json(Attributes),
			Att1 = {array, Att},
			Password = binary_to_list(PWBin),
			RespObj1 = [{"id", Id}, {"href", "/ocs/v1/subscriber/" ++ Id}],
			RespObj2 = [{"attributes", Att1}],
			RespObj3 = case Filters == []
				orelse lists:member("password", Filters) of
					true ->
						[{"password", Password}];
					false ->
						[]
				end,
			RespObj4 = case Filters == []
				orelse lists:member("balance", Filters) of
					true ->
						[{"balance", Balance}];
					false ->
						[]
				end,
			RespObj5 = case Filters == []
				orelse lists:member("enabled", Filters) of
					true ->
						[{"enabled", Enabled}];
					false ->
						[]
				end,
			RespObj6 = case Filters == []
				orelse lists:member("multisession", Filters) of
					true ->
						[{"multisession", Multi}];
					false ->
						[]
				end,
			JsonObj  = {struct, RespObj1 ++ RespObj2 ++ RespObj3
					++ RespObj4 ++ RespObj5 ++ RespObj6},
			Body = mochijson:encode(JsonObj),
			Headers = [{content_type, "application/json"}, {etag, Etag}],
			{ok, Headers, Body};
		{error, not_found} ->
			{error, 404}
	end.

-spec get_subscribers(Query) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber'
%% requests.
get_subscribers(Query) ->
	case ocs:get_subscribers() of
		{error, _} ->
			{error, 404};
		Subscribers ->
			case lists:keytake("fields", 1, Query) of
				{value, {_, L}, NewQuery} ->
					get_subscribers(Subscribers, NewQuery, string:tokens(L, ","));
				false ->
					get_subscribers(Subscribers, Query, [])
			end
	end.
%% @hidden
get_subscribers(Subscribers, Query, Filters) ->
	try
		case lists:keytake("sort", 1, Query) of
			{value, {_, "id"}, NewQuery} ->
				{lists:keysort(#subscriber.name, Subscribers), NewQuery};
			{value, {_, "-id"}, NewQuery} ->
				{lists:reverse(lists:keysort(#subscriber.name, Subscribers)), NewQuery};
			{value, {_, "password"}, NewQuery} ->
				{lists:keysort(#subscriber.password, Subscribers), NewQuery};
			{value, {_, "-password"}, NewQuery} ->
				{lists:reverse(lists:keysort(#subscriber.password, Subscribers)), NewQuery};
			{value, {_, "balance"}, NewQuery} ->
				{lists:keysort(#subscriber.balance, Subscribers), NewQuery};
			{value, {_, "-balance"}, NewQuery} ->
				{lists:reverse(lists:keysort(#subscriber.balance, Subscribers)), NewQuery};
			{value, {_, "enabled"}, NewQuery} ->
				{lists:keysort(#subscriber.enabled, Subscribers), NewQuery};
			{value, {_, "-enabled"}, NewQuery} ->
				{lists:reverse(lists:keysort(#subscriber.enabled, Subscribers)), NewQuery};
			{value, {_, "multisession"}, NewQuery} ->
				{lists:keysort(#subscriber.multisession, Subscribers), NewQuery};
			{value, {_, "-multisession"}, NewQuery} ->
				{lists:reverse(lists:keysort(#subscriber.multisession, Subscribers)), NewQuery};
			false ->
				{Subscribers, Query};
			_ ->
				throw(400)
		end
	of
		{SortedSubscribers, NextQuery} ->
			get_subscribers1(SortedSubscribers, NextQuery, Filters)
	catch
		throw:400 ->
			{error, 400}
	end.
%% @hidden
get_subscribers1(Subscribers, Query, Filters) ->
	{Id, Query1} = case lists:keytake("id", 1, Query) of
		{value, {_, V1}, Q1} ->
			{V1, Q1};
		false ->
			{[], Query}
	end,
	{Password, Query2} = case lists:keytake("password", 1, Query1) of
		{value, {_, V2}, Q2} ->
			{V2, Q2};
		false ->
			{[], Query1}
	end,
	{Balance, Query3} = case lists:keytake("balance", 1, Query2) of
		{value, {_, V3}, Q3} ->
			{V3, Q3};
		false ->
			{[], Query2}
	end,
	{Enabled, Query4} = case lists:keytake("enabled", 1, Query3) of
		{value, {_, V4}, Q4} ->
			{V4, Q4};
		false ->
			{[], Query3}
	end,
	{Multi, Query5} = case lists:keytake("multisession", 1, Query4) of
		{value, {_, V5}, Q5} ->
			{V5, Q5};
		false ->
			{[], Query4}
	end,
	get_subscribers2(Subscribers, Id, Password, Balance, Enabled, Multi, Query5, Filters).
%% @hidden
get_subscribers2(Subscribers, Id, Password, Balance, Enabled, Multi, [] = _Query, Filters) ->
	F = fun(#subscriber{name = Na, password = Pa, attributes = Attributes, 
			balance = Ba, enabled = Ena, multisession = Mul}) ->
		Nalist = binary_to_list(Na),
		T1 = lists:prefix(Id, Nalist),
		Palist = binary_to_list(Pa),
		T2 = lists:prefix(Password, Palist),
		Att = radius_to_json(Attributes),
		Att1 = {array, Att},
		Balist = integer_to_list(Ba),
		T3 = lists:prefix(Balance, Balist),
		T4 = lists:prefix(Enabled, [Ena]),
		T5 = lists:prefix(Multi, [Mul]),
		if
			T1 and T2 and T3 and T4 and T5->
				RespObj1 = [{"id", Nalist}, {"href", "/ocs/v1/subscriber/" ++ Nalist}],
				RespObj2 = [{"attributes", Att1}],
				RespObj3 = case Filters == []
						orelse lists:member("password", Filters) of
					true ->
						[{"password", Palist}];
					false ->
						[]
				end,
				RespObj4 = case Filters == []
						orelse lists:member("balance", Filters) of
					true ->
						[{"balance", Ba}];
					false ->
						[]
				end,
				RespObj5 = case Filters == []
						orelse lists:member("enabled", Filters) of
					true ->
						[{"enabled", Ena}];
					false ->
						[]
				end,
				RespObj6 = case Filters == []
						orelse lists:member("multisession", Filters) of
					true ->
						[{"multisession", Mul}];
					false ->
						[]
				end,
				{true, {struct, RespObj1 ++ RespObj2 ++ RespObj3
							++ RespObj4 ++ RespObj5 ++ RespObj6}};
			true ->
				false
		end
	end,
	try
		JsonObj = lists:filtermap(F, Subscribers),
		Size = integer_to_list(length(JsonObj)),
		ContentRange = "items 1-" ++ Size ++ "/" ++ Size,
		Body  = mochijson:encode({array, lists:reverse(JsonObj)}),
		{ok, [{content_type, "application/json"},
				{content_range, ContentRange}], Body}
	catch
		_:_Reason ->
			{error, 500}
	end;
get_subscribers2(_, _, _, _, _, _, _, _) ->
	{error, 400}.

-spec post_subscriber(RequestBody) -> Result 
	when 
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/subscriber' and add a new `subscriber'
%% resource.
post_subscriber(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		IdIn = case lists:keyfind("id", 1, Object) of
			{"id", ID} ->
				ID;
			false ->
				undefined
		end,
		PasswordIn = case lists:keyfind("password", 1, Object) of
			{"password", Pass} ->
				Pass;
			false ->
				undefined
		end,
		Attributes = case lists:keyfind("attributes", 1, Object) of
			{_, {array, JsonObjList}} ->
				json_to_radius(JsonObjList);
			false ->
				[]
		end,
		Balance = case lists:keyfind("balance", 1, Object) of
			{"balance", Bal} ->
				Bal;
			false ->
				undefined
		end,
		Enabled = case lists:keyfind("enabled", 1, Object) of
			{_, En} ->
				En;
			false ->
				undefined
		end,
		Multi = case lists:keyfind("multisession", 1, Object) of
			{_, Mu} ->
				Mu;
			false ->
				undefined
		end,
		case ocs:add_subscriber(IdIn, PasswordIn, Attributes, Balance, Enabled, Multi) of
			{ok, #subscriber{name = IdOut, last_modified = LM} = S} ->
				Id = binary_to_list(IdOut),
				Location = "/ocs/v1/subscriber/" ++ Id,
				JAttributes = {array, radius_to_json(S#subscriber.attributes)},
				RespObj = [{id, Id}, {href, Location},
						{password, binary_to_list(S#subscriber.password)},
						{attributes, JAttributes}, {balance, S#subscriber.balance},
						{enabled, S#subscriber.enabled},
						{multisession, S#subscriber.multisession}],
				JsonObj  = {struct, RespObj},
				Body = mochijson:encode(JsonObj),
				Headers = [{location, Location}, {etag, etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec patch_subscriber(Id, Etag, ContenType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | list(),
		ContenType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/subscriber/{id}' request and
%% Updates a existing `subscriber''s password or attributes. 
patch_subscriber(Id, undefined, CType, ReqBody) ->
	patch_subscriber1(Id, undefined, CType, ReqBody);
patch_subscriber(Id, Etag, CType, ReqBody) ->
	try
		Etag1 = etag(Etag),
		patch_subscriber1(Id, Etag1, CType, ReqBody)
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
patch_subscriber1(Id, Etag, CType, ReqBody) ->
	case ocs:find_subscriber(Id) of
		{ok, #subscriber{password = CurrPassword, attributes = CurrAttr,
				balance = Bal, enabled = Enabled,
				multisession = Multi, last_modified = CurrentEtag}}
				when Etag == CurrentEtag; Etag == undefined ->
			patch_subscriber2(Id, Etag, CType, ReqBody, CurrPassword, CurrAttr,
					Bal, Enabled, Multi);
		{ok,  _} ->
			{error, 412};
		{error, _} ->
			{error, 404}
	end.
%% @hidden
patch_subscriber2(Id, Etag, "application/json", ReqBody, CurrPassword,
		CurrAttr, Bal, Enabled, Multi) ->
	try
		{struct, Object} = mochijson:decode(ReqBody),
		{_, Type} = lists:keyfind("update", 1, Object),
		{Password, RadAttr, NewEnabled, NewMulti} = case Type of
			"attributes" ->
				{_, {array, AttrJs}} = lists:keyfind("attributes", 1, Object),
				NewAttributes = json_to_radius(AttrJs),
				{_, Balance} = lists:keyfind("balance", 1, Object),
				{_, EnabledStatus} = lists:keyfind("enabled", 1, Object),
				{_, MultiSession} = lists:keyfind("multisession", 1, Object),
				ocs:update_attributes(Id, Balance, NewAttributes, EnabledStatus, MultiSession),
				{CurrPassword, NewAttributes, EnabledStatus, MultiSession};
			"password" ->
				{_, NewPassword } = lists:keyfind("newpassword", 1, Object),
				ocs:update_password(Id, NewPassword),
				{NewPassword, CurrAttr, Enabled, Multi}
		end,
		patch_subscriber3(Id, Etag, Password, RadAttr, Bal, NewEnabled, NewMulti)
	catch
		_:_ ->
			{error, 400}
	end;
patch_subscriber2(Id, Etag, "application/json-patch+json", ReqBody,
		CurrPassword, CurrAttr, Bal, Enabled, Multi) ->
	try
		{array, OpList} = mochijson:decode(ReqBody),
		CurrentValues = [{"password", CurrPassword}, {"balance", Bal},
				{"attributes", CurrAttr}, {"enabled", Enabled}, {"multisession", Multi}],
		ValidOpList = validated_operations(OpList),
		case execute_json_patch_operations(ValidOpList, Id, CurrentValues) of
			{NPwd, NBal, NAttr, NEnabled, NMulti} ->
				patch_subscriber3(Id, Etag, NPwd, NAttr, NBal, NEnabled, NMulti);
			{error, Status} ->
				{error, Status}
		end
	catch
		_:_ ->
			{error, 400}
	end.
%% @hidden
patch_subscriber3(Id, Etag, Password, RadAttr, Balance, Enabled, Multi) ->
	Attributes = {array, radius_to_json(RadAttr)},
	RespObj =[{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
		{password, Password}, {attributes, Attributes}, {balance, Balance},
		{enabled, Enabled}, {multisession, Multi}],
	JsonObj  = {struct, RespObj},
	RespBody = mochijson:encode(JsonObj),
	Headers = case Etag of
		undefined ->
			[];
		_ ->
			[{etag, etag(Etag)}]
	end,
	{ok, Headers, RespBody}.

-spec delete_subscriber(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /ocs/v1/subscriber/{id}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
delete_subscriber(Id) ->
	ok = ocs:delete_subscriber(Id),
	{ok, [], []}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
json_to_radius(JsonObjList) ->
	json_to_radius(JsonObjList, []).
%% @hidden
json_to_radius([{struct, [{"name", "ascendDataRate"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T,Acc);
json_to_radius([{struct, [{"name", "ascendDataRate"}, {"value", V}]} | T], Acc) ->
	Attribute = {?VendorSpecific, {?Ascend, {?AscendDataRate, V}}},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct, [{"name", "ascendXmitRate"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T,Acc);
json_to_radius([{struct, [{"name", "ascendXmitRate"}, {"value", V}]} | T], Acc) ->
	Attribute = {?VendorSpecific, {?Ascend, {?AscendXmitRate, V}}},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct,[{"name","sessionTimeout"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T, Acc);
json_to_radius([{struct,[{"name","sessionTimeout"}, {"value", V}]} | T], Acc) ->
	Attribute = {?SessionTimeout, V},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct,[{"name","acctInterimInterval"}, {"value", V}]} | T], Acc) when V == null; V == ""->
	json_to_radius(T,Acc);
json_to_radius([{struct,[{"name","acctInterimInterval"}, {"value", V}]} | T], Acc) ->
	Attribute = {?AcctInterimInterval, V},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct,[{"name","class"}, {"value", V}]} | T], Acc) when V == null; V == "" ->
	json_to_radius(T, Acc);
json_to_radius([{struct,[{"name","class"}, {"value", V}]} | T], Acc) ->
	Attribute = {?Class, V},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{struct, [{"name", "vendorSpecific"} | VendorSpecific]} | T], Acc) ->
	case vendor_specific(VendorSpecific) of
		[] ->
			json_to_radius(T, Acc);
		Attribute ->
			json_to_radius(T, [Attribute | Acc])
	end;
json_to_radius([], Acc) ->
	Acc.

%% @hidden
radius_to_json(RadiusAttributes) ->
	radius_to_json(RadiusAttributes, []).
%% @hidden
radius_to_json([{?SessionTimeout, V} | T], Acc) ->
	Attribute = {struct, [{"name", "sessionTimeout"}, {"value",  V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?AcctInterimInterval, V} | T], Acc) ->
	Attribute = {struct, [{"name", "acctInterimInterval"}, {"value", V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?Class, V} | T], Acc) ->
	Attribute = {struct, [{"name", "class"}, {"value", V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, {?Ascend, {?AscendDataRate, V}}} | T], Acc) ->
	Attribute = {struct, [{"name", "ascendDataRate"}, {"value",  V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, {?Ascend, {?AscendXmitRate, V}}} | T], Acc) ->
	Attribute = {struct, [{"name", "ascendXmitRate"}, {"value",  V}]},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, _} = H | T], Acc) ->
	Attribute = {struct, vendor_specific(H)},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([_| T], Acc) ->
	radius_to_json(T, Acc);
radius_to_json([], Acc) ->
	Acc.

%% @hidden
vendor_specific(AttrJson) when is_list(AttrJson) ->
	{_, Type} = lists:keyfind("type", 1, AttrJson),
	{_, VendorID} = lists:keyfind("vendorId", 1, AttrJson),
	{_, Key} = lists:keyfind("vendorType", 1, AttrJson),
	case lists:keyfind("value", 1, AttrJson) of
		{_, null} ->
			[];
		{_, Value} ->
			{Type, {VendorID, {Key, Value}}}
	end;
vendor_specific({?VendorSpecific, {VendorID, {VendorType, Value}}}) ->
	AttrObj = [{"name", vendorSpecific},
				{"vendorId", VendorID},
				{"vendorType", VendorType},
				{"value", Value}],
	{struct, AttrObj}.

-spec etag(V1) -> V2
	when
		V1 :: string() | {N1, N2},
		V2 :: {N1, N2} | string(),
		N1 :: integer(),
		N2 :: integer().
%% @doc Generate a tuple with 2 integers from Etag string
%% value or vice versa.
%% @hidden
etag(V) when is_list(V) ->
	[TS, N] = string:tokens(V, "-"),
	{list_to_integer(TS), list_to_integer(N)};
etag(V) when is_tuple(V) ->
	{TS, N} = V,
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

%% @hidden
-spec validated_operations(UnOrderAttributes) -> OrderedAtttibutes
	when
		UnOrderAttributes :: [{struct, [tuple()]}],
		OrderedAtttibutes :: [tuple()].
%% @doc Processes scrambled json attributes (with regard to
%% https://tools.ietf.org/html/rfc6902#section-3) and return
%% a list of key, value tuples.
validated_operations(UAttr) ->
	F = fun(F, [{struct, Op} | T],  Acc) ->
			{_, "replace"} = lists:keyfind("op", 1, Op),
			{_, P} = lists:keyfind("path", 1, Op),
			{_, V} = lists:keyfind("value", 1, Op),
			[P1] = string:tokens(P, "/"),
			F(F, T, [{P1, V} | Acc]);
		(_, [], Acc) ->
			lists:reverse(Acc)
	end,
	F(F, UAttr, []).

%% @doc Execute json-patch opearations and return resulting object's
%% attributes.
%% @hidden
execute_json_patch_operations(OpList, ID, CValues) ->
	F = fun(_, [{"password", V} | _], Acc) ->
			{password, lists:keyreplace("password", 1, Acc, {"password", V})};
		(F, [{"attributes", {array, V}} | T], Acc) ->
			NV = json_to_radius(V),
			NewAcc = lists:keyreplace("attributes", 1, Acc, {"attributes", NV}),
			F(F, T, NewAcc);
		(F, [{Path, V} | T], Acc) ->
			NewAcc = lists:keyreplace(Path, 1, Acc, {Path, V}),
			F(F, T, NewAcc);
		(_, [], Acc) ->
			{attributes, Acc}
	end,
	{Update, NValues} = F(F, OpList, CValues),
	{_, NPwd} = lists:keyfind("password", 1, NValues),
	{_, NAttr} = lists:keyfind("attributes", 1, NValues),
	{_, NBal} = lists:keyfind("balance", 1, NValues),
	{_, NEnabled} = lists:keyfind("enabled", 1, NValues),
	{_, NMulti} = lists:keyfind("multisession", 1, NValues),
	case Update of
		password ->
			case ocs:update_password(ID, NPwd) of
				ok ->
					{NPwd, NBal, NAttr, NEnabled, NMulti};
				{error, not_found} ->
					{error, 404};
				{error, _Reason} ->
					{error, 500}
			end;
		attributes ->
			case ocs:update_attributes(ID, NBal, NAttr, NEnabled, NMulti) of
				ok ->
					{NPwd, NBal, NAttr, NEnabled, NMulti};
				{error, not_found} ->
					{error, 404};
				{error, _Reason} ->
					{error, 500}
			end
	end.

