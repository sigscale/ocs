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
		get_subscriber/0, get_subscriber/1, post_subscriber/1,
		patch_subscriber/2, delete_subscriber/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_subscriber(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber/{id}'
%% requests.
get_subscriber(Id) ->
	case ocs:find_subscriber(Id) of
		{ok, PWBin, Attributes, Balance, Enabled} ->
			Password = binary_to_list(PWBin),
			JSAttributes = radius_to_json(Attributes),
			AttrObj = {array, JSAttributes},
			RespObj = [{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
				{password, Password}, {attributes, AttrObj}, {balance, Balance},
				{enabled, Enabled}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			Headers = [{content_type, "application/json"}],
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end.

-spec get_subscriber() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber'
%% requests.
get_subscriber() ->
	case ocs:get_subscribers() of
		{error, _} ->
			{error, 404};
		Subscribers ->
			get_subscriber1(Subscribers)
	end.
%% @hidden
get_subscriber1(Subscribers) ->
	F = fun(#subscriber{name = Id, password = Password,
			attributes = Attributes, balance = Balance, enabled = Enabled}, Acc) ->
		JSAttributes = radius_to_json(Attributes),
		AttrObj = {array, JSAttributes},
		RespObj = [{struct, [{id, Id}, {href, "/ocs/v1/subscriber/" ++ binary_to_list(Id)},
			{password, Password}, {attributes, AttrObj}, {balance, Balance},
			{enabled, Enabled}]}],
		[RespObj | Acc]
	end,
	JsonObj = lists:flatten(lists:foldl(F, [], Subscribers)),
	Body  = mochijson:encode({array, lists:reverse(JsonObj)}),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body}.

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
		case ocs:add_subscriber(IdIn, PasswordIn, Attributes, Balance, Enabled) of
			{ok, #subscriber{name = IdOut} = S} ->
				Id = binary_to_list(IdOut),
				Location = "/ocs/v1/subscriber/" ++ Id,
				JAttributes = {array, radius_to_json(S#subscriber.attributes)},
				RespObj = [{id, Id}, {href, Location},
						{password, binary_to_list(S#subscriber.password)},
						{attributes, JAttributes}, {balance, S#subscriber.balance},
						{enabled, S#subscriber.enabled}],
				JsonObj  = {struct, RespObj},
				Body = mochijson:encode(JsonObj),
				Headers = [{location, Location}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec patch_subscriber(Id, ReqBody) -> Result
	when
		Id :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/subscriber/{id}' request and
%% Updates a existing `subscriber''s password or attributes. 
patch_subscriber(Id, ReqBody) ->
	case ocs:find_subscriber(Id) of
		{ok, CurrentPwd, CurrentAttr, Bal, Enabled} ->
			try 
				{struct, Object} = mochijson:decode(ReqBody),
				{_, Type} = lists:keyfind("update", 1, Object),
				{Password, RadAttr} = case Type of
					"attributes" ->
						{_, {array, AttrJs}} = lists:keyfind("attributes", 1, Object),
						NewAttributes = json_to_radius(AttrJs),
						{_, Balance} = lists:keyfind("balance", 1, Object),
						{_, EnabledStatus} = lists:keyfind("enabled", 1, Object),
						ocs:update_attributes(Id, Balance, NewAttributes, EnabledStatus),
						{CurrentPwd, NewAttributes};
					"password" ->
						{_, NewPassword } = lists:keyfind("newpassword", 1, Object),
						ocs:update_password(Id, NewPassword),
						{NewPassword, CurrentAttr}
				end,
				Attributes = {array, radius_to_json(RadAttr)},
				RespObj =[{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
					{password, Password}, {attributes, Attributes}, {balance, Bal},
					{enabled, Enabled}],
				JsonObj  = {struct, RespObj},
				RespBody = mochijson:encode(JsonObj),
				{ok, [], RespBody}
			catch
				_:_Reason ->
					{error, 400}
			end;
		{error, _Reason} ->
			{error, 404}
	end.

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

