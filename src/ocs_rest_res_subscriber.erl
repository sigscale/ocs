%%% ocs_rest_res_subscriber.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_accepted/0,
				content_types_provided/0,
				perform_get/1,
				perform_get_all/0,
				perform_post/1,
				perform_patch/2,
				perform_delete/1]).

-include_lib("radius/include/radius.hrl").
-include("ocs.hrl").

-define(VendorID, 529).
-define(AscendDataRate, 197).
-define(AscendXmitRate, 255).

-spec content_types_accepted() -> ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/hal+json"].

-spec perform_get(Id :: string()) ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber/{id}'
%% requests.
perform_get(Id) ->
	case ocs:find_subscriber(Id) of
		{ok, PWBin, Attributes, Balance, Enabled} ->
			Password = binary_to_list(PWBin),
			JSAttributes = radius_to_json(Attributes),
			AttrObj = {struct, JSAttributes}, 
			RespObj = [{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
				{password, Password}, {attributes, AttrObj}, {balance, Balance},
				{enabled, Enabled}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			{body, Body};
		{error, _Reason} ->
			{error, 404}
	end.

-spec perform_get_all() ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/subscriber'
%% requests.
perform_get_all() ->
	case ocs:get_subscribers() of
		{error, _} ->
			{error, 404};
		Subscribers ->
			Response = perform_get_all1(Subscribers),
			Body  = mochijson:encode(Response),
			{body, Body}
	end.
%% @hidden
perform_get_all1(Subscribers) ->
			F = fun(#subscriber{name = Id, password = Password,
					attributes = Attributes, balance = Balance, enabled = Enabled}, Acc) ->
				JSAttributes = radius_to_json(Attributes),
				AttrObj = {struct, JSAttributes}, 
				RespObj = [{struct, [{id, Id}, {href, "/ocs/v1/subscriber/" ++ binary_to_list(Id)},
					{password, Password}, {attributes, AttrObj}, {balance, Balance},
					{enabled, Enabled}]}],
				RespObj ++ Acc
			end,
			JsonObj = lists:foldl(F, [], Subscribers),
			{array, JsonObj}.

-spec perform_post(RequestBody :: list()) ->
	{Location :: string(), Body :: iolist()}
	| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/v1/subscriber' and add a new `subscriber'
%% resource.
perform_post(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		{_, Id} = lists:keyfind("id", 1, Object),
		{_, Password} = lists:keyfind("password", 1, Object),
		{_, {struct, AttrJs}} = lists:keyfind("attributes", 1, Object),
		RadAttributes = json_to_radius(AttrJs),
		{_, BalStr} = lists:keyfind("balance", 1, Object),
		{Balance , _}= string:to_integer(BalStr),
		perform_post1(Id, Password, RadAttributes, Balance)
	catch
		_Error ->
			{error, 400}
	end.
%% @hidden
perform_post1(Id, Password, RadAttributes, Balance) ->
	try
	case catch ocs:add_subscriber(Id, Password, RadAttributes, Balance) of
		ok ->
			Attributes = {struct, radius_to_json(RadAttributes)},
			RespObj = [{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
				{password, Password}, {attributes, Attributes}, {balance, Balance}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			Location = "/ocs/v1/subscriber" ++ Id,
			{Location, Body};
		{error, _Reason} ->
			{error, 400}
	end catch
		throw:_ ->
			{error, 400}
	end.

-spec perform_patch(Id :: list(), ReqBody :: list()) ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/v1/subscriber/{id}' request and
%% Updates a existing `subscriber''s password or attributes. 
perform_patch(Id, ReqBody) ->
	case ocs:find_subscriber(Id) of
		{ok, CurrentPwd, CurrentAttr, Bal, Enabled} ->
			try 
				{struct, Object} = mochijson:decode(ReqBody),
				{_, Type} = lists:keyfind("update", 1, Object),
				{Password, RadAttr} = case Type of
					"attributes" ->
						{_, {struct, AttrJs}} = lists:keyfind("attributes", 1, Object),
						NewAttributes = json_to_radius(AttrJs),
						ocs:update_attributes(Id, NewAttributes),
						{CurrentPwd, NewAttributes};
					"password" ->
						{_, NewPassword } = lists:keyfind("newpassword", 1, Object),
						ocs:update_password(Id, NewPassword),
						{NewPassword, CurrentAttr}
				end,
				Attributes = {struct, radius_to_json(RadAttr)},
				RespObj =[{id, Id}, {href, "/ocs/v1/subscriber/" ++ Id},
					{password, Password}, {attributes, Attributes}, {balance, Bal},
					{enabled, Enabled}],
				JsonObj  = {struct, RespObj},
				RespBody = mochijson:encode(JsonObj),
				{body, RespBody}
			catch
				throw : _ ->
					{error, 400}
			end;
		{error, _Reason} ->
			{error, 404}
	end.

-spec perform_delete(Id :: list()) ->
	ok .
%% @doc Respond to `DELETE /ocs/v1/subscriber/{id}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
perform_delete(Id) ->
	ok = ocs:delete_subscriber(Id),
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
json_to_radius(JsonAttributes) ->
	json_to_radius(JsonAttributes, []).
%% @hidden
json_to_radius([{"ascendDataRate", {struct, VendorSpecific}} | T], Acc) ->
	Attribute = vendor_specific(VendorSpecific),
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{"ascendXmitRate", {struct, VendorSpecific}} | T], Acc) ->
	Attribute = vendor_specific(VendorSpecific),
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{"sessionTimeout", Value} | T], Acc) ->
	Attribute = {?SessionTimeout, Value},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{"acctInterimInterval", Value} | T], Acc) ->
	Attribute = {?AcctInterimInterval, Value},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([{"class", Value} | T], Acc) ->
	Attribute = {?Class, Value},
	json_to_radius(T, [Attribute | Acc]);
json_to_radius([], Acc) ->
	Acc.

%% @hidden
radius_to_json(RadiusAttributes) ->
	radius_to_json(RadiusAttributes, []).
%% @hidden
radius_to_json([{?VendorSpecific, {?VendorID, {?AscendDataRate, _}}}
		= H | T], Acc) ->
	Attribute = {"ascendDataRate", vendor_specific(H)},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?VendorSpecific, {?VendorID, {?AscendXmitRate, _}}} 
		= H | T], Acc) ->
	Attribute = {"ascendXmitRate", vendor_specific(H)},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?SessionTimeout, V} | T], Acc) ->
	Attribute = {"sessionTimeout", V},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?AcctInterimInterval, V} | T], Acc) ->
	Attribute = {"acctInterimInterval", V},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([{?Class, V} | T], Acc) ->
	Attribute = {"class", V},
	radius_to_json(T, [Attribute | Acc]);
radius_to_json([], Acc) ->
	Acc.

%% @hidden
vendor_specific(AttrJson) when is_list(AttrJson) ->
	{_, Type} = lists:keyfind("type", 1, AttrJson),
	{_, VendorID} = lists:keyfind("vendorId", 1, AttrJson),
	{_, Key} = lists:keyfind("vendorType", 1, AttrJson),
	{_, Value} = lists:keyfind("value", 1, AttrJson),
	{Type, {VendorID, {Key, Value}}};
vendor_specific({Type, {VendorID, {VendorType, Value}}}) ->
	AttrObj = [{"type", Type},
				{"vendorId", VendorID},
				{"vendorType", VendorType},
				{"value", Value}],
	{struct, AttrObj}.

