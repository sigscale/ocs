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
%%%
-module(ocs_rest_res_subscriber).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_provided/0,
				find_subscriber/1,
				find_subscribers/0,
				add_subscriber/1,
				update_subscriber/2,
				delete_subscriber/1]).

%% @headerfile "include/radius.hrl"
-include_lib("radius/include/radius.hrl").
-include("ocs_wm.hrl").
-include("ocs.hrl").

-define(VendorID, 529).
-define(AscendDataRate, 197).
-define(AscendXmitRate, 255).

-spec content_types_provided() -> ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/hal+json"].

-spec find_subscriber(Identity :: string()) ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/subscriber/{identity}'
%% requests.
find_subscriber(Identity) ->
	case ocs:find_subscriber(Identity) of
		{ok, PWBin, Attributes, Balance, Enabled} ->
			Password = binary_to_list(PWBin),
			JSAttributes = radius_to_json(Attributes),
			AttrObj = {struct, JSAttributes}, 
			RespObj = [{identity, Identity}, {password, Password}, {attributes, AttrObj},
					 {balance, Balance}, {enabled, Enabled}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			{body, Body};
		{error, _Reason} ->
			{error, 404}
	end.

-spec find_subscribers() ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/subscriber'
%% requests.
find_subscribers() ->
	case ocs:get_subscribers() of
		{error, _} ->
			{error, 404};
		Subscribers ->
			Response = find_subscribers1(Subscribers),
			Body  = mochijson:encode(Response),
			{body, Body}
	end.
find_subscribers1(Subscribers) ->
			F = fun(#subscriber{name = Identity, password = Password,
					attributes = Attributes, balance = Balance, enabled = Enabled}, Acc) ->
				JSAttributes = radius_to_json(Attributes),
				AttrObj = {struct, JSAttributes}, 
				RespObj = [{struct, [{identity, Identity}, {password, Password},
					{attributes, AttrObj}, {balance, Balance},
					{enabled, Enabled}]}],
				RespObj ++ Acc
			end,
			JsonObj = lists:foldl(F, [], Subscribers),
			{array, JsonObj}.

-spec add_subscriber(RequestBody :: list()) ->
	{Location :: string(), Body :: iolist()}
	| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /ocs/subscriber' and add a new `subscriber'
%% resource.
add_subscriber(RequestBody) ->
	try 
		{struct, Object} = mochijson:decode(RequestBody),
		{_, Identity} = lists:keyfind("subscriber", 1, Object),
		{_, Password} = lists:keyfind("password", 1, Object),
		{_, {struct, AttrJs}} = lists:keyfind("attributes", 1, Object),
		RadAttributes = json_to_radius(AttrJs),
		{_, BalStr} = lists:keyfind("balance", 1, Object),
		{Balance , _}= string:to_integer(BalStr),
		add_subscriber1(Identity, Password, RadAttributes, Balance)
	catch
		_Error ->
			{error, 400}
	end.
add_subscriber1(Identity, Password, RadAttributes, Balance) ->
	try
	case catch ocs:add_subscriber(Identity, Password, RadAttributes, Balance) of
		ok ->
			Attributes = {struct, radius_to_json(RadAttributes)},
			RespObj = [{identity, Identity}, {password, Password}, {attributes, Attributes},
						{balance, Balance}],
			JsonObj  = {struct, RespObj},
			Body = mochijson:encode(JsonObj),
			Location = "/ocs/v1/subscriber" ++ Identity,
			{Location, Body};
		{error, _Reason} ->
			{error, 400}
	end catch
		throw:_ ->
			{error, 400}
	end.

-spec update_subscriber(Identity :: list(), ReqBody :: list()) ->
	{body, Body :: iolist()} | {error, ErrorCode :: integer()} .
%% @doc	Respond to `PATCH /ocs/subscriber/{identity}' request and
%% Updates a existing `subscriber''s password or attributes. 
update_subscriber(Identity, ReqBody) ->
	case ocs:find_subscriber(Identity) of
		{ok, CurrentPwd, CurrentAttr, Bal, Enabled} ->
			try 
				{struct, Object} = mochijson:decode(ReqBody),
				{_, Type} = lists:keyfind("update", 1, Object),
				{Password, RadAttr} = case Type of
					"attributes" ->
						{_, {struct, AttrJs}} = lists:keyfind("attributes", 1, Object),
						NewAttributes = json_to_radius(AttrJs),
						ocs:update_attributes(Identity, NewAttributes),
						{CurrentPwd, NewAttributes};
					"password" ->
						{_, NewPassword } = lists:keyfind("newpassword", 1, Object),
						ocs:update_password(Identity, NewPassword),
						{NewPassword, CurrentAttr}
				end,
				Attributes = {struct, radius_to_json(RadAttr)},
				RespObj =[{identity, Identity}, {password, Password},
					{attributes, Attributes}, {balance, Bal}, {enabled, Enabled}],
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

-spec delete_subscriber(Identity :: list()) ->
	ok .
%% @doc Respond to `DELETE /ocs/subscriber/{identity}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
delete_subscriber(Identity) ->
	ok = ocs:delete_subscriber(Identity),
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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

