%%% ocs_wm_res_subscriber.erl
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
%%% @doc This {@link //webmachine. webmachine} callback module
%%% 	implements resource handlers for the `/subscriber' URI within the
%%% 	{@link //oss_gw. oss_gw} application's REST API.
%%% @reference WebMachine Wiki
%%% 	<a href="https://github.com/webmachine/webmachine/wiki/Resource-Functions">
%%% 	Resource Functions</a>.
%%%
-module(ocs_wm_res_subscriber).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

% export the webmachine callbacks
-export([init/1,
		allowed_methods/2,
		content_types_accepted/2,
		content_types_provided/2,
		post_is_create/2,
		create_path/2,
		delete_resource/2,
		find_subscriber/2,
		find_subscribers/2,
		add_subscriber/2,
		update_subscriber/2,
		options/2]).

-include("ocs_wm.hrl").
-include("ocs.hrl").

-record(state,
		{subscriber :: string(),
		current_password :: string(),
		new_password :: string(),
		attributes = [] :: radius_attributes:attributes(),
		balance :: integer(),
		frange :: integer(),
		lrange :: integer(),
		partial_content = false :: boolean()}).

%%----------------------------------------------------------------------
%%  webmachine callbacks
%%----------------------------------------------------------------------

-spec init(Config :: proplists:proplist()) ->
	{ok | {trace, Dir :: file:filename()}, Context :: state()}.
%% @doc The dispatcher calls this function for every request to the resource.
init(Config) ->
	Result = case proplists:lookup(trace, Config) of
		{trace, false} ->
			ok;
		{trace, File} when is_list(File) ->
			{trace, File};
		none ->
			ok
	end,
	{Result, #state{}}.

-spec allowed_methods(rd(), state()) -> {[Method], rd(), state()}
		when Method :: 'GET' | 'HEAD' | 'PUT' | 'POST' | 'DELETE' | 'OPTIONS'.
%% @doc If a Method not in this list is requested, then a
%% 	`405 Method Not Allowed' will be sent.
allowed_methods(ReqData, Context) ->
	{['POST', 'GET', 'HEAD', 'DELETE', 'PUT', 'OPTIONS'], ReqData, Context}.

-spec content_types_accepted(ReqData :: rd(), Context :: state()) ->
	{[{MediaType :: string(), Handler :: atom()}],
	ReqData :: rd(), Context :: state()}.
%% @doc Content negotiation for request body.
content_types_accepted(ReqData, Context) ->
	case wrq:method(ReqData) of
		'POST' ->
			{[{"application/json", add_subscriber}], ReqData, Context};
		'PUT' ->
			{[{"application/json", update_subscriber}], ReqData, Context};
		'GET' ->
			{[], ReqData, Context};
		'OPTIONS' ->
			{[], ReqData, Context};
		'HEAD' ->
			{[], ReqData, Context};
		'DELETE' ->
			{[], ReqData, Context}
	end.

-spec content_types_provided(ReqData :: rd(), Context :: state()) ->
	{[{MediaType :: string(), Handler :: atom()}],
	ReqData :: rd(), Context :: state()}.
%% @doc Content negotiation for response body.
content_types_provided(ReqData, Context) ->
	NewReqData = wrq:set_resp_header("Access-Control-Allow-Origin", "*", ReqData),
	case wrq:method(NewReqData) of
		'POST' ->
			{[{"application/json", add_subscriber}], NewReqData, Context};
		'PUT' ->
			{[{"application/json", update_subscriber}], NewReqData, Context};
		Method when Method == 'GET'; Method == 'HEAD' ->
			case {wrq:path_info(identity, NewReqData), wrq:req_qs(NewReqData),
					 wrq:get_req_header("range", NewReqData)} of
				{undefined, _, undefined} ->
					{[{"application/json", find_subscribers}], NewReqData, Context};
				{undefined, _, Range} ->
					[SFR, SLR] = string:tokens(Range, "-"),
					{FR, LR} = {list_to_integer(SFR), list_to_integer(SLR)},
					NextContext = Context#state{frange = FR, lrange = LR,
							partial_content = true},
					{[{"application/json", find_subscribers}], NewReqData, NextContext};
				{Identity, _, undefined} ->
					NextContext = Context#state{subscriber = Identity},
					{[{"application/json", find_subscriber}], NewReqData, NextContext}
			end;
		'DELETE' ->
			{[{"application/json", delete_subscriber}], NewReqData, Context}
	end.

%% @doc Return HTTP headers for a OPTIONS request
options(ReqData, Context) ->
		{[{"Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, PUT, OPTIONS"},
			{"Access-Control-Allow-Origin", "*"},
			{"Access-Control-Allow-Headers", "Content-Type"}], ReqData, Context}.

-spec post_is_create(ReqData :: rd(), Context :: state()) ->
	{Result :: boolean(), ReqData :: rd(), Context :: state()}.
%% @doc If POST requests put content into a (potentially new) resource.
post_is_create(ReqData, Context) ->
	{true, ReqData, Context}.

-spec create_path(ReqData :: rd(), Context :: state()) ->
	{Path :: string(), ReqData :: rd(), Context :: state()}.
%% @doc Called on a `POST' request if {@link post_is_create/2} returns true.
create_path(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	try 
		{struct, Object} = mochijson:decode(Body),
		{_, Subscriber} = lists:keyfind("subscriber", 1, Object),
		{_, Password} = lists:keyfind("password", 1, Object),
%% @todo Ignore attributes values temporarily
%%		{_, {array, ArrayAttributes}} = lists:keyfind("attributes", 1, Object),
		{_, BalStr} = lists:keyfind("balance", 1, Object),
		{Balance , _}= string:to_integer(BalStr),
		NewContext = Context#state{subscriber = Subscriber,
			current_password = Password, balance = Balance},
		{Subscriber, ReqData, NewContext}
	catch
		_Error ->
			{{halt, 400}, ReqData, Context}
	end.

-spec delete_resource(ReqData :: rd(), Context :: state()) ->
	{Result :: boolean() | halt(), ReqData :: rd(), Context :: state()}.
%% @doc Respond to `DELETE /ocs/subscriber/{identity}' request and deletes
%% a `subscriber' resource. If the deletion is succeeded return true.
delete_resource(ReqData, Context) ->
	Name = wrq:path_info(identity, ReqData),
	ok = ocs:delete_subscriber(Name),
	{true, ReqData, Context}.

-spec add_subscriber(ReqData :: rd(), Context :: state()) ->
	{true | halt(), ReqData :: rd(), Context :: state()}.
%% @doc Respond to `POST /ocs/subscriber' and add a new `subscriber'
%% resource.
add_subscriber(ReqData, #state{subscriber = Subscriber,
		current_password = Password, attributes = ArrayAttributes,
		balance = Balance} = Context) ->
	try
	case catch ocs:add_subscriber(Subscriber, Password,
			ArrayAttributes, Balance) of
		ok ->
			{true, ReqData, Context};
		{error, _Reason} ->
			{{halt, 400}, ReqData, Context}
	end catch
		throw:_ ->
			{{halt, 400}, ReqData, Context}

	end.

-spec update_subscriber(ReqData :: rd(), Context :: state()) ->
	{true | halt(), ReqData :: rd(), Context :: state()}.
%% @doc	Respond to `PUT /ocs/subscriber/{identity}' request and
%% Updates a existing `subscriber''s password or attributes. 
update_subscriber(ReqData, Context) ->
	Identity = wrq:path_info(identity, ReqData),
	case ocs:find_subscriber(Identity) of
		{ok, Password, _, _, _} ->
			try 
				Body = wrq:req_body(ReqData),
				{struct, Object} = mochijson:decode(Body),
				{_, RecvPwd} = lists:keyfind("password", 1, Object),
				Password = list_to_binary(RecvPwd),
				{_, Type} = lists:keyfind("update", 1, Object),
				ok = case Type of
					"attributes" ->
						{_, Attributes} = lists:keyfind("attributes", 1, Object),
						ocs:update_attributes(Identity, Password, Attributes);
					"password" ->
						{_, NewPassword } = lists:keyfind("newpassword", 1, Object),
						ocs:update_password(Identity, Password, NewPassword)
				end,
				{true, ReqData, Context}
			catch
				throw : _ ->
					{{halt, 400}, ReqData, Context}
			end;
		{error, _Reason} ->
			{{halt, 400}, ReqData, Context}
	end.


-spec find_subscriber(ReqData :: rd(), Context :: state()) ->
	{Result :: iodata() | {stream, streambody()} | halt(),
	 ReqData :: rd(), Context :: state()}.
%% @doc Body producing function for `GET /ocs/subscriber/{identity}'
%% requests.
find_subscriber(ReqData, #state{subscriber = Identity} = Context) ->
	case ocs:find_subscriber(Identity) of
		{ok, SubBin, Attributes, Balance, Enabled} ->
			Subscriber = binary_to_list(SubBin),
			Obj = [{identity, Subscriber}, {attributes, Attributes}, {balance, Balance},
						 {enabled, Enabled}],
			JsonObj  = {struct, Obj},
			Body  = mochijson:encode(JsonObj),
			{Body, ReqData, Context};
		{error, _Reason} ->
			{{halt, 400}, ReqData, Context}
	end.

-spec find_subscribers(ReqData :: rd(), Context :: state()) ->
	{Result :: iodata() | {stream, streambody()} | halt(),
	ReqData :: rd(), Context :: state()}.
%% @doc Body producing function for `GET /ocs/subscriber'
%% requests.
find_subscribers(ReqData, #state{partial_content = false} = Context) ->
	case ocs:get_subscribers() of
		{error, _} ->
			{{halt, 400}, ReqData, Context};
		Subscribers ->
			ObjList = [{strcut,
			[{identity, S#subscriber.name},{password, S#subscriber.password},
				{attributes, S#subscriber.attributes}, {enabled, S#subscriber.enabled}]}
				|| S <- Subscribers],
			JsonArray = {array, ObjList},
			Body  = mochijson:encode(JsonArray),
			{Body, ReqData, Context}
	end.
%% @todo partion_content
%find_subscribers(ReqData, #state{partial_content = true,
%		frange = FR, lrange = LR, buf = []} = Context) ->
		
