%%% ocs_rest_res_log.erl
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
-module(ocs_rest_res_log).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([content_types_accepted/0,
				content_types_provided/0,
				perform_get_all/0]).

-include_lib("radius/include/radius.hrl").

-spec content_types_accepted() -> ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/hal+json"].

-spec perform_get_all() -> {body, Body :: iolist()}
		| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /ocs/v1/log'
%% requests.
perform_get_all() ->
	{body, []}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

