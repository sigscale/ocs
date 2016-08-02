%%% ocs.erl
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
%%% @doc This library module implements the public API for the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export the ocs public API
-export([open/0, close/1]).

%%----------------------------------------------------------------------
%%  The ocs API
%%----------------------------------------------------------------------

-spec open() ->
	{ok, SAP :: pid()} | {error, Reason :: term()}.
%% @doc Open a new service access point (SAP).
%%
open() ->
	case supervisor:start_child(ocs_fsm_sup, [[], []]) of
		{ok, undefined} ->
			{error, undefined};
		{ok, Child} ->
			{ok, Child};
		{ok, Child, _Info} ->
			{ok, Child};
		{error, Reason} ->
			{error, Reason}
	end.

-spec close(SAP :: pid()) -> ok.
%% @doc Close an existing service access point (SAP).
%% 
close(SAP) when is_pid(SAP) ->
	case supervisor:terminate_child(ocs_fsm_sup, SAP) of
		ok ->
			ok;
		{error, not_found} ->
			exit(badarg);
		{error, Reason} ->
			exit(Reason)
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

