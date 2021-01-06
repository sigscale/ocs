%%% user_default.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2021 SigScale Global Inc.
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

-module(user_default).
-copyright('Copyright (c) 2016 - 2021 SigScale Global Inc.').

%% export the user_default public API
-export([get_diamater_status/0]).

%%----------------------------------------------------------------------
%%  The user_default public API
%%----------------------------------------------------------------------

-spec get_diamater_status() -> Result
	when
		Result :: [] | [tuple()].
%% @doc Get the status of running diameter services
get_diamater_status() ->
	case diameter:services() of
		Services when length(Services) > 0 ->
			get_diamater_status(Services, []);
		[] ->
			[]
	end.
%% @hidden
get_diamater_status([H | T], Acc) ->
	Info = [peer, applications, capabilities,
			transport, connections, statistics],
	get_diamater_status(T, [diameter:service_info(H, Info) | Acc]);
get_diamater_status([], Acc) ->
	lists:reverse(Acc).

%%----------------------------------------------------------------------
%%  The user_default private API
%%----------------------------------------------------------------------
