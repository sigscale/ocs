%%% ocs_rest_server.erl
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
-module(ocs_rest_server).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([start/1]).

-spec start(Args :: [term()]) ->
	{ok, Pid :: pid()} | ignore.
%% @doc Starts ocs_rest_server to enable REST API
start([{port, Port}] = _Args) ->
	case inets:start(httpd,[{modules, [mod_ocs_rest_get]}, {port, Port},
			{server_name,"ocs_rest_server"}, {server_root,"."},
			{document_root,"."}]) of
		{ok, Pid} ->
			{ok, Pid};
		{error, _Reason} ->
			ignore
	end.

