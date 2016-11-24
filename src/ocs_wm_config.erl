%%% ocs_wm_config.erl
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
%%% @doc This library module implements the configuration of dispatching
%%% 	in the {@link //webmachine. webmachine} application for URIs within
%%%   the {@link //oss_gw. oss_gw} application's REST API.
%%%
-module(ocs_wm_config).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([dispatch/0]).

-spec dispatch() -> [tuple()].
%% @doc Returns the dispatch map tuples for {@link //webmachine. webmachine}.
dispatch() ->
	{TraceResource, Args} = case application:get_env(rest_trace) of
		{ok, Dir}  ->
			Res = [{["wmtrace", '*'], wmtrace_resource, [{trace_dir, Dir}]}],
			case file:make_dir(Dir) of
				ok ->
					{Res, [{trace, Dir}]};
				{error, eexist} ->
					{Res, [{trace, Dir}]};
				{error, Reason} ->
					error_logger:warning_report(["REST trace",
							{rest_trace, Dir}, {error, Reason}]),
					{[], []}
			end;
		undefined ->
			{[], []}
	end,
	[{["ocs", "subscriber"], ocs_wm_res_subscriber, Args},
	 {["ocs", "subscriber", identity], ocs_wm_res_subscriber, Args}] ++ TraceResource.

