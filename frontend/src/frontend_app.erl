%% @author Mochi Media <dev@mochimedia.com>
%% @copyright frontend Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the frontend application.

-module(frontend_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for frontend.
start(_Type, _StartArgs) ->
    frontend_deps:ensure(),
    frontend_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for frontend.
stop(_State) ->
    ok.
