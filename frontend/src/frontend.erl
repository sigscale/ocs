%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc frontend.

-module(frontend).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the frontend server.
start() ->
    frontend_deps:ensure(),
    ensure_started(crypto),
    application:start(frontend).


%% @spec stop() -> ok
%% @doc Stop the frontend server.
stop() ->
    application:stop(frontend).
