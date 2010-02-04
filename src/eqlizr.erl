%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(eqlizr).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the eqlizr server.
start() ->
    eqlizr_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    ensure_started(inets),
    application:start(eqlizr).

%% @spec stop() -> ok
%% @doc Stop the eqlizr server.
stop() ->
    Res = application:stop(eqlizr),
    application:stop(crypto),
    application:stop(mnesia),
    application:stop(inets),
    Res.
