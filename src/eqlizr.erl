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
    ensure_started(inets),
    ensure_started(ecouch),
    ensure_started(erlsom),
    ensure_started(esolr),
    %esolr:start_link(),
    application:start(eqlizr),
    register(feed_synchronizer, spawn(blipService,update_all_feeds,[7200])).

%% @spec stop() -> ok
%% @doc Stop the eqlizr server.
stop() ->
    Res = application:stop(eqlizr),
    application:stop(crypto),
    application:stop(ecouch),
    feed_synchronizer ! finished,
    application:stop(erlsom),
    application:stop(inets),
    application:stop(esolr),
    Res.
