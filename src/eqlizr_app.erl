%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the eqlizr application.

-module(eqlizr_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for eqlizr.
start(_Type, _StartArgs) ->
    eqlizr_deps:ensure(),
    eqlizr_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for eqlizr.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
