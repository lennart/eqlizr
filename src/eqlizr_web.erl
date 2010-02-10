%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for eqlizr.

-module(eqlizr_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

respond(Req, Data) ->
  Req:ok({"application/json", [{"Server","eqlizr"}], [json:encode(Data)]}).

respond_error(Req, Message) ->
  Req:ok({"application/json", [{"Server", "eqlizr"}], [json:encode({obj, [{"error", Message}]})]}).
  

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "blip/timeline" ++ Id ->
                  case blipService:fetch_radar(Id) of
                    [{feed, Feed}|Blips] ->
                      RawBlips = [{obj, X} || {_, X} <- Blips],
                      respond(Req, [{obj, Feed}|RawBlips]);
                    _ ->
                      respond_error(Req, <<"not_found">>)
                    end;
                "blip/" ++ Id ->
                  case blipService:fetch_station(Id) of
                    [{feed, Feed}|Blips] ->
                      RawBlips = [{obj, X} || {_, X} <- Blips],
                      respond(Req, [{obj, Feed}|RawBlips]);
                    _ ->
                      respond_error(Req, <<"not_found">>)
                    end;
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

feed(Response, Path, N) ->
  receive
    %{router_msg, Msg} ->
    %    Html = io_lib:format("Recvd msg #~w: ‘~s’<br/>", [N, Msg]),
    %    Response:write_chunk(Html);
  after 10000 ->
      Msg = io_lib:format("Chunk ~w for id ~s\n", [N, Path]),
      Response:write_chunk(Msg)
  end,
  feed(Response, Path, N+1).


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
