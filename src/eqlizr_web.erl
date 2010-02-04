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

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "blip/" ++ Id ->
                  Url = io_lib:format("http://api.blip.fm/blip/getUserProfile.json?username=~s",[Id]),
                  Raw = blipService:fetch(Url),
                  NewRaw = mochijson2:encode(Raw),
                  Req:ok({"application/json", [{"Server","Mochiweb-Blipper"}], [NewRaw]});
%                  Response:write_chunk(NewRaw);
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
