-module(feeds).
-compile(export_all).
-export([read_all/0,create/2,read/1,update/1,delete/1,fetch/3]).

-include_lib("xmerl/include/xmerl.hrl").

read_all() -> 
  case blipdb:all(feed) of
    none ->
      {error, [{<<"message">>, <<"Something Strange happened">>}]};
    [] -> 
      {error, [{<<"message">>, <<"No Blips Found">>}]};
    {error, _} ->
      {error, [{<<"message">>, <<"Feed not found">>}]};
    [_LatestFeed|_Tail] = Feeds ->
      [{feed, F} || {doc, F} <- Feeds]
  end.

create(Url, Source) ->
  case read(Url) of
    {feed, Doc} ->
      {feed, Doc};
    {error, _} ->
      SourceValue = case Source of
        api ->
          <<"api">>;
        web ->
          <<"web">>
      end,
      {doc, Feed} = blipdb:create({json, [{<<"source">>,SourceValue},{<<"url">>, erlang:list_to_binary(Url)}]}),
      {feed, Feed}
  end.

read(Url) ->
  case blipdb:find(feed, "by_url", [{key, Url}]) of
    {doc, Doc} ->
      {feed, Doc};
    {error, _} ->
      {error, [{<<"message">>, <<"Feed not found">>}]}
  end.

update({feed, Feed}) ->
  FreshFeed = json:set(<<"last_update">>, current_time_for_json(), Feed),
  case blipdb:update({json, FreshFeed}) of
    {doc, Doc} ->
      {feed, Doc};
    {error, {<<"message">>,Error}} ->
      io:format("Failed ~s~n",[Error])
  end.

delete(S) ->
  {feed, Feed} = S,
  {atomic, ok} = blipdb:delete({json, Feed}),
  {struct, [{<<"message">>, ok}]}.

% Internal Methods (Quite a lot, so maybe move some of the to utils)

fetch_blips({feed, Feed}, Force) when is_boolean(Force), Force == true ->
  fetch_remote_blips({feed,Feed}).

fetch_blips({feed, Feed}) ->
  Id = json:get("_id",Feed),

  case is_outdated({feed, Feed}) of
    true ->
      fetch_remote_blips({feed, Feed});
    false ->
      case blipdb:find_all(feed, "by_blips", [{startkey, [Id]}, {endkey, [Id, {struct, []}, {struct, []}]}]) of
        none ->
          {error, [{<<"message">>, <<"Something Strange happened">>}]};
        [] -> 
          {error, [{<<"message">>, <<"No Blips Found">>}]};
        {error, _} ->
          {error, [{<<"message">>, <<"Feed not found">>}]};
        [{doc, LatestFeed}|Blips] ->
          [{feed, LatestFeed}|[{blip, Blip} || {doc, Blip} <- Blips]]

      end
  end.

fetch_remote_blips({feed, Feed}) ->
  Url = json:get("url", Feed),
  Source = json:get("source", Feed),
  io:format("Url: ~p~n", [Url]),
  {ok, Body} = http_fetch(binary_to_list(Url)),
  Blips = case list_to_atom(binary_to_list(Source)) of
    api ->
      store(parse(Body), Feed);
    web ->
      Ids = parse_ids_from_rss_feed(Body),
      {ok, Response} = http_fetch(url_for(api, blip, [{"id", string:join(Ids,",")}])),
      store(parse(Response), Feed)
  end,
  case feeds:update({feed, Feed}) of
    {feed, _Feed} ->
      [{feed, _Feed}|Blips];
    {error, Reason} ->
      {error, Reason}
  end.

http_fetch(Url) ->
  case http:request(get, {Url, [{"User-Agent", "eqlizr"}]}, [], []) of
    {ok, {_, _, Body}} ->
      {ok, Body};
    {error, Reason} ->
      {error, Reason}
  end.

parse(Content) ->
  Json = json:decode(Content),
  {struct, Result} = json:get("result", Json),
  {struct, Collection} = json:get("collection", Result),
  Blips = json:get("Blip", Collection),
  Blips.

parse_ids_from_rss_feed(Content) ->
  {"rss", _, [{"channel",[], Entries}]} = xml:parse_string(Content),
  lists:foldl(
    fun({_Key, _, _Value}, Feed) when _Key == "item" -> 
        [Item] = lists:foldl(
          fun({"link", _, [Url]}, Result) ->
              {_,_,Path,_,_} = mochiweb_util:urlsplit(binary_to_list(Url)),
              [get_blip_id(Path)|Result];
            (_, Result) ->
              Result
          end,[],_Value),
        [Item|Feed];

      (_, Feed) ->
        Feed
    end,[],Entries).


get_blip_id(Path) ->
  {ok, Rxp} = re:compile("^\/([^\/]+\/){3}([^\/]+)\/"),
  {match, [Blip|_]} = re:run(Path, Rxp,[{capture,[2],list}]),
  Blip.

store(Blips, Feed) ->
  lists:map(fun (Blip) -> find_or_create(Blip, Feed) end, Blips).

find_or_create({struct, Blip}, Feed) ->
  find_or_create(Blip, Feed);

find_or_create(Blip, Feed) ->
  Id = json:get("id",Blip),
  FeedId = json:get("_id",Feed),
  case blips:read(Id) of
    {blip, NewBlip} ->
      BlipWithFeed = add_feed_to_blip(NewBlip, FeedId),
      case blipdb:update({json, BlipWithFeed}) of
        {doc, UpdatedBlip} ->
          {blip, UpdatedBlip};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, _} ->
      BlipWithFeed = add_feed_to_blip(Blip, FeedId),
      blips:create({struct, BlipWithFeed});
    _ ->
      io:write("Failed\n")
  end.

add_feed_to_blip(Blip, FeedId) ->
  case json:get("feeds", Blip) of
    none ->
      Feeds = [FeedId];
    CurrentFeeds ->
      io:format("Current Feeds: ~p~nFeed ID: ~p~n",[FeedId,CurrentFeeds]),
      Feeds = sets:to_list(sets:from_list([FeedId|CurrentFeeds]))
  end,
  json:set("feeds",Feeds,Blip).

is_outdated({feed, _Feed}) ->
  case json:get("last_update",_Feed) of
    none ->
      true;
    LastUpdate ->
      {ok, Tokens, _} = erl_scan:string(binary_to_list(LastUpdate)),
      [Year, Month, Day | _] = [Value || {integer, _, Value} <- Tokens],
      {{CurrentYear, CurrentMonth, CurrentDay}, _} = calendar:now_to_universal_time(erlang:now()),
      case {CurrentYear, CurrentMonth,CurrentDay - 1} >= {Year, Month, Day} of
        true ->
          true;
        false ->
          false
      end
  end.

fetch(Source, Section, Variable) ->
  Url = url_for(Source, Section, Variable),
  Feed = case feeds:read(Url) of
    {feed, _Feed} -> 
      {feed, _Feed};
    {error, _} ->
      feeds:create(Url, Source)
  end,
  case feeds:fetch_blips(Feed) of
    [{feed, UpdatedFeed}|Blips] ->
      [{feed, UpdatedFeed}|Blips];
    Random ->
      io:format("Result: ~p~n", [Random])
  end.

url_for(Source, Section, Variable) ->
  case Source of
    api ->
      Path = case Section of
        station ->
          ["getUserProfile.json"];
        blip ->
          ["getById.json"];
        _Section ->
          [_Section]
      end,
      build_url({"http","api.blip.fm","/blip"}, Path, Variable);
    web ->
      {Path, Query} = case {Section,Variable} of
        {radar, User} ->
          {["feed", User], []};
        {replies, User} ->
          {["feed", User, "replies"], []};
        {_Section, Var} ->
          {[_Section, Var], []}
      end,
      build_url({"http","blip.fm",""}, Path, Query)
  end.

api_root_url(Path, Query) ->
  build_url({"http","api.blip.fm","/blip"}, Path, Query).

build_url({Scheme, Host, Rootpath}, Path, Query) ->
  mochiweb_util:urlunsplit({Scheme, Host, mochiweb_util:join(lists:append([Rootpath],Path),"/"),encode_query(Query),[]}).

web_root_url(Path, Query) ->
  build_url({"http","blip.fm",""}, Path, Query).

blip_url(Query) ->
  api_root_url(["getById.json"], Query).

station_url(Query) ->
  api_root_url(["getUserProfile.json"], Query). 

radar_url(User) ->
  web_root_url(["feed",User],[]).

replies_url(User) ->
  web_root_url(["feed",User,"replies"],[]).

encode_query(Query) ->
  mochiweb_util:urlencode(Query).

current_time_for_json() ->
  time_for_json(calendar:now_to_universal_time(erlang:now())).

time_for_json(Time) ->
  {{Year,Month,Day},{Hour,Minute,Second}} = Time,
  erlang:list_to_binary(io_lib:format("~B/~B/~B ~B:~B:~B +0000",[Year,Month,Day,Hour,Minute,Second])).

