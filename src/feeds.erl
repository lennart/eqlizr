-module(feeds).
-compile(export_all).
%-export([read_all/0,create/2,read/1,update/1,delete/1,fetch_blips/1,fetch_blips/2,parse_xml_feed/1]).

-include("records.hrl").
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
    {error, Reason} ->
      SourceValue = case Source of
        api ->
          <<"api">>;
        web ->
          <<"web">>
      end,
      {doc, Feed} = blipdb:create({json, [{"source",SourceValue},{"url", erlang:list_to_binary(Url)}]}),
      {feed, Feed}
  end.
%  LastUpdate = proplists:get_value(<<"last_update">>, Feed),

read(Url) ->
  case blipdb:find(feed, "by_url", [{key, Url}]) of
    {doc, Doc} ->
      {feed, Doc};
    {error, _} ->
      {error, [{<<"message">>, <<"Feed not found">>}]}
  end.


current_time_for_json() ->
  time_for_json(calendar:now_to_universal_time(erlang:now())).

time_for_json(Time) ->
  {{Year,Month,Day},{Hour,Minute,Second}} = Time,
  erlang:list_to_binary(io_lib:format("~B/~B/~B ~B:~B:~B +0000",[Year,Month,Day,Hour,Minute,Second])).


update({feed, Feed}) ->
  FreshFeed = json:set("last_update", current_time_for_json(), Feed),
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

fetch_blips({feed, Feed}, Force) when is_boolean(Force), Force == true ->
  fetch_remote_blips({feed,Feed}).

fetch_blips({feed, Feed}) ->
  io:format("Fetching Blips for ~p~n",[Feed]),
  Id = json:get("_id",Feed),
  Url = json:get("url", Feed),

  case is_outdated({feed, Feed}) of
    true ->
      fetch_remote_blips({feed, Feed});
    false ->
      io:format("Using Cached Feed~n",[]),
      case blipdb:find_all(feed, "by_blips", [{startkey, [Id]}, {endkey, [Id, {}, {}]}]) of
        none ->
          {error, [{<<"message">>, <<"Something Strange happened">>}]};
        [] -> 
          {error, [{<<"message">>, <<"No Blips Found">>}]};
        {error, _} ->
          {error, [{<<"message">>, <<"Feed not found">>}]};
        [{doc, LatestFeed}|Blips] ->
          [{feed, LatestFeed}|Blips]

      end
  end.

fetch_remote_blips({feed, Feed}) ->
  %File = filename:absname("data/lmaa-station.json"),
  %io:format("File: ~p~n", [File]),
  %  {ok, Body} = file:read_file(File),
  Url = json:get("url", Feed),
  Source = json:get("source", Feed),
  io:format("Url: ~p~n", [Url]),
  {ok, Body} = http_fetch(binary_to_list(Url)),
  Blips = case list_to_atom(binary_to_list(Source)) of
    api ->
      store(parse(Body), Feed);
    web ->
      store(parse_xml_feed(Body), Feed)
  end,
  case feeds:update({feed, Feed}) of
    {feed, _Feed} ->
      [{feed, _Feed}|Blips];
    {error, Reason} ->
      {error, Reason}
  end.

http_fetch(Url) ->
  case http:request(get, {Url, [{"User-Agent", "eqlizr"}]}, [], []) of
    {ok, {_, Headers, Body}} ->
      {ok, Body};
    {error, Reason} ->
      {error, Reason}
  end.

parse(Content) ->
  {ok, {obj, Json},_} = rfc4627:decode(Content),
  {obj, Result} = json:get("result", Json),
  {obj, Collection} = json:get("collection", Result),
  Blips = json:get("Blip", Collection),
  Blips.

parse_xml_feed(Content) ->
  {"rss", _, [{"channel",[], Entries}]} = xml:parse_string(Content),
  %  io:format("Channel: ~p~n",[Entries]),
  %  NewEntries = [{Key, Value} || {Key, Value} <- Entries, Key == "item"],
  lists:foldl(
    fun({_Key, _Attrs, _Value}, Feed) when _Key == "item" -> 
        Item =lists:foldl(
          fun({Key, Attrs, [Value]}, Result) when Key == "pubDate" ->
              case parse_atom_date(binary_to_list(Value)) of
                {ok, Date} ->
              json:set("insTime", time_for_json(Date), Result);
                {error, Reason} ->
                  throw({error, Reason})
              end;
            ({"link", Attrs, [Url]}, Result) -> 
              io:format("With a URL, ~p~n", [Url]),
              {_,_,Path,_,_} = mochiweb_util:urlsplit(binary_to_list(Url)),
              json:set("id",list_to_binary(get_blip_id(Path)), Result);
            ({"title", Attrs, [Title]}, Result) -> 
              json:set("title",Title, Result);
            ({Key, Attrs, Value}, Result) ->
              Result
%              json:set(Key, Value, Result)
          end,[],_Value),
        io:format("Item: ~p~n",[Item]),
        [Item|Feed];

      ({_Key, Attrs, _Value}, Feed) ->
        Feed
    end,[],Entries).

parse_atom_date(String) ->
  [_,Day, MonthName, Year, Time, Zone] = string:tokens(String," "),
  Month = case MonthName of
    "Jan" ->
      1;
    "Feb" ->
      2;
    "Mar" ->
      3;
    "Apr" ->
      4;
    "May" ->
      5;
    "Jun" ->
      6;
    "Jul" ->
      7;
    "Aug" ->
      8;
    "Sep" ->
      9;
    "Oct" ->
      10;
    "Nov" ->
      11;
    "Dec" ->
      12
  end,
  {Timezone,_} = string:to_integer(Zone),
  [{Hour,_}, {Minute,_}, {Second,_}] = [string:to_integer(I) || I <- string:tokens(Time,":")],
  {DayAsNumber,_} = string:to_integer(Day),
  {YearAsNumber,_} = string:to_integer(Year),
  UnshiftedDate = calendar:datetime_to_gregorian_seconds({{YearAsNumber, Month, DayAsNumber},{Hour,Minute,Second}}),
  {ok, calendar:gregorian_seconds_to_datetime(UnshiftedDate+((Timezone div 100)*3600))}.


get_blip_id(Path) ->
  {ok, Rxp} = re:compile("^\/([^\/]+\/){3}([^\/]+)\/"),
  {match, [Blip|Rest]} = re:run(Path, Rxp,[{capture,[2],list}]),
  io:format("Captures: ~p~n", [Blip]),
  Blip.

store(Blips, Feed) ->
  %  BlipIds = lists:map(fun (Blip) -> json:get("id",Blip) end, Blips),
  %  case blipdb:find_all(blip, "by_blip_id", [{keys, BlipIds}]) of
  %    [Blip] ->
  %      
  %    [] ->
  %      blipdb:create({bulk, Blips}).
  %      ecouch:doc__bulk(BlipIds 
  %    {error, Reason} ->
  %      {error, Reason}
  %  end.
  lists:map(fun (Blip) -> find_or_create(Blip, Feed) end, Blips).

find_or_create({obj, Blip}, Feed) ->
  find_or_create(Blip, Feed);

find_or_create(Blip, Feed) ->
  Id = json:get("id",Blip),
  FeedId = json:get("_id",Feed),
  case blips:read(Id) of
    {blip, NewBlip} ->
      BlipWithFeed = add_feed_to_blip(NewBlip, FeedId),
      blipdb:update({json, BlipWithFeed});
    {error, Reason} ->
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

is_outdated({feed, _Feed} = Feed) ->
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
