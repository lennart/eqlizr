-module(blipService).
-compile(export_all).
%-export([fetch_station/1,fetch_radar/1,update_all_feeds/1]).
-include("records.hrl").

fetch_station(User) ->
  fetch(station_url([{"username",User}]), api).
fetch_radar(User) ->
  fetch(radar_url(User), web).

full_update() ->
  feed_synchronizer ! update.

update_all_feeds(Interval) when is_integer(Interval) ->
  case feeds:read_all() of
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason]);
    Feeds ->
      lists:foreach(fun({feed,F}) -> feeds:fetch_blips({feed,F},true) end, Feeds)
  end,
  receive
    update ->
      update_all_feeds(Interval);
    finished ->
      io:format("Ending Work")
  after Interval*1000 ->
      update_all_feeds(Interval)
  end.

% Internal Methods

fetch(Url, Source) ->
  Feed = case feeds:read(Url) of
    {feed, _Feed} -> 
      io:format("Reading Feed: ~p~n", [_Feed]),
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

api_root_url(Path, Query) ->
  build_url({"http","api.blip.fm","/blip"}, Path, Query).

build_url({Scheme, Host, Rootpath}, Path, Query) ->
  mochiweb_util:urlunsplit({Scheme, Host, mochiweb_util:join(lists:append([Rootpath],Path),"/"),encode_query(Query),[]}).

web_root_url(Path, Query) ->
  build_url({"http","blip.fm",""}, Path, Query).

station_url(Query) ->
  api_root_url(["getUserProfile.json"], Query). 

radar_url(User) ->
  web_root_url(["feed",User],[]).

replies_url(User) ->
  web_root_url(["feed",User,"replies"],[]).

encode_query(Query) ->
  mochiweb_util:urlencode(Query).
