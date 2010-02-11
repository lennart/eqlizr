-module(blipService).
-compile(export_all).
%-export([fetch_station/1,fetch_radar/1,update_all_feeds/1]).
-include("records.hrl").

fetch_station(User) ->
  feeds:fetch(api, station, [{"username",User}]).
fetch_radar(User) ->
  feeds:fetch(web, radar, User).

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

