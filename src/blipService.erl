-module(blipService).
-compile(export_all).

fetch_station(User) ->
  feeds:fetch(api, station, [{"username",string:to_lower(User)}]).

fetch_full_station(User) ->
  feeds:fetch(api, full_station, [{"username",string:to_lower(User)}]).

full_update() ->
  feed_synchronizer ! update.

update_all_feeds(Interval) when is_integer(Interval) ->
  case feeds:read_all() of
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason]);
    Feeds ->
      io:format("Updating all feeds now, next time in ~p Minutes~n",[Interval/60]),
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
