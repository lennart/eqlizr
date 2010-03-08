-module(blipService).
-compile(export_all).

fetch_station(User) ->
  feeds:fetch(api, station, [{"username",string:to_lower(User)}]).

fetch_full_station(User) ->
  feeds:fetch(api, full_station, [{"username",string:to_lower(User)}]).

full_update() ->
  feed_synchronizer ! update.

update_all_feeds(Interval) when is_integer(Interval) ->
  process_flag(trap_exit, true),
  spawn_link(?MODULE,update_all_feeds,[self(),Interval]),
  receive
    {'EXIT',_From,Reason} ->
      io:format("Feed Synchronizer Crashed ~p, restarting it~n", [Reason]),
      update_all_feeds(Interval);
    {error, Reason} ->
      io:format("Error: ~p~nRestarting Feed Synchronizer~n", [Reason]),
      update_all_feeds(Interval);
    done ->
      io:format("Ending Work of the Feed Synchronizer")
  end.

update_all_feeds(Pid, Interval) ->
  case feeds:read_all() of
    {error, Reason} ->
      Pid ! {error, Reason};
    Feeds ->
      io:format("Updating all feeds now, next time in ~p Minutes~n",[Interval/60]),
      lists:foreach(fun({feed,F}) -> feeds:fetch_blips({feed,F},true) end, Feeds)
  end,
  receive
    update ->
      update_all_feeds(Interval);
    finished ->
      Pid ! done
  after Interval*1000 ->
      update_all_feeds(Interval)
  end.
