-module(blipService).
-export([fetch/1]).
-include("records.hrl").

fetch(Url) ->
  case feeds:read({feed, {<<"id">>,Url}}) of
    {struct, Feed} -> 
%      case Feed.last_update of
%         
%      end,
      {struct, feed:update(Feed)};
    {error, Reason} ->
      {struct, feed:create({struct, {<<"id">>,Url}})},
      {ok, {_ | Body}} = http_fetch(Url),
      parse(Body)
  end.

http_fetch(Url) ->
  case http:request(get, {Url, [{"User-Agent", "eqlizr"}]}, [], []) of
    {ok, {_, Headers, Body}} ->
      parse(Body);
    {error, Reason} ->
      {error, Reason}
  end.

parse(Content) ->
  {struct, Json} = mochijson2:decode(Content),
  {struct, Result} = proplists:get_value(<<"result">>, Json),
  {struct, Collection} = proplists:get_value(<<"collection">>, Result),
  Blips = proplists:get_value(<<"Blip">>, Collection),
  store(Blips).

store(Blips) ->
  lists:map(fun (Blip) -> find_or_create(Blip) end, Blips).

find_or_create(Blip) ->
  case blips:read(Blip) of
    {struct, NewBlip} ->
      {struct, NewBlip};
    {error, Reason} ->
      {struct, blips:create(Blip)};
    _ ->
      io:write("Failed\n")
  end.

