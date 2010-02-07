-module(blipService).
-export([fetch/1]).
-include("records.hrl").

fetch(Url) ->
  case feeds:read(Url) of
    {struct, Feed} -> 
      {struct, feeds:update(Feed)};
    {error, Reason} ->
      {struct, feeds:create({struct, {<<"id">>,Url}})},
      {ok, {_,_,Body}} = http_fetch(Url),
      parse(Body)
  end.

% Internal Methods

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
  Id = lists:keyfind(<<"id">>,1,Blip),
  case blips:read(Id) of
    {blip, NewBlip} ->
      {blip, NewBlip};
    {error, Reason} ->
      {blip, blips:create(Blip)};
    _ ->
      io:write("Failed\n")
  end.

