-module(blips).
-compile(export_all).

read_all() -> 
  blipdb:all(blip).

create(S) ->
  {struct, Blip} = S,
  BlipId = json:get("id", Blip),
  case read(BlipId) of
    {blip, ExistingBlip} ->
      {blip, ExistingBlip};
    {error, _} ->
      case blipdb:create({json, Blip}) of
        {doc, NewBlip} ->
          {blip, NewBlip};
        {error, Reason} ->
          {error, Reason}
      end
  end.

find_by_blip_id(BlipId) ->
  case blipdb:find("Blip", "by_blip_id", BlipId) of
    {doc, Result} ->
      {blip, Result};
    [] ->
      {error, [{<<"message">>, <<"blip not found">>}]}
  end.

read(Id) ->
  case blipdb:find(blip, "by_blip_id", [{key, Id}]) of
    {doc, Doc} ->
      {blip, Doc};
    {error, _} ->
      {error, [{<<"message">>, <<"Feed not found">>}]}
  end.

update({blip, Blip}) -> 
  case blipdb:update({json, Blip}) of
    {doc, Doc} ->
      {blip, Doc};
    {error, Reason} ->
      {error, Reason}
  end.

delete({blip, Blip}) ->
  case blipdb:delete({json, Blip}) of
    {ok, Result} ->
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.
