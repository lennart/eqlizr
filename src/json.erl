-module(json).
-compile(export_all).

get(Key, {_, [_Head|_Tail] = List}) ->
  get(Key, List);
get(Key, List) ->
  case proplists:lookup(Key, List) of
    none ->
      none;
    {Key, Value} ->
      Value 
  end.

set(Key, Value, List) ->
  [{Key, Value}|proplists:delete(Key, List)].

encode([_Head|_Tail] = List) ->
  rfc4627:encode(List);
encode({obj, Object}) -> 
  rfc4627:encode({obj, Object}).

