-module(json).
-compile(export_all).

get(Key, List) when is_list(Key) ->
  get(list_to_binary(Key), List);
get(Key, {_, [_Head|_Tail] = List}) ->
  get(Key, List);
get(Key, List) ->
  case proplists:lookup(Key, List) of
    none ->
      none;
    {Key, Value} ->
      Value 
  end.

set(Key, Value, List) when is_list(Key) ->
  set(list_to_binary(Key), Value, List);
set(Key, Value, List) ->
  [{Key, Value}|proplists:delete(Key, List)].

encode(Object) -> 
  list_to_binary(lists:flatten(mochijson2:encode(Object))).

decode(Object) ->
  mochijson2:decode(Object).

