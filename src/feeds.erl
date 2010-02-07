-module(feeds).
-compile(export_all).

-include("records.hrl").

read_all() -> 
	blipdb:all(feed).

create(Url) ->
%  LastUpdate = proplists:get_value(<<"last_update">>, Feed),
{json, Feed} = blipdb:create([{<<"url">>, erlang:list_to_binary(Url)}, {<<"last_update">>, current_time_for_json()}]),
	Feed.

read(Url) ->
	case blipdb:find(feed, "by_url", Url) of
    {doc, Doc} ->
      {feed, Doc};
    {error, Reason} ->
			{error, [{<<"message">>, <<"Feed not found">>}]}
	end.

current_time_for_json() ->
 {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(erlang:now()),
 erlang:list_to_binary(io_lib:format("~B/~B/~B ~B:~B:~B +0000",[Year,Month,Day,Hour,Minute,Second])).
 


update(S) ->
  {feed, Feed} = #feed{doc=S#feed.doc},
  case blipdb:update({feed, Feed}) of
  {doc, Doc} ->
    {feed, Doc};
  _ ->
    {error, [{<<"message">>, <<"Feed update Failed">>}]}
  end.

delete(S) ->
  {feed, Feed} = S,
  {<<"_id">>, Id} = lists:keyfind(<<"_id">>,1,S),
	{atomic, ok} = blipdb:delete(Id),
	{struct, [{<<"message">>, ok}]}.
