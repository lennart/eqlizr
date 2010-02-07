-module(feeds).
-compile(export_all).

-include("records.hrl").

read_all(_S) -> 
	Feeds = blipdb:all(feeds),
  Feeds.
	%lists:map(fun(F) -> {struct, [{<<"id">>, F#feed.id}, {<<"last_update">>, F#feed.last_update}]} end, Feeds).

create(S) ->
  {struct, Feed} = S,
  Id = lists:keyfind(<<"id">>,1, Feed),
%  LastUpdate = proplists:get_value(<<"last_update">>, Feed),
{atomic, ok} = blipdb:create(#feed{id= Id,last_update= calendar:now_to_universal_time(erlang:now())}),
	Feed.

read(S) ->
  {feed, Feed} = S,
	case blipdb:read({feed, Feed#feed.id}) of
		{feed, Feed} ->
      {feed, Feed};
		_ ->
			{error, [{<<"message">>, <<"Feed not found">>}]}
	end.

update(S) ->
  {feed, Feed} = S,
  case blipdb:update(Feed#feed.id, {feed, Feed#feed.id, calendar:now_to_universal_time(erlang:now())}) of
  {feed, Feed} ->
    {feed, Feed};
  _ ->
    {error, [{<<"message">>, <<"Feed update Failed">>}]}
  end.

delete(S) ->
  {feed, Feed} = S,
	{atomic, ok} = blipdb:delete(Feed#feed.id, {feed, Feed#feed.id}),
	{struct, [{<<"message">>, ok}]}.
