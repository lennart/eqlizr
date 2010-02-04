-module(feeds).
-compile(export_all).

-include("records.hrl").

read_all(_S) -> 
	Feeds = blipdb:read_all(feeds),
	lists:map(fun(F) -> {struct, [{<<"id">>, F#feed.id}, {<<"last_update">>, F#feed.last_update}]} end, Feeds).

create(S) ->
  {struct, Feed} = S,
  Id = proplists:get_value(<<"id">>, Feed),
%  LastUpdate = proplists:get_value(<<"last_update">>, Feed),
{atomic, ok} = blipdb:write(#feed{id= Id,last_update= calendar:now_to_universal_time(erlang:now())}),
	Feed.

read(S) ->
  {struct, Feed} = S,
	Id = proplists:get_value(<<"id">>, Feed),

	case blipdb:read({feed, Id}) of
		[{feed, Id, LastUpdate}] ->
      {struct, [{<<"id">>,Id}},{<<"last_update">>, LastUpdate}]};
		[] ->
			{error, [{<<"message">>, <<"Feed not found">>}]}
	end.

update(S) ->
  {feed, Feed} = S,
  {atomic, ok} = blipdb:write({feed, Feed.id, calendar:now_to_universal_time(erlang:now())}),
	{struct, [{<<"message">>, ok}]}.

delete(S) ->
  {feed, Feed} = S,
	{atomic, ok} = blipdb:delete({feed, Feed.id}),
	{struct, [{<<"message">>, ok}]}.
