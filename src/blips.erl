-module(blips).
-compile(export_all).

-include("records.hrl").

read_all(_S) -> 
	Notes = blipdb:read_all(blip),
	lists:map(fun(F) -> {struct, [{<<"id">>, F#blip.id}, {<<"doc">>, F#blip.doc}]} end, Notes).

create(S) ->
  {struct, Blip} = S,
  BlipId = proplists:get_value(<<"id">>, Blip),
	{atomic, ok} = blipdb:write(#blip{id = BlipId, doc = Blip}),
	Blip.

read(S) ->
  {struct, Blip} = S,
	Id = proplists:get_value(<<"id">>, Blip),

	case blipdb:read({blip, Id}) of
		[{blip, Id, Doc}] ->
      {struct, Doc};
		[] ->
			{error, [{<<"message">>, <<"blip not found">>}]}
	end.

update(S) ->
	Id = struct:get_value(<<"id">>, S),
	Doc = struct:get_value(<<"doc">>, S),
	{atomic, ok} = blipdb:write({blip, Id, Doc}),
	{struct, [{<<"message">>, ok}]}.

delete(S) ->
	Id = struct:get_value(<<"id">>, S),
	{atomic, ok} = blipdb:delete({blip, Id}),
	{struct, [{<<"message">>, ok}]}.
