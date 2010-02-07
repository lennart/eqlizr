-module(blips).
-compile(export_all).

-include("records.hrl").

read_all() -> 
	blipdb:all(blip).

create(S) ->
  {struct, Blip} = S,
	{json, BlipResponse} = blipdb:create(Blip),
	BlipResponse.

find_by_blip_id(BlipId) ->
	case blipdb:find("Blip", "by_blip_id", BlipId) of
    {struct, Result} ->
      {blip, Result};
		[] ->
			{error, [{<<"message">>, <<"blip not found">>}]}
	end.


read(Id) ->
	case blipdb:find(blip, "by_blip_id", Id) of
    {doc, Doc} ->
      {feed, Doc};
    {error, Reason} ->
			{error, [{<<"message">>, <<"Feed not found">>}]}
	end.

update(S) ->
  %{blip, 
	Id = struct:get_value(<<"id">>, S),
	Doc = struct:get_value(<<"doc">>, S),
	{atomic, ok} = blipdb:update({blip, Id, Doc}),
	{struct, [{<<"message">>, ok}]}.

delete(S) ->
	Id = struct:get_value(<<"id">>, S),
	{atomic, ok} = blipdb:delete({blip, Id}),
	{struct, [{<<"message">>, ok}]}.
