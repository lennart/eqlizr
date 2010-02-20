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
          case catch doc_for_index({blip, NewBlip}) of
            {'EXIT', Reason} ->
              io:format("Indexing of Blip failed: ~p~n", [Reason]);
            {error, Reason} ->
              io:format("Indexing of Blip not supported: ~p~n", [Reason]);
            IndexDoc -> 
              case esolr:add([IndexDoc]) of
                ok ->
                  io:format("Adding Blip to Index: ~p~n",[IndexDoc]);
                Reason ->
                  io:format("Couldn't store Feed to Index~n~p~n", [Reason])
              end
          end,
          {blip, NewBlip};
        {error, Reason} ->
          {error, Reason}
      end
  end.

reindex() ->
  esolr:delete("*:*"),
  lists:foreach(fun(Blip) -> 
        case catch doc_for_index({blip, Blip}) of
          {'EXIT', Reason} ->
            io:format("Indexing of Blip not supported: ~p~n", [Reason]),
            {blip, Blip};
          IndexDoc -> 
            case esolr:add([IndexDoc]) of
              ok ->
                io:format("Adding Blip to Index: ~p~n",[IndexDoc]),
                {blip, Blip};
              Reason ->
                io:format("Couldn't store Feed to Index~n~p~n", [Reason])
            end
        end
    end, read_all()).

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

    % Internal methods

    doc_for_index({blip, Blip}) ->
      Type = json:get("type",Blip),
      Url = case Type of
        <<"youtubeVideo">> ->
          io_lib:format("youtube://~s",[json:get("url",Blip)]);
        <<"songUrl">> ->
          case mochiweb_util:urlsplit(binary_to_list(json:get("url",Blip))) of
            {Proto, Host, Path, Query, Fragment} ->
              list_to_binary(mochiweb_util:urlunsplit({Proto, Host, escape_path(Path), Query, Fragment}));
            {error, Reason} ->
              throw({error, Reason})
          end;
        <<"fuzzSong">> ->
          throw({error, not_supported});
        <<"imeemSong">> ->
          throw({error, not_supported});
        _ ->
          throw({error, not_supported})
          %io_lib:format("s3://~s", [json:get("url",Blip)])


      end,
      {doc,[
          {id,json:get("id",Blip)},
          {track,json:get("title",Blip)},
          {artist,json:get("artist",Blip)},
          {url, Url}
        ]}.

    escape_path(Input) when is_binary(Input) ->
      list_to_binary(escape_path(binary_to_list(Input)));

    escape_path(Input) ->
      lists:flatten(escape_path(Input, [])).

    escape_path(Input, Acc) ->
      Index = string:rstr(Input, "/"),
      Rest = string:sub_string(Input, 1, Index-1),
      ToEncode = string:sub_string(Input, Index+1),
      Result = ["/",edoc_lib:escape_uri(ToEncode)|Acc],
      case Index of
        1 ->
          Result;
        _Index ->
          escape_path(Rest, Result)
      end.

