-module(blipdb).
-export([reset/0,find/3,find_all/3,all/1,update/1,read/1,create/1,delete/1,clear_index/0]).

-include_lib("stdlib/include/qlc.hrl").

-define(DATABASE, "blipfm").

reset() ->
  esolr:delete({q, "*:*"}),
  esolr:optimize(),
  ecouch:db_delete(?DATABASE),
  ecouch:db_create(?DATABASE),

  create_view("Blip", [
      {"all",
        "function(doc) { if (doc['id'] && doc['title'])  emit(doc._id, doc) }"},
      {"by_title", "function(doc) {if (doc['id'] && doc['title']) { emit(doc.title,doc); } }"},
      {"by_blip_id", "function(doc) { if (doc['id'] && doc['title'])  emit(doc.id, doc) }"}
    ]),
  create_view("Feed", [
      {"all",
        "function(doc) { if (doc['url'] && doc['last_update'])  emit(doc._id, doc) }"},
      {"by_url", 
        "function(doc) { if (doc['url'] && doc['last_update'])  emit(doc.url, doc) }"},
      {"by_blips",
        "function(doc) { if (doc['url'] && doc['last_update'])  emit([doc._id,1], doc); if (doc['id'] && doc['title'] && doc['feeds']) { for(var k = 0; k < doc['feeds'].length; k++) { emit([doc.feeds[k],2,doc['insTime']],doc); } }}"}
    ]).

all(Design) ->
  find_all(Design, "all", []).

find(Design, View, Q) ->
  case fetch_all(Design, View, Q) of 
    [{struct, _Result} = Result] -> 
      extract_document(Result);
    [Result|_] ->
      extract_document(Result);
    [] ->
      {error, {<<"message">>, "Document not found"}}
  end.

find_all(Design, View, Options) ->
  case fetch_all(Design, View, Options) of
    false ->
      {error, {<<"message">>, "Document not found"}};
    Results ->
      lists:map(fun (Doc) -> extract_document(Doc) end,Results)
  end.

fetch_all(Design, View, Options) ->
  NewOptions = lists:foldl(
    fun({Key, Value}, Result) when Key == key -> 
        case is_binary(Value) of
          true ->
            lists:append([{Key,Value}], Result); 
          false ->
            lists:append([{Key,list_to_binary(Value)}], Result)
        end;
      (KV, Result) -> 
        lists:append([KV], Result)  
    end,
    [], Options),
  DesignName  = record_to_design_name(Design),
  case ecouch:view_access(database(), DesignName, View, NewOptions) of 
    {ok, {struct, Response}} ->
      case extract_rows(Response) of
        {Rows, _, _} -> 
          Rows;
        none ->
          {error, {<<"message">>, "None Found"}}
      end;
    {error, Reason} ->
      {error, {<<"message">>, Reason}}
  end.

% CRUD - Methods

create({bulk, List}) ->
  case ecouch:doc_bulk_create(database(), List) of
    {ok, {struct, DocList}} ->
      io:format("Response for Bulkcreation ~p~n", [DocList]);
    {error, Reason} ->
      {error, {<<"message">>, io_lib:format("Error: ~p~n",[Reason])}}
  end;

create({json, Doc}) ->
  case ecouch:doc_create(database(),{struct, Doc}) of
    {ok, {struct, Rest}} ->
      UpdatedDoc = update_id_and_revision(Rest, Doc),
      {doc, UpdatedDoc};
    {error, Reason} ->
      {error, {<<"message">>, io_lib:format("Error: ~p~n",[Reason])}}
  end.

read(Oid) ->
  ecouch:doc_get(database(), Oid).

update({json, Doc}) ->
  case json:get("_id",Doc) of
    none ->
      {error, {<<"message">>,"Missing Document _id"}};
    UUID ->
      case ecouch:doc_update(database(), UUID, {struct, Doc}) of
        {ok, {struct, Rest}} ->
          Rev = json:get("rev",{struct, Rest}),
          UpdatedDoc = json:set("_rev",Rev,Doc),
          {doc, UpdatedDoc};
        {error, Reason} ->
          {error, {<<"message">>, io_lib:format("Error: ~p~n",[Reason])}}
      end
  end.

delete({json, Doc}) ->
  case json:get("_id",Doc) of
    false ->
      {error, {<<"message">>,"Missing Document _id"}};
    UUID ->
      case ecouch:doc_delete(database(), UUID) of
        {ok, _} ->
          {ok, true};
        {error, Reason} ->
          {error, Reason}
      end
  end.

% Internal Methods

extract_document({struct, Result}) ->
  {struct, Doc} = json:get("value",Result),
  {doc, Doc}.

extract_rows(Response) -> 
  [{<<"total_rows">>, Count}, {<<"offset">>, Offset}, {<<"rows">>,Rows}] = Response,
  {Rows, Count, Offset}.

record_to_design_name(Name) ->
  case Name of
    feed ->
      "Feed";
    blip ->
      "Blip"
  end.

database() ->
  "blipfm".

create_view(Design, Maps) ->
  PreparedMaps = [{list_to_binary(Key), 
      {struct, 
        [
          {<<"map">>, 
            list_to_binary(Code)}
        ]
      }
    } || {Key, Code} <- Maps],
  ecouch:view_create(?DATABASE,Design, {struct, PreparedMaps}).

update_id_and_revision(Response, Doc) ->
  json:set("_id",json:get("id",Response),json:set("_rev",json:get("rev",Response),Doc)).

clear_index() ->
  esolr:delete({q, "*:*"}).
