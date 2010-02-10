-module(blipdb).
-export([reset/0,find/3,find_all/3,all/1,update/1,read/1,create/1,delete/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

reset() ->
  erlang_couchdb:delete_database(host(), database()),
  erlang_couchdb:create_database(host(), database()),
  create_view("Blip", [
      {<<"all">>, ["function(doc) { if (doc['id'] && doc['title'])  emit(doc._id, doc) }"]},
      {<<"by_title">>, 
["function(doc) {
  if (doc['id'] && doc['title']) {
    emit(doc.title,doc);
  }
}"]},
      {<<"by_blip_id">>, ["function(doc) { if (doc['id'] && doc['title'])  emit(doc.id, doc) }"]}]),
  create_view("Feed", [{<<"all">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit(doc._id, doc) }"]}, {<<"by_url">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit(doc.url, doc) }"]},
      {<<"by_blips">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit([doc._id,1], doc); if (doc['id'] && doc['title'] && doc['feeds']) { for(var k = 0; k < doc['feeds'].length; k++) { emit([doc.feeds[k],2,doc['insTime']],doc); } }}"]}]).

all(Design) ->
  find_all(Design, "all", []).

find(Design, View, Q) ->
  case fetch_all(Design, View, Q) of 
    [{obj, _Result} = Result] -> 
      %io:format("Single Result: ~p~n",[Result]),
      extract_document(Result);
    [Result|_] ->
      %io:format("Multiple Results: ~p~n",[Result]),
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
    {ok, {obj, Response}} ->
      case extract_rows(Response) of
        {Rows, _, _} -> 
          %io:format("Rows: ~p~n",[Rows]),
          Rows;
        none ->
          {error, {<<"message">>, "None Found"}}
      end;
    {error, Reason} ->
      {error, {<<"message">>, Reason}}
  end.

create({bulk, List}) ->
  case ecouch:doc_bulk_create(database(), List) of
    {ok, {obj, DocList}} ->
      io:format("Response for Bulkcreation ~p~n", [DocList]);
%      [{doc, update_id_and_revision(Doc)} || {obj, Doc} <- DocList];
    {error, Reason} ->
      {error, {<<"message">>, io_lib:format("Error: ~p~n",[Reason])}}
  end;

create({json, Doc}) ->
  case ecouch:doc_create(database(),{obj, Doc}) of
    %  case erlang_couchdb:create_document(host(), database(), Doc) of
    {ok, {obj, Rest}} ->
      %Rev = json:get("rev",Rest),

      UpdatedDoc = update_id_and_revision(Rest, Doc),
      {doc, UpdatedDoc};
    {error, Reason} ->
      {error, {<<"message">>, io_lib:format("Error: ~p~n",[Reason])}}
  end.



read(Oid) ->
  ecouch:doc_get(database(), Oid).

update({json, Doc}) ->
  io:format("Document before Update ~p~n", [Doc]),
  case proplists:lookup("_id",Doc) of
    none ->
      {error, {<<"message">>,"Missing Document _id"}};
    {"_id", UUID} ->
      case ecouch:doc_update(database(), UUID, {obj, Doc}) of
        %      case erlang_couchdb:update_document(host(), database(), erlang:binary_to_list(UUID), Doc) of
        {ok, {obj, Rest}} ->
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
      ecouch:doc_delete(database(), UUID)
      %      erlang_couchdb:delete_document(host(), database(), UUID)
  end.

% Internal Methods



extract_document({obj, Result}) ->
  {obj, Doc} = json:get("value",Result),
  {doc, Doc}.

extract_rows(Response) -> 
  [{"total_rows", Count}, {"offset", Offset}, {"rows",Rows}] = Response,
  {Rows, Count, Offset}.



record_to_design_name(Name) ->
  case Name of
    feed ->
      "Feed";
    blip ->
      "Blip"
  end.

host() ->
  {"localhost", 5984}.

database() ->
  "blipfm".

create_view(Design, Code) ->
  case erlang_couchdb:create_view(host(), database(), Design, <<"javascript">>, Code) of
    {json, {struct, [{<<"error">>,Error},{<<"reason">>,Reason}]}} -> 
      {error, {Error, Reason}};
    {json, Result} ->
      {json, Result}
  end.


update_id_and_revision(Response, Doc) ->
  json:set("_id",json:get("id",Response),json:set("_rev",json:get("rev",Response),Doc)).
