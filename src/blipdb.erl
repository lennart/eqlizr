-module(blipdb).
-export([reset/0,find/3,all/1,update/2,read/1,create/1,delete/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

reset() ->
  erlang_couchdb:delete_database(host(), database()),
  case erlang_couchdb:create_database(host(), database()) of
    ok ->
      io_lib:format("Created new database ~s~n",[database()]);
    {error, Error} ->
      Error
  end,
  create_view("Blip", [{<<"all">>, ["function(doc) { if (doc['blip_id'] && doc['title'])  emit(doc._id, doc) }"]}, {<<"by_blip_id">>, ["function(doc) { if (doc['blip_id'] && doc['title'])  emit(doc.blip_id, doc) }"]}]),
  create_view("Feed", [{<<"all">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit(doc._id, doc) }"]}, {<<"by_url">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit(doc.url, doc) }"]}]).

create_view(Design, Code) ->
  case erlang_couchdb:create_view(host(), database(), Design, <<"javascript">>, Code) of
    {json, {struct, [{<<"error">>,Error},{<<"reason">>,Reason}]}} -> 
      {error, {Error, Reason}};
    {json, Result} ->
      {json, Result}
  end.

all(Design) ->
  {Rows, _, _} = extract_rows(erlang_couchdb:invoke_view(host(), database(), record_to_design_name(Design), "all", [])),
  lists:map(fun extract_document/1, Rows).

find(Design, View, Q) ->
  Key = mochijson:encode(Q),
  {DesignName, Query} = {record_to_design_name(Design), [{"key",Key}]},
  case extract_rows(erlang_couchdb:invoke_view(host(), database(), DesignName, View, Query)) of
    {[Result], _, _} -> 
      extract_document(Result);
    {_, 0, _} -> 
      {error, {<<"message">>, "Document not found"}}
  end.

read(Oid) ->
  erlang_couchdb:retrieve_document(host(), database(), Oid).

create(Doc) ->
  erlang_couchdb:create_document(host(), database(), Doc).


update(UUID, Doc) ->
  erlang_couchdb:update_document(host(), database(), UUID, Doc).

delete(UUID) ->
  erlang_couchdb:delete_document(host(), database(), UUID).

% Internal Methods

extract_document({struct, Result}) ->
  {<<"value">>, {struct, Doc}} = lists:keyfind(<<"value">>,1,Result),
  {doc, Doc}.

extract_rows(Response) -> 
  {json, {struct, [{<<"total_rows">>, Count}, {<<"offset">>, Offset}, {<<"rows">>,Rows}]}} = Response,
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

