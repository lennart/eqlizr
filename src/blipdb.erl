-module(blipdb).
-export([reset/0,find/3,all/1,update/2,read/1,create/1,delete/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

host() ->
  {"localhost", 5984}.

database() ->
  "blipfm".

reset() ->
  erlang_couchdb:create_database(host(), database()),
  erlang_couchdb:create_view(host(), database(), "Blip", <<"javascript">>,[{<<"all">>, ["function(doc) { if (doc['blip_id'] && doc['title'])  emit(doc._id, doc) }"]}]),
  erlang_couchdb:create_view(host(), database(), "Blip", <<"javascript">>,[{<<"by_blip_id">>, ["function(doc) { if (doc['blip_id'] && doc['title'])  emit(doc.blip_id, doc) }"]}]),
  erlang_couchdb:create_view(host(), database(), "Feed", <<"javascript">>,[{<<"all">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit(doc._id, doc) }"]}]),
  erlang_couchdb:create_view(host(), database(), "Feed", <<"javascript">>,[{<<"by_url">>, ["function(doc) { if (doc['url'] && doc['last_update'])  emit(doc.url, doc) }"]}]).

all(Design) ->
  erlang_couchdb:invoke_view(host(), database(), Design, "all").

find(Design, View, Q) ->
  DesignName = case Design of
    feeds ->
      "Feed";
    blips ->
      "Blip"
    end,
  erlang_couchdb:invoke_view(host(), database(),DesignName, View, [{"key", mochijson2:encode(Q)}]).

read(Oid) ->
  erlang_couchdb:retrieve_document(host(), database(), Oid).

create(Rec) ->
  erlang_couchdb:create_document(host(), database(), Rec).

update(Oid, Rec) ->
  erlang_couchdb:update_document(host(), database(), Oid, Rec).

delete(Oid) ->
  erlang_couchdb:delete_document(host(), database(), Oid).
