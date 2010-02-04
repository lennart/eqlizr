-module(blipdb).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

reset() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),

	mnesia:create_table(blip, [{disc_copies, [node()]}, {attributes, record_info(fields, blip)}]).

find(Q) ->
	F = fun() ->
			qlc:e(Q)
	end,
	transaction(F).

transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, _Reason} ->
			[]
	end.	


read(Oid) ->
	F = fun() ->
			mnesia:read(Oid)
	end,
	transaction(F).

read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),
	find(Q). 

write(Rec) ->
	F = fun() ->
			mnesia:write(Rec)
	end,
	mnesia:transaction(F).

delete(Oid) ->
	F = fun() ->
			mnesia:delete(Oid)
	end,
	mnesia:transaction(F).
