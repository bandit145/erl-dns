-module(db).
-include("dns.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([store_record/1, get_record/1]).

store_record(Record) ->
	F = fun() ->
		mnesia:write(Record)
	end,
	mnesia:transaction(F).

get_record(Record) ->
	Q = fun() ->
		mnesia:select(dns_record, ets:fun2ms(fun(R = #dns_record{type=Type, name=Name, ttl=TTL, data=Data}) when Type =:= Record#dns_record.type andalso Name =:= Record#dns_record.name -> R end))
	end,
	mnesia:transaction(Q).