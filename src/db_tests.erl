-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("dns.hrl").

store_and_get_test_() ->
	{setup, fun start/0, fun stop/1, fun store_record/1}.


start() ->
	mnesia:start(),
	{_, ok} = mnesia:create_table(dns_record, [{type, bag},{attributes, record_info(fields, dns_record)}]),
	ok.

stop(_) ->
	mnesia:delete_table(dns_record),
	mnesia:stop().	


store_record(_) ->
	Rec = [#dns_record{name=dns:string_to_dns_label("google.com"), type=1, ttl=5, data={1,1,1,1}}, #dns_record{name=dns:string_to_dns_label("google.com"), type=1, ttl=5, data={1,1,1,2}}],
	{_, ok} = db:store_record(lists:nth(1,Rec)),
	{_, ok} = db:store_record(lists:nth(2,Rec)),
	{_, FoundRecs} = db:get_record(lists:nth(1,Rec)),
	[?_assertEqual(Rec, FoundRecs)].