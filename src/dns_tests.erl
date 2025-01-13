-module(dns_tests).
-include("dns.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_query_test() ->
	{ok, Resp} = dns:query("ns1.default.com", {?A,{127,0,0,1}, 8053, udp}),
	Answer = lists:nth(1,Resp#dns_packet.answer),

	%TODO: Write wrapper code that doesn't make the result look like garbage/hard/ to use
	%	?assertEqual(Answer#dns_record.name, dns:string_to_dns_label("ns1.default.com")),
	?assertEqual(Answer#dns_record.data, [192,1,1,1]).
	

