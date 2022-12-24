-module(dns).
-export([parse_packet/1, test/1]).

-record(dns_header, {id, qr, opcode, aa, tc, rd, ra, z, rcode, qdcount, ancount, nscount, arcount}).
-record(dns_record, {name, type, class}).


parse_packet(<<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Data/binary>>) ->
	io:format("ID: ~w QR: ~w Questions: ~w ~n", [ID, QR, QDCOUNT]),
	Records = parse_records(QDCOUNT, Data),
	io:format("Name: ~w~n", [Records]),
	{ok, #dns_header{id=ID, qr=QR, opcode=OPCODE, aa=AA, tc=TC, rd=RD, ra=RA, z=Z, rcode=RCODE, qdcount=QDCOUNT, ancount=ANCOUNT, nscount=NSCOUNT, arcount=ARCOUNT}, Records}.


parse_records(Num, Packet) ->
	parse_records(Num, Packet, []).
parse_records(1, Packet, Records) ->
	{ok, DNSRecord, NewData} = parse_record(Packet, []),
	[DNSRecord | Records];
parse_records(Num, Packet, Records) ->
	{ok, DNSRecord, NewData} = parse_record(Packet, []),
	parse_records(Num -1, NewData, [DNSRecord | Records ]).


parse_record(<<00:8,Type:16, Class:16, Data/binary>>, Accum) ->
	Name = lists:reverse(Accum),
	{ok, #dns_record{name=Name, type=Type, class=Class}, Data};
parse_record(<<Char:8, Data/binary>>, Accum) ->
	io:format("~w~n",[Accum]),
	parse_record(Data, [Char | Accum]).
