-module(dns).
-export([parse_packet/1, encode_packet/1, encode_question/1]).


%define DNS packet related constants
%OPCODES 3-15 are reserved for feature use
-define(QUERY, 0).
-define(IQUERY, 1).
-define(STATUS, 2).
%Response Codes
-define(NOERR, 0).
-define(FORMATERR, 1).
-define(SERVFAIL, 2).
-define(NAMEERR, 3).
-define(NOTIMPL, 4).
-define(REFUSED, 5).

-include("dns.hrl").

%returns IO list for feeding into gen_tcp/gen_udp:send
encode_packet(#dns_packet{header=Header, question=Question, answer=Answer, authority=Authority, additional=Additional}) ->
	ID = Header#dns_header.id,
	Z = Header#dns_header.z,
	RCODE = Header#dns_header.rcode,
	QDCOUNT = Header#dns_header.qdcount,
	ANCOUNT = Header#dns_header.ancount,
	NSCOUNT = Header#dns_header.nscount,
	ARCOUNT = Header#dns_header.arcount,
	%encode question
	QIOList = encode_question(Question),
	%encode answer
	AIOList = encode_answer(Answer),
	%encode Additonal data
	AddIOList = encode_additonal(Additional),
	% Build IOList for DNS packet
	IOList = [<<ID:16>>, Header#dns_header.qr, Header#dns_header.opcode, Header#dns_header.aa, Header#dns_header.tc, Header#dns_header.ra, <<Z:3>>, <<RCODE:4>>, <<QDCOUNT:16>>, <<ANCOUNT:16>>, <<NSCOUNT:16>>, <<ARCOUNT:16>>],
	{ok, IOList ++ QIOList ++ AIOList ++ AddIOList}.

encode_answer(Answer) ->
	ok.

encode_additonal(Additional) ->
	ok.

encode_question(Question) ->
	encode_question(Question, []).
encode_question([], IOList) ->
	R = lists:reverse(IOList),
		lists:flatten(R);
encode_question([Record| T], IOList) ->
	Type = Record#dns_record.type,
	Class = Record#dns_record.class,
	Packed = Record#dns_record.name ++ [<<00:8>>, <<Type:16>> , <<Class:16>>],
	encode_question(T, [Packed | IOList]).

parse_packet(<<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Data/binary>>) ->
	io:format("ID: ~w QR: ~w Questions: ~w ~n", [ID, QR, QDCOUNT]),
	Records = parse_records(QDCOUNT, Data),
	{ok, #dns_packet{header=#dns_header{id=ID, qr=QR, opcode=OPCODE, aa=AA, tc=TC, rd=RD,ra=RA, z=Z, rcode=RCODE, qdcount=QDCOUNT,ancount=ANCOUNT, nscount=NSCOUNT, arcount=ARCOUNT}, question=Records, authority={}, additional={}}}.


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
