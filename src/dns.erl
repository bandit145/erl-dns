-module(dns).
-export([parse_packet/1, encode_packet/1, encode_question/1, encode_string/1, query/2, get_port/0, query_test/0, test_meme/0]).


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

query_test() -> 
	EName = encode_string("google.com"),
	DNSPacket = #dns_packet{header=#dns_header{id=rand:bytes(2), qr=0, opcode=?QUERY, aa=0, tc=0, rd=1, ra=0, z=0, rcode=?NOERR, qdcount=1, ancount=0, nscount=0, arcount=0}, question=#dns_record{name=EName, type=1, class=1}},
	{ok, Pack} = encode_packet(DNSPacket),
	io:format("Packet info: ~w~n", [iolist_to_binary(Pack)]),
	parse_packet(iolist_to_binary(Pack)).

test_meme() ->
	Q = #dns_record{name=dns:encode_string("google.com"), type=1, class=1},
	encode_question([Q]).

%high level functons
query(Name, {Type, Server, Port, udp}) ->
	EName = encode_string(Name),
	DNSPacket = #dns_packet{header=#dns_header{id=rand:bytes(2), qr=0, opcode=?QUERY, aa=0, tc=0, rd=1, ra=0, z=0, rcode=?NOERR, qdcount=1, ancount=0, nscount=0, arcount=0}, question=#dns_record{name=EName, type=Type, class=1}},
	{ok, Packet} = encode_packet(DNSPacket),
	io:format("Packet info ~w~n", [Packet]),
	{ok, Sock} = gen_udp:open(get_port(), [binary, {active, false}]),
	ok = gen_udp:connect(Sock, Server, Port),
	ok = gen_udp:send(Sock, Packet),
	{ok, {_, _, RPacket}} = gen_udp:recv(Sock, 0, 200),
	ok = gen_udp:close(Sock),
	parse_packet(RPacket).

get_port() ->
	case rand:uniform(65535) of 
		Num when Num < 1024 -> Num + 1024;
		Num when Num >= 1024 -> Num
	end.


encode_string(Str) ->
	F = fun(S, Accum) ->
			Len = length(S),
			LData = [Len] ++ [X || X <- S],
			[LData | Accum]
		end,
	QList = string:split(Str, "."),
	lists:foldr(F, [], QList).

%returns IO list for feeding into gen_tcp/gen_udp:send
encode_packet(#dns_packet{header=Header, question=Question, answer=Answer, authority=Authority, additional=Additional}) ->
	ID = Header#dns_header.id,
	Z = Header#dns_header.z,
	RCODE = Header#dns_header.rcode,
	QDCOUNT = Header#dns_header.qdcount,
	ANCOUNT = Header#dns_header.ancount,
	NSCOUNT = Header#dns_header.nscount,
	ARCOUNT = Header#dns_header.arcount,
	QR = Header#dns_header.qr,
	OPCODE = Header#dns_header.opcode,
	AA = Header#dns_header.aa,
	TC = Header#dns_header.tc,
	RD = Header#dns_header.rd,
	RA = Header#dns_header.ra,
	%encode question
	% we are at 96 bits here (for label pointer)
	QBinary = encode_question([Question]),
	%encode answer
	AnswerBin = if 
		Answer =/= undefined -> encode_answer(Answer);
		true -> <<0>>
		end,
	%encode Additonal data
	AdditionalBin = if
		Additional =/= undefined -> encode_additonal(Additional);
		true -> <<0>>
		end,
	% io:format("byte size ~w~n", [binary:referenced_byte_size(<<Z:3>>)]),
	% Build IOList for DNS packet
	BinHeader = <<ID/binary, QR:1, OPCODE:4/big, AA:1, TC:1, RD:1, RA:1, Z:3/big, RCODE:4/big, QDCOUNT:16/big, ANCOUNT:16/big, NSCOUNT:16/big, ARCOUNT:16/big>>,
	io:format("Binary header ~w~n",[BinHeader]),
	io:format("Binary question ~w~n",[QBinary]),
	{ok, <<BinHeader/binary, QBinary/binary, AnswerBin/binary, AdditionalBin/binary>>}.

encode_answer(Answer) ->
	ok.

encode_additonal(Additional) ->
	ok.

encode_question(Question) ->
	encode_question(Question, []).
encode_question([], IOList) ->
	R = lists:reverse(IOList),
		list_to_binary(lists:flatten(R));
encode_question([Record| T], IOList) ->
	Type = Record#dns_record.type,
	Class = Record#dns_record.class,
	Packed = Record#dns_record.name ++ [<<00:8>>, <<Type:16>> , <<Class:16>>],
	encode_question(T, [Packed | IOList]).

%TODO: parse full packet
parse_packet(<<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Data/binary>>) ->
	io:format("ID: ~w QR: ~w Questions: ~w ~n", [ID, QR, QDCOUNT]),
	io:format("Data: ~w~n", [Data]),
	%96 bits
	Records = parse_records(QDCOUNT, Data),
	case QR of
		0 ->
			Additional = undefined,
			Answer = undefined,
			Authority = undefined;
		1 ->
			%need to extract data from from the packet after the question data
			Answer = parse_records(Data)
	end,
	{ok, #dns_packet{header=#dns_header{id=ID, qr=QR, opcode=OPCODE, aa=AA, tc=TC, rd=RD,ra=RA, z=Z, rcode=RCODE, qdcount=QDCOUNT,ancount=ANCOUNT, nscount=NSCOUNT, arcount=ARCOUNT}, question=Records, authority=Authority, additional=Additional, answer=Answer}}.


parse_records(Num, Packet) ->
	Records = parse_records(Num, Packet, []),
	QRecord = lists:nth(1),
	F = fun(Record, Accum) ->
			[Pointer, _] = Record#dns_record.name,
			NewRec = if
				Record#dns_record.name =:= <<11:8>> -> Record#dns_record{name=QRecord#dns_record.name};
				true -> Record
				end
			NewRec;
		end,
	lists:foldl(F, Accum).

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
