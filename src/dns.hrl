-record(dns_packet, {header, question, answer, authority, additional}).
-record(dns_header, {id, qr, opcode, aa, tc, rd, ra, z, rcode, qdcount, ancount, nscount, arcount}).
-record(dns_record, {name, type, class, data, ttl}).

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


% Record types
-define(A, 1).
-define(NS, 2).
-define(CNAME, 5).
-define(SOA, 6).
-define(PTR, 12).
-define(HINFO, 13).
-define(MX, 15).
-define(TXT, 16).
-define(RP, 17).
-define(AFSDB, 18).
-define(SIG, 24).
-define(KEY, 25).
-define(AAAA, 28).
-define(LOC, 29).
-define(SRV, 33).
-define(NAPTR, 35).
-define(KX, 36).
-define(CERT, 37).
-define(DNAME, 39).
-define(APL, 42).
-define(DS, 43).
-define(SSHFP, 44).
-define(IPSECKEY, 45).
-define(RRSIG, 46).
-define(NSEC, 47).
-define(DNSKEY, 48).
-define(DHCID, 49).
-define(NSEC3, 50).
-define(NSEC3PARAM, 51).
-define(TLSA, 52).
-define(SMIMEA, 53).
-define(HIP, 55).
-define(CDS, 59).
-define(CDNSKEY, 60).
-define(OPENPGPKEY, 61).
-define(CSYNC, 62).
-define(ZONEMD, 63).
-define(SVCB, 64).
-define(HTTPS, 65).
-define(EUI48, 108).
-define(EUI64, 109).
-define(TKEY, 249).
-define(TSIG, 250).
-define(URI, 256).
-define(CAA, 257).
-define(TA, 32768).
-define(DLV, 32769).
-define(ANY, 255).
-define(AXFR, 252).
-define(IXFR, 251).
-define(OPT, 41).