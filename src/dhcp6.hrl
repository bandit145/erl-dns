%-record(dns_packet, {header, question, answer, authority, additional}).
-record(dhcp6_packet,{message_type, transaction_id}).

% DHCP message types
%  https://www.rfc-editor.org/rfc/rfc8415

-define(SOLICIT, 1).
-define(ADVERTISE, 2).
-define(REQUEST, 3).
-define(CONFIRM, 4).
-define(RENEW, 5).
-define(REBIND, 6).
-define(REPLY, 7).
-define(RELEASE, 8).
-define(DECLINE, 9).
-define(RECONFIGURE, 10).
-define(INFORMATION_REQUEST, 11).
-define(RELAY_FORW, 12).
-define(RELAY_REPL, 13).

% Option codes
-define(OPTION_CLIENTID, 1).