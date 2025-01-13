%-record(dns_packet, {header, question, answer, authority, additional}).
-record(dhcp6_packet,{message_type, transaction_id, options}).

-record(dhcp_lease, {duid, ip_addr}).
-record(subnet, {subnet, interface}).
-record(option, {id, key ,data}).

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
-define(OPTION_SERVERID, 2).
-define(OPTION_IA_NA, 3).
-define(OPTION_IA_TA, 4).
-define(OPTION_IAADDR, 5).
-define(OPTION_ORO, 6).
-define(OPTION_PREFERENCE, 7).
-define(OPTION_ELAPSED_TIME, 8).
-define(OPTION_RELAY_MSG, 9).
-define(OPTION_AUTH, 11).
-define(OPTION_UNICAST, 12).
-define(OPTION_STATUS_CODE, 13).
-define(OPTION_RAPID_COMMIT, 14).
-define(OPTION_USER_CLASS, 15).
-define(OPTION_VENDOR_CLASS, 16).
-define(OPTION_VENDOR_OPTS, 17).
-define(OPTION_INTERFACE_ID, 18).
-define(OPTION_RECONF_MSG, 19).
-define(OPTION_RECONF_ACCEPT, 20).
-define(OPTION_IA_PD, 25).
-define(OPTION_IAPREFIX, 26).
-define(OPTION_INFORMATION_REFRESH_TIMER, 32).
-define(OPTION_SOL_MAX_RT, 82).
-define(OPTION_INF_MAX_RT, 83).