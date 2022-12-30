-module(request_handler).
-include("dns.hrl").
-behavior(gen_event).

-export([init/1, handle_event/2, terminate/2, start_link/0]).

start_link() ->
	{ok, Pid} = gen_event:start_link(),
	ok = gen_event:add_handler(Pid, ?MODULE, []),
	{ok, Pid}.

init(_) ->
	DNSPid = whereis(dns_ingest),
	gen_server:cast(DNSPid, {add_handler, self()}),
	{ok, []}.

terminate(_, _) ->
	DNSPid = whereis(dns_ingest),
	gen_server:cast(DNSPid, {remove_handler, self()}),
	ok.

handle_event({dns_request, Packet}, State) ->
	{ok, ParsedPacket} = dns:parse_packet(Packet),
	io:format("Packet info ~w~n", [ParsedPacket]),
	{ok, State}.

% private API stuff
