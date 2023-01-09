-module(request_handler).
-include("dns.hrl").
-behavior(gen_server).

-export([init/1, handle_cast/2, terminate/2, start_link/0]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init(_) ->
	DNSPid = whereis(dns_ingest),
	gen_server:cast(DNSPid, {add_handler, self()}),
	{ok, []}.

terminate(_, _) ->
	DNSPid = whereis(dns_ingest),
	gen_server:cast(DNSPid, {remove_handler, self()}),
	ok.

handle_cast({dns_request, Address, Port, Packet}, State) ->
	{ok, ParsedPacket} = dns:parse_packet(Packet),
	io:format("Packet info: ~w~n", [ParsedPacket]),
	{noreply, State}.

handle_call(_, _, State) -> {noreply, State}.

% private API stuff


