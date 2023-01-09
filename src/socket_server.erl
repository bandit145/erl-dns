-module(socket_server).
-behavior(gen_server).
-include("dns.hrl").
-export([init/1, terminate/2, code_change/3, start_link/0, start_link/1, handle_cast/2, handle_call/3, handle_info/2]).

start_link() -> gen_server:start_link(?MODULE, {8053}, []).
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init({Port}) -> 
	% Make udp socket and tcp socket, and wire them up on specified port
	{ok, UDPSocket} = gen_udp:open(Port,[binary, {active, true}]),
	io:format("I have opened sockets~n"),
	register(dns_ingest, self()),
	{ok, {UDPSocket,[]}}.

terminate(_, {UDPSocket}) ->
	io:format("I am terminating: ~w~n", [self()]),
	gen_udp:close(UDPSocket),
	ok.

code_change(_, State, _) ->
	{ok, State}.

handle_info({udp, Socket, Address, Port, Packet}, {UDPSocket, Handlers}) ->
	Handler = lists:nth(rand:uniform(length(Handlers)), Handlers),
	gen_server:cast(Handler, {dns_request, Address, Port, Packet}),
	io:format("Data: ~w~n", [Packet]),
	{noreply, {UDPSocket, Handlers}}.

handle_cast({add_handler, Pid}, {UDPSocket, Handlers}) ->
	io:format('Added Handler: ~w~n', [Pid]),
	{noreply, {UDPSocket, Handlers ++ [Pid]}};
handle_cast({remove_handler, Pid}, {UDPSocket, Handlers}) ->
	io:format('removed Handler: ~w~n', [Pid]),
	NewHandlers = [X || X <- Handlers, X =:= Pid],
	{noreply, {UDPSocket, NewHandlers}}.


handle_call(_, _, State) ->
	{noreply, State}.