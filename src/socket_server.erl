-module(socket_server).
-behavior(gen_server).
-include("dns.hrl").
-export([init/1, terminate/2, code_change/3, start_link/0, start_link/1, handle_cast/2, handle_call/3]).

start_link() -> gen_server:start_link(?MODULE, {8053}, []).
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init({Port}) -> 
	% Make udp socket and tcp socket, and wire them up on specified port
	{ok, UDPSocket} = gen_udp:open(Port,[binary, {active, false}]),
	io:format("I have opened sockets~n"),
	register(dns_ingest, self()),
	gen_server:cast(self(), listen),
	{ok, {UDPSocket, []}}.

terminate(_, {UDPSocket}) ->
	io:format("I am terminating: ~w~n", [self()]),
	gen_udp:close(UDPSocket),
	ok.

code_change(_, State, _) ->
	{ok, State}.

handle_cast(listen, {UDPSocket, Handlers}) ->
	case gen_udp:recv(UDPSocket, 0, 1) of
	{ok, {_, _, Packet}} ->
		Handler = lists:nth(rand:uniform(length(Handlers)), Handlers),
		gen_event:notify(Handler, {dns_request, Packet}),
		io:format("Data: ~w~n", [Packet]);
	{error, timeout} ->
		ok
	end,
	gen_server:cast(self(), listen),
	{noreply, {UDPSocket, Handlers}};
handle_cast({add_handler, Pid}, {UDPSocket, Handlers}) ->
	io:format('Added Handler: ~w~n', [Pid]),
	{noreply, {UDPSocket, Handlers ++ [Pid]}};
handle_cast({remove_handler, Pid}, {UDPSocket, Handlers}) ->
	io:format('removed Handler: ~w~n', [Pid]),
	NewHandlers = [X || X <- Handlers, X =:= Pid],
	{noreply, {UDPSocket, NewHandlers}}.


handle_call(_, _, State) ->
	{noreply, State}.