-module(socket_server).
-behavior(gen_statem).
-export([init/1, terminate/3, code_change/4, start_link/0, callback_mode/0]).
-export([listen/3]).

start_link() -> gen_statem:start_link(?MODULE, [], []).

init([]) -> 
	% Make udp socket and tcp socket, and wire them up on specified port
	{ok, UDPSocket} = gen_udp:open(8053,[binary, {active, false}]),
	io:format("I have opened sockets~n"),
	{ok, listen, {UDPSocket}}.

terminate(_, _, {UDPSocket}) ->
	io:format("I am terminating: ~w~n", [self()]),
	gen_udp:close(UDPSocket),
	ok.

callback_mode() -> state_functions.

code_change(_, _, _, _) ->
	{error, not_implemented}.

listen(_, _, {UDPSocket}) ->
	io:format("I am trying to listen!~n"),
	{ok, {_, _, Packet}} = gen_udp:recv(UDPSocket, 0),
	io:format("Data: ~w~n", [Packet]),
	io:format("wat~n"),
	{next_state, listen, {UDPSocket}}.