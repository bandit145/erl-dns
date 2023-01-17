-module(dns_server_sup).
-include("eunit/")
-behavior(supervisor).

-export([init/1, start_link/1]).

start_link(Args) ->
	supervisor:start_link(?MODULE, Args).

init({Workers}) ->
	SupFlags = #{strategy => one_for_one},
	ChildSpec = [#{id => dns_ingest, start => {socket_server, start_link, []}, restart => permanent, type => worker}],
	{ok, {SupFlags, ChildSpec ++ create_handlers(Workers)}}.

create_handlers(N) ->
	create_handlers(N, []).
create_handlers(1, Accum) ->
	[#{id => string:concat("request_handler",1), start => {request_handler, start_link, []}, restart => permanent, type => worker} | Accum];
create_handlers(N, Accum) ->
	create_handlers(N-1, [#{id => string:concat("request_handler", N), start => {request_handler, start_link, []}, restart => permanent, type => worker} | Accum]).