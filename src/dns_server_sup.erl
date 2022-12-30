-module(dns_server_sup).
-behavior(supervisor).

-export([init/1, start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init(_) ->
	SupFlags = #{strategy => one_for_one},
	ChildSpec = [#{id => dns_ingest, start => {socket_server, start_link, []}, restart => permanent, type => worker}, #{id => request_handler, start => {request_handler, start_link, []}, restart => permanent, type => worker}],
	{ok, {SupFlags, ChildSpec}}.
