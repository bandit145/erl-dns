-module(dhcp_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("dhcp6.hrl").


store_and_get_test_() ->
	{setup, fun start/0, fun stop/1, fun option_storage/1}.

start() ->
	mnesia:start(),
	_ = mnesia:delete_table(option),
	_ = mnesia:delete_table(dhcp_lease),
	_ = mnesia:delete_table(subnet),
	dhcp_db:configure().

stop(_) ->
	mnesia:delete_table(option),
	mnesia:delete_table(dhcp_lease),
	mnesia:delete_table(subnet),
	mnesia:stop().

option_storage(_) ->
	Option  = #option{id=?OPTION_ORO, key=2, data=3},
	dhcp_db:store_option(Option),
	{_, Data} = dhcp_db:get_options(2),
	io:format("~w",[Data]),
	[?_assertEqual(Option, lists:nth(1, Data))].
