-module(dhcp_db).
-include("dhcp6.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([configure/0, get_lease/1, get_options/1, store_option/1]).

configure() ->
	{atomic, ok} =  mnesia:create_table(dhcp_lease, [{attributes, record_info(fields, dhcp_lease)}]),
	{atomic, ok} = mnesia:create_table(subnet, [{attributes, record_info(fields, subnet)}]),
	{atomic, ok} = mnesia:create_table(option, [{attributes, record_info(fields, option)}]).

get_lease(Lease) ->
	ok.

get_options(OptKey) ->
	Q = fun() ->
		mnesia:select(option, ets:fun2ms(fun(Opt = #option{id=Id, key=Key, data=Data }) when Key =:= OptKey -> Opt end))
		end,
	mnesia:transaction(Q).

store_option(Option) ->
	Q = fun() ->
		mnesia:write(Option)
		end,
	mnesia:transaction(Q).
