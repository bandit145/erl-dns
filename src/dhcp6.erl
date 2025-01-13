-module(dhcp6).
-include("dhcp6.hrl").

-export([parse_packet/1, encode_packet/1]).

encode_packet(#dhcp6_packet{message_type=MSGTYPE, transaction_id=ID, options=Options}) ->
	ok.

parse_packet(<<MSGTYPE:8, TRANSACTIONID:24, Data/binary>>) ->
	io:format("MSGTYPE: ~w, TRANSID: ~w~n", [MSGTYPE, TRANSACTIONID]),
	Options = parse_options(Data),
	#dhcp6_packet{message_type=MSGTYPE, transaction_id=TRANSACTIONID, options=Options}.

parse_options(Data) ->
	parse_options(Data, []).
parse_options(<<>>, Accum) ->
	Accum;
parse_options(Data, Accum) ->
	{Option, MoreData} = parse_option(Data),
	parse_options(MoreData, Accum ++ [Option]).
% OPTION_CLIENTID
parse_option(<<?OPTION_CLIENTID:16, Length:16, Data/binary>>) ->
	DataLen = (Length - 2) * 8,
	io:format("DUID Data leng in bit ~w~n",[DataLen]),
	<<Duid_type:16, UUID:DataLen/bitstring, MoreData/binary>> = Data,
	io:format("DUID data type: ~w~nUUID: ~w~n", [Duid_type, UUID]),
	{{?OPTION_CLIENTID, Duid_type, UUID}, MoreData};
parse_option(<<?OPTION_SERVERID:16, Length:16, Data/binary>>) ->
	DataLen = (Length - 2) * 8,
	io:format("DUID Data leng in bit ~w~n",[DataLen]),
	<<Duid_type:16, UUID:DataLen/bitstring, MoreData/binary>> = Data,
	{{?OPTION_SERVERID, Duid_type, UUID}, MoreData};
parse_option(<<?OPTION_IA_NA:16, _:16, IAID:32, T1:32, T2:32, Data/binary>>) ->
	{IAADDR, MoreData} = parse_option(Data),
	{{?OPTION_IA_NA, IAID, T1, T2, IAADDR}, MoreData};
parse_option(<<?OPTION_ORO:16, Length:16, Data/binary>>) ->
	<<OroData:Length/binary, MoreData/binary>> = Data,
	RequestedOpts = parse_option_request([], OroData),
	{{?OPTION_ORO, RequestedOpts}, MoreData};
parse_option(<<?OPTION_ELAPSED_TIME:16, Length:16, Data/binary>>) ->
	DataLen = Length * 8,
	<<ElapsedTime:DataLen, MoreData/binary>> = Data,
	{{?OPTION_ELAPSED_TIME, ElapsedTime}, MoreData};
% IPV6 address
parse_option(<<?OPTION_IAADDR:16, _:16, IPV6Addr:128, PrefLifetime:32, ValidLifetime:32, Data/binary>>) ->
	{{?OPTION_IAADDR, IPV6Addr, PrefLifetime, ValidLifetime}, Data};
%unknown option
parse_option(<<Option:16, Length:16, Data/binary>>) ->
	DataLen = Length * 8,
	io:format("Uknown option: ~w~n", [Option]),
	io:format("unknown option leng in bit ~w~n",[DataLen]),
	<<OptionData:DataLen, MoreData/binary>> = Data,
	{{Option, OptionData}, MoreData}.

parse_option_request(Accum, <<>>) ->
	Accum;
parse_option_request(Accum, Data) ->
	<<OroData:16, MoreData/binary>> = Data,
	io:format("Option request ~w~n", [OroData]),
	parse_option_request(Accum ++ [OroData], MoreData).
