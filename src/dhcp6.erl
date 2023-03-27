-module(dhcp6).
-include("dhcp6.hrl").

-export([parse_packet/1]).

parse_packet(<<MSGTYPE:8, TRANSACTIONID:24, Data/binary>>) ->
	io:format("MSGTYPE: ~w, TRANSID: ~w~n", [MSGTYPE, TRANSACTIONID]),
	Options = parse_options(Data),
	io:format("Options: ~w~n", [Options]),
	#dhcp6_packet{message_type=MSGTYPE, transaction_id=TRANSACTIONID}.

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
	{{1, Duid_type, UUID}, MoreData};
%unknown option
parse_option(<<Option:16, Length:16, Data/binary>>) ->
	DataLen = Length * 8,
	io:format("Uknown option: ~w~n", [Option]),
	io:format("unknown option leng in bit ~w~n",[DataLen]),
	<<OptionData:DataLen, MoreData/binary>> = Data,
	{{Option, OptionData}, MoreData}.
