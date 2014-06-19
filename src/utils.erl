-module(utils).

-export([timestamp/0, log/1, socket_to_address/1, client_to_string/1]).

-include("client.hrl").

timestamp() ->
    {_Date, {H, M, S}} = calendar:local_time(),
    io_lib:format("[~2..0w:~2..0w:~2..0w]", [H, M, S]).

log(Message) ->
    io:format("~s ~s~n", [timestamp(), Message]).

socket_to_address(Socket) ->
    case inet:peername(Socket) of
	{ok, {Address, _Port}} ->
	    inet_parse:ntoa(Address);
	_ ->
	    "Unknown"
    end.

client_to_string(Client) ->
    io_lib:format("~s [~s] (~s)", [Client#client.nick, Client#client.address,
		   Client#client.application]).

