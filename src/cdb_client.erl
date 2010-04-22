-module(cdb_client).
-author('flatter@gmail.com').

-export([client/1]).

client(Request) ->
	{ok, Socket} = gen_udp:open(0, [binary]),
	ok = gen_udp:send(Socket, "localhost", 25826, term_to_binary(Request)),
	gen_udp:close(Socket).