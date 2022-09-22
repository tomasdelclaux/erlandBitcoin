-module(bitcoinWorker).
-behaviour(gen_server).
-compile(export_all).

client(PortNo) ->
    {ok,Sock} = gen_tcp:connect("localhost",PortNo,[{active,false},
                                                    {packet,2}]),
    gen_tcp:send(Sock, "READY" ++ pid_to_list(self())),
    case gen_tcp:recv(Sock,0) of
        {ok, "GO"++Num} -> io:format("mining with ~s zeroes~n", [Num]);
        {ok, "CHILL"}-> io:format("chilling out~n")
    end,
    gen_tcp:close(Sock).
