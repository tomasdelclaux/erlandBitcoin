-module(bitcoinWorker).
-behaviour(gen_server).
-compile(export_all).
-define(GATOR, "tomas.delclauxro;").

find_hash(Num) ->  
    ZeroString = lists:duplicate(Num, "0"),
    RString = ?GATOR ++ base64:encode_to_string(crypto:strong_rand_bytes(24)),
    Hashed = binary:decode_unsigned(crypto:hash(sha256,RString)),
    HashedString = io_lib:format("~64.16.0b", [Hashed]),
    Test = string:prefix(HashedString, ZeroString),
    case Test of
        nomatch -> find_hash(Num);
        _ -> [RString ++ "\t" ++ HashedString]
    end.

main() ->
    {ok,[Num]} = io:fread("", "~d"),
    Res = find_hash(Num),
    io:format("~s\n", Res).


client(PortNo) ->
    {ok,Sock} = gen_tcp:connect("localhost",PortNo,[{active,false},
                                                    {packet,2}]),
    gen_tcp:send(Sock, "READY " ++ pid_to_list(self())),
    loop(Sock),
    gen_tcp:close(Sock).

loop(S) ->
    case gen_tcp:recv(S,0) of
        {ok, "GO "++Num} -> Res = find_hash(list_to_integer(Num)), gen_tcp:send(S, "NEW COIN " ++ Res), loop(S);
        %{ok, "GO "++Num} -> io:format("mining with ~s zeroes~n", [Num]);
        {ok, "CHILL"} -> []
    end.