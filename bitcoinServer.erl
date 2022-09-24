-module(bitcoinServer).
-author("Tomas Delclaux Rodriguez Rey and Ariel Weitzenfeld").
-compile(export_all).
-define(TCP_PORT, 4500).
-define(NUM_THREADS_SERVERS, 2).
-define(NUM_SERVER_WORKERS, 6).
-define(GATOR, "tomas.delclauxro;").
-define(COOKIE, froggy).
-define(NUMZEROES, 5).
-define(DOS, false).

start() ->
    statistics(runtime),
    statistics(wall_clock),
    case ?DOS of
        true ->erlang:set_cookie(?MODULE,?COOKIE);
        false -> []
    end,
    case gen_tcp:listen(?TCP_PORT,[{active, once},{packet,2}]) of
        {ok, LSock} ->
            start_servers(?NUM_THREADS_SERVERS,LSock),
            start_server_workers(?NUM_SERVER_WORKERS),
            {ok, Port} = inet:port(LSock),
            Port;
        {error,Reason} ->
            io:format("error ~s", [Reason]),
            {error,Reason}
    end.

start_servers(0,_) ->    
    ok;

start_servers(Num,LS) ->
    ServerPid=spawn_link(?MODULE,server,[LS]),
    register(list_to_atom("bitcoin_server_tcp_"++integer_to_list(Num)), ServerPid),
    start_servers(Num-1,LS).

start_server_workers(0) ->    
    ok;

start_server_workers(Num) ->
    WorkerPid = spawn_link(?MODULE, super_find_hash, [?NUMZEROES]),
    register(list_to_atom("bitcoin_server_miner"++integer_to_list(Num)), WorkerPid),
    start_server_workers(Num-1).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

super_find_hash(NumZeros) ->  
    ZeroString = lists:duplicate(NumZeros, "0"),
    RString = ?GATOR ++ base64:encode_to_string(crypto:strong_rand_bytes(24)),
    Hashed = binary:decode_unsigned(crypto:hash(sha256,RString)),
    HashedString = io_lib:format("~64.16.0b", [Hashed]),
    Test = string:prefix(HashedString, ZeroString),
    case Test of
        nomatch -> super_find_hash(NumZeros);
        _ -> io:format("SERVER:" ++ pid_to_list(self())++" ~s\n", [RString ++ "\t" ++ HashedString]),
        super_find_hash(NumZeros)
    end.

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Message} ->
            % io:format("new client ~s~n", [Message]),
            case Message of
                "READY " ++ PID -> 
                    ChildPid = list_to_pid(PID),
                    gen_tcp:send(S, "GO " ++ integer_to_list(5));
                "NEW COIN " ++ HASH ->
                    io:format("WORKER:~s\n", [HASH]),
                    gen_tcp:send(S, "GO " ++ integer_to_list(5))
            end,
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

stop()->
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    CPU = Time1 * 1000,
    Elapsed = Time2 * 1000,
    io:format("Cpu time=~p microseconds~n", [CPU]),
    io:format("Elapsed time=~p microseconds~n", [Elapsed]),
    exit(self(),kill).