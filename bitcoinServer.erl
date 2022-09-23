-module(bitcoinServer).
-author("Tomas Delclaux Rodriguez Rey and Ariel Weitzenfeld").
% -behaviour(supervisor).
% -export([start_link/0, start_socket/0]).
% -export([init/1]).
-compile(export_all).
-define(TCP_PORT, 4500).
-define(NUM_THREADS_SERVERS, 2).
-define(GATOR, "tomas.delclauxro;").
-define(COOKIE, froggy).
-define(NUMZEROES, 5).

% start_link() ->
%   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    % SupFlags = {one_for_one, 10, 3600},
    % ChildSpec = {bitcoinWorker, {bitcoinWorker, start_link, [mine]}, temporary, 1000, worker, [bitcoinWorker]},
    case gen_tcp:listen(?TCP_PORT,[{active, once},{packet,2}]) of
        {ok, LSock} ->
            start_servers(?NUM_THREADS_SERVERS,LSock),
            {ok, Port} = inet:port(LSock),
            Port;
        {error,Reason} ->
            io:format("error ~s", [Reason]),
            {error,Reason}
    end.
    % {ok, {SupFlags, [ChildSpec]}}.

start_servers(0,_) ->
    Pid = spawn(?MODULE, super_find_hash, [?NUMZEROES]),
    register(list_to_atom("bitcoin_server_miner"), Pid),    
    ok;

start_servers(Num,LS) ->
    Pid=spawn(?MODULE,server,[LS]),
    register(list_to_atom("bitcoin_server_tcp_"++integer_to_list(Num)), Pid),
    start_servers(Num-1,LS).

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
        _ -> io:format("SERVER: ~s\n", [RString ++ "\t" ++ HashedString]),
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
                    io:format("WORKER: ~s\n", [HASH]),
                    gen_tcp:send(S, "GO " ++ integer_to_list(5))
            end,
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.