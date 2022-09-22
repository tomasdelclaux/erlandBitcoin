-module(bitcoinServer).
-author("Tomas Delclaux Rodriguez Rey and Ariel Weitzenfeld").
% -behaviour(supervisor).
% -export([start_link/0, start_socket/0]).
% -export([init/1]).
-compile(export_all).
-define(TCP_PORT, 4500).
-define(NUM_THREADS_SERVERS, 2).

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
    ok;
start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
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

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Message} ->
            io:format("new client ~s~n", [Message]),
            case Message of
                "READY" ++ PID -> 
                    ChildPid = list_to_pid(PID),
                    gen_tcp:send(S, "GO 2");
                "NEW COIN" ++ HASH ->
                    io:format(HASH),
                    gen_tcp:send(S, "CHILL")
            end,
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.
