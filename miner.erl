%% Hashing implementation to find bitcoins

-module(miner).
-author("Tomas Delclaux").
-compile(export_all).

% io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,
%  "COP5615 is a boring class"))]).

prefix([], _) -> true;
prefix([Ch | Rest1], [Ch | Rest2]) ->
        prefix(Rest1, Rest2);
prefix(_, _) -> false.


find_hash(true, [String]) -> io:format("mined string: ~s~n", [String]).
find_hash(Num) ->  
    ZeroString = lists:duplicate(Num, "0"),
    RString = "tomas.delclauxro;" ++ base64:encode(crypto:strong_rand_bytes(8)),
    Res=prefix(lists:flatten(ZeroString)    
                ,io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,RString))])),
    case Res of
        true -> io:format("FOUND: ~s~n", [io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,RString))])]),find_hash(true, [RString]);
        false -> io:format("NOT FOUND~n"),find_hash(Num)
    end.

lets_go() ->
    {ok,[Num]} = io:fread("Enter Number of Zeroes:", "~d"),
    find_hash(Num).
