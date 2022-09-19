%% Hashing implementation to find bitcoins

-module(miner).
-author("Tomas Delclaux").
-compile(export_all).
-define(Gator, {Tomas: "tomas.delclauxro").

% io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,
%  "COP5615 is a boring class"))]).

prefix([], _) -> true;
prefix([Ch | Rest1], [Ch | Rest2]) ->
        prefix(Rest1, Rest2);
prefix(_, _) -> false.


find_hash(true, [Num]) -> io:format("hash found = ~w~n", [Num]).
find_hash(Num) ->  
    ZeroString = lists:duplicate(Num, "0"),
        lists:flatten(ZeroString),
    RString = base64:encode(crypto:strong_rand_bytes(8)),
    Res=prefix("0",io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,RString))])),
    case Res of
        true -> io:format("FOUND~n"),find_hash(true, [Num]);
        false -> io:format("NOT FOUND"),find_hash(Num)
    end.

run() ->
    {ok,[Num]} = io:fread("Enter Number of Zeroes:", "~d"),
    find_hash(Num).
