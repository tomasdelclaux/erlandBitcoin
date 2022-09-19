%% Hashing implementation to find bitcoins

-module(miner).
-author("Tomas Delclaux").
-export([main/0]).
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
