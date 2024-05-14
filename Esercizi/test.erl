-module(test).
-export([start/0, split/0]).

start() ->
    PID = spawn(test, split, []),
    PID ! "CIAO".

split() ->
    receive 
        [Char | _] -> io:fwrite("~w is head\n", [Char])
    end.