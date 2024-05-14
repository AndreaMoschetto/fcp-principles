-module(counter).
-export([start/0, loop_counter/2, manager/2]).

% In questo esercizio si vuole realizzare due attori, 
% un counter che incrementa un contatore, lo restituisce e se supera un dato valore muore
% un manager che incrementa il contatore e stampa il suo valore 
start() ->
    CounterPID = spawn(counter, loop_counter, [0,5]),
    spawn(counter, manager, [CounterPID, 7]). % 7 > 5 così si apprezza come la chiamata 6 e 7 siano inefficaci
    % spawn(counter, manager, [CounterPID, 10]). % volendo si possono aggiungere manager e giocarci un po'

loop_counter(Index, Max) -> 
    receive 
        increment -> 
            if 
                Index < Max -> loop_counter(Index + 1, Max);
                true -> true 
            end;

        {From, value} -> 
            From ! Index,
            loop_counter(Index, Max);

        _ -> loop_counter(Index, Max) 
    end.

manager(CounterPID, NumtoSend) ->
    CounterPID ! increment,
    io:fwrite("[MANAGER] Increment sent: "), 
    CounterPID ! {self(), value},

    receive
        Value -> 
            io:fwrite("~w\n", [Value]),
            if
                NumtoSend > 0 -> manager(CounterPID, NumtoSend -1);
                true -> true
            end
    end.
