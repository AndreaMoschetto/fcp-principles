-module(regExprPar).
-export([manager/0,state/1,create/2,linker/5,spawner/4, debug/0]).

debug() ->
    % The automaton corresponding to the regular expression abc*+c(bb)*
    ManagerPID = create(
        [
        %   IsFinal A  B  C
            [inner, 1, x, 3],   %q0 - Init
            [inner, x, 2, x],   %q1
            [final, x, x, 2],   %q2
            [final, x, 4, x],   %q3
            [inner, x, 3, x]    %q4
        ],
        [a,b,c]
    ),

    % The word we are trying to parse.
    % ManagerPID ! {requestParsing,self(),[a,b]},     % Should be Rejected
    % ManagerPID ! {requestParsing,self(),[a,b,c]},   % Should be Accepted
    ManagerPID ! {requestParsing,self(),[a,b,c,c]},   % Should be Accepted
    % ManagerPID ! {requestParsing,self(),[c]},       % Should be Accepted
    % ManagerPID ! {requestParsing,self(),[c,b,b]},   % Should be Accepted
    % ManagerPID ! {requestParsing,self(),[c,b,b,b]}, % Should be Rejected
    receive
        {accepted,Word} -> io:fwrite("[~w]Accepted\n", [Word]);
        {rejected,Word} -> io:fwrite("[~w]Rejected\n", [Word])
    end.


create(TransitionMatrix, Alphabet) -> 
    ManagerPID = spawn(regExprPar,manager,[]),
    spawn(regExprPar,spawner,[self(), ManagerPID, TransitionMatrix, []]),
    loopcreate(TransitionMatrix, Alphabet, ManagerPID),
    ManagerPID.

loopcreate(TransitionList, Alphabet, ManagerPID) ->
    receive
        allLinked -> true;
        PIDs -> 
            spawn(regExprPar, linker, [self(), TransitionList, PIDs, Alphabet, PIDs]),
            [InitNodeID | _] = PIDs,
            ManagerPID ! InitNodeID,
            loopcreate(TransitionList, Alphabet,ManagerPID)
    end.

spawner(CreatePID, ManagerPID, [_ | Matrix], PIDs) ->
    % spawn di tutti i nodi dell'automa ciclando sulla matrice di transizione.
    PID = spawn(regExprPar, state, [ManagerPID]),
    if
        Matrix == [] -> CreatePID ! lists:append(PIDs, [PID]);
        true -> spawner(CreatePID, ManagerPID, Matrix, lists:append(PIDs, [PID]))
    end.

linker(CreatePID, [Row | Matrix], [PID | RemainingPIDs], Alphabet, PIDs) ->
    % assegno ad ogni nodo dell'automa la sua tabella di transizione.
    [IsFinal | Transitions] = Row,
    PID ! {Transitions, Alphabet, IsFinal, PIDs},
    if
        Matrix == [] -> CreatePID ! allLinked;
        true -> linker(CreatePID, Matrix, RemainingPIDs, Alphabet, PIDs)
    end.            
    

manager() -> 
    receive
        InitNodeID -> loopmanager(InitNodeID)
    end.                       
                   

loopmanager(InitNodeID) ->
    receive
        {requestParsing,ID,List} -> InitNodeID ! {List,ID,List};
        {accepted,ID,List}       -> ID ! {accepted,List};
        {rejected,ID,List}       -> ID ! {rejected,List}
    end,
    loopmanager(InitNodeID).


state(ManagerPID) -> 
    receive
        {Transitions, Alphabet, IsFinal, PIDs} -> loopstate(Transitions, Alphabet, ManagerPID, IsFinal, PIDs) 
    end. 


loopstate(Transitions, Alphabet, ManagerPID, IsFinal, PIDs) ->
    receive
        {[],CallerId,Word} -> % ho controllato tutta la stringa di input, controllo se il nodo è finale o meno.
            if 
                (IsFinal == final)  -> ManagerPID ! {accepted,CallerId,Word};
                true                -> ManagerPID ! {rejected,CallerId,Word}
            end;
        {[ Char | RemainingString ], CallerId, Word} -> % analizzo la stringa di input carattere per carattere.
            labelIterator(PIDs, Transitions, Alphabet, Char, RemainingString, CallerId, Word, ManagerPID)
    end,
    loopstate(Transitions, Alphabet, ManagerPID, IsFinal, PIDs).


labelIterator(PIDs, [Hop | Transitions], [Label | Alphabet], Char, RemainingString, CallerId, Word, ManagerPID) -> 
    if
        Label == Char -> 
            pidIterator(PIDs, Hop, RemainingString, CallerId, Word, ManagerPID); % se ci troviamo nella cella giusta della matrice di transizione, cerchiamo il pid del nodo a cui dobbiamo andare per gestire il prossimo stato
        true -> labelIterator(PIDs, Transitions, Alphabet, Char, RemainingString, CallerId, Word, ManagerPID) % altrimenti continuiamo a cercare il carattere corrente nella tabella di transizione.
    end.

pidIterator([PID | PIDs], Hop, RemainingString, CallerId, Word, ManagerPID) ->
    if
        Hop == x -> 
            ManagerPID ! {rejected,CallerId,Word}; % non posso spostarmi in nessuno stato da questo input, quindi rigetto l'input. 
        Hop == 0 -> 
            PID ! {RemainingString,CallerId,Word}; % ho trovato il pid corrispondente al prossimo stato indicato nella tabella di transizione, quindi passo a lui il controllo.
        true -> 
            pidIterator(PIDs, Hop-1, RemainingString, CallerId, Word, ManagerPID) % non ho ancora finito di ciclare sui pids quindi proseguo.
    end.