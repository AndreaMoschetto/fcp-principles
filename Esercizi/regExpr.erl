-module(regExpr).
-export([manager/0,state/2,create/0]).

create () -> %%we create the manager and the (process-)states. The PID of manager is returned.

    IDmanager = spawn(regExpr,manager,[]),
    IDinit    = spawn(regExpr,state,[IDmanager,notfinal]),
    IDq1      = spawn(regExpr,state,[IDmanager,notfinal]),
    IDq2      = spawn(regExpr,state,[IDmanager,final]),
    IDq3      = spawn(regExpr,state,[IDmanager,final]),
    IDq4      = spawn(regExpr,state,[IDmanager,notfinal]),

    %%we connect the manager to qinit and the various states among themselves

    IDmanager!IDinit,
    IDinit!{IDq1,none,IDq3},
    IDq1!{none,IDq2,none},
    IDq2!{none,none,IDq2},
    IDq3!{none,IDq4,none},
    IDq4!{none,IDq3,none},
    IDmanager.
                      

manager() -> 
    receive
        IDinitial -> loopmanager(IDinitial)
    end.                       
                   

loopmanager(IDinitial) ->
             
    receive
        {requestParsing,ID,List} -> IDinitial ! {List,ID,List};
                                    
        {accepted,ID,List} -> ID ! {accepted,List};

        {rejected,ID,List} -> ID ! {rejected,List}
    end,
    loopmanager(IDinitial).




                    


state(IDmanager,Final) -> 
    receive
        {IDA,IDB,IDC} -> loop(IDA,IDB,IDC,IDmanager,Final) 
    end. 




 loop(IDA,IDB,IDC,IDmanager,Final) ->
    receive

        {[],ID,List} -> 
            if 
                (Final == final) -> IDmanager ! {accepted,ID,List};

                true -> IDmanager ! {rejected,ID,List}

            end;

        {[ Char | Cs ],ID,List}  ->  
            Next = (if (Char == a) -> IDA; (Char == b) -> IDB; (Char == c) -> IDC end),                           
            if
                (Next == none) -> IDmanager ! {rejected,ID,List};
                true -> Next ! {Cs,ID,List}
            end
    end,
             
    loop(IDA,IDB,IDC,IDmanager,Final).