# Definizione ricorsiva sui naturali
Volendo definire funzioni sui numeri naturali, è di nostro interesse vedere come l'insieme dei numeri naturali sia definito.
## Reminder sui numeri naturali 
Intuitivamente possiamo vedere i naturali disposti su una semiretta secondo una relazione di ordinamento, da 0 a \\(+\infty\\).  
Definendoli tutti in funzione del primo elemento iterando una procedura di generazione (la funzione *successore*) si ottiene una sequenza del tipo:  
 
\\[0, S(0), S(S(0)), \dots\\]  

Possiamo definirli per ricorsione nel seguente modo:  
>_L'insieme dei numeri naturali (indicato con \\(\mathbb{N}\\)) si definisce come il più piccolo insieme tale che:_   
>\\(n \in \mathbb{N}\\) _se:_
>- \\(n = 0\\)
>- \\(n = S(x-1)\\)
>
>_dove \\(S\\) rappresenta la funzione **successore**._  

NB. "il più piccolo insieme tale che" è un sinonimo di "tutti e soli gli elementi tali che"

## Definire funzioni a valori in \\(\mathbb{N}\\)
Tornando al topic di questo capitolo, una funzione \\(f\\) sull'insieme dei naturali può essere elegantemente definirla per ricorsione sull'elemento base di \\(\mathbb{N}\\) e sul suo elemento generico (o induttivo).  

Scrivendo in Haskell abbiamo bisogno di :
- Denotare l'oggetto di base 
- Denotare un costruttore
- Decomporre oggetti composti

> Un **selettore** mi decompone un oggetto composto.  
> Per le liste ad esempio possiamo usare i selettori head e tail per decomporre l'oggetto lista in due: 
> - il primo elemento (head)
> - la lista rimanente (tail)  

Volendo definire la funzione *len* che presa una lista ne restituisca la sua lunghezza possiamo scrivere:  
\\[
    len(l)=
    \begin{cases}
    0 & \text{se } |l| = 0 \\\\
    1+len( tail(l)) & \text{altrimenti}
    \end{cases}    
\\]

e rappresentarla in haskel come:  
``` len l = if(null l) then 0 else 1 + (len (tail l))```  
dove *null* è una funzione che da valore di verità positivo se il suo argomento è una lista vuota.  


