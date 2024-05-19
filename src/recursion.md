# Definizione ricorsiva sui naturali

## Reminder sui numeri naturali  
Dal momento in cui gli elementi dei naturali sono ordinati e hanno punto di partenza ma non hanno una fine, possiamo definirli tutti in funzione del primo elemento con l'ausilio di una funzione generativa chiamata **successore**.

Possiamo vederli intuitivamente come una sequenza del tipo:  
\\[0, S(0), S(S(0)), \dots\\]  

Possiamo definirli per ricorsione nel seguente modo:  
>_L'insieme dei numeri naturali (indicato con \\(\mathcal{N}\\)) si definisce come il più piccolo insieme tale che:_   
>\\(n \in \mathcal{N}\\) _se:_
>- \\(n = 0\\)
>- \\(n = S(x-1)\\)
>
>_dove \\(S\\) rappresenta la funzione **successore**._  

NB. "il più piccolo insieme tale che" è un sinonimo di "tutti e soli gli elementi tali che"

## Definire funzioni a valori in \\(\mathcal{N}\\)

Se abbiamo una funzione \\(f\\) definita sull'insieme dei naturali allora è molto elegante definirla per ricorsione sull'elemento base di \\(\mathcal{N}\\) e sul suo elemento generico (o induttivo).  

In un linguaggio come Haskell abbiamo bisogno di :
- Denotare l'ogetto di base 
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
dove *null* è una funzione che da valore di verita à positivo se il suo argomento è una lista vuota.  


