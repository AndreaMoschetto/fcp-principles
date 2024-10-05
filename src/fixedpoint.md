# Rappresentare funzioni ricorsive
Chi non ha mai avuto a che fare con un modello computazionale funzionale, pensa che per un linguaggio di programmazione sia necessario poter dare nomi alle variabili per rappresentare una funzione ricorsiva.  
Ad esempio la funzione fattoriale 

\\[
    fact(n)=
    \begin{cases}
    1 & \text{se } n = 0 \\\\
    fact(n-1) & \text{se } n > 0
    \end{cases}    
\\]  
In Haskell viene rappresentata così:  
```
    fact 0 = 1
    fact n = n * fact n-1
```  
Il lambda calcolo non dispone di nessuna primitiva che ci consenta di fare questo.  
Nonostante ciò riusciamo a rappresentare queste funzioni attraverso il concetto di **punto fisso**.

> **Punto fisso**  
> Il punto fisso di una funzione \\(F: D \rightarrow D\\) è quell'elemento \\(d \in D\\) tale che  
> \\[F(d)=d\\]
  
Nel lambda-calcolo chiamiamo \\(Y\\) *l'operatore di punto fisso*. Ovvero il termine così definito:  
\\[\lambda f.(\lambda x. f(xx))(\lambda x. f(xx))\\]  
Esso rappresenta una funzione che si definisce in termini di se stessa  
(si noti la similarità col termine \\(\Omega\\) visto nella sezione sulle [strategie di riduzione](./reduction.md))  

Per rappresentare la funzione fattoriale nel lambda calcolo possiamo didatticamente immaginare tre passaggi:
1. Rappresentiamo la funzione in una maniera intuitiva esplicitandone il nome:  
  \\[\text{fact = }\lambda \text{x. if(x=0) then 1 else (x*fact(x-1))}\\]
2. Sostituiamo il nome della funzione con una astrazione funzionale:  
  \\[\lambda f.\lambda \text{x. if(x=0) then 1 else x*}\lambda f.\text{(x-1)}\\]
3. Applichiamo l'operatore di punto fisso alla funzione:  
  \\[Y(\lambda f.\lambda \text{x. if(x=0) then 1 else x*}\lambda f.\text{(x-1)})\\]
> Durante la valutazione di questo termine è importante tenere a mente che \\(Y(F) = F(Y(F))\\)

## Esempio valutazione lambda termine con punto fisso
>**Fibonacci 3**  
>Questo esempio mostra una valutazione del lambda-termine che rappresenta il fattoriale di 3  
>  
>**Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 3  
>  
>applico l'operatore di punto fisso  
>\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1))) **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 3  
>  
>adesso applico la nuova funzione alla prima sostituendo \\(\lambda\\)f con Y(se stessa)  
>(\\(\lambda\\)n.if(= n 0)1(mult n **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) (minus n 1))) 3  
>  
>applico la costante 3 sostituendola al \\(\lambda\\)n  
>if(= 3 0)1(mult 3 **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) (minus 3 1))  
>  
>risolvo le valutazioni della funzione più esterna (quindi l'if a sinistra e il minus più a destra)  
>mult 3 **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 2  
>  
>ora reitero i passaggi precedenti  
>mult 3 if(= 2 0)1(mult 2 **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) (minus 2 1))  
>  
>mult 3 mult 2 **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 1  
>  
>mult 3 mult 2 if(= 2 0)1(mult 2 **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) (minus 1 1))  
>  
>mult 3 mult 2 mult 1 **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 0  
>  
>mult 3 mult 2 mult 1 1  
>  
>6

## Bibliografia
- [Short Introduction to Functional Programming and Lambda-calculus](https://www.dmi.unict.it/barba/PRINC-FUN-CONC/PROGRAMMI-TESTI/READING-MATERIAL/ShortIntroFPprog-lang.htm) by Franco Barbanera