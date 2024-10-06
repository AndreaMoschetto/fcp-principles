# Rappresentare funzioni ricorsive
Chi non ha mai avuto a che fare con questo modello computazionale penserà che per un linguaggio di programmazione sia necessario poter dare nomi alle funzioni per rappresentare una funzione ricorsiva.  
Consideriamo ad esempio la funzione fattoriale 

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
La definizione è possibile grazie al fatto che alla funzione viene attribuito un nome.
Il lambda calcolo non dispone di nessuna primitiva che ci consenta di fare questo.  
Nonostante ciò riusciamo a rappresentare queste funzioni attraverso il concetto di **punto fisso**.
## Punto fisso
 
> Il punto fisso di una generica funzione \\(F: D \rightarrow D\\) è quell'elemento \\(d \in D\\) tale che  
> \\[F(d)=d\\]
> In aritmetica non tutte le funzioni dispongono di punti fissi.  
> Ad esempio la funzione \\(f(x)=x^2\\) ha due punti fissi: 0 e 1.
> Invece la funzione \\(f(x)=x+1\\) non ne ha.
  
Trasportando la nozione di punto fisso nel lambda calcolo possiamo dire che se \\(F\\) e \\(N\\) sono lambda termini, 
si dice che se \\(N\\) è un punto fisso di \\(F\\) se \\(F N =_\beta N\\)
### Teorema
Nel lambda calcolo non tipato ogni termine F ha un punto fisso.

**Dimostrazione**  
Sia \\(Y\\) un particolare lambda termine così definito  
\\[\lambda f.(\lambda x. f(xx))(\lambda x. f(xx))\\]  
supponiamo che \\(F\\) sia un generico lambda termine e chiamiamo \\(N = YF\\).  
Dimostriamo che \\(N\\) è un puntofisso di F nel seguente modo:
\\[
    \begin{align}
    N &= YF\\\\
    &= \lambda f.(\lambda x. f(xx))(\lambda x. f(xx))F\\\\
    &= (\lambda x. F(xx))(\lambda x. F(xx))\\\\
    &\twoheadrightarrow F((\lambda x. F(xx))(\lambda x. F(xx)))\\\\
    &= F(YF)\\\\
    &= F(N)
    \end{align}
\\]\\[\qquad \qquad \qquad \qquad \qquad \qquad \blacksquare\\]

Il termine \\(Y\\) viene chiamato **operatore di punto fisso** e permette di trovare il punto fisso di una termine semplicemente applicandolo allo stesso.  
> Esistono molteplici operatori di punto fisso: di Kleene, di Tarsky, di Turing, di Church (quello utilizzato sopra), ecc.
> Ognuno è rappresentato in maniera diversa e risponde ad esigenze diverse funziona meglio con strategie di valutazione diverse.  
> Consiglio la lettura della [dimostrazione](https://www.dmi.unict.it/barba/FONDAMENTI/PROGRAMMI-TESTI/READING-MATERIAL/LAMBDA-CALCULUS/lambda-calculus.pdf)(pag.21)del medesimo teorema utilizzando l'operatore di punto fisso di Turing.
  
Vediamo come applicare questo concetto alla definizione di fattoriale attraverso i seguenti passaggi:
\\[
  \begin{align}
  \textbf{fact }\text{n} &= \text{if_then_else(iszero n)(1) (mult (n }\textbf{fact}\text{(pred n)))}\\\\
  \textbf{fact} &= \lambda n. \text{if_then_else(iszero n)(1) (mult (n fact(pred n)))}\\\\
  \textbf{fact} &= \lambda f. \lambda n. \text{if_then_else(iszero n)(1) (mult (n f(pred n)))}\textbf{fact}\\\\
  \end{align}
\\]
Se adesso chiamiamo \\(F\\) il termine:
\\[\lambda f. \lambda n. \text{if_then_else(iszero n)(1) (mult (n f(pred n)))}\\]  
otteniamo:  
\\[\textbf{fact} = F \textbf{ fact}\\]  
Il problema si riduce una equazione di punto fisso: trovare **fact**, in quanto punto fisso del termine F.  
Ormai abbiamo capito che questo genere di problemi in lambda calcolo possono essere agilmente risolti ponendo:  
\\[
\begin{align}
\textbf{fact} &= \textbf{Y } F\\\\
&=\textbf{Y }\lambda f. \lambda n. \text{if_then_else(iszero n)(1) (mult (n f(pred n)))}
\end{align}  
\\]

Adesso il lambda termine al secondo membro è un termine chiuso che rappresenta perfettamente la funzione fattoriale.

## Esempio valutazione lambda termine con punto fisso
>**Fattoriale di 3**  
>Questo esempio mostra in maniera più esplicita possibile il lavoro che svolge l'operatore di punto fisso su una valutazione del lambda-termine che rappresenta il fattoriale di 3  
>  
>**Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 3  
>  
>applico l'operatore di punto fisso  
>\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1))) **Y**(\\(\lambda\\)f.(\\(\lambda\\)n.if(= n 0)1(mult n f(minus n 1)))) 3  
>  
>adesso applico la nuova funzione alla prima sostituendo \\(\lambda\\)f con Y(F)  
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
- [Lecture Notes on the Lambda Calculus](https://www.dmi.unict.it/barba/FONDAMENTI/PROGRAMMI-TESTI/READING-MATERIAL/LAMBDA-CALCULUS/lambda-calculus.pdf) by Peter Selinger