# Curryficazione
La curryficazione è una trasformazione da una funzione:  
\\[f:(A_1,\dots,A_n)\rightarrow B\\]  
a una funzione  
\\[f_C:A_1\rightarrow(A_2\rightarrow(\dots(A_n\rightarrow B)..))\\]  

**in \\(\lambda\\)-calcolo ogni funzione è curryficata**.  
Questa è una delle prime cose che ci si sente dire quando si impara il \\(\lambda\\)-calcolo.  
Vediamo intanto di capire praticamente cosa significa che una funzione sia curryficata.  
Prendiamo in considerazione una funzione  
\\[sum:\mathbb{N}\times\mathbb{N} \rightarrow \mathbb{N}\\]  
che prende come argomento due naturali e ne restituisce la somma.
La sua versione curryficata sarà  
\\[sum_c:\mathbb{N} \rightarrow \mathbb{N} \rightarrow \mathbb{N}\\]  
che prende come argomento un intero \\(a\\) e restituisce una nuova funzione (detta applicazione parziale) che prende un intero e vi applica la somma di \\(a\\).    

> **Ad esempio**  
> \\(sum(3,5)=8\\) non essendo curryficata applica *subito* la somma fra i due argomenti;  
>   
> Invece \\(sum_c(3,5)=8\\) *in un primo momento* restituisce una **applicazione parziale**, rappresentabile come una
> funzione \\(f(x)=3+x\\) che prende un intero e vi somma sempre 3; *in un secondo momento* questa nuova funzione si applica al 5
> restituendo 8.  
>
> La stessa notazione usata sopra, infatti, è inesatta:  
> dovremmo scrivere \\(sum_c(3)(5)=8\\)

## Lati negativi della curryficazione
Nessuno.  
Per quanto sembri controintuitivo all'inizio, la curryficazione aggiunge solo espressività in più rispetto alla forma non curryficata di una funzione.  
Per questo motivo in \\(\lambda\\)-calcolo scriviamo soltanto funzioni con un solo argomento.
> **N.B.**  
> Quando si vedono \\(\lambda\\)-termini come \\(\lambda x.y.zxy\\)  
> è solo una abbreviazione di \\(\lambda x. \lambda y. zxy\\)  

E dato che Haskel implementa il modello computazionale del \\(\lambda\\)-calcolo, la funzione somma viene applicata come segue  
```
sum 3 5
```
senza l'ausilio di parentesi, dal momento in cui *in haskel ogni funzione è currificata*  
## Nota sulle funzioni di ordine superiore
Quando parliamo di funzioni che prendono come argomento o restituiscono altre funzioni, vi si fa riferimento col termine **funzioni di ordine superiore**.  
> Ad esempio la funzione *atzero* che prende una funzione come argomento e restituisce l'applicazione di quella funzione a zero.  
> ``` atzero f = f 0 ```  
> Oppure la funzione compose che prende due funzioni (sempre in versione curryficata) e ne restituisce la composizione  
>  ``` compose f g = f g ```  

## Bibliografia
- [Computazioni, Programmazione funzionale e Preludio al λ-calcolo](https://www.dmi.unict.it/barba/PRINC-FUN-CONC/PROGRAMMI-TESTI/READING-MATERIAL/preludioLambda.pdf) by Franco Barbanera
- [Short Introduction to Functional Programming and Lambda-calculus](https://www.dmi.unict.it/barba/PRINC-FUN-CONC/PROGRAMMI-TESTI/READING-MATERIAL/ShortIntroFPprog-lang.htm) by Franco Barbanera