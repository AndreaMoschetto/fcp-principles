# Ripasso sul lambda calcolo

## Modelli computazionali
Un modello computazionale è un formalismo matematico costruito appositamente per studiare le proprietà di una computazione indipendentemente dal linguaggio di programmazione.
Possiamo dire infatti che ogni buon linguaggio di programmazione *implementa* uno specifico modello computazionale.  
Questo da la garanzia che alcune proprietà siano rispettate e semplifica eventuali studi e dimostrazioni di affermazioni su programmi scritti tramite lo stesso.  
I linguaggi di programmazione funzionale si basano sul \\(\lambda\\)-calcolo.

## Lambda termine
Tre principi fondamentali sono sufficienti per derivare da essi qualunque definizione di funzione.
- **Variabile**
- **Applicazione**
- **Astrazione funzionale**  

> Si noti che ogni \\(\lambda\\)-termine rappresenta la definizione di una funzione, non la funzione stessa.  

Un \\(\lambda\\)-termine si definisce formalmente come:  

\\(\Lambda ::= X | (\Lambda \Lambda) | \lambda X.\Lambda \\)  
dove \\(\Lambda\\) rappesenta il genrico \\(\lambda\\)-termine e \\(X\\) rappresenta qualunque nome di variabile (\\(x,y,z,\dots\\))

## Scope
In un'astrazione funzionale \\(\lambda x.P\\) il termine \\(P\\) è detto *scope* dell'astrazione.  
Possiamo dire che il termine \\(P\\) rappresenta il corpo di una funzione e \\(x\\) il suo argomento.  
  
### Variabile legata (BV)
\\(
    BV(x) = \emptyset \\\\ 
    BV(PQ) = BV(P) \cup BV(Q)\\\\
    BV(\lambda x.P) = x \cup BV(P)
\\)  

### Variabile libera (FV)
\\(
    FV(x) = x \\\\ 
    BV(PQ) = FV(P) \cup FV(Q)\\\\
    FV(\lambda x.P) = FV(P) \setminus \\{x\\}
\\)  

## Sostituzione
Si definisce *sostituzione*, denotata come \\(M[L/x]\\), il tremine M in cui ogni occorrenza della variabile libera \\(x\\) viene sostituita col termine \\(L\\).  
> **Attenzione**  
> Si può fare solo a patto che valga la condizione che:
> \\[BV(M) \cap FV(L) = \emptyset\\]
> vediamo perchè:  
> rappresentiamo la funzione costante come \\(\lambda x.z\\).  
> Se applichiamo la sostituzione \\(\lambda x.z[y/z]\\) ottenendo \\(\lambda x.y\\) il senso della funzione rimane invariato.  
> Ma se scriviamo \\(\lambda x.z[x/z]\\) otteniamo \\(\lambda x.x\\) che invece rappresenta la funzione identità.  
> Sostanzialmente \\(x\\) prima di essere sostituita su \\(z\\) è libera ma poi diventa legata.
> Questo evento si evita rispettando la condizione di cui sopra.

### Definizione formale di sostituzione
Come spiegato anche sul primo capitolo. Se i \\(\lambda\\)-termini sono refiniti in funzione di tre concetti, allora anche la sostituzione sui \\(\lambda\\)-termini la definiremo sugli stessi 3 concetti. 
Rimando al [testo di riferimento](https://www.dmi.unict.it/barba/PRINC-FUN-CONC/PROGRAMMI-TESTI/READING-MATERIAL/ShortIntroFPprog-lang.htm) per veferla in dettaglio.

## \\(\beta\\)-riduzione
Un termine \\(P\\) \\(\beta\\)-riduce **in uno step** in un termine \\(Q\\) se \\(Q\\) può essere ottenuto da \\(P\\) sostituendo un sottotermine di \\(P\\) della forma \\((\lambda x.M)N\\) tramite \\(M[N/x]\\).  

> **Nota**
> Diremo che ogni termine della forma \\((\lambda x.M)N\\) si chiama **\\(\beta\\)-redex** (o *redesso*)
> mentre diremo che il relativo termine della forma \\(M[N/x]\\) è il suo **contractum**.

E si scrive:

\\[
    (\lambda.M)N \rightarrow_{\beta} M[N/x]  
\\]  
*assumiamo da ora in poi per semplicità sintattica che ogni riduzione indicata con \\(\rightarrow\\) sia una \\(\beta\\)-riduzione*  

> **Esempio**  
> Supponendo di avere la moltiplicazione (indicata con \\(*\\) )
> \\((\lambda x.x * x)2 \rightarrow 2 * 2\\)  


Possiamo anche usare la sintassi:
\\[
    P \twoheadrightarrow Q
\\]
per dire \\(P \rightarrow P_1 \rightarrow P_2 \rightarrow \dots \rightarrow P_k\\) con \\(M_k \equiv Q\\)  

## Normalizzazione
- Un \\(\lambda\\)-termine si dice **in forma normale** se non contiene nessun \\(\beta\\)-redex.  
- Un termine \\(N\\) si dice **normalizzabile** se esiste un termine \\(Q\\) in forma normale tale che  
  \\[N \twoheadrightarrow Q\\]
- Un termine si dice **fortemente normalizzabile** se ogni possibile sequenza di \\(\beta\\)-riduzioni porta a una forma normale
  
> **Il termine \\(\Omega\\)**  
> Esiste un termine molto particolare chiamato \\(\Omega\\) che ha questa forma  
> \\[(\lambda x.xx)(\lambda y.yy)\\]  
> tale che \\(\Omega \rightarrow \Omega\\) all'infinito.
> Vediamo una implicazione interessante nel capitolo sulle strategie di riduzione

## Bibliografia
[Short Introduction to Functional Programming and Lambda-calculus](https://www.dmi.unict.it/barba/PRINC-FUN-CONC/PROGRAMMI-TESTI/READING-MATERIAL/ShortIntroFPprog-lang.htm) by Franco Barbanera