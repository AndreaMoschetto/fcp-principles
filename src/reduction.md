# Strategie di riduzione
Possiamo vedere una strategia di riduzione come una regola che disciplina la scelta di quale \\(\beta\\)-redex ridurre prima e quale dopo.
Ogni linguaggio di programmazione funzionale può essere caratterizzato da una strategia di riduzione differente.
Possiamo individuare due macro-famiglie: **call-by-value** e **call-by-name**

## Call-by-value
Questa strategia prevede che un \\(\beta\\)-redex possa essere ridotto solo se il suo argomento è un *valore*.
Non ci curiamo adesso di definire cosa sia un *valore* proprio per mantenere un certo grado di genericità.
Ponendo ad esempio per *valore* un qualunque termine in forma normale, possiamo dire che nel seguente termine:  
\\[
 ((\lambda x.x)((\underline{\lambda y.y)z})) (\lambda x.xx)(\lambda z.z)
\\]
ridurre con una strategia call-by-value significa scegliere per primo il termine sottolineato, perchè è l'unico in forma normale.  
> N.B.
> Sottolineo ancora che la definizione di valore può essere diversa in base al linguaggio.  
> Scheme (un famoso linguaggio funzionale), ad esempio, considera anche una funzione come un valore.

## Call-by-name
Questa famiglia racchiude tutte le altre strategie, ovvero quelle che riducono un \\(\beta\\)-redex senza controllare se il suo argomento sia un valore o no.  
Due note strategie call-by-name sono:
- **lazy strategy** (chiamate anche **call-by-need**): che riducono un redex solo se strettamente necessario per valutare il valore finale;
- **leftmost-outermost** (chiamate anche **normal order**): che come dice il nome provano a ridurre per primo il \\(\beta\\)-redex più esterno e più a sinistra.

> **Esempio di normal order strategy**
> se consideriamo il termine:
> \\[(\underline{(\lambda x.x)\lambda y.yy})((\lambda x.xx)z)\\]
> una strategia normal order ridurrà per primo il termine sottolineato proprio perchè è tra i più esterni quello più a sinistra.  

## Confronto tra strategie
Avevo detto che il nostro termine \\(\Omega\\) sarebbe servito.  
Se un termine ha una forma normale ma è anche possibile ridurlo un numero infinito di volte, significa che possiede un sottotermine che non ha una forma normale. Ad esempio il termine  
\\[(\lambda x.z)\Omega\\]
Notiamo che, dato che \\(\Omega\\) riduce sempre in se stesso, solo una strategia che lo tratti "per intero" senza ridurlo potrà "eliminarlo" ottenendo:
\\[(\lambda x.z)\Omega \rightarrow z\\]
Fare questo significa usare una strategia call-by-name.  
Se invece tentassimo di ridurre prima il termine \\(\Omega\\) per poi (immaginandolo possibile) applicarvi la funzione a sinistra, otterremmo una sequenza infinita del tipo:
\\[(\lambda x.z)\Omega \rightarrow (\lambda x.z)\Omega \rightarrow \dots\\]
Fare questo significa usare un strategia call-by-value.  
In conclusione possiamo dire che seppur una call-by-name possa metterci un numero di riduzioni maggiore di una call-by-value a raggiungere una forma normale (ammesso che esista), essa ci garantisce sempre che venga raggiunta.
Mentre invece, una sequenza di riduzioni call-by-value potrebbe raggiungere una forma normale più rapidamente come potrebbe divergere all'infinito senza mai trovarla.

## Teorema di Standardizzazione
Se un termine ha una forma normale essa sarà sempre raggiunta con una strategia di riduzione normal order.
