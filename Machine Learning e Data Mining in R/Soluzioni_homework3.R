# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione --------------------------------------------------------
rm(list = ls()) # Rimuove tutte le variabili
dev.off() # Chiude tutti i grafici
cat("\f") # Pulisce la console
cat("\014") # Pulisce la console

# Utilizziamo il data set SpotWeldingData che descrive le caratteristiche dei punti di
# saldatura realizzati per saldatura a resistenza.

# Carica le librerie necessarie 
library(dplyr)
library(GGally)
library(PerformanceAnalytics)

# Carica il data set SpotWeldingData ---------------------------------------------------
# che descrive le caratteristiche dei punti di saldatura realizzati 
# mediante saldatura a resistenza per punti. 

SpotWeldingData <- read.csv2("data/SpotWeldingData.csv")

# Descrizione variabili ---------------------------------------------------
# Per completezza, ti elenco il significato sintetico di ciascuna colonna:
# * R_Med è il valore medio della resistenza elettrica
# * I_Med è il valore medio della corrente elettrica
# * Energia è il valore dell'energia elettrica impiegata nella saldatura
# * Punti è il numero di punti di saldatura realizzati con lo stesso utensile
# * Ravv è il numero di volte che l'utensile è stato rinnovato
# * R_Fin è il valore finale della resistenza elettrica
# * R_Max è il valore massimo della resistenza elettrica
# * R_Min è il valore minimo della resistenza elettrica
# * T_Max è l'istante di tempo in cui si è registrato il valore massimo della resistenza 
#   elettrica
# * T_Min è l'istante di tempo in cui si è registrato il valore minimo della resistenza 
#   elettrica
# * Indice_Spruzzo indica l'intensità dello spruzzo, un fenomeno che riduce la qualità della 
#   saldatura. Assume valore zero solo se tale fenomeno non si è verificato.
# * T_Spruzzo è l'istante di tempo in cui si è verificato il fenomeno di spruzzo


# Esplorazione dei dati ---------------------------------------------------

# Realizza una scatter plot matrix usando o la funzione chart.Correlation del pacchetto
# Performance Analytics o la funzione ggpairs contenuta nel pacchetto GGally.

SpotWeldingData %>% chart.Correlation(histogram=TRUE, pch=19)
SpotWeldingData %>% ggpairs()

# Nota che la matrice è di difficle lettura a causa dell'elevato numero di colonne/variabili
# del data set. Inoltre, nota che ci sono variabili che sono correlate tra loro.
# Infatti alcuni coefficienti di determinazione indicati nella parte superiore della matrice 
# sono molto elevati.

# In tali circostanze è consigliabile applicare la PCA al fine di: 
#  * ottimizzare la memoria occupata dal data set 
#  * ridurre lo sforzo computazionale delle successive analisi 
#  * visualizzare il data set attraverso una sua proiezione su piani ottimali in grado di 
#    catturare quasi tutta la variabilità totale. 

# Calcola le componenti principali mediante la funzione prcomp del pacchetto stats  ---------------------------------------------------
# scalando il dataset

pr.out <- SpotWeldingData %>% prcomp(., scale=TRUE)

# Ricorda che il numero massimo di PC è pari al numero di variabili (dimensionalità) del
# data set originale.
# Produci uno o più grafici che ti consentano di poter scegliere in maniera opportuna
# il numero di componenti principali


par(mfrow=c(1,2)) 
fvs <-  pr.out$sdev^2/sum(pr.out$sdev^2)
plot(
  fvs,
  xlab = " PC ",
  ylab = " Frazione di varianza spiegata",
  main= "scree plot",
  ylim = c(0, 1) ,
  type = 'b') #scree plot

plot(
  cumsum(fvs) ,
  xlab = " PC ",
  ylab = " Frazione cumulata di varianza spiegata",
  ylim = c(0, 1) ,
  type = 'b')

# Quante componenti hai scelto? Perché?   ---------------------------------------------------
# Nota che il numero di PC da tenere in considerazione per una compressione soddisfacente 
# del data set originale è pari a 3. Infatti, il gomito della `fvs` si configura proprio in 
# corrispondenza della terza PC e le prime tre PC spiegano poco meno dell'80% della varianza 
# totale.

# Costruisci il biplot   ---------------------------------------------------

par(mfrow=c(1,1)) #per un solo grafico
biplot(pr.out, scale=0, cex=.7); abline(h=0,v=0,lty=2)

# Visto che sono state scelte 3 componenti principali, è utile visualizzare anche i biplot riferiti ai piani PC1-PC3 e PC2-PC3
par(mfrow=c(1,2)) 
biplot(pr.out, choices = c(1,3), scale=0, cex=.7); abline(h=0,v=0,lty=2)
biplot(pr.out, choices = c(2,3), scale=0, cex=.7); abline(h=0,v=0,lty=2)

# Che conclusioni potresti trarre?   ---------------------------------------------------

# Nota che le proiezioni delle colonne originali del data set nel piano delle prime due evidenziano che 
# le colonne sono riconducibili a due grandi gruppi di variabili A e B.

# Il gruppo A è formato dalle variabili: Indice_Spruzzo, T_Spruzzo, Energia, R_Med e R_Fin; 
# Queste hanno una componente orizzontale maggiore di quella verticale quindi pesano più 
# sulla prima componente principale. 
# Inoltre, questo gruppo può essere suddiviso in due 
# sottogruppi A1 e A2, negativamente correlati. 
# Il sottogruppo A1 è formato da Indice_Spruzzo e T_Spruzzo.
# Il sottogruppo A2 è formato da Energia, R_Med e R_Fin. 
# In questi casi si può sempre dare un'interpretazione di questo tipo:
# Le variabili del sottogruppo A1 (A2) hanno loading sulla prima componente 
# principale concorde, e quindi sono correlate positivamente tra di loro. 
# Le variabili del sottogruppo A1 hanno loading discordi in segno rispetto a quelle 
# del sottogruppo A2, quindi le variabili in A1 tendono ad essere correlate negativamente con le variabili in A2.
# Interpretazione: Il gruppo A corrisponde a variabili che forniscono informazioni che descrivono 
# il fenomeno di spruzzo nella saldatura

# Il gruppo B è formato dalle variabili: R_Max, R_Min, T_Min, T_Max, Punti, Ravv, I_Med, che
# hanno una componente verticale maggiore di quella orizzontale, e che quindi pesano più 
# sulla seconda componente principale. 
# Anche questo gruppo può essere suddiviso in due sottogruppi B1 e B2, con analoga interpretazione ai gruppi A1 e A2.
# Il sottogruppo B1 è formato da R_Max e R_Min.
# Il sottogruppo B2 è formato da: T_Min, T_Max, Punti, Ravv e I_Med.
# Il gruppo B corrisponde a variabili che forniscono informazioni sull'utensile che ha eseguito la saldatura.


# Elementi aggiuntivi di visualizzazione grafica del biplot ---------------

# Il biplot evidenzia che gli scores si dispongono lontani dall'origine degli assi a 
#formare due gruppi.

# Dall'interpretazione del biplot (in particolare del loading plot) riesci ad avere un'idea 
#della variabile o del gruppo di variabili responsabile di tale separazione?

# Sì, sembrerebbe che la separazione avvenga lungo la direzione individuata dalle variabili 
#Indice_Spruzzo, T_Spruzzo,Energia, R_Med.

# Questa deduzione, permette agli ingegneri di processo di avanzare la congettura che 
#i due gruppi siano legati alla presenza/assenza del fenomeno dello "spruzzo". 

# Come facciamo a validare questa congettura?
# Potremmo colorare gli score sulla base della variabile Indice_Spruzzo che indica l'intensità dello spruzzo. Assume valore zero solo se tale fenomeno non si è verificato.

# Quindi costruiamo una variabile ausiliaria di tipo carattere
Spruzzo <- as.character(factor(SpotWeldingData$Indice_Spruzzo!=0,levels=c(TRUE,FALSE),labels = c("red","black"))) 
# che restituisce "red" in presenza di spruzzo, "black" altrimenti
# NB: potresti usare l'istruzione meno leggibile ma più compatta
Spruzzo <- (SpotWeldingData$Indice_Spruzzo!=0)+1
#che restituisce 2 in presenza di spruzzo, 1 altrimenti

# Purtroppo non è possibile colorare direttamente il biplot, ma dobbiamo costruire lo scoreplot
#con la funzione di base plot()

plot(pr.out$x[,1],pr.out$x[,2],
     main= "score plot",
     xlab="PC1", ylab="PC2" )

# sostituendo i punti con le etichette colorate in base alla variabile ausiliaria Spruzzo
plot(pr.out$x[,1],pr.out$x[,2],
     main= "Score plot",
     xlab="PC1", ylab="PC2", type="n") 
text(pr.out$x[,1],pr.out$x[,2], labels = row.names(SpotWeldingData), col=Spruzzo, cex = 0.7)

# La congettura è validata!
# e i due gruppi effettivamente sono dovuti esattamente alla presenza/assenza di spruzzo!
# Continueremo questa discussione dopo aver affrontato i metodi automatici per l'analisi dei gruppi.

# Sei riuscito a sintetizzare gran parte del contenuto della scatter plot matrix agevolandoti 
# della rappresentazione semplificata del dataset offerta dal biplot delle prime due 
# componenti principali. Inoltre, a differenza della scatter plot matrix con la PCA sei stato in grado di  
# considerare tutte le variabili del data set originale contemporaneamente e non solo a coppie..


# Ti aspetto al RStudio Lab di fine modulo, per completare la storia di questo data set!


# Spunti di riflessione per analisi aggiuntive-------------------------------------------------------------

# Il raggruppamento di variabili è stato ottenuto facendo riferimento alle
# proiezioni delle variabili originali solo sulle prime due componenti principali PC1 e PC2.
# Queste spiegano circa il 60% della varianza totale del dataset.
# Dalla visualizzazione dei biplot riferiti ai piani PC1-PC3 e PC2-PC3 non ricaviamo informazioni aggiuntive.

# Tuttavia, per analizzare anche le variabili che incidono su PC3, varrebbe la pena procedere nel modo seguente.

# Avendo scoperto che c'è una separazione degli scores molto forte relativa unicamente alle variabili del gruppo A, 
# ed, in particolare, della sola variabile Indice_Spruzzo si dovrebbe procedere ad 
# eliminare dal dataset questa informazione, (ossia eliminando tutte le variabili del gruppo A)
# e ripetere le analisi per trovarne di nuove
pr.out1 <-
  prcomp(SpotWeldingData %>% select(-c(
    Indice_Spruzzo, T_Spruzzo, Energia, R_Med, R_Fin
  )), scale = T)

fvs1 <-  pr.out1$sdev^2/sum(pr.out1$sdev^2)
plot(
  cumsum(fvs1) ,
  xlab = " PC ",
  ylab = " Frazione cumulata di varianza spiegata",
  ylim = c(0, 1) ,
  type = 'b')

biplot(pr.out1, cex=0.7); abline(h=0,v=0,lty=2)
#I gruppi sono scomparsi, come ci aspettavamo, e si possono individuare, tramite i loading visibili sul biplot, dei nuovi gruppi di variabili (che prima incidevano sulla PC3).


