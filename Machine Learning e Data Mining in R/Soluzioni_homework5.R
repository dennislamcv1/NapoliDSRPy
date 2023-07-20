# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione --------------------------------------------------------
rm(list = ls()) # Rimuove tutte le variabili
dev.off() # Chiude tutti i grafici
cat("\f") # Pulisce la console
cat("\014") # Pulisce la console

# Carica le librerie necessarie----------
library(tidyverse)
library(cluster)
library(reshape)


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
# inoltre abbiamo definito la variabile
Spruzzo <- (SpotWeldingData$Indice_Spruzzo != 0)+1


# Nel precedente practice lab sulla PCA hai imparato che il biplot nel piano individuato dalle prime due
# componenti principali può evidenziare la presenza di eventuali cluster.

# Calcola le componenti principali mediante la funzione prcomp del pacchetto stats standardizzando il
# data set

pr.out <- SpotWeldingData %>% prcomp(., scale=TRUE)

#che equivale ad applicare prcomp al data set standardizzato SpotWeldingData_stand
#definito di seguito

# Visualizza il biplot nel piano individuato dalle prime due componenti principali

biplot(pr.out, cex = 0.5)
abline(h=0,v=0,lty=2)

# Hai già osservato che i due cluster sono legati alla presenza/assenza del 
# fenomeno dello spruzzo

# Clustering K-means su data set standardizzato------------------------------------------------------
#standardizza il dataset SpotWeldingData salvandolo in SpotWeldingData_stand
SpotWeldingData_stand <- SpotWeldingData %>% scale() %>% as.data.frame()

# Crea un vettore con i valori di K che vuoi esplorare

k_vec <- 2:10

# Crea un vettore numerico (inizializzandolo con zeri) della stessa lunghezza di k_vec in cui
# salvare i valori di silhouette media per ogni K considerato in k_vec

silhouette_vec <- numeric(length(k_vec)) 

for (l in seq_along(k_vec)) { 
  # Applica il k-means per ogni valore di K in k_vec e cerca l'ottimo globale a partire da 
  # 100 soluzioni iniziali
  km_out <- kmeans(SpotWeldingData_stand, centers = k_vec[l], nstart = 30) 
  # Calcola il coefficiente di silhouette per la soluzione corrente
  sil <- silhouette(km_out$cluster, dist = dist(SpotWeldingData_stand)) 
  silhouette_vec[l] <- summary(sil)$avg.width
}


# Visualizza un grafico che riporti per ogni K i valori di silhouette media
plot(k_vec, silhouette_vec, 
     type = "b",
     xlab = "K",
     ylab = "Silhouette (media)")

# Qual è il numero di cluster ottimale?
k_vec[which.max(silhouette_vec)]
# L'indice di silhouette è massimo in corrispondenza di due cluster
# in accordo con quanto ci aspettavamo dai risultati visualizzati nel biplot/score plot dal RStudio Lab su PCA

# Qual è quindi il coefficiente di silhouette?
max(silhouette_vec)


# Applica il clustering k-means in corrispondenza del numero di cluster ottimale=2 e 30 punti di partenza
# salva i risultati in una variabile chiamata km.out
km.out <-
  kmeans(SpotWeldingData_stand, k_vec[which.max(silhouette_vec)], nstart = 30)


#registra il vettore di etichette di assegnazione dei cluster nella variabile km.cluster 
km.cluster <- km.out$cluster


## L'algoritmo K-means su data set standardizzato riesce ad individuare correttamente i due cluster? --------
# Per rispondere a questa domanda, realizza lo score plot nelle prime due componenti principali
# come fatto nel Lab RStudio sulla PCA e colora gli score in base alla variabile km.cluster 
# invece che alla variabile Spruzzo identificata nel Lab RStudio sulla PCA
# Spruzzo <- (SpotWeldingData$Indice_Spruzzo != 0)+1

plot(pr.out$x[,1],pr.out$x[,2],
     main= "Score plot + clustering K-means",
     xlab="PC1", ylab="PC2", type="n") 
text(
  pr.out$x[, 1],
  pr.out$x[, 2],
  labels = row.names(SpotWeldingData),
  col = km.cluster,
  cex = 0.5
)

#il clustering K-means produce il risultato sperato 
#in quanto i due gruppi di score visualizzati nel grafico precedente 
# sono colorati coerentemente

## La risposta è: Sì -----------------------------------------------------
# Tale risultato può essere riassunto anche nella seguente tabella (detta matrice di confusione)
# che tipicamente non è possibile produrre in quanto nei problemi di apprendimento non supervisionato
# non conosciamo le vere etichette. 
# In questo caso è possibile grazie alla variabile Spruzzo identificata nel Lab RStudio sulla PCA
tab_clust <- data.frame(Spruzzo, Cluster = km.cluster)
table(tab_clust)
#Quando il clustering  produce il risultato sperato, 
#abbiamo su una delle due diagonali della tabella tutti valori nulli


# Clustering K-means su data set non standardizzato------------------------------------------------------

# Ripeti il clustering K-means sul data set non standardizzato

k_vec <- 2:10
silhouette_vec <- numeric(length(k_vec)) 
for (l in seq_along(k_vec)) { 
  # Applica il k-means per ogni valore di K in k_vec e cerca l'ottimo globale a partire da 
  # 100 soluzioni iniziali
  km_out <- kmeans(SpotWeldingData, centers = k_vec[l], nstart = 30) 
  # Calcola il coefficiente di silhouette per la soluzione corrente
  sil <- silhouette(km_out$cluster, dist = dist(SpotWeldingData)) 
  silhouette_vec[l] <- summary(sil)$avg.width
}
km.out <- kmeans(SpotWeldingData, k_vec[which.max(silhouette_vec)], nstart = 30)
km.cluster <- km.out$cluster

## L'algoritmo K-means su data set non standardizzato riesce ad individuare correttamente i due cluster? --------
plot(pr.out$x[,1],pr.out$x[,2],
     main= "Score plot + clustering K-means su SpotweldingData non standardizzato",
     xlab="PC1", ylab="PC2", type="n") 
text(
  pr.out$x[, 1],
  pr.out$x[, 2],
  labels = row.names(SpotWeldingData),
  col = km.cluster,
  cex = 0.5
)
# gli score non sono colorati coerentemente
# La risposta è: No -----------------------
tab_clust <- data.frame(Spruzzo, Cluster = km.cluster)
table(tab_clust)
# Nessuna diagonale presenta  valori nulli
# I gruppi non sono stati correttamente individuati 
# La standardizzazione è quindi necessaria, per evitare che variabili con troppa varianza
# dovuta all'unità di misura inficino la divisione in gruppi
# esplora i boxplot con la funzione già usata nel reading sulla PCA
boxplot(SpotWeldingData)
SpotWeldingData %>% 
  melt() %>% 
  ggplot(aes(variable, value)) + 
  geom_boxplot()

SpotWeldingData_stand %>% 
  melt() %>% 
  ggplot(aes(variable, value)) + 
  geom_boxplot()

# Clustering gerarchico su dataset standardizzato---------------------------------------------------
# Utilizzando il data set SpotWeldingData_stand, esegui il clustering gerarchico 
# con distanza 
distance.method = "euclidean"
# e linkage  
linkage.method = "complete"
#e visualizza il dendogramma 
par(mfrow=c(1,1))

hc.out <-
  hclust(dist(SpotWeldingData_stand, method = distance.method),
         method = linkage.method)
plot(hc.out, main = paste("distanza=", distance.method, "  linkage=", linkage.method))

#Taglia il dendogramma a K=2 
hc.clusters <- cutree(hc.out, k = 2)
# e visualizza i risultati del clustering colorando i punti 
# nello score plot in base ai cluster assegnati
# come fatto per l'algoritmo k-means
# evitiamo di mostrare le etichette degli score
plot(
  pr.out$x[, 1],
  pr.out$x[, 2],
  pch = 16,
  cex = 0.5,
  col = hc.clusters,
  main = paste("distanza=", distance.method, "  linkage=", linkage.method),
  xlab = "PC1",
  ylab = "PC2"
)

## Il clustering gerarchico su data set standardizzato con distanza euclidea e linkage completo riesce ad individuare correttamente i due cluster?-------
# No 



# Clustering gerarchico al variare delle metriche e del linkage-------
# #Utilizzando il data set SpotWeldingData_stand, esegui il clustering gerarchico 
# al variare della distanza 
distance.method <- c("euclidean","manhattan","canberra")
# e del linkage  
linkage.method <- c("complete", "average", "single")
#e visualizza i dendogrammi in una matrice di pannelli
par(mar=c(1,4.1, 4.1, 2.1)) # per modificare i margini tra i grafici 
# par()$mar ## visualizza i valori correnti  c(bottom, left, top, right)

par(mfrow=c(length(distance.method),length(linkage.method)))

for (i in seq_along(distance.method)) {
  for (j in seq_along(linkage.method)) {
    hc.out <-
      hclust(dist(SpotWeldingData_stand, method = distance.method[i]),
             method = linkage.method[j])
    plot(
      hc.out,
      main = paste("Distanza=", distance.method[i], "    Linkage=",linkage.method[j]),
    )
  }
}


# Osserva (se necessario premi su zoom nel pannello plot)
# che non tutti i dendogrammi hanno la stessa distanza verticale tra 2 e 3 cluster
#Puoi visualizzarli se necessario uno alla volta.
par(mar=c(5.1,4.1, 4.1, 2.1)) # per modificare i margini tra i grafici 
par(mfrow=c(1,1)) 
distance.method <- c("euclidean","manhattan","canberra")
linkage.method <- c("complete", "average", "single")
i=1 # scegli distance.method
j=1 # scegli linkage method

hc.out <-
  hclust(dist(SpotWeldingData_stand, method = distance.method[i]),
         method = linkage.method[j])
plot(
  hc.out,
  main = paste("Distanza=", distance.method[i], "    Linkage=",linkage.method[j]))

# Taglia tutti i dendogrammi a K=2 e visualizza i risultati del clustering colorando i punti
# nello score plot in base ai cluster assegnati
# come fatto per l'algoritmo k-means
par(mar=c(4, 4.1, 4.1, 2.1)) # per modificare i margini tra i grafici 
par(mfrow=c(length(distance.method),length(linkage.method)))

for (i in seq_along(distance.method)) {
  for (j in seq_along(linkage.method)) {
    hc.out <-
      hclust(dist(SpotWeldingData_stand, method = distance.method[i]),
             method = linkage.method[j])
    hc.clusters <- cutree(hc.out, k = 2)
    # Genera una tabella per confrontare i risultati del clustering con i gruppi di punti
    # di saldatura individuati mediante la PCA, come già fatto per il K-Means.
    tab_clust <- data.frame(Spruzzo, Cluster = hc.clusters)
    paste("Distanza=", distance.method[i], "    Linkage=",linkage.method[j]) %>% print()
    table(tab_clust) %>% print()
    #score plot nelle prime due componenti principali
    plot(
      pr.out$x[, 1],
      pr.out$x[, 2],
      pch = 16,
      cex = 0.5,
      col = hc.clusters,
      main = paste("Distanza=", distance.method[i], "    Linkage=",linkage.method[j]),
      xlab = "PC1",
      ylab = "PC2"
    )
  }
}


## I risultati migliorano al variare di distanza e linkage utilizzati?-----------
#Sì, alcune combinazioni metrica/linkage riescono a distinguere correttamente i punti di saldatura 
# con spruzzo e senza spruzzo. In particolare: euclidean/single, manhattan/average, manhattan/single,
#canberra/complete, canberra/average

# Ripeti quanto fatto in precedenza considerando il data set non standardizzato-------
# usa quindi il data set SpotWeldingData

par(mar=c(4, 4.1, 4.1, 2.1)) # per modificare i margini tra i grafici 
par(mfrow=c(length(distance.method),length(linkage.method)))

for (i in seq_along(distance.method)) {
  for (j in seq_along(linkage.method)) {
    hc.out <-
      hclust(dist(SpotWeldingData, method = distance.method[i]),
             method = linkage.method[j])
    hc.clusters <- cutree(hc.out, k = 2)
    # Genera una tabella per confrontare i risultati del clustering con i gruppi di punti
    # di saldatura individuati mediante la PCA, come già fatto per il K-Means.
    tab_clust <- data.frame(Spruzzo, Cluster = hc.clusters)
    paste("Distanza=", distance.method[i], "    Linkage=",linkage.method[j]) %>% print()
    table(tab_clust) %>% print()
    #score plot nelle prime due componenti principali
    plot(
      pr.out$x[, 1],
      pr.out$x[, 2],
      pch = 16,
      cex = 0.5,
      col = hc.clusters,
      main = paste("Distanza=", distance.method[i], "    Linkage=",linkage.method[j]),
      xlab = "PC1",
      ylab = "PC2"
    )
  }
}
## Cosa puoi osservare?--------------

# Solo la metrica=Canberra se combinata a linkage=complete e average
# è in grado di distinguere correttamente 
# le saldature con spruzzo e senza spruzzo 


# Clustering su PCA --------------------------------------------------------



## Clustering K-means su PC1 e PC2--------
# Se ripeti il clustering K-means con K=2 direttamente sulle prime due PC cosa osservi?----
km.out.pca <- kmeans(pr.out$x[, c(1, 2)], 2, nstart = 25)
par(mfrow=c(1,1))
plot(
  pr.out$x[, 1],
  pr.out$x[, 2],
  pch = 16,
  cex = 0.5,
  col = km.out.pca$cluster,
  main = "K-means su PC1 e PC2",
  xlab = "PC1",
  ylab = "PC2"
)
## Il K-means funziona altrettanto bene anche prendendo in considerazione
# il data set compresso sulle sole prime due PC
## in questo modo sei stato in grado di combinare PCA e clustering per ottenere risultati più
# efficienti. Analizzare (in questo caso col K-means) un data set ridotto diminuisce lo sforzo 
# computazionale, senza inficiarne (in questo caso) il risultato.


# Complimenti! hai terminato con successo il Lab Rstudio di fine modulo
# e sei in grado di trattare i casi principali di unsupervised learning.
# Sei pronto per affrontare il Quiz di fine week!
