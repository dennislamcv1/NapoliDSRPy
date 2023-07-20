# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione 
rm(list = ls()) # Rimuove tutte le variabili
dev.off() # Chiude tutti i grafici
cat("\f") # Pulisce la console
cat("\014") # Pulisce la console

# Lavoriamo ancora coi dati Covid-19.
# I dati COVID-19 sono i dati ufficiali a livello regionale forniti dalla Protezione 
# Civile al seguente link: 
# https://github.com/pcm-dpc/COVID-19/tree/master/dati-regioni. 
#
# Guardiamo i dati giornalieri (a livello regionale) del numero di persone in terapia intensiva.
# Cerchiamo di identificare gruppi di regioni simili in base all'andamento del numero di 
# persone in terapia intensiva.

library(tidyverse)
df_rg <- read_csv("data/dpc-covid19-ita-regioni.csv",
                  col_types = c("casi_testati" = "n",
                                "casi_da_sospetto_diagnostico" = "n",
                                "casi_da_screening" = "n",
                                "ingressi_terapia_intensiva" = "n",
                                "note_casi" = "c",
                                "note_test" = "c"))
df_rg$data <- as.Date(df_rg$data)

df_rg %>% 
  select(data, terapia_intensiva, totale_ospedalizzati, denominazione_regione) %>% 
  head()

df_rg %>% 
  ggplot() +
  geom_line(mapping = aes(data, terapia_intensiva, colour = denominazione_regione))

# Questo tipo di visualizzazione non è molto chiara. 
# Possiamo usare facet_wrap() 

df_rg %>% 
  ggplot() +
  geom_line(mapping = aes(data, terapia_intensiva)) +
  facet_wrap(~denominazione_regione)

# Vogliamo effettuare un raggruppamento delle regioni, per individuare quelle più simili 
# in termini dell'andamento del numero di pazienti in terapia intensiva. 
# Abbiamo bisogno che i dati siano organizzati nel seguente modo:
# 
# * Le righe devono corrispondere alle regioni da clusterizzare
# * Le colonne, che in genere corrispondono alle diverse variabili, devono rappresentare
#   i giorni di nostro interesse. 
# 
# Puoi usare la funzione `pivot_wider()`:


df_rg_wider <- df_rg %>% 
  select(data, terapia_intensiva, denominazione_regione) %>% 
  pivot_wider(id_cols = data, names_from = denominazione_regione, values_from = terapia_intensiva) 
df_rg_wider

# Quindi rimuovi la colonna data e trasponi le righe e le colonne

mat <- df_rg_wider %>% 
  select(-data) %>% 
  t()

# I dati sono tutti espressi nella stessa unità di misura, per cui non è necessario
# standardizzarli
# Applica il clustering gerarchico usando 
# * diversi linkage: complete, average e single
# * diverse distanze/dissimilarità: distanza euclidea e distanza basata sulla correlazione.


# Iniziamo con la distanza euclidea
 
euclid.d.matrix <- dist(mat)

hc_complete <- hclust(d = euclid.d.matrix, method = "complete")
hc_average <- hclust(d = euclid.d.matrix, method = "average")
hc_single <- hclust(d = euclid.d.matrix, method = "single")
k_means_3 <- kmeans(mat, centers = 3)

plot(hc_complete)
plot(hc_average)
plot(hc_single)

# Scegliamo il linkage complete con 3 cluster.
 
df_rg_cluster <- data.frame(cluster = cutree(hc_complete, k = 3)) %>% 
  rownames_to_column(var = "denominazione_regione")
df_rg_cluster

# Puoi  utilizzare la funzione inner_join() per unire il data frame df_rg_cluster con il data 
# frame iniziale df_rg, in modo da poter diagrammare i dati e colorarli in base al cluster 
# assegnato. 

inner_join(x = df_rg_cluster,
           y = select(df_rg, data, terapia_intensiva, totale_ospedalizzati, denominazione_regione),
           by = "denominazione_regione") %>% 
  mutate(cluster = factor(cluster)) %>%
  ggplot() +
  geom_line(mapping = aes(data, terapia_intensiva, group = denominazione_regione, colour = cluster)) +
  facet_wrap(~denominazione_regione)

# Quali conclusioni puoi trarre da questo grafico?
 
# * La Lombardia è un cluster a sé. Ha dei numeri di gran lunga maggiori rispetto a tutte 
#   le altre regioni
# * Il secondo cluster include regioni con un numero "intermedio" di persone in 
#   terapia intensiva (Emilia-Romagna, Lazio, Piemonte, Toscana, Veneto)
# * Il terzo cluster include le  regioni che, rispetto alle altre, hanno avuto meno casi.


# Ripeti le stesse analisi fatte ma considerando la distanza basata sulla correlazione


# La distanza euclidea consente di raggruppare le regioni per numero di casi complessivo. 
# Potresti invece essere interessato a raggruppare le regioni in base all'andamento del
# numero di persone in terapia intensiva, indipendentemente dai numeri in assoluto. 
# Prova quindi ad usare come misura di dissimilarità la distanza basata sulla correlazione.
# Suggerimento: le osservazioni sono le diverse regioni e le variabili sono i numeri di casi 
# giornalieri. 
# Puoi usare la funzione corr.d.matrix.fun definita nel corso

corr.d.matrix.fun <- function(x) as.dist(1-cor(x %>% as.matrix() %>% t()))
corr.d.matrix <- corr.d.matrix.fun(mat)

# Applica  nuovamente il clustering gerarchico, con linkage "method=complete". 
# Assegna il risultato alla variabile hc_complete

hc_complete <- hclust(d = corr.d.matrix, method = "complete")
plot(hc_complete)


# Puoi tagliare a k=2 ed ottenere un oggetto di tipo data frame.

data.frame(cluster = cutree(hc_complete, k = 2))


# Rinomina la prima colonna, rendi la colonna cluster di tipo factor e registra 
# il data frame nella variabile df_rg_cluster.

df_rg_cluster <- data.frame(cluster = cutree(hc_complete, k = 2)) %>% 
  rownames_to_column(var = "denominazione_regione") %>% 
  mutate(cluster = factor(cluster))
df_rg_cluster


# Attraverso un inner_join su denominazione_regione, associare il cluster df_rg_cluster 
# alle colonne di interesse  dal dataset iniziale df_rg, selezionate attraverso 
# select(data, terapia_intensiva, totale_ospedalizzati, denominazione_regione)

inner_join(x = df_rg_cluster,
           y = df_rg %>% select(data, terapia_intensiva, totale_ospedalizzati, denominazione_regione),
           by = "denominazione_regione")

# e visualizza un grafico mediante %>% ggplot() colorato per cluster e visualizzato in 
# pannelli per regione con facet_wrap(~denominazione_regione)

inner_join(x = df_rg_cluster,
           y = df_rg %>% select(data, terapia_intensiva, totale_ospedalizzati, denominazione_regione),
           by = "denominazione_regione")%>%
  ggplot() +
  geom_line(mapping = aes(data, terapia_intensiva, colour = cluster)) +
  facet_wrap(~denominazione_regione)

 
# I raggruppamenti risultano diversi rispetto al caso precedente. 
# Per agevolare la visualizzazione dei risultati, potresti utilizzare l'argomento 
# scales = "free_y"` e permettere che la scala sull'asse y possa cambiare tra le regioni.

inner_join(x = df_rg_cluster,
           y = select(df_rg, data, terapia_intensiva, totale_ospedalizzati, denominazione_regione),
           by = "denominazione_regione") %>% 
  mutate(cluster = factor(cluster)) %>%
  ggplot() +
  geom_line(mapping = aes(data, terapia_intensiva, colour = cluster)) +
  facet_wrap(~denominazione_regione, scales = "free_y")

# Che cosa puoi osservare?

# In base all'analisi effettuate puoi osservare che:
# * Il primo cluster raggruppa regioni in cui la seconda ondata non è stata 
#   peggiore della prima.
# * Il secondo cluster raggruppa regioni in cui la seconda ondata 
#   è peggiore della prima.
# 


# Ripeti le stesse analisi impostando k=3----

df_rg_cluster <- data.frame(cluster = cutree(hc_complete, k = 3)) %>% 
  rownames_to_column(var = "denominazione_regione") %>% 
  mutate(cluster = factor(cluster))

plot(hc_complete)

inner_join(x = df_rg_cluster,
           y = select(df_rg, data, terapia_intensiva, totale_ospedalizzati, denominazione_regione),
           by = "denominazione_regione") %>% 
  mutate(cluster = factor(cluster)) %>%
  ggplot() +
  geom_line(mapping = aes(data, terapia_intensiva, colour = cluster)) +
  facet_wrap(~denominazione_regione, scales = "free_y")

# Che cosa cambia rispetto al caso con k = 2?

# ottieni uno split del secondo cluster del caso precedente (k = 2), 
# che si differenziano tra loro (osserva i cluster 2 e 3, in verde ed azzurro, rispettivamente) 
# per una differente pendenza nella risalita del numero di pazienti in terapia intensiva  
# nella seconda ondata. Il secondo picco per tutte le curve del cluster 2 e 3 
# si attesta agli inizi di novembre, ma le curve/regioni appartenenti al cluster 2 hanno 
# un leggero ritardo nell'inizio della seconda ondata.
# Non ci siamo soffermati a notare la differente scala delle y, in quanto il raggruppamento ottenuto 
# è stato basato sulla distanza basata sulla correlazione, che mira proprio a giudicare simili
# osservazioni/cluster a prescindere dalla scala.

# Complimenti! Hai completato con successo il Lab RStudio relativo al clustering. 
# Sei pronto per autovalutarti, mettendoti alla prova con i quiz di fine lezione!


