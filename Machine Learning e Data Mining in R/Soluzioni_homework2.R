# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione --------------------------------------------------------
rm(list = ls()) # Rimuove tutte le variabili
dev.off() # Chiude tutti i grafici
cat("\f") # Pulisce la console
cat("\014") # Pulisce la console

# Data visualization: Elementi di base ------------------------------------

# Esegui questa linea di codice per generare numeri casuali 
set.seed(0)
long_vec <- rnorm(1000)

# Plotta i dati
plot(long_vec)

# Come puoi vedere, la maggior parte dei numeri è tra -3 e 3.
# Adesso che il numero di punti è molto più grande 
# di quelli del vettore num_vec,
# vogliamo essere in grado di estrarre informazioni rilevanti dai dati
#
#
# Produci un istogramma del vettore long_vec in modo da avere
# un'idea della distribuzione dei dati
hist(long_vec)

# Produci un istogramma dei soli valori positivi di long_vec
hist(long_vec[long_vec > 0])

# Per piacere, cerca di dire quali sono le differenze nella forma tra i due istogrammi 
# Il primo presenta una distribuzione simmetrica a differenza del secondo istogramma
# Il secondo assume solo valori positivi a differenza del primo istogramma

# Visualizza il boxplot di long_vec
boxplot(long_vec)

# Osservando l'istogramma e il boxplot, 
# pensi che i dati vengano da una distribuzione simmetrica?
# Sì

# Produci diverse statistiche sintetiche di long_vec utilizzando la funzione summary
summary(long_vec)

# Qual è il valore mediano di long_vec?
# -0.05887

# Estrai solamente gli elementi di long_vec in posizioni pari.
# Prima di fare ciò, crea un vettore idx di indici c(2, 4, 8, ..., 998, 1000),
# poi digita long_vec[idx]
# Nota: quando hai estratto gli elementi dispari da num_vec 
# probabilmente lo hai semplicemente fatto definendo il vettore c(1, 3, 5),
# adesso non è più possibile scrivere l'intera sequenza 
# c(2, 4, 8, ..., 998, 1000)
# come si può definire il vettore idx?
# Ci sono molti modi possibili per farlo
# per esempio, usa la funzione seq
idx <- seq(from = 2, to = 1000, by = 2)
idx <- seq(from = 2, to = 1000, length = 1000 / 2)
idx <- (1:(1000 / 2)) * 2

long_vec[idx]

# calcola la differenza nella media campionaria tra gli elementi di long_vec in posizione pari e dispari
mean(long_vec[- idx]) - mean(long_vec[idx])
mean(long_vec[- idx] - long_vec[idx])

# la media degli elementi dispari è più grande di quelli pari del vettore long_vec? 
# Sì/no?
# No

# Data visualization: ggplot ----------------------------------------------

# Si vogliono riprodurre alcuni grafici mostrati in un recente report sul COVID-19 su sky tg24.
# LINK: https://tg24.sky.it/cronaca/2020/11/06/bollettino-coronavirus-italia-6-novembre
# Ovviamente, l'obiettivo sarà creare una versione più semplice di questi grafici.
# Inoltre, alcuni grafici che produrremo sono basati sul materiale  
# https://lab24.ilsole24ore.com/coronavirus/.
# 
# I dati del COVID-19 sono stati importati dall'archivio ufficiale della 
# "Protezione Civile" (https://github.com/pcm-dpc/COVID-19):
# * dati nazionali: https://github.com/pcm-dpc/COVID-19/blob/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv
# * dati regionali: https://github.com/pcm-dpc/COVID-19/blob/master/dati-regioni/dpc-covid19-ita-regioni.csv
# * dati provinciali: https://github.com/pcm-dpc/COVID-19/blob/master/dati-province/dpc-covid19-ita-province.csv
# 
# I tre file .csv sono presenti nella cartella data
# Per prima cosa si dovranno importare i dati.

library(tidyverse)
df_it <- read_csv("data/dpc-covid19-ita-andamento-nazionale.csv")
df_rg <- read_csv("data/dpc-covid19-ita-regioni.csv",
                  col_types = c("casi_testati" = "n",
                                "casi_da_sospetto_diagnostico" = "n",
                                "casi_da_screening" = "n",
                                "ingressi_terapia_intensiva" = "n",
                                "note_casi" = "c",
                                "note_test" = "c"))
df_pr <- read_csv("data/dpc-covid19-ita-province.csv",
                  col_types = c("note" = "c"))

# Si noti che è stato nessario aggiungere l'argomento col_types alla funzione read_csv, 
# perché read_csv identifica la tipologia dei dati osservando solamente le prime 1000 righe. 
# Tuttavia se ci sono tutti NA, il tipo di variabile potrebbe non essere riconosciuto 
# correttamente. In questo caso, la colonna "note" ha molti valori mancanti e read_csv 
# restituisce un warning. Pertanto si usa il comando col_types per specificare, ad esempio, 
# che la colonna note sia "c", ossia un vettore di caratteri (si veda ?read_csv) 

# Bisogna creare alcune nuove colonne.
# Si raccomanda l'uso di mutate e di aggiornare il dataset originale prima di proseguire 
# con l'analisi per evitare di ripetere le operazioni ogni volta.

# Ad esempio, non è disponibile il numero giornaliero di persone in terapia intensiva, 
# ma solamente i valori giornalieri cumulati. Lo stesso per il numero giornaliero di tamponi,
# di deceduti, di persone ricoverate in ospedale con sintomi, ma non in terapia intensiva e
# di persone ospedalizzate

# Nota che df_pr (dati per province) non contiene informazioni su test, deceduti e terapie 
# intensive.
# Suggerimento: usa la funzione diff per creare queste colonne, 
# ma nota: dato un vettore x di lunghezza n, diff(x) restituisce un vettore di lunghezza n-1. 

df_it <- df_it %>% 
  mutate(
    terapia_intensiva_giorno = c(terapia_intensiva[1], diff(terapia_intensiva)),
    nuovi_tamponi = c(tamponi[1], diff(tamponi)),
    deceduti_giorno = c(deceduti[1], diff(deceduti)),
    variazione_quotidiana_ricoverati = c(ricoverati_con_sintomi[1], diff(ricoverati_con_sintomi)),
    nuovi_ospedalizzati = c(totale_ospedalizzati[1], diff(totale_ospedalizzati))
    )

df_rg <- df_rg %>% 
  group_by(denominazione_regione) %>% 
  mutate(
    terapia_intensiva_giorno = c(terapia_intensiva[1], diff(terapia_intensiva)),
    nuovi_tamponi = c(tamponi[1], diff(tamponi)),
    deceduti_giorno = c(deceduti[1], diff(deceduti)),
    variazione_quotidiana_ricoverati = c(
      ricoverati_con_sintomi[1], diff(ricoverati_con_sintomi)),
    nuovi_ospedalizzati = c(totale_ospedalizzati[1], diff(totale_ospedalizzati))) %>% 
  ungroup


# Produrre il primo barplot con il numero di nuovi casi negli ultimi 30 giorni.
# Usa geom_col(mapping = aes(x = .., y = ..)) o geom_bar(mapping = aes(x = .., y = ..), 
# stat = "identity").
# Per riprodurre lo stesso colore rosso del grafico di sky, usa `fill="#c7232f"`.

df_it %>% 
  tail(30) %>% 
  ggplot(mapping = aes(data, nuovi_positivi)) + 
  geom_bar(fill = "#c7232f", stat = "identity")

# Produrre un altro barplot con il numero di test positivi (nuovi_tamponi)

df_it %>% 
  mutate(nuovi_tamponi = c(tamponi[1], diff(tamponi))) %>% 
  tail(30) %>% 
  ggplot(mapping = aes(x = data, y = nuovi_tamponi)) + 
  geom_bar(fill = "#818181", stat = "identity")

 
# Creare una nuova colonna con la percentuale di nuovi positivi sui nuovi test effettuati


df_it %>% 
  mutate(percentuale_positivi = nuovi_positivi / nuovi_tamponi * 100) %>% 
  tail(30) %>% 
  ggplot(mapping = aes(x = data, y = percentuale_positivi)) +
  geom_bar(fill = "#c7232f", stat = "identity")


# Creare il bar plot con il numero di deceduti al giorno

df_it %>% 
  tail(30) %>% 
  ggplot(mapping = aes(x = data, y = deceduti_giorno)) +
  geom_bar(fill = "black", stat = "identity")


# Creare un grafico che riporti il numero di nuove persone in terapia intensiva negli ultimi 18 giorni...
 
df_it %>% 
  tail(18) %>% 
  ggplot(mapping = aes(x = data, y = terapia_intensiva_giorno)) +
  geom_bar(fill = "#c7232f", stat = "identity")


# ...ed il numero totale di persone in terapia intensiva

df_it %>% 
  ggplot(mapping = aes(x = data, y = terapia_intensiva)) +
  geom_line(colour = "#c7232f", stat = "identity", size = 2)


# I prossimi grafici riprodurranno https://lab24.ilsole24ore.com/coronavirus/#
# Per inserire molte colonne, una sotto l'altra, è consigliato l'uso della funzione 
# pivot_longer presente nel pacchetto tidyr prima di usare ggplot

df_it %>% 
  select(data, totale_positivi, dimessi_guariti, deceduti, totale_casi) %>% 
  pivot_longer(- data, names_to = "tipo_grafico", values_to = "value")

df_it %>% 
  select(data, totale_positivi, dimessi_guariti, deceduti, totale_casi) %>% 
  pivot_longer(- data, names_to = "tipo_grafico", values_to = "value") %>% 
  ggplot + 
  geom_line(mapping = aes(x = data, y = value, colour = tipo_grafico, linetype = tipo_grafico))

 
# Riportare un grafico che visualizzi le cinque regioni con il maggior numero di casi 
# giornalieri.
# Nota: prima di tutto è consigliato creare un vettore top_5_reg con le cinque regioni 
# con il numero maggiore di casi giornalieri. Poi inserire le cinque regioni in facet_wrap, 
# in base all'ordine decrescente dei casi dell'ultimo giorno e usare 
# mutate(denominazione_regione = factor(denominazione_regione, levels = top_5_reg))
 
top_5_reg <- df_rg %>%
  filter(data == last(data)) %>% 
  arrange(desc(nuovi_positivi)) %>% 
  slice(1:5) %>% 
  pull(denominazione_regione)
df_rg %>% 
  filter(denominazione_regione %in% top_5_reg) %>% 
  mutate(denominazione_regione = factor( 
    denominazione_regione, levels = top_5_reg)) %>% 
  ggplot +
  geom_line(mapping = aes(x = data, y = nuovi_positivi, colour = denominazione_regione)) +
  facet_wrap(~ denominazione_regione, nrow= 1)


# Riportare un grafico che visualizzi i casi giornalieri nelle dieci province con 
# il numero maggiore di casi giornalieri
 
top_10_pr <- df_pr %>% 
  filter(data == last(data)) %>% 
  arrange(desc(totale_casi)) %>% 
  slice(1:10) %>% 
  pull(denominazione_provincia)
df_pr %>% 
  filter(denominazione_provincia %in% top_10_pr) %>% 
  mutate(denominazione_provincia = factor(denominazione_provincia, levels = top_10_pr)) %>% 
  ggplot +
  geom_line(mapping = aes(x = data, y = totale_casi, colour = denominazione_provincia))

# Produrre il grafico che fornisce informazioni sulle persone infette regione per regione 
# nell'ultimo giorno disponibile.
# Ordinare le regioni nel grafico, in ordine decrescente, di terapie intensive

df_rg %>% 
  filter(data == last(data)) %>% 
  select(denominazione_regione, terapia_intensiva, 
         ricoverati_con_sintomi, isolamento_domiciliare) %>% 
  arrange(terapia_intensiva) %>% 
  mutate(denominazione_regione = factor(denominazione_regione, 
                                        levels = denominazione_regione)) %>% 
  pivot_longer(- denominazione_regione, names_to = "tipo_grafico", values_to = "value") %>% 
  ggplot + 
  geom_bar(mapping = aes(x = value, y = denominazione_regione, fill = tipo_grafico), 
           stat = "identity") +
  facet_wrap(~ tipo_grafico, scales = "free_x")

# Riportare un grafico che visualizzi l'andamento totale delle terapie intensive in tutte 
# le regioni

df_rg %>% 
  ggplot +
  geom_line(mapping = aes(x = data, y = terapia_intensiva, colour = denominazione_regione))

# Produrre un grafico a dispersione (scatter plot).
# Riportare un grafico che visualizzi il numero giornaliero della variazione del numero di 
# persone ospedalizzate in funzione del numero di persone in terapia intensiva.
# Si consiglia l'uso di `geom_smooth` per aggiungere una funzione di smooth, e colorare i 
# punti in base alla colonna data.

df_it %>% 
  ggplot(mapping = aes(x = nuovi_ospedalizzati, y = terapia_intensiva_giorno,
                       colour = data)) + 
  geom_smooth() +
  geom_point()

# Infine, creare una nuova variabile che distingua le regioni collegate geograficamente 
# nel nord e nel sud Italia.
# Per fare ciò in modo semplice, si può usare la condizione lat < 43 per individuare le 
# regioni del sud e produrre i seguenti boxplot:

df_rg %>% 
  mutate(zona = ifelse(lat < 43, "sud", "nord")) %>% 
  ggplot(mapping = aes(x = terapia_intensiva, y = denominazione_regione, colour = zona)) +  
  geom_boxplot()
