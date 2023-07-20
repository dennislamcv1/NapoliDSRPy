# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione 
rm(list = ls()) # Rimuove tutte le variabili
dev.off() # Chiude tutti i grafici
cat("\f") # Pulisce la console
cat("\014") # Pulisce la console

# Importa librerie e dati

library(tidyverse)
library(nycflights13)
data(flights)

# Trova tutti i voli che: 

# - hanno un ritardo all'arrivo pari a tre ore

filter(flights, arr_delay == 180)

# - sono partiti in primavera (Marzo, Aprile e Maggio)

filter(flights, month >= 3, month <= 5)
filter(flights, month >= 3 & month <= 5)
filter(flights, month %in% 3:5)

# - sono arrivati con più di tre ore di ritardo e che sono partiti in ritardo

filter(flights, arr_delay > 180 & dep_delay > 0)

# - sono stati ritardati di almeno un'ora, ma che hanno recuperato almeno 10 minuti di ritardo

filter(flights, dep_delay >= 60 & dep_delay - arr_delay > 10)
flights %>% 
  mutate(delta_ritardo = dep_delay - arr_delay) %>% 
  filter(dep_delay >= 60 & delta_ritardo > 10) %>%
  select(delta_ritardo, dep_delay, arr_delay, everything())

# - sono partiti tra la mezzanotte e mezzogiorno (incluse)
#     (NOTA: dep_time ha un formato particolare, ad esempio
#      1351 è 13:51 o 1:51 pm, 600 è 06:00 am,
#      52 è 00:52 am, 1 è 00:01 am, etc.,
#      In generale sarebbe meglio convertire questa colonna nel formato date/time,  
#      puoi rispondere a questa domanda anche senza la conversione,
#      NOTA anche che 2400 è 00:00)

filter(flights, dep_time <= 1200 | dep_time == 2400)

# Un'altra utile funzione di dplyr per filtrare è between(). 
# Che cosa fa? 
# Puoi usarla per semplificare il codice per rispondere alla precedente domanda? 

filter(flights, between(month, 7, 9))
filter(flights, between(dep_time, 0, 1200) | dep_time == 2400)

# Quanti voli hanno un valore mancante in dep_time? 

flights %>% 
  filter(is.na(dep_time)) %>% 
  nrow

# Quali altre variabili sono mancanti? 
# (puoi anche controllare visivamente)
# (o puoi usare la funzione summary sul data frame filtrato)

flights %>% 
  filter(is.na(dep_time)) %>% 
  summary

# Ordina i voli per trovare quelli più in ritardo all'arrivo
  
flights %>% 
  arrange(desc(arr_delay))

# Trova i voli che sono partiti prima
  
flights %>% 
  arrange(dep_delay)

# Ordina i voli per trovare quelli più lenti (in termini di velocità)
  
flights %>% 
  arrange(desc(air_time)) %>% 
  select(air_time, everything())

# Quali voli hanno viaggiato più lontano? 
  
flights %>% 
  arrange(desc(distance)) %>% 
  select(distance, everything())


# Pensa a quanti più modi possibili per selezionare 
# arr_delay e arr_time e da flights.

select(flights, c(7, 9))
select(flights, arr_time, arr_delay)
select(flights, starts_with("arr_"))
select(flights, (ends_with("_delay") | ends_with("_time")) & starts_with("arr"))


# Quale compagnia aerea (carrier) ha il ritardo peggiore? 

flights %>% 
  group_by(carrier) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(delay))


# Quale piano di volo (tailnum) è il meno puntuale?

flights %>% 
  group_by(tailnum) %>% 
  summarise(n = n(),
            ontime_rate = mean(arr_delay <= 0, na.rm = TRUE),
            tot_delay = sum(arr_delay[arr_delay > 0], na.rm = TRUE)) %>% 
  arrange(ontime_rate)

# Visualizza i piani di volo che hanno troppi pochi viaggi

flights %>% 
  count(tailnum) %>% 
  summary


# A che ora del giorno dovresti viaggiare per evitare quanto più possibile ritardi?

flights %>% 
  group_by(hour) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(arr_delay)

# RISPOSTA: la mattina presto

# Per ciascuna destinazione, calcola il numero totale di minuti di ritardo. 
# Per ciascun volo, calcola la proporzione del ritardo totale per la sua destinazione.
# Suggerimento: questo richiede l'uso della funzione  mutate dopo group_by invece di summarise.
# Controlla la differenza tra l'uso di mutate e group_by dopo summarise!

flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  mutate(tot_arr_delay = sum(arr_delay, na.rm = TRUE),
         prop_arr_delay = arr_delay / tot_arr_delay) %>% 
  arrange(dest, desc(prop_arr_delay)) %>% 
  select(dest, prop_arr_delay, tot_arr_delay, everything())


# Trova tutte le destinazioni coperte da almeno due compagnie aeree. 
# Poi, ordina le compagnie in base al numero di destinazioni coperte tra queste.

flights %>% 
  group_by(dest) %>% 
  mutate(n_carriers = n_distinct(carrier)) %>% 
  filter(n_carriers >= 2) %>% 
  group_by(carrier) %>% 
  summarise(n_dest = n_distinct(dest)) %>% 
  arrange(desc(n_dest))

