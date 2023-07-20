# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione --------------------------------------------------------

dev.off()        # Chiude tutti i grafici
rm(list = ls())  # Rimuove tutte le variabili salvate nell'environment
cat("\f")        # Pulisce la console


# Librerie ----------------------------------------------------------------

library(tidyverse)
library(GGally)
library(lattice)
library(rgl)
library(leaps)
library(glmnet)
library(pls)
library(PerformanceAnalytics)


# Data set mtcars -------------------------------------------------

# Il data set mtcars viene introdotto per descrivere il legame tra il consumo di 
# carburante contenuto nella variabile mpg
# ed altre variabili che descrivono le performance ed il design di 32 differenti 
# modelli di automobili vendute negli anni 1973-1974.

# Carica il data set 

data(mtcars)

# Visualizzane la descrizione
?mtcars

# Utilizza quanto imparato fino ad ora per controllare la qualità dei dati.
# Ad esempio, ci sono valori mancanti?

sum(is.na(mtcars))
# Non c'è nessun valore NA

# Esplora il contenuto del data set

summary(mtcars)  # Statistiche di riepilogo
str(mtcars)      # Struttura di ciascuna colonna
dim(mtcars)      # Numero di righe e colonne
names(mtcars)    # Nomi delle colonne
head(mtcars)     # Prime sei righe del data set
rownames(mtcars) # Nome delle righe

# Considera (per il momento) solo le variabili continue
        mtcars_cont <- mtcars %>%  select ("mpg", "disp", "hp", "drat", "wt", "qsec")
        plot(mtcars_cont)
        ggpairs(mtcars_cont)


# Cosa puoi osservare?

# Molte variabili sono fortemente correlate:
# - alcune positivamente
# - alcune negativamente

# Potresti impostare, a scopo comparativo, tante regressioni semplici tra mpg e le 
# altre variabili continue e visualizzare i risultati del summary

mod_simple1 <- lm(mpg ~ disp, data = mtcars_cont)
mod_simple2 <- lm(mpg ~ hp, data = mtcars_cont)
mod_simple3 <- lm(mpg ~ drat, data = mtcars_cont)
mod_simple4 <- lm(mpg ~ wt, data = mtcars_cont)
mod_simple5 <- lm(mpg ~ qsec, data = mtcars_cont)

summary.mod_simple1<- summary(mod_simple1)
summary.mod_simple2<- summary(mod_simple2)
summary.mod_simple3<- summary(mod_simple3)
summary.mod_simple4<- summary(mod_simple4)
summary.mod_simple5<- summary(mod_simple5)

summary.mod_simple1
summary.mod_simple2
summary.mod_simple3
summary.mod_simple4
summary.mod_simple5

# Quali variabili superano il test di dipendenza lineare con mpg?

# Tutti i modelli di regressione semplice risultano significativi, 
# superando il test di dipendenza lineare

# Modello di regressione lineare multipla completo ----------------------------------------------------------

# Stima un modello di regressione lineare multipla tra il consumo di carburante e 
# tutte le altre variabili  continue e salva i risultati del modello nella variabile 
# mod_multiple  

mod_multiple <- lm(mpg ~ ., mtcars_cont)
summary.mod_multiple <- summary(mod_multiple)

        
# Che cosa puoi osservare dalla tabella di riepilogo?

# Nel modello con tutte le covariate l'unica variabile a superare il test di 
# dipendenza lineare è la variabile wt, che tra l'altro è quella con il più alto 
# coefficiente di correlazione campionaria con mpg.
# Come abbiamo osservato nella lezione sulla regressione lineare multipla
# un predittore ritenuto significativo nella regressione semplice, potrebbe non 
# esserlo in un modello di regressione multipla  quando viene considerato insieme 
# ad altri predittori.
# Inoltre, le stime e i test di dipendenza lineare sui singoli predittori 
# possano cambiare esito passando ad un modello con un insieme diverso di covariate. 
# Quindi può esistere un modello con un numero di covariate minore di p=5 come quello contenuto in mod_multiple
# in cui risulti significativo il contributo di più di un pedittore.

# Esplora per esempio il modello di regressione multipla mpg ~ hp+wt e salva i 
# risultati del modello nella variabile mod_multiple2

mod_multiple2<- lm(mpg ~ hp+wt, mtcars_cont)
        summary.mod_multiple2 <- summary(mod_multiple2)
        summary.mod_multiple2

# Cosa osservi?

# Entrambi i p-value relativi ai test di linearità sui singoli regressori sono 
# minori di 0.05/2=0.025 (correzione di Bonferroni) e quindi risultano significativi 
# e migliorano il modello di previsione.
# Nella prossima lezione vedrai che per stabilire un criterio di confronto tra i 
# diversi modelli può essere considerato il valore di Adjusted R-squared. 
# Verrà preferito il modello con Adjusted R-squared maggiore.

# Il valore di Adjusted R-squared del mod_multiple_2 è pari a 

summary.mod_multiple2$adj.r.squared

# Confrontalo con quello del modello di regressione semplice mpg ~ wt e del modello
# mod_multiple. Cosa puoi concludere?

# È di gran lunga migliore di quello ottenuto con il modello di regressione semplice 
# mpg ~ wt
summary.mod_simple4$adj.r.squared
#ed è comparabile con quello completo
summary.mod_multiple$adj.r.squared


# Confronta i tre modelli mpg ~ wt, mpg ~ hp+wt, mpg ~ . ----------------------

# Assegna la stima del coefficiente di wt nel modello di regressione semplice alla 
# variabile beta_wt_simple

beta_wt_simple <- mod_simple4$coefficients[[2]]
beta_wt_simple

# Assegna la stima del coefficiente di wt nel modello di regressione multipla 
# mpg ~ hp+wt alla variabile beta_wt_multiple2

beta_wt_multiple2 <- mod_multiple2$coefficients[[3]]
beta_wt_multiple2

# Assegna la stima del coefficiente di wt nel modello di regressione multipla 
# completo alla variabile beta_wt_multiple

beta_wt_multiple <- mod_multiple$coefficients[[5]]
beta_wt_multiple 

# Calcola deviazione standard dello stimatore del coefficiente di wt
# nel modello di regressione semplice mpg ~ wt ed assegna il valore alla variabile 
# beta_wt_simple

sigma_beta_wt_simple <- summary(mod_simple4)$coefficients[, "Std. Error"][[2]]
sigma_beta_wt_simple  %>% round(3)

# nel modello di regressione multipla mpg ~ hp+wt alla variabile beta_wt_multiple2

sigma_beta_wt_multiple2 <- summary(mod_multiple2)$coefficients[, "Std. Error"][[3]]
sigma_beta_wt_multiple2  %>% round(3)

# nel modello di regressione multipla completo ed assegna il valore alla variabile 
# beta_wt_multiple

sigma_beta_wt_multiple <- summary(mod_multiple)$coefficients[, "Std. Error"][[5]]
sigma_beta_wt_multiple  %>% round(3)

# Calcola l'intervallo di confidenza per il coefficiente di regressione relativo 
# a wt 
# nel modello di regressione semplice mpg ~ wt
        
n <- nrow(mtcars_cont); n #ampiezza campionaria
alfa <- 0.05
df <- mod_simple4$df.residual; df 
t_0975 <- qt(1-alfa/2, df)
I_beta1_sup <- beta_wt_simple + t_0975 * sigma_beta_wt_simple #estremo superiore
I_beta1_inf <- beta_wt_simple - t_0975 * sigma_beta_wt_simple #estremo inferiore
c(I_beta1_inf,I_beta1_sup) %>% round(2) # Stampa l'intervallo
(I_beta1_sup-I_beta1_inf) %>% round(2) #Calcola l'ampiezza dell'intervallo

# nel modello di regressione multipla mpg ~ hp+wt
        
n <- nrow(mtcars_cont); n #ampiezza campionaria
alfa <- 0.05
df <- mod_multiple2$df.residual; df 
t_0975 <- qt(1-alfa/2, df)
I_beta1_sup <- beta_wt_multiple2 + t_0975 * sigma_beta_wt_multiple2 #estremo superiore
I_beta1_inf <- beta_wt_multiple2 - t_0975 * sigma_beta_wt_multiple2 #estremo inferiore
c(I_beta1_inf,I_beta1_sup) %>% round(2) #Stampa l'intervallo
(I_beta1_sup-I_beta1_inf) %>% round(2) #Calcola l'ampiezza dell'intervallo

# nel modello di regressione lineare multipla completo mpg ~ .
        
df <- mod_multiple$df.residual; df 
alfa <- 0.05
t_0975 <- qt(1-alfa/2, df)
I_beta1_sup <- beta_wt_multiple + t_0975 * sigma_beta_wt_multiple #estremo superiore
I_beta1_inf <- beta_wt_multiple - t_0975 * sigma_beta_wt_multiple #estremo inferiore
c(I_beta1_inf,I_beta1_sup) %>% round(2) # Stampa l'intervallo
(I_beta1_sup-I_beta1_inf) %>% round(2) # Calcola l'ampiezza dell'intervallo

# Calcola l'intervallo di confidenza al livello 0.95 della media  della variabile 
# di risposta per un valore delle covariate "disp=350", "hp=150", "drat=4", 
# "wt=5", "qsec=21"
# nel modello di regressione semplice mpg ~ wt
predict(mod_simple4, interval = "confidence",
                newdata = data.frame(wt=5))

# nel modello di regressione multipla mpg ~ hp+wt
predict(mod_multiple2, interval = "confidence",
                newdata = data.frame(hp=150, wt=4))

# nel modello di regressione lineare multipla completo mpg ~ .
predict(mod_multiple, interval = "confidence",
                newdata = data.frame(disp=350, hp=150, drat=4, wt=5, qsec=21))


# Calcola l'intervallo di previsione della variabile di risposta per un valore 
# delle covariate  "disp=350", "hp=150", "drat=4", "wt=5", "qsec=21"
# nel modello di regressione semplice mpg ~ wt

predict(mod_simple4, interval = "prediction",
                newdata = data.frame(wt=5))

# nel modello di regressione multipla mpg ~ hp+wt

predict(mod_multiple2, interval = "prediction",
                newdata = data.frame(hp=150, wt=4))

# nel modello di regressione lineare multipla completo mpg ~ .
predict(mod_multiple, interval = "prediction",
                newdata = data.frame(disp=350, hp=150, drat=4, wt=5, qsec=21))


# Grafici diagnostici---------------------------------------------------        

# Produci i seguenti grafici diagnostici del modello

# per il  modello di regressione semplice mpg ~ wt

plot(mod_simple4$fitted.values, mtcars_cont$mpg, 
        xlab = "Valori previsti dal modello", ylab = "Valori osservati")
abline(0,1, col=2)

# per il  modello di regressione multipla mpg ~ hp+wt

plot(mod_multiple2$fitted.values, mtcars_cont$mpg, 
     xlab = "Valori previsti dal modello", ylab = "Valori osservati")
abline(0,1, col=2)

# per il  modello di regressione multipla completo mpg ~ .

plot(mod_multiple$fitted.values, mtcars_cont$mpg, 
     xlab = "Valori previsti dal modello", ylab = "Valori osservati")
abline(0,1, col=2)

# Grafico dei residui
# per il  modello di regressione semplice mpg ~ wt

plot(mod_simple4$residuals, ylab = "Residui")

# per il  modello di regressione multipla mpg ~ hp+wt-

plot(mod_multiple2$residuals, ylab = "Residui")

# per il  modello di regressione multipla completo mpg ~ .
 
plot(mod_multiple$residuals, ylab = "Residui")

# Grafico dei residui studentizzati
# per il  modello di regressione semplice mpg ~ wt

stud_res <- rstandard(mod_simple4)
plot(stud_res, ylab = "Residui studentizzati")

# per il  modello di regressione multipla mpg ~ hp+wt

stud_res <- rstandard(mod_multiple2)
plot(stud_res, ylab = "Residui studentizzati")
# per il  modello di regressione multipla completo mpg ~ .---

stud_res <- rstandard(mod_multiple)
plot(stud_res, ylab = "Residui studentizzati")

# Residui vs covariate e y predetto
# per il  modello di regressione semplice mpg ~ wt

par(mar=c(2,2,2,2)) # per modificare i margini tra i grafici
par(mfrow=c(2,1))
plot(mtcars$wt,mod_simple4$residuals)
plot(mod_simple4$fitted.values,mod_simple4$residuals)

# per il  modello di regressione multipla mpg ~ hp+wt

par(mfrow=c(3,1))
plot(mtcars$wt,mod_multiple2$residuals)
plot(mtcars$hp,mod_multiple2$residuals)
plot(mod_multiple2$fitted.values,mod_multiple2$residuals)

# per il  modello di regressione multipla completo mpg ~ .
# disp=350, hp=150, drat=4, wt=5, qsec=21)

par(mfrow = c(3, 2))
plot(mtcars$disp, mod_multiple$residuals)
plot(mtcars$hp, mod_multiple$residuals)
plot(mtcars$drat, mod_multiple$residuals)
plot(mtcars$wt, mod_multiple$residuals)
plot(mtcars$qsec, mod_multiple$residuals)
plot(mod_multiple$fitted.values, mod_multiple$residuals)
dev.off()

# Ripeti le stesse analisi su tutto il data set includendo anche le variabili 
# discrete "cyl" "vs" "am" "gear" "carb"

mod_multiple_full <- lm(mpg ~ ., mtcars)
summary.mod_multiple <- summary(mod_multiple_full)
summary.mod_multiple
        
# In ogni caso, riprenderemo questo data set nel Lab Rstudio di fine week, dopo 
# averti illustrato dei metodi che riescono a selezionare il miglior sottinsieme di 
# variabili da includere in modello di regressione lineare e criteri di confronto 
# per valutare la capacità descrittiva e predittiva dei diversi modelli
# Prenditi una breve pausa, e passa alla lezione successiva. Ti aspettiamo!