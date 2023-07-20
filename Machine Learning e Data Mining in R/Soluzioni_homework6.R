# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.

# Inizializzazione --------------------------------------------------------

dev.off()        # Chiude tutti i grafici
rm(list = ls())  # Rimuove tutte le variabili salvate nell'environment
cat("\f")        # Pulisce la console

# Regressione lineare semplice --------------------------------------------

# Questo è un nuovo esempio. È noto che la resistenza a rottura (N/mm^2) del 
# calcestruzzo dipende dalla sua densità (kg/m^3). Tant’è che per realizzare buoni 
# calcestruzzi vengono impiegati additivi fluidificanti che, riducendo del 20-30% la 
# quantità d’acqua necessaria per l’impasto, ne riducono la porosità migliorandone la
# densità. Pur esistendo, questo legame funzionale non è completamente noto. Tuttavia,
# allorché non sia possibile estrarre provini idonei per le prove meccaniche, 
# esso costituisce l’unico strumento disponibile per esprimere un parere sull’idoneità
# di un determinato calcestruzzo già da tempo in opera. Ciò premesso, siamo riusciti 
# a procurarci ben 12 (ed è tanto!) coppie di misure sperimentali (resistenza a rottura
# e densità) su altrettanti provini di calcestruzzo di un fabbricato per civili 
# abitazioni crollato a tre anni dalla sua sopraelevazione di ulteriori due piani.


# Carica i nuovi dati e ripeti i calcoli già svolti adattando i nomi alle nuove 
# variabili in gioco.
 
# Lettura dati -----------------------------------------------------------
library(tidyverse)
calcestruzzo_df <- read.csv("data/calcestruzzo.csv")

# Esplora i dati ----------------------------------------------------------

plot(calcestruzzo_df, ylim=c(0,60))
head(calcestruzzo_df)
str(calcestruzzo_df)

# Modello di regressione lineare ------------------------------------------

# Stima il modello con lm() e salva i risultati nella variabile modello_regressione
x_densita <- calcestruzzo_df$densita
y_resistenza <- calcestruzzo_df$resistenza

modello_regressione <- lm(resistenza ~ densita , data=calcestruzzo_df)
class(modello_regressione)

# Salva i coefficienti del modello nelle variabili beta0_hat e beta1_hat

modello_regressione$coefficients
beta0_hat <- modello_regressione$coefficients[[1]]
beta1_hat <-modello_regressione$coefficients[[2]]




# Riproduci un grafico a dispersione tra la resistenza e la densità e traccia la 
#retta di regressione

abline(a = beta0_hat, # intercetta
       b = beta1_hat, # coefficiente angolare
       col = "blue") 

# Bande di confidenza e previsione -------------------------------------

# Considera nuove osservazioni
x_densita_new <- seq(from = min(x_densita) - 20, 
                     to = max(x_densita) + 20, length = 100)
y_hat_new <- beta1_hat * x_densita_new + beta0_hat

# Calcola le bande di confidenza per la media della funzione di risposta ad un 
# livello di fiducia pari a 0.95
conf_interval <- predict(modello_regressione, interval = "confidence",
                         newdata = data.frame(densita = x_densita_new)) 
lines(x_densita_new, conf_interval[, 2], lty = 2, col = "green")
lines(x_densita_new, conf_interval[, 3], lty = 2, col = "green")

# Se diminuissi il livello di fiducia pari a 0.90, le bande di confidenza si 
# restringerebbero o allargherebbero intorno alla retta di regressione?

conf_interval_99 <- predict(modello_regressione, interval = "confidence", level=0.90,
                         newdata = data.frame(densita = x_densita_new)) 
lines(x_densita_new, conf_interval_99[, 2], lty = 3, col = "blue")
lines(x_densita_new, conf_interval_99[, 3], lty = 3, col = "blue")
# Si restringerebbero

# Calcola le Bande di previsione per un valore futuro della funzione di risposta 
# ad un livello di fiducia pari a 0.95
pred_interval <- predict(modello_regressione, interval = "prediction",
                         newdata = data.frame(densita = x_densita_new))
lines(x_densita_new, pred_interval[, 2], lty = 2, col = "red")
lines(x_densita_new, pred_interval[, 3], lty = 2, col = "red")

# Calcola le bande di previsione per un valore futuro della funzione di risposta 
# ad un livello di fiducia pari a 0.90

pred_interval <- predict(modello_regressione, interval = "prediction", level=0.90,
                         newdata = data.frame(densita = x_densita_new))
lines(x_densita_new, pred_interval[, 2], lty = 3, col = "red")
lines(x_densita_new, pred_interval[, 3], lty = 3, col = "red")


# Problema ----------------------------------------------------------------

# Il giudice ha posto, tra i numerosi altri, il seguente specifico quesito: 
# “... Accerti il CTU (consulente tecnico d’ufficio, N.d.R.) se vi siano concause 
# diverse dalla mancanza di tenuta del calcestruzzo e determini la fondatezza del 
# valore di resistenza di 20 N/mm^2, assunto dal progettista come dato di progetto 
# per la sopraelevazione, sulla base del valore di densità di 2202 kg/m^3 misurato 
# sull’opera preesistente.”

# Per dare risposta al quesito posto dal giudice è necessario prima di tutto un
# formale test di dipendenza lineare.
# Puoi procedere calcolando o intervallo di confidenza (al livello di fiducia 0.95) 
# per il parametro b = beta1_hat

df=modello_regressione$df.residual
alfa <- 0.05
t_0975 <- qt(1-alfa/2, df)
sigma_hat_beta1 <- summary(modello_regressione)$coefficients[, "Std. Error"][[2]]
I_beta1_sup <- beta1_hat + t_0975 * sigma_hat_beta1 #estremo superiore
I_beta1_inf <- beta1_hat - t_0975 * sigma_hat_beta1 #estremo inferiore
c(I_beta1_inf,I_beta1_sup) %>% round(2)

# C'è evidenza sperimentale di un legame lineare tra variabile risposta e covariata?
# Se sì, perché?

# Si perché l'intervallo non contiene lo 0.

# o usando il p-value del regressore/covariata x_densità
summary(modello_regressione)
summary(modello_regressione)$coefficients[, "Pr(>|t|)"][[2]]
# In questo caso come faresti a stabilire se c'è evidenza sperimentale di un legame 
# lineare tra variabile risposta e covariata?

# Bisogna verificare che il p-value sia inferiore ad alfa=0.05

# Calcola il coefficiente di determinazione (R_squared)

cor(y_resistenza,x_densita)^2
reg.summary <- summary(modello_regressione)
reg.summary$r.squared

# Calcola il coefficiente Adjusted R-squared

1-(reg.summary$sigma/sd(y_resistenza))^2
reg.summary$adj.r.squared

# Cosa puoi osservare?

# Possiamo affermare che la regressione, ossia il modello lineare tra resistenza e 
# densità, spiega più del 55% della varianza iniziale della resistenza

# Per rispondere al giudice bisogna valutare (già possiamo farlo graficamente) se
# il valore della variabile di risposta resistenza di 20 N/mm^2 sia plausibile 
# in corrispondenza di una densità pari a 2202 kg/m^3

# La stima per la media e per un valore futuro della resistenza è pari

predict(modello_regressione,
newdata = data.frame(densita = 2202))

# Che conclusione puoi trarre?

# Anche se tale valore è al di sopra di quello indicato dal progettista, bisogna
# mettere in guardia il giudice che sulla base dei dati sperimentali in nostro 
# possesso, l'incertezza associata a tale stima è pari con una confidenza del 95% 
# a una forchetta di valori

predict(
  modello_regressione,
  interval = "prediction",
  newdata = data.frame(densita = 2202)
)

# Volendo essere più conservativi potremmo consigliare di richiedere un valore di 
# densità maggiore di 2202 kg/m^3.
# Per esempio se richiedessimo un valore di densità pari a 2276 kg/m^3 otterremo 

predict(modello_regressione, interval = "prediction", level=0.90,
        newdata = data.frame(densita = 2276))

# che puoi visualizzare sul grafico 
lwr <- predict(modello_regressione, interval = "prediction", level=0.90,
               newdata = data.frame(densita = 2276))[2]
segments (2276, -10, 2276, lwr,  lty = 2, col="grey")
segments (0, lwr, 2276, lwr,  lty = 2, col="grey")
points(2276, lwr, col = "red", pch = 16)
axis(side=1, at=2276, labels = TRUE)
text(2276,lwr, round(lwr,1), pos=3)
# ossia otterremo un valore di resistenza per una futura realizzazione 
# con un rischio del solo 5% che sia minore di 20 N/mm^2.