# File delle soluzioni. Usalo solo per un confronto dopo esserti messo alla prova
# con il file homework.R nella cartella superiore.


# Inizializzazione --------------------------------------------------------

dev.off()        # Chiude tutti i grafici
rm(list = ls())  # Rimuove tutte le variabili salvate nell'environment
cat("\f")        # Pulisce la console


# Librerie ----------------------------------------------------------------

library(ggplot2)
library(GGally)
library(lattice)
library(rgl)
library(leaps)
library(glmnet)
library(pls)

# Data set mtcars -------------------------------------------------
# Carica il data set e visualizzane la descrizione

data(mtcars)
?mtcars

# Il data set mtcars, che hai già incontrato alla fine della scorsa lezione,
# è stato introdotto per descrivere il legame tra il consumo di carburante 
# contenuto nella variabile mpg che riporta il consumo specifico di ogni autovettura 
# in miglia per gallone (mpg) ed altre 10 variabili che descrivono le performance ed 
# il design di 32 differenti modelli di automobili vendute negli anni 1973-1974, 
# attraverso il modello di regressione lineare multipla.
# Lo utilizzeremo qui per confrontare tutti i diversi modelli di 
# regressione che hai imparato in questa lezione.

# Training set e test set -------------------------------

# Quando i data set da analizzare contengono molte osservazioni,
# il 60%/70%  (training set) vengono utilizzate per la stima del modello e dell'errore 
# in-sample e il rimanente (30/40%) (test set) per valutare l'errore out-of-sample.
# Tuttavia, quando ci sono poche osservazioni, come in questo caso,
# è consigliato utilizzarle tutte per formare il training set e usare il metodo di 
# validazione incrociata (cross-validation) per valutare l'errore out-of-sample.

# Nonostante ciò, ed ai soli fini didattici, dividi il data set mtcars in training set
# e test set:
# Training set: 22 osservazioni
# Test set: 10 osservazioni

# Training set e test set
set.seed(0)
train.obs <- sample(1:nrow(mtcars), size = 22)
train_df <- mtcars[train.obs, ]
test_df <- mtcars[- train.obs, ]

# Selezione del miglior sottoinsieme di variabili (subset selection) -------
# Sai già come stimare attraverso la funzione lm() un modello di regressione multipla 
# per la stima della variabile consumo espresso in miglia per gallone (mpg).
# Seleziona il miglior sottoinsieme di variabili, utilizzando la funzione 
# regsubsets ed il metodo = "exhaustive" (che è il metodo predefinito).
# Salva i risultati ottenuti nelle variabili
# m.bs e m.bs.summary 

m.bs <- regsubsets(mpg ~ ., data = train_df, nvmax = ncol(train_df) - 1)
m.bs.summary <- summary(m.bs)
m.bs.summary

# Quali sono i modelli migliori sulla base dell'errore in-sample?

par(mfrow = c(2,2))
plot(m.bs.summary$rss, xlab = "Numero di variabili", ylab = "RSS", 
     type = "l")
plot(m.bs.summary$adjr2, xlab = "Numero di variabili", ylab = "RSq corretto",
     type = "l")
m.bs.max.adjr2 <- which.max(m.bs.summary$adjr2)
points(m.bs.max.adjr2, m.bs.summary$adjr2[m.bs.max.adjr2], 
       col = "red", cex = 2, pch = 20)
plot(m.bs.summary$cp, xlab = "Numero di variabili", ylab = "Cp", type = 'l')
m.bs.min.cp <- which.min(m.bs.summary$cp)
points(m.bs.min.cp, m.bs.summary$cp[m.bs.min.cp], 
       col = "red", cex = 2, pch = 20)
m.bs.min.bic <- which.min(m.bs.summary$bic)
plot(m.bs.summary$bic,xlab="Numero di variabili",ylab="BIC",type='l')
points(m.bs.min.bic, m.bs.summary$bic[m.bs.min.bic], 
       col = "red", cex = 2, pch = 20)
# I modelli migliori e più parsimoniosi sono quelli con 3  variabili
# Seleziona il modello con 3 variabili, al fine di confrontarlo con gli altri
coef(m.bs, 3) # cyl, wt, e carb
m.bs.final <- lm(mpg ~ cyl + wt +  carb, train_df)
summary(m.bs.final)
plot(m.bs.final)


# Seleziona il miglior sottoinsieme di variabili, utilizzando la funzione 
# regsubsets ed il metodo = "forward". Salva i risultati nelle variabili
# m.fws e m.fws.summary come fatto in precedenza

m.fws <- regsubsets(mpg ~ ., data = train_df, nvmax = ncol(train_df) - 1, 
                    method = "forward")
m.fws.summary <- summary(m.fws)
m.fws.summary

# Quali sono i modelli migliori sulla base dell'errore in-sample?
# Salva il modello finale nella variabile m.fws.final

par(mfrow = c(2,2))
plot(m.fws.summary$rss, xlab = "Numero di variabili", ylab = "RSS", 
     type = "l")
plot(m.fws.summary$adjr2, xlab = "Numero di variabili", ylab = "RSq corretto",
     type = "l")
m.fws.max.adjr2 <- which.max(m.fws.summary$adjr2)
points(m.fws.max.adjr2, m.fws.summary$adjr2[m.fws.max.adjr2], 
       col = "red", cex = 2, pch = 20)
plot(m.fws.summary$cp, xlab = "Numero di variabili", ylab = "Cp", type = 'l')
m.fws.min.cp <- which.min(m.fws.summary$cp)
points(m.fws.min.cp, m.fws.summary$cp[m.fws.min.cp], 
       col = "red", cex = 2, pch = 20)
m.fws.min.bic <- which.min(m.fws.summary$bic)
plot(m.fws.summary$bic,xlab="Numero di variabili",ylab="BIC",type='l')
points(m.fws.min.bic, m.fws.summary$bic[m.fws.min.bic], 
       col = "red", cex = 2, pch = 20)

# Con la selezione forward stepwise, scegliamo 4 variabili.
coef(m.fws, id = 4) # wt, carb, am, qsec
m.fws.final <- lm(mpg ~ wt + carb + am + qsec, data = train_df)
summary(m.fws.final)
plot(m.fws.final)

# Seleziona il miglior sottoinsieme di variabili, utilizzando la funzione 
# regsubsets ed il metodo = "backward". Salva i risultati nelle variabili
# m.bws e m.bws.summary come fatto in precedenza

m.bws <- regsubsets(mpg ~ ., data = train_df, nvmax = ncol(train_df) - 1,
                    method = "backward")
m.bws.summary <- summary(m.bws)
m.bws.summary

# Quali sono i  modelli migliori basati sull'errore in-sample?

par(mfrow = c(2,2))
plot(m.bws.summary$rss, xlab = "Numero di variabili", ylab = "RSS", 
     type = "l")
plot(m.bws.summary$adjr2, xlab = "Numero di variabili", ylab = "RSq corretto",
     type = "l")
m.bws.max.adjr2 <- which.max(m.bws.summary$adjr2)
points(m.bws.max.adjr2, m.bws.summary$adjr2[m.bws.max.adjr2], 
       col = "red", cex = 2, pch = 20)
plot(m.bws.summary$cp, xlab = "Numero di variabili", ylab = "Cp", type = 'l')
m.bws.min.cp <- which.min(m.bws.summary$cp)
points(m.bws.min.cp, m.bws.summary$cp[m.bws.min.cp], 
       col = "red", cex = 2, pch = 20)
m.bws.min.bic <- which.min(m.bws.summary$bic)
plot(m.bws.summary$bic,xlab="Numero di variabili",ylab="BIC",type='l')
points(m.bws.min.bic, m.bws.summary$bic[m.bws.min.bic], 
       col = "red", cex = 2, pch = 20)

# Con la selezione backward stepwise, scegliamo 4 variabili.
coef(m.bws, id = 4) # wt, carb, am, qsec
# Coincide con il modello m.fws
m.bws.final <- lm(mpg ~ wt + carb + am + qsec, data = train_df)
summary(m.bws.final)
plot(m.bws.final)

# Regressione Ridge -------------------------------------------------------

# Le seguenti considerazioni si riferiscono sia alla regressione ridge che lasso. 
# Nota: ti può essere utile consultare la guida in linea ?glmnet.
# Ricorda che la funzione glmnet non richiede la sintassi y ~ x1 + x2...
# come primo argomento. Invece della formula, bisogna fornire due argomenti:
# 1) il primo è la matrice del modello X (senza l'intercetta)
#           * Nota che la matrice del modello può essere ottenuta usando la 
#             funzione model.matrix(). Il suo argomento deve essere la solita formula
#             y ~ x1 + x2...
#           * model.matrix() converte anche le variabili factor in binarie. 
#             È necessario farlo perché glmnet richiede che X sia una matrice numerica
#           * Nota che non devi fornire l'intercetta: rimuovila usando [,-1]
# 2) il secondo è il vettore della variabile di risposta y
# Puoi anche fornire ulteriori argomenti
# 3) alpha = 0 per ridge, alpha = 1 per lasso
# 4) lambda, una sequenza di valori per il parametro di penalità

X <- model.matrix(mpg ~ ., train_df)[, - 1]
y <- train_df$mpg
grid <- 10 ^ seq(10, - 2, length = 100)
m.ridge <- glmnet(X, y, alpha = 0, lambda = grid)

# Dobbiamo scegliere il lambda ottimale.
# Usiamo la cross validazione (CV) con la funzione cv.glmnet.
# cv.glmnet: di default considera 10 parti (fold). Usa l'argomento nfolds per 
# selezionare il numero di fold che ritieni opportuno.
# Nel nostro caso, abbiamo solo 22 osservazioni nel training set.
# Non possiamo usare  10 fold, perché avremmo meno di 3 osservazioni per gruppo.
# Con poche osservazioni, è preferibile una validazione incrociata leave-one-out,
# ossia il numero di fold è uguale al numero di osservazioni (22).

par(mfrow = c(1, 1))
set.seed(1)
cv.ridge <- cv.glmnet(X, y, alpha = 0, nfolds = nrow(train_df))
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge # Il migliore lambda
plot(cv.ridge)
ridge.coef <- predict(m.ridge, type = "coefficients", s = bestlam.ridge)
ridge.coef


# Regressione Lasso -------------------------------------------------------

# Adesso, ripeti quanto fatto in precedenza per la regressione lasso. 
# Salva i risultati  del modello nella variabile m.lasso e il lambda ottimale ottenuto con cv.glmnet nella 
# variabile bestlam.lasso.

m.lasso <- glmnet(X, y, alpha = 1, lambda = grid)
set.seed(2)
cv.lasso <- cv.glmnet(X, y, alpha = 1, nfolds = nrow(train_df)) # ignora i warning
plot(cv.lasso)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
lasso.coef <- predict(m.lasso, type = "coefficients", s = bestlam.lasso)
lasso.coef

# vediamo come i coefficienti della regressione ridge e lasso variano in 
# funzione di lambda

par(mfrow = c(2, 2))
plot(m.ridge)
plot(m.ridge, xvar = "lambda")
abline(v = log(bestlam.ridge)) # la linea verticale corrisponde al migliore lambda
plot(m.lasso)
plot(m.lasso, xvar = "lambda")
abline(v = log(bestlam.lasso)) # la linea verticale corrisponde al migliore lambda

# Regressione con le componenti principali --------------------------------

# L'argomento validation considera automaticamente una cross-validation
# - CV: cross-validation con 10 fold
# - LOO: cross-validation leave-one-out
# Poiché le osservazioni sono poche optiamo per la seconda

set.seed(4)
m.pcr <- pcr(mpg ~ ., data = train_df, scale = TRUE, validation = "LOO")
summary(m.pcr)

# Vediamo la proporzione della varianza totale in X spiegata dalle componenti
# principali
pcr.var.explained <- cumsum(m.pcr$Xvar / m.pcr$Xtotvar)
par(mfrow = c(1, 1))
barplot(pcr.var.explained, border = NA, 
        ylab = "Proporzione della varianza spiegata")

# Seleziona il numero ottimale di componenti minimizzando l'errore in cross-validazione
validationplot(m.pcr, val.type = "MSEP")
ncomp.pcr <- which.min(m.pcr$validation$PRESS)
m.pcr.final <- pcr(mpg ~ ., data = train_df, scale = TRUE, ncomp = ncomp.pcr)
# Valori reali vs valori predetti
plot(m.pcr.final)
abline(1,1)
# coefficienti del modello finale
coef(m.pcr.final)


# Confronto tra i modelli sul test set -----------------------------------

# Da usare per la predizione
X.test <- model.matrix(mpg ~ ., test_df)[, - 1]
y.test <- test_df$mpg

# MSE SELEZIONE BEST SUBSET 
bs.pred <- predict(m.bs.final, newdata = test_df)
MSE.bs <- mean((y.test - bs.pred) ^ 2)

# MSE SELEZIONE FORWARD STEPWISE (metodo forward e backward)
fws.pred <- predict(m.fws.final, newdata = test_df)
MSE.fws <- mean((y.test - fws.pred) ^ 2)

# MSE RIDGE
ridge.pred <- predict(m.ridge, s = bestlam.ridge, newx = X.test)
MSE.ridge <- mean((ridge.pred - y.test) ^ 2)

# MSE LASSO
lasso.pred <- predict(m.lasso,s = bestlam.lasso, newx = X.test)
MSE.lasso <- mean((lasso.pred - y.test) ^ 2)

# MSE PCR
pcr.pred <- predict(m.pcr.final, newdata = test_df, ncomp = ncomp.pcr)
MSE.pcr <- mean((pcr.pred - y.test) ^ 2)


# Compara tutti i valori. Quale modello ottiene l'MSE più basso sul test set 
# (errore di previsione out.of-sample)?

MSE.all <- c( 
             BS = MSE.bs,
             FWS = MSE.fws,
             BWS = MSE.bs,
             RIDGE = MSE.ridge,
             LASSO = MSE.lasso,
             PCR = MSE.pcr
             )
par(mfrow = c(1, 1))
barplot(MSE.all, main = "Errore quadratico medio",
        col = "#d66363", border = NA)

