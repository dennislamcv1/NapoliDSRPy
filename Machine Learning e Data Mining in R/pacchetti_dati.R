# Inizializzazione --------------------------------------------------------
rm(list = ls()) # Rimuove tutte le variabili
dev.off() # Chiude tutti i grafici
cat("\f") # Pulisce la console
cat("\014") # Pulisce la console

# Pacchetti pre-installati ------------------------------------------------
library(mvtnorm)
library(rgl)
library(readxl)
library(readr)
library(dplyr)
library(car)
library(PerformanceAnalytics)
library(MASS)
library(ISLR)
library(readr)
library(nycflights13)
library(tidyverse)
library(GGally)
library(glmnet)
library(pls)
library(leaps)
library(corrplot)
library(cluster)
library(matrixStats)
library(reshape2)
library(scatterplot3d)
library(patchwork)

# Dati pre-caricati (Environment) -----------------------------------------
data("flights")
data("iris")
data("mpg")
data("Boston")

SpotWeldingData <- read.csv2("~/data/SpotWeldingData.csv")
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
GTDataset_all <- read_csv("data/GTDataset_all.csv")