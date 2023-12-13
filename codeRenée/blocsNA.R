# packages
library(ggplot2)
library(tidyverse)
library(imputeTS) # pour la fonction ggplot_na_distribution
library(cowplot) # pour afficher les grids des données manquantes
library(dplyr)
library(data.table)
library(plotly)
library(lubridate)
library(scales)  # Load the scales package for date_format
library(data.table)
library(gridExtra)

## -- Importation des données
# jdd à changer !!!!
data_all_RH_LUX<- readRDS("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/data/cleaned_data_ALL.rds")

# nombre de ligne par site:
# Verification du nombre d'observations dans les jeux de données et par sites
nlignes<-data_all_RH_LUX %>%
  group_by(Site) %>%
  summarise(n = n())

# pour le site2, qui n'a presque aucune NA:
nligneaenlever2=round(nlignes$n[2]*0.2) # on prend 20% des données du site 2
print(paste("on enlève", nlignes$n[2], "lignes, ce qui correspond à", round(26130/4/24,1), "jours"))

# on pioche au hasard un entier qui derterminera le début du bloc manquant:
# set.seed(123)
# sample(nligneaenlever2: nlignes$n[2]-nligneaenlever2,1)
# > 51662
#
intervalaenlever=seq(51662:51662+nligneaenlever2)




