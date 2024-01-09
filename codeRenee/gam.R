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
library(MASS)
library(RcmdrMisc)
library(randomForest)
library(glmnet)
library(mgcv)

##----------------- Importation des données et gestion des classes: --------------

tab_indiv_date<- readRDS("data/tab_indiv_date.rds")
tab_indiv_date<-as.data.table(tab_indiv_date)

# Les variables liées au hobo LUX du site 2 et du hobo RH du site 4 contiennent trop de NA: elles sont retirées du jdd
tab_indiv_date <- subset(tab_indiv_date, select= -c(RH_04,Rosee_04,Temp_RH_04,Intensity_lux_02,Temp_LUX_02,Year)) 
colnames(tab_indiv_date)

# Gestion des classes des colonnes
tab_indiv_date[, Date_Heure_GMT02 := as.POSIXct(Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")]

data_fac <- c("Hour", "Month", "Day")
data_num <- c( "RH_01", "Rosee_01", "Temp_RH_01", "RH_02", "Rosee_02", "Temp_RH_02", "RH_03", "Rosee_03" , 
               "Intensity_lux_01", "Temp_LUX_01", "Intensity_lux_03", "Temp_LUX_03", "Intensity_lux_04", "Temp_LUX_04")

# Utilisation de data.table pour convertir les colonnes en facteurs et numériques
tab_indiv_date[, (data_fac) := lapply(.SD, as.factor), .SDcols = data_fac]
tab_indiv_date[, (data_num) := lapply(.SD, as.numeric), .SDcols = data_num]

print(paste("on a dans ce jdd", nrow(tab_indiv_date),"lignes"))

######---------------- prepapation des jdd pour le modèle: ---------------------------

# On enlève les autres données liés aux capteurs. 
bloc_data<-na.omit(tab_indiv_date)
print(paste("on a dans le meme jdd", nrow(bloc_data),"lignes non vides !"))

# jdd pour le modèle
bloc_data_mod <- subset(bloc_data, select= -c(Rosee_03,Temp_RH_03,Date_Heure_GMT02,Day)) 
# Définir la taille du jdd initial
block_size <- length(bloc_data_mod$RH_01)

# Définir le nombre segments de NA 
num_blocks <- 5

# Calculer la taille de chaque petit bloc
small_block_size <- round(block_size / num_blocks,0)


#---------------------- DATES ------------------------------
list_dates=list()
for (i in 1:num_blocks) {
  print(paste("*     Bloc n°", i, "/", num_blocks, "   *"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round((i - 1) * small_block_size) + 1  # +1 pour éviter d'avoir 0
  end_index <- min(round(i * small_block_size), nrow(bloc_data))
  
  print(paste("Test index from :", start_index, "to", end_index))
  list_dates[[i]]<-as.POSIXct(bloc_data$Date_Heure_GMT02[start_index], format = "%Y-%m-%d %H:%M:%S")
  
  print(bloc_data$Date_Heure_GMT02[start_index])
  print(bloc_data$Date_Heure_GMT02[end_index])
}
list_dates

# ------------------------- Tableau récap pour les résidus  ------------------

data_residual<-bloc_data_mod[,c("RH_03","Hour","Month")]

##########################################################################################
##########################################################################################


summary(bloc_data_mod)

# selection des variables qu'on a envie de prendre en cyclique ou pas:

# HEURE
bloc_data_mod$Hour <- as.numeric(as.character(bloc_data$Hour))
mod_RH3 <- gam(RH_03 ~ s(Hour, k = 10, bs="cc"), data = bloc_data_mod)
# on affiche: 
plot(mod_RH3, residuals = FALSE, pch = 1)
title("Modèle GAM - Relation entre RH_03 et l'heure en cyclique")
# splin cyclique sur l'heure a l'air de bien fonctionner , on a bien envie de le mettre dans un modèles


# MOIS
bloc_data_mod$Month <- as.numeric(as.character(bloc_data$Month))
mod_RH3 <- gam(RH_03 ~ s(Month, k = 10,bs="cc"), data = bloc_data_mod)
# on affiche: 
plot(mod_RH3, residuals = FALSE, pch = 1)
title("Modèle GAM - Relation entre RH_03 et le Mois en cyclique")
# splin cyclique sur mois a l'air de bien fonctionner , on a bien envie de le mettre dans un modèles


# DOY:
bloc_data_mod$Doy <- as.numeric(as.character(bloc_data$Doy))
mod_RH3 <- gam(RH_03 ~ s(Doy, k = 366), data = bloc_data_mod)
# on affiche: 
plot(mod_RH3, residuals = FALSE, pch = 1)
title("Modèle GAM - Relation entre RH_03 et le Jour de l'année en cyclique")
# bof bof pas envie de le prendre dans le modèle
## --------------------

rmsep_list <- numeric(num_blocks)
for (i in 1:(num_blocks )) {
  print(paste("----------------------- Bloc n°", i,"/",num_blocks," -------------------"))
  
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  # creation du modèle gam
  model_gam<-gam(RH_03~RH_01+Rosee_01+ Temp_RH_01+ RH_02+ Rosee_02+ Temp_RH_02+ Intensity_lux_01+ 
                   Temp_LUX_01+ Intensity_lux_03+ Temp_LUX_03+ Intensity_lux_04+ Temp_LUX_04+ s(Month,k=10,bs="cc")+ s(Hour,k=10,bs="cc") ,data =bloc_data_mod[-c(start_index:end_index),] ,  )
  # print(summary(model_lin))
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(model_gam,newdata=bloc_data_mod[start_index:end_index,])
  # Calculer la RMSEP
  
  data_residual[start_index:end_index, "predictedRH_gam_cheuremois"] <- predicted_RH
  
  true_RH <- bloc_data_mod$RH_03[start_index:end_index]
  rmsep <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # implementation de la liste
  rmsep_list[i] <- round(rmsep,2)
  
  # Afficher les résultats
  print(paste("RMSEP (GAM)= ", round(rmsep,2)))
  print("")
  print("")
  
}

rmsep_list
round(mean(rmsep_list),2)

summary(model_gam)
summary(data_residual)

##----- graph residus



# Calculer l'erreur entre les valeurs réelles et prédites
data_residual$Error_gam <- sqrt((data_residual$RH_03 - data_residual$predictedRH_gam_cheuremois)^2)


# Calculer la moyenne de l'erreur par heure et par mois
mean_error_by_hour <- aggregate(Error_gam ~ Hour, data = data_residual, FUN = mean)
mean_error_by_month <- aggregate(Error_gam ~ Month, data = data_residual, FUN = mean)


# Créer le graphique avec ggplot2
ggplot(mean_error_by_hour, aes(x = Hour, y = Error_gam)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur de l'humidité prédite par modèle gam ",
       x = "Heure",
       y = "Erreur moyenne") +
  #ylim(ymin=0,ymax=17)+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))


# Créer le graphique avec ggplot2
ggplot(mean_error_by_month, aes(x = Month, y = Error_gam)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur de l'humidité prédite par modèle gam ",
       x = "Mois",
       y = "Erreur moyenne") +
  #ylim(ymin=0,ymax=17)+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))



