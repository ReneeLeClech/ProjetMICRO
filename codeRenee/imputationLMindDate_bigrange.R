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

##----------------- Importation des données et gestion des classes: --------------

tab_indiv_date<- readRDS("data/tab_indiv_date.rds")
tab_indiv_date<-as.data.table(tab_indiv_date)

# Les variables liées au hobo LUX du site 2 et du hobo RH du site 4 contiennent trop de NA: elles sont retirées du jdd
tab_indiv_date <- subset(tab_indiv_date, select= -c(RH_04,Rosee_04,Temp_RH_04,Intensity_lux_02,Temp_LUX_02,Year,Doy)) 
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
bloc_data_mod <- subset(bloc_data, select= -c(Rosee_03,Temp_RH_03,Date_Heure_GMT02)) 
# Définir la taille du jdd initial
block_size <- length(bloc_data_mod$RH_01)

# Définir le nombre de petits blocs
num_blocks <- 10

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
}
list_dates


# -----------------------------  GRAPH  ------------------------------
# Liste des colonnes à vérifier








#-------------------------- IMPUTATION PAR LA MOYENNE ----------------------------

# Calculer la moyenne de RH_03 sur le jeu de données train

rmsep_list <- numeric(num_blocks)
# Boucle pour découper le bloc initial et effectuer les prédictions
for (i in 1:(num_blocks)) {
  print(paste("*     Bloc n°", i, "/", num_blocks, "   *"))
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size + 1, 1), 0) # pour ne pas être = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)), 0) # pour ne pas dépasser nrow
  
  print(paste("Test index from :", round(start_index, 0), "to", round(end_index, 0)))
  
  # Prédiction de la moyenne de RH_03 du jeu de données train
  
  moy = mean(bloc_data_mod$RH_03[-c(start_index:end_index)])
  predicted_RH <- rep(moy, end_index - start_index + 1)
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index, ]$RH_03
  rmsep <- sqrt(mean((true_RH - predicted_RH)^2))
  
  rmsep_list[i] <- round(rmsep,2)
  # Afficher les résultats
  print(paste("RMSEP (moyenne)=", round(rmsep, 2)))
  print("")

  }
rmsep_list
round(mean(rmsep_list),2)

#-------------------------- MODELE LINEAIRE COMPLET ----------------------------

rmsep_list <- numeric(num_blocks)
for (i in 1:(num_blocks )) {
  print(paste("----------------------- Bloc n°", i,"/",num_blocks," -------------------"))
  
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  # creation du modèle linéaire complet
  model_lin<-lm(RH_03~. ,data =bloc_data_mod[-c(start_index:end_index),] ,  )
  # print(summary(model_lin))
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(model_lin,newdata=bloc_data_mod[start_index:end_index,])
  # Calculer la RMSEP
  true_RH <- bloc_data_mod$RH_03[start_index:end_index]
  rmsep <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # implementation de la liste
  rmsep_list[i] <- round(rmsep,2)
  
  # Afficher les résultats
  print(paste("RMSEP (LIN)= ", round(rmsep,2)))
  print("")
  print("")
  
}

rmsep_list
round(mean(rmsep_list),2)


# ---------------------------------LASSO
rmsep_list <- numeric(num_blocks)
list_lambda=c(0.1,0.01)
for (lambda in list_lambda){
  # Boucle pour découper le bloc initial et effectuer les prédictions
for (i in 1:(num_blocks )) {
  print(paste("---------------------Bloc n°", i,"/",num_blocks,"------------------------"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  
  # jdd train et test
  train_data <- bloc_data_mod[-c(start_index:end_index),]
  test_data <- bloc_data_mod[start_index:end_index, ]
  
  X= model.matrix(RH_03 ~ ., data = train_data)[, -1] 
  Y=train_data$RH_03
  
  # Il faut d'abord faire le choix de lambda.
  model_lasso_fit<-cv.glmnet(X,Y, alpha = 1)  # alpha = 1 pour la régression Lasso
  plot(model_lasso_fit)
  
  print(paste("Lamba utilisé:",lambda))
  
  # Regression avec ledit lambda
  best_model_lasso<- glmnet(X,Y, alpha = 1, lambda=lambda)  # alpha = 1 pour la régression Lasso
  coef(best_model_lasso)
  
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(best_model_lasso,s=lambda,newx=model.matrix(RH_03 ~ ., data = test_data)[, -1])
  # rmsep_lasso <- sqrt(min(model_lasso_fit$cvm))
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  rmsep_list[i] <- round(rmsep_gam,2)
  
  # Afficher les résultats
  print(paste("RMSEP (lasso)= ", round(rmsep_gam,4)))
}
  print("")
print("*               les 10 rmspe:********")
rmsep_list
print(paste("moyenne des rmsep pour lambda =",lambda))
print(round(mean(rmsep_list),2))

}



## ---------------------------------------------------------------------------- 
## ---------------------------------------------------------------------------- 
## ---------------------------------------------------------------------------- 
## ---------------------------------------------------------------------------- 


