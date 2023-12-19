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
library(mgcv)
library(glmnet)

##----------------- Importation des données et gestion des classes: --------------

tab_indiv_date<- readRDS("data/tab_indiv_date.rds")
tab_indiv_date<-as.data.table(tab_indiv_date)

colnames(tab_indiv_date)
# Gestion des classes des colonnes
tab_indiv_date[, Date_Heure_GMT02 := as.POSIXct(Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")]

# on enlève la variable moi car on n'a pas tous les mois on ça pose des problèmes dans les modèles
tab_indiv_date<- tab_indiv_date[,-c("Doy",'Day',"Month")]
#data_fac <- c("Hour", "Month", "Year","Doy",'Day')
data_fac <- c("Hour", "Year")
data_num <- c( "RH_01", "Rosee_01", "Temp_RH_01", "RH_02", "Rosee_02", "Temp_RH_02", "RH_03", "Rosee_03", "RH_04" , 
                 "Rosee_04", "Temp_RH_04", "Intensity_lux_01", "Temp_LUX_01", "Intensity_lux_02", "Temp_LUX_02",
                 "Intensity_lux_03", "Temp_LUX_03", "Intensity_lux_04", "Temp_LUX_04")

# Utilisation de data.table pour convertir les colonnes en facteurs et numériques
tab_indiv_date[, (data_fac) := lapply(.SD, as.factor), .SDcols = data_fac]
tab_indiv_date[, (data_num) := lapply(.SD, as.numeric), .SDcols = data_num]


# Définir les bornes temporelles
start_date <- as.POSIXct("2018-10-18 10:30:00", format="%Y-%m-%d %H:%M:%S")
#=> Date a partir de laquel on a des données sur tous les sites. 
end_date   <- as.POSIXct("2019-07-05 10:30:00", format="%Y-%m-%d %H:%M:%S")
#=> Date à partir de laquel on a 
bloc_data <- subset(tab_indiv_date, Date_Heure_GMT02 >= start_date & Date_Heure_GMT02 <= end_date)
# enlever les LIGNES avec des NA
bloc_data<-na.omit(bloc_data)
length(bloc_data)

# On enlève les autres données liés aux capteurs. 
bloc_data_mod <- subset(bloc_data, select= -c(Rosee_03,Temp_RH_03,Date_Heure_GMT02)) 
# Définir la taille du jdd initial
block_size <- length(bloc_data_mod$RH_01)

# Définir le nombre de petits blocs
num_blocks <- 5

# Calculer la taille de chaque petit bloc
small_block_size <- block_size / num_blocks

#---------------------- DATES ------------------------------

for (i in 1:(num_blocks )) {
  print(paste("*     Bloc n°", i,"/",num_blocks,"   *"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  print(bloc_data$Date_Heure_GMT02[start_index])
}

#-------------------------- IMPUTATION PAR LA MOYENNE ----------------------------

# Calculer la moyenne de RH_03 sur le jeu de données train


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
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # Afficher les résultats
  print(paste("RMSEP (moyenne)=", round(rmsep_gam, 2)))
}

#-------------------------- MODELE LINEAIRE COMPLET ----------------------------
i=1
# Boucle pour découper le bloc initial et effectuer les prédictions
# Boucle pour découper le bloc initial et effectuer les prédictions
for (i in 1:(num_blocks )) {
  print(paste("----------------------- Bloc n°", i,"/",num_blocks," -------------------"))
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  # creation du modèle linéaire complet
  model_lin<-lm(RH_03~. ,data =bloc_data_mod[-c(start_index:end_index),] )
  # print(summary(model_lin))
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(model_lin,newdata=bloc_data_mod[start_index:end_index,])
  # Calculer la RMSEP
  true_RH <- bloc_data_mod$RH_03[start_index:end_index]
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # Afficher les résultats
  print(paste("RMSEP (LIN)= ", round(rmsep_gam,2)))
  print("")
  print("")
  
}


####--------------- SELECTION MODELE LINEAIRE PAR AIC ---------------
for (i in 1:(num_blocks )) {
  print(paste("----------------------- Bloc n°", i,"/",num_blocks," -------------------"))
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  # creation du modèle linéaire complet
  model_lin<-lm(RH_03~. ,data =bloc_data_mod[-c(start_index:end_index),] )
  
  selected_model<-stepwise(model_lin,direction="backward",criterion="AIC",trace=TRUE)
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(selected_model,newdata=bloc_data_mod[start_index:end_index,])
  # Calculer la RMSEP
  true_RH <- bloc_data_mod$RH_03[start_index:end_index]
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # Afficher les résultats
  print(paste("RMSEP (LIN)= ", round(rmsep_gam,2)))
  print("")
  print("")
  
}

# Boucle pour découper le bloc initial et effectuer les prédictions
for (i in 1:(num_blocks )) {
  print(paste("*     Bloc n°", i,"/",num_blocks,"   *"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  
  model_lin<-lm(RH_03~. ,data =bloc_data_mod[-c(start_index:end_index),] )
  
  # selection de variables par AIC
  mod_sel<-stepAIC(model_lin,direction="backward",criterion="AIC",trace=TRUE)
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(mod_sel,newdata=bloc_data_mod[start_index:end_index,])
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # Afficher les résultats
  print(paste("RMSEP (GAM)= ", round(rmsep_gam,2)))
  
}

####--------------- LASSO:  SELECTION MODELE LINEAIRE  ---------------

i=1
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
  
  # Recherche du lambda
  best_lambda<-model_lasso_fit$lambda.min
  print(paste("Meilleur lambda:",best_lambda))
  
  # Regression avec ledit lambda
  best_model_lasso<- glmnet(X,Y, alpha = 1, lambda=0.1)  # alpha = 1 pour la régression Lasso
  coef(best_model_lasso)
  
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(best_model_lasso,s=best_lambda,newx=model.matrix(RH_03 ~ ., data = test_data)[, -1])
  # rmsep_lasso <- sqrt(min(model_lasso_fit$cvm))
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # Afficher les résultats
  print(paste("RMSEP (GAM)= ", round(rmsep_gam,4)))
}

####--------------------------- GAM ---------------------------------------------------
bloc_data_mod<- bloc_data_mod %>%mutate (Doy= as.numeric(Doy))
  
# Créer un modèle GAM avec des splines en incluant toutes les variables
my_formula <- RH_03 ~  s(RH_01) + s(Temp_RH_01) + s(Rosee_01) + s(Temp_LUX_01) + s(Intensity_lux_01) +
  s(RH_02) + s(Temp_RH_02) + s(Rosee_02) + s(Temp_LUX_02) + s(Intensity_lux_02) +
  s(Temp_LUX_03) + s(Intensity_lux_03) +
  s(RH_04) + s(Temp_RH_04) + s(Rosee_04) + s(Temp_LUX_04) + s(Intensity_lux_04) + s(Doy)+
  Year + Day 


# Boucle pour découper le bloc initial et effectuer les prédictions
for (i in 1:(num_blocks )) {
  print(paste("*     Bloc n°", i,"/",num_blocks,"   *"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  
  model_gam <- gam(formula = my_formula, data = bloc_data_mod[-c(start_index:end_index), ])
  
  # Faire des prédictions sur les données du bloc actuel
  predicted_RH <- predict(model_gam, newdata = bloc_data_mod[start_index:end_index, ], type = "response")
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  # Afficher les résultats
  print(paste("RMSEP (GAM)= ", round(rmsep_gam,2)))
  
}

####------------------------GAM avec variation de sp  --------------------------
  
# Boucle pour ajuster le paramètre de lissage
sp_values <- c(0.01, 0.1, 1, 5)  # Liste des valeurs à tester
for (sp_value in sp_values) {
  print(paste("(-------------------- sp: ",sp_value ," -------------------)"))
    
  # Boucle pour découper le bloc initial et effectuer les prédictions
  for (i in 1:(num_blocks )) {
    print(paste("*     Bloc n°", i,"/",num_blocks,"   *"))
    
    # Découper le bloc initial en petits blocs
    start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
    end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
    
    print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
    
    model_gam <- gam(formula = my_formula, 
                     data = bloc_data_mod[-c(start_index:end_index), ],
                     sp = sp_value)
    
    # Faire des prédictions sur les données du bloc actuel
    predicted_RH <- predict(model_gam, newdata = bloc_data_mod[start_index:end_index, ], type = "response")
    
    # Calculer la RMSEP
    true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
    rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
    
    # Afficher les résultats
    print(paste("RMSEP (GAM)= ", round(rmsep_gam,2)))
    
  }
}


# Boucle pour ajuster le paramètre de lissage
sp_values <- c(0.01, 0.1, 1, 5)  # Liste des valeurs à tester
for (sp_value in sp_values) {
  print(paste("(-------------------- sp: ",sp_value ," -------------------)"))
  # Boucle pour découper le bloc initial et effectuer les prédictions
  for (i in 1:(num_blocks)) {
    # Découper le bloc initial en petits blocs
    start_index <- (i - 1) * small_block_size + 1
    end_index <- i * small_block_size
    
    print(paste("*     Bloc n°", i,"/",num_blocks,"   *"))
    # Vérifier les indices de découpage
    print(paste("Index from :", round(start_index,0),"to", round(end_index,0)))
    
    # Sous-ensemble de données
    subset_data <- bloc_data_mod[start_index:end_index, ]
    #print(head(subset_data))  # Vérifier les données du bloc
    
    # Modèle GAM
    model_gam <- gam(formula = my_formula, data = bloc_data_mod[-c(start_index:end_index), ], sp = sp_value)
    
    # Faire des prédictions sur les données du bloc actuel
    predicted_RH <- predict(model_gam, newdata = subset_data, type = "response")
    
    # Calculer la RMSEP
    true_RH <- subset_data$RH_03
    rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
    
    # Afficher les résultats
    print(paste("RMSEP (GAM) :", round(rmsep_gam, 2)))
    print("")
  }
}




####------------------- RANDOM FORET -------------------------------

library(randomForest)
bloc_data_mod<- bloc_data_mod %>%mutate (Doy= as.numeric(Doy))


# Boucle pour découper le bloc initial et effectuer les prédictions avec Random Forest
for (i in 1:(num_blocks )) {
  print(paste("*     Bloc n°", i,"/",num_blocks,"   *"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0)
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0)
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  
  # création du modèle Random Forest
  model_rf <- randomForest(RH_03 ~ ., data = bloc_data_mod[-c(start_index:end_index),])
  
  # Prédiction sur le jeu de données test du modèle sélectionné
  predicted_RH_rf <- predict(model_rf, newdata = bloc_data_mod[start_index:end_index,])
  
  # Calculer la RMSEP pour Random Forest
  rmsep_rf <- sqrt(mean((true_RH - predicted_RH_rf)^2))
  
  # Afficher les résultats pour Random Forest
  print(paste("RMSEP (Random Forest) = ", round(rmsep_rf,2)))
}

