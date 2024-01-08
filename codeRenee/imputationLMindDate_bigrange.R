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

# ------------------------- Tableau récap ------------------

data_residual<-bloc_data_mod[,c("RH_03","Hour","Month")]


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
  
  data_residual[start_index:end_index, "predictedRH_moyenne"] <- predicted_RH
  #head(data_residual)
  
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

###------- Graph pour les résidus:

summary(data_residual)

# Calculer l'erreur entre les valeurs réelles et prédites
data_residual$Error_moyenne <- sqrt((data_residual$RH_03 - data_residual$predictedRH_moyenne)^2)

# Convertir l'heure en facteur pour avoir un axe horizontal discret
data_residual$Hour <- factor(data_residual$Hour, levels = 0:24)

# Calculer la moyenne de l'erreur par heure
mean_error_by_hour <- aggregate(Error_moyenne ~ Hour, data = data_residual, FUN = mean)
mean_error_by_month<- aggregate(Error_moyenne ~ Month, data = data_residual, FUN = mean)

# Créer le graphique avec ggplot2
ggplot(mean_error_by_hour, aes(x = Hour, y = Error_moyenne)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur de l'humidité prédite par heure",
       x = "Heure",
       y = "Erreur moyenne") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))

# Créer le graphique avec ggplot2
ggplot(mean_error_by_month, aes(x = Month, y = Error_moyenne)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur de l'humidité prédite par mois",
       x = "Mois",
       y = "Erreur moyenne") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))





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
  
  data_residual[start_index:end_index, "predictedRH_lm_complet"] <- predicted_RH
  
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

##----- graph residus
summary(data_residual)

# Calculer l'erreur entre les valeurs réelles et prédites
data_residual$Error_lm_complet <- sqrt((data_residual$RH_03 - data_residual$predictedRH_lm_complet)^2)


# Calculer la moyenne de l'erreur par heure et par mois
mean_error_by_hour <- aggregate(Error_lm_complet ~ Hour, data = data_residual, FUN = mean)
mean_error_by_month <- aggregate(Error_lm_complet ~ Month, data = data_residual, FUN = mean)


# Créer le graphique avec ggplot2
ggplot(mean_error_by_hour, aes(x = Hour, y = Error_lm_complet)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur la vraie humidité et l'humidité prédite par modèle linéaire complet",
       x = "Heure",
       y = "Erreur moyenne") +
  #ylim(ymin=0,ymax=17)+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))


# Créer le graphique avec ggplot2
ggplot(mean_error_by_month, aes(x = Month, y = Error_lm_complet)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur la vraie humidité et l'humidité prédite par modèle linéaire complet",
       x = "Mois",
       y = "Erreur moyenne") +
  #ylim(ymin=0,ymax=17)+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))

# ---------------------------------LASSO ----------------
rmsep_list <- numeric(num_blocks)
list_lambda=c(1,0.5)
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
  # model_lasso_fit<-cv.glmnet(X,Y, alpha = 1)  # alpha = 1 pour la régression Lasso
  # plot(model_lasso_fit)
  
  print(paste("Lamba utilisé:",lambda))
  
  # Regression avec ledit lambda
  best_model_lasso<- glmnet(X,Y, alpha = 1, lambda=lambda)  # alpha = 1 pour la régression Lasso
  
  # Afficher les variables gardées
  print("Variables gardées:")
  x<-which(coef(best_model_lasso)[,1]!=0)
  print(x)

  
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
print("*               les 6 rmspe:              *")
print(rmsep_list)
print(paste("moyenne des rmsep pour lambda =",lambda))
print(round(mean(rmsep_list),2))

}

# 
# # Lasso mais avec lambda qui s'ajuste automatiquement:
# rmsep_list <- numeric(num_blocks)
# 
#   # Boucle pour découper le bloc initial et effectuer les prédictions
# for (i in 1:(num_blocks )) {
#     print(paste("---------------------Bloc n°", i,"/",num_blocks,"------------------------"))
#     
#     # Découper le bloc initial en petits blocs
#     start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
#     end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
#     
#     print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
#     
#     # jdd train et test
#     train_data <- bloc_data_mod[-c(start_index:end_index),]
#     test_data <- bloc_data_mod[start_index:end_index, ]
#     
#     X= model.matrix(RH_03 ~ ., data = train_data)[, -1] 
#     Y=train_data$RH_03
#     
#     # Il faut d'abord faire le choix de lambda.
#     model_lasso_fit<-cv.glmnet(X,Y, alpha = 1)  # alpha = 1 pour la régression Lasso
#     # plot(model_lasso_fit)
#     lambda=model_lasso_fit$lambda.min
#     print(paste("Lamba utilisé:",lambda))
#     
#     # Regression avec ledit lambda
#     best_model_lasso<- glmnet(X,Y, alpha = 1, lambda=lambda)  # alpha = 1 pour la régression Lasso
#     coef(best_model_lasso)
#     
#     
#     # Prediction sur le jeu de donnée test du modèle selectionné
#     predicted_RH <- predict(best_model_lasso,s=lambda,newx=model.matrix(RH_03 ~ ., data = test_data)[, -1])
#     # rmsep_lasso <- sqrt(min(model_lasso_fit$cvm))
#     
#     # Calculer la RMSEP
#     true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
#     rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
#     
#     rmsep_list[i] <- round(rmsep_gam,2)
#     
#     # Afficher les résultats
#     print(paste("RMSEP (lasso)= ", round(rmsep_gam,4)))
#   }
# print("")
# print("*               les 5 rmspe:              *")
# print(rmsep_list)
# print(paste("moyenne des rmsep pour lambda =",lambda))
# print(round(mean(rmsep_list),5))

  
#### ------------ POUR LAMBDA=1 la plus grosse pénalité: résidus ----------------------
rmsep_list <- numeric(num_blocks)
lambda=1
for (i in 1:(num_blocks )) {
  print(paste("---------------------Bloc n°", i,"/",num_blocks,"------------------------"))
  
  # Découper le bloc initial en petits blocs
  start_index <- round(max((i - 1) * small_block_size +1,1),0) # pour ne pas etre = 0
  end_index <- round(min(start_index + small_block_size, nrow(bloc_data)),0) # pour ne pas desppaser nrow
  
  print(paste("Test index from :", round(start_index,0),"to", round(end_index,0)))
  
  # jdd train et test
  train_data <- bloc_data_mod[-c(start_index:end_index),]
  test_data <- bloc_data_mod[start_index:end_index, ]
  
  # sous forme matricielle 
  X= model.matrix(RH_03 ~ ., data = train_data)[, -1] 
  Y=train_data$RH_03
  
  # lambda = 1
  print(paste("Lamba utilisé:",1))
  
  # Regression avec ledit lambda
  best_model_lasso<- glmnet(X,Y, alpha = 1, lambda=lambda)  # alpha = 1 pour la régression Lasso
  
  # Afficher les variables gardées
  print("Variables gardées:")
  x<-which(coef(best_model_lasso)[,1]!=0)
  print(x)
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(best_model_lasso,s=lambda,newx=model.matrix(RH_03 ~ ., data = test_data)[, -1])
  # rmsep_lasso <- sqrt(min(model_lasso_fit$cvm))
  
  # ajout de la colonne de prediction
  data_residual[start_index:end_index, "predictedRH_lasso1"] <- predicted_RH
  
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  rmsep_list[i] <- round(rmsep_gam,2)
  
  # Afficher les résultats
  print(paste("RMSEP (lasso)= ", round(rmsep_gam,4)))
}


print("")
print("*               les 5 rmspe:              *")
print(rmsep_list)
print(paste("moyenne des rmsep pour lambda =",lambda))
print(round(mean(rmsep_list),5))


### ---- graphs

summary(data_residual)

# Calculer l'erreur entre les valeurs réelles et prédites
data_residual$Error_lasso1 <- sqrt((data_residual$RH_03 - data_residual$predictedRH_lasso1)^2)


# Calculer la moyenne de l'erreur par heure
mean_error_by_hour <- aggregate(Error_lasso1 ~ Hour, data = data_residual, FUN = mean)
mean_error_by_month <- aggregate(Error_lasso1 ~ Month, data = data_residual, FUN = mean)

# Par journée
ggplot(mean_error_by_hour, aes(x = Hour, y = Error_lasso1)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur la vraie humidité et l'humidité prédite par modèle LASSO",
       subtitle= "lambda = 1",
       x = "Heure",
       y = "Erreur moyenne") +
  #ylim(ymin=0,ymax=17)+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))

# par mois
ggplot(mean_error_by_month, aes(x = Month, y = Error_lasso1)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Moyenne de l'erreur la vraie humidité et l'humidité prédite par modèle LASSO",
       subtitle= "lambda = 1",
       x = "Mois",
       y = "Erreur moyenne") +
  #ylim(ymin=0,ymax=17)+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 17))







## -------------------------------- RIDGE ----------------------------------------- 


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
    
    print(paste("Lamba utilisé:",lambda))
    
    # Regression avec ledit lambda
    best_model_ridge<- glmnet(X,Y, alpha = 0, lambda=lambda)  # alpha = 1 pour la régression Ridge
    coef(best_model_ridge)
    
    
    # Prediction sur le jeu de donnée test du modèle selectionné
    predicted_RH <- predict(best_model_ridge,s=lambda,newx=model.matrix(RH_03 ~ ., data = test_data)[, -1])
    # rmsep_lasso <- sqrt(min(model_lasso_fit$cvm))
    
    # Calculer la RMSEP
    true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
    rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
    
    rmsep_list[i] <- round(rmsep_gam,2)
    
    # Afficher les résultats
    print(paste("RMSEP (ridge)= ", round(rmsep_gam,4)))
  }
  print("")
  print("*               les 5 rmspe:              *")
  print(rmsep_list)
  print(paste("moyenne des rmsep pour lambda =",lambda))
  print(round(mean(rmsep_list),2))
  
}


# Ridge mais avec lambda qui s'ajuste automatiquement:


rmsep_list <- numeric(num_blocks)

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
  model_ridge_fit<-cv.glmnet(X,Y, alpha = 0)  # alpha = 0 pour la régression Ridge
  # plot(model_lasso_fit)
  lambda=model_ridge_fit$lambda.min
  print(paste("Lamba utilisé:",lambda))
  
  # Regression avec ledit lambda
  best_model_lasso<- glmnet(X,Y, alpha = 0, lambda=lambda)  # alpha = 1 pour la régression Lasso
  coef(best_model_lasso)
  
  
  # Prediction sur le jeu de donnée test du modèle selectionné
  predicted_RH <- predict(best_model_lasso,s=lambda,newx=model.matrix(RH_03 ~ ., data = test_data)[, -1])
  # rmsep_lasso <- sqrt(min(model_lasso_fit$cvm))
  
  # Calculer la RMSEP
  true_RH <- bloc_data_mod[start_index:end_index,]$RH_03
  rmsep_gam <- sqrt(mean((true_RH - predicted_RH)^2))
  
  rmsep_list[i] <- round(rmsep_gam,2)
  
  # Afficher les résultats
  print(paste("RMSEP (ridge)= ", round(rmsep_gam,4)))
}
print("")
print("*               les 5 rmspe:              *")
print(rmsep_list)
print(paste("moyenne des rmsep pour lambda qui varie "))
print(round(mean(rmsep_list),5))
## ---------------------------------------------------------------------------- 



rmsep_list <- numeric(num_blocks)
residuals_list <- list()  # Créer une liste pour stocker les résidus de chaque bloc

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

# Concaténer les résidus de chaque bloc
all_residuals <- do.call(rbind, residuals_list)

# Calculer la moyenne par heure
mean_residuals_by_hour <- aggregate(residus ~ heure, data = all_residuals, FUN = mean)

# Afficher la moyenne des écarts par heure
cat("Moyenne des écarts par heure:\n", mean_residuals_by_hour, "\n")

# Afficher la moyenne des RMSEP
cat("Moyenne RMSEP:", round(mean(rmsep_list), 2), "\n")


## ---------------------------------------------------------------------------- 
## ---------------------------------------------------------------------------- 


