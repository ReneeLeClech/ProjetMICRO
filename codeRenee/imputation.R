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
library(caret)


####------------------------ IMPORTATION DES DONNEES--------------------------------------

# Utilisation de fread pour lire le fichier CSV
data_tot_temporalRH <- fread("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/transfer_6340563_files_5e328b85/data_tot_temporalRH.csv", header = TRUE, sep = ",")

# Gestion des classes des colonnes
data_tot_temporalRH[, Date := as.Date(Date, format = "%Y-%m-%d")]
data_tot_temporalRH[, Date_Heure_GMT02 := as.POSIXct(Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")]

dataRH_fac <- c("Site", "IdRH", "Hour", "Month", "Day", "Year", "longitude", "latitude")
dataRH_num <- c("Doy", "Rosee", "RH", "Temp_C")

# Utilisation de data.table pour convertir les colonnes en facteurs et numériques
data_tot_temporalRH[, (dataRH_fac) := lapply(.SD, as.factor), .SDcols = dataRH_fac]
data_tot_temporalRH[, (dataRH_num) := lapply(.SD, as.numeric), .SDcols = dataRH_num]

####------- 1. Recherche des NA par merge avec une série temporelle complète-------------------------

# Création d'un dataframe DATARANGE avec les dates et les heures sur toute la période d'expériences
Date_min <- min(data_tot_temporalRH$Date_Heure_GMT02)
Date_max <- max(data_tot_temporalRH$Date_Heure_GMT02)
DateRange <- data.table(Date_Heure_GMT02 = seq(from = Date_min, to = Date_max, by = 900))

# Ajout des colonnes supplémentaires
DateRange[, `:=`(Month = month(Date_Heure_GMT02),
                 Hour = hour(Date_Heure_GMT02),
                 Year = year(Date_Heure_GMT02),
                 Day = day(Date_Heure_GMT02),
                 Doy = yday(Date_Heure_GMT02))]

# Nombre d'enregistrements par année
DateRangeY <- as.data.table(table(Year = DateRange$Year))

# Création des 4 jeux de données pour les 4 sites
data_tot_temporalRH_site1 <- data_tot_temporalRH[Site == "RH_01"]
data_tot_temporalRH_site2 <- data_tot_temporalRH[Site == "RH_02"]
data_tot_temporalRH_site3 <- data_tot_temporalRH[Site == "RH_03"]
data_tot_temporalRH_site4 <- data_tot_temporalRH[Site == "RH_04"]

# Fusion du tableau de data range et de notre tableau
merge1RH <- merge(DateRange, data_tot_temporalRH_site1, by = "Date_Heure_GMT02", all.x = TRUE)
merge2RH <- merge(DateRange, data_tot_temporalRH_site2, by = "Date_Heure_GMT02", all.x = TRUE)
merge3RH <- merge(DateRange, data_tot_temporalRH_site3, by = "Date_Heure_GMT02", all.x = TRUE)
merge4RH <- merge(DateRange, data_tot_temporalRH_site4, by = "Date_Heure_GMT02", all.x = TRUE)
#### -----------------------------------------

# Emplilement des tableaux:
MissRH<-rbind(merge1RH,merge2RH,merge3RH,merge4RH)
# Supprimer les lignes où la colonne RH contient des valeurs manquantes
NotMissRH <- MissRH[complete.cases(MissRH$RH), ]

# nrow(MissRH)-nrow(NotMissRH)==sum(is.na(MissRH$RH))
# TRUE

levels(NotMissRH$Site)

# Fixer une graine aléatoire pour la reproductibilité
set.seed(123)

# Utiliser createDataPartition pour diviser les indices des lignes en ensembles de train et de test
indices <- createDataPartition(NotMissRH$Site, p = 0.8, list = FALSE)

# Créer les ensembles de données d'entraînement et de test
RHtrain <- NotMissRH[indices, ]
RHtest <- NotMissRH[-indices, ]

# Verification du nombre d'observations dans les jeux de données et par sites
# RHtest %>% 
#   group_by(Site) %>% 
#   summarise(n = n())
# 
# RHtrain %>% 
#   group_by(Site) %>% 
#   summarise(n = n())


#####------------------2. Imputation par la moyenne par site:  ---------------------------

# Calculer les moyennes de RH pour chaque site dans l'ensemble d'entraînement
site_means <- RHtrain %>% 
  group_by(Site) %>% 
  summarise(mean_RH = mean(RH, na.rm = TRUE))

# Fusionner les moyennes avec l'ensemble de test en fonction du site
predicted_test <- merge(RHtest, site_means, by = "Site", all.x = TRUE)

# Remplacer les valeurs de RH dans l'ensemble de test par les moyennes prédites
predicted_test$predicted_RH <- predicted_test$mean_RH


###----- Qualité de l'imputation: 
#   RMSEP
rmsep <- sqrt(mean((predicted_test$RH - predicted_test$predicted_RH)^2, na.rm = TRUE))

# Calculer le MAPE
mape <- mean(abs((predicted_test$RH - predicted_test$predicted_RH) / predicted_test$RH)) * 100

# Afficher le MAPE
print(paste("MAPE (Moyenne):", round(mape, 2), "%"))
print(paste("RMSEP (Moyenne):", round(rmsep, 2)))


#####------------------3. Imputation par modèle linéaire simple:  ---------------------------


# Créer un modèle de régression
model_1 <- lm(RH ~ Site + Hour.x + Doy.x , data = RHtrain)
anova(model_1)

predicted_test <- data.frame(
  RH = RHtest$RH,  # Valeurs réelles dans l'ensemble de test
  predicted_RH_regression = predict(model_1, newdata = RHtest)
)

###----- Qualité du modèle de régression:
# Calculer la RMSEP
rmsep_regression_1 <- sqrt(mean((predicted_test$RH - predicted_test$predicted_RH_regression)^2, na.rm = TRUE))

# Calculer le MAPE
mape <- mean(abs((predicted_test$RH - predicted_test$predicted_RH_regression) / predicted_test$RH)) * 100

# Afficher le MAPE
print(paste("MAPE (Régression):", round(mape, 2), "%"))
print(paste("RMSEP (Régression 1):", round(rmsep_regression_1, 2)))



#####------------------4. Imputation par modèle linéaire plus complet:  ---------------------------


# Créer un modèle de régression
model_2 <- lm(RH ~ Site + Hour.x + Date + Doy.y + Year.y + Day.x + Month.x, data = RHtrain)
anova(model_2)


predicted_test <- data.frame(
  RH = RHtest$RH,  # Valeurs réelles dans l'ensemble de test
  predicted_RH_regression = predict(model_2, newdata = RHtest)
)

###----- Qualité du modèle de régression:
# Calculer la RMSEP
rmsep_regression_2 <- sqrt(mean((predicted_test$RH - predicted_test$predicted_RH_regression)^2, na.rm = TRUE))
# Calculer le MAPE
mape <- mean(abs((predicted_test$RH - predicted_test$predicted_RH_regression) / predicted_test$RH)) * 100

# Afficher le MAPE
print(paste("MAPE (Régression):", round(mape, 2), "%"))
print(paste("RMSEP (Régression 2):", round(rmsep_regression_2, 2)))


#####------------------5. Imputation par modèle linéaire:  ---------------------------


# Créer un modèle de régression
model_3 <- lm(RH ~ Site + Hour.x + Doy.y + Year.y + Month.x, data = RHtrain)
anova(model_3)


predicted_test <- data.frame(
  RH = RHtest$RH,  # Valeurs réelles dans l'ensemble de test
  predicted_RH_regression = predict(model_3, newdata = RHtest)
)

###----- Qualité du modèle de régression:
# Calculer la RMSEP
rmsep_regression_3 <- sqrt(mean((predicted_test$RH - predicted_test$predicted_RH_regression)^2, na.rm = TRUE))

# Calculer l'écart type des vraies valeurs de RH
sd_test_regression_3 <- sd(predicted_test$RH, na.rm = TRUE)

# Calculer l'accuracy en pourcentage
accuracy_percentage_regression_3 <- 100 * (1 - rmsep_regression_3 / sd_test_regression_3)

# Afficher l'accuracy en pourcentage
print(paste("Accuracy en pourcentage (Régression 3):", round(accuracy_percentage_regression_3, 2), "%"))
print(paste("RMSEP (Régression 3):", round(rmsep_regression_3, 2)))




