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

##-----
data_all_RH_LUX<- readRDS("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/data/cleaned_data_ALL.rds")
data_all_RH_LUX<-as.data.table(data_all_RH_LUX)

colnames(data_all_RH_LUX)
# Gestion des classes des colonnes
data_all_RH_LUX[, Date_Heure_GMT02 := as.POSIXct(Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")]

dataRH_fac <- c("Site",  "Hour", "Month", "Day", "Year","Doy")
dataRH_num <- c( "Rosee", "RH","Temp_C_RH","Temp_C_LUX","Intensity_lux")

# Utilisation de data.table pour convertir les colonnes en facteurs et numériques
data_all_RH_LUX[, (dataRH_fac) := lapply(.SD, as.factor), .SDcols = dataRH_fac]
data_all_RH_LUX[, (dataRH_num) := lapply(.SD, as.numeric), .SDcols = dataRH_num]

#-------------------------------------------------------------------------------------------------
# Création d'un dataframe DATARANGE avec les dates et les heures sur toute la période d'expériences
Date_min=min(data_all_RH_LUX$Date_Heure_GMT02)

Date_max=max(data_all_RH_LUX$Date_Heure_GMT02)
DateRange <- data.table(Date_Heure_GMT02 = seq(from = Date_min,
                                               to = Date_max, by = 900))


# Ajout des colonnes supplémentaires
DateRange[, `:=`(Month = month(Date_Heure_GMT02),
                 Hour = hour(Date_Heure_GMT02),
                 Year = year(Date_Heure_GMT02),
                 Day = day(Date_Heure_GMT02),
                 Doy = yday(Date_Heure_GMT02))]

# Nombre d'enregistrements par année
DateRangeY <- as.data.table(table(Year = DateRange$Year))

# Creation des 4 jeux de donées---------------------------------------------------------------
# Création des 4 jeux de données pour les 4 sites et fusion avec DateRange
merge1 <- DateRange %>% left_join(filter(data_all_RH_LUX, Site == "RH_01"), by = "Date_Heure_GMT02")
merge2 <- DateRange %>% left_join(filter(data_all_RH_LUX, Site == "RH_02"), by = "Date_Heure_GMT02")
merge3 <- DateRange %>% left_join(filter(data_all_RH_LUX, Site == "RH_03"), by = "Date_Heure_GMT02")
merge4 <- DateRange %>% left_join(filter(data_all_RH_LUX, Site == "RH_04"), by = "Date_Heure_GMT02")

merge1LUX <- as.data.table(merge1) %>% 
  select("Intensity_lux","Date_Heure_GMT02")
merge2LUX <- as.data.table(merge2) %>% 
  select("Intensity_lux","Date_Heure_GMT02")
merge3LUX <- as.data.table(merge3) %>% 
  select("Intensity_lux","Date_Heure_GMT02")
merge4LUX <- as.data.table(merge4) %>% 
  select("Intensity_lux","Date_Heure_GMT02")

merge1RH <- as.data.table(merge1) %>% 
  select("RH","Date_Heure_GMT02")
merge2RH <- as.data.table(merge2) %>% 
  select("RH","Date_Heure_GMT02")
merge3RH <- as.data.table(merge3) %>% 
  select("RH","Date_Heure_GMT02")
merge4RH <- as.data.table(merge4) %>% 
  select("RH","Date_Heure_GMT02")


##----------------------------------------------------------------------------------

# Fonction pour créer les blocs de données manquantes pour un site donné
create_filtered_data_LUX <- function(merge_data, site) {
  filtered_data <- merge_data %>%
    filter(is.na(Intensity_lux)) %>%
    # identification du nombre de blocs manquants
    mutate(Block_ID = cumsum(!is.na(Intensity_lux) & lag(is.na(Intensity_lux), default = TRUE)),
           # on met un format de temps 
           reference_time = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
           # on cree les colonnes du numéro du bloc et de la convertion en secondes
           
           TimeNumeric = as.numeric(difftime(Date_Heure_GMT02, reference_time, units = "secs")) / (15 * 60),
           Block_ID = cumsum(c(0, diff(TimeNumeric) > 1)) + 1,
           # on met a jour la colonne site
           Site = rep(site, n()))
  return(filtered_data)
}

# Liste pour stocker les données filtrées pour chaque site
filtered_data_list_LUX <- list()

# Application de la fonction pour trouver les blocs des données manquantes
for (site in 1:4) {
  site_data <- get(paste0("merge", site, "LUX"))
  filtered_data_list_LUX[[site]] <- create_filtered_data_LUX(site_data, site)
}

# Empilement des données
stacked_data_LUX <- rbindlist(filtered_data_list_LUX)

# Plot
tabforplot_LUX <- stacked_data_LUX[, .(Date_Heure_GMT02, Block_ID, Site)]

#------ RH

# Fonction pour créer les blocs de données manquantes pour un site donné
create_filtered_data_RH <- function(merge_data, site) {
  filtered_data <- merge_data %>%
    filter(is.na(RH)) %>%
    # identification du nombre de blocs manquants
    mutate(Block_ID = cumsum(!is.na(RH) & lag(is.na(RH), default = TRUE)),
           # on met un format de temps 
           reference_time = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
           # on cree les colonnes du numéro du bloc et de la convertion en secondes
           
           TimeNumeric = as.numeric(difftime(Date_Heure_GMT02, reference_time, units = "secs")) / (15 * 60),
           Block_ID = cumsum(c(0, diff(TimeNumeric) > 1)) + 1,
           # on met a jour la colonne site
           Site = rep(site, n()))
  return(filtered_data)
}

# Liste pour stocker les données filtrées pour chaque site
filtered_data_list_RH <- list()

# Application de la fonction pour trouver les blocs des données manquantes
for (site in 1:4) {
  site_data <- get(paste0("merge", site, "RH"))
  filtered_data_list_RH[[site]] <- create_filtered_data_RH(site_data, site)
}

# Empilement des données
stacked_data_RH <- rbindlist(filtered_data_list_RH)

# Plot
tabforplot_RH <- stacked_data_RH[, .(Date_Heure_GMT02, Block_ID, Site)]


## ------------------ 4. plot des bloc de données manquantes -------------------
plot1<- ggplot(tabforplot_LUX, aes(x = Date_Heure_GMT02, y = Site, color = as.factor(Site))) +
  
  # echelle temporelle
  scale_x_datetime(labels = scales::date_format("%Y"), limits = c(Date_min, Date_max)) +
  # mise en avant des années
  geom_vline(xintercept = as.numeric(as.POSIXct("2018-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2019-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2022-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  
  #Traits pour les sites
  geom_hline(yintercept = 1, color = "darkred", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 2, color = "darkgreen", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 3, color = "darkblue", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 4, color = "darkorchid4", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  
  
  # Ajout des NA
  geom_point(size = 4) +
  
  # Titre, sous-titre et axes
  labs(title = "Répartition temporelle des NAs de la luminosité sur les différents sites",
       subtitle = "1 point = 1 NA",
       x = "Date",
       y = "Site",
       color = "Site") +
  
  # Autres paramètres
  theme(legend.position = "none",plot.margin = margin(20, 20, 20, 20, "pt"))+
  guides(color = "none")+
  theme_classic()

#--------RH
plot2<- ggplot(tabforplot_RH, aes(x = Date_Heure_GMT02, y = Site, color = as.factor(Site))) +
  
  # echelle temporelle
  scale_x_datetime(labels = scales::date_format("%Y"),limits = c(Date_min, Date_max)) +
  # mise en avant des années
  geom_vline(xintercept = as.numeric(as.POSIXct("2018-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2019-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2022-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  
  #Traits pour les sites
  geom_hline(yintercept = 1, color = "darkred", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 2, color = "darkgreen", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 3, color = "darkblue", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 4, color = "darkorchid4", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  
  
  # Ajout des NA
  geom_point(size = 4) +
  
  # Titre, sous-titre et axes
  labs(title = "Répartition temporelle des NAs de l'humidité sur les différents sites",
       subtitle = "1 point = 1 NA",
       x = "Date",
       y = "Site",
       color = "Site") +
  
  # Autres paramètres
  theme(legend.position = "none",plot.margin = margin(20, 20, 20, 20, "pt"))+
  guides(color = "none")+
  theme_classic()

plot1
plot2

