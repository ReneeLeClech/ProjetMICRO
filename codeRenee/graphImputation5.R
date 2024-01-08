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
###-------------------------------


##----------------- Importation des données et gestion des classes: --------------

tab_indiv_date<- readRDS("data/tab_indiv_date.rds")
tab_indiv_date<-as.data.table(tab_indiv_date)

# Les variables liées au hobo LUX du site 2 et du hobo RH du site 4 contiennent trop de NA: elles sont retirées du jdd
tab_indiv_date <- subset(tab_indiv_date, select= -c(RH_04,Rosee_04,Temp_RH_04,Intensity_lux_02,Temp_LUX_02)) 
colnames(tab_indiv_date)

# Gestion des classes des colonnes
tab_indiv_date[, Date_Heure_GMT02 := as.POSIXct(Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")]

data_fac <- c("Hour", "Month", "Day", "Year","Doy")
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
    
  #print(bloc_data$Date_Heure_GMT02[start_index])
}
list_dates[[i+1]]<-as.POSIXct(bloc_data$Date_Heure_GMT02[end_index], format = "%Y-%m-%d %H:%M:%S")
list_dates

# Utilisation de la fonction sub pour supprimer la partie de l'heure
list_dates <- lapply(list_dates, function(date) {
  sub(" [0-9]{2}:[0-9]{2}:[0-9]{2} GMT$", "", date)
})

list_dates <- lapply(list_dates, function(date) {
  substr(date, 1, 10)
})


##-----
data_all_RH_LUX<- readRDS("data/cleaned_data_ALL.rds")
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

merge1LUX <- as.data.table(merge1)[, .(Intensity_lux, Date_Heure_GMT02)]
merge2LUX <- as.data.table(merge2)[, .(Intensity_lux, Date_Heure_GMT02)]
merge3LUX <- as.data.table(merge3)[, .(Intensity_lux, Date_Heure_GMT02)]
merge4LUX <- as.data.table(merge4)[, .(Intensity_lux, Date_Heure_GMT02)]


merge1RH <- as.data.table(merge1)[, .(RH, Date_Heure_GMT02)]
merge2RH <- as.data.table(merge2)[, .(RH, Date_Heure_GMT02)]
merge3RH <- as.data.table(merge3)[, .(RH, Date_Heure_GMT02)]
merge4RH <- as.data.table(merge4)[, .(RH, Date_Heure_GMT02)]

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

tabforplot_LUX <- subset(tabforplot_LUX, Site != 2)

plot1<- ggplot(tabforplot_LUX, aes(x = Date_Heure_GMT02, y = Site, color = as.factor(Site))) +
  
  # echelle temporelle
  scale_x_datetime(labels = scales::date_format("%Y"), limits = c(Date_min, Date_max)) +
  # echelle site:
  scale_y_continuous(breaks = c(1,3,4)) + 
  # mise en avant des années
  geom_vline(xintercept = as.numeric(as.POSIXct("2018-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2019-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2022-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[1]])), color = "darkblue", linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[2]])), color = "darkblue", linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[3]])), color = "darkblue", linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[4]])), color = "darkblue", linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[5]])), color = "darkblue", linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[6]])), color = "darkblue", linewidth = 0.9, alpha= 0.5) +
  # 
  #Traits pour les sites
  geom_hline(yintercept = 1, color = "darkred", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  # geom_hline(yintercept = 2, color = "darkgreen", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
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
  theme(legend.position = "none")+
  guides(color = "none")+
  theme_classic()


plot1
#--------RH
# Supprimer les lignes où Site est égal à 4
tabforplot_RH <- subset(tabforplot_RH, Site != 4)

plot2<- ggplot(tabforplot_RH, aes(x = Date_Heure_GMT02, y = Site, color = as.factor(Site))) +
  
  # echelle temporelle
  scale_x_datetime(labels = scales::date_format("%Y"),limits = c(Date_min, Date_max)) +
  # echelle des sites
  scale_y_continuous(breaks = seq(1, 3, by = 1)) + 
  # mise en avant des années
  geom_vline(xintercept = as.numeric(as.POSIXct("2018-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2019-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2022-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[1]])), color = "darkblue",  linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[2]])), color = "darkblue",  linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[3]])), color = "darkblue",  linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[4]])), color = "darkblue",  linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[5]])), color = "darkblue",  linewidth = 0.9, alpha= 0.5) +
  # geom_vline(xintercept = as.numeric(as.POSIXct(list_dates[[6]])), color = "darkblue",  linewidth = 0.9, alpha= 0.5) +

  
  #Traits pour les sites
  geom_hline(yintercept = 1, color = "darkred", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 2, color = "darkgreen", linewidth = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 3, color = "darkblue", linewidth = 0.5, linetype="dashed", alpha= 0.5) +

  
  # Ajout des NA
  geom_point(size = 4) +
  
  # Titre, sous-titre et axes
  labs(title = "Répartition temporelle des NAs de l'humidité sur les différents sites",
       subtitle = "1 point = 1 NA",
       x = "Date",
       y = "Site",
       color = "Site") +
  
  # Autres paramètres
  theme(legend.position = "none")+
  guides(color = "none")+
  theme_classic()

plot1
# plot2

grid.arrange(plot1,plot2)

