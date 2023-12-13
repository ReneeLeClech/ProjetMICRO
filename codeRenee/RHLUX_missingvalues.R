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
####--------

####-------------------------- 1. Data range de RH  -------------------------------------

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


# Création d'un dataframe DATARANGE avec les dates et les heures sur toute la période d'expériences
RH_Date_min <- min(data_tot_temporalRH$Date_Heure_GMT02)
RH_Date_max <- max(data_tot_temporalRH$Date_Heure_GMT02)


####-------------------------- 2. Data range de LUX  -------------------------------------

# Utilisation de fread pour lire le fichier CSV
data_tot_temporalLUX <- fread("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/transfer_6340563_files_5e328b85/data_tot_temporalLUX.csv", header = TRUE, sep = ",")

data_tot_temporalLUX <- data_tot_temporalLUX %>%
  rename(LUX = Intensity_lux)

# Gestion des classes des colonnes
data_tot_temporalLUX[, Date := as.Date(Date, format = "%Y-%m-%d")]
data_tot_temporalLUX[, Date_Heure_GMT02 := as.POSIXct(Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")]

dataLUX_fac <- c("Site", "IdLUX", "Hour", "Month", "Day", "Year", "longitude", "latitude")
dataLUX_num <- c("Doy", "LUX", "Temp_C")

# Utilisation de data.table pour convertir les colonnes en facteurs et numériques
data_tot_temporalLUX[, (dataLUX_fac) := lapply(.SD, as.factor), .SDcols = dataLUX_fac]
data_tot_temporalLUX[, (dataLUX_num) := lapply(.SD, as.numeric), .SDcols = dataLUX_num]


# Création d'un dataframe DATARANGE avec les dates et les heures sur toute la période d'expériences
LUX_Date_min <- min(data_tot_temporalLUX$Date_Heure_GMT02)
LUX_Date_max <- max(data_tot_temporalLUX$Date_Heure_GMT02)

####-------------------------- 3. Data Range  -------------------------------------
print(paste("RH:  min=", RH_Date_min, "max=",RH_Date_max))
print(paste("RH:  min=", LUX_Date_min, "max=",LUX_Date_max))

## On refais le Datarange:

Date_min=max(RH_Date_min,LUX_Date_min)
Date_max=min(RH_Date_max,LUX_Date_max)

DateRange <- data.table(Date_Heure_GMT02 = seq(from = Date_min, to = Date_max, by = 900))


# Ajout des colonnes supplémentaires
DateRange[, `:=`(Month = month(Date_Heure_GMT02),
                 Hour = hour(Date_Heure_GMT02),
                 Year = year(Date_Heure_GMT02),
                 Day = day(Date_Heure_GMT02),
                 Doy = yday(Date_Heure_GMT02))]

# Nombre d'enregistrements par année
DateRangeY <- as.data.table(table(Year = DateRange$Year))


# Creation des 4 jeux de donées:

# Création des 4 jeux de données pour les 4 sites et fusion avec DateRange
merge1LUX <- DateRange %>% left_join(filter(data_tot_temporalLUX, Site == "LUX_01"), by = "Date_Heure_GMT02")
merge2LUX <- DateRange %>% left_join(filter(data_tot_temporalLUX, Site == "LUX_02"), by = "Date_Heure_GMT02")
merge3LUX <- DateRange %>% left_join(filter(data_tot_temporalLUX, Site == "LUX_03"), by = "Date_Heure_GMT02")
merge4LUX <- DateRange %>% left_join(filter(data_tot_temporalLUX, Site == "LUX_04"), by = "Date_Heure_GMT02")

# Création des 4 jeux de données pour les 4 sites et fusion avec DateRange
merge1RH <- DateRange %>% left_join(filter(data_tot_temporalRH, Site == "RH_01"), by = "Date_Heure_GMT02")
merge2RH <- DateRange %>% left_join(filter(data_tot_temporalRH, Site == "RH_02"), by = "Date_Heure_GMT02")
merge3RH <- DateRange %>% left_join(filter(data_tot_temporalRH, Site == "RH_03"), by = "Date_Heure_GMT02")
merge4RH <- DateRange %>% left_join(filter(data_tot_temporalRH, Site == "RH_04"), by = "Date_Heure_GMT02")



######------------------ 2. Nombre de données manquantes par site:---------------

# Count the number of missing values for each site
missing_counts_LUX <- c(
  sum(is.na(merge1LUX$LUX)),
  sum(is.na(merge2LUX$LUX)),
  sum(is.na(merge3LUX$LUX)),
  sum(is.na(merge4LUX$LUX))
)

par(mar = c(5, 5, 2, 2))  # Réduit les marges (bottom, left, top, right)

barplot(
  missing_counts_LUX,
  names.arg = c("site 1", "site 2", "site 3", "site 4"),
  main = "LUX: Nombre de données manquantes par site",
  xlab = "",
  ylab = "Nombre de données manquantes",
  col = "khaki",
  ylim = c(0, max(missing_counts_LUX) + 5000)
)

text(seq_along(missing_counts_LUX), missing_counts_LUX, labels = missing_counts_LUX, pos = 3, col = "yellow4", cex = 1.2)

# Remettre les marges par défaut après le tracé du graphique
par(mar = c(5, 4, 4, 2) + 0.1)


#####---------- RH:
# Count the number of missing values for each site
missing_counts_RH <- c(
  sum(is.na(merge1RH$RH)),
  sum(is.na(merge2RH$RH)),
  sum(is.na(merge3RH$RH)),
  sum(is.na(merge4RH$RH))
)

par(mar = c(5, 5, 2, 2))  # Réduit les marges (bottom, left, top, right)

barplot(
  missing_counts_RH,
  names.arg = c("site 1", "site 2", "site 3", "site 4"),
  main = "RH: Nombre de données manquantes par site",
  xlab = "",
  ylab = "Nombre de données manquantes",
  col = "skyblue",
  ylim = c(0, max(missing_counts_RH) + 5000)
)

text(seq_along(missing_counts_RH), missing_counts_RH, labels = missing_counts_RH, pos = 3, col = "darkblue", cex = 1.2)


######------------------ 3. LUX: creation de BLOCS de données manquantes----------------

# Fonction pour créer les blocs de données manquantes pour un site donné
create_filtered_data_LUX <- function(merge_data, site) {
  filtered_data <- merge_data %>%
    filter(is.na(LUX)) %>%
    # identification du nombre de blocs manquants
    mutate(Block_ID = cumsum(!is.na(LUX) & lag(is.na(LUX), default = TRUE)),
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

# plot1
# plot2

grid.arrange(plot1, plot2)


