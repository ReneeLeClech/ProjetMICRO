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


######------- 2. Nombre de données manquantes par site:----------------------------------------------------

# Count the number of missing values for each site
missing_counts <- c(
  sum(is.na(merge1RH$RH)),
  sum(is.na(merge2RH$RH)),
  sum(is.na(merge3RH$RH)),
  sum(is.na(merge4RH$RH))
)

missing_counts
# Create a bar plot
# Set a y-axis limit (adjust the value as needed)
ylim <- max(missing_counts) + 5000

# Create a bar plot with y-axis limit
barplot(missing_counts, names.arg = c("site 1", "site 2", "site 3", "site 4"), 
        main = "Nombre de données manquantes par site",
        xlab = "", ylab = "Nombre de données manquantes",
        col = "skyblue", ylim = c(0, ylim))
text(seq_along(missing_counts), missing_counts, labels = missing_counts, pos = 3, col = "darkblue", cex = 1.2)



######------- 3. creation de BLOCS de données manquantes:------------------------------------------

# Fonction pour créer les blocs de données manquantes pour un site donné
create_filtered_data <- function(merge_data, site) {
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
filtered_data_list <- list()

# Application de la fonction pour trouver les blocs des données manquantes
for (site in 1:4) {
  site_data <- get(paste0("merge", site, "RH"))
  filtered_data_list[[site]] <- create_filtered_data(site_data, site)
}

# Empilement des données
stacked_data <- rbindlist(filtered_data_list)

# Plot
tabforplot <- stacked_data[, .(Date_Heure_GMT02, Block_ID, Site)]

# Define the data range
start_date <- ymd_hms("2018-10-16 15:30:00")
end_date <- ymd_hms("2022-07-09 13:15:00")



## ------------------ 4. plot des bloc de données manquantes -------------------
# Plot using ggplot2
ggplot(tabforplot, aes(x = Date_Heure_GMT02, y = Site, color = as.factor(Site))) +
  
  # echelle temporelle
  scale_x_datetime(labels = scales::date_format("%Y")) +
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
  labs(title = "Répartition temporelle des NAs pour l'humidité sur les différents sites",
       subtitle = "1 point = 1 NA",
       x = "Date",
       y = "Site",
       color = "Site") +
  
  # Autres paramètres
  theme(legend.position = "none",plot.margin = margin(20, 20, 20, 20, "pt"))+
  guides(color = "none")+
  theme_classic()



## ---------------------------- 5. Variation de l'humidité par Jour

# on commence par aleger le jeu de données:

# Créer le data frame RHdata
RHdata <- data_tot_temporalRH %>%
  select(RH, Date_Heure_GMT02, Site, Month, Date,) %>%
  rename(Mois = Month)

# Convertir Date_Heure_GMT02 en Date et Heure:Minute
RHdata$Date_Heure_GMT02 <- as.POSIXct(RHdata$Date_Heure_GMT02)
RHdata$Hour_Minute <- format(RHdata$Date_Heure_GMT02, "%H:%M")

# Calculer la moyenne et l'écart-type par heure:minute et par site
RHdata_hourly <- RHdata %>%
  group_by(Site, Hour_Minute) %>%
  summarise(Mean_RH = mean(RH, na.rm = TRUE),
            SD_RH = sd(RH, na.rm = TRUE))

# Définir les couleurs pour chaque site
site_colors <- c("RH_01" = "darkred", "RH_02" = "darkgreen", "RH_03" = "darkblue", "RH_04" = "darkorchid4")

# Créer un graphique ggplot avec la moyenne et l'écart-type
RHdata_hourly %>%
  ggplot(aes(x = as.POSIXct(Hour_Minute, format = "%H:%M"), y = Mean_RH, ymin = Mean_RH - SD_RH, ymax = Mean_RH + SD_RH, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Valeur moyenne de l'humidité relative sur tous les jours de l'année avec écart type",
       x = "Heure",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  scale_x_datetime(date_labels = "%H", date_breaks = "2 hour") +  # Formater l'axe horizontal
  theme_minimal()


## ---------------------------- 6. Variation de l'humidité par Saison

#----- Hiver
RHdata_winter <- RHdata %>%
  # période du 15 novembre au 14 février
  filter((month(Date) == 11 & day(Date) >= 15) | 
           (month(Date) == 12) |
           (month(Date) == 1) |
           (month(Date) == 2 & day(Date) <= 14)) %>% 
  # on regroupe les données qui ont les meme heures: minutes dans al journée ET par site
  group_by(Site, Hour_Minute) %>%
  # Calcul de la moyenne et de l'écart-type par heure:minute et par site
  summarise(Mean_RH = mean(RH, na.rm = TRUE),
            SD_RH = sd(RH, na.rm = TRUE))

# Créer un graphique ggplot avec les facettes pour la période d'hiver
RHdata_winter %>%
  ggplot(aes(x = as.POSIXct(Hour_Minute, format = "%H:%M"), y = Mean_RH, ymin = Mean_RH - SD_RH, ymax = Mean_RH + SD_RH, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Hiver",
       subtitle="Valeur moyenne de l'humidité relative entre le 15 novembre et le 14 février",
       x = "Heure",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  scale_x_datetime(date_labels = "%H", date_breaks = "2 hour") +
  theme_minimal()


#----- Printemp

# Filtrer les données pour la période du 15 février au 14 mai
RHdata_spring <- RHdata %>%
  filter((month(Date) == 2 & day(Date) >= 15) | 
           (month(Date) == 3) |
           (month(Date) == 4) |
           (month(Date) == 5 & day(Date) <= 14)) %>% 
  group_by(Site, Hour_Minute) %>%
  summarise(Mean_RH = mean(RH, na.rm = TRUE),
            SD_RH = sd(RH, na.rm = TRUE))

# Créer un graphique ggplot avec les facettes pour la période d'hiver
RHdata_spring %>%
  ggplot(aes(x = as.POSIXct(Hour_Minute, format = "%H:%M"), y = Mean_RH, ymin = Mean_RH - SD_RH, ymax = Mean_RH + SD_RH, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Printemps",
       subtitle="Valeur moyenne de l'humidité relative entre le 15 février au 14 mai",
       x = "Heure",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  scale_x_datetime(date_labels = "%H", date_breaks = "2 hour") +
  theme_minimal()

#----- Ete

# Filtrer les données pour la période du 15 mai au 14 aout
RHdata_summer<- RHdata %>%
  filter((month(Date) == 5 & day(Date) >= 15) | 
           (month(Date) == 6) |
           (month(Date) == 7) |
           (month(Date) == 8 & day(Date) <= 14)) %>% 
  group_by(Site, Hour_Minute) %>%
  summarise(Mean_RH = mean(RH, na.rm = TRUE),
            SD_RH = sd(RH, na.rm = TRUE))

# Créer un graphique ggplot avec les facettes pour la période d'hiver
RHdata_summer %>%
  ggplot(aes(x = as.POSIXct(Hour_Minute, format = "%H:%M"), y = Mean_RH, ymin = Mean_RH - SD_RH, ymax = Mean_RH + SD_RH, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Eté",
       subtitle="Valeur moyenne de l'humidité relative entre le 15 mai au 14 aout",
       x = "Heure",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  scale_x_datetime(date_labels = "%H", date_breaks = "2 hour") +
  theme_minimal()



#----- Automne

# Filtrer les données pour la période du 15 aout au 14 novembre
RHdata_fall <- RHdata %>%
  filter((month(Date) == 8 & day(Date) >= 15) | 
           (month(Date) == 9) |
           (month(Date) == 10) |
           (month(Date) == 11 & day(Date) <= 14)) %>% 
  group_by(Site, Hour_Minute) %>%
  summarise(Mean_RH = mean(RH, na.rm = TRUE),
            SD_RH = sd(RH, na.rm = TRUE))

# Créer un graphique ggplot avec les facettes pour la période d'hiver
RHdata_fall %>%
  ggplot(aes(x = as.POSIXct(Hour_Minute, format = "%H:%M"), y = Mean_RH, ymin = Mean_RH - SD_RH, ymax = Mean_RH + SD_RH, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Automne",
       subtitle="Valeur moyenne de l'humidité relative entre le 15 aout au 14 novembre",
       x = "Heure",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  scale_x_datetime(date_labels = "%H", date_breaks = "2 hour") +
  theme_minimal()

## ---------------------------- 7. Variation de l'humidité annuelle:
sum(is.na(data_tot_temporalRH$RH))
# on commence par aleger le jeu de données:
RHdatayear <- data_tot_temporalRH %>%
  select(RH, Date_Heure_GMT02, Site, Month, Date,Doy)

RHdatayear <- na.omit(RHdatayear)
sum(is.na(RHdatayear$RH))
# Convertir Date_Heure_GMT02 en Date et Heure:Minute
RHdatayear$Date_Heure_GMT02 <- as.POSIXct(RHdatayearly$Date_Heure_GMT02)
RHdatayear$Hour_Minute <- format(RHdatayearly$Date_Heure_GMT02, "%H:%M")
RHdatayear$Doy <- as.factor(RHdatayearly$Doy)



# Calculer la moyenne et l'écart-type par heure:minute et par site
RHdatayearly <- RHdatayear %>%
  group_by(Site, Doy, Hour_Minute) %>%
  summarise(Mean_RH = mean(RH, na.rm = TRUE),
            SD_RH = sd(RH, na.rm = TRUE))

# Définir les couleurs pour chaque site
site_colors <- c("RH_01" = "darkred", "RH_02" = "darkgreen", "RH_03" = "darkblue", "RH_04" = "darkorchid4")

# Créer un graphique ggplot avec la moyenne et l'écart-type
RHdatayearly %>%
  ggplot(aes(x = Doy, y = Mean_RH, ymin = Mean_RH - SD_RH, ymax = Mean_RH + SD_RH, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.1) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Valeur moyenne de l'humidité relative sur tous les jours de l'année avec écart type",
       x = "Jour de l'année",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  theme_minimal()


####--------------- test -------------------------------------------------------


RHdataTEST <- RHdatayear %>%
  group_by(Site, Doy) %>%
  summarise(Mean_RH_day = mean(RH, na.rm = TRUE),
            SD_RH_day = sd(RH, na.rm = TRUE))

# Créer un graphique ggplot avec la moyenne et l'écart-type
RHdataTEST %>%
  ggplot(aes(x = Doy, y = Mean_RH_day, ymin = Mean_RH_day - SD_RH_day, ymax = Mean_RH_day + SD_RH_day, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = 0.1) +
  facet_wrap(~ Site, labeller = labeller(Site = c("RH_01" = "Site 1", "RH_02" = "Site 2", "RH_03" = "Site 3", "RH_04" = "Site 4"))) +
  labs(title = "Valeur moyenne de l'humidité relative sur tous les jours de l'année avec écart type",
       x = "Jour de l'année",
       y = "Humidité relative") +
  scale_color_manual(values = site_colors) + 
  theme_minimal()

  
  



