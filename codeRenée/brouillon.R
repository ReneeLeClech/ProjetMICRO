# Brouillon pour la visualisation des données
library(ggplot2)
library(tidyverse)
library(imputeTS) # pour la fonction ggplot_na_distribution
library(cowplot) # pour afficher les grids des données manquantes
library(dplyr)
library(data.table)
library(plotly)
library(lubridate)
library(scales)  # Load the scales package for date_format



data_tot_temporalRH <- read.csv("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/transfer_6340563_files_5e328b85/data_tot_temporalRH.csv", sep=",", stringsAsFactors=TRUE)
# recordingsHJ_RHRECTMP <- read.csv("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/transfer_6340563_files_5e328b85/recordingsHJ_RHRECTMP.csv", sep=";", stringsAsFactors=TRUE)


# # on s'interesse donc à ce jeu de donnée
# data_tot_temporalRH %>% 
#   filter(Site == "RH_03") %>% 
#   ggplot(aes(x = Date_Heure_GMT02, y = RH)) + 
#   geom_point() +
#   labs(title = "Scatter Plot of RH for Site RH_03",
#        x = "Date and Time (GMT+02)",
#        y = "Relative Humidity (%)")
# 
# 
# # première date des mesures pour chaque site:
# data_tot_temporalRH %>%
#   group_by(Site) %>%
#   filter(row_number() == 1) %>%
#   select(Site, Date_Heure_GMT02)


######---------------------------------------
#### Sript Manon Ducrettet:
data_tot_temporalRH <- read.csv("C:/Users/renax/Desktop/ACO/S9/ProjetOFB/transfer_6340563_files_5e328b85/data_tot_temporalRH.csv", sep=",", stringsAsFactors=TRUE)
str(data_tot_temporalRH)
data_tot_temporalRH<-data_tot_temporalRH[,-1]

# gestion des classes des colonnes:
data_tot_temporalRH$Date<-as.Date(data_tot_temporalRH$Date, format="%Y-%m-%d")
data_tot_temporalRH$Date_Heure_GMT02 <- as.POSIXct(data_tot_temporalRH$Date_Heure_GMT02, format = "%Y-%m-%d %H:%M:%S")

dataRH_fac<-c("Site","IdRH","Hour","Month","Day","Year","longitude","latitude")
dataRH_num<-c("Doy","Rosee","RH","Temp_C")

data_tot_temporalRH <- data_tot_temporalRH %>%
  mutate_at(dataRH_fac, list(~as.factor(.)))

data_tot_temporalRH <- data_tot_temporalRH %>%
  mutate_at(dataRH_num, list(~as.numeric(.)))



## Données manquantes:

# creation d'un dataframe DATARANGE avec les dates et les heures sur toutes la période d'experiences:
Date_min<-data_tot_temporalRH[which.min(data_tot_temporalRH$Date_Heure_GMT02),"Date_Heure_GMT02"]
Date_max<-data_tot_temporalRH[which.max(data_tot_temporalRH$Date_Heure_GMT02),"Date_Heure_GMT02"]

DateRange <-as.data.frame(seq(Date_min, Date_max, by = 900)) 
# the minimum unit of the temporal scale is in sec 1 min of recordings 
# every 15 min*60 sec=900 "1 min on 14 min off"
colnames(DateRange)<-c("Date_Heure_GMT02")
DateRange$Month<-month(DateRange$Date_Heure_GMT02)
DateRange$Hour<-hour(DateRange$Date_Heure_GMT02)
DateRange$Year<-year(DateRange$Date_Heure_GMT02)
DateRange$Day<-day(DateRange$Date_Heure_GMT02)
DateRange$Doy<-yday(DateRange$Date_Heure_GMT02)

DateRange$Date_Heure_GMT02<-as.POSIXct(DateRange$Date_Heure_GMT02,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# nombre d'enregistrement par année
DateRangeY<-as.data.frame(table(DateRange$Year))
colnames(DateRangeY)<-c("Date_Heure_GMT02", "Number of 5 min recordings")

# creation des 4 jeux de données pour les 4 sites:
data_tot_temporalRH_site1<-data_tot_temporalRH[which(data_tot_temporalRH$Site=="RH_01"),]
data_tot_temporalRH_site2<-data_tot_temporalRH[which(data_tot_temporalRH$Site=="RH_02"),]
data_tot_temporalRH_site3<-data_tot_temporalRH[which(data_tot_temporalRH$Site=="RH_03"),]
data_tot_temporalRH_site4<-data_tot_temporalRH[which(data_tot_temporalRH$Site=="RH_04"),]

# Fusion du tableau de data range et de notre tableau, afin de voir les enregistrements qui matchent 
# les enregistrements attenuds, et voir où sont les enregistrements manquants:
merge1RH<-merge(DateRange,data_tot_temporalRH_site1,by=c("Date_Heure_GMT02"),all.x = T)
merge2RH<-merge(DateRange,data_tot_temporalRH_site2,by=c("Date_Heure_GMT02"),all.x = T)
merge3RH<-merge(DateRange,data_tot_temporalRH_site3,by=c("Date_Heure_GMT02"),all.x = T)
merge4RH<-merge(DateRange,data_tot_temporalRH_site4,by=c("Date_Heure_GMT02"),all.x = T)

#Plot de la distribution des NA pour chacun des sites

# distribution_NAsite1RH<-
#   ggplot_na_distribution(merge1RH$RH, x_axis_labels =merge1RH$Date_Heure_GMT02,title="",ylab="Relative humidity", color_missing = "black", color_missing_border = "black")
# 
# distribution_NAsite2RH<-
#   ggplot_na_distribution(merge2RH$RH, x_axis_labels =merge2RH$Date_Heure_GMT02,title="",ylab="Relative humidity", color_missing = "black", color_missing_border = "black")
# 
# distribution_NAsite3RH<-
#   ggplot_na_distribution(merge3RH$RH, x_axis_labels =merge3RH$Date_Heure_GMT02,title="",ylab="Relative humidity", color_missing = "black", color_missing_border = "black")
# 
# distribution_NAsite4RH<-
#   ggplot_na_distribution(merge4RH$RH, x_axis_labels =merge4RH$Date_Heure_GMT02,title="",ylab="Relative humidity", color_missing = "black", color_missing_border = "black")

# # plot avec les 4 sites affichés:
# distributionNARH<-plot_grid(distribution_NAsite1RH,distribution_NAsite2RH,distribution_NAsite3RH,distribution_NAsite4RH, labels=c("Site 1", "Site 2","Site 3","Site 4"), ncol = 2, nrow = 2)
# distributionNARH




#Nombre de valeurs manquantes par site:

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



######------- creation de BLOCS de données manquantes:

# SITE 1
# On s'intéresse uniquement au données manquantes
filtered_data_1RH <- merge1RH %>%
  filter(is.na(RH))
# identification du nombre de blocs manquants
filtered_data_1RH <- filtered_data_1RH %>%
  mutate(Block_ID = cumsum(!is.na(RH) & lag(is.na(RH), default = TRUE)))
# on met un format de temps 
reference_time <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
# on cree
filtered_data_1RH <- filtered_data_1RH %>%
  mutate(TimeNumeric = as.numeric(difftime(Date_Heure_GMT02,
                                           reference_time, units = "secs")) / (15 * 60),
         Block_ID = cumsum(c(0, diff(TimeNumeric) >1))+1)
# on met a jout la colonne site
filtered_data_1RH$Site<-c(rep(1,nrow(filtered_data_1RH)))


# SITE 2
filtered_data_2RH <- merge2RH %>%
  filter(is.na(RH))
filtered_data_2RH <- filtered_data_2RH %>%
  mutate(Block_ID = cumsum(!is.na(RH) & lag(is.na(RH), default = TRUE)))
reference_time <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
filtered_data_2RH <- filtered_data_2RH %>%
  mutate(TimeNumeric = as.numeric(difftime(Date_Heure_GMT02,
                                           reference_time, units = "secs")) / (15 * 60),
         Block_ID = cumsum(c(0, diff(TimeNumeric) >1))+1)
filtered_data_2RH$Site<-c(rep(2,nrow(filtered_data_2RH)))


# SITE 3
filtered_data_3RH <- merge3RH %>%
  filter(is.na(RH))
filtered_data_3RH <- filtered_data_3RH %>%
  mutate(Block_ID = cumsum(!is.na(RH) & lag(is.na(RH), default = TRUE)))
reference_time <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
filtered_data_3RH <- filtered_data_3RH %>%
  mutate(TimeNumeric = as.numeric(difftime(Date_Heure_GMT02,
                                           reference_time, units = "secs")) / (15 * 60),
         Block_ID = cumsum(c(0, diff(TimeNumeric) >1))+1)
filtered_data_3RH$Site<-c(rep(3,nrow(filtered_data_3RH)))


# SITE 4
filtered_data_4RH <- merge4RH %>%
  filter(is.na(RH))
filtered_data_4RH <- filtered_data_4RH %>%
  mutate(Block_ID = cumsum(!is.na(RH) & lag(is.na(RH), default = TRUE)))
reference_time <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
filtered_data_4RH <- filtered_data_4RH %>%
  mutate(TimeNumeric = as.numeric(difftime(Date_Heure_GMT02,
                                           reference_time, units = "secs")) / (15 * 60),
         Block_ID = cumsum(c(0, diff(TimeNumeric) >1))+1)
filtered_data_4RH$Site<-c(rep(4,nrow(filtered_data_4RH)))



# Assuming the relevant columns are the same in all data frames
stacked_data <- rbind(filtered_data_1RH,filtered_data_2RH, filtered_data_3RH, filtered_data_4RH)

# View the stacked data
summary(stacked_data)
#--------
# PLot: 
tabforplot<-stacked_data[,c(1,8,10,20)]

# Assuming you have loaded the lubridate library for handling date-time objects

# Define the data range
start_date <- ymd_hms("2018-10-16 15:30:00")
end_date <- ymd_hms("2022-07-09 13:15:00")


# Plot using ggplot2
ggplot(tabforplot, aes(x = Date_Heure_GMT02, y = Site, color = as.factor(Site))) +
  
  # echelle temporelle
  scale_x_datetime(labels = scales::date_format("%Y")) +

  geom_vline(xintercept = as.numeric(as.POSIXct("2018-01-01")), color = "lightblue", linewitdh = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2019-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2022-01-01")), color = "lightblue", linewidth = 0.5, alpha= 0.5) +

  geom_hline(yintercept = 1, color = "darkred", size = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 2, color = "darkgreen", size = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 3, color = "darkblue", size = 0.5, linetype="dashed", alpha= 0.5) +
  geom_hline(yintercept = 4, color = "darkorchid4", size = 0.5, linetype="dashed", alpha= 0.5) +
  
    
  geom_point(size = 4) +
  labs(title = "Répartition temporelle des NAs pour l'humidité sur les différents sites",
       subtitle = "1 point = 1 NA",
       x = "Date",
       y = "Site",
       color = "Site") +
  
  theme(legend.position = "none",plot.margin = margin(20, 20, 20, 20, "pt"))+
  guides(color = "none")+
  theme_classic()


