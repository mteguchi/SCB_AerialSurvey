#CC_shipLineTransect


rm(list=ls())

library(readr)
library(tidyverse)
library(lubridate)

source('CcSCB_functions.R')

Cc_cols <- cols(ID = col_integer(),
                Date_Observed = col_datetime('%m/%d/%Y %H:%M:%S'),
                Species_Code = col_character(),
                CommonName = col_character(),
                Latitude = col_double(),
                Longitude = col_double(),
                Latitude_Precision = col_double(),
                Longitude_Precision = col_double(),
                Latitude_Precision_Units = col_character(),
                Longitude_Precision_Units = col_character(),
                Edit_Date = col_datetime('%m/%d/%Y %H:%M:%S'),
                Distance = col_double(),
                Bearing = col_double(),
                Observation_Description = col_character(),
                Cruise_Number = col_character())

Cc.sightings <- readr::read_delim(file = 'Data/Cc Sightings Query 20Mar2017.txt',
                                  delim = ",",
                                  quote = "\"",
                                col_types = Cc_cols)

Cc.sightings <- subset(Cc.sightings, Longitude > -127.5 &
                         Longitude < -115 & Latitude > 28 &
                         Latitude < 36)

## extract public sightings:
# get rid of ones with cruise number and aerial survey
Cc.public.sightings <- subset(Cc.sightings, is.na(Cruise_Number)) %>%
  subset(., Observation_Description != "Aerial Survey") %>%
  subset(., Observation_Description != "Aerial survey sighting")

# remove the CalCOFI sightings
Cc.public.sightings <- Cc.public.sightings[-grep('CalCOFI',
                                                  Cc.public.sightings$Observation_Description),]



# extract sighitngs sith distance/bearing only here
# there is one sighting with just bearing and no distance - remove that too
Cc.ship.LT.sightings <- na.omit(subset(Cc.sightings,
                                    !is.na(Bearing)))

# remove sightings from 90 to 270
Cc.ship.LT.sightings <- subset(Cc.ship.LT.sightings,
                               Bearing <= 90 | Bearing >= 270)

Cc.ship.LT.sightings$Bearing2 <- Cc.ship.LT.sightings$Bearing
Cc.ship.LT.sightings$Bearing2[Cc.ship.LT.sightings$Bearing2 > 180] <- 360 - Cc.ship.LT.sightings$Bearing2[Cc.ship.LT.sightings$Bearing2 > 180]

Cc.ship.LT.sightings$PerpDistanceKm <- nm2km(Cc.ship.LT.sightings$Distance * sin(deg2rad(Cc.ship.LT.sightings$Bearing2)))

