#Sightings2database


# converts simple sightings data from aerial survey to
# sightings database format for easy uploading

# Tomo Eguchi
# 22 February 2017

rm(list=ls())
sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

library(readr)
library(lubridate)

path2file <- 'Data/tmpTurtles.txt'
path2SpTable <- 'Data/dbo_tblSpecies.txt'

col.types.in <- cols(Date = col_character(),
                     Time = col_character(),
                     Effort = col_integer(),
                     Sp = col_character(),
                     Size = col_character(),
                     Number = col_integer(),
                     Angle = col_double(),
                     Condition = col_character(),
                     Beaufort = col_integer(),
                     Latitude = col_double(),
                     Longitude = col_double(),
                     Line = col_character(),
                     Depth = col_double(),
                     PerpDist = col_double())

data.raw <- readr::read_csv(path2file, col_types = col.types.in)
data.raw$Date <- lubridate::mdy(data.raw$Date)

data.raw$Date2 <- paste0(formatC(YMD2Y(data.raw$Date),
                                 width = 2, flag = '0'),
                         formatC(YMD2m(data.raw$Date),
                                 width = 2, flag = '0'),
                         formatC(YMD2d(data.raw$Date),
                                 width = 2, flag = '0'))

data.raw$Sp <- toupper(data.raw$Sp)

# read the species table and look up appropriate species code:
col.types.SpTable <- cols(ID = col_integer(),
                          TaxanomicOrder = col_character(),
                          SubOrder = col_character(),
                          InfraOrder = col_character(),
                          Family = col_character(),
                          FamilyCommonName = col_character(),
                          Genus = col_character(),
                          Species = col_character(),
                          Subspecies = col_character(),
                          CommonName = col_character(),
                          NomenclatureAuthority = col_character(),
                          SpType = col_character(),
                          SpName = col_character(),
                          CetaceanCode = col_character(),
                          AerialFisheryCode = col_character(),
                          BirderCode = col_character(),
                          AlternateCode1 = col_character(),
                          AlternateCode2 = col_character(),
                          Comments = col_character(),
                          EditDate = col_datetime(format = '%m/%d/%Y %H:%M:%S'),
                          EditUserID = col_character(),
                          RecordCreationDate = col_character())

# needed to use ';' as the delimiter as a field contains commas. Also,
# needed to delete ';' on line 116 in a field.
sp.table <- readr::read_csv2(path2SpTable,
                             col_types = col.types.SpTable)

#sp.table$CetaceanCode <- as.factor(sp.table$CetaceanCode)
#sp.table$AerialFisheryCode <- as.factor(sp.table$AerialFisheryCode)

### Using pipes and right join to do many to many merging:
# Pull out just ID and AerialfisheryCode from the species table
# remove NA entries - select seems to pull out all NA rows
# join sp.table (x) that becomes x in right_join, and data.raw (which is y and 'right')
# join left one (selected sp.table) to the right one (data.raw) using 'AerialFisheryCode'
# and 'Sp' from the two, respectively. Left and right sides in the by input is
# important. Then rename ID into Sp_ID - the new name comes first, kinda strange.
data.raw <- dplyr::select(sp.table, ID, AerialFisheryCode) %>%
  na.omit() %>%
  dplyr::right_join(y = data.raw, by = c('AerialFisheryCode' = 'Sp')) %>%
  dplyr::rename(Sp_ID = ID)

# fill in the sightings numbers:
unique.dates <- unique(data.raw$Date2)
data.raw$Sighting_Number <- 0
for (k in 1:length(unique.dates)){
  tmp <- subset(data.raw, Date2 == unique.dates[k])
  for (k1 in 1:nrow(tmp)){
    tmp$Sighting_Number[k1] <- paste0(unique.dates[k], '-', k1)
  }
  data.raw[data.raw$Date2 == unique.dates[k], 'Sighting_Number'] <- tmp$Sighting_Number
}

data.raw$Time <- hhmmss2hms(data.raw$Time)

df.out <- tibble(Cruise_Number = NA,
                 Observer_Number = NA,
                 Effort = NA,
                 Water_Temperature = NA,
                 Bearing = NA,
                 Distance = NA,
                 Animal_Count = data.raw$Number,
                 JFR = NA,
                 Reticle = NA,
                 Captured = NA,
                 Date_Observed = format(data.raw$Date, '%m/%d/%Y'),
                 Date_Approximate = 'N',
                 Time_Observed = paste('"', data.raw$Time, '"', sep = ""),
                 Sighting_Number = data.raw$Sighting_Number,
                 Observed_By = NA,
                 Observed_By_Type = NA,
                 Observed_By_Organization_ID = NA,
                 Observed_By_Phone = NA,
                 Observed_By_Email = NA,
                 Diagnostic_Features = NA,
                 Observation_Description = paste0('"', 'Aerial Survey', '"'),
                 Behavior = NA,
                 Injuries_Description = NA,
                 Entanglement_Description = NA,
                 Estimated_Size = NA,
                 Length_Of_Time = NA,
                 Species_ID = data.raw$Sp_ID,
                 Digital_Photos_Taken = NA,
                 Species_Verified_By = NA,
                 Biopsy = NA,
                 Age = NA,
                 Sex = NA,
                 Tail_Beyond_Carapace = NA,
                 Tags_Description = NA,
                 Tag_Scars_Description = NA,
                 Final_Disposition = NA,
                 Island_ID = NA,
                 City_ID = NA,
                 County_ID = NA,
                 State_ID = NA,
                 Country_ID = NA,
                 Locality_Details = NA,
                 Latitude = data.raw$Latitude,
                 Latitude_Precision = 0.00001,
                 Latitude_Precision_Units = 'Degree',
                 Longitude = data.raw$Longitude,
                 Longitude_Precision = 0.00001,
                 Longitude_Precision_Units = 'Degree',
                 Lat_Long_Determined_By = 'GPS')

write_csv(df.out, path = 'Data/aerial_sightings_Feb2017.csv')

# After exporting, use a text editor to remove all NAs also the precisions need to be changed
# from 1e-5 to 0.00001

# col.types.out <- cols(Cruise_Number = col_integer(),
#                       Observer_Number = col_integer(),
#                       Effort = col_character(),
#                       Water_Temperature = col_double(),
#                       Bearing = col_integer(),
#                       Distance = col_double(),
#                       Animal_Count = col_integer(),
#                       JFR = col_character(),
#                       Reticle = col_integer(),
#                       Captured = col_character(),
#                       Date_Observed = col_datetime('%m/%d/%Y'),
#                       Time_Observed = col_time('%H:%M:%S'),
#                       Sighting_Number = col_integer(),
#                       Observed_By = col_character(),
#                       Observed_By_Type = col_character(),
#                       Observed_By_Organization_ID = col_integer(),
#                       Observed_By_Phone = col_character(),
#                       Observed_By_Email = col_character(),
#                       Diagnostic_Features = col_character(),
#                       Observation_Description = col_character(),
#                       Behavior = col_character(),
#                       Injuries_Description = col_character(),
#                       Entanglement_Description = col_character(),
#                       Estimated_Size = col_character(),
#                       Length_Of_Time = col_character(),
#                       Species_ID = col_character(),
#                       Digital_Photos_Taken = col_character(),
#                       Species_Verified_By = col_character(),
#                       Biopsy = col_character(),
#                       Age = col_character(),
#                       Sex = col_character(),
#                       Tail_Beyond_Carapace = col_character(),
#                       Tags_Description = col_character(),
#                       Tag_Scars_Description = col_character(),
#                       Final_Disposition = col_character(),
#                       Island_ID = col_integer(),
#                       City_ID = col_integer(),
#                       County_ID = col_integer(),
#                       State_ID = col_integer(),
#                       Country_ID = col_integer(),
#                       Locality_Details = col_character(),
#                       Latitude = col_double(),
#                       Latitude_Precision = col_double(),
#                       Latitude_Precision_Units = col_character(),
#                       Longitude = col_double(),
#                       Longitude_Precision = col_double(),
#                       Longitude_Precision_Units = col_character(),
#                       Lat_Long_Determined_By = col_character())

# create an empty data frame:
#df.out <- tibble(nrows = nrow(data.raw), col_types = col.types.out)

