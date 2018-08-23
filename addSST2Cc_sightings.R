#addSST2Cc_sightings



# adds SST readings for aerial survey sightings from 2015

rm(list=ls())
source('CcSCB_functions.R')

# tmpTurtles includes uh and cms... don't use!
# Cc.raw <- read.csv(file = 'Data/tmpTurtles.txt')
# Cc.raw$Date2 <- mmddyy2date(Cc.raw$Date)

Cc.raw <- mutate(ccData, Date2 = paste(year,
                                       formatC(month, width = 2, flag = '0'),
                                       formatC(day, width = 2, flag = '0'),
                                       sep = '-')) %>%
  dplyr::rename(., Longitude = X, Latitude = Y)

Cc.raw$sst <- NA
k.1 <- 129
for (k in k.1:dim(Cc.raw)[1]){
  xlim <- c(Cc.raw[k, 'Longitude']-0.01, Cc.raw[k, 'Longitude']+0.01)
  ylim <- c(Cc.raw[k, 'Latitude']-0.01, Cc.raw[k, 'Latitude']+0.01)

  tlim <- c(Cc.raw[k, 'Date2'], Cc.raw[k, 'Date2'])

  file.name <- paste0('Data/sst_data/Cc_location_', xlim[1], '_',
                      ylim[1], '_', tlim[1], '.nc')

  if (!file.exists(file.name)){
    sstURL <-paste0('https://coastwatch.pfeg.noaa.gov/erddap/griddap/',
                    'jplMURSST41.nc?analysed_sst[(',
                    tlim[1], '):1:(', tlim[2], ')][(',
                    ylim[1], '):(', ylim[2], ')][(',
                    xlim[1], '):(', xlim[2], ')]')

    download.file(sstURL,
                  destfile= file.name,
                  mode='wb')
  }

  test <- nc_open(file.name)

  Cc.raw[k, 'sst'] <- mean(ncvar_get(test, 'analysed_sst'), na.rm = T)
  nc_close(test)
}

# join the raw data and sst:
write.csv.rename(Cc.raw,
                 file = 'Data/ccData_sst.csv',
                 quote = F, row.names = F)
