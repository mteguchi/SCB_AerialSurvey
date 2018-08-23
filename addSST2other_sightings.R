#addSST2other_sightings


stop('Do not use this scipt - use getSSTatSightings_MMsurvey.R or getSSTatSightings_hotline.R')
# adds SST readings for other Cc sightings

rm(list=ls())
source('CcSCB_functions.R')

#YMD2Y is in TomosFunctions.R
Cc_HotlineSightings <- read.csv(file = 'Data/HotlineSightings_Cc.csv') %>%
  rename(., Date = Date_Observed) %>%
  mutate(., Year = YMD2Y(Date)) %>%
  select(., Date, Latitude, Longitude, Year, Genus)

Cc_CruiseSightings <- read.csv(file = 'Data/CruiseSightings_Cc.csv') %>%
  rename(., Date = Date_Observed) %>%
  mutate(., Year = YMD2Y(Date)) %>%
  select(., Date, Latitude, Longitude, Year, Genus)

# DGN_bycatch <- read.csv('Data/CC_bycatch_DGN.csv') %>%
#   filter(., !is.na(LatDD1)) %>%
#   mutate(., Date = paste(Year,
#                          formatC(MM, width = 2, flag = 0),
#                          formatC(DD, width = 2, flag = 0),
#                          sep = '-'),
#          Genus = "Caretta") %>%
#   select(., Date, LatDD1, LonDD1, Year, Genus) %>%
#   rename(., Latitude = LatDD1, Longitude = LonDD1)

Cc_sightings.raw <- rbind(Cc_HotlineSightings, Cc_CruiseSightings)


#summary(Cc_bycatch)
Cc_sightings.raw$sst <- NA
k.1 <- 144
for (k in k.1:dim(Cc_sightings.raw)[1]){

  xlim <- c(Cc_sightings.raw[k, 'Longitude']-0.01,
            Cc_sightings.raw[k, 'Longitude']+0.01)
  ylim <- c(Cc_sightings.raw[k, 'Latitude']-0.01,
            Cc_sightings.raw[k, 'Latitude']+0.01)

  tlim <- c(as.character(Cc_sightings.raw[k, 'Date']),
            as.character(Cc_sightings.raw[k, 'Date']))

  file.name <- paste0('Data/sst_data/Cc_location_nonAerial_',
                      formatC(k, width = 3, flag = '0'), '.nc')

  if (as.Date(Cc_sightings.raw[k, 'Date']) > as.Date('2002-01-01')){
    sstURL <-paste0('https://coastwatch.pfeg.noaa.gov/erddap/griddap/',
                    'jplMURSST41.nc?analysed_sst[(',
                    tlim[1], '):1:(', tlim[2], ')][(',
                    ylim[1], '):(', ylim[2], ')][(',
                    xlim[1], '):(', xlim[2], ')]')
    varname <- 'analysed_sst'

  } else {
    # this one is 0.0417 degree resolutions... change!
    # SEE getSSSTatSightings_MMsurvey.R and getSSTatSightings_hotline.R
    # these have been fixed.
    'http://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiPH53sstd1day.nc?sea_surface_temperature[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)],dt_analysis[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)],wind_speed[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)],sea_ice_fraction[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)],quality_level[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)],pathfinder_quality_level[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)],l2p_flags[(2014-12-31T12:00:00Z):1:(2014-12-31T12:00:00Z)][(89.97917):1:(-89.97916)][(-179.9792):1:(179.9792)]'

    sstURL <-paste0('http://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst2Agg.nc?sst[(',
                    tlim[1], 'T00:00:00Z):1:(', tlim[2],
                    'T00:00:00Z)][(0.0):1:(0.0)][(',
                    signif(min(ylim) - 0.125, digits = 4), '):1:(',
                    signif(max(ylim)+0.125, digits = 4), ')][(',
                    signif(min(xlim)- 0.125, digits = 6), '):1:(',
                    signif(max(xlim)+0.125, digits = 6), '],anom[(', tlim[1],
                    'T00:00:00Z):1:(', tlim[2], 'T00:00:00Z)][(0.0):1:(0.0)][(',
                    signif(min(ylim)- 0.125, digits = 4), '):1:(',
                    signif(max(ylim)+0.125, digits = 4), ')][(',
                    signif(min(xlim)- 0.125, digits = 6),'):1:(',
                    signif(max(xlim)+0.125, digits = 6), ')],err[(',
                    tlim[1], 'T00:00:00Z):1:(', tlim[2],
                    'T00:00:00Z)][(0.0):1:(0.0)][(',
                    signif(min(ylim)- 0.125, digits = 4), '):1:(',
                    signif(max(ylim)+0.125, digits = 4), ')][(',
                    signif(min(xlim)- 0.125, digits = 6), '):1:(',
                    signif(max(xlim)+0.125, digits = 6), ')],ice[(',
                    tlim[1], 'T00:00:00Z):1:(', tlim[2],
                    'T00:00:00Z)][(0.0):1:(0.0)][(',
                    signif(min(ylim)- 0.125, digits = 4), '):1:(',
                    signif(max(ylim)+0.125, digits = 4), ')][(',
                    signif(min(xlim)- 0.125, digits = 6), '):1:(',
                    signif(max(xlim)+0.125, digits = 6), ')]')
    varname <- 'sst'

  }

  #}
  download.file(sstURL,
                destfile= file.name,
                mode='wb')
  test <- nc_open(file.name)

  Cc_sightings.raw[k, 'sst'] <- mean(ncvar_get(test, varname), na.rm = T)

  nc_close(test)
}

#Cc_sightings.raw <- read.csv(file = 'Data/nonAerialSightings_Cc.csv')
Cc_sightings.Cc <- filter(Cc_sightings.raw, Genus == "Caretta")
Cc_sightings.other <- filter(Cc_sightings.raw, Genus != 'Caretta')

write.csv.rename(Cc_sightings.Cc,
                 file = 'Data/nonAerial_Cc_sst.csv',
                 quote = F, row.names = F)

write.csv.rename(Cc_sightings.other,
                 file = 'Data/nonAerial_other_sst.csv',
                 quote = F, row.names = F)
