#CcSCB_GetOceData
#
# Retrieve oceanographic data for loggerhead turtle aerial survey data.


# Tomo Eguchi
# 12 May 2016

rm(list=ls())
runDate <- Sys.Date()

library(dplyr)
library(xtractomatic)
library(reshape2)
library(ncdf4)

source('CcSCB_functions.R')

# combine data
dat.all <- merge(dat2011, dat2015, all = T)

# create date variable
dat.all$date <- ISOdate(year = dat.all$year,
                        month = dat.all$month,
                        day = dat.all$day,
                        hour = floor(dat.all$mtime),
                        min = floor((dat.all$mtime - floor(dat.all$mtime))*60),
                        tz = "America/Los_Angeles")

dat.all$date.GMT <- as.Date(format(dat.all$date, tz = "Europe/London"))

# using code chunks in lostturtles_map.R, written by Cara Wilson:
tlim2011<-range(dat.all$date.GMT[dat.all$year == 2011])
tlim2015<-range(dat.all$date.GMT[dat.all$year == 2015])
xlim<-range(dat.all$mlon) + 360  # needs to be positive values!
ylim<-range(dat.all$mlat)

#Get current data, its not in xtractomatic. Only needs to be done once!
# added 1 degrees to the survey limits; both lat and lon
#tlim <- tlim2015

dates <- unique(dat.all$date.GMT)
n.dates<-length(dates)
#n.dates
for (i in 1:n.dates) {

  # data availability changes... removing currents for now 21 July 2016
  # if (dates[i] > as.Date("2015-09-30")){
  #   # miamicurrents covers from 2015-10-01 to 2016-05-11 as of 2016-05-12
  #   myURL <- paste('http://coastwatch.pfeg.noaa.gov/erddap/griddap/miamicurrents.nc?',
  #                  'u_current[(',dates[i],'):1:(',dates[i],')]',
  #                  '[(',floor(ylim[1]-1),'):1:(',ceiling(ylim[2]+1),')]',
  #                  '[(',floor(xlim[1]-1),'):1:(',ceiling(xlim[2]+1),')],',
  #                  'v_current[(',dates[i],'):1:(',dates[i],')]',
  #                  '[(',floor(ylim[1]-1),'):1:(',ceiling(ylim[2]+1),')]',
  #                  '[(',floor(xlim[1]-1),'):1:(',ceiling(xlim[2]+1),')]',sep="")
  #
  # } else if (dates[i] < as.Date("2012-12-08")){
  #   # erdTAgeo1day covers from 1992-10-14 to 2012-12-08  as of 2016-05-12
  #   # this should work with xtracto... but somehow it doesn't. 2016-05-12
  #   myURL <- paste('http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAgeo1day.nc?',
  #                  'u_current[(',dates[i],'):1:(',dates[i],')][(0.0):1:(0.0)]',
  #                  '[(',floor(ylim[1]-1),'):1:(',ceiling(ylim[2]+1),')]',
  #                  '[(',floor(xlim[1]-1),'):1:(',ceiling(xlim[2]+1),')],',
  #                  'v_current[(',dates[i],'):1:(',dates[i],')][(0.0):1:(0.0)]',
  #                  '[(',floor(ylim[1]-1),'):1:(',ceiling(ylim[2]+1),')]',
  #                  '[(',floor(xlim[1]-1),'):1:(',ceiling(xlim[2]+1),')]',sep="")
  #
  # }
  #
  #
  # filename <- paste0("Data/ocean/current_", dates[i], ".nc")
  #
  # if (file.exists(filename) == FALSE){
  #   test<-download.file(myURL, destfile = filename, mode='wb')
  # }

  # now read the ncdf file (note the run date from above)
  # datafileID <- nc_open(filename)
  # lon <- ncvar_get(datafileID, varid="longitude")
  # lat <- ncvar_get(datafileID, varid="latitude")
  # time <- ncvar_get(datafileID, varid="time")
  # time <- as.POSIXlt(time,origin='1970-01-01',tz= "GMT")
  # u <- ncvar_get(datafileID, varid="u_current")
  # v <- ncvar_get(datafileID, varid="v_current")
  # nc_close(datafileID)
  #
  # curr.df <- currFrame(lon,lat,u,v)
  # save(curr.df, file = paste0('RData/curr', dates[i], '.RData'))

  # Get MODIS chl, GRSST SST data, and NRL SSH.
  # They are in xtractomatic , so use that

  #tlim<-c("2014-10-14","2015-01-29")
  #xlim<-xlim-360
  tlim <- c(dates[i], dates[i])

  var.names <- c('erdMWchla8day', 'jplMURSST', 'gatfnt14day')
  chl <- get.oce.data('erdMWchla8day', dates[i], xlim, ylim, tlim)
  sst <- get.oce.data('jplMURSST', dates[i], xlim, ylim, tlim)
  if (dates[i] < as.Date('2015-10-24')){
    fnt <- get.oce.data('gatfnt14day', dates[i], xlim, ylim, tlim)
  }
  if (dates[i] > as.Date("2014-04-07")){
    ssh <- get.oce.data('nrlHycomGLBu008e911S',
                        dates[i], xlim, ylim, tlim)
  }
  # Now SSH
  # if (dates[i] > as.Date("2014-04-07")){
  #   ssh<-xtracto_3D(xlim, ylim, tlim,"nrlHycomGLBu008e911S")
  #   ssh$time<-as.Date(ssh$time)
  #
  #   longitude<-ssh$longitude
  #   latitude<-ssh$latitude
  #   iday<-1:length(ssh$time)
  #   tmp <- lapply(iday,
  #                 function(x) melt_data(longitude,
  #                                       latitude,
  #                                       ssh$time[x],
  #                                       ssh$data))
  #   allssh <- do.call(rbind, tmp)
  #   save(allssh, file = paste0('RData/ssh', dates[i], '.RData'))
  #} else {

    # this requires UTM coordinates to be used for writing the URL...
    # lat/lon doesn't work! Need to know how to convert between their X/Y
    # and lat/lon... Ugh! 20 July 2016
    # myURL <- paste0('http://coastwatch.pfeg.noaa.gov/erddap/griddap/hycom_GLBa008_tyx.nc?qtot[(',
    #                 dates[i], 'T00:00:00Z):1:(', dates[i], 'T00:00:00Z)]',
    #                 '[(', floor(ylim[1]-1), '):1:(', ceiling(ylim[2]+1), ')]',
    #                 '[(', floor(xlim[1]-1), '):1:(', ceiling(xlim[2]+1), ')],',
    #                 'emp[(', dates[i], 'T00:00:00Z):1:(', dates[i], 'T00:00:00Z)]',
    #                 '[(', floor(ylim[1]-1), '):1:(', ceiling(ylim[2]+1), ')]',
    #                 '[(', floor(xlim[1]-1), '):1:(', ceiling(xlim[2]+1), ')],',
    #                 'surface_temperature_trend[(',
    #                 dates[i], 'T00:00:00Z):1:(', dates[i], 'T00:00:00Z)]',
    #                 '[(', floor(ylim[1]-1), '):1:(', ceiling(ylim[2]+1), ')]',
    #                 '[(', floor(xlim[1]-1), '):1:(', ceiling(xlim[2]+1), ')],',
    #                 'surface_salinity_trend[(',
    #                 dates[i], 'T00:00:00Z):1:(', dates[i], 'T00:00:00Z)]',
    #                 '[(', floor(ylim[1]-1), '):1:(', ceiling(ylim[2]+1), ')]',
    #                 '[(', floor(xlim[1]-1), '):1:(', ceiling(xlim[2]+1), ')],',
    #                 'ssh[(', dates[i], 'T00:00:00Z):1:(', dates[i], 'T00:00:00Z)]',
    #                 '[(', floor(ylim[1]-1), '):1:(', ceiling(ylim[2]+1), ')]',
    #                 '[(', floor(xlim[1]-1), '):1:(', ceiling(xlim[2]+1), ')]')
    #
    # filename <- paste0("Data/ocean/ssh_", dates[i], ".nc")
    #
    # if (file.exists(filename) == FALSE){
    #   test<-download.file(myURL, destfile = filename, mode='wb')
    # }
    # now read the ncdf file (note the run date from above)
    # datafileID <- nc_open(filename)
    # nc_close(datafileID)
    #
    # curr.df <- currFrame(lon,lat,u,v)
    # save(curr.df, file = paste0('RData/curr', dates[i], '.RData'))

#  }

}



