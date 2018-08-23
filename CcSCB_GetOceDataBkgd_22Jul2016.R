#CcSCB_GetOceDataBkgd
#
# Retrieve oceanographic data for loggerhead turtle predictions.


# Tomo Eguchi
# 22 July 2016

rm(list=ls())
runDate <- Sys.Date()

#library(dplyr)
#library(xtractomatic)
#library(reshape2)
#library(ncdf4)

source('CcSCB_functions.R')

#var.names <- c('erdMWchla8day', 'jplMURSST', 'gatfnt14day')

# convert the study area into a data frame with 2x2 km^2 cells
# eventually, add covariates - depths, sst, etc. and save in a file
pred.data.Sp <- makegrid(study.area.Sp, cellsize = 2000)  # 2000m

# convert UTM back to lat/lon - needed for xtracto
colnames(pred.data.Sp) <- c("X", "Y")
coordinates(pred.data.Sp) <- c("X", "Y")
proj4string(pred.data.Sp) <- CRS("+proj=utm +zone=10 ellps=wGS84")
pred.data <- as.data.frame(spTransform(pred.data.Sp,
                                       CRS("+proj=longlat +datum=WGS84")))

# this is for modeling; all in km. and centered at the center coordinate.
pred.data$newX <- (pred.data.Sp$X - center.UTM$X)/1000   # in Km
pred.data$newY <- (pred.data.Sp$Y - center.UTM$Y)/1000

bkgd.data.spatial <- data.frame(x = pred.data$newX,
                                y = pred.data$newY,
                                latitude = pred.data$Y,
                                longitude = pred.data$X)
xlim <- range(bkgd.data.spatial$longitude) + 360
ylim <- range(bkgd.data.spatial$latitude)

# data availability changes so go back to download necessary data
# combination of two years and two months
years <- c(2009, 2010, 2012, 2013, 2014)
months <- 1:12
days <- c(1, 7, 14, 21, 28)
for (y in 1:length(years)){
  for (m in 1:length(months)){
    for (d in 1:length(days)){
      # create data frame
      bkgd.data <- cbind(bkgd.data.spatial,
                         date = ISOdate(year = years[y],
                                        month = months[m],
                                        day = days[d],
                                        tz = "America/Los_Angeles"))
      bkgd.data$date.GMT <- as.Date(format(bkgd.data$date, tz = "Europe/London"))
      tlim <- c(bkgd.data$date[1], bkgd.data$date[1])

      da <- as.Date(tlim[1])
      chl <- get.oce.data.bkgd('erdMWchla8day', da, xlim, ylim, tlim)
      sst <- get.oce.data.bkgd('jplMURSST', da, xlim, ylim, tlim)
      #if (as.Date(bkgd.data$date[1]) < as.Date('2015-10-24')){
      fnt <- get.oce.data.bkgd('gatfnt14day', da, xlim, ylim, tlim)
      #}
      if (as.Date(bkgd.data$date[1]) > as.Date("2014-04-07")){
        ssh <- get.oce.data.bkgd('nrlHycomGLBu008e911S',
                                 da, xlim, ylim, tlim)
      }


    }

  }
}




