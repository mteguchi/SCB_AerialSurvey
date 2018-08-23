#CcSCB_GetOceDataLargeBkgd
#
# Retrieve oceanographic data for larger background area


# Tomo Eguchi
# 6 Sep 2016

rm(list=ls())
runDate <- Sys.Date()

#library(dplyr)
#library(xtractomatic)
#library(reshape2)
#library(ncdf4)

source('CcSCB_functions.R')

xlim <- c(205, 243.4)
ylim <- c(22, 40)

# data availability changes so go back to download necessary data
# combination of two years and two months
years <- 2016 #c(2013, 2014)
months <- 1:12
days <- c(1, 7, 14, 21, 28)
for (y in 1:length(years)){
  for (m in 1:length(months)){
    for (d in 1:length(days)){
      #print(paste(years[y], '-', months[m], d))
      # create data frame
      date.GMT <- as.Date(format(paste0(years[y], '-',
                                        months[m], '-',
                                        days[d]), tz = "Europe/London"))
      tlim <- c(date.GMT, date.GMT)

      chl <- get.oce.data.large.bkgd('erdMWchla8day', date.GMT, xlim, ylim, tlim)
      sst <- get.oce.data.large.bkgd('jplMURSST', date.GMT, xlim, ylim, tlim)
      if (date.GMT < as.Date("2012-12-09")){
        cur_u <- get.oce.data.large.bkgd('erdTAugeo1day', date.GMT, xlim, ylim, tlim)
        cur_v <- get.oce.data.large.bkgd('erdTAvgeo1day', date.GMT, xlim, ylim, tlim)
      }
      if (date.GMT < as.Date('2015-10-24')){
        fnt <- get.oce.data.large.bkgd('gatfnt14day', date.GMT, xlim, ylim, tlim)
      }
      if (date.GMT > as.Date("2014-04-07") & date.GMT < as.Date("2016-04-19")){
        ssh <- get.oce.data.large.bkgd('nrlHycomGLBu008e911S',
                                 date.GMT, xlim, ylim, tlim)
      }


    }

  }
}




