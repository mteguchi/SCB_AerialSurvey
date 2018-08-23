#addSST2DGNbycatch



# adds SST readings for DGN bycatch location and time

rm(list=ls())
source('CcSCB_functions.R')

# DGN bycatch data were obtained from FRD, uisng get_DGN_bycatch.R in
# Cc_SCB project. The output file was copied to this project
Cc_bycatch.raw <- read.csv(file = 'Data/CC_bycatch_DGN.csv')


# find the mean differences in latitudes and longitude between two ends:
Cc_bycatch.unique <- ddply(Cc_bycatch.raw, .(TripNumber), head, n=1 )
Cc_bycatch.unique %>% mutate(latdif = LatDD2 - LatDD1,
                             londif = LonDD2 - LonDD1) -> Cc_bycatch

Cc_bycatch$date <- paste(Cc_bycatch$Year,
                         formatC(Cc_bycatch$MM, width = 2, flag = '0'),
                         formatC(Cc_bycatch$DD, width = 2, flag = '0'),
                         sep = '-')

# no time for this data so it does not matter too much...
#Cc_bycatch$UTC <- as.POSIXct(Cc_bycatch$local.date, tz = 'UTC')

#summary(Cc_bycatch)
Cc_bycatch$sst <- NA
k <- 1
for (k in 1:dim(Cc_bycatch)[1]){
  if (!is.na(Cc_bycatch[k, 'LatDD1']) & !is.na(Cc_bycatch[k, 'LatDD2'])){
    xlim <- Cc_bycatch[k, c('LonDD1', 'LonDD2')]
    ylim <- Cc_bycatch[k, c('LatDD1', 'LatDD2')]
  } else if (!is.na(Cc_bycatch[k, 'LatDD1']) & is.na(Cc_bycatch[k, 'LatDD2'])){
    xlim <- c(Cc_bycatch[k, 'LonDD1'],
              Cc_bycatch[k, 'LonDD1'] + mean(abs(Cc_bycatch$londif), na.rm = T))
    ylim <- c(Cc_bycatch[k, 'LatDD1'],
              Cc_bycatch[k, 'LatDD1'] + mean(abs(Cc_bycatch$latdif), na.rm = T))
  }

  tlim <- c(Cc_bycatch[k, 'date'], Cc_bycatch[k, 'date'])

  xlim <- -xlim + 360
  #ylim <- sort(ylim)

  file.name <- paste0('Data/sst_data/DGN_Cc_', Cc_bycatch[k, 'TripNumber'], '.nc')

  if (!file.exists(file.name)){
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


    download.file(sstURL,
                  destfile= file.name,
                  mode='wb')
  }

  test <- nc_open(file.name)

  Cc_bycatch[k, 'sst'] <- mean(ncvar_get(test, 'sst'), na.rm = T)
}

# join the raw data and sst:
Cc_bycatch %>% select(., TripNumber, sst) %>%
  inner_join(., Cc_bycatch.raw, by = "TripNumber") -> Cc_bycatch.sst

write.csv.rename(Cc_bycatch.sst,
                 file = 'Data/Cc_bycatch_sst.csv',
                 quote = F, row.names = F)
