#addCovs2Prediction

# add covariates to prediction background data

# 25 July 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')

#var.names <- c('erdMWchla8day', 'jplMURSST', 'gatfnt14day')

#chl.m <- sst.m <- fnt.m <- vector(mode = 'numeric', dim(prediction.data)[1])
#chl.sd <- sst.sd <- fnt.sd <- vector(mode = 'numeric', dim(prediction.data)[1])
#chl.n <- sst.n <- fnt.n <- vector(mode = 'numeric', dim(prediction.data)[1])

years <- 2011:2013 #2012:2014 #c(2011, 2015)
#months <- c(7, 8)
#days <- c(31, 31)
months <- 1:12
days <- c(1, 7, 14, 21, 28)

covnames <- c('sst_08', 'sst_14', 'sst_30',
              'chl_08', 'chl_14', 'chl_30',
              'fnt_08', 'fnt_14', 'fnt_30', 'uvgeo_0',
              'uvgeo_08', 'uvgeo_14', 'uvgeo_30')

varnames <- c('jplMURSST', 'erdMWchla8day', 'gatfnt14day')
y<-m<-d<-1
for (y in 1:length(years)){
  for (m in 1:length(months)){
    for (d in 1:length(days)){

      date1 <- as.Date(paste0(years[y], '-',
                              months[m], '-',
                              days[d]))
      print(date1)
      load(paste0('RData/predictions/predictionDataWithCovariates_',
                                    date1, '.RData'))

      if (file.exists(paste0('RData/predictions/predictionData_SstChlFntGeo_',
                             date1, '.RData')) == F){
        # seems like some have duplicated variables - take care of
        # those here:
        for (v in 1:length(covnames)){
          var1 <- covnames[v]
          idx <- grep(covnames[v], names(prediction.data.cov))
          if (length(idx) > 1){
            prediction.data.cov <- prediction.data.cov[, -(idx[2:length(idx)])]
          } else if (length(idx) == 0){
            time.lag <- as.numeric(strsplit(covnames[v], '_')[[1]][2])
            if (length(grep('geo', covnames[v])) == 0){

              if (length(grep('sst', covnames[v])) == 1){
                temp <- extract.cov.bkgd('jplMURSST',
                                         (date1-time.lag),
                                         center.UTM,
                                         prediction.data.cov[, c('x', 'y')])

              } else if (length(grep('chl', covnames[v])) == 1){
                temp <- extract.cov.bkgd('erdMWchla8day',
                                         (date1-time.lag),
                                         center.UTM,
                                         prediction.data.cov[, c('x', 'y')])

              } else if (length(grep('fnt', covnames[v])) == 1){
                temp <- extract.cov.bkgd('gatfnt14day',
                                         (date1-time.lag),
                                         center.UTM,
                                         prediction.data.cov[, c('x', 'y')])

              }

              if (length(grep('mean', colnames(temp))) == 0){
                colnames(temp)[grep('var', colnames(temp))] <- 'mean'
              }

              header <- names(prediction.data.cov)

              prediction.data.cov <- cbind(prediction.data.cov, temp$mean)
              names(prediction.data.cov) <- c(header,
                                              paste0(covnames[v], '_mean'))

            } else {
              temp <- extract.geo.bkgd((date1-time.lag),
                                       center.UTM,
                                       prediction.data.cov[, c('x', 'y')])

              header <- names(prediction.data.cov)
              prediction.data.cov <- cbind(prediction.data.cov, temp$u, temp$v)
              names(prediction.data.cov) <- c(header,
                                              paste0('ugeo_', time.lag),
                                              paste0('vgeo_', time.lag))
            }

          }

        }

        save(prediction.data.cov,
             file = paste0('RData/predictions/predictionData_SstChlFntGeo_',
                           date1, '.RData'))
      }

    }
  }
}

