#addUVgeo2PredictionData

# add Ugeo and Vgeo data to prediction background data

# 2 December 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')

var.names <- c('erdTAugeo', 'erdTAvgeo')

years <- 2011:2015
months <- 1:12
days <- c(1, 7, 14, 21, 28)
y <- m <- d <- 1
# months <- c(7, 8)
#days <- c(30, 31, 31, 30, 31)
for (y in 1:length(years)){
  for (m in 1:length(months)){
    for (d in 1:length(days)){

      date1 <- as.Date(paste0(years[y], '-',  months[m], '-', days[d]))
      print(date1)
      if (file.exists(file = paste0('RData/predictions/predictionDataWithCovariates_',
                                    date1, '.RData')) == TRUE){
        load(paste0('RData/predictions/predictionDataWithCovariates_',
                                    date1, '.RData'))
        tmp.data <- prediction.data.cov
        uvgeo_0 <- extract.geo.bkgd(date1, center.UTM,
                                    tmp.data[, c('x', 'y')])
        uvgeo_8 <- extract.geo.bkgd(date1-8, center.UTM,
                                    tmp.data[, c('x', 'y')])
        uvgeo_14 <- extract.geo.bkgd(date1-14, center.UTM,
                                     tmp.data[, c('x', 'y')])
        uvgeo_30 <- extract.geo.bkgd(date1-30, center.UTM,
                                     tmp.data[, c('x', 'y')])

        prediction.data.cov <- cbind(tmp.data,
                                     ugeo_0 = uvgeo_0$u,
                                     vgeo_0 = uvgeo_0$v,
                                     ugeo_8 = uvgeo_8$u,
                                     vgeo_8 = uvgeo_8$v,
                                     ugeo_14 = uvgeo_14$u,
                                     vgeo_14 = uvgeo_14$v,
                                     ugeo_30 = uvgeo_30$u,
                                     vgeo_30 = uvgeo_30$v)

        save(prediction.data.cov,
             file = paste0('RData/predictions/predictionData_SstChlFntUvgeo_',
                           date1, '.RData'))
        rm(list=c('uvgeo_0', 'uvgeo_8', 'uvgeo_14', 'uvgeo_30',
                  'prediction.data.cov'))


      }
    }

  }
}

