#addCovs2Prediction

# add covariates to prediction background data

# 25 July 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')

var.names <- c('erdMWchla8day', 'jplMURSST', 'gatfnt14day')

# Convert segdata into a spatial data frame
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

prediction.data <- data.frame(x = pred.data$newX,
                              y = pred.data$newY,
                              latitude = pred.data$Y,
                              longitude = pred.data$X)

#chl.m <- sst.m <- fnt.m <- vector(mode = 'numeric', dim(prediction.data)[1])
#chl.sd <- sst.sd <- fnt.sd <- vector(mode = 'numeric', dim(prediction.data)[1])
#chl.n <- sst.n <- fnt.n <- vector(mode = 'numeric', dim(prediction.data)[1])

years <- 2015 #c(2011, 2015)
months <- 6:10
#days <- c(1, 7, 14, 21, 28)
# months <- c(7, 8)
days <- c(30, 31, 31, 30, 31)
for (y in 1:length(years)){
  for (m in 1:length(months)){
    for (d in 1:length(days)){

      date1 <- as.Date(paste0(years[y], '-',  months[m], '-', days[d]))
      print(date1)
      if (file.exists(file = paste0('RData/predictions/predictionDataWithCovariates_',
                                    date1, '.RData')) == FALSE){
        sst <- extract.cov.bkgd('jplMURSST', date1, center.UTM,
                                prediction.data[, c('x', 'y')])
        if (length(grep('mean', colnames(sst))) == 0){
          colnames(sst)[grep('var', colnames(sst))] <- 'mean'
        }

        chl <- extract.cov.bkgd('erdMWchla8day',date1, center.UTM,
                                prediction.data[, c('x', 'y')])
        if (length(grep('mean', colnames(chl))) == 0){
          colnames(chl)[grep('var', colnames(chl))] <- 'mean'
        }

        fnt <- extract.cov.bkgd('gatfnt14day',date1, center.UTM,
                                prediction.data[, c('x', 'y')])
        if (length(grep('mean', colnames(fnt))) == 0){
          colnames(fnt)[grep('var', colnames(fnt))] <- 'mean'
        }

        prediction.data.cov <- cbind(prediction.data,
                                     sst_mean = sst$mean,
                                     chl_mean = chl$mean,
                                     fnt_mean = fnt$mean)

        save(prediction.data.cov,
             file = paste0('RData/predictions/predictionDataWithCovariates_',
                           date1, '.RData'))
        rm(list=c('sst', 'chl', 'fnt', 'prediction.data.cov'))
      }

    }
  }

}


