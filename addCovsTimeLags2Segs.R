#addCovs2Segs

# add covariates to segmented data

# 22 July 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')
load('RData/segmentDataWithCovariates_2016-07-22.RData')
segment.data <- segment.data.cov
rm(list = 'segment.data.cov')
var.names <- c('erdMWchla8day', 'jplMURSST', 'gatfnt14day')


chl_08.m <- sst_08.m <- fnt_08.m <- vector(mode = 'numeric',
                                           dim(segment.data)[1])
chl_08.sd <- sst_08.sd <- fnt_08.sd <- vector(mode = 'numeric',
                                              dim(segment.data)[1])
chl_08.n <- sst_08.n <- fnt_08.n <- vector(mode = 'numeric',
                                           dim(segment.data)[1])
chl_14.m <- sst_14.m <- fnt_14.m <- vector(mode = 'numeric',
                                           dim(segment.data)[1])
chl_14.sd <- sst_14.sd <- fnt_14.sd <- vector(mode = 'numeric',
                                              dim(segment.data)[1])
chl_14.n <- sst_14.n <- fnt_14.n <- vector(mode = 'numeric',
                                           dim(segment.data)[1])
chl_30.m <- sst_30.m <- fnt_30.m <- vector(mode = 'numeric',
                                           dim(segment.data)[1])
chl_30.sd <- sst_30.sd <- fnt_30.sd <- vector(mode = 'numeric',
                                              dim(segment.data)[1])
chl_30.n <- sst_30.n <- fnt_30.n <- vector(mode = 'numeric',
                                           dim(segment.data)[1])

for (k in 1:dim(segment.data)[1]){
  date1 <- as.Date(paste0(segment.data$year[k], '-',
                          segment.data$month[k], '-',
                          segment.data$day[k]))

  sst_08 <- extract.cov('jplMURSST', date1-8, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  sst_14 <- extract.cov('jplMURSST', date1-14, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  sst_30 <- extract.cov('jplMURSST', date1-30, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  chl_08 <- extract.cov('erdMWchla8day', date1-8, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  chl_14 <- extract.cov('erdMWchla8day', date1-14, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  chl_30 <- extract.cov('erdMWchla8day', date1-30, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  fnt_08 <- extract.cov('gatfnt14day',date1-8, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  fnt_14 <- extract.cov('gatfnt14day',date1-14, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))

  fnt_30 <- extract.cov('gatfnt14day',date1-30, center.UTM,
                        c(segment.data$x[k], segment.data$y[k]))


  sst_08.m[k] <- sst_08[1]
  sst_08.sd[k] <- sst_08[2]
  sst_08.n[k] <- sst_08[3]
  sst_14.m[k] <- sst_14[1]
  sst_14.sd[k] <- sst_14[2]
  sst_14.n[k] <- sst_14[3]
  sst_30.m[k] <- sst_30[1]
  sst_30.sd[k] <- sst_30[2]
  sst_30.n[k] <- sst_30[3]

  chl_08.m[k] <- chl_08[1]
  chl_08.sd[k] <- chl_08[2]
  chl_08.n[k] <- chl_08[3]
  chl_14.m[k] <- chl_14[1]
  chl_14.sd[k] <- chl_14[2]
  chl_14.n[k] <- chl_14[3]
  chl_30.m[k] <- chl_30[1]
  chl_30.sd[k] <- chl_30[2]
  chl_30.n[k] <- chl_30[3]

  fnt_08.m[k] <- fnt_08[1]
  fnt_08.sd[k] <- fnt_08[2]
  fnt_08.n[k] <- fnt_08[3]
  fnt_14.m[k] <- fnt_14[1]
  fnt_14.sd[k] <- fnt_14[2]
  fnt_14.n[k] <- fnt_14[3]
  fnt_30.m[k] <- fnt_30[1]
  fnt_30.sd[k] <- fnt_30[2]
  fnt_30.n[k] <- fnt_30[3]

}

segment.data.cov <- cbind(segment.data,
                           sst_08_mean = sst_08.m,
                           sst_08_SD = sst_08.sd,
                           sst_08_n = sst_08.n,
                           sst_14_mean = sst_14.m,
                           sst_14_SD = sst_14.sd,
                           sst_14_n = sst_14.n,
                           sst_30_mean = sst_30.m,
                           sst_30_SD = sst_30.sd,
                           sst_30_n = sst_30.n,
                           chl_08_mean = chl_08.m,
                           chl_08_SD = chl_08.sd,
                           chl_08_n = chl_08.n,
                           chl_14_mean = chl_14.m,
                           chl_14_SD = chl_14.sd,
                           chl_14_n = chl_14.n,
                           chl_30_mean = chl_30.m,
                           chl_30_SD = chl_30.sd,
                           chl_30_n = chl_30.n,
                           fnt_08_mean = fnt_08.m,
                           fnt_08_SD = fnt_08.sd,
                           fnt_08_n = fnt_08.n,
                           fnt_14_mean = fnt_14.m,
                           fnt_14_SD = fnt_14.sd,
                           fnt_14_n = fnt_14.n,
                           fnt_30_mean = fnt_30.m,
                           fnt_30_SD = fnt_30.sd,
                           fnt_30_n = fnt_30.n)

save(segment.data.cov,
     file = paste0('RData/segmentDataWithCovariates_', Sys.Date(), '.RData'))
