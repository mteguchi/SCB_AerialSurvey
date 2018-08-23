#addCovs2Segs

# add covariates to segmented data

# 22 July 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')

var.names <- c('erdMWchla8day', 'jplMURSST', 'gatfnt14day')

chl.m <- sst.m <- fnt.m <- vector(mode = 'numeric', dim(segment.data)[1])
chl.sd <- sst.sd <- fnt.sd <- vector(mode = 'numeric', dim(segment.data)[1])
chl.n <- sst.n <- fnt.n <- vector(mode = 'numeric', dim(segment.data)[1])

for (k in 1:dim(segment.data)[1]){
  date1 <- as.Date(paste0(segment.data$year[k], '-',
                          segment.data$month[k], '-',
                          segment.data$day[k]))

  sst <- extract.cov('jplMURSST',date1, center.UTM,
                     c(segment.data$x[k], segment.data$y[k]))

  sst.m[k] <- sst[1]
  sst.sd[k] <- sst[2]
  sst.n[k] <- sst[3]
  chl <- extract.cov('erdMWchla8day',date1, center.UTM,
                     c(segment.data$x[k], segment.data$y[k]))
  chl.m[k] <- chl[1]
  chl.sd[k] <- chl[2]
  chl.n[k] <- chl[3]

  fnt <- extract.cov('gatfnt14day',date1, center.UTM,
                     c(segment.data$x[k], segment.data$y[k]))

  fnt.m[k] <- fnt[1]
  fnt.sd[k] <- fnt[2]
  fnt.n[k] <- fnt[3]

}

segment.data.cov <- cbind(segment.data,
                          sst_mean = sst.m, sst_SD = sst.sd, sst_n = sst.n,
                          chl_mean = chl.m, chl_SD = chl.sd, chl_n = chl.n,
                          fnt_mean = fnt.m, fnt_SD = fnt.sd, cnf_n = fnt.n)

save(segment.data.cov,
     file = paste0('RData/segmentDataWithCovariates_', Sys.Date(), '.RData'))
