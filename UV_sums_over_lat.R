#UV_sums_over_lat
# Sums/averages geostrophic currents over latitudes
# in 0.25 longitudinal bands

# Tomo Eguchi
# 12 December 2016

rm(list=ls())
source('CcSCB_functions.R')

yrs <- 2009:2016
mos <- 1:12
das <- c(1, 7, 14, 28)

dx <- 90 # km
long.bands <- seq(from = -3717, 
	by = dx, to = 384 - dx)

center.X <- u.sum <- v.sum <- u.avg <-v.avg <- u.sd <- v.sd <- u.SE <- v.SE <- vector(mode = 'numeric', length = length(long.bands))

y <- m <- d <- 1
for (y in 1:length(yrs)){
  for (m in 1:length(mos)){
    for (d in 1:length(das)){
      date.str <- as.Date(paste0(yrs[y], '-', mos[m], '-', das[d]))
      print(date.str)
      out.filename <- paste0('RData/large_uv_stats/uv_stats_', date.str, '.RData' )

      uv.geo <- extract.geo.large.bkgd(date.str, center.UTM)
      uv.geo <-na.omit(uv.geo)
      #tmp <- sp2latlon(uv.geo, center.UTM)@coords
      #colnames(tmp) <- c('longitude', 'latitude')
      #uv.geo <- cbind(uv.geo, tmp)

      for (k in 1:length(long.bands)){
      	center.X[k] <- long.bands[k] + dx/2

      	u.sum[k] <- sum(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'u'], na.rm = T)
      	v.sum[k] <- sum(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'v'], na.rm = T)
      	u.avg[k] <- mean(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'u'], na.rm = T)
      	v.avg[k] <- mean(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'v'], na.rm = T)
      	u.sd[k] <-  sd(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'u'], na.rm = T)
      	v.sd[k] <-  sd(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'v'], na.rm = T)
      	u.SE[k] <-  SE(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'u'])
      	v.SE[k] <-  SE(uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= (long.bands[k] + dx), 'v'])
      		
      }

      uv.stats <- data.frame(cbind(center.X, u.sum, v.sum, u.avg, v.avg, u.sd, v.sd, u.SE, v.SE))
      save(uv.stats, file = out.filename)


  	}	
  }
}

