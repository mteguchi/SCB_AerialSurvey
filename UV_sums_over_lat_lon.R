#UV_sums_over_lat
# Sums/averages geostrophic currents over latitudes
# in 0.25 longitudinal bands

# Tomo Eguchi
# 12 December 2016

rm(list=ls())
source('CcSCB_functions.R')

yrs <- 2009:2015
mos <- 1:12
das <- c(1, 7, 14, 28)

dx <- 90 # km
long.bands <- seq(from = -3717,
	by = dx, to = 384 - dx)
dy <- 90
lat.bands <- seq(from = -1300, by = dy, to = (1300-dy))

uv.stats <- matrix(data = NA, nrow = length(long.bands) * length(lat.bands), ncol = 10)
#center.X <- vector(mode = 'numeric', length = length(long.bands))
#center.Y <- vector(mode = 'numeric', length = length(lat.bands))
#u.sum <- v.sum <- u.avg <-v.avg <- u.sd <- v.sd <- u.SE <- v.SE <- matrix(data = NA, ncol = length(long.bands), nrow = length(lat.bands))

y <- m <- d <- 1
for (y in 1:length(yrs)){
  for (m in 1:length(mos)){
    for (d in 1:length(das)){
      date.str <- as.Date(paste0(yrs[y], '-', mos[m], '-', das[d]))
      print(date.str)
      out.filename <- paste0('RData/large_uv_stats/uv_stats2_', date.str, '.RData' )

      uv.geo <- extract.geo.large.bkgd(date.str, center.UTM)
      uv.geo <-na.omit(uv.geo)
      #tmp <- sp2latlon(uv.geo, center.UTM)@coords
      #colnames(tmp) <- c('longitude', 'latitude')
      #uv.geo <- cbind(uv.geo, tmp)
      r <- k <- k1 <- 1
      for (k in 1:length(long.bands)){

      	for (k1 in 1:length(lat.bands)){
      		uv.stats[r, 1] <- long.bands[k] + dx/2
      		uv.stats[r, 2] <- lat.bands[k1] + dy/2
      		tmp.data <- uv.geo[uv.geo$X > long.bands[k] & uv.geo$X <= long.bands[k] + dx &
      									uv.geo$Y > lat.bands[k1] & uv.geo$Y <= lat.bands[k1] + dy, ]
      		uv.stats[r, 3] <- sum(tmp.data$u, na.rm = T)
      		uv.stats[r, 4] <- sum(tmp.data$v, na.rm = T)
      		uv.stats[r, 5] <- mean(tmp.data$u, na.rm = T)
      		uv.stats[r, 6] <- mean(tmp.data$v, na.rm = T)
      		uv.stats[r, 7] <-  sd(tmp.data$u, na.rm = T)
      		uv.stats[r, 8] <-  sd(tmp.data$v, na.rm = T)
      		uv.stats[r, 9] <-  SE(tmp.data$u)
      		uv.stats[r, 10] <-  SE(tmp.data$v)
      		r <- r + 1
      	}


      }

      uv.stats <- as.data.frame(uv.stats)
      colnames(uv.stats) <- c('center.X', 'center.Y', 'u.sum', 'v.sum',
      	'u.avg', 'v.avg', 'u.sd', 'v.sd', 'u.SE', 'v.SE')
      save(uv.stats, file = out.filename)


  	}
  }
}

