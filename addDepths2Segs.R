#addDepths2Segs

# add Depthss to segmented data

# 2 September 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')

depth.file <- read.csv('Data/ocean/depth.csv')
names(depth.file) <- c("Y", "X", "depth")
depth.sp <- latlon2sp(depth.file, center.UTM)
depth.df <- data.frame(X = depth.sp$newX,
                       Y = depth.sp$newY,
                       depth = depth.sp$depth)

# load segment data with covaraites
load('RData/segmentDataWithCovariates_2016-09-01.RData')

segment.data <- segment.data.cov
rm(list = "segment.data.cov")

depth.m <- depth.sd <- depth.n <- vector(mode = 'numeric', dim(segment.data)[1])

for (k in 1:dim(segment.data)[1]){
  xy <- c(segment.data$x[k], segment.data$y[k])
  x.min <- xy[1] - 1
  x.max <- xy[1] + 1
  y.min <- xy[2] - 1
  y.max <- xy[2] + 1

  depth.xy <- depth.df[depth.df$X >= x.min & depth.df$X <= x.max &
                         depth.df$Y >= y.min & depth.df$Y <= y.max, 'depth']

  if (!is.null(length(depth.xy))){
    # remove positive values
    depth.out <- c(mean = mean(depth.xy[depth.xy <= 0], na.rm = T),
                   sd = sd(depth.xy[depth.xy <= 0], na.rm = T),
                   n = length(depth.xy[depth.xy <= 0]))
  }
  depth.m[k] <- depth.out[1]
  depth.sd[k] <- depth.out[2]
  depth.n[k] <- depth.out[3]
}

segment.data.cov <- cbind(segment.data,
                          depth_mean = depth.m,
                          depth_SD = depth.sd,
                          depth_n = depth.n)

save(segment.data.cov,
     file = paste0('RData/segmentDataWithCovariatesDepth_', Sys.Date(), '.RData'))
