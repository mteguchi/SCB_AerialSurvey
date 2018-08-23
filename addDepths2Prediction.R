#addDepths2Prediction

# add depths to prediction background data

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

file.names <- list.files(path = "RData/predictions/",
                         pattern = "predictionDataWithCovariates_")

for (k in 1:length(file.names)){
  print(paste(k, 'of', length(file.names)))
  load(paste0("RData/predictions/", file.names[k]))

  # prediction files have the same grid - so extract depths just once
  if (k == 1){
    depth.m <- depth.sd <- depth.n <- vector(mode = 'numeric',
                                             length = dim(prediction.data.cov)[1])

    for (k1 in 1:dim(prediction.data.cov)[1]){
      xy <- c(prediction.data.cov$x[k1], prediction.data.cov$y[k1])
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
      depth.m[k1] <- depth.out[1]
      depth.sd[k1] <- depth.out[2]
      depth.n[k1] <- depth.out[3]

    }

  }
  prediction.data.cov2 <- cbind(prediction.data.cov,
                                depth_mean = depth.m,
                                depth_SD = depth.sd,
                                depth_n = depth.n)

  file.parts <- unlist(strsplit(file.names[k], '_'))
  save(prediction.data.cov2,
       file = paste0('RData/predictions/',
                     file.parts[1], 'Depth_', file.parts[2]))
  rm(list=c('prediction.data.cov'))
}



