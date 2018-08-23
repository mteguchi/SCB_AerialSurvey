#createIndexedDepths_SCB

# creates depth readings for indexed SCB

# 25 November 2016
# Tomo Eguchi

rm(list=ls())
source('CcSCB_functions.R')

depth.file <- read.csv('Data/ocean/depth.csv')
names(depth.file) <- c("Y", "X", "depth")
depth.sp <- latlon2sp(depth.file, center.UTM)
var.df <- data.frame(X = depth.sp$newX,
                     Y = depth.sp$newY,
                     var = depth.sp$depth)

# use prediction.data dataframe:
x.range <- seq(from = min(pred.data$newX),
               to = max(pred.data$newX), by = 2)

y.range <- seq(from = min(pred.data$newY),
               to = max(pred.data$newY), by = 2)

# create the output data frame  ##########################
xy.ranges <- expand.grid(x.range, y.range)
names(xy.ranges) <- c('X', 'Y')
xy.idx <- expand.grid(1:length(x.range), 1:length(y.range))
names(xy.idx) <- c('X.idx', 'Y.idx')
xy.ranges <- cbind(xy.ranges, xy.idx)

# initialize index vectors
# faster to make two loops than combined them together
# missing.x <- vector(mode = 'numeric', length = length(x.range))
for (x in 1:length(x.range)){
   var.df$X.idx[var.df$X >= x.range[x] &
                  var.df$X < (x.range[x]+2)] <- x
}

    # missing.y <- vector(mode = 'numeric', length = length(y.range))
for (y in 1:length(y.range)){
  var.df$Y.idx[var.df$Y >= y.range[y] &
                 var.df$Y < (y.range[y]+2)] <- y
}
# # Use ddply in 'plyr' package
xy.means <- ddply(var.df, .(X.idx, Y.idx),
                  summarize,
                  mean = mean(var, na.rm = TRUE))

xy.means <- na.omit(xy.means)
xy.ranges$mean <- NA
for (k in 1:dim(xy.ranges)[1]){
  tmp.val <- xy.means[xy.means$X.idx == xy.ranges[k, 'X.idx'] &
                        xy.means$Y.idx == xy.ranges[k, 'Y.idx'],
                       'mean']
  if (length(tmp.val) > 0) xy.ranges$mean[k] <- tmp.val
}

save(xy.ranges, file = 'RData/indexed_SCB/depth_idx_SCB.RData')


