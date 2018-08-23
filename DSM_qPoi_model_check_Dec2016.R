#plot_DSM_predictions

# Tomo Eguchi
# 14 November 2016

rm(list=ls())
source('CcSCB_functions.R')
save.images <- F
#save.images <- T
# get the DSM analysis results:
load('RData/DSMresults_qPoi_2016-12-01.RData')

off.set <- 4  # area of a cell in square km

# prediction data get loaded here:

prediction.data.files <- list.files(path = "RData/predictions",
                                    pattern = "predictionDataWithCovariates_")

prediction.date <- '2015-09-23'
k <- grep(prediction.date, prediction.data.files)

# the date of prediction needs to be defined:
date.str <- strsplit(strsplit(prediction.data.files[k],
                              split = '_')[[1]][2],
                     split = '[.]')[[1]][1]
load(paste0("RData/predictions/",
            prediction.data.files[k]))
#out.filename <- paste0('Figures/DSM_predictions/chl/DSM_',
#                       date.str, '.png')

DSM.prediction.data <- prediction.data.cov
rm(list = c('prediction.data.cov'))
DSM.prediction.data$logChl <- log(DSM.prediction.data$chl_mean)
DSM.prediction.data$logChl_08 <- log(DSM.prediction.data$chl_08_mean)
DSM.prediction.data$logChl_14 <- log(DSM.prediction.data$chl_14_mean)
DSM.prediction.data$logChl_30 <- log(DSM.prediction.data$chl_30_mean)

# need to automate the following but for now this would do...
# 11/14/2016
DSM.prediction.data <- na.omit(DSM.prediction.data[, c('logChl',
                                                       'logChl_08',
                                                       'logChl_14',
                                                       'logChl_30',
                                                       'sst_mean',
                                                       'sst_08_mean',
                                                       'sst_14_mean',
                                                       'sst_30_mean',
                                                       'x', 'y')])

best.DSM.model.pred <- predict(best.DSM.model[[best.DSM.model.name]],
                               DSM.prediction.data,
                               off.set)

log.pred <- log10(best.DSM.model.pred)
#best.DSM.model.pred[best.DSM.model.pred > 100] <- NA
pp <- na.omit(cbind(DSM.prediction.data,
                    best.DSM.model.pred,
                    log.pred))

pp.100 <- pp[pp$best.DSM.model.pred > 100,]
pp.1000 <- pp[pp$best.DSM.model.pred > 1000,]

# look at the ones inside the study area.
# trunc.study.area is made in CcSCB_functions.R
x <- pp$x
y <- pp$y
colnames(trunc.study.area) <- c("x", "y")
idx <- inSide(trunc.study.area, x, y)
tmp<-pp$best.DSM.model.pred[idx]
sum(tmp)   # but this is so wrong... why's that?

# make a study area figure with transect lines overlaid...
# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# plot sightings if any
sightings.1 <- sightings.data[sightings.data$date == as.Date(date.str),
                              c('Longitude', 'Latitude')]
sightings.2 <- ccData[ccData$date == as.Date(date.str),
                      c('mlon', 'mlat')]

colnames(sightings.2) <- c('Longitude', 'Latitude')

sightings.all <- rbind(sightings.1, sightings.2)
names(sightings.all) <- c("X", "Y")

lines.1 <- lines.df[lines.df$date == as.Date(date.str), ]

# make a plot
land.color <- '#333333'
alpha.value <- 0.8

p <- ggplot() +
  geom_raster(data = pp,
              aes(x = x, y = y,
                  fill = log.pred)) +
  scale_fill_gradient(limits = c(0,3),
                      low = "gray",
                      high = "red",
                      name = "DSM\nprediction\nlog10(N)") +
  # scale_fill_gradientn(na.value = land.color,
  #                      limits = c(0, 1),
  #                      colors = rainbow(n = 20)) +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=newX, y=newY, group = name),
               inherit.aes = F) +
  geom_polygon(fill = land.color,
               data = coast.line.xy,
               aes(x=newX, y=newY))  +
  #geom_segment(data = lines.strata.df.2,
  #             aes(x = newX_offshore, xend = newX_inshore,
  #                 y = newY_offshore, yend = newY_inshore),
  #             linetype = 'dotted') +
  #geom_path(data = study.area.Sp@data,
  #          aes(x = newX, y = newY)) +
  geom_path(data = as.data.frame(inshore.polygon@coords),
            aes(x = newX, y = newY)) +
  geom_path(data = as.data.frame(offshore.polygon@coords),
            aes(x = newX, y = newY)) +
  xlab("x") +
  ylab("y") +
  ggtitle(date.str) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0))

if (dim(lines.1)[1] >0){
  p <-  p +
    geom_segment(data = lines.1,
                 aes(x = beginX, xend = endX,
                     y = beginY, yend = endY),
                 color = 'yellow',
                 size = 0.8)
}

if (dim(sightings.all)[1] > 0){
  sightings.all.XY <- latlon2sp(sightings.all, center.UTM)@data
  row.names(sightings.all.XY) <- NULL
  p <- p +
    geom_point(data = sightings.all.XY,
               aes(x = newX, y = newY),
               colour = 'black',
               fill = 'blue',
               shape = 23,
               size = 2)

}
if (save.images){
  ggsave(plot = p,
         dpi = 600,
         width = 8.96,
         height = 5.74,
         file = paste0('figures/DSM_predictions/DSM_prediction_',
                       date.str, '.png'))
}


