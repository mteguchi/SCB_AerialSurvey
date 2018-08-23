#plot_DSM_predictions

# Tomo Eguchi
# 14 November 2016

rm(list=ls())
source('CcSCB_functions.R')

# get the DSM analysis results:
load('RData/DSMresults_2016-11-14.RData')

off.set <- 4  # area of a cell in square km

# prediction data get loaded here:
# it returns a dataframe with various oceanographic variables that
# were extracted using addCovs2Prediction.R. Dataframe is named
# prediction.data.cov

prediction.data.files <- list.files(path = "Rdata/predictions",
                                    pattern = "predictionDataWithCovariatesDepth_")

for (k in 1:length(prediction.data.files)){
  # the date of prediction needs to be defined:
  prediction.date <- strsplit(strsplit(prediction.data.files[k],
                                       split = '_')[[1]][2],
                              split = '[.]')[[1]][1]
  load(paste0("RData/predictions/",
              prediction.data.files[k]))

  out.filename <- paste0('Figures/DSM_predictions/chl/DSM_',
                         prediction.date, '.png')

  DSM.prediction.data <- prediction.data.cov2
  rm(list = c('prediction.data.cov2'))
  DSM.prediction.data$logChl <- log(DSM.prediction.data$chl_mean)
  DSM.prediction.data$logChl_08 <- log(DSM.prediction.data$chl_08_mean)
  DSM.prediction.data$logChl_14 <- log(DSM.prediction.data$chl_14_mean)
  DSM.prediction.data$logChl_30 <- log(DSM.prediction.data$chl_30_mean)

  # need to automate the following but for now this would do...
  # 11/14/2016
  DSM.prediction.data <- na.omit(DSM.prediction.data[, c('logChl', 'logChl_08',
                                                         'logChl_14', 'logChl_30',
                                                         'x', 'y')])

  best.DSM.model.pred <- predict(best.DSM.model,
                                 DSM.prediction.data,
                                 off.set)
  best.DSM.model.pred[best.DSM.model.pred > 1] <- NA
  pp <- na.omit(cbind(DSM.prediction.data,
                      best.DSM.model.pred))

  # make a study area figure with transect lines overlaid...
  # create a new data frame for plotting
  study.area.df <- data.frame(x = study.area.Sp$newX,
                              y = study.area.Sp$newY)

  # make a plot
  land.color <- '#333333'
  alpha.value <- 0.8

  p <- ggplot() +
    geom_raster(data = pp,
                aes(x = x, y = y, fill = best.model.pred)) +
    scale_fill_gradient(limits = c(0,1),
                        low = "blue",
                        high = "red") +
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
    xlab("x") +
    ylab("y") +
    ggtitle(prediction.date)
  p

  plot.study.area.2015 <- ggplot(data = study.area.df,
                                 aes(x = x, y = y)) +
    geom_polygon(alpha = 0.7) +
    geom_path(color=land.color) +
    #geom_tile(data = depth.df,
    #          aes(x = x, y = y, fill = mean)) +
    geom_polygon(fill = land.color, data = all.islands.df,
                 aes(x=newX, y=newY, group = name),
                 inherit.aes = F,
                 alpha = alpha.value)  +
    ylab("y") +
    xlab("x") +
    geom_line(data = segment.data.2015,
              aes(x=x, y=y, group = Transect.Label),
              size = 1.2) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())

  plot.study.area.2015

  plot.study.area.2011 <- ggplot(data = study.area.df,
                                 aes(x = x, y = y)) +
    geom_polygon(alpha = 0.7) +
    geom_path(color=land.color) +
    #geom_tile(data = depth.df,
    #          aes(x = x, y = y, fill = mean)) +
    geom_polygon(fill = land.color, data = all.islands.df,
                 aes(x=newX, y=newY, group = name),
                 inherit.aes = F,
                 alpha = alpha.value)  +
    ylab("y") +
    xlab("x") +
    geom_line(data = segment.data.2011,
              aes(x=x, y=y, group = Transect.Label),
              size = 1.2) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())
  plot.study.area.2011

  tmp <- segment.data.2015[observation.data$Sample.Label,]
  p0 <- ggplot(data = prediction.data) +
    geom_tile(aes(x = x, y = y, fill = depth_mean)) +
    geom_point(data = segment.data.2015,
               aes(x=x, y=y, color = sst_mean),
               size = 1.2) +
    geom_point(data = tmp, aes(x = x, y = y, color = sst_30_mean),
               shape = 17, size = 1.5) +   # filled triangle
    scale_color_gradient(low = 'blue', high = 'red') +
    ggtitle('2015')

  p0

  #tmp <- segment.data.2015[observation.data$Sample.Label,]
  p1 <- ggplot(data = prediction.data) +
    geom_tile(aes(x = x, y = y, fill = depth_mean)) +
    geom_point(data = segment.data.2011,
               aes(x=x, y=y, color = sst_mean),
               size = 1.2) +
    #geom_point(data = tmp, aes(x = x, y = y, color = sst_30_mean),
    #           shape = 17, size = 1.5) +   # filled triangle
    scale_color_gradient(low = 'blue', high = 'red') +
    ggtitle('2011')

  p1


}
