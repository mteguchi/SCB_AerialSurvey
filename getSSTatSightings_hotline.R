#plot_SSTatSightings.

# plots SST at where turtles were sighted.


rm(list=ls())

library(dplyr)
library(lubridate)
library(cowplot)
library(viridis)

source('CcSCB_functions.R')

save.fig <- F
# prep4logisticRegression_04Nov2016.R created logistic regression
# data
#load('RData/data4logisticRegression_2016-12-07.RData')
#survey.data <- dat.all.df
#sighted.locs <- subset(dat.all.df, nSI.CC > 0)

  # hotline sightings data
  hotline.data <- read_csv('Data/HotlineSightings_Cc.csv') %>%
    select(., Date_Observed, Latitude, Longitude, Genus) %>%
    dplyr::rename(., Date = Date_Observed, Species = Genus)

  SCB.data <- subset(hotline.data, Longitude > -127.5 &
                       Longitude < -115 & Latitude > 28 &
                       Latitude < 36)

  # get SST data for each sighting at 2 km x 2 km squares
  colnames(SCB.data) <- c('Date', 'Y', 'X')

  # convert to linear units first:
  SCB.data.Sp <- latlon2sp(SCB.data, center.UTM)

  SCB.data.km <- SCB.data.Sp@data
  SCB.data.km$beginX <- SCB.data.km$newX - 1
  SCB.data.km$endX <- SCB.data.km$newX + 1
  SCB.data.km$beginY <- SCB.data.km$newY - 1
  SCB.data.km$endY <- SCB.data.km$newY + 1

  # then convert them back to lat lon;
  SCB.data.km.beginXY <- data.frame(newX = SCB.data.km$beginX,
                                    newY = SCB.data.km$beginY)
  SCB.data.latlon.begin <- sp2latlon(SCB.data.km.beginXY, center.UTM)

  SCB.data.km.endXY <- data.frame(newX = SCB.data.km$endX,
                                  newY = SCB.data.km$endY)

  SCB.data.latlon.end <- sp2latlon(SCB.data.km.endXY, center.UTM)

  SCB.data.latlon <- data.frame(beginLat = SCB.data.latlon.begin@coords[,'Y'],
                                beginLon = SCB.data.latlon.begin@coords[,'X'],
                                endLat = SCB.data.latlon.end@coords[,'Y'],
                                endLon = SCB.data.latlon.end@coords[,'X'],
                                Date = SCB.data$Date,
                                Species = SCB.data$Species)

  SCB.data.latlon$sst <- NA
  k.1 <- 1
  for (k in 97:nrow(SCB.data)){

    xlim <- unlist(c(SCB.data.latlon[k, 'beginLon'],
              SCB.data.latlon[k, 'endLon']))
    ylim <- unlist(c(SCB.data.latlon[k, 'beginLat'],
              SCB.data.latlon[k, 'endLat']))

    tlim <- SCB.data.latlon[k, 'Date']

    file.name <- paste0('Data/sst_data/Cc_hotline_sighting_', xlim[1], '_',
                        ylim[1], '_', tlim[[1]], '.nc')

    if (tlim[[1]] > as.Date('2002-01-01')){
      sstURL <-paste0('https://coastwatch.pfeg.noaa.gov/erddap/griddap/',
                      'jplMURSST41.nc?analysed_sst[(',
                      tlim[[1]], '):1:(', tlim[[1]], ')][(',
                      ylim[1], '):(', ylim[2], ')][(',
                      xlim[1], '):(', xlim[2], ')]')
      varname <- 'analysed_sst'

    } else {
      # this one is 0.0417 degree resolutions...
      sstURL <- paste0('http://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiPH53sstd1day.nc?sea_surface_temperature[(', tlim[[1]], 'T12:00:00Z):1:(', tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')],dt_analysis[(',
                       tlim[[1]], 'T12:00:00Z):1:(', tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')],wind_speed[(',
                       tlim[[1]], 'T12:00:00Z):1:(', tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')],sea_ice_fraction[(',
                       tlim[[1]], 'T12:00:00Z):1:(', tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')],quality_level[(',
                       tlim[[1]], 'T12:00:00Z):1:(', tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')],',
                       'pathfinder_quality_level[(', tlim[[1]], 'T12:00:00Z):1:(',
                       tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')],l2p_flags[(',
                       tlim[[1]], 'T12:00:00Z):1:(', tlim[[1]],
                       'T12:00:00Z)][(', signif(max(ylim) + 0.02, digits = 5), '):1:(',
                       signif(min(ylim) - 0.02, digits = 5), ')][(',
                       signif(min(xlim) + 0.02, digits = 5), '):1:(',
                       signif(max(xlim) + 0.02, digits = 5), ')]')

      varname <- 'sea_surface_temperature'

    }

    #}
    download.file(sstURL,
                  destfile= file.name,
                  mode='wb')
    test <- nc_open(file.name)

    SCB.data.latlon[k, 'sst'] <- mean(ncvar_get(test, varname), na.rm = T)

    nc_close(test)

  }

  saveRDS(SCB.data.latlon,
          file = 'RDSfiles/SSTatHotlineSightings.rds')

# #
# histo.data <- subset(SCB.data.latlon, sst>0)
# # create longitude categories
# histo.data$lonCat <- cut(histo.data$beginLon,
#                          breaks = seq(from=floor(min(histo.data$beginLon)),
#                                       to = ceiling(max(histo.data$beginLon))),
#                          length = 20)
# histo.data$sstCat <- cut(histo.data$sst,
#                          breaks = seq(from = floor(min(histo.data$sst)),
#                                       to = ceiling(max(histo.data$sst)),
#                                       by = 0.5))
# # create longitude means for each sst bin
# lon.means <- aggregate(histo.data$beginLon,
#                        list(histo.data$sstCat),
#                        mean)
#
# # add another variable
# histo.data$meanLon <- NA
# for (k in 1:nrow(lon.means)){
#   histo.data[histo.data$sstCat == lon.means[k,1], 'meanLon'] <- lon.means[k,2]
#
# }
#
# # code bits from here: http://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot
#
# f <- function(x) {
#   r <- c(quantile(x, probs = c(0.05, 0.25)),
#          mean(x),
#          quantile(x, probs = c(0.75, 0.95)))
#   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
#   r
# }
#
# o <- function(x) {
#   subset(x, x < quantile(x, 0.05) | quantile(x, 0.95) < x)
# }
#
# p0 <- ggplot(data = histo.data, aes(x = 1.0, y = sst)) +
#   stat_summary(fun.data = f, geom="boxplot") +
#   stat_summary(fun.y = o, geom="point")  +
#   ylim(10, 25)+ coord_flip() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank())
#
# # Howell's distribution
# data.howell <- data.frame(x = 1, mean = 16.3,
#                           min = 11,0,
#                           y25 = 15.6,
#                           y75 = 17.1,
#                           max = 21.6)
#
# p0.1 <- ggplot(data = data.howell, aes(x = x)) +
#   geom_boxplot(aes(ymin = min, lower = y25,
#                    middle = mean,
#                    upper = y75, ymax = max),
#                stat = 'identity',
#                fill = 'gray') +
#   ylim(10, 25) + coord_flip() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank())
#
#
# p1 <- ggplot(data = histo.data, aes(sst)) +
#   geom_histogram(binwidth = 0.5,
#                  color = 'black') +
#   theme(axis.text = element_text(size = 12, face = 'bold'),
#         axis.title = element_text(size = 14, face = 'bold')) +
#   xlim(10, 25) +
#     xlab('SST (C)') + ylab('Frequency')
#
# p1.0 <- plot_grid(p0.1, p0, p1,
#                   align = 'v',
#                   ncol = 1,
#                   rel_heights = c(1, 1, 6))
#
# if (save.fig){
#   ggsave(plot = p1.0,
#          dpi = 1200,
#          file = 'Figures/SSTatSightings.png')
#
# }
#
# # look at before 2015 survey:
# histo.data.pre2015 <- subset(histo.data, date < as.Date('2015-09-23'))
# p2 <- ggplot(data = histo.data.pre2015) +
#   geom_histogram(aes(sst), binwidth = 0.5, color = 'black') +
#   theme(axis.text = element_text(size = 12, face = 'bold'),
#         axis.title = element_text(size = 14, face = 'bold')) +
#   xlab('SST (C)') + ylab('Frequency')
#
# if (save.fig){
#   ggsave(plot = p2,
#          dpi = 1200,
#          file = 'Figures/SSTatSightings_preSurvey.png')
#
# }
#
# p3 <- ggplot(histo.data) +
#   geom_point(aes(x = beginLon, y = sst)) +
#   xlab('Longitude') + ylab('SST (C)')
#
#
#
#
# coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
#                            lon.limits = c(-131, -115),
#                            lat.limits = c(28, 36))
#
# coast.line.df <- do.call(rbind, coast.line)
# colnames(coast.line.df) <- c('X', 'Y', 'idx')
# coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)
#
# land.color <- '#333333'
# p2 <- ggplot() +
#   geom_path(data = map.LTCA,
#             aes(x = X, y = Y),
#             linetype = 2, size = 1.5)+
#   geom_polygon(fill = land.color,
#                data = coast.line.df,
#                aes(x=X, y=Y, group = idx)) +
#   geom_point(data = histo.data,
#              aes(x = beginLon, y = beginLat, color = sst)) +
#   scale_fill_gradientn(na.value = land.color,
#                        limits = c(16, 24),
#                        colors = rainbow(n = 20)) +
#   coord_map() +
#   ylab("Latitude") +
#   xlab("Longitude") +
#   ggtitle("Loggerhead Turtle Sightings") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10, hjust = 0.5),
#         legend.text = element_text(size = 8, vjust = 0))
#
# ggsave(plot = p2,
#        dpi = 1200,
#        file = 'Figures/SSTat')
#
# save(list = ls(),
#      file = paste0('RData/SSTatSightings_',
#                    Sys.Date(), '.RData'))


