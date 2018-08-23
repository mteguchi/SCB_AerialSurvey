#plot_SSTatSightings.

# plots SST at where turtles were sighted.


rm(list=ls())
library(ggplot2)
# ifelse(Sys.info()[1] == 'Linux',
#        source('~/Documents/R/TomosFunctions.R'),
#        source('~/R/TomosFunctions.R'))
source('CcSCB_functions.R')
save.fig <- F
land.color <- '#333333'
alpha.value <- 0.8

# get the large coast line:
coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                           lon.limits = c(-131, -115),
                           lat.limits = c(28, 36))

coast.line.df <- do.call(rbind, coast.line)
colnames(coast.line.df) <- c('X', 'Y', 'idx')

# data
load('RData/SSTatSightings_2016-12-19.RData')
data1 <- subset(sightings.data.latlon, sst >0)
data1.1 <- data1[, c('medLon', 'medLat', 'sst')]

# these are from the 2011 and 2015 survey
load('RData/data4logisticRegression_2016-12-07.RData')
data2 <- subset(dat.all.df, nSI.CC > 0)
data2.1 <- data2[, c('mlon', 'mlat', 'mean.sst_0')]
colnames(data2.1) <- c('medLon', 'medLat', 'sst')
all.data <- rbind(data1.1, data2.1)
colnames(all.data) <- c('Longitude', 'Latitude', 'sst')

SCB.data <- subset(all.data, Longitude > -127.5 &
                     Longitude < -115 & Latitude > 28 &
                     Latitude < 36)

p1 <- ggplot(data = SCB.data) +
  geom_histogram(aes(x = sst),
                 binwidth = 0.5,
                 color = 'black',
                 fill = 'white') +
  xlab('SST (C)') +
  ylab('') +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p1a <- ggplot(data = all.data) +
  geom_histogram(aes(x = sst),
                 binwidth = 0.5,  # 1.5 make it a bit smoother.
                 color = 'black',
                 fill = 'white') +
  xlab('SST (C)') +
  ylab('') +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

coast.line.df <- do.call(rbind, coast.line)
colnames(coast.line.df) <- c('X', 'Y', 'idx')
coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)

land.color <- '#333333'
p2 <- ggplot() +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = 2, size = 1.5)+
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx)) +
  geom_point(data = SCB.data,
             aes(x = Longitude, y = Latitude, color = sst)) +
  scale_color_gradient(na.value = land.color,
                       limits = c(16, 24),
                       low = 'blue', high = 'red',
                       name = 'SST (C)') +
  coord_map() +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("SST at loggerhead turtle sightings") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0, hjust = 0.5))

if (save.fig){
  ggsave(plot = p1,
         dpi = 1200,
         file = 'Figures/sstAtSightings_histogram.png')

  ggsave(plot = p2,
         dpi = 1200,
         file = 'Figures/sstAtSightings_latlon.png')

}
