#KDE_analysis
# KDE for loggerhead sightings

# Tomo Eguchi
# 4 November 2016


rm(list=ls())

# in Ubuntu, rgl library needs to be installed directly from cran:
# sudo apt-get install r-cran-rgl  4 Nov 2016
# rgl package installatins can be tricky: http://stackoverflow.com/questions/31820865/error-in-installing-rgl-package
library(ks)

save.data <- T
save.images <- F

source('CcSCB_functions.R')
land.color <- '#333333'
alpha.value <- 0.8
# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# for KDE, we only need 2015 data because no sightings in 2011
Sdata.2015$X <- Sdata.2015$mlon
Sdata.2015$Y <- Sdata.2015$mlat
CcData.2015 <- subset(as.data.frame(latlon2sp(Sdata.2015, center.UTM)),
                     species == 'cc')

dat.KDE <- CcData.2015[, c('newX', 'newY')]
# exclude the outlier:
dat.KDE <- dat.KDE[dat.KDE$newY < 100,]

# the following seems to be working but not sure what or how
# bandwidth is computed.
fhat <- kde(x=dat.KDE,
            xmin = c(-267.09, -235.58),
            xmax = c(324.91, 162.42),
            bgridsize = 2.0,
            compute.cont = T)

# create a data frame for KDE output
KDE.estimate <- as.data.frame(expand.grid(x = fhat$eval.points[[1]],
                                         y = fhat$eval.points[[2]]))
KDE.estimate$estimate <- as.vector(fhat$estimate)/max(fhat$estimate)
# get the liens for 2015
data.2015 <- subset(lines.df, Year == 2015)
# get the sighting locations
xy.df <- ccData.Sp@data

p1 <- ggplot() +
  geom_raster(data = KDE.estimate,
              aes(x = x, y = y, fill = estimate)) +
  scale_fill_gradient(low = "white",
                      high = "red",
                      name = 'KDE') +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=newX, y=newY, group = name),
               inherit.aes = F) +
  geom_polygon(fill = land.color,
               data = coast.line.xy,
               aes(x=newX, y=newY))  +
  geom_segment(data = data.2015,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY)) +
  geom_point(data = dat.KDE,
             aes(x = newX, y = newY)) +
  #geom_polygon(data = study.area.df,
  #             aes(x = x, y = y),
  #             fill = NA,
  #             color='black') +
  geom_path(data = as.data.frame(inshore.polygon@coords),
            aes(x = newX, y = newY)) +
  geom_path(data = as.data.frame(offshore.polygon@coords),
            aes(x = newX, y = newY)) +
  stat_contour(data = KDE.estimate,
               aes(x=x, y=y, z=estimate),
               breaks = 0.95,
               colour = "gray") +
  stat_contour(data = KDE.estimate,
               aes(x=x, y=y, z=estimate),
               breaks = 0.5,
               colour = "yellow") +
  # geom_point(data = xy.df,
  #            aes(x = newX, y = newY)) +
  ylab("y") +
  xlab("x")

#plot(fhat)

if (save.images == T)
  ggsave(plot = p1,
         dpi = 600,
         width = 8.96,
         height = 5.74,
         file = paste0('figures/KDE_out_', Sys.Date(), '.png'))

if (save.data == T)
  save(list = "KDE.estimate",
       file = paste0('RData/KDE_out_', Sys.Date(), '.RData'))


