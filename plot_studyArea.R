#plot_studyArea

rm(list=ls())

source('CcSCB_functions.R')

save.fig <- FALSE
land.color <- '#333333'

coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                           lon.limits = c(-125, -115),
                           lat.limits = c(27, 38))

# figure out the dimension of the plot:
# > tmp<-data.frame(X = c(-125, -115), Y = c(27,38))
# > tmp.Sp <- latlon2sp(tmp, center.UTM )
# > tmp.Sp
# coordinates      newX      newY
# 1 (301545.8, 2988008) -481.5438 -677.5713
# 2  (1202932, 4236139)  419.8421  570.5599
# > tmp.Sp@data
# newX      newY
# 1 -481.5438 -677.5713
# 2  419.8421  570.5599
# > tmp.Sp@data$newX[2] - tmp.Sp@data$newX[1]
# [1] 901.386
# > tmp.Sp@data$newY[2] - tmp.Sp@data$newY[1]
# [1] 1248.131


coast.line.df <- do.call(rbind, coast.line)

study.area.Sp <- latlon2sp(study.area, center.UTM)
study.area.df2 <- data.frame(lat = study.area.Sp@data$lat,
                             lon = study.area.Sp@data$lon-360)

inshore.study.area.latlon <- as.data.frame(sp2latlon(inshore.study.area,
                                                     center.UTM)@coords)

p2 <- ggplot() +
  geom_polygon(data = study.area.df2,
               aes(x = lon, y = lat),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x = Longitude,
                   y = Latitude, group = idx))  +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = 2, size = 1.5) +
  geom_path(data = inshore.study.area.latlon,
            aes(x = X, y = Y)) +
  geom_segment(data = all.lines.2,
               aes(y = Lat_offshore, yend = Lat_inshore,
                   x = Lon_offshore, xend = Lon_inshore),
               size = 1.0, linetype = 1,
               color = 'darkcyan') +
  coord_map()+   # this is smart!
  #geom_path(data = offshore.box,
  #          aes(x = Longitude, y = Latitude)) +
  ylab("Latitude") +
  xlab("Longitude") +
  #ggtitle("Loggerhead turtle sightings") +
  theme(plot.title = element_text(hjust = 0.5))

if (save.fig){
  ggsave(plot = p2,
         dpi = 1200,
         file = 'Figures/Study_Area_tracklines_large.png')
  # legend.title = element_text(size = 10, hjust = 0.5),
  # legend.text = element_text(size = 8, vjust = 0))

}
