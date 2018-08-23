#plot_SSTat2015Survey

# plots SST data for aerial survey 2015
# Plots of track lines with SST, histograms of SST along the track lines
# and SST at sightings

rm(list = ls())
source('CcSCB_functions.R')
save.fig <- F

sst.tracklines <- read.csv('Data/tmpFlightPathsSST.txt', header = F)
colnames(sst.tracklines) <- c('Date1', 'Time1', 'Latitude', 'Longitude',
                              'LineID', 'Effort', 'SST', 'Depth')
sst.tracklines <- sst.tracklines[!is.na(sst.tracklines$SST),]
sst.tracklines <- sst.tracklines[sst.tracklines$Effort == 1,]
sst.sightings <- read.csv('Data/onEftTurtleTemp.txt')

# compute the middle 90% and 50%:
middle50 <- quantile(sst.sightings$Temp, c(0.25, 0.75))
middle90 <- quantile(sst.sightings$Temp, c(0.05, 0.95))

lines.df.latlon$fYear <- as.factor(lines.df.latlon$Year)
lines.df.latlon.2015 <- subset(lines.df.latlon, fYear == 2015)
study.area.Sp <- latlon2sp(study.area, center.UTM = center.UTM)

study.area.df2 <- data.frame(lat = study.area.Sp@data$lat,
                             lon = study.area.Sp@data$lon-360)

coast.line <- getCoastLine(paste0(dirSelector()$Rdir,
                                  'OceanDepths/coast/coast_Epac.txt'),
                           lon.limits = c(-131, -115),
                           lat.limits = c(28, 36))

coast.line.df <- do.call(rbind, coast.line)
colnames(coast.line.df) <- c('X', 'Y', 'idx')
#coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)

# make a plot
p1 <- ggplot() +
  geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
            aes(x = X, y = Y)) +
  geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
            aes(x = X, y = Y)) +
  geom_segment(data = lines.df.latlon.2015,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY),
               size = 1.2,
               color = "#FDE725FF")  +
  geom_polygon(data = study.area.df2,
               aes(x = lon, y = lat),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx)) +
  geom_point(data = sst.sightings,
             aes(x = Longitude,
                 y = Latitude, color = Temp),
             shape = 19, size = 2) +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = 2, size = 1.5,
            alpha = 0.6) +
  coord_map() +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("SST at loggerhead sightings") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0))

# p2 <- ggplot() +
#   geom_histogram(data = sst.sightings,
#                  aes(x = Temp), binwidth = 0.25) +
#   xlab("SST (C)") +
#   ggtitle("SST at loggerhead sightings") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10, hjust = 0.5),
#         legend.text = element_text(size = 8, vjust = 0))

p3 <- ggplot() +
  geom_histogram(data = sst.tracklines,
                 aes(x = SST, y = ..density..),
                 binwidth = 0.25,
                 alpha = 0.3,
                 fill = 'darkblue') +
  geom_histogram(data = sst.sightings,
                 aes(x = Temp, y = ..density..),
                binwidth = 0.25,
                alpha = 0.3,
                 fill = 'darkgreen') +
  ylab("Density") +
  xlab("SST (C)") +
#  ggtitle("SST along track lines") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 12, vjust = 0))

p3.1 <- ggplot() +
  geom_histogram(data = sst.tracklines,
                 aes(x = SST, y = ..density..),
                 binwidth = 0.25,
                 alpha = 0.3,
                 fill = 'darkblue') +
  geom_histogram(data = sst.sightings,
                 aes(x = Temp, y = ..density..),
                 binwidth = 0.25,
                 alpha = 0.3,
                 fill = 'darkred')+
  stat_density(data = sst.tracklines,
               aes(x = SST),
               bw=0.2, geom='density',
               color = 'darkblue',
               size = 1) +
  stat_density(data = sst.sightings,
               aes(x = Temp),
               bw=0.2,geom='density',
               color = 'darkred',
               size = 1) +
  #xlim(10, 25) + ylim(0, 1) +
  ylab("Density") +
  xlab("SST (C)") +
  #  ggtitle("SST along track lines") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

f <- function(x) {
  r <- c(quantile(x, probs = c(0.05, 0.25)),
         mean(x),
         quantile(x, probs = c(0.75, 0.95)))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

o <- function(x) {
  subset(x, x < quantile(x, 0.05) | quantile(x, 0.95) < x)
}

# p3.2 <- ggplot(data = sst.sightings,
#              aes(x = 1.0, y = Temp)) +
#   stat_summary(fun.data = f, geom="boxplot") +
#   stat_summary(fun.y = o, geom="point")  +
#   ylim(10, 25)+ coord_flip() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank()) +
#   theme_void()
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
#         axis.ticks = element_blank()) +
#   theme_void()
#
#
# p3.3 <- cowplot::plot_grid(p3.2, p0.1, p3.1,
#                            align = 'v',
#                            ncol = 1,
#                            rel_heights = c(1, 1, 6))

# per Scott Benson's request:
# calculate # turtles/km by SST bin
bins <- seq(from = floor(min(sst.tracklines$SST)),
            to = ceiling(max(sst.tracklines$SST)),
            by = 0.25)

survey.dist <- NA
turtle.sightings <- NA

# take one bin at a time
k <- 1
k0 <- 5
for(k0 in 1:(length(bins) -1 )){
  track.tmp1 <- filter(sst.tracklines, SST > bins[k0], SST <= bins[k0 + 1])
  total.dist <- 0
  if(nrow(track.tmp1) > 0){
    for(k in 1:(nrow(track.tmp1)-1)){
      temp.dist <- geosphere::distGeo(c(track.tmp1$Longitude[k],
                                        track.tmp1$Latitude[k]),
                                      c(track.tmp1$Longitude[k+1],
                                        track.tmp1$Latitude[k+1]))/1000
      if (temp.dist > 3.3)  # if more than 3.3 km ~ 1.8 nm (about one minute of silence)
        temp.dist <- 0  # skip that difference - starting calculation new

      total.dist <- total.dist + temp.dist
    }

  }
  survey.dist[k0] <- total.dist
  turtle.tmp1 <- filter(sst.sightings, Temp > bins[k0], Temp <= bins[k0 + 1])
  turtle.sightings[k0] <- nrow(turtle.tmp1)
}

turtle.sst.df <- data.frame(bins = bins[1:(length(bins)-1)],
                            effort = survey.dist,
                            turtle = turtle.sightings,
                            turtle.effort = turtle.sightings/survey.dist)
p4 <- ggplot() +
  geom_line(data = turtle.sst.df,
            aes(x = bins, y = turtle.effort),
            size = 1.0) +
  ylab("turtles/km") +
  xlab("SST (C)") +
  #  ggtitle("SST along track lines") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 12, vjust = 0))
p4

if(save.fig){
  ggsave(filename = 'Figures/sightingsSST.png',
         plot = p1, device = 'png',
         dpi = 600)

  ggsave(filename = 'Figures/SST_sightings_tracks.png',
         plot = p3,
         dpi = 600,
         device = 'png')

  ggsave(filename = 'Figures/SST_sightings_tracks_KDE.png',
         plot = p3.1,
         dpi = 600,
         device = 'png')

  ggsave(filename = 'Figures/turtlesPerKmSST.png',
         plot = p4,
         dpi = 600,
         device = 'png')

}
