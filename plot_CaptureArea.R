#Plots capture area:

# Using the track lines created in CaptureArea.R script and summarized flown
# track lines from survey_2017.R and summary_track_lines.R, this script
# makes a plot of completed track lines overlaid with planned lines.
#

# 31 August 2017
# Tomo Eguchi


rm(list=ls())
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

source('CcSCB_functions.R')
library(geosphere)
library(rgeos)

save.image <- F
dpi <- 600
SanClemente <- 'N'
dlines <- 10    # distance between lines in km
runDate <- '2017-08-27'

# check with the output file. This file contains planned lines.
planned.track.lines.file <- paste0('data/tracklines_SC_', SanClemente, '_',
                                   dlines, 'km_', runDate, '.csv')

# define the northern limit: 33.5 N
# western limit is 118.3W
search.area <- filter(coast.line, Y <= 33.5 & Y >= 32.6)[, c('X', 'Y')]
N.most <- search.area[search.area$Y == max(search.area$Y),]
S.most <- search.area[search.area$Y == min(search.area$Y),]

S.tip.SanClemente <- c(-118.3453, 32.8178)
N.tip.SanClemente <- c(-118.5911, 33.0379)

pt1.df <- data.frame(X = N.most$X,
                     Y = N.most$Y)

# manually removed lines 12 and 14 from the file
lines.data <- read.csv(file = planned.track.lines.file,
                       header = T)
if (SanClemente == 'N'){
  # if using N tip of San Clemente:
  southSeg <- data.frame(X = c(-117.1241,-117.2382,-117.2626,
                               -117.4639,-117.8255,
                               S.tip.SanClemente[1],
                               N.tip.SanClemente[1]),
                         Y = c(32.5343, 32.5274, 32.5534,
                               32.5895, 32.626869,
                               S.tip.SanClemente[2],
                               N.tip.SanClemente[2]))

} else if (SanClemente == 'S'){
  southSeg <- data.frame(X = c(-117.1241,-117.2382,-117.2626,
                               -117.4639,-117.8255,
                               S.tip.SanClemente[1]),
                         Y = c(32.5343, 32.5274, 32.5534,
                               32.5895, 32.626869,
                               S.tip.SanClemente[2]))
}
southSeg.sp <- latlon2sp(southSeg, center.UTM = center.UTM)

search.area <- rbind(search.area,
                     southSeg,
                     search.area[1,])

search.area.Sp <- latlon2sp(search.area, center.UTM)
search.area.df <- data.frame(x = search.area.Sp$newX,
                             y = search.area.Sp$newY)

# Create SpatialPolygon object from search area:
search.area.Sp.poly <- SpatialPolygons(list(Polygons(list(Polygon(search.area.Sp@coords)),
                                                     ID = '1')),
                                       proj4string = CRS("+proj=utm +zone=10 ellps=WGS84"))

# Get the coast line
all.coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                               lon.limits = c(-119, -117),
                               lat.limits = c(32, 34))

all.coast.line.df <- do.call(rbind, all.coast.line)
colnames(all.coast.line.df) <- c('X', 'Y', 'idx')

# summarized track lines, created by survey_2017.R
lines.2017 <- get.track.lines('Data/tmpTracks_2017.txt')

# make a plot
p1 <- ggplot() +
  geom_polygon(fill = land.color,
               data = all.coast.line.df,
               aes(x = X, y = Y, group = idx))  +
  geom_polygon(data = search.area,
               aes(x = X, y = Y),
               fill = NA, size = 1.2,
               alpha = 0.6,
               color='black') +
  geom_segment(data = lines.data,
               aes(x = lonInshore, y = latInshore,
                   xend = lonOffshore, yend = latOffshore),
               size = 0.5) +
  geom_text(data = lines.data,
            aes(x = lonOffshore,
                y = latOffshore, label = lineID),
            color = 'red')+
  geom_segment(data = lines.2017$data,
               aes(x = Lon1, y = Lat1,
                   xend = Lon2, yend = Lat2),
               size = 1.2, color = 'darkorange') +
  coord_map() +
  xlab('') + ylab('')

p1

if (save.image) {
  ggsave(plot = p1,
         dpi = dpi,
         file = paste0('Figures/completedTrackLines_SC_', SanClemente, '_',
                       dlines, 'km_', Sys.Date(), '.png'))
}
