#capture area script:

# Creates track lines within the study area in the southern half of
# the SCB.  To change the study area, Change the search.area and
# southSeg. To change the distance between lines, change dlines.
#
# When save.image is TRUE, it creates a figure of track lines and
# saves the end points of track lines in a file.

# 16 August 2017
# Tomo Eguchi


rm(list=ls())
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

source('CcSCB_functions.R')
library(geosphere)
library(rgeos)

save.image <- F
save.data <- T
dpi <- 600
SanClemente <- 'N'

dlines <- 10    # distance between lines in km

# airport locations have been moved to CcSCB_functions.R

# define the northern limit: 33.5 N
# western limit is 118.3W
search.area <- filter(coast.line, Y <= 33.5 & Y >= 32.6)[, c('X', 'Y')]
N.most <- search.area[search.area$Y == max(search.area$Y),]
S.most <- search.area[search.area$Y == min(search.area$Y),]

S.tip.SanClemente <- c(-118.3453, 32.8178)
N.tip.SanClemente <- c(-118.5911, 33.0379)

pt1.df <- data.frame(X = N.most$X,
                     Y = N.most$Y)

if (SanClemente == 'N'){
  # if using N tip of San Clemente:
  pt2.df <- data.frame(X = N.tip.SanClemente[1],
                       Y = N.tip.SanClemente[2])
  southSeg <- data.frame(X = c(-117.1241,-117.2382,-117.2626,
                               -117.4639,-117.8255,
                               S.tip.SanClemente[1],
                               N.tip.SanClemente[1]),
                         Y = c(32.5343, 32.5274, 32.5534,
                               32.5895, 32.626869,
                               S.tip.SanClemente[2],
                               N.tip.SanClemente[2]))

  # using the northern boundary as the guide, we find the rhumb line:
  rhumb_bear <- bearingRhumb(N.most, N.tip.SanClemente)

} else if (SanClemente == 'S'){
  # if using S tip of San Clemente:
  pt2.df <- data.frame(X = S.tip.SanClemente[1],
                       Y = S.tip.SanClemente[2])
  southSeg <- data.frame(X = c(-117.1241,-117.2382,-117.2626,
                               -117.4639,-117.8255,
                               S.tip.SanClemente[1]),
                         Y = c(32.5343, 32.5274, 32.5534,
                               32.5895, 32.626869,
                               S.tip.SanClemente[2]))

  # using the northern boundary as the guide, we find the rhumb line:
  rhumb_bear <- bearingRhumb(N.most, S.tip.SanClemente)

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

## draw transect lines:
# find the distance between the N and S tips of the study area in km:
dist_NS <- distGeo(N.most, S.most)/1000
nlines <- ceiling(dist_NS/dlines)
k<-2
for (k in 2:nlines){
  # find two points on the next line
  # Offshore
  pt2.df[k,] <- destPointRhumb(pt2.df[k-1,], rhumb_bear-90, dlines*1000)
  # Inshore - go long enough because it will be truncated later.
  pt1.df[k,] <- destPointRhumb(pt2.df[k,], rhumb_bear-180, nm2km(60)*1000)
  #pt1.df[k,] <- destPointRhumb(pt1.df[k-1,], rhumb_bear-90, dlines*1000)

}

pt1.df.sp <- latlon2sp(in.df = pt1.df, center.UTM = center.UTM)
pt2.df.sp <- latlon2sp(in.df = pt2.df, center.UTM = center.UTM)

# find intersections between lines and the outside boundary line:
# Create Lines objects and put them in a list
lines.list <- vector(mode = 'list', length = (nlines-1))
k <- 3
for (k in 2:nlines){
  line1 <- as.matrix(rbind(pt1.df.sp@coords[k,],
                           pt2.df.sp@coords[k,]))
  lines.list[[k-1]] <- Lines(list(Line(line1)),
                             ID = paste0('line', k))
}

# Create SpatialLines object
line1.Sp <- SpatialLines(lines.list,
                         proj4string = CRS("+proj=utm +zone=10 ellps=WGS84"))

# use gIntersection to find intersections between lines and the polygon
# This was a headache... found some code, which is saved in gIntersection_example.R
edges <- coordinates(gIntersection(line1.Sp, search.area.Sp.poly))

# create two empty data frames for begin and end points
transect.begin.lines.Sp <- data.frame(X = NA,
                                      Y = NA)
transect.end.lines.Sp <- data.frame(X = NA,
                                    Y = NA)

# extract the edge points into the dataframes
for (k in 1:length(edges[[1]])){
  transect.begin.lines.Sp[k, 'X'] <- edges[[1]][[k]][1, 'x']
  transect.begin.lines.Sp[k, 'Y'] <- edges[[1]][[k]][1, 'y']
  transect.end.lines.Sp[k, 'X'] <- edges[[1]][[k]][2, 'x']
  transect.end.lines.Sp[k, 'Y'] <- edges[[1]][[k]][2, 'y']

}

# flip the data frames upside down so they are numbered from north to south
# apply converts dataframes into matrices so convert them bacck to
# dataframes. Add the first line - the northern boundary line first.
transect.begin.lines.Sp <- as.data.frame(apply(rbind(transect.begin.lines.Sp,
                                                     pt1.df.sp@coords[1, ]), 2, rev))
transect.end.lines.Sp <- as.data.frame(apply(rbind(transect.end.lines.Sp,
                                                   pt2.df.sp@coords[1, ]), 2, rev))

# convert these into spatial objects with appropriate projections
coordinates(transect.begin.lines.Sp) <- c('X', 'Y')
proj4string(transect.begin.lines.Sp) <- CRS("+proj=utm +zone=10 ellps=WGS84")
coordinates(transect.end.lines.Sp) <- c('X', 'Y')
proj4string(transect.end.lines.Sp) <- CRS("+proj=utm +zone=10 ellps=WGS84")

# convert them into lat/lon
transect.begin.lines.latlon <- spTransform(transect.begin.lines.Sp,
                                           CRS("+proj=longlat +datum=WGS84"))

transect.end.lines.latlon <- spTransform(transect.end.lines.Sp,
                                           CRS("+proj=longlat +datum=WGS84"))

# combine them in one data frame along with track lines:
transect.lines.latlon <- data.frame(lineID = 1:nrow(transect.begin.lines.Sp@coords),
                                    latInshore = transect.begin.lines.latlon@coords[,'Y'],
                                    lonInshore = transect.begin.lines.latlon@coords[,'X'],
                                    latOffshore = transect.end.lines.latlon@coords[, 'Y'],
                                    lonOffshore = transect.end.lines.latlon@coords[, 'X'])

# provide decimal degrees and decimal minutes in the output:
transect.lines.latlon$degLatInshore <- trunc(transect.lines.latlon$latInshore)
transect.lines.latlon$minLatInshore <- abs((transect.lines.latlon$latInshore - trunc(transect.lines.latlon$latInshore)) * 60)
transect.lines.latlon$degLonInshore <- trunc(transect.lines.latlon$lonInshore)
transect.lines.latlon$minLonInshore <- abs((transect.lines.latlon$lonInshore - trunc(transect.lines.latlon$lonInshore)) * 60)

transect.lines.latlon$degLatOffshore <- trunc(transect.lines.latlon$latOffshore)
transect.lines.latlon$minLatOffshore <- abs((transect.lines.latlon$latOffshore - trunc(transect.lines.latlon$latOffshore)) * 60)
transect.lines.latlon$degLonOffshore <- trunc(transect.lines.latlon$lonOffshore)
transect.lines.latlon$minLonOffshore <- abs((transect.lines.latlon$lonOffshore - trunc(transect.lines.latlon$lonOffshore)) * 60)

transect.lines.latlon$distInNM <- km2nm(distGeo(transect.begin.lines.latlon@coords,
                                                transect.end.lines.latlon@coords)/1000)

transect.lines.latlon$dist2MYF <- km2nm(distGeo(transect.begin.lines.latlon@coords,
                                                MYF)/1000)
transect.lines.latlon$dist2CRQ <- km2nm(distGeo(transect.begin.lines.latlon@coords,
                                                CRQ)/1000)
transect.lines.latlon$dist2RNM <- km2nm(distGeo(transect.begin.lines.latlon@coords,
                                                RNM)/1000)

# the last one is in the bay - remove: (this may not happen when dline is changed)
#transect.lines.latlon <- transect.lines.latlon[-12,]

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
  geom_segment(data = transect.lines.latlon,
               aes(x = lonInshore, y = latInshore,
                   xend = lonOffshore, yend = latOffshore),
               size = 0.9) +
  geom_text(data = transect.lines.latlon,
            aes(x = lonOffshore,
                y = latOffshore, label = lineID),
            color = 'red')+
  coord_map() +
  xlab('') + ylab('')

if (save.data){
  saveRDS(search.area,
          file = paste0("RDSfiles/SearchArea_", Sys.Date(), ".rds"))
  saveRDS(transect.lines.latlon,
          file = paste0("RDSfiles/TransectLines_", Sys.Date(), ".rds"))
  saveRDS(all.coast.line.df,
          file = paste0("RDSfiles/CoastLine_", Sys.Date(), ".rds"))
}


if (save.image) {
  ggsave(plot = p1,
         dpi = dpi,
         file = paste0('Figures/tracklines_SC_', SanClemente, '_',
                       dlines, 'km_', Sys.Date(), '.png'))

  write.table(signif(transect.lines.latlon, digits = 6), sep = ',',
            file = paste0('Data/tracklines_SC_', SanClemente, '_',
                          dlines, 'km_', Sys.Date(), '.csv'),
            append = F, quote = F, row.names = F, col.names = T)
}
