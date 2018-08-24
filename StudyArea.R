#study area script:

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

library(foreach)
library(rgeos)
source('CcSCB_functions.R')

## some constants are defined here:

alt <- 500
## Data files are defined here
# create background grids
# study area
study.area <- read.csv("Data/StudyArea.csv", header = F)
colnames(study.area) <- c('lat', 'lon')
study.area$X <- study.area$lon - 360
study.area$Y <- study.area$lat
study.area.Sp <- latlon2sp(study.area, center.UTM)

plot.SCB.area <- data.frame(X = c(-123, -123, -115, -115, -123),
                            Y = c(28, 36, 36, 28, 28))
plot.SCB.area.Sp <- latlon2sp(plot.SCB.area, center.UTM)
plot.SCB.area.polygon <- Polygon(plot.SCB.area.Sp@data[, c('newX', 'newY')])

# to create shapefiles, use the following:
#raster::shapefile(study.area.Sp, filename = 'study_area.shp')

study.area.polygon <- Polygon(study.area.Sp@data[, c('newX', 'newY')])

# this is for 2011 study area:
study.area.2011 <- read.csv("Data/studyArea_2011.csv", header = F)
colnames(study.area.2011) <- c('lat', 'lon')
study.area.2011$X <- study.area.2011$lon - 360
study.area.2011$Y <- study.area.2011$lat
study.area.2011.Sp <- latlon2sp(study.area.2011, center.UTM)

plot.SCB.area.2011 <- data.frame(X = c(-123, -123, -115, -115, -123),
                                 Y = c(28, 36, 36, 28, 28))
plot.SCB.area.2011.Sp <- latlon2sp(plot.SCB.area.2011, center.UTM)
plot.SCB.area.2011.polygon <- Polygon(plot.SCB.area.2011.Sp@data[, c('newX', 'newY')])

# to create shapefiles, use the following:
#raster::shapefile(study.area.2011.Sp, filename = 'study_area_2011.shp')
study.area.2011.polygon <- Polygon(study.area.2011.Sp@data[, c('newX', 'newY')])

# middle line:
middle.line <- data.frame(X = c(-120.472, -118.496),
                          Y = c(34.450, 31.172))

middle.line.Sp <- latlon2sp(middle.line, center.UTM)
middle.line.newXY <- data.frame(newX = middle.line.Sp$newX,
                                newY = middle.line.Sp$newY)

# Find the intersection between the middle line and LTCA polygon
#Create SpatialPolygon object from search area:
study.area.2011.Sp.poly <- SpatialPolygons(list(Polygons(list(Polygon(study.area.2011.Sp@coords)),
                                                       ID = '1')),
                                         proj4string = CRS("+proj=utm +zone=10 ellps=WGS84"))
lines.list <- list(Lines(list(Line(middle.line.Sp@coords)),
                           ID = paste0('line', k)))
# Create SpatialLines object
line1.Sp <- SpatialLines(lines.list,
                         proj4string = CRS("+proj=utm +zone=10 ellps=WGS84"))

# use gIntersection in rgeos to find intersections between lines and the polygon
# This was a headache... found some code, which is saved in gIntersection_example.R
edges <- coordinates(gIntersection(line1.Sp, study.area.2011.Sp.poly))
edges <- matrix(unlist(edges), nrow = 2, ncol = 2)
edges.Sp <- data.frame(X = edges[, 1], Y = edges[,2])

coordinates(edges.Sp) <- c('X', 'Y')
proj4string(edges.Sp) <- CRS("+proj=utm +zone=10 ellps=WGS84")

edges.latlon <- spTransform(edges.Sp,
                            CRS("+proj=longlat +datum=WGS84"))

middle.line.2011 <- data.frame(X = c(middle.line$X[1], edges.latlon@coords[2,1]),
                               Y = c(middle.line$Y[1], edges.latlon@coords[2,2]))

middle.line.2011.Sp <- latlon2sp(middle.line.2011, center.UTM)
middle.line.2011.newXY <- data.frame(newX = middle.line.2011.Sp$newX,
                                newY = middle.line.2011.Sp$newY)

# get planned lines with the middle points
all.lines <- read.csv('Data/TransectLines.csv',
                      header = TRUE)
all.lines.2 <- all.lines[, c('Line', 'Lat_offshore', 'Lon_offshore',
                             'Lat_inshore', 'Lon_inshore', 'Lat_middle',
                             'Lon_middle')]

# find intersections between lines and the middle line:
# first convert the coordinate system for the beginning and end of
# each line:
all.lines.offshore <- all.lines.2[, c('Line', 'Lat_offshore', 'Lon_offshore')]
colnames(all.lines.offshore) <- c('Line', 'Y', 'X')
all.lines.offshore.Sp <- latlon2sp(all.lines.offshore, center.UTM)

all.lines.inshore <- all.lines.2[, c('Line', 'Lat_inshore', 'Lon_inshore')]
colnames(all.lines.inshore) <- c('Line', 'Y', 'X')
all.lines.inshore.Sp <- latlon2sp(all.lines.inshore, center.UTM)

all.lines.newXY <- data.frame(Line = all.lines.offshore$Line,
                              newX_offshore = all.lines.offshore.Sp@data$newX,
                              newY_offshore = all.lines.offshore.Sp@data$newY,
                              newX_inshore = all.lines.inshore.Sp@data$newX,
                              newY_inshore = all.lines.inshore.Sp@data$newY)

# Also use distGeo function to measure these (not done yet - 8/22/2017)
planned.tracklines.distances <- sqrt((all.lines.newXY[,4]-all.lines.newXY[,2])^2 +
                                       (all.lines.newXY[,5]-all.lines.newXY[,3])^2)

intersections <- foreach(i = 1:nrow(all.lines.newXY), .combine = c) %do%
  # intersection2lines is in TomosFunctions.R.
  intersection2lines(as.matrix(middle.line.newXY),
                     matrix(unlist(all.lines.newXY[i,2:5]),
                            nrow = 2, ncol = 2, byrow = TRUE))

intersections.matrix <- matrix(intersections,
                               nrow = nrow(all.lines.newXY),
                               ncol = 2,
                               byrow = TRUE)

intersections.df <- as.data.frame(intersections.matrix)

colnames(intersections.df)<- c('newX', 'newY')

intersections.latlon.df.Sp <- sp2latlon(intersections.df, center.UTM)
intersections.latlon <- data.frame(cbind(all.lines.newXY$Line,
                                         intersections.latlon.df.Sp@coords))
colnames(intersections.latlon) <- c('Line', 'Longitude', 'Latitude')


# loggerhead conservation area
# two ends of the 120W (MX border and north coastline) were pulled out from
# Google Earth.
waterSeg <- data.frame(X = c(-117.1241,-117.2382,-117.2626,
                             -117.4639,-117.8255,-118.6047,
                             -119.9992,-120.0002),
                       Y = c(32.5343, 32.5274, 32.5534,
                             32.5895, 32.626869, 31.1326,
                             30.8906, 34.4558))

# find the coastal segment from the study area:
landSeg <- study.area[study.area$X >= -120 & study.area$Y>=32.5343,]
map.LTCA <- rbind(waterSeg, landSeg[,c('X', 'Y')])
map.LTCA.Sp <- latlon2sp(map.LTCA, center.UTM)

# Convert segdata into a spatial data frame
# convert the study area into a data frame with 2x2 km^2 cells
# eventually, add covariates - depths, sst, etc. and save in a file
pred.data.Sp <- makegrid(plot.SCB.area.Sp, cellsize = 2000)  # 2000m
# convert UTM back to lat/lon - needed for xtracto
colnames(pred.data.Sp) <- c("X", "Y")
coordinates(pred.data.Sp) <- c("X", "Y")
proj4string(pred.data.Sp) <- CRS("+proj=utm +zone=10 ellps=wGS84")
pred.data <- as.data.frame(spTransform(pred.data.Sp,
                                       CRS("+proj=longlat +datum=WGS84")))

# this is for modeling; all in km. and centered at the center coordinate.
pred.data$newX <- (pred.data.Sp$X - center.UTM$X)/1000   # in Km
pred.data$newY <- (pred.data.Sp$Y - center.UTM$Y)/1000

prediction.data <- data.frame(x = pred.data$newX,
                              y = pred.data$newY,
                              latitude = pred.data$Y,
                              longitude = pred.data$X)

# get the coast line, no including the islands:
# the input file was created using extractCoastLines.R - not in this directory
coast.line <- read.csv("Data/SCB_mainCoast.csv", header = T)
coast.line$X <- coast.line$Longitude
coast.line$Y <- coast.line$Latitude
coast.line.Sp <- latlon2sp(coast.line, center.UTM)
coast.line.xy <- data.frame(newX = coast.line.Sp$newX,
                            newY = coast.line.Sp$newY)

# get all islands loaded:
# get islands:
island.files <- c('Sutil.csv', 'SantaRosa.csv', 'SantaCruz.csv',
                  'SantaCatalina.csv', 'SantaBarbara.csv',
                  'SanNicolas.csv', 'SanClemente.csv',
                  'RockOffSanClemente.csv',
                  'GullIsland.csv', 'SanMiguel.csv','PrinceIsland.csv',
                  'CastleRock.csv')

# get all island info here:
all.islands <- lapply(island.files, FUN = get.island, center.UTM)

# combine them into a data frame
all.islands.df <- do.call(rbind, lapply(all.islands, FUN = function(x) x$df))

# get polygon objects for all islands:
islands.poly <- lapply(lapply(all.islands, FUN = function(x) x$xy),
                       FUN = get.island.polygon)

# remove San Miguel
island.names <- lapply(all.islands, FUN = function(x) x$name)

islands.poly.no.SanMiguel <- islands.poly[unlist(island.names) != "SanMiguel"]

area.islands.no.SanMiguel <- sum(unlist(lapply(islands.poly.no.SanMiguel,
                                               FUN = function(x) x@area)))
area.islands <- sum(unlist(lapply(islands.poly,
                                  FUN = function(x) x@area)))

# split the study area into inshore and offshore areas:
inshore.coast.line <- coast.line[coast.line$Latitude >= 31.891 &
                                   coast.line$Latitude < 34.5 &
                                   coast.line$Longitude >= -120.472,
                                 c('Latitude', 'Longitude', 'X', 'Y')]
inshore.coast.line.Sp <- latlon2sp(rbind(c(34.450, -120.472,
                                           -120.472, 34.450),
                                         inshore.coast.line), center.UTM)

# SW point of the inshore study area
inshore.SW <- middle.line[2,]
#inshore.SW$X <- inshore.SW$Longitude
#inshore.SW$Y <- inshore.SW$Latitude
inshore.SW.Sp <- latlon2sp(inshore.SW, center.UTM)
inshore.bottom <- study.area[study.area$Y < 31.85 &
                               study.area$X >= middle.line[2, 1], ]
inshore.bottom.Sp <- latlon2sp(inshore.bottom, center.UTM)
inshore.study.area <- rbind(inshore.coast.line.Sp@data[, c("newX", "newY")],
                            inshore.bottom.Sp@data[, c("newX", "newY")],
                            middle.line.Sp@data[2, c("newX", "newY")],
                            inshore.coast.line.Sp@data[1, c("newX", "newY")])

inshore.polygon <- Polygon(inshore.study.area[,c('newX', 'newY')])
inshore.polygon.latlon <- sp2latlon(as.data.frame(inshore.polygon@coords), center.UTM)

inshore.area <- inshore.polygon@area - area.islands.no.SanMiguel

offshore.bottom <- study.area[study.area$lat < middle.line[2,2],]
offshore.bottom.Sp <- latlon2sp(offshore.bottom, center.UTM)
lat1 <- intersections.latlon[intersections.latlon$Line == 20, 'Latitude']
lon1 <- intersections.latlon[intersections.latlon$Line == 20, 'Longitude']
offshore.box <- data.frame(Latitude = c(lat1, middle.line[2,2],
                                        offshore.bottom[,"Y"],
                                        33.229, lat1),
                           Longitude = c(lon1, middle.line[2,1],
                                         offshore.bottom[,"X"],
                                         -121.830, lon1))
offshore.box$X <- offshore.box$Longitude
offshore.box$Y <- offshore.box$Latitude
offshore.box.Sp <- latlon2sp(offshore.box, center.UTM)
offshore.polygon <- Polygon(offshore.box.Sp@data[,c('newX', 'newY')])
offshore.polygon.latlon <- sp2latlon(as.data.frame(offshore.polygon@coords), center.UTM)

trunc.study.area <- rbind(inshore.coast.line.Sp@data[, c("newX", "newY")],
                          inshore.bottom.Sp@data[, c("newX", "newY")],
                          offshore.bottom.Sp@data[, c("newX", "newY")],
                          offshore.box.Sp@data[(nrow(offshore.box.Sp@data)-1),
                                               c("newX", "newY")],
                          offshore.box.Sp@data[(nrow(offshore.box.Sp@data)),
                                               c("newX", "newY")],
                          inshore.coast.line.Sp@data[1, c("newX", "newY")])

offshore.area <- offshore.polygon@area

total.area <- offshore.area + inshore.area

save(alt, study.area, plot.SCB.area.Sp, plot.SCB.area.polygon,
     study.area.polygon, study.area.2011, plot.SCB.area.2011.Sp,
     plot.SCB.area.2011.polygon,
     study.area.2011.polygon, middle.line, middle.line.newXY,
     middle.line.2011, middle.line.2011.newXY,
     all.lines.2, all.lines.offshore, all.lines.offshore.Sp,
     all.lines.inshore, all.lines.inshore.Sp, all.lines.newXY,
     planned.tracklines.distances, intersections.df,
     intersections.latlon, intersections.latlon.df.Sp,
     waterSeg, landSeg, map.LTCA, map.LTCA.Sp, pred.data.Sp,
     pred.data, prediction.data, coast.line, coast.line.Sp,
     coast.line.xy, all.islands, all.islands.df, islands.poly,
     island.names, islands.poly.no.SanMiguel, area.islands.no.SanMiguel,
     area.islands, inshore.coast.line, inshore.coast.line.Sp,
     inshore.SW, inshore.SW.Sp, inshore.bottom, inshore.bottom.Sp,
     inshore.study.area, inshore.polygon, inshore.polygon.latlon,
     inshore.area, offshore.bottom, offshore.bottom.Sp,
     offshore.box, offshore.box.Sp, offshore.polygon,
     offshore.polygon.latlon, trunc.study.area, offshore.area,
     total.area, file = 'RData/studyArea.RData')

