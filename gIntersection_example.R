
rm(list=ls())

#Code from https://gis.stackexchange.com/questions/154689/how-to-find-the-coordinates-of-the-intersection-points-between-two-spatiallines

# Required packages
library(rgeos)
library(sp)
library(maptools)
library(raster)
library(grDevices)

#intersectedline=gIntersection(SpatialLine, SpatialPoly, byid=T)

# intersectpts=function(intersectedline, SpatialPoly){
#   x=2*rep(NA, length(intersectedline))
#   y=2*rep(NA, length(intersectedline))
#   xylast=cbind(x,y)
#   xy1=cbind(x,y)
#
#   for(i in 1:length(intersectedline)){
#     xy1[i,] <- coordinates(intersectedline)[[i]][[1]][1,]
#     lastpoint=length(coordinates(intersectedline)[[i]][[1]][,1])
#     xylast[i,] <- coordinates(intersectedline)[[i]][[1]][lastpoint,]
#     i=i+1
#   }
#
#   xy=rbind(xy1,xylast)
#
#   plot(SpatialPoly)
#   lines(intersectedline, col="red")
#   points(xy, col="blue")
#
#   return(xy)
# }

# Create convex hull
# Points to create hull
Hull_pts <- structure(list(x = c(38.87584, 38.89215, 38.87062,
                                 38.86157, 38.72808, 38.22315,
                                 38.12702, 38.05936, 37.95169,
                                 37.96915, 38.14758, 38.22325,
                                 38.34001, 38.6394, 38.6447),
                           y = c(4.17092, 4.05521, 3.93639, 3.9168,
                                 3.85601, 3.89487, 4.17066, 4.38951,
                                 4.98351, 4.99706, 5.12187, 5.17153,
                                 5.16003, 4.77422, 4.76607)),
                      .Names = c("x", "y"),
                      row.names = c(1574L, 1540L, 1490L, 1482L, 1457L,
                                    1473L, 294L, 1718L, 1131L, 974L,
                                    2838L, 2101L, 111L, 1914L, 1909L),
                      class = "data.frame")

# Create SpatialLines of Hull
Hull_lines <- list()
for (i in 1:length(Hull_pts$x)-1) {
  Hull_lines[i] <- Lines(list(Line(rbind(Hull_pts[i,1:2], Hull_pts[i+1,1:2]))), ID=i)

}
Hull_lines[15] <- Lines(list(Line(rbind(Hull_pts[15,1:2], Hull_pts[1,1:2]))), ID="15")

# Make it a SpatialLines object
Hull_spLines <- SpatialLines(Hull_lines,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Create SpatialPolygon of Hull (as alternative)
Hull_poly <- SpatialPolygons(list(Polygons(list(Polygon(Hull_pts)), "ID")),
                             proj4string=CRS("+proj=longlat +datum=WGS84"))

# Outer points
Site <- c(seq(1,10,1))
x <- c(38.21467,37.22799, 38.22852, 39.42621,
       37.85457, 38.10600, 37.84077, 38.02475, 38.07228, 37.40716)
y <- c(6.327209, 5.691638, 6.237393, 5.382760,
       6.042394, 6.078254, 6.007846, 5.973651, 6.044092, 5.746368)
Sites <- data.frame(Site, x, y)
xy <- Sites[2:3]
Sites2 <- SpatialPointsDataFrame(coords=xy, data=Sites)
projection(Sites2) <- CRS('+proj=longlat +datum=WGS84')
crs(Sites2) <- "+proj=longlat +datum=WGS84"

# Create lines
Radial_lines <- list()
for (i in 1:length(Sites$x)) {
  Radial_lines[[i]] <- Lines(list(Line(rbind(c(38.35419, 4.483533), Sites[i,2:3]))),
                           ID=Sites[i,1])
  # Make SpatialLines object
  Radial_spLines <- SpatialLines(Radial_lines,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # Plot
  plot(Hull_spLines, xlim=c(37,40.5), ylim=c(2.5,6.6), axes=T)
  points(Sites2$x, Sites2$y, pch=20, col="blue")
  #plot(Centre, add=T, col="red", pch="+", cex=1.5)
  plot(Radial_spLines, add=T)
}


# Cut radial lines at polygon edge
Centre_edge_lines <- gIntersection(Radial_spLines, Hull_poly, byid=T)

# Extract edge points from lines
for (i in 1:length(Sites$x)){     # i-th list element, 2 and 4 are edge co-ords (1 and 3 are centre point)
  Sites$Edge_x[i] <- coordinates(Centre_edge_lines)[[i]][[1]][2]
  Sites$Edge_y[i] <- coordinates(Centre_edge_lines)[[i]][[1]][4]
}

