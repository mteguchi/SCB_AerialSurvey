#CcSCB_DSM_analysis
# should fit into the chunk named dsm_analysis in Cc in SCB.Rmd


# Started mid July 2016
# using code in Appendix from
#Miller et al. 2013. Spatial models for distance sampling data: recent developments and future directions. Methods in Ecology and Evolution 4:1001-1010.
rm(list=ls())
sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))
library(dsm)
library(rgdal)
library(ggplot2)
library(Distance)
library(sp)

# function to convert lat/lon data frame into spatial data frame
latlon2sp <- function(in.df, center.UTM){
  coordinates(in.df) <- c("X", "Y")
  proj4string(in.df) <- CRS("+proj=longlat +datum=WGS84")
  out.df <- spTransform(in.df, CRS("+proj=utm +zone=10 ellps=WGS84"))
  out.df$newX <- (out.df$X - center.UTM$X)/1000
  out.df$newY <- (out.df$Y - center.UTM$Y)/1000
  return(out.df)
}


# get the data in:
dat2011 <- read.csv('Data/processed/SEGDATA_SCB2011_DAS_2km_2016-06-06.csv')
dat2015 <- read.csv('Data/processed/SEGDATA_SCB2015_DAS_2km_2016-06-06.csv')
# dist variable in the above files is the length of each segment and NOT
# distances of the objects from the track line.

alt <- 500

# sightings data are in "SITEINFO_SCB" files
Sdata.2011 <- read.csv('Data/processed/SITEINFO_SCB2011_DAS_2km_2016-06-06.csv')
Sdata.2015 <- read.csv('Data/processed/SITEINFO_SCB2015_DAS_2km_2016-06-06.csv')

Sdata.2011$transectID <- paste(Sdata.2011$year,
                               Sdata.2011$transectNum,
                               sep = '_')

Sdata.2015$transectID <- paste(Sdata.2015$year,
                               Sdata.2015$transectNum,
                               sep = '_')

Sdata <- rbind(Sdata.2011, Sdata.2015)

# compute the perpendicular distances in meters. ft2m is in
# tomosfunctions.R
Sdata$PerpDist <- ft2m(alt * tan(deg2rad(90 - abs(Sdata$DecAngle))))

# fix slon to all negative values:
Sdata$slon[Sdata$slon > 0] <- Sdata$slon[Sdata$slon > 0] - 360

# pull out just loggerheads
ccData <- subset(Sdata, species == 'cc')
#ccDataOn <- subset(ccData, Effort == 1)

# perp distance in m converted into km
ccData$distance <- ccData$PerpDist/1000

# convert the lat/lon into northing/easting
# the study area covers zones 10 and 11. An arbitrary center point
# was created here.
approx.center <- data.frame(X=-119.967, Y=33.092)
coordinates(approx.center) <- c("X", "Y")
proj4string(approx.center) <- CRS("+proj=longlat +datum=WGS84")
center.UTM <- spTransform(approx.center, CRS("+proj=utm +zone=10 ellps=WGS84"))

ccData$X <- ccData$mlon
ccData$Y <- ccData$mlat
ccData.Sp <- latlon2sp(ccData, center.UTM)

# effort data are here for 2015
# Convert segdata into a spatial data frame
dat2015$X <- dat2015$mlon
dat2015$Y <- dat2015$mlat
dat2015.Sp <- latlon2sp(dat2015, center.UTM)

# study area
study.area <- read.csv("Data/StudyArea.txt", header = T)
study.area$X <- study.area$lon
study.area$Y <- study.area$lat
study.area.Sp <- latlon2sp(study.area, center.UTM)

# convert the study area into a data frame with 2x2 km^2 cells
# eventually, add covariates - depths, sst, etc. and save in a file
pred.data.Sp <- makegrid(study.area.Sp, cellsize = 2000)  # 2000m

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
                              lontigude = pred.data$X)

# create data frames for dsm fitting:
# extrat effort for each sighting.
effort <- vector(mode = 'numeric', length = dim(ccData)[1])
for (k in 1:length(effort)){
  effort[k] <- dat2015[dat2015$segnum == ccData$segnum[k],]$dist
}

# distance data are the sightings that used to fit detection function
distance.data <- data.frame(object = seq(from = 1, to = dim(ccData)[1]),
                            Sample.Label = ccData$segnum,
                            size = 1,
                            distance = ccData$distance,
                            Effort = effort)

# chopped up segments of transect lines
segment.data <- data.frame(Effort = dat2015$dist,
                           Sample.Label = dat2015$segnum,
                           BF = dat2015$aveBF,
                           x = dat2015.Sp$newX,
                           y = dat2015.Sp$newY,
                           Transect.Label = dat2015$transectNum)

# observation data are observations aggregated into chopped segments
# this should have the same number of rows as distance.data
observation.data <- data.frame(object = seq(from = 1, to = dim(ccData)[1]),
                               Sample.Label = ccData$segnum,
                               size = 1,
                               distance = ccData$distance,
                               Effort = effort)

# make a study area figure with transect lines overlaid...
# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# get islands:
island.files <- c('Sutil.csv', 'SantaRosa.csv', 'SantaCruz.csv',
                  'SantaCatalina.csv', 'SantaBarbara.csv',
                  'SanNicolas.csv', 'SanClemente.csv',
                  'RockOffSanClemente.csv',
                  'GullIsland.csv', 'Anacapa.csv','PrinceIsland.csv',
                  'CastleRock.csv')

# function to get islands and convert lat/lon into
# the common scale.
get.island <- function(filename, center.UTM){
  dat <- read.csv(paste0('Data/islands/', filename), header = F)
  colnames(dat) <- c('X', 'Y')
  dat$X <- dat$X - 360
  coordinates(dat) <- c("X", "Y")
  proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")
  dat.Sp <- spTransform(dat, CRS("+proj=utm +zone=10 ellps=WGS84"))
  dat.Sp$newX <- (dat.Sp$X - center.UTM$X)/1000
  dat.Sp$newY <- (dat.Sp$Y - center.UTM$Y)/1000

  out.df <- data.frame(lat = dat$Y,
                       lon = dat$X,
                       newX = dat.Sp$newX,
                       newY = dat.Sp$newY)

  return(out.df)
}

all.islands <- lapply(island.files, FUN = get.island, center.UTM)

# get the depth file
depth.data <- read.csv('Data/ocean/SCB_depths.csv', header = F)
names(depth.data.df) <- c('Y', 'X', 'mean', 'SD',
                          'min', 'max', 'median', 'n')
# convert the lat/lon
depth.Sp <- latlon2sp(depth.data.df, center.UTM)

depth.df <- data.frame(x = depth.Sp$newX,
                       y = depth.Sp$newY,
                       z = depth.Sp$mean)

# make a plot
land.color <- 'gray'
alpha.value <- 0.8
plot.study.area <- ggplot(data = study.area.df,
                          aes(x = x, y = y)) +
  geom_polygon(alpha = 0.7) +
  geom_path(color=land.color) +
  geom_tile(data = depth.df,
            aes(x = x, y = y, fill = mean)) +
  geom_polygon(fill = land.color, data = all.islands[[1]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[2]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[3]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[4]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[5]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[6]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[7]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[8]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[9]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[10]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[11]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  geom_polygon(fill = land.color, data = all.islands[[12]],
               aes(x=newX, y=newY), inherit.aes = F,
               alpha = alpha.value) +
  ylab("y") +
  xlab("x") +
  geom_line(data = segment.data,
            aes(x=x, y=y, group = Transect.Label),
            size = 1.2) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())

plot.study.area

# look at distance data:
# I don't use X% truncation because observers were instructed
# to not look too far away in the field.
hr.model <- ds(data = distance.data,
               key = "hr",
               truncation = max(distance.data$distance),
               adjustment = NULL)

# simple modeling with x and y.
model1 <- dsm(N ~ s(x,y),
              ddf.obj = hr.model$ddf,
              segment.data = segment.data,
              observation.data = observation.data)

off.set <- 4  # area of a cell in square km
model1.pred <- predict(model1, prediction.data, off.set)

# it doesn't look so good...

pp <- cbind(prediction.data, model1.pred)
p <- ggplot(pp) + geom_tile(aes(x = x, y = y, fill = model1.pred))
p