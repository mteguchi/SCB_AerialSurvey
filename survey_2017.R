#survey_2017

rm(list=ls())
source('CcSCB_functions.R')
#Uncomment some lines below to prep for the input files:
######################################################################
#get the data in:

# first merge the two DAS files:
#source('fcn.AirSegChopCc.R')
# fnames2017 <- list.files('Data/', pattern = 'SCB2017')
# outfile.name <- 'Data/SCB2017_DAS.das'
# outfile <- file(outfile.name, 'w')
# k<-1
# for ( k in 1:length(fnames2017)){
#   x <- readLines(paste0('Data/', fnames2017[k]))
#   writeLines(x, outfile)
#
# }
# close(outfile)

#fcn.AirSegChopCc(outfile.name)
#####################################################################

# created by Elizabeth Becker and Karin Forney and was modified
# for turtle sightings by me.
dat2017 <- read.csv('Data/processed/SEGDATA_SCB2017_DAS_2km_2017-08-30.csv')

# sightings data are in "SITEINFO_SCB" files
Sdata.2017 <- read.csv('Data/processed/SITEINFO_SCB2017_DAS_2km_2017-08-30.csv')


# dist variable in the above files is the length of each segment and NOT
# distances of the objects from the track line.

# Add date info merged between Sdata and dat
date.2017.df <- dat2017[dat2017$segnum %in% Sdata.2017$segnum,
                        c('segnum', 'year', 'month', 'day')]

# then assign the averages to each sighting - there probably is a more elegant
# way of doing this but loops do the job...
Sdata.2017$year <- Sdata.2017$month <- Sdata.2017$day <- NA
for (k in 1:dim(Sdata.2017)[1]){
  Sdata.2017[k, c('year',
                  'month',
                  'day')] <- date.2017.df[Sdata.2017$segnum[k] == date.2017.df$segnum,
                                          c('year', 'month', 'day')]
}


# create Sdata that includes both 2011 and 2015
Sdata.2017$transectID <- paste(Sdata.2017$year,
                               Sdata.2017$transectNum,
                               sep = '_')

bft.2017.df <- dat2011[dat2017$segnum %in% Sdata.2017$segnum,
                       c('segnum', 'aveBF')]

# then assign the averages to each sighting - there probably is a more elegant
# way of doing this but loops do the job...
Sdata.2017$Beaufort <- NA
for (k in 1:dim(Sdata.2017)[1]){
  Sdata.2017$Beaufort[k] <- bft.2017.df$aveBF[Sdata.2017$segnum[k] == bft.2017.df$segnum]
}

# compute the perpendicular distances in meters
Sdata.2017$PerpDist <- ft2m(alt * tan(deg2rad(90 - abs(Sdata.2017$DecAngle))))

# fix slon to all negative values:
Sdata.2017$slon[Sdata.2017$slon > 0] <- Sdata.2017$slon[Sdata.2017$slon > 0] - 360

#ccData <- subset(Sdata.2017, species == 'cc')
#ccDataOn <- subset(ccData, Effort == 1)

# perp distance in km
#ccData$distance <- ccData$PerpDist/1000

# convert lat/lon to x/y
# ccData$X <- ccData$mlon
# ccData$Y <- ccData$mlat
# ccData.Sp <- latlon2sp(ccData, center.UTM)

# get lines --- Start here!!! 8/30/2017
# Distance is in nm
lines.2017 <- get.track.lines('Data/tmpTracks_2017.txt')

lines.2017$lines$Year <- 2017
lines.df <- lines.2017$lines
# lines.df$distance <- sqrt((lines.df$endX - lines.df$beginX)^2 +
#                             (lines.df$endY - lines.df$beginY)^2)
#
# dplyr::select(lines.df, beginX, beginY) %>%  # get begin and end points
#   dplyr::rename(., newX = beginX, newY = beginY) %>%  # rename them so sp2latlon can work
#   sp2latlon(., center.UTM) %>%  # convert spatial points to lat/lon
#   as.data.frame() %>%           # change it to a regular dataframe
#   dplyr::rename(., beginX = X, beginY = Y) -> lines.df.begin.latlon  # rename them to beginX.beginY
#
# dplyr::select(lines.df, endX, endY) %>%
#   dplyr::rename(., newX = endX, newY = endY) %>%
#   sp2latlon(., center.UTM)  %>%
#   as.data.frame() %>%
#   dplyr::rename(., endX = X, endY = Y) -> lines.df.end.latlon
#
#
# lines.df.latlon <- data.frame(date = as.Date(lines.df$date),
#                               Year = lines.df$Year,
#                               beginX = lines.df.begin.latlon$beginX,
#                               beginY = lines.df.begin.latlon$beginY,
#                               endX = lines.df.end.latlon$endX,
#                               endY = lines.df.end.latlon$endY,
#                               distance = lines.df$distance)

#lines.2017$data

