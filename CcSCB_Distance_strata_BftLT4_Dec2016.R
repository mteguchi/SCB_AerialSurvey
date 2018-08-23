#CcSCB_Distance
#
# Distance analysis of loggerhead turtles
# in the SCB from aerial survey data.


# Tomo Eguchi
# 13 May 2016

rm(list=ls())

#library(unmarked)  # --- seems to use discrete distance data only
#library(DSpat)  # Models by Devin Johnson et al.
library(mrds)   # Package by Jeff Laake et al.

source('CcSCB_functions.R')
# 2015 data that are split in 2 km chunks are here
# these were created using AirSegChopCc_2016_04_11.R on merged data files
# which were created using combineFiles.m in Matlab. AirSegChop script
# was first created by Elizabeth Becker and Karin Forney and was modified
# for turtle sightings by me.

# data are stored in CcSCB_functions.R
Sdata.2011$transectID <- paste(Sdata.2011$year,
                               Sdata.2011$transectNum,
                               sep = '_')

Sdata.2015$transectID <- paste(Sdata.2015$year,
                               Sdata.2015$transectNum,
                               sep = '_')

# compute the perpendicular distances in meters.
Sdata$PerpDist <- ft2m(alt * tan(deg2rad(90 - abs(Sdata$DecAngle))))

# fix slon to all negative values:
Sdata$slon[Sdata$slon > 0] <- Sdata$slon[Sdata$slon > 0] - 360

#ccData <- subset(Sdata, species == 'cc')
#ccDataOn <- subset(ccData, Effort == 1)

# perp distance in km
ccData$distance <- ccData$PerpDist/1000

# transect effort data:
# this is for 2015 only. - need 2011? - probably not because
# effort is only needed for estimating density/abundance.
# no sightings from 2011
effortData <- read.table('Data/tmpTracks_Nov2016.txt',
                         header = TRUE, sep = ",")

# first order lat/lon pairs so that offshore point is obvious
# for each track segment:
new.effort.data <- matrix(data = NA,
                          nrow = dim(effortData)[1],
                          ncol = 5)

for (k in 1:dim(effortData)[1]){
  if (effortData[k, 'Lon1'] < effortData[k, 'Lon2']){
    new.effort.data[k, ] <- unlist(as.vector(c(effortData[k, c('Lat1', 'Lon1',
                                                         'Lat2', 'Lon2',
                                                         'Line')])))
  } else {
    new.effort.data[k, ] <- unlist(as.vector(c(effortData[k, c('Lat2', 'Lon2',
                                                         'Lat1', 'Lon1',
                                                         'Line')])))
  }
}

# Nov 2016 - need to split each line into inshore and offshore for
# post-stratification. Need to find the splitting lat/lon and new
# line IDs.
effort.data.2 <- matrix(data = NA,
                        nrow = (dim(new.effort.data)[1] * 2),
                        ncol = 7)

transect.data <- read.table('Data/transectLines_strata.csv',
                            header = TRUE, sep = ",")

#new.lines <- vector(mode = 'numeric', length = dim(new.effort.data)[1])
c <- 1
for (k in 1:dim(new.effort.data)[1]){
  line.ID <- new.effort.data[k, 5]
  strata.lines <- transect.data[transect.data$ID == line.ID, ]
  if (dim(strata.lines)[1] > 1){
    lon.middle <- strata.lines[1, 'lon_inshore']
    # the segment goes over the middle point
    if (new.effort.data[k, 2] < lon.middle & new.effort.data[k, 4] > lon.middle){
      # W of the middle point
      effort.data.2[c, ] <- unlist(c(new.effort.data[k, c(1,2)],
                                     strata.lines[1, c('lat_inshore',
                                                       'lon_inshore')],
                                     new.effort.data[k, 5],
                                     strata.lines[1, 'line'],
                                     strata.lines[1, 'offshore']))
      c <- c + 1
      # E of the middle point
      effort.data.2[c, ] <- unlist(c(strata.lines[1, c('lat_inshore',
                                                       'lon_inshore')],
                                     new.effort.data[k, c(3,4)],
                                     new.effort.data[k, 5],
                                     strata.lines[2, 'line'],
                                     strata.lines[2, 'offshore']))
      c <- c + 1

      # both points are west of the middle point
    } else if (new.effort.data[k, 2] < lon.middle &
               new.effort.data[k, 4] < lon.middle){
      effort.data.2[c, ] <- unlist(c(new.effort.data[k, c(1:5)],
                              strata.lines[1, 'line'],
                              strata.lines[1, 'offshore']))
      c <- c + 1
      # both points are E of the middle point
    } else if (new.effort.data[k, 2] > lon.middle){
      effort.data.2[c, ] <- unlist(c(new.effort.data[k, c(1:5)],
                              strata.lines[2, 'line'],
                              strata.lines[2, 'offshore']))
      c <-c + 1
    }
  } else {
    # only one strata.lines - take all at once
    effort.data.2[c, ] <- unlist(c(new.effort.data[k, c(1:5)],
                                   strata.lines[1, c('line', 'offshore')]))
    c <- c + 1
  }
}

effort.data.2 <- as.data.frame(na.omit(effort.data.2))
colnames(effort.data.2) <- c('Lat1', 'Lon1',
                             'Lat2', 'Lon2',
                             'oldID', 'Line', 'offshore')

# find distances between Lat1/Lon1 and Lat2/Lon2:
# create the new UTM based coordinates for Lat1/Lon1
effort.data.2.1 <- effort.data.2
colnames(effort.data.2.1) <- c('Y', 'X', 'Lat2', 'Lon2',
                               'oldID', 'Line', 'offshore')
effort.data.2.Sp <- latlon2sp(effort.data.2.1, center.UTM)
effort.data.2.2 <- effort.data.2.Sp@data
# rename again for Lat2/Lon2
colnames(effort.data.2.2) <- c('Y', 'X',
                               'oldID', 'Line', 'offshore',
                               'newX1', 'newY1')
effort.data.2.Sp <- latlon2sp(effort.data.2.2, center.UTM)
effort.data.2.3 <- effort.data.2.Sp@data
# rename again for Lat2/Lon2
colnames(effort.data.2.3) <- c('oldID', 'Line', 'offshore',
                               'newX1', 'newY1',
                               'newX2', 'newY2')

effort.data.3 <- cbind(effort.data.2.3, effort.data.2[, c('Lat1', 'Lon1',
                                                          'Lat2', 'Lon2')])

euclidDistance <- function(x1, x2) dist <- sqrt(sum((x1 - x2)^2))
library(foreach)
distances <- foreach(i = 1:nrow(effort.data.3), .combine = c) %do%
  euclidDistance(effort.data.3[i, c('newX1', 'newY1')],
                 effort.data.3[i, c('newX2', 'newY2')])

effort.data.3$Distance <- distances

lineIDs <- sort(unique(effort.data.2$Line))

effortByLine <- vector(mode = "numeric",
                       length = length(lineIDs))

for (k in 1:length(lineIDs)){
  effortByLine[k] <- sum(effort.data.3$Distance[effort.data.3$Line == lineIDs[k]])
}

effort.data.3$Region <- 'inshore'
effort.data.3$Region[effort.data.3$offshore == 1] <- 'offshore'

effort.df <- data.frame(ID = lineIDs,
                        effort = effortByLine)

region <- data.frame(Region.Label = c('inshore', 'offshore'), #"SCB",
                     Area = c(inshore.area, offshore.area))  #total.area)

# transect numbers need to be updated with new IDs
ccData$Region <- NA
line.vec <- vector(mode = 'numeric', length = nrow(ccData))
for (k in 1:nrow(ccData)){
  sighting.mlon <- ccData[k, 'mlon']
  sighting.line <- ccData[k, 'transectNum']
  strata.lines <- transect.data[transect.data$ID == sighting.line, ]
  if (nrow(strata.lines) > 1){
    if (sighting.mlon < strata.lines[1, 'lon_inshore']){
      line.vec[k] <- strata.lines[1, 'line']
    } else {
      line.vec[k] <- strata.lines[2, 'line']
    }
  } else {
    line.vec[k] <- strata.lines[1, 'line']
  }

  line <- transect.data[transect.data$line == line.vec[k],]
  ifelse(line$offshore == 0,
         ccData$Region[k] <- 'inshore',
         ccData$Region[k] <- 'offshore')
}

ccData$line.strata <- line.vec

obs <- data.frame(Region.Label = ccData$Region,
                  object = seq(1, dim(ccData)[1]),
                  Sample.Label = ccData$line.strata)

# sample.table needs region, line IDs, and effort:
sample <- data.frame(Sample.Label = lineIDs,
                     Effort = effortByLine)
sample$Region.Label <- NA
for (k in 1:nrow(sample)){
  line <- transect.data[transect.data$line == sample$Sample.Label[k],]
  ifelse(line$offshore == 0,
         sample$Region.Label[k] <- 'inshore',
         sample$Region.Label[k] <- 'offshore')
}

ccDataBftLT4 <- ccData[ccData$Beaufort < 4, ]

# may not need to truncate because observers were instructed
# not to look too far away; changed from 15% to 1%
run.hr.cos.bft <- ds(data = ccDataBftLT4,
                     truncation="1%",
                     key = "hr",
                     adjustment = "cos",
                     formula = ~ Beaufort,
                     region.table = region,
                     sample.table = sample,
                     obs.table = obs)

run.hn.cos.bft <- ds(data = ccDataBftLT4,
                     truncation="1%",
                     key = "hn",
                     adjustment = "cos",
                     formula = ~ Beaufort,
                     region.table = region,
                     sample.table = sample,
                     obs.table = obs)

run.hr.cos.1 <- ds(data = ccDataBftLT4,
                   truncation="1%",
                   key = "hr",
                   adjustment = "cos",
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

run.hr.null.1 <- ds(data = ccDataBftLT4,
                   truncation="1%",
                   key = "hr",
                   adjustment = NULL,
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

run.hn.cos.1 <- ds(data = ccDataBftLT4,
                   truncation="1%",
                   key = "hn",
                   adjustment = "cos",
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

run.hn.null.1 <- ds(data = ccDataBftLT4,
                    truncation="1%",
                    key = "hn",
                    adjustment = NULL,
                    formula = ~ 1,
                    region.table = region,
                    sample.table = sample,
                    obs.table = obs)

run.hr.bft <- ds(data = ccDataBftLT4,
                 truncation="1%",
                 key = "hr",
                 adjustment = NULL,
                 formula = ~ Beaufort,
                 region.table = region,
                 sample.table = sample,
                 obs.table = obs)

run.hn.bft <- ds(data = ccDataBftLT4,
                 truncation="1%",
                 key = "hn",
                 adjustment = NULL,
                 formula = ~ Beaufort,
                 region.table = region,
                 sample.table = sample,
                 obs.table = obs)

gof.hn.null.1 <- ds.gof(run.hn.null.1)
gof.hr.null.1 <- ds.gof(run.hr.null.1)

gof.hr.cos.1 <- ds.gof(run.hr.cos.1)
gof.hn.cos.1 <- ds.gof(run.hn.cos.1)

gof.hr.cos.bft <- ds.gof(run.hr.cos.bft)
gof.hn.cos.bft <- ds.gof(run.hn.cos.bft)

AIC_all <- c('HN.Cos.Bft' = AIC(run.hn.cos.bft),
             'HR.Cos.Bft' = AIC(run.hr.cos.bft),
             'HN.Cos' = AIC(run.hn.cos.1),
             'HR.Cos' = AIC(run.hr.cos.1),
             'HR.Bft' = AIC(run.hr.bft),
             'HN.Bft' = AIC(run.hn.bft),
             'HN' = AIC(run.hn.null.1),
             'HR' = AIC(run.hr.null.1))

minAIC <- min(AIC_all)
deltaAIC <- AIC_all - min(AIC_all)


# AIC_hn.cos.bft <- AIC(run.hn.cos.bft)
# AIC_hr.cos.bft <- AIC(run.hr.cos.bft)
# AIC_hn.cos <- AIC(run.hn.cos.1)
# AIC_hr.cos <- AIC(run.hr.cos.1)
# AIC_hn <- AIC(run.hn.null.1)
# AIC_hr <- AIC(run.hr.null.1)


check.mono(run.hr.null.1$ddf, plot=T, n.pts=100)

# save the results
save(list = ls(),
     file = paste0("RData/HR_null_1_Strata_BftLT4_out_", Sys.Date(), ".RData"))





