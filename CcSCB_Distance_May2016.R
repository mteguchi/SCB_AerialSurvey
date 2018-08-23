#CcSCB_Distance
#
# Distance analysis of loggerhead turtles
# in the SCB from aerial survey data.


# Tomo Eguchi
# 13 May 2016

rm(list=ls())
library(Distance)
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
effortData <- read.table('Data/tmpTracks.txt',
                         header = TRUE, sep = ",")
lineIDs <- sort(unique(effortData$Line))

effortByLine <- vector(mode = "numeric",
                       length = length(lineIDs))

for (k in 1:length(lineIDs)){
  effortByLine[k] <- nm2km(sum(effortData$Distance[effortData$Line == lineIDs[k]]))
}

effort.df <- data.frame(ID = lineIDs,
                        effort = effortByLine)

region <- data.frame(Region.Label = "SCB",
                     Area = 113780)

obs <- data.frame(Region.Label = "SCB",
                  object = seq(1, dim(ccData)[1]),
                  Sample.Label = ccData$transectNum)

sample <- data.frame(Sample.Label = lineIDs,
                     Region.Label = "SCB",
                     Effort = effortByLine)

# may not need to truncate because observers were instructed
# not to look too far away; changed from 15% to 1%
run.hr.cos.bft <- ds(data = ccData,
                 truncation="1%",
                 key = "hr",
                 formula = ~ Beaufort,
                 region.table = region,
                 sample.table = sample,
                 obs.table = obs)

run.hn.cos.bft <- ds(data = ccData,
                     truncation="1%",
                     key = "hn",
                     formula = ~ Beaufort,
                     region.table = region,
                     sample.table = sample,
                     obs.table = obs)

run.hr.cos.1 <- ds(data = ccData,
                   truncation="1%",
                   key = "hr",
                   adjustment = "cos",
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

run.hr.null.1 <- ds(data = ccData,
                   truncation="1%",
                   key = "hr",
                   adjustment = NULL,
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

run.hn.cos.1 <- ds(data = ccData,
                   truncation="1%",
                   key = "hn",
                   adjustment = "cos",
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

run.hn.null.1 <- ds(data = ccData,
                   truncation="1%",
                   key = "hn",
                   adjustment = NULL,
                   formula = ~ 1,
                   region.table = region,
                   sample.table = sample,
                   obs.table = obs)

gof.hn.null.1 <- ds.gof(run.hn.null.1)
gof.hr.null.1 <- ds.gof(run.hr.null.1)

gof.hr.cos.1 <- ds.gof(run.hr.cos.1)
gof.hn.cos.1 <- ds.gof(run.hn.cos.1)

gof.hr.cos.bft <- ds.gof(run.hr.cos.bft)
gof.hn.cos.bft <- ds.gof(run.hn.cos.bft)  # seems best from AIC

#AIC(run.hn.cos.bft)
#AIC(run.hr.cos.bft)
#AIC(run.hn.cos.1)
#AIC(run.hr.cos.1)
#AIC(run.hn.null.1)
#AIC(run.hr.null.1)

check.mono(run.hn.cos.bft$ddf, plot=T, n.pts=100)

# save the results - just the best one:
#save(run.hn.cos.bft, file = "RData/HN_Cos_Bft_out.RData")





