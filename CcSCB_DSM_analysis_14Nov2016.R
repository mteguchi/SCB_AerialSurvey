#CcSCB_DSM_analysis
# should fit into the chunk named dsm_analysis in Cc in SCB.Rmd


# Started mid July 2016
# using code in Appendix from
#Miller et al. 2013. Spatial models for distance sampling data: recent developments and future directions. Methods in Ecology and Evolution 4:1001-1010.
rm(list=ls())

source('CcSCB_functions.R')

save.results <- T
ccData$X <- ccData$mlon
ccData$Y <- ccData$mlat
ccData.Sp <- latlon2sp(ccData, center.UTM)

# create data frames for dsm fitting:
# extrat effort for each sighting.
effort <- vector(mode = 'numeric', length = dim(ccData)[1])
for (k in 1:length(effort)){
  effort[k] <- dat2015[dat2015$segnum == ccData$segnum[k],]$dist
}

# distance data are the sightings that used to fit detection function
distance.data <- data.frame(object = seq(from = 1,
                                         to = dim(ccData)[1]),
                            Sample.Label = ccData$segnum,
                            size = 1,
                            distance = ccData$distance,
                            Effort = effort,
                            Beaufort = ccData$Beaufort)

load('RData/segmentDataWithCovariatesDepth_2016-09-02.RData')
segment.data.cov$logChl <- log(segment.data.cov$chl_mean)
segment.data.cov$logChl_08 <- log(segment.data.cov$chl_08_mean)
segment.data.cov$logChl_14 <- log(segment.data.cov$chl_14_mean)
segment.data.cov$logChl_30 <- log(segment.data.cov$chl_30_mean)

segment.data.2015 <- subset(segment.data.cov, year == 2015)
segment.data.2011 <- subset(segment.data.cov, year == 2011)

# observation data are observations aggregated into chopped segments
# this should have the same number of rows as distance.data
observation.data <- data.frame(object = seq(from = 1,
                                            to = dim(ccData)[1]),
                               Sample.Label = ccData$segnum,
                               size = 1,
                               distance = ccData$distance,
                               Effort = effort)

# load the results from the line transect analysis:
load("RData/HR_null_1_Strata_out_2016-12-01.RData")

ds.model <- run.hr.null.1
# simple modeling with x and y.
M_xy <- dsm(N ~ s(x,y),
            ddf.obj = ds.model$ddf,
            segment.data = segment.data.2015,
            observation.data = observation.data)

M_xy_sst <- dsm(N ~ s(x,y) + s(sst_mean),
                ddf.obj = ds.model$ddf,
                segment.data = segment.data.2015,
                observation.data = observation.data)

# convergence problems in this one
M_xy_chl <- dsm(N ~ s(x,y) + s(logChl),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

# convergence problems in this one - fixed with log(chl)
M_xy_sst_chl <- dsm(N ~ s(x,y) + s(sst_mean) + s(logChl),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

M_sst_chl <- dsm(N ~ s(sst_mean) + s(logChl),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

M_sst <- dsm(N ~ s(sst_mean),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

M_sst_sst08_sst14_sst30 <- dsm(N ~ s(sst_mean) + s(sst_08_mean) +
                s(sst_14_mean) + s(sst_30_mean),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

# divergence warnings on this one
M_xy_chl_chl08_chl14_chl30 <- dsm(N ~ s(x,y) + s(logChl) + s(logChl_08) +
                s(logChl_14) + s(logChl_30),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

M_sst_sst08_sst14_sst30_depth <- dsm(N ~ s(sst_mean) + s(sst_08_mean) +
                s(sst_14_mean) + s(sst_30_mean) + s(depth_mean),
              ddf.obj = ds.model$ddf,
              segment.data = segment.data.2015,
              observation.data = observation.data)

M_depth <- dsm(N ~  s(depth_mean),
               ddf.obj = ds.model$ddf,
               segment.data = segment.data.2015,
               observation.data = observation.data)

M_xy_depth <- dsm(N ~  s(depth_mean) + s(x,y),
               ddf.obj = ds.model$ddf,
               segment.data = segment.data.2015,
               observation.data = observation.data)

DSM.M.list <- list("M_xy" = M_xy,
               'M_xy_sst' = M_xy_sst,
               'M_xy_chl' = M_xy_chl,
               'M_xy_sst_chl' = M_xy_sst_chl,
               'M_sst' = M_sst,
               'M_sst_sst08_sst14_sst30' = M_sst_sst08_sst14_sst30,
               'M_xy_chl_chl08_chl14_chl30' = M_xy_chl_chl08_chl14_chl30,
               'M_sst_sst08_sst14_sst30_depth' = M_sst_sst08_sst14_sst30_depth,
               'M_depth' = M_depth,
               'M_xy_depth' = M_xy_depth)

DSM.deviances <- unlist(lapply(DSM.M.list,
                               FUN = dsm.dev.explained))

best.DSM.model.name <- names(DSM.deviances[DSM.deviances == max(DSM.deviances)])
best.DSM.model <- DSM.M.list[names(DSM.M.list) == best.DSM.model.name]

if (save.results == T){
  save(DSM.M.list, best.DSM.model.name, best.DSM.model,
       file = paste0('RData/DSMresults_', Sys.Date(), '.RData'))

}


