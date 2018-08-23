# get_uv_twoweeks

rm(list=ls())
source('CcSCB_functions.R')

yrs <- 2009:2015
mos <- 1:12
das <- c(1, 15)
xlim <- c(205, 244)
ylim <- c(22, 40)

y <- m <- d <- 1
for (y in 1:length(yrs)){
  for (m in 1:length(mos)) {
    for (d in 1:length(das)){
      t1 <- c(as.Date(paste0(yrs[y], '-', mos[m], '-', das[d])))
      t2 <- t1 + 14
      tlim <- c(t1, t2)
      tmp <- get.geo.dt(xlim, ylim, tlim)
      # save(tmp,
      #      file = paste0('Data/uv_data/uvgeo_',
      #                    tlim[1], '_', tlim[2] , '.nc'))

    }
  }
}