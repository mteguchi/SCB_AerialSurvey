#plot_large_SST_Geo
# plots SST and geostrphic current maps

rm(list=ls())

source('CcSCB_functions.R')

wf <- 200

coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                           lon.limits = c(-125, -116),
                           lat.limits = c(22, 40))

coast.line.df <- do.call(rbind, coast.line)
colnames(coast.line.df) <- c('X', 'Y', 'idx')
coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)

# create a new data frame for plotting
study.area.Sp <- latlon2sp(study.area, center.UTM)
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

land.color <- '#333333'
alpha.value <- 0.8

yrs <- 2009:2015 #c(2015, 2016)
mos <- 1:12
das <- c(1, 7, 14, 21, 28)

xlim <- c(205, 243.4)
ylim <- c(22, 40)
#1:length(file.names)
#k<-1
#for (k in 1:length(file.names.t)){
y <- m <- d <- 1
for (y in 1:length(yrs)){
  for (m in 1:length(mos)){
    for (d in 1:length(das)){
      date.str <- as.Date(paste0(yrs[y], '-', mos[m], '-', das[d]))
      print(date.str)
      #var.date.str <- unlist(strsplit(unlist(strsplit(file.names.t[k], '_'))[1],
      #'_bkgd.RData'))
      #date.str <- unlist(strsplit(var.date.str, 'jplMURSST'))[2]

      #print(paste(k, 'in', length(file.names.t), ': ', date.str))
      #load(paste0('RData/large_bkgd/', file.names[k]))
      out.filename <- paste0('Figures/uv_large/uv_14days_sst_large_bkgd_',
                             date.str, '.png')
      if (file.exists(out.filename) == FALSE){

        # Aggregate into the same box size as the others: 2 km x 2 km
        sst <- extract.cov.large.bkgd('jplMURSST',
                                      date.str,
                                      center.UTM)
        #sst.tmp <- sst
        # colnames(sst.tmp) <- c('X.idx', 'Y.idx', 'mean', 'newX', 'newY')
        # sst.latlon <- sp2latlon(sst.tmp, center.UTM)
        # sst.latlon.df <- data.frame(sst.latlon@coords)
        # sst.latlon.df$mean <- sst.tmp$mean
        # # Geo currents are in 5 x 5 km as of 10/19/2016
        # changed to 30 x 30 km 12/9/2016
        tlim <- c(as.Date(date.str), as.Date(date.str) + 13)
        #tmp <- get.geo.dt(xlim, ylim, tlim)
        uv.geo <- extract.geo.ndays.large.bkgd(date.str,
                                               ndays = 14, center.UTM)
        uv.geo <-na.omit(uv.geo)

        p1 <- ggplot() +
          geom_raster(data = sst,
                      aes(x =X, y = Y, fill = mean)) +
          scale_fill_gradient(limits = c(10, 30),
                              low = "blue",
                              high = "red") +
          geom_polygon(fill = land.color,
                       data = all.islands.df,
                       aes(x=newX, y=newY, group = name),
                       inherit.aes = F) +
          stat_contour(data = sst,
                       aes(x=X, y=Y, z=mean),
                       breaks = 20,
                       colour = "gray") +
          stat_contour(data = sst,
                       aes(x=X, y=Y, z=mean),
                       breaks = 24,
                       colour = "gold") +
          geom_segment(data = uv.geo,
                       aes(x = newX, xend = newX + u * wf,
                           y = newY, yend = newY + v * wf),
                       arrow = arrow(length = unit(0.05, 'cm'),
                                     type = 'closed')) +
          geom_polygon(fill = land.color,
                       data = coast.line.Sp@data,
                       aes(x=newX, y=newY, group = idx))  +
          ylab("y") +
          xlab("x") +
          ggtitle(date.str) +
          theme(plot.title = element_text(hjust = 0.5),
                legend.title = element_text(size = 10, hjust = 0.5),
                legend.text = element_text(size = 8, vjust = 0))


        ggsave(plot = p1,
               dpi = 1200,
               width = 9.74,
               height = 6.44,
               file = out.filename)

      }

    }
  }
}


# different color scheme
#low = '#ffc500', high = '#c21500',
#low = '#FF4E50', high = '#F9D423',
# try rainbow(n=20, end = 4/6)


# ,arrow = arrow(length=unit(0.08, "cm"))
