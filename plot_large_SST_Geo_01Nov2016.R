#plot_large_SST_Geo
# plots SST and geostrphic current maps

rm(list=ls())

source('CcSCB_functions.R')

wf <- 500

# find all SST files

file.names.t <- list.files(path = "Rdata/large_bkgd",
                           pattern = "jplMURSST")

# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# get islands:
island.files <- c('Sutil.csv', 'SantaRosa.csv',
                  'SantaCruz.csv',
                  'SantaCatalina.csv', 'SantaBarbara.csv',
                  'SanNicolas.csv', 'SanClemente.csv',
                  'RockOffSanClemente.csv',
                  'GullIsland.csv', 'Anacapa.csv',
                  'PrinceIsland.csv',
                  'CastleRock.csv')

# get all island info here:
all.islands <- lapply(island.files, FUN = get.island, center.UTM)
# combine them into a data frame
all.islands.df <- do.call(rbind, all.islands)
land.color <- '#333333'
alpha.value <- 0.8

# different from 19Oct2016 version for specifying years, months, days
years <- c(2013, 2014, 2015)
months <- 1:12
days <- c(1, 7, 14, 21, 28)
for (y in 1:length(years)){
  for (m in 1:length(months)){
    for (d in 1:length(days)){
      #print(paste(years[y], '-', months[m], d))
      # create data frame
      date.str <- as.Date(format(paste0(years[y], '-',
                                        months[m], '-',
                                        days[d]), tz = "Europe/London"))

      print(date.str)
      #load(paste0('RData/large_bkgd/', file.names[k]))
      out.filename <- paste0('Figures/uv_large/uv_sst_large_bkgd_',
                             date.str, '.png')
      if (file.exists(out.filename) == FALSE){

        # Aggregate into the same box size as the others: 5 km x 5 km
        sst <- extract.cov.large.bkgd('jplMURSST', date.str,
                                      center.UTM)

        # Geo currents are in 5 x 5 km as of 10/19/2016
        uv.geo <- extract.geo.large.bkgd(date.str, center.UTM)

        p1 <- ggplot() +
          geom_raster(data = sst,
                      aes(x =X, y = Y, fill = mean)) +
          scale_fill_gradient(limits = c(10, 30),
                              low = "blue",
                              high = "red") +
          geom_polygon(fill = land.color,
                       data = all.islands.df,
                       aes(x=newX, y=newY, group = name),
                       inherit.aes = F)  +
          stat_contour(data = sst,
                       aes(x=X, y=Y, z=mean),
                       breaks = 20,
                       colour = "gray") +
          stat_contour(data = sst,
                       aes(x=X, y=Y, z=mean),
                       breaks = 24,
                       colour = "gold") +
          geom_segment(data = uv.geo,
                       aes(x = X, xend = X + u * wf,
                           y = Y, yend = Y + v * wf),
                       arrow = arrow(length = unit(0.01, 'cm'))) +
          ylab("y") +
          xlab("x") +
          ggtitle(date.str)

        ggsave(plot = p1,
               dpi = 1200,
               width = 9.74,
               height = 6.44,
               file = out.filename)

      }


      # different color scheme
      #low = '#ffc500', high = '#c21500',
      #low = '#FF4E50', high = '#F9D423',
      # try rainbow(n=20, end = 4/6)

    }
    # ,arrow = arrow(length=unit(0.08, "cm"))
  }
}