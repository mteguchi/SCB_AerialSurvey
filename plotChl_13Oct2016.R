#plot_SST
# plots SST maps

rm(list=ls())
sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

source('CcSCB_functions.R')

file.names <- list.files(path = "RData/bkgd",
                         pattern = 'erdMWchla8day')

# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# need to get date info
date.2011.df <- dat2011[dat2011$segnum %in% Sdata.2011$segnum,
                        c('segnum', 'year', 'month', 'day')]
date.2015.df <- dat2015[dat2015$segnum %in% Sdata.2015$segnum,
                        c('segnum', 'year', 'month', 'day')]
# then assign the averages to each sighting - there probably is a more elegant
# way of doing this but loops do the job...
Sdata.2011$year <- Sdata.2011$month <- Sdata.2011$day <- NA
for (k in 1:dim(Sdata.2011)[1]){
  Sdata.2011[k, c('year',
                  'month',
                  'day')] <- date.2011.df[Sdata.2011$segnum[k] == date.2011.df$segnum,
                                                           c('year', 'month', 'day')]
}
Sdata.2015$year <- Sdata.2015$month <- Sdata.2015$day <- NA
for (k in 1:dim(Sdata.2015)[1]){
  Sdata.2015[k, c('year',
                  'month',
                  'day')] <- date.2015.df[Sdata.2015$segnum[k] == date.2015.df$segnum,
                                                           c('year', 'month', 'day')]
}

# these are from CcSCB_function.R
Sdata <- rbind(Sdata.2011, Sdata.2015)
# Create a date field - formatC works like the C function printf
Sdata$date <- paste(Sdata$year,
                    formatC(Sdata$month, width=2, format="d", flag="0"),
                    formatC(Sdata$day, width=2, format="d", flag="0"),
                    sep = '-')

ccData <- subset(Sdata, species == 'cc')
# convert lat/lon to x/y and extract date
latlon.df <- ccData[, c('mlon', 'mlat')]
names(latlon.df) <- c("X", "Y")
xy.df <- latlon2sp(latlon.df, center.UTM)@data
xy.df <- cbind(xy.df, ccData[, c('year', 'month', 'day')])

study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# get islands:
island.files <- c('Sutil.csv', 'SantaRosa.csv', 'SantaCruz.csv',
                  'SantaCatalina.csv', 'SantaBarbara.csv',
                  'SanNicolas.csv', 'SanClemente.csv',
                  'RockOffSanClemente.csv',
                  'GullIsland.csv', 'Anacapa.csv','PrinceIsland.csv',
                  'CastleRock.csv')

# get all island info here:
all.islands <- lapply(island.files, FUN = get.island, center.UTM)
# combine them into a data frame
all.islands.df <- do.call(rbind, all.islands)
land.color <- '#333333'
alpha.value <- 0.8

max.chl <- 500
#length(file.names)
for (k in 1:length(file.names)){
  print(paste(k, 'in', length(file.names)))
  #load(paste0('RData/bkgd/', file.names[k]))  # returns alltmp (df with date/lat/long/var)

  var.date.str <- unlist(strsplit(unlist(strsplit(file.names[k], '_'))[1],
                                  '_bkgd.RData'))
  date.str <- unlist(strsplit(var.date.str, 'erdMWchla8day'))[2]

  out.filename <- paste0('Figures/chl_SCB/chl_', date.str, '.png')
  if (file.exists(out.filename) == F){
    # prediction.data comes from CcSCB_function.R
    chl <- extract.cov.bkgd('erdMWchla8day', date.str,
                            center.UTM,
                            prediction.data[, c('x', 'y')])

    chl <- na.omit(chl)
    chl$logmean <- log(chl$mean)

    # plot sightings if any
    sightings.1 <- sightings.data[sightings.data$date > (as.Date(date.str)-8) &
                                    sightings.data$date <= as.Date(date.str),
                                  c('Longitude', 'Latitude')]
    sightings.2 <- ccData[ccData$date > (as.Date(date.str)-8) &
                            ccData$date <= as.Date(date.str),
                          c('mlon', 'mlat')]
    colnames(sightings.2) <- c('Longitude', 'Latitude')
    sightings.all <- rbind(sightings.1, sightings.2)
    names(sightings.all) <- c("X", "Y")

    # make a study area figure with transect lines overlaid...
    # create a new data frame for plotting
    lines.1 <- lines.df[lines.df$date > (as.Date(date.str)-8) &
                          lines.df$date <= as.Date(date.str), ]

    # Find the date of data - if one of sampling days, draw track lines
    # and sightings if any.

    p1 <- ggplot() +
      geom_raster(data = chl,
                  aes(x = X, y = Y, fill = logmean)) +
      scale_fill_gradient(limits = c(-7, log(max.chl)),
                          low = "blue",
                          high = "red") +
      geom_polygon(fill = land.color,
                   data = all.islands.df,
                   aes(x=newX, y=newY, group = name),
                   inherit.aes = F)  +
      stat_contour(data = chl,
                   aes(x=X, y=Y, z = logmean),
                   breaks = 0,
                   colour = "gray") +
      geom_polygon(fill = land.color,
                   data = coast.line.xy,
                   aes(x=newX, y=newY))  +

      ylab("y") +
      xlab("x") +
      ggtitle(date.str)

    if (dim(lines.1)[1] >0){
      p1 <-  p1 +
        geom_segment(data = lines.1,
                     aes(x = beginX, xend = endX,
                         y = beginY, yend = endY))
    }

    if (dim(sightings.all)[1] > 0){
      sightings.all.XY <- latlon2sp(sightings.all, center.UTM)@data
      row.names(sightings.all.XY) <- NULL
      p1 <- p1 +
        geom_point(data = sightings.all.XY,
                   aes(x = newX, y = newY),
                   colour = 'black',
                   shape = 23)

    }
    # different color scheme
    #low = '#ffc500', high = '#c21500',
    #low = '#FF4E50', high = '#F9D423',

    ggsave(plot = p1,
           dpi = 600,
           height = 5.74,
           width = 8.96,
           file = out.filename)
  }

}

