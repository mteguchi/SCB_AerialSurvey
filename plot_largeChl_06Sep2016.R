#plot_Chl
# plots Chl maps

rm(list=ls())
library(ggplot2)

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

source('CcSCB_functions.R')

# find all SST files
file.names <- list.files(path = "RData/large_bkgd",
                         pattern = 'erdMWchla8day')

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

# get all island info here:
all.islands <- lapply(island.files, FUN = get.island, center.UTM)
# combine them into a data frame
all.islands.df <- do.call(rbind, all.islands)
land.color <- '#333333'
alpha.value <- 0.8

###  The following section was used to find the maximum value...
# max.val <- 0
# for (k1 in 1:length(file.names)){
#   var.date.str <- unlist(strsplit(unlist(strsplit(file.names[k], '_'))[1],
#                                   '_bkgd.RData'))
#   date.str <- unlist(strsplit(var.date.str, 'erdMWchla8day'))[2]
#
#   load(paste0('RData/large_bkgd/erdMWchla8day', date.str, '_bkgd.RData'))
#   alltmp <- na.omit(alltmp)
#   if (max.val < max(alltmp$var)){
#     max.val <- max(alltmp$var)
#   }
#
# }
# Turns out it was 464.528. Round up to 500.
### End section

max.chl <- 500

#1:length(file.names)
for (k in 241:length(file.names)){
  print(paste(k, 'in', length(file.names)))
  #load(paste0('RData/large_bkgd/', file.names[k]))
  var.date.str <- unlist(strsplit(unlist(strsplit(file.names[k], '_'))[1],
                              '_bkgd.RData'))
  date.str <- unlist(strsplit(var.date.str, 'erdMWchla8day'))[2]

  # load(paste0('RData/large_bkgd/erdMWchla8day', date.str, '_bkgd.RData'))
  # alltmp <- na.omit(alltmp)
  # tmp.df <- data.frame(X = alltmp$long,
  #                      Y = alltmp$lat,
  #                      var = alltmp$var)
  #
  # var.df.Sp <- latlon2sp(tmp.df, center.UTM)
  # var.df <- data.frame(X = var.df.Sp$newX,
  #                      Y = var.df.Sp$newY,
  #                      var = tmp.df$var)

  # Aggregate into the same box size as the others: 2 km x 2 km
  chl <- extract.cov.large.bkgd('erdMWchla8day', date.str,
                                center.UTM)

  chl$logmean <- log(chl$mean)
  p1 <- ggplot() +
    geom_raster(data = chl,
                aes(x =X, y = Y, fill = logmean)) +
    scale_fill_gradient(limits = c(-7, log(max.chl)),
                        low = "blue",
                        high = "red",
                        name = "ln(mean)") +
    geom_polygon(fill = land.color,
                 data = all.islands.df,
                 aes(x=newX, y=newY, group = name),
                 inherit.aes = F)  +
    stat_contour(data = chl,
                 aes(x=X, y=Y, z=logmean),
                 breaks = 0,
                 colour = "black") +
    # stat_contour(data = sst,
    #              aes(x=X, y=Y, z=mean),
    #              breaks = 24,
    #              colour = "gold") +

    ylab("y") +
    xlab("x") +
    ggtitle(date.str)

  # different color scheme
  #low = '#ffc500', high = '#c21500',
  #low = '#FF4E50', high = '#F9D423',
  # try rainbow(n=20, end = 4/6)

  ggsave(plot = p1,
         dpi = 600,
         height = 6.44,
         width = 9.74,
         file = paste0('Figures/chl_large_bkgd_', date.str, '.png'))
}

