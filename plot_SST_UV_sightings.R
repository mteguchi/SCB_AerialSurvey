#plot_SST
# plots SST maps

rm(list=ls())
# sysInfo <- Sys.info()
# ifelse(sysInfo[1] == 'Linux',
#        source('~/Documents/R/TomosFunctions.R'),
#        source('~/R/TomosFunctions.R'))

source('CcSCB_functions.R')
library(viridis)
library(dplyr)
library(raster)
library(rasterVis)

save.fig <- T

coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                           lon.limits = c(-127.55, -115),
                           lat.limits = c(28, 36))
coast.line.df <- do.call(rbind, coast.line)

# take two-week or 8-day SST and geostrophic current data files, overlay with
# sightings data.  Get sightings first:
# Combine all sightings
sightings.data.df <- sightings.data # in CcSCB_functions.R
sightings.data.df$Year <- as.numeric(unlist(lapply(strsplit(sightings.data.df$date,
                                                           split = '-'),
                                                  FUN = function(x) x[1])))

sightings.data.df <- dplyr::select(sightings.data.df, date,
                                   Latitude, Longitude, Year)

sightings.data.df <- dplyr::filter(sightings.data.df,
                                   Latitude >= 28 & Latitude <= 36 &
                                     Longitude >= -127.5 & Longitude <= -115)

names(sightings.data.df) <- c('Date', 'Y', 'X', 'Year')
#sightings.data.Sp <- latlon2sp(sightings.data.df, center.UTM)

sightings.2006.2014 <- dplyr::filter(sightings.data.df,
                                     Year == 2006 | Year == 2014)

sightings.2006.2014$Date <- as.Date(sightings.2006.2014$Date,
                                    format = '%Y-%m-%d')

# combine ship-board sightings with aerial sightings:
air.sights <- ccData[, c('mlat', 'mlon', 'year', 'month', 'day')]
air.sights$Date <- as.Date(paste(air.sights$year,
                                 air.sights$month,
                                 air.sights$day,
                                 sep = '-'), format = '%Y-%m-%d')

air.sights <- air.sights[, c('Date', 'mlat', 'mlon', 'year')]
colnames(air.sights) <- c('Date', 'Y', 'X', 'Year')

all.sights <- rbind(sightings.2006.2014, air.sights)

ylim <- c(28, 36)
xlim <- c(-127.5, -115)

# get uv data; 2-week period around the sighting date:
# get sst data; 2-week period around the sighting date:
k <- 1
for (k in 1:nrow(all.sights)){
  # consistent # days  5 days for sst and uv (OSCAR)
  tlim <- c(all.sights$Date[k]-4, all.sights$Date[k])
  # Multi-scale ultra-high resolution (MUR) SST analysis fv04.1,
  # Global, 0.01 degrees, 2002-present, daily
  tmp <- get.sst.dt(xlim, ylim, tlim)

  # Surface currents from a diagnostic model (SCUD): Pacific
  # 1/4 degree - this is daily
  tmp <- get.geo.dt(xlim, ylim, tlim) # in CcSCB_functions.R

  # OSCAR sea surface velocity, 1/3 degree, L4, Global
  # 1992-present 5 day composite
  tmp <- get.geo.dt.jplOscar(xlim, ylim, tlim)
}

vec.std.df <- data.frame(x = -117, y = 35.0,
                         u = 0.2, v = 0.0,
                         text = '0.2 m/s')
land.color <- '#333333'
alpha.value <- 0.8
wf <- 2
k <- 1
for (k in 1:nrow(all.sights)){
  tlim <- c(all.sights$Date[k]-4, all.sights$Date[k])
  tlims <- paste0(all.sights$Date[k]-4, '_', all.sights$Date[k])
  sst.data.file <- paste0('sst_jplMURSST41_', tlims, '.nc')
  datafileID <- nc_open(paste0("Data/sst_data/", sst.data.file))

  lon.sst <- ncvar_get(datafileID, varid="longitude")
  lat.sst <- ncvar_get(datafileID, varid="latitude")
  time.sst <- ncvar_get(datafileID, varid="time")
  time.sst <- as.POSIXlt(time.sst, origin='1970-01-01',tz= "GMT")
  sst.mat <- ncvar_get(datafileID, varid = 'analysed_sst')
  nc_close(datafileID)
  # sst.mat is a 3-d array with days over the 3rd dimension
  sst.avg <- apply(sst.mat, MARGIN = c(1,2), FUN = mean, na.rm = T)

  sst.df <- na.omit(sstFrame(lon.sst, lat.sst, sst.avg))
  colnames(sst.df) <- c('longitude', 'latitude', 'sst')
  #sst.df.Sp <- latlon2sp(sst.df, center.UTM)
  #sst <- na.omit(sst.df.Sp@data)

  # pool by 2 km by 2 km
  # dx <- dy <- 2.0
  # X.seq <- seq(from = min(sst$newX), to = max(sst$newX), by = dx)
  # Y.seq <- seq(from = min(sst$newY), to = max(sst$newY), by = dy)

  # using raster package
  sst_pts <- SpatialPointsDataFrame(coords = cbind(sst.df$longitude,
                                                   sst.df$latitude),
                                    data = data.frame(sst.df$sst))
  sst.df.2 <- raster(nrows = length(lat.sst),
                     ncols = length(lon.sst),
                     xmn = min(lon.sst),
                     ymn = min(lat.sst),
                     xmx = max(lon.sst),
                     ymx = max(lat.sst),
                     res = 0.1,
                     crs = "+proj=longlat +datum=WGS84")

  sst.raster <- rasterize(sst_pts, sst.df.2, fun = mean)
  sst.raster.df <- data.frame(lon = coordinates(sst.raster$sst.df.sst)[,1],
                              lat = coordinates(sst.raster$sst.df.sst)[,2],
                              sst = getValues(sst.raster$sst.df.sst))

  uv.data.file <- paste0('uvgeo_', tlims, '.nc')
  datafileID <- nc_open(paste0('Data/uv_data/', uv.data.file))
  lon.uv <- ncvar_get(datafileID, varid = 'longitude')
  lat.uv <- ncvar_get(datafileID, varid = 'latitude')
  time.uv <- ncvar_get(datafileID, varid="time")
  time.uv <- as.POSIXlt(time.uv, origin='1970-01-01',tz= "GMT")

  # some years are u and v, whereas others are u_current and v_current
  u <- ncvar_get(datafileID, varid = names(datafileID$var)[1])
  v <- ncvar_get(datafileID, varid = names(datafileID$var)[2])
  #t.units <- datafileID$dim$time$units
  #t <- utcal.nc(t.units,
  #              ncvar_get(datafileID, varid = 'time'),
  #              type = 's')
  nc_close(datafileID)

  u.avg <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
  v.avg <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

  curr.df <- na.omit(currFrame(lon.uv, lat.uv, u.avg, v.avg))
  # longitude is not consistent among years - some are negative and others
  # are >180
  curr.df$lon[curr.df$lon > 180] <- curr.df$lon[curr.df$lon > 180] - 360
  colnames(curr.df) <- c('X', 'Y', 'u', 'v')
  curr.df.Sp <- latlon2sp(curr.df, center.UTM)
  curr <- curr.df.Sp@data

  ############################
  uv.data.file.Oscar <- paste0('uvgeo_jplOscar_', tlims, '.nc')
  datafileID.Oscar <- nc_open(paste0('Data/uv_data/', uv.data.file.Oscar))
  lon.uv.Oscar <- ncvar_get(datafileID.Oscar, varid = 'longitude')
  lat.uv.Oscar <- ncvar_get(datafileID.Oscar, varid = 'latitude')

  # some years are u and v, whereas others are u_current and v_current
  u <- ncvar_get(datafileID.Oscar, varid = names(datafileID.Oscar$var)[1])
  v <- ncvar_get(datafileID.Oscar, varid = names(datafileID.Oscar$var)[2])
  #t.units <- datafileID$dim$time$units
  #t <- utcal.nc(t.units,
  #              ncvar_get(datafileID, varid = 'time'),
  #              type = 's')
  nc_close(datafileID.Oscar)

  u.avg.Oscar <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
  v.avg.Oscar <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

  curr.df.Oscar <- na.omit(currFrame(lon.uv.Oscar, lat.uv.Oscar,
                                     u.avg.Oscar, v.avg.Oscar))
  # longitude is not consistent among years - some are negative and others
  # are >180
  curr.df.Oscar$lon[curr.df.Oscar$lon > 180] <- curr.df.Oscar$lon[curr.df.Oscar$lon > 180] - 360
  colnames(curr.df.Oscar) <- c('X', 'Y', 'u', 'v')
  curr.df.Oscar.Sp <- latlon2sp(curr.df.Oscar, center.UTM)
  curr.Oscar <- curr.df.Oscar.Sp@data
  ############################

  out.filename <- paste0('Figures/uv_sst/uv_sst_',
                         tlims, '.png')

  out.filename.Oscar <- paste0('Figures/uv_sst/uv_sst_Oscar_',
                         tlims, '.png')

  if (file.exists(out.filename) == FALSE){

    # plot sightings if any
    sightings.1 <- filter(all.sights, Date >= tlim[1] &
                            Date <= tlim[2])

    p1 <- ggplot() +
      geom_raster(data = sst.raster.df,
                  aes(x = lon,
                      y = lat,
                      fill = sst)) +

      #scale_color_viridis(na.value = land.color,
      #                    limits = c(10, 25),
      #                    discrete = T) +
      scale_fill_gradientn(na.value = land.color,
                           limits = c(10, 30),
                           colors = viridis(n = 20)) +
      geom_polygon(fill = land.color,
                   data = all.islands.df,
                   aes(x=lon, y=lat, group = name),
                   inherit.aes = F) +
      geom_polygon(fill = land.color,
                   data = coast.line.df,   # needs to be expanded a little
                   aes(x=Longitude, y=Latitude, group=idx)) +
      geom_segment(data = curr.df,
                   aes(x = X, xend = X + u * wf,
                       y = Y, yend = Y + v * wf),
                   arrow = arrow(length = unit(0.1, 'cm'))) +
      geom_point(data = sightings.1,
                 aes(x = X, y = Y),
                 color = 'red',
                 shape = 18, size = 2) +
      geom_segment(data = vec.std.df,
                   aes(x = x, xend = x + u * wf,
                       y = y, yend = y + v * wf),
                   arrow = arrow(length = unit(0.1, ('cm'))),
                   color = 'white',
                   size = 1.0) +
      geom_text(data = vec.std.df,
                aes(x = x, y = y-0.2, label = text),
                color = 'white',
                vjust = 1,
                hjust = 0) +
      ylab("Latitude") +
      xlab("Longitude") +
      ggtitle(tlims) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 10, hjust = 0.5),
            legend.text = element_text(size = 8, vjust = 0))

    p1.Oscar <- ggplot() +
      geom_raster(data = sst.raster.df,
                  aes(x = lon,
                      y = lat,
                      fill = sst)) +

      #scale_color_viridis(na.value = land.color,
      #                    limits = c(10, 25),
      #                    discrete = T) +
      scale_fill_gradientn(na.value = land.color,
                           limits = c(10, 30),
                           colors = viridis(n = 20)) +
      geom_polygon(fill = land.color,
                   data = all.islands.df,
                   aes(x=lon, y=lat, group = name),
                   inherit.aes = F) +
      geom_polygon(fill = land.color,
                   data = coast.line.df,   # needs to be expanded a little
                   aes(x=Longitude, y=Latitude, group=idx)) +
      geom_segment(data = curr.df.Oscar,
                   aes(x = X, xend = X + u * wf,
                       y = Y, yend = Y + v * wf),
                   arrow = arrow(length = unit(0.1, 'cm'))) +
      geom_point(data = sightings.1,
                 aes(x = X, y = Y),
                 color = 'red',
                 shape = 18, size = 2) +
      geom_segment(data = vec.std.df,
                   aes(x = x, xend = x + u * wf,
                       y = y, yend = y + v * wf),
                   arrow = arrow(length = unit(0.1, ('cm'))),
                   color = 'white',
                   size = 1.0) +
      geom_text(data = vec.std.df,
                aes(x = x, y = y-0.2, label = text),
                color = 'white',
                vjust = 1,
                hjust = 0) +
      ylab("Latitude") +
      xlab("Longitude") +
      ggtitle(tlims) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 10, hjust = 0.5),
            legend.text = element_text(size = 8, vjust = 0))
    # if (dim(lines.1)[1] >0){
    #   p1 <-  p1 +
    #     geom_segment(data = lines.1,
    #                  aes(x = beginX, xend = endX,
    #                      y = beginY, yend = endY))
    # }
    #
    # if (dim(sightings.all)[1] > 0){
    #   sightings.all.XY <- latlon2sp(sightings.all, center.UTM)@data
    #   row.names(sightings.all.XY) <- NULL
    #   p1 <- p1 +
    #     geom_point(data = sightings.all.XY,
    #                aes(x = newX, y = newY),
    #                colour = 'black',
    #                shape = 23)

    # different color scheme
    #low = '#ffc500', high = '#c21500',
    #low = '#FF4E50', high = '#F9D423',

    if (save.fig){
      ggsave(plot = p1,
             dpi = 1200,
             height = 9.5,
             width = 10.5,
             file = out.filename)
      ggsave(plot = p1.Oscar,
             dpi = 1200,
             height = 9.5,
             width = 10.5,
             file = out.filename.Oscar)

    }
  }
}


