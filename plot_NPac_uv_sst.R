#plot_NPac_uv_sst


rm(list=ls())
source('CcSCB_functions.R')

# thewe were downloaded from upwell.pfeg.noaa.gov/erddap...

sst.file <- 'Data/sst_data/erdMBsstdmday025_20140101_20151231.nc'
uv.file <- 'Data/uv_data/SCUD_Pac_20140101_20151231.nc'

# get sst:
sst.data <- nc_open(sst.file)
sst.lon <- ncvar_get(sst.data, varid = 'longitude')
sst.lat <- ncvar_get(sst.data, varid = 'latitude')
sst <- ncvar_get(sst.data, varid = 'sst')
sst.t.units <- sst.data$dim$time$units
sst.t <- utcal.nc(sst.t.units,
                  ncvar_get(sst.data, varid = 'time'),
                  type = 's')

nc_close(sst.data)


uv.data <- nc_open(uv.file)
uv.lon <- ncvar_get(uv.data, varid = 'longitude')
uv.lat <- ncvar_get(uv.data, varid = 'latitude')
u <- ncvar_get(uv.data, varid = 'u')
v <- ncvar_get(uv.data, varid = 'v')
uv.t.units <- uv.data$dim$time$units
uv.t <- utcal.nc(uv.t.units,
                 ncvar_get(uv.data, varid = 'time'),
                 type = 's')

nc_close(uv.data)

# UV data need to be averaged over a larger spatial scale, like
# 1 degree by 1 degree so the arrows can show up on plots...
long.bands <- 135:244
lat.bands <- 5:50

# uv data are daily... so need to be averaged over each month:
u.avg <- v.avg <- array(data = NA, dim = dim(sst))
title.str <- vector(mode = 'character', length = dim(sst)[3])

yrs <- c(2014, 2015)
mos <- 1:12
das <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
y <- m <- c <- 1
for (y in 1:length(yrs)){
  for (m in 1:length(mos)){
    u2 <- u[, , uv.t >= as.Date(paste0(yrs[y], '-', mos[m], '-01')) &
              uv.t <= as.Date(paste0(yrs[y], '-', mos[m], '-', das[m]))]
    v2 <- v[, , uv.t >= as.Date(paste0(yrs[y], '-', mos[m], '-01')) &
              uv.t <= as.Date(paste0(yrs[y], '-', mos[m], '-', das[m]))]

    u.avg[, , c] <- apply(u2, MARGIN = c(1,2), FUN = mean, na.rm = T)
    v.avg[, , c] <- apply(v2, MARGIN = c(1,2), FUN = mean, na.rm = T)
    title.str[c] <- paste0(yrs[y], '-', mos[m])
    c <- c + 1

  }
}

wf <- 10
breaks <- 0:36
k<-1
for (k in 1:dim(sst)[3]){
  out.filename <- paste0('Figures/uv_sst_NPac/UV_sst_', title.str[k], '.png')
  curr.df <- na.omit(currFrame(uv.lon, uv.lat, u.avg[, , k], v.avg[, , k]))
  curr.df$date <- sst.t[k]
  sst.df <- na.omit(sstFrame(uv.lon, uv.lat, sst[, , k]))
  uv.stats <- matrix(data = NA,
                     nrow = length(long.bands) * length(lat.bands), ncol = 10)

  r <- k2 <- k1 <- 1
  for (k2 in 1:length(long.bands)){

    for (k1 in 1:length(lat.bands)){
      uv.stats[r, 1] <- long.bands[k2] + 0.5
      uv.stats[r, 2] <- lat.bands[k1] + 0.5
      tmp.data <- curr.df[curr.df$lon > long.bands[k2] & curr.df$lon <= long.bands[k2] + 1 &
                           curr.df$lat > lat.bands[k1] & curr.df$lat <= lat.bands[k1] + 1, ]
      uv.stats[r, 3] <- sum(tmp.data$u, na.rm = T)
      uv.stats[r, 4] <- sum(tmp.data$v, na.rm = T)
      uv.stats[r, 5] <- mean(tmp.data$u, na.rm = T)
      uv.stats[r, 6] <- mean(tmp.data$v, na.rm = T)
      uv.stats[r, 7] <-  sd(tmp.data$u, na.rm = T)
      uv.stats[r, 8] <-  sd(tmp.data$v, na.rm = T)
      uv.stats[r, 9] <-  SE(tmp.data$u)
      uv.stats[r, 10] <-  SE(tmp.data$v)
      r <- r + 1
    }


  }

  uv.stats <- na.omit(as.data.frame(uv.stats))
  colnames(uv.stats) <- c('center.lon', 'center.lat', 'u.sum', 'v.sum',
                          'u.avg', 'v.avg', 'u.sd', 'v.sd', 'u.SE', 'v.SE')
  #colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')
  p1 <- ggplot() +
    geom_raster(data = sst.df,
                aes(x = lon, y = lat, fill = sst)) +
    scale_fill_gradient(limits = c(0, 30),
                        low = "blue",
                        high = "red") +
    #geom_polygon(fill = land.color,
    #             data = all.islands.df,
    #             aes(x=newX, y=newY, group = name),
    #             inherit.aes = F) +
    stat_contour(data = sst.df,
                 aes(x=lon, y=lat, z=sst),
                 breaks = 20,
                 colour = "gray") +
    stat_contour(data = sst.df,
                 aes(x=lon, y=lat, z=sst),
                 breaks = 24,
                 colour = "gold") +
    geom_segment(data = uv.stats,
                 aes(x = center.lon, xend = center.lon + u.avg * wf,
                     y = center.lat, yend = center.lat + v.avg * wf),
                 arrow = arrow(length = unit(0.05, 'cm'),
                               type = 'closed'))  +
    # geom_polygon(fill = land.color,
    #              data = coast.line.Sp@data,
    #              aes(x=newX, y=newY, group = idx))  +
    ylab("Latitude") +
    xlab("Longitude") +
    ggtitle(title.str[k]) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 10, hjust = 0.5),
          legend.text = element_text(size = 8, vjust = 0))

  ggsave(plot = p1,
         dpi = 1200,
         width = 9.74,
         height = 6.44,
         file = out.filename)

}

