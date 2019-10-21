#Easy plot of SST from ERDDAP

# change date:

# http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.largePng?SST[(2017-08-14T00:00:00Z)][(31.005):(36.005)][(-121.995):(-116.995)]&.draw=surface&.vars=longitude%7Clatitude%7CSST&.colorBar=%7C%7C%7C12%7C26%7C&.bgColor=0xffccccff


plot.date <- "2019-09-26"
dir.name <- "Figures/Oct_2019/"
dat.dir <- "data/ncfiles/"

URL <- paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/erdVH2018chla8day.largePng?chla%5B(",
              plot.date, "T00:00:00Z)%5D%5B(36.02083):(31.02083)%5D%5B(-122.0208):(-117.0208)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchla&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff")

download.file(URL,
              destfile = paste0(dir.name, 'erdVH2018Chla8day_', plot.date, '.png'),
              mode='wb')


# this one has more up-to-date data
plot.date <- "2019-10-19"
URL <- paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/erdMBchla1day_LonPM180.largePng?chlorophyll%5B(",
              plot.date, "T12:00:00Z)%5D%5B(0.0)%5D%5B(31.0):(36.0)%5D%5B(-122.0):(-117.0)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlorophyll&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff")

download.file(URL,
              destfile = paste0(dir.name, 'erdMBchla1day_', plot.date, '.png'),
              mode='wb')

# also get data:
URL <- paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/erdMWchla1day.nc?chlorophyll[(",
              plot.date, "T12:00:00Z):1:(", plot.date, 
              "T12:00:00Z)][(0.0):1:(0.0)][(31):1:(36)][(238):1:(243)]")

dat <- download.file(URL,
                     destfile = paste0(dat.dir, "erdMBchla1day_", plot.date, ".nc"),
                     mode = "wb")
