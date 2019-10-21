#Easy plot of SST from ERDDAP

# change date:

# http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.largePng?SST[(2017-08-14T00:00:00Z)][(31.005):(36.005)][(-121.995):(-116.995)]&.draw=surface&.vars=longitude%7Clatitude%7CSST&.colorBar=%7C%7C%7C12%7C26%7C&.bgColor=0xffccccff


plot.date <- '2019-10-14'
dir.name <- "Figures/Oct_2019/"

URL <- paste0('http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.largePng?SST[(',
              plot.date, 'T00:00:00Z)][(31.005):(36.005)][(-121.995):(-116.995)]',
              '&.draw=surface&.vars=longitude%7Clatitude%7CSST',
              '&.colorBar=%7C%7C%7C12%7C26%7C&.bgColor=0xffccccff')

download.file(URL,
              destfile = paste0(dir.name, 'jplG1SST_', plot.date, '.png'),
              mode='wb')


URL <- paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/erdMWsstd14day.largePng?sst[(",
              plot.date, "T00:00:00Z):1:(", 
              plot.date, "T00:00:00Z)][(0.0):1:(0.0)][(31.005):1:(36.005)][(238.0):1:(243.0)]")

#https://upwell.pfeg.noaa.gov/erddap/griddap/erdMWsstd14day.largePng?sst[(2019-10-13T00:00:00Z):1:(2019-10-13T00:00:00Z)][(0.0):1:(0.0)][(22.0):1:(51.0)][(205.0):1:(255.0)]

download.file(URL,
              destfile = paste0(dir.name, 'erdMWsstd14day_', plot.date, '.png'),
              mode='wb')
