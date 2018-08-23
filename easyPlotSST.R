#Easy plot of SST from ERDDAP

# change date:

# http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.largePng?SST[(2017-08-14T00:00:00Z)][(31.005):(36.005)][(-121.995):(-116.995)]&.draw=surface&.vars=longitude%7Clatitude%7CSST&.colorBar=%7C%7C%7C12%7C26%7C&.bgColor=0xffccccff


plot.date <- '2015-10-22'

URL <- paste0('http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.largePng?SST[(',
              plot.date, 'T00:00:00Z)][(31.005):(36.005)][(-121.995):(-116.995)]',
              '&.draw=surface&.vars=longitude%7Clatitude%7CSST',
              '&.colorBar=%7C%7C%7C12%7C26%7C&.bgColor=0xffccccff')

download.file(URL,
              destfile = paste0('Figures/SST_Aug_Sep2017/jplG1SST_', plot.date, '.png'),
              mode='wb')
