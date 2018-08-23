#runAirSegChopCc.R

source('fcn.AirSegChopCc.R')
# for 2015
# fnames2015 <- list.files('Data/', pattern = 'ed2.')
# fnames2011 <- list.files('Data/', pattern = 'SCB2011');
#
# for ( k in 1:length(fnames2015)){
#   filename <- paste0('Data/', fnames2015[k])
#   fcn.AirSegChopCc(filename)
# }
#
# for ( k in 1:length(fnames2011)){
#   filename <- paste0('Data/', fnames2011[k])
#   fcn.AirSegChopCc(filename)
# }


#for 2017
fnames2017 <- list.files('Data/', pattern = 'SCB2017')

for ( k in 1:length(fnames2017)){
  filename <- paste0('Data/', fnames2017[k])
  fcn.AirSegChopCc(filename)
}


