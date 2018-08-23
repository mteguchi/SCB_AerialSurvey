# --------------------------------------------------------------------------
#
# LIST OF REMAINING ISSUES:   NONE KNOWN AS OF 6/18/2016
#
#
# --------------------------------------------------------------------------
  rm(list=ls())
################## R CHOPPING CODE FOR AERIAL SURVEY DATA ###################
#
#  Written by K. Forney and E. Becker 
#  in R 3.2.04;                               Last modified:  06/18/2016
# 
#  MODIFIED FROM ship segment chopping program segchopr_2015_07_26 (EAB)
#  
#  MODIFIED FROM matlab program segchopJAN.m (version 4 Feb 2011 by E. Becker)
#
#  MODIFIED 6/18/2016 to correct difference between "R" and "T" events
#
# R PACKAGES REQUIRED:
#
  library(splancs)
#
# Chops a given SWFSC aerial survey DAS file into on-effort
# subsegments of equal length and assigns sightings and related information
# to each subsegment.  Major segments (i.e., from "T" or "R" to "E" or "O") 
# are used as the basis for chopping.  If the extra km of transect remaining 
# after chopping is >/= 0.5 of the selected subsegment length, it is randomly
# assigned to a portion of the major segment and the associated distance
# provided as an offset value.  If the extra km is < 0.5 of the selected
# subsegment length, it is randomly added to one of the equal-length
# subsegments and the associated distance provided as an offset value.
# Therefore, the length of each subsegment is constrained to be between
# 0.5 - 1.5 of the target length.  The central tendency is thus
# approximately equal to the target subsegment length. (The only exception
# is when a major segment is < 0.5 of the subsegment length.)
#
# USER INPUT: The user must specify segment length (seglen; in km) and a
# list of target species ("targetsp"; consistent with SWFSC species codes).
#
# OUTPUTS: 
# ALLSEGS includes the start, end, and midpoints for all
# subsegments, the average Beaufort sea state and percent cloud cover for 
# each subsegment, year, month, day, time, and segment length (the majority 
# will be equal to the user-prescribed length). The first column of the 
# file is a subsegment counter.  For each target species selected, the file 
# includes two columns with number of sightings and number of animals.
#
# SITEINFO includes details for each sighting, including segment it belongs on,
# whether it was included in output segments (based on decliniation angle,
# obtype, etc.) decimal time, sighting number, latitude, longitude, declination
# angle, Obs type (1=primary, 0=other), No. species, species code
# and group size for each species.  Multi-species sightings are printed as 
# multiple lines (one for each species).
#
#--------------------------------------------------------------------------

#  03/21/2016 -- EAB modified to chop aerial .das files that use 1991-2000 key code

############################################################################
# Specify input DAS filename, species list, and declination angle cut off here. 
# Also set toggle whether to include or exclude sightings without 
# declination angle values.
#
filename <- 'test_EAB.das'
# filename <- 'CCS91_92_eab.das'

DASfile <- paste("_",filename,"_",sep="")
targetspchar <- c("DD","GG","LO","LB","PD")  # Provide these in upper case here
targetsp <-  c( 05,  21,  22,  27,  44)
TruncDecAngle <- 12   # Set cut off for including sightings
exclcodes <- c("ZC","EJ","MA","PV","CU","AT","PU","UA","UO","US","EL","MS")  # codes to exclude
Incl.da.NA <- TRUE # Set to include sightings without declination angle

#-----------------------------------------------------
  hdr2<-rep(targetsp,each=2)
  hdr1<-c("nSI","ANI")
  sihdr<- paste(hdr1,hdr2,sep=".")
  r<- length(targetsp)*2
  speciesdata <- data.frame(rbind(rep(0,r)))    #Set up data.frame with columns
  names(speciesdata) <- sihdr                   # for all target species

  sitecntr <- 0
  allsites <- NULL
  allsegs <- NULL
  numlines <- 0
  effortseg<-""
  gotV <- 0
  gotP <- 0
  gotW <- 0

  seglen <- 2                # segment length
  minsegdist <- 0.1           # km to assign to "R to E"s with zero distance
  segtol <- 0.5 * seglen      # max proportion of segment length to add
  set.seed(37)                #Initialize random seed 
                              #(matches matlab seed=137) for ttest file
  rand.loop <- NULL
  transnum<-NA
  flightyr<-NA
  clouds <-NA
  bf <- NA
  glare <- NA

#################### END USER INPUT ########################################
#
################### DEFINE FUNCTIONS #######################################
#
# FUNCTION to trim leading and trailing white space. Returns trimmed string.
#
  fn.trim <- function(string) {string <- gsub("(^ +) |( +$)", "",string)}
#
#---------------------------------------------------------------------------

#
# FUNCTION to convert lat/longs from DAS file line into decimal format
# with longitudes represented by 0-360 degrees.  
# Sample line format:  " 11E.065936 071896 N48:14.07 W124:54.98  134   10"
# Returns variables pos[1] = lat, pos[2] = lon

fn.getpostime <- function(line) {
   lat <- as.numeric(substr(line, 21,22)) + as.numeric(substr(line,24,28))/60
   lon <- as.numeric(substr(line, 31,33)) + as.numeric(substr(line,35,39))/60
   if (substr(line,20,20)=='S') {lat = lat * -1}
   if (substr(line,30,30) == 'W') {lon = 360 -lon}
   segtime <- substr(line,6,11)                  #get time
   segtime<-gsub(" ","0",segtime)                #replace all blanks with zeros
   hr <- as.numeric(substr(segtime,1,2))
   mn <- as.numeric(substr(segtime,3,4))
   sc <- as.numeric(substr(segtime,5,6))
   segtime <- hr + (mn/60)+ (sc/3600)
   postime <- c(lat,lon,segtime)
}
#---------------------------------------------------------------------------
# FUNCTION to convert 0-369 longitudes back so W longitudes are negative
# 0-180, E longtitudes are positive
#
fn.lon180 <- function(lon) {
   newlon <- lon
   if (!is.na(lon)) {
     if (lon > 180) {newlon <- lon - 360 } 
   }
   lon <- newlon
}
#---------------------------------------------------------------------------
# FUNCTION to calculate the great circle distance (in km) between two lat/lons
fn.grcirclkm <- function(lat1,lon1,lat2,lon2) {
  R <- pi/180      #angle in radians = angle in degrees * R
  D <- 180/pi      #angle in degrees = angle in radains * D
  dist <- 0

  NAcheck <- sum(is.na(c(lat1,lon1,lat2,lon2)))
  if (NAcheck==0) {             #only continue if no NA positions
    if ((lat1!=lat2) | (lon1!=lon2))  {
      dlat1 <- lat1 * R              # convert to radian values:
      dlng1 <- lon1 * R
      dlat2 <- lat2 * R
      dlng2 <- lon2 * R
      las <- sin(dlat1) * sin(dlat2);   # compute distance
      lac <- cos(dlat1) * cos(dlat2) * cos(dlng1 - dlng2)
      laf <- las + lac
      if (laf < -1) {
        laf <- -1
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)))
      } else if (laf < 1) {
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)));
      } else {
        error ('laf value out of bounds')
      }
      dist <- (dacos * D * 60) * 1.852           #calculate distance in km
    }
  }
  dist <- dist
}
#---------------------------------------------------------------------------
# FUNCTION to obtain W event data (Beaufort sea state and % cloud)
# Returns object with 2 data values 1=BF, 2=Clouds
#
fn.getW <- function(line,oldBF,oldCLOUDS) {
       BF <- substr(line,52,54)
       if (fn.trim(BF)!='') {BF <- as.numeric(BF)} else {BF <- oldBF}
       CLOUDS <- substr(line, 46, 49)
       if (fn.trim(CLOUDS)!='') {
         CLOUDS <- as.numeric(CLOUDS)
       } else {
         CLOUDS <- oldCLOUDS
       }
  Wdata <- c(BF, CLOUDS)
}
#---------------------------------------------------------------------------
# FUNCTION to obtain V event data (Viewing Conditions)
# Returns object with 2 data values 1=Lglare, 2=Rglare
#
#  Currently not using these data  PLACEHOLDER: CORRECT IF NEEDED
#
fn.getV <- function(line,oldGLARE) {
       GLARE <- substr(line,41,44)
       if (fn.trim(GLARE)!='') {GLARE <- as.numeric(GLARE)} else {GLARE <- oldGLARE}
}
#---------------------------------------------------------------------------
# FUNCTION to obtain P event data (Observer codes)
# Returns object with 3 data values 1=LOBS, 2=REC, 3=ROBS
#
fn.getP <- function(line,oldOBS) {
       LOBS <- substr(line,41,44)
       REC  <- substr(line,46,49)
       ROBS <- substr(line,51,54)
  Pdata <- c(LOBS, REC, ROBS)
}
#---------------------------------------------------------------------------
# FUNCTION to obtain Sighting event data 
# Returns object with multiple data values 
#
fn.getS <- function(DASLINES, slinenum, obsteam){
   nspp <- 0
   Sline <- DASLINES[slinenum]
   sitenum <- substr(Sline,41,44)
   postime <- fn.getpostime(Sline)
   sitepos <- postime[1:2]
   sitetime = postime[3]               #Identifies time of sighting
   DecAngle  <- as.numeric(substr(Sline,51,54))
   obsvr <- substr(Sline,46,49)
   if (obsvr==obsteam[1] | obsvr==obsteam[2] | obsvr==obsteam[3]) {
     obtype <- 1
   } else {
     obtype <- 0
   }
   if (DecAngle == '    ') {            #If Declination Angle was not recorded.
     DecAngle <- NA
   } else {
     DecAngle = as.numeric(DecAngle)
   }

   sppdata <- fn.trim(substr(Sline,60,80))       #Figure out how many species
   nspp <-  floor(nchar(sppdata)/5)+1
   spp <- rep(0, nspp)
   for (s in 1:nspp) {
     step <- (s-1)* 5
      spp[s] <- (substr(Sline,61+step,64+step))   # Species codes are characters here
   }
   numests <- 0
#  Move on to group size estimate lines
   gs<-rep(0,5)
   slinenum <- slinenum + 1
   GSline <- DASLINES[slinenum]
   while (substr(GSline,6,8)== '   ') {
     obok <- TRUE
     if (substr(GSline,4,4)!="C" & substr(GSline,4,4)!="?" & substr(GSline,46,49)!="xxxx") {
       best <- substr(GSline,46,49)
       if (best=='    '| best=='BEST') {            #If best not provided
           mings <- substr(GSline,56,59)            # use min or (if missing too) 
           if (mings=='    ') {mings <- '   1'}     # gs=1  as estimate
           best <- mings
       }         
       best <- as.numeric(best)
       for (s in 1:nspp) {                          #do for each species in sighting
         step <- (s-1)* 5
         tmp <- fn.trim(substr(GSline,61+step,64+step))
         if (nchar(tmp) == 0) {
           tmp <- 0
           obok <- FALSE                          #If any species % are missing, exclude observer
         }
         pcnt = as.numeric(tmp)
         if (obok) {
           if (s==1) {numests <-  numests + 1}    # Tally numests only once per sighting (@1st species)
           gs[s] <- gs[s] + best*pcnt/100
         }
       } # end looping through species
     } # end of check for group size estimate line
     slinenum <- slinenum + 1
     GSline <- DASLINES[slinenum]
   }
   if (numests==0) {
     gs <- NA
     Totgs <- NA
   } else {
     Totgs <- round(sum(gs)/numests,1)      #Total group size for sighting (all spp)
     for (s in 1:nspp) {
       gs[s] <- (gs[s]/numests)
       if (gs[s] < 1) {gs[s] <- 1}         # there has to be at least ONE!
       gs[s] <- round(gs[s],1)
     }
   }
   sitepos[2] <- fn.lon180(sitepos[2])
   Specdata <- list(nspp=nspp, species=spp, gs=gs)    # make list of Spp info
#  note that items set=NA here will be filled in in main program when segment is created
   Sitedata0 <- data.frame(segnum=NA, mlat=NA, mlon=NA, year=NA, 
                    transectNum=NA, included=0, stime=sitetime,  
                    snum=sitenum,  slat=sitepos[1], slon=sitepos[2], 
                    DecAngle=DecAngle,  obtype=obtype, nspp=nspp)
                   
   Sitedata <- cbind(Sitedata0, data.frame(spp=1, species=spp[1], Sppgs=gs[1], Totgs=Totgs))
   if (nspp>1) {
     for (s in 2:nspp) {
       Specline <- cbind(Sitedata0, data.frame(spp=s, species=spp[s], Sppgs=gs[s], Totgs=Totgs))
       Sitedata <- rbind(Sitedata, Specline)
     }
   }
  Sdata <- list(Specdata,Sitedata)
}

############################# END FUNCTIONS ################################
############################################################################
#
#Initialize misc loop variables and empty data frames
#
 segnum <- 0
 eff<-0
 numrandeffsegs <- 0

#
# Open connection to input file and read one line at a time, setting up
# 'infinite' loop until end of file is encountered 
#
 con <- file(filename, "r", blocking = FALSE)  # open connection to DAS file
 zz <- 1
 while (zz==1) {
   newline <- readLines(con, n=1)
   if (length(newline) == 0) {break}            # exit while loop at eof
   event1 <- substr(newline,4,4)
#
#  Check to see if we are starting new effort.  If so, re-initialize variables
#  and set eff = 1
#
   if (event1=="T") { 
      eff <- 1
      numlines <- 0
      effortseg<-""
      gotP <- 0
      gotW <- 0
      noKM <- 0
      transnum <- fn.trim(substr(newline,40,44))                  # Get transect number.
      if (transnum=="") {transnum <- prevtransnum}                 # If transect # = 0, we are
        if (transnum =="0") {efftyp <- "N"} else {efftyp <- "S"}  # on a connector or transit line
     prevtransnum <- transnum 

     flightyr = as.numeric(substr(newline,17,18)) # get flight year
     if (flightyr < 50) {
       flightyr <- flightyr + 2000
     } else {
       flightyr <- flightyr + 1900
     }               
   }
   
   if (event1=="R") { 
     eff <- 1
     numlines <- 0
     effortseg<-""
   }
#
# Store DAS line if we are on effort or at the end of effort
# (effortseg will have all lines from R to E)
#
   if (eff==1 | event1=="E" | event1=="O") {
    numlines <- numlines + 1
    effortseg[numlines] <- newline               # this should be E line
   }
#
#  Now check for other event codes, and score needed data accordingly
#  When the E line is encountered, this will trigger main chopping steps
#
   if (event1=='A') {
     altitude <- fn.trim(substr(newline,40,44))  # Get altitude.
     speed <- fn.trim(substr(newline,45,49))     # Get speed.                          
   } else if (gotW==0 & event1=='W') {           # During this run, pre-record
     prevbf <- bf                                # BF, clouds, & obs
     prevclouds <- clouds                         # at their first occurrence 
     bf <- fn.getW(newline, bf, clouds)[1]       # so these can be used as      
     clouds <- fn.getW(newline, bf, clouds)[2]   # values at start of effort     
     gotW <- 1                                   # during chopping later            
   } else if (gotV==0 & event1=='V') {         
     prevglare <- glare
     glare <- fn.getV(newline, glare)            # placeholder - not using glare   
     gotV <- 1 
   } else if (gotP==0 & event1=='P') {
     obs <- fn.getP(newline, c('','',''))
#
#  Determine whether we are at the end of an R -> E effort segment
#  If so, then all the other calculations, checks and tallies will be
#  made for the effortset lines stored so far.
#
   } else if (event1=="E" | event1=="O" ) { 
     eff <- 0     
#
# Now calculate distances and figure out how many subsegments there are
#
     alldist<-0
     startpos <-0
     for (i in 2:numlines) {
       if ((substr(effortseg[i],1,3)=='   '         # Check for and skip 
          & substr(effortseg[i],6,8)=='   ')        # group size, and comment 
          | substr(effortseg[i],4,4)=='C'           # or canceled '#' lines 
          | substr(effortseg[i],4,4)=='#' ) {       # without positions
          alldist[i] <- 0
          if (startpos == 0) {startpos <- i-1}
       } else {
          if (startpos == 0) {startpos <- i-1}
          line1pos <- fn.getpostime(effortseg[startpos])     # get decimal lat/longs for each line
          line2pos <- fn.getpostime(effortseg[i])            # linepos[1]=lat; linepos[2]=lon
          alldist[i] <- fn.grcirclkm(line1pos[1],line1pos[2],line2pos[1],line2pos[2])
          startpos <-0
       }
     }
     if (sum(alldist)==0) {                     #In rare cases when R to E covers 
        nline <-length(alldist)                 # zero distance, change alldist 
        alldist[2:nline]<-minsegdist/(nline-1)  # values to make the total distance
        noKM <- 1                               # equal minsegdist specified above,
     }                                          # and set a toggle for later
     totdist <- sum(alldist)
     if (totdist < seglen) {                    #If totdist < target segment length
       numsegs <- 1                             # then we have only a single short
       tgtdist <- totdist                       # segment of totdist length.
     } else {
       numsegs <-floor(totdist/seglen)    
       leftover <-((totdist/seglen) - floor(totdist/seglen))*seglen
       if (leftover >= segtol | numsegs == 0) {   # Figure out how many segments and
         numsegs = numsegs + 1                    # assign extra bit to a random segment
         extrabit = leftover
       } else {    
         extrabit <- seglen + leftover
       }
       tgtdist <-0                  # tgtdist stores each subsegment's target length
       tgtdist[1:numsegs] <- seglen  
       randnum <- runif(1,0,1)
       randpick <- floor(randnum*numsegs)+1     #choose random segment to absorb 
       tgtdist[randpick] <- extrabit            # extrabit. 
#      rand.loop <- c(rand.loop,randnum)    
       numrandeffsegs <- numrandeffsegs + 1     #Count randomly placed effort segs
     }
#
###############################################################################
# Now we go through effortseg and chop up accordingly, based on target 
# distances stored in tgtdist, grabbing sighting and other info as we go
###############################################################################
# 
     addlines<-speciesdata
     addlines[1:numsegs,] <- speciesdata
     allsites <- rbind(allsites,addlines)       #add required lines to allsites
     segctr <- 1                                #Keep track of segments, sightings,
     cumdist <- 0                               #Initialize segment distance=0
     cumbf <- 0
     cumclouds <- 0
     prevdist <- 0
     startline <-1
     postime <-fn.getpostime(effortseg[startline]) 
     lat2 <- postime[1]  #This is start of first segment
     lon2 <- postime[2]    
     time2 <- postime[3]  
     startlat <- lat2
     startlon <- lon2
     starttime<- time2
     halftgt <- tgtdist[segctr]/2
     gotmid <-0
     for (linenum in 2:length(effortseg))  {         #Run through data lines
       siline <- 0
       dasline <- effortseg[linenum]
       cumdist <- cumdist + alldist[linenum]         # get line distance
                                                     #Keep track of weather
       event <- substr(dasline,4,4)                  # Get event code
       cumbf <- cumbf + bf*alldist[linenum]          # Keep track of Beauf
       cumclouds <- cumclouds + clouds*alldist[linenum] # Keep track of clouds
                                                     # Check Event codes for changes
       if (event=="W") {                             # Need to get new BF & clouds   
         prevbf <- bf
         prevclouds <- clouds
         bf <- fn.getW(dasline, bf, clouds)[1]        
         clouds <- fn.getW(dasline, bf, clouds)[2]
       }      
       if (event=="V") {                             #Need to get new glare
         prevglare <- glare                              # Fix this later if needed
         glare <- fn.getV(dasline, glare)                
       }
       if (event=="P") {                             #Need to get new obs codes
         obs <- fn.getP(dasline, obs)
       }
       if (event=="A") {                             # placeholder for now
         Newaltitude <- altitude
       }
       if (event=="S") {                             #OK, 
         Sinfo <- fn.getS(effortseg, linenum, obs)   #It's a SIGHTING!!!!
         siline <- 1
       }      
       if ((substr(dasline,1,3)=='   '               #Check for group 
          & substr(dasline,6,8)=='   ')              # size and comment / canceled
          | substr(dasline,4,4)=='#'                 # lines  and set toggle 
          | substr(dasline,4,4)=='C')  { 
         lineok <-0                                  # toggle so we don't
       } else {                                      # try to get positions
         lineok <-1                                  # where there are none
       }                                             # in the next section
       if (lineok==1) {
         lat1 <- lat2
         lon1 <- lon2
         time1 <- time2
         postime <- fn.getpostime(dasline)
         lat2 <- postime[1]
         lon2 <- postime[2]
         time2 <- postime[3]
         segmo = as.numeric(substr(dasline,13,14))   # get month
         segda = as.numeric(substr(dasline,15,16))   # get day
       }

       while (round(cumdist,3) >= round(halftgt,3)){        #While our cumdist 
         linedist <- fn.grcirclkm(lat1, lon1, lat2, lon2)   # still has halftgts 
         if (linedist==0) {                                 # left in it....
           intratio<-0                                      #Include catch for
         } else {                                           # linedist=0 cases, so
           intratio <- (halftgt-prevdist)/linedist          # halflat/halflon don't 
         }                                                  # end up as NAs
         latdiff <- lat2-lat1
         londiff <- lon2-lon1
         timediff <- time2-time1
         halflat <- lat1 + intratio*latdiff
         halflon <- lon1 + intratio*londiff
         halftime <- time1 + intratio*timediff
         if (gotmid==0) {           #if gotmid==0, we have passed midpoint of segment
           gotmid <- 1
           mlat <- halflat
           mlon <- halflon
           mtime <- halftime
           prevdist <- 0
           cumdist <- cumdist - halftgt
         } else {                   #if gotmid==1; this is end of segment
           endlat <- halflat
           endlon <- halflon
           endtime <- halftime
           gotmid <- 0    
           cumdist <- cumdist - halftgt

           avgbf    <- (cumbf - cumdist*prevbf)/(halftgt*2)
           avgclouds <- (cumclouds - cumdist*prevclouds)/(halftgt*2)
           prevdist <- 0

           outlon <- sapply(c(startlon, endlon, mlon),fn.lon180)  #change lons to 0-180

           segnum <- segnum + 1
           segdata <- data.frame(segnum=segnum, efftyp=efftyp,  
                      stlin=startline, endlin=linenum, 
                      lat1=round(startlat,4), lon1=round(outlon[1],4), 
                      lat2=round(endlat,4), lon2=round(outlon[2],4), 
                      mlat=round(mlat,4), mlon=round(outlon[3],4),
                      transectNum=transnum, year=flightyr,
                      month=segmo, day=segda, mtime=round(mtime,3),
                      dist=round(halftgt*2,4), aveBF=round(avgbf,1), 
                      aveClouds=round(avgclouds,1))
           allsegs<- rbind(allsegs, segdata)

           cumbf <- cumdist*prevbf
           cumclouds <- cumdist*prevclouds

           if (segctr<numsegs) {
             segctr <- segctr + 1
             halftgt <- tgtdist[segctr]/2
             startline <- linenum
             startlat <- endlat
             startlon <- endlon
             starttime <-endtime
           }
         } 
         lat1 <- halflat
         lon1 <- halflon         
         time1 <- halftime
       }                                                        #end of while loop

       if (event=="S") {
         sitecntr <- sitecntr + 1                               # Cumulative sighting tally

         seg4sit <- segnum + 1                                  # Sighting should be scored on next segment, unless
         if (round(cumdist,3)==0 &                              # it is right at the end of the segment (in which 
            sum(alldist[1:linenum])>0) {                        # case cumdist=0 and sum(alldist[1:linenum])>0 
            seg4sit <- segnum                                   # NOTE: the latter =0 if sighting is at R position,
         }                                                      # in which case is should be scored on segnum + 1
                                                                # [THIS FIXES ERROR FOUND BY PAUL FIEDLER 4/2015]
         spinfo <- Sinfo[1]
         Sinfo[[2]]$segnum  <- seg4sit                          # Record segment #
         if (Sinfo[[2]]$obtype[1]==1) {                         # dec angle checks later
           for (s in 1:spinfo[[1]]$nspp) {                      # For all species in sighting
             spchar  <- toupper(spinfo[[1]]$species[s])
             spchar <- fn.trim(spchar)
             species <- targetsp[which(spchar==targetspchar)]   # Is it a target species?
            noncet <- which(exclcodes ==spinfo[[1]]$species[s]) # Check for pinniped, etc. codes to exclude.
            if (length(noncet)==0) {                            # Only continue scoring if cetacean. 
             sp <- which(targetsp==species)                    
             if (length(sp)>0)  {                               # If so, check declination angle
               da<-Sinfo[[2]]$DecAngle[1]
               da <- abs(da)
               if (da>=TruncDecAngle |(is.na(da) & Incl.da.NA)) {  # to see if within dec angle (NA's included by default)
                 spcol<- ((sp-1)*2)+1                           # anis in appropriate col
                 allsites[seg4sit,spcol]<-allsites[seg4sit,spcol]+1
                 allsites[seg4sit,spcol+1]<-allsites[seg4sit,spcol+1]+spinfo[[1]]$gs[s]
                 Sinfo[[2]][s,]$included <- 1              #Set toggle that sighting included in segments
               }
             }
            }
          }
         }
         if (sitecntr == 1) {
            siteinfo <- data.frame(Sinfo[[2]])
         } else {
            siteinfo <- rbind(siteinfo,data.frame(Sinfo[[2]]))
         }
       } 
       prevdist <- cumdist
       prevbf <- bf
       prevclouds <- clouds
     } # end of for - get next DAS line
   }  #End of 'if else' statements above for event codes
 } #THIS ENDS LOOP FOR CURRENT R to E SET OF LINES -- GO READ SOME MORE
 close(con)

# Before writing siteinfo to file, add final segment details for each sighting line
# (these have so far all been recorded as NA).

 for (s in 1:nrow(siteinfo)) {                 
  siteinfo$mlat[s]      <- allsegs$mlat[allsegs$segnum==siteinfo$segnum[s]]       # midpoint, year, and
  siteinfo$mlon[s]      <- allsegs$mlon[allsegs$segnum==siteinfo$segnum[s]]       # transect number  
  siteinfo$year[s]      <- allsegs$year[allsegs$segnum==siteinfo$segnum[s]]       # details
  siteinfo$transectNum[s] <- as.numeric(as.character(allsegs$transectNum[allsegs$segnum==siteinfo$segnum[s]]))
 }

#   
# Write output files for allsegs, allsites, siteinfo, and all data combined  
#
 filedate <- format(Sys.Date(), "_%Y-%m-%d")
 write.csv(siteinfo, paste("SITEINFO",DASfile,seglen,"km",filedate,".csv",sep=""))
# write.csv(allsites, paste("ALLSITES",DASfile,seglen,"km",filedate,".csv",sep=""))
# write.csv(allsegs, paste("ALLSEGS",DASfile,seglen,"km",filedate,".csv",sep=""))

 if (nrow(allsegs)== nrow(allsites)) {
   segdata <- cbind(allsegs, allsites)
   write.csv(segdata, paste("SEGDATA",DASfile,seglen,"km",filedate,".csv",sep=""))
 } else {
   print("---")
   print(paste("PROCESSING ERROR -- Rows for ALLSEGS vs ALLSITES are:",
           nrow(allsegs),"vs",nrow(allsites),sep=" "))
   print("No combined SEGDATA file could be created.")
   print("Please check separate files for details.")
}

#
#  Lastly, re-set random number seed and re-generate the randpick values for
#  processed effort segments so they can be stored, in case we ever need to
#  recreate them in the future.
#

  set.seed(37)                # Re-set random seed to starting value 
  rand.redo <- runif(numrandeffsegs,0,1)
  rand.out <- data.frame(RandPick=rand.redo)
  write.csv(rand.out,paste("RandPicks",DASfile,seglen,"km",filedate,".csv",sep=""))

######################### END OF PROGRAM ####################################

