#'ObtainSpatialAxes.R
#'Code for writing dummy spatial axes and storing them as 
ObtainSpatialAxes <- function(key, verbose=FALSE, 
                              lat.dim.vals=NA,
                              lat.units="",
                              lat.bnds.vals=NA,
                              lon.dim.vals=NA,
                              lon.units="",
                              lon.bnds.vals=NA){ 
  if(verbose){print(paste('using key', key))}
  
  if(is.character(key)){
    #There are two keys that rely on a reference file to generate a result, 
    #and one that generates the time dim and bounds on its own
    if(key=='dummy'){
      #provides a minimalist spatial axes, with units of ""
      lon.dim <- ncdim_def(name='lon', 
                           vals=seq(from=0, by=0, length.out=1), 
                           unlim=FALSE,
                           units='', #no units
                           create_dimvar=TRUE, 
                           longname="longitude")
      lat.dim <- ncdim_def(name='lat', 
                           vals=seq(from=0, by=0, length.out=1), 
                           unlim=FALSE,
                           units='', #no units
                           create_dimvar=TRUE, 
                           longname="latitude")
      lon.bounds <- c(seq(0, by=0, length.out=1), 
                      seq(0, by=0, length.out=1)) 
      dim(lon.bounds) <- c(2,1)
      lat.bounds <- c(seq(0, by=0, length.out=1), 
                      seq(0, by=0, length.out=1)) 
      dim(lat.bounds) <- c(2,1)
    }else if (key=='mean'){
      #print(match.call(expand.dots=T))
      #print(lat.bnds.vals)
      if (is.na(lat.bnds.vals)){
        #build from the lat dim instead
        message('lat bnds not present; calculating from lat values')
        lat.diff <- mean(diff(lat.dim.vals))
        lat.bnds.vals <- c(lat.dim.vals[1] - lat.diff, 
                           lat.dim.vals + lat.diff)
      }
      if (is.na(lon.bnds.vals)){
        message('lon bnds not present; calculating from lon values')
        lon.diff <- mean(diff(lon.dim.vals))
        lon.bnds.vals <- c(lon.dim.vals[1] - lon.diff, 
                           lon.dim.vals + lon.diff)
      }
      lon.dim <- ncdim_def(name='lon', 
                           #units="",
                           units=paste(as.character(lon.units)), 
                           vals=mean(range(lon.bnds.vals)), 
                           unlim=FALSE,
                           create_dimvar=TRUE, 
                           longname="longitude")
      lat.dim <- ncdim_def(name='lat', 
                           units=paste(as.character(lat.units)), 
                           #units="",
                           vals=mean(range(lat.bnds.vals)), 
                           unlim=FALSE,
                           create_dimvar=TRUE, 
                           longname="latitude")
      lon.bounds <- range(lon.bnds.vals) 
      dim(lon.bounds) <- c(2,1)
      lat.bounds <- range(lat.bnds.vals) 
      dim(lat.bounds) <- c(2,1)
    }else{
      stop(paste("Error in ObtainTimeAxis: Character key argument", key, 
                 "not recognized!"))
    }
  }else{
    stop(paste("Error in ObtainTimeAxis: Character key argument required!"))
  }
  #Longitude bounds attributes setting
  lon.bounds <- create.fudge.metavar.object('lon.bounds', lon.bounds, 
                                            prec='double', dimnames=c('bnds', 'lon'), 
                                            longname='longitude axis boundaries', 
                                            units=lon.units)
  lat.bounds <- create.fudge.metavar.object('lat.bounds', lat.bounds, 
                                            prec='double', dimnames=c('bnds', 'lat'), 
                                            longname='latitude axis boundaries', 
                                            units=lon.units)
#   attr(lon.bounds, "dimids") <- "bnds,lon"
#   attr(lon.bounds, "units") <- lon.dim$units
#   attr(lon.bounds, "prec") <- "double"
#   attr(lon.bounds, "longname") <- "longitude axis boundaries"
#   attr(lon.bounds, "dim") <- dim(lon.bounds)
#   #Latitude bounds attributes setting
#   attr(lat.bounds, "dimids") <- "bnds,lat"
#   attr(lat.bounds, "units") <- lat.dim$units
#   attr(lat.bounds, "prec") <- "double"
#   attr(lat.bounds, "longname") <- "latitude axis boundaries"
#   attr(lat.bounds, "dim") <- dim(lat.bounds)
  return(list(dim=list('lat'=lat.dim, 'lon'=lon.dim), 
              vars=list('lat_bnds'=lat.bounds, 'lon_bnds'=lon.bounds)))
}
