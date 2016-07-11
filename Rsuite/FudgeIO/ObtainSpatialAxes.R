#'ObtainSpatialAxes.R
#'Code for writing dummy spatial axes and storing them as 
ObtainSpatialAxes <- function(key, units = "", verbose=FALSE){ 
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
    }else{
      stop(paste("Error in ObtainTimeAxis: Character key argument", key, 
                 "not recognized!"))
    }
  }else{
    stop(paste("Error in ObtainTimeAxis: Character key argument required!"))
  }
  #Longitude bounds attributes setting
  attr(lon.bounds, "dimids") <- "bnds,lon"
  attr(lon.bounds, "units") <- lon.dim$units
  attr(lon.bounds, "prec") <- "double"
  attr(lon.bounds, "longname") <- "longitude axis boundaries"
  attr(lon.bounds, "dim") <- dim(lon.bounds)
  #Latitude bounds attributes setting
  attr(lat.bounds, "dimids") <- "bnds,lat"
  attr(lat.bounds, "units") <- lat.dim$units
  attr(lat.bounds, "prec") <- "double"
  attr(lat.bounds, "longname") <- "latitude axis boundaries"
  attr(lat.bounds, "dim") <- dim(lat.bounds)
  return(list(dim=list('lat'=lat.dim, 'lon'=lon.dim), 
              vars=list('lat_bnds'=lat.bounds, 'lon_bnds'=lon.bounds)))
}