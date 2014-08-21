# Aparna Radhakrishnan 08/04/2014
ReadNC <- function(nc.object,var.name=NA,dstart=NA,dcount=NA,verbose=FALSE) {
  #'Reads data from netCDF file object
  #'Returns netCDF variable data
  #'uses get.var.ncdf
  #'Returned values will be in ordinary R double precision if the netCDF variable type is float or double. 
  #'Returned values will be in R's integer storage mode if the netCDF variable type is short or int. 
  #'Returned values will be of character type if the netCDF variable is of character type.
  #'And finally, all returned values will have 3 dimensions, even if the lenght of those dimensions is 1
  #'(i.e. a single data point has the dimensions c(1,1,1))
  #'@param [count]: A vector of integers indicating the count of values to read along each dimen- sion (order is X-Y-Z-T). 
  #'@param [start]: A vector of indices indicating where to start reading the passed values (begin- ning at 1). 
  #'The length of this vector must equal the number of dimensions the variable has. Order is X-Y-Z-T (i.e., the time dimension is last).
  #'If not specified, reading starts at the beginning of the file (1,1,1,...). 
  if((is.na(dstart)) && (is.na(dcount)) && (is.na(var.name))) {
  clim.in <- ncvar_get(nc.object)            
  }else {
  var.read <- ncvar_get(nc.object,var.name,dstart,dcount,collapse_degen=FALSE)
  }
#  nc_close(nc.object) #Creates an error when called again if left uncommented
  return(var.read)
=======
  clim.in <- ncvar_get(nc.object,var.name,dstart,dcount,collapse_degen=FALSE) 
  }
  #### get standard name,long name, units if present ####
  attname = 'standard_name'
  cfname <- ncatt_get(nc.object, var.name, attname) 
  attname = 'long_name'
  long_name <- ncatt_get(nc.object, var.name, attname)
  attname <- 'units' 
  units <- ncatt_get(nc.object, var.name, attname)
  #######################################################
  listout <- list("clim.in"=clim.in,"cfname"=cfname,"long_name"=long_name,"units"=units)
  nc_close(nc.object)
  return(listout)
}

