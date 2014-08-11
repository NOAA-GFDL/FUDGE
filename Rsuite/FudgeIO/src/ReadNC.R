ReadNC <- function(nc.object,var.name=NA,dstart=NA,dcount=NA,verbose=FALSE) {
  #'Reads data from netCDF file object
  #'Returns netCDF variable data
  #'uses get.var.ncdf
  #'Returned values will be in ordinary R double precision if the netCDF variable type is float or double. Returned values will be in R's integer storage mode if the netCDF variable type is short or int. Returned values will be of character type if the netCDF variable is of character type.
  #'[count]: A vector of integers indicating the count of values to read along each dimen- sion (order is X-Y-Z-T). 
  #'[start]: A vector of indices indicating where to start reading the passed values (begin- ning at 1). The length of this vector must equal the number of dimensions the variable has. Order is X-Y-Z-T (i.e., the time dimension is last). If not specified, reading starts at the beginning of the file (1,1,1,...). 
  if((is.na(dstart)) && (is.na(dcount)) && (is.na(var.name))) {
  print("check 1")
  read.nc <- ncvar_get(nc.object)            
  }else {
  print("check 2")
  print(dcount)
  var.read <- ncvar_get(nc.object,var.name,dstart,dcount) 
  }
#  nc_close(nc.object) #Creates an error when called again if left uncommented
  return(var.read)
}

