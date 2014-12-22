# Aparna Radhakrishnan 08/04/2014
ReadNC <- function(nc.object,var.name=NA,dstart=NA,dcount=NA,dim='none',verbose=FALSE) {
  #'Reads data from netCDF file object
  #'Returns netCDF variable data
  #'uses get.var.ncdf
  #'Returned values will be in ordinary R double precision if the netCDF variable type is float or double. 
  #'Returned values will be in R's integer storage mode if the netCDF variable type is short or int. 
  #'Returned values will be of character type if the netCDF variable is of character type.
  #'[count]: A vector of integers indicating the count of values to read along each dimension (order is X-Y-Z-T). 
  #'[start]: A vector of indices indicating where to start reading the passed values (begin- ning at 1). 
  #'The length of this vector must equal the number of dimensions the variable has. Order is X-Y-Z-T 
  #'(i.e., the time dimension is last). If not specified, reading starts at the beginning of the file (1,1,1,...). 
  #'the 'dim' argument controls whether or not to look for dimensions and the variables that read off of those
  #'dimensions and add them to the output structure. 

  
  if((is.na(dstart)) && (is.na(dcount)) && (is.na(var.name))) {
  clim.in <- ncvar_get(nc.object)            
  }else {
    message('obtaining vars')
  clim.in <- ncvar_get(nc.object,var.name,dstart,dcount,collapse_degen=FALSE) 
    message('vars obtained')
  }
  #### get standard name,long name, units if present ####
  attname = 'standard_name'
  cfname <- ncatt_get(nc.object, var.name, attname) 
  attname = 'long_name'
  long_name <- ncatt_get(nc.object, var.name, attname)
  attname <- 'units' 
  units <- ncatt_get(nc.object, var.name, attname)
  #######################################################
  #Control getting the dimensions and other variables in the output file
  for(d in 1:length(dim)){
    temp.list <- switch(dim[d], 
                        "spatial"=get.space.vars(nc.object, var.name), 
                        "temporal"=get.time.vars(nc.object, var.name), 
                        #If other arg or "nothing", do nothing
                        list("dim"=list("none"), 'vars'=list("none"))
                        )
  }
  #######################################################
  listout <- list("clim.in"=clim.in,"cfname"=cfname,"long_name"=long_name,"units"=units, 
                  "dim"=temp.list$dim, 'vars'=temp.list$vars)
  #print("debug .......")
  #print(clim.in)
  
  ###Add attributes for later QC checking against each other
  attr(listout, "calendar") <- nc.object$dim$time$calendar
  attr(listout, "filename") <- nc.object$filename
  nc_close(nc.object)
  return(listout)
}

get.space.vars <- function(nc.object, var){
  #Obtains spatial vars, grid specs and all vars not the main var of interest
  #that depend upon those vars
  #Axes with spatial information
  axes <- c("X", "Y")
  file.axes <- nc.get.dim.axes(nc.object, var)
  if(is.null(file.axes)){
    stop(paste("Error in ReadNC: File", test.nc$filename, "has no variable", var, "; please examine your inputs."))
  }else{
    spat.axes <- file.axes[file.axes%in%axes]
    spat.vars <- names(spat.axes)
  }
  #Obtain any dimensions that reference space
  for (sd in 1:length(spat.vars)){
    ax <- spat.axes[[sd]]
    spat.dims[dim] <- nc.get.dim.for.axis(nc,object, var, ax)    
  }
  #Obtain any dimensions that are not time
  #Obtain any variables that do not reference time
  vars.present <- names(test.nc$var)[names(test.nc$var!=var)]
  for(i in 1:length(vars.present)){
    if(! ("time"%in%lapply(nc.object$var[vars.present]$dim, obtain.ncvar.dimnames))){
      
    }
  }
  return(list("dim"=spat.dims, "vars"=spat.vars))
}

get.time.vars <- function(nc.object, var){
  #Obtains time vars, calendar attributes and all vars that depend on time
  #that are not the main var of interest
  axes<- c("T")
}

obtain.ncvar.dimnames <- function(nc.obj){
  #obtains one of the names of the dimensions of a netcdf 
  #variable
  return(nc.obj[['name']])
}

