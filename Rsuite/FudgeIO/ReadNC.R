# Aparna Radhakrishnan 08/04/2014
ReadNC <- function(nc.object,var.name=NA,dstart=NA,dcount=NA,dim='none',verbose=FALSE, force_3_dimensions=FALSE) {
  #'Reads data from a variable of a netCDF file object
  #'Returns netCDF variable data and, if dim!= 'none', one or more dimensions of that
  #'netCDF object
  #'-----Arguments-----
  #'@param nc.object: A NetCDF object returned from nc_open of R's ncdf4 package
  #'@param var.name: The name of the variable within the NetCDf file
  #'@param dstart: A vector of indices indicating where to start reading the passed values (begin- ning at 1).
  #'@param dcount: A vector of integers indicating the count of values to read along the variable (order is X-Y-Z-T).
  #'@param dim: Which dimensions to include in the netCDF object. Can be one of 'temporal', 
  #'which queries the dimension associated with the T axis, 'spatial', which queries the dimensions associated with the
  #'X, Y and Z axes, and 'none', which makes no query. The dimensions queried also affect the other variables returned;
  #''temporal' returns all variables that reference the T axis underneath the $vars list of the output, and 
  #'
  #'@returns A list containing the following elements: 
  #'  $clim.in: A 3-dimensional array of the values in the variable with the same dimensions
  #'      as the input dataset. Returned values will be in ordinary R double precision if the netCDF 
  #'      variable type is float or double. Has a 'filename' attribute that points to the input file, 
  #'      which is used for error messages. If dim='temporal', then it also has a calendar attribute.
  #'  $dim: The dimension(s) queried in the file, of class ncdim4. Very useful for writing to file. 
  #'  $vars: The variables *not* the variable of interest that use the dimensions queried 
  #'      (i.e. lat_bnds, j_offset, height).
  #'  $cfname: The CF-standard name of the variable. Only applies to tasmax, tasmin and pr.
  #'  $long_name: The long name of the variable. Derived from the long_name attribute of the netCDF object.
  #'  $units: The units of the variable of interest. Derived from the units attribute of the netCDF object.
  #'
  #'@include ncdf4, ncdf4.helpers

  
  if((is.na(dstart)) && (is.na(dcount)) && (is.na(var.name))) {
  clim.in <- ncvar_get(nc.object)            
  }else {
    message('obtaining variable slice')
  clim.in <- ncvar_get(nc.object,var.name,dstart,dcount,collapse_degen=FALSE) 
    message('vars obtained')
  }
    #Fudge currently deals badly with input vectors of more than three dimensions - and the first two
    #have to be spatial. This will collapse any OTHER degenerate dimensions present.
    #In the future, this will NOT apply to ensemble dimensions, but we are not there yet by any means.
    if(length(dim(clim.in)) > 3 && force_3_dimensions){
      message("Adjusting dimensions to fit the FUDGE framework")
      #Last dim is always time, first two should always be X and Y
      dim.adjust <- dim(clim.in)[2:length(dim(clim.in))-1]
      if(sum(dim.adjust!=1)!=0){ 
        #If there are any non-degenerate dimensions, throw an error
        #NOTE: This is not meant to be used with ensemble dims - not yet, anyway
        stop(paste("Error in ReadNC: The file", attr(nc.object, "filename"), "had a var that could not be adjusted to an x,y,t dimension system", 
                   "due to one or more non-degenerate dimensions. Please examine the input file and try again."))
      }else{
        #Redimension as (x,y,t)
        dim(clim.in) <- c(dim(clim.in[1:2]), dim(clim.in)[length(clim.in)])
      }
    }
  #### get standard name,long name, units if present ####
  attname = 'standard_name'
  cfname <- ncatt_get(nc.object, var.name, attname) 
  attname = 'long_name'
  long_name <- ncatt_get(nc.object, var.name, attname)
  attname <- 'units' 
  units <- ncatt_get(nc.object, var.name, attname)
    attr(clim.in, "units") <- units
  ###Test code for determining what happens for unitless vars
  #######################################################
  #Control getting the dimensions and other variables in the output file
  dim.list <- list(dim=list(), vars=list())
  for(d in 1:length(dim)){
    temp.list <- switch(dim[d], 
                        "spatial"=get.space.vars(nc.object, var.name), 
                        "temporal"=get.time.vars(nc.object, var.name), 
                        "ensemble" = get.ens.dim(nc.object, var.name),
                        #If other arg or "nothing", do nothing
                        list("dim"=list("none"), 'vars'=list("none"))
                        )
    dim.list$dim <- c(dim.list$dim, temp.list$dim)
    dim.list$vars <- c(dim.list$vars, temp.list$vars)
    print(names(dim.list$dim))
  }
  #######################################################
  listout <- list("clim.in"=clim.in,"cfname"=cfname,"long_name"=long_name,"units"=units, 
                  "dim"=dim.list$dim, 'vars'=dim.list$vars)
  
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
  message('getting spatial vars')
  axes <- c("X", "Y", "Z")
  file.axes <- nc.get.dim.axes(nc.object, var)
  if(is.null(file.axes)){
    stop(paste("Error in ReadNC: File", nc.object$filename, "has no variable", var, "; please examine your inputs."))
  }else{
    print("Obtaining axis", )
    spat.axes <- file.axes[file.axes%in%axes] #Here is where the extra Z dim check comes in
    spat.varnames <- names(file.axes[file.axes%in%axes])
  }
  #Obtain any dimensions that reference space
  spat.dims <- list()
  for (sd in 1:length(spat.varnames)){
    ax <- spat.axes[[sd]]
    dim <- spat.varnames[[sd]]
    spat.dims[[dim]] <- nc.get.dim.for.axis(nc.object, var, ax)
    #Make sure that original file is being included, in order to support attribute cloning
    attr(spat.dims[[dim]], "filename") <- attr(nc.object, "filename")
  }
  #Obtain any dimensions that are not time
  #Obtain any variables that do not reference time
  #THIS is the bit that was tripping you up last time. deal with it, please.
  vars.present <- names(nc.object$var)[names(nc.object$var)!=var]
  spat.vars <- list()
  for(i in 1:length(vars.present)){
    var.loop <- vars.present[i]
    if(! ("time"%in%lapply(nc.object$var[[var.loop]]$dim, obtain.ncvar.dimnames))){
      spat.vars[[var.loop]] <- ncvar_get(nc.object, var.loop, collapse_degen=FALSE)
      #Grab the bits used to build the vars later
      att.vector <- c(nc.object$var[[var.loop]]$units, nc.object$var[[var.loop]]$longname, 
                      #nc.object$var[[var.loop]]$missval, 
                      nc.object$var[[var.loop]]$prec)
      att.vector[4] <- paste(names(nc.object$dim)[(nc.object$var[[var.loop]]$dimids)+1], collapse=",") #formerly 5
      names(att.vector) <- c("units", "longname", "prec", "dimids") #"missval", 
      att.vector[att.vector=='int'] <- "integer"
      for (a in 1:length(att.vector)){
        attr(spat.vars[[var.loop]], which=names(att.vector)[[a]]) <- att.vector[[a]]
      }
      #And finally, grab the comments attribute, which is important
      #for i and j offsets (but not much else)
      comments <- ncatt_get(nc.object, var.loop, 'comments')
      if(comments$hasatt){
        attr(spat.vars[[var.loop]], which='comments') <- comments$value
      }
    }
  }
  return(list("dim"=spat.dims, "vars"=spat.vars))
}

get.time.vars <- function(nc.object, var){
  #Obtains time vars, calendar attributes and all vars that depend on time
  #that are not the main var of interest
  message('getting time vars')
  axes<- c("T")
  file.axes <- nc.get.dim.axes(nc.object, var)
  if(is.null(file.axes)){
    stop(paste("Error in ReadNC: File", nc.object$filename, "has no variable", var, "; please examine your inputs."))
  }else{
    time.axes <- file.axes[file.axes%in%axes]
    time.varnames <- names(file.axes[file.axes%in%axes])
  }
  #Obtain any dimensions that reference time
  time.dims <- list()
  for (td in 1:length(time.varnames)){
    ax <- time.axes[[td]]
    dim <- time.varnames[[td]]
    time.dims[[dim]] <- nc.get.dim.for.axis(nc.object, var, ax)
    #Make sure that original file is being included, in order to support attribute cloning
    attr(time.dims[[dim]], "filename") <- attr(nc.object, "filename")
  }
  #Obtain any dimensions that are not time
  #Obtain any variables that do not reference time
  #THIS is the bit that was tripping you up last time. deal with it, please.
  if(length(time.varnames > 1)){
    vars.present <- names(nc.object$var)[names(nc.object$var)!=var]
    time.vars <- list()
    for(i in 1:length(vars.present)){
      var.loop <- vars.present[i]
      #Obtain all vars that have a dim named 'time'
      if( "time"%in%lapply(nc.object$var[[var.loop]]$dim, obtain.ncvar.dimnames) ){
        time.vars[[var.loop]] <- ncvar_get(nc.object, var.loop, collapse_degen=FALSE)
        #Grab bits needed to construct vars later; store as attributes
        att.vector <- c(nc.object$var[[var.loop]]$units, nc.object$var[[var.loop]]$longname, 
                        #nc.object$var[[var.loop]]$missval, 
                        nc.object$var[[var.loop]]$prec)
        att.vector[4] <- paste(names(nc.object$dim)[(nc.object$var[[var.loop]]$dimids)+1], collapse=",") #formerly 
        names(att.vector) <- c("units", "longname", "prec", "dimids") #"missval", 
        att.vector[att.vector=='int'] <- "integer"
        for (a in 1:length(att.vector)){
          attr(time.vars[[var.loop]], which=names(att.vector)[[a]]) <- att.vector[[a]]
        }
        #And finally, grab the comments attribute, which is important
        #for i and j offsets (but not much else)
        comments <- ncatt_get(nc.object, var.loop, 'comments')
        if(comments$hasatt){
          attr(time.vars[[var.loop]], which='comments') <- comments$value
        }
      }
    }
  }else{
    message("No variables but the main variable found using time dimension; continue on.")
    time.dims[[dim]]
  }
  return(list("dim"=time.dims, "vars"=time.vars))
}

obtain.ncvar.dimnames <- function(nc.obj){
  #obtains one of the names of the dimensions of a netcdf 
  #variable
  return(nc.obj[['name']])
}

get.ens.dim <- function(nc.obj, var.name){
  message('getting ensemble dimension, if present')
  ens.dim.name <- "ensmem"
  if(ens.dim.name%in%names(nc.obj$dim)){
    print("ensmem present")
    ensmem=nc.obj$dim$ensmem
    attr(ensmem, "filename") <- attr(nc.obj, "filename")
    return(list(dim=list(ensmem=ensmem), vars=list()))
  }else{
    print("no dim named ensmem present")
    return(list(dim=list(), vars=list()))
  }
}