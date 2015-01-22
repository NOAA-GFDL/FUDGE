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
    stop(paste("Error in ReadNC: File", nc.object$filename, "has no variable", var, "; please examine your inputs."))
  }else{
    spat.axes <- file.axes[file.axes%in%axes]
    spat.varnames <- names(file.axes[file.axes%in%axes])
  }
  #Obtain any dimensions that reference space
  spat.dims <- list()
  for (sd in 1:length(spat.varnames)){
    ax <- spat.axes[[sd]]
    dim <- spat.varnames[[sd]]
    spat.dims[[dim]] <- nc.get.dim.for.axis(nc.object, var, ax)  
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
                      nc.object$var[[var.loop]]$missval, nc.object$var[[var.loop]]$prec)
      att.vector[5] <- paste(names(nc.object$dim)[(nc.object$var[[var.loop]]$dimids)+1], collapse=",")
      names(att.vector) <- c("units", "longname", "missval", "prec", "dimids")
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
                        nc.object$var[[var.loop]]$missval, nc.object$var[[var.loop]]$prec)
        att.vector[5] <- paste(names(nc.object$dim)[(nc.object$var[[var.loop]]$dimids)+1], collapse=",")
        names(att.vector) <- c("units", "longname", "missval", "prec", "dimids")
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