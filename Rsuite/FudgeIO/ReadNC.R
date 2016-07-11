# Aparna Radhakrishnan 08/04/2014
ReadNC <- function(nc.object,var.name=NA,dstart=NA,dcount=NA,dim='none',
                   is.dimvar=FALSE,
                   verbose=FALSE, add.ens.dim=FALSE, force_4_dim=TRUE, 
                   close_nc=TRUE) {
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
  #'  $dim: The dimension(s) queried in the file, of class ncdim4. Very useful for writing to file; 
  #'  as of 3-10, now REQUIRED for writing to file. 
  #'  $vars: The variables *not* the variable of interest that use the dimensions queried 
  #'      (i.e. lat_bnds, j_offset, height).
  #'  $cfname: The CF-standard name of the variable. Only applies to tasmax, tasmin and pr; for all other
  #'  vars, the short name is the cfname
  #'  $long_name: The long name of the variable. Derived from the long_name vale of the netCDF object.
  #'  $units: The units of the variable of interest. Derived from the units value of the netCDF object.
  #'
  #'@include ncdf4, ncdf4.helpers

  att_list <- list('var' = var.name, 
                   'rip' = "", #Leave a blank space; I think this is one of the things that gets filled in later
                   'units'=ncatt_get(nc.object, var.name, 'units')$value, 
                   'standard_name'=ncatt_get(nc.object, var.name, 'standard_name')$value,
                   'long_name'=ncatt_get(nc.object, var.name, 'long_name')$value,
                   'prec'=nc.object$var[[var.name]]$prec, 
                   'climatology' = ncatt_get(nc.object, 0, 'climatology')$value
  )
  #Correct prec after the fact
  if(is.null(att_list$prec)){
    att_list$prec <- 'float'
  }
  if(is.null(att_list$climatology)){
    att_list$climatology <- NA
  }

  if((is.na(dstart)) && (is.na(dcount)) && (is.na(var.name))) {
  clim.in <- ncvar_get(nc.object)            
  }else {
    if(verbose){ message('obtaining variable slice') }
  clim.in <- ncvar_get(nc.object,var.name,dstart,dcount,collapse_degen=FALSE) 
    if(verbose) { message('vars obtained') }
  }
    #Fudge currently deals badly with input vectors of more than three dimensions - and the first two
    #have to be spatial. This will collapse any OTHER degenerate dimensions present.
    #In the future, this will NOT apply to ensemble dimensions, but we are not there yet by any means.
    #if(length(dim(clim.in)) > 3){
    if( force_4_dim){
      if(verbose) { message("Adjusting dimensions to fit the FUDGE framework") }
      if(length(dim(clim.in)) < 4){
        message('correcting to 4-dimensional output')
        #Determine dimensions present
        dim.names <- c('lon','lat','ensmem','time')
        check.dimlen <- function(dim.name,nc.obj){
          if(!is.null(nc.obj$dim[[dim.name]])){
            return(nc.obj$dim[[dim.name]][['len']])
          }else{
            return(1)
          }}
        true.dims <- as.vector(unlist(sapply(dim.names,'check.dimlen',nc.object)))
        print(true.dims)
        dim(clim.in) <- true.dims
      }
    }else if(add.ens.dim){
      dim.adjust <- c(dim(clim.in)[1:2], 1, dim(clim.in)[3])
      dim(clim.in) <- dim.adjust
    }
  #### get standard name,long name, units if present ####
  attname = 'standard_name'
  cfname <- ncatt_get(nc.object, var.name, attname) 
  attname = 'long_name'
  long_name <- ncatt_get(nc.object, var.name, attname)
  attname <- 'units' 
  units <- ncatt_get(nc.object, var.name, attname)
  if(units$hasatt==F){
    units <- list(hastatt=F, value="")
  }
  attr(clim.in, "units") <- units
  prec <- nc.object$var[[var.name]]$prec
  if(is.null(prec)){
    #set prec to float if not present in input file
    prec <- 'float'
  }
  attr(clim.in, "prec") <- prec
  ###Test code for determining what happens for unitless vars
  #######################################################
  #Control getting the dimensions and other variables in the output file
    dim.list <- list(dim=list(), vars=list())
    if(!('none'%in%dim)){
      #Needed to make sure that the dimensions are always
      #queried in the correct order (x, y, ens, time)
      order.vector <- c('spatial',  'ensemble', 'temporal')
      dim <- order.vector[order.vector%in%dim]
      for(d in 1:length(dim)){
        temp.list <- switch(dim[d], 
                            "spatial"=get.space.vars(nc.object, var.name, is.dimvar, verbose=verbose), 
                            "temporal"=get.time.vars(nc.object, var.name, is.dimvar, verbose=verbose), 
                            "ensemble" = get.ens.dim(nc.object, var.name, verbose=verbose),
                            #If other arg or "nothing", do nothing
                            list("dim"=list("none"), 'vars'=list("none"))
        )
        if(verbose){print(paste(dim[d], 'dim and vars obtained'))}
        dim.list$dim <- c(dim.list$dim, temp.list$dim)
        dim.list$vars <- c(dim.list$vars, temp.list$vars)
      }
    }else{
      dim.list$dim <- c("dim" = list('none'))
      dim.list$vars <- c("vars" = list('none'))
    }
    #print('all checks complete')
    #print(names(dim.list$dim))
    #print(names(dim.list$vars))
  #######################################################
    envout =  new.env(parent = emptyenv())
    envout$clim.in = clim.in
    envout$cfname=cfname
    envout$long_name=long_name
    envout$units=units
    envout$dim=dim.list$dim
    envout$vars=dim.list$vars
  envout$att_table <- att_list
  
#  listout <- list("clim.in"=clim.in,"cfname"=cfname,"long_name"=long_name,"units"=units, 
#                  "dim"=dim.list$dim, 'vars'=dim.list$vars)
  
#   ###Add attributes for later QC checking against each other
#   attr(listout, "calendar") <- nc.object$dim$time$calendar
#   attr(listout, "filename") <- nc.object$filename
    ###Add attributes for later QC checking against each other
    attr(envout, "calendar") <- nc.object$dim$time$calendar
    attr(envout, "filename") <- nc.object$filename
    if(close_nc){
      nc_close(nc.object)
    }
  return(envout)
}

get.space.vars <- function(nc.object, var, is.dimvar=FALSE, verbose=FALSE){
  #Obtains spatial vars, grid specs and all vars not the main var of interest
  #that depend upon those vars
  #Axes with spatial information
  if(verbose) { message('getting spatial vars') }
  axes <- c("X", "Y", "Z")
  file.axes <- nc.get.dim.axes(nc.object, var)
  if(is.null(file.axes)){
    if(!is.dimvar){
      stop(paste("Error in ReadNC: File", nc.object$filename, "has no variable", 
                 var, "referencing spatial dimensions; please examine your inputs."))
    }else{
      message("Checking for var ", var, ' as a dimension variable')
      spat.axes <- axes
      file.varnames <- 'height'
    }
  }else{
    if(verbose) { print("Obtaining axis", ) }
    spat.axes <- file.axes[file.axes%in%axes] #Here is where the extra Z dim check comes in
    spat.varnames <- names(file.axes[file.axes%in%axes])
  }
  if(length(spat.axes)==0){
    message("No spatial axes present for this var")
    return(list("dim"=NULL, "vars"=NULL))
  }
  #Obtain any dimensions that reference space
  spat.dims <- list()
  if(!is.dimvar){
    for (sd in 1:length(spat.varnames)){
      ax <- spat.axes[[sd]]
      dim <- spat.varnames[[sd]]
      spat.dims[[dim]] <- nc.get.dim.for.axis(nc.object, var, ax)
      #Make sure that original file is being included, in order to support attribute cloning
      if(verbose) { print(paste("writing filename attribute", nc.object$filename)) }
      attr(spat.dims[[dim]], "filename") <- nc.object$filename
    }
  }else{
    spat.dimnames <- names(nc.object$dim[!(names(nc.object$dim)%in%c('time','bnds'))])
    for(d in spat.dimnames){
      spat.dims[[d]] <- nc.object$dim[[d]]
      if(verbose) { print(paste("writing filename attribute", nc.object$filename)) }
      attr(spat.dims[[d]], "filename") <- nc.object$filename
    }
  }
  #Obtain any dimensions that are not time
  #Obtain any variables that do not reference time
  #THIS is the bit that was tripping you up last time. deal with it, please.
  vars.present <- names(nc.object$var)[names(nc.object$var)!=var]
  if(is.na(vars.present[[1]])){
    vars.present <- list()
  }
  spat.vars <- list()
  for(i in 1:length(vars.present)){
    var.loop <- vars.present[i]
    #This is the code that is meant to keep anything referencing time from being added
    if(! ("time"%in%lapply(nc.object$var[[var.loop]]$dim, obtain.ncvar.dimnames))){
      spat.vars[[var.loop]] <- ncvar_get(nc.object, var.loop, collapse_degen=FALSE)
      #Grab the bits used to build the vars later
      att.vector <- c(nc.object$var[[var.loop]]$units, nc.object$var[[var.loop]]$longname, 
                      nc.object$var[[var.loop]]$missval, 
                      nc.object$var[[var.loop]]$prec)
      att.vector[5] <- paste(names(nc.object$dim)[(nc.object$var[[var.loop]]$dimids)+1], collapse=",") #previously 5, with missval
      #att.vector[6] <- 'spatial'
      names(att.vector) <- c("units", "longname", "missval", 
                             "prec", "dimids") #"var_type"
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
    #Finally, make sure that you associate the axe with the respective dins and vars
    #you know what? That might not be needed.
  }
  return(list("dim"=spat.dims, "vars"=spat.vars))
}

get.time.vars <- function(nc.object, var, is.dimvar=FALSE, verbose=FALSE){
  #Obtains time vars, calendar attributes and all vars that depend on time
  #that are not the main var of interest
  if(verbose) { message('getting time vars') }
  axes<- c("T")
  file.axes <- nc.get.dim.axes(nc.object, var)
  if(is.null(file.axes)){
    if(!is.dimvar){
      stop(paste("Error in ReadNC: File", nc.object$filename, 
                 "has no variable",
                 var, "referencing time dims; please examine your inputs."))
    }else{
      message("Checking for var ", var, ' as a dimension variable')
      time.axes <- axes
      time.varnames <- "time"
    }
  }else{
    time.axes <- file.axes[file.axes%in%axes]
    time.varnames <- names(file.axes[file.axes%in%axes])
  }
  if(length(time.axes)==0){
    #message("No time axes present for this var")
    message("No time axes present for this var; obtaining dummy dimension")
    dummy.dim <- ObtainTimeAxis(key=1)
   # print(summary(dummy.dim))
   # stop('examine output')
    #return(ObtainTimeAxis(key=1))
    return(list("dim"=list('time'=dummy.dim$dim), "vars"=list('time_bnds'=dummy.dim$vars[[1]])))
  }
  #Obtain any dimensions that reference time
  time.dims <- list()
  if(!is.dimvar){
    for (td in 1:length(time.varnames)){
      ax <- time.axes[[td]]
      dim <- time.varnames[[td]]
      time.dims[[dim]] <- nc.get.dim.for.axis(nc.object, var, ax)
      mod.att <- ncatt_get(nc.object, dim, 'modulo')
      if (mod.att$hasatt){
        attr(time.dims[[dim]], 'modulo') <- mod.att$value
      }
      #Make sure that original file is being included, in order to support attribute cloning
      attr(time.dims[[dim]], "filename") <- nc.object$filename
    }
  }else{
    #Simple case for stealing dimensions for cloning
    time.dims[['time']] <- nc.object$dim[['time']]
    mod.att <- ncatt_get(nc.object, 'time', 'modulo')
    if (mod.att$hasatt){
      attr(time.dims[['time']], 'modulo') <- mod.att$value
    }
    attr(time.dims[['time']], "filename") <- nc.object$filename
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
      #Edited 4-17 to make sure that it avoids dims that also refrence space
      if( "time"%in%lapply(nc.object$var[[var.loop]]$dim, obtain.ncvar.dimnames) ){
        if(!("lon"%in%lapply(nc.object$var[[var.loop]]$dim, obtain.ncvar.dimnames)) &&
             !("lat"%in%lapply(nc.object$var[[var.loop]]$dim, obtain.ncvar.dimnames))){
          time.vars[[var.loop]] <- ncvar_get(nc.object, var.loop, collapse_degen=FALSE)
          #Grab bits needed to construct vars later; store as attributes
          att.vector <- c(nc.object$var[[var.loop]]$units, nc.object$var[[var.loop]]$longname, 
                          nc.object$var[[var.loop]]$missval, 
                          nc.object$var[[var.loop]]$prec)
          att.vector[5] <- paste(names(nc.object$dim)[(nc.object$var[[var.loop]]$dimids)+1], collapse=",") #previously 5 with missval
          #Added for indexing spatial and temporal vars for writing
          #att.vector[6] <- 'temporal'
          names(att.vector) <- c("units", "longname", "missval", 
                                 "prec", "dimids")
          att.vector[att.vector=='int'] <- "integer"
          for (a in 1:length(att.vector)){
            attr(time.vars[[var.loop]], which=names(att.vector)[[a]]) <- att.vector[[a]]
          }
          #Grab the missing value if and only if the 
          #And finally, grab the comments attribute, which is important
          #for i and j offsets (but not much else)
          comments <- ncatt_get(nc.object, var.loop, 'comments')
          if(comments$hasatt){
            attr(time.vars[[var.loop]], which='comments') <- comments$value
          }
        }
        #3-20: does this fix the att copying issue?
        #attr(time.vars[[var.loop]], 'filename') <- nc.object$filename
      }
    }
  }else{
    if(verbose) { message("No variables but the main variable found using time dimension; continue on.") }
    time.dims[[dim]]
  }
  return(list("dim"=time.dims, "vars"=time.vars))
}

obtain.ncvar.dimnames <- function(nc.obj){
  #obtains one of the names of the dimensions of a netcdf 
  #variable
  return(nc.obj[['name']])
}

get.ens.dim <- function(nc.obj, var.name, verbose=FALSE){
  if(verbose) { message('getting ensemble dimension, if present') }
  ens.dim.name <- "ensmem"
  if(ens.dim.name%in%names(nc.obj$dim)){
    ensmem=nc.obj$dim$ensmem
    attr(ensmem, "filename") <- attr(nc.obj, "filename")
    return(list(dim=list(ensmem=ensmem), vars=list()))
  }else{
    return(list(dim=list(), vars=list()))
  }
}