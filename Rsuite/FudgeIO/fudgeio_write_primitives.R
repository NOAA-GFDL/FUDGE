WriteNC <-  function(filename,out.data,var.names,
                     #xlon,ylat,time.index.start=NA, time.index.end=NA, downscale.tseries=NA, downscale.origin=NA,
                     dim.list,
                     prec='double', missval=1.e20, units="",
                     longname=var.names, standard_name=NA,
                     var.data=NA, 
                     modulo=NA, modulo.range="", comment=NA,
                     clone_globals=F, clone_all=F, global_clone_file=NA, 
                     clone_exclude=c(), clone_include=c(),
                     global_append=list(), global_overwrite=list(), #These are new - trying to include the simple global cloning function
                     verbose=FALSE) {
  
  #Define variable list and populate it
  var.dat <- list()
  
  #Under what circumstances do you need this? I don't know why the ens dims need it
  #     #Changed 3-10 to support writing ensemble dimensions
  #     true.dim <- unlist((lapply(dim.list, "[[", "len")))
  
  #Expected form of out.data is a named list with array elements that all have the 
  #same dimensions. 
  #print(mode(out.data))

  if ( !mode(out.data)=='list' ){
    test.data <- list()
    test.data[[var.names]] <- out.data
    out.data <- test.data
    rm(test.data)
    gc()
  }
  if(verbose){message('writing data variables')}
  if(length(units)==1){
    units <- rep(units, length(out.data))
  }
  for (loop in 1:length(out.data)){
    if(verbose) { message(paste("defining var", var.names[[loop]])) }
    var.dat[[var.names[loop]]] <- ncvar_def(var.names[loop],
                                            units=as.character(units[loop]), #needed if units are null
                                            dim.list,
                                            missval=missval,
                                            longname=longname[loop],
                                            prec=prec)#x,y,t1
  }
  
  #create a bounds dimension here, since that does not get used in any of the calculations above
  bnds <- ncdim_def(name="bnds", longname = 'bounds', units='', vals=seq(1:2), create_dimvar=FALSE)
  
  #the metadata vars associated with the dimensions
  #the metadata vars associated with the dimensions
  if(verbose){message("writing metadata variables")}
  if (length(var.data) > 0){
    metavars.present <- T
  }else{
    metavars.present <- F
  }
  
  if (metavars.present){
    for (v in 1:length(var.data)){
      var <- names(var.data)[v]
      if(verbose) { message(paste("defining var", var)) }
      dimlist <- list()
      dimnames <- var.data[[var]]$dimnames
      print(dimnames)
      if( !dimnames[1] == ""){
        for (el in dimnames){
          #If anything but bnds, get corresponding dim from the dim list;
          #otherwise, use newly-constructed bnds var
          dimlist[[el]] <- switch(as.character(el), 
                                  "bnds"=bnds, 
                                  dim.list[[as.character(el)]])
        }
      }
      var.dat[[var]] <- ncvar_def(var, #var.data[[v]], #Don't forget to add the data in somewhere 
                                  units=var.data[[var]]$units, #Add check for adding units back in if not present
                                  dim=dimlist, #It should be fine for this to be NULL.
                                  longname = var.data[[var]]$longname, 
                                  prec = var.data[[var]]$prec #This one gave me hives last time.
      )
    }
  }
  
  if(verbose) { message("creating nc objects") }
  #print(lapply(var.dat, 'summary'))
  nc.obj <- nc_create(filename, var.dat, verbose=FALSE)
  if(verbose) { message("placing nc vars") }
  for (loop in 1:length(out.data)){
    loop.varname <- names(out.data)[loop]
    if(verbose) { message(paste("populating", loop.varname)) }
    ncvar_put(nc=nc.obj, 
              varid=var.dat[[loop.varname]], 
              vals=out.data[[loop.varname]], verbose=FALSE)
  }
  if (metavars.present){
    for (v in 1:length(var.data)){
      loop.var <- names(var.data)[[v]]
      if(verbose) { message(paste("adding", loop.var, "and attributes")) }  
      ###This is the code that's giving buggy responses as of 12-16
      ncvar_put(nc.obj, var.dat[[loop.var]], var.data[[loop.var]]$vals, verbose=FALSE)
      #Note: failure to include missval as one of the attributes that  doesn't get set 
      #will result in giving all vars a missing value of 1e30. 
      all.att.list <- attributes(var.data[[loop.var]])
      all.att.list <- all.att.list[!(names(all.att.list)%in% c("units", "longname", "prec", "dim", "dimids", "missval", 'names'))]
      if(length(all.att.list) > 0){
        for (at in 1:length(all.att.list)){
          #print(paste(loop.var, ":", names(all.att.list)[[at]], "=", all.att.list[[at]]))
          ncatt_put(nc.obj, loop.var,  names(all.att.list)[[at]], all.att.list[[at]])
        }
      }
    }
  }
  if(verbose) { print("cloning attributes") }
  for (i in 1:length(dim.list)){
    dim.name <- names(dim.list)[i]
    #orig.file <- attr(dim.list[[i]], "filename")
    orig.file <- dim.list[[i]]$filename
    if(!is.null(orig.file)){
      orig.nc <- nc_open(orig.file)
      dim.name <- names(dim.list)[i]
      #This may not work if you name the arguments present. Check on that?
      nc.copy.atts(orig.nc, dim.name, nc.obj, dim.name, c("_FillValue", "FillValue", "_missval"))
      nc_close(orig.nc)
    }else{
      if(verbose) { print("null dim; skipping to next") }
    }
    dim.namelist <- c('time', 'lat', 'lon')
    if(dim.name%in%dim.namelist){
      #Link bounds variables to the corresponding dims; link axes
      #Cloning won't do this if you are creating your own time dimension
      # revising 2017-06-13 because it inserts bnds attributes when we 
      # do not have bnds vars present
      dim.axis <- switch(dim.name, 'time' = 'T', 'lat'='Y', 'lon'='X')
      bnds.name <- paste0(dim.name, "_bnds")
      #       print(bnds.name)
      #       print(names(var.data))
      #       print(bnds.name %in% names(var.data))
      if (bnds.name %in% names(var.data)){
        set_att_fun <- function(dim.name, dim.axis, bnds.name, nc.obj){
          ncatt_put(nc.obj, dim.name, 'axis', dim.axis)
          ncatt_put(nc.obj, dim.name, 'bounds', bnds.name)
          return(nc.obj)
        }
        nc.obj <- set_att_fun(dim.name, dim.axis, bnds.name, nc.obj)
      }
    }
    #stop('examine this output')
  }
  #clone most globals as well
  if (clone_globals){
    #...okay, I know this isn't all that good. But it saves passing a lot
    #of parameters, and I'm not sure about passing the file handler as part
    #of a function call.
    global_clone_function()
  }
  #Add a comment for each of the variables present?
  #CEW 9-16: I just know that this is used by one of the section 8 tools.
  #Wait to see what else it interferes with?
  #It's used by climdex - each var gets a comment
  if(!is.na(comment[[1]])){
    comment.vars <- names(comment)
    for(var in comment.vars){
      ncatt_put(nc.obj, var, "comments", comment[[var]])
    }
  }
  #Standard name
  #SdWriter, of all things, needs this to be in place
  if (!is.na(standard_name)){
    for (v in var.names){
      ncatt_put(nc.obj, v, 'standard_name', standard_name)
    }
  }
  #Climatology
  if(!is.na(modulo)){
    ncatt_put(nc.obj, "time", 'modulo', "") #there seems to be no good way to specify that this covers 365 days
    ncatt_put(nc.obj, "time", 'modulo_range', 
              modulo.range, prec="character")
  }
  if(length(global_append) > 0){
    if(verbose){message("Writing appending globals")}
    WriteSelectGlobals(nc.obj, global_append, append=T)
  }
  if(length(global_overwrite) > 0){
    if(verbose){message("Writing overwritten globals")}
    WriteSelectGlobals(nc.obj, global_overwrite, append=F)
  }
  #################  
  nc_close(nc.obj)
  return(filename)
}

################################################################################
global_clone_function <- function(env=parent.frame()){
  if(env$clone_all){
    #prepare to clone all atts from all variables cloned from input
    clone.var.list <- names(env$var.dat)
  }
  global.except.list <- c(c('initialization_method', 'physics_version', 'realization',
                            'product', 
                            'creation_date', 
                            'title', 'forcing', 'source',
                            'parent_experiment_id', 
                            'branch_time', 'modeling_realm', 
                            'table_id', 'tracking_id', 'parent_experiment_rip', 
                            "Institution", 'Institute_id', 'contact'), 
                          as.vector(env$clone_exclude))
  if(length(env$clone_include) > 0){
    global.except.list <- global.except.list[!(global.except.list %in% env$clone_include)]
  }
  nc.array <- lapply( env$global_clone_file, nc_open)
  nc.copy.and.combine.atts(nc.array, 0, env$nc.obj, 0, global.except.list, 
                           verbose=FALSE)
  if (!is.null(env$clone_all)){
    if(env$clone_all){
      for(v in env$clone.var.list){
        nc.copy.and.combine.atts(nc.array, v, env$nc.obj, v, c("_FillValue", "FillValue", "_missval", 'missing_value', 'units'),
                                 verbose=FALSE)
      }
    }
  }
  return(env)
}