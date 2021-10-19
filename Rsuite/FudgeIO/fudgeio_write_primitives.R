# Aparna Radhakrishnan 08/04/2014
WriteNC <-  function(filename,out.data,var.names,
                     #xlon,ylat,time.index.start=NA, time.index.end=NA, downscale.tseries=NA, downscale.origin=NA,
                     dim.list,
                     prec='double', missval=1.e20, units="",
                     longname=var.names, standard_name=NA,
                     var.data=NA, 
                     modulo=NA, modulo.range="", comment=NA,
                     clone_globals=F, clone_all=F, global_clone_file=NA, 
                     clone_data_vars=F,
                     clone_exclude=c(), clone_include=c(),
                     global_append=list(), global_overwrite=list(), #These are new - trying to include the simple global cloning function
                     var_attributes = list(),
                     multiple_var_specs = F, all.dimlist = list(),
                     clone_missvals=FALSE,
                     verbose=FALSE) {
  
  #Define variable list and populate it
  var.dat <- list()
  
  #2020-02-14: adding checks on writing directly to /archive
  if (grepl("^/archive", filename, perl=T)){
    stop(paste("Error in WriteNC: file", filename, 
               "had a path that started with /archive.", 
               "Please write to an intermediate location (like $TMP)", 
               "and then copy to /archive!"))
  }
  
  #Under what circumstances do you need this? I don't know why the ens dims need it
  #     #Changed 3-10 to support writing ensemble dimensions
  #     true.dim <- unlist((lapply(dim.list, "[[", "len")))
  
  #Expected form of out.data is a named list with array elements that all have the 
  #same dimensions. 
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
  if (length(prec)==1){
    prec <- rep(prec, length(out.data))
  }
  #Check to make sure that dims are not of length 0
  if (length(dim.list) == 0){
    stop(paste("Error in WriteNC: dim.list was of length 0!", 
               "This will keep you from writing data!"))
  }
  #If there is not more than one list of vars, rep to simplify looping
  if (!multiple_var_specs){
    tmp.dimlist <- rep(list(dim.list), length(out.data))
  }else{
    #Make sure that the list of dimensions is of the same length
    #as the list of variables to write
    if (!length(out.data)==length(dim.list)){
      stop(paste("Error in WriteNC! If multiple_var_specs, length of out.list and dim.list must agree!"))
    }else{
      #Make sure that you have 
      tmp.dimlist <- dim.list
      #max.dims <- which.max(sapply(dim.list, 'length'))
      #dim.list <- tmp.dimlist[[max.dims]]
      ##Okay, for PCA none of these are going to have the whole story
      dim.list <- all.dimlist
    }
  }
  loop_missvals <- list()
  #FINALLY get to the variable writing process
  for (loop in 1:length(out.data)){
    if(verbose) { message(paste("defining var", var.names[[loop]])) }
    if (prec[loop] == 'char'){
      loop_miss = ' '
    }else if (prec[loop] == 'integer'){
      loop_miss = as.integer((-2) * 10^9)
    }else{
      loop_miss = missval
    }
    loop_missvals[[var.names[loop]]] <- loop_miss
    var.dat[[var.names[loop]]] <- ncvar_def(var.names[loop],
                                            units=as.character(units[loop]), #needed if units are null
                                            tmp.dimlist[[loop]],
                                            missval=loop_miss,
                                            longname=longname[loop],
                                            prec=prec[loop])#x,y,t1
  }
  
  #create a bounds dimension here, since that does not get used in any of the calculations above
  bnds <- ncdim_def(name="bnds", longname = 'bounds', units='', vals=seq(1:2), create_dimvar=FALSE)
  
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
      if(verbose) { message(paste("defining metavar", var)) }
      dimlist <- list()
      dimnames <- var.data[[var]]$dimnames
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
  nc.obj <- nc_create(filename, var.dat, verbose=F)
  if(verbose) { message("placing nc vars") }
  for (loop in 1:length(out.data)){
    loop.varname <- names(out.data)[loop]
    if(verbose) { message(paste("populating", loop.varname)) }
    ## Added 2017-08-31 - Nan should be NA
    out.data[[loop.varname]][is.nan(out.data[[loop.varname]])] <- NA
    ## Added 2018-06-29 - infinite values shouldn't be written b/c they
    ## can't be converted
    if ( any(is.infinite(out.data[[loop.varname]])) ){
      warning(paste("WARNING IN WriteNC: in the variable", loop.varname, 
                    "one or more values were infinite!"))
      #warning("replacing infinte values with largest/smallest possible integer")
      out.data[[loop.varname]][is.infinite(out.data[[loop.varname]])] <- NA
    }
    var_len <- length(out.data[[loop.varname]])
    max_vector <- 2^31 - 1 
    #technically, max_vector is 2^31 - 1; however, lower setting speeds some writes
    if (var_len < max_vector){
      ncvar_put(nc=nc.obj, 
                varid=var.dat[[loop.varname]], 
                vals=out.data[[loop.varname]], verbose=FALSE)
      #2020-08: missing_value should be defined in addition to _FillValue; 
      #this is a hack (and not properly documented in the package)
      #but it works.
      ncvar_change_missval(nc.obj, var.dat[[loop.varname]], loop_missvals[[loop.varname]])
    }else{
      print("Support for writing large data to file is currently turned off.")
      print("If you need to write large data, please talk to Dennis or Carolyn.")
      stop(paste("Size of data", paste(dim(out.data[[loop.varname]])),
                 "was larger than R's max integer size"))
    }
    if (verbose) print("placing calculation data complete.")
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
          ncatt_put(nc.obj, loop.var,  names(all.att.list)[[at]], all.att.list[[at]])
        }
      }
    }
  }
  #2020-08: missing value changed from input attribute; 
  #see fudgeio_primitives:obtain.fudge.var.metadata
  if (exists("missval_input_changed", where=.GlobalEnv)){
    if (missval_input_changed){
      ncatt_put(nc.obj, 0, "any_missing_fill_changed","TRUE")
    }
  }
  if(verbose) { print("cloning attributes") }
  for (i in 1:length(dim.list)){
    dim.name <- names(dim.list)[i]
    if(verbose) {print(dim.name)}
    #orig.file <- attr(dim.list[[i]], "filename")
    orig.file <- dim.list[[i]]$filename
    if(!is.null(orig.file)){
      orig.nc <- nc_open(orig.file)
      dim.name <- names(dim.list)[i]
      #This may not work if you name the arguments present. Check on that?
      if (orig.nc$dim[[dim.name]]$create_dimvar){
        print(paste(dim.name, "is a var; cloning atts"))
        nc.copy.atts(orig.nc, dim.name, nc.obj, dim.name, c("_FillValue", "FillValue", "_missval"))
      }
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
      if (bnds.name %in% names(var.data)){
        set_att_fun <- function(dim.name, dim.axis, bnds.name, nc.obj){
          ncatt_put(nc.obj, dim.name, 'axis', dim.axis)
          ncatt_put(nc.obj, dim.name, 'bounds', bnds.name)
          return(nc.obj)
        }
        nc.obj <- set_att_fun(dim.name, dim.axis, bnds.name, nc.obj)
      }
    }
  }
  #clone most globals as well
  if (clone_globals){
    #...okay, I know this isn't all that good. But it saves passing a lot
    #of parameters, and I'm not sure about passing the file handler as part
    #of a function call.
    clone.missing.value.defs = clone_missvals #do we clone missing values for vars from the input file?
    global_clone_function()
    gc()
  }
  #Add a comment for each of the variables present?
  #CEW 9-16: I just know that this is used by one of the section 8 tools.
  #Wait to see what else it interferes with?
  #It's used by climdex - each var gets a comment
  if(!is.na(comment[[1]])){
    comment.vars <- names(comment)
    for(var in comment.vars){
      ncatt_put(nc.obj, var, "comment", comment[[var]])
    }
  }
  #Standard name
  #SdWriter, of all things, needs this to be in place
  if (!is.na(standard_name[[1]])){
    for (v in var.names){
      ncatt_put(nc.obj, v, 'standard_name', standard_name)
    }
  }
  #Climatology
  if(!is.na(modulo[[1]])){
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
  #2021-09: adding the var_attributes from the input files
  if (length(var_attributes) > 0){
    for (v in names(var_attributes)){
      vstruct = var_attributes[[v]]
      for (a in names(vstruct)){
        #print(a)
        ncatt_put(nc.obj, v, a, vstruct[[a]])
      }
    }
  }
  ##2020-08: adding the last_modified date for the tool
  #explicit error in FUDGE; not needed
  #ncatt_put(nc.obj, 0, "tool_version", moddate(), prec='character')
  #################  
  nc_close(nc.obj)
  ##2021 - adding compression settings via nccopy
  #is_compress <- TRUE
  #compress_factor <- 4
  #compress_cmd <- paste("nccopy -d", compress_factor, filename, filename)
  #system(compress_cmd)
  return(filename)
}

################################################################################
global_clone_function <- function(env=parent.frame()){
  #parent.frame()
#   print(ls())
#   print(env$clone_include)
#   stop('examine output of environment')
  clone.var.list <- c()
  if(env$clone_all){
    #prepare to clone all atts from all variables cloned from input
    clone.var.list <- names(env$var.dat)
  }
  get.global.except.atts()
  global.except.list <- c(global.except.list, 
                          as.vector(env$clone_exclude))
  if(length(env$clone_include) > 0){
    if (!('all' %in% env$clone_include)){
    global.except.list <- global.except.list[!(global.except.list %in% env$clone_include)]
    }else{
      global.except.list <- c()
    }
  }
  nc.array <- lapply( env$global_clone_file, nc_open)
  nc.copy.and.combine.atts(nc.array, 0, env$nc.obj, 0, global.except.list, 
                           verbose=FALSE)
  nco_cmd_list <- c()
  if (!is.null(env$clone_all)){
    if(env$clone_all){
      for(v in clone.var.list){
        #print(env$clone.missing.value.defs)
        if ( env$clone.missing.value.defs ){
          #Used only when we're trying to clone absolutely everything
          nco_cmd <- nc.copy.missval.atts(nc.array[[1]], v, env$nc.obj, v, 
                                          exception.list = c('calendar'))
          if (length(nco_cmd) > 0){
            nco_cmd_list <- c(nco_cmd_list, nco_cmd)
          }
        }else{
          #This should be the more common case - often, our default for these
          #conflict with the vals that the original files use
          nc.copy.and.combine.atts(nc.array, v, env$nc.obj, v, 
                                   exception.list = c("_FillValue", "FillValue", 
                                                      "_missval", 'missing_value', 'units'),
                                   verbose=FALSE)
        }
      }
    }
  }
  #print(nco_cmd_list)
  if (env$clone.missing.value.defs){
    env$nco_cmd_list <- nco_cmd_list
  }
  return(env)
}

get.global.except.atts <- function(env=parent.frame()){
  env$global.except.list <- c('initialization_method', 'physics_version', 'realization',
                              'product', 
                              'creation_date', 
                              'title', 'forcing', 'source',
                              'parent_experiment_id', 
                              'branch_time', 'modeling_realm', 
                              'table_id', 'tracking_id', 'parent_experiment_rip', 
                              "Institution", 'Institute_id', 'contact', 
                              'tool_version')
  return(env)
}
