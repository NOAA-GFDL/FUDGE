write.time.data <- function(dim.list, var.data, output, input, hist, verbose=F){
  #'Given a structure of the data that we've been working with, 
  #'goes through and writes the results to file.
  #'Separate function from WriteNC because it strips out a lot of the 
  #'checks that WriteNC uses for writing out data that has been altered
  
  var.dat <- list()
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
    if(verbose) { message(paste("defining var", var)) }
    dimlist <- list()
    dimnames <- var.data[[var]]$dimnames
    if (any(dimnames == 'bnds')){
      dimnames <- c('bnds', dimnames[dimnames != 'bnds'])
    }
    if( !dimnames[1] == ""){
      for (el in dimnames){
        #If anything but bnds, get corresponding dim from the dim list;
        #otherwise, use newly-constructed bnds var
        dimlist[[el]] <- switch(as.character(el), 
                                "bnds"=bnds, 
                                dim.list[[as.character(el)]])
      }
    }
    #We need alternate behavior at this step, depending on whether missvals
    #were specified in the original var
    #missval.strings <- c("_FillValue", "FillValue", "_missval", 'missing_value', 'missval')
    #missval.present <- sapply(names(var.data[[var]]), function(x){x %in% missval.strings})
    #missval.present <- ('missval' %in% names(var.data[[var]]))
    missval.present <- 'make_missing_value' %in% names(var.data[[var]])
    if (missval.present){
      if (var.data[[var]]$make_missing_value){
        print("missing value in var found")
        #print(str(var.data[[var]]))
        var.dat[[var]] <- ncvar_def(var, #var.data[[v]], #Don't forget to add the data in somewhere 
                                    units=var.data[[var]]$units, #Add check for adding units back in if not present
                                    dim=dimlist, #It should be fine for this to be NULL.
                                    longname = var.data[[var]]$longname, 
                                    prec = var.data[[var]]$prec, #This one gave me hives last time. 
                                    missval = var.data[[var]]$missval
        )
      }else{
        var.dat[[var]] <- ncvar_def(var, #var.data[[v]], #Don't forget to add the data in somewhere 
                                    units=var.data[[var]]$units, #Add check for adding units back in if not present
                                    dim=dimlist, #It should be fine for this to be NULL.
                                    longname = var.data[[var]]$longname, 
                                    prec = var.data[[var]]$prec #This one gave me hives last time. 
        )
      }
    }else{
      print('no missval found')
      var.dat[[var]] <- ncvar_def(var, #var.data[[v]], #Don't forget to add the data in somewhere 
                                  units=var.data[[var]]$units, #Add check for adding units back in if not present
                                  dim=dimlist, #It should be fine for this to be NULL.
                                  longname = var.data[[var]]$longname, 
                                  prec = var.data[[var]]$prec #This one gave me hives last time.                                
      )
    }
  }
  if(verbose) { message("creating nc objects") }
  nc.obj <- nc_create(output, var.dat, verbose=F)
  if(verbose) { message("placing nc vars") }
  ##Populate the vars, add to new data
  #if (metavars.present){
  for (v in 1:length(var.data)){
    loop.var <- names(var.data)[[v]]
    if(verbose) { message(paste("adding", loop.var, "and attributes")) }  
    ###This is the code that's giving buggy responses as of 12-16
    ncvar_put(nc.obj, var.dat[[loop.var]], var.data[[loop.var]]$vals, verbose=FALSE)
    #Note: failure to include missval as one of the attributes that  doesn't get set 
    #will result in giving all vars a missing value of 1e30. 
    all.att.list <- attributes(var.data[[loop.var]])
    all.att.list <- all.att.list[!(names(all.att.list)%in% c("units", "longname", 
                                                             "prec", "dim", "dimids", 
                                                             "missval", 'names'))]
    if(length(all.att.list) > 0){
      for (at in 1:length(all.att.list)){
        ncatt_put(nc.obj, loop.var,  names(all.att.list)[[at]], all.att.list[[at]])
      }
    }
  }
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
  
  #clone attributes from variables
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
  clone_all = T
  global_clone_file = input
  clone.var.list = names(var.data)
  nco_cmd_list <- list()
  clone.missing.value.defs  = TRUE
  clone_include <- "all"
  #print("clone missing value defs:")
  #print(clone.missing.value.defs)
  global_clone_function(environment())
  #   print("nco_command_list:")
  #   print(nco_cmd_list)
  #You are always going to write a history att
  if(verbose){message("Writing history attribute")}
  WriteSelectGlobals(nc.obj, hist, append=T)
  ### Close and return filename
  nc_close(nc.obj)
  ##...and edit your file attributes. I'm not sure what this does when the file is
  ##open, but the overhead seems light.
  #   if (length(nco_cmd_list) > 0){
  #     Sys.sleep(5)
  #     for (nco_cmd in nco_cmd_list){
  #       file_cmd <- paste(nco_cmd, output)
  #       print(file_cmd)
  #       system2(file_cmd)
  #     }
  #   }
  return(list(output, nco_cmd_list))
}
