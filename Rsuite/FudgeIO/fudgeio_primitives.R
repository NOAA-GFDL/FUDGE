#Note: regex instead of pattern means that you can get all dims by using the 
#empty string as your pattern

get.ncdim.names <- function(obj.nc, nc.var, include.none=F){
  #'Gets the names of all the dimensions a given var will reference
  get.dim.names()
  dim.names <- unlist(lapply(obj.nc$var[[nc.var]]$dim, '[[', 'name'), use.names = F)
  if(length(dim.names)==0){
    if(include.none){
      dim.names <- "none" #...okay, it's been '' for a long time. But that breaks the regex later.
    }else{
      dim.names <- ""
    }
  }else{
    if(include.none && any(check.fudge.dimname.match(dim.names, c(x.dims, y.dims)))){
      dim.names <- c(dim.names, 'none')
    }
  }
  return(dim.names)
}

get.ncvar.dims <- function(obj.nc, nc.var){
  #Gets the dimensions of a variable stored in an open etcdf file, but 
  #not yeat read into memory
  return(sapply(lapply(test.nc[[nc.var]]$tasmax$dim, '[[', 'vals'), length))
}

obtain.fudge.var.old <- function(obj.nc, nc.var, coords){
  #'Grabs a varaible about to be used in FUDGE
  #'and makes sure that its diemsnions correspond with the 
  #'dimensions expected for FUDGE use.
  var.data <- ncvar_get(obj.nc, nc.var, collapse_degen=F)
  var.dims <- get.ncdim.names(obj.nc, nc.var)
  get.dim.names()
  #Dimension order is different for the older code
  name.list <- list(x.dims, y.dims, z.dims, ens.dims, time.dims)
  dim.order <- c(0, 0, 0, 0, 0)
  dim.len   <- c(1, 1, 1, 1, 1)
  for(n in 1:5){
    dim.loc <- which(var.dims %in% name.list[[n]])
    if(length(dim.loc) == 0){
      dim.loc <- 0
      dim(var.data) <- c(dim(var.data), 1)
    }
    dim.order[n] <- dim.loc
  }
  num.zero <- sum(dim.order==0)
  dim.order[dim.order==0] <- seq( (6-num.zero), by=1, length.out=num.zero)
  test.data <- aperm(var.data, perm=dim.order)
  test.dim <- dim(test.data)
  #Force data into four dimensions
  if ( !(test.dim[3]==1 || test.dim[4]==1)){
    stop(paste("Error in obtain.fudge.var.old: More than one of plev", 
               "and ensemble was non-degenerate!"))
  }else{
    test.dim <- c(test.dim[1:2], prod(test.dim[3:4]),test.dim[5])
    dim(test.data) <- test.dim
  }
  return(test.data)
}

obtain.fudge.var <- function(obj.nc, nc.var, subset=list()){
  #'Grabs a varaible about to be used in FUDGE
  #'and makes sure that its diemsnions correspond with the 
  #'dimensions expected for FUDGE use.
  #var.data <- ncvar_get(obj.nc, nc.var, collapse_degen=F)
  var.dims <- get.ncdim.names(obj.nc, nc.var)
  #var.dim.lengths <- get.ncvar.dims(obj.nc, nc.var)
  get.dim.names()
  name.list <- list(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims)
  dim.order <- c(0, 0, 0, 0, 0, 0)
  dim.len   <- c(1, 1, 1, 1, 1, 1)
  dim.pres  <- c(T, T, T, T, T, T)
  dim.start <- c()
  dim.count <- c()
  for(n in 1:6){
    dim.loc <- which(var.dims %in% name.list[[n]])
    if(length(dim.loc) == 0){
      dim.loc <- 0
      #var.dim.lengths <- c(var.dim.lengths, 1)
      dim.pres[n] <- F
    }
    dim.order[n] <- dim.loc
    if (any(names(subset) %in% name.list[[n]])){
      dim.subset <- subset[[which(names(subset) %in% name.list[[n]])]]
      print(dim.subset)
      dim.start <- c(dim.start, dim.subset[1])
      dim.count <- c(dim.count, diff(dim.subset))
    }else{
      dim.start <- c(dim.start, 1)
      dim.count <- c(dim.count, (-1))
    }
  }
  num.zero <- sum(dim.order==0)
  dim.order[dim.order==0] <- seq( (7-num.zero), by=1, length.out=num.zero)
  dim.count <- dim.count[dim.order]
  dim.start <- dim.start[dim.order]
  var.data <- ncvar_get(obj.nc, nc.var, collapse_degen=F, 
                        start=dim.start[dim.pres[dim.order]], count=dim.count[dim.pres[dim.order]])
  dim(var.data) <- c(dim(var.data), dim.len[!dim.pres])
  test.data <- aperm(var.data, perm=dim.order) #dim.order used to get ordered as well, but that might not be right
  return(test.data)
}

get.dim.names <- function(env=parent.frame()){
  #What I want this function to do is return the 
  #possible vals of each dim name to the parent
  #environment.
  env$x.dims <- c('x', 'i', 'lon', 'longitude') #'none'
  env$y.dims <- c('y', 'j', 'lat', 'latitude')
  env$z.dims <- c('z', 'p', 'k','plevel', 'plev', 'height')
  env$time.dims <- c('time')
  env$ens.dims <- c('m', 'ensmem', 'ensemble', 'ens')
  env$n.dims <- c('n')
  env$bnds.dims <- c('bnds', 'bounds')
  return(env)
}

get.metavar.names <- function(env=parent.frame()){
  #What I want this function to do is return the 
  #possible vals of each dim name to the parent
  #environment.
  env$x.vars <- c('i_offset', 'lon_bnds', 'lon_bounds')
  env$y.vars <- c('j_offset', 'lat_bnds', 'lat_bounds')
  env$z.vars <- c('z', 'p', 'k_offset','plevel', 'plev', 'height')
  env$time.vars <- c('t_bnds','time_bnds', 'time_bounds')
  #env$ens.dims <- c('m', 'ensmem', 'ensemble', 'ens')
  #env$n.dims <- c('n')
  #env$bnds.dims <- c('bnds', 'bounds')
  return(env)
}

get.selection.dim.names <- function(select, include.none=F){
  get.dim.names()
  if(!is.na(select[1])){
    select.names <- c()
    for (el in select) {
      select.el <- switch(el, 
                          'spatial' = paste0(c(x.dims, y.dims, z.dims), "$"), 
                          'space' = paste0(c(x.dims, y.dims, z.dims), "$"),
                          #'spatial' = paste0("^(?!", time.dims, "$)"), #paste0("^(?!", ens.dims, "$)")),
                          #'space' = paste0("^(?!", time.dims, "$)"), #paste0("^(?!", ens.dims, "$)")),
                          'temporal' = paste0(time.dims, "$"), 
                          'time' = paste0(time.dims, "$"), 
                          'ens' = paste0(ens.dims, "$"),
                          'ensemble' = paste0(ens.dims, "$"))
      select.names <- c(select.el, select.names)
      if(el %in% c('space', 'spatial')){
        if (include.none){
          select.names <- c(select.names, "none$")
        }
      }
    }
  }else{
    select.names <- ""
  }
  select.names <- unique(select.names)
  return(select.names)
}

check.fudge.dimname.match <- function(patt, vec){
  #Sees if there are overlaps in the sets of dimensions
  #described by patt within vec
  #Excludes bounds vars b/c those don't get used for metadata
  get.dim.names()
  vec <- vec[!(vec %in% bnds.dims)]
  s.out <- sapply(patt, grepl, vec, perl=T)
  if(!is.null(dim(s.out))){
    s.out <- apply(s.out, 1, sum)
  }
  return(s.out > 0)
}

obtain.fudge.var.dimensions <- function(nc.obj, nc.var, select=NA, subset=list()){
  #Grabs the dimension object wholesale from the 
  #netcdf object
  print('obtaining dimensions')
  get.dim.names()
  name.list <- list(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims)
  select.dimnames <- get.selection.dim.names(select) 
  if(is.na(select[1])){
    dim.select <- nc.obj$var[[nc.var]]$dim
  }else{
    dim.select <- list()
    dimvar.names <- sapply(nc.obj$var[[nc.var]]$dim, '[[', 'name')
    dim.match <- check.fudge.dimname.match(select.dimnames,dimvar.names)
    dim.select <- nc.obj$var[[nc.var]]$dim[dim.match==T]
  }
  dim.select.names <- sapply(dim.select, '[[', 'name')
  names(dim.select) <- dim.select.names
  out <- dim.select[!is.null(dim.select)]
  for (el in names(out)){
    out[[el]]$filename <- nc.obj$filename
  }
  #If you are specifying a subset, it is neccessary to 
  for(n in 1:length(name.list) ){
    if (any(names(subset) %in% name.list[[n]])){
      n.subset <- subset[[which(names(subset) %in% name.list[[n]])]]
      n.outname <- dim.select.names[which(dim.select.names %in% name.list[[n]])]
      out[[n.outname]]$vals <- out[[n.outname]]$vals[seq(n.subset[1]:n.subset[2])]
    }
  }
  return(out)
}

obtain.fudge.var.metavars <- function(nc.obj, nc.var, select=NA, subset=list()){
  #Get the non-data vars from the input file
  # note: it is assumed that all vars using exactly the
  # same vars as the var of interest are data vars
  all.varnames <- names(nc.obj$var)
  nc.var.dims <- get.ncdim.names(nc.obj, nc.var, include.none=T)
  select.dimnames <- get.selection.dim.names(select, include.none=T)
  select.vars <- c()
  for(var in all.varnames){
    var.dims <- get.ncdim.names(nc.obj, var, include.none=T)
    #If not covering exactly the same dims as data var
    if( !all(all.equal(nc.var.dims, var.dims)==T) ){
      #And if any of the dims the var uses match the dims of interest
      if(any(check.fudge.dimname.match(select.dimnames, var.dims))){
        select.vars <- c(select.vars, var)
      }
    }
  }
  out <- lapply(select.vars, 'clone.fudge.metavar.object', nc.obj, subset)
  names(out) <- select.vars
  for (vname in select.vars){
    attributes(out[[vname]]) <- c(ncatt_get(nc.obj, vname), attributes(out[[vname]]))
  }
  #stop('examine output')
  return(out)
}

clone.fudge.metavar.object <- function(var.name, nc.obj, subset=list()){
  #'Returns a list of various data and metadata used by the read/write functions
  #'for writing data.
  dim.names <- get.ncdim.names(nc.obj, var.name)
  get.dim.names()
  name.list <-  list(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims)
  var.start <- rep(1, length(dim.names))
  var.count <- rep((-1), length(dim.names))
  print(names(subset))
  for(el in names(subset)){
    n <- sapply(name.list, function(x,y){any(x%in%y)}, el)
    dim.which <- dim.names %in% unlist(name.list[n])
    var.start[dim.which] <- subset[[el]][1]
    var.count[dim.which] <- diff(subset[[el]])
  }
  var.all <- nc.obj$var[[var.name]]
  print(var.start)
  print(var.count)
  var.data <- ncvar_get(nc.obj, var.name, collapse_degen=F, 
                        start=var.start, count=var.count)
  if (var.all$prec=='int'){
    var.all$prec <- "integer"
  }
  var.dims <- dim.names
  out <- c(var.all, 
           list(vals=var.data, dimnames=dim.names))
  return(out)
}

create.fudge.metavar.object <- function(var.name, var.data, ...){
  #Creates a list of data and metadata used by the read/write functions
  #based on user-supplied input (i.e. what you need for creating time_bnds)
  return(list(vals=var.data, ...)) 
}

obtain.fudge.var.metadata <- function(nc.obj, nc.var){
  #'Grabs the single-attribute components
  #'of the attribute table from the input 
  #'nc object and variable.
  attnames <- c('standard_name', 'long_name', 'units', 
                'prec', 'calendar', 'filename', 'rips')
  att.defaults <- c('', nc.var, "", 
                    'float', "", '', "")
  var.att.list <- list()
  print(nc.var)
  for (n in 1:length(attnames)){
    tmp.att <- ncatt_get(nc.obj, nc.var, attnames[n])
    if ( !(tmp.att$hasatt)){
      tmp.att$value <- att.defaults[n]
    }
    var.att.list[n] <- tmp.att$value
  }
  names(var.att.list) <- attnames
  #Add backstip for empty rip attribute
  print(var.att.list)
  if(var.att.list$rips==""){
    var.att.list$rips <- get.rip.data(nc.obj)
  }
  #optional.atts <- c("comments", 'modulo')
  var.att.list['filename'] <- nc.obj$filename
  return(var.att.list)
}

get.rip.data <- function(nc.obj){
  #Tries a few methods for getting the rip information
  #from an input file
  #Try the filename first; accept both alt paterns
  rip <- 'ZZ'
  data.list <- unlist(strsplit(basename(nc.obj$filename), split="_"), use.names = F)
  test.rip <- data.list[5]
  if (grepl('r[[:digit:]].*i[[:digit:]]p[[:digit:]]', test.rip) || 
        test.rip %in% c('DSDEPh', 'DSCVh')){
    rip <- test.rip
  }
  if(rip=='ZZ'){
    test.atts <- ncatt_get(nc.obj, 0)
    #Check for 'realizations'
    if ('realizations' %in% names(test.atts)){
      rip <- test.atts['realizations']
    }else if( 'rip' %in% names(test.atts) ){
      rip <- test.atts['rip']
    }else if (sum(c('realization', 'initialization_method', 'physics_version') %in% names(test.atts))==3){
      rip = paste0('r', test.atts['realization'], 'i', 
                   test.atts['initialization_method'], 
                   'p', test.atts['physics_version'])
    }else{
      #Absolute backstop: feed a string
      rip = "unknown.rip"
    }
  }
  return(rip)
}