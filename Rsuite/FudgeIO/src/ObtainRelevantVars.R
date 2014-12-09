#Obtain.all.relevant.vars

obtain.all.relevant.vars <- function(source.nc, dim="time", verbose=FALSE){
  #initialize the structure for later
  var.struct <- list()
  if(!dim%in%names(source.nc$dim)){
    stop(paste("Missing Dimension Error: file", source.nc$filename, "does not contain the dimension", dim))
  }else if(dim=="time"){
    
    #get time dimension
    var.struct$dim$time <- source.nc$dim$time
    calendar <- var.struct$dim$time$calendar
    attr(var.struct$dim$time, "calendar") <- source.nc$dim$time$calendar
    #grab origin for later use
    origin <- var.struct$dim$time$units
    attr(var.struct$tseries, "origin") <- origin
    #Timeseries is sort of strange, but it makes several calculations easier (kfold crossval)
    var.struct$timeseries <- CreateTimeseries(source.nc$dim$time, origin, calendar, sourcefile = source.nc$filename)
    
    #get every var and every attribute related to time (pretty much calendar)
    time.vars <- names(source.nc$var)[which(regexpr(pattern="time", names(source.nc$var)) != -1)]
    #time.list <- list()
    for (name in 1:length(time.vars)){
      var.name <- time.vars[name]
      if(verbose){
        message(paste("Obtaining", var.name, ":mask", name, "of", length(time.vars)))
      }
      var.struct$var[[var.name]] <- ncvar_get(source.nc,var.name, collapse_degen=FALSE) #verbose adds too much info
      message(paste("Added time dimension"))
    }    
  }else{
    #get all dims that are not time
    non.time.dims <- names(source.nc$dim)[which(regexpr(pattern="time", names(source.nc$dim)) == -1)]
    for(ntdim in non.time.dims){
      var.struct$dim[[ntdim]] <- source.nc$dim[[ntdim]]
    }
    #get all vars that don't have the word "time" in them
    non.time.vars <- names(source.nc$var)[which(regexpr(pattern="time", names(source.nc$var)) == -1)]
    for(ntvar in non.time.vars){
      var.struct$dim[[ntvar]] <- ncvar_get(source.nc, ntvar, collapse_degen=FALSE)
    }
  }
  return(var.struct)
}