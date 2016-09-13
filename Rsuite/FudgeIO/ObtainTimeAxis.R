#'ObtainTimeAxis.R
#'Obtains a climatology time series for a Julian or Noleap
#'calendar, starting on Jan. 1 and ending on Dec. 31 of year 0.
#'
#'Can also create a regular time axis of length key, starting at Jan 1 year 0, 
#'with appropriate bounds vars
#'@requires ncdf4
ObtainTimeAxis <- function(key, calendar, name="time", time.range='not_required', verbose=FALSE,
                           time.factor = "", time.vals = "", time.bnds = "", units = "", 
                           adapt.end.factors=F){
  
  if(verbose){print(paste('using key', key))}
  
  if(is.character(key)){
    #There are two keys that rely on a reference file to generate a result, 
    #and one that generates the time dim and bounds on its own
    if(key=='ann' || key=='mon' || key=='seas'){
      if (verbose) { print(paste("Keying off of provided key", key)) }
      #Obtaining the reference file
      base.path <- Sys.getenv("S8_PATH")
      if(base.path==""){
        #If S8_tools not set, assume that you are working from the correct directory
        base.path <- getwd()
      }
      #TODO: replace once the sample code is working
      #clim.file <- paste0(base.path, "/FudgeIO/references/fudge_climatological_axes.nc")
      clim.file <- paste0(base.path, "/FudgeIO/references/text_climseries_all.nc")
      if (verbose) { print(paste("Reference file:", clim.file)) }
      #clim.file <- "/net2/kd/PROJECTS/DOWNSCALING/SEC8/sampledata/fudge_climatological_axes.nc"
      clim.nc <- nc_open(clim.file)
      calendar.string <- switch(calendar, 
                                "julian"="JULIAN", 
                                "noleap"='NOLEAP', 
                                stop(paste("Error in ObtainTimeAxis: calendar", calendar, "not supported.")))
      time.string <- switch(key, 
                            "mon"="MONTH", 
                            "ann"="ANNUAL", 
                            "seas"="SEASONAL", 
                            stop(paste("Error in ObtainTimeAxis: key", key, "not supported.")))
      units="days since 0000-01-01"
      origin="1-JAN-0000"
      
      #Create time dimension
      time.var <- ncvar_get(clim.nc, paste(time.string, calendar.string, sep="_"))
      time.dim <- ncdim_def(name, units=units, vals=time.var, create_dimvar=TRUE, 
                            calendar=calendar, longname=paste(key, "time dimension"))
      #And time_bnds var
      time.edges <- ncvar_get(clim.nc, paste(time.string, calendar.string, "EDGES", sep="_"), collapse_degen=FALSE)
      time.bounds <- c((time.edges[1:length(time.edges)-1]), 
                       (time.edges[2:length(time.edges)]))
      #climatology bounds
      clim.bounds <- c((time.edges[1:length(time.edges)-1] + time.range[1]), 
                       (time.edges[2:length(time.edges)] + time.range[2]))
      dim(time.bounds) <- c(length(time.edges)-1, 2)
      dim(clim.bounds) <- c(length(time.edges)-1, 2)
      time.bounds <- aperm(time.bounds, c(2,1))
      clim.bounds <- aperm(clim.bounds, c(2,1))
    }else if(key=='factor'){
      #if there is a factor key, search for the factor var
      #search for factor, time.vals
      if(units!="" && time.factor!="" && time.vals!=""){
        time.dim.vals <- rep(NA, nlevels(time.factor))
        time.bnds.vals <- array(NA, dim=c(2, nlevels(time.factor)))
        print(nlevels(time.factor))
        print(length(time.factor))
        
        for (level in 1:nlevels(time.factor)){
          loop.fac <- time.factor==levels(time.factor)[level]
          level.select <- time.vals[loop.fac]
          time.bnds.vals[1,level] <- min(time.bnds[1,][loop.fac]) #smallest first val
          if(adapt.end.factors){ #exceeds ncount-specific adjustment
            time.bnds.vals[2,level] <- 2*time.mean - time.bnds.vals[1,level] #avoid overlapping bnds
          }else{ #This is the most common case
            time.bnds.vals[2,level] <- max(time.bnds[2,][loop.fac]) #largest second val
          }
          #time.mean <- mean(time.bnds.vals[,level])
          time.mean <- mean(c(time.bnds.vals[1,level], time.bnds.vals[2,level]))
          time.dim.vals[level] <- time.mean
        }
        #Create var and dimvar; return
        time.dim <- ncdim_def("time", units, 
                              vals=time.dim.vals, 
                              unlim=TRUE, 
                              create_dimvar=TRUE, 
                              calendar=calendar, 
                              longname="time")
        time.bnds <- create.fudge.metavar.object('time.bnds', time.bnds.vals, 
                                                 prec='double', dimnames=c('bnds', 'time'), 
                                                 longname='time axis boundaries', 
                                                 units=units)
        attributes(time.bnds) <- list('units' = units, 
                                      'dimids' = "bnds,time", 
                                      'prec' = "double", 
                                      'longname' = "time axis boundaries")
        #print(summary(time.bnds))
        #stop('examine output')
        return(list('dim' = time.dim, 'vars' = list('time_bnds' = time.bnds)))
      }else{
        stop(paste("Error in ObtainTimeAxis: key was factor, but factor, units and time vals not present!"))
      }
    }else if(key=='365_day'){
      time.dim <- ncdim_def(name=name, 
                            vals=seq(from=0.5, by=1, length.out=365), 
                            unlim=TRUE,
                            units='days since 0000-01-01', 
                            create_dimvar=TRUE, 
                            calendar=calendar, 
                            longname="time")
      time.bounds <- c(seq(0, by=1, length.out=365), 
                       seq(1, by=1, length.out=365))
      #dim(time.bounds) <- c(2,365)
      dim(time.bounds) <- c(365,2)
      #time bounds variable
      time.bounds <- aperm(time.bounds, c(2,1))
      #Climatology bounds variable
      clim.bounds <- c((time.bounds[1,] + time.range[1]), 
                       time.bounds[2,] + time.range[2])
    }else if(key=='values'){
      #Keep in mind you have a set of values that are used
      time.dim <- ncdim_def(name=name, 
                            vals=time.vals, 
                            unlim=TRUE,
                            units=units, 
                            create_dimvar=TRUE, 
                            calendar=calendar, 
                            longname="time")
      time.bounds <- time.bnds
      clim.bounds=NULL
    }else{
      stop(paste("Error in ObtainTimeAxis: please check your key to make sure that it matches one of the allowed strings!"))
    }
    time.bounds <- create.fudge.metavar.object('time.bnds', time.bounds, 
                                               prec='double', dimnames=c('bnds', 'time'), 
                                               longname='time axis boundaries', 
                                               units=time.dim$units)
    attributes(time.bounds) <- list('units' = time.dim$units, 
                                    'dimids' = "bnds,time", 
                                    'prec' = "double", 
                                    'longname' = "time axis boundaries")
    #These are needed for some cases
    if(!is.null(clim.bounds)){
      #climatology bounds
      clim.bounds <- create.fudge.metavar.object('clim.bnds', clim.bounds, 
                                                 prec='double', dimnames=c('bnds', 'time'), 
                                                 longname='climatology axis boundaries', 
                                                 units=time.dim$units)
      attributes(clim.bounds) <- list('units' = time.dim$units, 
                                      'dimids' = "bnds,time", 
                                      'prec' = "double", 
                                      'longname' = "climatology axis boundaries")
      var.list <- list('time_bnds' = time.bounds, 'climatology_bounds' = clim.bounds)
    }else{
      var.list <- list('time_bnds' = time.bounds)
    }
    return(list(dim=time.dim, vars=var.list))
  }else{
    #This is the alternative that provides a minimalist
    #clim time axis, with units of ""
    if(is.numeric(key)){
      time.dim <- ncdim_def(name='time', 
                            vals=seq(from=1, by=1, length.out=key), #Changed from 0.5 4-7
                            unlim=TRUE,
                            units='', #days since 0000-01-01
                            create_dimvar=TRUE, 
                            calendar="none", 
                            longname="time")
      time.bounds <- c(seq(0.5, by=1, length.out=key), #Changed from 0 on 4-7
                       seq(1.5, by=1, length.out=key)) #Same
      #dim(time.bounds) <- c(2,key)
      dim(time.bounds) <- c(key,2)
      time.bounds <- aperm(time.bounds, c(2,1))
    }else{
      stop(paste("Error in ObtainTimeAxis: You need either a numeric or character key argument!"))
    }
    time.bounds.var <- create.fudge.metavar.object('time.bnds', time.bounds, 
                                                   prec='double', dimnames=c('bnds', 'time'), 
                                                   longname='time axis boundaries', 
                                                   units=time.dim$units)
    attributes(time.bounds) <- list('units' = time.dim$units, 
                                    'dimids' = "bnds,time", 
                                    'prec' = "double", 
                                    'longname' = "time axis boundaries")
  }
  return(list(dim=time.dim, vars=list('time_bnds'=time.bounds.var))) #'climatology_bounds' = clim.bounds
}