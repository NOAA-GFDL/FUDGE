

build.fudge.object.csv <- function(file_list, var_list, dimlist){
  #'Builds a FUDGE-style object with time and spatial dimensions
  #'of 1, where applicable.
  
  #Note: These functions need to source the section8 tools functions 
  #called ObtainTimeAxis and ObtainSpatialAxis
  
  fudge_data <- list()
  fudge_metadata <- list()
  for (v in var_list){
    #Get data in csv file
    csv_all <- read.csv(file_list, header = T, sep = ",")
    v_data <- csv_all[[v]]
    var_len <- length(v_data)
    dim(v_data) <- c(1,1,1,1,1,var_len)
    fudge_data[[v]] <- v_data
    #make required metadata
    meta_list <- list('standard_name' = v,
                      'long_name' = v,
                      'units' = 'completely_fake_units',
                      'rips' = 'fakerip', 
                      'frequency' = 'day',
                      'calendar' = 'noleap', #is this reasonable?
                      'climatology' = NA, 
                      'dim' = list(), 
                      'vars' = list())
    if (v == 'pr'){
      stop(paste("Error in build.fudge.object.csv: we need actual units for this case!"))
    }
    fudge_metadata[[v]] <- meta_list
    #For now, assume that there are exactly 3 things we might look for: 
    #time, lat and lon
    if ('spatial' %in% dimlist){
      #dims
      spat_data <- ObtainSpatialAxes(key='mean', 
                                     lat.dim.vals=c(1),
                                     lat.units="",
                                     lat.bnds.vals=NA,
                                     lon.dim.vals=c(1),
                                     lon.units="",
                                     lon.bnds.vals=NA)
      fudge_metadata[[v]][['dim']] <- spat_data$dim
      fudge_metadata[[v]][['vars']] <- spat_data$vars
    }
    if ('temporal' %in% dimlist){
      time_data <- ObtainTimeAxis(key=var_len, 
                                  time.vals = 1:var_len, 
                                  units="days since 0000-01-01",
                                  calendar="none", 
                                  verbose=FALSE)
      fudge_metadata[[v]][['dim']] <- time_data$dim
      fudge_metadata[[v]][['vars']] <- time_data$vars
    }
  }
  return(list(data=fudge_data, metadata=fudge_metadata))
}