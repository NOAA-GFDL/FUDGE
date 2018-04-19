

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
    print(head(csv_all))
    print(paste("Obtaining var", v))
    v_data <- csv_all[[v]]
    var_len <- length(v_data)
    dim(v_data) <- c(1,1,1,1,1,var_len)
    fudge_data[[v]] <- v_data
    #make required metadata
    meta_list <- list('standard_name' = v,
                      'long_name' = v,
                      'units' = 1,
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
    fudge_metadata[[v]][['dim']] <- list()
    fudge_metadata[[v]][['vars']] <- list()
    if ('spatial' %in% dimlist){
      #dims
      spat_data <- ObtainSpatialAxes(key='dummy', 
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
      fudge_metadata[[v]][['dim']] <- c(fudge_metadata[[v]][['dim']], list('time' = time_data$dim))
      fudge_metadata[[v]][['vars']] <- c(fudge_metadata[[v]][['vars']], time_data$vars)
    }
  }
  
  #Add metadata properties of var based off of lookup table
  var_classification <- T
  if (var_classification){
    #print('var clasification added')
    var.table <- paste0(FUDGEROOT, "Rsuite/aux/var_att_sheet.csv")
    if (! file.exists(var.table)){
      stop(paste("Error in build.fudge.object.csv: var_classification was true, but", 
                 var.table, "was not found!"))
    }else{
      #open table
      input <- read.csv(var.table, header=T, stringsAsFactors=F)
      table_varnames <- input$var_name
      properties <- c('is_precip')
      defaults <- list(is_precip = F)
      for (v in var_list){
        if (! v %in% table_varnames){
          print(paste(v, 'not found in lookup table; using defaults'))
          for (el in properties){
            fudge_metadata[[v]][[el]] <- defaults[[el]]
          }
        }else{
          var_idx <- which(table_varnames==v)
          for (el in properties){
            fudge_metadata[[v]][[el]] <- input[[el]][[var_idx]]
          }
        }
      }
    }
  }
  
  return(list(data=fudge_data, metadata=fudge_metadata))
}