#QCDSArguments.R
#'Checks for agreement between the core and optional parameters for the 
#'downscaling method and data. 
#'Relies upon variables set in DSMethodInformation.R
#'Does not return anything at present.

QCDSArguments <- function(k, ds.method...){
  if(k > 1 && ds.method%in%no.crossval.possible){
    stop(paste("Cross-Validation Conflict Error: Method", ds.method, 
               "does not support cross-validaiton. Check documentation for more details."))
  }
}