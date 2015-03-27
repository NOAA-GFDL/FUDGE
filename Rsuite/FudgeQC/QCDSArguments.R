#QCDSArguments.R
#'Checks for agreement between the core and optional parameters for the 
#'downscaling method and data. 
#'Relies upon variables set in DSMethodInformation.R
#'Does not return anything at present.

QCDSArguments <- function(k, ds.method,...){
  #Check for cross-validation conflict
  if(k > 1 && !crossval.possible){
    stop(paste("Cross-Validation Conflict Error: Method", ds.method, 
               "does not support cross-validaton. Check documentation for more details."))
  }
}