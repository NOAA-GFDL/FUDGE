#QCDSArguments.R
#'Checks for agreement between the core and optional parameters for the 
#'downscaling method and data. 
#'Relies upon variables set in DSMethodInformation.R
#'Does not return anything at present.

QCDSArguments <- function(k, ds.method, args=NA, ...){
  #message("Entering QCDSArguments")
  if(k > 1 && !(crossval.possible) ){
    stop(paste("Cross-Validation Conflict Error: Method", ds.method, 
               "does not support cross-validaiton. Check documentation for more details."))
  }
  ##Check for compatibility between names of args and the downscaling function
  for(name in 1:length(names(args)) ){
    argname <- names(args)[name]
    if(!(argname %in% names.of.args)){ #Set by SetDSMethodInfo()
      warning(paste("Downscaling Argument Warning: arg", argname, "is not used by method", ds.method, 
                    "; this could create an error or nonstandard output, depending upon the method."))
    }
  }
}