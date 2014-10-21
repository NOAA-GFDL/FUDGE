#SetDSMethodInfo.R
#' SetDSMethodInfo
#' Sets information about a given downscaling method as global variables, 
#' including whether or not the method supports cross-validation, 
#' and whether or not the method uses future data in its training. 
#' More checks to be included as QC expands.  
#' 
#' @param ds.method: a string representation of the downscaling method to be used. 
#' 

SetDSMethodInfo <- function(ds.method){
  switch(ds.method, 
                "simple.lm" = setSimpleLM(),
                'CDFt' = setCDFt(),
                'CDFtv1' = setCDFt(),
                'simple.bias.correct' = setSimple.Bias.Correct(),
                'nothing' = setNothing(), 'Nothing' = setNothing(),     
                'general.bias.correct' = setGeneral.Bias.Correct(),
         "BCQM" = setBiasCorrection(), 
         "EDQM" = setEquiDistant(), 
         "CFQM" = setChangeFactor(),
                ReturnDownscaleError(ds.method))
  #Function returns nothing, just sets globals
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

setSimpleLM <- function(){
 #Sets global variables if the DS method used is simple.lm
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("")
}

setCDFt<- function(){
 #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("npas", "dev")
}

setSimple.Bias.Correct <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE 
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  #In hindsight, I am not even sure that this applies here. 
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("ds.method", "qc.method")
}

setGeneral.Bias.Correct <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE #TODO: ASK JRL about this! It might be possible to combine two methods
  #for which that is not possible.
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  #In hindsight, I am not even sure that this applies here. 
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("ds.method", "qc.method", "compare.factor")
}

setNothing <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("")
}

setBiasCorrection <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("size")
}

setEquiDistant <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("size")
}

setChangeFactor <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("size")
}