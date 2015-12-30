#SetDSMethodInfo.R
#' SetDSMethodInfo
#' Sets information about a given downscaling method as global variables, 
#' including whether or not the method supports cross-validation, 
#' and whether or not the method uses future data in its training. 
#' More checks to be included as QC expands.  
#' 
#' @param ds.method: a string representation of the downscaling method to be used. 
#' 

SetDSMethodInfo <- function(ds.method, predictor.vars=list('na')){
  message(ds.method)
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
         'lm' = setMultiLM(),
         'lmstep' = setMultiLM(),
         "DeltaSD" = setDeltaSD(),
         "ARRM" = setARRM(),
         ReturnDownscaleError(ds.method))
  #Function returns nothing, just sets globals
  
  #But does run a couple extra checks to make sure that there are no gross mismatches
  if(!supports.multivariate && length(predictor.vars) > 1){
    stop(paste("Error in SetDSMethodInfo: method", ds.method, "does not support multiple predictor vars,", 
               "but vars", paste(predictor.vars, collapse=" "), "were provided!"))
  }
  #At the moment, do not require a specific check for supporting univariate 
  #methods, since EVERYTHING should support that
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, 
             "is not supported for FUDGE at this time."))
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
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'cdft'
  #missval.handling <<- 'distributional'
  #What are the names of the datasets used in training?
  train.names <<- c('OH', 'MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('MF')
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
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional'
  train.names <<- c('OH', 'MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('OH', 'MH', 'MF')
}

setGeneral.Bias.Correct <- function(){
  #Sets global variables if the DS method used is the general bias correction method
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE #TODO: ASK JRL about this! It might be possible to combine two methods
  #for which that is not possible.
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  #In hindsight, I am not even sure that this applies here. 
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("ds.method", "qc.method", "compare.factor")
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional'
  train.names <<- c('OH', 'MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('OH', 'MH', 'MF')
}

setNothing <- function(){
  #Sets global variables if the DS method used is a fake method for testing purposes
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("")
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional'
  train.names <<- c('OH', 'MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('OH', 'MH', 'MF')
}

setBiasCorrection <- function(){
  #Sets global variables if the DS method used is BCQM
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE 
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("size", "flip")
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional'
  train.names <<- c('OH', 'MH')
  #train.names <<- c('MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('MF')
}

setEquiDistant <- function(){
  #Sets global variables if the DS method used is CDFt
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE 
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("size")
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional'
  train.names <<- c('OH', 'MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('MF')
}

setChangeFactor <- function(){
  #Sets global variables if the DS method used is CFQM
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- FALSE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE 
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("size")
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional'
  #train.names <<- c('OH', 'MH')
  train.names <<- c('MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('OH')
}

setDeltaSD<- function(){
  #Sets global variables if the DS method used is DeltaSD
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- TRUE #Temporarily set to TRUE for testing purposes; supposed to be FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c("deltatype", "delataop", 'ignore0.Delta')
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'delta'
  train.names <<- c('MH', 'MF')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('OH')
}

setMultiLM <- function(){
  #Sets global variables if the DS method used is lm
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c()
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- TRUE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'synchronous'
  train.names <<- c('OH', 'MH')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('MF')
}

setARRM <- function(){
  #Sets global variables if the DS method used is lm
  #Is it possible to use cross-validation with this method?
  crossval.possible <<- TRUE
  # Does this method use some of the same data to train the 
  # ESD equations/quantiles AND generate the downscaled data?
  train.and.use.same <<- FALSE
  # What are the arguments to the args() parameter that are accepted? 
  names.of.args <<- c()
  #Is it possible to use multiple predictors with this method?
  supports.multivariate <<- FALSE
  #How are missing values handled when going into the downscaling code?
  missval.handling <<- 'distributional' #probably should NOT be synchronous
  train.names <<- c('OH', 'MH')
  #What are the names of the datasets used to downscale?
  ds.names <<- c('MF')
}
  
