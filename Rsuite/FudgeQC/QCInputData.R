#QCInputData.R
#Carolyn Whitlock, August 2014
#' Performs quality control checks upon the 
#' inputs to the downscaling functions, both training and predictor
#' datasets. These checks include, but are not limited to:
#' *Whether the spatial dimensions of all input data agree
#' *Whether the training predictor and target datasts cover
#' the same time range
#' *If k > 1, whether the training and esdgen datasets cover the same
#' time range. 
#' 
#' @param train.predictor
#' @param train.target
#' @param esd.gen
#' -----Parameters referring to the checks to be performed on the data
#' @param k: The kfold cross-validation to be performed later
#' @param ds.method: The downscaling method used by the downscaling function
#' @param missval.threshold: The maximum percentage of missing values allowed 
#' in the data. Defaults to NA, which performs no checks. 
#' 
#' At present, nothing is returned
#' 
#' TODO: Implement check for anomalous data ranges, throw a warning message
#' TODO: Check to see whether it would ever be a good thing to change the 
#' input datasets for QC compliance (i.e. section 2.5)
#' TODO: Are there other checks that should be run? 
#' 

QCInputData <- function(train.predictor, train.target, esd.gen, k=0, ds.method="none", missval.threshold = NA){
  #Inititalize list of data to be checked
  #arg.list <- c(train.predictor, train.target, esd.gen)
  arg.names <- c("train.predictor", "train.target", "esd.gen")
  #Check for cntaining nothing but missing values
  for (arg in 1:length(arg.names)){
    arg.data <- eval(parse(text=paste(arg.names[arg],"$clim.in", sep="")))
    if( sum(!is.na(arg.data))==0){
      stop(paste("Missing value error:", arg.names[arg], "contained all NA values."))
    }
  }
#   if(sum(!is.na(train.predictor$clim.in)==0 || sum(!is.na(train.predictor$clim.in))==0 || 
#                                                      sum(!is.na(train.predictor$clim.in))==0)){
#     stop(paste("Missing value error: one or more of", "contained all NA values."))
#   }
  message("Passed all missing value check")
  #Check for more missing values than the threshold. Currently a percentage, 
  #but that can change. 
  if (!is.na(missval.threshold)){
    for (arg in 1:length(arg.names)){
      arg.data <- eval(parse(text=paste(arg.names[arg],"$clim.in", sep="")))
      missing.percentage <- sum(is.na(arg.data)) / length(arg.data)
      if (missing.percentage > (missval.threshold/100)){
        warning(paste("Missing value warning: argument", arg.names[arg], "had", missing.percentage*100,
                      "percent missing values, more than the missing value threshold of", missval.threshold))
      }
    }
    message("Passed mising value threshold check")
  }
  #Check for spatial dimension agreement
  if(dim(train.predictor$clim.in)[1:2]!=dim(train.target$clim.in)[1:2] || dim(train.predictor$clim.in)[1:2]!=dim(esd.gen$clim.in)[1:2]){
    stop(paste("Spatial dimension error: train.target had spatial dimensions of", dim(train.target)[1], dim(train.target)[2], 
               ",train.predictor had spatial dimensions of", dim(train.predictor)[1], dim(train.predictor)[2], 
               "and esd.gen had spatial dimensions of", dim(esd.gen)[1], dim(esd.gen)[2]))
  }else if(k > 1 && dim(train.predictor$clim.in)[1:2]!=dim(esd.gen$clim.in)[1:2]){
    stop(paste("Spatial dimension error: k > 1 and train.predictor had spatial dimensions of", dim(train.predictor)[1], 
               dim(train.predictor)[2], "while esd.gen had spatial dimensions of", dim(esd.gen)[1], dim(esd.gen)[2]))
  }
  message("Passed spatial dimension agreement check")
  #Check for training predictor and target timeseries agreement
  if (dim(train.predictor$clim.in)[3]!=dim(train.target$clim.in)[3]){
    stop(paste("Training time period error: Training predictor had time series length of", length(train.predictor$clim.in[1,1,]), 
               "while training target had time series length of",length(train.target$clim.in[1,1,]) ))
  }else if(k > 1 && dim(train.predictor$clim.in)[3]!=dim(esd.gen$clim.in)[3]){
    stop(paste("Time period error: k > 1 and train.predictor had time series length of", dim(train.predictor$clim.in)[3], 
               "while esd.gen had time series length of",dim(esd.gen$clim.in)[3] ))
  }
  #Insert date agreement check in here
  message("passed time series agreement check")         
  #At present, does not return anything - just throws warning messages
}