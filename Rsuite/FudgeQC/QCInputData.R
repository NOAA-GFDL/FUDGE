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
#' @param calendar: The calendar of the time masks. Should match the calendars
#' of the input data
#' 
#' At present, nothing is returned
#' 
#' TODO: Implement check for anomalous data ranges, throw a warning message
#' TODO: Check to see whether it would ever be a good thing to change the 
#' input datasets for QC compliance (i.e. section 2.5)
#' TODO: Are there other checks that should be run? 
#' 

QCInputData <- function(train.predictor, train.target, esd.gen, k=0, ds.method="none", missval.threshold = NA, calendar="julian", 
                        allow.timeseries.nas=FALSE){
  #Inititalize list of data to be checked
  arg.names <- c("train.predictor", "train.target", "esd.gen")
  #Do the checks for consistency within a dataset
  for (arg in 1:length(arg.names)){
    loop.arg <- eval(parse(text=arg.names[arg]))
    #arg.data <- eval(parse(text=paste(arg.names[arg],"$clim.in", sep="")))
    arg.data <- loop.arg$clim.in
    ## Were all values missing? 
    if( sum(!is.na(arg.data))==0){
      warning(paste("Missing value warning:", arg.names[arg], 
                    "contained all NA values. No non-NA values will be produced from this run."))
    }
    ## Were there more missing values than the missing value threshold? 
    if (!is.na(missval.threshold)){
        missing.percentage <- sum(is.na(arg.data)) / length(arg.data)
        if (missing.percentage > (missval.threshold/100)){
          warning(paste("Missing value warning: argument", arg.names[arg], "had", missing.percentage*100,
                        "percent missing values, more than the missing value threshold of", missval.threshold))
        }
      }
    #arg.cal <- eval(parse(text=paste("attr(", arg.names[arg], ", 'calendar')", sep="")))
    arg.cal <- attr(loop.arg, "calendar")
    ## Did the calendar match the calendar of the common data? 
    if ( arg.cal != calendar){
      stop(paste("Calendar mismatch error:", arg.names[arg], "read in from", attr(loop.arg, "filename"), 
                 "had a calendar attribute of", attr(loop.arg, "calendar"), 
                 "and an expected calendar attribute of", calendar))
    }
    #Were there discontinuous timeseries within the dataset?
#     if(!allow.timeseries.nas){
#       out <- rle(is.na(as.vector(arg.data)))
#       print(paste("allow.timeseries.check on", arg.names[arg], ":", TRUE%in%out$values))
# #       print(out$values)
# #       print(out$lengths)
#       if(TRUE%in%out$values){
#         #If there are missing values:
#         out2<-rle(out$lengths[out$values==TRUE])
#         #a continuous time series, if it has spatial discontinuities, will have length(timeseries)
#         #entries in out, all with the same length
#         print(length(out$lengths))
#         print(out2$values)
#         if(length(out$lengths)%%dim(arg.data)[3]!=0){
#           #If there are missing values present and those missing values do 
#           #not occupy an entire time series
#           stop(paste("Discontinuous Time Series Error:", arg.names[arg], "read in from", attr(loop.arg, "filename"), 
#                      "had a discontinuous time series. Please check missing values."))
#         }
#       }
#     }
  }
  message("Datasets passed internal consistency checks")
  
  message("Checking for consistency between input datasets") 
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