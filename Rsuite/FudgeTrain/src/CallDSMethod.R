#CallDSMethod.R
#' CallDSMethod
#' Calls a downscaling method specified as ds.method and returns the result of training it 
#' upon train.predict and train.target, and then running the relvant equation upon
#' esd.gen. No cross-validation is used for these methods. 
#' 
#' @param ds.method: a string representation of the downscaling data to be used. 
#' @param train.predict: a vector of predictor data
#' @param train.target: a vector of the target data
#' @param esd.gen: a vector of the data used to generate downscaled data 
#' @param args = NULL: a list of arguments to be passed to the downscaling function.
#' Defaults to NULL (no arguments)
#' @param ds.var: The target variable. Can be used to key off of some methods. 
#' @examples 
#' @references \url{link to the FUDGE API documentation} 
#' TODO: Find a better name for general.bias.corrector

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL, ds.var='irrelevant'){
  #  library(CDFt)
  return(switch(ds.method, 
                "simple.lm" = callSimple.lm(train.predict, train.target, esd.gen),
                'CDFt' = callCDFt(train.predict, train.target, esd.gen, args),
                'simple.bias.correct' = callSimple.bias.correct(train.predict, train.target, esd.gen, args),
                'general.bias.correct' = callGeneral.Bias.Corrector(train.predict, train.target, esd.gen, args),
                "BCQM" = callBiasCorrection(train.predict, train.target, esd.gen, args), 
                "EDQM" = callEquiDistant(train.target, train.predict, esd.gen, args), 
                "CFQM" = callChangeFactor(train.target, train.predict, esd.gen, args), 
                "DeltaSD" = callDeltaSD(train.target, train.predict, esd.gen, args, ds.var),
                'Nothing' = callNothing(train.target, train.predict, esd.gen, args),
                ReturnDownscaleError(ds.method)))
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

callSimple.lm <- function(pred, targ, new, args){
  #Creates a simple linear model without a cross-validation step. 
  #Mostly used to check that the procedure is working
  lm.results <- lm(targ ~ pred)
  lm.intercept <- lm.results$coefficients[1]
  lm.slope <- lm.results$coefficients[2]
  if(is.na(lm.intercept) || is.na(lm.slope) ){
    warning(paste("simple.lm warning: intercept was", lm.intercept, 
                  "and intercept was", lm.slope, ": therefore no ESD values will be generated."))
  }
  trained.function<-function(x){
    print(lm.intercept)
    print(lm.slope)
    return( lm.intercept + unlist(x)*lm.slope)
  }
  #insert save command for saving 
  return(trained.function(new))
}

callCDFt <- function (pred, targ, new, args){
  #Calls the CDFt function
  #If no argument is provided for npas, defaults to 
  #npas=length(targ)
  ###Obtain required arguments (and throw errors if not specified)
  if(!is.null(args$npas)){
    npas <- args$npas
    if(npas=='default' || npas==0){
      #npas = shorter of future predictor/esd.gen or training target
      npas=ifelse(length(targ) > length(new), length(new), length(targ))
    }else if(npas=='training_target'){
      #Note: this option is needed to duplicate 'default'
      #for results prior to 10-20-14
      npas=length(targ)
    }else if(npas=='future_predictor'){
      npas=length(new)
    }
  }else{
    stop(paste("CDFt Method Error: parameter npas was missing from the args list"))
  }
  if(!is.null(args$dev)){
    dev <- args$dev
  }else{
    stop(paste("CDFt Method Error: parameter dev was missing from the args list"))
  }
  if(is.null(args)){
    return(CDFt(targ, pred, new, npas=length(targ))$DS)
  }else{
    ##Note: if any of the input data parameters are named, CDFt will 
    ## fail to run with an 'unused arguments' error, without any decent
    ## explanation as to why. This way works.
    #     if(!'npas'%in%names(args)){
    #       args <- c(npas=length(targ), args)
    #     }
    #    args.list <- c(list(targ, pred, new), args)
    args.list <- c(list(targ, pred, new), list(npas=npas, dev=dev))
    #    print("calling CDFt with args:")
    #    print(args)
    return(do.call("CDFt", args.list)$DS)
  }
}

callSimple.bias.correct <- function(pred, targ, new, args){
  #Performs a simple bias correction adjustment,
  #applying the mean difference between the
  #predictor and target over the time series
  #to the esd.gen dataset to give downscaled data.
  bias <- mean(pred-targ)
  new.targ <- new-bias
  return(new.targ)
}

callGeneral.Bias.Corrector <- function(pred, targ, new, args){
  #Calls two downscaling methods: one used as a source of downscaling
  #values, the other used as a check against those values. Those values
  #are then compared; if the values are sufficiently similar to each other, 
  #the downscaled values are used; otherwise, the qc values are used. Ideally, 
  #the method used for QC should be less computationally-expensive than the 
  #method used for downscaling. 
  qc.method <- args$qc.method
  args$qc.method <- NULL
  ds.method <- args$ds.method
  args$ds.method <- NULL
  if(!is.null(args$compare.factor)){
    correct.factor <- args$compare.factor
    args$compare.factor <- NULL
  }else{
    correct.factor = 0.5
  }
  if(length(args)!=0) sample.args=args else sample.args=NULL
  ds.vals <- CallDSMethod(ds.method=ds.method, pred, targ, new, sample.args)
  qc.vals <- CallDSMethod(ds.method=qc.method, pred, targ, new, sample.args)
  out.vals <- ifelse( (abs(ds.vals-qc.vals) < correct.factor), yes=ds.vals, no=qc.vals )
  return(out.vals)
}

callBiasCorrection <- function(LH, CH, CF, args){
  #'Performs a bias correction adjustment with parameters
  #'that I will ask CG about tomorrow
  # first define vector with probabilities [0,1]
  # LH: Local Historical (a.k.a. observations)
  # CH: Coarse Historical (a.k.a. GCM historical)
  # CF: Coarse Future (a.k.a GCM future)
  # args: A vector of arguments to the function. 
  #currently takes one: preserve.order="true"
  #or preserve.order="false"
  #   if(!is.null(args$size)){
  #     size <- args$size
  #     args$size <- NULL
  #   }else{
  size <- length(CF)
  prob<-c(0.001:1:size)/size
  #check the order preservation status
    in.sort <- order(CF)
    CF.out <- CF[in.sort]
  # QM Change Factor
  #
  #SDF<-quantile(LH,ecdf(CH)(quantile(CF.out,prob)),names=FALSE)
  SDF<-quantile(LH,ecdf(CF.out)(quantile(CH,prob)),names=FALSE)
  #CEW: creation of historical values commented out for the moment
  #SDH<-quantile(LH,ecdf(CH)(quantile(CH,prob)),names=FALSE)
  #SDoutput<-list("SDF"=SDF,"SDH"=SDH)
    SDF <- SDF[order(in.sort)]
  return (SDF)
}

callEquiDistant <- function(LH, CH, CF, args){
  #'Performs an equidistant correction adjustment with parameters
  #'that I will ask CG about tomorrow
  # first define vector with probabilities [0,1]
  # LH: Local Historical (a.k.a. observations)
  # CH: Coarse Historical (a.k.a. GCM historical)
  # CF: Coarse Future (a.k.a GCM future)
  #'Cites Li et. al. 2010
  size <- length(CF)
  prob<-c(0.001:1:size)/size
  
  #check order preservation status
    in.sort <- order(CF)
    CF.out <- CF[in.sort]

  #Create numerator and denominator of equation
  temporal<-quantile(LH,(ecdf(CF.out)(quantile(CF.out,prob))),names=FALSE)
  temporal2<-quantile(CH,(ecdf(CF.out)(quantile(CF.out,prob))),names=FALSE)
  
  # EQUIDISTANT CDF (Li et al. 2010)
  SDF<-CF.out + temporal-temporal2
  #CEW creation of downscaled historical values turned off for the moment
  #SDH<-CH + temporal-temporal2
  #SDoutput<-list("SDF"=SDF,"SDH"=SDH)

    SDF <- SDF[order(in.sort)] 
  return (SDF)
}

callChangeFactor <- function(LH, CH, CF, args){
  #'The script uses the Quantile Mapping Change Factor
  #'(Ho, 2012) CDF to downscale coarse res. climate variables
  #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'@param args: named list of arguments for the function
    #     if(!is.null(args$size)){
    #       size <- args$size
    #       args$size <- NULL
    #     }else{
    size <- length(CF)
    # first define vector with probabilities [0,1]
    prob<-c(0.001:1:size)/size
    
    #Check for arg for specifying calendar order preservation
      in.sort <- order(CF)
      CF.out <- CF[in.sort]

    # QM Change Factor
    SDF<-quantile(CF.out,(ecdf(CH)(quantile(LH,prob))),names=FALSE)
    ##CEW: creation of historical quantiles turned off for the moment
    #SDH<-quantile(CH,(ecdf(CH)(quantile(LH,prob))),names=FALSE)
    #SDoutput<-list("SDF"=SDF,"SDH"=SDH)

      SDF <- SDF[order(in.sort)]
 
    return (SDF)
}

callDeltaSD <- function(LH,CH,CF,args){
  #'@author carlos.gaitan@noaa.gov
    #'@description The script uses the Delta Method to downscale coarse res. climate variables  
    #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'@param args: A list containing two arguments: deltatype, one of 'mean' or 'median', 
    #' which will be used to determine what single value will be used for the difference
    #' between the CH and CF, and deltaop, one of 'ratio' or 'add', which will be used 
    #' to determine the mthod for calculating the delta
    #'Uses the difference (ratio or subtraction) between CF and CH means or medians to calculate
    #' a delta that is applied to the LH (observational) data
    #'@return SDF: Downscaled Future (Local)
    ########################################
    # Delta Downscaling
    # 1) Calculate mean difference between CH and CF 
    if(!is.null(args$deltatype)){
      deltatype <- args$deltatype
    }else{
      stop(paste("DeltaSD Downscaling Error: deltatype not found in args"))
    }
    if(!is.null(args$deltaop)){
      deltaop <- args$deltaop
    }else{
      stop(paste("DeltaSD Downscaling Error: deltaop not found in args"))
    }
    if(deltaop=='add'){
      #Downscale by difference delta
      delta<-do.call(deltatype, list(CF))-do.call(deltatype, list(CH))
      message("ignore warning message; vector recycling in effect")
      CF[1:length(CF)] <- LH
      SDF<- CF+delta
    }else if(deltaop=='ratio'){
      #Downscale by percentage delta (never negative)
      delta<-do.call(OPT, list(deltatype))/do.call(deltatype, list(CH))
      message("ignore warning message; vector recycling in effect")
      CF[1:length(CF)] <- LH
      SDF<-CF*delta
    }else{
      stop(paste("DeltaSD Downscaling Error: deltaop", deltaop, "is not one of 'ratio' or 'add'"))
    }
    return (SDF)
}

# callNothing <- function(pred=NA, targ=NA, new=NA, args=NA){
#   #Does absolutely nothing to the downscaling values of the current 
#   #function. 
# #   print('inside method')
# #   print(summary(new))
#   return(new)
# }

