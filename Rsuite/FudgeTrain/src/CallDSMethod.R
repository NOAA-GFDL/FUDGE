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
#' @examples 
#' @references \url{link to the FUDGE API documentation} 
#' TODO: Find a better name for general.bias.corrector

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL){
#  library(CDFt)
  return(switch(ds.method, 
                "simple.lm" = callSimple.lm(train.predict, train.target, esd.gen),
                'CDFt' = callCDFt(train.predict, train.target, esd.gen, args),
                'simple.bias.correct' = callSimple.bias.correct(train.predict, train.target, esd.gen, args),
                'general.bias.correct' = callGeneral.Bias.Corrector(train.predict, train.target, esd.gen, args),
                'Nothing' = callNothing(train.predict, train.target, esd.gen, args),
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
  if(is.null(args)){
    return(CDFt(targ, pred, new, npas=length(targ))$DS)
  }else{
    ##Note: if any of the input data parameters are named, CDFt will 
    ## fail to run with an 'unused arguments' error, without any decent
    ## explanation as to why. This way works.
    if(!'npas'%in%names(args)){
      args <- c(npas=length(targ), args)
    }
    args.list <- c(list(targ, pred, new), args)
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

callNothing <- function(pred, targ, new, args){
  #Does absolutely nothing to the downscaling values of the current 
  #function. 
  return(new)
}

##########Section for PP methods: Methods that take a dataset, adjust its values somehow, and 
##########produce a vector of the same type. Note that there ***might*** be an overlap with the
##########downscaling methods, but that's slightly incidental. 

#'Methods in this section gernally assume four arguments: 
#'@param data: The data to be adjusted. Generally a product of a previous
#'downscaling run. 
#'@param check: A vector of 1's and 0's representing the results of a 
#'previous QC check on the data vector. A 1 means that the data passed,
#' a 0 means that it failed.
#'@param check.data: An optional parameter for some methods. Instead of
#'attempting to correct the data parameter, the check.data vector is used
#'to substitute for the corresponding value in data.
#'@args: Optional arguments for the adjustment function. 

postProc_byCheck <- function(data, check, check.data, args){
  #TODO: At some point, include the var post-processing option
  #and some sort of units check to go with it.
  if(!is.null(args$compare.factor)){
    correct.factor <- args$compare.factor
    args$compare.factor <- NULL
  }else{
    correct.factor = 6
  }
  if(length(args)!=0) sample.args=args else sample.args=NULL
  out.vals <- ifelse( (check==1), yes=data, no=check.data )
  return(out.vals)
}
