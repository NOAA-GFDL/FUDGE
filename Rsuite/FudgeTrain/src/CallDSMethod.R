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
#' TODO: Find a better name for this function
#' TODO: Integrate properly with the 
#' 

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL){
  library(CDFt)
  return(switch(ds.method, 
                "simple.lm" = callSimple.lm(train.predict, train.target, esd.gen),
                'CDFt' = callCDFt(train.predict, train.target, esd.gen, args),
               # 'CDFtv1' = callCDFt(train.target, train.predict, esd.gen, npas=34333)$DS,  #This takes *SIX TIMES* as long to run
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