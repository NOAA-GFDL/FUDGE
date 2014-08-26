#DownscaleWithAllArgs
#' DownscaleWithAllArgs
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
#' @examples (insert examples here)
#' @references \url{link to the FUDGE API documentation}
#' TODO: Find a better name for this function
#' TODO: Integrate properly with the 
#' 

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL){
  library(CDFt)
  return(switch(ds.method, 
                "simple.lm" = simple.nocross.lm(train.predict, train.target, esd.gen),
                'CDFt' = CDFt(train.target, train.predict, esd.gen, npas=length(esd.gen))$DS,
                'CDFtv1' = CDFt(train.target, train.predict, esd.gen, npas=34333)$DS,
                ReturnDownscaleError(ds.method)))
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

simple.nocross.lm <- function(pred, targ, new){
  #Creates a simple linear model without a cross-validation step. 
  #Mostly used to check that the procedure is working
  lm.results <- lm(targ ~ pred)
  lm.intercept <- lm.results$coefficients[1]
  lm.slope <- lm.results$coefficients[2]
  if(is.na(lm.intercept) || is.na(lm.slope) ){
    warning(paste("simple.nocross.lm warning: intercept was", lm.intercept, 
                  "and intercept was", lm.slope, ": therefore no ESD values will be generated."))
  }
  trained.function<-function(x){
    print(lm.intercept)
    print(lm.slope)
    return( lm.intercept + unlist(x)*lm.slope)
  }
  return(trained.function(new))
}