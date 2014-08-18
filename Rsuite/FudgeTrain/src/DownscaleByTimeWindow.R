#DownscaleByTimeWindow.R
#Written by Carolyn Whitlock, August 2014
#'Calls temporal masking functions, given a set of time data
#'and a mask for each function.
#'Then, calls CrossValidate for each masked window
#'and merges the downscaled results it gets back
#'into a single time series
#'
#'####Arguments related to downscaling data:
#'@param train.predictor
#'@param train.target
#'@param esd.gen: a single file or a list of files pointing to the data to which masks should be applied. Currently, 
#'only a single esd.gen vector is supported.
#'
#'####Arguments related to the CrossValidate call:
#'@param kfold: the k-fold cross-validation. Note that *ONLY* if k=0 will downscaling
#'equations be called on the esd.gen dataset(s).
#'@param downscale.fxn: A string referring to the downscaling function to be called.
#'@param downscale.args: A list of the arguments to the downscaling function. 
#'
#'####Arguments related to the masking function:
#'@param masklist: a list of pathnames pointing to the files which contain masks to apply. If none are provided, 
#'defaults to running all datasets without temporal masks of any sort.
#'Currently, the code assumes that all maskfiles will have the same number of masks within them
#'to apply; this assumption needs to be talked about at some point.
#'
#'@return A single timeseries containing all downscaled data.
#'####
#' @example insert example here
#' @references \url{link to the FUDGE API documentation}
#' TODO: Check on assumption that all maskfiles will have the same number of masks
#' RESOLVED: They may not in later versions, but it's a valid 1^5 assumption.
#' TODO: Do train.predictor and train.target need to be able to accept lists?
#' RESOLVED: Yes they do, but not for 1^5.
#' TODO: Check on how to determine which masks go with which data. Actually, check with MJ
#' on how the time pruning might work.
#' TODO: Figure out how the sourcing/wrappers are going to work, as well as the args input.
#' TODO: Also, is this the place to start doing simple checks to avoid calling the fxn if all points
#' are NA? Or do we do that twice: once for lat/lon, and then again for the time masking?
DownscaleByTimeWindow <- function(train.predictor, train.target, esd.gen, 
                                  downscale.fxn, downscale.args = NULL, kfold, masklist){
  #May be advisable to hold fewer masks in memory. Can move some of the looping code to compensate.
  #At the present time, it might make more sense to call the more complicted fxns from elsewhere.
  #source("../../FudgePreDS/ApplyTemporalMask.R")
  #source("MaskMerge.R")
  #source("CrossValidate.R")
  t.predictor <- ApplyTemporalMask(train.predictor, masknc=masklist[[1]])
  t.target <-ApplyTemporalMask(train.target, masknc=masklist[[2]])
  new.predictor <- ApplyTemporalMask(esd.gen, masknc=masklist[[3]], type="run")
  num.masks <- length(t.predictor)
  #This is not how you pre-allocate a vector. Please work on it later.
  out.chunk <- as.list(rep(NA, length(esd.gen)))
  output <-list(rep(out.chunk, num.masks))      #Pre-allocate output vector for speed and meory efficency
  for (window in 1:num.masks){
    print(paste("starting on window", window, "of", num.masks))
    output[[window]] <- CrossValidate(train.predict = t.predictor[[window]], train.target = t.target[[window]], 
                                      esd.gen = new.predictor[[window]], 
                                      k = kfold, downscale.function = downscale.fxn, args = downscale.args)
  }
  print("Merging masks from all time series")
  out.merge <- MaskMerge(output, collide=TRUE)
  return(out.merge)
}