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
#' @examples
#' sample_t_predict <- seq(1:365)
#' sample_t_target <- sin(sample_t_predict*0.05)
#' sample_esd_gen <- seq(1:365)
#' mask_list <- list("/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
#'                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
#'                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc")
#'                   d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
#' esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
#' downscale.args=NULL,
#' masklist = mask_list)
#' lines(seq(1:365), d_data, col="cyan")
#' d2_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
#'                                  esd.gen = sample_esd_gen, kfold = 2, downscale.fxn = "ESD.Train.totally.fake", 
#'                                  downscale.args=NULL,
#'                                  masklist = alt_mask_list)
#' lines(seq(1:365), d2_data, col="magenta")
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
#' MORE INFO NEEDED: At the moment, the checks take place in the CrossValidate function. Calls elsewhere
#' migth make sense; it's something to discuss once the driver script is working.
DownscaleByTimeWindow <- function(train.predictor, train.target, esd.gen, 
                                  downscale.fxn, downscale.args = NULL, kfold, masklist, debug=FALSE){
  #May be advisable to hold fewer masks in memory. Can move some of the looping code to compensate.
  #At the present time, it might make more sense to call the more complicted fxns from elsewhere.
  #source("../../FudgePreDS/ApplyTemporalMask.R")
  #source("MaskMerge.R")
  #source("CrossValidate.R")

  t.predictor <- ApplyTemporalMask(train.predictor, masknc=masklist[[1]], run=(kfold > 2)) #This option means that
  t.target <-ApplyTemporalMask(train.target, masknc=masklist[[2]])                         #pred.masks cannot collide
  new.predictor <- ApplyTemporalMask(esd.gen, masknc=masklist[[3]], run=TRUE)              #for k > 1
  num.masks <- length(t.predictor)
  out.chunk <- as.list(rep(NA, length(esd.gen)))
  output <-list(rep(out.chunk, num.masks))      #Pre-allocate output vector for speed and meory efficency
  
#  plot(train.predictor, train.target*15, type="n", main=paste("Mask and lines of best fit for k=", kfold, sep=""))
#  lines(train.predictor, train.target)
#  plot(train.predictor, train.target*2, type="n", main=paste("Mask and lines of best fit for kfold crossval"))
#  lines(train.predictor, train.target)
  if(debug){
    mask.cols = rainbow(num.masks)
    fit.cols = rainbow(num.masks*kfold)
    plot(seq(1:length(train.target)), train.target, type = "l", lwd = 3, main=paste("Mask and lines of best fit for time windowing"))
  }
  for (window in 1:num.masks){
    print(paste("starting on window", window, "of", num.masks))
    print("********")
    window.predict <- t.predictor[[window]]
    window.target <- t.target[[window]]
    window.gen <- new.predictor[[window]]
    output[[window]] <- window.gen

    if (sum(!is.na(window.predict))!=0 && sum(!is.na(window.target))!=0 && sum(!is.na(window.gen))!=0){
      #If there aren't any entire series of missing data, perform downscaling on the series
      output[[window]][!is.na(window.gen)] <- DownscaleWithAllArgs(ds.method = downscale.fxn,
                                                                   train.predict = window.predict[!is.na(window.predict)], 
                                                                   train.target = window.target[!is.na(window.target)], 
                                                                   esd.gen = window.gen[!is.na(window.gen)], 
                                                                   args=NULL)
      if(debug){
        #       abline(v=which(!is.na(new.predictor[[window]]))[1])                       #Option for plotting masks as lines on graph
        lines(seq(1:length(window.gen)), output[[window]], lty = window, lwd = 4, col=mask.cols[window])
      }
      
    }else{
      #Otherwise, you don't need to do anything because that loop should be full of NAs
      print(paste("Too many NAs in loop", window, "of", num.masks, "; passing loop without downscaling"))
    }
    #output[[window]] <- tempout
    print("********")
  }
  
#    abline(v=which(!is.na(new.predictor[[window]]))[1])                       #Option for plotting masks as bars on graph
    #lines(train.predictor, t.target[[window]], col=mask.cols[[window]])     #option for color-coding masks
  print("Merging masks from all time series")
  out.merge <- MaskMerge(output, collide=TRUE)
#   if(debug){
#     legend(legend = c(as.character(seq(1:num.masks))), pch = c(1:num.masks), col = mask.cols, "bottomright")
#   }
  return(out.merge)
}