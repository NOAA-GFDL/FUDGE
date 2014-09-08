QCTimeWindowList <- function(tmask.list, k=0){
  #All members of training (train predictor and train target)
  #should have same length and same start/end date
  #message("Attempting time comparison")
  if ( (tmask.list$train.pred$dim$time[1] != tmask.list$train.targ$dim$time[1]) ||
         (tmask.list$train.pred$dim$time[length(tmask.list$train.pred$dim$time)] != tmask.list$train.targ$dim$time[length(tmask.list$train.targ$dim$time)]) ){
    stop(paste("Training period time error: The start and end dates of the training target", 
               tmask.list$train.targ$dim$time[1], tmask.list$train.targ$dim$time[length(tmask.list$train.targ$dim$time)],
               "are not the same as the start and end dates of the training predictor,", 
               tmask.list$train.pred$dim$time[1], tmask.list$train.pred$dim$time[length(tmask.list$train.pred$dim$time)]))
  }
  #When k > 1, both training and esdgen will have the same length
  #and start/end date
  message("Passing k > 1 time comparison")
  if (k > 1){
    if ( (tmask.list$train.pred$dim$time[1] != tmask.list$esd.gen$dim$time[1]) ||
           (tmask.list$train.pred$dim$time[length(tmask.list$train.pred$dim$time)] != tmask.list$esd.gen$dim$time[length(tmask.list$esd.gen$dim$time)]) ){
      stop(paste("K > 1 Time Period Error: The start and end dates of the training period", 
                 tmask.list$esd.gen$dim$time[1], tmask.list$esd.gen$dim$time[length(tmask.list$esd.gen$dim$time)],
                 "are not the same as the start and end dates of the generation period,", 
                 tmask.list$train.pred$dim$time[1], tmask.list$train.pred$dim$time[length(tmask.list$train.pred$dim$time)]))
    }
  }
  #At present, all mask files need to have the same number of masks present 
  #within the file
  message("Checking for same numbers of masks within files")
  if (length(tmask.list$train.pred$masks)!= length(tmask.list$train.targ$masks) || 
        length(tmask.list$train.targ$masks)!= length(tmask.list$esd.gen$masks)){
    stop(paste("Time mask dimension error: time mask files are expected to have the", 
               "same number of masks per file, but", attr(tmask.list$train.pred, "filename"), "had", 
               length(tmask.list$train.pred$masks), ",", attr(tmask.list$train.targ, "filename"), "had",
               length(tmask.list$train.targ$masks), ",", "and", attr(tmask.list$esd.gen, "filename"), "had", 
               length(tmask.list$esd.gen$masks)))
  }
}