QCTimeWindowList <- function(tmask.list, k=0){
  #All members of training (train predictor and train target)
  #should have same length and same start/end date
  #message("Attempting time comparison")
  #tmask.names <- names(tmask.list)
  if ( (tmask.list[[1]]$dim$time[1] != tmask.list[[2]]$dim$time[1]) ||
         (tmask.list[[1]]$dim$time[length(tmask.list[[1]]$dim$time)] != tmask.list[[2]]$dim$time[length(tmask.list[[2]]$dim$time)]) ){
    stop(paste("Training period time error: The start and end dates of the training target", 
               tmask.list[[2]]$dim$time[1], tmask.list[[2]]$dim$time[length(tmask.list[[2]]$dim$time)],
               "are not the same as the start and end dates of the training predictor,", 
               tmask.list[[1]]$dim$time[1], tmask.list[[1]]$dim$time[length(tmask.list[[1]]$dim$time)]))
  }
  #When k > 1, both training and esdgen will have the same length
  #and start/end date
  message("Passing k > 1 time comparison")
  if (k > 1){
    if ( (tmask.list[[1]]$dim$time[1] != tmask.list[[3]]$dim$time[1]) ||
           (tmask.list[[1]]$dim$time[length(tmask.list[[1]]$dim$time)] != tmask.list[[3]]$dim$time[length(tmask.list[[3]]$dim$time)]) ){
      stop(paste("K > 1 Time Period Error: The start and end dates of the training period", 
                 tmask.list[[3]]$dim$time[1], tmask.list[[3]]$dim$time[length(tmask.list[[3]]$dim$time)],
                 "are not the same as the start and end dates of the generation period,", 
                 tmask.list[[1]]$dim$time[1], tmask.list[[1]]$dim$time[length(tmask.list[[1]]$dim$time)]))
    }
  }
  #At present, all mask files need to have the same number of masks present 
  #within the file
  message("Checking for same numbers of masks within files")
  if (length(tmask.list[[1]]$masks)!= length(tmask.list[[2]]$masks) || 
        length(tmask.list[[2]]$masks)!= length(tmask.list[[3]]$masks)){
    stop(paste("Time mask dimension error: time mask files are expected to have the", 
               "same number of masks per file, but", attr(tmask.list[[1]], "filename"), "had", 
               length(tmask.list[[1]]$masks), ",", attr(tmask.list[[2]], "filename"), "had",
               length(tmask.list[[2]]$masks), ",", "and", attr(tmask.list[[3]], "filename"), "had", 
               length(tmask.list[[3]]$masks)))
  }
}