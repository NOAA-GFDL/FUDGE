#'QCTimeMask.R
QCTimeMask<-function(time.mask.obj, run=FALSE){
  #'Given a path to a filename, returns all variables containing the word 'mask'
  #'within that file, aling with the timeseries and the information needed to 
  #'generate the timeseries at a later date
  #'@param time.mask.object: 
  #'@param run: Will this mask be used for adding data back into the downscaled time
  #'series? If so, the masks cannot overlap. Defaults to FALSE (overlap allowed)
  #'@return A time mask that will not cause errors in the FUDGE workflow
  ###Assumes that:
  #The data has a calendar attribute, which is one of '365', 'noleap', '360' or 'gregorian'
  #The data is in 'days since' form
  #The data has a dimension actually named 'time'
    
  message("Checking time windowing mask")
  time.length <- length(time.mask.obj$masks[[1]])
  ###Pre-allocaate vector and loop over the available masks
  print(time.length)
  checkvector <- rep(0, time.length)
  #Loop over the names of each mask in the file
  mask.names <- names(time.mask.obj$masks)
  for (mask in 1:length(time.mask.obj$masks)){
    mask.data <- time.mask.obj$masks[[mask]]
    if(length(mask.data) != time.length){
      warning(paste("Mask Time Dimension Warning: mask", mask.names[mask], 
                    "within", attr(time.mask.obj, 'filename'), "had length of", length(mask.data),
                    "while time dimension had length of", time.length))
    }
    #Implement overlap checks if run==TRUE
    if(run){                          
      checkvector <- checkvector + convert.NAs(mask.data)
      if(max(checkvector) > 1){
        stop(paste("Mask collision error: Masks within the first", mask, "masks of", attr(time.mask.obj, 'filename'), 
                   ", provided as an ESD generation mask file", 
                   "overlap along the time series."))
        
      }
    }
  }
  return(time.mask.obj)
}

#Converts NAs to 0, and all non-NA values to 1
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(as.vector(dataset2))
}