QCTimeMask<-function(time.mask.obj, run=FALSE){
  #Given a path to a filename, returns all masks
  #within that file, along with the timeseries and the information needed to 
  #generate it at a later date.
  ###Assumes that:
  #The data has a calendar attribute, which is one of '365', 'noleap', '360' or 'gregorian'
  #The data is in 'days since' form
  #The data has a dimension actually named 'time'
  ####TODO: Get the read functions to add to the filename as an attribute when reading in files, 
  ####which is probably going to require an edit to the OpenNC and readNC functions...

#   library(ncdf4)
#   library(PCICt)
  message("Checking time windowing mask")
  #   thisnc <- nc_open(mask.nc)
  #   time.mask.obj <- ReadMaskNC(thisnc)
  time.length <- length(time.mask.obj$dim$time$vals)
  ###Pre-allocaate vector and loop over the available masks
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