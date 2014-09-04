QCTimeMask<-function(all.masks, run=FALSE){
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
  #   all.masks <- ReadMaskNC(thisnc)
  time.length <- length(all.masks$dim$time)
  ###Pre-allocaate vector and loop over the available masks
  checkvector <- rep(0, time.length)
  #Loop over the names of each mask in the file
  mask.names <- names(all.masks$masks)
  for (mask in 1:length(all.masks$masks)){
    mask.data <- all.masks$masks[[mask]]
    if(length(mask.data) != time.length){
      warning(paste("Mask Time Dimension Warning: mask", mask.names[mask], 
                    "within", "this nc file", "had length of", length(mask.data),
                    "while time dimension had length of", time.length))
    }
    #Implement overlap checks if run==TRUE
    if(run){                          
      checkvector <- checkvector + convert.NAs(mask.data)
      if(max(checkvector) > 1){
        stop(paste("Mask collision error: Masks within the first", mask, "masks of", "this nc file", #Add file name attribute for error messages
                   ", provided as an ESD generation mask file", 
                   "overlap along the time series."))
        
      }
    }
  }
  return(all.masks)
}

#Converts NAs to 0, and all non-NA values to 1
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(as.vector(dataset2))
}