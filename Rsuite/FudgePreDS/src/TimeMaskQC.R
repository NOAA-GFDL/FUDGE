#'Reads in time masks and performs basic QC, including checks between the masks
#'to make sure there are the same number of masks per file.
#' @param hist.train.mask
#' @param hist.targ.mask
#' @param esd.gen.mask: Mask to apply to the esdgen datasets. Needs to be checked
#' for overlapping masks as well. 
#' @return A list containing (in this version) three elements (one for the hist.train,
#' hist.targ and fut.train), each containing the masks of the file, the names of the
#' masks, and the timeseries associated with each mask (used in cross-validation)
#' @example insert example here
#' @references \url{link to the FUDGE API documentation}
#' TODO: set function to perform checks for more than one esdgen dataset
#' TODO: Is there a good way to preallocate lists for speeed in this place?
#' TODO: Develop better mask comparison when more than one mask is present
#' TODO: Do a check for Julian calendars over the relevant time windows

MaskQC <- function(hist.train.mask, hist.targ.mask, esd.gen.mask){
  train.predict.masks <- ObtainAllMasks(hist.train.mask)
  train.target.masks <- ObtainAllMasks(hist.targ.mask)
  esd.gen.masks <- ObtainAllMasks(esd.gen.mask, run=TRUE)
  ###Think of a good if statment to check even when more than one esdgen dataset provided
  if (length(train.predict.masks)!= length(train.target.masks) || 
        length(train.target.masks)!= length(esd.gen.masks)){
    stop(paste("Time mask dimension error: time mask files are expected to have the", 
               "same number of masks per file, but", hist.train.mask, "had", 
               length(train.predict.masks)-1, ",", hist.targ.mask, "had",
               length(train.target.masks)-1, ",", "and", esd.gen.mask, "had", 
               length(esd.gen.masks)-1))
  }
  tmask.list <- list(train.predict.masks, train.target.masks, esd.gen.masks)
  return(tmask.list)
}

ObtainAllMasks<-function(mask.nc, run=FALSE){
  #Given a path to a filename, returns all masks
  #within that file, along with the timeseries and the information needed to 
  #generate it at a later date.
  ###Assumes that:
  #The data has a calendar attribute, which is one of '365', 'noleap', '360' or 'gregorian'
  #The data is in 'days since' form
  #The data has a dimension actually named 'time'
  ####TODO: Actually calculate the timeseries in this step. It can be used later, 
  ####But it's nto computationally intense and it's arguably more compact
  ####than carrying around the origin and the calendar.
  ####TODO: Can the kfold masking step be made a separate function to be called once?
  ####It really will be pretty much the same for all timeseries.
  #Should probably be initialized before the first loop, and then passed to the time masking function...
  library(ncdf4)
  library(PCICt)
  thisnc <- nc_open(mask.nc)
  mask.names <- RemoveBounds(names(thisnc$var))
  ##Create a timeseries (used for subsetting in kfold cross-validation)
  origin <- thisnc$dim$time$units
  if (is.null(origin)){
    warning(paste("NetCDF formatting warning: file", mask.nc, 
                  "used as a source of time masks, does not have units underneath this$dim$time$units."))
  }
  origin <- substr(origin, regexpr("[[:digit:]]", origin)[1], nchar(origin))
  calendar <- thisnc$dim$time$calendar
  if (is.null(calendar)){
    warning(paste("NetCDF formatting warning: file", mask.nc, 
                  "used as a source of time masks, does not have a calendar underneath this$dim$time$calendar."))
  }
  if (!calendar%in%c('gregorian', 'proleptic_gregorian', '365-day', 'noleap', '360', '360-day')){
    warning(paste("NetCDF formatting warning: file", mask.nc, "used as a source of time masks,",
                  'contained calendar', calendar,',not recognized by PCICt'))
  }
  origin.time <- as.PCICt(origin, calendar)
  all.time <- origin.time + thisnc$dim$time$vals * 86400  #Converts to a timeseries
  time.length <- length(all.time)
  ###Pre-allocaate vector and loop over the available masks
  mask.list <- list('timeseries' = all.time)    #Is there a better way to initialize this list?
  checkvector <- rep(0, length(out$timeseries))
  #Loop over the names of each mask in the file
  for (name in 1:length(mask.names)){
    mask.data <- ncvar_get(thisnc, mask.names[name])
    if(length(mask.data) != time.length){
      warning(paste("Mask Time Dimension Warning: mask", mask.names[name], 
                    "within", mask.nc, "had length of", length(mask.data),
                    "while time dimension had length of", time.length))
    }
    mask.list[[mask.names[name]]] <- mask.data
    #Implement overlap checks if run==TRUE
    if(run){                          
      checkvector <- checkvector + convert.NAs(mask.data)
      if(max(checkvector) > 1){
        stop(paste("Mask collision error: Masks within the first", name, "masks of", mask.nc,
                   ", provided as an ESD generation mask file", 
                   "overlap along the time series."))
      }
    }
  } ##Is it neccesary to do the second check? Ask Keith.
  if (run && sum(checkvector != time.length)){
    stop(paste("Mask Gap Error: Masks within the file", mask.nc, 
               ",provides as an ESD generation mask file, do not wholly cover the time series."))
  }
  out$data[["mask.list"]] <- mask.list
  return(out)
}

#Removes anything that might not be a data variable from a list of 
#names generated by names(thisnc$var)
#I know, it's barely a function; however, it does mean it's easy
#to chain on more conditionals as things progress. And it gets used
#twice.
RemoveBounds<-function(names){
  return(names[names!="lon_bnds"&names!="lat_bnds"&names!="time_bnds"&
                 names!="i_offset"&names!="j_offset"&names!="height"])
}

#Converts NAs to 0, and all non-NA values to 1
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(as.vector(dataset2))
}
