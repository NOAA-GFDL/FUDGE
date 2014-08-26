#'Reads in time masks and performs basic QC, including checks between the masks
#'to make sure there are the same number of masks per file.
#' @param hist.train.mask
#' @param hist.targ.mask
#' @param esd.gen.mask: Mask to apply to the esdgen datasets. Needs to be checked
#' for overlapping masks as well. 
#' @param method=NULL: The method included, if it will change the checks that need to
#' be performed on the intial data. CDFt, for example, currently has a check for 
#' overlapping masks on all predictors
#' @return A list containing (in this version) three elements (one for the hist.train,
#' hist.targ and fut.train), each containing the masks of the file, the names of the
#' masks, and the timeseries associated with each mask (used in cross-validation)
#' @example insert example here
#' @references \url{link to the FUDGE API documentation}
#' TODO: set function to perform checks for more than one esdgen dataset
#' TODO: Is there a good way to preallocate lists for speeed in this place?
#' TODO: Develop better mask comparison when more than one mask is present
#' TODO: Do a check for Julian calendars over the relevant time windows

TimeMaskQC <- function(hist.train.mask, hist.targ.mask, esd.gen.mask, k=0, method=NULL){
  #This is the list of methods that use all arguments in order to generate the esd
  #equations, and therefore should probably be run without
  #overlapping time windows
  use.all.list <- c("CDFt")
  if (!method%in%use.all.list){   #If method uses only historical data to generate eq's
    t.pred.masks <- ObtainAllMasks(hist.train.mask)
    t.targ.masks <- ObtainAllMasks(hist.targ.mask)
    esd.gen.masks <- ObtainAllMasks(esd.gen.mask, run=TRUE)
  }else{                          #If method uses historic and future data to generate eq's
    t.pred.masks <- ObtainAllMasks(hist.train.mask, run=TRUE)
    t.targ.masks <- ObtainAllMasks(hist.targ.mask, run=TRUE)
    esd.gen.masks <- ObtainAllMasks(esd.gen.mask, run=TRUE)
  }
  #All members of training (train predictor and train target)
  #should have same length and same start/end date
  if ( (t.pred.masks$time[1] != t.targ.masks$time[1]) ||
      (t.pred.masks$time[length(t.pred.masks$time)] != t.targ.masks$time[length(t.targ.masks$time)]) ){
    stop(paste("Training period time error: The start and end dates of the training target", 
               t.targ.masks$time[1], t.targ.masks$time[length(t.targ.masks$time)],
               "are not the same as the start and end dates of the training predictor,", 
               t.pred.masks$time[1], t.pred.masks$time[length(t.pred.masks$time)]))
  }
  #When k > 1, both training and esdgen will have the same length
  #and start/end date
  if (k > 1){
    if ( (t.pred.masks$time[1] != esd.gen.masks$time[1]) ||
           (t.pred.masks$time[length(t.pred.masks$time)] != esd.gen.masks$time[length(esd.gen.masks$time)]) ){
      stop(paste("K > 1 Time Period Error: The start and end dates of the training period", 
                 esd.gen.masks$time[1], esd.gen.masks$time[length(esd.gen.masks$time)],
                 "are not the same as the start and end dates of the generation period,", 
                 t.pred.masks$time[1], t.pred.masks$time[length(t.pred.masks$time)]))
    }
  }
  #At present, all mask files need to have the same number of masks present 
  #within the file
  if (length(t.pred.masks)!= length(t.targ.masks) || 
        length(t.targ.masks)!= length(esd.gen.masks)){
    stop(paste("Time mask dimension error: time mask files are expected to have the", 
               "same number of masks per file, but", hist.train.mask, "had", 
               length(t.pred.masks)-1, ",", hist.targ.mask, "had",
               length(t.targ.masks)-1, ",", "and", esd.gen.mask, "had", 
               length(esd.gen.masks)-1))
  }
  tmask.list <- list("train.pred" = t.pred.masks, "train.targ" = t.targ.masks, "esd.gen" = esd.gen.masks)
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
                  "used as a source of time masks, does not have a calendar", 
                  "underneath this$dim$time$calendar."))
  }
  if (calendar %in% c('Julian', 'julian', "JULIAN")){
    all.time <- CheckJulian(calendar, thisnc$dim$time$vals, origin)
  } else if (!calendar%in%c('gregorian', 'proleptic_gregorian', '365-day', 'noleap', '360', '360-day')){
    warning(paste("NetCDF formatting warning: file", mask.nc, "used as a source of time masks,",
                  'contained calendar', calendar,',not recognized by PCICt'))
  }else{
    origin.time <- as.PCICt(origin, calendar)
    all.time <- origin.time + thisnc$dim$time$vals * 86400  #Converts to a timeseries
  }
  time.length <- length(all.time)
  ###Pre-allocaate vector and loop over the available masks
  out.list <- list('time' = all.time)
  mask.list <- list()
  checkvector <- rep(0, length(mask.list$time))
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
      #Something is triggering this with all NA arguments 36 times during the run
      if(max(checkvector) > 1){
        stop(paste("Mask collision error: Masks within the first", name, "masks of", mask.nc,
                   ", provided as an ESD generation mask file", 
                   "overlap along the time series."), .call=TRUE)
      }
    }
  } ##Is it neccesary to do the second check? Ask Keith.
  ##Important to note that the current round of checks happens to trip this one on hist.train.mask
#   if (run && sum(checkvector != time.length)){
#     stop(paste("Mask Gap Error: Masks within the file", mask.nc, 
#                ",provides as an ESD generation mask file, do not wholly cover the time series."))
#   }
  out.list$masks <- mask.list
  return(out.list)
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

CheckJulian <- function(calendar, timeseries, origin){
  #Checks to see whether or not the Julian calendar
  #is supported over the date range, and returns
  #the converted time series if that is the case.
  time.origin <- as.PCICt(origin, 'gregorian')
  startdate <- time.origin + timeseries[1]*86400
  enddate <- time.origin + timeseries[length(timeseries)]*86400
  if (startdate >= as.PCICt("1900-01-01 0:00:00","gregorian") && 
        enddate <= as.PCICt("2099-12-31 23:59:59", "gregorian")){
    return(time.origin + timeseries * 86400)
  }else{
    warning(paste("Julian calendar error: PCICt does not support", 
               "the Julian calendar over range", startdate,"to",enddate))
  }
}
