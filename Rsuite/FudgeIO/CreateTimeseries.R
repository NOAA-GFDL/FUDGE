#'CreateTimeseries.R
#'@include PCICt
#'Given a series of numbers representing dates since a date, 
#'an origin that contains the date and a calendar, returns
#'a PCICt formatted time series
#'@param timeseries: A vector representing a series of days since
#'a date
#'@param origin: A date from which the timeseries are counted, presented
#'in "days since" format.
#'@param calendar: The calendar of the timeseries. Defaults to noleap. 
#'@param sourcefile; The path to the file from which these were obtained; 
#'used in printing helpful error messages. Defaults to "some file"
#'@return A vector of PCICt date obejcts, with a calendar attribute 
#'@examples


CreateTimeseries <- function(timeseries, origin, calendar, sourcefile="some file"){
  #Figure out how to include the source file in the function call
  #Check origin
  library(PCICt)
  message("Creating date series")
  seconds.per.day <- 86400
  if (is.null(origin)){
    warning(paste("NetCDF formatting warning: file", sourcefile, 
                  "used as a source of time coordinates, does not have units underneath ncobject$dim$time$units."))
  }
  origin <- substr(origin, regexpr("[[:digit:]]", origin)[1], nchar(origin))
  #Check calendar
  if (is.null(calendar)){
    warning(paste("NetCDF formatting warning: file", sourcefile, 
                  "used as a source of time coordinates, does not have a calendar", 
                  "underneath this$dim$time$calendar."))
  }
  if (calendar %in% c('Julian', 'julian', "JULIAN")){
    all.time <- CheckJulian(calendar, timeseries, origin)
    attr(all.time, "calendar") <- "julian"
  } else if (!calendar%in%c('gregorian', 'proleptic_gregorian', '365-day', 'noleap', '360', '360-day')){
    warning(paste("NetCDF formatting warning: file", sourcefile, "used as a source of time coordinates,",
                  'contained calendar', calendar,',not recognized by PCICt'))
  }else{
    origin.time <- as.PCICt(origin, calendar)
    all.time <- origin.time + timeseries * seconds.per.day  #Converts to a timeseries
    attr(all.time, "calendar") <- calendar
  }
  return(all.time)
}


CheckJulian <- function(calendar, timeseries, origin){
  #Checks to see whether or not the Julian calendar
  #is supported over the date range, and returns
  #the converted time series if that is the case.
  seconds.per.day <- 86400
  time.origin <- as.PCICt(origin, 'gregorian')
  startdate <- time.origin + timeseries[1]*seconds.per.day
  enddate <- time.origin + timeseries[length(timeseries)]*seconds.per.day
  if (startdate >= as.PCICt("1900-01-01 0:00:00","gregorian") && 
        enddate <= as.PCICt("2099-12-31 23:59:59", "gregorian")){
    return(time.origin + timeseries * seconds.per.day)
  }else{
    warning(paste("Julian calendar error: PCICt does not support", 
                  "the Julian calendar over range", startdate,"to",enddate))
  }
}