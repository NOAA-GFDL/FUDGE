
get.PCICt.time.range <- function(origin, time.data, calendar){
  #returns the start and end dates of a time series
  #given the calendar and origin
  if(!is.null(calendar) && calendar!='none'){
    pcict.series <- get.PCICt.time.series(origin, time.data, calendar)
  }else{
    pcict.series <- c(0,0)
  }
  return(c(pcict.series[1], pcict.series[length(pcict.series)]))
}

get.cal.strings <- function(env=parent.frame()){
  #'Returns all possible names for 3 different kinds of calendar
  #'to the current environment: 
  #'julian.strings: calendars with leap years that are identical from
  #'  1900-01-01 to 2099-12-31
  #'noleap.strings: calendars that use 365 days with no Feb. 29
  #'cal360.strings: the 360-day calendar. 
  #'Roughly correlated with the PCICt package, since we use that for most
  #'of our date conversions
  env$julian.strings <- c("JULIAN", 'julian', "Julian", 
                          'gregorian', 'proleptic_gregorian', 
                          'standard', 'STANDARD', 'Standard')
  env$noleap.strings <- c("365_day", "365", "noleap", "Noleap","NOLEAP", '365-day', "365 day")
  env$cal360.strings <-  c("360_day", "360", "360-day", "360 day")
  return(env)
}

get.PCICt.time.series <- function(origin, time.data, calendar){
  #Returns a fast and dirty date conversion into the PCICt time
  #series format, whcih contains noleap data
  get.cal.strings()
  if(calendar%in%julian.strings){
    calendar <- 'gregorian'
  }else if (calendar %in% noleap.strings){
    calendar <- 'noleap'
  }else if (calendar %in% cal360.strings){
    calendar <- "360_day"
  }else{
    stop(paste("Error in get.PCICt.time.series: calendar", calendar, 
               "not recognized! Please add to get.cal.strings()"))
  }
  if (grepl("days since ", origin)){
    seconds.per.day <- 86400
    origin <- sub("days since ", "", origin)
    origin <- sub(" 00:00:00", "", origin)
    origin.pcict <- as.PCICt(origin, calendar)
    return(origin.pcict + (time.data * seconds.per.day))
  }else{
    seconds.per.hour <- 3600
    origin <- sub("hours since ", "", origin)
    origin <- sub(" 00:00:00", "", origin)
    origin.pcict <- as.PCICt(origin, calendar)
    return(origin.pcict + (time.data * seconds.per.hour))
  }
}