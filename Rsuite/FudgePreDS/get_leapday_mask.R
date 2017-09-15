get.leapday.mask <- function(time.string, calendar="julian", gcp=T){
  #Builds a pth to a file of leapday mask data
  #since we currently copy the ENTIRE FILE PATH.
  #time.string: 19790101-20081231
  #valendar: julian
  noleap.strings <- c("noleap", "NOLEAP", "365", "365-day", 
                      "360", "360-day")
  if ( !(calendar%in%noleap.strings) ){
    base.leap.path <- "/archive/esd/PROJECTS/DOWNSCALING/MASKS/timemasks/"
    filename <- paste0('maskdays_missingleapdays_', time.string, ".nc")
    tmpdir = Sys.getenv("TMPDIR")
    leap.file <- file.path(tmpdir, base.leap.path, filename)
    print(file.exists(leap.file))
    print(leap.file)
    if (file.exists(leap.file)){
      gcp.tmpdir(file.path(base.leap.path, filename))
      #otherwise, read your data
      return(ReadMaskNC(nc_open(leap.file))$masks[[1]])
    }else{
#       print("No leapmask found; generating mask on the fly")
#       start.date <- time.string[1] + "01-01"
#       end.date <- time.string[2] + "12-31"
#       time.data <- as.PCICt(cal=calendar, origin=origin)
 #      return(make.leapday.mask(time.data))
    }
  }else{
    #If your calendar has no leapdays, you simply need to return
    #a vector of all missing values
    #Honestly, for the period of time that we are going to be using this 
    #hack, we are probably okay doing a naiive assumption of 365 days per year
    year.str <- sapply(strsplit(time.string, '-'), substr, 1, 4)
    if ( calendar %in% c("360", "360-day")){
        return(rep(1, (as.numeric(year.str[2]) - as.numeric(year.str[1]) + 1) * 360 ))
    }else{
        return(rep(1, (as.numeric(year.str[2]) - as.numeric(year.str[1]) + 1) * 365 ))
    }    
  }
}

make.leapday.mask <- function(timeseries){
  #Given a timeseries formatted in a date-like format (such as PCICt), 
  #creates a mask with NAs on all Feb. 29ths.
  leapdays <- which(format(timeseries, "%m-%d")=="02-29")
  mask <- rep(1, length(timeseries))
  mask[leapdays] <- NA
  return(mask)
}

gcp.tmpdir <- function(file, verbose=F){
  #'Calls the GFDL-specific gcp function
  #'to copy files from /archive to /tmp 
  TMPDIR = Sys.getenv(c("TMPDIR"))
  gcp.file <- file.path(TMPDIR,file)
  if(verbose){
    message(paste("copying", file, "to", gcp.file))
  }
  ret <- system2("gcp", c("-cd", file, gcp.file), stdout=T, stderr=T)
  return(gcp.file)
}

get.year.divisions <- function(start_string, end_string, calendar='julian'){
  #Gets the location of all Januray 1sts in an input dataset
  #based off of start and end strings
  #i.e. 20951231
  start_date <- paste(substr(start_string, 1, 4), 
                      substr(start_string, 5, 6), 
                      substr(start_string, 7, 8), sep="-")
  end_date <- paste(substr(end_string, 1, 4), 
                    substr(end_string, 5, 6), 
                    substr(end_string, 7, 8), sep="-")
  time_span <- as.numeric((get.PCICt.time.series(end_date, 0, calendar) - 
                             get.PCICt.time.series(start_date, 0, calendar))/86400)
  time.vals <- 0:(time_span)
  time.dates <- get.PCICt.time.series(start_date, time.vals, calendar)
#   print(time_span)
#   print(time.dates[1:100])
#   print(format(time.dates, '%m-%d')[1:100])
  jan.dates <- which(format(time.dates, '%m-%d')=='12-31') #was 01-01
  return(jan.dates)
}

get.PCICt.time.series <- function(origin, time.data, calendar){
  #Returns a fast and dirty date conversion into the PCICt time
  #series format, whcih contains noleap data
  julian.strings <- c("JULIAN", 'julian', "Julian", 'gregorian', 'proleptic_gregorian', 'standard', 'STANDARD', 'Standard')
  if(calendar%in%julian.strings){
    calendar <- 'gregorian'
  }
  seconds.per.day <- 86400
  origin <- sub("days since ", "", origin)
  origin <- sub(" 00:00:00", "", origin)
  print(origin)
  origin.pcict <- as.PCICt(origin, calendar)
  return(origin.pcict + (time.data * seconds.per.day))
}
