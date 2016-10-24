get.leapday.mask <- function(time.string, calendar="julian", gcp=T){
  #Builds a pth to a file of leapday mask data
  #since we currently copy the ENTIRE FILE PATH.
  #time.string: 19790101-20081231
  #valendar: julian
  noleap.strings <- c("noleap", "NOLEAP", "365", "365-day")
  if ( !(calendar%in%noleap.strings) ){
    base.leap.path <- "/archive/esd/PROJECTS/DOWNSCALING/MASKS/timemasks/"
    filename <- paste0('maskdays_missingleapdays_', time.string, ".nc")
    tmpdir = Sys.getenv("TMPDIR")
    leap.file <- file.path(tmpdir, base.leap.path, filename)
    if (!(file.exists(leap.file))){
      gcp.tmpdir(file.path(base.leap.path, filename))
    }
    #otherwise, read your data
    return(ReadMaskNC(nc_open(leap.file))$masks[[1]])
  }else{
    #If your calendar has no leapdays, you simply need to return
    #a vector of all missing values
    #Honestly, for the period of time that we are going to be using this 
    #hack, we are probably okay doing a naiive assumption of 365 days per year
    year.str <- sapply(strsplit(time.string, '-'), substr, 1, 4)
    return(rep(1, (as.numeric(year.str[2]) - as.numeric(year.str[1]) + 1) * 365 ))
  }
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