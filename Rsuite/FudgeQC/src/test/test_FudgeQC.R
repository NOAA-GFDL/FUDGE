#test_FudgeQC.R
#' Tests all the FudgeQC functions
#' with a few diversions to test the 
#' reading, writing and organization funcitons
#' that create the data structures that FudgeQC
#' will check for inclusion in 
#' 

library(RUnit)

####Code to test

#SourceReadNC up here, ReadMaskNC,. CreateTimeWindowList
#Set the working directory up here as well, and read in PCICt and ncdf4

test_QCIO <- function(){
  output.directory <- "does.not.exist"
  CheckException(QCIO(output.directory), "Output directory error: Directory at does.not.exist does not exist. Please check the path")
}

test_QCDSArguments <- function(){
  print("Testing for cross-validation not possible error")
  crossval.possible <- FALSE
  k <- 2
  checkException(QCDSArguments('CDFt', k), paste("Cross-Validation Conflict Error: Method CDFt",
                                                 "does not support cross-validaiton.", 
                                                 "Check documentation for more details."))
}

test_QCInputData <- function(){
  ###Initialize common variables
  train.target.filename <- "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/atmos/day/r0i0p0/v1p2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I300_J31-170.nc"
  train.predict.filename <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/historical/atmos/day/r1i1p1/v20111006/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_historical_r1i1p1_SCCSC0p1_19610101-20051231.I300_J31-170.nc "
  esd.gen.filename <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc"
  print("Testing for all missing values in input data error")
  all.missvals <- ""
  print("Testing for missing value threshold error")
  too.many.missvals <- ""
  print("Testing for calendar mismatch error")
  train.predictor <- ReadNC(nc_open(train.predict.filename), "tasmax")
  train.target <- ReadNC(nc_open(train.target.filename), "tasmax")
  esd.gen <- ReadNC(nc_open(esd.gen.filename), "tasmax")
  checkException(QCInputData(train.predictor, train.target, esd.gen, k=0, calendar="noleap"), 
                 paste("Calendar mismatch error: train.predictor read in from",
                       "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/atmos/day/r0i0p0/v1p2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I300_J31-170.nc", 
                       "had a calendar attribute of julian and an expected calendar attribute of noleap"))
  ##Now entering dataset consistency checks
  print("Testing for training period spatial dimension mismatch error")
  shortened.training.file <- ""
  print("Testing for k > 1 spatial dimension mismatch error")
  shortened.esd.gen.file <- ""
  print("Testing for training time period error")
  short.time.range.training <- ""
  print("Testing for k > 1 time period error")
  short.time.range.esdgen <- ""
}

test_QCTimeMask <- function(){
  print("Testing for time dimension and mask variable length mismatch error")
  print("Testing for mask collision error")
  time.mask.overlap.file <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231.nc"
  checkException(QCTimeMask(ReadMaskNC(nc_open(time.mask.overlap.file)), run=TRUE), 
                 paste("Mask collision error: Masks within the first 2 masks of", 
                       "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231.nc", 
                       ", provided as an ESD generation mask file overlap along the time series.")) 
}

test_QCTimeWindowList <- function(){
  print("Testing for training time period mismatch")
  train.time.mask <-"/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
  pred.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc"
  esd.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc"
  train.and.use.same <<- TRUE
  tmask.list <- CreateTimeWindowList(hist.train.mask = pred.time.mask, hist.targ.mask = train.time.mask, 
                                     esd.gen.mask = esd.time.mask, k=0, method="CDFt")
  checkException(QCTimeWindowList(tmask.list, k=0), 
                 paste("Training period time error: The start and end dates of the training target", 
                       "1961-01-01 12:00:00 2005-12-31 12:00:00 are not the same as the start",
                       "and end dates of the training predictor, 2006-01-01 12:00:00 2099-12-31 12:00:00"))
  print("Testing for esdgen time period mismatch wih k > 1")
  train.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc"
  pred.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc"
  esd.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
  train.and.use.same <<- FALSE
  tmask.list <- CreateTimeWindowList(hist.train.mask = pred.time.mask, hist.targ.mask = train.time.mask, 
                                     esd.gen.mask = esd.time.mask, k=2, method="simple.lm")
  checkException(QCTimeWindowList(tmask.list, k=2), 
                paste("K > 1 Time Period Error: The start and end dates of the training period 1961-01-01 12:00:00",
                      "2005-12-31 12:00:00 are not the same as the start and end dates of the generation period,",
                      "2006-01-01 12:00:00 2099-12-31 12:00:00"))
  print("Testing for mismatches between the number of masks present in each time window")
  train.time.mask <-"/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
  pred.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
  anomalous.time.mask <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_august_1961-2005.nc"
  train.and.use.same <<- TRUE
  tmask.list <- CreateTimeWindowList(hist.train.mask = pred.time.mask, hist.targ.mask = train.time.mask, 
                       esd.gen.mask = anomalous.time.mask, k=0, method="CDFt")
  checkException(QCTimeWindowList(tmask.list, k=0), 
                 paste("Time mask dimension error: time mask files are expected to have the same number of masks per file, but",
                       "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc had 12", 
                       ", /archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc had 12 ,", 
                       "and /archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_august_1961-2005.nc had 1"))
}



Create.nc.objects <- function(){
  #Create objects to be used for testing purposes
  
}