#' task_driver_a1rv1.r
#' phase 1 of driver script implementation for FUDGE: CDFt train driver 
#' originally created:8/09/2014
#' Modified by CEW: 8/25/2014, adding FudgeTrainDriver, time windowing functions

# ------ Set FUDGE environment ---------------
FUDGEROOT = Sys.getenv(c("FUDGEROOT"))
print(paste("FUDGEROOT is now activated:",FUDGEROOT,sep=''))
##How do you get and set 

#------- Add libraries -------------
library(ncdf4)
library(CDFt)
#TODO the following sapplys and sourcing should be a library call
###CEW: This is not working today (8-25), but it was yesterday
###Fixed; FUDGEROOT isn't being set as my current working directory.
###Need as setwd()
setwd("~/Code/fudge2014/")
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/src/',sep=''), full.names=TRUE), source);

#' key data objects -----
#' clim.var.in:   numeric vector containing data of the input climate variable
#                to be processed
#' esd.output:    numeric vector representing data produced by statisical
#                downscaling processes of FUDGE schematic section 4.
#' esd.final:     numeric vector containing the version of the downscaled output
#                to be archived, either the same as esd.output or a 
#                back-transformed version of esd.output

# ----- Begin segment like FUDGE Schematic Section 2: Read User Specs -----
# TODO The following variables will be passed from experGen 
#' user specified options -----
#' predictor.var: predictor variable list 
#' predictand.var: predictand variable
#' -----Information on Historical Training Predictor dataset------
#' hist.start.year_1: start year
#' hist.end.year_1: end year
#' hist.scenario_1: experiment,ensemble member specification 
#' hist.nyrtot_1: Total number of years 
#' hist.model_1: Name of the model
#' hist.freq_1: Time Frequency 
#' hist.indir_1: Input directory 
#' hist.spatial.mask_1: Spatial mask file path 
#' hist.time.window: Training time window, to be used on the training data
#' -----Information on Future ESDGEN Predictor dataset---------
#' fut.start.year_1: start year
#' fut.end.year_1: end year
#' fut.scenario_1: experiment and ensemble member specification
#' fut.nyrtot_1: Total number of years 
#' fut.model_1: Model name 
#' fut.freq_1: Time frequency 
#' fut.indir_1: Input directory
#' fut.spatial.mask: Spatial mask 
#' fut.time.window: Path to esdgen time window masks, to be used on the esdgen data
#' -----Information on Target Dataset used in training------
#' target.start.year_1: start year
#' target.end.year_1: end year
#' target.scenario_1: experiment, ensemble member specification 
#' target.nyrtot_1: Total number of years
#' target.model_1: Name of the model 
#' target.freq_1: Time frequency 
#' target.indir_1: Input directory
#' target.spatial.mask: Spatial mask file location
#' -----Method descriptors------------------------
#' ds.method: Name of the downscaling method/library
#' ds.experiment: Name of the downscaling experiment
#' k.fold: Value of 'k' in k-fold cross-validation
#' -----Custom method-specific information if any ----
#'  lopt.wetday:   single element logical vector containing user-specified option
#'                 specifying whether function WetDayID should be called (T,F)
#'  opt.wetday:    single element numeric vector containing user-specified option
#'                 indicating which threshold option for wet vs dry day to use
#'  opt.transform: single element numeric vector containing user-specified option
#'                 setting what type of data transform, if any, is to be done

#' -----Output root location---------------------
# ----- Begin driver program to develop FUDGE CDFt train -----
#' output.dir: Output root location
#'-------Name of the Region to be downscaled----
#' region: Name of the region to be downscaled
#'------- Lon/I,Lat/J range to be downscaled ------
#' i.start: I index start
#' i.end: I index end
#' j.start: J index start
#' j.end: J index end
#' J.range.suffix: J File range suffix to idenify suffix of the input files
# sample input files for working with 1^5
#historical_predictor<- '/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/historical/atmos/day/r1i1p1/v20111006/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_historical_r1i1p1_SCCSC0p1_19610101-20051231.I300_J31-170.nc'
#future_predictor <- '/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-21001231.I300_J31-170.nc'
#target <- '/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/atmos/day/r0i0p0/v1p2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I300_J31-170.nc'

# ----- Begin segment like FUDGE Schematic Section 2: Access Input Data Sets -----

# netcdf handlers: Call FudgeIO

# construct file-names

#TODO filename <- getFileName(variable,freq,model,scenario,start.year,end.year,i.index,j.range.suffix)
hist.filename <- 'tasmax_day_MPI-ESM-LR_historical_r1i1p1_SCCSC0p1_19610101-20051231.I300_J31-170.nc'
fut.filename <- 'tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc'
target.filename <- 'tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I300_J31-170.nc'

# load the sample input datasets to numeric vectors
hist.ncobj <- OpenNC(hist.indir_1,hist.filename)
message("OpenNC: success..1")
target.ncobj <- OpenNC(target.indir_1,target.filename)
message("OpenNC: success..2")
#fut.ncobj <- OpenNC(fut.indir_1,fut.filename)
fut.ncobj <- OpenNC("/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/","tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc")
message("OpenNC: success..3")
message("get xlon,ylat")
xlon <- sort(ncvar_get(fut.ncobj,"lon"))
message("xlon: received")
ylat <- sort(ncvar_get(fut.ncobj,'lat'))
message("ylat: received")

hist.clim.in <- ReadNC(hist.ncobj,predictor.var)#dstart=c(1,22,1),dcount=c(1,2,1)  ##, dstart = c(1,100,1), dcount=c(1,1,16436)
message("ReadNC: success..1")
fut.clim.in  <- ReadNC(fut.ncobj,predictor.var) #dstart=c(1,22,1),dcount=c(1,2,1)  ##, dstart = c(1,100,1), dcount=c(1,1,34333)
message("ReadNC: success..2")
target.clim.in <- ReadNC(target.ncobj,predictor.var) #dstart=c(1,22,1),dcount=c(1,2,1) ##, dstart = c(1,100,1), dcount=c(1,1,16436)
message("ReadNC: success..3")

# simulate the user-specified choice of climate variable name to be processed
clim.var.in <- hist.clim.in

# ----- Begin segment like FUDGE Schematic Section 3: Pre-processing of Input Data -----

# + + + function MyStats + + + moved to MyStats.R
source(paste(FUDGEROOT,'Rsuite/aux/','MyStats.R',sep=''))
# use the my_stats function to compute the statistics of the user-specified variable
MyStats(clim.var.in$clim.in)

##CEW edit: Added TimeMaskQC to FudgePP
##Performs QC of mask data and returns a list of all masks used for applying time windows
##Implements checks based on method and kfold crossval
source('Rsuite/FudgePreDS/src/TimeMaskQC.R')
message("Performing QC on time mask data")
if (ds.method=='CDFt' || ds.method=='CDFtv1'){
  #Future data used in downscaling will be underneath the fut.time tag
  tmask.list <-TimeMaskQC(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                          esd.gen.mask = fut.time.window, k=k.fold, method=ds.method)
}else{
  #Data used in downscaling (as opposed to training ) will be underneath the esdgen tag
  tmask.list <- TimeMaskQC(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                           esd.gen.mask = esdgen.time.window, k=kfold, method=ds.method)
}
# ##CEW edit: added spatial masking function calls
# source('Rsuite/FudgePreDS/src/ApplySpatialMask.R')
# message("Applying spatial masks")
# target.clim.in$clim.in <- ApplySpatialMask(target.clim.in$clim.in, )
# if (ds.method=='CDFt' || ds.method=='CDFtv1'){
#   #Future data used in downscaling will be underneath the fut.time tag
# }else{
#   #Data used in downscaling (as opposed to training ) will be underneath the esdgen tag
# }

# ----- Begin segment like FUDGE Schematic Section 3: Apply Distribution Transform -----

# If indicated by the user-specified logical variable lopt.wetday,
# classify each day as being a wetday or not, according to the method
# indicated by the user-specified numerical variable opt.wetday
# Note: function WetDayID is in file task1_WetDayID.R
message("Entering FUDGE-like section 3: Apply Distribution Transform")

source(paste(FUDGEROOT,'Rsuite/aux/','task1_wetdayid.R',sep=''))


  if (lopt.wetday == TRUE) { 
   message("Calling WetDayID")
   wetday.output <- WetDayID(clim.var.in$clim.in, opt.wetday)
   wetday.masks <- wetday.output$is.wetday
   wetday.threshold <- wetday.output$threshold.wetday
  } else {
    message("Not calling WetDayID")
    ##CEW change 
#   wetday.masks <- clim.var.in != NA
    wetday.masks <- !is.na(clim.var.in$clim.in)
    #end of CEW change
   wetday.threshold <- NA
  }

  print(wetday.masks[1:15])
  print(wetday.threshold)

message("Calculating MyStats")
##CEW change
#MyStats(wetday.masks)
#MyStats(clim.var.in$clim.in[wetday.masks==TRUE])
##End CEW change

# If the user-specified numerical variable opt.transform > 1, then initiate a 
# data transformation processed using the method indicated by the user-specified
# numerical variable opt.transform
# Note: function TransformData is in file task1_transform.R
message("Transforming data")
source(paste(FUDGEROOT,'Rsuite/aux/','task1_transform.R',sep=''))

  if (opt.transform == 0) {   
   esd.input <- clim.var.in
  } else {
   esd.input <- TransformData(clim.var.in, wetday.masks, opt.transform)
  }

# ----- Begin segment like FUDGE Schematic Section 3: QC of Data After Pre-Processing -----
# compute the statistics of the vector to be passed into the downscaling training,
message("Begin segment similar to FUDGE Schematic Section 3")
MyStats(esd.input$clim.in)

# ----- Begin segment FUDGE Schematic Section 4: ESD Method Training and Generation -----
# + + + begin defining function BlackBox + + +
#
####Start by invoking the time windowing function
# if(exists("fut.time.window")){
#  mask.list <- list(hist.time.window, hist.time.window, hist.time.window)
# }else{
#   mask.list <- list(hist.time.window, hist.time.window, hist.time.window)
# }
message("FUDGE training begins..")
start.time <- proc.time()
source("Rsuite/FudgeTrain/src/TrainDriver.R")
source("Rsuite/FudgeTrain/src/LoopByTimeWindow.R")
source("Rsuite/FudgeTrain/src/CallDSMethod.R")
source("Rsuite/FudgePreDS/src/ApplyTemporalMask.R")
esd.output <- TrainDriver(target.masked.in = target.clim.in$clim.in, 
                          hist.masked.in = hist.clim.in$clim.in, 
                          fut.masked.in = fut.clim.in$clim.in, 
                          mask.list = tmask.list, ds.method = ds.method, k=0, time.steps=NA, 
                          istart = NA,loop.start = NA,loop.end = NA)
##Commented out CEW
#list.CDFt.result <- CDFt(target.clim.in,hist.clim.in,fut.clim.in,npas = 34333) #34333 16436 
message("FUDGE training ends")
message(paste("FUDGE training took", proc.time()[1]-start.time[1], "seconds to run"))
#esd.output <- list.CDFt.result$DS
#plot(fut.clim.in,esd.output,xlab="fut.esdGen.predictor -- Large-scale data", ylab="ds -- Downscaled data")

# + + + end Training + + +


# ----- Begin segment like FUDGE Schematic Section 5: Apply Distribution Back-Transform -----

# do the transformation based on the user-specified choice stored in opt.transform
# (we signal that the back-transform is desired by mutipying opt.transform by -1.0)
  opt.backtransform <- (-1.0) * opt.transform
  if (opt.transform == 0) {   
   esd.final <- esd.output 
  } else {
   esd.final <- TransformData(esd.output, wetdays.masks, opt.backtransform)
  }

# compute the statistics of the variable after back transform
# using the previously define MyStats function
#MyStats(esd.final)

# compute the statistics of the back transformed results vs. the original data read in
###CEW change: fut.clim.in is closer to what you want than clim.var.in
inout.diff = esd.final - fut.clim.in$clim.in
MyStats(inout.diff)

# Check to see is difference exceed machine precision by more than 10 percent
my.epsilon <- .Machine$double.eps * 1.1 

# count.inoutdiff<- length(inout.diff[inout.diff>=abs(inout.diff-my.epsilon)])

count.bigdiffs <-  length(inout.diff[abs(inout.diff)>my.epsilon])
if (count.bigdiffs == 0) {
  print("Good news. All backtransformed values are within machine precision of the original input") 
} else {
  print(paste("Hmmm. There were ", count.bigdiffs, " occurences of the 
  back-transformed values differing from the original input by more than machine precision. But that's sort of the point.")) 
}
# ----- Begin segment like FUDGE Schematic Section 6: Write Downscaled results to data files -----
#out.file <- paste(output.dir,"/","ds.",fut.filename,sep='')
#CEW change, bceause no write permissions
out.file <- "~/sample_full_ds_output.nc"
print(out.file)
ds.out.filename = WriteNC(out.file,esd.final,predictand.var,xlon[1],ylat,0,34332,start.year="undefined","K","julian") #34332 ylat[1]
###There are options in ds.out that could be obtained by looking at dims of input data
###Units of input data and calendar objects (train.fut, for example)

## End Of Program


