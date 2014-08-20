#' task_driver_a1rv1.r
#' phase 1 of driver script implementation for FUDGE: CDFt train driver 
#' originally created:8/09/2014

# ------ Set FUDGE environment ---------------
FUDGEROOT = Sys.getenv(c("FUDGEROOT"))
print(paste("FUDGEROOT is now activated:",FUDGEROOT,sep=''))

#------- Add libraries -------------
library(ncdf4)
library(CDFt)
#TODO the following sapplys and sourcing should be a library call
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/src/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgePreDS/src/',sep=''), full.names=TRUE), source);

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
#' -----Information on Future ESDGEN Predictor dataset---------
#' fut.start.year_1: start year
#' fut.end.year_1: end year
#' fut.scenario_1: experiment and ensemble member specification
#' fut.nyrtot_1: Total number of years 
#' fut.model_1: Model name 
#' fut.freq_1: Time frequency 
#' fut.indir_1: Input directory
#' -----Information on Target Dataset used in training------
#' target.start.year_1: start year
#' target.end.year_1: end year
#' target.scenario_1: experiment, ensemble member specification 
#' target.nyrtot_1: Total number of years
#' target.model_1: Name of the model 
#' target.freq_1: Time frequency 
#' target.indir_1: Input directory
#' ----------- masks -----------------------
#' spat.mask.dir_1: spatial mask directory
#' spat.mask.var: spatial mask variable
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
spat.mask.filename <- 'red_river_0p1_masks.I300_J31-170.nc'

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

hist.clim.in <- ReadNC(hist.ncobj,predictor.var,dstart=c(1,1,1),dcount=c(1,140,16436))
message("ReadNC: success..1")
fut.clim.in  <- ReadNC(fut.ncobj,predictor.var,dstart=c(1,1,1),dcount=c(1,140,34333)) 
message("ReadNC: success..2")
target.clim.in <- ReadNC(target.ncobj,predictor.var,dstart=c(1,1,1),dcount=c(1,140,16436))
message("ReadNC: success..3")

message("ReadNC: success..3")
# spatial mask read check
spat.mask.ncobj <- OpenNC(spat.mask.dir_1,spat.mask.filename)
message('OpenNC spatial mask: success..4') 

ReadNC(spat.mask.ncobj,spat.mask.var,dstart=c(1,22),dcount=c(1,2))
message('ReadNC spatial mask: success..4')

# simulate the user-specified choice of climate variable name to be processed
clim.var.in <- hist.clim.in

# ----- Begin segment like FUDGE Schematic Section 3: Pre-processing of Input Data -----

# + + + function MyStats + + + moved to MyStats.R
source(paste(FUDGEROOT,'/Rsuite/aux/','MyStats.R',sep=''))
# use the my_stats function to compute the statistics of the user-specified variable
MyStats(clim.var.in)

#Init ds vector
ds.vector = c()

# ----- Begin segment like FUDGE Schematic Section 3: Apply Distribution Transform -----

# If indicated by the user-specified logical variable lopt.wetday,
# classify each day as being a wetday or not, according to the method
# indicated by the user-specified numerical variable opt.wetday
# Note: function WetDayID is in file task1_WetDayID.R

source(paste(FUDGEROOT,'Rsuite/aux/','task1_wetdayid.R',sep=''))

  if (lopt.wetday == TRUE) {   
   wetday.output <- WetDayID(clim.var.in, opt.wetday)
   wetday.masks <- wetday.output$is.wetday
   wetday.threshold <- wetday.output$threshold.wetday
  } else {
   wetday.masks <- clim.var.in != NA
   wetday.threshold <- NA
  }

  print(wetday.masks[1:15])
  print(wetday.threshold)

MyStats(wetday.masks)

# If the user-specified numerical variable opt.transform > 1, then initiate a 
# data transformation processed using the method indicated by the user-specified
# numerical variable opt.transform
# Note: function TransformData is in file task1_transform.R

source(paste(FUDGEROOT,'Rsuite/aux/','task1_transform.R',sep=''))

  if (opt.transform == 0) {   
   esd.input <- clim.var.in
  } else {
   esd.input <- TransformData(clim.var.in, wetday.masks, opt.transform)
  }

# ----- Begin segment like FUDGE Schematic Section 3: QC of Data After Pre-Processing -----
# compute the statistics of the vector to be passed into the downscaling training,
MyStats(esd.input)

# ------ Section 3:  Spatial Range For Predictors -------------------------
 
target.masked.in <- ApplySpatialMask(target.clim.in,paste(spat.mask.dir_1,spat.mask.filename,sep=''),spat.mask.var,xlon,ylat)
message("ApplySpatialMask target: success..1")
hist.masked.in <- ApplySpatialMask(hist.clim.in,paste(spat.mask.dir_1,spat.mask.filename,sep=''),spat.mask.var,xlon,ylat)
message("ApplySpatialMask hist.predictor: success..2")
fut.masked.in <- ApplySpatialMask(fut.clim.in,paste(spat.mask.dir_1,spat.mask.filename,sep=''),spat.mask.var,xlon,ylat)
message("ApplySpatialMask esgden.fut.predictor: success..2")
#cmessage(target.masked.in)
#- - - - - Loop through masked.data to downscale points ------------- #
# ----- Begin segment FUDGE Schematic Section 4: ESD Method Training and Generation -----
# + + + begin defining function BlackBox + + +
## Call CDFt method
message("CDFt training begins....")
for(jindex in j.start:j.end){ #j.end
  print(paste("Start jindex",jindex,sep=''))
  if(!is.na(target.masked.in[1,jindex,1])){
    list.CDFt.result <- CDFt(target.masked.in[1,jindex,],hist.masked.in[1,jindex,],fut.masked.in[1,jindex,],npas = 16436) #34333 16436 
    message("CDFt training ends")
    ds.vector = c(ds.vector,list.CDFt.result$DS)
  }else{
  print("NA.......")
  ds.vector = c(ds.vector, rep(NA,34333))
  print(target.masked.in[1,jindex,1])
  }
#TODO -- write na to output dataset for those jindex that were skipped..accumulate ds values and write them too..:)
}
ds.vector[is.na(ds.vector)] <- 1.0e+20 

esd.output <- ds.vector 
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
MyStats(esd.final)

# compute the statistics of the back transformed results vs. the original data read in
inout.diff =  esd.final - fut.clim.in
MyStats(inout.diff)

# Check to see is difference exceed machine precision by more than 10 percent
my.epsilon <- .Machine$double.eps * 1.1 

# count.inoutdiff<- length(inout.diff[inout.diff>=abs(inout.diff-my.epsilon)])

count.bigdiffs <-  length(inout.diff[abs(inout.diff)>my.epsilon])
if (opt.transform != 0){
if (count.bigdiffs == 0) {
  print("Good news. All backtransformed values are within machine precision of the original input") 
} else {
  print(paste("Hmmm. There were ", count.bigdiffs, " occurences of the 
  back-transformed values differing from the original input by more than machine precision ")) 
}
}
# ----- Begin segment like FUDGE Schematic Section 6: Write Downscaled results to data files -----
out.file <- paste(output.dir,"/","ds.",fut.filename,sep='')
#print("check..")
#print(length(esd.final))
#Write to netCDF
ds.out.filename = WriteNC(out.file,esd.final,predictand.var,xlon,ylat,0,34332,start.year=fut.start.year_1,"K","julian")
#Write Global attributes to downscaled netcdf
label.training <- paste(hist.model_1,".",hist.scenario_1,".",hist.start.year_1,"-",hist.end.year_1,sep='')
label.validation <- paste(fut.model_1,".",fut.scenario_1,".",fut.start.year_1,"-",fut.end.year_1,sep='')
WriteGlobals(ds.out.filename,k.fold,predictand.var,predictor.var,label.training,ds.method,configURL,label.validation,institution='NOAA/GFDL',version='testing',title="CDFt tests in 1^5")

print(paste("Downscaled output file: ",ds.out.filename,sep=''))
## End Of Program


