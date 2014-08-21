#' driver_a1rv2.r
#' phase 1 of driver script implementation for FUDGE: CDFt train driver 
#' originally created: a1r,08/2014

############### Library calls, source necessary functions ###################################################
#TODO the following sapplys and sourcing should be a library call
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/src/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgePreDS/src/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/drivers/',sep=''), full.names=TRUE), source);
source(paste(FUDGEROOT,'Rsuite/drivers/CDFt/TrainDriver.R',sep=''))
#------- Add libraries -------------
LoadLib(ds.method)
#-------End Add libraries ---------
###############################################################################################################
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
#' predictor.vars: String vector, list of predictor variables  
#' predictand.var: predictand variable
#' -----Information on Historical Training Predictor dataset------
#' hist.file.start.year_1: file start year
#' hist.file.end.year_1: file end year
#' hist.train.start.year_1: train start year
#' hist.train.end.year_1: train end year
#' hist.scenario_1: experiment,ensemble member specification 
#' hist.nyrtot_1: Total number of years 
#' hist.model_1: Name of the model
#' hist.freq_1: Time Frequency 
#' hist.indir_1: Input directory 
#' -----Information on Future Predictor dataset---------
#' fut.file.start.year_1: file start year
#' fut.file.end.year_1: file end year
#' fut.train.start.year_1: train start year
#' fut.train.end.year_1: train end year
#' fut.scenario_1: experiment and ensemble member specification
#' fut.nyrtot_1: Total number of years 
#' fut.model_1: Model name 
#' fut.freq_1: Time frequency 
#' fut.indir_1: Input directory
#' -----Information on Target Dataset used in training------
#' target.file.start.year_1: file start year
#' target.file.end.year_1: file end year
#' target.train.start.year_1: train start year
#' target.train.end.year_1: train end year
#' target.scenario_1: experiment, ensemble member specification 
#' target.nyrtot_1: Total number of years
#' target.model_1: Name of the model 
#' target.freq_1: Time frequency 
#' target.indir_1: Input directory
#' ----------- masks -----------------------
#' spat.mask.dir_1: spatial mask directory
#' spat.mask.var: spatial mask variable (Name of the Region to be downscaled)
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
#'  npas : for CDFt number of cuts
#' -----Output root location---------------------
# ----- Begin driver program to develop different sections in FUDGE plus CDFt train -----
#' output.dir: Output root location
#'------- Lon/I,Lat/J range to be downscaled ------
#' i.start: I index start, it is typically 1 since we are reading and writing to minifiles per longitude
#' J.range.suffix: J File range suffix to identify suffix of the input files. Get j.start, j.end from here
#' j.start: J index start - use in file suffix
#' j.end: J index end - use in file suffix
#' loop.start: J loop start , use in writing output
#' loop.end: J loop end, use in writing output

# ----- Begin segment like FUDGE Schematic Section 2: Access Input Data Sets -----

# netcdf handlers: Call FudgeIO

# construct file-names
for (predictor.var in predictor.vars){
print(paste("predictor:",predictor.var,sep='')) 
#TODO with multiple predictors, use this as outer loop before retrieving input files,assign names with predictor.var as suffix. 
} 
######################## input minifiles ####################
hist.filename <- GetMiniFileName(predictor.var,hist.freq_1,hist.model_1,hist.scenario_1,grid,hist.file.start.year_1,hist.file.end.year_1,i.file,file.j.range)
print(hist.filename)
fut.filename <- GetMiniFileName(predictor.var,fut.freq_1,fut.model_1,fut.scenario_1,grid,fut.file.start.year_1,fut.file.end.year_1,i.file,file.j.range)
print(fut.filename)
target.filename <- GetMiniFileName(target.var,target.freq_1,target.model_1,target.scenario_1,grid,target.file.start.year_1,target.file.end.year_1,i.file,file.j.range)
print(target.filename)
spat.mask.filename <- paste(spat.mask.var,".","I",i.file,"_",file.j.range,".nc",sep='')
print(spat.mask.filename)

# load the sample input datasets to numeric vectors
hist.ncobj <- OpenNC(hist.indir_1,hist.filename)
print("OpenNC: success..1")
target.ncobj <- OpenNC(target.indir_1,target.filename)
print("OpenNC: success..2")
fut.ncobj <- OpenNC(fut.indir_1,fut.filename)

print("OpenNC: success..3")
print("get xlon,ylat")
xlon <- sort(ncvar_get(fut.ncobj,"lon"))
print("xlon: received")
ylat <- sort(ncvar_get(fut.ncobj,'lat'))
print("ylat: received")

list.hist <- ReadNC(hist.ncobj,predictor.var,dstart=c(1,1,1),dcount=c(1,140,16436))
print("ReadNC: success..1")
list.fut  <- ReadNC(fut.ncobj,predictor.var,dstart=c(1,1,1),dcount=c(1,140,34333)) 
print("ReadNC: success..2")
list.target <- ReadNC(target.ncobj,predictor.var,dstart=c(1,1,1),dcount=c(1,140,16436))
print("ReadNC: success..3")

# spatial mask read check
spat.mask.ncobj <- OpenNC(spat.mask.dir_1,spat.mask.filename)
print('OpenNC spatial mask: success..4') 

ReadNC(spat.mask.ncobj,spat.mask.var,dstart=c(1,22),dcount=c(1,2))
print('ReadNC spatial mask: success..4')


# simulate the user-specified choice of climate variable name to be processed
clim.var.in <- list.fut$clim.in
# ----- Begin segment like FUDGE Schematic Section 3: Pre-processing of Input Data -----

# Spatial Range For Predictors -------------------------
#TODO cew: Explore passing spat.mask.ncobj as second arg, making checks currently done internal to the function that relies on the path to outside. This way, we open the file just once. spat.mask.ncobj potentially to be used in final sections

target.masked.in <- ApplySpatialMask(list.target$clim.in,paste(spat.mask.dir_1,spat.mask.filename,sep=''),spat.mask.var,xlon,ylat)
print("ApplySpatialMask target: success..1")
hist.masked.in <- ApplySpatialMask(list.hist$clim.in,paste(spat.mask.dir_1,spat.mask.filename,sep=''),spat.mask.var,xlon,ylat)
print("ApplySpatialMask hist.predictor: success..2")
fut.masked.in <- ApplySpatialMask(list.fut$clim.in,paste(spat.mask.dir_1,spat.mask.filename,sep=''),spat.mask.var,xlon,ylat)
print("ApplySpatialMask esgden.fut.predictor: success..3")
#- - - - - Loop through masked.data to downscale points ------------- #

# ----- Begin segment like FUDGE Schematic Section 3: QC of Data After Pre-Processing -----
# compute the statistics of the vector to be passed into the downscaling training,

# + + + function MyStats + + + moved to MyStats.R
source(paste(FUDGEROOT,'/Rsuite/aux/','MyStats.R',sep=''))
# use the my_stats function to compute the statistics of the user-specified variable

print("STATS: Training Target")
liststats <- MyStats(target.masked.in,verbose="yes")
print("STATS: Historical Predictors")
liststats <- MyStats(hist.masked.in,verbose="yes")
print("STATS: Future predictors")
liststats <- MyStats(fut.masked.in,verbose="yes")

# -- QC of input data ends --#

# ----- Begin segment like FUDGE Schematic Section 3: Apply Distribution Transform -----

# If indicated by the user-specified logical variable lopt.wetday,
# classify each day as being a wetday or not, according to the method
# indicated by the user-specified numerical variable opt.wetday
# Note: function WetDayID is in file task1_WetDayID.R

# If the user-specified numerical variable opt.transform > 1, then initiate a
# data transformation processed using the method indicated by the user-specified
# numerical variable opt.transform
# Note: function TransformData is in file task1_transform.R


# ----- Begin segment FUDGE Schematic Section 4: ESD Method Training and Generation -----

## Call CDFt method

print("CDFt training begins....")
##TODO a1r: can be deduced from future train time dimension length or esdgen's ##
time.steps <- 34333 # No.of time steps in the downscaled output.
##

################ call train driver ######################################
ds.vector <- TrainDriver(i.start,loop.start,loop.end,target.masked.in,hist.masked.in,fut.masked.in,ds.method,k=0,time.steps)
############## end call TrainDriver ######################################

esd.final <- ds.vector 
#plot(fut.clim.in,esd.output,xlab="fut.esdGen.predictor -- Large-scale data", ylab="ds -- Downscaled data")

# + + + end Training + + +


# ----- Begin segment like FUDGE Schematic Section 5: Apply Distribution Back-Transform -----
#TODO Diana

#--QC Downscaled Values
print("STATS: Downscaled output")
MyStats(esd.final,verbose="yes")
# ----- Begin segment like FUDGE Schematic Section 6: Write Downscaled results to data files -----
#Replace NAs by missing 
ds.vector[is.na(ds.vector)] <- 1.0e+20

out.file <- paste(output.dir,"/","dstest2.",fut.filename,sep='')
#Write to netCDF
ds.out.filename = WriteNC(out.file,esd.final,target.var,xlon,ylat[loop.start:loop.end],0,(time.steps-1),start.year=fut.train.start.year_1,list.fut$units$value,"julian",lname=paste('Downscaled ',list.fut$long_name$value,sep=''),cfname=list.fut$cfname$value)
#Write Global attributes to downscaled netcdf
label.training <- paste(hist.model_1,".",hist.scenario_1,".",hist.train.start.year_1,"-",hist.train.end.year_1,sep='')
label.validation <- paste(fut.model_1,".",fut.scenario_1,".",fut.train.start.year_1,"-",fut.train.end.year_1,sep='')
WriteGlobals(ds.out.filename,k.fold,target.var,predictor.var,label.training,ds.method,configURL,label.validation,institution='NOAA/GFDL',version='testing',title="CDFt tests in 1^5")
print(paste('Downscaled output file:',ds.out.filename,sep=''))
message(paste('Downscaled output file:',ds.out.filename,sep=''))

print(paste("END TIME:",Sys.time(),sep=''))

## End Of Program


