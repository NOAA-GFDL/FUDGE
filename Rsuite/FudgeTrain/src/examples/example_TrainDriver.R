#example_TrainDriver.R
#Tests many of the scripts required for TrainDriver, and provides examples of their use
#Includes TimeMaskQC, LoopByTimeWindow, and TrainDriver.
#Written by Carolyn Whitlock, August 2014
#
#
#
setwd("~/Code/fudge2014/Rsuite/FudgeTrain/src/examples/")
library(ncdf4)
source("../../src/TrainDriver.R")
source("../../src/LoopByTimeWindow.R")
source("../../src/CallDSMethod.R")
source("../../../FudgePreDS/src/TimeMaskQC.R")
source("../../../FudgePreDS/src/ApplyTemporalMask.R")
#######
##Example code for calling and testing the Fudge driver script and the time-windowing functions
##with real data.

#train_time_window = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231.nc"
train_time_window = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
#esdgen_time_window = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20991231.nc"
esdgen_time_window <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc"

check.mask.list <- TimeMaskQC(hist.train.mask = train_time_window, hist.targ.mask = train_time_window, 
                              esd.gen.mask = esdgen_time_window, k=0, method='CDFt')
#Then, read in data 
historical_target = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/atmos/day/r0i0p0/v1.2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I250_J31-170.nc"
new.nc <- nc_open(historical_target)
hist.targ <- ncvar_get(new.nc, "tasmax", collapse_degen=FALSE)
historical_predictor = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/historical/atmos/day/r1i1p1/v20111006/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_historical_r1i1p1_SCCSC0p1_19610101-20051231.I250_J31-170.nc"
new.nc <- nc_open(historical_predictor)
hist.pred <- ncvar_get(new.nc, "tasmax", collapse_degen=FALSE)
future_predictor <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I250_J31-170.nc"
new.nc <- nc_open(future_predictor)
fut.pred <- ncvar_get(new.nc, "tasmax", collapse_degen=FALSE)

#And run your DS method of choice
start.time <- proc.time()
real.lm <- LoopByTimeWindow(train.predictor = hist.pred[1,100,], train.target = hist.targ[1,100,], 
                            esd.gen = fut.pred[1,100,], 
                            mask.struct = check.mask.list, downscale.fxn = "simple.lm", 
                            downscale.args = NULL, kfold=0, kfold.mask=NULL, graph=TRUE, masklines=FALSE)
print(paste("Entire run with simple.lm took", proc.time()[1]-start.time[1], "to complete."))
start.time <- proc.time()
real.CDFt <- LoopByTimeWindow(train.predictor = hist.pred[1,100,], train.target = hist.targ[1,100,], 
                              esd.gen = fut.pred[1,100,], 
                              mask.struct = check.mask.list, downscale.fxn = "CDFt", 
                              downscale.args = NULL, kfold=0, kfold.mask=NULL, graph=TRUE, masklines=FALSE)
print(paste("Entire run with CDFt took", proc.time()[1]-start.time[1], "to complete."))
start.time <- proc.time()
real.CDFtv1 <- LoopByTimeWindow(train.predictor = hist.pred[1,100,], train.target = hist.targ[1,100,], 
                              esd.gen = fut.pred[1,100,], 
                              mask.struct = check.mask.list, downscale.fxn = "CDFtv1", 
                              downscale.args = NULL, kfold=0, kfold.mask=NULL, graph=TRUE, masklines=FALSE)
print(paste("Entire run with CDFtv1 took", proc.time()[1]-start.time[1], "to complete."))

#Plot results to compare over a five-year period
timevector <- new.nc$dim$time$vals[1:(365*5)]
library(PCICt)
origin <- as.PCICt("1961-01-01", "gregorian")
timeseries <- origin + timevector * 86400

plot(timeseries, fut.pred[1,100,1:(365*5)], type="l", main="Test time window calls with real data on k=0", lwd=2)
lines(timeseries, real.CDFt[1:(365*5)], lwd=3, lty = 3, col='red')
lines(timeseries, real.lm[1:(365*5)], lwd=3, lty = 1, col='blue')
legend(legend = c("target", "CDFt", "simple.lm"), col=c("black", "red", "blue"), lty = c(1,3,1),
       "bottomright")

#Now, attempt inclusion of the TrainDriver script on the same target datasets as before

start.time <- proc.time()
all.real.lm.data <- TrainDriver(target.masked.in = hist.targ, hist.masked.in = hist.pred, 
                                fut.masked.in = fut.pred, mask.list = check.mask.list, 
                                ds.method = 'simple.lm', k=0, time.steps=NA, 
                                istart = NA,loop.start = NA,loop.end = NA)
print(paste("Entire run with simple.lm took", proc.time()[1]-start.time[1], "to complete."))
#Takes roughly 18 seconds to run if workspace is cleared first

start.time <- proc.time()
all.real.CDFt.data <- TrainDriver(target.masked.in = hist.targ, hist.masked.in = hist.pred, fut.masked.in = fut.pred, 
                                  mask.list = check.mask.list, ds.method = 'CDFt', k=0, time.steps=NA, 
                                  istart = NA,loop.start = NA,loop.end = NA)
print(paste("Entire run with CDFt took", proc.time()[1]-start.time[1], "to complete."))
#CDFt took **6 MINUTES** to run over the entire dataset. I think that this might be doing okay.