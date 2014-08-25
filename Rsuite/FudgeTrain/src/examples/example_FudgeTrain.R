#example.FudgeTrain.R
#Tests several of the scripts in FudgeTrain and provides examples of their use
#Written by Carolyn Whitlock, August 2014
#
#
#
setwd("~/Code/fudge2014/Rsuite/FudgeTrain/src/examples/")
library(ncdf4)


######
###Example code for calling and testing the cross-validation functions

#Source the relevant files in the directory
source("../CrossValidate.R")
source("../MaskMerge.R")

###Inititalize data to be messed about with
train.predictor <- seq(1:101)
train.target <- train.predictor^1.4 + 12
esd.gen <-seq(from=1, to=151, by=1)

####Run the cross-validation with the commented sample option
source("ESD.train.totally.fake.R")
k0 <- CrossValidate(train.predictor, train.target, esd.gen, 0, "ESD.Train.totally.fake")
print(paste("k0 values:", cat(k0)))
k4 <- CrossValidate(train.predictor, train.target, esd.gen, 4, "ESD.Train.totally.fake")
print(paste("k4 values:", cat(k4)))

###And now, plot the data to see how the series compare to each other
xrange=c(0,151)
yrange=c(0,800)
plot(xrange, yrange, type="n", main="Comparison of cross-validation methods", 
     xlab="independent", ylab="dependent")
lines(train.predictor, train.target, col="blue")
lines(esd.gen, k0, col="red")
lines(train.predictor, k4, col="orange")
legend("bottomright", legend=c("target", "k=0", "k=4"), col=c("blue", "red", "orange"), 
       lty=c(1,1,1), title="Data source")

#######
##Example code for calling and testing the Fudge driver script and the time-windowing functions

#Source relevant files in the code or directory
source("../DownscaleByTimeWindow.R")
source("../../../FudgePreDS/src/ApplyTemporalMask.R")
source("../DownscaleWithAllArgs.R")
source("../TrainDriver.R")

sample.t.predict <- seq(1:365)
sample.t.target <- sin(sample.t.predict*0.05)
sample.esd.gen <- seq(1:365)
mask.list <- list("/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
                  "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
                  "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc")

#Alt.mask.list is used for k-fold validation of k > 1, 
#since predictor and esdged datasets are the same dataset
alt.mask.list <- list("/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc", 
                  "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc", 
                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc")


d.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "simple.lm", 
                                downscale.args=NULL,
                                masklist = mask.list, debug=TRUE)
cdft.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "CDFt", 
                                downscale.args=NULL,
                                masklist = mask.list, debug=TRUE)
#####Testing on real data
train_time_window = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231.nc"
esdgen_time_window = "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
historical_target = "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/atmos/day/r0i0p0/v1.2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I250_J31-170.nc"
new.nc <- nc_open(historical_target)
hist.targ <- ncvar_get(new.nc, "tasmax", collapse_degen=FALSE)
historical_predictor = "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/historical/atmos/day/r1i1p1/v20111006/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_historical_r1i1p1_SCCSC0p1_19610101-20051231.I250_J31-170.nc"
new.nc <- nc_open(historical_predictor)
hist.pred <- ncvar_get(new.nc, "tasmax", collapse_degen=FALSE)

#hmm...error message that gives a better description of the data throwing the error?

real.mask.list <- list(train_time_window, train_time_window, esdgen_time_window)

real.data <- DownscaleByTimeWindow(train.predictor = hist.pred[100,], train.target = hist.targ[100,], 
                                esd.gen = hist.pred[100,], kfold = 0, downscale.fxn = "simple.lm", 
                                downscale.args=NULL,
                                masklist = real.mask.list, debug=TRUE)

real.cdft.data <- DownscaleByTimeWindow(train.predictor = hist.pred[100,], train.target = hist.targ[100,], 
                                   esd.gen = hist.pred[100,], kfold = 0, downscale.fxn = "CDFt", 
                                   downscale.args=NULL,
                                   masklist = real.mask.list, debug=TRUE)

#############################
#Now, attempt inclusion of the driver script

start.time <- proc.time()
all.real.data <- TrainDriver(target.masked.in = hist.targ, hist.masked.in = hist.pred, fut.masked.in = hist.pred, 
                             mask.list = real.mask.list, ds.method = 'CDFt', k=0, time.steps=NA, 
                             istart = NA,loop.start = NA,loop.end = NA)
print(paste("Entire run took", proc.time()-start.time, "to complete."))

#############################
plot(sample.t.predict, sample.t.target, type="n", main="Test time window calls with alt.mask on k > 0", 
     ylim = c(-1.5, 1.5), xlim = c(1, 370))
lines(sample.t.predict, sample.t.target)
crossval.data <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 0, "ESD.Train.totally.fake")
lines(seq(1:365), crossval.data, col="blue")
crossval.data2 <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 2, "ESD.Train.totally.fake")
lines(seq(1:365), crossval.data2, col="red")
crossval.data4 <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 4, "ESD.Train.totally.fake")
lines(seq(1:365), crossval.data4, col="green")
d.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask.list)
lines(seq(1:365), d.data, col="cyan")
d2.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 2, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt.mask.list)
lines(seq(1:365), d2.data, col="magenta")
d4.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 4, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt.mask.list)
lines(seq(1:365), d4.data, col="yellow")
legend(legend = c("k0", "k2", "k4", "win12k0", "win12k2", "win12k4"), 
       col = c("blue", "red", "green", "cyan", "magenta", "yellow"), 
       pch = rep("_", 6), "topright")

######Testing the same commands on non-sine data
sample.t.predict <- seq(1:365)
sample.t.target <- rnorm(1:365, mean=20, sd=20)+seq(1:365)*0.5
sample.esd.gen <- seq(1:365)


plot(sample.t.predict, sample.t.target, type="n", main="Test time window calls with alt.mask on k > 0", 
     ylim = c(0, 200), xlim = c(1, 370))
lines(sample.t.predict, sample.t.target)
crossval.data <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 0, "ESD.Train.totally.fake")
lines(seq(1:365), crossval.data, col="blue")
crossval.data2 <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 2, "ESD.Train.totally.fake")
lines(seq(1:365), crossval.data2, col="red")
crossval.data4 <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 4, "ESD.Train.totally.fake")
lines(seq(1:365), crossval.data4, col="green")
d.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask.list)
lines(seq(1:365), d.data, col="cyan")
d2.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 2, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt.mask.list)
lines(seq(1:365), d2.data, col="magenta")
d4.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 4, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt.mask.list)
lines(seq(1:365), d4.data, col="yellow")
legend(legend = c("k0", "k2", "k4", "win12k0", "win12k2", "win12k4"), 
       col = c("blue", "red", "green", "cyan", "magenta", "yellow"), 
       pch = rep(".", 6), "topleft")

###THIS SECTION IS PRELIMINARY: 
###Seeing if the CDFt code can work from the crossval fxn w/wo time windowing when k = 0
sample.t.predict <- seq(1:365)
sample.t.target <- sin(sample.t.predict*0.05)+10
sample.esd.gen <- seq(1:365)
plot(sample.t.predict, sample.t.target, type="n", main="Test time window calls with CDFt", 
     ylim=c(0,100), xlim=c(1,365))
lines(sample.t.predict, sample.t.target)
#Plotting as a simple call to CDFt
no.sep <- CDFt(sample.t.predict, sample.t.target, sample.esd.gen, npas=length(sample.esd.gen))$DS
lines(sample.t.predict, no.sep, col="lightsteelblue")
#Plotting as a call to CrossValidate with k=0
crossval.new.data <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 0, "CDFt")
lines(sample.t.predict, crossval.new.data, col="orange")
#Plotting as a call to DownscaleByTimeWindow with overlapping masks
d0.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "CDFt", 
                                 downscale.args=NULL,
                                 masklist = mask.list)
lines(sample.t.predict, d0.data, col="violet")
#Plotting as a call to DownscaleByTimeWindow without overlapping masks
d0.alt.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "CDFt", 
                                 downscale.args=NULL,
                                 masklist = alt.mask.list)
lines(sample.t.predict, d0.alt.data, col="blue")
legend(legend=c("CDFt", "win0k0", "win12k0", "win12k0 alt mask"), 
       col=c("lightsteelblue", "orange", "violet", "blue"), pch = c("-", "-", "-", "-"), "bottomright")

####Trying as part of a non-sine test:
sample.t.predict <- seq(1:365)
sample.t.target <- rnorm(1:365, mean=20, sd=20)+seq(1:365)
sample.esd.gen <- seq(1:365)
plot(sample.t.predict, sample.t.target, type="n", main="Test time window calls with CDFt")
lines(sample.t.predict, sample.t.target)
#Plotting as a simple call to CDFt

no.sep <- CDFt(sample.t.predict, sample.t.target, sample.esd.gen, npas=length(sample.esd.gen))$DS
lines(sample.t.predict, no.sep, col="lightsteelblue")

#Plotting as a call to CrossValidate
crossval.new.data <- CrossValidate(sample.t.predict, sample.t.target, sample.esd.gen, 0, "CDFt")
lines(sample.t.predict, crossval.new.data, col="orange")
#Plotting as a call to DownscaleByTimeWindow with overlapping masks
d0.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                 esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "CDFt", 
                                 downscale.args=NULL,
                                 masklist = mask.list)
lines(sample.t.predict, d0.data, col="violet")
#Plotting as a call to DownscaleByTimeWindow without overlapping masks
d0.alt.data <- DownscaleByTimeWindow(train.predictor = sample.t.predict, train.target = sample.t.target, 
                                     esd.gen = sample.esd.gen, kfold = 0, downscale.fxn = "CDFt", 
                                     downscale.args=NULL,
                                     masklist = alt.mask.list)
lines(sample.t.predict, d0.alt.data, col="blue")
legend(legend=c("CDFt", "win0k0", "win12k0", "win12k0 alt mask"), 
col=c("lightsteelblue", "orange", "violet", "blue"), pch = c("-", "-", "-", "-"), "bottomright")
