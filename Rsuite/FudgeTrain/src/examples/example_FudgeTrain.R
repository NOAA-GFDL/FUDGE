#example_FudgeTrain.R
#Tests several of the scripts in FudgeTrain and provides examples of their use
#Written by Carolyn Whitlock, August 2014
#
#
#
setwd("~/Code/fudge2014/Rsuite/FudgeTrain/src/examples/")

######
###Example code for calling and testing the cross-validation functions

#Source the relevant files in the directory
source("../CrossValidate.R")
source("../MaskMerge.R")

###Inititalize data to be messed about with
train_predictor <- seq(1:101)
train_target <- train_predictor^1.4 + 12
esd_gen <-seq(from=1, to=151, by=1)

####Run the cross-validation with the commented sample option
source("ESD.train.totally.fake.R")
k0 <- CrossValidate(train_predictor, train_target, esd_gen, 0, "ESD.Train.totally.fake")
print(paste("k0 values:", cat(k0)))
k4 <- CrossValidate(train_predictor, train_target, esd_gen, 4, "ESD.Train.totally.fake")
print(paste("k4 values:", cat(k4)))

###And now, plot the data to see how the series compare to each other
xrange=c(0,151)
yrange=c(0,800)
plot(xrange, yrange, type="n", main="Comparison of cross-validation methods", 
     xlab="independent", ylab="dependent")
lines(train_predictor, train_target, col="blue")
lines(esd_gen, k0, col="red")
lines(train_predictor, k4, col="orange")
legend("bottomright", legend=c("target", "k=0", "k=4"), col=c("blue", "red", "orange"), 
       lty=c(1,1,1), title="Data source")

#######
##Example code for calling and testing the cross-validation and time-windowing functions

#Source relevant files in the code or directory
source("../DownscaleByTimeWindow.R")
source("../../../FudgePreDS/src/ApplyTemporalMask.R")

sample_t_predict <- seq(1:365)
sample_t_target <- sin(sample_t_predict*0.05)
sample_esd_gen <- seq(1:365)
mask_list <- list("/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc")

#Alt_mask_list is used for k-fold validation of k > 1, 
#since predictor and esdged datasets are the same dataset
alt_mask_list <- list("/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc", 
                  "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc", 
                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc")
 
sample_t_predict <- seq(1:365)
sample_t_target <- sin(sample_t_predict*0.05)
sample_esd_gen <- seq(1:365)
plot(sample_t_predict, sample_t_target, type="n", main="Test time window calls with alt_mask on k > 0", 
     ylim = c(-1.5, 1.5), xlim = c(1, 370))
lines(sample_t_predict, sample_t_target)
crossval_data <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 0, "ESD.Train.totally.fake")
lines(seq(1:365), crossval_data, col="blue")
crossval_data2 <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 2, "ESD.Train.totally.fake")
lines(seq(1:365), crossval_data2, col="red")
crossval_data4 <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 4, "ESD.Train.totally.fake")
lines(seq(1:365), crossval_data4, col="green")
d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:365), d_data, col="cyan")
d2_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 2, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt_mask_list)
lines(seq(1:365), d2_data, col="magenta")
d4_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 4, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt_mask_list)
lines(seq(1:365), d4_data, col="yellow")
legend(legend = c("k0", "k2", "k4", "win12k0", "win12k2", "win12k4"), 
       col = c("blue", "red", "green", "cyan", "magenta", "yellow"), 
       pch = rep("_", 6), "topright")

######Testing the same commands on non-sine data
sample_t_predict <- seq(1:365)
sample_t_target <- rnorm(1:365, mean=20, sd=20)+seq(1:365)*0.5
sample_esd_gen <- seq(1:365)


plot(sample_t_predict, sample_t_target, type="n", main="Test time window calls with alt_mask on k > 0", 
     ylim = c(0, 200), xlim = c(1, 370))
lines(sample_t_predict, sample_t_target)
crossval_data <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 0, "ESD.Train.totally.fake")
lines(seq(1:365), crossval_data, col="blue")
crossval_data2 <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 2, "ESD.Train.totally.fake")
lines(seq(1:365), crossval_data2, col="red")
crossval_data4 <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 4, "ESD.Train.totally.fake")
lines(seq(1:365), crossval_data4, col="green")
d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:365), d_data, col="cyan")
d2_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 2, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt_mask_list)
lines(seq(1:365), d2_data, col="magenta")
d4_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 4, downscale.fxn = "ESD.Train.totally.fake", 
                                 downscale.args=NULL,
                                 masklist = alt_mask_list)
lines(seq(1:365), d4_data, col="yellow")
legend(legend = c("k0", "k2", "k4", "win12k0", "win12k2", "win12k4"), 
       col = c("blue", "red", "green", "cyan", "magenta", "yellow"), 
       pch = rep("_", 6), "topleft")

###THIS SECTION IS PRELIMINARY: 
###Seeing if the CDFt code can work from the crossval fxn w/wo time windowing when k = 0
sample_t_predict <- seq(1:365)
sample_t_target <- sin(sample_t_predict*0.05)+10
sample_esd_gen <- seq(1:365)
plot(sample_t_predict, sample_t_target, type="n", main="Test time window calls with CDFt", 
     ylim=c(0,20), xlim=c(1,365))
lines(sample_t_predict, sample_t_target)
crossval_new_data <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 0, "CDFt")
lines(sample_t_predict, crossval_new_data, col="orange")
d0_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "CDFt", 
                                 downscale.args=NULL,
                                 masklist = mask_list)
lines(sample_t_predict, d0_data, col="violet")
d0_alt_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "CDFt", 
                                 downscale.args=NULL,
                                 masklist = alt_mask_list)
lines(sample_t_predict, d0_alt_data, col="blue")
legend(legend=c("win0k0", "win12k0", "win12k0 alt mask"), 
       col=c("orange", "violet", "blue"), pch = c("-", "-", "-"), "bottomright")
####Trying as part of a non-sine test:
sample_t_predict <- seq(1:365)
sample_t_target <- rnorm(1:365, mean=20, sd=20)+seq(1:365)
sample_esd_gen <- seq(1:365)
plot(sample_t_predict, sample_t_target, type="n", main="Test time window calls with CDFt")
lines(sample_t_predict, sample_t_target)
crossval_new_data <- CrossValidate(sample_t_predict, sample_t_target, sample_esd_gen, 0, "CDFt")
lines(sample_t_predict, crossval_new_data, col="orange")
d0_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                 esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "CDFt", 
                                 downscale.args=NULL,
                                 masklist = mask_list)
lines(sample_t_predict, d0_data, col="violet")
d0_alt_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                     esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "CDFt", 
                                     downscale.args=NULL,
                                     masklist = alt_mask_list)
lines(sample_t_predict, d0_alt_data, col="blue")
legend(legend=c("win0k0", "win12k0", "win12k0 alt mask"), 
col=c("orange", "violet", "blue"), pch = c("-", "-", "-"), "bottomright")
