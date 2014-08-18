#example_FudgeTrain.R
#Tests several of the scripts in FudgeTrain and provides examples of their use
#Written by Carolyn Whitlock, August 2014

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

#########Now, run the data on a timeseries with downscaled data
source("../DownscaleByTimeWindow.R")
source("../../../FudgePreDS/src/ApplyTemporalMask.R")
sample_t_predict <- seq(1:50769)
sample_t_target <- sin(sample_t_predict*0.0003)
sample_esd_gen <- seq(1:50769)

##Special arguments
mask_list <- list("/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20991231.nc")
plot(seq(1:50769), sin(seq(1:50769)*0.0003), type="l", main="Test time window calls")
d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:50769), d_data, col="cyan")
d2_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:50769), d2_data, col="violet")
###Not doing what it should. Try again.
sample_t_predict <- seq(1:50769)
sample_t_target <- sin(sample_t_predict*0.0003)+1
sample_esd_gen <- seq(1:50769)

##Special arguments
mask_list <- list("/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20991231.nc")
plot(seq(1:50769), sample_t_target, type="l", main="Test time window calls")
d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:50769), d_data, col="cyan")

t.predictor <- ApplyTemporalMask(sample_t_predict, masknc=masklist[[1]])
t.target <-ApplyTemporalMask(sample_t_target, masknc=masklist[[2]])
plot.colors <- rainbow(12)
for (i in 1:12){
  lines(x=seq(1:50769), y=t.target[[i]], col=plot.colors[i])
}
new.predictor <- ApplyTemporalMask(sample_esd_gen, masknc=masklist[[3]], type="run")

downscale.colors <- cm.colors(12)
out.chunk <- as.list(rep(NA, length(sample_esd_gen)))
output <-list(rep(out.chunk, 12))      #Pre-allocate output vector for speed and meory efficency
for (window in 1:12){
  print(paste("starting on window", window, "of 12"))
  output[[window]] <- CrossValidate(train.predict = t.predictor[[window]], train.target = t.target[[window]], 
                                    esd.gen = new.predictor[[window]], 
                                    k = 0, downscale.function = downscale.fxn, args = downscale.args)
  lines(x=seq(1:50769), y=output[[window]], col=downscale.colors[[window]])
}

####Plot the distribution of the first mask
first.mask <- output[[1]]
hist(first.mask[!is.na(first.mask)])

hist(unlist(output))

####Hmmm...okay: try this again with the sinusoidal model instead.
sample_t_predict <- seq(1:50769)
sample_t_target <- sin(sample_t_predict*0.0003)+1
sample_esd_gen <- seq(1:50769)

##Special arguments
mask_list <- list("/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20991231.nc")
plot(seq(1:50769), sample_t_target, type="l", main="Test time window calls")
d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:50769), d_data, col="cyan")

t.predictor <- ApplyTemporalMask(sample_t_predict, masknc=masklist[[1]])
t.target <-ApplyTemporalMask(sample_t_target, masknc=masklist[[2]])
plot.colors <- rainbow(12)
for (i in 1:12){
  lines(x=seq(1:50769), y=t.target[[i]], col=plot.colors[i])
}
new.predictor <- ApplyTemporalMask(sample_esd_gen, masknc=masklist[[3]], type="run")

downscale.colors <- cm.colors(12)
out.chunk <- as.list(rep(NA, length(sample_esd_gen)))
output <-list(rep(out.chunk, 12))      #Pre-allocate output vector for speed and meory efficency
for (window in 1:12){
  print(paste("starting on window", window, "of 12"))
  output[[window]] <- CrossValidate(train.predict = t.predictor[[window]], train.target = t.target[[window]], 
                                    esd.gen = new.predictor[[window]], 
                                    k = 4, downscale.function = "ESD.Train.totally.fake", args = downscale.args)
  lines(x=seq(1:50769), y=output[[window]], col=downscale.colors[[window]])
}

####Okay, maybe it's jsut a problem wiht the methods. Maybe they are all flawed.

sample_t_predict <- seq(1:50769)
sample_t_target <- rnorm(50769, mean=100, sd=1000)+seq(1:50769)
sample_esd_gen <- seq(1:50769)
mask_list <- list("/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc", 
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc",
                  "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20991231.nc")
                  #"/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20991231.nc")
plot(seq(1:50769), sample_t_target, type="l", main="Test time window calls")
d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
                                esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
                                downscale.args=NULL,
                                masklist = mask_list)
lines(seq(1:50769), d_data, col="cyan")
d3_data <- CrossValidate(train.predict = sample_t_predict, train.target = sample_t_target, 
                         esd.gen = sample_esd_gen, k=0, downscale.function="ESD.Train.totally.fake", 
                         compare.function=NA, args=NULL)
lines(seq(1:50769), d_data, col="yellow")
d4_trained.function <- do.call(downscale.fxn, list(sample_t_predict, sample_t_target))
print(d4_trained_function)
d4_data <- do.call(d4_trained.function, list(sample_esd_gen))


t.predictor <- ApplyTemporalMask(sample_t_predict, masknc=masklist[[1]])
t.target <-ApplyTemporalMask(sample_t_target, masknc=masklist[[2]])
plot.colors <- rainbow(12)
for (i in 1:12){
  lines(x=seq(1:50769), y=t.target[[i]], col=plot.colors[i])
}
new.predictor <- ApplyTemporalMask(sample_esd_gen, masknc=masklist[[3]], type="run")

downscale.colors <- cm.colors(12)
out.chunk <- as.list(rep(NA, length(sample_esd_gen)))
output <-list(rep(out.chunk, 12))      #Pre-allocate output vector for speed and meory efficency
for (window in 1:12){
  print(paste("starting on window", window, "of 12"))
  output[[window]] <- CrossValidate(train.predict = t.predictor[[window]], train.target = t.target[[window]], 
                                    esd.gen = new.predictor[[window]], 
                                    k = 4, downscale.function = "ESD.Train.totally.fake", args = downscale.args)
  lines(x=seq(1:50769), y=output[[window]], col=downscale.colors[[window]])
}

