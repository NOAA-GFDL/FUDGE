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