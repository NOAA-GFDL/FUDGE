#Example code for the WriteNC and WriteGlobals functions
library(ncdf4)

##Edit base directory to source relevant files
#basedir <- "/home/a1r/gitlab/fudge2014/Rsuite/FudgeIO/"
basedir <- "/home/cew/Code/fudge2014/"
setwd(basedir)
source(paste(basedir, "Rsuite/FudgeIO/src/WriteNC.R", sep=""))
source(paste(basedir, "Rsuite/FudgeIO/src/WriteGlobals.R", sep=''))


filename='/tmp/testOut.nc'

data.array=c(100,200,300,150)
var.name="tasmax"
xlon=c(360.1)
ylat=c(88,88.5)

###Proposed CW changes###
timeseries = c(1.5, 2.5)
origin = "days since Jan. 1, 2014"
calendar = "julian"
#time.series <- seq(0:2, by=1)
#time.index.start <- time.series[1]
#time.index.end <- time.series[length(time.series)]
#start.year=0001
units="K"
calendar="julian"
#WriteNC(filename,data.array,var.name,xlon,ylat,time.index.start,time.index.end,start.year,units,calendar)
WriteNC(filename = filename, data.array = data.array, var.name = var.name, xlon = xlon, ylat = ylat, downscale.tseries = timeseries, downscale.origin = origin,
        start.year="undefined", units= units, calendar = calendar,
        lname="Sample Tasmax output",cfname=var.name)
#WriteGlobals
count.dep.samples=30
count.indep.samples=10
kfold=2
print(filename)
fname = WriteGlobals(filename,count.dep.samples,count.indep.samples,kfold)
print(paste('Globals written to ',fname,sep=''))

#WriteGlobals(filename,count.dep.samples,count.indep.samples,kfold,predictand=NA,predictor=NA,label.training=NA,downscaling.method=NA,reference=NA,label.validation=NA,institution="NOAA/GFDL".version='none')

