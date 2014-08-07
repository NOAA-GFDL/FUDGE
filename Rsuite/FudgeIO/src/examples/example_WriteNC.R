library(ncdf)
basedir <- "/home/a1r/gitlab/fudge2014/Rsuite/FudgeIO/"
source("/home/a1r/gitlab/fudge2014/Rsuite/FudgeIO/src/WriteNC.R")
source("/home/a1r/gitlab/fudge2014/Rsuite/FudgeIO/src/WriteGlobals.R")

filename='/tmp/testOut.nc'
data.array=c(100,200,300,150)
var.name="tasmax"
xlon=c(360.1)
ylat=c(88,88.5)
time.index.start=0
time.index.end=c(1,2)
start.year=0001
units="K"
calendar="julian"
WriteNC(filename,data.array,var.name,xlon,ylat,time.index.start,time.index.end,start.year,units,calendar)
#WriteGlobals
count.dep.samples=30
count.indep.samples=10
kfold=2
print(filename)
filename="/tmp/new.nc"
fname = WriteGlobals(filename,count.dep.samples,count.indep.samples,kfold)
print(paste('Globals written to ',fname,sep=''))

#WriteGlobals(filename,count.dep.samples,count.indep.samples,kfold,predictand=NA,predictor=NA,label.training=NA,downscaling.method=NA,reference=NA,label.validation=NA,institution="NOAA/GFDL".version='none')

