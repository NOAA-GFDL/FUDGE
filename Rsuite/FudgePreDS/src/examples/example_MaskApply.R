#example_MaskApply.R
#shows how to apply temporal and spatial masks to a sample dataset
#Carolyn Whitlock, August 2014

###Initialize libraries and source spatial mask functions (and netcdf write functions)
library(ncdf4)
source('../ApplySpatialMask.R')
#Assumes that you are working in ../FudgePreDS/src/examples, the location of this script.
workingdir <- getwd()
basedir <- sub("FudgePreDS/src/examples", "", workingdir)
sample.nc <- nc_open(paste(basedir, 
                           "sampleNC/tasmax_day_GFDL-HIRAM-C360-COARSENED_amip_r1i1p1_19790101-20081231.I748_J454-567.nc", 
                           sep = ""))
sample.data <- ncvar_get(sample.nc, "tasmax")
summary(as.vector(sample.data))

###Mask data spatial
spat.mask <- "/net3/kd/PROJECTS/DOWNSCALING/DATA/3ToThe5th/masks/geomasks/OneD/red_river_0p1_masks.I181_J31-170.nc"
masked.data <- ApplySpatialMask(sample.data, spat.mask)

###Initalize temporal mask functions
source("../ApplyTemporalMask.R")

###Apply all masks in a file to a dataset
time.mask <- "sample_time_mask.nc"
time.data <- sample.nc$dim$time$vals
all.masked.data <- ApplyTemporalMask(sample.data, time.mask, time.data)
