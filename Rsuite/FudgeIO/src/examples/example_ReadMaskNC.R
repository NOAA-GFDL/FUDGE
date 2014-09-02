#Example code for invoking the ReadMaskNC script upon a spatial and temporal mask
source("../ReadMaskNC.R")
source("../CreateTimeseries.R")
source("../CreateTimeseries.R")
source("../../../FudgePreDS/src/TimeMaskQC.R")

#####Time mask first
library(ncdf4)
time.window <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
mask.nc <- nc_open(time.window)
library(PCICt)
masked.out <- ReadMaskNC(mask.nc, verbose=TRUE)

###And maks sure that TimeMaskQC still works
hist.train.mask <-'/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc'
hist.targ.mask<- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc'
esd.gen.mask <-'/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc'
output.ready <- TimeMaskQC(hist.train.mask, hist.targ.mask, esd.gen.mask, k=0, method="faaaaaake")


#####Code for testing the reading and application of a spatial mask
spat.mask <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/geomasks/red_river_0p1/OneD/red_river_0p1_masks.I300_J31-170.nc'
spat.nc <- nc_open(spat.mask)

mask <- ReadMaskNC(spat.nc, verbose=TRUE)