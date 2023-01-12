#test_MaskIO.R
#Unit tests via RUnit for testing the spatial/temporal mask reading
#mask QC and mask application functions

library(RUnit)
library(ncdf4)

##Or you know what? I don't have to do teh sample mask application. 
##The sample mask application is now three lines long. Three.
#lines. Long. I overengineered that function like woah, and now its sins have come
#home to rest. 

test_ReadMaskNC <- function(){
  sample.mask.nc <- ""
  sample.fake.mask.nc <- ""
  print("Testing reading sample mask")
  print("Testing reading sample mask with time dimension")
  print("Testing reading sample mask with no masks in the file")
}

test_QCTimeMask <- function(){
  mask.file <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc'
  mask.object <- load()
  print("Testing functional mask")
  checkEquals(QCTimeMask(ReadMaskNC()), mask.object)
  print("Testing mask with overlapping time periods when run==TRUE")
  colliding.mask.file <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231.nc'
  colliding.mask.object <- ReadMaskNC(nc_open(colliding.mask.file))
  checkError(QCTimeMask(colliding.mask.object, run=TRUE), 
             "Mask Collision Error: Masks within, , provided as an ESDGen mask file, overlap and do not cover the entire time series.")
}

test_QCAllTimeMasks <- function(){
  
  print("Testing check of three agreeing masks")
  print('Testing check of three masks with unsimilar time series')
  print("Testing check of uneven number of masks in one mask")
  print("Testing check of overlapping masks on CDFt")
}