#test_apply_all_masks.R
#Unit tests via RUnit for testing the spatial/temproral mask application functions
library(RUnit)
dummy  <-  rep(12, 2000)
dim(dummy) <- c(10,10,20)

test_ApplySpatialMask<-function(){
  source('../ApplySpatialMask.R')
  #Test for very simple mask function
  dummy  <-  rep(12, 2000)
  dim(dummy) <- c(10,10,20)
  dummy1 <- dummy
  dummy1[5,5,] <- NA
  print(paste("number of nas in sample array", length(dummy1[is.na(dummy1)==TRUE])))
  masknc<-"dummy_spatial_mask.nc"
  print('testing simple mask function')
  checkEquals(ApplySpatialMask(dummy, masknc), dummy1)
  print("Testing for supplying a mask name")
  masknc<-"dummy_oddly_named_spatial_mask.nc"
  checkEquals(ApplySpatialMask(dummy, masknc, maskname = "oddly_named_spatial_mask"), dummy1)
  print("Testing for subsetting of a larger mask")
  masknc<-"dummy_large_spatial_mask.nc"
  xy_data <- seq(1:10)
  checkEquals(ApplySpatialMask(dummy, "dummy_large_spatial_mask.nc", dataLon=xy_data, dataLat=xy_data), dummy1)
}

test_ApplyTemporalMask<-function(){
  source('../ApplyTemporalMask.R')
  print("Test for very simple mask function")
  dummy <-rep(28, 10*10*10958)
  dim(dummy) <- c(10,10,10958)
  tmask_nc<-"/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_19790101-20081231.nc"
  load("masked_mon01_test_data.Rdata")
  load("tmask_nc_timeseries.Rdata")
  load("all_tmask_nc_output.Rdata")
  print("Test the simple named mask case")
  checkEquals(ApplyTemporalMask(dummy, tmask_nc, time_data, maskname="mask_mon01"), single_out)#dummy3Keep in mind the type agreement. Could be an issue later.
  print("Test the error thrown by an overlapping mask for the run case")
  checkException(ApplyTemporalMask(dummy, "colliding_masks.nc", time_data, type="run"), "Mask collision error: Masks within colliding_masks.nc, 
provided as ESD downscaling mask file, either overlap or do not wholly cover the timeseries.")
  print("Test for returning a mismatched mask/data error")
  dummy4 <- rep(28, 10*10*10)
  dim(dummy4) <- c(10, 10, 10)
  checkException(ApplyTemporalMask(dummy4, tmask_nc, time_data), paste("Temporal mask dimension error: mask", tmask_nc, "was of length 10958", 
                 "and was expected to be of length", length(dummy4[1,1,])))
  print("Test for returning the results of more than one mask in a file (note: this takes about ten minutes to run")
  checkEquals(ApplyTemporalMask(dummy, tmask_nc, time_data),out_total)
}

create_things<-function(){
  spatial_mask  <-  rep(1, 100)
  dim(spatial_mask) <- c(10,10)
  spatial_mask[5,5] <- 99
  #Create normal ncdf file
  x_dim<-ncdim_def("lon", units = "degrees_east", seq(from=1, to=10, by=1), create_dimvar=TRUE)
  y_dim<-ncdim_def("lat", units = "degrees_north", seq(from=1, to=10, by=1), create_dimvar=TRUE)
  maskvar<-ncvar_def("spatial_mask", units = "", list(x_dim, y_dim), longname = "dummy mask variable", missval = 99, prec="integer")
  thisnc<-nc_create("dummy_spatial_mask.nc", maskvar, verbose=FALSE)
  ncvar_put(thisnc, "spatial_mask", spatial_mask)
  nc_close(thisnc)
  
  #Create long-named ncdf file
  x_dim<-ncdim_def("lon", units = "degrees_east", seq(from=1, to=10, by=1), create_dimvar=TRUE)
  y_dim<-ncdim_def("lat", units = "degrees_north", seq(from=1, to=10, by=1), create_dimvar=TRUE)
  maskvar2<-ncvar_def("oddly_named_spatial_mask", units = "", list(x_dim, y_dim), longname = "dummy mask variable", missval = 99, prec="integer")
  thisnc<-nc_create("dummy_oddly_named_spatial_mask.nc", maskvar2, verbose=FALSE)
  ncvar_put(thisnc, "oddly_named_spatial_mask", spatial_mask)
  nc_close(thisnc)
  #Create large spatial mask ncdf file
  large_spatial_mask  <-  rep(1, 26^2)
  dim(large_spatial_mask) <- c(26,26)
  large_spatial_mask[11,11] <- 99
  x_dim<-ncdim_def("lon", units = "degrees_east", seq(from=-5, to=20, by=1), create_dimvar=TRUE)
  y_dim<-ncdim_def("lat", units = "degrees_north", seq(from=-5, to=20, by=1), create_dimvar=TRUE)
  maskvar3<-ncvar_def("spatial_mask", units = "", list(x_dim, y_dim), longname = "dummy mask variable", missval = 99, prec="integer")
  thisnc<-nc_create("dummy_large_spatial_mask.nc", maskvar3, verbose=FALSE)
  ncvar_put(thisnc, "spatial_mask", large_spatial_mask)
  nc_close(thisnc)
  #
}
