#'QCMaskScript

setwd("/home/cew/Code/fudge2014/")
library(ncdf4)
source("Rsuite/cew_testing_drivers/CDFt/CreateQCMask.R")
source("Rsuite/FudgeIO/src/ReadMaskNC.R")
source("Rsuite/FudgeIO/src/CreateTimeseries.R")
system('module load gcp')
system('module load nco')
#181, 370
lons <- 300
lone <- 301 

tmpdir <- paste("/vftmp/cew/", round(runif(n=1)*10000), sep="")
mkdir.commandstr <- paste("mkdir", tmpdir, sep=" ")
print(mkdir.commandstr)
system(mkdir.commandstr)

data.dir <- "/work/cew/downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtxp1-CDFt-A38L01K00/tasmax/RR/OneD/v20140108/"
# commandstr <- paste('gcp -cdr', data.dir, paste(tmpdir, data.dir, sep=""))
# print(commandstr)
# system(commandstr)
hist.pred.dir <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/historical/atmos/day/r1i1p1/v20111006/tasmax/SCCSC0p1/OneD/"

hist.targ.dir <- "/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/atmos/day/r0i0p0/v1p2/tasmax/SCCSC0p1/OneD/"
# commandstr <- paste('gcp -cd', hist.targ.dir, paste(tmpdir, hist.targ.dir, sep=""))
# print(commandstr)
# system(commandstr)
fut.pred.dir <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/"
# commandstr <- paste('gcp -cd', fut.pred.dir, paste(tmpdir, fut.pred.dir, sep=""))
# print(commandstr)
# system(commandstr)
print('directories created')

qc.test <- 'kdAdjust'
out.dir <- paste(data.dir, "/QCMasks/", sep="")
if(!file.exists(out.dir)){
  print(out.dir)
  system(paste('mkdir', out.dir))
}else{
  stop('QC directory already exists. Please delete and try again.')
}

#If there are time windows, time windows will be used for all coordinate points
time.window.file <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc"
time.data.window.file <- "/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc"

time.window <- ReadMaskNC(nc_open(time.window.file))
time.data.window <- ReadMaskNC(nc_open(time.data.window.file))

#And so will window.dim time dimension
# window.dim <- ncdim_def(name='mask_time', units='period', vals=seq(1:length(time.window)), 
#                         unlim=FALSE, create_dimvar=TRUE, calendar=NA, longname="Number of time windows in mask")

for (i in lons:lone){
  print(paste('Starting index', i, "of", lone))
  index <- paste("20991231.I", as.character(i), sep="")
  
  data.file <- list.files(data.dir, pattern=as.character(index))
  data.path <- paste(data.dir, data.file, sep="")
  hist.pred.path <- paste(hist.pred.dir, list.files(hist.pred.dir, pattern=as.character(i)), sep="") #Note separate search in hist and future
  temp.hist.pred.path <- paste(tmpdir, hist.pred.path, sep="")
  commandstr <- paste('gcp -cd', hist.pred.path, temp.hist.pred.path)
  print(commandstr)
  system(commandstr)
  hist.targ.path <- paste(hist.targ.dir, list.files(hist.targ.dir, pattern=as.character(i)), sep="")
  temp.hist.targ.path <- paste(tmpdir, hist.targ.path, sep="")
  commandstr <- paste('gcp -cd', hist.targ.path, temp.hist.targ.path)
  print(commandstr)
  system(commandstr)
  fut.pred.path <- paste(fut.pred.dir, list.files(fut.pred.dir, pattern=as.character(index)), sep="")
  temp.fut.pred.path <- paste(tmpdir, fut.pred.path, sep="")
  commandstr <- paste('gcp -cd', fut.pred.path, temp.fut.pred.path)
  print(commandstr)
  system(commandstr)
  
  out.path <- paste(out.dir, paste(sub(pattern=".nc",replacement="", x=basename(data.file)), 
                                   "-", qc.test, "-QCMask.nc", sep=""), sep="")  
  
  #Open files and obtain data
  data.nc <- nc_open(data.path)
  hist.pred.nc <- nc_open(temp.hist.pred.path)
  hist.targ.nc <- nc_open(temp.hist.targ.path)
  fut.pred.nc <- nc_open(temp.fut.pred.path)
  
  var.name <- names(data.nc$var)
  var.name <- var.name[!(var.name %in% c("lat_bnds", "j_offset", "lon_bnds", 
                                         "height", "i_offset", "time_bnds"))]
  data <- ncvar_get(data.nc, var.name, collapse_degen=FALSE)
  hist.pred <- ncvar_get(hist.pred.nc, var.name, collapse_degen=FALSE)
  hist.targ <- ncvar_get(hist.targ.nc, var.name, collapse_degen=FALSE)
  fut.pred <- ncvar_get(fut.pred.nc, var.name, collapse_degen=FALSE)
  
#  qc.test <- 'kdAdjust'
  qc.mask <- CreateQCMask(data, qc.test = qc.test, var='tasmax', 
                           hist.pred=hist.pred, hist.targ=hist.targ, fut.pred=fut.pred, 
                           time.window=time.window, time.data.window=time.data.window)
  
  #Now, begin file writing operations
  command.string <- paste("ncks -x -v ", "'", var.name, "' ", data.path, 
                          " ", out.path, sep="")
  print(command.string)
  system(command.string)
  
  out.nc<-nc_open(out.path, write=TRUE)
  out.var <- ncvar_def(paste(var.name, 'qc_mask', sep="_"), #TODO: Add test to the filename? 
                       units='boolean', 
                       list(data.nc$dim$lon, data.nc$dim$lat),#, data.nc$dim$time), 
                       prec='integer')
  out.nc <- ncvar_add(out.nc, out.var, verbose=TRUE)
  print('variable added')
  ncvar_put(out.nc, out.var, qc.mask, verbose=TRUE)
  nc_close(out.nc)
  
}

command.cat.string <- paste('ncecat -v', paste(var.name, 'qc_mask', sep="_"), '-h',
                            out.dir, paste(out.dir, 'Concatenated_output.nc'))
print(command.cat.string)
system(command.cat.string)
print("Done with concatenation.")

# filename.data <- "/work/cew/downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtxp1-CDFt-A38h-anL01K00/tasmax/RR/OneD/v20140108/tasmax_day_RRtxp1-CDFt-A38h-anL01K00_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc"
# #filename.qc <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc"
# 
# data.nc <- nc_open(filename.data)
# qc.nc <- nc_open(filename.qc)
# 
# var.name <- names(data.nc$var)
# var.name <- var.name[!(var.name %in% c("lat_bnds", "j_offset", "lon_bnds", 
#                                        "height", "i_offset", "time_bnds"))]
# 
# print(var.name)
# data <- ncvar_get(data.nc, var.name, collapse_degen=FALSE)
# print(summary(as.vector(data)))
# qc.data <- ncvar_get(qc.nc, var.name, collapse_degen=FALSE)
# print(summary(as.vector(qc.data)))
# source("Rsuite/cew_testing_drivers/CDFt/CreateQCMask.R")
# 
# # qc.test <- 'sdev2'
# # qc.mask <- CreateQCMask(data, qc.data, qc.test)
# 
# qc.test <- 'kdAdjust'
# qc.mask <- CreateQCMask( data, qc.test, var='tasmax', 
#                          hist.pred=hist.pred, hist.targ=hist.targ, fut.pred=fut.pred, 
#                          time.window=time.window, time.data.window=time.data.window)
# 
# #Create output file
# filename.outdir <- dirname(filename.data)
# filename.name <- paste(sub(pattern=".nc",replacement="", x=basename(filename.data)), 
#                        "-", qc.test, "-QCMask.nc", sep="")
# 
# #system(paste("mkdir ", filename.outdir, "/QCMasks/", sep=""))
# system(paste("mkdir ", "/home/cew/Code/testing", "/QCMasks/", sep=""))
# # filepath.out <- paste(filename.outdir,"/QCMasks/", filename.name, sep="" )
# filepath.out <- paste('/home/cew/Code/testing/',"/QCMasks/", filename.name, sep="" )
# if(!file.exists(filepath.out)){
#   command.string <- paste("ncks -x -v ", "'", var.name, "' ", filename.data, 
#                           " ", filepath.out, sep="")
#   print(command.string)
#   system(command.string)
#   #Finally, write to file
#   if(is.null(time.window)){
#     out.nc<-nc_open(filepath.out, write=TRUE)
#     out.var <- ncvar_def(paste(var.name, 'qc_mask', sep="_"), #TODO: Add test to the filename? 
#                          units='boolean', 
#                          list(data.nc$dim$lon, data.nc$dim$lat),#, data.nc$dim$time), 
#                          prec='integer')
#     out.nc <- ncvar_add(out.nc, out.var, verbose=TRUE)
#     print('variable added')
#     ncvar_put(out.nc, out.var, qc.mask, verbose=TRUE)
#     nc_close(out.nc)
#   }else{
#     window.dim <- ncdim_def(name='mask_time', units='period', vals=seq(1:length(time.window)), 
#                             unlim=FALSE, create_dimvar=TRUE, calendar=NA, longname="Number of time windows in mask")
#     out.nc<-nc_open(filepath.out, write=TRUE)
#     out.var <- ncvar_def(paste(var.name, 'qc_mask', sep="_"), #TODO: Add test to the filename? 
#                          units='boolean', 
#                          list(data.nc$dim$lon, data.nc$dim$lat, window.dim), 
#                          prec='integer')
#     out.nc <- ncvar_add(out.nc, out.var, verbose=TRUE)
#     print('variable added')
#     ncvar_put(out.nc, out.var, qc.mask, verbose=TRUE)
#     nc_close(out.nc)
#   }
# }else{
#   print("Remove the file from the output directory.")
# }
# 
# 
# test.nc <- nc_open(filepath.out)
# test.var <- ncvar_get(test.nc, paste(var.name, 'qc_mask', sep="_"))
# test.var