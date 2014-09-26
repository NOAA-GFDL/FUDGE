#'QCMaskScript

setwd("/home/cew/Code/fudge2014/")
library(ncdf4)

filename.data <- "/work/cew/downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtxp1-CDFt-A38h-anL01K00/tasmax/RR/OneD/v20140108/tasmax_day_RRtxp1-CDFt-A38h-anL01K00_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc"
filename.qc <- "/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/CMIP5/MPI-ESM-LR/rcp85/atmos/day/r1i1p1/v20111014/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc"

data.nc <- nc_open(filename.data)
qc.nc <- nc_open(filename.qc)

var.name <- names(data.nc$var)
var.name <- var.name[!(var.name %in% c("lat_bnds", "j_offset", "lon_bnds", 
                                       "height", "i_offset", "time_bnds"))]

print(var.name)
data <- ncvar_get(data.nc, var.name, collapse_degen=FALSE)
print(summary(as.vector(data)))
qc.data <- ncvar_get(qc.nc, var.name, collapse_degen=FALSE)
print(summary(as.vector(qc.data)))
source("Rsuite/cew_testing_drivers/CDFt/CreateQCMask.R")

qc.test <- 'sdev2'
qc.mask <- CreateQCMask(data, qc.data, qc.test)

#Create output file
filename.outdir <- dirname(filename.data)
filename.name <- paste(sub(pattern=".nc",replacement="", x=basename(filename.data)), 
                       "-", qc.test, "1-QCMask.nc", sep="")

#system(paste("mkdir ", filename.outdir, "/QCMasks/", sep=""))
system(paste("mkdir ", "/home/cew/Code/testing", "/QCMasks/", sep=""))
# filepath.out <- paste(filename.outdir,"/QCMasks/", filename.name, sep="" )
filepath.out <- paste('/home/cew/Code/testing/',"/QCMasks/", filename.name, sep="" )
if(!file.exists(filepath.out)){
command.string <- paste("ncks -x -v ", "'", var.name, "' ", filename.data, 
                        " ", filepath.out, sep="")
print(command.string)
system(command.string)
#Finally, write to file
out.nc<-nc_open(filepath.out, write=TRUE)
out.var <- ncvar_def(paste(var.name, 'qc_mask', sep="_"), #TODO: Add test to the filename? 
                     units='boolean', 
                     list(data.nc$dim$lon, data.nc$dim$lat),#, data.nc$dim$time), 
                     prec='integer')
out.nc <- ncvar_add(out.nc, out.var, verbose=TRUE)
print('variable added')
ncvar_put(out.nc, out.var, qc.mask, verbose=TRUE)
nc_close(out.nc)
}else{
  print("Remove the file from the output directory.")
}

test.nc <- nc_open(filepath.out)
test.var <- ncvar_get(test.nc, paste(var.name, 'qc_mask', sep="_"))
test.var