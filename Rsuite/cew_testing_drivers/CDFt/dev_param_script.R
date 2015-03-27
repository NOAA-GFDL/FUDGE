library(ncdf4)
out.nc <- nc_open("/home/cew/Code/testing//tasmax_day_testing-dev1-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc")
dev1.tasmax <- ncvar_get(out.nc, "tasmax")
out.nc <- nc_open("/home/cew/Code/testing//tasmax_day_testing-dev5-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc")
dev5.tasmax <- ncvar_get(out.nc, "tasmax")
out.nc <- nc_open("/home/cew/Code/testing//tasmax_day_testing-dev5-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc")
dev0.5.tasmax <- ncvar_get(out.nc, "tasmax")
out.nc <- nc_open("/home/cew/Code/testing//tasmax_day_testing-dev10-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc")
dev10.tasmax <- ncvar_get(out.nc, "tasmax")

summary(as.vector(dev0.5.tasmax))
summary(as.vector(dev1.tasmax))
summary(as.vector(dev5.tasmax))
summary(as.vector(dev10.tasmax))

dev1 <- dev1.tasmax[!is.na(dev1.tasmax)]
dev5 <- dev5.tasmax[!is.na(dev5.tasmax)]
dev0.5 <- dev0.5.tasmax[!is.na(dev0.5.tasmax)]
dev10 <- dev10.tasmax[!is.na(dev10.tasmax)]

summary(as.vector(dev0.5), digits=6)
summary(as.vector(dev1), digits=6)
summary(as.vector(dev5), digits=6)
summary(as.vector(dev10), digits=6)

hist(dev0.5, breaks=20)
hist(dev1, breaks=20)
hist(dev5, breaks=20)
hist(dev10, breaks=20)

out.nc <- nc_open("/home/cew/Code/testing//tasmax_day_testing-dev2-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc")
dev2.tasmax <- ncvar_get(out.nc, "tasmax")
dev2 <- dev2.tasmax[!is.na(dev2.tasmax)]
summary(as.vector(dev2))
hist(dev2)
hist(dev2-dev1)

###bias correction statistic
out.nc <- nc_open("/archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/historical//atmos/day/r1i1p1/v20111006/tasmax/SCCSC0p1/OneD/tasmax_day_MPI-ESM-LR_historical_r1i1p1_SCCSC0p1_19610101-20051231.I300_J31-170.nc")
predict.tasmax <- ncvar_get(out.nc, "tasmax")
out.nc <- nc_open("/archive/esd/PROJECTS/DOWNSCALING///OBS_DATA/GRIDDED_OBS//livneh/historical//atmos/day/r0i0p0/v1p2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I300_J31-170.nc")
train.tasmax <- ncvar_get(out.nc, "tasmax")

data.tasmax.diff <- abs(predict.tasmax-train.tasmax)
mean.tasmax.diff <- apply(data.tasmax.diff, 1, mean)

flag.outliers <- function(data, value){
  return(data > value)
}

out.nc <- nc_open("/archive/esd/PROJECTS/DOWNSCALING///OBS_DATA/GRIDDED_OBS//livneh/historical//atmos/day/r0i0p0/v1p2/tasmax/SCCSC0p1/OneD/tasmax_day_livneh_historical_r0i0p0_SCCSC0p1_19610101-20051231.I300_J31-170.nc")
future.tasmax <- ncvar_get(out.nc, "tasmax")

dev2.flagged <- apply(abs(dev2-future.tasmax), 1, flag.outliers)


