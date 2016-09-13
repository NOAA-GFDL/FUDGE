#fudgeio_primitives_testing

library(ncdf4)
library(ncdf4.helpers)
source("/home/cew/Code/section_8_tools/s8_utils/S8Utils.R")

tasmax.file <- "/home/cew/Code/testing/test_minfiles//9_pt_tasmax.nc"
tasmin.file <- "/home/cew/Code/testing/test_minfiles//9_pt_tasmin.nc"
pr.file     <- "/home/cew/Code/testing/test_minfiles//9_pt_pr.nc"

file.list <- c(tasmax.file, tasmin.file, pr.file)
var.list <- c('tasmax', 'tasmin', 'pr')
output <- build.fudge.object(file.list, var.list,)

output.new <- build.legacy.fudge.object(tasmax.file, 'tasmax', 
                                        dim=c('spatial', 'temporal'))


#So: Tests for checking input files

################################################################################
##Okay, now try that again on a multi-ens file. Does the time dim parsing work properly?

multiens.file <- paste0("/archive/esd/PROJECTS/DOWNSCALING/GCM_DATA/NCPP/", 
                       "GFDL-HIRAM-C360-COARSENED/sst2090/day/atmos/day/", 
                       "r1to3i1p1/v20110601/zg500/US48/zg500_day_GFDL-HIRAM-", 
                       "C360-COARSENED_sst2090_r1to3i1p1_US48_20860101-20951231_ensmbl.nc")

test.fudge <- build.fudge.object(gcp.tmpdir(multiens.file), 'zg500', 
                                         dim=c('spatial'), 
                                        subset=list(x=c(1,10), y=c(100,114)))


test.legacy <- build.legacy.fudge.object(gcp.tmpdir(multiens.file), 'zg500', 
                                         dim=c('spatial'))

spatstat.file <- gcp.tmpdir(paste0("/work/cew/testing/0831/area_stat_file.nc"))
test.legacy.ii <- build.legacy.fudge.object(spatstat.file, 'tasmaxDiff_AreaMin', 
                                            dim=c('spatial', 'ensemble'))
test.legacy.iii <- build.legacy.fudge.object(spatstat.file, 'tasmaxDiff_AreaMin', 
                                            dim=c('temporal'))

tempstat.file <- gcp.tmpdir(paste0("/work/cew/testing/0831/stat_file_mon.nc"))
test.legacy.iii <- build.legacy.fudge.object(tempstat.file, 'tasmaxDiffp99mon', 
                                             dim='spatial')
test.legacy.iv <- build.legacy.fudge.object(tempstat.file, 'tasmaxDiffp99mon', 
                                             dim='temporal')

################################################################################
####Need to test the writing functions (or, well, function) as well

test.output <- WriteNC("/home/cew/Code/testing/test_new_io.nc",
                       list(tasmax=output.new$tasmax),'tasmax', 
                       dim.list=output.new$dim,
                       var.data=output.new$vars,
                       units=output.new$att_table$units$value,
                       lname=paste("Maksked ", data$long_name$value,sep=''),
                       prec=attr(data$clim.in, "prec"), 
                       clone_globals=T, global_clone_file=c(input,maskfile),
                       verbose=parsed.args$verbose
)


################################################################################

r1.file <- gcp.tmpdir(paste0("/archive/esd/PROJECTS//DOWNSCALING/GCM_DATA//NCPP",
                             "/GFDL-HIRAM-C360-COARSENED//sst2090//day//atmos//day//r1i1p1/",
                             "/v20110601//tasmax//US48//tasmax_day_GFDL-HIRAM-C360-COARSENED_sst2090_r1i1p1",
                             "_US48_20860101-20951231.nc"))
r2.file <- gcp.tmpdir(paste0("/archive/esd/PROJECTS//DOWNSCALING/GCM_DATA//NCPP",
                             "/GFDL-HIRAM-C360-COARSENED//sst2090//day//atmos//day//r2i1p1/",
                             "/v20110601//tasmax//US48//tasmax_day_GFDL-HIRAM-C360-COARSENED_sst2090_r2i1p1",
                             "_US48_20860101-20951231.nc"))

test.output <- build.fudge.object(c(r1.file,r2.file), c('tasmax', 'tasmax'))