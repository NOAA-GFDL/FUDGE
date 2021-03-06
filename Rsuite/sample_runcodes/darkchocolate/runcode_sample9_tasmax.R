########Input R parameters generated by experGen suite of tools for use in driver script -------
rm(list=ls())

#--------------predictor and target variable names--------#
 	predictor.vars <- 'tasmax' 
 	target.var <- 'tasmax'
# predictor.vars <- 'pr'
# target.var <- 'pr'
#--------------grid region, mask settings----------#
grid <- 'SCCSC0p1' 
spat.mask.dir_1 <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/geomasks/red_river_0p1/OneD/' 
spat.mask.var <- 'red_river_0p1_masks' 
ds.region <- 'RR'
#--------------- I,J settings ----------------#
file.j.range <- 'J31-170' 
i.file <- 300   
j.start <- 31 
j.end <- 170 
loop.start <-  j.start - (j.start-1)
loop.end <-  j.end - (j.start-1)
#------------ historical predictor(s)----------# 
hist.file.start.year_1 <- 19610101 
hist.file.end.year_1 <- 20051231
hist.train.start.year_1 <- 19610101
hist.train.end.year_1 <- 20051231 
hist.scenario_1 <- 'historical_r1i1p1'
hist.nyrtot_1 <- (hist.train.end.year_1 - hist.train.start.year_1) + 1
hist.model_1 <- 'MPI-ESM-LR' 
hist.freq_1 <- 'day' 
hist.indir_1 <- paste0('/archive/esd/PROJECTS/DOWNSCALING//GCM_DATA/CMIP5//MPI-ESM-LR/historical/day/atmos/day/r1i1p1/v20111006/', target.var,'/SCCSC0p1/OneD/') 
	hist.time.window <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231_olap.nc' 
#hist.time.window <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc'
#------------ future predictor(s) -------------# 
fut.file.start.year_1 <- 20060101 
fut.file.end.year_1 <- 20991231 
fut.train.start.year_1 <- 20060101 
fut.train.end.year_1 <- 20991231 
fut.scenario_1 <- 'rcp45_r1i1p1'
fut.nyrtot_1 <- (fut.train.end.year_1 - fut.train.start.year_1) + 1
fut.model_1 <- 'MPI-ESM-LR' 
fut.freq_1 <- 'day' 
fut.indir_1 <- paste0('/archive/esd/PROJECTS/DOWNSCALING//GCM_DATA/CMIP5//MPI-ESM-LR/rcp45/day/atmos/day/r1i1p1/v20111006/', target.var, '/SCCSC0p1/OneD/')
fut.time.window <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_20060101-20991231_olap.nc'
#fut.time.window <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc'
 fut.time.trim.mask <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc'
#fut.time.trim.mask <- 'na'
#-------------- predictor directory suffix -----------#
pred.dir.suffix <- '/SCCSC0p1/OneD/' #use this predictor variable and this  suffix with fut.indir_1 and hist.indir_1
#------------- target -------------------------# 
target.file.start.year_1 <- 19610101 
target.file.end.year_1 <- 20051231 
target.train.start.year_1 <- 19610101 
target.train.end.year_1 <- 20051231 
target.scenario_1 <- 'historical_r0i0p0'
target.nyrtot_1 <- (target.train.end.year_1 - target.train.start.year_1) + 1 
target.model_1 <- 'livneh'
target.freq_1 <- 'day' 
target.indir_1 <- paste0('/archive/esd/PROJECTS/DOWNSCALING//OBS_DATA/GRIDDED_OBS//livneh/historical/day/atmos/day/r0i0p0/v1p2/',
                         target.var,'/SCCSC0p1/OneD/')
	target.time.window <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_pm2weeks_19610101-20051231_olap.nc'
#target.time.window <- '/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc'

#------------- method name k-fold specs-----------------------#
        ds.method <- 'CDFt' 
	ds.experiment <- paste('RR2txp1', ds.method, 'C34atTx_offset_dc4L01K00', sep="-") 
	k.fold <- 0 
	
#-------------- output -----------------------#
	output.dir <- '/home/cew/Code/testing/'
	mask.output.dir <- '/home/cew/Code/testing/' 
#-------------  custom -----------------------#
        args=list(dev=1,npas='default') 
#args=list('na')
 #Number of "cuts" for which quantiles will be empirically estimated (Default is 100 in CDFt package).
#-------------- pp ---------------------------#
        #mask.list <- list(mask1=list(type='SBiasCorr',adjust.out='off',qc.mask='on',qc_options=list(botlim=-6.,toplim=6.)))
mask.list <- list("na")
# pr_opts=list(pr_threshold_in='us_trace',pr_freqadj_in='off',pr_conserve_in='on',
#              pr_threshold_out='us_trace',pr_conserve_out='on', #'us_trace'
#              apply_0_mask='off')
################### others ###################################
#---------------- reference to go in globals ----------------------------------- 
	configURL <-' Ref:http://gfdl.noaa.gov/esd_experiment_configs'
# ------ Set FUDGE environment ---------------
	FUDGEROOT = Sys.getenv(c("FUDGEROOT"))
	#FUDGEROOT <- '/home/a1r/local/opt/fudge//candy-testing/fudge2014/'
	FUDGEROOT <- '/home/cew/Code/fudge2014/'
	print(paste("FUDGEROOT is now activated:",FUDGEROOT,sep=''))
	#BRANCH <- 'candy-testing'
	BRANCH <- 'anything-but-candy-testing'
################ call main driver ###################################
print(paste("START TIME:",Sys.time(),sep=''))

#----------Use /vftmp as necessary---------------# 
TMPDIR <- ""
#TMPDIR = Sys.getenv(c("TMPDIR"))
#if (TMPDIR == ""){
#  stop("ERROR: TMPDIR is not set. Please set it and try it") 
#  }
#########################################################################
if(spat.mask.dir_1 != 'na'){
if((grepl('^/archive',spat.mask.dir_1)) | (grepl('^/work',spat.mask.dir_1))){
spat.mask.dir_1 <- paste(TMPDIR,spat.mask.dir_1,sep='')
}}
if(hist.indir_1 != 'na'){
if((grepl('^/archive',hist.indir_1)) | (grepl('^/work',hist.indir_1))){
hist.indir_1 <- paste(TMPDIR,hist.indir_1,sep='')
}}
if(fut.indir_1 != 'na'){
if((grepl('^/archive',fut.indir_1)) | (grepl('^/work',fut.indir_1))){
fut.indir_1 <- paste(TMPDIR,fut.indir_1,sep='')
}}
if(hist.indir_1 != 'na'){
if((grepl('^/archive',hist.indir_1)) | (grepl('^/work',hist.indir_1))){
target.indir_1 <- paste(TMPDIR,target.indir_1,sep='')
}}
if(target.time.window != 'na'){
if((grepl('^/archive',target.time.window)) | (grepl('^/work',target.time.window))){
target.time.window <- paste(TMPDIR,target.time.window,sep='')
}}
if(hist.time.window != 'na'){
if((grepl('^/archive',hist.time.window)) | (grepl('^/work',hist.time.window))){
hist.time.window <- paste(TMPDIR,hist.time.window,sep='')
}}
if(fut.time.window != 'na'){
if((grepl('^/archive',fut.time.window)) | (grepl('^/work',fut.time.window))){
fut.time.window <- paste(TMPDIR,fut.time.window,sep='')
}}
if(fut.time.trim.mask != 'na'){
if((grepl('^/archive',fut.time.trim.mask)) | (grepl('^/work',fut.time.trim.mask))){
fut.time.trim.mask <- paste(TMPDIR,fut.time.trim.mask,sep='')
}
}
output.dir <- paste(TMPDIR,output.dir,sep='')
mask.output.dir <- paste(TMPDIR,mask.output.dir,sep='')

#########################################################################
#-------------------------------------------------#
#source(paste(FUDGEROOT,'Rsuite/Drivers/',ds.method,'/Driver_',ds.method,'.R',sep=''))
source(paste(FUDGEROOT,'Rsuite/Drivers/','Master_Driver.R',sep=''))
