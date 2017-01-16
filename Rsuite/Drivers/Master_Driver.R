#' driver_a1rv2.r
#' phase 1 of driver script implementation for FUDGE: CDFt train driver
#' originally created: a1r,08/2014
#' Modified heavily by 4-6-2015

fudge.start.time <- proc.time()[3]
############### Library calls, source necessary functions ###################################################
#TODO the following sapplys and sourcing should be a library call
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgePreDS/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeQC/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeTrain/',sep=''), full.names=TRUE), source);
source(paste(FUDGEROOT,'Rsuite/Drivers/LoadLib.R',sep=''))
source(paste(FUDGEROOT, 'Rsuite/Drivers/UtilityFunctions.R', sep=""))
#source("/home/cew/Code/h_testing//FudgeIO_reboot/new_driver/looping_xy.R")
#source("/home/cew/Code/h_testing/FudgeIO_reboot/fudgeio_primitives.R")
#sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/Drivers/',sep=''), full.names=TRUE), source);
#source(paste(FUDGEROOT,'Rsuite/drivers/CDFt/TrainDriver.R',sep=''))

#-------Add traceback call for error handling -------
# stored.opts <- options()[c('warn', 'error', 'showErrorCalls')]
# error.handler.function <- function(){
#   #message(writeLines(traceback()))
#   traceback()
#   message("Quitting gracefully with exit status 1")
#   #If quitting from a non-interactive session, a status of 1 should be sent. Test this.
#   #quit(save="no", status=1, runLast=FALSE, save=FALSE) #runLast=FALSE
# }
# #previously traceback
# options(error=error.handler.function, warn = 1, showErrorCalls=TRUE)
###See if there's a good way to return back to the original settings
###after this point. Probably not a component of a --vanilla run.
###But it unquestionably simplifies debugging.

#------- Add libraries -------------
LoadLib(ds.method)
#-------End Add libraries ---------
###############################################################################################################
console.args <- commandArgs(TRUE)
print(console.args)
# i is longitude index in the netCDF file, j is latitude
if (length(console.args) > 0){
  oh_by <- as.numeric(console.args[1])
  mh_by <- as.numeric(console.args[2])
  mf_by <- as.numeric(console.args[3])
  i_idx <- as.numeric(console.args[4])
  i_max <- as.numeric(console.args[5])
  x.pad <- "NA"
}else{
  oh_by <- 1
  mh_by <- 1 #5 #3
  mf_by <- 1 #5 #3
  i_idx <- 41 #167 #pr_diff = 37 i800 = 53 #194 #10
  i_max <- 194
  x.pad <- "NA"
}

fudge.start.time <- proc.time()[3]
message("Setting downscaling method information")
ds.ref <- getDSInfo(ds.method)

message("Checking downscaling arguments")
ds.args <<- args
QCDSArguments(k=k.fold, ds.method = ds.method, args=args, 
              is.multivar=(length(predictor.vars) > 1), 
              is.multi_ens=(length(fut.pred.rip.list) > 1))

message("Checking the missing value arguments")
missval.args <- getMissvalArgs(args, ds.ref$missval_method)

message("Checking input predictor var order")
predictor.vars <- fix.var.order(predictor.vars, target.var)

if (mask.list=='na'){
  mask.list <- list(list(type='na', qc.mask='off', adjust.out='off'))
}

##Call the code that converts the instructions for section 3 and section 5
if(!exists('pre_ds')){ #put better check in here once you are done with the testing
  message('Conversion of pre- and post- downscaling adjustment input')
  if(exists('pr_opts')){
    pp.out <- adapt.pp.input(mask.list, pr_opts)
  }else{
    pp.out <- adapt.pp.input(mask.list)
  }
  pre_ds <- pp.out$pre_ds
  post_ds <- pp.out$post_ds
  train_qcmask <- pp.out$train_qcmask
}

##Make sure that you have the experiment rip ready to apply to
##other code (i.e. the actual rip experiment is being run under)
experiment.rip <- gsub("(.*_)", "", fut.scenario_1)
##Extra code added for compatibility with the darkchocolte-style
##runcodes that expergen is capable of producing
if(exists('RIP')){
  fut.pred.rip.list <- unlist(strsplit(RIP, split=","))
}else{
  message('Creating darkchocolate-compatible RIP list')
  fut.pred.rip.list <- unlist(strsplit(fut.scenario_1, "_"))[2]
}

#Initialize instructions for pre- and post-ds adjustment
message("Checking pre- and post-downscaling adjustments")
post.ds <- compact(lapply(post_ds, index.a.list, 'loc', 'outloop'))
post.ds.train <- compact(lapply(post_ds, index.a.list, 'loc', 'inloop'))
qc.maskopts <- qc.mask.check(post.ds.train, post.ds, train_qcmask)
pre.ds <- compact(lapply(pre_ds, index.a.list, 'loc', 'outloop'))
pre.ds.train <- compact(lapply(pre_ds, index.a.list, 'loc', 'inloop'))
#Generate metadata references for the pre- and post-processing functions
#Things that contain an ADJUSTMENT
post_ds_adj_tmp <- compact(lapply(post_ds, index.a.list, 'adjust.out', 'on'))
post_ds_inloop <- compact(lapply(post_ds_adj_tmp, index.a.list,'loc', 'inloop'))
post_ds_outloop <- compact(lapply(post_ds_adj_tmp, index.a.list,'loc', 'outloop'))
post_ds_adj <- c(post_ds_inloop, post_ds_outloop)
##Initialize a few globals for looping in loop.xy()
s3.outloop <- length(pre.ds) > 0
s3.inloop <- length(pre.ds.train) > 0
#training qc makss are their own thing
s5.inloop <- length(post_ds_inloop) > 0
s5.outloop <- length(post_ds_outloop) > 0
#And finally, check to see if a training (minsamplesize) qc mask
train_qcmask <- 'qc.train' %in% sapply(mask.list, '[[', 'type')

#Adapt some of the parameters being set by the f-to-g bridge code
if(!exists('pr_opts')){
  f.list <- override.f.defaults(args)
}else{
  f.list <- override.f.defaults(args, pr_opts)
}
min.present <- f.list$threshold
args <- f.list$args

#Then, check that the s3 and s5 adjustments you are performing make sense in
#this context
QCAdjustArguments(pre_ds, post_ds,
                  is.multivar=(length(predictor.vars) > 1),
                  is.multi.ens=(length(fut.pred.rip.list) > 1),
                  target.in.predictors = (target.var%in%predictor.vars),
                  predictor.vars, target.var)

#### Then, read in spatial and temporal masks. Those will be used
#### not only for the masks, but as an immediate check upon the dimensions
#### of the files being read in.

# # spatial mask read check
message("Checking for spatial masks vars")
if(spat.mask.dir_1!="na"){
  spat.mask.nc <- nc_open(file.path(dirname(spat.mask.dir_1), 
                                    paste0(spat.mask.var, ".nc")))
}else{
  spat.mask.nc <- NULL
  message("no spatial mask included; skipping to next step")
}

### Now, access input data sets
message("Reading in target data")

oh_x_subset <- get.FUDGE.subset(oh_by, i_idx, i_max)
if (ds.ref$multivar_target){
  #Right now, only applies to MBC
  OH.files <- c()
  target.indir_1 <- dirname(dirname(dirname(target.indir_1)))
  for (var in predictor.vars){
    print(hist.indir_1)
    print(target.indir_1)
    print(paste("reading target var", var))
    OH.files <- append(OH.files, get.FUDGE.filepath(target.indir_1, 
                                                    var,target.freq_1,target.model_1,
                                                    target.scenario_1,grid,
                                                    target.file.start.year_1,target.file.end.year_1, 
                                                    target.suffix, is_OH = F))
  }
  print(OH.files)
  OH <- build.FUDGE.dataset(OH.files, predictor.vars, list(x=oh_x_subset), 
                            dim=ds.ref$oh_dimlist, 
                            oh_by, spat.mask.nc)
}else{
  OH.path <- get.FUDGE.filepath(target.indir_1, target.var,target.freq_1,target.model_1,target.scenario_1,grid,
                                target.file.start.year_1,target.file.end.year_1, 
                                target.suffix, is_OH = T)
  OH <- build.FUDGE.dataset(OH.path, target.var, list(x=oh_x_subset), 
                            dim=ds.ref$oh_dimlist, 
                            oh_by, spat.mask.nc)
}
oh.dim <- dim(OH$data[[1]])
oh.coords <- get.loop.indices(oh.dim[1], oh.dim[2], oh_by)

message("Reading in historical predictor data")
MH.files <- c()
for (var in predictor.vars){
  MH.files <- append(MH.files, get.FUDGE.filepath(hist.indir_1, 
                                                  var,hist.freq_1,hist.model_1,
                                                  hist.scenario_1,grid,
                                                  hist.file.start.year_1,hist.file.end.year_1, 
                                                  hist.pred.suffix))
}
mh_x_subset <- get.FUDGE.subset(mh_by, i_idx, i_max)
MH <- build.FUDGE.dataset(MH.files, predictor.vars, subset=list(x=mh_x_subset), 
                          dim=ds.ref$mh_dimlist, 
                          mh_by, spat.mask.nc)
mh.dim <- dim(MH$data[[1]])
mh.coords <- get.loop.indices(mh.dim[1], mh.dim[2], mh_by)

message("Reading in future predictor data")
MF.files <- c()
for (var in predictor.vars){
  MF.files <- append(MF.files, get.FUDGE.filepath(fut.indir_1, 
                                                  var,fut.freq_1,fut.model_1,
                                                  fut.scenario_1,grid,
                                                  fut.file.start.year_1,fut.file.end.year_1, 
                                                  fut.pred.suffix))
}
mf_x_subset <- get.FUDGE.subset(mf_by, i_idx, i_max)
MF <- build.FUDGE.dataset(MF.files, predictor.vars, 
                          subset=list(x=mh_x_subset), 
                          dim=ds.ref$mf_dimlist, 
                          mf_by, spat.mask.nc)
mf.dim <- dim(MF$data[[1]])
mf.coords <- get.loop.indices(mf.dim[1], mf.dim[2], mf_by)

out.filename <- GetMiniFileName(target.var,fut.freq_1,ds.experiment,fut.scenario_1,ds.region,
                                fut.file.start.year_1,fut.file.end.year_1,i.file,file.j.range)
out.filename <- file.path(output.dir, out.filename)

#close spatial mask nc, if using
if (!is.null(spat.mask.nc)){
  nc_close(spat.mask.nc)
}

###### Set target dataset for blind reference at a later point
###### (note: the clever delayed evaluation in getDSInfo() is not working)
delayedAssign('TARG', eval(parse(text=ds.ref$target)))
targ.dim <- switch(ds.ref$target, 
                      'OH' = oh.dim, 
                      'MH' = mh.dim, 
                      'MF' = mf.dim)

#Create list of time masks for application
message("Reading in and checking time windowing masks")
tmask.list <- get.fudge.timemask.list()

message("Obtaining metadata for section 3 and 5 adjustments")
#making global to faclilitate trace pr calls in the MBC function
adjust.metadata <<- get.adjust.metadata(OH, MH, MF)

#delayedAssign('targ.dim', c(eval(parse(text=targ.dimstr))))
######
######################################################
###Hasty DeltaSD adjustment for asynchronous leap years
###Hasty adjustment tentaively no longer needed as of 2016-20-21; 
###now relying on internal DeltaSd call to adjust the leap days
# delta.string <- ""
# if('OH'%in% ds.ref$predictors){
#   h.time.len <- oh.dim[6]
#   delta.adj.index <- correct.hist.leapdays(oh.dim[6],
#                                            mf.dim[6],
#                                            hist.file.start.year_1, hist.file.end.year_1,
#                                            fut.file.start.year_1, fut.file.end.year_1)
#   delta.mask <<- c(rep(NA, delta.adj.index[1]), 
#                    rep(1, (h.time.len-(sum(delta.adj.index[1:2])))), 
#                    c(rep(NA, delta.adj.index[2])))
#   print(summary(delta.mask))
#   delta.string <- paste("using an OH timseries trimmed from", delta.adj.index[3], 
#                         "to", delta.adj.index[4], "for the downscaling step")
# }
######################################################

####Now that you have your input data, initialize your outputs.
###This will include your downscaled output and your qc masks.
###Until the input structure is fixed, that means you can get
###the qc mask information from the qc.maskopts structure, 
###and that the solitary qcmask will be named 'qc' in order
###to simplify the general qcmask calls.

output.dim <- c(targ.dim[1:3], #Spatial vars come from target dataset, 
                  mf.dim[4:6]) #time/ensemble/n vars come from the MF dataset
ds <- list('ds' = array(NA, dim=output.dim), 
           'qc' = array(NA, dim=output.dim))

##s3 call formerly here

message("FUDGE training begins...")
message(paste("Starting at:", Sys.time()))
start.time <- proc.time()

#args should always exist; it's specified in the runcode
#as of 9-29 I'm not sure why this is here, but it seems to have been
#needed in the past. 
if (args[1]=='na'){
  ds.args=NULL
}else{
  ds.args=args
}

#For now, make this a global to simplify data being copied
tmasks <<- tmask.list #global assign due to missval retry functions in ds step
#names(tmasks) <- c('MH', "OH", "MF", 'trim')
ds <- loop.xy.data()
print('summary of ds:')
print(summary(ds))
message("FUDGE downscaling ends")
message(paste("FUDGE downscaling took", proc.time()[1]-start.time[1], "seconds to run"))

#s5 call used to go in here

# ----- Begin segment like FUDGE Schematic Section 6: Write Downscaled results to data files -----
# rip.list <- lapply(att_table, '[[', 'rip')
# rips <- unique(rip.list)

#Output file destinations should be saved and printed for later reference
#But there can be some debate about how best to do that
out.file.list <- rep(list(NA), length(fut.pred.rip.list))
names(out.file.list) <- unlist(fut.pred.rip.list)

for (r in 1:length(fut.pred.rip.list)){
  rip = fut.pred.rip.list[r]
  #substitute instances of future predictor rip for the current rip
  loop.out.filename <- sub('r[[:digit:]][[:alpha:]]*[[:digit:]]*i[[:digit:]]p[[:digit:]]', 
                           rip,out.filename, perl = T)
  loop.out.filename <- sub('$(RIP)', rip, loop.out.filename, fixed = T)
  #Write Global attributes to downscaled netcdf
  label.training <- paste(hist.model_1,".",hist.scenario_1,".",hist.train.start.year_1,"-",hist.train.end.year_1,sep='')
  label.validation <- paste(fut.model_1,".",fut.scenario_1,".",fut.train.start.year_1,"-",fut.train.end.year_1,sep='')
  
  message('writing global attributes')
  global_att_list <- make.fudge.globals()
  ##For now, assume that we are sticking with LM-style training; 
  ##spatial coords from OH, time/ensemble coords from MH
  ##...man, this is going to take close to forever - you also need 
  ## to get the target data & metadata from the input files.
  ## Which implies that you set oh.dimlist, mh.dimlist
  ## and mf.dimlist at the beginning of the entire exercise.
  if (ds.ref$target != "MF"){
    dimlist <- c(TARG$metadata[[1]]$dim,  MF$metadata[[1]]$dim)
    varlist <- c(TARG$metadata[[1]]$vars, MF$metadata[[1]]$vars)
  }else{
    dimlist <- TARG$metadata[[1]]$dim
    varlist <- TARG$metadata[[1]]$vars
  }
  ##############################################################################
  message(paste("Writing to file", loop.out.filename))
  #tmp.ds <- aperm(ds$ds, c(1,2,3,5,6,4))
  ds.out.filename = WriteNC(loop.out.filename, ds$ds[,,,r,, , drop=F],
                            target.var,
                            dim.list=dimlist,
                            var.data=varlist,
                            prec='float', #Swapped from double Dec. 2015
                            units=OH$metadata[[target.var]]$units,
                            longname=paste(OH$metadata[[target.var]]$long_name),
                            standard_name=paste(OH$metadata[[target.var]]$long_name),
                            global_overwrite=global_att_list,
                            verbose=F
  )
  message(paste('Downscaled output file:',ds.out.filename,sep=''))  
  if(qc.maskopts$qc.inloop || qc.maskopts$qc.outloop || train_qcmask){ 
    ##Created waaay back at the beginning, as part of the QC functions
    for (var in target.var){
      ds$qc[is.na(ds$qc)] <- as.double(1.0e20)
      qc.var <- paste(var, 'qcmask', sep="_")
      #Sunstitute to get mask output directory
      #There are two setps - one to add _qcmask to the output directory after OneD; 
      #the next, to correct the filename
      qc.file <- sub('OneD/', paste0('OneD/', qc.var, '/'), loop.out.filename)
      qc.filename <- sub(var, qc.var, basename(qc.file))
      qc.file <- file.path(dirname(qc.file), qc.filename)
      
      ###Check to make sure that it is possible to create the qc file; create dirs if not
      message("Attempting creation of QC file")
      message(qc.file)
      exists <- file.create(qc.file)
      if(!exists){
        print("ERROR! Dir creation script not beahving as expected!")
      }
      message(paste('attempting to write to', qc.file))
      qc.comment <- get.qc.comment(qc.maskopts$qc.method)
      qc.comment.att <- list()
      qc.comment.att[[qc.var]] <- qc.comment
      qc.globals <- global_att_list
      #qc.globals$info <- ""
      qc.globals$comments <- make.fudge.comment(ds.method, target.var, grid, ds.region, k.fold,  
                                               ds.experiment, label.validation, label.training, 
                                               predictor.vars, fut.pred.rip.list, qc.maskopts$qc.method)
      qc.globals$climatology <- NA
      qc.list <- list()
      qc.list[[qc.var]] <- ds$qc
      qc.out.filename  = WriteNC(qc.file, lapply(qc.list, '[', ,,,r,, , drop=F), qc.var, 
                                 dim.list = dimlist, var.data=varlist, 
                                 prec='float', missval=1e20, units="1", 
                                 comment=qc.comment.att, 
                                 longname="QC Mask",
                                 standard_name = paste("QC mask of", OH$metadata[[target.var]]$long_name),
                                 global_overwrite=qc.globals)
      message(paste('QC Mask output file:',qc.out.filename,sep=''))
    }
  }
}

#As a final step, print basic profiling information to the logfile
profileFUDGERun()
#Note: this means the runscript needs to include the call for system memory usage; 
#or, that can wait for a bit
