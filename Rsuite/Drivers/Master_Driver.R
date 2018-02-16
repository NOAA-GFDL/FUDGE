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
stored.opts <- options()[c('warn', 'error', 'showErrorCalls')]
ival <<- 0
jval <<- 0
   w <<- 0
FUDGE.error.handler.function <- function(){
  #'Prints information needed for debugging/reproducing
  #'in a preoduction context
  message(paste("ival:", ival))
  message(paste("jval:", jval))
  message(paste("twindow:", w))
  f.args <- as.list(sys.call(which=1))
  if (length(f.args) > 0){
    message("Arguments to current function:")
    message(str(f.args))
  }
  traceback(2)
  #If quitting from a non-interactive session, a status of 1 should be sent.
  if (!interactive()){
    quit(save="no", status=1, runLast=FALSE) #runLast=FALSE
  }
}
#previously traceback
options(error=FUDGE.error.handler.function,
        warn = 1, showErrorCalls=TRUE)
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
  i_idx <<- as.numeric(console.args[1])
  i_max <- as.numeric(console.args[2])
  i_stride <- as.numeric(console.args[3])
  i_offset <<- as.numeric(console.args[4])
  j_idx <<- as.numeric(console.args[5])
  j_max <- as.numeric(console.args[6])
  j_stride <- as.numeric(console.args[7])
  j_offset <<- as.numeric(console.args[8])
  auxfile <- console.args[9]
  copy_from_tmp = console.args[10]
  x.pad <- "NA"
}else{
  oh_by <- 1 #3
  mh_by <- 1 #3 #5 #3
  mf_by <- 1 #3 #5 #3
  i_idx <- 48 #167 #pr_diff = 37 i800 = 53 #194 #10
  i_max <- 194
  i_stride <- 1
  i_offset <<- 748
  j_idx <- 1
  j_max <- 114
  j_stride <- 114
  j_offset <<- 454
  auxfile <- "/home/cew/Code/h_testing/subsample/aux/all_i1_j1.json"
  #auxfile <- "/home/cew/Code/h_testing/subsample/aux_files/aux_04.json" ##01 has spatial masks; 05_offset has asymmetric grid
  copy_from_tmp <- F #Do you assume that the aux files are located in local tmpdir?
  x.pad <- "NA"
}

fudge.start.time <- proc.time()[3]
message("Setting downscaling method information")
ds.ref <- getDSInfo(ds.method)

message("Getting info from aux file")
print(auxfile)
aux.ref <- get_aux_params(auxfile, copy_from_tmp)
#1-23-17 adding bypass to see if the 'to the left' works
#Note: this is going to be a bad idea to use on anything but the
#
if ("multivar_target" %in% aux.ref){
  ds.ref$multivar_target <- aux.ref$multivar_target
}

message("Checking downscaling arguments")
ds.args <<- args
QCDSArguments(k=k.fold, ds.method = ds.method, args=ds.args,
              is.multivar=(length(predictor.vars) > 1),
              is.multi_ens=(length(fut.pred.rip.list) > 1))

message("Checking the missing value arguments")
missval.args <- getMissvalArgs(ds.args, ds.ref$missval_method)

message("Checking input predictor var order")
predictor.vars <- fix.var.order(predictor.vars, target.var)

if (mask.list=='na'){
  mask.list <- list(list(type='na', qc.mask='off', adjust.out='off'))
}

##Call the code that converts the instructions for section 3 and section 5
if(!exists('pre_ds')){ #put better check in here once you are done with the testing
  message('Conversion of pre- and post- downscaling adjustment input')
  if(exists('pr_opts')){
    pp.out <- adapt.pp.input(mask.list, pr_opts, ds.ref)
  }else{
    pp.out <- adapt.pp.input(mask.list)
  }
  pre_ds <- pp.out$pre_ds
  post_ds <- pp.out$post_ds
  train_qcmask <- pp.out$train_qcmask
}

# print(pre_ds)
# print(post_ds)
# stop('examine this output')

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
  f.list <- override.f.defaults(ds.args)
}else{
  f.list <- override.f.defaults(ds.args, pr_opts)
}
min.present <- f.list$threshold
ds.args <- f.list$args

#Then, check that the s3 and s5 adjustments you are performing make sense in
#this context
print(length(pre_ds))
print(length(post_ds))
QCAdjustArguments(pre_ds, post_ds,
                  is.multivar=(length(predictor.vars) > 1),
                  is.multi.ens=(length(fut.pred.rip.list) > 1),
                  target.in.predictors = (target.var%in%predictor.vars),
                  predictor.vars, target.var, ds.method)

#### Then, read in spatial and temporal masks. Those will be used
#### not only for the masks, but as an immediate check upon the dimensions
#### of the files being read in.

# # spatial mask read check
message("Checking for spatial masks vars")
if(spat.mask.dir_1!="na"){
  apply_spatmask <- T
  spatmask.file <- file.path(dirname(spat.mask.dir_1),
                             paste0(spat.mask.var, ".nc"))
  #spat.mask.nc <- nc_open()
}else{
  #spat.mask.nc <- NULL
  apply_spatmask <- F
  message("no spatial mask included; skipping to next step")
}

### Now, access input data sets
message("Reading in target data")

oh_x_subset <- get.FUDGE.subset(aux.ref$OH$idim, i_idx, i_max, i_stride-1)
oh_y_subset <- get.FUDGE.subset(aux.ref$OH$jdim, j_idx, j_max, j_stride-1)
if (ds.ref$multivar_target){
  #Right now, only applies to MBC
  OH.files <- c()
  target.indir_1 <- dirname(dirname(dirname(target.indir_1)))
  for (var in predictor.vars){
    print(paste("reading target var", var))
    OH.files <- append(OH.files, get.FUDGE.filepath(target.indir_1,
                                                    var,target.freq_1,target.model_1,
                                                    target.scenario_1,grid,
                                                    target.file.start.year_1,target.file.end.year_1,
                                                    target.suffix, is_OH = F))
  }
  OH <- build.FUDGE.dataset(OH.files, predictor.vars, list(x=oh_x_subset, y=oh_y_subset),
                            dim=ds.ref$oh_dimlist,
                            x_by=aux.ref$OH$idim, y_by=aux.ref$OH$jdim,
                            aux.ref$OH$geomask)
  if (apply_spatmask){
    spatmask <- ReadSpatialMask(spatmask.file, oh_x_subset$subset, oh_y_subset$subset)
    for (el in names(OH$data)){
     OH$data[[el]] <- apply.any.mask(data=OH$data[[el]], mask=spatmask, dim='spatial')
    }
  }
}else{
  OH.path <- get.FUDGE.filepath(target.indir_1, target.var,target.freq_1,target.model_1,target.scenario_1,grid,
                                target.file.start.year_1,target.file.end.year_1,
                                target.suffix, is_OH = T)
  OH.files <- OH.path
  OH <- build.FUDGE.dataset(OH.path, target.var, list(x=oh_x_subset, y=oh_y_subset),
                            dim=ds.ref$oh_dimlist,
                            x_by=aux.ref$OH$idim, y_by=aux.ref$OH$jdim,
                            aux.ref$OH$geomask)
  if (apply_spatmask){
    spatmask <- ReadSpatialMask(spatmask.file, oh_x_subset$subset, oh_y_subset$subset)
    for (el in names(OH$data)){
      OH$data[[el]] <- apply.any.mask(data=OH$data[[el]], mask=spatmask, dim='spatial')
    }
  }
}
#Now that we can read in stencils, the below is not
#going to work anymore. Some of the fixes are down just before
#the s3-4-5 call; the mroe flexible ones are scattered about.
#oh.dim <- dim(OH$data[[1]])
oh.orig.dim <- OH$orig_dims[[1]]
oh.dim <- dim(OH$data[[1]])
oh.coords <- get.loop.indices(oh.dim[1], oh.dim[2],
                              x.by=aux.ref$OH$idim, y.by=aux.ref$OH$jdim,
                              allow_partial=F)

message("Reading in historical predictor data")
MH.files <- c()
for (var in predictor.vars){
  MH.files <- append(MH.files, get.FUDGE.filepath(hist.indir_1,
                                                  var,hist.freq_1,hist.model_1,
                                                  hist.scenario_1,grid,
                                                  hist.file.start.year_1,hist.file.end.year_1,
                                                  hist.pred.suffix))
}
mh_x_subset <- get.FUDGE.subset(aux.ref$MH$idim, i_idx, i_max, i_stride-1)
mh_y_subset <- get.FUDGE.subset(aux.ref$MH$jdim, j_idx, j_max, j_stride-1)
MH <- build.FUDGE.dataset(MH.files, predictor.vars, list(x=mh_x_subset,y=mh_y_subset),
                          dim=ds.ref$mh_dimlist,
                          x_by=aux.ref$MH$idim, y_by=aux.ref$MH$jdim,
                          aux.ref$MH$geomask)
if (apply_spatmask){
  spatmask <- ReadSpatialMask(spatmask.file, mh_x_subset$subset, mh_y_subset$subset)
  for (el in names(MH$data)){
    MH$data[[el]] <- apply.any.mask(data=MH$data[[el]], mask=spatmask, dim='spatial')
  }
}
mh.orig.dim <- MH$orig_dims[[1]]
mh.dim <- dim(MH$data[[1]])
mh.coords <- get.loop.indices(mh.dim[1], mh.dim[2],
                              x.by = aux.ref$MH$idim, y.by = aux.ref$MH$jdim,
                              allow_partial=F)#(aux.ref$ds_handle_missing=='continue'))

message("Reading in future predictor data")
MF.files <- c()
for (var in predictor.vars){
  MF.files <- append(MF.files, get.FUDGE.filepath(fut.indir_1,
                                                  var,fut.freq_1,fut.model_1,
                                                  fut.scenario_1,grid,
                                                  fut.file.start.year_1,fut.file.end.year_1,
                                                  fut.pred.suffix))
}
mf_x_subset <- get.FUDGE.subset(aux.ref$MF$idim, i_idx, i_max, i_stride-1)
mf_y_subset <- get.FUDGE.subset(aux.ref$MF$jdim, j_idx, j_max, j_stride-1)
MF <- build.FUDGE.dataset(MF.files, predictor.vars,
                          subset=list(x=mf_x_subset, y=mf_y_subset),
                          dim=ds.ref$mf_dimlist,
                          x_by=aux.ref$MF$idim, y_by=aux.ref$MF$jdim,
                          aux.ref$MF$geomask)
if (apply_spatmask){
  spatmask <- ReadSpatialMask(spatmask.file, mf_x_subset$subset, mf_y_subset$subset)
  for (el in names(MH$data)){
    MF$data[[el]] <- apply.any.mask(data=MF$data[[el]], mask=spatmask, dim='spatial')
  }
}
mf.orig.dim <- MF$orig_dims[[1]]
mf.dim <- dim(MF$data[[1]])
mf.coords <- get.loop.indices(mf.dim[1], mf.dim[2],
                              x.by=aux.ref$MF$idim, y.by=aux.ref$MF$jdim,
                              allow_partial=F)

out.filename <- GetMiniFileName(target.var,fut.freq_1,ds.experiment,fut.scenario_1,ds.region,
                                fut.file.start.year_1,fut.file.end.year_1,
                                i_idx, i_stride, i_offset, i_max, j_idx, j_stride, j_offset, j_max)
out.filename <- file.path(output.dir, out.filename)

#close spatial mask nc, if using
# if (!is.null(spat.mask.nc)){
#   nc_close(spat.mask.nc)
# }

###### Set target dataset for blind reference at a later point
###### (note: the clever delayed evaluation in getDSInfo() is not working)
message("Setting target dimensions")
delayedAssign('TARG', eval(parse(text=ds.ref$target)))
if (!ds.ref$multivar_target){
  targ.dim <- switch(ds.ref$target,
                     'OH' = oh.orig.dim,
                     'MH' = mh.orig.dim,
                     'MF' = mf.orig.dim)
  if (ds.ref$target=='OH'){
    ds.coords <- oh.coords
  }else if (ds.ref$target=='MF'){
    ds.coords <- mf.coords
  }
}else{
  #This is a special bypass for the MBC; we can't count on OH
  #being all 1, and some of the other workflow assumes that
  targ.dim <- c(oh.orig.dim[1],oh.orig.dim[2],1,1,1,1)
  targ.x.subset <- get.FUDGE.subset(1, i_idx, i_max, i_stride-1)
  targ.y.subset <- get.FUDGE.subset(1, j_idx, j_max, j_stride-1)
  TARG <- build.FUDGE.dataset(OH.files[[1]], target.var, list(x=targ.x.subset,y=targ.y.subset),
                              dim=ds.ref$oh_dimlist,
                              x_by=aux.ref$OH$idim, y_by=aux.ref$OH$jdim,
                              "none")
  ds.coords <- get.loop.indices(targ.dim[1], targ.dim[2],
                                x.by=1, y.by=1, allow_partial=F)
}

#Get string of training data for the metadata atts
train.string <- paste(paste(ds.ref$training[-length(ds.ref$training)], collapse=","),
                      paste("and", ds.ref$training[length(ds.ref$training)]))

#Create list of time masks for application
message("Reading in and checking time windowing masks")
tmask.list <- get.fudge.timemask.list()

##Adding 2017-09 for the intermediate KDDM steps
# julian.strings <- c("JULIAN", 'julian', "Julian", 'gregorian',
#                     'proleptic_gregorian', 'standard', 'STANDARD', 'Standard')
# if (tmask.list$calendar %in% julian.strings) {
#   if (ds.method == 'KDDM'){
#     stop(paste("Error in Master_Driver: KDDM is not currently supported",
#                "for calendars with leapdays!"))
#   }
# }

message("Obtaining metadata for section 3 and 5 adjustments")
#making global to faclilitate trace pr calls in the MBC function
if (!exists('pr_opts')){
  adjust.metadata <<- get.adjust.metadata(OH, MH, MF)
}else{
  adjust.metadata <<- get.adjust.metadata(OH, MH, MF, pr_opts=pr_opts)
}

#adjust.metadata <<- get.adjust.metadata(OH, MH, MF)

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
                  mf.orig.dim[4:6]) #time/ensemble/n vars come from the MF dataset
# ds <- list('ds' = array(NA, dim=output.dim),
#            'qc' = array(NA, dim=output.dim))

#Added 02-14-17 - trying to set up defaults differently
set.ds.defaults <- function(ds.method, args, ..., env=parent.frame()){
  #Alter args for a given method depending on the args allowed
  #(NOTE: may include settings other than default, such as QDM type to ratio)

  #Okay - from now on, we also need to set a default prime_predictor;
  #the lm() and MBC(0 family of methods may reset this value)
  #print(str(as.list(match.call())))
  env$prime_predictor <- paste0(predictor.vars[1], ".0.0")
  default.methods <- c("MBC", "QDM", "CFQM", "lmAsync", 'lmstepAsync', "KDDM")
  if (! (ds.method %in% default.methods)){
    env$prime_predictor <- paste0(predictor.vars[1], ".0.0")
  }else{
    env <- switch(ds.method,
           'MBC' = set.MBC.defaults(args, ..., env=env),
           'QDM' = set.QDM.defaults(args, ..., env=env),
           "CFQM" = set.CFQM.defaults(args, ..., env=env),
           "lmAsync" = set.lmAsync.defaults(args, ..., env=env),
           "lmstepAsync" = set.lmstepAsync.defaults(args, ..., env=env),
           "KDDM" = set.KDDM.defaults(args, ..., env=env),
           stop(paste("Error in set.ds.defaults: no method",
                      ds.method, "!")))
    #return(new.args)
  }
  return(env)
}
message("Checking for default ds arguments (requires var units)")
set.ds.defaults(ds.method, ds.args, predictor.vars=predictor.vars,
                            pre.ds.train=pre.ds.train,
                            ds.ref=ds.ref, aux.ref=aux.ref,
                            env=environment())

# print(prime_predictor)
# stop('examine this output')

# print(s5.outloop)
# print(post.ds)
# stop('examine this out')

##s3 call formerly here

message("FUDGE training begins...")
message(paste("Starting at:", Sys.time()))
start.time <- proc.time()

#For now, make this a global to simplify data being copied
tmasks <<- tmask.list #global assign due to missval retry functions in ds step

#names(tmasks) <- c('MH', "OH", "MF", 'trim')
# print(summary(pre.ds.train))
# print(summary(post.ds.train))
# stop('examine this out')
ds <- loop.xy.data()
#ds <- list(ds=MF$data[[1]])
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
		if (ds.method == "KDDM") {
			dimlist <- MF$metadata[[1]]$dim
			varlist <- MF$metadata[[1]]$vars
		} else {
			dimlist <- c(TARG$metadata[[1]]$dim,  MF$metadata[[1]]$dim)
			varlist <- c(TARG$metadata[[1]]$vars, MF$metadata[[1]]$vars)
		}
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
                            global_clone_file = OH.files[[1]],
                            clone_globals=T, clone_all=F,
                            clone_include="frequency",clone_exclude="climatology",
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
                                                ds.experiment,
                                                #label.validation, label.training,
                                                predictor.vars, fut.pred.rip.list,
                                                qc.maskopts$qc.method, missval.args, train.string=train.string)
      qc.globals$climatology <- NA
      qc.list <- list()
      qc.list[[qc.var]] <- ds$qc
      qc.dimlist <- dimlist
      qc.varlist <- varlist
      ### Commenting out the save-specific adaptations as of 2017-05-11
      #  qc.dimlist$time <- save.time.dim
      #  qc.varlist$time_bnds <- save.time.bnds
      #  out.array <- list('pr_trace' = pr_trace, 'OH_ss' = OH_ss, "MH_ss" = MH_ss, "MF_ss" = MF_ss)
      #  out.varnames <- c("pr_trace", 'OH_ss', "MH_ss", "MF_ss")
      #  print(lapply(lapply(out.array, '[', ,,,r,, ,drop=F), 'dim'))
      #  print(out.varnames)
      #qc.comment.att <- list("pr_trace" = "adjusted trace pr for MH and MF",
      #                   "OH_ss" = "sample size of OH before downscaling",
      #                   "MH_ss" = "sample size of MH before downscaling",
      #                   "MF_ss" = "sample size of MF before downscaling")
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
