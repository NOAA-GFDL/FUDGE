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
copy_from_tmp <- TRUE
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
  twindow_run <<- 'all'
}else{
  if (!exists('i_idx', inherits=T, envir=globalenv())){
    stop(paste("Error in Master_Driver: you should be using a config file with", 
               "command line params if you aren't using command-line args!"))
  }
}

fudge.start.time <- proc.time()[3]
message("Setting downscaling method information")
ds.ref <- getDSInfo(ds.method)

message("Getting info from aux file")
print(auxfile)
aux.ref <- get_aux_params(auxfile, copy_from_tmp)
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

# # print(pre_ds)
# # print(post_ds)
# # stop('examine this output')
# 
# ##Make sure that you have the experiment rip ready to apply to
# ##other code (i.e. the actual rip experiment is being run under)
# experiment.rip <- gsub("(.*_)", "", fut.scenario_1)
# ##Extra code added for compatibility with the darkchocolte-style
# ##runcodes that expergen is capable of producing
# if(exists('RIP')){
#   fut.pred.rip.list <- unlist(strsplit(RIP, split=","))
# }else{
#   message('Creating darkchocolate-compatible RIP list')
#   fut.pred.rip.list <- unlist(strsplit(fut.scenario_1, "_"))[2]
# }

#2018-04-10: Changing more information to come in via aux
fut.pred.rip.list <- aux.ref$DS$rips

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

########################
#2018-04-11 : now checking on how to read in csv data
#via the file_format parameter in the aux file
print("Reading OH files")
OH.files <- unlist(lapply(aux.ref$OH, '[[', 'file'), use.names = F)
if (copy_from_tmp){
  OH.files <- sapply(OH.files, function(x){file.path(TMPDIR, x)})
}
OH.vars <- names(aux.ref$OH)
oh_x_subset <- get.FUDGE.subset(aux.ref$OH[[1]]$idim, i_idx, i_max, i_stride-1)
oh_y_subset <- get.FUDGE.subset(aux.ref$OH[[1]]$jdim, j_idx, j_max, j_stride-1)
OH <- build.FUDGE.dataset(OH.files, OH.vars, list(x=oh_x_subset,y=oh_y_subset), 
                          dim=ds.ref$oh_dimlist, 
                          x_by = aux.ref$OH[[1]]$idim, y_by = aux.ref$OH[[1]]$jdim, 
                          aux.ref$OH[[1]]$geomask, 
                          file_format = aux.ref$file_format)
if (apply_spatmask){
  spatmask <- ReadSpatialMask(spatmask.file, oh_x_subset$subset, oh_y_subset$subset)
  for (el in names(OH$data)){
    OH$data[[el]] <- apply.any.mask(data=OH$data[[1]], mask=spatmask, dim='spatial')
  }
}
oh.orig.dim <- OH$orig_dims[[1]]
oh.dim <- dim(OH$data[[1]])
oh.coords <- get.loop.indices(oh.dim[1], oh.dim[2],
                              x.by=aux.ref$OH[[1]]$idim, y.by=aux.ref$OH[[1]]$jdim,
                              allow_partial=F)

message("Reading in MH files")
MH.files <- unlist(lapply(aux.ref$MH, '[[', 'file'), use.names=F)
if (copy_from_tmp){
  MH.files <- sapply(MH.files, function(x){file.path(TMPDIR, x)})
}
MH.vars <- names(aux.ref$MH)
mh_x_subset <- get.FUDGE.subset(aux.ref$MH[[1]]$idim, i_idx, i_max, i_stride-1)
mh_y_subset <- get.FUDGE.subset(aux.ref$MH[[1]]$jdim, j_idx, j_max, j_stride-1)
MH <- build.FUDGE.dataset(MH.files, MH.vars, list(x=mh_x_subset,y=mh_y_subset),
                          dim=ds.ref$mh_dimlist,
                          x_by=aux.ref$MH[[1]]$idim, y_by=aux.ref$MH[[1]]$jdim,
                          aux.ref$MH[[1]]$geomask, 
                          file_format = aux.ref$file_format)
if (apply_spatmask){
  spatmask <- ReadSpatialMask(spatmask.file, mh_x_subset$subset, mh_y_subset$subset)
  for (el in names(MH$data)){
    MH$data[[el]] <- apply.any.mask(data=MH$data[[1]], mask=spatmask, dim='spatial')
  }
}
mh.orig.dim <- MH$orig_dims[[1]]
mh.dim <- dim(MH$data[[1]])
mh.coords <- get.loop.indices(mh.dim[1], mh.dim[2],
                              x.by = aux.ref$MH[[1]]$idim, y.by = aux.ref$MH[[1]]$jdim,
                              allow_partial=F)
message("Reading in MF files")
MF.files <- unlist(lapply(aux.ref$MF, '[[', 'file'), use.names=F)
if (copy_from_tmp){
  MF.files <- sapply(MF.files, function(x){file.path(TMPDIR, x)})
}
MF.vars <- names(aux.ref$MF)
mf_x_subset <- get.FUDGE.subset(aux.ref$MF[[1]]$idim, i_idx, i_max, i_stride-1)
mf_y_subset <- get.FUDGE.subset(aux.ref$MF[[1]]$jdim, j_idx, j_max, j_stride-1)
MF <- build.FUDGE.dataset(MF.files, MF.vars,
                          subset=list(x=mf_x_subset, y=mf_y_subset),
                          dim=ds.ref$mf_dimlist,
                          x_by=aux.ref$MF[[1]]$idim, y_by=aux.ref$MF[[1]]$jdim,
                          aux.ref$MF[[1]]$geomask, 
                          file_format = aux.ref$file_format)
if (apply_spatmask){
  spatmask <- ReadSpatialMask(spatmask.file, mf_x_subset$subset, mf_y_subset$subset)
  for (el in names(MH$data)){
    MF$data[[el]] <- apply.any.mask(data=MF$data[[el]], mask=spatmask, dim='spatial')
  }
}
mf.orig.dim <- MF$orig_dims[[1]]
mf.dim <- dim(MF$data[[1]])
mf.coords <- get.loop.indices(mf.dim[1], mf.dim[2],
                              x.by=aux.ref$MF[[1]]$idim, y.by=aux.ref$MF[[1]]$jdim,
                              allow_partial=F)
########################
#2018-04-10: Creating file paths and checking for existance
outdirs <- c(file.path(aux.ref$DS$out_dir, aux.ref$DS$out_subdir), 
              file.path(aux.ref$DS$out_dir, aux.ref$DS$out_subdir, aux.ref$DS$out_supp[1]))
write_to_tmp = TRUE
if (write_to_tmp){
  outdirs <- sapply(outdirs, function(x){file.path(TMPDIR, x)})
}
#Check for existance of output dirs, after replacing $rip a bit
out_rip_dirs <- list()
for (rip in fut.pred.rip.list){
  dirlist <- sapply(outdirs, function(x){sub('<RIP>', rip, x)})
  if (!all(file.exists(dirlist))){
    stop(paste("Error in dir existance: One or more of the dirs", 
               paste(dirlist, collapse=','), 
               "do not yet exist!"))
  }
  out_rip_dirs[[rip]] <- dirlist
}

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
                              x_by=aux.ref$OH[[1]]$idim, y_by=aux.ref$OH$jdim,
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

message("Obtaining metadata for section 3 and 5 adjustments")
#making global to faclilitate trace pr calls in the MBC function
if (!exists('pr_opts')){
  adjust.metadata <<- get.adjust.metadata(OH, MH, MF)
}else{
  adjust.metadata <<- get.adjust.metadata(OH, MH, MF, pr_opts=pr_opts)
}

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
  #commenting out 2018-04-19 - not all methods need this
  #env$prime_predictor <- paste0(predictor.vars[1], ".0.0")
  env$prime_predictor <- paste0(MH.vars[1], ".0.0")
  default.methods <- c("MBC", "QDM", "CFQM", "lmAsync", 'lmstepAsync', "KDDM")
  if (! (ds.method %in% default.methods)){
    #env$prime_predictor <- paste0(predictor.vars[1], ".0.0")
    env$prime_predictor <- paste0(MH.vars[1], ".0.0")
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


ds <- loop.xy.data()
#ds <- list(ds=array(NA, dim=mf.dim), qc=array(NA, dim=mf.dim))

print('summary of ds:')
print(summary(ds))
message("FUDGE downscaling ends")
message(paste("FUDGE downscaling took", proc.time()[1]-start.time[1], "seconds to run"))

#s5 call used to go in here

# ----- Begin segment like FUDGE Schematic Section 6: Write Downscaled results to data files -----
# rip.list <- lapply(att_table, '[[', 'rip')
# rips <- unique(rip.list)

#As of 2017-04-10, we are still writing out individual rips
#to file when we write out data
tmp_filename <- aux.ref$DS$out_file

istart <- i_idx + i_offset - 1
iend <- istart + i_stride - 1
jstart <- j_idx + j_offset - 1 
jend <- jstart + j_stride - 1
locstring <- paste0("I", istart, '-', iend, '_J', jstart, "-", jend)

tmp_filename <- sub('I<ISTART>-<IEND>_J<JSTART>-<JEND>', locstring, tmp_filename)
for (r in 1:length(fut.pred.rip.list)){
  rip <- fut.pred.rip.list[r]
  #First do the filename, then the output dir
  loop_filename <- sub("<RIP>", rip, tmp_filename)
  loop_outdirs <- sapply(outdirs, function(x){sub("<RIP>", rip, x)})
  loop_filepath = file.path(loop_outdirs[1], loop_filename)
  #Metadata-related things
  label.training <- paste(hist.model_1,".",hist.scenario_1,".",hist.train.start.year_1,"-",hist.train.end.year_1,sep='')
  label.validation <- paste(fut.model_1,".",fut.scenario_1,".",fut.train.start.year_1,"-",fut.train.end.year_1,sep='')  
  message('writing global attributes')
  if (aux.ref$file_format == 'netcdf'){
    global_att_list <- make.fudge.globals()
  }else{
    global_att_list <- list('fakemeta' = "metadata att that doesn't need to exist")
  }
  
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
  
  global_clone_file = NA
  clone_globals = F
  if (aux.ref$file_format != 'csv'){
    global_clone_file = OH.files[[1]]
    clone_globals = T
  }

  message(paste("Writing to file", loop_filepath))
  ds.out.filename = WriteNC(loop_filepath, ds$ds[,,,r,, , drop=F],
                            aux.ref$DS$vars, #formerly target.var
                            dim.list=dimlist,
                            var.data=varlist,
                            prec='float',
                            units=OH$metadata[[OH.vars[1]]]$units,
                            longname=paste(OH$metadata[[OH.vars[1]]]$long_name),
                            standard_name=paste(OH$metadata[[OH.vars[1]]]$long_name),
                            global_overwrite=global_att_list,
                            global_clone_file = global_clone_file,
                            clone_globals=clone_globals, clone_all=F,
                            clone_include="frequency",clone_exclude="climatology",
                            verbose=T
  )
  message(paste('Downscaled output file:',ds.out.filename,sep=''))
  if(qc.maskopts$qc.inloop || qc.maskopts$qc.outloop || train_qcmask){
    ##Created waaay back at the beginning, as part of the QC functions
    for (var in aux.ref$DS$vars){
      ds$qc[is.na(ds$qc)] <- as.double(1.0e20)
      qc.var <- paste(var, 'qcmask', sep="_")
      
      #Substitute to get mask output directory
      qc_filename <- sub(paste0(var, '_'), paste0(qc.var, '_'), loop_filename)
      qc_filepath = file.path(loop_outdirs[2], qc_filename)
    
      message(paste('attempting to write to', qc_filename))
      qc.comment <- get.qc.comment(qc.maskopts$qc.method, ds.method)
      qc.comment.att <- list()
      qc.comment.att[[qc.var]] <- qc.comment
      qc.globals <- global_att_list

      qc.globals$comments <- make.fudge.comment(ds.method, target.var, grid, ds.region, k.fold,
                                                ds.experiment,
                                                #label.validation, label.training,
                                                predictor.vars, fut.pred.rip.list,
                                                qc.maskopts$qc.method, missval.args, train.string=train.string)
      qc.longname <- make.qc.longname(qc.maskopts$qc.method, ds.method, target.var)
      qc.globals$climatology <- NA
      qc.list <- list()
      qc.list[[qc.var]] <- ds$qc
      qc.dimlist <- dimlist
      qc.varlist <- varlist
      qc.list <- list()
      qc.list[[qc.var]] <- ds$qc
      qc.out.filename  = WriteNC(qc_filepath, lapply(qc.list, '[', ,,,r,, , drop=F), qc.var,
                                 dim.list = dimlist, var.data=varlist,
                                 prec='float', missval=1e20, units="1",
                                 comment=qc.comment.att,
                                 longname=qc.longname,
                                 standard_name = paste("QC mask of", OH$metadata[[OH.vars[1]]]$long_name),
                                 global_overwrite=qc.globals)
      
      message(paste('QC Mask output file:',qc.out.filename,sep=''))
    }
  }  
}

#As a final step, print basic profiling information to the logfile
profileFUDGERun()
#Note: this means the runscript needs to include the call for system memory usage;
#or, that can wait for a bit
