#' driver_a1rv2.r
#' phase 1 of driver script implementation for FUDGE: CDFt train driver
#' originally created: a1r,08/2014
#' Modified heavily by 4-6-2015

print(paste("START TIME:",Sys.time(),sep=''))

############## Set environment variables #######################
FUDGEROOT <<- Sys.getenv(c("BASEDIR"))
FUDGETAG <<- Sys.getenv(c("FUDGETAG"))
BRANCH <- Sys.getenv("BRANCH")
print(paste("FUDGEROOT is now activated:",FUDGEROOT,sep=''))
TMPDIR = Sys.getenv(c("TMPDIR"))
if (TMPDIR == ""){
  stop("ERROR: TMPDIR is not set. Please set it and try it") 
}  

########### Library calls, source necessary functions ################
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgePreDS/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeQC/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeTrain/',sep=''), full.names=TRUE), source);
source(paste(FUDGEROOT,'Rsuite/Drivers/LoadLib.R',sep=''))
source(paste(FUDGEROOT, 'Rsuite/Drivers/UtilityFunctions.R', sep=""))

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



################################################################################
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
################################################################################

fudge.start.time <- proc.time()[3]

message("Getting info from aux file")
print(auxfile)
aux.ref <- get_aux_params(auxfile, copy_from_tmp)
if ("multivar_target" %in% aux.ref){
  ds.ref$multivar_target <- aux.ref$multivar_target
}

message("Setting downscaling method information")
ds.ref <- getDSInfo(aux.ref$ds_method, aux.ref$ds_args, aux.ref$kfold)

#------- Add libraries -------------
LoadLib(ds.ref$method)

message("Checking the missing value arguments")
missval.args <- getMissvalArgs(ds.ref$args, ds.ref$missval_method)

target.var = names(aux.ref$OH)
predictor.vars = names(aux.ref$MH)
message("Checking input predictor var order")
predictor.vars <- fix.var.order(predictor.vars, target.var)

#if (mask.list=='na'){
#  mask.list <- list(list(type='na', qc.mask='off', adjust.out='off'))
#}

# ##Call the code that converts the instructions for section 3 and section 5
# if(!exists('pre_ds')){ #put better check in here once you are done with the testing
#   message('Conversion of pre- and post- downscaling adjustment input')
#   if(exists('pr_opts')){
#     pp.out <- adapt.pp.input(mask.list, pr_opts, ds.ref)
#   }else{
#     pp.out <- adapt.pp.input(mask.list)
#   }
#   pre_ds <- pp.out$pre_ds
#   post_ds <- pp.out$post_ds
#   train_qcmask <- pp.out$train_qcmask
# }

#2018-04-10: Changing more information to come in via aux
fut.pred.rip.list <- aux.ref$DS$rips
message("Checking pre- and post-downscaling adjustments")
pre_ds <- aux.ref$s3
post_ds <- aux.ref$s5
pre.ds <- pre_ds$outloop
pre.ds.train <- pre_ds$inloop
post.ds <- post_ds$outloop
post.ds.train <- post_ds$inloop
s3.inloop <- length(pre_ds$inloop) > 0
s3.outloop <- length(pre_ds$outloop) > 0
s5.inloop <- length(post_ds$inloop) > 0
s5.outloop <- length(post_ds$outloop) > 0
train_qcmask <- aux.ref$return_qcmask_from_ds
 
# ('qc.inloop'=(inloop.count > 0), 'qc.outloop'=(outloop.count > 0), 
#                 'qc.method'=qc.method, 
#                 'qc.args'=qc.args)

# #Initialize instructions for pre- and post-ds adjustment
# 
# post.ds <- compact(lapply(post_ds, index.a.list, 'loc', 'outloop'))
# post.ds.train <- compact(lapply(post_ds, index.a.list, 'loc', 'inloop'))
# qc.maskopts <- qc.mask.check(post.ds.train, post.ds, train_qcmask)
# pre.ds <- compact(lapply(pre_ds, index.a.list, 'loc', 'outloop'))
# pre.ds.train <- compact(lapply(pre_ds, index.a.list, 'loc', 'inloop'))
# #Generate metadata references for the pre- and post-processing functions
# #Things that contain an ADJUSTMENT
# post_ds_adj_tmp <- compact(lapply(post_ds, index.a.list, 'adjust.out', 'on'))
# post_ds_inloop <- compact(lapply(post_ds_adj_tmp, index.a.list,'loc', 'inloop'))
# post_ds_outloop <- compact(lapply(post_ds_adj_tmp, index.a.list,'loc', 'outloop'))
# post_ds_adj <- c(post_ds_inloop, post_ds_outloop)
# ##Initialize a few globals for looping in loop.xy()
# s3.outloop <- length(pre.ds) > 0
# s3.inloop <- length(pre.ds.train) > 0
# #training qc makss are their own thing
# s5.inloop <- length(post_ds_inloop) > 0
# s5.outloop <- length(post_ds_outloop) > 0
# #And finally, check to see if a training (minsamplesize) qc mask
# train_qcmask <- 'qc.train' %in% sapply(mask.list, '[[', 'type')

#Adapt some of the parameters being set by the f-to-g bridge code
if(!exists('pr_opts')){
  f.list <- override.f.defaults(ds.ref$args)
}else{
  f.list <- override.f.defaults(ds.ref$args, pr_opts)
}
min.present <- f.list$threshold
ds.ref$args <- f.list$args

#Then, check that the s3 and s5 adjustments you are performing make sense in
#this context. The pre_ds and post_ds datasets were created waaay back up near
#the head of the class. 
is_multi_ens = length(aux.ref$DS$rips) > 1
is_multi_var = length(aux.ref$MH) > 1
QCAdjustArguments(pre_ds, post_ds,
                  is.multivar=is_multi_var,
                  is.multi.ens=is_multi_ens,
                  target.in.predictors = (target.var%in%predictor.vars),
                  predictor.vars, target.var, ds.ref$method)

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
for (el in names(OH$data)){
  if (aux.ref$OH[[1]]$geomask != 'none'){
    spatmask <- ReadSpatialMask(file.path(TMPDIR, aux.ref$OH[[1]]$geomask), 
                                oh_x_subset$subset, oh_y_subset$subset)
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
for (el in names(MH$data)){
  if (aux.ref$MH[[1]]$geomask != 'none'){
    spatmask <- ReadSpatialMask(file.path(TMPDIR, aux.ref$MH[[el]]$geomask), 
                                mh_x_subset$subset, mh_y_subset$subset)
    MH$data[[el]] <- apply.any.mask(data=MH$data[[el]], mask=spatmask, dim='spatial')
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
for (el in names(MH$data)){
  if (aux.ref$MF[[1]]$geomask != 'none'){
    spatmask <- ReadSpatialMask(file.path(TMPDIR, aux.ref$MF[[el]]$geomask), 
                                mf_x_subset$subset, mf_y_subset$subset)
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
#Please note: for purposes of structure simplification, 
#we assume for the moment that we only get the tmask from the first var in the
#list, even though the structure supports many vars.
tmask.list <- get.fudge.timemask.list(aux.ref, ds.ref, TMPDIR)
#For now, make this a global to simplify data being copied
tmasks <<- tmask.list #global assign due to missval retry functions in ds step

message("Checking downscaling arguments")
print(is_multi_ens)
QCDSArguments(k=aux.ref$kfold, ds.method = ds.ref$method, args=ds.ref$args,
              is.multivar=is_multi_var,
              is.multi_ens=is_multi_ens, 
              tmasks=tmasks, ds.experiment = aux.ref$ds_experiment_name)

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
set.ds.defaults(ds.ref$method, ds.ref$args, predictor.vars=predictor.vars,
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
for (r in 1:length(aux.ref$DS$rips)){
  rip <- fut.pred.rip.list[r]
  #First do the filename, then the output dir
  loop_filename <- sub("<RIP>", rip, tmp_filename)
  loop_outdirs <- sapply(outdirs, function(x){sub("<RIP>", rip, x)})
  loop_filepath = file.path(loop_outdirs[1], loop_filename)
  #Metadata-related things
  message('writing global attributes')
  if (aux.ref$file_format == 'netcdf'){
    global_att_list <- make.fudge.globals()
  }else{
    global_att_list <- list('fakemeta' = "metadata att that doesn't need to exist")
  }
  
  if (ds.ref$target != "MF"){
    if (ds.ref$method == "KDDM") {
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
  if (aux.ref$make_qcmask){
    for (var in aux.ref$DS$vars){
      ds$qc[is.na(ds$qc)] <- as.double(1.0e20)
      qc.var <- paste(var, 'qcmask', sep="_")
      
      #Substitute to get mask output directory
      qc_filename <- sub(paste0(var, '_'), paste0(qc.var, '_'), loop_filename)
      qc_filepath = file.path(loop_outdirs[2], qc_filename)
      
      message(paste('attempting to write to', qc_filename))
      qc.comment <- get.qc.comment(aux.ref$qc_method, ds.ref$method)
      qc.comment.att <- list()
      qc.comment.att[[qc.var]] <- qc.comment
      qc.globals <- global_att_list
      
      qc.globals$comments <- make.fudge.comment(ds.ref$method, target.var, aux.ref$output_grid, aux.ref$ds_region, aux.ref$kfold,
                                                aux.ref$ds_experiment_name,
                                                predictor.vars, aux.ref$DS$rips,
                                                aux.ref$qc_method, missval.args, train.string=train.string)
      qc.longname <- make.qc.longname(aux.ref$qc_method, ds.ref$method, target.var)
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
