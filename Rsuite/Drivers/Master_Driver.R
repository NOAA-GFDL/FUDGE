#' driver_a1rv2.r
#' phase 1 of driver script implementation for FUDGE: CDFt train driver 
#' originally created: a1r,08/2014
#' Modified heavily by 4-6-2015 

############### Library calls, source necessary functions ###################################################
#TODO the following sapplys and sourcing should be a library call
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgePreDS/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeQC/',sep=''), full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeTrain/',sep=''), full.names=TRUE), source);
source(paste(FUDGEROOT,'Rsuite/Drivers/LoadLib.R',sep=''))
source(paste(FUDGEROOT, 'Rsuite/Drivers/UtilityFunctions.R', sep=""))
#sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/Drivers/',sep=''), full.names=TRUE), source);
#source(paste(FUDGEROOT,'Rsuite/drivers/CDFt/TrainDriver.R',sep=''))

#-------Add traceback call for error handling -------
stored.opts <- options()[c('warn', 'error', 'showErrorCalls')]
error.handler.function <- function(){
  #message(writeLines(traceback()))
  traceback()
  message("Quitting gracefully with exit status 1")
  #If quitting from a non-interactive session, a status of 1 should be sent. Test this.
  #quit(save="no", status=1, runLast=FALSE, save=FALSE) #runLast=FALSE
}
#previously traceback
options(error=error.handler.function, warn = 1, showErrorCalls=TRUE)
###See if there's a good way to return back to the original settings
###after this point. Probably not a component of a --vanilla run. 
###But it unquestionably simplifies debugging.

#------- Add libraries -------------
LoadLib(ds.method)
#-------End Add libraries ---------
###############################################################################################################
#' key data objects -----
#' clim.var.in:   numeric vector containing data of the input climate variable
#                to be processed
#' esd.output:    numeric vector representing data produced by statisical
#                downscaling processes of FUDGE schematic section 4.
#' esd.final:     numeric vector containing the version of the downscaled output
#                to be archived, either the same as esd.output or a 
#                back-transformed version of esd.output

# ----- Begin segment like FUDGE Schematic Section 2: Read User Specs -----
# TODO The following variables will be passed from experGen 
#' user specified options -----
#' predictor.vars: String vector, list of predictor variables  
#' predictand.var: predictand variable
#' -----Information on Historical Training Predictor dataset------
#' hist.file.start.year_1: file start year
#' hist.file.end.year_1: file end year
#' hist.train.start.year_1: train start year
#' hist.train.end.year_1: train end year
#' hist.scenario_1: experiment,ensemble member specification 
#' hist.nyrtot_1: Total number of years 
#' hist.model_1: Name of the model
#' hist.freq_1: Time Frequency 
#' hist.indir_1: Input directory 
#' -----Information on Future Predictor dataset---------
#' fut.file.start.year_1: file start year
#' fut.file.end.year_1: file end year
#' fut.train.start.year_1: train start year
#' fut.train.end.year_1: train end year
#' fut.scenario_1: experiment and ensemble member specification
#' fut.nyrtot_1: Total number of years 
#' fut.model_1: Model name 
#' fut.freq_1: Time frequency 
#' fut.indir_1: Input directory
#' -----Information on Target Dataset used in training------
#' target.file.start.year_1: file start year
#' target.file.end.year_1: file end year
#' target.train.start.year_1: train start year
#' target.train.end.year_1: train end year
#' target.scenario_1: experiment, ensemble member specification 
#' target.nyrtot_1: Total number of years
#' target.model_1: Name of the model 
#' target.freq_1: Time frequency 
#' target.indir_1: Input directory
#' ----------- masks -----------------------
#' spat.mask.dir_1: spatial mask directory
#' spat.mask.var: spatial mask variable (Name of the Region to be downscaled)
#' -----Method descriptors------------------------
#' ds.method: Name of the downscaling method/library
#' ds.experiment: Name of the downscaling experiment
#' k.fold: Value of 'k' in k-fold cross-validation
#' -----Custom method-specific information if any ----
#'  lopt.wetday:   single element logical vector containing user-specified option
#'                 specifying whether function WetDayID should be called (T,F)
#'  opt.wetday:    single element numeric vector containing user-specified option
#'                 indicating which threshold option for wet vs dry day to use
#'  opt.transform: single element numeric vector containing user-specified option
#'                 setting what type of data transform, if any, is to be done
#'  npas : for CDFt number of cuts
#' -----Output root location---------------------
# ----- Begin driver program to develop different sections in FUDGE plus CDFt train -----
#' output.dir: Output root location
#'------- Lon/I,Lat/J range to be downscaled ------
#' i.start: I index start, it is typically 1 since we are reading and writing to minifiles per longitude
#' J.range.suffix: J File range suffix to identify suffix of the input files. Get j.start, j.end from here
#' j.start: J index start - use in file suffix
#' j.end: J index end - use in file suffix
#' loop.start: J loop start , use in writing output
#' loop.end: J loop end, use in writing output

#message("Attempting to break script deliberately")
#print(fakevar)

message("Setting downscaling method information")
SetDSMethodInfo(ds.method, predictor.vars)
message("Checking downscaling arguments")
QCDSArguments(k=k.fold, ds.method = ds.method, args=args, 
              is.multivar=(length(predictor.vars) > 1), 
              is.multi_ens=(length(fut.pred.rip.list) > 1))

#Check for writable output directory 
#TODO: remove this; only needed if testing R code separately
# message("Checking output directory")
# QCIO(output.dir)

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
qc.maskopts <- qc.mask.check(post.ds.train, post.ds)
pre.ds <- compact(lapply(pre_ds, index.a.list, 'loc', 'outloop'))
pre.ds.train <- compact(lapply(pre_ds, index.a.list, 'loc', 'inloop'))
#Generate metadata references for the pre- and post-processing functions
#Things that contain an ADJUSTMENT
post_ds_adj <- compact(lapply(post_ds, index.a.list, 'adjust.out', 'on'))

#Then, check that the s3 and s5 adjustments you are performing make sense in 
#this context
QCAdjustArguments(pre_ds, post_ds, 
                 is.multivar=(length(predictor.vars) > 1), 
                 is.multi.ens=(length(fut.pred.rip.list) > 1), 
                 target.in.predictors = (target.var%in%predictor.vars))

#### Then, read in spatial and temporal masks. Those will be used
#### not only for the masks, but as an immediate check upon the dimensions
#### of the files being read in.

# # spatial mask read check
message("Checking for spatial masks vars")
if(spat.mask.dir_1!="na"){
  spat.mask.filename <- paste(spat.mask.var,".","I",i.file,"_",file.j.range,".nc",sep='')
  spat.mask.ncobj <- OpenNC(spat.mask.dir_1,spat.mask.filename)
  print('OpenNC spatial mask: success..1')
  spat.mask <- ReadMaskNC(spat.mask.ncobj, get.bounds.vars=FALSE)#TODO: remove opt for getting the bounds vars from fxn
  print('ReadMaskNC spatial mask: success..1')
}else{
  spat.mask <- NULL
  message("no spatial mask included; skipping to next step")
}

message("Reading in and checking time windowing masks")
#Either all data will use a time-windowing mask, or none will use it. 
if (train.and.use.same){ #set by SetDSMethodInfo() (currently edited for test settings)
  #Future data used in downscaling will be underneath the fut.time tag
  if(fut.time.trim.mask=='na'){
    #If there is no time trimming mask:
    print(paste("time trimming mask", fut.time.trim.mask))
    if(target.time.window!='na'){
      #If there are masks included (this should be the most common use case)
      message('Creating list of time windows')
      tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                         esd.gen.mask = fut.time.window, k=k.fold, method=ds.method)#TODO: Edit createtimewindowlist, too
      names(tmask.list) <- c("train.pred", "train.targ", "fut.pred")
    }else{
      #Otherwise, if there were no masks included at all
      message("no time windows included; moving on to next step")
      tmask.list <- list("na")
    }
  }else{
    #If there is a time trimming mask, and there are masks of all kinds
    print(paste("time trimming mask", fut.time.trim.mask))
    tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                       esd.gen.mask = fut.time.window, k=k.fold, method=ds.method, 
                                       time.prune.mask = fut.time.trim.mask)
    names(tmask.list) <- c("train.pred", "train.targ", "fut.pred", "time.trim.mask")
  }
}else{ #Once esd.gen implemented, should be most common use case
  #Data used in downscaling (as opposed to training ) will be underneath the esdgen tag
  if(target.time.window!='na'){
  tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                     esd.gen.mask = esdgen.time.window, k=kfold, method=ds.method)
  names(tmask.list) <- c("train.pred", "train.targ", "esd.gen")
  }else{
    #Otherwise, if there were no masks included at all
    message("no time windows included; moving on to next step")
    tmask.list <- list("na")
  }
}

### Now, access input data sets
message("Reading in target data")
target.filename <- GetMiniFileName(target.var,target.freq_1,target.model_1,target.scenario_1,grid,
                                   target.file.start.year_1,target.file.end.year_1,i.file,file.j.range)
print(target.filename)
list.target <- ReadNC(OpenNC(target.indir_1, target.filename), var=target.var, dim="spatial") #no longer adding ens dim
message("Applying spatial mask to target data")
#list.target$clim.in <- ApplySpatialMask(list.target$clim.in, spat.mask$masks[[1]])
if(!is.null(spat.mask)){
  list.target$clim.in <-apply.any.mask(data=list.target$clim.in, mask=spat.mask$masks[[1]], dim.apply='spatial')
}

#TODO: When multiple RIP support is enabled, move the output files to the inner RIP loop
out.filename <- GetMiniFileName(target.var,fut.freq_1,ds.experiment,fut.scenario_1,ds.region,
                                fut.file.start.year_1,fut.file.end.year_1,i.file,file.j.range)

hist.filename <- GetMiniFileName("VAR",hist.freq_1,hist.model_1,hist.scenario_1,grid,
                                 hist.file.start.year_1,hist.file.end.year_1,i.file,file.j.range)
message("Reading in historical predictor data")
list.hist <- ReadMultiVars(file.prefix=hist.indir_1, 
                           file.suffix=pred.dir.suffix, 
                           blank.filename=hist.filename, 
                           var.list=predictor.vars, 
                           add.ens.dim=TRUE,
                           verbose=TRUE)
message("Applying spatial mask to historic predictor dataset")
if(!is.null(spat.mask)){
  list.hist$clim.in <- lapply(list.hist$clim.in, apply.any.mask, mask=spat.mask$masks[[1]], dim.apply='spatial')
}

fut.filename <- GetMiniFileName("VAR",fut.freq_1,fut.model_1,fut.scenario_1,grid,
                                 fut.file.start.year_1,fut.file.end.year_1,i.file,file.j.range)
message("Reading in future predictor data")
list.fut <- ReadMultiVars(file.prefix=fut.indir_1, 
                           file.suffix=pred.dir.suffix, 
                           blank.filename=fut.filename, 
                           var.list=predictor.vars,
                          rip.list = fut.pred.rip.list, rip.suffix = fut.pred.suffix, 
                           dim=c('temporal'), 
                           add.ens.dim=TRUE,
                          #check.for.ens = TRUE, ens.member = fut.scenario_1,
                          #rip.list = fut.pred.rip.list, rip.suffix = fut.filename.suffix, 
                           verbose=TRUE)

message("Applying spatial mask to future predictor dataset")
if(!is.null(spat.mask)){
  list.fut$clim.in <- lapply(list.fut$clim.in, apply.any.mask, mask=spat.mask$masks[[1]], dim.apply='spatial')
}

message('Looking for pre-processing functions to apply')
if(length(pre.ds) !=0){
  message('Applying S3 Adjustment')
  temp.output <- callS3Adjustment(s3.instructions=pre.ds, 
                                  hist.pred = list.hist$clim.in,
                                  hist.targ = list.target$clim.in,
                                  fut.pred = list.fut$clim.in,,
                                  att.list =list('hist.pred'=list.hist$att_table, 
                                                 'hist.targ'=list.target$att_table, 
                                                 'fut.pred'=list.fut$att_table),
                                  s5.instructions=post.ds)
  #Assign output and remove temporary output 
  post.ds <- temp.output$s5.list
  list.target$clim.in <- temp.output$input$hist.targ
  list.hist$clim.in <- temp.output$input$hist.pred
  list.fut$clim.in <- temp.output$input$fut.pred
  remove(temp.output)
}

message("FUDGE training begins...")
message(paste("Starting at:", Sys.time()))
start.time <- proc.time()

#args should always exist; it's specified in the runcode
if (args[1]=='na'){
  ds.args=NULL
}else{
  ds.args=args
}
all.att.table <- list('hist.pred'=list.hist$att_table, 
                      'hist.targ'=list.target$att_table, 
                      'fut.pred'=list.fut$att_table)
##DS does not currently include the attribute table; frankly, should include it. 
##This si sprobably the next to-do item on the list
  ds <- TrainDriver(target.masked.in = list.target$clim.in, 
                    hist.masked.in = list.hist$clim.in, 
                    fut.masked.in = list.fut$clim.in, ds.var=target.var, 
                    att.table =list('hist.pred'=list.hist$att_table, 
                                    'hist.targ'=list.target$att_table, 
                                    'fut.pred'=list.fut$att_table),
                    mask.list = tmask.list, ds.method = ds.method, k=0, time.steps=NA, 
                    istart = NA,loop.start = NA,loop.end = NA, downscale.args=ds.args,
                    s3.instructions=pre.ds.train,
                    s5.instructions=post.ds.train, 
                    create.qc.mask=(qc.maskopts$qc.inloop))
#print(summary(ds$esd.final[!is.na(ds$esd.final)]))
message("FUDGE training ends")
message(paste("FUDGE training took", proc.time()[1]-start.time[1], "seconds to run"))
# save(file="~/Code/testing/test_out.R", list=c('ds'))
# stop('examine your results')
############## end call TrainDriver ######################################
# + + + end Training + + +


# ----- Begin segment like FUDGE Schematic Section 5: Apply Distribution Back-Transform -----

#Call the Section 5 Adjustments to be applied to post-downscaled output
message("Calling Section 5 Adjustments")
if(length(post.ds) !=0){
  temp.postproc <- callS5Adjustment(post.ds,
                                    data = ds$esd.final,
                                    hist.pred = list.hist$clim.in, 
                                    hist.targ = list.target$clim.in, 
                                    fut.pred  = list.fut$clim.in, 
                                    data.atts = all.att.table)
  ds$esd.final <- temp.postproc$ds.out
  if(qc.maskopts$qc.outloop){
    ds$qc.mask <- temp.postproc$qc.mask
  }
  remove(temp.postproc)
}

# ----- Begin segment like FUDGE Schematic Section 6: Write Downscaled results to data files -----
# rip.list <- lapply(att_table, '[[', 'rip')
# rips <- unique(rip.list)

#Output file destinations should be saved and printed for later reference
#But there can be some debate about how best to do that
out.file.list <- rep(list(NA), length(fut.pred.rip.list))
names(out.file.list) <- unlist(fut.pred.rip.list)

for (rip in fut.pred.rip.list){
  #Replace NAs by missing
  ds.var <- paste0(target.var, ".", rip)
  ds.temp <- ds$esd.final[[ds.var]]
  ds.temp[is.na(ds.temp)] <- 1.0e+20 #TODO: Mod for changing all missing values. 
  
  loop.out.dir <- output.dir
  loop.out.dir <- sub("$(RIP)", rip, loop.out.dir, fixed=TRUE)
  out.file <- paste(loop.out.dir,"/", out.filename,sep='')
  out.loop.file <- sub(experiment.rip, rip, out.file)
  message(paste("Writing to file", out.loop.file))
  ds.out.filename = WriteNC(out.loop.file, ds.temp,
                            target.var,
                            dim.list=c(list.target$dim, list.fut$dim),
                            var.data=c(list.target$vars, list.fut$vars),
                            #prec=list.target$att_table[[target.var]]$prec,
                            prec='double',
                            units=list.target$att_table[[target.var]]$units,
                            lname=paste('Downscaled', list.target$att_table[[target.var]]$long_name), #list.fut$long_name$value
                            cfname=list.target$att_table[[target.var]]$standard_name, 
                            #var.orig.file=attr(list.target, "filename"),
                            verbose=TRUE 
  )
  out.file.list[[rip]] <- ds.out.filename
  
  #Write Global attributes to downscaled netcdf
  label.training <- paste(hist.model_1,".",hist.scenario_1,".",hist.train.start.year_1,"-",hist.train.end.year_1,sep='')
  label.validation <- paste(fut.model_1,".",fut.scenario_1,".",fut.train.start.year_1,"-",fut.train.end.year_1,sep='')
  
  ###Code to determine whether or not to include the git branch
  if(Sys.getenv("USERNAME")=='cew'){
    git.needed=TRUE
  }else{
    #Someone else is running it, modules are available and presumably git branch not needed
    git.needed=FALSE
  }
  message('writing global attributes')
  WriteFUDGEGlobals(out.loop.file,k.fold,target.var,predictor.vars,label.training,ds.method,
                    configURL,label.validation,institution='NOAA/GFDL',
                    version=as.character(parse(file=paste(FUDGEROOT, "version", sep=""))),
                    title=paste(simpleCap(target.var), "downscaled from", 
                                convert.list.to.string(predictor.vars), "with", ds.method, ds.experiment),
                    ds.arguments=args, time.masks=tmask.list, ds.experiment=ds.experiment, 
                    grid_region=grid, mask_region=ds.region,
                    time.trim.mask=fut.time.trim.mask, 
                    tempdir=TMPDIR, include.git.branch=git.needed, FUDGEROOT=FUDGEROOT, BRANCH=BRANCH,
                    is.pre.ds.adjust=(length(pre_ds) > 0),
                    pre.ds.adjustments=pre_ds,
                    is.post.ds.adjust=(length(post_ds) > 0),
                    post.ds.adjustments=post_ds, 
                    climatology=list.fut$att_table[[target.var]]$climatology, 
                    fut.pred.rip = rip)
  message(paste('Downscaled output file:',ds.out.filename,sep=''))
  #}
  
  if(qc.maskopts$qc.inloop || qc.maskopts$qc.outloop){ ##Created waaay back at the beginning, as part of the QC functions
    for (var in target.var){
      ds$qc.mask[is.na(ds$qc.mask)] <- as.double(1.0e20)
      qc.var <- paste(var, 'qcmask', sep="_")
      #Sunstitute to get mask output directory
      mask.loop.out.dir <- mask.output.dir
      mask.loop.out.dir <- sub("$(RIP)", rip, mask.loop.out.dir, fixed=TRUE)
      #REMOVE THIS LINE FOR THE FINAL RELEASE
      if(!(Sys.info()["nodename"]=="ldt-4078325")){
        mask.loop.out.dir <- paste0(mask.loop.out.dir, "/", qc.var, "/")
      }
      qc.file <- paste(mask.loop.out.dir, sub(pattern=var, replacement=qc.var, out.filename), sep="")
      qc.file <- sub(experiment.rip, rip, qc.file)
      
      ###Check to make sure that it is possible to create the qc file; create dirs if not
      message("Attempting creation of QC file")
      message(qc.file)
      exists <- file.create(qc.file)
      if(!exists){
        print("ERROR! Dir creation script not beahving as expected!")
      }
      message(paste('attempting to write to', qc.file))
      qc.out.filename = WriteNC(qc.file,ds$qc.mask[[ds.var]],qc.var,
                                dim.list=c(list.target$dim, list.fut$dim),
                                var.data=c(list.target$vars, list.fut$vars),
                                prec='float',
                                units="1",
                                lname=paste('QC Mask')
      )
      #For now, patch the variables in here until se get s5 formalized in the XML
      WriteFUDGEGlobals(qc.out.filename,k.fold,target.var,predictor.vars,label.training,ds.method,
                        configURL,label.validation,institution='NOAA/GFDL',
                        version=as.character(parse(file=paste(FUDGEROOT, "version", sep=""))),title=paste(target.var, "downscaled with", 
                                                                                                          ds.method, ds.experiment), 
                        ds.arguments=args, time.masks=tmask.list, ds.experiment=ds.experiment, 
                        grid_region=grid, mask_region=ds.region,
                        time.trim.mask=fut.time.trim.mask, 
                        tempdir=TMPDIR, include.git.branch=git.needed, FUDGEROOT=FUDGEROOT, BRANCH=BRANCH,
                        is.qcmask=TRUE,
                        qc.method=qc.maskopts$qc.method, qc.args=qc.maskopts$qc.args,
                        is.pre.ds.adjust=(length(pre_ds) > 0),
                        pre.ds.adjustments=pre_ds,
                        is.post.ds.adjust=(length(post_ds) > 0),
                        post.ds.adjustments=post_ds_adj, 
                        fut.pred.rip = rip)
      message(paste('QC Mask output file:',qc.out.filename,sep=''))
    }
  }
}
#Do not change formatting of this: it is used as a flag by two components of the
#regression testing scripts parsing stdout
#message(paste('Final Downscaled output file location:', sub(pattern=TMPDIR, replacement="", ds.out.filename),sep=""))
#message(paste('Final Downscaled output file location:', ds.out.filename,sep=""))
message(paste('File(s)', convert.list.to.string(out.file.list), "written to file."))
message(paste('Final Downscaled output file location:', convert.list.to.string(out.file.list),sep=""))
message(paste('Target file location:', attr(list.target, 'filename')))
message(paste('Future Predictor file(s):', attr(list.fut, 'filename')))
message(paste('Historic Predictor files(s):', attr(list.hist, 'filename')))