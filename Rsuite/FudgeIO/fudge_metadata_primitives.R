make.fudge.history <- function(inst,ver){
  paste('File processed at',inst,
        'using FUDGE (Framework For Unified Downscaling of GCMs Empirically) developed at GFDL, using version:', 
        ver,'on', date())
}
make.fudge.comment <- function(ds.method, ds.var, mask.region, grid.region, kfold, 
                               experiment.name, experiment.config, training.data, 
                               predictor.vars, fut.pred.rips, qc.method=NA){
  #'The comment attribute stores a human-readable description of the experiment
  #'and its arguments.
  is_qcmask <- !is.na(qc.method)
  if (is_qcmask){
    string.start <- paste("Output produced from a QC mask check", qc.method, "performed upon", ds.method, 'downscaled output')
    string.desc <- paste("This is a QC check related to a downscaled estimate of", ds.var) 
  }else{
    string.start <- paste("Output produced by downscaling by the", ds.method, "method")
    string.desc <- paste("This is a downscaled estimate of", ds.var)
  }
  string.main <- paste("on the subset", mask.region, "of the", grid.region, "grid", 
                       ",and based on", paste0(kfold, "-fold cross-validation,"), 
                       "with experiment configuration", paste0(experiment.name,"."), 
                       string.desc, "for the", experiment.config, "experiment", 
                       "having done training using the", training.data, "time series,", 
                       "predictor(s) used:", paste0(predictor.vars,","), 
                       "future predictor rip used:", paste(fut.pred.rips, collapse=","))
  return(paste(string.start, string.main))
}
make.fudge.info <- function(){
#make.fudge.info <- function(tmask.filenames, pre_ds, post_ds, missval.args){
  #'The info attribute in the output file stores a summary of all args that would
  #'go into a downscaling experiment, with little comcern about being human-readable.
  #'This makes some bits trickier, since it has to do a lot more parsing to figure out
  #'what pieces are going to be present.
#   info = paste("Path to time mask files:  ", 
#                "train.pred:/archive/esd/PROJECTS/DOWNSCALING/MASKS/timemasks/maskdays_byseason_19790101-20081231.nc, ", 
#                "train.targ:/archive/esd/PROJECTS/DOWNSCALING/MASKS/timemasks/maskdays_byseason_19790101-20081231.nc, ", 
#                "fut.pred:/archive/esd/PROJECTS/DOWNSCALING/MASKS/timemasks/maskdays_byseason_20260101-20351231.nc", 
#                "Arguments used in adjustment functions before downscaling: PR:(thold = us_trace, freqadj = on, conserve = off, ignore0 = on)", 
#                "and after downscaling: PR:(thold = us_trace, conserve = off, ignore0 = on),flag.neg:(on) ;", 
#                "Downscaling performed on x,y,tmask points where, after", 
#                "removing missing values in input datasets with the distributional",
#                "missval handing method, input datasets had at least 30 non-missing value(s).; FUDGE:  hazelnut_20161013")
#   tmask.filenames <- sub(TMPDIR, "", tmask.filenames)
#   tmask.string <- paste("Path to time mask files:", tmask.filenames)
#   missval.string <- paste("after removing missing values with the", missval.args$method, "method", 
#                           "inputs had at least", missval.args$minsamplesize, "non-missing value(s);")
  
#   ds.method <- "lm"
#   ds.argstring <- ""
#   info_ds <- paste0(ds.method, " : (", paste(ds.argstring), ")")
#   #Info_tmasks
#   tmask_str<- tmasks$filenames
#   tmask_list <- mapply(paste, names(tmask_str), sapply(tmask_str, sub, pattern=TMPDIR, replacement="", fixed=T))
#   info_tmask <- paste(tmask_list, collapse=",")
#   #info pre_ds
#   generate_s3s5_arglist <- function(arglist, argname='pp.args'){
#     #Creates formatted list of args for keeping track of metadata attributes
#     info <- paste('fxn', mapply(paste, lapply(arglist, '[[', 'type'), 
#                                 paste0(paste(lapply(arglist, '[[', argname), sep=",")), 
#                                 sep=":"), 
#                          collapse=";")
#     info <- gsub('\"', "", info)
#     info <- gsub('list', "", info)
#     info <- gsub('na', "", info)
#     return(info)
#   }
#   info_pre_ds <- generate_s3s5_arglist(pre_ds, 'pp.args')
#   info_post_ds <- generate_s3s5_arglist(post_ds, 'qc_args')
  #flag.neg needs to be adjusted to NOT have it only arg be 'on'
  #info post_ds
  return("Adding info attributes later; this is going to take a while to track")
}
get.fudge.climatology <- function(){
  return("Why isn't the climatology being picked up as one of the returned atts?")
}

get.qc.comment <- function(qc.method){
  #'When we get mor einto the mask revisions, use qc_maskvar instead
  basedir = Sys.getenv("BASEDIR")
  if(basedir==""){basedir='/home/cew/Code/fudge/'}
  qc.metatable <- file.path(basedir, '/Rsuite/aux/qcmask_metadata.csv')
  qc.metadata <- read.csv(qc.metatable)
  qc.methodlist <- qc.metadata[,2]
  qc.match <- which(qc.methodlist==qc.method)
  if (length(qc.match)==0){
    qc.comment <- "1 = unadjusted; NA = adjusted"
  }else{
    qc.comment <- qc.metadata[qc.match,3] 
  }
  return(qc.comment)
}