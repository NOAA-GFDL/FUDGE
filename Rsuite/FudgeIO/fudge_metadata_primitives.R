make.fudge.history <- function(inst,ver){
  paste('File processed at',inst,
        'using FUDGE (Framework For Unified Downscaling of GCMs Empirically) developed at GFDL, using version:', 
        ver,'on', date())
}
make.fudge.comment <- function(ds.method, ds.var, grid.region, mask.region, kfold, 
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

make.fudge.info <- function(tmask.filenames, missval.args, 
                            ds.method, ds.args, 
                            pres_ds, post_ds){
  #'Makes several attribute that, when put together, produce the info
  #'attributes - the description of the settings that get put into 
  #'a FUDGE run.
  missval.string <- paste("After removing missing values with the", 
                          missval.args$missval_method, "method,", 
                          "inputs had at least", missval.args$minsamplesize, 
                          "non-missing value(s);")
  #Downscaling method
  if(ds.args[[1]]=='na'){
    fudge.argstring <- "default settings"
  }else{
    fudge.argstring <- paste(mapply(paste, names(ds.args), ds.args, sep="="), collapse=",")
  }
  info_ds <- paste0(ds.method, " : (", paste(fudge.argstring), ")")
  
  #Info_tmasks
  if (length(tmask.filenames) != 0){
  tmask_list <- mapply(paste, names(tmask.filenames), 
                       #sapply(tmask_str, sub, pattern=TMPDIR, replacement="", fixed=T))
                       sapply(tmask.filenames, basename), sep="=")
  info_tmask <- paste(tmask_list, collapse=",")
  }else{
    info_tmask <- 'none_used'
  }
  #info pre_ds
  generate_s3s5_arglist <- function(arglist, argname='pp.args'){
    #Creates formatted list of args for keeping track of metadata attributes
    if (length(arglist)!=0){
    info <- paste('fxn', mapply(paste, lapply(arglist, '[[', 'type'), 
                                paste0(paste(lapply(arglist, '[[', argname), sep=",")), 
                                sep=":"), 
                         collapse=";")
    info <- gsub('\"', "", info)
    info <- gsub('list', "", info)
    info <- gsub('na', "", info)
    }else{
      info <- "none specified"
    }
    return(info)
  }
  #Okay - this bit right here is sort of horribly ad-hoc.
  #but it looks like it will work to get the 3^5 experiments out.
    info_pre_ds <- generate_s3s5_arglist(pre_ds, 'pp.args')
    info_post_ds <- generate_s3s5_arglist(post_ds, 'qc_args')
  return(list(info_ds = info_ds, 
              info_pre_ds = info_pre_ds, 
              info_post_ds = info_post_ds, 
              info_tmask = info_tmask, 
              info_missval = missval.string))
}
get.fudge.climatology <- function(){
  return("Why isn't the climatology being picked up as one of the returned atts?")
}

get.qc.comment <- function(qc.method){
  #'When we get more into the mask revisions, use qc_maskvar instead
  basedir = Sys.getenv("BASEDIR")
  if(basedir==""){basedir='/home/cew/Code/fudge/'}
  qc.metatable <- file.path(basedir, '/Rsuite/aux/qcmask_metadata.csv')
  qc.metadata <- read.csv(qc.metatable, stringsAsFactors=F)
  qc.methodlist <- qc.metadata[,2]
  qc.match <- which(qc.methodlist==qc.method)
  if (length(qc.match)==0){
    qc.comment <- "1 = unadjusted; NA = adjusted"
  }else{
    qc.comment <- as.character(qc.metadata[qc.match,3]) 
  }
  return(qc.comment)
}